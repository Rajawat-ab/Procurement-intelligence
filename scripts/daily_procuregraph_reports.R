#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(mongolite)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(jsonlite)
  library(grid)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a) && nzchar(as.character(a))) a else b

load_project_env <- function() {
  for (path in c(".Renviron", ".env")) {
    if (file.exists(path)) readRenviron(path)
  }
}

env_value <- function(name, required = TRUE, default = "") {
  value <- trimws(Sys.getenv(name, unset = default))
  if (required && !nzchar(value)) {
    stop(sprintf("Missing required environment variable: %s", name), call. = FALSE)
  }
  value
}

safe_rename <- function(df, new_name, possible) {
  m <- intersect(possible, colnames(df))
  if (!length(m)) {
    df[[new_name]] <- NA
    return(df)
  }
  dplyr::rename(df, !!new_name := !!m[1])
}

to_numeric_amount <- function(x) {
  if (is.null(x)) return(NA_real_)
  cleaned <- gsub(",", "", trimws(as.character(x)), fixed = TRUE)
  cleaned <- gsub("[^0-9.-]", "", cleaned)
  suppressWarnings(as.numeric(cleaned))
}

extract_amount_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  if (is.data.frame(x)) {
    preferred <- intersect(c("amount", "value", "total", "netAmount", "grossAmount"), names(x))
    if (length(preferred)) return(extract_amount_value(x[[preferred[1]]][1]))
    return(extract_amount_value(x[[1]][1]))
  }
  if (is.list(x)) {
    preferred <- intersect(c("amount", "value", "total", "netAmount", "grossAmount"), names(x))
    if (length(preferred)) return(extract_amount_value(x[[preferred[1]]]))
    return(extract_amount_value(x[[1]]))
  }
  val <- to_numeric_amount(x[1])
  if (length(val) == 0 || is.na(val)) NA_real_ else val
}

normalize_amount_column <- function(x, n) {
  if (is.null(x)) return(rep(0, n))
  if (is.data.frame(x) && nrow(x) == n) {
    preferred <- intersect(c("amount", "value", "total", "netAmount", "grossAmount"), names(x))
    if (length(preferred)) return(coalesce(to_numeric_amount(x[[preferred[1]]]), 0))
    return(coalesce(to_numeric_amount(x[[1]]), 0))
  }
  if (is.list(x) && length(x) == n) {
    return(coalesce(vapply(x, extract_amount_value, numeric(1)), 0))
  }
  out <- to_numeric_amount(x)
  if (length(out) == n) return(coalesce(out, 0))
  if (length(out) == 1) return(rep(coalesce(out, 0), n))
  rep(0, n)
}

get_mongo_items <- function(start_date, end_date, mongo_conn) {
  col <- mongo(collection = "item", db = "procurement", url = mongo_conn)
  on.exit(col$disconnect(), add = TRUE)
  t0 <- as.numeric(as.POSIXct(start_date)) * 1000
  t1 <- as.numeric(as.POSIXct(end_date + 1)) * 1000
  pipeline <- sprintf('[
    { "$match": { "creationDate": { "$gte": %s, "$lte": %s } } },
    { "$project": {
        "_id": 1,
        "buyerId": "$buyer.id",
        "status": 1,
        "creationDate": 1,
        "source": 1,
        "stage": 1,
        "histPoCustomerPoNo": "$history.po.customerPoNo",
        "histPoCustomerSANumber": "$history.po.customerSANumber",
        "amount": 1
    }}
  ]', t0, t1)
  col$aggregate(pipeline)
}

get_mongo_grn <- function(start_date, end_date, mongo_conn) {
  col <- mongo(collection = "grn", db = "procurement", url = mongo_conn)
  on.exit(col$disconnect(), add = TRUE)
  t0 <- as.numeric(as.POSIXct(start_date)) * 1000
  t1 <- as.numeric(as.POSIXct(end_date + 1)) * 1000
  query <- sprintf('{"creationDate":{"$gte":%s,"$lte":%s}}', t0, t1)
  fields <- '{"_id":1,"buyerId":1,"creationDate":1,"customerGrnNo":1,"poId":1,"itemId":1}'
  tryCatch(col$find(query = query, fields = fields), error = function(e) data.frame())
}

load_mapping <- function(path) {
  pid <- read_excel(path, sheet = "PID-CID")
  colnames(pid) <- trimws(colnames(pid))
  plant <- pid %>%
    mutate(across(everything(), ~trimws(as.character(.x)))) %>%
    rename_with(~ifelse(.x %in% c("plantId", "PlantId", "Plant ID", "plant_id"), "plantId", .x)) %>%
    rename_with(~ifelse(.x %in% c("companyId", "CompanyId", "Company ID", "company_id"), "companyId", .x)) %>%
    select(plantId, companyId) %>%
    distinct(plantId, .keep_all = TRUE) %>%
    mutate(across(c(plantId, companyId), as.character))

  cid <- read_excel(path, sheet = "150 CID")
  colnames(cid) <- trimws(colnames(cid))
  col_map <- list(
    companyId = c("Company ID", "CompanyID", "company_id", "companyId", "COMPANY ID"),
    companyName = c("Company Name", "CompanyName", "company_name", "COMPANY NAME"),
    `Final Status` = c("Integration Status", "IntegrationStatus", "integration_status", "Final Status", "INTEGRATION STATUS"),
    GRN = c("GRN Status", "GRNStatus", "grn_status", "GRN", "GRN STATUS"),
    BusinessPOC = c("Business POC", "BusinessPoc", "business_poc", "Business POC Email", "BUSINESS POC", "POC Email")
  )
  master <- cid
  for (n in names(col_map)) master <- safe_rename(master, n, col_map[[n]])
  company <- master %>%
    mutate(
      companyId = trimws(as.character(companyId)),
      companyName = as.character(companyName),
      `Final Status` = as.character(`Final Status`),
      GRN = as.character(GRN),
      BusinessPOC = trimws(as.character(BusinessPOC))
    ) %>%
    distinct(companyId, .keep_all = TRUE)

  list(plant = plant, company = company)
}

process_items <- function(raw, mapping, start_date, end_date) {
  if (is.null(raw) || !nrow(raw)) {
    return(data.frame(
      source = character(),
      Amount_Crore = numeric(),
      `Final Status` = character(),
      `Remove(Y/N)` = character(),
      `PID-CID` = character(),
      `Client Name` = character(),
      Month = character(),
      check.names = FALSE
    ))
  }
  if (!"amount" %in% names(raw)) raw$amount <- NA
  amount_numeric <- normalize_amount_column(raw$amount, nrow(raw))

  raw %>%
    mutate(
      buyer_id_str = trimws(as.character(buyerId)),
      histPoCustomerPoNo = trimws(as.character(histPoCustomerPoNo)),
      histPoCustomerSANumber = trimws(as.character(histPoCustomerSANumber)),
      amount = amount_numeric,
      Amount_Crore = amount / 10000000,
      Stage = as.character(stage),
      CreationDate = as.Date(as.POSIXct(creationDate / 1000, origin = "1970-01-01")),
      Month = format(CreationDate, "%Y-%m"),
      `Consider Month` = ifelse(CreationDate >= start_date & CreationDate <= end_date, "Consider", "Ignore")
    ) %>%
    left_join(mapping$plant, by = c("buyer_id_str" = "plantId")) %>%
    left_join(mapping$company, by = "companyId") %>%
    mutate(
      `PID-CID` = coalesce(companyId, "Unknown"),
      `Client Name` = coalesce(companyName, "Unknown"),
      `Consider Source` = ifelse(source %in% c("EOC", "Manual", "SAP"), source, "Ignore"),
      `Consider Stage` = ifelse(Stage %in% c("Closed", "Cancelled"), "Ignore", "Consider"),
      `Final Status` = coalesce(`Final Status`, "Ignore"),
      GRN = coalesce(GRN, "Ignore"),
      RemoveHavellsCapex = ifelse(`PID-CID` == "1211",
                                  ifelse(substr(histPoCustomerSANumber, 1, 2) == "45", "Consider", "Ignore"),
                                  "Consider"),
      RemoveSC = ifelse(`PID-CID` == "8853",
                        ifelse(buyer_id_str %in% c("9550", "9557", "25976", "26023", "26186", "26187"),
                               ifelse(substr(histPoCustomerSANumber, 1, 2) == "71", "Ignore", "Consider"),
                               "Ignore"),
                        "Consider"),
      `Remove(Y/N)` = ifelse(
        `Consider Source` == "Ignore" | `Consider Stage` == "Ignore" |
          `Consider Month` == "Ignore" | `Final Status` == "Ignore" |
          RemoveHavellsCapex == "Ignore" | RemoveSC == "Ignore" | GRN == "Ignore",
        "Remove", "Consider"
      )
    )
}

create_pivot <- function(data, value_mode = FALSE) {
  if (is.null(data) || !nrow(data)) {
    out <- data.frame(
      `Final Status` = c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout", "Grand Total"),
      EOC = 0,
      Manual = 0,
      SAP = 0,
      `Grand Total` = 0,
      check.names = FALSE
    )
    return(out)
  }
  metric <- if (value_mode) "value_cr" else "count"
  pv <- data %>%
    filter(`Remove(Y/N)` == "Consider",
           `Final Status` %in% c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout")) %>%
    group_by(`Final Status`, source) %>%
    summarise("{metric}" := if (value_mode) sum(Amount_Crore, na.rm = TRUE) else n(), .groups = "drop") %>%
    pivot_wider(names_from = source, values_from = all_of(metric), values_fill = 0)
  for (col in c("EOC", "Manual", "SAP")) if (!col %in% names(pv)) pv[[col]] <- 0
  pv <- pv %>%
    mutate(
      `Grand Total` = EOC + Manual + SAP,
      `Final Status` = factor(`Final Status`, levels = c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout"))
    ) %>%
    arrange(`Final Status`)
  tr <- pv %>%
    summarise(`Final Status` = "Grand Total", EOC = sum(EOC), Manual = sum(Manual), SAP = sum(SAP), `Grand Total` = sum(`Grand Total`))
  out <- bind_rows(pv, tr)
  if (value_mode) out <- out %>% mutate(across(c(EOC, Manual, SAP, `Grand Total`), ~round(.x, 2)))
  out
}

calc_metrics <- function(pv) {
  pc <- pv %>% filter(`Final Status` != "Grand Total")
  total <- sum(pc$`Grand Total`, na.rm = TRUE)
  if (total <= 0) return(list(integration = 0, cbb = 0, final = 0))
  sap <- sum(pc$SAP, na.rm = TRUE)
  man <- sum(pc$Manual, na.rm = TRUE)
  list(
    integration = round(sap / total * 100, 1),
    cbb = round(man / total * 100, 1),
    final = round((sap + man) / total * 100, 1)
  )
}

company_summary <- function(df) {
  if (!nrow(df)) {
    return(data.frame(
      `PID-CID` = character(),
      `Client Name` = character(),
      SAP = integer(),
      Manual = integer(),
      EOC = integer(),
      Grand_Total = integer(),
      Integrated_Volume = integer(),
      Integration_Pct = numeric(),
      Total_Value_Cr = numeric(),
      Integrated_Value_Cr = numeric(),
      Value_Integration_Pct = numeric(),
      EOC_Value_Cr = numeric(),
      check.names = FALSE
    ))
  }
  df %>%
    filter(`Remove(Y/N)` == "Consider", `PID-CID` != "9802", `Client Name` != "Unknown") %>%
    group_by(`PID-CID`, `Client Name`) %>%
    summarise(
      SAP = sum(source == "SAP", na.rm = TRUE),
      Manual = sum(source == "Manual", na.rm = TRUE),
      EOC = sum(source == "EOC", na.rm = TRUE),
      SAP_Value_Cr = sum(ifelse(source == "SAP", Amount_Crore, 0), na.rm = TRUE),
      Manual_Value_Cr = sum(ifelse(source == "Manual", Amount_Crore, 0), na.rm = TRUE),
      EOC_Value_Cr = sum(ifelse(source == "EOC", Amount_Crore, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Grand_Total = SAP + Manual + EOC,
      Integrated_Volume = SAP + Manual,
      Integration_Pct = ifelse(Grand_Total > 0, round(Integrated_Volume / Grand_Total * 100, 1), 0),
      Total_Value_Cr = SAP_Value_Cr + Manual_Value_Cr + EOC_Value_Cr,
      Integrated_Value_Cr = SAP_Value_Cr + Manual_Value_Cr,
      Value_Integration_Pct = ifelse(Total_Value_Cr > 0, round(Integrated_Value_Cr / Total_Value_Cr * 100, 1), 0)
    ) %>%
    filter(Grand_Total > 0)
}

fmt_n <- function(x) format(round(x, 0), big.mark = ",", scientific = FALSE)
fmt_cr <- function(x) format(round(x, 2), nsmall = 2, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) paste0(ifelse(is.na(x), 0, x), "%")

grn_summary_metrics <- function(df, grn, mapping) {
  grn_integrated_status <- c("Live", "Live - CBB", "PR Int. - Punchout")
  eligible <- mapping$company %>%
    mutate(GRN = toupper(trimws(as.character(GRN)))) %>%
    filter(
      GRN == "Y",
      companyId != "9802",
      `Final Status` %in% grn_integrated_status
    ) %>%
    distinct(companyId, .keep_all = TRUE)

  po_base <- df %>%
    filter(
      `Remove(Y/N)` == "Consider",
      `PID-CID` != "9802",
      `Final Status` %in% grn_integrated_status
    )

  grn_filtered <- data.frame()
  if (!is.null(grn) && nrow(grn)) {
    grn_filtered <- grn %>%
      mutate(
        buyerId = trimws(as.character(buyerId)),
        GrnDate = as.Date(as.POSIXct(creationDate / 1000, origin = "1970-01-01")),
        Month = format(GrnDate, "%Y-%m")
      ) %>%
      left_join(mapping$plant, by = c("buyerId" = "plantId")) %>%
      left_join(mapping$company, by = "companyId") %>%
      mutate(`PID-CID` = coalesce(companyId, "Unknown"))

    grn_y <- df %>%
      filter(GRN == "Y") %>%
      distinct(`PID-CID`) %>%
      pull(`PID-CID`)

    grn_filtered <- grn_filtered %>%
      filter(`PID-CID` %in% grn_y, `PID-CID` != "9802")
  }

  grn_records <- nrow(grn_filtered)
  po_line_items <- nrow(po_base)
  list(
    eligible_accounts = nrow(eligible),
    po_accounts = n_distinct(po_base$`PID-CID`),
    po_line_items = po_line_items,
    grn_records = grn_records,
    grn_accounts = if (grn_records > 0) n_distinct(grn_filtered$`PID-CID`) else 0,
    grn_pct = if (po_line_items > 0) round(grn_records / po_line_items * 100, 1) else 0
  )
}

draw_title <- function(title, subtitle) {
  grid.newpage()
  grid.rect(gp = gpar(fill = "#f8fbff", col = NA))
  grid.rect(y = unit(0.985, "npc"), height = unit(0.03, "npc"), gp = gpar(fill = "#da191e", col = NA))
  grid.text(title, x = unit(0.06, "npc"), y = unit(0.92, "npc"), just = "left", gp = gpar(fontsize = 22, fontface = "bold", col = "#0e1d28"))
  grid.text(subtitle, x = unit(0.06, "npc"), y = unit(0.875, "npc"), just = "left", gp = gpar(fontsize = 10, col = "#64748b"))
}

draw_kpis <- function(items, y = 0.74) {
  n <- length(items)
  w <- 0.88 / n
  for (i in seq_along(items)) {
    x <- 0.06 + (i - 1) * w
    grid.roundrect(x = unit(x, "npc"), y = unit(y, "npc"), width = unit(w - 0.015, "npc"), height = unit(0.16, "npc"),
                   just = c("left", "center"), r = unit(0.012, "npc"), gp = gpar(fill = "white", col = "#e2e8f0"))
    grid.text(items[[i]]$label, x = unit(x + 0.02, "npc"), y = unit(y + 0.035, "npc"), just = "left", gp = gpar(fontsize = 8, col = "#64748b", fontface = "bold"))
    grid.text(items[[i]]$value, x = unit(x + 0.02, "npc"), y = unit(y - 0.02, "npc"), just = "left", gp = gpar(fontsize = 18, col = items[[i]]$color, fontface = "bold"))
  }
}

draw_table <- function(tbl, x, y, w, row_h = 0.04, title = NULL, max_rows = 8) {
  if (!is.null(title)) {
    grid.text(title, x = unit(x, "npc"), y = unit(y + 0.04, "npc"), just = "left",
              gp = gpar(fontsize = 11, fontface = "bold", col = "#0e1d28"))
    grid.rect(x = unit(x, "npc"), y = unit(y + 0.018, "npc"), width = unit(0.06, "npc"), height = unit(0.004, "npc"),
              just = c("left", "top"), gp = gpar(fill = "#da191e", col = NA))
  }
  if (is.null(tbl) || !nrow(tbl)) {
    grid.text("No data available", x = unit(x, "npc"), y = unit(y, "npc"), just = "left", gp = gpar(fontsize = 9, col = "#94a3b8"))
    return(invisible(NULL))
  }
  tbl <- head(tbl, max_rows)
  cols <- names(tbl)
  col_w <- w / length(cols)
  grid.roundrect(x = unit(x, "npc"), y = unit(y + 0.006, "npc"), width = unit(w, "npc"),
                 height = unit(row_h * (nrow(tbl) + 1) + 0.012, "npc"),
                 just = c("left", "top"), r = unit(0.008, "npc"),
                 gp = gpar(fill = "white", col = "#dbeafe"))
  grid.rect(x = unit(x, "npc"), y = unit(y, "npc"), width = unit(w, "npc"), height = unit(row_h, "npc"), just = c("left", "top"), gp = gpar(fill = "#0e1d28", col = "#0e1d28"))
  for (j in seq_along(cols)) {
    tx <- if (j == 1) x + 0.012 else x + (j - 0.5) * col_w
    grid.text(cols[j], x = unit(tx, "npc"), y = unit(y - row_h / 2, "npc"),
              just = if (j == 1) "left" else "center",
              gp = gpar(fontsize = 7, fontface = "bold", col = "#ffffff"))
  }
  for (i in seq_len(nrow(tbl))) {
    yy <- y - i * row_h
    grid.rect(x = unit(x, "npc"), y = unit(yy, "npc"), width = unit(w, "npc"), height = unit(row_h, "npc"), just = c("left", "top"), gp = gpar(fill = ifelse(i %% 2 == 0, "#ffffff", "#f8fafc"), col = "#eef2f7"))
    for (j in seq_along(cols)) {
      tx <- if (j == 1) x + 0.012 else x + (j - 0.5) * col_w
      grid.text(as.character(tbl[i, j, drop = TRUE]), x = unit(tx, "npc"), y = unit(yy - row_h / 2, "npc"),
                just = if (j == 1) "left" else "center",
                gp = gpar(fontsize = 7, col = if (j == 1) "#0e1d28" else "#334155", fontface = if (j == 1) "bold" else "plain"))
    }
  }
}

make_report_pdf <- function(label, start_date, end_date, df, grn, mapping, out_file) {
  valid <- df %>% filter(`Remove(Y/N)` == "Consider")
  valid_wo <- valid %>% filter(`PID-CID` != "9802")
  pv <- create_pivot(valid_wo, FALSE)
  pv_value <- create_pivot(valid_wo, TRUE)
  m_count <- calc_metrics(pv)
  m_value <- calc_metrics(pv_value)
  grn_metrics <- grn_summary_metrics(df, grn, mapping)
  comp <- company_summary(df)
  total_value <- sum(valid$Amount_Crore, na.rm = TRUE)
  integrated_value <- sum(valid$Amount_Crore[valid$source %in% c("SAP", "Manual")], na.rm = TRUE)

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  pdf(out_file, width = 11.69, height = 8.27, onefile = TRUE)
  on.exit(dev.off(), add = TRUE)

  subtitle <- sprintf("%s to %s | Generated %s", format(start_date, "%d %b %Y"), format(end_date, "%d %b %Y"), format(Sys.time(), "%d %b %Y %H:%M"))

  draw_title(paste("ProcureGraph", label, "Report"), subtitle)
  draw_kpis(list(
    list(label = "Valid Line Items", value = fmt_n(nrow(valid)), color = "#006495"),
    list(label = "Count Integration", value = fmt_pct(m_count$final), color = "#16a34a"),
    list(label = "Total Value Cr", value = fmt_cr(total_value), color = "#f97316"),
    list(label = "Value Integration", value = fmt_pct(m_value$final), color = "#da191e")
  ))
  top_comp <- comp %>% arrange(desc(Integrated_Volume)) %>% transmute(Company = str_trunc(`Client Name`, 28), `Int %` = fmt_pct(Integration_Pct), `Int Items` = fmt_n(Integrated_Volume), Total = fmt_n(Grand_Total))
  top_value <- comp %>% arrange(desc(Integrated_Value_Cr)) %>% transmute(Company = str_trunc(`Client Name`, 28), `Value %` = fmt_pct(Value_Integration_Pct), `Int Cr` = fmt_cr(Integrated_Value_Cr), `Total Cr` = fmt_cr(Total_Value_Cr))
  draw_table(top_comp, 0.06, 0.55, 0.42, title = "Top Companies by Integrated Items")
  draw_table(top_value, 0.53, 0.55, 0.42, title = "Top Companies by Integrated Value")

  draw_title("PO Integration - Count View", subtitle)
  pv_show <- pv %>% mutate(across(c(EOC, Manual, SAP, `Grand Total`), fmt_n))
  draw_kpis(list(
    list(label = "SAP %", value = fmt_pct(m_count$integration), color = "#006495"),
    list(label = "Manual/CBB %", value = fmt_pct(m_count$cbb), color = "#16a34a"),
    list(label = "Final Count %", value = fmt_pct(m_count$final), color = "#da191e")
  ), y = 0.76)
  draw_table(pv_show, 0.08, 0.56, 0.84, title = "Integration Pivot - Line Item Counts", max_rows = 10)
  no_int <- valid_wo %>% filter(`Final Status` == "No Integration", `Client Name` != "Unknown") %>% count(`Client Name`, name = "Line Items", sort = TRUE) %>% transmute(Company = str_trunc(`Client Name`, 34), `Line Items` = fmt_n(`Line Items`))
  draw_table(no_int, 0.08, 0.28, 0.84, title = "No Integration - Highest Count Accounts", max_rows = 6)

  draw_title("PO Integration - Value View", subtitle)
  pv_value_show <- pv_value %>% mutate(across(c(EOC, Manual, SAP, `Grand Total`), fmt_cr))
  draw_kpis(list(
    list(label = "Integrated Value Cr", value = fmt_cr(integrated_value), color = "#16a34a"),
    list(label = "Total Value Cr", value = fmt_cr(total_value), color = "#f97316"),
    list(label = "Final Value %", value = fmt_pct(m_value$final), color = "#da191e")
  ), y = 0.76)
  draw_table(pv_value_show, 0.08, 0.56, 0.84, title = "Integration Pivot - Value in Crores", max_rows = 10)
  value_action <- comp %>% arrange(desc(EOC_Value_Cr), desc(Total_Value_Cr)) %>% transmute(Company = str_trunc(`Client Name`, 34), `EOC Cr` = fmt_cr(EOC_Value_Cr), `Total Cr` = fmt_cr(Total_Value_Cr), `Value %` = fmt_pct(Value_Integration_Pct))
  draw_table(value_action, 0.08, 0.28, 0.84, title = "Largest Value Opportunity Accounts", max_rows = 6)

  draw_title("GRN and Action Summary", subtitle)
  draw_kpis(list(
    list(label = "GRN Records", value = fmt_n(grn_metrics$grn_records), color = "#16a34a"),
    list(label = "Eligible PO Line Items", value = fmt_n(grn_metrics$po_line_items), color = "#006495"),
    list(label = "Overall GRN %", value = fmt_pct(grn_metrics$grn_pct), color = "#f59e0b"),
    list(label = "Action Accounts", value = fmt_n(nrow(no_int)), color = "#da191e")
  ), y = 0.76)
  grn_tbl <- data.frame(
    Metric = c("GRN Eligible Accounts", "PO Volume Accounts", "Valid PO Line Items", "GRN Volume Accounts", "GRN Records", "Overall GRN %"),
    Value = c(
      fmt_n(grn_metrics$eligible_accounts),
      fmt_n(grn_metrics$po_accounts),
      fmt_n(grn_metrics$po_line_items),
      fmt_n(grn_metrics$grn_accounts),
      fmt_n(grn_metrics$grn_records),
      fmt_pct(grn_metrics$grn_pct)
    ),
    check.names = FALSE
  )
  draw_table(grn_tbl, 0.08, 0.52, 0.34, title = "GRN Integration Snapshot", max_rows = 6)
  action_tbl <- comp %>%
    filter(Grand_Total >= 10) %>%
    mutate(Opportunity = round((100 - Integration_Pct) * log1p(Grand_Total), 1)) %>%
    arrange(desc(Opportunity), desc(Grand_Total)) %>%
    transmute(Company = str_trunc(`Client Name`, 34), `Count %` = fmt_pct(Integration_Pct), `Value %` = fmt_pct(Value_Integration_Pct), Items = fmt_n(Grand_Total), `Total Cr` = fmt_cr(Total_Value_Cr))
  draw_table(action_tbl, 0.47, 0.52, 0.46, title = "Priority Accounts for Follow-up", max_rows = 8)
  grid.text("GRN % follows the app: GRN records / valid PO line items for GRN-enabled, PO-integrated accounts. Values shown in crores.",
            x = unit(0.08, "npc"), y = unit(0.12, "npc"), just = "left", gp = gpar(fontsize = 9, col = "#64748b"))

  invisible(out_file)
}

mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "pdf") "application/pdf" else "application/octet-stream"
}

send_email_with_attachments <- function(files, subject, body, html = FALSE) {
  from <- env_value("PROCUREGRAPH_EMAIL_FROM", required = FALSE)
  to <- env_value("PROCUREGRAPH_EMAIL_TO", required = FALSE)
  password <- env_value("PROCUREGRAPH_EMAIL_PASS", required = FALSE)
  host <- env_value("PROCUREGRAPH_SMTP_HOST", required = FALSE, default = "smtp.office365.com")
  port <- env_value("PROCUREGRAPH_SMTP_PORT", required = FALSE, default = "587")
  if (!nzchar(from) || !nzchar(to) || !nzchar(password)) {
    message("Email skipped: set PROCUREGRAPH_EMAIL_FROM, PROCUREGRAPH_EMAIL_TO, and PROCUREGRAPH_EMAIL_PASS.")
    return(invisible(FALSE))
  }

  boundary <- paste0("PROCUREGRAPH-", format(Sys.time(), "%Y%m%d%H%M%S"))
  recipients <- trimws(unlist(strsplit(to, "[,;]")))
  recipients <- recipients[nzchar(recipients)]
  lines <- c(
    paste0("From: ", from),
    paste0("To: ", paste(recipients, collapse = ", ")),
    paste0("Subject: ", subject),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/mixed; boundary=\"", boundary, "\""),
    "",
    paste0("--", boundary),
    if (html) "Content-Type: text/html; charset=UTF-8" else "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: 7bit",
    "",
    body
  )

  for (file in files) {
    encoded <- jsonlite::base64_enc(readBin(file, "raw", n = file.info(file)$size))
    encoded <- paste(strwrap(encoded, width = 76), collapse = "\r\n")
    lines <- c(
      lines,
      paste0("--", boundary),
      paste0("Content-Type: ", mime_type(file), "; name=\"", basename(file), "\""),
      "Content-Transfer-Encoding: base64",
      paste0("Content-Disposition: attachment; filename=\"", basename(file), "\""),
      "",
      encoded
    )
  }
  lines <- c(lines, paste0("--", boundary, "--"), "")
  eml <- tempfile(fileext = ".eml")
  writeLines(lines, eml, useBytes = TRUE)
  on.exit(unlink(eml), add = TRUE)

  args <- c(
    "-sS", "--show-error",
    "--url", sprintf("smtp://%s:%s", host, port),
    "--ssl-reqd",
    "--user", paste0(from, ":", password),
    "--mail-from", from,
    "--upload-file", eml
  )
  rcpt_args <- unlist(lapply(recipients, function(addr) c("--mail-rcpt", addr)), use.names = FALSE)
  args <- append(args, rcpt_args, after = which(args == "--mail-from") + 1)
  status <- system2("curl", args = args)
  if (!isTRUE(status == 0)) stop(sprintf("Email send failed. curl exit code: %s", status), call. = FALSE)
  invisible(TRUE)
}

build_email_summary_html <- function(df, grn, mapping, report_files) {
  valid <- df %>% filter(`Remove(Y/N)` == "Consider", `PID-CID` != "9802")
  count_metrics <- calc_metrics(create_pivot(valid, value_mode = FALSE))
  value_metrics <- calc_metrics(create_pivot(valid, value_mode = TRUE))
  grn_metrics <- grn_summary_metrics(df, grn, mapping)
  escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  file_list <- paste0(
    "<li style='margin:4px 0;color:#475569;'>",
    escape_html(basename(report_files)),
    "</li>",
    collapse = ""
  )

  metric_row <- function(label, value, accent) {
    paste0(
      "<tr>",
      "<td style='padding:8px 0;color:#64748b;font-size:13px;'>", label, "</td>",
      "<td style='padding:8px 0;text-align:right;color:", accent, ";font-weight:800;font-size:15px;'>", value, "</td>",
      "</tr>"
    )
  }

  card <- function(title, rows, accent, icon) {
    paste0(
      "<td style='width:33.33%;padding:8px;vertical-align:top;'>",
      "<div style='background:#ffffff;border:1px solid #e2e8f0;border-radius:16px;padding:18px;box-shadow:0 12px 24px rgba(15,23,42,.06);min-height:190px;'>",
      "<div style='display:flex;align-items:center;gap:10px;margin-bottom:12px;'>",
      "<div style='width:34px;height:34px;border-radius:10px;background:", accent, ";color:#fff;text-align:center;line-height:34px;font-weight:800;'>", icon, "</div>",
      "<div style='font-size:13px;font-weight:800;color:#0e1d28;text-transform:uppercase;letter-spacing:.04em;'>", title, "</div>",
      "</div>",
      "<table style='width:100%;border-collapse:collapse;'>", paste(rows, collapse = ""), "</table>",
      "</div>",
      "</td>"
    )
  }

  paste0(
    "<!doctype html><html><body style='margin:0;padding:0;background:#eef4f8;font-family:Segoe UI,Arial,sans-serif;'>",
    "<div style='max-width:920px;margin:0 auto;padding:28px 18px;'>",
    "<div style='background:#0e1d28;border-radius:22px 22px 0 0;padding:24px 28px;border-bottom:4px solid #da191e;'>",
    "<div style='font-size:12px;color:#93c5fd;font-weight:700;text-transform:uppercase;letter-spacing:.12em;'>ProcureGraph Daily Intelligence</div>",
    "<div style='font-size:28px;line-height:1.2;color:#ffffff;font-weight:900;margin-top:6px;'>Automated Procurement Integration Summary</div>",
    "<div style='font-size:13px;color:#cbd5e1;margin-top:8px;'>", format(Sys.Date(), "%d %b %Y"), " | PDFs attached for MTD, Last Month and Last 3 Months</div>",
    "</div>",
    "<div style='background:#f8fbff;border:1px solid #dbeafe;border-top:none;border-radius:0 0 22px 22px;padding:18px;'>",
    "<table role='presentation' style='width:100%;border-collapse:collapse;'><tr>",
    card(
      "PO Integration Item Wise",
      c(
        metric_row("Integration", paste0(count_metrics$integration, "%"), "#006495"),
        metric_row("CBB", paste0(count_metrics$cbb, "%"), "#16a34a"),
        metric_row("Final", paste0(count_metrics$final, "%"), "#da191e")
      ),
      "#006495",
      "PO"
    ),
    card(
      "PO Integration Value Wise",
      c(
        metric_row("Integration", paste0(value_metrics$integration, "%"), "#006495"),
        metric_row("CBB", paste0(value_metrics$cbb, "%"), "#16a34a"),
        metric_row("Final", paste0(value_metrics$final, "%"), "#da191e")
      ),
      "#16a34a",
      "Rs"
    ),
    card(
      "GRN Integration",
      c(
        metric_row("GRN Records", fmt_n(grn_metrics$grn_records), "#16a34a"),
        metric_row("Eligible PO Items", fmt_n(grn_metrics$po_line_items), "#006495"),
        metric_row("Integration %", paste0(grn_metrics$grn_pct, "%"), "#f97316")
      ),
      "#f97316",
      "GR"
    ),
    "</tr></table>",
    "<div style='margin:18px 8px 4px;padding:14px 16px;border-radius:14px;background:#ffffff;border:1px solid #e2e8f0;'>",
    "<div style='font-size:12px;font-weight:800;color:#0e1d28;text-transform:uppercase;letter-spacing:.08em;margin-bottom:6px;'>Attached PDFs</div>",
    "<ul style='margin:0;padding-left:18px;'>", file_list, "</ul>",
    "</div>",
    "</div>",
    "<div style='font-size:11px;color:#64748b;text-align:center;margin-top:14px;'>Generated automatically by ProcureGraph</div>",
    "</div></body></html>"
  )
}

date_ranges <- function(today = Sys.Date()) {
  first_this_month <- as.Date(format(today, "%Y-%m-01"))
  first_last_month <- first_this_month %m-% months(1)
  last_last_month <- first_this_month - 1
  list(
    list(key = "MTD", label = "MTD", start = first_this_month, end = today),
    list(key = "LAST_MONTH", label = "Last Month", start = first_last_month, end = last_last_month),
    list(key = "LAST_3_MONTHS", label = "Last 3 Months", start = today - 89, end = today)
  )
}

main <- function() {
  load_project_env()
  mongo_conn <- env_value("PROCUREGRAPH_MONGO_CONN")
  mapping_file <- env_value("PROCUREGRAPH_MAPPING_FILE")
  output_root <- env_value("PROCUREGRAPH_REPORT_DIR", required = FALSE, default = file.path(getwd(), "reports", "daily"))

  mapping <- load_mapping(mapping_file)
  ranges <- date_ranges(Sys.Date())
  run_dir <- file.path(output_root, format(Sys.Date(), "%Y-%m-%d"))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  pdfs <- character()
  mtd_df <- NULL
  mtd_grn <- NULL
  for (rng in ranges) {
    message("Generating ", rng$label, " report: ", rng$start, " to ", rng$end)
    raw <- get_mongo_items(rng$start, rng$end, mongo_conn)
    grn <- get_mongo_grn(rng$start, rng$end, mongo_conn)
    df <- process_items(raw, mapping, rng$start, rng$end)
    out_file <- file.path(run_dir, paste0("ProcureGraph_", rng$key, "_", format(Sys.Date(), "%Y%m%d"), ".pdf"))
    make_report_pdf(rng$label, rng$start, rng$end, df, grn, mapping, out_file)
    pdfs <- c(pdfs, out_file)
    if (identical(rng$key, "MTD")) {
      mtd_df <- df
      mtd_grn <- grn
    }
  }

  email_body <- build_email_summary_html(mtd_df, mtd_grn, mapping, pdfs)
  message("Sending email with ", length(pdfs), " PDF attachment(s).")
  email_sent <- send_email_with_attachments(
    pdfs,
    sprintf("ProcureGraph Daily Reports - %s", format(Sys.Date(), "%d %b %Y")),
    email_body,
    html = TRUE
  )
  if (isTRUE(email_sent)) {
    message("Email sent successfully.")
  }
  message("Done. Reports saved to: ", normalizePath(run_dir, winslash = "\\", mustWork = FALSE))
}

tryCatch(main(), error = function(e) {
  message("ProcureGraph daily report failed: ", e$message)
  quit(status = 1)
})
