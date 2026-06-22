################################################################################
# PROCUREGRAPH v6.1 — CLOUD-COMPATIBLE
# Changes vs v6.0:
#   • Data source: MongoDB ↔ Excel upload toggle (mod_data_source.R)
#   • AI: Ollama (local) + Groq API (cloud free tier) dual-mode
#   • Dashboard save/load: auto-detects local filesystem vs cloud memory,
#     Export/Import JSON bundle for cross-session persistence
#   • All original v5.1 / v6.0 functionality fully retained
################################################################################

# ═══════════════════════════════════════════════════════════════════════════════
# 1. LIBRARIES
# ═══════════════════════════════════════════════════════════════════════════════
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)
library(mongolite)
library(readxl)
library(lubridate)
library(purrr)
library(htmltools)
library(memoise)
library(cachem)
library(stringr)
library(curl)
library(plotly)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(openxlsx)

# ═══════════════════════════════════════════════════════════════════════════════
# 2. SOURCE MODULES
# ═══════════════════════════════════════════════════════════════════════════════
source("mod_global_filters.R")
source("mod_custom_analytics.R")
source("mod_ai_insights.R")
source("mod_dashboard_save.R")
source("mod_data_source.R")
source("mod_removed_data.R")

# ═══════════════════════════════════════════════════════════════════════════════
# 3. ENVIRONMENT CONFIG
# ═══════════════════════════════════════════════════════════════════════════════
load_project_env <- function() {
  candidates <- c(".Renviron", ".env")
  for (path in candidates) {
    if (file.exists(path)) {
      readRenviron(path)
    }
  }
}

load_project_env()

get_env_config <- function(name, required = TRUE) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (required && !nzchar(value)) {
    stop(sprintf("Missing required environment variable: %s", name), call. = FALSE)
  }
  value
}

get_mongo_conn <- function() {
  get_env_config("PROCUREGRAPH_MONGO_CONN")
}

get_license_config <- function() {
  tier <- tolower(get_env_config("PROCUREGRAPH_LICENSE_TIER", required = FALSE))
  if (!nzchar(tier)) tier <- "trial"
  expires_raw <- get_env_config("PROCUREGRAPH_LICENSE_EXPIRES", required = FALSE)
  expires <- suppressWarnings(as.Date(expires_raw))
  if (!nzchar(expires_raw)) expires <- NA
  list(
    tier = tier,
    key = get_env_config("PROCUREGRAPH_LICENSE_KEY", required = FALSE),
    customer = get_env_config("PROCUREGRAPH_CUSTOMER_NAME", required = FALSE) %||% "Trial customer",
    contact = get_env_config("PROCUREGRAPH_SALES_CONTACT", required = FALSE) %||% "sales@procuregraph.app",
    monthly_price = get_env_config("PROCUREGRAPH_PRO_PRICE", required = FALSE) %||% "299",
    expires = expires
  )
}

license_is_active <- function(license) {
  paid_tier <- license$tier %in% c("pro", "enterprise")
  not_expired <- is.na(license$expires) || license$expires >= Sys.Date()
  paid_tier && nzchar(license$key %||% "") && not_expired
}

license_status_label <- function(license) {
  if (license_is_active(license)) {
    paste0(toupper(license$tier), " active")
  } else if (!is.na(license$expires) && license$expires < Sys.Date()) {
    "License expired"
  } else {
    "License inactive"
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# 4. MONGO FETCH FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════
get_mongo_data <- function(start_date, end_date) {
  col <- mongo(collection = "item", db = "procurement", url = get_mongo_conn())
  t0  <- as.numeric(as.POSIXct(start_date))      * 1000
  t1  <- as.numeric(as.POSIXct(end_date + 1))    * 1000
  
  pipeline <- sprintf('[
    { "$match": { "creationDate": { "$gte": %s, "$lte": %s } } },
    { "$project": {
        "_id":                    1,
        "buyerId":                "$buyer.id",
        "status":                 1,
        "creationDate":           1,
        "source":                 1,
        "stage":                  1,
        "histPoCustomerPoNo":     "$history.po.customerPoNo",
        "histPoCustomerSANumber": "$history.po.customerSANumber",
        "amount":1
    }}
  ]', t0, t1)
  
  data <- tryCatch(
    col$aggregate(pipeline),
    error = function(e) {
      showNotification(paste("MongoDB:", e$message), type = "error")
      NULL
    }
  )
  col$disconnect()
  data
}

get_grn_data <- function(start_date, end_date) {
  col  <- mongo(collection = "grn", db = "procurement", url = get_mongo_conn())
  t0   <- as.numeric(as.POSIXct(start_date))      * 1000
  t1   <- as.numeric(as.POSIXct(end_date + 1))    * 1000
  q    <- sprintf('{"creationDate":{"$gte":%s,"$lte":%s}}', t0, t1)
  proj <- '{"_id":1,"buyerId":1,"creationDate":1,"customerGrnNo":1,"poId":1,"itemId":1}'
  data <- tryCatch(
    col$find(query = q, fields = proj),
    error = function(e) { message("GRN: ", e$message); data.frame() }
  )
  col$disconnect()
  data
}

# ═══════════════════════════════════════════════════════════════════════════════
# 5. HELPERS
# ═══════════════════════════════════════════════════════════════════════════════
safe_rename <- function(df, new_name, possible) {
  m <- intersect(possible, colnames(df))
  if (!length(m)) { df[[new_name]] <- NA; return(df) }
  df %>% rename(!!new_name := !!m[1])
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

create_pivot <- function(data) {
  pv <- data %>%
    filter(`Remove(Y/N)` == "Consider",
           `Final Status` %in% c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout")) %>%
    group_by(`Final Status`, source) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = source, values_from = count, values_fill = 0)
  for (col in c("EOC", "Manual", "SAP"))
    if (!col %in% colnames(pv)) pv[[col]] <- 0
  pv <- pv %>%
    mutate(
      `Grand Total` = EOC + Manual + SAP,
      `Final Status` = factor(`Final Status`,
                              levels = c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout"))
    ) %>%
    arrange(`Final Status`)
  tr <- pv %>% summarise(
    `Final Status` = "Grand Total",
    EOC = sum(EOC), Manual = sum(Manual),
    SAP = sum(SAP), `Grand Total` = sum(`Grand Total`)
  )
  bind_rows(pv, tr)
}

create_value_pivot <- function(data) {
  if (!"Amount_Crore" %in% names(data)) data$Amount_Crore <- 0
  pv <- data %>%
    filter(`Remove(Y/N)` == "Consider",
           `Final Status` %in% c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout")) %>%
    group_by(`Final Status`, source) %>%
    summarise(value_cr = sum(Amount_Crore, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = source, values_from = value_cr, values_fill = 0)
  for (col in c("EOC", "Manual", "SAP"))
    if (!col %in% colnames(pv)) pv[[col]] <- 0
  pv <- pv %>%
    mutate(
      `Grand Total` = EOC + Manual + SAP,
      `Final Status` = factor(`Final Status`,
                              levels = c("Live", "Live - CBB", "No Integration", "PR Int. - Punchout"))
    ) %>%
    arrange(`Final Status`) %>%
    mutate(across(c(EOC, Manual, SAP, `Grand Total`), ~round(.x, 2)))
  tr <- pv %>% summarise(
    `Final Status` = "Grand Total",
    EOC = round(sum(EOC), 2), Manual = round(sum(Manual), 2),
    SAP = round(sum(SAP), 2), `Grand Total` = round(sum(`Grand Total`), 2)
  )
  bind_rows(pv, tr)
}

calc_metrics <- function(pv) {
  pc    <- pv %>% filter(`Final Status` != "Grand Total")
  total <- sum(pc$`Grand Total`, na.rm = TRUE)
  if (total == 0) return(list(integration = "0%", cbb = "0%", final = "0%"))
  sap <- sum(pc$SAP,    na.rm = TRUE)
  man <- sum(pc$Manual, na.rm = TRUE)
  list(
    integration = paste0(round(sap / total * 100, 1), "%"),
    cbb         = paste0(round(man / total * 100, 1), "%"),
    final       = paste0(round((sap + man) / total * 100, 1), "%")
  )
}

is_integrated_line_item <- function(source_value) {
  source_value %in% c("SAP", "Manual")
}

write_mom_company_workbook <- function(tbl, file) {
  wb <- createWorkbook()
  addWorksheet(wb, "MoM by Company", gridLines = FALSE)

  if (is.null(tbl) || !nrow(tbl)) {
    writeData(wb, "MoM by Company", data.frame(Message = "No data available"))
    saveWorkbook(wb, file, overwrite = TRUE)
    return(invisible(NULL))
  }

  col_names <- names(tbl)
  month_pattern <- "^(SAP|Manual|EOC|Total|Integration_Pct)_(\\d{4})-(\\d{2})$"
  display_names <- vapply(col_names, function(name) {
    match <- str_match(name, month_pattern)
    if (!is.na(match[1, 1])) {
      metric <- dplyr::recode(
        match[1, 2],
        SAP = "SAP",
        Manual = "Manual",
        EOC = "EOC",
        Total = "Total",
        Integration_Pct = "Integration %"
      )
      month_label <- format(as.Date(sprintf("%s-%s-01", match[1, 3], match[1, 4])), "%b %Y")
      return(paste(month_label, metric))
    }

    dplyr::recode(
      name,
      "PID-CID" = "Company ID",
      "Client Name" = "Company Name",
      "SAP_Total" = "Overall SAP",
      "Manual_Total" = "Overall Manual",
      "EOC_Total" = "Overall EOC",
      "Total_Total" = "Overall Total",
      "Integration_Pct_Total" = "Overall Integration %",
      .default = name
    )
  }, character(1))

  export_tbl <- tbl
  names(export_tbl) <- make.unique(display_names, sep = " ")

  pct_cols <- grep("Integration %", names(export_tbl), fixed = TRUE)
  numeric_cols <- grep("SAP|Manual|EOC|Total", names(export_tbl))
  numeric_cols <- setdiff(numeric_cols, c(1, 2, pct_cols))

  if (length(pct_cols)) {
    export_tbl[pct_cols] <- lapply(export_tbl[pct_cols], function(x) suppressWarnings(as.numeric(x) / 100))
  }

  writeDataTable(wb, "MoM by Company", export_tbl, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium2")

  header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#006495", halign = "center", valign = "center", textDecoration = "bold", border = "Bottom", borderColour = "#D8E4EF")
  text_style <- createStyle(fontColour = "#0E1D28", halign = "left", valign = "center", border = "Bottom", borderColour = "#EEF2F7")
  number_style <- createStyle(fontColour = "#0E1D28", halign = "right", valign = "center", border = "Bottom", borderColour = "#EEF2F7", numFmt = "#,##0")
  percent_style <- createStyle(fontColour = "#0E1D28", halign = "center", valign = "center", border = "Bottom", borderColour = "#EEF2F7", numFmt = "0.0%")
  total_header_style <- createStyle(fontColour = "#0E1D28", fgFill = "#E0F2FE", halign = "center", valign = "center", textDecoration = "bold", border = "Bottom", borderColour = "#BAE6FD")

  addStyle(wb, "MoM by Company", header_style, rows = 1, cols = 1:ncol(export_tbl), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "MoM by Company", text_style, rows = 2:(nrow(export_tbl) + 1), cols = 1:2, gridExpand = TRUE, stack = TRUE)

  if (length(numeric_cols)) {
    addStyle(wb, "MoM by Company", number_style, rows = 2:(nrow(export_tbl) + 1), cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
  }
  if (length(pct_cols)) {
    addStyle(wb, "MoM by Company", percent_style, rows = 2:(nrow(export_tbl) + 1), cols = pct_cols, gridExpand = TRUE, stack = TRUE)
    tryCatch(
      conditionalFormatting(
        wb, "MoM by Company", cols = pct_cols, rows = 2:(nrow(export_tbl) + 1),
        style = c("#FECACA", "#FED7AA", "#D1FAE5"), type = "colourScale", rule = c(0, 0.70, 0.85)
      ),
      error = function(e) message("Skipping MoM Excel colour scale: ", e$message)
    )
  }

  overall_cols <- grep("^Overall ", names(export_tbl))
  if (length(overall_cols)) {
    addStyle(wb, "MoM by Company", total_header_style, rows = 1, cols = overall_cols, gridExpand = TRUE, stack = TRUE)
  }

  freezePane(wb, "MoM by Company", firstActiveRow = 2, firstActiveCol = 3)
  setColWidths(wb, "MoM by Company", cols = 1, widths = 14)
  setColWidths(wb, "MoM by Company", cols = 2, widths = 30)
  if (ncol(export_tbl) > 2) setColWidths(wb, "MoM by Company", cols = 3:ncol(export_tbl), widths = 16)

  saveWorkbook(wb, file, overwrite = TRUE)
}

html_bar <- function(label, value, max_val, count_label, color, rank = NULL) {
  pct <- if (max_val > 0) round(value / max_val * 100, 1) else 0
  rank_badge <- if (!is.null(rank)) {
    badge_cols <- c("#f59e0b", "#94a3b8", "#b45309", "#006495", "#0284c7")
    col <- badge_cols[min(rank, 5)]
    sprintf(
      '<span style="min-width:22px;height:22px;border-radius:50%%;background:%s;color:white;font-size:10px;font-weight:800;display:inline-flex;align-items:center;justify-content:center;">%d</span>',
      col, rank
    )
  } else ""
  sprintf('
<div style="margin-bottom:14px;">
  <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;">
    <div style="display:flex;align-items:center;gap:8px;">
      %s
      <span style="font-size:12px;font-weight:700;color:#0e1d28;max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="%s">%s</span>
    </div>
    <span style="font-size:11px;font-weight:700;color:%s;white-space:nowrap;">%s</span>
  </div>
  <div style="height:10px;background:#f1f5f9;border-radius:6px;overflow:hidden;">
    <div style="height:100%%;width:%s%%;background:%s;border-radius:6px;transition:width .6s ease;"></div>
  </div>
</div>', rank_badge, label, label, color, count_label, pct, color)
}

BTN    <- function(bg, ...) paste0("background:", bg, ";color:white;border:none;font-weight:700;font-size:10px;padding:8px 14px;border-radius:8px;cursor:pointer;letter-spacing:.05em;text-transform:uppercase;", ...)
CARD   <- "bg-white rounded-2xl shadow-sm"
BORDER <- "border:1px solid #f1f5f9;"
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nchar(trimws(as.character(a[1]))) > 0) a else b

# ═══════════════════════════════════════════════════════════════════════════════
# 6. UI
# ═══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&family=Manrope:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;600&display=swap", rel = "stylesheet"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:wght,FILL@100..700,0..1&display=swap", rel = "stylesheet"),
    tags$script(src = "https://cdn.tailwindcss.com?plugins=forms,container-queries"),
    tags$script(HTML('tailwind.config={darkMode:"class",theme:{extend:{colors:{"primary":"#da191e","secondary":"#006495"},fontFamily:{"headline":["Plus Jakarta Sans","sans-serif"],"body":["Manrope","sans-serif"]}}}}')),
    tags$style(HTML("
      *{box-sizing:border-box}
      body{font-family:'Manrope',sans-serif;background:#f6faff;color:#0e1d28;margin:0}
      h1,h2,h3{font-family:'Plus Jakarta Sans',sans-serif}
      .material-symbols-outlined{font-variation-settings:'FILL' 0,'wght' 400,'GRAD' 0,'opsz' 24;vertical-align:middle}
      .sidebar-section .form-group{margin-bottom:0!important}
      .sidebar-section label{display:none!important}
      .sidebar-section .form-control,.sidebar-section input[type=date]{background:#263544!important;border:1px solid #374d60!important;color:#cbd5e1!important;border-radius:8px!important;font-size:12px!important;padding:8px 10px!important;width:100%!important}
      .sidebar-section input[type=text].form-control{background:#263544!important;border:1px solid #374d60!important;color:#cbd5e1!important;border-radius:8px!important;font-size:12px!important;padding:8px 10px!important}
      #refresh_btn{background:#006495!important;color:white!important;border:none!important;font-weight:700!important;font-size:12px!important;width:100%!important;padding:12px!important;border-radius:8px!important;letter-spacing:.05em!important;text-transform:uppercase!important;cursor:pointer!important;margin-top:8px!important}
      #refresh_btn:hover{background:#00527b!important}
      .dataTables_wrapper{font-family:'Manrope',sans-serif;font-size:12px}
      .dataTables_wrapper .dataTable thead th{background:#f8fafb!important;color:#64748b!important;font-size:10px!important;text-transform:uppercase!important;letter-spacing:.06em!important;font-weight:700!important;border-bottom:2px solid #e2e8f0!important;padding:12px 16px!important}
      .dataTables_wrapper .dataTable tbody td{padding:10px 16px!important;border-bottom:1px solid #f1f5f9!important}
      .dataTables_wrapper .dataTable tbody tr:hover td{background:#f8faff!important}
      .dataTables_filter input,.dataTables_length select{border:1px solid #e2e8f0!important;border-radius:6px!important;padding:4px 10px!important;font-size:12px!important}
      @keyframes fadeSlideIn{from{opacity:0;transform:translateY(12px)}to{opacity:1;transform:translateY(0)}}
      .anim-in{animation:fadeSlideIn .4s ease both}
      .anim-in-1{animation-delay:.05s}.anim-in-2{animation-delay:.1s}.anim-in-3{animation-delay:.15s}.anim-in-4{animation-delay:.2s}.anim-in-5{animation-delay:.25s}
      ::-webkit-scrollbar{width:5px;height:5px}::-webkit-scrollbar-track{background:#f1f5f9}::-webkit-scrollbar-thumb{background:#94a3b8;border-radius:3px}
      html{scroll-behavior:smooth}
      .btn-ai{background:#6f42c1!important;color:white!important;border:none!important;border-radius:8px!important;padding:10px 20px!important;font-weight:700!important;cursor:pointer!important}
      .btn-ai:hover{background:#5a379b!important}
      #ds_item_upload_area .form-group,#ds_grn_upload_area .form-group{margin-bottom:4px!important}
      #ds_item_upload_area label,#ds_grn_upload_area label{display:none!important}
      #mobile_menu_btn,.mobile-sidebar-backdrop{display:none}
      .login-page{
        min-height:100vh;
        position:relative;
        overflow:hidden;
        display:flex;
        align-items:center;
        justify-content:center;
        padding:32px;
        background:
          radial-gradient(circle at 13% 18%, rgba(20,184,166,.2), transparent 28%),
          radial-gradient(circle at 84% 12%, rgba(245,158,11,.14), transparent 28%),
          linear-gradient(135deg, #06131f 0%, #0a1d2b 44%, #101820 100%);
        color:#e5f0f6;
      }
      .login-page:before{
        content:'';
        position:absolute;
        inset:0;
        background-image:
          linear-gradient(rgba(148,163,184,.08) 1px, transparent 1px),
          linear-gradient(90deg, rgba(148,163,184,.08) 1px, transparent 1px);
        background-size:56px 56px;
        mask-image:linear-gradient(to bottom, rgba(0,0,0,.8), rgba(0,0,0,.18));
        pointer-events:none;
      }
      .login-shell{
        position:relative;
        z-index:1;
        width:min(1120px,100%);
        min-height:680px;
        display:grid;
        grid-template-columns:minmax(0,1.12fr) minmax(380px,.88fr);
        border:1px solid rgba(148,163,184,.22);
        border-radius:28px;
        overflow:hidden;
        background:rgba(8,22,34,.66);
        box-shadow:0 34px 90px rgba(0,0,0,.42);
        backdrop-filter:blur(18px);
      }
      .login-visual{
        position:relative;
        padding:42px;
        overflow:hidden;
        background:
          linear-gradient(145deg, rgba(15,30,43,.84), rgba(8,22,34,.62)),
          radial-gradient(circle at 28% 20%, rgba(20,184,166,.26), transparent 36%);
      }
      .login-brand{display:flex;align-items:center;gap:14px}
      .login-mark{
        width:50px;height:50px;border-radius:14px;
        display:flex;align-items:center;justify-content:center;
        background:linear-gradient(135deg,#14b8a6,#006495 72%);
        box-shadow:0 14px 32px rgba(20,184,166,.28);
      }
      .login-brand-title{margin:0;color:#f8fafc;font-size:21px;font-weight:800;letter-spacing:0}
      .login-brand-sub{margin:3px 0 0;color:#91a9b8;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:.14em}
      .login-copy{max-width:500px;margin-top:92px}
      .login-copy h1{margin:0;color:white;font-size:50px;line-height:1.02;font-weight:800;letter-spacing:0}
      .login-copy p{margin:18px 0 0;color:#adc0ca;font-size:16px;line-height:1.7;max-width:440px}
      .login-network{
        position:absolute;
        left:42px;right:42px;bottom:38px;
        height:236px;
        border:1px solid rgba(148,163,184,.2);
        border-radius:22px;
        background:rgba(7,18,28,.58);
        box-shadow:inset 0 1px 0 rgba(255,255,255,.06);
        overflow:hidden;
      }
      .login-network:before,.login-network:after{
        content:'';
        position:absolute;
        inset:28px;
        border:1px solid rgba(20,184,166,.22);
        border-radius:18px;
        transform:skewX(-8deg);
      }
      .login-network:after{inset:70px 64px;border-color:rgba(245,158,11,.24);transform:skewX(8deg)}
      .network-line{
        position:absolute;height:2px;background:linear-gradient(90deg, transparent, rgba(20,184,166,.78), transparent);
        transform-origin:left center;
      }
      .network-line.l1{width:330px;left:88px;top:82px;transform:rotate(16deg)}
      .network-line.l2{width:270px;left:190px;top:152px;transform:rotate(-14deg)}
      .network-line.l3{width:210px;left:86px;top:158px;transform:rotate(-30deg);background:linear-gradient(90deg, transparent, rgba(245,158,11,.7), transparent)}
      .network-node{
        position:absolute;width:18px;height:18px;border-radius:50%;background:#14b8a6;
        box-shadow:0 0 0 8px rgba(20,184,166,.1),0 0 28px rgba(20,184,166,.55);
      }
      .network-node.n1{left:74px;top:72px}.network-node.n2{left:258px;top:120px;background:#f59e0b;box-shadow:0 0 0 8px rgba(245,158,11,.12),0 0 28px rgba(245,158,11,.48)}
      .network-node.n3{right:92px;top:58px}.network-node.n4{left:142px;bottom:52px}.network-node.n5{right:150px;bottom:50px;background:#f59e0b;box-shadow:0 0 0 8px rgba(245,158,11,.12),0 0 28px rgba(245,158,11,.48)}
      .signal-card{
        position:absolute;
        min-width:138px;
        padding:12px 14px;
        border:1px solid rgba(148,163,184,.24);
        border-radius:14px;
        background:rgba(15,30,43,.82);
        box-shadow:0 18px 36px rgba(0,0,0,.18);
      }
      .signal-card strong{display:block;color:#f8fafc;font-size:18px;line-height:1;font-weight:800}
      .signal-card span{display:block;margin-top:6px;color:#91a9b8;font-size:10px;text-transform:uppercase;letter-spacing:.12em;font-weight:700}
      .signal-card.s1{right:34px;top:26px}.signal-card.s2{left:34px;bottom:24px}
      .login-panel{
        display:flex;
        align-items:center;
        justify-content:center;
        padding:42px;
        background:linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
        color:#0f172a;
      }
      .login-card{width:100%;max-width:420px}
      .login-card h2{margin:0;color:#0f172a;font-size:34px;line-height:1.1;font-weight:800;letter-spacing:0}
      .login-card .login-subtitle{margin:12px 0 30px;color:#64748b;font-size:15px;line-height:1.6}
      .login-card .form-group{margin-bottom:16px!important}
      .login-card label{display:block!important;color:#334155!important;font-size:12px!important;font-weight:800!important;letter-spacing:.08em!important;text-transform:uppercase!important;margin-bottom:8px!important}
      .login-card .form-control{
        height:50px!important;
        border:1px solid #d9e3ec!important;
        border-radius:12px!important;
        background:#f8fafc!important;
        color:#0f172a!important;
        font-size:14px!important;
        font-weight:600!important;
        padding:12px 14px!important;
        box-shadow:none!important;
        transition:border-color .18s ease, box-shadow .18s ease, background .18s ease!important;
      }
      .login-card .form-control:focus{
        background:white!important;
        border-color:#14b8a6!important;
        box-shadow:0 0 0 4px rgba(20,184,166,.14)!important;
      }
      #login_btn{
        width:100%!important;
        min-height:52px!important;
        margin-top:4px!important;
        border:none!important;
        border-radius:12px!important;
        background:linear-gradient(135deg,#006495,#14b8a6)!important;
        color:white!important;
        font-size:14px!important;
        font-weight:800!important;
        letter-spacing:.04em!important;
        text-transform:uppercase!important;
        box-shadow:0 18px 34px rgba(0,100,149,.24)!important;
        transition:transform .18s ease, box-shadow .18s ease, filter .18s ease!important;
      }
      #login_btn:hover{transform:translateY(-1px);filter:saturate(1.08);box-shadow:0 22px 42px rgba(0,100,149,.32)!important}
      #login_btn:active{transform:translateY(0)}
      .login-security{
        display:flex;
        align-items:center;
        justify-content:center;
        gap:8px;
        margin-top:22px;
        color:#64748b;
        font-size:12px;
        font-weight:700;
      }
      .login-security .material-symbols-outlined{font-size:18px;color:#0f9f8f;font-variation-settings:'FILL' 1,'wght' 500,'GRAD' 0,'opsz' 20}
      @media (max-width: 768px){
        body{overflow-x:hidden}
        body.mobile-nav-open{overflow:hidden}
        .login-page{padding:16px!important;align-items:flex-start!important;padding-top:24px!important}
        .login-shell{grid-template-columns:1fr!important;min-height:auto!important;border-radius:22px!important}
        .login-visual{padding:26px!important;min-height:260px!important}
        .login-copy{margin-top:38px!important}
        .login-copy h1{font-size:34px!important}
        .login-copy p{font-size:14px!important;line-height:1.6!important}
        .login-network{display:none!important}
        .login-panel{padding:26px!important}
        .login-card{width:100%!important;max-width:420px!important}
        .login-card h2{font-size:28px!important}
        .login-card .form-group{margin-bottom:12px!important}
        .login-card input{font-size:16px!important;padding:12px!important}
        #login_btn{font-size:13px!important;padding:13px!important;border-radius:10px!important}
        .app-sidebar{width:min(88vw,340px)!important;max-width:340px!important;transform:translateX(-105%);transition:transform .25s ease;box-shadow:18px 0 45px rgba(15,23,42,.22);padding-top:18px!important}
        body.mobile-nav-open .app-sidebar{transform:translateX(0)}
        .mobile-sidebar-backdrop{display:block;position:fixed;inset:0;background:rgba(15,23,42,.48);z-index:45;opacity:0;pointer-events:none;transition:opacity .2s ease}
        body.mobile-nav-open .mobile-sidebar-backdrop{opacity:1;pointer-events:auto}
        .app-main{margin-left:0!important;width:100%!important;min-width:0!important}
        .app-header{height:auto!important;min-height:60px!important;padding:10px 12px!important;gap:10px!important;align-items:flex-start!important}
        .app-header>div:first-child{width:100%;gap:10px!important;flex-wrap:wrap}
        .app-header>div:first-child>span{font-size:17px!important;line-height:1.2}
        .app-header>div:first-child>div{width:100%;display:grid!important;grid-template-columns:1fr!important;gap:8px!important}
        .app-header input#global_search{width:100%!important;font-size:13px!important}
        .app-header select#integration_filter{width:100%!important;font-size:13px!important}
        .app-header>div:last-child{position:absolute;right:10px;top:9px;gap:6px!important}
        .app-header>div:last-child .w-px,.app-header>div:last-child #open_email_modal,.app-header>div:last-child #open_email_modal_poc,.app-header>div:last-child .w-9{display:none!important}
        #mobile_menu_btn{display:inline-flex!important;align-items:center;justify-content:center;background:#0e1d28!important;color:white!important;border:none!important;border-radius:10px!important;padding:8px 10px!important;min-width:40px!important}
        .main-content>div{padding:16px!important}
        .main-content section.grid,.main-content .grid.grid-cols-2,.main-content .grid.grid-cols-3,.main-content .grid.grid-cols-4{grid-template-columns:1fr!important}
        .main-content .flex.justify-between{gap:12px;flex-wrap:wrap}
        .main-content h2{font-size:20px!important;line-height:1.25}
        .main-content h3{font-size:16px!important;line-height:1.3}
        .main-content .text-4xl{font-size:28px!important;line-height:1.15}
        .dataTables_wrapper{font-size:11px!important;overflow-x:auto!important}
        .dataTables_wrapper .dataTables_filter,.dataTables_wrapper .dataTables_length{float:none!important;text-align:left!important;margin:8px 10px!important}
        .dataTables_wrapper .dataTables_filter input{width:100%!important;max-width:100%!important;margin-left:0!important;margin-top:6px!important}
        table.dataTable{width:100%!important}
        .sidebar-section{padding-bottom:24px}
        .sidebar-section .grid.grid-cols-2{grid-template-columns:repeat(2,minmax(0,1fr))!important}
      }
    "))
  ),
  tags$div(style = "display:none;", fileInput("company_file", label = NULL, accept = ".xlsx")),
  tags$script(HTML("
    function triggerFileUpload(){
      var w=document.getElementById('company_file');if(!w)return;
      var r=w.querySelector('input[type=file]');if(r)r.click();else w.click();
    }
    document.addEventListener('keydown',function(e){
      if(e.key!=='Enter')return;
      if(e.target&&['login_username','login_password'].indexOf(e.target.id)>-1){
        var b=document.getElementById('login_btn');if(b)b.click();
      }
    });
    function highlightNav(id){
      ['nav-dashboard','nav-po','nav-grn','nav-grn2','nav-removed','nav-reports','nav-analytics'].forEach(function(n){
        var el=document.getElementById(n);if(!el)return;
        if(n===id){el.style.background='#da191e';el.style.color='white';}
        else{el.style.background='';el.style.color='#64748b';}
      });
      document.body.classList.remove('mobile-nav-open');
    }
    function toggleMobileNav(){
      document.body.classList.toggle('mobile-nav-open');
    }
    function closeMobileNav(){
      document.body.classList.remove('mobile-nav-open');
    }
  ")),
  uiOutput("main_ui")
)

# ═══════════════════════════════════════════════════════════════════════════════
# 7. SERVER
# ═══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # ── Reactive state ─────────────────────────────────────────────────────────
  CACHE_SEC    <- 5 * 60
  refresh_time <- reactiveVal(NULL)
  current_page <- reactiveVal("dashboard")
  cached_data  <- reactiveVal(NULL)
  cache_ts     <- reactiveVal(NULL)
  
  authenticated <- reactiveVal(FALSE)
  VALID_USER    <- get_env_config("PROCUREGRAPH_APP_USER", required = FALSE)
  VALID_PASS    <- get_env_config("PROCUREGRAPH_APP_PASS", required = FALSE)
  license       <- get_license_config()
  pro_enabled   <- reactive(license_is_active(license))
  
  observeEvent(input$login_btn, {
    if (!nzchar(VALID_USER) || !nzchar(VALID_PASS)) {
      showNotification(
        "App credentials are not configured. Set PROCUREGRAPH_APP_USER and PROCUREGRAPH_APP_PASS.",
        type = "error",
        duration = 8
      )
    } else if (identical(input$login_username, VALID_USER) && identical(input$login_password, VALID_PASS)) {
      authenticated(TRUE)
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  observeEvent(input$logout, {
    authenticated(FALSE)
    cached_data(NULL)
    cache_ts(NULL)
  })
  
  output$main_ui <- renderUI({
    if (!authenticated()) {
      # ── LOGIN PAGE ──────────────────────────────────────────────────────────
      tags$div(class = "login-page",
               tags$div(class = "login-shell anim-in",
                        tags$section(class = "login-visual",
                                     tags$div(class = "login-brand",
                                              tags$div(class = "login-mark",
                                                       tags$span(class = "material-symbols-outlined text-white", style = "font-size:28px;font-variation-settings:'FILL' 1,'wght' 500,'GRAD' 0,'opsz' 28;", "account_tree")),
                                              tags$div(
                                                tags$p(class = "login-brand-title", "ProcureGraph"),
                                                tags$p(class = "login-brand-sub", "Procurement Intelligence")
                                              )),
                                     tags$div(class = "login-copy",
                                              tags$h1("Command your procurement data with clarity."),
                                              tags$p("Track PO integration, GRN signals, buyer performance, and exception patterns from one secure analytics workspace.")),
                                     tags$div(class = "login-network",
                                              tags$div(class = "network-line l1"),
                                              tags$div(class = "network-line l2"),
                                              tags$div(class = "network-line l3"),
                                              tags$div(class = "network-node n1"),
                                              tags$div(class = "network-node n2"),
                                              tags$div(class = "network-node n3"),
                                              tags$div(class = "network-node n4"),
                                              tags$div(class = "network-node n5"),
                                              tags$div(class = "signal-card s1",
                                                       tags$strong("Live"),
                                                       tags$span("Integration pulse")),
                                              tags$div(class = "signal-card s2",
                                                       tags$strong("GRN"),
                                                       tags$span("Exception radar")))),
                        tags$section(class = "login-panel",
                                     tags$div(class = "login-card",
                                              tags$h2("Welcome back"),
                                              tags$p(class = "login-subtitle", "Sign in to your procurement command center"),
                                              textInput("login_username", "Username", placeholder = "Username", width = "100%"),
                                              passwordInput("login_password", "Password", placeholder = "Password", width = "100%"),
                                              actionButton("login_btn", "Login"),
                                              tags$p(class = "login-security",
                                                     tags$span(class = "material-symbols-outlined", "verified_user"),
                                                     tags$span("Secure access"))))))
    } else {
      # ── DASHBOARD SHELL ────────────────────────────────────────────────────
      tags$div(class = "dashboard-shell flex min-h-screen overflow-x-hidden", style = "width:100%;",
               tags$div(class = "mobile-sidebar-backdrop", onclick = "closeMobileNav()"),
               
               # SIDEBAR
               tags$aside(class = "app-sidebar w-64 fixed left-0 top-0 h-screen flex flex-col py-6 z-50", style = "background:#1c2b36;",
                          tags$div(class = "px-6 mb-8",
                                   tags$div(class = "flex items-center gap-3",
                                            tags$div(class = "w-10 h-10 flex items-center justify-center rounded", style = "background:#da191e;",
                                                     tags$span(class = "material-symbols-outlined text-white text-xl", style = "font-variation-settings:'FILL' 1;", "architecture")),
                                            tags$div(
                                              tags$p(class = "text-white font-bold text-xs uppercase tracking-widest leading-none", "ProcureGraph"),
                                              tags$p(class = "text-xs mt-1 font-medium", style = "color:#64748b;letter-spacing:.1em;", "Buyers Intel")
                                            ))),
                          tags$nav(class = "px-3 space-y-1 mb-6",
                                   tags$a(id = "nav-dashboard", class = "flex items-center gap-3 px-4 py-3 rounded-lg font-semibold text-sm text-white",
                                          style = "background:#da191e;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','dashboard',{priority:'event'});highlightNav('nav-dashboard');",
                                          tags$span(class = "material-symbols-outlined text-lg", "dashboard"), "Dashboard"),
                                   tags$a(id = "nav-po", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','po',{priority:'event'});highlightNav('nav-po');",
                                          tags$span(class = "material-symbols-outlined text-lg", "pivot_table_chart"), "PO Integration"),
                                   tags$a(id = "nav-grn", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','grn',{priority:'event'});highlightNav('nav-grn');",
                                          tags$span(class = "material-symbols-outlined text-lg", "receipt_long"), "GRN Analysis"),
                                   tags$a(id = "nav-grn2", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','grn2',{priority:'event'});highlightNav('nav-grn2');",
                                          tags$span(class = "material-symbols-outlined text-lg", "account_tree"), "GRN 2.0"),
                                   tags$a(id = "nav-removed", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','removed',{priority:'event'});highlightNav('nav-removed');",
                                          tags$span(class = "material-symbols-outlined text-lg", "production_quantity_limits"), "Removed Data"),
                                   tags$a(id = "nav-reports", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','reports',{priority:'event'});highlightNav('nav-reports');",
                                          tags$span(class = "material-symbols-outlined text-lg", "analytics"), "Raw Reports"),
                                   tags$a(id = "nav-analytics", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','analytics',{priority:'event'});highlightNav('nav-analytics');",
                                          tags$span(class = "material-symbols-outlined text-lg", "tuning"), "Advanced Analytics")),
                          tags$hr(style = "border-color:#263544;margin:0 24px 16px;"),
                          tags$div(class = "px-4 flex-1 overflow-y-auto sidebar-section space-y-4",
                                   # ── 1. Upload Mapping ────────────────────
                                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest", style = "color:#64748b;", "1. Upload Mapping"),
                                   tags$div(class = "space-y-2",
                                            tags$button(onclick = "triggerFileUpload()",
                                                        style = "background:#006495;color:white;border:none;cursor:pointer;width:100%;padding:8px 12px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;text-align:left;",
                                                        HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">upload_file</span>&nbsp;SELECT XLSX')),
                                            uiOutput("file_name_display")),
                                   actionButton("show_missing_report",
                                                HTML('<span class="material-symbols-outlined text-sm" style="vertical-align:middle;">error</span>&nbsp;Missing PID-CID'),
                                                style = "background:#b45309;color:white;border:none;cursor:pointer;width:100%;padding:8px 12px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;text-align:left;margin-top:4px;"),
                                   
                                   # ── NEW: Data Source Toggle ─────────────
                                   data_source_sidebar_ui(),
                                   
                                   # ── 2. Dates + Load ──────────────────────
                                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest pt-2", style = "color:#64748b;", "2. Start Date"),
                                   dateInput("date_filter_start", label = NULL, value = Sys.Date() - 30, format = "yyyy-mm-dd"),
                                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest", style = "color:#64748b;", "End Date"),
                                   dateInput("date_filter_end", label = NULL, value = Sys.Date(), format = "yyyy-mm-dd"),
                                   tags$div(class = "grid grid-cols-2 gap-2",
                                            tags$button(type = "button",
                                                        onclick = "Shiny.setInputValue('date_preset','last_7',{priority:'event'});",
                                                        style = "background:#263544;color:#cbd5e1;border:1px solid #374d60;cursor:pointer;width:100%;padding:6px 8px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.04em;text-transform:uppercase;",
                                                        "Last 7D"),
                                            tags$button(type = "button",
                                                        onclick = "Shiny.setInputValue('date_preset','last_30',{priority:'event'});",
                                                        style = "background:#263544;color:#cbd5e1;border:1px solid #374d60;cursor:pointer;width:100%;padding:6px 8px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.04em;text-transform:uppercase;",
                                                        "Last 30D"),
                                            tags$button(type = "button",
                                                        onclick = "Shiny.setInputValue('date_preset','last_90',{priority:'event'});",
                                                        style = "background:#263544;color:#cbd5e1;border:1px solid #374d60;cursor:pointer;width:100%;padding:6px 8px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.04em;text-transform:uppercase;",
                                                        "Last 90D"),
                                            tags$button(type = "button",
                                                        onclick = "Shiny.setInputValue('date_preset','mtd',{priority:'event'});",
                                                        style = "background:#263544;color:#cbd5e1;border:1px solid #374d60;cursor:pointer;width:100%;padding:6px 8px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.04em;text-transform:uppercase;",
                                                        "Month TD")),
                                   actionButton("refresh_btn", HTML('<span class="material-symbols-outlined text-sm" style="vertical-align:middle;">sync</span>&nbsp;LOAD DATA')),
                                   actionButton("logout", "Logout",
                                                style = "background:#da191e;color:white;border:none;width:100%;padding:8px 12px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;margin-top:8px;")),
                          tags$div(class = "px-4 mt-4",
                                   tags$div(class = "p-3 rounded-lg", style = "background:rgba(255,255,255,.04);border:1px solid #263544;",
                                            tags$p(class = "text-[9px] font-bold uppercase tracking-widest mb-2", style = "color:#475569;", "System Status"),
                                            tags$div(class = "flex items-center gap-2",
                                                     tags$span(class = "relative flex h-2 w-2",
                                                               tags$span(class = "animate-ping absolute inline-flex h-full w-full rounded-full opacity-75", style = "background:#10b981;"),
                                                               tags$span(class = "relative inline-flex h-2 w-2 rounded-full", style = "background:#10b981;")),
                                                     tags$span(class = "text-xs font-medium", style = "color:#94a3b8;", "Integrations Online"))))),
               
               # MAIN CONTENT
               tags$main(class = "app-main main-content flex-1 overflow-x-hidden", style = "margin-left:256px;width:calc(100% - 256px);",
                         tags$header(class = "app-header flex justify-between items-center px-8 h-16 bg-white sticky top-0 z-40",
                                     style = "border-bottom:1px solid #f1f5f9;",
                                     tags$div(class = "flex items-center gap-6",
                                              tags$button(id = "mobile_menu_btn", type = "button", onclick = "toggleMobileNav()",
                                                          tags$span(class = "material-symbols-outlined text-lg", "menu")),
                                              tags$span(class = "text-xl font-extrabold", style = "color:#da191e;font-family:'Plus Jakarta Sans',sans-serif;", "ProcureGraph"),
                                              tags$div(class = "flex items-center gap-3",
                                                       tags$div(class = "relative",
                                                                tags$span(class = "material-symbols-outlined absolute left-3 top-1/2 -translate-y-1/2 text-lg", style = "color:#94a3b8;", "search"),
                                                                tags$input(id = "global_search", type = "text",
                                                                           placeholder = "Search client, source, stage...",
                                                                           class = "pl-10 pr-4 py-2 text-sm rounded-lg outline-none w-72",
                                                                           style = "background:#f1f5f9;border:none;",
                                                                           oninput = "Shiny.setInputValue('global_search',this.value,{priority:'event'})")),
                                                       tags$select(
                                                         id = "integration_filter",
                                                         class = "px-3 py-2 text-sm rounded-lg outline-none",
                                                         style = "background:#f1f5f9;border:none;color:#475569;font-weight:600;",
                                                         tags$option(value = "all", "All MOM"),
                                                         tags$option(value = "integrated", "Integrated"),
                                                         tags$option(value = "non_integrated", "Non-Integrated")
                                                       ))),
                                     tags$div(class = "flex items-center gap-3",
                                              uiOutput("last_refreshed"),
                                              tags$div(class = "w-px h-8 mx-2", style = "background:#e2e8f0;"),
                                              actionButton("open_email_modal",
                                                           HTML('<span class="material-symbols-outlined" style="font-size:18px;vertical-align:middle;color:#64748b;">mail</span>'),
                                                           title = "Email Key Insights",
                                                           style = "background:none;border:1px solid #e2e8f0;border-radius:8px;padding:6px 10px;cursor:pointer;"),
                                              actionButton("open_email_modal_poc",
                                                           HTML('<span class="material-symbols-outlined" style="font-size:18px;vertical-align:middle;color:#f97316;">warning</span>'),
                                                           title = "Send Performance Alerts to Business POCs",
                                                           style = "background:none;border:1px solid #e2e8f0;border-radius:8px;padding:6px 10px;cursor:pointer;margin-left:8px;"),
                                              tags$div(class = "w-9 h-9 rounded-full flex items-center justify-center font-bold text-white text-sm", style = "background:#da191e;", "PI"))),
                         uiOutput("dashboard_content"))
      )
    }
  })
  
  # ── All logic post-auth ────────────────────────────────────────────────────
  observe({
    req(authenticated())
    
    is_fresh <- function() {
      !is.null(cached_data()) && !is.null(cache_ts()) &&
        as.numeric(difftime(Sys.time(), cache_ts(), units = "secs")) < CACHE_SEC
    }
    
    observeEvent(input$date_filter_start, cached_data(NULL))
    observeEvent(input$date_filter_end,   cached_data(NULL))
    observeEvent(input$company_file,      cached_data(NULL))
    observeEvent(input$nav_page,          current_page(input$nav_page))
    observeEvent(input$date_preset, {
      today <- Sys.Date()
      preset <- input$date_preset %||% ""
      start_date <- switch(
        preset,
        last_7  = today - 6,
        last_30 = today - 29,
        last_90 = today - 89,
        mtd     = as.Date(format(today, "%Y-%m-01")),
        NULL
      )
      if (!is.null(start_date)) {
        updateDateInput(session, "date_filter_start", value = start_date)
        updateDateInput(session, "date_filter_end", value = today)
        showNotification(
          paste0("Date preset applied: ", gsub("_", " ", toupper(preset))),
          type = "message",
          duration = 2
        )
      }
    })
    
    # ── NEW: Invalidate cache when data source mode changes ──────────────────
    observeEvent(input$data_source_mode,  cached_data(NULL))
    
    # ── NEW: Data source module (MongoDB ↔ Excel) ────────────────────────────
    data_src <- setup_data_source(
      input             = input,
      output            = output,
      session           = session,
      get_mongo_item_fn = get_mongo_data,
      get_mongo_grn_fn  = get_grn_data
    )
    
    # ── Excel mapping ────────────────────────────────────────────────────────
    company_data <- reactive({
      req(input$company_file)
      path <- input$company_file$datapath
      
      pid <- tryCatch(read_excel(path, sheet = "PID-CID"),
                      error = function(e) { showNotification(paste("PID-CID:", e$message), type = "error"); NULL })
      req(!is.null(pid))
      colnames(pid) <- trimws(colnames(pid))
      
      plant <- pid %>%
        mutate(across(everything(), ~trimws(as.character(.x)))) %>%
        rename_with(~ifelse(.x %in% c("plantId",  "PlantId",   "Plant ID",   "plant_id"),   "plantId",   .x)) %>%
        rename_with(~ifelse(.x %in% c("companyId", "CompanyId", "Company ID", "company_id"), "companyId", .x)) %>%
        select(plantId, companyId) %>%
        distinct(plantId, .keep_all = TRUE) %>%
        mutate(across(c(plantId, companyId), as.character))
      
      cid <- tryCatch(read_excel(path, sheet = "150 CID"),
                      error = function(e) { showNotification(paste("150 CID:", e$message), type = "error"); NULL })
      req(!is.null(cid))
      colnames(cid) <- trimws(colnames(cid))
      
      col_map <- list(
        companyId      = c("Company ID", "CompanyID",  "company_id",        "companyId",     "COMPANY ID"),
        companyName    = c("Company Name","CompanyName","company_name",       "COMPANY NAME"),
        `Final Status` = c("Integration Status","IntegrationStatus","integration_status","Final Status","INTEGRATION STATUS"),
        GRN            = c("GRN Status", "GRNStatus",  "grn_status",        "GRN",           "GRN STATUS"),
        BusinessPOC    = c("Business POC", "BusinessPoc", "business_poc", "Business POC Email", "BUSINESS POC", "POC Email")
      )
      master <- cid
      for (n in names(col_map)) master <- safe_rename(master, n, col_map[[n]])
      master <- master %>%
        mutate(
          companyId      = trimws(as.character(companyId)),
          companyName    = as.character(companyName),
          `Final Status` = as.character(`Final Status`),
          GRN            = as.character(GRN),
          BusinessPOC    = if ("BusinessPOC" %in% colnames(.)) {
            trimws(as.character(BusinessPOC))
          } else {
            NA_character_
          }
        ) %>%
        distinct(companyId, .keep_all = TRUE)
      
      list(plant = plant, company = master)
    })
    
    output$file_name_display <- renderUI({
      if (is.null(input$company_file))
        tags$p(style = "color:#475569;font-size:10px;", "No file selected")
      else
        tags$p(style = "color:#10b981;font-size:10px;font-weight:700;word-break:break-all;",
               HTML(paste0('<span class="material-symbols-outlined" style="font-size:12px;vertical-align:middle;">check_circle</span> ',
                           input$company_file$name)))
    })
    
    output$mapping_warnings <- renderUI({
      req(company_data())
      cd   <- company_data()
      msgs <- c()
      if (!"plantId"   %in% colnames(cd$plant))  msgs <- c(msgs, "'plantId' not found in PID-CID")
      if (!"companyId" %in% colnames(cd$plant))  msgs <- c(msgs, "'companyId' not found in PID-CID")
      if (any(is.na(cd$company$companyId)))       msgs <- c(msgs, "Some companyId are NA in 150 CID")
      if (all(is.na(cd$company$`Final Status`)))  msgs <- c(msgs, "'Integration Status' missing or misnamed")
      if (all(is.na(cd$company$GRN)))             msgs <- c(msgs, "'GRN Status' missing or misnamed")
      un <- setdiff(cd$plant$companyId, cd$company$companyId)
      if (length(un) > 0)
        msgs <- c(msgs, paste0(length(un), " companyId(s) unmatched: ", paste(head(un, 3), collapse = ", ")))
      if (!length(msgs))
        return(tags$div(class = "flex items-center gap-3 p-4 rounded-xl",
                        style = "background:#d4edda;border-left:4px solid #28a745;",
                        tags$span(class = "material-symbols-outlined", style = "color:#28a745;", "check_circle"),
                        tags$div(
                          tags$p(class = "text-sm font-bold", style = "color:#155724;", "Excel mapping loaded successfully"),
                          tags$p(class = "text-xs", style = "color:#155724;", "All mandatory columns matched."))))
      tags$div(class = "flex items-center gap-3 p-4 rounded-xl",
               style = "background:#fff3cd;border-left:4px solid #ffc107;",
               tags$span(class = "material-symbols-outlined", style = "color:#856404;", "warning"),
               tags$div(
                 tags$p(class = "text-sm font-bold", style = "color:#856404;", "Mapping Warnings"),
                 tags$p(class = "text-xs", style = "color:#856404;", paste(msgs, collapse = " | "))))
    })
    
    # ── UPDATED: processed_df supports both MongoDB and Excel ─────────────────
    processed_df <- eventReactive(input$refresh_btn, {
      req(input$date_filter_start, input$date_filter_end, company_data())
      
      mode <- isolate(input$data_source_mode) %||% "mongodb"
      
      # Use cache only in MongoDB mode
      if (mode == "mongodb" && is_fresh()) {
        showNotification("Using cached data (<5 min old)", type = "message", duration = 2)
        return(cached_data())
      }
      
      withProgress(
        message = if (mode == "excel") "Reading Excel data..." else "Fetching from MongoDB...",
        value = 0.2, {
          
          # ── FETCH ─────────────────────────────────────────────────────────────
          if (mode == "excel") {
            raw_excel <- data_src$item_raw()
            if (is.null(raw_excel) || nrow(raw_excel) == 0) {
              showNotification(
                "No Item Excel uploaded. Please upload in the sidebar or switch to MongoDB mode.",
                type = "error", duration = 6
              )
              return(NULL)
            }
            raw <- raw_excel
            # Ensure _id column exists
            if (!"_id" %in% names(raw)) raw[["_id"]] <- as.character(seq_len(nrow(raw)))
          } else {
            raw <- tryCatch(
              get_mongo_data(input$date_filter_start, input$date_filter_end),
              error = function(e) { showNotification(paste("MongoDB:", e$message), type = "error"); NULL }
            )
            req(!is.null(raw), nrow(raw) > 0)
          }
          
          setProgress(0.5, message = "Building columns...")
          d_start <- as.Date(input$date_filter_start)
          d_end   <- as.Date(input$date_filter_end)
          if (!"amount" %in% names(raw)) raw$amount <- NA
          amount_numeric <- normalize_amount_column(raw$amount, nrow(raw))
          
          df <- raw %>% mutate(
            buyer_id_str           = trimws(as.character(buyerId)),
            histPoCustomerPoNo     = trimws(as.character(histPoCustomerPoNo)),
            histPoCustomerSANumber = trimws(as.character(histPoCustomerSANumber)),
            amount       = amount_numeric,
            Amount_Crore = amount / 10000000,
            Stage        = as.character(stage),
            CreationDate = as.Date(as.POSIXct(creationDate / 1000, origin = "1970-01-01")),
            Month        = format(CreationDate, "%Y-%m"),
            `Consider Month` = ifelse(
              CreationDate >= d_start & CreationDate <= d_end, "Consider", "Ignore"
            )
          )
          
          setProgress(0.7, message = "Joining mapping...")
          df <- df %>%
            left_join(company_data()$plant,   by = c("buyer_id_str" = "plantId")) %>%
            left_join(company_data()$company, by = "companyId") %>%
            mutate(
              `PID-CID`     = coalesce(companyId,   "Unknown"),
              `Client Name` = coalesce(companyName, "Unknown")
            )
          
          setProgress(0.85, message = "Business logic...")
          df <- df %>% mutate(
            `Consider Source`  = ifelse(source %in% c("EOC", "Manual", "SAP"), source, "Ignore"),
            `Consider Stage`   = ifelse(Stage  %in% c("Closed", "Cancelled"), "Ignore", "Consider"),
            `Final Status`     = coalesce(`Final Status`, "Ignore"),
            GRN                = coalesce(GRN, "Ignore"),
            RemoveHavellsCapex = ifelse(`PID-CID` == "1211",
                                        ifelse(substr(histPoCustomerSANumber, 1, 2) == "45", "Consider", "Ignore"),
                                        "Consider"),
            RemoveSC           = ifelse(`PID-CID` == "8853",
                                        ifelse(buyer_id_str %in% c("9550","9557","25976","26023","26186","26187"),
                                               ifelse(substr(histPoCustomerSANumber, 1, 2) == "71", "Ignore", "Consider"),
                                               "Ignore"),
                                        "Consider")
          ) %>%
            mutate(`Remove(Y/N)` = ifelse(
              `Consider Source` == "Ignore" | `Consider Stage` == "Ignore" |
                `Consider Month` == "Ignore" | `Final Status` == "Ignore" |
                RemoveHavellsCapex == "Ignore" | RemoveSC == "Ignore" | GRN == "Ignore",
              "Remove", "Consider"
            )) %>%
            rename(
              `buyer.id`                    = buyer_id_str,
              `history.po.customerPoNo`     = histPoCustomerPoNo,
              `history.po.customerSANumber` = histPoCustomerSANumber
            ) %>%
            select(
              `_id`, `buyer.id`, `history.po.customerPoNo`, `history.po.customerSANumber`,
              status, creationDate, amount, Amount_Crore, source, stage, Stage, CreationDate, Month, `Consider Month`,
              `PID-CID`, `Client Name`, `Consider Source`, `Consider Stage`, `Final Status`,
              RemoveHavellsCapex, RemoveSC, `Remove(Y/N)`, GRN
            )
          
          setProgress(1, message = "Done!")
          refresh_time(Sys.time())
          if (mode == "mongodb") { cached_data(df); cache_ts(Sys.time()) }
          df
        })
    })
    
    filtered_df <- reactive({
      req(processed_df())
      df <- processed_df()
      s  <- trimws(input$global_search %||% "")
      integration_filter <- input$integration_filter %||% "all"

      if (integration_filter == "integrated") {
        df <- df[is_integrated_line_item(df$source), , drop = FALSE]
      } else if (integration_filter == "non_integrated") {
        df <- df[!is_integrated_line_item(df$source), , drop = FALSE]
      }

      if (!nchar(s)) return(df)
      sl   <- tolower(s)
      mask <- grepl(sl, tolower(df$`buyer.id`),    fixed = TRUE) |
        grepl(sl, tolower(df$`Client Name`), fixed = TRUE) |
        grepl(sl, tolower(df$source),        fixed = TRUE) |
        grepl(sl, tolower(df$Stage),         fixed = TRUE)
      df[mask, , drop = FALSE]
    })
    
    consider_df <- reactive({
      req(filtered_df())
      filtered_df() %>% filter(`Remove(Y/N)` == "Consider")
    })
    
    # ── UPDATED: grn_raw supports both MongoDB and Excel ──────────────────────
    grn_raw <- eventReactive(input$refresh_btn, {
      req(input$date_filter_start, input$date_filter_end)
      mode <- isolate(input$data_source_mode) %||% "mongodb"
      
      withProgress(
        message = if (mode == "excel") "Reading GRN Excel..." else "Fetching GRN...",
        value = 0.5, {
          d <- if (mode == "excel") {
            grn_excel <- data_src$grn_raw()
            if (is.null(grn_excel) || nrow(grn_excel) == 0) {
              showNotification("No GRN Excel uploaded — GRN section will be empty.", type = "warning")
              data.frame()
            } else {
              grn_excel
            }
          } else {
            tryCatch(
              get_grn_data(input$date_filter_start, input$date_filter_end),
              error = function(e) { showNotification(paste("GRN:", e$message), type = "warning"); data.frame() }
            )
          }
          setProgress(1); d
        })
    })
    
    grn_df <- reactive({
      raw <- grn_raw()
      if (is.null(raw) || nrow(raw) == 0)
        return(data.frame(`PID-CID` = character(), `Client Name` = character(),
                          buyerId = character(), customerGrnNo = character(),
                          poId = character(), itemId = character(),
                          GrnDate = as.Date(character()), Month = character(),
                          stringsAsFactors = FALSE))
      req(company_data(), processed_df())
      raw$buyerId <- trimws(as.character(raw$buyerId))
      raw$GrnDate <- as.Date(as.POSIXct(raw$creationDate / 1000, origin = "1970-01-01"))
      raw$Month   <- format(raw$GrnDate, "%Y-%m")
      raw <- raw %>%
        left_join(company_data()$plant,   by = c("buyerId" = "plantId")) %>%
        left_join(company_data()$company, by = "companyId") %>%
        mutate(`PID-CID` = coalesce(companyId, "Unknown"), `Client Name` = coalesce(companyName, "Unknown"))
      grn_y <- processed_df() %>% filter(GRN == "Y") %>% pull(`PID-CID`) %>% unique()
      raw %>% filter(`PID-CID` %in% grn_y, `PID-CID` != "9802")
    })
    
    # ── Missing mapping modal ────────────────────────────────────────────────
    missing_map_data <- reactive({
      req(processed_df())
      processed_df() %>%
        filter(source == "SAP", `Consider Source` != "Ignore", `Consider Stage` != "Ignore",
               `Consider Month` == "Consider", `Final Status` == "Ignore", GRN == "Ignore",
               RemoveHavellsCapex == "Consider", RemoveSC == "Consider", `PID-CID` == "Unknown") %>%
        group_by(`buyer.id`) %>% summarise(Missing_Count = n(), .groups = "drop") %>%
        arrange(desc(Missing_Count))
    })
    
    observeEvent(input$show_missing_report, {
      req(missing_map_data())
      if (!nrow(missing_map_data())) { showNotification("No missing mapping found", type = "message"); return() }
      showModal(modalDialog(
        title = "Missing PID-CID Report", size = "l", easyClose = TRUE, footer = modalButton("Close"),
        DTOutput("missing_map_modal")
      ))
    })
    
    output$missing_map_modal <- renderDT({
      req(missing_map_data())
      datatable(missing_map_data(), rownames = FALSE, options = list(pageLength = 15, dom = "frtip")) %>%
        formatStyle("Missing_Count",
                    background = styleColorBar(range(missing_map_data()$Missing_Count), "#ffedd5"),
                    backgroundSize = "100% 90%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })
    
    # ── Company integration summary ──────────────────────────────────────────
    build_company_integration <- function(df) {
      if (!"Amount_Crore" %in% names(df)) df$Amount_Crore <- 0
      df %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown") %>%
        group_by(`PID-CID`, `Client Name`, `Final Status`) %>%
        summarise(SAP    = sum(source == "SAP",    na.rm = TRUE),
                  Manual = sum(source == "Manual", na.rm = TRUE),
                  EOC    = sum(source == "EOC",    na.rm = TRUE),
                  SAP_Value_Cr    = sum(ifelse(source == "SAP",    Amount_Crore, 0), na.rm = TRUE),
                  Manual_Value_Cr = sum(ifelse(source == "Manual", Amount_Crore, 0), na.rm = TRUE),
                  EOC_Value_Cr    = sum(ifelse(source == "EOC",    Amount_Crore, 0), na.rm = TRUE),
                  .groups = "drop") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(SAP    = sum(SAP),
                  Manual = sum(Manual),
                  EOC    = sum(EOC),
                  SAP_Value_Cr    = sum(SAP_Value_Cr),
                  Manual_Value_Cr = sum(Manual_Value_Cr),
                  EOC_Value_Cr    = sum(EOC_Value_Cr),
                  Has_No_Integration = any(`Final Status` == "No Integration"),
                  .groups = "drop") %>%
        mutate(Grand_Total       = SAP + Manual + EOC,
               Integration_Pct   = ifelse(Grand_Total > 0, round((SAP + Manual) / Grand_Total * 100, 1), 0),
               Integrated_Volume = SAP + Manual,
               Total_Value_Cr       = SAP_Value_Cr + Manual_Value_Cr + EOC_Value_Cr,
               Integrated_Value_Cr  = SAP_Value_Cr + Manual_Value_Cr,
               Value_Integration_Pct = ifelse(Total_Value_Cr > 0, round(Integrated_Value_Cr / Total_Value_Cr * 100, 1), 0)) %>%
        filter(Grand_Total > 0)
    }
    
    build_company_performance <- function(df) {
      if (!"Amount_Crore" %in% names(df)) df$Amount_Crore <- 0
      df %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(
          SAP    = sum(source == "SAP",    na.rm = TRUE),
          Manual = sum(source == "Manual", na.rm = TRUE),
          EOC    = sum(source == "EOC",    na.rm = TRUE),
          SAP_Value_Cr    = sum(ifelse(source == "SAP",    Amount_Crore, 0), na.rm = TRUE),
          Manual_Value_Cr = sum(ifelse(source == "Manual", Amount_Crore, 0), na.rm = TRUE),
          EOC_Value_Cr    = sum(ifelse(source == "EOC",    Amount_Crore, 0), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          Grand_Total       = SAP + Manual + EOC,
          Integration_Pct   = ifelse(Grand_Total > 0, round((SAP + Manual) / Grand_Total * 100, 1), 0),
          Integrated_Volume = SAP + Manual,
          Total_Value_Cr       = SAP_Value_Cr + Manual_Value_Cr + EOC_Value_Cr,
          Integrated_Value_Cr  = SAP_Value_Cr + Manual_Value_Cr,
          Value_Integration_Pct = ifelse(Total_Value_Cr > 0, round(Integrated_Value_Cr / Total_Value_Cr * 100, 1), 0)
        ) %>%
        filter(Grand_Total > 0)
    }

    company_integration <- reactive({
      req(consider_df())
      build_company_integration(consider_df())
    })
    
    company_performance <- reactive({
      req(consider_df(), company_data())
      build_company_performance(consider_df())
    })
    
    # ═══════════════════════════════════════════════════════════════════════════
    # 8. INITIALIZE MODULES (v6.1)
    # ═══════════════════════════════════════════════════════════════════════════
    
    global_filters <- setup_global_filters(
      input        = input,
      output       = output,
      session      = session,
      processed_df = processed_df,
      consider_df  = consider_df
    )

    company_integration_filtered <- reactive({
      req(global_filters$data())
      build_company_integration(global_filters$data())
    })

    company_performance_filtered <- reactive({
      req(global_filters$data())
      build_company_performance(global_filters$data())
    })

    output$analytics_data_health <- renderUI({
      req(processed_df(), global_filters$data())
      raw_df <- processed_df()
      scoped_df <- global_filters$data()
      if (is.null(raw_df) || !nrow(raw_df)) return(NULL)
      
      total_rows       <- nrow(raw_df)
      valid_rows       <- sum(raw_df$`Remove(Y/N)` == "Consider", na.rm = TRUE)
      remove_rate      <- if (total_rows > 0) round((total_rows - valid_rows) / total_rows * 100, 1) else 0
      unknown_clients  <- sum(raw_df$`Client Name` == "Unknown", na.rm = TRUE)
      active_companies <- scoped_df %>% distinct(`PID-CID`) %>% filter(`PID-CID` != "Unknown") %>% nrow()
      no_int_rows      <- sum(scoped_df$`Final Status` == "No Integration", na.rm = TRUE)
      
      health_card <- function(label, value, subtext, fg, bg) {
        tags$div(
          class = paste(CARD, "p-5"),
          style = paste0(BORDER, "background:", bg, ";"),
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = paste0("color:", fg, ";"), label),
          tags$h3(class = "text-2xl font-extrabold", style = paste0("color:", fg, ";"), value),
          tags$p(class = "text-xs mt-2", style = "color:#475569;", subtext)
        )
      }
      
      tags$div(
        class = "space-y-4",
        tags$div(
          class = "flex items-center justify-between",
          tags$div(
            tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Data Health Monitor"),
            tags$p(class = "text-xs mt-1", style = "color:#64748b;", "A quick quality snapshot for the loaded data and current analytics filter scope.")
          ),
          tags$div(
            class = "px-3 py-1 rounded-full text-[10px] font-bold uppercase tracking-widest",
            style = "background:#f0f9ff;color:#0c4a6e;border:1px solid #bae6fd;",
            paste(format(nrow(scoped_df), big.mark = ","), "filtered rows")
          )
        ),
        tags$div(
          class = "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-4 gap-4",
          health_card("Valid Rows", format(valid_rows, big.mark = ","), "Rows currently marked Consider", "#166534", "#f0fdf4"),
          health_card("Removal Rate", paste0(remove_rate, "%"), "Share excluded by business rules", "#b45309", "#fffbeb"),
          health_card("Unknown Clients", format(unknown_clients, big.mark = ","), "Rows not matched to company master", "#b91c1c", "#fef2f2"),
          health_card("No Integration", format(no_int_rows, big.mark = ","), paste0(active_companies, " active companies in current scope"), "#1d4ed8", "#eff6ff")
        )
      )
    })
    
    output$analytics_benchmark_summary <- renderUI({
      req(global_filters$data())
      scoped_df <- global_filters$data()
      if (is.null(scoped_df) || !nrow(scoped_df)) return(NULL)
      
      by_month <- scoped_df %>%
        group_by(Month) %>%
        summarise(
          Total = n(),
          Integrated = sum(source %in% c("SAP", "Manual"), na.rm = TRUE),
          Companies = n_distinct(`PID-CID`[`PID-CID` != "Unknown"]),
          .groups = "drop"
        ) %>%
        mutate(Integration_Pct = ifelse(Total > 0, round(Integrated / Total * 100, 1), 0)) %>%
        arrange(Month)
      
      latest <- tail(by_month, 1)
      previous <- if (nrow(by_month) >= 2) by_month[nrow(by_month) - 1, , drop = FALSE] else NULL
      delta <- if (!is.null(previous) && nrow(previous)) round(latest$Integration_Pct - previous$Integration_Pct, 1) else NA_real_
      trend_text <- if (is.na(delta)) "No previous month available"
      else if (delta > 0) paste0("Up ", delta, " pts vs previous month")
      else if (delta < 0) paste0("Down ", abs(delta), " pts vs previous month")
      else "Flat vs previous month"
      
      top_company <- build_company_performance(scoped_df) %>% arrange(desc(Integration_Pct), desc(Integrated_Volume)) %>% slice_head(n = 1)
      top_company_text <- if (nrow(top_company)) {
        paste0(top_company$`Client Name`[1], " at ", top_company$Integration_Pct[1], "%")
      } else {
        "No mapped company data"
      }
      
      pill <- function(label, value, accent, bg) {
        tags$div(
          class = paste(CARD, "p-5"),
          style = paste0(BORDER, "background:", bg, ";"),
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = paste0("color:", accent, ";"), label),
          tags$h3(class = "text-2xl font-extrabold", style = "color:#0e1d28;", value)
        )
      }
      
      tags$div(
        class = "grid grid-cols-1 md:grid-cols-3 gap-4",
        pill("Latest Month", as.character(latest$Month[1]), "#006495", "#f8fbff"),
        pill("Latest Integration", paste0(latest$Integration_Pct[1], "%"), "#15803d", "#f0fdf4"),
        pill("Best Company", top_company_text, "#7c3aed", "#faf5ff"),
        tags$div(
          class = "md:col-span-3 p-4 rounded-xl",
          style = "background:#fff7ed;border:1px solid #fed7aa;",
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#9a3412;", "Momentum Readout"),
          tags$p(class = "text-sm font-semibold", style = "color:#7c2d12;", trend_text),
          tags$p(class = "text-xs mt-2", style = "color:#9a3412;", paste0(format(latest$Companies[1], big.mark = ","), " active companies and ", format(latest$Total[1], big.mark = ","), " valid line items in the latest visible month."))
        )
      )
    })
    
    output$analytics_opportunity_radar <- renderUI({
      req(global_filters$data())
      scoped_df <- global_filters$data()
      perf <- build_company_performance(scoped_df)
      if (is.null(perf) || !nrow(perf)) return(NULL)
      
      focus <- perf %>%
        filter(Grand_Total >= 10) %>%
        mutate(
          Opportunity_Score = round((100 - Integration_Pct) * log1p(Grand_Total), 1)
        ) %>%
        arrange(desc(Opportunity_Score), desc(Grand_Total)) %>%
        slice_head(n = 5)
      
      if (!nrow(focus)) return(NULL)
      
      tags$div(
        class = paste(CARD, "p-6"),
        style = BORDER,
        tags$div(
          class = "flex items-center gap-2 mb-4",
          tags$span(class = "material-symbols-outlined", style = "color:#dc2626;", "radar"),
          tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Opportunity Radar")
        ),
        tags$div(
          class = "space-y-3",
          lapply(seq_len(nrow(focus)), function(i) {
            row <- focus[i, ]
            pct <- max(min(row$Integration_Pct, 100), 0)
            tags$div(
              class = "p-4 rounded-xl",
              style = "background:#f8fafc;border:1px solid #e2e8f0;",
              tags$div(
                class = "flex justify-between items-center gap-3 mb-2",
                tags$div(
                  tags$p(class = "text-sm font-extrabold m-0", row$`Client Name`),
                  tags$p(class = "text-[11px] m-0", style = "color:#64748b;", paste0("PID-CID: ", row$`PID-CID`, " | Volume: ", format(row$Grand_Total, big.mark = ","), " | Opportunity Score: ", row$Opportunity_Score))
                ),
                tags$div(class = "text-right",
                         tags$p(class = "text-lg font-extrabold m-0", style = if (pct >= 85) "color:#15803d;" else if (pct >= 70) "color:#92400e;" else "color:#dc2626;", paste0(pct, "%")))
              ),
              tags$div(class = "h-2 rounded-full overflow-hidden", style = "background:#e2e8f0;",
                       tags$div(class = "h-full rounded-full", style = paste0("width:", pct, "%;background:", if (pct >= 85) "#22c55e" else if (pct >= 70) "#f59e0b" else "#ef4444", ";")))
            )
          })
        )
      )
    })
    
    custom_analytics <- setup_custom_analytics(
      input               = input,
      output              = output,
      session             = session,
      processed_df        = processed_df,
      consider_df         = consider_df,
      company_integration = company_integration_filtered,
      filtered_data       = global_filters$data
    )

    removed_data <- setup_removed_data(
      input        = input,
      output       = output,
      session      = session,
      processed_df = processed_df,
      filtered_df  = filtered_df
    )
    
    ai_insights <- setup_ai_insights(
      input               = input,
      output              = output,
      session             = session,
      consider_df         = global_filters$data,
      company_integration = company_integration_filtered,
      company_performance = company_performance_filtered
    )
    
    # Cloud-compatible dashboard save/load (auto-detects local vs cloud)
    dashboard_save <- setup_dashboard_save(
      input        = input,
      output       = output,
      session      = session,
      chart_config = custom_analytics$config
    )
    
    # ── Dashboard outputs ────────────────────────────────────────────────────
    output$top5_chart <- renderUI({
      req(company_integration())
      data <- company_integration()
      if (!nrow(data)) return(tags$p(style = "color:#94a3b8;font-size:12px;", "No data loaded."))
      
      top5    <- data %>% arrange(desc(Integrated_Volume)) %>% slice_head(n = 5)
      max_vol <- max(top5$Integrated_Volume)
      
      bars <- paste(sapply(seq_len(nrow(top5)), function(i) {
        html_bar(
          label       = top5$`Client Name`[i],
          value       = top5$Integrated_Volume[i],
          max_val     = max_vol,
          count_label = sprintf("%s%% integrated · %s of %s line items",
                                top5$Integration_Pct[i],
                                format(top5$Integrated_Volume[i], big.mark = ","),
                                format(top5$Grand_Total[i], big.mark = ",")),
          color       = "#006495",
          rank        = i
        )
      }), collapse = "")
      HTML(paste0('<div style="padding:4px 0;">', bars, '</div>'))
    })
    
    output$no_integration_chart <- renderUI({
      req(company_integration())
      data <- company_integration()
      if (!nrow(data)) return(tags$p(style = "color:#94a3b8;font-size:12px;", "No data loaded."))
      no_int <- consider_df() %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown", `Final Status` == "No Integration") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(Orders = n(), SAP = sum(source == "SAP", na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Orders)) %>% slice_head(n = 5)
      if (!nrow(no_int))
        return(tags$div(style = "text-align:center;padding:20px;",
                        tags$span(class = "material-symbols-outlined", style = "color:#16a34a;font-size:32px;", "check_circle"),
                        tags$p(style = "color:#16a34a;font-weight:700;margin-top:8px;", "No companies on No Integration status!")))
      max_cnt <- max(no_int$Orders)
      bars <- paste(sapply(seq_len(nrow(no_int)), function(i) {
        html_bar(no_int$`Client Name`[i], no_int$Orders[i], max_cnt,
                 paste0(format(no_int$Orders[i], big.mark = ","), " line items · SAP: ", no_int$SAP[i]),
                 "#da191e", rank = i)
      }), collapse = "")
      HTML(paste0('<div style="padding:4px 0;">', bars, '</div>'))
    })
    
    memo_pivot <- memoise(create_pivot, cache = cachem::cache_mem(max_age = 60))
    memo_value_pivot <- memoise(create_value_pivot, cache = cachem::cache_mem(max_age = 60))
    
    render_pivot_dt <- function(df_fn, label) renderDT({
      req(filtered_df())
      p <- memo_pivot(df_fn())
      m <- calc_metrics(p)
      datatable(p, rownames = FALSE,
                caption = htmltools::tags$caption(
                  style = "caption-side:top;text-align:left;font-weight:700;font-size:12px;padding:12px 16px;color:#0e1d28;",
                  paste0(label, " — Integration: ", m$integration, " | CBB: ", m$cbb, " | Final: ", m$final)
                ),
                options = list(dom = "t", pageLength = 20)) %>%
        formatStyle("Grand Total", fontWeight = "bold") %>%
        formatStyle(columns = colnames(p), fontSize = "13px")
    })

    render_value_pivot_dt <- function(df_fn, label) renderDT({
      req(filtered_df())
      p <- memo_value_pivot(df_fn())
      m <- calc_metrics(p)
      datatable(p, rownames = FALSE,
                caption = htmltools::tags$caption(
                  style = "caption-side:top;text-align:left;font-weight:700;font-size:12px;padding:12px 16px;color:#0e1d28;",
                  paste0(label, " value (Cr) - Integration: ", m$integration, " | CBB: ", m$cbb, " | Final: ", m$final)
                ),
                options = list(
                  dom = "t",
                  pageLength = 20,
                  columnDefs = list(list(
                    targets = 1:(ncol(p) - 1),
                    className = "dt-right",
                    render = JS("function(data, type) { if (type === 'display' && data !== null && data !== '') { var val = Number(data); return Number.isFinite(val) ? val.toLocaleString('en-IN', {minimumFractionDigits:2, maximumFractionDigits:2}) : data; } return data; }")
                  ))
                )) %>%
        formatStyle("Grand Total", fontWeight = "bold") %>%
        formatStyle(columns = colnames(p), fontSize = "13px")
    })
    
    output$pivot_without_table <- render_pivot_dt(function() filtered_df() %>% filter(`PID-CID` != "9802"), "Without ABFRL")
    output$pivot_with_table    <- render_pivot_dt(function() filtered_df(), "With ABFRL")
    output$value_pivot_without_table <- render_value_pivot_dt(function() filtered_df() %>% filter(`PID-CID` != "9802"), "Without ABFRL")
    output$value_pivot_with_table    <- render_value_pivot_dt(function() filtered_df(), "With ABFRL")
    
    output$raw_table <- renderDT({
      req(filtered_df())
      datatable(filtered_df(), filter = "top", rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 15,
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("Remove(Y/N)",
                    backgroundColor = styleEqual(c("Remove", "Consider"), c("#ffe0e0", "#e0ffe0")),
                    fontWeight = "bold")
    })
    
    grn_integrated_status <- c("Live", "Live - CBB", "PR Int. - Punchout")

    grn_eligible_accounts <- reactive({
      req(company_data())
      company_data()$company %>%
        mutate(GRN = toupper(trimws(as.character(GRN)))) %>%
        filter(
          GRN == "Y",
          companyId != "9802",
          `Final Status` %in% grn_integrated_status
        ) %>%
        transmute(`PID-CID` = companyId, `Client Name` = companyName, `Final Status`, GRN) %>%
        distinct(`PID-CID`, .keep_all = TRUE)
    })

    grn_po_base <- reactive({
      req(consider_df())
      consider_df() %>%
        filter(
          `PID-CID` != "9802",
          `Final Status` %in% grn_integrated_status
        )
    })

    grn_pct_value <- function(grn_count, po_count) {
      ifelse(po_count > 0, round(grn_count / po_count * 100, 1), NA_real_)
    }

    output$grn_total_val <- renderUI({ req(grn_df()); format(nrow(grn_df()), big.mark = ",") })
    output$grn_companies_val <- renderUI({ req(grn_df()); format(n_distinct(grn_df()$`PID-CID`), big.mark = ",") })
    output$grn_match_rate_val <- renderUI({
      req(grn_df(), grn_po_base())
      po_items <- nrow(grn_po_base())
      paste0(if (po_items > 0) round(nrow(grn_df()) / po_items * 100, 1) else 0, "%")
    })
    output$grn_eligible_accounts_val <- renderUI({
      req(grn_eligible_accounts())
      format(nrow(grn_eligible_accounts()), big.mark = ",")
    })
    output$grn_po_accounts_val <- renderUI({
      req(grn_po_base())
      format(n_distinct(grn_po_base()$`PID-CID`), big.mark = ",")
    })
    output$grn_po_line_items_val <- renderUI({
      req(grn_po_base())
      format(nrow(grn_po_base()), big.mark = ",")
    })
    
    grn_company_tbl <- reactive({
      req(grn_eligible_accounts(), grn_df(), grn_po_base())

      eligible <- grn_eligible_accounts()
      grn_co <- grn_df() %>%
        group_by(`PID-CID`) %>%
        summarise(`GRN Records` = n(), `GRN Volume Account` = "Yes", .groups = "drop")
      po_co <- grn_po_base() %>%
        group_by(`PID-CID`) %>%
        summarise(
          `PO Line Items` = n(),
          SAP = sum(source == "SAP", na.rm = TRUE),
          Manual = sum(source == "Manual", na.rm = TRUE),
          EOC = sum(source == "EOC", na.rm = TRUE),
          `PO Volume Account` = "Yes",
          .groups = "drop"
        )

      eligible %>%
        left_join(po_co, by = "PID-CID") %>%
        left_join(grn_co, by = "PID-CID") %>%
        mutate(
          `PO Line Items` = coalesce(as.integer(`PO Line Items`), 0L),
          `GRN Records` = coalesce(as.integer(`GRN Records`), 0L),
          SAP = coalesce(as.integer(SAP), 0L),
          Manual = coalesce(as.integer(Manual), 0L),
          EOC = coalesce(as.integer(EOC), 0L),
          `PO Volume Account` = coalesce(`PO Volume Account`, "No"),
          `GRN Volume Account` = coalesce(`GRN Volume Account`, "No"),
          `GRN %` = grn_pct_value(`GRN Records`, `PO Line Items`)
        ) %>%
        arrange(desc(`GRN Records`), desc(`PO Line Items`), `Client Name`) %>%
        select(`PID-CID`, `Client Name`, `Final Status`, `PO Volume Account`, `GRN Volume Account`,
               `PO Line Items`, `GRN Records`, `GRN %`, SAP, Manual, EOC)
    })
    
    output$grn_by_company <- renderDT({
      tbl <- grn_company_tbl()
      if (!nrow(tbl)) return(datatable(data.frame(Message = "No GRN eligible accounts"), options = list(dom = "t")))
      datatable(tbl, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                               columnDefs = list(
                                 list(targets = "_all", className = "dt-center"),
                                 list(
                                   targets = which(names(tbl) == "GRN %") - 1L,
                                   render = JS("function(data, type) { if (type === 'display') { var val = Number(data); return Number.isFinite(val) ? val.toFixed(1) + '%' : 'N/A'; } return data; }")
                                 )
                               ))) %>%
        formatStyle("GRN Records",
                    background = styleColorBar(range(tbl$`GRN Records`), "#d1fae5"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
        formatStyle("GRN %",
                    color = styleInterval(c(50, 80), c("#dc2626", "#ea580c", "#16a34a")),
                    fontWeight = "bold") %>%
        formatStyle(columns = colnames(tbl), fontSize = "13px")
    })
    
    grn_month_tbl <- reactive({
      req(grn_df(), grn_po_base(), grn_eligible_accounts())
      months <- sort(unique(c(grn_df()$Month, grn_po_base()$Month)))
      if (!length(months)) {
        return(data.frame(
          Month = character(), `GRN Eligible Accounts` = integer(), `PO Volume Accounts` = integer(),
          `GRN Volume Accounts` = integer(), `PO Line Items` = integer(), `GRN Records` = integer(),
          `GRN %` = numeric(), `GRN MoM Growth` = character()
        ))
      }

      base <- data.frame(Month = months, stringsAsFactors = FALSE)
      po_m <- grn_po_base() %>%
        group_by(Month) %>%
        summarise(`PO Volume Accounts` = n_distinct(`PID-CID`), `PO Line Items` = n(), .groups = "drop")
      grn_m <- grn_df() %>%
        group_by(Month) %>%
        summarise(`GRN Volume Accounts` = n_distinct(`PID-CID`), `GRN Records` = n(), .groups = "drop")

      base %>%
        left_join(po_m, by = "Month") %>%
        left_join(grn_m, by = "Month") %>%
        mutate(
          `GRN Eligible Accounts` = nrow(grn_eligible_accounts()),
          `PO Volume Accounts` = coalesce(as.integer(`PO Volume Accounts`), 0L),
          `GRN Volume Accounts` = coalesce(as.integer(`GRN Volume Accounts`), 0L),
          `PO Line Items` = coalesce(as.integer(`PO Line Items`), 0L),
          `GRN Records` = coalesce(as.integer(`GRN Records`), 0L)
        ) %>%
        arrange(Month) %>%
        mutate(
          `GRN %` = grn_pct_value(`GRN Records`, `PO Line Items`),
          Prev = lag(`GRN Records`),
          `GRN MoM Growth` = ifelse(!is.na(Prev) & Prev > 0,
                                    paste0(round((`GRN Records` - Prev) / Prev * 100, 1), "%"), "-")
        ) %>%
        select(Month, `GRN Eligible Accounts`, `PO Volume Accounts`, `GRN Volume Accounts`,
               `PO Line Items`, `GRN Records`, `GRN %`, `GRN MoM Growth`)
    })
    
    output$grn_by_month <- renderDT({
      tbl <- grn_month_tbl()
      if (!nrow(tbl)) return(datatable(data.frame(Message = "No GRN / PO data"), options = list(dom = "t")))
        datatable(tbl, rownames = FALSE,
                options = list(pageLength = 15, dom = "t",
                               columnDefs = list(
                                 list(targets = "_all", className = "dt-center"),
                                 list(
                                   targets = which(names(tbl) == "GRN %") - 1L,
                                   render = JS("function(data, type) { if (type === 'display') { var val = Number(data); return Number.isFinite(val) ? val.toFixed(1) + '%' : 'N/A'; } return data; }")
                                 )
                               ))) %>%
        formatStyle("GRN Records",
                    background = styleColorBar(range(tbl$`GRN Records`), "#dbeafe"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
        formatStyle("GRN %",
                    color = styleInterval(c(50, 80), c("#dc2626", "#ea580c", "#16a34a")),
                    fontWeight = "bold") %>%
        formatStyle(columns = colnames(tbl), fontSize = "13px")
    })
    
    output$grn_detail_table <- renderDT({
      req(grn_df())
      if (!nrow(grn_df())) return(datatable(data.frame(Message = "No GRN data"), options = list(dom = "t")))
      display <- grn_df() %>%
        select(`PID-CID`, `Client Name`, buyerId, customerGrnNo, poId, itemId, GrnDate, Month) %>%
        arrange(desc(GrnDate))
      datatable(display, filter = "top", rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 15,
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle(columns = colnames(display), fontSize = "12px")
    })

    grn2_po_tbl <- reactive({
      req(grn_df())
      g <- grn_df()
      if (!nrow(g)) {
        return(data.frame(
          `PO ID` = character(), `PID-CID` = character(), `Client Name` = character(),
          `Buyer IDs` = character(), `First GRN Date` = as.Date(character()), `Last GRN Date` = as.Date(character()),
          `First Month` = character(), `Last Month` = character(), `GRN Records` = integer(),
          `Distinct GRNs` = integer(), `Distinct Items` = integer(), `GRN Relationship` = character(),
          `GRN Numbers` = character(), stringsAsFactors = FALSE
        ))
      }

      g %>%
        mutate(
          poId = trimws(as.character(poId)),
          customerGrnNo = trimws(as.character(customerGrnNo)),
          itemId = trimws(as.character(itemId)),
          `PO ID` = ifelse(is.na(poId) | poId == "", "Unknown PO", poId),
          `GRN Number Clean` = ifelse(is.na(customerGrnNo) | customerGrnNo == "", "Unknown GRN", customerGrnNo),
          `Item ID Clean` = ifelse(is.na(itemId) | itemId == "", "Unknown Item", itemId)
        ) %>%
        group_by(`PO ID`) %>%
        summarise(
          `PID-CID` = paste(sort(unique(`PID-CID`)), collapse = ", "),
          `Client Name` = paste(sort(unique(`Client Name`)), collapse = ", "),
          `Buyer IDs` = paste(sort(unique(buyerId)), collapse = ", "),
          `First GRN Date` = suppressWarnings(min(GrnDate, na.rm = TRUE)),
          `Last GRN Date` = suppressWarnings(max(GrnDate, na.rm = TRUE)),
          `First Month` = min(Month, na.rm = TRUE),
          `Last Month` = max(Month, na.rm = TRUE),
          `GRN Records` = n(),
          `Distinct GRNs` = n_distinct(`GRN Number Clean`),
          `Distinct Items` = n_distinct(`Item ID Clean`),
          `GRN Numbers` = paste(sort(unique(`GRN Number Clean`)), collapse = ", "),
          .groups = "drop"
        ) %>%
        mutate(
          `GRN Relationship` = case_when(
            `PO ID` == "Unknown PO" ~ "Missing PO ID",
            `Distinct GRNs` <= 1 ~ "Single GRN",
            TRUE ~ "Multiple GRNs"
          )
        ) %>%
        arrange(desc(`Distinct GRNs`), desc(`GRN Records`), desc(`Last GRN Date`))
    })

    grn2_month_tbl <- reactive({
      req(grn_df())
      g <- grn_df()
      if (!nrow(g)) {
        return(data.frame(
          Month = character(), `POs With GRNs` = integer(), `Single-GRN POs` = integer(),
          `Multi-GRN POs` = integer(), `GRN Records` = integer(), `Distinct GRNs` = integer(),
          `Avg GRNs / PO` = numeric(), stringsAsFactors = FALSE
        ))
      }

      g %>%
        mutate(
          poId = trimws(as.character(poId)),
          customerGrnNo = trimws(as.character(customerGrnNo)),
          `PO ID` = ifelse(is.na(poId) | poId == "", "Unknown PO", poId),
          `GRN Number Clean` = ifelse(is.na(customerGrnNo) | customerGrnNo == "", "Unknown GRN", customerGrnNo)
        ) %>%
        group_by(Month, `PO ID`) %>%
        summarise(`GRN Records` = n(), `Distinct GRNs` = n_distinct(`GRN Number Clean`), .groups = "drop") %>%
        group_by(Month) %>%
        summarise(
          `POs With GRNs` = n_distinct(`PO ID`),
          `Single-GRN POs` = sum(`Distinct GRNs` <= 1, na.rm = TRUE),
          `Multi-GRN POs` = sum(`Distinct GRNs` > 1, na.rm = TRUE),
          `GRN Records` = sum(`GRN Records`, na.rm = TRUE),
          `Distinct GRNs` = sum(`Distinct GRNs`, na.rm = TRUE),
          `Avg GRNs / PO` = round(ifelse(`POs With GRNs` > 0, `Distinct GRNs` / `POs With GRNs`, 0), 2),
          .groups = "drop"
        ) %>%
        arrange(Month)
    })

    output$grn2_total_po_val <- renderUI({
      req(grn2_po_tbl())
      format(nrow(grn2_po_tbl() %>% filter(`PO ID` != "Unknown PO")), big.mark = ",")
    })
    output$grn2_single_po_val <- renderUI({
      req(grn2_po_tbl())
      format(sum(grn2_po_tbl()$`GRN Relationship` == "Single GRN", na.rm = TRUE), big.mark = ",")
    })
    output$grn2_multi_po_val <- renderUI({
      req(grn2_po_tbl())
      format(sum(grn2_po_tbl()$`GRN Relationship` == "Multiple GRNs", na.rm = TRUE), big.mark = ",")
    })
    output$grn2_avg_val <- renderUI({
      req(grn2_po_tbl())
      tbl <- grn2_po_tbl() %>% filter(`PO ID` != "Unknown PO")
      avg <- if (nrow(tbl)) round(mean(tbl$`Distinct GRNs`, na.rm = TRUE), 2) else 0
      format(avg, nsmall = 2)
    })

    output$grn2_po_table <- renderDT({
      tbl <- grn2_po_tbl()
      if (!nrow(tbl)) return(datatable(data.frame(Message = "No GRN data"), options = list(dom = "t")))
      datatable(tbl, filter = "top", rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 15,
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("GRN Relationship",
                    backgroundColor = styleEqual(c("Single GRN", "Multiple GRNs", "Missing PO ID"),
                                                 c("#dcfce7", "#ffedd5", "#fee2e2")),
                    color = styleEqual(c("Single GRN", "Multiple GRNs", "Missing PO ID"),
                                       c("#166534", "#9a3412", "#991b1b")),
                    fontWeight = "bold") %>%
        formatStyle("Distinct GRNs",
                    background = styleColorBar(range(tbl$`Distinct GRNs`), "#dbeafe"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })

    output$grn2_month_table <- renderDT({
      tbl <- grn2_month_tbl()
      if (!nrow(tbl)) return(datatable(data.frame(Message = "No GRN data"), options = list(dom = "t")))
      datatable(tbl, rownames = FALSE,
                options = list(pageLength = 12, dom = "t",
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("Multi-GRN POs",
                    background = styleColorBar(range(tbl$`Multi-GRN POs`), "#ffedd5"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
        formatStyle("Avg GRNs / PO", fontWeight = "bold", color = "#006495")
    })
    
    output$companies_by_status_card <- renderUI({
      req(consider_df(), company_data())
      co_ids   <- consider_df() %>% distinct(`PID-CID`) %>% pull()
      st_data  <- company_data()$company %>% filter(companyId %in% co_ids) %>%
        count(`Final Status`, name = "count") %>% filter(!is.na(`Final Status`))
      total_co <- sum(st_data$count)
      st_cols  <- c("Live" = "#006495", "Live - CBB" = "#10b981",
                    "No Integration" = "#f97316", "PR Int. - Punchout" = "#da191e")
      tags$div(class = paste(CARD, "p-6"), style = BORDER,
               tags$div(class = "flex justify-between items-start mb-4",
                        tags$div(class = "flex items-center gap-2",
                                 tags$span(class = "material-symbols-outlined", style = "color:#006495;", "business"),
                                 tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Companies by Integration Status")),
                        tags$span(class = "text-3xl font-extrabold", style = "color:#0e1d28;", format(total_co, big.mark = ","))),
               tags$div(class = "space-y-3",
                        lapply(seq_len(nrow(st_data)), function(i) {
                          st  <- st_data$`Final Status`[i]; cnt <- st_data$count[i]
                          pct <- if (total_co > 0) round(cnt / total_co * 100, 1) else 0
                          col <- ifelse(st %in% names(st_cols), st_cols[[st]], "#94a3b8")
                          tags$div(
                            tags$div(class = "flex justify-between text-xs font-bold mb-1",
                                     tags$span(style = paste0("color:", col, ";"), st),
                                     tags$span(style = "color:#64748b;", paste0(cnt, " (", pct, "%)"))),
                            tags$div(class = "h-2 rounded-full overflow-hidden", style = "background:#f1f5f9;",
                                     tags$div(class = "h-full rounded-full",
                                              style = paste0("width:", pct, "%;background:", col, ";"))))
                        })),
               tags$p(class = "text-[10px] mt-3", style = "color:#94a3b8;", "Companies with at least one valid line item (Consider)"))
    })
    
    output$integration_rate_card <- renderUI({
      req(consider_df())
      total_all <- nrow(consider_df())
      sm_all    <- sum(consider_df()$source %in% c("SAP", "Manual"), na.rm = TRUE)
      rate_all  <- if (total_all > 0) round(sm_all / total_all * 100, 1) else 0
      df_wo     <- consider_df() %>% filter(`PID-CID` != "9802")
      total_wo  <- nrow(df_wo)
      sm_wo     <- sum(df_wo$source %in% c("SAP", "Manual"), na.rm = TRUE)
      rate_wo   <- if (total_wo > 0) round(sm_wo / total_wo * 100, 1) else 0
      tags$div(class = paste(CARD, "p-6"), style = BORDER,
               tags$div(class = "flex items-center gap-2 mb-4",
                        tags$span(class = "material-symbols-outlined", style = "color:#da191e;", "percent"),
                        tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Integration Rate")),
               tags$div(class = "grid grid-cols-2 gap-4",
                        tags$div(class = "text-center p-4 rounded-xl", style = "background:#f0f9ff;",
                                 tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#006495;", "With ABFRL"),
                                 tags$p(class = "text-4xl font-extrabold", style = "color:#0e1d28;", paste0(rate_all, "%")),
                                 tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", paste0(format(total_all, big.mark = ","), " valid line items"))),
                        tags$div(class = "text-center p-4 rounded-xl", style = "background:#fff7ed;",
                                 tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#ea580c;", "Without ABFRL"),
                                 tags$p(class = "text-4xl font-extrabold", style = "color:#0e1d28;", paste0(rate_wo, "%")),
                                 tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", paste0(format(total_wo, big.mark = ","), " valid line items")))))
    })
    
    po_company_monthly <- reactive({
      req(consider_df())
      df <- consider_df() %>% filter(`PID-CID` != "9802")
      if (nrow(df) == 0) return(NULL)
      by_month <- df %>%
        group_by(`PID-CID`, `Client Name`, Month) %>%
        summarise(
          SAP    = sum(source == "SAP",    na.rm = TRUE),
          Manual = sum(source == "Manual", na.rm = TRUE),
          EOC    = sum(source == "EOC",    na.rm = TRUE),
          Total  = n(),
          .groups = "drop"
        ) %>%
        mutate(Integration_Pct = ifelse(Total > 0, round((SAP + Manual) / Total * 100, 1), 0))
      months_sorted <- sort(unique(by_month$Month))
      if (length(months_sorted) == 0) return(NULL)
      wide <- by_month %>%
        pivot_wider(
          id_cols     = c(`PID-CID`, `Client Name`),
          names_from  = Month,
          values_from = c(SAP, Manual, EOC, Total, Integration_Pct),
          names_sep   = "_"
        )
      month_cols <- unlist(lapply(months_sorted, function(m) {
        paste0(c("SAP_", "Manual_", "EOC_", "Total_", "Integration_Pct_"), m)
      }))
      month_cols <- month_cols[month_cols %in% names(wide)]
      overall <- df %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(
          SAP_Total    = sum(source == "SAP",    na.rm = TRUE),
          Manual_Total = sum(source == "Manual", na.rm = TRUE),
          EOC_Total    = sum(source == "EOC",    na.rm = TRUE),
          Total_Total  = n(),
          .groups      = "drop"
        ) %>%
        mutate(Integration_Pct_Total = ifelse(Total_Total > 0,
                                              round((SAP_Total + Manual_Total) / Total_Total * 100, 1), 0))
      wide <- wide %>% left_join(overall, by = c("PID-CID", "Client Name"))
      final_cols <- c("PID-CID", "Client Name", month_cols,
                      "SAP_Total", "Manual_Total", "EOC_Total", "Total_Total", "Integration_Pct_Total")
      wide <- wide[, final_cols, drop = FALSE]
      wide %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    })

    po_value_company_monthly <- reactive({
      req(consider_df())
      df <- consider_df() %>% filter(`PID-CID` != "9802")
      if (!"Amount_Crore" %in% names(df)) df$Amount_Crore <- 0
      if (nrow(df) == 0) return(NULL)
      by_month <- df %>%
        group_by(`PID-CID`, `Client Name`, Month) %>%
        summarise(
          SAP_Value_Cr    = sum(ifelse(source == "SAP",    Amount_Crore, 0), na.rm = TRUE),
          Manual_Value_Cr = sum(ifelse(source == "Manual", Amount_Crore, 0), na.rm = TRUE),
          EOC_Value_Cr    = sum(ifelse(source == "EOC",    Amount_Crore, 0), na.rm = TRUE),
          Total_Value_Cr  = sum(Amount_Crore, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Value_Integration_Pct = ifelse(Total_Value_Cr > 0, round((SAP_Value_Cr + Manual_Value_Cr) / Total_Value_Cr * 100, 1), 0))
      months_sorted <- sort(unique(by_month$Month))
      if (length(months_sorted) == 0) return(NULL)
      wide <- by_month %>%
        pivot_wider(
          id_cols     = c(`PID-CID`, `Client Name`),
          names_from  = Month,
          values_from = c(SAP_Value_Cr, Manual_Value_Cr, EOC_Value_Cr, Total_Value_Cr, Value_Integration_Pct),
          names_sep   = "_"
        )
      month_cols <- unlist(lapply(months_sorted, function(m) {
        paste0(c("SAP_Value_Cr_", "Manual_Value_Cr_", "EOC_Value_Cr_", "Total_Value_Cr_", "Value_Integration_Pct_"), m)
      }))
      month_cols <- month_cols[month_cols %in% names(wide)]
      overall <- df %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(
          SAP_Value_Cr_Total    = sum(ifelse(source == "SAP",    Amount_Crore, 0), na.rm = TRUE),
          Manual_Value_Cr_Total = sum(ifelse(source == "Manual", Amount_Crore, 0), na.rm = TRUE),
          EOC_Value_Cr_Total    = sum(ifelse(source == "EOC",    Amount_Crore, 0), na.rm = TRUE),
          Total_Value_Cr_Total  = sum(Amount_Crore, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(Value_Integration_Pct_Total = ifelse(Total_Value_Cr_Total > 0,
                                                    round((SAP_Value_Cr_Total + Manual_Value_Cr_Total) / Total_Value_Cr_Total * 100, 1), 0))
      wide <- wide %>% left_join(overall, by = c("PID-CID", "Client Name"))
      final_cols <- c("PID-CID", "Client Name", month_cols,
                      "SAP_Value_Cr_Total", "Manual_Value_Cr_Total", "EOC_Value_Cr_Total",
                      "Total_Value_Cr_Total", "Value_Integration_Pct_Total")
      wide <- wide[, final_cols, drop = FALSE]
      wide %>% mutate(across(where(is.numeric), ~round(ifelse(is.na(.), 0, .), 2)),
                      across(where(~!is.numeric(.x)), ~ifelse(is.na(.x), "", .x)))
    })
    
    output$po_by_company <- renderDT({
      req(po_company_monthly())
      tbl <- po_company_monthly()
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(datatable(data.frame(Message = "No data available"), options = list(dom = "t"), rownames = FALSE))
      }
      col_names     <- colnames(tbl)
      month_pattern <- "^(SAP|Manual|EOC|Total|Integration_Pct)_(\\d{4})-(\\d{2})$"
      display_names <- vapply(col_names, function(name) {
        match <- str_match(name, month_pattern)
        if (!is.na(match[1, 1])) {
          metric <- dplyr::recode(
            match[1, 2],
            SAP = "SAP",
            Manual = "Manual",
            EOC = "EOC",
            Total = "Total",
            Integration_Pct = "Integration %"
          )
          month_label <- format(as.Date(sprintf("%s-%s-01", match[1, 3], match[1, 4])), "%b %Y")
          return(paste(month_label, metric))
        }

        dplyr::recode(
          name,
          "PID-CID" = "Company ID",
          "Client Name" = "Company Name",
          "SAP_Total" = "Overall SAP",
          "Manual_Total" = "Overall Manual",
          "EOC_Total" = "Overall EOC",
          "Total_Total" = "Overall Total",
          "Integration_Pct_Total" = "Overall Integration %",
          .default = name
        )
      }, character(1))
      colnames(tbl) <- make.unique(display_names, sep = " ")
      numeric_targets <- which(str_detect(col_names, "^(SAP|Manual|EOC|Total)_(\\d{4}-\\d{2}|Total)$")) - 1L
      pct_targets <- which(str_detect(col_names, "^Integration_Pct_(\\d{4}-\\d{2}|Total)$")) - 1L
      company_targets <- 0:1
      datatable(
        tbl, rownames = FALSE,
        escape = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = list("copy", "csv", "excel", "pdf", "print"),
          columnDefs = list(
            list(
              targets = numeric_targets,
              className = "dt-right",
              render = JS("function(data, type) { if (type === 'display' && data !== null && data !== '') { var val = Number(data); return Number.isFinite(val) ? val.toLocaleString('en-IN') : data; } return data; }")
            ),
            list(
              targets = pct_targets,
              className = "dt-center",
              render = JS("function(data, type) { if (type === 'display' && data !== null && data !== '') { var val = Number(data); return Number.isFinite(val) ? val.toFixed(1) + '%' : data; } return data; }"),
              createdCell = JS("function(td, cellData) { var val = Number(cellData); if (!Number.isFinite(val)) return; td.style.fontWeight = '700'; td.style.borderRadius = '6px'; if (val >= 85) { td.style.background = '#dcfce7'; td.style.color = '#166534'; } else if (val >= 70) { td.style.background = '#ffedd5'; td.style.color = '#9a3412'; } else { td.style.background = '#fee2e2'; td.style.color = '#b91c1c'; } }")
            ),
            list(
              targets = company_targets,
              className = "dt-left"
            )
          )
        ),
        caption = htmltools::tags$caption(
          style = "caption-side:top;text-align:left;font-weight:600;font-size:12px;padding:12px 16px;color:#0e1d28;",
          HTML(paste0("<strong>📊 Line Items by Company & Month</strong><br>",
                      "<span style='color:#64748b;font-size:11px;'>",
                      "Each month shows: SAP | Manual | EOC | Total | Integration %. ",
                      "Overall columns appear at the end of the table. ",
                      "Integration %: Green ≥85% | Orange 70–85% | Red <70%",
                      "</span>"))
        ),
        callback = JS(
          "table.on('draw.dt', function() {",
          "$(table.table().header()).css({'background':'#f8fafc','color':'#475569'});",
          "$(table.table().body()).find('tr').each(function(){",
          "  $(this).find('td:eq(0), td:eq(1)').css({'font-weight':'700','color':'#0f172a'});",
          "});",
          "});"
        )
      )
    })

    output$po_value_by_company <- renderDT({
      req(po_value_company_monthly())
      tbl <- po_value_company_monthly()
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(datatable(data.frame(Message = "No value data available"), options = list(dom = "t"), rownames = FALSE))
      }
      col_names <- colnames(tbl)
      month_pattern <- "^(SAP_Value_Cr|Manual_Value_Cr|EOC_Value_Cr|Total_Value_Cr|Value_Integration_Pct)_(\\d{4})-(\\d{2})$"
      display_names <- vapply(col_names, function(name) {
        match <- str_match(name, month_pattern)
        if (!is.na(match[1, 1])) {
          metric <- dplyr::recode(
            match[1, 2],
            SAP_Value_Cr = "SAP Value Cr",
            Manual_Value_Cr = "Manual Value Cr",
            EOC_Value_Cr = "EOC Value Cr",
            Total_Value_Cr = "Total Value Cr",
            Value_Integration_Pct = "Value Integration %"
          )
          month_label <- format(as.Date(sprintf("%s-%s-01", match[1, 3], match[1, 4])), "%b %Y")
          return(paste(month_label, metric))
        }
        dplyr::recode(
          name,
          "PID-CID" = "Company ID",
          "Client Name" = "Company Name",
          "SAP_Value_Cr_Total" = "Overall SAP Value Cr",
          "Manual_Value_Cr_Total" = "Overall Manual Value Cr",
          "EOC_Value_Cr_Total" = "Overall EOC Value Cr",
          "Total_Value_Cr_Total" = "Overall Total Value Cr",
          "Value_Integration_Pct_Total" = "Overall Value Integration %",
          .default = name
        )
      }, character(1))
      colnames(tbl) <- make.unique(display_names, sep = " ")
      value_targets <- which(str_detect(col_names, "^(SAP_Value_Cr|Manual_Value_Cr|EOC_Value_Cr|Total_Value_Cr)_(\\d{4}-\\d{2}|Total)$")) - 1L
      pct_targets <- which(str_detect(col_names, "^Value_Integration_Pct_(\\d{4}-\\d{2}|Total)$")) - 1L
      datatable(
        tbl, rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = list("copy", "csv", "excel", "pdf", "print"),
          columnDefs = list(
            list(
              targets = value_targets,
              className = "dt-right",
              render = JS("function(data, type) { if (type === 'display' && data !== null && data !== '') { var val = Number(data); return Number.isFinite(val) ? val.toLocaleString('en-IN', {minimumFractionDigits:2, maximumFractionDigits:2}) : data; } return data; }")
            ),
            list(
              targets = pct_targets,
              className = "dt-center",
              render = JS("function(data, type) { if (type === 'display' && data !== null && data !== '') { var val = Number(data); return Number.isFinite(val) ? val.toFixed(1) + '%' : data; } return data; }"),
              createdCell = JS("function(td, cellData) { var val = Number(cellData); if (!Number.isFinite(val)) return; td.style.fontWeight = '700'; td.style.borderRadius = '6px'; if (val >= 85) { td.style.background = '#dcfce7'; td.style.color = '#166534'; } else if (val >= 70) { td.style.background = '#ffedd5'; td.style.color = '#9a3412'; } else { td.style.background = '#fee2e2'; td.style.color = '#b91c1c'; } }")
            ),
            list(targets = 0:1, className = "dt-left")
          )
        ),
        caption = htmltools::tags$caption(
          style = "caption-side:top;text-align:left;font-weight:600;font-size:12px;padding:12px 16px;color:#0e1d28;",
          HTML(paste0("<strong>PO Value by Company & Month</strong><br>",
                      "<span style='color:#64748b;font-size:11px;'>",
                      "Values are in crores. Value Integration % = (SAP value + Manual value) / Total value.",
                      "</span>"))
        )
      )
    })
    
    mom_integration_chart <- reactive({
      req(consider_df())
      df <- consider_df() %>% filter(`PID-CID` != "9802")
      if (nrow(df) == 0) return(NULL)
      df %>%
        group_by(Month) %>%
        summarise(SAP = sum(source == "SAP", na.rm = TRUE),
                  Manual = sum(source == "Manual", na.rm = TRUE),
                  Total = n(), .groups = "drop") %>%
        arrange(Month) %>%
        mutate(Integration_Pct = ifelse(Total > 0, round((SAP + Manual) / Total * 100, 1), 0),
               Month_Label = format(as.Date(paste0(Month, "-01")), "%b %Y"))
    })
    
    output$mom_integration_chart <- renderPlot({
      req(mom_integration_chart())
      data <- mom_integration_chart()
      ggplot(data, aes(x = factor(Month_Label, levels = Month_Label), y = Integration_Pct, group = 1)) +
        geom_line(color = "#006495", size = 1.2, lineend = "round") +
        geom_point(aes(fill = Integration_Pct), color = "#006495", size = 4, shape = 21, stroke = 2) +
        scale_fill_gradientn(colors = c("#fecaca", "#fed7aa", "#d1fae5"),
                             values = scales::rescale(c(0, 70, 85, 100)), limits = c(0, 100), guide = "none") +
        geom_text(aes(label = paste0(Integration_Pct, "%")), vjust = -1.5, size = 3.5, color = "#0e1d28", fontface = "bold") +
        geom_hline(yintercept = 85, linetype = "dashed", color = "#15803d", size = 0.7, alpha = 0.5) +
        geom_hline(yintercept = 70, linetype = "dashed", color = "#dc2626", size = 0.7, alpha = 0.5) +
        annotate("text", x = 0.5, y = 85, label = "Target (85%)",  size = 3, color = "#15803d", hjust = 0, vjust = -0.5) +
        annotate("text", x = 0.5, y = 70, label = "Minimum (70%)", size = 3, color = "#dc2626", hjust = 0, vjust = -0.5) +
        scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%")) +
        labs(title = "Month-on-Month Integration % Progress", subtitle = "Trend over selected date range",
             x = "Month", y = "Integration %", caption = "Green ≥85% | Orange 70–85% | Red <70%") +
        theme_minimal() +
        theme(plot.title       = element_text(size = 14, face = "bold", color = "#0e1d28", family = "Plus Jakarta Sans"),
              plot.subtitle    = element_text(size = 11, color = "#64748b", family = "Manrope"),
              axis.title       = element_text(size = 10, face = "bold", color = "#0e1d28", family = "Manrope"),
              axis.text        = element_text(size = 9,  color = "#475569", family = "Manrope"),
              axis.text.x      = element_text(angle = 45, hjust = 1),
              panel.grid.major.y = element_line(color = "#e2e8f0", size = 0.3),
              panel.grid.minor   = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.caption     = element_text(size = 9, color = "#94a3b8", family = "Manrope"))
    }, bg = "white", width = 700, height = 400)
    
    output$po_by_month <- renderDT({
      req(consider_df())
      if (!nrow(consider_df())) return(datatable(data.frame(Message = "No valid line items"), options = list(dom = "t")))
      tbl <- consider_df() %>% filter(`PID-CID` != "9802") %>%
        group_by(Month) %>%
        summarise(`Line items` = n(), Companies = n_distinct(`PID-CID`), .groups = "drop") %>%
        arrange(Month) %>%
        mutate(Prev = lag(`Line items`),
               `MoM Growth` = ifelse(!is.na(Prev) & Prev > 0,
                                     paste0(round((`Line items` - Prev) / Prev * 100, 1), "%"), "-")) %>%
        select(Month, `Line items`, Companies, `MoM Growth`)
      datatable(tbl, rownames = FALSE,
                options = list(pageLength = 15, dom = "t",
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("Line items",
                    background = styleColorBar(range(tbl$`Line items`), "#dbeafe"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
        formatStyle(columns = colnames(tbl), fontSize = "13px")
    })
    
    # Downloads
    dl <- function(fn, ...) downloadHandler(filename = fn, content = ...)
    output$dl_pivot_without <- dl(function() paste0("pivot_without_", Sys.Date(), ".csv"),
                                  function(f) write.csv(memo_pivot(filtered_df() %>% filter(`PID-CID` != "9802")), f, row.names = FALSE, na = ""))
    output$dl_pivot_with    <- dl(function() paste0("pivot_with_", Sys.Date(), ".csv"),
                                  function(f) write.csv(memo_pivot(filtered_df()), f, row.names = FALSE, na = ""))
    output$dl_value_pivot_without <- dl(function() paste0("value_pivot_without_", Sys.Date(), ".csv"),
                                        function(f) write.csv(memo_value_pivot(filtered_df() %>% filter(`PID-CID` != "9802")), f, row.names = FALSE, na = ""))
    output$dl_value_pivot_with    <- dl(function() paste0("value_pivot_with_", Sys.Date(), ".csv"),
                                        function(f) write.csv(memo_value_pivot(filtered_df()), f, row.names = FALSE, na = ""))
    output$dl_raw           <- dl(function() paste0("raw_data_", Sys.Date(), ".csv"),
                                  function(f) write.csv(filtered_df(), f, row.names = FALSE, na = ""))
    output$dl_grn_company   <- dl(function() paste0("grn_company_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn_company_tbl(), f, row.names = FALSE))
    output$dl_grn_month     <- dl(function() paste0("grn_month_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn_month_tbl(), f, row.names = FALSE))
    output$dl_grn_detail    <- dl(function() paste0("grn_detail_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn_df(), f, row.names = FALSE))
    output$dl_grn2_po       <- dl(function() paste0("grn2_po_relationship_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn2_po_tbl(), f, row.names = FALSE))
    output$dl_grn2_month    <- dl(function() paste0("grn2_month_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn2_month_tbl(), f, row.names = FALSE))
    output$dl_po_company    <- dl(function() paste0("po_company_monthly_", Sys.Date(), ".xlsx"),
                                  function(f) {
                                    tbl <- po_company_monthly()
                                    tryCatch(
                                      write_mom_company_workbook(tbl, f),
                                      error = function(e) {
                                        wb <- createWorkbook()
                                        addWorksheet(wb, "Export Error")
                                        writeData(wb, "Export Error", data.frame(
                                          Message = "ProcureGraph could not build the MoM workbook.",
                                          Detail = e$message
                                        ))
                                        saveWorkbook(wb, f, overwrite = TRUE)
                                      }
                                    )
                                  })
    output$dl_po_value_company <- dl(function() paste0("po_value_company_monthly_", Sys.Date(), ".csv"),
                                     function(f) write.csv(po_value_company_monthly(), f, row.names = FALSE, na = ""))
    output$dl_po_month      <- dl(function() paste0("po_month_", Sys.Date(), ".csv"),
                                  function(f) {
                                    tbl <- consider_df() %>% filter(`PID-CID` != "9802") %>%
                                      group_by(Month) %>% summarise(Line_items = n(), Companies = n_distinct(`PID-CID`), .groups = "drop")
                                    write.csv(tbl, f, row.names = FALSE)
                                  })
    output$dl_po_detail     <- dl(function() paste0("po_detail_", Sys.Date(), ".csv"),
                                  function(f) write.csv(consider_df() %>% filter(`PID-CID` != "9802"), f, row.names = FALSE))

    output$dl_license_request <- downloadHandler(
      filename = function() paste0("procuregraph_license_request_", format(Sys.Date(), "%Y%m%d"), ".json"),
      content = function(file) {
        payload <- list(
          app = "ProcureGraph",
          requested_tier = "pro",
          current_tier = license$tier,
          customer_name = license$customer,
          generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
          machine_hint = Sys.info()[["nodename"]],
          contact = license$contact
        )
        writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE), file)
      }
    )

    output$dl_executive_pack <- downloadHandler(
      filename = function() paste0("procuregraph_executive_pack_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = function(file) {
        req(pro_enabled(), consider_df(), company_performance())
        df <- consider_df()
        perf <- company_performance()

        total_valid <- nrow(df)
        integrated <- sum(df$source %in% c("SAP", "Manual"), na.rm = TRUE)
        eoc <- sum(df$source == "EOC", na.rm = TRUE)
        integration_pct <- if (total_valid > 0) round(integrated / total_valid * 100, 1) else 0

        monthly <- df %>%
          group_by(Month) %>%
          summarise(
            Total = n(),
            Integrated = sum(source %in% c("SAP", "Manual"), na.rm = TRUE),
            EOC = sum(source == "EOC", na.rm = TRUE),
            Companies = n_distinct(`PID-CID`[`PID-CID` != "Unknown"]),
            .groups = "drop"
          ) %>%
          mutate(Integration_Pct = ifelse(Total > 0, round(Integrated / Total * 100, 1), 0)) %>%
          arrange(Month)

        opportunities <- perf %>%
          mutate(
            Manual_Opportunity = pmax(Grand_Total - Integrated_Volume, 0),
            Opportunity_Score = round((100 - Integration_Pct) * log1p(Grand_Total), 1),
            Priority = case_when(
              Integration_Pct < 50 & Grand_Total >= 25 ~ "Critical",
              Integration_Pct < 70 ~ "High",
              Integration_Pct < 85 ~ "Medium",
              TRUE ~ "Maintain"
            )
          ) %>%
          arrange(desc(Opportunity_Score), desc(Grand_Total))

        summary <- data.frame(
          Metric = c("Total valid line items", "Integrated line items", "EOC / manual opportunity", "Integration %", "Active companies", "Critical accounts"),
          Value = c(
            format(total_valid, big.mark = ","),
            format(integrated, big.mark = ","),
            format(eoc, big.mark = ","),
            paste0(integration_pct, "%"),
            format(nrow(perf), big.mark = ","),
            format(sum(opportunities$Priority == "Critical", na.rm = TRUE), big.mark = ",")
          )
        )

        actions <- opportunities %>%
          filter(Priority %in% c("Critical", "High")) %>%
          transmute(
            `PID-CID`, `Client Name`, Priority,
            `Integration %` = Integration_Pct,
            `Total Line Items` = Grand_Total,
            `Unintegrated Opportunity` = Manual_Opportunity,
            `Recommended Action` = ifelse(
              Priority == "Critical",
              "Schedule integration recovery call and assign owner this week",
              "Move to next onboarding sprint and monitor weekly"
            )
          ) %>%
          slice_head(n = 50)

        wb <- createWorkbook()
        addWorksheet(wb, "Executive Summary", gridLines = FALSE)
        addWorksheet(wb, "Monthly Trend", gridLines = FALSE)
        addWorksheet(wb, "Opportunity Pipeline", gridLines = FALSE)
        addWorksheet(wb, "Action Plan", gridLines = FALSE)

        title_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#0E1D28", textDecoration = "bold", fontSize = 16)
        header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#006495", textDecoration = "bold", halign = "center")
        risk_style <- createStyle(fontColour = "#7F1D1D", fgFill = "#FEE2E2", textDecoration = "bold")
        ok_style <- createStyle(fontColour = "#166534", fgFill = "#DCFCE7", textDecoration = "bold")

        writeData(wb, "Executive Summary", "ProcureGraph Pro Executive Pack", startRow = 1, startCol = 1)
        addStyle(wb, "Executive Summary", title_style, rows = 1, cols = 1:2, gridExpand = TRUE)
        writeData(wb, "Executive Summary", paste0("Generated: ", format(Sys.time(), "%d %b %Y %H:%M")), startRow = 2, startCol = 1)
        writeDataTable(wb, "Executive Summary", summary, startRow = 4, startCol = 1, tableStyle = "TableStyleMedium2")
        setColWidths(wb, "Executive Summary", cols = 1:2, widths = c(30, 24))

        writeDataTable(wb, "Monthly Trend", monthly, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium2")
        writeDataTable(wb, "Opportunity Pipeline", opportunities, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium4")
        writeDataTable(wb, "Action Plan", actions, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium9")

        sheet_cols <- list(
          "Monthly Trend" = max(1, ncol(monthly)),
          "Opportunity Pipeline" = max(1, ncol(opportunities)),
          "Action Plan" = max(1, ncol(actions))
        )
        for (sheet in names(sheet_cols)) {
          addStyle(wb, sheet, header_style, rows = 1, cols = seq_len(sheet_cols[[sheet]]), gridExpand = TRUE, stack = TRUE)
          freezePane(wb, sheet, firstActiveRow = 2)
          setColWidths(wb, sheet, cols = 1:20, widths = "auto")
        }

        if (nrow(opportunities)) {
          priority_col <- which(names(opportunities) == "Priority")
          if (length(priority_col)) {
            conditionalFormatting(wb, "Opportunity Pipeline", cols = priority_col, rows = 2:(nrow(opportunities) + 1), rule = '=="Critical"', style = risk_style)
            conditionalFormatting(wb, "Opportunity Pipeline", cols = priority_col, rows = 2:(nrow(opportunities) + 1), rule = '=="Maintain"', style = ok_style)
          }
        }

        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    output$last_refreshed <- renderUI({
      req(refresh_time())
      tags$span(class = "flex items-center gap-2 text-[10px] font-bold uppercase tracking-widest", style = "color:#94a3b8;",
                tags$span(class = "material-symbols-outlined text-sm", "update"),
                format(refresh_time(), "%H:%M:%S"))
    })

    output$license_badge <- renderUI(NULL)
    
    # ── EMAIL ────────────────────────────────────────────────────────────────
    outlook_smtp <- list(host = "smtp.office365.com", port = 587)

    email_log_file <- function() {
      log_dir <- file.path(getwd(), "logs")
      if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      file.path(log_dir, "email_smtp.log")
    }

    sanitize_smtp_output <- function(lines, password = "") {
      lines <- as.character(lines)
      if (nzchar(password)) {
        lines <- gsub(password, "[REDACTED_PASSWORD]", lines, fixed = TRUE)
      }
      lines <- gsub("(?i)(AUTH\\s+[^[:space:]]+\\s+).*$", "\\1[REDACTED]", lines, perl = TRUE)
      lines <- gsub("^>[[:space:]]*[A-Za-z0-9+/=]{12,}[[:space:]]*$", "> [REDACTED_AUTH_PAYLOAD]", lines)
      lines
    }

    append_email_log <- function(context, from, to, exit_code, output_lines) {
      log_path <- email_log_file()
      entry <- c(
        paste0("----- ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), " -----"),
        paste0("Context: ", context),
        paste0("From: ", from),
        paste0("To: ", to),
        paste0("Curl exit: ", exit_code),
        "Output:",
        if (length(output_lines)) output_lines else "<no output>",
        ""
      )
      cat(paste(entry, collapse = "\n"), "\n", file = log_path, append = TRUE)
      log_path
    }

    summarize_smtp_output <- function(output_lines) {
      clean <- trimws(output_lines)
      clean <- clean[nzchar(clean)]
      if (!length(clean)) return("No SMTP details returned by curl.")
      tail(clean, 1)
    }

    send_smtp_email <- function(raw_message, smtp, username, password, mail_from, mail_to, context) {
      tmp <- tempfile(fileext = ".eml")
      con <- file(tmp, open = "wb")
      tryCatch({
        writeBin(charToRaw(raw_message), con)
      }, finally = {
        close(con)
      })
      on.exit(unlink(tmp), add = TRUE)

      args <- c(
        "-sS", "--show-error",
        "--connect-timeout", "15",
        "--max-time", "90",
        "--url", sprintf("smtp://%s:%d", smtp$host, smtp$port),
        "--ssl-reqd",
        "--user", paste0(username, ":", password),
        "--mail-from", mail_from,
        "--mail-rcpt", mail_to,
        "--upload-file", tmp
      )

      stdout_file <- tempfile()
      stderr_file <- tempfile()
      on.exit(unlink(c(stdout_file, stderr_file)), add = TRUE)

      exit_code <- tryCatch(
        system2("curl", args = args, stdout = stdout_file, stderr = stderr_file),
        error = function(e) {
          writeLines(e$message, stderr_file)
          127
        }
      )
      output <- c(
        readLines(stdout_file, warn = FALSE),
        readLines(stderr_file, warn = FALSE)
      )
      output <- sanitize_smtp_output(output, password)
      log_path <- append_email_log(context, mail_from, mail_to, exit_code, output)

      list(
        ok = exit_code == 0,
        exit_code = exit_code,
        output = output,
        summary = summarize_smtp_output(output),
        log_path = log_path
      )
    }
    
    build_html_email <- function() {
      df            <- consider_df()
      total_valid   <- nrow(df)
      integrated_df <- df %>% filter(`Final Status` %in% c("Live", "Live - CBB", "PR Int. - Punchout"))
      integrated_orders <- nrow(integrated_df)
      integrated_pct    <- if (total_valid > 0) round(integrated_orders / total_valid * 100, 1) else 0
      
      pv_wo <- create_pivot(df %>% filter(`PID-CID` != "9802")); m_wo <- calc_metrics(pv_wo)
      pv_wi <- create_pivot(df);                                  m_wi <- calc_metrics(pv_wi)
      
      comp_data <- df %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(SAP = sum(source == "SAP", na.rm = TRUE),
                  Manual = sum(source == "Manual", na.rm = TRUE),
                  EOC = sum(source == "EOC", na.rm = TRUE), .groups = "drop") %>%
        mutate(Grand_Total = SAP + Manual + EOC,
               Integration_Pct = ifelse(Grand_Total > 0, round((SAP + Manual) / Grand_Total * 100, 1), 0),
               Integrated_Volume = SAP + Manual) %>%
        filter(Grand_Total > 0)
      
      top5 <- comp_data %>% arrange(desc(Integrated_Volume)) %>% slice_head(n = 5)
      no_int <- df %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown", `Final Status` == "No Integration") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(SAP = sum(source == "SAP", na.rm = TRUE), Orders = n(), .groups = "drop") %>%
        arrange(desc(Orders)) %>% slice_head(n = 5)
      
      make_rows <- function(tbl, pct_col, vol_col, empty_msg) {
        if (!nrow(tbl)) return(sprintf('<tr><td colspan="4" style="padding:16px;text-align:center;color:#94a3b8;">%s</td></table>', empty_msg))
        paste(sapply(seq_len(nrow(tbl)), function(i)
          sprintf('<tr>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;color:#94a3b8;font-size:12px;">%d</td>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;font-weight:600;">%s</td>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;text-align:center;font-weight:700;color:#0e1d28;">%s</td>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;text-align:right;color:#64748b;">%s</td>
            </tr>',
                  i, tbl$`Client Name`[i], tbl[[pct_col]][i], format(tbl[[vol_col]][i], big.mark = ","))),
          collapse = "")
      }
      
      top5_rows   <- make_rows(top5, "Integration_Pct", "Integrated_Volume", "No data")
      no_int_rows <- if (nrow(no_int) > 0) make_rows(no_int, "Orders", "SAP", "None")
      else '<tr><td colspan="4" style="padding:16px;text-align:center;color:#16a34a;font-weight:700;">No companies on No Integration status!</td></tr>'
      
      po_accounts  <- tryCatch(df %>% filter(source %in% c("SAP","Manual")) %>% distinct(`PID-CID`) %>% nrow(), error = function(e) 0)
      grn_accounts <- tryCatch(grn_df() %>% distinct(`PID-CID`) %>% nrow(), error = function(e) 0)
      grn_line <- tryCatch({
        g <- grn_df()
        sap_grn <- df %>% filter(source == "SAP", GRN == "Y", `PID-CID` != "9802") %>% nrow()
        rate <- if (sap_grn > 0) paste0(round(nrow(g) / sap_grn * 100, 1), "%") else "N/A"
        paste0(format(nrow(g), big.mark = ","), " GRNs raised | Match Rate: ", rate)
      }, error = function(e) "GRN data not available")
      date_rng <- paste0(format(as.Date(input$date_filter_start), "%d %b %Y"),
                         " to ", format(as.Date(input$date_filter_end), "%d %b %Y"))
      
      paste0('<!DOCTYPE html><html><head><meta charset="UTF-8">
<style>
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;background:#f6faff;margin:0;padding:20px;}
.container{max-width:700px;margin:0 auto;background:#fff;border-radius:24px;box-shadow:0 20px 35px -10px rgba(0,0,0,.05);overflow:hidden;border:1px solid #eef2f6;}
.header{background:#f8fafc;border-bottom:2px solid #da191e;padding:32px 28px;text-align:center;}
.header h1{margin:0;font-size:28px;font-weight:800;color:#0e1d28;letter-spacing:-.5px;}
.header p{margin:8px 0 0;font-size:14px;color:#475569;}
.content{padding:28px;}
.kpi-grid{display:flex;gap:16px;margin-bottom:28px;flex-wrap:wrap;}
.kpi-card{flex:1;background:#f8fafc;border-radius:20px;padding:20px;text-align:center;border:1px solid #eef2f6;}
.kpi-label{font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:#64748b;margin-bottom:8px;}
.kpi-value{font-size:30px;font-weight:800;color:#0e1d28;line-height:1.2;}
.kpi-sub{font-size:11px;color:#64748b;margin-top:6px;}
.section-title{font-size:16px;font-weight:700;margin:24px 0 12px;padding-bottom:8px;border-bottom:2px solid #da191e;display:inline-block;}
.metrics-row{display:flex;gap:16px;background:#f8fafc;border-radius:16px;padding:16px;margin-bottom:20px;flex-wrap:wrap;}
.metric-box{flex:1;text-align:center;}
.metric-label{font-size:11px;font-weight:600;color:#475569;text-transform:uppercase;}
.metric-value{font-size:22px;font-weight:800;color:#0e1d28;}
table{width:100%;border-collapse:collapse;margin:12px 0;font-size:13px;}
th{background:#f1f5f9;padding:10px 8px;text-align:left;font-weight:700;color:#1e293b;font-size:11px;text-transform:uppercase;letter-spacing:.04em;}
.accounts-box{background:#f0f9ff;border-radius:16px;padding:16px;margin:16px 0;display:flex;justify-content:space-around;text-align:center;}
.badge{background:#e6f7ec;color:#0e6b2e;font-size:12px;font-weight:600;padding:4px 12px;border-radius:30px;display:inline-block;}
.footer{background:#f8fafc;padding:20px 28px;text-align:center;font-size:12px;color:#1e293b;border-top:1px solid #eef2f6;}
</style></head><body>
<div class="container">
  <div class="header"><h1>&#128202; ProcureGraph</h1><p>Buyers Integration Intelligence &mdash; Moglix</p></div>
  <div class="content">
    <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;flex-wrap:wrap;">
      <span class="badge">&#128197; ', date_rng, '</span>
      <span style="font-size:12px;color:#64748b;">Generated: ', format(Sys.time(), "%d %b %Y %H:%M"), '</span>
    </div>
    <div class="kpi-grid">
      <div class="kpi-card"><div class="kpi-label">Total Valid Line items</div>
        <div class="kpi-value">', format(total_valid, big.mark = ","), '</div>
        <div class="kpi-sub">Remove(Y/N) = Consider</div></div>
      <div class="kpi-card"><div class="kpi-label">Line items of Integrated Customers</div>
        <div class="kpi-value" style="color:#10b981;">', format(integrated_orders, big.mark = ","), '</div>
        <div class="kpi-sub">', integrated_pct, '% of total valid</div></div>
    </div>
    <div class="section-title">&#128200; Integration Metrics</div>
    <div class="metrics-row">
      <div class="metric-box"><div class="metric-label">Without ABFRL</div>
        <div class="metric-value">', m_wo$integration, '</div>
        <div style="font-size:12px;color:#64748b;">Integration %</div>
        <div style="font-size:11px;color:#94a3b8;">CBB: ', m_wo$cbb, ' &nbsp;|&nbsp; Final: ', m_wo$final, '</div></div>
      <div style="width:1px;background:#e2e8f0;"></div>
      <div class="metric-box"><div class="metric-label">With ABFRL</div>
        <div class="metric-value">', m_wi$integration, '</div>
        <div style="font-size:12px;color:#64748b;">Integration %</div>
        <div style="font-size:11px;color:#94a3b8;">CBB: ', m_wi$cbb, ' &nbsp;|&nbsp; Final: ', m_wi$final, '</div></div>
    </div>
    <div class="section-title">&#127942; Top 5 by Integrated Volume</div>
    <table><thead><tr><th>#</th><th>Company</th><th>Integration %</th><th style="text-align:right;">Integrated Line items</th></tr></thead>
      <tbody>', top5_rows, '</tbody></table>
    <div class="section-title">&#127919; No Integration — Target List</div>
    <p style="font-size:12px;color:#64748b;margin:0 0 8px;">Highest-volume companies still on No Integration — prioritise for onboarding</p>
    <table><thead><tr><th>#</th><th>Company</th><th>Total Line items</th><th style="text-align:right;">SAP Line items</th></tr></thead>
      <tbody>', no_int_rows, '</tbody></table>
    <div class="accounts-box">
      <div><div style="font-size:20px;font-weight:800;color:#006495;">', po_accounts, '</div>
        <div style="font-size:11px;font-weight:600;color:#64748b;text-transform:uppercase;letter-spacing:.04em;">PO Integration Accounts</div></div>
      <div style="width:1px;background:#bae6fd;"></div>
      <div><div style="font-size:20px;font-weight:800;color:#16a34a;">', grn_accounts, '</div>
        <div style="font-size:11px;font-weight:600;color:#64748b;text-transform:uppercase;letter-spacing:.04em;">GRN Integration Accounts</div></div>
    </div>
    <div class="section-title">&#128230; GRN Summary</div>
    <div style="background:#f0fdf4;border-radius:12px;padding:14px;margin:12px 0;font-weight:600;color:#15803d;">', grn_line, '</div>
    <p style="font-size:11px;color:#94a3b8;margin-top:16px;">Integration % = (SAP+Manual) / Total Line items &times; 100. Only line items where Remove(Y/N)=Consider, ABFRL excluded.</p>
  </div>
</div></body></html>')
    }
    
    observeEvent(input$open_email_modal, {
      showModal(modalDialog(
        title = tags$div(class = "flex items-center gap-2",
                         tags$span(class = "material-symbols-outlined", style = "color:#da191e;", "mail"),
                         "Send Beautiful Email Report (Outlook only)"),
        size = "l", easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("send_email_btn", "Send Email",
                       style = "background:#da191e;color:white;border:none;font-weight:700;padding:8px 20px;border-radius:8px;cursor:pointer;margin-left:8px;")),
        tags$div(class = "space-y-4",
                 tags$div(
                   tags$p(class = "text-xs font-bold uppercase tracking-widest mb-2", style = "color:#64748b;", "Outlook / Office 365 Configuration"),
                   tags$p(class = "text-xs", style = "color:#64748b;", "SMTP: smtp.office365.com:587 with TLS")),
                 tags$div(class = "grid grid-cols-2 gap-3",
                          tags$div(tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "Your Outlook Email (sender)"),
                                   textInput("email_from", "", placeholder = "you@company.com", width = "100%")),
                          tags$div(tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "Recipient Email"),
                                   textInput("email_to", "", placeholder = "manager@company.com", width = "100%"))),
                 tags$div(tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "App Password"),
                          passwordInput("email_pass", "", placeholder = "16-char app password (from Outlook/Office 365)", width = "100%")),
                 uiOutput("email_help_box"),
                 tags$div(
                   tags$p(class = "text-xs font-bold uppercase tracking-widest mb-2", style = "color:#64748b;", "Email Preview"),
                   tags$div(style = "border:1px solid #e2e8f0;border-radius:8px;overflow:hidden;",
                            uiOutput("email_preview_html"))))
      ))
    })
    
    output$email_help_box <- renderUI({
      tags$div(class = "p-4 rounded-xl", style = "background:#eff6ff;border:1px solid #bfdbfe;",
               tags$p(class = "text-xs font-bold", style = "color:#1e40af;", "Outlook / Office 365 App Password — step by step"),
               tags$ol(class = "text-xs mt-2 space-y-1", style = "color:#1e3a8a;list-style:decimal;padding-left:16px;",
                       tags$li(HTML("Go to <b>portal.office.com</b> → My Account → Security info")),
                       tags$li(HTML("Click <b>Add method</b> → App password → name it ProcureGraph")),
                       tags$li(HTML("Copy the generated password — paste above")),
                       tags$li(HTML("Use your <b>full company email</b> as sender (e.g. you@moglix.com)")),
                       tags$li(HTML("If your organisation uses on-premise Exchange, ask IT for the SMTP relay host"))),
               tags$p(class = "text-[10px] mt-2", style = "color:#1e40af;", "smtp.office365.com | Port 587 | TLS — auto"))
    })
    
    output$email_preview_html <- renderUI({
      req(consider_df())
      html_content <- tryCatch(build_html_email(),
                               error = function(e) "<p style='padding:16px;color:#94a3b8;'>Load data first to preview.</p>")
      tags$iframe(srcdoc = html_content, style = "width:100%;height:260px;border:none;")
    })
    
    observeEvent(input$send_email_btn, {
      req(consider_df())
      to   <- trimws(input$email_to   %||% "")
      from <- trimws(input$email_from %||% "")
      pass <- input$email_pass %||% ""
      if (!nchar(to) || !nchar(from) || !nchar(pass)) { showNotification("Please fill in all fields", type = "error"); return() }
      if (!grepl("@", to) || !grepl("@", from)) { showNotification("Enter valid email addresses", type = "error"); return() }
      tryCatch({
        html_body <- build_html_email()
        date_rng  <- paste0(format(as.Date(input$date_filter_start), "%d %b %Y"), " to ",
                            format(as.Date(input$date_filter_end), "%d %b %Y"))
        subject   <- paste0("ProcureGraph Insights — ", date_rng)
        raw_msg   <- paste0(
          "From: ProcureGraph <", from, ">\r\n",
          "To: ", to, "\r\n",
          "Subject: ", subject, "\r\n",
          "MIME-Version: 1.0\r\n",
          "Content-Type: text/html; charset=UTF-8\r\n",
          "Content-Transfer-Encoding: 8bit\r\n\r\n",
          html_body, "\r\n"
        )
        smtp_result <- send_smtp_email(raw_msg, outlook_smtp, from, pass, from, to, "standard-insights")
        removeModal()
        if (smtp_result$ok) {
          showNotification(paste0("Email sent to ", to), type = "message", duration = 5)
        } else {
          showNotification(
            paste0(
              "Email failed (curl exit ", smtp_result$exit_code, "): ",
              smtp_result$summary,
              " | Log: ", smtp_result$log_path
            ),
            type = "error",
            duration = 12
          )
        }
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error", duration = 8))
    })
    
    # ── TIERED POC EMAIL ALERTS ──────────────────────────────────────────────
    get_performance_tier <- function(integration_pct, volume, total) {
      if (integration_pct >= 85) "excellent"
      else if (integration_pct >= 70) "good"
      else "critical"
    }
    
    build_poc_email_html <- function(company_name, integration_pct, volume, total,
                                     date_range_start, date_range_end, performance_tier) {
      if (performance_tier == "excellent") {
        header_bg <- "#f0fdf4"; header_border <- "#15803d"; header_color <- "#15803d"
        title <- "\u2728 Integration Performance Update"; subtitle <- "Great progress! Keep it up."; icon <- "\U0001F31F"
        tone <- "Your integration performance is <strong>excellent</strong>. You're at <strong>%s%%</strong> integration rate. This demonstrates strong operational efficiency and commitment to automation."
        actions <- c("Continue maintaining this excellent integration rate", "Consider expanding to additional buyers")
      } else if (performance_tier == "good") {
        header_bg <- "#fff7ed"; header_border <- "#f97316"; header_color <- "#92400e"
        title <- "\u26a0\ufe0f Integration Performance Review"; subtitle <- "Performance is trending down. Let's work together."; icon <- "\U0001F4CA"
        tone <- "Your integration performance is at <strong>%s%%</strong>, which is below our target of 85%%. While you're above the minimum threshold (70%%), there's room for improvement. We recommend reviewing your integration setup with our technical team."
        actions <- c("Schedule a meeting with your Buyers technical team")
      } else {
        header_bg <- "#fecaca"; header_border <- "#dc2626"; header_color <- "#dc2626"
        title <- "\U0001F6A8 Integration Performance Alert - Urgent Action Required"
        subtitle <- "Critical attention needed. Your integration rate has dropped significantly."; icon <- "\u26a1"
        tone <- "Your integration performance has dropped to <strong>%s%%</strong>, which is below our acceptable threshold (70%%). Only %s of your %s line items are integrated. This requires immediate attention to restore operational efficiency."
        actions <- c("Contact Moglix technical team immediately to discuss the failing integration")
      }
      manual_items <- total - volume
      manual_pct   <- if (total > 0) round(manual_items / total * 100, 1) else 0
      tone_filled  <- sprintf(tone, integration_pct, volume, total, manual_pct)
      actions_html <- paste(sprintf("<li style=\"margin:8px 0;color:%s;\">%s</li>", header_color, actions), collapse = "")
      sprintf('<!DOCTYPE html><html><head><meta charset="UTF-8">
<style>
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif;background:#f6faff;margin:0;padding:20px;}
.container{max-width:650px;margin:0 auto;background:#fff;border-radius:16px;box-shadow:0 10px 25px rgba(0,0,0,0.1);overflow:hidden;border:1px solid #eef2f6;}
.header{background:%s;border-bottom:3px solid %s;padding:32px 28px;text-align:center;}
.header h1{margin:0;font-size:26px;font-weight:800;color:%s;}
.header p{margin:8px 0 0;font-size:14px;color:%s;opacity:0.9;}
.content{padding:28px;}
.metric-grid{display:grid;grid-template-columns:1fr 1fr 1fr;gap:12px;margin:20px 0;}
.metric{background:#f8fafc;border:1px solid #e2e8f0;border-radius:12px;padding:16px;text-align:center;}
.metric-label{font-size:10px;color:#64748b;text-transform:uppercase;font-weight:700;letter-spacing:0.05em;margin-bottom:8px;}
.metric-value{font-size:26px;font-weight:800;color:%s;margin:8px 0;}
.tone-box{background:%s;border-left:4px solid %s;padding:16px;border-radius:8px;margin:20px 0;line-height:1.6;color:#0e1d28;}
.action-box{background:%s;border:1px solid %s;border-radius:12px;padding:20px;margin-top:20px;}
.action-box h3{color:%s;margin-top:0;margin-bottom:12px;}
.footer{background:#f8fafc;padding:16px;text-align:center;font-size:11px;color:#1e293b;border-top:1px solid #eef2f6;}
</style></head><body>
<div class="container">
  <div class="header"><h1>%s %s</h1><p>%s</p></div>
  <div class="content">
    <p>Hi,</p>
    <div class="tone-box">%s</div>
    <div class="metric-grid">
      <div class="metric"><div class="metric-label">Integrated Items</div><div class="metric-value">%s</div></div>
      <div class="metric"><div class="metric-label">Total Items</div><div class="metric-value">%s</div></div>
      <div class="metric"><div class="metric-label">Integration %%</div><div class="metric-value" style="color:%s;">%s%%</div></div>
    </div>
    <h3 style="color:#0e1d28;margin-top:24px;">\U0001F3AF Recommended Actions</h3>
    <div class="action-box"><ul style="margin:0;padding-left:20px;">%s</ul></div>
    <p style="margin-top:24px;padding:16px;background:#f8fafc;border-radius:8px;font-size:12px;color:#475569;">
      <strong>Report Period:</strong> %s to %s<br>
      <strong>Generated:</strong> %s<br>
      <strong>Company:</strong> %s
    </p>
  </div>
  <div class="footer">ProcureGraph Buyers Integration Intel | Moglix Procurement<br>This is an automated report. Do not reply directly.</div>
</div></body></html>',
              header_bg, header_border, header_color, header_color,
              header_color, header_bg, header_border, header_bg, header_border, header_color,
              icon, title, subtitle, tone_filled,
              format(volume, big.mark = ","), format(total, big.mark = ","), header_color, integration_pct,
              actions_html,
              format(as.Date(date_range_start), "%d %b %Y"), format(as.Date(date_range_end), "%d %b %Y"),
              format(Sys.time(), "%d %b %Y %H:%M"), company_name)
    }
    
    observeEvent(input$open_email_modal_poc, {
      req(company_performance(), company_data())
      companies_with_poc <- company_performance() %>%
        left_join(company_data()$company %>% select(companyId, BusinessPOC), by = c("PID-CID" = "companyId"))
      has_poc <- companies_with_poc %>% filter(!is.na(BusinessPOC), nchar(trimws(BusinessPOC)) > 0)
      if (nrow(has_poc) == 0) {
        showNotification("No companies with Business POC emails found. Add BusinessPOC column to Excel mapping.", type = "warning", duration = 6)
        return()
      }
      showModal(modalDialog(
        title = tags$div(class = "flex items-center gap-2",
                         tags$span(class = "material-symbols-outlined", style = "color:#f97316;", "warning"),
                         "Send Integration Performance Alerts (Outlook)"),
        size = "l", easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("send_poc_emails_btn", "Send to All POCs",
                       style = "background:#f97316;color:white;border:none;font-weight:700;padding:10px 20px;border-radius:8px;cursor:pointer;margin-left:8px;")),
        tags$div(class = "space-y-4",
                 tags$div(class = "p-4 rounded-xl", style = "background:#fff7ed;border:1px solid #fed7aa;",
                          tags$span(class = "material-symbols-outlined text-sm", style = "color:#f97316;vertical-align:middle;", "info"),
                          tags$span(style = "font-size:12px;color:#92400e;font-weight:600;margin-left:8px;",
                                    paste0(" About to send alerts to ", nrow(has_poc), " Business POC(s) via Outlook SMTP"))),
                 tags$div(class = "grid grid-cols-2 gap-3",
                          tags$div(tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "Your Outlook Email"),
                                   textInput("poc_email_from", "", placeholder = "your.email@moglix.com", width = "100%")),
                          tags$div(tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "Provider"),
                                   tags$span("Outlook / Office 365", style = "font-size:12px;color:#1e40af;background:#eff6ff;padding:6px 12px;border-radius:6px;display:inline-block;width:100%;"))),
                 tags$div(tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "App Password"),
                          passwordInput("poc_email_pass", "", placeholder = "16-char app password (from Outlook/Office 365)", width = "100%")),
                 tags$div(class = "p-4 rounded-xl", style = "background:#f0f9ff;border:1px solid #bfdbfe;",
                          tags$p(class = "text-xs font-bold mb-3", style = "color:#1e40af;", "Recipients"),
                          tags$div(class = "space-y-2 max-h-64 overflow-y-auto",
                                   lapply(seq_len(nrow(has_poc)), function(i) {
                                     d <- has_poc[i, ]
                                     tags$div(class = "flex items-center justify-between p-2 rounded-lg",
                                              style = "background:white;border:1px solid #dbeafe;",
                                              tags$div(
                                                tags$p(class = "text-xs font-bold", style = "color:#0c4a6e;", d$`Client Name`),
                                                tags$p(class = "text-[10px]", style = "color:#64748b;", d$BusinessPOC)),
                                              tags$div(class = "text-right",
                                                       tags$div(class = "text-xs font-bold", style = "color:#16a34a;",
                                                                paste0(format(d$Integrated_Volume, big.mark = ","), " items")),
                                                       tags$div(class = "text-[10px]", style = "color:#94a3b8;",
                                                                paste0(d$Integration_Pct, "%"))))
                                   }))))
      ))
    })
    
    observeEvent(input$send_poc_emails_btn, {
      req(company_performance(), company_data())
      sender_addr  <- trimws(input$poc_email_from %||% "")
      app_password <- input$poc_email_pass %||% ""
      if (!nchar(sender_addr) || !nchar(app_password)) { showNotification("Please fill sender email and app password", type = "error", duration = 5); return() }
      if (!grepl("@", sender_addr)) { showNotification("Invalid email address", type = "error", duration = 5); return() }
      
      recipients_df <- company_performance() %>%
        left_join(company_data()$company %>% select(companyId, BusinessPOC), by = c("PID-CID" = "companyId")) %>%
        filter(!is.na(BusinessPOC), nchar(trimws(BusinessPOC)) > 0)
      
      if (nrow(recipients_df) == 0) { showNotification("No recipients with valid emails", type = "warning"); removeModal(); return() }
      
      withProgress(message = "Sending tiered emails via Outlook...", value = 0, {
        total_recipients <- nrow(recipients_df)
        sent_count       <- 0
        failed_list      <- c()
        
        for (i in seq_len(total_recipients)) {
          setProgress(i / total_recipients, detail = paste0("Sending to ", recipients_df$`Client Name`[i], "..."))
          recipient_email <- trimws(recipients_df$BusinessPOC[i])
          company_name    <- recipients_df$`Client Name`[i]
          integration_pct <- recipients_df$Integration_Pct[i]
          volume          <- recipients_df$Integrated_Volume[i]
          total           <- recipients_df$Grand_Total[i]
          
          if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", recipient_email)) {
            failed_list <- c(failed_list, paste0(company_name, " (invalid: ", recipient_email, ")")); next
          }
          
          tier      <- get_performance_tier(integration_pct, volume, total)
          html_body <- build_poc_email_html(company_name, integration_pct, volume, total,
                                            input$date_filter_start, input$date_filter_end, tier)
          
          tryCatch({
            subject_prefix <- if (tier == "excellent") "\u2728 Integration Performance Update"
            else if (tier == "good") "\u26a0\ufe0f Integration Review Required"
            else "\U0001F6A8 URGENT: Integration Performance Alert"
            
            subject <- paste0(subject_prefix, " - ", company_name, " | ", format(Sys.Date(), "%d %b %Y"))
            raw_message <- paste0(
              "From: ProcureGraph <", sender_addr, ">\r\n",
              "To: ", recipient_email, "\r\n",
              "Subject: ", subject, "\r\n",
              "MIME-Version: 1.0\r\n",
              "Content-Type: text/html; charset=UTF-8\r\n",
              "Content-Transfer-Encoding: 8bit\r\n",
              "X-Mailer: ProcureGraph/v6.1\r\n\r\n",
              html_body, "\r\n"
            )
            smtp_result <- send_smtp_email(
              raw_message, outlook_smtp, sender_addr, app_password,
              sender_addr, recipient_email, paste0("poc-alert:", company_name)
            )
            if (smtp_result$ok) {
              sent_count <- sent_count + 1
            } else {
              failed_list <- c(
                failed_list,
                paste0(company_name, " (curl ", smtp_result$exit_code, ": ", smtp_result$summary, ")")
              )
            }
          }, error = function(e) { failed_list <<- c(failed_list, paste0(company_name, " (exception: ", e$message, ")")) })
        }
        setProgress(1)
      })
      
      removeModal()
      summary_msg <- paste0("\u2713 Sent to ", sent_count, " POC(s)")
      if (length(failed_list) > 0)
        summary_msg <- paste0(summary_msg, "\n\u2717 Failed (", length(failed_list), "): ",
                              paste(failed_list[1:min(3, length(failed_list))], collapse = ", "))
      showNotification(HTML(gsub("\n", "<br>", summary_msg)),
                       type = if (sent_count > 0) "message" else "error", duration = 10)
    })
    
    # ── Formula reference ────────────────────────────────────────────────────
    formula_row <- function(col, src, desc) {
      sc <- switch(src, MongoDB = "#e0f0ff", Calculated = "#f0fdf4", "#fff7ed")
      st <- switch(src, MongoDB = "#006495", Calculated = "#16a34a", "#ea580c")
      tags$div(class = "flex items-start gap-2 p-2 rounded-lg", style = "background:#f8faff;",
               tags$span(style = paste0("background:", sc, ";color:", st, ";font-size:9px;font-weight:700;padding:1px 6px;border-radius:4px;white-space:nowrap;min-width:60px;text-align:center;"), src),
               tags$div(tags$p(class = "font-bold", style = "color:#0e1d28;", col),
                        tags$p(style = "color:#64748b;", desc)))
    }
    
    shinyjs::hide("formulasCard")
    observeEvent(input$toggle_formulas, {
      shinyjs::toggle("formulasCard")
      updateActionButton(session, "toggle_formulas",
                         icon = icon(if (input$toggle_formulas %% 2 == 1) "minus" else "plus"))
    })
    
    # ═══════════════════════════════════════════════════════════════════════════
    # 9. PAGE ROUTER
    # ═══════════════════════════════════════════════════════════════════════════
    output$dashboard_content <- renderUI({
      page <- current_page()
      
      pivot_card <- function(title, subtitle, dt_id, dl_id, btn_color = "#006495") {
        tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER,
                 tags$div(class = "flex justify-between items-center px-5 pt-5 pb-3",
                          style = "border-bottom:1px solid #f1f5f9;",
                          tags$div(tags$h4(class = "text-sm font-extrabold", title),
                                   tags$p(class = "text-[10px] mt-0.5", style = "color:#94a3b8;", subtitle)),
                          downloadButton(dl_id,
                                         HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;CSV'),
                                         style = BTN(btn_color, "padding:6px 12px;border-radius:6px;font-size:10px;"))),
                 tags$div(class = "overflow-x-auto", DTOutput(dt_id)))
      }
      
      section_hdr <- function(title, subtitle, dl_id, btn_color = "#16a34a", btn_label = "CSV") {
        tags$div(class = "flex justify-between items-center",
                 tags$div(tags$h3(class = "text-lg font-extrabold", title),
                          tags$p(class = "text-xs mt-1", style = "color:#64748b;", subtitle)),
                 if (!is.null(dl_id))
                   downloadButton(dl_id,
                                  HTML(paste0('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;', btn_label)),
                                  style = BTN(btn_color))
                 else NULL)
      }
      
      kpi_card_inline <- function(icon_n, label, value_str, fg, bg_light, prog_pct = NULL, sub_str = NULL, anim = "") {
        tags$div(class = paste(CARD, "p-6 relative overflow-hidden anim-in", anim), style = BORDER,
                 tags$div(class = "absolute top-0 right-0 p-5 opacity-5",
                          tags$span(class = "material-symbols-outlined text-7xl", style = paste0("color:", fg, ";"), icon_n)),
                 tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-1", style = "color:#94a3b8;", label),
                 tags$h3(class = "text-4xl font-extrabold",
                         style = paste0("font-family:'Plus Jakarta Sans',sans-serif;color:", fg, ";"), value_str),
                 if (!is.null(prog_pct))
                   tags$div(class = "mt-3 h-1.5 rounded-full overflow-hidden", style = paste0("background:", bg_light, ";"),
                            tags$div(class = "h-full rounded-full", style = paste0("width:", prog_pct, "%;background:", fg, ";"))),
                 if (!is.null(sub_str))
                   tags$p(class = "text-[10px] font-bold uppercase mt-2", style = paste0("color:", fg, ";"), sub_str))
      }
      
      # ── DASHBOARD ──────────────────────────────────────────────────────────
      if (page == "dashboard") {
        df <- tryCatch(filtered_df(), error = function(e) NULL)
        total_all   <- if (!is.null(df)) nrow(df) else 0
        valid_n     <- if (!is.null(df)) sum(df$`Remove(Y/N)` == "Consider", na.rm = TRUE) else 0
        valid_pct   <- if (total_all > 0) round(valid_n / total_all * 100, 1) else 0
        df_consider <- if (!is.null(df)) df %>% filter(`Remove(Y/N)` == "Consider") else data.frame()
        integrated_customers_df <- if (!is.null(df_consider) && nrow(df_consider) > 0)
          df_consider %>% filter(`Final Status` %in% c("Live", "Live - CBB", "PR Int. - Punchout"))
        else data.frame()
        integrated_customers_lines <- nrow(integrated_customers_df)
        integrated_items_live <- if (!is.null(integrated_customers_df) && nrow(integrated_customers_df) > 0)
          integrated_customers_df %>% filter(source %in% c("SAP", "Manual")) %>% nrow()
        else 0
        live_customers_pct <- if (integrated_customers_lines > 0) round(integrated_items_live / integrated_customers_lines * 100, 1) else 0
        
        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$section(class = "grid grid-cols-4 gap-6",
                              kpi_card_inline("list",     "Total line items",                          format(total_all, big.mark = ","),                   "#006495", "#e0f0ff", 100,              NULL,                                                           "anim-in-1"),
                              kpi_card_inline("verified", "Valid line items",                          format(valid_n, big.mark = ","),                     "#f97316", "#ffedd5", valid_pct,         paste0(valid_pct, "% of total"),                                "anim-in-2"),
                              kpi_card_inline("business", "Line items of integrated customers",        format(integrated_customers_lines, big.mark = ","),   "#10b981", "#d1fae5", if(integrated_customers_lines>0) round(integrated_customers_lines/valid_n*100,1) else 0, paste0(if(valid_n>0) round(integrated_customers_lines/valid_n*100,1) else 0,"% of valid line items"), "anim-in-3"),
                              kpi_card_inline("sync_alt", "Integrated items of integrated customers",  format(integrated_items_live, big.mark = ","),         "#da191e", "#ffe0e0", live_customers_pct, paste0(live_customers_pct, "% of integrated lines"),          "anim-in-4")),
                 tags$section(class = "grid grid-cols-2 gap-8 anim-in anim-in-4",
                              tags$div(class = paste(CARD, "p-6"), style = BORDER,
                                       tags$div(class = "flex items-center gap-2 mb-1",
                                                tags$span(class = "material-symbols-outlined text-lg", style = "color:#006495;", "emoji_events"),
                                                tags$h3(class = "text-base font-extrabold", "Top 5 by Integrated Volume")),
                                       tags$p(class = "text-xs mb-4", style = "color:#94a3b8;", "Bar width = integrated line items · Label = Integration % · Integrated / Total"),
                                       uiOutput("top5_chart")),
                              tags$div(class = paste(CARD, "p-6"), style = BORDER,
                                       tags$div(class = "flex items-center gap-2 mb-1",
                                                tags$span(class = "material-symbols-outlined text-lg", style = "color:#da191e;", "gps_fixed"),
                                                tags$h3(class = "text-base font-extrabold", "No Integration — Target List")),
                                       tags$p(class = "text-xs mb-4", style = "color:#94a3b8;", "Highest-volume companies still on No Integration — prioritise for onboarding"),
                                       uiOutput("no_integration_chart"))),
                 tags$div(class = paste(CARD, "p-6"), style = BORDER,
                          tags$div(class = "flex items-center gap-2 mb-4",
                                   tags$span(class = "material-symbols-outlined", style = "color:#006495;", "functions"),
                                   tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Metric Formulas")),
                          tags$div(class = "grid grid-cols-3 gap-4",
                                   tags$div(class = "p-4 rounded-xl", style = "background:#f0f9ff;",
                                            tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#006495;", "Integration %"),
                                            tags$code(class = "text-xs font-bold", "SAP \u00f7 Grand Total \u00d7 100"),
                                            tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", "SAP line items as % of all valid line items")),
                                   tags$div(class = "p-4 rounded-xl", style = "background:#f0fdf4;",
                                            tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#16a34a;", "CBB %"),
                                            tags$code(class = "text-xs font-bold", "Manual \u00f7 Grand Total \u00d7 100"),
                                            tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", "Manual line items as % of all valid line items")),
                                   tags$div(class = "p-4 rounded-xl", style = "background:#fff7ed;",
                                            tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#ea580c;", "Final Integration %"),
                                            tags$code(class = "text-xs font-bold", "(SAP+Manual) \u00f7 Grand Total \u00d7 100"),
                                            tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", "Overall integration effectiveness")))),
                 tags$section(class = "space-y-4 anim-in anim-in-5",
                              tags$h2(class = "text-2xl font-extrabold", "Integration Pivot"),
                              tags$div(class = "grid grid-cols-2 gap-6",
                                       pivot_card("Without ABFRL", "Excl. Aditya Birla Fashion", "pivot_without_table", "dl_pivot_without"),
                                       pivot_card("With ABFRL",    "All companies included",     "pivot_with_table",    "dl_pivot_with"))),
                 tags$section(class = "space-y-4 anim-in anim-in-5",
                              tags$div(
                                tags$h2(class = "text-2xl font-extrabold", "Integration Value Pivot"),
                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                       "Values are summed from amount and shown in crores. Percentages use value instead of line-item counts.")
                              ),
                              tags$div(class = "grid grid-cols-2 gap-6",
                                       pivot_card("Without ABFRL - Value", "Excl. Aditya Birla Fashion; values in Cr", "value_pivot_without_table", "dl_value_pivot_without", "#16a34a"),
                                       pivot_card("With ABFRL - Value",    "All companies included; values in Cr",     "value_pivot_with_table",    "dl_value_pivot_with", "#16a34a"))))
        
        # ── PO INTEGRATION ─────────────────────────────────────────────────────
      } else if (page == "po") {
        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$div(class = "flex justify-between items-end",
                          tags$div(tags$h2(class = "text-2xl font-extrabold", "PO Integration Analysis"),
                                   tags$p(class = "text-sm mt-1", style = "color:#64748b;", "Valid line items (Consider only) — ABFRL excluded")),
                          tags$div(class = "flex items-center gap-2 px-4 py-2 rounded-xl",
                                   style = "background:#f0fdf4;border:1px solid #bbf7d0;",
                                   tags$span(class = "material-symbols-outlined text-sm", style = "color:#16a34a;", "info"),
                                   tags$span(style = "font-size:11px;color:#15803d;font-weight:600;", "Remove(Y/N) = Consider only"))),
                 tags$section(class = "grid grid-cols-2 gap-6",
                              uiOutput("companies_by_status_card"),
                              uiOutput("integration_rate_card")),
                 tags$section(class = "space-y-3",
                              section_hdr("Line items by Company (Month-on-Month)", "Each month block shows SAP, Manual, EOC, Total, Integration %. Final columns are overall totals.", "dl_po_company", "#16a34a", "Excel"),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("po_by_company"))),
                 tags$section(class = "space-y-3",
                              section_hdr("PO Value by Company (Month-on-Month)", "Each month block shows SAP, Manual, EOC, Total value in crores and Value Integration %.", "dl_po_value_company", "#16a34a"),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("po_value_by_company"))),
                 tags$section(class = "space-y-3",
                              section_hdr("Line items by Month", "MoM Growth = (Current-Prev)/Prev \u00d7 100", "dl_po_month", "#006495"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("po_by_month"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "Integration % Trend (Month-on-Month)"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;", "Track integration performance changes over time")),
                                       NULL),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER,
                                       plotOutput("mom_integration_chart", height = "400px"))),
                 tags$div(class = "flex justify-end",
                          downloadButton("dl_po_detail",
                                         HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT ALL VALID LINE ITEMS'),
                                         style = BTN("#64748b"))))
        
        # ── GRN ────────────────────────────────────────────────────────────────
      } else if (page == "grn") {
        kpi3 <- function(ic, lb, vid, fg, bg) {
          tags$div(class = paste(CARD, "p-6 relative overflow-hidden"), style = BORDER,
                   tags$div(class = "absolute top-0 right-0 p-5 opacity-5",
                            tags$span(class = "material-symbols-outlined text-7xl", style = paste0("color:", fg, ";"), ic)),
                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-1", style = "color:#94a3b8;", lb),
                   tags$h3(class = "text-4xl font-extrabold",
                           style = paste0("font-family:'Plus Jakarta Sans',sans-serif;color:", fg, ";"), uiOutput(vid, inline = TRUE)),
                   tags$div(class = "mt-3 h-1.5 rounded-full", style = paste0("background:", bg, ";"),
                            tags$div(class = "h-full rounded-full", style = paste0("width:100%;background:", fg, ";"))))
        }
        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$div(class = "flex justify-between items-end",
                          tags$div(tags$h2(class = "text-2xl font-extrabold", "GRN Integration"),
                                   tags$p(class = "text-sm mt-1", style = "color:#64748b;", "GRN = Y companies; PO volume uses all PO integrated accounts - ABFRL excluded")),
                          tags$div(class = "flex items-center gap-2 px-4 py-2 rounded-xl",
                                   style = "background:#f0fdf4;border:1px solid #bbf7d0;",
                                   tags$span(class = "material-symbols-outlined text-sm", style = "color:#16a34a;", "info"),
                                   tags$span(style = "font-size:11px;color:#15803d;font-weight:600;", "GRN Status = Y in Excel mapping only"))),
                 tags$section(class = "grid grid-cols-3 gap-6",
                              kpi3("receipt_long", "Total GRNs",             "grn_total_val",      "#16a34a", "#d1fae5"),
                              kpi3("business",     "Companies Raising GRNs", "grn_companies_val",  "#006495", "#e0f0ff"),
                              kpi3("percent",      "Overall GRN %",          "grn_match_rate_val", "#f59e0b", "#fef3c7")),
                 tags$section(class = "grid grid-cols-3 gap-6",
                              kpi3("verified",     "GRN Eligible Accounts",  "grn_eligible_accounts_val", "#006495", "#e0f0ff"),
                              kpi3("inventory_2",  "Accounts Giving PO Volume", "grn_po_accounts_val", "#b45309", "#fffbeb"),
                              kpi3("list",         "Valid PO Line Items",    "grn_po_line_items_val", "#da191e", "#ffe0e0")),
                 tags$section(class = "space-y-3",
                              section_hdr("GRN Integration by Company", "GRN % = GRN Records / valid PO line items for PO integrated accounts", "dl_grn_company"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn_by_company"))),
                 tags$section(class = "space-y-3",
                              section_hdr("GRN Integration by Month", "Eligible accounts, PO volume accounts, GRN accounts, line items and monthly GRN %", "dl_grn_month", "#006495"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn_by_month"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "GRN Detail Records"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;", "All GRN transactions from procurement.grn")),
                                       downloadButton("dl_grn_detail",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT ALL'),
                                                      style = BTN("#da191e"))),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn_detail_table"))))
        
        # ── RAW REPORTS ────────────────────────────────────────────────────────
      } else if (page == "grn2") {
        kpi_grn2 <- function(ic, lb, vid, fg, bg) {
          tags$div(class = paste(CARD, "p-6 relative overflow-hidden"), style = BORDER,
                   tags$div(class = "absolute top-0 right-0 p-5 opacity-5",
                            tags$span(class = "material-symbols-outlined text-7xl", style = paste0("color:", fg, ";"), ic)),
                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-1", style = "color:#94a3b8;", lb),
                   tags$h3(class = "text-4xl font-extrabold",
                           style = paste0("font-family:'Plus Jakarta Sans',sans-serif;color:", fg, ";"), uiOutput(vid, inline = TRUE)),
                   tags$div(class = "mt-3 h-1.5 rounded-full", style = paste0("background:", bg, ";"),
                            tags$div(class = "h-full rounded-full", style = paste0("width:100%;background:", fg, ";"))))
        }

        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$div(class = "flex justify-between items-end",
                          tags$div(tags$h2(class = "text-2xl font-extrabold", "GRN 2.0"),
                                   tags$p(class = "text-sm mt-1", style = "color:#64748b;",
                                          "PO-level GRN relationship analysis using poId from GRN records")),
                          tags$div(class = "flex items-center gap-2 px-4 py-2 rounded-xl",
                                   style = "background:#eff6ff;border:1px solid #bfdbfe;",
                                   tags$span(class = "material-symbols-outlined text-sm", style = "color:#006495;", "account_tree"),
                                   tags$span(style = "font-size:11px;color:#075985;font-weight:600;",
                                             "One PO can have one or multiple GRN numbers"))),
                 tags$section(class = "grid grid-cols-4 gap-6",
                              kpi_grn2("receipt_long", "POs With GRNs", "grn2_total_po_val", "#006495", "#e0f0ff"),
                              kpi_grn2("looks_one", "Single-GRN POs", "grn2_single_po_val", "#16a34a", "#d1fae5"),
                              kpi_grn2("stack", "Multi-GRN POs", "grn2_multi_po_val", "#b45309", "#fffbeb"),
                              kpi_grn2("functions", "Avg GRNs / PO", "grn2_avg_val", "#da191e", "#ffe0e0")),
                 tags$section(class = "space-y-3",
                              section_hdr("GRNs Related to Each PO", "Grouped by poId; Distinct GRNs counts unique customerGrnNo values and GRN Records counts transaction rows.", "dl_grn2_po", "#006495"),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("grn2_po_table"))),
                 tags$section(class = "space-y-3",
                              section_hdr("GRN 2.0 by Month", "Monthly count of POs with GRNs, single-GRN POs, multi-GRN POs and average GRNs per PO.", "dl_grn2_month", "#16a34a"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn2_month_table"))))

      } else if (page == "removed") {
        kpi_removed <- function(ic, lb, vid, fg, bg, sub_id = NULL) {
          tags$div(class = paste(CARD, "p-6 relative overflow-hidden"), style = BORDER,
                   tags$div(class = "absolute top-0 right-0 p-5 opacity-5",
                            tags$span(class = "material-symbols-outlined text-7xl", style = paste0("color:", fg, ";"), ic)),
                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-1", style = "color:#94a3b8;", lb),
                   tags$h3(class = "text-4xl font-extrabold",
                           style = paste0("font-family:'Plus Jakarta Sans',sans-serif;color:", fg, ";"), uiOutput(vid, inline = TRUE)),
                   if (!is.null(sub_id))
                     tags$p(class = "text-[10px] font-bold uppercase mt-2", style = paste0("color:", fg, ";"), uiOutput(sub_id, inline = TRUE)),
                   tags$div(class = "mt-3 h-1.5 rounded-full", style = paste0("background:", bg, ";"),
                            tags$div(class = "h-full rounded-full", style = paste0("width:100%;background:", fg, ";"))))
        }

        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$div(class = "flex justify-between items-end",
                          tags$div(tags$h2(class = "text-2xl font-extrabold", "Removed Data Opportunity"),
                                   tags$p(class = "text-sm mt-1", style = "color:#64748b;",
                                          "All removed Buyer IDs, with PunchIn volume highlighted to spot possible missed opportunity")),
                          tags$div(class = "flex items-center gap-2 px-4 py-2 rounded-xl",
                                   style = "background:#fef2f2;border:1px solid #fecaca;",
                                   tags$span(class = "material-symbols-outlined text-sm", style = "color:#b91c1c;", "warning"),
                                   tags$span(style = "font-size:11px;color:#b91c1c;font-weight:600;", "Remove(Y/N) = Remove only"))),
                 tags$section(class = "grid grid-cols-3 gap-6",
                              kpi_removed("production_quantity_limits", "Total removed rows", "removed_total_val", "#b45309", "#fffbeb"),
                              kpi_removed("hub", "Removed PunchIn records", "removed_punchin_val", "#da191e", "#ffe0e0"),
                              kpi_removed("person_search", "Largest buyer ID", "removed_top_buyer_val", "#006495", "#e0f0ff", "removed_top_buyer_count_val")),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "Missing PID-CID Opportunity"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                                       "Buyer IDs not available in PID-CID mapping, including latest-month growth signal")),
                                       downloadButton("dl_missing_pidcid",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT MISSING PID-CID'),
                                                      style = BTN("#006495"))),
                              tags$div(class = "grid grid-cols-3 gap-6",
                                       kpi_removed("person_off", "Unmapped buyer IDs", "missing_pidcid_buyer_val", "#006495", "#e0f0ff"),
                                       kpi_removed("format_list_numbered", "Unmapped records", "missing_pidcid_records_val", "#b45309", "#fffbeb"),
                                       kpi_removed("trending_up", "Increasing buyer IDs", "missing_pidcid_increasing_val", "#da191e", "#ffe0e0")),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("missing_pidcid_table"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "Removed Data MoM by Buyer ID"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                                       "Detect short-term buyer spikes versus recurring or continuously rising removed accounts")),
                                       downloadButton("dl_removed_mom",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT MOM'),
                                                      style = BTN("#b45309"))),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("removed_mom_table"))),
                 tags$section(class = "grid grid-cols-2 gap-6",
                              tags$div(class = paste(CARD, "p-6"), style = BORDER,
                                       tags$div(class = "flex items-center gap-2 mb-1",
                                                tags$span(class = "material-symbols-outlined text-lg", style = "color:#da191e;", "leaderboard"),
                                                tags$h3(class = "text-base font-extrabold", "All Removed Buyer IDs by Volume")),
                                       tags$p(class = "text-xs mb-4", style = "color:#94a3b8;",
                                              "Every removed Buyer ID is included; label shows total removed records and PunchIn records"),
                                       uiOutput("removed_top_buyer_chart")),
                              tags$div(class = paste(CARD, "p-6"), style = BORDER,
                                       tags$div(class = "flex items-center gap-2 mb-1",
                                                tags$span(class = "material-symbols-outlined text-lg", style = "color:#b45309;", "rule"),
                                                tags$h3(class = "text-base font-extrabold", "Why Rows Were Removed")),
                                       tags$p(class = "text-xs mb-4", style = "color:#94a3b8;",
                                              "First matching removal reason across source, stage, month, status, GRN and special rules"),
                                       DTOutput("removed_reason_table"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "Buyer ID Opportunity Table"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                                       "All removed Buyer IDs sorted by total removed records, with PunchIn records shown separately")),
                                       downloadButton("dl_removed_buyer",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT BUYERS'),
                                                      style = BTN("#da191e"))),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("removed_buyer_table"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "Removed Detail Records"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                                       "Line-level records behind the removed Buyer ID ranking")),
                                       downloadButton("dl_removed_detail",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT DETAIL'),
                                                      style = BTN("#64748b"))),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("removed_detail_table"))))

      } else if (page == "reports") {
        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$div(class = paste(CARD, "p-6"), style = BORDER,
                          tags$div(class = "flex justify-between items-center mb-4",
                                   tags$div(class = "flex items-center gap-2",
                                            tags$span(class = "material-symbols-outlined", style = "color:#da191e;", "table_chart"),
                                            tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Column Reference & Formulas")),
                                   actionButton("toggle_formulas", label = NULL, icon = icon("plus"), class = "btn btn-sm",
                                                style = "background:none;border:none;font-size:18px;cursor:pointer;")),
                          div(id = "formulasCard",
                              tags$div(class = "grid grid-cols-2 gap-3", style = "font-size:11px;",
                                       tags$div(class = "space-y-2",
                                                formula_row("A: _id",                          "MongoDB",    "Unique document identifier"),
                                                formula_row("B: buyer.id",                     "MongoDB",    "Plant/buyer ID (flattened via $project)"),
                                                formula_row("C: history.po.customerPoNo",      "MongoDB",    "Customer PO number (flattened via $project)"),
                                                formula_row("D: history.po.customerSANumber",  "MongoDB",    "Customer SA number (flattened via $project)"),
                                                formula_row("E: status",                       "MongoDB",    "Order status"),
                                                formula_row("F: creationDate",                 "MongoDB",    "Creation epoch (ms)"),
                                                formula_row("G-I: source / stage",             "MongoDB",    "Source and processing stage"),
                                                formula_row("J: CreationDate",                 "Calculated", "epoch / 1000 \u2192 readable date"),
                                                formula_row("K: Month",                        "Calculated", "format(CreationDate,'%Y-%m') — reuses J")),
                                       tags$div(class = "space-y-2",
                                                formula_row("L: Consider Month",               "Calculated", "Consider if in date range else Ignore"),
                                                formula_row("M: PID-CID",                      "Excel Join", "buyer.id \u2192 plantId \u2192 companyId"),
                                                formula_row("N: Client Name",                  "Excel Join", "companyName via companyId join"),
                                                formula_row("O: Consider Source",              "Calculated", "Consider if source in {EOC,Manual,SAP}"),
                                                formula_row("P: Consider Stage",               "Calculated", "Ignore if stage in {Closed,Cancelled}"),
                                                formula_row("Q: Final Status",                 "Excel Join", "Integration Status; NA \u2192 Ignore"),
                                                formula_row("R: RemoveHavellsCapex",           "Calculated", "PID-CID=1211: Ignore if SANumber starts 45"),
                                                formula_row("S: RemoveSC",                     "Calculated", "PID-CID=8853+buyers: Ignore if SANumber starts 71"),
                                                formula_row("T: Remove(Y/N)",                  "Calculated", "Remove if ANY of O,P,L,Q,R,S,U = Ignore"),
                                                formula_row("U: GRN",                          "Excel Join", "GRN Status from 150 CID; NA \u2192 Ignore"))),
                              tags$div(class = "flex gap-4 mt-4 pt-4", style = "border-top:1px solid #f1f5f9;",
                                       lapply(list(
                                         list("MongoDB",    "#e0f0ff", "#006495", "Direct from MongoDB"),
                                         list("Calculated", "#f0fdf4", "#16a34a", "Derived in R"),
                                         list("Excel Join", "#fff7ed", "#ea580c", "From Excel mapping")),
                                         function(x) tags$div(class = "flex items-center gap-1",
                                                              tags$span(style = paste0("background:", x[[2]], ";color:", x[[3]], ";font-size:9px;font-weight:700;padding:1px 8px;border-radius:4px;"), x[[1]]),
                                                              tags$span(style = "font-size:10px;color:#64748b;", x[[4]])))))),
                 tags$section(class = "space-y-4",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h2(class = "text-xl font-extrabold", "Raw Data"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                                       "All columns A-U — green=Consider, red=Remove")),
                                       downloadButton("dl_raw",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT CSV'),
                                                      style = BTN("#da191e", "padding:10px 18px;font-size:11px;border-radius:8px;"))),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("raw_table"))))
        
        # ── ADVANCED ANALYTICS ─────────────────────────────────────────────────
      } else if (page == "commercial") {
        active <- pro_enabled()
        status_color <- if (active) "#166534" else "#9a3412"
        status_bg <- if (active) "#f0fdf4" else "#fff7ed"
        status_border <- if (active) "#bbf7d0" else "#fed7aa"

        feature_row <- function(icon, title, text, locked = FALSE) {
          tags$div(class = "flex items-start gap-3 p-4 rounded-xl",
                   style = "background:#f8fafc;border:1px solid #eef2f7;",
                   tags$span(class = "material-symbols-outlined", style = paste0("color:", if (locked && !active) "#94a3b8" else "#006495", ";"), icon),
                   tags$div(
                     tags$p(class = "text-sm font-extrabold mb-1", style = "color:#0e1d28;", title),
                     tags$p(class = "text-xs mb-0", style = "color:#64748b;line-height:1.6;", text)
                   ),
                   if (locked && !active)
                     tags$span(class = "ml-auto text-[10px] font-bold uppercase px-2 py-1 rounded-full",
                               style = "background:#fee2e2;color:#991b1b;", "Pro")
                   else NULL)
        }

        price_card <- function(name, price, body, accent, primary = FALSE) {
          tags$div(class = "p-6 rounded-2xl",
                   style = paste0("background:white;border:2px solid ", if (primary) accent else "#eef2f7", ";"),
                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = paste0("color:", accent, ";"), name),
                   tags$div(class = "flex items-end gap-1 mb-3",
                            tags$span(class = "text-4xl font-extrabold", style = "color:#0e1d28;", price),
                            tags$span(class = "text-xs mb-2", style = "color:#64748b;", "/ month")),
                   tags$p(class = "text-sm", style = "color:#475569;line-height:1.7;", body))
        }

        tags$div(class = "p-8 space-y-8",
                 tags$section(class = "grid grid-cols-3 gap-6",
                              tags$div(class = "col-span-2 p-8 rounded-2xl",
                                       style = "background:#0e1d28;color:white;",
                                       tags$div(class = "flex items-center gap-2 mb-4",
                                                tags$span(class = "material-symbols-outlined", "workspace_premium"),
                                                tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-0", style = "color:#94a3b8;", "Commercial edition")),
                                       tags$h2(class = "text-3xl font-extrabold mb-3", "Sell ProcureGraph as a paid procurement intelligence product"),
                                       tags$p(class = "text-sm mb-5", style = "color:#cbd5e1;max-width:760px;line-height:1.8;",
                                              "This page turns the app from an internal dashboard into a commercial package with activation, buyer-facing pricing, and an executive workbook export."),
                                       tags$div(class = "flex gap-3",
                                                downloadButton("dl_license_request",
                                                               HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">description</span>&nbsp;LICENSE REQUEST'),
                                                               style = BTN("#006495")),
                                                if (active)
                                                  downloadButton("dl_executive_pack",
                                                                 HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT PRO PACK'),
                                                                 style = BTN("#16a34a"))
                                                else
                                                  tags$button(type = "button", disabled = "disabled",
                                                              style = paste0(BTN("#94a3b8"), "opacity:.65;cursor:not-allowed;"),
                                                              HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">lock</span>&nbsp;PRO PACK LOCKED')))),
                              tags$div(class = "p-6 rounded-2xl",
                                       style = paste0("background:", status_bg, ";border:1px solid ", status_border, ";"),
                                       tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = paste0("color:", status_color, ";"), "License status"),
                                       tags$h3(class = "text-2xl font-extrabold mb-2", style = paste0("color:", status_color, ";"), license_status_label(license)),
                                       tags$p(class = "text-xs mb-1", style = "color:#475569;", paste0("Customer: ", license$customer)),
                                       tags$p(class = "text-xs mb-1", style = "color:#475569;", paste0("Tier: ", toupper(license$tier))),
                                       tags$p(class = "text-xs", style = "color:#475569;",
                                              paste0("Expires: ", ifelse(is.na(license$expires), "Not set", as.character(license$expires)))))),

                 tags$section(class = "grid grid-cols-3 gap-6",
                              price_card("Trial", "Free", "Internal evaluation, core dashboard browsing, and license request export.", "#64748b"),
                              price_card("Pro", paste0("$", license$monthly_price), "Executive workbook, commercial dashboard management, AI analytics, and stakeholder reporting.", "#006495", TRUE),
                              price_card("Enterprise", "Custom", "Private deployment, custom branding, SSO-ready hosting plan, onboarding, and support.", "#da191e")),

                 tags$section(class = "grid grid-cols-2 gap-6",
                              tags$div(class = paste(CARD, "p-6"), style = BORDER,
                                       tags$div(class = "flex items-center gap-2 mb-5",
                                                tags$span(class = "material-symbols-outlined", style = "color:#006495;", "sell"),
                                                tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Sellable Pro Features")),
                                       tags$div(class = "space-y-3",
                                                feature_row("download_for_offline", "Executive Pack Export", "A branded Excel workbook with summary KPIs, monthly trend, opportunity pipeline, and action plan.", TRUE),
                                                feature_row("campaign", "POC Performance Alerts", "Send tiered email nudges to business owners based on account health and integration performance."),
                                                feature_row("smart_toy", "ProcureGraph AI", "Ask natural-language questions against the loaded procurement dataset."),
                                                feature_row("dashboard_customize", "Saved Dashboards", "Let paying teams save and move analytics configurations between sessions."))),
                              tags$div(class = paste(CARD, "p-6"), style = BORDER,
                                       tags$div(class = "flex items-center gap-2 mb-5",
                                                tags$span(class = "material-symbols-outlined", style = "color:#16a34a;", "vpn_key"),
                                                tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Activation Setup")),
                                       tags$p(class = "text-sm mb-4", style = "color:#475569;line-height:1.7;",
                                              "For a paid customer, set these environment variables in ShinyApps.io or your hosting server, then redeploy/restart."),
                                       tags$pre(style = "background:#0e1d28;color:#e2e8f0;border-radius:12px;padding:16px;font-size:12px;line-height:1.7;white-space:pre-wrap;",
                                                paste(
                                                  "PROCUREGRAPH_LICENSE_TIER=pro",
                                                  "PROCUREGRAPH_LICENSE_KEY=<customer-license-key>",
                                                  "PROCUREGRAPH_CUSTOMER_NAME=<customer-name>",
                                                  "PROCUREGRAPH_LICENSE_EXPIRES=2027-05-15",
                                                  "PROCUREGRAPH_PRO_PRICE=299",
                                                  paste0("PROCUREGRAPH_SALES_CONTACT=", license$contact),
                                                  sep = "\n"
                                                )),
                                       tags$p(class = "text-xs", style = "color:#64748b;",
                                              "Tip: sell annual Pro access, then issue one license key per customer deployment."))))

      } else if (page == "analytics") {
        tags$div(class = "p-8 space-y-8",
                 
                 # Data source mode status banner
                 uiOutput("data_source_status"),
                 
                 uiOutput("mapping_warnings"),
                 
                 # Global filters (Consider Source, Client Name, Source, Final Status)
                 uiOutput("global_filter_panel"),
                 
                 uiOutput("analytics_data_health"),
                 
                 uiOutput("analytics_benchmark_summary"),
                 
                 uiOutput("analytics_opportunity_radar"),
                 
                 # Dashboard save/load (cloud-compatible: Export/Import JSON bundle)
                 uiOutput("dashboard_save_load_ui"),
                 
                 # Custom chart builder with plotly + drill-down + labels
                 # (chart is embedded inside this uiOutput — no separate plotOutput needed)
                 uiOutput("custom_analytics_builder"),
                 
                 # AI chat panel (Ollama local OR Groq API cloud)
                 uiOutput("ai_insights_panel")
        )
      }
    }) # end dashboard_content renderUI
    
  }) # end observe(authenticated)
}

shinyApp(ui, server)
