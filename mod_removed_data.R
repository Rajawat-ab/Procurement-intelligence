################################################################################
# MODULE: Removed Data Opportunity
# Purpose:
#   Analyse rows excluded by Remove(Y/N) rules and identify all removed buyer IDs,
#   including their Punchout/PunchIn opportunity volume.
################################################################################

setup_removed_data <- function(input, output, session, processed_df, filtered_df) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nchar(trimws(as.character(a[1]))) > 0) a else b

  removed_df <- reactive({
    df <- filtered_df()
    if (is.null(df) || !nrow(df)) return(data.frame())
    df %>% filter(`Remove(Y/N)` == "Remove")
  })

  punchin_removed_df <- reactive({
    df <- removed_df()
    if (is.null(df) || !nrow(df)) return(data.frame())
    df %>% filter(`Final Status` == "PR Int. - Punchout")
  })

  missing_mapping_df <- reactive({
    df <- filtered_df()
    if (is.null(df) || !nrow(df)) return(data.frame())
    df %>%
      filter(
        is.na(`PID-CID`) | `PID-CID` == "Unknown" |
          is.na(`Client Name`) | `Client Name` == "Unknown"
      )
  })

  removed_reason_label <- function(df) {
    case_when(
      is.na(df$`PID-CID`) | df$`PID-CID` == "Unknown" ~ "Missing PID-CID mapping",
      df$`Consider Source` == "Ignore" ~ "Source ignored",
      df$`Consider Stage` == "Ignore" ~ "Closed / Cancelled stage",
      df$`Consider Month` == "Ignore" ~ "Outside selected month",
      df$`Final Status` == "Ignore" ~ "Missing / ignored final status",
      df$RemoveHavellsCapex == "Ignore" ~ "Havells Capex rule",
      df$RemoveSC == "Ignore" ~ "SC rule",
      df$GRN == "Ignore" ~ "GRN rule",
      TRUE ~ "Other removal rule"
    )
  }

  buyer_summary <- reactive({
    df <- removed_df()
    if (is.null(df) || !nrow(df)) {
      return(data.frame(
        `Buyer ID` = character(),
        `Client Name` = character(),
        `PID-CID` = character(),
        `Total Removed Records` = integer(),
        `PunchIn Removed Records` = integer(),
        SAP = integer(),
        Manual = integer(),
        EOC = integer(),
        `Top Removal Reason` = character(),
        stringsAsFactors = FALSE
      ))
    }

    df %>%
      mutate(Removal_Reason = removed_reason_label(.)) %>%
      group_by(`buyer.id`, `PID-CID`, `Client Name`) %>%
      summarise(
        `Total Removed Records` = n(),
        `PunchIn Removed Records` = sum(`Final Status` == "PR Int. - Punchout", na.rm = TRUE),
        SAP = sum(source == "SAP", na.rm = TRUE),
        Manual = sum(source == "Manual", na.rm = TRUE),
        EOC = sum(source == "EOC", na.rm = TRUE),
        `Top Removal Reason` = names(sort(table(Removal_Reason), decreasing = TRUE))[1],
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Removed Records`), desc(`PunchIn Removed Records`), `Client Name`, `buyer.id`) %>%
      rename(`Buyer ID` = `buyer.id`)
  })

  removal_reason_summary <- reactive({
    df <- removed_df()
    if (is.null(df) || !nrow(df)) {
      return(data.frame(Reason = character(), Records = integer(), `Share of Removed` = character()))
    }

    total <- nrow(df)
    df %>%
      mutate(Reason = removed_reason_label(.)) %>%
      count(Reason, name = "Records", sort = TRUE) %>%
      mutate(`Share of Removed` = paste0(round(Records / total * 100, 1), "%"))
  })

  removed_mom_summary <- reactive({
    df <- removed_df()
    if (is.null(df) || !nrow(df)) {
      return(data.frame(
        `Buyer ID` = character(),
        `Client Name` = character(),
        `PID-CID` = character(),
        `First Month` = character(),
        `Latest Month` = character(),
        `Active Months` = integer(),
        `Total Removed Records` = integer(),
        `Latest Month Records` = integer(),
        `Previous Month Records` = integer(),
        `MoM Change` = character(),
        Trend = character(),
        Pattern = character(),
        stringsAsFactors = FALSE
      ))
    }

    monthly <- df %>%
      mutate(Month = coalesce(Month, "Unknown")) %>%
      group_by(`buyer.id`, `PID-CID`, `Client Name`, Month) %>%
      summarise(
        Month_Records = n(),
        PunchIn_Records = sum(`Final Status` == "PR Int. - Punchout", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(`buyer.id`, Month)

    latest <- monthly %>%
      group_by(`buyer.id`, `PID-CID`, `Client Name`) %>%
      mutate(Prev_Month_Records = lag(Month_Records)) %>%
      slice_tail(n = 1) %>%
      ungroup()

    monthly %>%
      group_by(`buyer.id`, `PID-CID`, `Client Name`) %>%
      summarise(
        `First Month` = min(Month, na.rm = TRUE),
        `Active Months` = n_distinct(Month),
        `Total Removed Records` = sum(Month_Records, na.rm = TRUE),
        `PunchIn Removed Records` = sum(PunchIn_Records, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        latest %>% select(`buyer.id`, `PID-CID`, `Client Name`, `Latest Month` = Month, `Latest Month Records` = Month_Records, `Previous Month Records` = Prev_Month_Records),
        by = c("buyer.id", "PID-CID", "Client Name")
      ) %>%
      mutate(
        `Previous Month Records` = coalesce(`Previous Month Records`, 0L),
        `MoM Change` = ifelse(
          `Previous Month Records` > 0,
          paste0(round((`Latest Month Records` - `Previous Month Records`) / `Previous Month Records` * 100, 1), "%"),
          ifelse(`Latest Month Records` > 0, "New", "0%")
        ),
        Trend = case_when(
          `Latest Month Records` > `Previous Month Records` ~ "Increasing",
          `Latest Month Records` < `Previous Month Records` ~ "Decreasing",
          TRUE ~ "Flat"
        ),
        Pattern = case_when(
          `Active Months` <= 2 ~ "Short-term 1-2 months",
          Trend == "Increasing" ~ "Continuous / rising",
          TRUE ~ "Recurring"
        )
      ) %>%
      arrange(desc(Trend == "Increasing"), desc(`Active Months`), desc(`Total Removed Records`), `buyer.id`) %>%
      rename(`Buyer ID` = `buyer.id`)
  })

  missing_mapping_summary <- reactive({
    df <- missing_mapping_df()
    if (is.null(df) || !nrow(df)) {
      return(data.frame(
        `Buyer ID` = character(),
        `Total Records` = integer(),
        `Removed Records` = integer(),
        `PunchIn Records` = integer(),
        SAP = integer(),
        Manual = integer(),
        EOC = integer(),
        `Latest Month` = character(),
        `Latest Month Records` = integer(),
        `Previous Month Records` = integer(),
        `MoM Change` = character(),
        Trend = character(),
        stringsAsFactors = FALSE
      ))
    }

    monthly <- df %>%
      mutate(Month = coalesce(Month, "Unknown")) %>%
      count(`buyer.id`, Month, name = "Month_Records") %>%
      arrange(`buyer.id`, Month) %>%
      group_by(`buyer.id`) %>%
      mutate(Prev_Month_Records = lag(Month_Records)) %>%
      slice_tail(n = 1) %>%
      ungroup()

    df %>%
      group_by(`buyer.id`) %>%
      summarise(
        `Total Records` = n(),
        `Removed Records` = sum(`Remove(Y/N)` == "Remove", na.rm = TRUE),
        `PunchIn Records` = sum(`Final Status` == "PR Int. - Punchout", na.rm = TRUE),
        SAP = sum(source == "SAP", na.rm = TRUE),
        Manual = sum(source == "Manual", na.rm = TRUE),
        EOC = sum(source == "EOC", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(monthly, by = "buyer.id") %>%
      mutate(
        Prev_Month_Records = coalesce(Prev_Month_Records, 0L),
        `MoM Change` = ifelse(
          Prev_Month_Records > 0,
          paste0(round((Month_Records - Prev_Month_Records) / Prev_Month_Records * 100, 1), "%"),
          ifelse(Month_Records > 0, "New", "0%")
        ),
        Trend = case_when(
          Month_Records > Prev_Month_Records ~ "Increasing",
          Month_Records < Prev_Month_Records ~ "Decreasing",
          TRUE ~ "Flat"
        )
      ) %>%
      arrange(desc(Trend == "Increasing"), desc(`Total Records`), desc(`PunchIn Records`), `buyer.id`) %>%
      transmute(
        `Buyer ID` = `buyer.id`,
        `Total Records`,
        `Removed Records`,
        `PunchIn Records`,
        SAP,
        Manual,
        EOC,
        `Latest Month` = Month,
        `Latest Month Records` = Month_Records,
        `Previous Month Records` = Prev_Month_Records,
        `MoM Change`,
        Trend
      )
  })

  detail_data <- reactive({
    df <- removed_df()
    if (is.null(df) || !nrow(df)) return(data.frame())

    df %>%
      mutate(Removal_Reason = removed_reason_label(.)) %>%
      select(any_of(c(
        "_id", "buyer.id", "PID-CID", "Client Name", "source", "Stage", "CreationDate",
        "Month", "Final Status", "Removal_Reason", "Consider Source", "Consider Stage",
        "Consider Month", "RemoveHavellsCapex", "RemoveSC", "GRN",
        "history.po.customerPoNo", "history.po.customerSANumber"
      ))) %>%
      arrange(desc(CreationDate))
  })

  output$removed_total_val <- renderUI({
    format(nrow(removed_df()), big.mark = ",")
  })

  output$removed_punchin_val <- renderUI({
    format(nrow(punchin_removed_df()), big.mark = ",")
  })

  output$removed_top_buyer_val <- renderUI({
    tbl <- buyer_summary()
    if (!nrow(tbl)) return("-")
    tbl$`Buyer ID`[1]
  })

  output$removed_top_buyer_count_val <- renderUI({
    tbl <- buyer_summary()
    if (!nrow(tbl)) return("0 records")
    paste0(format(tbl$`Total Removed Records`[1], big.mark = ","), " removed records")
  })

  output$missing_pidcid_buyer_val <- renderUI({
    format(nrow(missing_mapping_summary()), big.mark = ",")
  })

  output$missing_pidcid_records_val <- renderUI({
    tbl <- missing_mapping_summary()
    format(sum(tbl$`Total Records`, na.rm = TRUE), big.mark = ",")
  })

  output$missing_pidcid_increasing_val <- renderUI({
    tbl <- missing_mapping_summary()
    format(sum(tbl$Trend == "Increasing", na.rm = TRUE), big.mark = ",")
  })

  output$removed_mom_table <- renderDT({
    tbl <- removed_mom_summary()
    if (!nrow(tbl)) {
      return(datatable(data.frame(Message = "No removed MoM records in current scope"), rownames = FALSE, options = list(dom = "t")))
    }

    datatable(
      tbl,
      filter = "top",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      )
    ) %>%
      formatStyle(
        "Total Removed Records",
        background = styleColorBar(range(tbl$`Total Removed Records`), "#fecaca"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Trend",
        color = styleEqual(c("Increasing", "Flat", "Decreasing"), c("#b91c1c", "#64748b", "#15803d")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Pattern",
        color = styleEqual(c("Continuous / rising", "Short-term 1-2 months", "Recurring"), c("#b91c1c", "#b45309", "#006495")),
        fontWeight = "bold"
      )
  })

  output$removed_top_buyer_chart <- renderUI({
    tbl <- buyer_summary()
    if (!nrow(tbl)) {
      return(tags$div(
        class = "p-8 text-center",
        tags$span(class = "material-symbols-outlined", style = "color:#16a34a;font-size:36px;", "check_circle"),
        tags$p(class = "mt-2 font-bold", style = "color:#16a34a;", "No removed buyer records in current scope")
      ))
    }

    max_val <- max(tbl$`Total Removed Records`, na.rm = TRUE)
    bars <- lapply(seq_len(nrow(tbl)), function(i) {
      pct <- if (max_val > 0) round(tbl$`Total Removed Records`[i] / max_val * 100, 1) else 0
      tags$div(
        class = "mb-4",
        tags$div(
          class = "flex justify-between items-center mb-1",
          tags$div(
            class = "flex items-center gap-2",
            tags$span(
              style = "min-width:24px;height:24px;border-radius:50%;background:#da191e;color:white;font-size:10px;font-weight:800;display:inline-flex;align-items:center;justify-content:center;",
              i
            ),
            tags$div(
              tags$p(class = "text-xs font-bold mb-0", style = "color:#0e1d28;", tbl$`Buyer ID`[i]),
              tags$p(class = "text-[10px] mb-0", style = "color:#64748b;", tbl$`Client Name`[i])
            )
          ),
          tags$span(
            class = "text-xs font-extrabold",
            style = "color:#da191e;",
            paste0(
              format(tbl$`Total Removed Records`[i], big.mark = ","),
              " removed | ",
              format(tbl$`PunchIn Removed Records`[i], big.mark = ","),
              " PunchIn"
            )
          )
        ),
        tags$div(
          class = "h-2.5 rounded-full overflow-hidden",
          style = "background:#f1f5f9;",
          tags$div(class = "h-full rounded-full", style = paste0("width:", pct, "%;background:#da191e;"))
        )
      )
    })

    tags$div(class = "p-1", style = "max-height:520px;overflow-y:auto;padding-right:8px;", bars)
  })

  output$removed_buyer_table <- renderDT({
    tbl <- buyer_summary()
    if (!nrow(tbl)) return(datatable(data.frame(Message = "No removed buyer records"), rownames = FALSE, options = list(dom = "t")))

    datatable(
      tbl,
      filter = "top",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      )
    ) %>%
      formatStyle(
        "Total Removed Records",
        background = styleColorBar(range(tbl$`Total Removed Records`), "#fecaca"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "PunchIn Removed Records",
        background = styleColorBar(range(tbl$`PunchIn Removed Records`), "#ffedd5"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold"
      )
  })

  output$removed_reason_table <- renderDT({
    tbl <- removal_reason_summary()
    if (!nrow(tbl)) return(datatable(data.frame(Message = "No removed records"), rownames = FALSE, options = list(dom = "t")))

    datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 10)) %>%
      formatStyle(
        "Records",
        background = styleColorBar(range(tbl$Records), "#ffedd5"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold"
      )
  })

  output$missing_pidcid_table <- renderDT({
    tbl <- missing_mapping_summary()
    if (!nrow(tbl)) {
      return(datatable(data.frame(Message = "No missing PID-CID buyer IDs in current scope"), rownames = FALSE, options = list(dom = "t")))
    }

    datatable(
      tbl,
      filter = "top",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      )
    ) %>%
      formatStyle(
        "Total Records",
        background = styleColorBar(range(tbl$`Total Records`), "#dbeafe"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Trend",
        color = styleEqual(c("Increasing", "Flat", "Decreasing"), c("#b91c1c", "#64748b", "#15803d")),
        fontWeight = "bold"
      )
  })

  output$removed_detail_table <- renderDT({
    tbl <- detail_data()
    if (!nrow(tbl)) return(datatable(data.frame(Message = "No removed detail records"), rownames = FALSE, options = list(dom = "t")))

    datatable(
      tbl,
      filter = "top",
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      )
    ) %>%
      formatStyle("Removal_Reason", color = "#b91c1c", fontWeight = "bold") %>%
      formatStyle(columns = colnames(tbl), fontSize = "12px")
  })

  output$dl_removed_buyer <- downloadHandler(
    filename = function() paste0("removed_all_buyers_", Sys.Date(), ".csv"),
    content = function(file) write.csv(buyer_summary(), file, row.names = FALSE)
  )

  output$dl_removed_detail <- downloadHandler(
    filename = function() paste0("removed_all_detail_", Sys.Date(), ".csv"),
    content = function(file) write.csv(detail_data(), file, row.names = FALSE)
  )

  output$dl_missing_pidcid <- downloadHandler(
    filename = function() paste0("missing_pidcid_opportunity_", Sys.Date(), ".csv"),
    content = function(file) write.csv(missing_mapping_summary(), file, row.names = FALSE)
  )

  output$dl_removed_mom <- downloadHandler(
    filename = function() paste0("removed_mom_by_buyer_", Sys.Date(), ".csv"),
    content = function(file) write.csv(removed_mom_summary(), file, row.names = FALSE)
  )

  list(
    removed = removed_df,
    punchin_removed = punchin_removed_df,
    buyer_summary = buyer_summary,
    missing_mapping = missing_mapping_summary,
    removed_mom = removed_mom_summary
  )
}
