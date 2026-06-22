################################################################################
# MODULE: Custom Analytics Builder — ENHANCED (with robust plotly handling)
# Changes:
#   - Added event_register('plotly_click')
#   - Simplified grouped bar chart using plotly's `split` (fixes request object bug)
#   - Added explicit class check for plotly object before returning
#   - Wrapped all plot building in tryCatch with fallback to plotly_empty()
################################################################################

setup_custom_analytics <- function(input, output, session,
                                   processed_df, consider_df,
                                   company_integration, filtered_data) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  PALETTE <- c("#006495","#10b981","#f59e0b","#da191e","#8b5cf6",
               "#06b6d4","#84cc16","#f97316","#ec4899","#14b8a6",
               "#0ea5e9","#a855f7","#22c55e","#ef4444","#3b82f6")
  
  # ─── Available datasets ──────────────────────────────────────────────────
  datasets <- list(
    "Valid Line Items (Consider)" = function() {
      df <- consider_df()
      if (nrow(df) == 0) return(df)
      df %>% select(any_of(c("PID-CID","Client Name","source","Final Status",
                             "Consider Source","Consider Stage","Month",
                             "amount","Amount_Crore")))
    },
    "Processed Data (All)" = function() {
      df <- processed_df()
      if (nrow(df) == 0) return(df)
      df %>% select(any_of(c("PID-CID","Client Name","source","Final Status",
                             "Consider Source","Consider Stage","Month",
                             "amount","Amount_Crore")))
    },
    "Company Summary" = function() {
      df <- company_integration()
      if (nrow(df) == 0) return(df)
      df %>% select(any_of(c("PID-CID","Client Name","SAP","Manual","EOC",
                             "Grand_Total","Integration_Pct","Integrated_Volume",
                             "Total_Value_Cr","Integrated_Value_Cr","Value_Integration_Pct")))
    }
  )
  
  get_numeric_fields <- function(df) {
    if (nrow(df) == 0) return(character())
    sort(names(df)[sapply(df, is.numeric)])
  }
  get_categorical_fields <- function(df) {
    if (nrow(df) == 0) return(character())
    sort(names(df)[sapply(df, function(x) is.character(x) | is.factor(x))])
  }
  
  # ─── Reactive state ──────────────────────────────────────────────────────
  chart_config <- reactiveValues(
    dataset     = "Valid Line Items (Consider)",
    x_axis      = NULL,
    y_axis      = NULL,
    chart_type  = "bar",
    group_by    = NULL,
    show_labels = FALSE,
    title       = "",
    subtitle    = ""
  )
  
  drill_state <- reactiveValues(
    active   = FALSE,
    category = NULL
  )
  
  refresh_trigger <- reactiveVal(0)
  
  available_fields <- reactive({
    req(chart_config$dataset)
    df <- datasets[[chart_config$dataset]]()
    list(numeric = get_numeric_fields(df), categorical = get_categorical_fields(df))
  })
  
  # ─── Builder UI ──────────────────────────────────────────────────────────
  output$custom_analytics_builder <- renderUI({
    flds <- available_fields()
    
    tags$div(
      class = "bg-white rounded-2xl shadow-sm p-6 mb-6",
      style = "border:1px solid #f1f5f9;",
      
      tags$div(
        class = "flex items-center gap-2 mb-6",
        tags$span(class = "material-symbols-outlined", style = "color:#da191e;", "bar_chart"),
        tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Custom Chart Builder")
      ),
      
      tags$div(
        class = "grid grid-cols-4 gap-4 mb-4",
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Data Source"),
          selectInput("ca_dataset", NULL, choices = names(datasets),
                      selected = chart_config$dataset, width = "100%")
        ),
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "X-Axis (Category)"),
          selectInput("ca_x_axis", NULL, choices = flds$categorical,
                      selected = chart_config$x_axis, width = "100%")
        ),
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Y-Axis (Value)"),
          selectInput("ca_y_axis", NULL,
                      choices = c(setNames(flds$numeric, flds$numeric), "Count" = "count"),
                      selected = chart_config$y_axis, width = "100%")
        ),
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Chart Type"),
          selectInput("ca_chart_type", NULL,
                      choices = list(
                        "Bar"            = "bar",
                        "Horizontal Bar" = "hbar",
                        "Stacked Bar"    = "stacked_bar",
                        "Line"           = "line",
                        "Area"           = "area",
                        "Pie"            = "pie",
                        "Donut"          = "donut",
                        "Scatter"        = "scatter"
                      ),
                      selected = chart_config$chart_type, width = "100%")
        )
      ),
      
      tags$div(
        class = "grid grid-cols-4 gap-4 mb-5",
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Group By (optional)"),
          selectInput("ca_group_by", NULL,
                      choices = c("None" = "", flds$categorical),
                      selected = chart_config$group_by %||% "", width = "100%")
        ),
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Chart Title"),
          textInput("ca_title", NULL,
                    placeholder = "Leave blank for auto-title",
                    value = chart_config$title, width = "100%")
        ),
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Subtitle"),
          textInput("ca_subtitle", NULL,
                    placeholder = "Optional subtitle",
                    value = chart_config$subtitle, width = "100%")
        ),
        tags$div(
          tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2",
                 style = "color:#64748b;", "Display Options"),
          checkboxInput("ca_show_labels", "Show data labels",
                        value = isTRUE(chart_config$show_labels))
        )
      ),
      
      tags$div(
        class = "flex gap-3 mb-6",
        actionButton(
          "ca_generate",
          HTML('<span class="material-symbols-outlined" style="vertical-align:middle;font-size:16px;">bar_chart</span>&nbsp;Generate Chart'),
          style = paste0("background:#006495;color:white;border:none;font-weight:700;",
                         "padding:10px 20px;border-radius:8px;cursor:pointer;")
        ),
        actionButton(
          "ca_save_config",
          HTML('<span class="material-symbols-outlined" style="vertical-align:middle;font-size:16px;">save</span>&nbsp;Save Config'),
          style = paste0("background:#10b981;color:white;border:none;font-weight:700;",
                         "padding:10px 20px;border-radius:8px;cursor:pointer;")
        ),
        actionButton(
          "ca_clear_drill",
          HTML('<span class="material-symbols-outlined" style="vertical-align:middle;font-size:16px;">zoom_out_map</span>&nbsp;Clear Drill-down'),
          style = paste0("background:#f1f5f9;color:#475569;border:1px solid #e2e8f0;font-weight:700;",
                         "padding:10px 16px;border-radius:8px;cursor:pointer;")
        )
      ),
      
      tags$div(
        class = "mb-4 px-3 py-2 rounded-lg text-[11px]",
        style = "background:#f0f9ff;border:1px solid #bfdbfe;color:#0c4a6e;",
        HTML('💡 <strong>Tip:</strong> Click any bar or slice to drill down into that category.')
      ),
      
      plotlyOutput("custom_chart_display", height = "480px"),
      uiOutput("drilldown_section")
    )
  })
  
  # ─── Aggregate data ───────────────────────────────────────────────────────
  get_plot_data <- reactive({
    refresh_trigger()
    req(chart_config$dataset, chart_config$x_axis, chart_config$y_axis)
    
    df <- datasets[[chart_config$dataset]]()
    if (nrow(df) == 0) return(NULL)
    
    x_col <- chart_config$x_axis
    y_col <- chart_config$y_axis
    grp   <- if (!is.null(chart_config$group_by) && nchar(chart_config$group_by) > 0 &&
                 chart_config$group_by %in% names(df)) chart_config$group_by else NULL
    
    if (!x_col %in% names(df)) return(NULL)
    if (y_col != "count" && !y_col %in% names(df)) return(NULL)
    
    tryCatch({
      if (y_col == "count") {
        if (!is.null(grp)) {
          df %>%
            filter(!is.na(!!sym(x_col)), !is.na(!!sym(grp))) %>%
            group_by(x_val = !!sym(x_col), grp_val = !!sym(grp)) %>%
            summarise(value = n(), .groups = "drop")
        } else {
          df %>%
            filter(!is.na(!!sym(x_col))) %>%
            group_by(x_val = !!sym(x_col)) %>%
            summarise(value = n(), .groups = "drop") %>%
            arrange(desc(value))
        }
      } else {
        if (!is.null(grp)) {
          df %>%
            filter(!is.na(!!sym(x_col)), !is.na(!!sym(y_col)), !is.na(!!sym(grp))) %>%
            group_by(x_val = !!sym(x_col), grp_val = !!sym(grp)) %>%
            summarise(value = sum(!!sym(y_col), na.rm = TRUE), .groups = "drop")
        } else {
          df %>%
            filter(!is.na(!!sym(x_col)), !is.na(!!sym(y_col))) %>%
            group_by(x_val = !!sym(x_col)) %>%
            summarise(value = sum(!!sym(y_col), na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(value))
        }
      }
    }, error = function(e) {
      message("[CHART DATA ERROR] ", e$message)
      NULL
    })
  })
  
  # ─── Main plotly chart (fully robust) ────────────────────────────────────
  output$custom_chart_display <- renderPlotly({
    pd <- get_plot_data()
    
    # No data fallback
    if (is.null(pd) || nrow(pd) == 0) {
      return(
        plotly::plotly_empty(type = "bar") %>%
          plotly::layout(
            title = list(text = "No data — check selections or filters",
                         font = list(color = "#94a3b8", size = 14)),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          ) %>%
          plotly::event_register('plotly_click')
      )
    }
    
    # Build chart inside tryCatch; ensure a plotly object is always returned
    p <- tryCatch({
      ct          <- chart_config$chart_type
      show_lbl    <- isTRUE(chart_config$show_labels)
      has_grp     <- "grp_val" %in% names(pd)
      auto_title  <- paste0(if (chart_config$y_axis == "count") "Count" else chart_config$y_axis,
                            " by ", chart_config$x_axis)
      title_txt   <- if (nchar(chart_config$title) > 0) chart_config$title else auto_title
      sub_txt     <- chart_config$subtitle %||% ""
      full_title  <- paste0(
        "<b>", title_txt, "</b>",
        if (nchar(sub_txt) > 0) paste0("<br><sup style='color:#64748b'>", sub_txt, "</sup>") else ""
      )
      
      base_layout <- list(
        title       = list(text = full_title,
                           font = list(family = "Manrope,sans-serif", size = 16, color = "#0e1d28")),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "#fafafa",
        font        = list(family = "Manrope,sans-serif"),
        xaxis       = list(
          title     = list(text = chart_config$x_axis,
                           font = list(size = 11, color = "#64748b")),
          tickfont  = list(size = 10, color = "#64748b"),
          gridcolor = "#f1f5f9", showgrid = TRUE
        ),
        yaxis       = list(
          title     = list(text = if (chart_config$y_axis == "count") "Count" else chart_config$y_axis,
                           font = list(size = 11, color = "#64748b")),
          tickfont  = list(size = 10, color = "#64748b"),
          gridcolor = "#f1f5f9", showgrid = TRUE
        ),
        margin      = list(t = 70, b = 90, l = 60, r = 20),
        hoverlabel  = list(font = list(family = "Manrope,sans-serif", size = 12),
                           bgcolor = "#0e1d28", bordercolor = "#0e1d28", font.color = "white")
      )
      
      # Build the plot object
      p_obj <- if (ct %in% c("bar", "hbar", "stacked_bar")) {
        orient  <- if (ct == "hbar") "h" else "v"
        barmode <- if (ct == "stacked_bar") "stack" else "group"
        
        if (has_grp) {
          # Use color grouping — much safer than manual add_trace loops
          plotly::plot_ly(
            data = pd,
            source = "main_chart",
            x = if (orient == "v") ~x_val else ~value,
            y = if (orient == "v") ~value else ~x_val,
            color = ~grp_val,
            type = "bar",
            orientation = orient,
            colors = PALETTE,
            text = if (show_lbl) ~value else NULL,
            textposition = if (show_lbl) "auto" else "none",
            hovertemplate = paste0("%{x}<br>%{y:,}<br><extra>%{fullData.name}</extra>"),
            customdata = ~x_val
          ) %>%
            plotly::layout(base_layout) %>%
            plotly::layout(barmode = barmode)
        } else {
          n_cats <- nrow(pd)
          colors <- rep_len(PALETTE, n_cats)
          plotly::plot_ly(
            pd, source = "main_chart",
            x = if (orient == "v") ~x_val else ~value,
            y = if (orient == "v") ~value else ~x_val,
            type = "bar",
            orientation = orient,
            marker = list(color = colors,
                          line = list(color = "white", width = 1.5)),
            text = if (show_lbl) ~value else NULL,
            textposition = if (show_lbl) "outside" else "none",
            hovertemplate = "<b>%{x}</b><br>Value: <b>%{y:,}</b><extra></extra>",
            customdata = ~x_val
          ) %>%
            plotly::layout(base_layout) %>%
            { if (orient == "h") plotly::layout(., yaxis = list(categoryorder = "total ascending")) else . }
        }
        
      } else if (ct == "line") {
        plotly::plot_ly(
          pd, x = ~x_val, y = ~value,
          type = "scatter", mode = "lines+markers",
          source = "main_chart",
          line   = list(color = "#006495", width = 2.5, shape = "spline"),
          marker = list(color = "#006495", size = 8, symbol = "circle",
                        line = list(color = "white", width = 2)),
          text   = if (show_lbl) ~value else NULL,
          textposition = "top center",
          hovertemplate = "<b>%{x}</b><br>%{y:,}<extra></extra>"
        ) %>% plotly::layout(base_layout)
        
      } else if (ct == "area") {
        plotly::plot_ly(
          pd, x = ~x_val, y = ~value,
          type = "scatter", mode = "lines",
          source = "main_chart",
          fill   = "tozeroy",
          line      = list(color = "#006495", width = 2.5, shape = "spline"),
          fillcolor = "rgba(0,100,149,0.12)",
          hovertemplate = "<b>%{x}</b><br>%{y:,}<extra></extra>"
        ) %>% plotly::layout(base_layout)
        
      } else if (ct %in% c("pie", "donut")) {
        hole <- if (ct == "donut") 0.45 else 0
        plotly::plot_ly(
          pd, labels = ~x_val, values = ~value,
          type   = "pie",
          source = "main_chart",
          hole   = hole,
          marker = list(
            colors = PALETTE[seq_len(min(nrow(pd), length(PALETTE)))],
            line   = list(color = "white", width = 2)
          ),
          textinfo      = if (show_lbl) "label+percent+value" else "percent",
          textfont      = list(family = "Manrope,sans-serif", size = 11),
          hovertemplate = "<b>%{label}</b><br>%{value:,} (%{percent})<extra></extra>"
        ) %>%
          plotly::layout(list(
            title       = list(text  = full_title,
                               font  = list(family = "Manrope,sans-serif", size = 16, color = "#0e1d28")),
            paper_bgcolor = "rgba(0,0,0,0)",
            margin      = list(t = 70, b = 20, l = 20, r = 20),
            legend      = list(font = list(family = "Manrope,sans-serif", size = 11)),
            font        = list(family = "Manrope,sans-serif")
          ))
        
      } else if (ct == "scatter") {
        plotly::plot_ly(
          pd, x = ~x_val, y = ~value,
          type   = "scatter", mode = "markers",
          source = "main_chart",
          marker = list(color = "#006495", size = 12, opacity = 0.75,
                        line = list(color = "white", width = 2)),
          text  = ~x_val,
          hovertemplate = "<b>%{text}</b><br>Value: %{y:,}<extra></extra>"
        ) %>% plotly::layout(base_layout)
        
      } else {
        plotly::plotly_empty(type = "bar") %>%
          plotly::layout(title = list(text = "Unsupported chart type",
                                      font = list(color = "#dc2626", size = 14)))
      }
      
      # Ensure we have a plotly object (sometimes errors return other classes)
      if (!inherits(p_obj, "plotly")) {
        message("Object is not a plotly widget, class: ", class(p_obj))
        return(
          plotly::plotly_empty(type = "bar") %>%
            plotly::layout(
              title = list(text = "Chart generation error – invalid data?",
                           font = list(color = "#dc2626", size = 14)),
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)"
            ) %>%
            plotly::event_register('plotly_click')
        )
      }
      
      # Final touches: config and event registration
      p_obj %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(
            format   = "png",
            filename = paste0("procuregraph_", gsub(" ", "_", title_txt)),
            width    = 1400, height = 700, scale = 2
          )
        ) %>%
        plotly::event_register('plotly_click')
      
    }, error = function(e) {
      message("[PLOTLY ERROR] ", e$message)
      plotly::plotly_empty(type = "bar") %>%
        plotly::layout(
          title = list(text = paste("Chart error:", e$message),
                       font = list(color = "#dc2626", size = 14)),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        ) %>%
        plotly::event_register('plotly_click')
    })
    
    p
  })
  
  # ─── Drill-down click handler ─────────────────────────────────────────────
  observeEvent(plotly::event_data("plotly_click", source = "main_chart"), {
    click <- plotly::event_data("plotly_click", source = "main_chart")
    if (is.null(click)) return()
    
    ct       <- chart_config$chart_type
    cat_val  <- if (ct == "hbar") click$y else click$x
    
    if (!is.null(cat_val) && length(cat_val) > 0 &&
        ct %in% c("bar","hbar","stacked_bar","line","area","scatter")) {
      drill_state$category <- as.character(cat_val[[1]])
      drill_state$active   <- TRUE
    } else if (ct %in% c("pie","donut") && !is.null(click$label)) {
      drill_state$category <- as.character(click$label)
      drill_state$active   <- TRUE
    }
  })
  
  observeEvent(input$ca_clear_drill, {
    drill_state$active   <- FALSE
    drill_state$category <- NULL
  })
  
  observeEvent(input$ca_generate, {
    refresh_trigger(refresh_trigger() + 1)
    drill_state$active   <- FALSE
    drill_state$category <- NULL
  })
  
  # ─── Drill-down section ───────────────────────────────────────────────────
  output$drilldown_section <- renderUI({
    if (!isTRUE(drill_state$active) || is.null(drill_state$category)) return(NULL)
    
    tags$div(
      class = "mt-6 p-5 rounded-2xl",
      style = "background:#f0f9ff;border:2px solid #bfdbfe;",
      tags$div(
        class = "flex items-center justify-between mb-4",
        tags$div(
          class = "flex items-center gap-2",
          tags$span(class = "material-symbols-outlined",
                    style = "color:#0c4a6e;font-size:18px;", "query_stats"),
          tags$div(
            tags$p(class = "text-xs font-bold mb-0", style = "color:#0c4a6e;",
                   paste0("Drilling into: ", drill_state$category)),
            tags$p(class = "text-[10px] mb-0", style = "color:#64748b;",
                   "Breakdown by best available dimension")
          )
        ),
        tags$button(
          onclick = "Shiny.setInputValue('ca_clear_drill', Math.random(), {priority:'event'});",
          class   = "px-3 py-1 rounded-lg text-xs font-semibold",
          style   = "background:#dbeafe;color:#1e40af;border:none;cursor:pointer;",
          HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">close</span>&nbsp;Close')
        )
      ),
      plotlyOutput("drilldown_chart", height = "320px")
    )
  })
  
  # ─── Drill-down chart ─────────────────────────────────────────────────────
  output$drilldown_chart <- renderPlotly({
    req(drill_state$active, drill_state$category, chart_config$x_axis)
    
    df <- tryCatch(datasets[[chart_config$dataset]](), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(plotly::plotly_empty())
    
    x_col <- chart_config$x_axis
    if (!x_col %in% names(df)) return(plotly::plotly_empty())
    
    sub_df <- df %>% filter(as.character(!!sym(x_col)) == drill_state$category)
    if (nrow(sub_df) == 0) return(plotly::plotly_empty())
    
    cat_candidates <- setdiff(
      names(sub_df)[sapply(sub_df, function(c) is.character(c) | is.factor(c))],
      x_col
    )
    breakdown_col <- if ("Final Status" %in% cat_candidates) "Final Status" else
      if ("Consider Source" %in% cat_candidates) "Consider Source" else
        if (length(cat_candidates) > 0) cat_candidates[1] else NULL
    
    if (is.null(breakdown_col)) {
      return(
        plotly::plotly_empty() %>%
          plotly::layout(title = list(
            text = "No categorical breakdown dimension available",
            font = list(color = "#94a3b8", size = 13)
          ))
      )
    }
    
    agg <- sub_df %>%
      filter(!is.na(!!sym(breakdown_col))) %>%
      group_by(cat = !!sym(breakdown_col)) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n))
    
    plotly::plot_ly(
      agg,
      x    = ~cat, y = ~n,
      type = "bar",
      marker = list(
        color = PALETTE[seq_len(min(nrow(agg), length(PALETTE)))],
        line  = list(color = "white", width = 1.5)
      ),
      text         = ~n,
      textposition = "outside",
      hovertemplate = "<b>%{x}</b><br>Count: %{y:,}<extra></extra>"
    ) %>%
      plotly::layout(
        title = list(
          text = paste0("<b>", breakdown_col, " breakdown</b> within <i>",
                        drill_state$category, "</i>"),
          font = list(family = "Manrope,sans-serif", size = 14, color = "#0c4a6e")
        ),
        xaxis = list(title = "", tickfont = list(size = 10, color = "#64748b"),
                     gridcolor = "#f1f5f9"),
        yaxis = list(title = "Count", tickfont = list(size = 10, color = "#64748b"),
                     gridcolor = "#f1f5f9"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "#fafafa",
        margin        = list(t = 55, b = 75, l = 55, r = 15),
        font          = list(family = "Manrope,sans-serif")
      ) %>%
      plotly::config(displaylogo = FALSE,
                     modeBarButtonsToRemove = c("lasso2d","select2d"))
  })
  
  # ─── Input observers ──────────────────────────────────────────────────────
  observeEvent(input$ca_dataset, {
    chart_config$dataset <- input$ca_dataset
    flds <- available_fields()
    updateSelectInput(session, "ca_x_axis",   choices = flds$categorical)
    updateSelectInput(session, "ca_y_axis",
                      choices = c(setNames(flds$numeric, flds$numeric), "Count" = "count"))
    updateSelectInput(session, "ca_group_by", choices = c("None" = "", flds$categorical))
  })
  observeEvent(input$ca_x_axis,      { chart_config$x_axis      <- input$ca_x_axis })
  observeEvent(input$ca_y_axis,      { chart_config$y_axis      <- input$ca_y_axis })
  observeEvent(input$ca_chart_type,  { chart_config$chart_type  <- input$ca_chart_type })
  observeEvent(input$ca_group_by,    { chart_config$group_by    <- input$ca_group_by })
  observeEvent(input$ca_show_labels, { chart_config$show_labels <- input$ca_show_labels })
  observeEvent(input$ca_title,       { chart_config$title       <- input$ca_title })
  observeEvent(input$ca_subtitle,    { chart_config$subtitle    <- input$ca_subtitle })
  
  list(config = chart_config, data = get_plot_data)
}
