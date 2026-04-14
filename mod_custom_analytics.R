################################################################################
# MODULE: Custom Analytics Builder
# Purpose: Dynamic chart builder with x/y axis selection + chart type switching
# Status: NEW CODE - Non-breaking
################################################################################

setup_custom_analytics <- function(input, output, session, 
                                   processed_df, consider_df, 
                                   company_integration, filtered_data) {
  
  # ─── Available datasets ────────────────────────────────────────────────
  datasets <- list(
    "Valid Line Items (Consider)" = function() consider_df(),
    "Processed Data (All)" = function() processed_df(),
    "Company Integration Summary" = function() company_integration()
  )
  
  # ─── Dynamic field detection ───────────────────────────────────────────
  get_numeric_fields <- function(df) {
    if (nrow(df) == 0) return(character())
    df %>%
      select(where(is.numeric)) %>%
      colnames() %>%
      sort()
  }
  
  get_categorical_fields <- function(df) {
    if (nrow(df) == 0) return(character())
    df %>%
      select(where(~is.character(.) | is.factor(.))) %>%
      colnames() %>%
      sort()
  }
  
  # ─── Reactive values for chart state ───────────────────────────────────
  chart_config <- reactiveValues(
    dataset = "Valid Line Items (Consider)",
    x_axis = NULL,
    y_axis = NULL,
    chart_type = "bar",
    title = "",
    subtitle = ""
  )
  
  # ─── Available fields (updates when dataset changes) ─────────────────────
  available_fields <- reactive({
    req(chart_config$dataset)
    df <- datasets[[chart_config$dataset]]()
    list(
      numeric = get_numeric_fields(df),
      categorical = get_categorical_fields(df)
    )
  })
  
  # ─── Chart generation ─────────────────────────────────────────────────
  generate_custom_chart <- reactive({
    req(chart_config$dataset, chart_config$x_axis, chart_config$y_axis, chart_config$chart_type)
    
    df <- datasets[[chart_config$dataset]]()
    if (nrow(df) == 0) return(NULL)
    
    x_col <- chart_config$x_axis
    y_col <- chart_config$y_axis
    chart_t <- chart_config$chart_type
    
    # Prepare data
    plot_data <- df %>%
      select(all_of(c(x_col, y_col))) %>%
      filter(!is.na(!!sym(x_col)), !is.na(!!sym(y_col)))
    
    if (nrow(plot_data) == 0) return(NULL)
    
    # Aggregate if needed
    if (is.numeric(plot_data[[y_col]])) {
      plot_data <- plot_data %>%
        group_by(!!sym(x_col)) %>%
        summarise(!!y_col := sum(!!sym(y_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(!!sym(y_col)))
    } else {
      plot_data <- plot_data %>%
        group_by(!!sym(x_col), !!sym(y_col)) %>%
        summarise(count = n(), .groups = "drop")
      y_col <- "count"
    }
    
    # Build ggplot
    p <- ggplot(plot_data, aes(x = reorder(!!sym(x_col), -!!sym(y_col)), 
                                y = !!sym(y_col), 
                                fill = !!sym(x_col)))
    
    if (chart_t == "bar") {
      p <- p +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (chart_t == "line") {
      p <- p +
        geom_line(aes(group = 1), color = "#006495", size = 1.2) +
        geom_point(color = "#006495", size = 3) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (chart_t == "pie") {
      p <- ggplot(plot_data, aes(x = "", y = !!sym(y_col), fill = !!sym(x_col))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void()
    }
    
    p <- p +
      labs(
        title = chart_config$title %||% paste0(y_col, " by ", x_col),
        subtitle = chart_config$subtitle %||% "",
        x = x_col,
        y = y_col,
        fill = x_col
      ) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#0e1d28", 
                                 family = "Plus Jakarta Sans"),
        plot.subtitle = element_text(size = 11, color = "#64748b", family = "Manrope"),
        axis.title = element_text(size = 10, face = "bold", color = "#0e1d28", 
                                 family = "Manrope"),
        panel.grid.major = element_line(color = "#f1f5f9")
      )
    
    p
  })
  
  # ─── UI: Analytics Builder Panel ───────────────────────────────────────
  output$custom_analytics_builder <- renderUI({
    tags$div(class = "bg-white rounded-2xl shadow-sm p-6 mb-6", style = "border:1px solid #f1f5f9;",
             tags$div(class = "flex items-center gap-2 mb-6",
                      tags$span(class = "material-symbols-outlined", style = "color:#da191e;", "bar_chart"),
                      tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Custom Chart Builder")),
             
             # Config grid
             tags$div(class = "grid grid-cols-4 gap-4 mb-6",
                      # Dataset selector
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Data Source"),
                        selectInput("ca_dataset", NULL, 
                                   choices = names(datasets),
                                   selected = chart_config$dataset,
                                   width = "100%")),
                      
                      # X-axis selector
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "X-Axis (Category)"),
                        selectInput("ca_x_axis", NULL, 
                                   choices = available_fields()$categorical,
                                   selected = chart_config$x_axis,
                                   width = "100%")),
                      
                      # Y-axis selector
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Y-Axis (Value)"),
                        selectInput("ca_y_axis", NULL, 
                                   choices = c(available_fields()$numeric, 
                                              "Count" = "count"),
                                   selected = chart_config$y_axis,
                                   width = "100%")),
                      
                      # Chart type selector
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Chart Type"),
                        selectInput("ca_chart_type", NULL, 
                                   choices = list("Bar" = "bar", "Line" = "line", "Pie" = "pie"),
                                   selected = chart_config$chart_type,
                                   width = "100%"))),
             
             # Chart title + subtitle
             tags$div(class = "grid grid-cols-2 gap-4 mb-6",
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-1", 
                               style = "color:#64748b;", "Chart Title"),
                        textInput("ca_title", NULL, 
                                 placeholder = "Leave blank for auto-title",
                                 value = chart_config$title,
                                 width = "100%")),
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-1", 
                               style = "color:#64748b;", "Subtitle"),
                        textInput("ca_subtitle", NULL, 
                                 placeholder = "Optional subtitle",
                                 value = chart_config$subtitle,
                                 width = "100%"))),
             
             # Action buttons
             tags$div(class = "flex gap-3",
                      actionButton("ca_generate", "Generate Chart",
                                  style = "background:#006495;color:white;border:none;font-weight:700;padding:10px 20px;border-radius:8px;cursor:pointer;"),
                      actionButton("ca_save_config", 
                                  HTML('<span class="material-symbols-outlined" style="vertical-align:middle;">save</span>&nbsp;Save Config'),
                                  style = "background:#10b981;color:white;border:none;font-weight:700;padding:10px 20px;border-radius:8px;cursor:pointer;"))
    )
  })
  
  # ─── Sync chart config from inputs ─────────────────────────────────────
  observeEvent(input$ca_dataset, {
    chart_config$dataset <- input$ca_dataset
    # Reset axis selections on dataset change
    updateSelectInput(session, "ca_x_axis", 
                     choices = available_fields()$categorical)
    updateSelectInput(session, "ca_y_axis", 
                     choices = c(available_fields()$numeric, "Count" = "count"))
  })
  
  observeEvent(input$ca_x_axis, {
    chart_config$x_axis <- input$ca_x_axis
  })
  
  observeEvent(input$ca_y_axis, {
    chart_config$y_axis <- input$ca_y_axis
  })
  
  observeEvent(input$ca_chart_type, {
    chart_config$chart_type <- input$ca_chart_type
  })
  
  observeEvent(input$ca_title, {
    chart_config$title <- input$ca_title
  })
  
  observeEvent(input$ca_subtitle, {
    chart_config$subtitle <- input$ca_subtitle
  })
  
  # ─── Generate chart on button click ───────────────────────────────────
  observeEvent(input$ca_generate, {
    # Trigger chart regeneration
  })
  
  # ─── Chart output ──────────────────────────────────────────────────────
  output$custom_chart_display <- renderPlot({
    req(generate_custom_chart())
    generate_custom_chart()
  }, bg = "white", width = 800, height = 500)
  
  # ─── Return chart config for saving ────────────────────────────────────
  list(
    config = chart_config,
    chart = generate_custom_chart
  )
}
