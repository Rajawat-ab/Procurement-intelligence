################################################################################
# MODULE: Global Filters for ProcureGraph
# Purpose: Centralized filter state + UI components
# Status: NEW CODE - Non-breaking
################################################################################

# ─── FILTER STATE & REACTIVES ───────────────────────────────────────────
setup_global_filters <- function(input, output, session, processed_df, consider_df) {
  
  # Filter state (module-level)
  filter_state <- reactiveValues(
    date_start = NULL,
    date_end = NULL,
    buyer_ids = NULL,
    client_names = NULL,
    sources = NULL,
    status = NULL
  )
  
  # Synchronized with main date inputs (no conflict)
  observe({
    filter_state$date_start <- input$date_filter_start
    filter_state$date_end <- input$date_filter_end
  })
  
  # ─── Dynamic dropdown options ──────────────────────────────────────────
  available_buyers <- reactive({
    req(processed_df())
    processed_df() %>%
      distinct(`buyer.id`) %>%
      pull() %>%
      sort()
  })
  
  available_clients <- reactive({
    req(processed_df())
    processed_df() %>%
      filter(`Client Name` != "Unknown") %>%
      distinct(`Client Name`) %>%
      pull() %>%
      sort()
  })
  
  available_sources <- reactive({
    req(processed_df())
    processed_df() %>%
      filter(`Consider Source` != "Ignore") %>%
      distinct(source) %>%
      pull() %>%
      sort()
  })
  
  available_status <- reactive({
    req(processed_df())
    processed_df() %>%
      filter(`Final Status` != "Ignore") %>%
      distinct(`Final Status`) %>%
      pull() %>%
      sort()
  })
  
  # ─── Filtered dataset reactive ─────────────────────────────────────────
  filtered_by_global <- reactive({
    req(consider_df())
    df <- consider_df()
    
    # Apply date filters (sync with main)
    if (!is.null(filter_state$date_start) && !is.null(filter_state$date_end)) {
      df <- df %>%
        filter(CreationDate >= as.Date(filter_state$date_start),
               CreationDate <= as.Date(filter_state$date_end))
    }
    
    # Apply buyer filter
    if (!is.null(filter_state$buyer_ids) && length(filter_state$buyer_ids) > 0) {
      df <- df %>% filter(`buyer.id` %in% filter_state$buyer_ids)
    }
    
    # Apply client filter
    if (!is.null(filter_state$client_names) && length(filter_state$client_names) > 0) {
      df <- df %>% filter(`Client Name` %in% filter_state$client_names)
    }
    
    # Apply source filter
    if (!is.null(filter_state$sources) && length(filter_state$sources) > 0) {
      df <- df %>% filter(source %in% filter_state$sources)
    }
    
    # Apply status filter
    if (!is.null(filter_state$status) && length(filter_state$status) > 0) {
      df <- df %>% filter(`Final Status` %in% filter_state$status)
    }
    
    df
  })
  
  # ─── UI: Filter Panel ──────────────────────────────────────────────────
  output$global_filter_panel <- renderUI({
    tags$div(class = "bg-white rounded-2xl shadow-sm p-6 mb-6", style = "border:1px solid #f1f5f9;",
             tags$div(class = "flex items-center gap-2 mb-4",
                      tags$span(class = "material-symbols-outlined", style = "color:#006495;", "filter_alt"),
                      tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Advanced Filters")),
             
             tags$div(class = "grid grid-cols-4 gap-4",
                      # Buyer ID filter
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Buyer ID"),
                        selectInput("filter_buyer_ids", NULL, 
                                   choices = c("All" = "", available_buyers()),
                                   multiple = TRUE,
                                   width = "100%")),
                      
                      # Client filter
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Client Name"),
                        selectInput("filter_client_names", NULL, 
                                   choices = c("All" = "", available_clients()),
                                   multiple = TRUE,
                                   width = "100%")),
                      
                      # Source filter
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Source"),
                        selectInput("filter_sources", NULL, 
                                   choices = c("All" = "", available_sources()),
                                   multiple = TRUE,
                                   width = "100%")),
                      
                      # Status filter
                      tags$div(
                        tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                               style = "color:#64748b;", "Final Status"),
                        selectInput("filter_status", NULL, 
                                   choices = c("All" = "", available_status()),
                                   multiple = TRUE,
                                   width = "100%"))),
             
             # Filter info row
             tags$div(class = "mt-4 p-3 rounded-lg flex justify-between items-center",
                      style = "background:#f8fafc;border:1px solid #e2e8f0;",
                      tags$div(class = "flex items-center gap-2 text-xs",
                               tags$span(class = "material-symbols-outlined text-sm", style = "color:#16a34a;", "check_circle"),
                               tags$span(style = "color:#64748b;font-weight:600;",
                                        paste0("Filtered: ", reactive({nrow(filtered_by_global())})(), " line items"))),
                      tags$button(class = "px-3 py-1 rounded-lg text-xs font-bold",
                                 style = "background:#f1f5f9;border:none;cursor:pointer;color:#64748b;",
                                 onclick = "Shiny.setInputValue('reset_filters', true, {priority:'event'});",
                                 HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">restart_alt</span>&nbsp;Reset'))))
  })
  
  # Sync filters
  observeEvent(input$filter_buyer_ids, {
    filter_state$buyer_ids <- if (length(input$filter_buyer_ids) > 0) input$filter_buyer_ids else NULL
  })
  
  observeEvent(input$filter_client_names, {
    filter_state$client_names <- if (length(input$filter_client_names) > 0) input$filter_client_names else NULL
  })
  
  observeEvent(input$filter_sources, {
    filter_state$sources <- if (length(input$filter_sources) > 0) input$filter_sources else NULL
  })
  
  observeEvent(input$filter_status, {
    filter_state$status <- if (length(input$filter_status) > 0) input$filter_status else NULL
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_buyer_ids", selected = "")
    updateSelectInput(session, "filter_client_names", selected = "")
    updateSelectInput(session, "filter_sources", selected = "")
    updateSelectInput(session, "filter_status", selected = "")
    filter_state$buyer_ids <- NULL
    filter_state$client_names <- NULL
    filter_state$sources <- NULL
    filter_state$status <- NULL
  })
  
  # Return filtered dataset for other modules
  list(
    data = filtered_by_global,
    state = filter_state,
    buyers = available_buyers,
    clients = available_clients,
    sources = available_sources
  )
}
