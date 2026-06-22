################################################################################
# MODULE: Global Filters for ProcureGraph
# Changes:
#   - Renders filter choices on first load so the panel is usable immediately
#   - Preserves valid selections when upstream data changes
#   - Narrows each dropdown based on the other active filters
#   - Exposes a single filtered Consider dataset for analytics modules
################################################################################

setup_global_filters <- function(input, output, session, processed_df, consider_df) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  filter_state <- reactiveValues(
    date_start       = NULL,
    date_end         = NULL,
    consider_sources = character(0),
    client_names     = character(0),
    sources          = character(0),
    status           = character(0)
  )
  
  preset_state <- reactiveValues(
    presets = list(),
    status  = "No preset saved in this session yet."
  )
  
  observe({
    filter_state$date_start <- input$date_filter_start
    filter_state$date_end   <- input$date_filter_end
  })
  
  base_consider_data <- reactive({
    df <- tryCatch(consider_df(), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(data.frame())
    df
  })
  
  apply_active_filters <- function(df, skip = character(0)) {
    if (is.null(df) || !nrow(df)) return(data.frame())
    
    out <- df
    if (!"date" %in% skip &&
        !is.null(filter_state$date_start) &&
        !is.null(filter_state$date_end) &&
        "CreationDate" %in% names(out)) {
      out <- out %>% filter(
        CreationDate >= as.Date(filter_state$date_start),
        CreationDate <= as.Date(filter_state$date_end)
      )
    }
    if (!"consider_sources" %in% skip && length(filter_state$consider_sources) > 0) {
      out <- out %>% filter(`Consider Source` %in% filter_state$consider_sources)
    }
    if (!"client_names" %in% skip && length(filter_state$client_names) > 0) {
      out <- out %>% filter(`Client Name` %in% filter_state$client_names)
    }
    if (!"sources" %in% skip && length(filter_state$sources) > 0) {
      out <- out %>% filter(source %in% filter_state$sources)
    }
    if (!"status" %in% skip && length(filter_state$status) > 0) {
      out <- out %>% filter(`Final Status` %in% filter_state$status)
    }
    out
  }
  
  cleaned_values <- function(df, column, drop_unknown = FALSE) {
    if (is.null(df) || !nrow(df) || !column %in% names(df)) return(character(0))
    vals <- df[[column]]
    vals <- as.character(vals)
    vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
    if (drop_unknown) vals <- vals[vals != "Unknown"]
    sort(unique(vals))
  }
  
  available_consider_sources <- reactive({
    cleaned_values(
      apply_active_filters(base_consider_data(), skip = "consider_sources"),
      "Consider Source"
    )
  })
  
  available_clients <- reactive({
    cleaned_values(
      apply_active_filters(base_consider_data(), skip = "client_names"),
      "Client Name",
      drop_unknown = TRUE
    )
  })
  
  available_sources <- reactive({
    cleaned_values(
      apply_active_filters(base_consider_data(), skip = "sources"),
      "source"
    )
  })
  
  available_status <- reactive({
    cleaned_values(
      apply_active_filters(base_consider_data(), skip = "status"),
      "Final Status"
    )
  })
  
  filtered_by_global <- reactive({
    apply_active_filters(base_consider_data())
  })
  
  active_filter_count <- reactive({
    sum(c(
      length(filter_state$consider_sources) > 0,
      length(filter_state$client_names) > 0,
      length(filter_state$sources) > 0,
      length(filter_state$status) > 0
    ))
  })
  
  preset_choices <- reactive({
    nms <- names(preset_state$presets)
    if (!length(nms)) c("No saved preset" = "") else c("Select preset..." = "", setNames(nms, nms))
  })
  
  sync_filter_choices <- function(input_id, choices, current_selected) {
    selected <- intersect(current_selected %||% character(0), choices %||% character(0))
    if (!is.null(input[[input_id]]) || identical(input$nav_page %||% "", "analytics")) {
      updateSelectizeInput(
        session,
        input_id,
        choices = choices,
        selected = selected,
        server = TRUE
      )
    }
    selected
  }
  
  output$global_filter_panel <- renderUI({
    consider_choices <- available_consider_sources()
    client_choices   <- available_clients()
    source_choices   <- available_sources()
    status_choices   <- available_status()
    
    tags$div(
      class = "bg-white rounded-2xl shadow-sm p-6 mb-6",
      style = "border:1px solid #f1f5f9;",
      tags$div(
        class = "flex items-center justify-between gap-3 mb-4",
        tags$div(
          class = "flex items-center gap-2",
          tags$span(class = "material-symbols-outlined", style = "color:#006495;", "filter_alt"),
          tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Advanced Filters")
        ),
        tags$div(
          class = "px-3 py-1 rounded-full text-[10px] font-bold uppercase tracking-widest",
          style = "background:#eff6ff;color:#1d4ed8;border:1px solid #bfdbfe;",
          paste(format(nrow(base_consider_data()), big.mark = ","), "valid line items")
        )
      ),
      tags$div(
        class = "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-4 gap-4",
        tags$div(
          tags$p(
            class = "text-[10px] font-bold uppercase tracking-widest mb-2",
            style = "color:#64748b;",
            "Consider Source"
          ),
          selectizeInput(
            "filter_consider_source", NULL,
            choices = consider_choices,
            selected = intersect(filter_state$consider_sources, consider_choices),
            multiple = TRUE,
            options = list(
              placeholder = "All sources...",
              plugins = list("remove_button"),
              maxItems = 30,
              closeAfterSelect = FALSE
            ),
            width = "100%"
          )
        ),
        tags$div(
          tags$p(
            class = "text-[10px] font-bold uppercase tracking-widest mb-2",
            style = "color:#64748b;",
            "Client Name"
          ),
          selectizeInput(
            "filter_client_names", NULL,
            choices = client_choices,
            selected = intersect(filter_state$client_names, client_choices),
            multiple = TRUE,
            options = list(
              placeholder = "All clients...",
              plugins = list("remove_button"),
              maxItems = 50,
              closeAfterSelect = FALSE
            ),
            width = "100%"
          )
        ),
        tags$div(
          tags$p(
            class = "text-[10px] font-bold uppercase tracking-widest mb-2",
            style = "color:#64748b;",
            "Source"
          ),
          selectizeInput(
            "filter_sources", NULL,
            choices = source_choices,
            selected = intersect(filter_state$sources, source_choices),
            multiple = TRUE,
            options = list(
              placeholder = "All sources...",
              plugins = list("remove_button"),
              maxItems = 20,
              closeAfterSelect = FALSE
            ),
            width = "100%"
          )
        ),
        tags$div(
          tags$p(
            class = "text-[10px] font-bold uppercase tracking-widest mb-2",
            style = "color:#64748b;",
            "Final Status"
          ),
          selectizeInput(
            "filter_status", NULL,
            choices = status_choices,
            selected = intersect(filter_state$status, status_choices),
            multiple = TRUE,
            options = list(
              placeholder = "All statuses...",
              plugins = list("remove_button"),
              maxItems = 20,
              closeAfterSelect = FALSE
            ),
            width = "100%"
          )
        )
      ),
      tags$div(
        class = "mt-4 p-4 rounded-xl space-y-3",
        style = "background:#fff7ed;border:1px solid #fed7aa;",
        tags$div(
          class = "flex items-center gap-2",
          tags$span(class = "material-symbols-outlined text-sm", style = "color:#c2410c;", "bookmark_manager"),
          tags$span(class = "text-[10px] font-bold uppercase tracking-widest", style = "color:#9a3412;", "Filter Presets")
        ),
        tags$div(
          class = "grid grid-cols-1 md:grid-cols-3 gap-3",
          textInput("filter_preset_name", NULL, placeholder = "Preset name"),
          selectInput("filter_preset_select", NULL, choices = preset_choices(), selected = ""),
          tags$div(
            class = "flex gap-2",
            actionButton(
              "save_filter_preset",
              "Save",
              style = "flex:1;background:#f97316;color:white;border:none;font-weight:700;padding:8px 12px;border-radius:8px;cursor:pointer;font-size:11px;"
            ),
            actionButton(
              "load_filter_preset",
              "Load",
              style = "flex:1;background:#0f766e;color:white;border:none;font-weight:700;padding:8px 12px;border-radius:8px;cursor:pointer;font-size:11px;"
            ),
            actionButton(
              "delete_filter_preset",
              "Delete",
              style = "flex:1;background:#475569;color:white;border:none;font-weight:700;padding:8px 12px;border-radius:8px;cursor:pointer;font-size:11px;"
            )
          )
        ),
        tags$p(class = "text-xs m-0", style = "color:#9a3412;", preset_state$status)
      ),
      tags$div(
        class = "mt-4 p-3 rounded-lg flex justify-between items-center gap-3",
        style = "background:#f8fafc;border:1px solid #e2e8f0;",
        tags$div(
          class = "flex items-center gap-2 text-xs",
          tags$span(class = "material-symbols-outlined text-sm", style = "color:#16a34a;", "check_circle"),
          uiOutput("filter_count_text", inline = TRUE)
        ),
        tags$button(
          class = "px-3 py-1 rounded-lg text-xs font-bold",
          style = "background:#f1f5f9;border:none;cursor:pointer;color:#64748b;",
          onclick = "Shiny.setInputValue('reset_filters', Math.random(), {priority:'event'});",
          HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">restart_alt</span>&nbsp;Reset')
        )
      )
    )
  })
  
  output$filter_count_text <- renderUI({
    tags$span(
      style = "color:#64748b;font-weight:600;",
      paste0(
        "Filtered: ", format(nrow(filtered_by_global()), big.mark = ","), " line items",
        " | Active filters: ", active_filter_count()
      )
    )
  })
  
  observe({
    filter_state$consider_sources <- sync_filter_choices(
      "filter_consider_source",
      available_consider_sources(),
      filter_state$consider_sources
    )
  })
  
  observe({
    filter_state$client_names <- sync_filter_choices(
      "filter_client_names",
      available_clients(),
      filter_state$client_names
    )
  })
  
  observe({
    filter_state$sources <- sync_filter_choices(
      "filter_sources",
      available_sources(),
      filter_state$sources
    )
  })
  
  observe({
    filter_state$status <- sync_filter_choices(
      "filter_status",
      available_status(),
      filter_state$status
    )
  })
  
  observeEvent(input$filter_consider_source, ignoreNULL = FALSE, {
    filter_state$consider_sources <- input$filter_consider_source %||% character(0)
  })
  
  observeEvent(input$filter_client_names, ignoreNULL = FALSE, {
    filter_state$client_names <- input$filter_client_names %||% character(0)
  })
  
  observeEvent(input$filter_sources, ignoreNULL = FALSE, {
    filter_state$sources <- input$filter_sources %||% character(0)
  })
  
  observeEvent(input$filter_status, ignoreNULL = FALSE, {
    filter_state$status <- input$filter_status %||% character(0)
  })
  
  observeEvent(input$reset_filters, ignoreInit = TRUE, {
    updateSelectizeInput(session, "filter_consider_source", selected = character(0), server = TRUE)
    updateSelectizeInput(session, "filter_client_names", selected = character(0), server = TRUE)
    updateSelectizeInput(session, "filter_sources", selected = character(0), server = TRUE)
    updateSelectizeInput(session, "filter_status", selected = character(0), server = TRUE)
    filter_state$consider_sources <- character(0)
    filter_state$client_names     <- character(0)
    filter_state$sources          <- character(0)
    filter_state$status           <- character(0)
  })
  
  observeEvent(input$save_filter_preset, ignoreInit = TRUE, {
    preset_name <- trimws(input$filter_preset_name %||% "")
    if (!nzchar(preset_name)) {
      preset_state$status <- "Enter a preset name before saving."
      return()
    }
    preset_state$presets[[preset_name]] <- list(
      consider_sources = filter_state$consider_sources,
      client_names     = filter_state$client_names,
      sources          = filter_state$sources,
      status           = filter_state$status
    )
    preset_state$status <- paste0("Saved preset: ", preset_name)
    updateSelectInput(session, "filter_preset_select", choices = preset_choices(), selected = preset_name)
    updateTextInput(session, "filter_preset_name", value = "")
  })
  
  observeEvent(input$load_filter_preset, ignoreInit = TRUE, {
    preset_name <- input$filter_preset_select %||% ""
    preset <- preset_state$presets[[preset_name]]
    if (!nzchar(preset_name) || is.null(preset)) {
      preset_state$status <- "Select a saved preset to load."
      return()
    }
    filter_state$consider_sources <- preset$consider_sources %||% character(0)
    filter_state$client_names     <- preset$client_names %||% character(0)
    filter_state$sources          <- preset$sources %||% character(0)
    filter_state$status           <- preset$status %||% character(0)
    updateSelectizeInput(session, "filter_consider_source", selected = filter_state$consider_sources, server = TRUE)
    updateSelectizeInput(session, "filter_client_names", selected = filter_state$client_names, server = TRUE)
    updateSelectizeInput(session, "filter_sources", selected = filter_state$sources, server = TRUE)
    updateSelectizeInput(session, "filter_status", selected = filter_state$status, server = TRUE)
    preset_state$status <- paste0("Loaded preset: ", preset_name)
  })
  
  observeEvent(input$delete_filter_preset, ignoreInit = TRUE, {
    preset_name <- input$filter_preset_select %||% ""
    if (!nzchar(preset_name) || is.null(preset_state$presets[[preset_name]])) {
      preset_state$status <- "Select a saved preset to delete."
      return()
    }
    preset_state$presets[[preset_name]] <- NULL
    preset_state$status <- paste0("Deleted preset: ", preset_name)
    updateSelectInput(session, "filter_preset_select", choices = preset_choices(), selected = "")
  })
  
  list(
    data             = filtered_by_global,
    state            = filter_state,
    clients          = available_clients,
    sources          = available_sources,
    consider_sources = available_consider_sources
  )
}
