################################################################################
# MODULE: Dashboard Save/Load
# Purpose: Save/load custom analytics configurations
# Status: NEW CODE - Non-breaking
# Storage: Local JSON (can extend to MongoDB)
################################################################################

setup_dashboard_save <- function(input, output, session, chart_config) {
  
  # ─── Storage location ──────────────────────────────────────────────────
  STORAGE_DIR <- "~/.procuregraph/dashboards"
  
  # Create dir if missing
  if (!dir.exists(STORAGE_DIR)) {
    dir.create(STORAGE_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  
  dashboard_state <- reactiveValues(
    saved_list = character(),
    current_name = NULL
  )
  
  # ─── Refresh saved dashboards list ─────────────────────────────────────
  refresh_dashboard_list <- function() {
    files <- list.files(
      path = STORAGE_DIR,
      pattern = "\\.json$",
      full.names = FALSE
    )
    dashboard_state$saved_list <- gsub("\\.json$", "", files)
  }
  
  # Initial load
  refresh_dashboard_list()
  
  # ─── Save dashboard config ─────────────────────────────────────────────
  save_dashboard_config <- function(name, config) {
    req(name, config)
    
    # Validate name
    if (!nchar(trimws(name))) {
      return(list(success = FALSE, message = "Dashboard name cannot be empty"))
    }
    
    # Sanitize filename
    safe_name <- gsub("[^a-zA-Z0-9_-]", "_", name)
    filepath <- file.path(STORAGE_DIR, paste0(safe_name, ".json"))
    
    # Build save object
    save_obj <- list(
      name = name,
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      config = list(
        dataset = config$dataset %||% NULL,
        x_axis = config$x_axis %||% NULL,
        y_axis = config$y_axis %||% NULL,
        chart_type = config$chart_type %||% "bar",
        title = config$title %||% "",
        subtitle = config$subtitle %||% ""
      )
    )
    
    tryCatch({
      write(jsonlite::toJSON(save_obj, pretty = TRUE), file = filepath)
      refresh_dashboard_list()
      list(success = TRUE, message = paste0("Dashboard '", name, "' saved"))
    }, error = function(e) {
      list(success = FALSE, message = paste0("Save failed: ", e$message))
    })
  }
  
  # ─── Load dashboard config ────────────────────────────────────────────
  load_dashboard_config <- function(name) {
    req(name)
    
    safe_name <- gsub("[^a-zA-Z0-9_-]", "_", name)
    filepath <- file.path(STORAGE_DIR, paste0(safe_name, ".json"))
    
    tryCatch({
      content <- read(filepath)
      obj <- jsonlite::fromJSON(content)
      list(
        success = TRUE,
        config = obj$config,
        message = paste0("Loaded: ", obj$name)
      )
    }, error = function(e) {
      list(success = FALSE, message = paste0("Load failed: ", e$message))
    })
  }
  
  # ─── Delete dashboard config ──────────────────────────────────────────
  delete_dashboard_config <- function(name) {
    req(name)
    
    safe_name <- gsub("[^a-zA-Z0-9_-]", "_", name)
    filepath <- file.path(STORAGE_DIR, paste0(safe_name, ".json"))
    
    tryCatch({
      if (file.exists(filepath)) {
        unlink(filepath)
        refresh_dashboard_list()
        list(success = TRUE, message = paste0("Deleted: ", name))
      } else {
        list(success = FALSE, message = "File not found")
      }
    }, error = function(e) {
      list(success = FALSE, message = paste0("Delete failed: ", e$message))
    })
  }
  
  # ─── UI: Save/Load Panel ───────────────────────────────────────────────
  output$dashboard_save_load_ui <- renderUI({
    tags$div(class = "bg-white rounded-2xl shadow-sm p-6 mb-6", style = "border:1px solid #f1f5f9;",
             tags$div(class = "flex items-center gap-2 mb-4",
                      tags$span(class = "material-symbols-outlined", style = "color:#10b981;", "folder_open"),
                      tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "Dashboard Management")),
             
             tags$div(class = "grid grid-cols-3 gap-4",
                      # Save new
                      tags$div(class = "p-4 rounded-xl", style = "background:#f0fdf4;border:1px solid #bbf7d0;",
                               tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                                      style = "color:#16a34a;", "Save Current Config"),
                               textInput("dash_save_name", NULL, 
                                        placeholder = "Dashboard name...",
                                        width = "100%"),
                               actionButton("dash_save_btn", "Save Dashboard",
                                           style = "background:#10b981;color:white;border:none;font-weight:700;padding:8px 12px;border-radius:6px;cursor:pointer;margin-top:8px;width:100%;font-size:11px;")),
                      
                      # Load existing
                      tags$div(class = "p-4 rounded-xl", style = "background:#f0f9ff;border:1px solid #bfdbfe;",
                               tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                                      style = "color:#0c4a6e;", "Load Configuration"),
                               selectInput("dash_load_select", NULL,
                                          choices = c("Select saved dashboard..." = "",
                                                      setNames(dashboard_state$saved_list, 
                                                              dashboard_state$saved_list)),
                                          width = "100%"),
                               actionButton("dash_load_btn", "Load",
                                           style = "background:#0c4a6e;color:white;border:none;font-weight:700;padding:8px 12px;border-radius:6px;cursor:pointer;margin-top:8px;width:100%;font-size:11px;")),
                      
                      # Quick actions
                      tags$div(class = "p-4 rounded-xl", style = "background:#fff7ed;border:1px solid #fed7aa;",
                               tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", 
                                      style = "color:#92400e;", "Actions"),
                               tags$div(class = "space-y-2",
                                        actionButton("dash_export_json", 
                                                    HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">download</span>&nbsp;Export JSON'),
                                                    style = "background:#ea580c;color:white;border:none;font-weight:700;padding:6px 10px;border-radius:6px;cursor:pointer;font-size:10px;width:100%;"),
                                        actionButton("dash_delete_btn", 
                                                    HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">delete</span>&nbsp;Delete'),
                                                    style = "background:#dc2626;color:white;border:none;font-weight:700;padding:6px 10px;border-radius:6px;cursor:pointer;font-size:10px;width:100%;")))),
             
             # Status messages
             uiOutput("dash_save_load_status")
    )
  })
  
  # ─── Status messages ───────────────────────────────────────────────────
  output$dash_save_load_status <- renderUI({
    status <- input$dash_save_status
    if (!is.null(status) && nchar(status) > 0) {
      is_error <- grepl("failed|error", tolower(status))
      bg_color <- if (is_error) "#fee2e2" else "#f0fdf4"
      text_color <- if (is_error) "#7f1d1d" else "#166534"
      border_color <- if (is_error) "#fecaca" else "#bbf7d0"
      icon <- if (is_error) "error" else "check_circle"
      
      tags$div(class = "mt-4 p-3 rounded-lg", style = paste0("background:", bg_color, ";border:1px solid ", border_color, ";"),
               tags$div(class = "flex items-center gap-2",
                        tags$span(class = "material-symbols-outlined text-sm", style = paste0("color:", text_color, ";"), icon),
                        tags$span(class = "text-xs font-bold", style = paste0("color:", text_color, ";"), status)))
    }
  })
  
  # ─── Save button handler ───────────────────────────────────────────────
  observeEvent(input$dash_save_btn, {
    req(input$dash_save_name, chart_config)
    
    result <- save_dashboard_config(input$dash_save_name, chart_config)
    
    if (result$success) {
      updateTextInput(session, "dash_save_name", value = "")
      shinyjs::runjs(paste0("Shiny.setInputValue('dash_save_status', '", 
                           result$message, "', {priority:'event'});"))
    } else {
      shinyjs::runjs(paste0("Shiny.setInputValue('dash_save_status', '", 
                           result$message, "', {priority:'event'});"))
    }
    
    # Refresh dropdown
    refresh_dashboard_list()
    updateSelectInput(session, "dash_load_select",
                     choices = c("Select saved dashboard..." = "",
                                setNames(dashboard_state$saved_list, 
                                        dashboard_state$saved_list)))
  })
  
  # ─── Load button handler ───────────────────────────────────────────────
  observeEvent(input$dash_load_btn, {
    req(input$dash_load_select)
    
    result <- load_dashboard_config(input$dash_load_select)
    
    if (result$success) {
      cfg <- result$config
      
      # Update chart config reactive
      chart_config$dataset <- cfg$dataset
      chart_config$x_axis <- cfg$x_axis
      chart_config$y_axis <- cfg$y_axis
      chart_config$chart_type <- cfg$chart_type
      chart_config$title <- cfg$title
      chart_config$subtitle <- cfg$subtitle
      
      # Update UI inputs
      updateSelectInput(session, "ca_dataset", selected = cfg$dataset)
      updateSelectInput(session, "ca_x_axis", selected = cfg$x_axis)
      updateSelectInput(session, "ca_y_axis", selected = cfg$y_axis)
      updateSelectInput(session, "ca_chart_type", selected = cfg$chart_type)
      updateTextInput(session, "ca_title", value = cfg$title)
      updateTextInput(session, "ca_subtitle", value = cfg$subtitle)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('dash_save_status', '", 
                           result$message, "', {priority:'event'});"))
    } else {
      shinyjs::runjs(paste0("Shiny.setInputValue('dash_save_status', '", 
                           result$message, "', {priority:'event'});"))
    }
  })
  
  # ─── Delete button handler ─────────────────────────────────────────────
  observeEvent(input$dash_delete_btn, {
    req(input$dash_load_select)
    
    if (showModal(modalDialog(
      title = "Confirm Delete",
      paste0("Delete dashboard '", input$dash_load_select, "'?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("dash_delete_confirm", "Delete",
                    style = "background:#dc2626;color:white;border:none;cursor:pointer;")
      ),
      easyClose = FALSE
    ))) {}
  })
  
  observeEvent(input$dash_delete_confirm, {
    result <- delete_dashboard_config(input$dash_load_select)
    removeModal()
    
    shinyjs::runjs(paste0("Shiny.setInputValue('dash_save_status', '", 
                         result$message, "', {priority:'event'});"))
    
    refresh_dashboard_list()
    updateSelectInput(session, "dash_load_select",
                     choices = c("Select saved dashboard..." = "",
                                setNames(dashboard_state$saved_list, 
                                        dashboard_state$saved_list)))
  })
  
  # ─── Export JSON ───────────────────────────────────────────────────────
  observeEvent(input$dash_export_json, {
    req(chart_config)
    
    export_obj <- list(
      name = input$dash_save_name %||% paste0("export_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      config = list(
        dataset = chart_config$dataset,
        x_axis = chart_config$x_axis,
        y_axis = chart_config$y_axis,
        chart_type = chart_config$chart_type,
        title = chart_config$title,
        subtitle = chart_config$subtitle
      )
    )
    
    json_str <- jsonlite::toJSON(export_obj, pretty = TRUE)
    
    # Trigger browser download
    output$dash_export_json_download <- downloadHandler(
      filename = function() paste0("procuregraph_config_", 
                                  format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
      content = function(file) {
        writeLines(json_str, file)
      }
    )
    
    # Trigger the download
    shinyjs::runjs(paste0(
      "document.getElementById('dash_export_json_download').click();"
    ))
  })
  
  # ─── Return functions for external use ─────────────────────────────────
  list(
    save = save_dashboard_config,
    load = load_dashboard_config,
    delete = delete_dashboard_config,
    refresh = refresh_dashboard_list,
    list = reactive(dashboard_state$saved_list)
  )
}
