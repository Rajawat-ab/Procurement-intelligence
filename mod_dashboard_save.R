################################################################################
# MODULE: Dashboard Save/Load — CLOUD-COMPATIBLE v2
#
# STORAGE STRATEGY — auto-detected at startup:
#
#   LOCAL (your machine / private server):
#     Configs written to ~/.procuregraph/dashboards/*.json
#     Persist across sessions automatically.
#
#   CLOUD (ShinyApps.io or any read-only filesystem):
#     Configs stored in reactiveValues — in-session memory only.
#     Cross-session persistence via Export / Import JSON bundle:
#       1. Save as many configs as you want during a session.
#       2. Click "Export Bundle" → downloads all configs as one .json file.
#       3. Next session: click "Import Bundle" → upload the .json file
#          → all configs restored instantly.
#
# The module detects which mode to use by attempting a test write at startup.
# No code changes needed when switching between local and cloud.
#
# Bugs from original version that are also fixed:
#   • read() → readLines()                         (crashed on Load)
#   • if (showModal()) {} → showModal() directly   (delete modal never showed)
#   • downloadHandler declared as output$           (export always failed)
################################################################################

setup_dashboard_save <- function(input, output, session, chart_config) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ─── Detect storage mode once ──────────────────────────────────────────────
  LOCAL_DIR <- "~/.procuregraph/dashboards"
  
  use_fs <- local({
    tryCatch({
      dir.create(LOCAL_DIR, recursive = TRUE, showWarnings = FALSE)
      tf <- file.path(LOCAL_DIR, ".write_test")
      writeLines("ok", tf); ok <- file.exists(tf); unlink(tf); ok
    }, error = function(e) FALSE)
  })
  
  message("[DashSave] mode: ", if (use_fs) "local filesystem" else "cloud / memory-only")
  
  # ─── Reactive state ────────────────────────────────────────────────────────
  # `configs` is a named list:  safe_key → list(name, created, config)
  # On local mode this mirrors disk. On cloud it's memory-only.
  state <- reactiveValues(
    configs    = list(),
    status_msg = NULL,
    status_ok  = TRUE
  )
  
  set_status <- function(msg, ok = TRUE) { state$status_msg <- msg; state$status_ok <- ok }
  
  # ─── Helpers ──────────────────────────────────────────────────────────────
  safe_key <- function(name) gsub("[^a-zA-Z0-9_-]", "_", trimws(name %||% ""))
  
  make_obj <- function(name, cfg) {
    list(
      name    = name,
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      config  = list(
        dataset     = cfg$dataset     %||% NULL,
        x_axis      = cfg$x_axis      %||% NULL,
        y_axis      = cfg$y_axis      %||% NULL,
        chart_type  = cfg$chart_type  %||% "bar",
        group_by    = cfg$group_by    %||% NULL,
        show_labels = isTRUE(cfg$show_labels),
        title       = cfg$title       %||% "",
        subtitle    = cfg$subtitle    %||% ""
      )
    )
  }
  
  apply_cfg_to_ui <- function(cfg) {
    chart_config$dataset     <- cfg$dataset     %||% chart_config$dataset
    chart_config$x_axis      <- cfg$x_axis      %||% chart_config$x_axis
    chart_config$y_axis      <- cfg$y_axis      %||% chart_config$y_axis
    chart_config$chart_type  <- cfg$chart_type  %||% "bar"
    chart_config$group_by    <- cfg$group_by    %||% NULL
    chart_config$show_labels <- isTRUE(cfg$show_labels)
    chart_config$title       <- cfg$title       %||% ""
    chart_config$subtitle    <- cfg$subtitle    %||% ""
    updateSelectInput(session,   "ca_dataset",    selected = cfg$dataset    %||% "")
    updateSelectInput(session,   "ca_x_axis",     selected = cfg$x_axis     %||% "")
    updateSelectInput(session,   "ca_y_axis",     selected = cfg$y_axis     %||% "")
    updateSelectInput(session,   "ca_chart_type", selected = cfg$chart_type %||% "bar")
    updateSelectInput(session,   "ca_group_by",   selected = cfg$group_by   %||% "")
    updateCheckboxInput(session, "ca_show_labels", value   = isTRUE(cfg$show_labels))
    updateTextInput(session,     "ca_title",       value   = cfg$title      %||% "")
    updateTextInput(session,     "ca_subtitle",    value   = cfg$subtitle   %||% "")
  }
  
  refresh_dd <- function() {
    nms <- names(state$configs)
    updateSelectInput(session, "dash_load_select",
                      choices = c("Select saved dashboard..." = "", setNames(nms, nms)))
  }
  
  # ─── Filesystem helpers (local mode only) ─────────────────────────────────
  fs_path   <- function(k) file.path(LOCAL_DIR, paste0(k, ".json"))
  fs_write  <- function(k, obj) writeLines(jsonlite::toJSON(obj, pretty=TRUE, auto_unbox=TRUE), fs_path(k))
  fs_delete <- function(k) { fp <- fs_path(k); if (file.exists(fp)) unlink(fp) }
  
  fs_read_all <- function() {
    files <- list.files(LOCAL_DIR, pattern="\\.json$", full.names=TRUE)
    out   <- list()
    for (f in files) {
      tryCatch({
        raw <- paste(readLines(f, warn=FALSE), collapse="\n")
        obj <- jsonlite::fromJSON(raw, simplifyVector=FALSE)
        out[[gsub("\\.json$","",basename(f))]] <- obj
      }, error=function(e) NULL)
    }
    out
  }
  
  # Bootstrap: load existing local configs on startup
  if (use_fs) {
    boot <- tryCatch(fs_read_all(), error=function(e) list())
    if (length(boot)) state$configs <- boot
  }
  
  # ─── CRUD operations ──────────────────────────────────────────────────────
  op_save <- function(name, cfg) {
    name <- trimws(name %||% "")
    if (!nchar(name)) return(list(success=FALSE, message="Name cannot be empty"))
    k   <- safe_key(name)
    obj <- make_obj(name, cfg)
    if (use_fs) tryCatch(fs_write(k, obj), error=function(e) NULL)
    state$configs[[k]] <- obj
    list(success=TRUE, message=paste0("\u2713 Saved '", name, "'"))
  }
  
  op_load <- function(k) {
    k   <- safe_key(k %||% "")
    obj <- state$configs[[k]]
    if (is.null(obj)) return(list(success=FALSE, message=paste0("Not found: ", k)))
    list(success=TRUE, config=obj$config, message=paste0("\u2713 Loaded: ", obj$name %||% k))
  }
  
  op_delete <- function(k) {
    k <- safe_key(k %||% "")
    if (!nchar(k)) return(list(success=FALSE, message="Nothing selected"))
    if (use_fs) tryCatch(fs_delete(k), error=function(e) NULL)
    state$configs[[k]] <- NULL
    list(success=TRUE, message=paste0("\u2713 Deleted: ", k))
  }
  
  # ─── Export: all configs as one JSON bundle ────────────────────────────────
  output$dash_export_bundle <- downloadHandler(
    filename = function() paste0("procuregraph_dashboards_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".json"),
    content  = function(file) {
      bundle <- list(
        app      = "ProcureGraph",
        version  = "2.0",
        exported = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        count    = length(state$configs),
        configs  = isolate(state$configs)
      )
      writeLines(jsonlite::toJSON(bundle, pretty=TRUE, auto_unbox=TRUE), file)
    }
  )
  
  # ─── Import: restore all configs from a bundle file ───────────────────────
  observeEvent(input$dash_import_file, {
    req(input$dash_import_file)
    result <- tryCatch({
      raw    <- paste(readLines(input$dash_import_file$datapath, warn=FALSE), collapse="\n")
      bundle <- jsonlite::fromJSON(raw, simplifyVector=FALSE)
      
      # Accept bundle format (has $configs) OR single-config format (has $config)
      cfg_list <- if (!is.null(bundle$configs)) {
        bundle$configs
      } else if (!is.null(bundle$config)) {
        k <- safe_key(bundle$name %||% paste0("import_", format(Sys.time(),"%H%M%S")))
        setNames(list(bundle), k)
      } else {
        stop("Unrecognised format — must be a ProcureGraph export bundle")
      }
      
      n <- 0L
      for (k in names(cfg_list)) {
        obj <- cfg_list[[k]]
        if (!is.null(obj$config)) {
          sk <- safe_key(k)
          state$configs[[sk]] <- obj
          if (use_fs) tryCatch(fs_write(sk, obj), error=function(e) NULL)
          n <- n + 1L
        }
      }
      list(success=TRUE, message=paste0("\u2713 Imported ", n, " config(s)"))
    }, error=function(e) list(success=FALSE, message=paste0("Import failed: ", e$message)))
    
    set_status(result$message, result$success)
    refresh_dd()
  })
  
  # ─── UI ───────────────────────────────────────────────────────────────────
  output$dashboard_save_load_ui <- renderUI({
    nms      <- names(state$configs)
    n_saved  <- length(nms)
    is_cloud <- !use_fs
    
    tags$div(
      class = "bg-white rounded-2xl shadow-sm p-6 mb-6",
      style = "border:1px solid #f1f5f9;",
      
      # ── Header ──
      tags$div(
        class = "flex items-center justify-between mb-4",
        tags$div(class="flex items-center gap-2",
                 tags$span(class="material-symbols-outlined", style="color:#10b981;", "folder_open"),
                 tags$h3(class="text-sm font-extrabold uppercase tracking-widest", "Dashboard Management")),
        tags$div(class="flex items-center gap-2",
                 tags$span(class="text-[10px] font-bold px-2 py-1 rounded-full",
                           style=if(is_cloud)"background:#6f42c1;color:white;"
                           else "background:#f0fdf4;color:#15803d;border:1px solid #bbf7d0;",
                           if(is_cloud)"☁ Cloud mode" else "💾 Local mode"),
                 tags$span(class="text-[10px]", style="color:#94a3b8;",
                           paste0(n_saved, " saved")))
      ),
      
      # ── Cloud-mode tip ──
      if (is_cloud)
        tags$div(class="mb-4 p-3 rounded-xl flex items-start gap-2 text-xs",
                 style="background:#faf5ff;border:1px solid #e9d5ff;",
                 tags$span(class="material-symbols-outlined text-sm flex-shrink-0",
                           style="color:#7c3aed;", "info"),
                 tags$div(style="color:#5b21b6;",
                          HTML("<strong>Cloud mode</strong> — configs exist for this session only. "),
                          HTML("Use <strong>Export Bundle</strong> to download all configs as one .json, "),
                          HTML("then <strong>Import Bundle</strong> next session to restore them.")))
      else NULL,
      
      # ── 4-col grid ──
      tags$div(class="grid grid-cols-4 gap-4",
               
               # 1 Save
               tags$div(class="p-4 rounded-xl", style="background:#f0fdf4;border:1px solid #bbf7d0;",
                        tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-2",
                               style="color:#16a34a;", "Save Config"),
                        textInput("dash_save_name", NULL, placeholder="Dashboard name...", width="100%"),
                        actionButton("dash_save_btn",
                                     HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">save</span>&nbsp;Save'),
                                     style=paste0("background:#10b981;color:white;border:none;font-weight:700;",
                                                  "padding:8px 12px;border-radius:6px;cursor:pointer;",
                                                  "margin-top:8px;width:100%;font-size:11px;"))),
               
               # 2 Load
               tags$div(class="p-4 rounded-xl", style="background:#f0f9ff;border:1px solid #bfdbfe;",
                        tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-2",
                               style="color:#0c4a6e;", "Load Config"),
                        selectInput("dash_load_select", NULL,
                                    choices=c("Select saved dashboard..."="", setNames(nms,nms)),
                                    width="100%"),
                        actionButton("dash_load_btn",
                                     HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">upload</span>&nbsp;Load'),
                                     style=paste0("background:#0c4a6e;color:white;border:none;font-weight:700;",
                                                  "padding:8px 12px;border-radius:6px;cursor:pointer;",
                                                  "margin-top:8px;width:100%;font-size:11px;"))),
               
               # 3 Export + Delete
               tags$div(class="p-4 rounded-xl", style="background:#fff7ed;border:1px solid #fed7aa;",
                        tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-2",
                               style="color:#92400e;", "Export / Delete"),
                        downloadButton("dash_export_bundle",
                                       HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">download</span>&nbsp;Export Bundle'),
                                       style=paste0("background:#ea580c;color:white;border:none;font-weight:700;",
                                                    "padding:6px 10px;border-radius:6px;cursor:pointer;",
                                                    "font-size:10px;width:100%;display:block;text-align:center;")),
                        actionButton("dash_delete_btn",
                                     HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">delete</span>&nbsp;Delete Selected'),
                                     style=paste0("background:#dc2626;color:white;border:none;font-weight:700;",
                                                  "padding:6px 10px;border-radius:6px;cursor:pointer;",
                                                  "font-size:10px;width:100%;margin-top:4px;"))),
               
               # 4 Import Bundle
               tags$div(class="p-4 rounded-xl", style="background:#faf5ff;border:1px solid #e9d5ff;",
                        tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-2",
                               style="color:#6d28d9;",
                               HTML('<span class="material-symbols-outlined text-xs" style="vertical-align:middle;">cloud_upload</span>&nbsp;Import Bundle')),
                        tags$p(class="text-[10px] mb-2", style="color:#7c3aed;",
                               "Upload a previously exported .json bundle to restore configs."),
                        fileInput("dash_import_file", NULL, accept=".json", width="100%",
                                  buttonLabel=HTML('<span style="font-size:10px;font-weight:700;">Choose .json</span>'),
                                  placeholder="No file selected"))
      ),
      
      # Status
      uiOutput("dash_status_display"),
      
      # Saved configs list (compact preview)
      if (n_saved > 0)
        tags$div(class="mt-4 pt-4", style="border-top:1px solid #f1f5f9;",
                 tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-3",
                        style="color:#64748b;", "Saved Configs"),
                 tags$div(class="grid grid-cols-2 gap-2",
                          lapply(seq_along(nms), function(i) {
                            k   <- nms[i]
                            obj <- state$configs[[k]]
                            cfg <- obj$config %||% list()
                            tags$div(class="flex items-center justify-between px-3 py-2 rounded-lg",
                                     style="background:#f8fafc;border:1px solid #f1f5f9;",
                                     tags$div(class="flex items-center gap-2",
                                              tags$span(class="text-[10px] font-bold w-5 h-5 rounded-full flex items-center justify-center flex-shrink-0",
                                                        style="background:#e2e8f0;color:#64748b;", i),
                                              tags$div(
                                                tags$p(class="text-xs font-bold mb-0",style="color:#0e1d28;", obj$name %||% k),
                                                tags$p(class="text-[10px] mb-0",style="color:#94a3b8;",
                                                       paste0(cfg$chart_type%||%"—"," · ",
                                                              cfg$x_axis%||%"—"," vs ",cfg$y_axis%||%"—")))),
                                     tags$span(class="text-[10px]",style="color:#cbd5e1;", obj$created%||%""))
                          })))
      else NULL
    )
  })
  
  # ─── Status banner ────────────────────────────────────────────────────────
  output$dash_status_display <- renderUI({
    msg <- state$status_msg
    if (is.null(msg) || !nchar(msg)) return(NULL)
    ok  <- isTRUE(state$status_ok)
    tags$div(class="mt-4 p-3 rounded-lg",
             style=paste0("background:",if(ok)"#f0fdf4" else "#fee2e2",
                          ";border:1px solid ",if(ok)"#bbf7d0" else "#fecaca",";"),
             tags$div(class="flex items-center gap-2",
                      tags$span(class="material-symbols-outlined text-sm",
                                style=paste0("color:",if(ok)"#166534" else "#7f1d1d",";"),
                                if(ok)"check_circle" else "error"),
                      tags$span(class="text-xs font-bold",
                                style=paste0("color:",if(ok)"#166534" else "#7f1d1d",";"), msg)))
  })
  
  # ─── Button handlers ──────────────────────────────────────────────────────
  observeEvent(input$dash_save_btn, {
    nm <- trimws(input$dash_save_name %||% "")
    if (!nchar(nm)) { set_status("Enter a dashboard name", FALSE); return() }
    r <- op_save(nm, chart_config)
    set_status(r$message, r$success)
    if (r$success) { updateTextInput(session, "dash_save_name", value=""); refresh_dd() }
  })
  
  observeEvent(input$dash_load_btn, {
    k <- input$dash_load_select %||% ""
    if (!nchar(trimws(k))) { set_status("Select a dashboard first", FALSE); return() }
    r <- op_load(k)
    if (r$success) apply_cfg_to_ui(r$config)
    set_status(r$message, r$success)
  })
  
  observeEvent(input$dash_delete_btn, {
    k    <- input$dash_load_select %||% ""
    name <- (state$configs[[safe_key(k)]]$name) %||% k
    if (!nchar(trimws(k))) { set_status("Select a dashboard to delete", FALSE); return() }
    
    showModal(modalDialog(
      title = tags$div(class="flex items-center gap-2",
                       tags$span(class="material-symbols-outlined",style="color:#dc2626;","warning"),
                       "Confirm Delete"),
      tags$p(class="text-sm",style="color:#475569;", paste0("Delete '", name, "'?")),
      if (!use_fs)
        tags$p(class="text-xs mt-2 p-2 rounded-lg",style="background:#fef3c7;color:#92400e;",
               "\u26a0\ufe0f Cloud mode: export the bundle first if you want to keep this config.")
      else NULL,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("dash_delete_confirm","Delete",
                     style="background:#dc2626;color:white;border:none;padding:6px 14px;border-radius:6px;cursor:pointer;font-weight:600;")
      ),
      easyClose=FALSE
    ))
  })
  
  observeEvent(input$dash_delete_confirm, {
    r <- op_delete(input$dash_load_select %||% "")
    removeModal()
    set_status(r$message, r$success)
    refresh_dd()
  })
  
  # ─── Return API ───────────────────────────────────────────────────────────
  list(
    save    = op_save,
    load    = op_load,
    delete  = op_delete,
    configs = reactive(state$configs),
    list    = reactive(names(state$configs))
  )
}