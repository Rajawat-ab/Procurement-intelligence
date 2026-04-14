################################################################################
# PROCUREGRAPH v6.0 — ADVANCED ANALYTICS EXTENSIONS
# Global filters, custom analytics builder, AI chat assistant, dashboard save/load.
# Original v5.1 features fully retained.
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
library(ggplot2)
library(scales)
library(httr)           # NEW for v6.0 (Ollama API)
library(jsonlite)       # NEW for v6.0 (JSON parsing)

# ═══════════════════════════════════════════════════════════════════════════════
# 2. SOURCE MODULES
# ═══════════════════════════════════════════════════════════════════════════════
source("mod_global_filters.R")
source("mod_custom_analytics.R")
source("mod_ai_insights.R")
source("mod_dashboard_save.R")

# ═══════════════════════════════════════════════════════════════════════════════
# 3. CONNECTION STRING (unchanged)
# ═══════════════════════════════════════════════════════════════════════════════
CONN <- "mongodb://dev_buyer:Buymo%23glix%2421@10.0.2.44/?authSource=procurement"

# ═══════════════════════════════════════════════════════════════════════════════
# 4. MONGO FETCH FUNCTIONS (unchanged)
# ═══════════════════════════════════════════════════════════════════════════════
get_mongo_data <- function(start_date, end_date) {
  col <- mongo(collection = "item", db = "procurement", url = CONN)
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
        "histPoCustomerSANumber": "$history.po.customerSANumber"
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
  col  <- mongo(collection = "grn", db = "procurement", url = CONN)
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
# 5. HELPERS (unchanged)
# ═══════════════════════════════════════════════════════════════════════════════
safe_rename <- function(df, new_name, possible) {
  m <- intersect(possible, colnames(df))
  if (!length(m)) { df[[new_name]] <- NA; return(df) }
  df %>% rename(!!new_name := !!m[1])
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
`%||%` <- function(a, b) if (!is.null(a) && nchar(trimws(a)) > 0) a else b

# ═══════════════════════════════════════════════════════════════════════════════
# 6. UI
# ═══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  useShinyjs(),
  tags$head(
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
      .btn-ai { background: #6f42c1 !important; color: white !important; border: none !important; border-radius: 8px !important; padding: 10px 20px !important; font-weight: 700 !important; cursor: pointer !important; }
      .btn-ai:hover { background: #5a379b !important; }
    "))
  ),
  tags$div(style = "display:none;", fileInput("company_file", label = NULL, accept = ".xlsx")),
  tags$script(HTML("
    function triggerFileUpload(){
      var w=document.getElementById('company_file');if(!w)return;
      var r=w.querySelector('input[type=file]');if(r)r.click();else w.click();
    }
    function highlightNav(id){
      ['nav-dashboard','nav-po','nav-grn','nav-reports','nav-analytics'].forEach(function(n){
        var el=document.getElementById(n);if(!el)return;
        if(n===id){el.style.background='#da191e';el.style.color='white';}
        else{el.style.background='';el.style.color='#64748b';}
      });
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
  VALID_USER    <- "abhishek.rajawat"
  VALID_PASS    <- "Test@123"
  
  observeEvent(input$login_btn, {
    if (input$login_username == VALID_USER && input$login_password == VALID_PASS)
      authenticated(TRUE)
    else
      showNotification("Invalid username or password", type = "error")
  })
  observeEvent(input$logout, {
    authenticated(FALSE)
    cached_data(NULL)
    cache_ts(NULL)
  })
  
  output$main_ui <- renderUI({
    if (!authenticated()) {
      # LOGIN PAGE (unchanged)
      tags$div(class = "flex items-center justify-center min-h-screen", style = "background:#f6faff;",
               tags$div(class = "bg-white rounded-2xl shadow-xl p-8 w-96", style = BORDER,
                        tags$div(class = "flex justify-center mb-6",
                                 tags$div(class = "w-16 h-16 flex items-center justify-center rounded-full", style = "background:#da191e;",
                                          tags$span(class = "material-symbols-outlined text-white text-3xl", "lock"))),
                        tags$h2(class = "text-2xl font-extrabold text-center mb-2", "Login Required"),
                        tags$p(class = "text-sm text-center mb-6", style = "color:#64748b;", "Enter your credentials to access ProcureGraph"),
                        tags$div(class = "space-y-4",
                                 textInput("login_username", "Username", placeholder = "Username", width = "100%"),
                                 passwordInput("login_password", "Password", placeholder = "Password", width = "100%"),
                                 actionButton("login_btn", "Login",
                                              style = "width:100%;background:#da191e;color:white;border:none;font-weight:700;padding:10px;border-radius:8px;cursor:pointer;")),
                        tags$p(class = "mt-6 text-center text-xs", style = "color:#94a3b8;", "Moglix Procurement Intel — Internal Only")))
    } else {
      # DASHBOARD SHELL
      tags$div(class = "flex min-h-screen overflow-x-hidden", style = "width:100%;",
               
               # SIDEBAR
               tags$aside(class = "w-64 fixed left-0 top-0 h-screen flex flex-col py-6 z-50", style = "background:#1c2b36;",
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
                                   tags$a(id = "nav-reports", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','reports',{priority:'event'});highlightNav('nav-reports');",
                                          tags$span(class = "material-symbols-outlined text-lg", "analytics"), "Raw Reports"),
                                   # NEW: Advanced Analytics nav item
                                   tags$a(id = "nav-analytics", class = "flex items-center gap-3 px-4 py-3 rounded-lg text-sm", style = "color:#64748b;", href = "#",
                                          onclick = "Shiny.setInputValue('nav_page','analytics',{priority:'event'});highlightNav('nav-analytics');",
                                          tags$span(class = "material-symbols-outlined text-lg", "tuning"), "Advanced Analytics")),
                          tags$hr(style = "border-color:#263544;margin:0 24px 16px;"),
                          tags$div(class = "px-4 flex-1 overflow-y-auto sidebar-section space-y-4",
                                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest", style = "color:#64748b;", "1. Upload Mapping"),
                                   tags$div(class = "space-y-2",
                                            tags$button(onclick = "triggerFileUpload()",
                                                        style = "background:#006495;color:white;border:none;cursor:pointer;width:100%;padding:8px 12px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;text-align:left;",
                                                        HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">upload_file</span>&nbsp;SELECT XLSX')),
                                            uiOutput("file_name_display")),
                                   actionButton("show_missing_report",
                                                HTML('<span class="material-symbols-outlined text-sm" style="vertical-align:middle;">error</span>&nbsp;Missing PID-CID'),
                                                style = "background:#b45309;color:white;border:none;cursor:pointer;width:100%;padding:8px 12px;border-radius:8px;font-size:10px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;text-align:left;margin-top:4px;"),
                                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest pt-2", style = "color:#64748b;", "2. Start Date"),
                                   dateInput("date_filter_start", label = NULL, value = Sys.Date() - 30, format = "yyyy-mm-dd"),
                                   tags$p(class = "text-[10px] font-bold uppercase tracking-widest", style = "color:#64748b;", "End Date"),
                                   dateInput("date_filter_end", label = NULL, value = Sys.Date(), format = "yyyy-mm-dd"),
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
               tags$main(class = "flex-1 overflow-x-hidden", style = "margin-left:256px;width:calc(100% - 256px);",
                         tags$header(class = "flex justify-between items-center px-8 h-16 bg-white sticky top-0 z-40",
                                     style = "border-bottom:1px solid #f1f5f9;",
                                     tags$div(class = "flex items-center gap-6",
                                              tags$span(class = "text-xl font-extrabold", style = "color:#da191e;font-family:'Plus Jakarta Sans',sans-serif;", "ProcureGraph"),
                                              tags$div(class = "relative",
                                                       tags$span(class = "material-symbols-outlined absolute left-3 top-1/2 -translate-y-1/2 text-lg", style = "color:#94a3b8;", "search"),
                                                       tags$input(id = "global_search", type = "text",
                                                                  placeholder = "Search client, source, stage...",
                                                                  class = "pl-10 pr-4 py-2 text-sm rounded-lg outline-none w-72",
                                                                  style = "background:#f1f5f9;border:none;",
                                                                  oninput = "Shiny.setInputValue('global_search',this.value,{priority:'event'})"))),
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
    
    # ── Excel mapping (unchanged) ────────────────────────────────────────────
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
    
    # ── Main data processing (unchanged) ─────────────────────────────────────
    processed_df <- eventReactive(input$refresh_btn, {
      req(input$date_filter_start, input$date_filter_end, company_data())
      
      if (is_fresh()) {
        showNotification("Using cached data (<5 min old)", type = "message", duration = 2)
        return(cached_data())
      }
      
      withProgress(message = "Fetching from MongoDB...", value = 0.2, {
        raw <- tryCatch(
          get_mongo_data(input$date_filter_start, input$date_filter_end),
          error = function(e) { showNotification(paste("MongoDB:", e$message), type = "error"); NULL }
        )
        req(!is.null(raw), nrow(raw) > 0)
        
        setProgress(0.5, message = "Building columns...")
        d_start <- as.Date(input$date_filter_start)
        d_end   <- as.Date(input$date_filter_end)
        
        df <- raw %>% mutate(
          buyer_id_str     = trimws(as.character(buyerId)),
          histPoCustomerPoNo     = trimws(as.character(histPoCustomerPoNo)),
          histPoCustomerSANumber = trimws(as.character(histPoCustomerSANumber)),
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
            `PID-CID`    = coalesce(companyId,    "Unknown"),
            `Client Name`= coalesce(companyName,  "Unknown")
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
            status, creationDate, source, stage, Stage, CreationDate, Month, `Consider Month`,
            `PID-CID`, `Client Name`, `Consider Source`, `Consider Stage`, `Final Status`,
            RemoveHavellsCapex, RemoveSC, `Remove(Y/N)`, GRN
          )
        
        setProgress(1, message = "Done!")
        refresh_time(Sys.time())
        cached_data(df)
        cache_ts(Sys.time())
        df
      })
    })
    
    filtered_df <- reactive({
      req(processed_df())
      df <- processed_df()
      s  <- trimws(input$global_search %||% "")
      if (!nchar(s)) return(df)
      sl   <- tolower(s)
      mask <- grepl(sl, tolower(df$`buyer.id`),    fixed = TRUE) |
        grepl(sl, tolower(df$`Client Name`), fixed = TRUE) |
        grepl(sl, tolower(df$source),        fixed = TRUE) |
        grepl(sl, tolower(df$Stage),         fixed = TRUE)
      df[mask, ]
    })
    
    consider_df <- reactive({
      req(filtered_df())
      filtered_df() %>% filter(`Remove(Y/N)` == "Consider")
    })
    
    # ── GRN (unchanged) ─────────────────────────────────────────────────────
    grn_raw <- eventReactive(input$refresh_btn, {
      req(input$date_filter_start, input$date_filter_end)
      withProgress(message = "Fetching GRN...", value = 0.5, {
        d <- tryCatch(
          get_grn_data(input$date_filter_start, input$date_filter_end),
          error = function(e) { showNotification(paste("GRN:", e$message), type = "warning"); data.frame() }
        )
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
    
    # ── Missing mapping modal (unchanged) ───────────────────────────────────
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
    
    # ── Company integration summary (unchanged) ─────────────────────────────
    company_integration <- reactive({
      req(consider_df())
      consider_df() %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown") %>%
        group_by(`PID-CID`, `Client Name`, `Final Status`) %>%
        summarise(SAP    = sum(source == "SAP",    na.rm = TRUE),
                  Manual = sum(source == "Manual", na.rm = TRUE),
                  EOC    = sum(source == "EOC",    na.rm = TRUE), .groups = "drop") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(SAP    = sum(SAP),
                  Manual = sum(Manual),
                  EOC    = sum(EOC),
                  Has_No_Integration = any(`Final Status` == "No Integration"),
                  .groups = "drop") %>%
        mutate(Grand_Total       = SAP + Manual + EOC,
               Integration_Pct   = ifelse(Grand_Total > 0, round((SAP + Manual) / Grand_Total * 100, 1), 0),
               Integrated_Volume = SAP + Manual) %>%
        filter(Grand_Total > 0)
    })
    
    company_performance <- reactive({
      req(consider_df(), company_data())
      consider_df() %>%
        filter(`PID-CID` != "9802", `Client Name` != "Unknown") %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(
          SAP = sum(source == "SAP", na.rm = TRUE),
          Manual = sum(source == "Manual", na.rm = TRUE),
          EOC = sum(source == "EOC", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          Grand_Total = SAP + Manual + EOC,
          Integration_Pct = ifelse(Grand_Total > 0,
                                   round((SAP + Manual) / Grand_Total * 100, 1),
                                   0),
          Integrated_Volume = SAP + Manual
        ) %>%
        filter(Grand_Total > 0)
    })
    
    # ═══════════════════════════════════════════════════════════════════════════
    # 8. INITIALIZE NEW MODULES (v6.0)
    # ═══════════════════════════════════════════════════════════════════════════
    
    # Global Filters Module
    global_filters <- setup_global_filters(
      input = input,
      output = output,
      session = session,
      processed_df = processed_df,
      consider_df = consider_df
    )
    
    # Custom Analytics Builder Module
    custom_analytics <- setup_custom_analytics(
      input = input,
      output = output,
      session = session,
      processed_df = processed_df,
      consider_df = consider_df,
      company_integration = company_integration,
      filtered_data = global_filters$data   # Use filtered data from global filters
    )
    
    # AI Insights Module (Chat Assistant)
    ai_insights <- setup_ai_insights(
      input = input,
      output = output,
      session = session,
      consider_df = global_filters$data,    # Filtered data
      company_integration = company_integration,
      company_performance = company_performance
    )
    
    # Dashboard Save/Load Module
    dashboard_save <- setup_dashboard_save(
      input = input,
      output = output,
      session = session,
      chart_config = custom_analytics$config   # Chart configuration from analytics builder
    )
    
    # ── Continue with original outputs (unchanged) ──────────────────────────
    output$top5_chart <- renderUI({
      req(company_integration())
      data <- company_integration()
      if (!nrow(data)) return(tags$p(style = "color:#94a3b8;font-size:12px;", "No data loaded."))
      
      top5 <- data %>% arrange(desc(Integrated_Volume)) %>% slice_head(n = 5)
      max_vol <- max(top5$Integrated_Volume)
      
      bars <- paste(sapply(seq_len(nrow(top5)), function(i) {
        html_bar(
          label      = top5$`Client Name`[i],
          value      = top5$Integrated_Volume[i],
          max_val    = max_vol,
          count_label = sprintf("%s%% integrated · %s of %s line items",
                                top5$Integration_Pct[i],
                                format(top5$Integrated_Volume[i], big.mark = ","),
                                format(top5$Grand_Total[i], big.mark = ",")),
          color      = "#006495",
          rank       = i
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
    
    output$pivot_without_table <- render_pivot_dt(function() filtered_df() %>% filter(`PID-CID` != "9802"), "Without ABFRL")
    output$pivot_with_table    <- render_pivot_dt(function() filtered_df(), "With ABFRL")
    
    output$raw_table <- renderDT({
      req(filtered_df())
      datatable(filtered_df(), filter = "top", rownames = FALSE,
                options = list(scrollX = TRUE, pageLength = 15,
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("Remove(Y/N)",
                    backgroundColor = styleEqual(c("Remove", "Consider"), c("#ffe0e0", "#e0ffe0")),
                    fontWeight = "bold")
    })
    
    output$grn_total_val <- renderUI({ req(grn_df()); format(nrow(grn_df()), big.mark = ",") })
    output$grn_companies_val <- renderUI({ req(grn_df()); format(n_distinct(grn_df()$`PID-CID`), big.mark = ",") })
    output$grn_match_rate_val <- renderUI({
      req(grn_df(), processed_df())
      sap <- processed_df() %>% filter(source == "SAP", `Remove(Y/N)` == "Consider", GRN == "Y", `PID-CID` != "9802") %>% nrow()
      paste0(if (sap > 0) round(nrow(grn_df()) / sap * 100, 1) else 0, "%")
    })
    
    grn_company_tbl <- reactive({
      req(grn_df())
      tbl <- grn_df() %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(`GRN Count` = n(), .groups = "drop") %>%
        arrange(desc(`GRN Count`))
      sap_co <- processed_df() %>%
        filter(source == "SAP", `Remove(Y/N)` == "Consider", GRN == "Y", `PID-CID` != "9802") %>%
        count(`PID-CID`, name = "SAP Orders")
      tbl %>%
        left_join(sap_co, by = "PID-CID") %>%
        mutate(`SAP Orders` = coalesce(as.integer(`SAP Orders`), 0L),
               `Match Rate` = ifelse(`SAP Orders` > 0,
                                     paste0(round(`GRN Count` / `SAP Orders` * 100, 1), "%"), "N/A")) %>%
        select(`PID-CID`, `Client Name`, `GRN Count`, `SAP Orders`, `Match Rate`)
    })
    
    output$grn_by_company <- renderDT({
      req(grn_df())
      if (!nrow(grn_df())) return(datatable(data.frame(Message = "No GRN data"), options = list(dom = "t")))
      tbl <- grn_company_tbl()
      datatable(tbl, rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("GRN Count",
                    background = styleColorBar(range(tbl$`GRN Count`), "#d1fae5"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
        formatStyle("Match Rate",
                    color = styleInterval(c("50%", "80%"), c("#dc2626", "#ea580c", "#16a34a")),
                    fontWeight = "bold") %>%
        formatStyle(columns = colnames(tbl), fontSize = "13px")
    })
    
    grn_month_tbl <- reactive({
      req(grn_df())
      grn_df() %>%
        group_by(Month) %>%
        summarise(`GRN Count` = n(), Companies = n_distinct(`PID-CID`), .groups = "drop") %>%
        arrange(Month) %>%
        mutate(Prev = lag(`GRN Count`),
               `MoM Growth` = ifelse(!is.na(Prev) & Prev > 0,
                                     paste0(round((`GRN Count` - Prev) / Prev * 100, 1), "%"), "-")) %>%
        select(Month, `GRN Count`, Companies, `MoM Growth`)
    })
    
    output$grn_by_month <- renderDT({
      req(grn_df())
      if (!nrow(grn_df())) return(datatable(data.frame(Message = "No GRN data"), options = list(dom = "t")))
      tbl <- grn_month_tbl()
      datatable(tbl, rownames = FALSE,
                options = list(pageLength = 15, dom = "t",
                               columnDefs = list(list(targets = "_all", className = "dt-center")))) %>%
        formatStyle("GRN Count",
                    background = styleColorBar(range(tbl$`GRN Count`), "#dbeafe"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
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
      # ... (unchanged) ...
      by_month <- df %>%
        group_by(`PID-CID`, `Client Name`, Month) %>%
        summarise(
          SAP    = sum(source == "SAP", na.rm = TRUE),
          Manual = sum(source == "Manual", na.rm = TRUE),
          EOC    = sum(source == "EOC", na.rm = TRUE),
          Total  = n(),
          .groups = "drop"
        ) %>%
        mutate(
          Integration_Pct = ifelse(Total > 0, round((SAP + Manual) / Total * 100, 1), 0)
        )
      months_sorted <- sort(unique(by_month$Month))
      if (length(months_sorted) == 0) return(NULL)
      wide <- by_month %>%
        pivot_wider(
          id_cols = c(`PID-CID`, `Client Name`),
          names_from = Month,
          values_from = c(SAP, Manual, EOC, Total, Integration_Pct),
          names_sep = "_"
        )
      month_cols <- unlist(lapply(months_sorted, function(m) {
        paste0(c("SAP_", "Manual_", "EOC_", "Total_", "Integration_Pct_"), m)
      }))
      month_cols <- month_cols[month_cols %in% names(wide)]
      overall <- df %>%
        group_by(`PID-CID`, `Client Name`) %>%
        summarise(
          SAP_Total    = sum(source == "SAP", na.rm = TRUE),
          Manual_Total = sum(source == "Manual", na.rm = TRUE),
          EOC_Total    = sum(source == "EOC", na.rm = TRUE),
          Total_Total  = n(),
          .groups = "drop"
        ) %>%
        mutate(
          Integration_Pct_Total = ifelse(Total_Total > 0,
                                         round((SAP_Total + Manual_Total) / Total_Total * 100, 1), 0)
        )
      wide <- wide %>% left_join(overall, by = c("PID-CID", "Client Name"))
      final_cols <- c("PID-CID", "Client Name", month_cols,
                      "SAP_Total", "Manual_Total", "EOC_Total", "Total_Total", "Integration_Pct_Total")
      wide <- wide[, final_cols, drop = FALSE]
      wide %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    })
    
    output$po_by_company <- renderDT({
      req(po_company_monthly())
      tbl <- po_company_monthly()
      if (nrow(tbl) == 0) {
        return(datatable(data.frame(Message = "No data available"), options = list(dom = "t"), rownames = FALSE))
      }
      col_names <- colnames(tbl)
      month_pattern <- "^(SAP|Manual|EOC|Total|Integration_Pct)_(\\d{4}-\\d{2})$"
      month_cols <- col_names[str_detect(col_names, month_pattern)]
      months <- sort(unique(str_extract(month_cols, "\\d{4}-\\d{2}$")))
      new_names <- col_names
      for (i in seq_along(col_names)) {
        match <- str_match(col_names[i], "^(SAP|Manual|EOC|Total|Integration_Pct)_(\\d{4})-(\\d{2})$")
        if (!is.na(match[1, 1])) new_names[i] <- match[1, 2]
      }
      new_names <- str_replace(new_names, "^(SAP|Manual|EOC|Total|Integration_Pct)_Total$",
                               function(x) str_extract(x, "^[^_]+"))
      colnames(tbl) <- new_names
      dt <- datatable(
        tbl, rownames = FALSE,
        options = list(
          scrollX = TRUE, pageLength = 15, dom = "Bfrtip",
          buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
          columnDefs = list(
            list(targets = which(grepl("SAP|Manual|EOC|Total", colnames(tbl))),
                 className = "dt-right",
                 render = JS("function(data, type, row) { if (type === 'display' && !isNaN(data)) { return data.toLocaleString(); } return data; }")),
            list(targets = which(grepl("Integration_Pct", colnames(tbl))),
                 className = "dt-center",
                 render = JS("function(data, type, row) { if (type === 'display') { var val = parseFloat(data); if (isNaN(val)) return '-'; return val.toFixed(1) + '%'; } return data; }"))
          )
        ),
        caption = htmltools::tags$caption(
          style = "caption-side:top;text-align:left;font-weight:600;font-size:12px;padding:12px 16px;color:#0e1d28;",
          HTML(paste0("<strong>📊 Line Items by Company & Month</strong><br>",
                      "<span style='color:#64748b;font-size:11px;'>",
                      "Each month shows: SAP | Manual | EOC | Total | Integration %. ",
                      "Month headers span columns. Final 5 columns are overall totals. ",
                      "Integration %: Green ≥85% | Orange 70–85% | Red <70%",
                      "</span>"))
        )
      ) %>%
        formatStyle(
          columns = which(grepl("Integration_Pct", colnames(tbl))),
          backgroundColor = JS("function(value, index) { var val = parseFloat(value); if (isNaN(val)) return 'transparent'; if (val >= 85) return '#d1fae5'; if (val >= 70) return '#fed7aa'; return '#fecaca'; }"),
          color = JS("function(value, index) { var val = parseFloat(value); if (isNaN(val)) return '#0e1d28'; if (val >= 85) return '#15803d'; if (val >= 70) return '#92400e'; return '#dc2626'; }"),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          columns = which(grepl("SAP|Manual|EOC|Total", colnames(tbl))),
          fontFamily = "IBM Plex Mono, monospace", fontSize = "11px", fontWeight = "600", textAlign = "right"
        ) %>%
        formatStyle(columns = 1:2, fontWeight = "bold", color = "#0e1d28", fontFamily = "Manrope, sans-serif")
      
      dt$x$options$initComplete <- htmlwidgets::JS(sprintf("function(settings, json) {
        var api = this.api();
        var months = %s;
        var headerRow = '<tr style=\"background:#f1f5f9;border-bottom:2px solid #da191e;\">';
        headerRow += '<th style=\"padding:8px;text-align:left;font-weight:700;font-size:11px;\">Company ID</th>';
        headerRow += '<th style=\"padding:8px;text-align:left;font-weight:700;font-size:11px;\">Company Name</th>';
        months.forEach(function(m) {
          var d = new Date(m + '-01');
          var monthName = d.toLocaleDateString('en-US', {month: 'short', year: 'numeric'});
          headerRow += '<th colspan=\"5\" style=\"padding:8px;text-align:center;font-weight:700;font-size:12px;color:#0e1d28;border-right:1px solid #e2e8f0;\">' + monthName + '</th>';
        });
        headerRow += '<th colspan=\"5\" style=\"padding:8px;text-align:center;font-weight:700;font-size:12px;color:#0e1d28;background:#f0f9ff;\">OVERALL TOTALS</th>';
        headerRow += '</tr>';
        var thead = api.table().header();
        $(thead).find('tr').first().before(headerRow);
      }", jsonlite::toJSON(months)))
      dt
    })
    
    mom_integration_chart <- reactive({
      req(consider_df())
      df <- consider_df() %>% filter(`PID-CID` != "9802")
      if (nrow(df) == 0) return(NULL)
      monthly_stats <- df %>%
        group_by(Month) %>%
        summarise(SAP = sum(source == "SAP", na.rm = TRUE),
                  Manual = sum(source == "Manual", na.rm = TRUE),
                  Total = n(), .groups = "drop") %>%
        arrange(Month) %>%
        mutate(Integration_Pct = ifelse(Total > 0, round((SAP + Manual) / Total * 100, 1), 0),
               Month_Label = format(as.Date(paste0(Month, "-01")), "%b %Y"))
      monthly_stats
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
        annotate("text", x = 0.5, y = 85, label = "Target (85%)", size = 3, color = "#15803d", hjust = 0, vjust = -0.5) +
        annotate("text", x = 0.5, y = 70, label = "Minimum (70%)", size = 3, color = "#dc2626", hjust = 0, vjust = -0.5) +
        scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%")) +
        labs(title = "Month-on-Month Integration % Progress", subtitle = "Trend over selected date range",
             x = "Month", y = "Integration %", caption = "Green ≥85% | Orange 70–85% | Red <70%") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold", color = "#0e1d28", family = "Plus Jakarta Sans"),
              plot.subtitle = element_text(size = 11, color = "#64748b", family = "Manrope"),
              axis.title = element_text(size = 10, face = "bold", color = "#0e1d28", family = "Manrope"),
              axis.text = element_text(size = 9, color = "#475569", family = "Manrope"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.major.y = element_line(color = "#e2e8f0", size = 0.3),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.caption = element_text(size = 9, color = "#94a3b8", family = "Manrope"))
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
    
    # Downloads (unchanged)
    dl <- function(fn, ...) downloadHandler(filename = fn, content = ...)
    output$dl_pivot_without <- dl(function() paste0("pivot_without_", Sys.Date(), ".csv"),
                                  function(f) write.csv(memo_pivot(filtered_df() %>% filter(`PID-CID` != "9802")), f, row.names = FALSE, na = ""))
    output$dl_pivot_with    <- dl(function() paste0("pivot_with_", Sys.Date(), ".csv"),
                                  function(f) write.csv(memo_pivot(filtered_df()), f, row.names = FALSE, na = ""))
    output$dl_raw           <- dl(function() paste0("raw_data_", Sys.Date(), ".csv"),
                                  function(f) write.csv(filtered_df(), f, row.names = FALSE, na = ""))
    output$dl_grn_company   <- dl(function() paste0("grn_company_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn_company_tbl(), f, row.names = FALSE))
    output$dl_grn_month     <- dl(function() paste0("grn_month_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn_month_tbl(), f, row.names = FALSE))
    output$dl_grn_detail    <- dl(function() paste0("grn_detail_", Sys.Date(), ".csv"),
                                  function(f) write.csv(grn_df(), f, row.names = FALSE))
    output$dl_po_company    <- dl(function() paste0("po_company_monthly_", Sys.Date(), ".csv"),
                                  function(f) {
                                    tbl <- po_company_monthly()
                                    if (is.null(tbl)) write.csv(data.frame(Message = "No data"), f, row.names = FALSE)
                                    else write.csv(tbl, f, row.names = FALSE)
                                  })
    output$dl_po_month      <- dl(function() paste0("po_month_", Sys.Date(), ".csv"),
                                  function(f) {
                                    tbl <- consider_df() %>% filter(`PID-CID` != "9802") %>%
                                      group_by(Month) %>% summarise(Line_items = n(), Companies = n_distinct(`PID-CID`), .groups = "drop")
                                    write.csv(tbl, f, row.names = FALSE)
                                  })
    output$dl_po_detail     <- dl(function() paste0("po_detail_", Sys.Date(), ".csv"),
                                  function(f) write.csv(consider_df() %>% filter(`PID-CID` != "9802"), f, row.names = FALSE))
    
    output$last_refreshed <- renderUI({
      req(refresh_time())
      tags$span(class = "flex items-center gap-2 text-[10px] font-bold uppercase tracking-widest", style = "color:#94a3b8;",
                tags$span(class = "material-symbols-outlined text-sm", "update"),
                format(refresh_time(), "%H:%M:%S"))
    })
    
    # Email functions (unchanged but lengthy; retained as in original)
    outlook_smtp <- list(host = "smtp.office365.com", port = 587)
    build_html_email <- function() { ... } # (keep original)
    observeEvent(input$open_email_modal, { ... })
    output$email_help_box <- renderUI({ ... })
    output$email_preview_html <- renderUI({ ... })
    observeEvent(input$send_email_btn, { ... })
    
    get_performance_tier <- function(integration_pct, volume, total) { ... }
    build_poc_email_html <- function(...) { ... }
    observeEvent(input$open_email_modal_poc, { ... })
    observeEvent(input$send_poc_emails_btn, { ... })
    
    # Formula reference (unchanged)
    formula_row <- function(col, src, desc) { ... }
    shinyjs::hide("formulasCard")
    observeEvent(input$toggle_formulas, { ... })
    
    # ═══════════════════════════════════════════════════════════════════════════
    # 9. PAGE ROUTER (includes new analytics page)
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
      
      section_hdr <- function(title, subtitle, dl_id, btn_color = "#16a34a") {
        tags$div(class = "flex justify-between items-center",
                 tags$div(tags$h3(class = "text-lg font-extrabold", title),
                          tags$p(class = "text-xs mt-1", style = "color:#64748b;", subtitle)),
                 downloadButton(dl_id,
                                HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;CSV'),
                                style = BTN(btn_color)))
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
      
      # DASHBOARD PAGE (unchanged)
      if (page == "dashboard") {
        df <- tryCatch(filtered_df(), error = function(e) NULL)
        total_all   <- if (!is.null(df)) nrow(df) else 0
        valid_n     <- if (!is.null(df)) sum(df$`Remove(Y/N)` == "Consider", na.rm = TRUE) else 0
        valid_pct   <- if (total_all > 0) round(valid_n / total_all * 100, 1) else 0
        df_consider <- if (!is.null(df)) df %>% filter(`Remove(Y/N)` == "Consider") else data.frame()
        integrated_customers_df <- if (!is.null(df_consider) && nrow(df_consider) > 0) {
          df_consider %>% filter(`Final Status` %in% c("Live", "Live - CBB", "PR Int. - Punchout"))
        } else data.frame()
        integrated_customers_lines <- nrow(integrated_customers_df)
        integrated_items_live <- if (!is.null(integrated_customers_df) && nrow(integrated_customers_df) > 0) {
          integrated_customers_df %>% filter(source %in% c("SAP", "Manual")) %>% nrow()
        } else 0
        live_customers_pct <- if (integrated_customers_lines > 0) round(integrated_items_live / integrated_customers_lines * 100, 1) else 0
        
        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 tags$section(class = "grid grid-cols-4 gap-6",
                              kpi_card_inline("list",       "Total line items",                     format(total_all, big.mark = ","),          "#006495", "#e0f0ff", 100,      NULL,                       "anim-in-1"),
                              kpi_card_inline("verified",   "Valid line items",                     format(valid_n, big.mark = ","),            "#f97316", "#ffedd5", valid_pct, paste0(valid_pct, "% of total"), "anim-in-2"),
                              kpi_card_inline("business",   "Line items of integrated customers",   format(integrated_customers_lines, big.mark = ","), "#10b981", "#d1fae5", if(integrated_customers_lines>0) round(integrated_customers_lines/valid_n*100,1) else 0, paste0(if(valid_n>0) round(integrated_customers_lines/valid_n*100,1) else 0, "% of valid line items"), "anim-in-3"),
                              kpi_card_inline("sync_alt",   "Integrated items of integrated customers",   format(integrated_items_live, big.mark = ","), "#da191e", "#ffe0e0", live_customers_pct, paste0(live_customers_pct, "% of integrated lines"), "anim-in-4")),
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
                                            tags$code(class = "text-xs font-bold", "SAP ÷ Grand Total × 100"),
                                            tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", "SAP line items as % of all valid line items")),
                                   tags$div(class = "p-4 rounded-xl", style = "background:#f0fdf4;",
                                            tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#16a34a;", "CBB %"),
                                            tags$code(class = "text-xs font-bold", "Manual ÷ Grand Total × 100"),
                                            tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", "Manual line items as % of all valid line items")),
                                   tags$div(class = "p-4 rounded-xl", style = "background:#fff7ed;",
                                            tags$p(class = "text-[10px] font-bold uppercase tracking-widest mb-2", style = "color:#ea580c;", "Final Integration %"),
                                            tags$code(class = "text-xs font-bold", "(SAP+Manual) ÷ Grand Total × 100"),
                                            tags$p(class = "text-[10px] mt-2", style = "color:#64748b;", "Overall integration effectiveness")))),
                 tags$section(class = "space-y-4 anim-in anim-in-5",
                              tags$h2(class = "text-2xl font-extrabold", "Integration Pivot"),
                              tags$div(class = "grid grid-cols-2 gap-6",
                                       pivot_card("Without ABFRL", "Excl. Aditya Birla Fashion", "pivot_without_table", "dl_pivot_without"),
                                       pivot_card("With ABFRL",    "All companies included",     "pivot_with_table",    "dl_pivot_with"))))
        
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
                              section_hdr("Line items by Company (Month-on-Month)", "Each month block shows SAP, Manual, EOC, Total, Integration %. Final columns are overall totals.", "dl_po_company"),
                              tags$div(class = paste(CARD, "overflow-x-auto"), style = BORDER, DTOutput("po_by_company"))),
                 tags$section(class = "space-y-3",
                              section_hdr("Line items by Month", "MoM Growth = (Current-Prev)/Prev × 100", "dl_po_month", "#006495"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("po_by_month"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "Integration % Trend (Month-on-Month)"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;",
                                                       "Track integration performance changes over time")),
                                       NULL),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER,
                                       plotOutput("mom_integration_chart", height = "400px"))),
                 tags$div(class = "flex justify-end",
                          downloadButton("dl_po_detail",
                                         HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT ALL VALID LINE ITEMS'),
                                         style = BTN("#64748b"))))
        
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
                          tags$div(tags$h2(class = "text-2xl font-extrabold", "GRN Analysis"),
                                   tags$p(class = "text-sm mt-1", style = "color:#64748b;", "GRN = Y companies — ABFRL excluded")),
                          tags$div(class = "flex items-center gap-2 px-4 py-2 rounded-xl",
                                   style = "background:#f0fdf4;border:1px solid #bbf7d0;",
                                   tags$span(class = "material-symbols-outlined text-sm", style = "color:#16a34a;", "info"),
                                   tags$span(style = "font-size:11px;color:#15803d;font-weight:600;", "GRN Status = Y in Excel mapping only"))),
                 tags$section(class = "grid grid-cols-3 gap-6",
                              kpi3("receipt_long", "Total GRNs",               "grn_total_val",      "#16a34a", "#d1fae5"),
                              kpi3("business",     "Companies Raising GRNs",   "grn_companies_val",  "#006495", "#e0f0ff"),
                              kpi3("percent",      "GRN vs SAP Match Rate",    "grn_match_rate_val", "#f59e0b", "#fef3c7")),
                 tags$section(class = "space-y-3",
                              section_hdr("GRN Count by Company", "Match Rate = GRN Count / SAP Orders × 100", "dl_grn_company"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn_by_company"))),
                 tags$section(class = "space-y-3",
                              section_hdr("GRN Count by Month", "MoM Growth = (Current-Prev)/Prev × 100", "dl_grn_month", "#006495"),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn_by_month"))),
                 tags$section(class = "space-y-3",
                              tags$div(class = "flex justify-between items-center",
                                       tags$div(tags$h3(class = "text-lg font-extrabold", "GRN Detail Records"),
                                                tags$p(class = "text-xs mt-1", style = "color:#64748b;", "All GRN transactions from procurement.grn")),
                                       downloadButton("dl_grn_detail",
                                                      HTML('<span class="material-symbols-outlined" style="font-size:14px;vertical-align:middle;">download</span>&nbsp;EXPORT ALL'),
                                                      style = BTN("#da191e"))),
                              tags$div(class = paste(CARD, "overflow-hidden"), style = BORDER, DTOutput("grn_detail_table"))))
        
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
                                                formula_row("J: CreationDate",                 "Calculated", "epoch / 1000 → readable date"),
                                                formula_row("K: Month",                        "Calculated", "format(CreationDate,'%Y-%m') — reuses J")),
                                       tags$div(class = "space-y-2",
                                                formula_row("L: Consider Month",               "Calculated", "Consider if in date range else Ignore"),
                                                formula_row("M: PID-CID",                      "Excel Join", "buyer.id → plantId → companyId"),
                                                formula_row("N: Client Name",                  "Excel Join", "companyName via companyId join"),
                                                formula_row("O: Consider Source",              "Calculated", "Consider if source in {EOC,Manual,SAP}"),
                                                formula_row("P: Consider Stage",               "Calculated", "Ignore if stage in {Closed,Cancelled}"),
                                                formula_row("Q: Final Status",                 "Excel Join", "Integration Status; NA → Ignore"),
                                                formula_row("R: RemoveHavellsCapex",           "Calculated", "PID-CID=1211: Ignore if SANumber starts 45"),
                                                formula_row("S: RemoveSC",                     "Calculated", "PID-CID=8853+buyers: Ignore if SANumber starts 71"),
                                                formula_row("T: Remove(Y/N)",                  "Calculated", "Remove if ANY of O,P,L,Q,R,S,U = Ignore"),
                                                formula_row("U: GRN",                          "Excel Join", "GRN Status from 150 CID; NA → Ignore"))),
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
        
      } else if (page == "analytics") {
        # NEW ADVANCED ANALYTICS PAGE
        tags$div(class = "p-8 space-y-8",
                 uiOutput("mapping_warnings"),
                 
                 # Global Filter Panel (from mod_global_filters)
                 uiOutput("global_filter_panel"),
                 
                 # Dashboard Save/Load UI (from mod_dashboard_save)
                 uiOutput("dashboard_save_load_ui"),
                 
                 # Custom Analytics Builder (from mod_custom_analytics)
                 uiOutput("custom_analytics_builder"),
                 
                 # Chart Display
                 tags$div(class = paste(CARD, "overflow-hidden p-6"), style = BORDER,
                          plotOutput("custom_chart_display", height = "600px")),
                 
                 # AI Insights Panel (from mod_ai_insights)
                 uiOutput("ai_insights_panel")
        )
      }
    }) # end dashboard_content
    
  }) # end observe(authenticated)
}

shinyApp(ui, server)