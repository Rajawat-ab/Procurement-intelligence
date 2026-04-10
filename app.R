################################################################
# PROCUREGRAPH v5.1 — ENHANCED FEATURES
# Outlook‑only email, dark email headers & footers,
# month‑on‑month table with grouped headers,
# integration % colour scale, MoM line chart,
# tiered POC email templates.
################################################################

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
library(scales)   # for rescale in gradient

############################################
# CONNECTION STRING
############################################
CONN <- "mongodb://dev_buyer:Buymo%23glix%2421@10.0.2.44/?authSource=procurement"

############################################
# MONGO FETCH — v4 (aggregate pipeline)
############################################
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

############################################
# HELPERS
############################################
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

############################################
# UI
############################################
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
    "))
  ),
  tags$div(style = "display:none;", fileInput("company_file", label = NULL, accept = ".xlsx")),
  tags$script(HTML("
    function triggerFileUpload(){
      var w=document.getElementById('company_file');if(!w)return;
      var r=w.querySelector('input[type=file]');if(r)r.click();else w.click();
    }
    function highlightNav(id){
      ['nav-dashboard','nav-po','nav-grn','nav-reports'].forEach(function(n){
        var el=document.getElementById(n);if(!el)return;
        if(n===id){el.style.background='#da191e';el.style.color='white';}
        else{el.style.background='';el.style.color='#64748b';}
      });
    }
  ")),
  uiOutput("main_ui")
)

############################################
# SERVER
############################################
server <- function(input, output, session) {
  
  # ── Reactive state ──────────────────────────────────
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
      # DASHBOARD SHELL (unchanged)
      tags$div(class = "flex min-h-screen overflow-x-hidden", style = "width:100%;",
               
               # SIDEBAR (unchanged)
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
                                          tags$span(class = "material-symbols-outlined text-lg", "analytics"), "Raw Reports")),
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
               
               # MAIN CONTENT AREA
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
  
  # ── All logic post-auth ────────────────────────────────────────────────
  observe({
    req(authenticated())
    
    is_fresh <- function() {
      !is.null(cached_data()) && !is.null(cache_ts()) &&
        as.numeric(difftime(Sys.time(), cache_ts(), units = "secs")) < CACHE_SEC
    }
    
    # Invalidate cache when inputs change
    observeEvent(input$date_filter_start, cached_data(NULL))
    observeEvent(input$date_filter_end,   cached_data(NULL))
    observeEvent(input$company_file,      cached_data(NULL))
    observeEvent(input$nav_page,          current_page(input$nav_page))
    
    # ── Excel mapping ──────────────────────────────────────────────────
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
    
    # ── Main data processing (v4 — optimised) ─────────────────────────
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
    
    # ── filtered_df — v4 (fixed=TRUE plain-string search) ──
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
    
    # ── GRN ─────────────────────────────────────────────────────────────
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
    
    # ── Missing mapping modal ────────────────────────────────────────────
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
    
    # ── Company integration summary (used for Top 5 and No Integration charts) ──
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
    
    # ── New: company_performance for POC alerts ───────────────────────────
    company_performance <- reactive({
      req(consider_df(), company_data())
      
      current <- consider_df() %>%
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
      
      current
    })
    
    # ── HTML bar charts (Top 5 by Integrated Volume) ────────────────
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
    
    # ── Pivot tables ─────────────────────────────────────────────────────
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
    
    # ── GRN outputs ──────────────────────────────────────────────────────
    output$grn_total_val <- renderUI({
      req(grn_df()); format(nrow(grn_df()), big.mark = ",")
    })
    output$grn_companies_val <- renderUI({
      req(grn_df()); format(n_distinct(grn_df()$`PID-CID`), big.mark = ",")
    })
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
    
    # ── PO insight cards ───────────────────────────────────────────────
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
    
    # ── PO tables (with month‑on‑month expansion) ──────────────────────────
    po_company_monthly <- reactive({
      req(consider_df())
      df <- consider_df() %>% filter(`PID-CID` != "9802")
      if (nrow(df) == 0) return(NULL)
      
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
      
      wide <- wide %>%
        left_join(overall, by = c("PID-CID", "Client Name"))
      
      final_cols <- c("PID-CID", "Client Name", month_cols,
                      "SAP_Total", "Manual_Total", "EOC_Total", "Total_Total", "Integration_Pct_Total")
      wide <- wide[, final_cols, drop = FALSE]
      wide <- wide %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
      
      wide
    })
    
    # ENHANCED po_by_company with month-spanning headers
    output$po_by_company <- renderDT({
      req(po_company_monthly())
      tbl <- po_company_monthly()
      
      if (nrow(tbl) == 0) {
        return(datatable(
          data.frame(Message = "No data available"),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }
      
      col_names <- colnames(tbl)
      
      # Extract month columns
      month_pattern <- "^(SAP|Manual|EOC|Total|Integration_Pct)_(\\d{4}-\\d{2})$"
      month_cols <- col_names[str_detect(col_names, month_pattern)]
      months <- sort(unique(str_extract(month_cols, "\\d{4}-\\d{2}$")))
      
      # Build new column names for display (just metric names)
      new_names <- col_names
      for (i in seq_along(col_names)) {
        match <- str_match(col_names[i], "^(SAP|Manual|EOC|Total|Integration_Pct)_(\\d{4})-(\\d{2})$")
        if (!is.na(match[1, 1])) {
          new_names[i] <- match[1, 2]
        }
      }
      new_names <- str_replace(new_names, "^(SAP|Manual|EOC|Total|Integration_Pct)_Total$",
                               function(x) str_extract(x, "^[^_]+"))
      colnames(tbl) <- new_names
      
      dt <- datatable(
        tbl,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          dom = "Bfrtip",
          buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
          columnDefs = list(
            list(
              targets = which(grepl("SAP|Manual|EOC|Total", colnames(tbl))),
              className = "dt-right",
              render = JS("function(data, type, row) {
                if (type === 'display' && !isNaN(data)) {
                  return data.toLocaleString();
                }
                return data;
              }")
            ),
            list(
              targets = which(grepl("Integration_Pct", colnames(tbl))),
              className = "dt-center",
              render = JS("function(data, type, row) {
                if (type === 'display') {
                  var val = parseFloat(data);
                  if (isNaN(val)) return '-';
                  return val.toFixed(1) + '%';
                }
                return data;
              }")
            )
          )
        ),
        caption = htmltools::tags$caption(
          style = "caption-side:top;text-align:left;font-weight:600;font-size:12px;padding:12px 16px;color:#0e1d28;",
          HTML(paste0(
            "<strong>📊 Line Items by Company & Month</strong><br>",
            "<span style='color:#64748b;font-size:11px;'>",
            "Each month shows: SAP | Manual | EOC | Total | Integration %. ",
            "Month headers span columns. Final 5 columns are overall totals. ",
            "Integration %: Green ≥85% | Orange 70–85% | Red <70%",
            "</span>"
          ))
        )
      ) %>%
        formatStyle(
          columns = which(grepl("Integration_Pct", colnames(tbl))),
          backgroundColor = JS("function(value, index) {
            var val = parseFloat(value);
            if (isNaN(val)) return 'transparent';
            if (val >= 85) return '#d1fae5';
            if (val >= 70) return '#fed7aa';
            return '#fecaca';
          }"),
          color = JS("function(value, index) {
            var val = parseFloat(value);
            if (isNaN(val)) return '#0e1d28';
            if (val >= 85) return '#15803d';
            if (val >= 70) return '#92400e';
            return '#dc2626';
          }"),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          columns = which(grepl("SAP|Manual|EOC|Total", colnames(tbl))),
          fontFamily = "IBM Plex Mono, monospace",
          fontSize = "11px",
          fontWeight = "600",
          textAlign = "right"
        ) %>%
        formatStyle(
          columns = 1:2,
          fontWeight = "bold",
          color = "#0e1d28",
          fontFamily = "Manrope, sans-serif"
        )
      
      # Add month-spanning header row via JS
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
    
    # MoM integration chart data
    mom_integration_chart <- reactive({
      req(consider_df())
      df <- consider_df() %>% filter(`PID-CID` != "9802")
      if (nrow(df) == 0) return(NULL)
      
      monthly_stats <- df %>%
        group_by(Month) %>%
        summarise(
          SAP = sum(source == "SAP", na.rm = TRUE),
          Manual = sum(source == "Manual", na.rm = TRUE),
          Total = n(),
          .groups = "drop"
        ) %>%
        arrange(Month) %>%
        mutate(
          Integration_Pct = ifelse(Total > 0,
                                   round((SAP + Manual) / Total * 100, 1),
                                   0),
          Month_Label = format(as.Date(paste0(Month, "-01")), "%b %Y")
        )
      monthly_stats
    })
    
    output$mom_integration_chart <- renderPlot({
      req(mom_integration_chart())
      data <- mom_integration_chart()
      
      ggplot(data, aes(x = factor(Month_Label, levels = Month_Label),
                       y = Integration_Pct, group = 1)) +
        geom_line(color = "#006495", size = 1.2, lineend = "round") +
        geom_point(aes(fill = Integration_Pct),
                   color = "#006495",
                   size = 4,
                   shape = 21,
                   stroke = 2) +
        scale_fill_gradientn(
          colors = c("#fecaca", "#fed7aa", "#d1fae5"),
          values = scales::rescale(c(0, 70, 85, 100)),
          limits = c(0, 100),
          guide = "none"
        ) +
        geom_text(aes(label = paste0(Integration_Pct, "%")),
                  vjust = -1.5,
                  size = 3.5,
                  color = "#0e1d28",
                  fontface = "bold") +
        geom_hline(yintercept = 85, linetype = "dashed", color = "#15803d", size = 0.7, alpha = 0.5) +
        geom_hline(yintercept = 70, linetype = "dashed", color = "#dc2626", size = 0.7, alpha = 0.5) +
        annotate("text", x = 0.5, y = 85, label = "Target (85%)", size = 3, color = "#15803d", hjust = 0, vjust = -0.5) +
        annotate("text", x = 0.5, y = 70, label = "Minimum (70%)", size = 3, color = "#dc2626", hjust = 0, vjust = -0.5) +
        scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%")) +
        labs(
          title = "Month-on-Month Integration % Progress",
          subtitle = "Trend over selected date range",
          x = "Month",
          y = "Integration %",
          caption = "Green ≥85% | Orange 70–85% | Red <70%"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", color = "#0e1d28", family = "Plus Jakarta Sans"),
          plot.subtitle = element_text(size = 11, color = "#64748b", family = "Manrope"),
          axis.title = element_text(size = 10, face = "bold", color = "#0e1d28", family = "Manrope"),
          axis.text = element_text(size = 9, color = "#475569", family = "Manrope"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major.y = element_line(color = "#e2e8f0", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.caption = element_text(size = 9, color = "#94a3b8", family = "Manrope")
        )
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
    
    # ── Downloads ───────────────────────────────────────────────────────
    dl <- function(fn, ...) downloadHandler(filename = fn, content = ...)
    
    output$dl_pivot_without <- dl(
      function() paste0("pivot_without_", Sys.Date(), ".csv"),
      function(f) write.csv(memo_pivot(filtered_df() %>% filter(`PID-CID` != "9802")), f, row.names = FALSE, na = "")
    )
    output$dl_pivot_with <- dl(
      function() paste0("pivot_with_", Sys.Date(), ".csv"),
      function(f) write.csv(memo_pivot(filtered_df()), f, row.names = FALSE, na = "")
    )
    output$dl_raw <- dl(
      function() paste0("raw_data_", Sys.Date(), ".csv"),
      function(f) write.csv(filtered_df(), f, row.names = FALSE, na = "")
    )
    output$dl_grn_company <- dl(
      function() paste0("grn_company_", Sys.Date(), ".csv"),
      function(f) write.csv(grn_company_tbl(), f, row.names = FALSE)
    )
    output$dl_grn_month <- dl(
      function() paste0("grn_month_", Sys.Date(), ".csv"),
      function(f) write.csv(grn_month_tbl(), f, row.names = FALSE)
    )
    output$dl_grn_detail <- dl(
      function() paste0("grn_detail_", Sys.Date(), ".csv"),
      function(f) write.csv(grn_df(), f, row.names = FALSE)
    )
    output$dl_po_company <- dl(
      function() paste0("po_company_monthly_", Sys.Date(), ".csv"),
      function(f) {
        tbl <- po_company_monthly()
        if (is.null(tbl)) {
          write.csv(data.frame(Message = "No data"), f, row.names = FALSE)
        } else {
          write.csv(tbl, f, row.names = FALSE)
        }
      }
    )
    output$dl_po_month <- dl(
      function() paste0("po_month_", Sys.Date(), ".csv"),
      function(f) {
        tbl <- consider_df() %>% filter(`PID-CID` != "9802") %>%
          group_by(Month) %>%
          summarise(Line_items = n(), Companies = n_distinct(`PID-CID`), .groups = "drop")
        write.csv(tbl, f, row.names = FALSE)
      }
    )
    output$dl_po_detail <- dl(
      function() paste0("po_detail_", Sys.Date(), ".csv"),
      function(f) write.csv(consider_df() %>% filter(`PID-CID` != "9802"), f, row.names = FALSE)
    )
    
    output$last_refreshed <- renderUI({
      req(refresh_time())
      tags$span(class = "flex items-center gap-2 text-[10px] font-bold uppercase tracking-widest", style = "color:#94a3b8;",
                tags$span(class = "material-symbols-outlined text-sm", "update"),
                format(refresh_time(), "%H:%M:%S"))
    })
    
    # ── Formula reference ────────────────────────────────────────────────
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
    
    # ── EMAIL (standard insights) ─────────────────────────────────────────
    outlook_smtp <- list(host = "smtp.office365.com", port = 587)
    
    build_html_email <- function() {
      df          <- consider_df()
      total_valid <- nrow(df)
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
          sprintf('
            <tr>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;color:#94a3b8;font-size:12px;">%d</td>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;font-weight:600;">%s</td>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;text-align:center;font-weight:700;color:#0e1d28;">%s</td>
              <td style="padding:8px;border-bottom:1px solid #f1f5f9;text-align:right;color:#64748b;">%s</td>
            </tr>',
                  i, tbl$`Client Name`[i], tbl[[pct_col]][i], format(tbl[[vol_col]][i], big.mark = ","))),
          collapse = "")
      }
      
      top5_rows   <- make_rows(top5, "Integration_Pct", "Integrated_Volume", "No data")
      no_int_rows <- if (nrow(no_int) > 0)
        make_rows(no_int, "Orders", "SAP", "None")
      else
        '<tr><td colspan="4" style="padding:16px;text-align:center;color:#16a34a;font-weight:700;">No companies on No Integration status!</td></tr>'
      
      po_accounts  <- tryCatch(df %>% filter(source %in% c("SAP","Manual")) %>% distinct(`PID-CID`) %>% nrow(), error = function(e) 0)
      grn_accounts <- tryCatch(grn_df() %>% distinct(`PID-CID`) %>% nrow(), error = function(e) 0)
      grn_line <- tryCatch({
        g       <- grn_df()
        sap_grn <- df %>% filter(source == "SAP", GRN == "Y", `PID-CID` != "9802") %>% nrow()
        rate    <- if (sap_grn > 0) paste0(round(nrow(g) / sap_grn * 100, 1), "%") else "N/A"
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
  <div class="header">
    <h1>&#128202; ProcureGraph</h1>
    <p>Buyers Integration Intelligence &mdash; Moglix</p>
  </div>
  <div class="content">
    <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;flex-wrap:wrap;">
      <span class="badge">&#128197; ', date_rng, '</span>
      <span style="font-size:12px;color:#64748b;">Generated: ', format(Sys.time(), "%d %b %Y %H:%M"), '</span>
    </div>
    <div class="kpi-grid">
      <div class="kpi-card">
        <div class="kpi-label">Total Valid Line items</div>
        <div class="kpi-value">', format(total_valid, big.mark = ","), '</div>
        <div class="kpi-sub">Remove(Y/N) = Consider</div>
      </div>
      <div class="kpi-card">
        <div class="kpi-label">Line items of Integrated Customers</div>
        <div class="kpi-value" style="color:#10b981;">', format(integrated_orders, big.mark = ","), '</div>
        <div class="kpi-sub">', integrated_pct, '% of total valid</div>
      </div>
    </div>
    <div class="section-title">&#128200; Integration Metrics</div>
    <div class="metrics-row">
      <div class="metric-box">
        <div class="metric-label">Without ABFRL</div>
        <div class="metric-value">', m_wo$integration, '</div>
        <div style="font-size:12px;color:#64748b;">Integration %</div>
        <div style="font-size:11px;color:#94a3b8;">CBB: ', m_wo$cbb, ' &nbsp;|&nbsp; Final: ', m_wo$final, '</div>
      </div>
      <div style="width:1px;background:#e2e8f0;"></div>
      <div class="metric-box">
        <div class="metric-label">With ABFRL</div>
        <div class="metric-value">', m_wi$integration, '</div>
        <div style="font-size:12px;color:#64748b;">Integration %</div>
        <div style="font-size:11px;color:#94a3b8;">CBB: ', m_wi$cbb, ' &nbsp;|&nbsp; Final: ', m_wi$final, '</div>
      </div>
    </div>
    <div class="section-title">&#127942; Top 5 by Integrated Volume</div>
    <table>
      <thead><tr><th>#</th><th>Company</th><th>Integration %</th><th style="text-align:right;">Integrated Line items</th></tr></thead>
      <tbody>', top5_rows, '</tbody>
    </table>
    <div class="section-title">&#127919; No Integration — Target List</div>
    <p style="font-size:12px;color:#64748b;margin:0 0 8px;">Highest-volume companies still on No Integration — prioritise for onboarding</p>
    <table>
      <thead><tr><th>#</th><th>Company</th><th>Total Line items</th><th style="text-align:right;">SAP Line items</th></tr></thead>
      <tbody>', no_int_rows, '</tbody>
    </table>
    <div class="accounts-box">
      <div>
        <div style="font-size:20px;font-weight:800;color:#006495;">', po_accounts, '</div>
        <div style="font-size:11px;font-weight:600;color:#64748b;text-transform:uppercase;letter-spacing:.04em;">PO Integration Accounts</div>
      </div>
      <div style="width:1px;background:#bae6fd;"></div>
      <div>
        <div style="font-size:20px;font-weight:800;color:#16a34a;">', grn_accounts, '</div>
        <div style="font-size:11px;font-weight:600;color:#64748b;text-transform:uppercase;letter-spacing:.04em;">GRN Integration Accounts</div>
      </div>
    </div>
    <div class="section-title">&#128230; GRN Summary</div>
    <div style="background:#f0fdf4;border-radius:12px;padding:14px;margin:12px 0;font-weight:600;color:#15803d;">', grn_line, '</div>
    <p style="font-size:11px;color:#94a3b8;margin-top:16px;">Integration % = (SAP+Manual) / Total Line items &times; 100. Only line items where Remove(Y/N)=Consider, ABFRL excluded.</p>
  </div>
  <div class="footer">Sent from ProcureGraph &mdash; Moglix Procurement Intel<br>Do not reply to this email.</div>
</div>
</body></html>')
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
                   tags$p(class = "text-xs", style = "color:#64748b;", "SMTP: smtp.office365.com:587 with TLS")
                 ),
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
                       tags$li(HTML("If your organisation uses on‑premise Exchange, ask IT for the SMTP relay host"))),
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
      to       <- trimws(input$email_to   %||% "")
      from     <- trimws(input$email_from %||% "")
      pass     <- input$email_pass %||% ""
      if (!nchar(to) || !nchar(from) || !nchar(pass)) { showNotification("Please fill in all fields", type = "error"); return() }
      if (!grepl("@", to) || !grepl("@", from))        { showNotification("Enter valid email addresses", type = "error"); return() }
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
        tmp <- tempfile(fileext = ".eml")
        con <- file(tmp, open = "wb"); writeBin(charToRaw(raw_msg), con); close(con)
        curl_cmd <- sprintf(
          'curl -s --url "smtp://%s:%d" --ssl-reqd --user "%s:%s" --mail-from "%s" --mail-rcpt "%s" --upload-file "%s"',
          outlook_smtp$host, outlook_smtp$port, from, pass, from, to, tmp
        )
        exit_code <- system(curl_cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)
        unlink(tmp); removeModal()
        if (exit_code == 0)
          showNotification(paste0("Email sent to ", to), type = "message", duration = 5)
        else
          showNotification(paste0("Failed (curl exit ", exit_code, "). Check app password."), type = "error", duration = 8)
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error", duration = 8))
    })
    
    # ── TIERED POC EMAIL ALERTS ──────────────────────────────────────────
    get_performance_tier <- function(integration_pct, volume, total) {
      if (integration_pct >= 85) {
        return("excellent")
      } else if (integration_pct >= 70) {
        return("good")
      } else {
        return("critical")
      }
    }
    
    build_poc_email_html <- function(company_name, integration_pct, volume, total,
                                     date_range_start, date_range_end, performance_tier) {
      
      if (performance_tier == "excellent") {
        header_bg <- "#f0fdf4"
        header_border <- "#15803d"
        header_color <- "#15803d"
        title <- "✨ Integration Performance Update"
        subtitle <- "Great progress! Keep it up."
        icon <- "🌟"
        tone <- "Your integration performance is <strong>excellent</strong>. You're at <strong>%s%%</strong> integration rate. This demonstrates strong operational efficiency and commitment to automation."
        actions <- c(
          "Continue maintaining this excellent integration rate",
          "Consider expanding to additional buyers"
        )
        cta <- "Schedule a success review call"
      } else if (performance_tier == "good") {
        header_bg <- "#fff7ed"
        header_border <- "#f97316"
        header_color <- "#92400e"
        title <- "⚠️ Integration Performance Review"
        subtitle <- "Performance is trending down. Let's work together."
        icon <- "📊"
        tone <- "Your integration performance is at <strong>%s%%</strong>, which is below our target of 85%%. While you're above the minimum threshold (70%%), there's room for improvement. We recommend reviewing your integration setup with our technical team."
        actions <- c(
          "Schedule a meeting with your Buyers technical team"
        )
        cta <- "Schedule a technical review"
      } else {
        header_bg <- "#fecaca"
        header_border <- "#dc2626"
        header_color <- "#dc2626"
        title <- "🚨 Integration Performance Alert - Urgent Action Required"
        subtitle <- "Critical attention needed. Your integration rate has dropped significantly."
        icon <- "⚡"
        tone <- "Your integration performance has dropped to <strong>%s%%</strong>, which is below our acceptable threshold (70%%). Only %s of your %s line items are integrated. This requires immediate attention to restore operational efficiency."
        actions <- c(
          "Contact Moglix technical team immediately to disuss the failing integration"
        )
        cta <- "Start urgent support ticket"
      }
      
      manual_items <- total - volume
      manual_pct <- if (total > 0) round(manual_items / total * 100, 1) else 0
      tone_filled <- sprintf(tone, integration_pct, volume, total, manual_pct)
      
      actions_html <- paste(
        sprintf("<li style=\"margin: 8px 0; color: %s;\">%s</li>", header_color, actions),
        collapse = ""
      )
      
      sprintf('<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      background: #f6faff;
      margin: 0;
      padding: 20px;
    }
    .container {
      max-width: 650px;
      margin: 0 auto;
      background: #fff;
      border-radius: 16px;
      box-shadow: 0 10px 25px rgba(0,0,0,0.1);
      overflow: hidden;
      border: 1px solid #eef2f6;
    }
    .header {
      background: %s;
      border-bottom: 3px solid %s;
      padding: 32px 28px;
      text-align: center;
    }
    .header h1 {
      margin: 0;
      font-size: 26px;
      font-weight: 800;
      color: %s;
    }
    .header p {
      margin: 8px 0 0;
      font-size: 14px;
      color: %s;
      opacity: 0.9;
    }
    .content { padding: 28px; }
    .metric-grid {
      display: grid;
      grid-template-columns: 1fr 1fr 1fr;
      gap: 12px;
      margin: 20px 0;
    }
    .metric {
      background: #f8fafc;
      border: 1px solid #e2e8f0;
      border-radius: 12px;
      padding: 16px;
      text-align: center;
    }
    .metric-label {
      font-size: 10px;
      color: #64748b;
      text-transform: uppercase;
      font-weight: 700;
      letter-spacing: 0.05em;
      margin-bottom: 8px;
    }
    .metric-value {
      font-size: 26px;
      font-weight: 800;
      color: %s;
      margin: 8px 0;
    }
    .tone-box {
      background: %s;
      border-left: 4px solid %s;
      padding: 16px;
      border-radius: 8px;
      margin: 20px 0;
      line-height: 1.6;
      color: #0e1d28;
    }
    .action-box {
      background: %s;
      border: 1px solid %s;
      border-radius: 12px;
      padding: 20px;
      margin-top: 20px;
    }
    .action-box h3 {
      color: %s;
      margin-top: 0;
      margin-bottom: 12px;
    }
    .action-box ul {
      margin: 0;
      padding-left: 20px;
    }
    .cta-button {
      display: inline-block;
      background: %s;
      color: white;
      padding: 12px 28px;
      border-radius: 8px;
      text-decoration: none;
      font-weight: 700;
      font-size: 13px;
      margin-top: 16px;
      text-align: center;
    }
    .footer {
      background: #f8fafc;
      padding: 16px;
      text-align: center;
      font-size: 11px;
      color: #1e293b;
      border-top: 1px solid #eef2f6;
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>%s %s</h1>
      <p>%s</p>
    </div>
    <div class="content">
      <p>Hi,</p>
      <div class="tone-box">
        %s
      </div>
      <div class="metric-grid">
        <div class="metric">
          <div class="metric-label">Integrated Items</div>
          <div class="metric-value">%s</div>
        </div>
        <div class="metric">
          <div class="metric-label">Total Items</div>
          <div class="metric-value">%s</div>
        </div>
        <div class="metric">
          <div class="metric-label">Integration %%</div>
          <div class="metric-value" style="color: %s;">%s%%</div>
        </div>
      </div>
      <h3 style="color: #0e1d28; margin-top: 24px;">🎯 Recommended Actions</h3>
      <div class="action-box">
        <ul style="margin: 0; padding-left: 20px;">
          %s
        </ul>
      </div>
      <p style="margin-top: 24px; padding: 16px; background: #f8fafc; border-radius: 8px; font-size: 12px; color: #475569;">
        <strong>Report Period:</strong> %s to %s<br>
        <strong>Generated:</strong> %s<br>
        <strong>Company:</strong> %s
      </p>
    </div>
    <div class="footer">
      ProcureGraph Buyers Integration Intel | Moglix Procurement<br>
      This is an automated report. Do not reply directly.
    </div>
  </div>
</body>
</html>',
              header_bg, header_border, header_color, header_color,
              header_color, header_bg, header_border, header_bg, header_border, header_color,
              header_border, icon, title, subtitle, tone_filled,
              format(volume, big.mark = ","), format(total, big.mark = ","), header_color, integration_pct,
              actions_html,
              format(as.Date(date_range_start), "%d %b %Y"), format(as.Date(date_range_end), "%d %b %Y"),
              format(Sys.time(), "%d %b %Y %H:%M"), company_name)
    }
    
    observeEvent(input$open_email_modal_poc, {
      req(company_performance(), company_data())
      
      companies_with_poc <- company_performance() %>%
        left_join(
          company_data()$company %>% select(companyId, BusinessPOC),
          by = c("PID-CID" = "companyId")
        )
      
      has_poc <- companies_with_poc %>%
        filter(!is.na(BusinessPOC), nchar(trimws(BusinessPOC)) > 0)
      
      if (nrow(has_poc) == 0) {
        showNotification("No companies with Business POC emails found. Add BusinessPOC column to Excel mapping.",
                         type = "warning", duration = 6)
        return()
      }
      
      showModal(modalDialog(
        title = tags$div(class = "flex items-center gap-2",
                         tags$span(class = "material-symbols-outlined", style = "color:#f97316;", "warning"),
                         "Send Integration Performance Alerts (Outlook)"),
        size = "l",
        easyClose = TRUE,
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
                          tags$div(
                            tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "Your Outlook Email"),
                            textInput("poc_email_from", "", placeholder = "your.email@moglix.com", width = "100%")),
                          tags$div(
                            tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "Provider"),
                            tags$span("Outlook / Office 365", style = "font-size:12px;color:#1e40af;background:#eff6ff;padding:6px 12px;border-radius:6px;display:inline-block;width:100%;"))),
                 tags$div(
                   tags$p(class = "text-xs font-bold uppercase tracking-widest mb-1", style = "color:#64748b;", "App Password"),
                   passwordInput("poc_email_pass", "",
                                 placeholder = "16-char app password (from Outlook/Office 365)",
                                 width = "100%")),
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
                                                       tags$div(class = "text-xs font-bold",
                                                                style = "color:#16a34a;",
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
      
      if (!nchar(sender_addr) || !nchar(app_password)) {
        showNotification("Please fill sender email and app password", type = "error", duration = 5)
        return()
      }
      
      if (!grepl("@", sender_addr)) {
        showNotification("Invalid email address", type = "error", duration = 5)
        return()
      }
      
      recipients_df <- company_performance() %>%
        left_join(
          company_data()$company %>% select(companyId, BusinessPOC),
          by = c("PID-CID" = "companyId")
        ) %>%
        filter(!is.na(BusinessPOC), nchar(trimws(BusinessPOC)) > 0)
      
      if (nrow(recipients_df) == 0) {
        showNotification("No recipients with valid emails", type = "warning")
        removeModal()
        return()
      }
      
      withProgress(message = "Sending tiered emails via Outlook...", value = 0, {
        total_recipients <- nrow(recipients_df)
        sent_count <- 0
        failed_list <- c()
        
        for (i in seq_len(total_recipients)) {
          setProgress(i / total_recipients,
                      detail = paste0("Sending to ", recipients_df$`Client Name`[i], "..."))
          
          recipient_email <- trimws(recipients_df$BusinessPOC[i])
          company_name    <- recipients_df$`Client Name`[i]
          integration_pct <- recipients_df$Integration_Pct[i]
          volume          <- recipients_df$Integrated_Volume[i]
          total           <- recipients_df$Grand_Total[i]
          
          if (!grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", recipient_email)) {
            failed_list <- c(failed_list, paste0(company_name, " (invalid: ", recipient_email, ")"))
            next
          }
          
          tier <- get_performance_tier(integration_pct, volume, total)
          
          html_body <- build_poc_email_html(
            company_name = company_name,
            integration_pct = integration_pct,
            volume = volume,
            total = total,
            date_range_start = input$date_filter_start,
            date_range_end = input$date_filter_end,
            performance_tier = tier
          )
          
          tryCatch({
            subject_prefix <- if (tier == "excellent") {
              "✨ Integration Performance Update"
            } else if (tier == "good") {
              "⚠️ Integration Review Required"
            } else {
              "🚨 URGENT: Integration Performance Alert"
            }
            
            subject <- paste0(subject_prefix, " - ", company_name, " | ", format(Sys.Date(), "%d %b %Y"))
            
            raw_message <- paste0(
              "From: ProcureGraph <", sender_addr, ">\r\n",
              "To: ", recipient_email, "\r\n",
              "Subject: ", subject, "\r\n",
              "MIME-Version: 1.0\r\n",
              "Content-Type: text/html; charset=UTF-8\r\n",
              "Content-Transfer-Encoding: 8bit\r\n",
              "X-Mailer: ProcureGraph/v5.1\r\n\r\n",
              html_body, "\r\n"
            )
            
            temp_eml <- tempfile(fileext = ".eml")
            writeBin(charToRaw(raw_message), file(temp_eml, open = "wb"))
            
            curl_command <- sprintf(
              'curl -s --url "smtp://%s:%d" --ssl-reqd --user "%s:%s" --mail-from "%s" --mail-rcpt "%s" --upload-file "%s" 2>&1',
              outlook_smtp$host, outlook_smtp$port,
              sender_addr, app_password, sender_addr, recipient_email, temp_eml
            )
            exit_code <- system(curl_command, ignore.stdout = TRUE, ignore.stderr = TRUE)
            unlink(temp_eml)
            
            if (exit_code == 0) {
              sent_count <- sent_count + 1
            } else {
              failed_list <- c(failed_list, paste0(company_name, " (SMTP error)"))
            }
            
          }, error = function(e) {
            failed_list <<- c(failed_list, paste0(company_name, " (exception)"))
          })
        }
        
        setProgress(1)
      })
      
      removeModal()
      
      summary_msg <- paste0("✓ Sent to ", sent_count, " POC(s)")
      if (length(failed_list) > 0) {
        summary_msg <- paste0(summary_msg, "\n✗ Failed (", length(failed_list), "): ",
                              paste(failed_list[1:min(3, length(failed_list))], collapse = ", "))
      }
      
      showNotification(
        HTML(gsub("\n", "<br>", summary_msg)),
        type = if (sent_count > 0) "message" else "error",
        duration = 10
      )
    })
    
    # ── PAGE ROUTER ─────────────────────────────────────────────────────────
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
      
      # DASHBOARD PAGE
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
        
        # PO PAGE
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
        
        # GRN PAGE
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
        
        # REPORTS PAGE
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
      }
    }) # end dashboard_content
    
  }) # end observe(authenticated)
}

shinyApp(ui, server)