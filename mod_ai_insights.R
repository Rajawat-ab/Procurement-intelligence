################################################################################
# MODULE: AI Insights — Chat Assistant (Ollama + Groq dual-mode)
#
# LOCAL  : Ollama /api/chat  → gemma2:2b on localhost
#           → ollama serve  +  ollama pull gemma2:2b
#
# CLOUD  : Groq API (FREE, no credit card needed)
#           → Sign up at console.groq.com → API Keys → Create key
#           → Model: llama-3.1-8b-instant  (fast, generous free quota)
#           → Free quota: 14,400 req/day | 100K tokens/min
#           → Data is NOT stored by Groq by default (see privacy.groq.com)
#
# On ShinyApps.io free tier, Ollama cannot run — always use Groq.
# Dependencies: httr, jsonlite, shinyjs
################################################################################

setup_ai_insights <- function(input, output, session,
                              consider_df, company_integration,
                              company_performance) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  OLLAMA_HOST   <- "http://localhost:11434"
  OLLAMA_MODEL  <- "gemma2:2b"
  OLLAMA_TOUT   <- 90
  
  GROQ_URL      <- "https://api.groq.com/openai/v1/chat/completions"
  
  ai <- reactiveValues(
    loading      = FALSE,
    error        = NULL,
    timestamp    = NULL,
    chat_history = list(),
    data_summary = NULL,
    backend_used = NULL
  )
  
  # ─── Health ───────────────────────────────────────────────────────────────
  ollama_ok <- function() {
    tryCatch(
      httr::status_code(httr::GET(paste0(OLLAMA_HOST,"/api/tags"), httr::timeout(4))) == 200,
      error = function(e) FALSE
    )
  }
  
  # ─── Active backend ───────────────────────────────────────────────────────
  active_backend <- function() {
    pref <- isolate(input$ai_backend_pref) %||% "auto"
    if (pref == "ollama") return("ollama")
    if (pref == "groq")   return("groq")
    if (ollama_ok()) "ollama" else "groq"
  }
  
  active_groq_model <- function() {
    isolate(input$ai_groq_model) %||% "llama-3.1-8b-instant"
  }
  
  # ─── Data summary ─────────────────────────────────────────────────────────
  build_summary <- function() {
    df        <- tryCatch(consider_df(),         error = function(e) NULL)
    comp_perf <- tryCatch(company_performance(),  error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    total      <- nrow(df)
    integrated <- sum(df$source %in% c("SAP","Manual"), na.rm = TRUE)
    int_pct    <- round(integrated/total*100, 1)
    
    monthly <- tryCatch({
      df %>%
        dplyr::group_by(Month) %>%
        dplyr::summarise(total=dplyr::n(),
                         int=sum(source %in% c("SAP","Manual"), na.rm=TRUE), .groups="drop") %>%
        dplyr::arrange(dplyr::desc(Month)) %>%
        dplyr::mutate(pct=round(int/total*100,1))
    }, error=function(e) NULL)
    
    mom_txt <- if (!is.null(monthly) && nrow(monthly)>=2) {
      paste0(monthly$Month[1],": ",monthly$pct[1],"% | ",monthly$Month[2],": ",monthly$pct[2],"%")
    } else "Insufficient monthly data"
    
    comp_txt <- if (!is.null(comp_perf) && nrow(comp_perf)>0) {
      top3 <- comp_perf %>% dplyr::arrange(dplyr::desc(Integration_Pct)) %>% head(3)
      bot3 <- comp_perf %>% dplyr::arrange(Integration_Pct) %>% head(3)
      exc  <- sum(comp_perf$Integration_Pct>=85, na.rm=TRUE)
      und  <- sum(comp_perf$Integration_Pct<50, na.rm=TRUE)
      paste0("COMPANY PERFORMANCE (", nrow(comp_perf), " active):\n",
             "  Excellent (>=85%): ", exc, " | Under-performing (<50%): ", und, "\n",
             "  Top: ", paste(paste0(top3$`Client Name`," (",top3$Integration_Pct,"%)"), collapse=", "), "\n",
             "  Needs help: ", paste(paste0(bot3$`Client Name`," (",bot3$Integration_Pct,"%)"), collapse=", "))
    } else ""
    
    src_dist <- tryCatch({
      df %>% dplyr::count(source) %>% dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::mutate(txt=paste0(source,": ",n," (",round(n/sum(n)*100,1),"%)")) %>%
        dplyr::pull(txt) %>% paste(collapse=" | ")
    }, error=function(e) "")
    
    paste0("PROCUREGRAPH — ", format(Sys.Date(),"%B %Y"), "\n",
           "Total Valid Line Items: ", format(total,big.mark=","), "\n",
           "Integrated (SAP+Manual): ", format(integrated,big.mark=","), " (", int_pct, "%)\n\n",
           "MoM TREND: ", mom_txt, "\n\n",
           comp_txt, "\n\nSOURCE MIX: ", src_dist)
  }
  
  # ─── Ollama call ──────────────────────────────────────────────────────────
  call_ollama <- function(history, summary) {
    sys_msg <- paste0(
      "You are ProcureGraph AI, a concise procurement analytics expert. ",
      "Be direct. Use bullets. Max 200 words unless asked for more.",
      if (!is.null(summary)) paste0("\n\nDATA:\n", summary) else ""
    )
    msgs <- c(list(list(role="system",content=sys_msg)),
              lapply(history, function(m) list(role=m$role, content=m$content)))
    payload <- list(model=OLLAMA_MODEL, messages=msgs, stream=FALSE,
                    options=list(temperature=0.7, num_predict=400))
    tryCatch({
      r <- httr::POST(paste0(OLLAMA_HOST,"/api/chat"),
                      body=jsonlite::toJSON(payload,auto_unbox=TRUE),
                      httr::content_type_json(), httr::timeout(OLLAMA_TOUT))
      if (httr::status_code(r)==200) {
        p <- jsonlite::fromJSON(httr::content(r,as="text"), simplifyVector=FALSE)
        p$message$content %||% "No response from Ollama."
      } else paste0("Ollama error: HTTP ",httr::status_code(r))
    }, error=function(e) paste0("Ollama error: ",e$message))
  }
  
  # ─── Groq call ────────────────────────────────────────────────────────────
  call_groq <- function(history, summary, api_key, model) {
    api_key <- trimws(api_key %||% "")
    if (nchar(api_key) < 20) {
      return(paste0(
        "⚠️ No Groq API key entered.\n\n",
        "To use AI on cloud (ShinyApps.io):\n",
        "1. Go to console.groq.com and sign up (free)\n",
        "2. Create an API key under 'API Keys'\n",
        "3. Paste it in the AI Settings panel above\n\n",
        "Free quota: 14,400 requests/day — more than enough for daily use."
      ))
    }
    sys_msg <- paste0(
      "You are ProcureGraph AI, a concise procurement analytics expert at Moglix. ",
      "Help understand integration rates, vendor performance, bottlenecks. ",
      "Be direct. Use bullet points. Max 200 words unless asked for more. ",
      "Tie insights to revenue or efficiency impact.",
      if (!is.null(summary) && nchar(summary)>0) paste0("\n\nDATA:\n",summary) else ""
    )
    msgs <- c(list(list(role="system",content=sys_msg)),
              lapply(history, function(m) list(role=m$role, content=m$content)))
    payload <- list(model=model, messages=msgs, max_tokens=450, temperature=0.7)
    tryCatch({
      r <- httr::POST(GROQ_URL,
                      httr::add_headers(Authorization=paste0("Bearer ",api_key),
                                        `Content-Type`="application/json"),
                      body=jsonlite::toJSON(payload,auto_unbox=TRUE),
                      encode="raw", httr::timeout(30))
      s <- httr::status_code(r)
      raw <- httr::content(r, as="text", encoding="UTF-8")
      if (s==200) {
        p <- jsonlite::fromJSON(raw, simplifyVector=FALSE)
        p$choices[[1]]$message$content %||% "No response from Groq."
      } else if (s==401) {
        "Invalid Groq API key. Check at console.groq.com."
      } else if (s==429) {
        "Rate limit hit. Free tier: 14,400 req/day. Try in a moment."
      } else {
        p <- tryCatch(jsonlite::fromJSON(raw,simplifyVector=FALSE), error=function(e) NULL)
        paste0("Groq API error (", s, "): ", p$error$message %||% raw)
      }
    }, error=function(e) paste0("Network error: ",e$message))
  }
  
  # ─── Dispatcher ───────────────────────────────────────────────────────────
  call_ai <- function(history, summary) {
    backend <- active_backend()
    ai$backend_used <- backend
    if (backend=="ollama") {
      call_ollama(history, summary)
    } else {
      call_groq(history, summary,
                isolate(input$ai_groq_key) %||% "",
                active_groq_model())
    }
  }
  
  add_msg <- function(role, content, backend=NULL) {
    ai$chat_history <- c(ai$chat_history,
                         list(list(role=role, content=content,
                                   time=format(Sys.time(),"%H:%M"),
                                   backend=backend)))
  }
  
  scroll_bottom <- function() {
    shinyjs::runjs("setTimeout(function(){var e=document.getElementById('ai_chat_scroll');if(e)e.scrollTop=e.scrollHeight;},120);")
  }
  
  # ─── Settings panel ───────────────────────────────────────────────────────
  output$ai_settings_panel <- renderUI({
    b <- ai$backend_used %||% "—"
    badge_style <- if (b=="groq") "background:#6f42c1;color:white;"
    else if (b=="ollama") "background:#10b981;color:white;"
    else "background:#475569;color:white;"
    
    tags$div(class="mb-4 p-4 rounded-xl", style="background:#0e1d28;border:1px solid #263544;",
             tags$div(class="flex items-center gap-2 mb-3",
                      tags$span(class="material-symbols-outlined text-sm",style="color:#f59e0b;","settings"),
                      tags$span(class="text-xs font-bold uppercase tracking-widest",style="color:#64748b;","AI Settings")),
             tags$div(class="grid grid-cols-2 gap-3 mb-3",
                      tags$div(
                        tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-1",style="color:#64748b;","Backend"),
                        tags$select(id="ai_backend_pref",
                                    style="width:100%;background:#1c2b36;color:#cbd5e1;border:1px solid #374d60;border-radius:6px;padding:6px 8px;font-size:11px;",
                                    onchange="Shiny.setInputValue('ai_backend_pref',this.value,{priority:'event'});",
                                    tags$option(value="auto",  "Auto (Ollama → Groq)"),
                                    tags$option(value="groq",  "Groq API (Cloud ☁)"),
                                    tags$option(value="ollama","Ollama (Local 🖥)"))
                      ),
                      tags$div(
                        tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-1",style="color:#64748b;","Groq Model"),
                        tags$select(id="ai_groq_model",
                                    style="width:100%;background:#1c2b36;color:#cbd5e1;border:1px solid #374d60;border-radius:6px;padding:6px 8px;font-size:11px;",
                                    onchange="Shiny.setInputValue('ai_groq_model',this.value,{priority:'event'});",
                                    tags$option(value="llama-3.1-8b-instant",  "Llama 3.1 8B (fastest ⚡)"),
                                    tags$option(value="llama-3.3-70b-versatile","Llama 3.3 70B (best quality)"),
                                    tags$option(value="gemma2-9b-it",           "Gemma 2 9B"),
                                    tags$option(value="mixtral-8x7b-32768",     "Mixtral 8x7B (long context)"))
                      )
             ),
             tags$div(
               tags$p(class="text-[10px] font-bold uppercase tracking-widest mb-1",
                      style="color:#64748b;",
                      HTML('Groq API Key &nbsp;<a href="https://console.groq.com" target="_blank" style="color:#f59e0b;text-decoration:underline;">→ Get free key</a>')),
               tags$input(id="ai_groq_key", type="password",
                          placeholder="gsk_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                          style="width:100%;background:#1c2b36;color:#cbd5e1;border:1px solid #374d60;border-radius:6px;padding:8px 10px;font-size:11px;",
                          onblur="Shiny.setInputValue('ai_groq_key',this.value,{priority:'event'});",
                          onchange="Shiny.setInputValue('ai_groq_key',this.value,{priority:'event'});")
             ),
             tags$div(class="flex items-center justify-between mt-3",
                      tags$p(class="text-[10px]",style="color:#475569;",
                             "Free: 14,400 req/day · Key never stored — only sent to Groq"),
                      tags$span(class="text-[10px] font-bold px-2 py-0.5 rounded-full",
                                style=badge_style,
                                if(b=="groq")"☁ Groq" else if(b=="ollama")"🖥 Ollama" else "— not used yet"))
    )
  })
  
  # ─── Chat messages ────────────────────────────────────────────────────────
  output$ai_chat_messages <- renderUI({
    msgs <- ai$chat_history
    if (length(msgs)==0) {
      qp <- c("What are the top 3 action items?",
              "Which company needs immediate attention?",
              "Explain the month-on-month trend",
              "What is causing low integration rates?",
              "Give me an executive summary")
      return(tags$div(class="flex flex-col items-center justify-center py-10 text-center",
                      tags$div(style="font-size:40px;margin-bottom:12px;","🤖"),
                      tags$p(class="text-sm font-bold mb-1",style="color:#0c4a6e;","ProcureGraph AI"),
                      tags$p(class="text-xs mb-4",style="color:#94a3b8;",
                             "Click Analyze Data to load context, then ask questions."),
                      tags$div(class="flex flex-wrap justify-center gap-2",
                               lapply(qp, function(p)
                                 tags$button(
                                   onclick=paste0("Shiny.setInputValue('ai_quick_prompt','",p,"',{priority:'event'});"),
                                   class="px-3 py-1.5 rounded-full text-xs font-medium",
                                   style="background:#f0f9ff;color:#0c4a6e;border:1px solid #bfdbfe;cursor:pointer;",p)))))
    }
    
    bubbles <- lapply(seq_along(msgs), function(i) {
      m   <- msgs[[i]]
      fmt <- gsub("\\*\\*(.+?)\\*\\*","<strong>\\1</strong>",m$content)
      fmt <- gsub("\n","<br>",fmt)
      
      if (m$role=="user") {
        tags$div(class="flex justify-end mb-4",
                 tags$div(
                   tags$div(class="text-[10px] text-right mb-1",style="color:#94a3b8;",
                            paste0("You • ",m$time%||%"")),
                   tags$div(class="inline-block px-4 py-3 rounded-2xl rounded-tr-sm text-sm",
                            style="background:#006495;color:white;max-width:78%;line-height:1.6;",
                            m$content)))
      } else {
        b <- m$backend %||% NULL
        badge <- if (!is.null(b))
          tags$span(class="text-[9px] px-1.5 py-0.5 rounded-full ml-1",
                    style=if(b=="groq")"background:#6f42c1;color:white;" else "background:#10b981;color:white;",
                    if(b=="groq")"☁ Groq" else "🖥 Ollama") else NULL
        tags$div(class="flex justify-start mb-4",
                 tags$div(class="flex items-start gap-2 max-w-[85%]",
                          tags$div(style="font-size:22px;margin-top:18px;flex-shrink:0;","🤖"),
                          tags$div(
                            tags$div(class="text-[10px] mb-1 flex items-center",style="color:#94a3b8;",
                                     paste0("AI • ",m$time%||%""), badge),
                            tags$div(class="px-4 py-3 rounded-2xl rounded-tl-sm text-sm",
                                     style="background:#f0f9ff;color:#0c4a6e;border:1px solid #bfdbfe;line-height:1.75;",
                                     HTML(fmt)))))
      }
    })
    
    if (isTRUE(ai$loading))
      bubbles <- c(bubbles, list(
        tags$div(class="flex justify-start mb-4",
                 tags$div(class="flex items-center gap-2",
                          tags$div(style="font-size:22px;","🤖"),
                          tags$div(class="px-4 py-3 rounded-2xl rounded-tl-sm",
                                   style="background:#f0f9ff;border:1px solid #bfdbfe;",
                                   HTML('<span style="color:#94a3b8;font-size:13px;">
                                     <span style="animation:pulse 1.2s ease-in-out infinite;display:inline-block;">●</span>
                                     <span style="animation:pulse 1.2s ease-in-out infinite;animation-delay:.3s;margin:0 3px;display:inline-block;">●</span>
                                     <span style="animation:pulse 1.2s ease-in-out infinite;animation-delay:.6s;display:inline-block;">●</span>
                                     &nbsp;Thinking...</span>
                                   <style>@keyframes pulse{0%,100%{opacity:.3}50%{opacity:1}}</style>'))))))
    
    do.call(tagList, bubbles)
  })
  
  # ─── Main panel UI ────────────────────────────────────────────────────────
  output$ai_insights_panel <- renderUI({
    has <- length(ai$chat_history) > 0
    tags$div(class="bg-white rounded-2xl shadow-sm mb-6",style="border:1px solid #f1f5f9;overflow:hidden;",
             tags$div(class="flex items-center justify-between px-6 py-4",style="border-bottom:1px solid #f1f5f9;",
                      tags$div(class="flex items-center gap-2",
                               tags$span(class="material-symbols-outlined",style="color:#f59e0b;","smart_toy"),
                               tags$h3(class="text-sm font-extrabold uppercase tracking-widest","ProcureGraph AI Chat")),
                      tags$div(class="flex items-center gap-2",
                               if(isTRUE(ai$loading))
                                 tags$span(class="text-[10px] font-bold px-2 py-1 rounded-full",
                                           style="background:#fef3c7;color:#92400e;","⟳ Thinking...")
                               else if(!is.null(ai$timestamp))
                                 tags$span(class="text-[10px]",style="color:#94a3b8;",
                                           paste0("Updated ",format(ai$timestamp,"%H:%M")))
                               else NULL,
                               if(has) actionButton("ai_clear_chat",
                                                    HTML('<span class="material-symbols-outlined" style="font-size:16px;vertical-align:middle;">delete_sweep</span>'),
                                                    style="background:#f1f5f9;border:none;color:#64748b;padding:5px 8px;border-radius:8px;cursor:pointer;") else NULL)),
             tags$div(class="px-6 pt-4", uiOutput("ai_settings_panel")),
             if (!is.null(ai$error))
               tags$div(class="mx-6 mt-3 p-3 rounded-xl flex items-start gap-2",
                        style="background:#fee2e2;border:1px solid #fecaca;",
                        tags$span(class="material-symbols-outlined text-sm flex-shrink-0",style="color:#dc2626;","error"),
                        tags$p(class="text-xs mb-0",style="color:#991b1b;",ai$error)) else NULL,
             tags$div(id="ai_chat_scroll",class="px-6 py-4",
                      style="height:420px;overflow-y:auto;scroll-behavior:smooth;",
                      uiOutput("ai_chat_messages")),
             tags$div(class="px-6 pb-5 pt-3",style="border-top:1px solid #f1f5f9;",
                      tags$div(class="flex flex-wrap items-center gap-2 mb-3",
                               actionButton("ai_generate_insights",
                                            HTML('<span class="material-symbols-outlined text-sm" style="vertical-align:middle;">analytics</span>&nbsp;Analyze Data'),
                                            style="background:#f59e0b;color:white;border:none;font-weight:700;padding:8px 14px;border-radius:8px;cursor:pointer;font-size:11px;"),
                               tags$button(onclick="Shiny.setInputValue('ai_quick_prompt','What are the top 3 action items?',{priority:'event'});",
                                           class="text-xs px-3 py-1.5 rounded-full",
                                           style="background:#f1f5f9;color:#475569;border:1px solid #e2e8f0;cursor:pointer;","Top actions"),
                               tags$button(onclick="Shiny.setInputValue('ai_quick_prompt','Which company needs immediate attention and why?',{priority:'event'});",
                                           class="text-xs px-3 py-1.5 rounded-full",
                                           style="background:#f1f5f9;color:#475569;border:1px solid #e2e8f0;cursor:pointer;","At-risk company"),
                               tags$button(onclick="Shiny.setInputValue('ai_quick_prompt','Give me a one-paragraph executive summary I can share.',{priority:'event'});",
                                           class="text-xs px-3 py-1.5 rounded-full",
                                           style="background:#f1f5f9;color:#475569;border:1px solid #e2e8f0;cursor:pointer;","Exec summary")),
                      tags$div(class="flex gap-2",
                               tags$input(id="ai_input_raw",type="text",
                                          placeholder="Ask anything about your procurement data...",
                                          style="flex:1;padding:11px 16px;border:1.5px solid #e2e8f0;border-radius:12px;font-size:13px;outline:none;background:#fafafa;color:#0e1d28;",
                                          onkeydown="if(event.key==='Enter'&&this.value.trim()){Shiny.setInputValue('ai_chat_input',this.value,{priority:'event'});Shiny.setInputValue('ai_send_btn',Math.random(),{priority:'event'});this.value='';}",
                                          onfocus="this.style.borderColor='#006495';",
                                          onblur="this.style.borderColor='#e2e8f0';"),
                               tags$button(onclick="var v=document.getElementById('ai_input_raw').value;if(v.trim()){Shiny.setInputValue('ai_chat_input',v,{priority:'event'});Shiny.setInputValue('ai_send_btn',Math.random(),{priority:'event'});document.getElementById('ai_input_raw').value='';}",
                                           class="flex-shrink-0 px-4 py-2 rounded-xl font-bold text-sm",
                                           style="background:#006495;color:white;border:none;cursor:pointer;",
                                           HTML('<span class="material-symbols-outlined text-sm">send</span>')))))
  })
  
  observe({ ai$chat_history; ai$loading; scroll_bottom() })
  
  observeEvent(input$ai_generate_insights, {
    ai$error <- NULL
    s <- tryCatch(build_summary(), error=function(e) NULL)
    if (is.null(s)) { ai$error <- "No data. Load data first."; return() }
    ai$data_summary <- s
    add_msg("user","Analyze my procurement data and give 3-4 key insights with specific recommendations.")
    ai$loading <- TRUE
    resp <- call_ai(ai$chat_history, ai$data_summary)
    ai$loading <- FALSE
    add_msg("assistant", resp, ai$backend_used)
    ai$timestamp <- Sys.time()
  })
  
  handle_send <- function(msg) {
    msg <- trimws(msg %||% "")
    if (!nchar(msg)) return()
    ai$error <- NULL
    if (is.null(ai$data_summary))
      ai$data_summary <- tryCatch(build_summary(), error=function(e) NULL)
    add_msg("user", msg)
    ai$loading <- TRUE
    resp <- call_ai(ai$chat_history, ai$data_summary)
    ai$loading <- FALSE
    add_msg("assistant", resp, ai$backend_used)
    ai$timestamp <- Sys.time()
  }
  
  observeEvent(input$ai_send_btn,     { handle_send(input$ai_chat_input %||% "") })
  observeEvent(input$ai_quick_prompt, { req(input$ai_quick_prompt); handle_send(input$ai_quick_prompt) })
  observeEvent(input$ai_clear_chat,   {
    ai$chat_history <- list(); ai$error <- NULL
    ai$data_summary <- NULL;  ai$timestamp <- NULL
  })
  
  list(
    state    = ai,
    insights = reactive({
      msgs <- Filter(function(m) m$role=="assistant", ai$chat_history)
      if (length(msgs)>0) msgs[[length(msgs)]]$content else NULL
    })
  )
}