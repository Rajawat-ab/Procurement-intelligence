################################################################################
# MODULE: AI Insights (Gemma/Ollama Integration)
# Purpose: Generate business-focused insights using local Gemma model
# Status: NEW CODE - Non-breaking
# Dependencies: curl (system), jsonlite (R package)
################################################################################

setup_ai_insights <- function(input, output, session, 
                             consider_df, company_integration, 
                             company_performance) {
  
  ai_state <- reactiveValues(
    loading = FALSE,
    insights = NULL,
    error = NULL,
    timestamp = NULL
  )
  
  # ─── Ollama Configuration ──────────────────────────────────────────────
  OLLAMA_HOST <- "http://localhost:11434"
  OLLAMA_MODEL <- "gemma2:2b"
  OLLAMA_TIMEOUT <- 60
  
  # ─── Check Ollama availability ──────────────────────────────────────────
  check_ollama_health <- function() {
    tryCatch({
      response <- httr::GET(
        paste0(OLLAMA_HOST, "/api/tags"),
        httr::timeout(5)
      )
      httr::status_code(response) == 200
    }, error = function(e) FALSE)
  }
  
  # ─── Build insight summary from data ───────────────────────────────────
  build_insight_summary <- function(filtered_data, company_perf) {
    req(filtered_data, company_perf)
    
    df <- filtered_data()
    comp_perf <- company_perf()
    
    if (nrow(df) == 0 || nrow(comp_perf) == 0) return(NULL)
    
    # Calculate key metrics
    total_lines <- nrow(df)
    integrated_lines <- sum(df$source %in% c("SAP", "Manual"), na.rm = TRUE)
    integration_pct <- if (total_lines > 0) round(integrated_lines / total_lines * 100, 1) else 0
    
    top_company <- comp_perf %>%
      arrange(desc(Integrated_Volume)) %>%
      slice(1)
    
    underperforming <- comp_perf %>%
      filter(Integration_Pct < 50) %>%
      nrow()
    
    excellent_performers <- comp_perf %>%
      filter(Integration_Pct >= 85) %>%
      nrow()
    
    # MoM trend
    monthly_stats <- df %>%
      group_by(Month) %>%
      summarise(SAP = sum(source == "SAP", na.rm = TRUE),
                Manual = sum(source == "Manual", na.rm = TRUE),
                Total = n(), .groups = "drop") %>%
      arrange(desc(Month)) %>%
      mutate(Int_Pct = if_else(Total > 0, round((SAP + Manual) / Total * 100, 1), 0))
    
    mom_trend <- if (nrow(monthly_stats) >= 2) {
      curr <- monthly_stats$Int_Pct[1]
      prev <- monthly_stats$Int_Pct[2]
      if (!is.na(curr) && !is.na(prev)) {
        direction <- if (curr > prev) "improving" else "declining"
        change <- abs(curr - prev)
        paste0("MoM trend is ", direction, " by ", change, "pp")
      } else ""
    } else ""
    
    # Build summary
    paste0(
      "PROCUREMENT ANALYTICS SUMMARY\n",
      "=====================================\n",
      "Period: ", format(Sys.Date(), "%B %Y"), "\n",
      "Total Valid Line Items: ", format(total_lines, big.mark = ","), "\n",
      "Integrated Line Items: ", format(integrated_lines, big.mark = ","), " (",
      integration_pct, "%)\n",
      "Active Companies: ", nrow(comp_perf), "\n\n",
      
      "TOP PERFORMER:\n",
      "- Company: ", top_company$`Client Name`, "\n",
      "- Integration Rate: ", top_company$Integration_Pct, "%\n",
      "- Integrated Volume: ", format(top_company$Integrated_Volume, big.mark = ","), " items\n\n",
      
      "PERFORMANCE DISTRIBUTION:\n",
      "- Excellent (≥85%): ", excellent_performers, " companies\n",
      "- Underperforming (<50%): ", underperforming, " companies\n\n",
      
      "TRENDS:\n",
      "- ", mom_trend, "\n",
      "- Status Mix: Live (", sum(df$`Final Status` == "Live", na.rm = TRUE), 
      "), CBB (", sum(df$`Final Status` == "Live - CBB", na.rm = TRUE),
      "), Non-integrated (", sum(df$`Final Status` == "No Integration", na.rm = TRUE), ")\n"
    )
  }
  
  # ─── Call Ollama API with streaming ────────────────────────────────────
  call_gemma_insights <- function(summary_text) {
    req(summary_text)
    
    prompt <- paste0(
      "You are a procurement analytics expert. Based on this data summary, ",
      "provide 3-4 concise, actionable business insights. Be specific and focus on ",
      "integration strategy and revenue impact. Keep each insight to 1-2 sentences. ",
      "Use a professional but conversational tone.\n\n",
      "DATA SUMMARY:\n", summary_text, "\n\n",
      "INSIGHTS:"
    )
    
    payload <- list(
      model = OLLAMA_MODEL,
      prompt = prompt,
      stream = FALSE,
      temperature = 0.7,
      top_p = 0.9
    )
    
    tryCatch({
      response <- httr::POST(
        paste0(OLLAMA_HOST, "/api/generate"),
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::timeout(OLLAMA_TIMEOUT)
      )
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, as = "text")
        parsed <- jsonlite::fromJSON(content)
        parsed$response
      } else {
        paste0("Ollama API Error: ", httr::status_code(response))
      }
    }, error = function(e) {
      paste0("Connection Error: ", e$message)
    })
  }
  
  # ─── UI: AI Insights Panel ─────────────────────────────────────────────
  output$ai_insights_panel <- renderUI({
    tags$div(class = "bg-white rounded-2xl shadow-sm p-6 mb-6", style = "border:1px solid #f1f5f9;",
             tags$div(class = "flex items-center justify-between mb-4",
                      tags$div(class = "flex items-center gap-2",
                               tags$span(class = "material-symbols-outlined", style = "color:#f59e0b;", "lightbulb"),
                               tags$h3(class = "text-sm font-extrabold uppercase tracking-widest", "AI Insights (Gemma)")),
                      tags$div(
                        if (ai_state$loading) {
                          tags$span(class = "text-[10px] font-bold", style = "color:#64748b;",
                                   HTML('<span style="animation:spin 1s linear infinite;display:inline-block;">⟳</span>&nbsp;Generating...'))
                        } else if (!is.null(ai_state$timestamp)) {
                          tags$span(class = "text-[10px]", style = "color:#94a3b8;",
                                   paste0("Updated: ", format(ai_state$timestamp, "%H:%M")))
                        } else {
                          tags$span(class = "text-[10px]", style = "color:#94a3b8;", "Not yet generated")
                        }
                      )),
             
             tags$div(class = "p-4 rounded-xl mb-4", style = "background:#f0f9ff;border:1px solid #bfdbfe;",
                      tags$div(class = "text-xs mb-2", style = "color:#0c4a6e;",
                               HTML("ℹ️ <strong>How it works:</strong> Analyzes your procurement data and generates business-focused insights using a local Gemma 2B model (no data leaves your server).")),
                      tags$p(class = "text-[10px] mb-0", style = "color:#0c4a6e;",
                             HTML("⚡ <strong>Note:</strong> Ollama must be running: <code>ollama serve</code> in terminal"))),
             
             # Action buttons
             tags$div(class = "flex gap-2 mb-4",
                      actionButton("ai_generate_insights", 
                                  HTML('<span class="material-symbols-outlined" style="vertical-align:middle;">spark</span>&nbsp;Generate Insights'),
                                  style = "background:#f59e0b;color:white;border:none;font-weight:700;padding:10px 20px;border-radius:8px;cursor:pointer;"),
                      actionButton("ai_copy_insights",
                                  HTML('<span class="material-symbols-outlined" style="vertical-align:middle;">content_copy</span>&nbsp;Copy'),
                                  style = "background:#e2e8f0;color:#64748b;border:none;font-weight:700;padding:10px 14px;border-radius:8px;cursor:pointer;")),
             
             # Insights display
             tags$div(id = "ai_insights_output",
                      if (!is.null(ai_state$error)) {
                        tags$div(class = "p-4 rounded-xl", style = "background:#fee2e2;border:1px solid #fecaca;",
                                 tags$p(class = "text-xs font-bold", style = "color:#dc2626;margin:0;",
                                        HTML('<span class="material-symbols-outlined" style="vertical-align:middle;font-size:14px;">error</span>&nbsp;Error')),
                                 tags$p(class = "text-xs mt-2 mb-0", style = "color:#7f1d1d;",
                                        ai_state$error))
                      } else if (!is.null(ai_state$insights)) {
                        tags$div(class = "p-4 rounded-xl", style = "background:#f0fdf4;border:1px solid #bbf7d0;",
                                 tags$div(class = "prose prose-sm max-w-none",
                                          HTML(
                                            paste0(
                                              '<div style="color:#15803d;font-size:13px;line-height:1.8;',
                                              'font-family:Manrope,sans-serif;">',
                                              gsub("\n", "<br>", ai_state$insights),
                                              '</div>'
                                            )
                                          )))
                      } else {
                        tags$div(class = "p-4 rounded-xl", style = "background:#f8fafc;text-align:center;",
                                 tags$p(class = "text-sm", style = "color:#94a3b8;margin:0;",
                                        "Click 'Generate Insights' to analyze your procurement data with AI"))
                      })
    )
  })
  
  # ─── Generate insights on button click ──────────────────────────────────
  observeEvent(input$ai_generate_insights, {
    ai_state$loading <- TRUE
    ai_state$error <- NULL
    
    summary <- build_insight_summary(consider_df, company_performance)
    
    if (is.null(summary)) {
      ai_state$error <- "No data available to analyze"
      ai_state$loading <- FALSE
      return()
    }
    
    # Check Ollama
    if (!check_ollama_health()) {
      ai_state$error <- paste0(
        "Ollama not running. Start it with: ollama serve (then run 'ollama pull gemma2:2b')"
      )
      ai_state$loading <- FALSE
      return()
    }
    
    # Call Gemma
    insights <- call_gemma_insights(summary)
    
    ai_state$insights <- insights
    ai_state$timestamp <- Sys.time()
    ai_state$loading <- FALSE
  })
  
  # ─── Copy insights to clipboard (JS) ───────────────────────────────────
  observeEvent(input$ai_copy_insights, {
    req(ai_state$insights)
    shinyjs::runjs(paste0(
      "navigator.clipboard.writeText('", 
      gsub("'", "\\'", gsub('\n', '\\n', ai_state$insights)),
      "').then(() => {",
      "  Shiny.setInputValue('show_copy_toast', true);",
      "});"
    ))
  })
  
  # ─── Toast notification for copy ───────────────────────────────────────
  observe({
    if (isTruthy(input$show_copy_toast)) {
      shinyjs::runjs("
        const toast = document.createElement('div');
        toast.innerHTML = '✓ Insights copied to clipboard';
        toast.style.cssText = `
          position: fixed; bottom: 20px; right: 20px;
          background: #10b981; color: white; padding: 12px 16px;
          border-radius: 8px; font-size: 12px; font-weight: 600;
          z-index: 9999; animation: slideUp 0.3s ease;
        `;
        document.body.appendChild(toast);
        setTimeout(() => toast.remove(), 2000);
      ")
    }
  })
  
  # Return state for integration
  list(
    state = ai_state,
    insights = reactive(ai_state$insights)
  )
}
