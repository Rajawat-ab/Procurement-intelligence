################################################################################
# MODULE: Data Source Manager (MongoDB ↔ Excel Fallback)
# Purpose: Allows app to run on cloud (ShinyApps.io) without MongoDB dependency.
#          If user selects "Excel Upload" mode, data is read from uploaded files
#          instead of fetching from MongoDB.
#
# Integration into main app:
#   1. source("mod_data_source.R")
#   2. Call setup_data_source_ui() in the sidebar
#   3. Replace get_mongo_data() + get_grn_data() calls with
#      data_source$get_item() and data_source$get_grn() reactives
#
# Template column specs:
#   Item  : _id, buyerId, status, creationDate_epoch, source, stage,
#            customerPoNo, customerSANumber, amount
#   GRN   : _id, buyerId, creationDate_epoch, customerGrnNo, poId, itemId
#
# Note: creationDate columns must be Unix timestamps in MILLISECONDS
#       (e.g. 1704067200000 = 2024-01-01 00:00:00 UTC)
#       OR valid date strings "YYYY-MM-DD" — both formats are handled.
################################################################################

# ─── Sidebar UI elements for data source selection ─────────────────────────
# Call this from within the sidebar section in ui / renderUI
data_source_sidebar_ui <- function() {
  tags$div(
    class = "sidebar-section space-y-3",
    tags$p(
      class = "text-[10px] font-bold uppercase tracking-widest mt-4",
      style = "color:#64748b;",
      "Data Source"
    ),
    tags$div(
      class = "flex rounded-lg overflow-hidden",
      style = "border:1px solid #374d60;",
      tags$button(
        id      = "ds_mode_mongo",
        onclick = "Shiny.setInputValue('data_source_mode','mongodb',{priority:'event'});",
        style   = "flex:1;padding:7px;font-size:10px;font-weight:700;cursor:pointer;border:none;background:#006495;color:white;letter-spacing:.05em;",
        "MongoDB"
      ),
      tags$button(
        id      = "ds_mode_excel",
        onclick = "Shiny.setInputValue('data_source_mode','excel',{priority:'event'});",
        style   = "flex:1;padding:7px;font-size:10px;font-weight:700;cursor:pointer;border:none;background:#263544;color:#94a3b8;letter-spacing:.05em;",
        "Excel"
      )
    ),
    # Item Excel upload (hidden in MongoDB mode via JS below)
    tags$div(
      id = "ds_item_upload_area",
      tags$p(
        class = "text-[10px] font-bold uppercase tracking-widest mb-1",
        style = "color:#10b981;",
        "Item Data (.xlsx)"
      ),
      fileInput(
        "ds_item_file", NULL,
        accept = c(".xlsx", ".xls"),
        width  = "100%"
      ),
      downloadButton(
        "ds_item_template",
        label = HTML('<span style="font-size:10px;">⬇ Download Template</span>'),
        style = paste0("background:#10b981;color:white;border:none;cursor:pointer;",
                       "width:100%;padding:6px;border-radius:6px;font-size:10px;",
                       "font-weight:700;text-align:center;display:block;")
      )
    ),
    # GRN Excel upload
    tags$div(
      id = "ds_grn_upload_area",
      style = "margin-top:8px;",
      tags$p(
        class = "text-[10px] font-bold uppercase tracking-widest mb-1",
        style = "color:#f59e0b;",
        "GRN Data (.xlsx)"
      ),
      fileInput(
        "ds_grn_file", NULL,
        accept = c(".xlsx", ".xls"),
        width  = "100%"
      ),
      downloadButton(
        "ds_grn_template",
        label = HTML('<span style="font-size:10px;">⬇ Download Template</span>'),
        style = paste0("background:#f59e0b;color:white;border:none;cursor:pointer;",
                       "width:100%;padding:6px;border-radius:6px;font-size:10px;",
                       "font-weight:700;text-align:center;display:block;")
      )
    ),
    # JS to toggle panel visibility + button highlight
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name !== 'data_source_mode') return;
        var isExcel = (e.value === 'excel');
        $('#ds_item_upload_area, #ds_grn_upload_area').toggle(isExcel);
        $('#ds_mode_mongo').css({background: isExcel ? '#263544' : '#006495',
                                 color:      isExcel ? '#94a3b8' : 'white'});
        $('#ds_mode_excel').css({background: isExcel ? '#10b981' : '#263544',
                                 color:      isExcel ? 'white'   : '#94a3b8'});
      });
      // Hide Excel panels on init
      $(function(){ $('#ds_item_upload_area, #ds_grn_upload_area').hide(); });
    "))
  )
}

# ─── Server module ────────────────────────────────────────────────────────────
setup_data_source <- function(input, output, session,
                               get_mongo_item_fn,   # function(start, end) → raw df
                               get_mongo_grn_fn) {  # function(start, end) → raw df

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # ── Helpers: parse a creationDate column that is either epoch(ms) or string ─
  parse_creation_date <- function(x) {
    x <- as.character(x)
    # numeric → epoch ms
    if (all(grepl("^\\d{10,13}$", x[!is.na(x)]), na.rm = TRUE)) {
      as.numeric(x)   # return as-is; downstream code converts /1000
    } else {
      # date string → convert to epoch ms so downstream code works unchanged
      dt <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
      as.numeric(dt) * 1000
    }
  }

  # ── Read item Excel ───────────────────────────────────────────────────────
  read_item_excel <- function(path) {
    df <- tryCatch(
      readxl::read_excel(path, sheet = 1),
      error = function(e) {
        showNotification(paste("Item Excel read error:", e$message), type = "error")
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)

    # Normalise column names (allow common variations)
    cn <- colnames(df)
    rename_if <- function(df, to, from_candidates) {
      m <- intersect(from_candidates, colnames(df))
      if (length(m) > 0 && !to %in% colnames(df))
        df <- df %>% dplyr::rename(!!to := !!m[1])
      df
    }

    df <- df %>%
      rename_if("_id",             c("_id","id","ID","document_id","docId")) %>%
      rename_if("buyerId",         c("buyerId","buyer_id","BuyerID","buyer.id","buyerid","PlantId","plantId")) %>%
      rename_if("status",          c("status","Status","ORDER_STATUS")) %>%
      rename_if("creationDate",    c("creationDate","creation_date","CreationDate",
                                     "creationdate","creationDate_epoch","date")) %>%
      rename_if("source",          c("source","Source","SOURCE")) %>%
      rename_if("stage",           c("stage","Stage","STAGE")) %>%
      rename_if("amount",          c("amount","Amount","AMOUNT","itemAmount","ItemAmount",
                                     "poAmount","PO Amount","PO_AMOUNT","value","Value","VALUE")) %>%
      rename_if("histPoCustomerPoNo",     c("histPoCustomerPoNo","customerPoNo","customer_po_no",
                                            "CustomerPoNo","CUSTOMER_PO_NO","history.po.customerPoNo")) %>%
      rename_if("histPoCustomerSANumber", c("histPoCustomerSANumber","customerSANumber","customer_sa_number",
                                            "CustomerSANumber","CUSTOMER_SA_NUMBER","history.po.customerSANumber"))

    # Add missing optional columns with NA
    for (col in c("_id","status","amount","histPoCustomerPoNo","histPoCustomerSANumber"))
      if (!col %in% colnames(df)) df[[col]] <- NA_character_

    # Parse creation date → keep as numeric epoch ms
    if ("creationDate" %in% colnames(df))
      df$creationDate <- parse_creation_date(df$creationDate)

    df %>%
      dplyr::mutate(across(everything(), as.character)) %>%
      dplyr::mutate(creationDate = as.numeric(creationDate))
  }

  # ── Read GRN Excel ────────────────────────────────────────────────────────
  read_grn_excel <- function(path) {
    df <- tryCatch(
      readxl::read_excel(path, sheet = 1),
      error = function(e) {
        showNotification(paste("GRN Excel read error:", e$message), type = "error")
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)

    rename_if <- function(df, to, from_candidates) {
      m <- intersect(from_candidates, colnames(df))
      if (length(m) > 0 && !to %in% colnames(df))
        df <- df %>% dplyr::rename(!!to := !!m[1])
      df
    }

    df <- df %>%
      rename_if("_id",           c("_id","id","ID","document_id")) %>%
      rename_if("buyerId",       c("buyerId","buyer_id","BuyerID","buyer.id","plantId","PlantId")) %>%
      rename_if("creationDate",  c("creationDate","creation_date","CreationDate",
                                   "creationdate","creationDate_epoch","date")) %>%
      rename_if("customerGrnNo", c("customerGrnNo","customer_grn_no","CustomerGrnNo","GRN_Number","grnNo")) %>%
      rename_if("poId",          c("poId","po_id","PoId","PO_ID","po_number")) %>%
      rename_if("itemId",        c("itemId","item_id","ItemId","ITEM_ID"))

    for (col in c("_id","customerGrnNo","poId","itemId"))
      if (!col %in% colnames(df)) df[[col]] <- NA_character_

    if ("creationDate" %in% colnames(df))
      df$creationDate <- parse_creation_date(df$creationDate)

    df %>%
      dplyr::mutate(across(everything(), as.character)) %>%
      dplyr::mutate(creationDate = as.numeric(creationDate))
  }

  # ── Reactives exposed to main app ─────────────────────────────────────────
  # Returns raw item data (same shape as get_mongo_data output)
  item_data_raw <- reactive({
    mode <- input$data_source_mode %||% "mongodb"

    if (mode == "excel") {
      req(input$ds_item_file)
      read_item_excel(input$ds_item_file$datapath)
    } else {
      NULL  # signal: use MongoDB
    }
  })

  grn_data_raw <- reactive({
    mode <- input$data_source_mode %||% "mongodb"

    if (mode == "excel") {
      if (is.null(input$ds_grn_file)) return(data.frame())
      read_grn_excel(input$ds_grn_file$datapath)
    } else {
      NULL  # signal: use MongoDB
    }
  })

  # Status banner shown at top of analytics panel (or dashboard)
  output$data_source_status <- renderUI({
    mode <- input$data_source_mode %||% "mongodb"

    if (mode == "excel") {
      item_ok <- !is.null(input$ds_item_file)
      grn_ok  <- !is.null(input$ds_grn_file)
      tags$div(
        class = "flex items-center gap-3 p-3 rounded-xl mb-4",
        style = "background:#f0fdf4;border:1px solid #bbf7d0;",
        tags$span(class = "material-symbols-outlined text-sm", style = "color:#16a34a;", "upload_file"),
        tags$div(
          tags$span(class = "text-xs font-bold", style = "color:#15803d;", "Excel Mode — "),
          tags$span(class = "text-xs", style = "color:#15803d;",
                    paste0("Item data: ", if (item_ok) "✓ loaded" else "⚠ not uploaded",
                           " | GRN data: ", if (grn_ok) "✓ loaded" else "⚠ not uploaded (optional)"))
        )
      )
    } else {
      tags$div(
        class = "flex items-center gap-3 p-3 rounded-xl mb-4",
        style = "background:#f0f9ff;border:1px solid #bfdbfe;",
        tags$span(class = "material-symbols-outlined text-sm", style = "color:#006495;", "database"),
        tags$span(class = "text-xs font-bold", style = "color:#006495;",
                  "MongoDB Mode — data fetched directly from procurement database")
      )
    }
  })

  # ── Template download handlers ─────────────────────────────────────────────
  output$ds_item_template <- downloadHandler(
    filename = function() "procuregraph_item_template.xlsx",
    content  = function(file) {
      # Generate sample rows — one for each common source type
      template <- data.frame(
        `_id`                   = c("doc001","doc002","doc003","doc004"),
        `buyerId`               = c("12345","67890","11111","22222"),
        `status`                = c("Closed","Closed","Cancelled","Closed"),
        `creationDate_epoch_ms` = c(
          as.numeric(as.POSIXct("2024-11-01", tz="UTC")) * 1000,
          as.numeric(as.POSIXct("2024-11-15", tz="UTC")) * 1000,
          as.numeric(as.POSIXct("2024-12-01", tz="UTC")) * 1000,
          as.numeric(as.POSIXct("2024-12-20", tz="UTC")) * 1000
        ),
        `source`                = c("SAP","Manual","EOC","SAP"),
        `stage`                 = c("Closed","Closed","Closed","Open"),
        `amount`                = c(1250000, 950000, 2100000, 1750000),
        `customerPoNo`          = c("PO-1001","PO-1002","PO-1003","PO-1004"),
        `customerSANumber`      = c("4500001111","4500002222","7100003333","4500004444"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      instructions <- data.frame(
        Column               = c(
          "_id", "buyerId", "status", "creationDate_epoch_ms",
          "source", "stage", "amount", "customerPoNo", "customerSANumber"
        ),
        Description          = c(
          "Unique document ID (any string, can be row number)",
          "Plant/buyer ID — must match plantId in PID-CID sheet of mapping Excel",
          "Order status (Closed / Cancelled / Open / etc.)",
          "Creation date as Unix epoch in MILLISECONDS, OR use YYYY-MM-DD string",
          "Source system: SAP | Manual | EOC  (only these 3 are processed)",
          "Processing stage: Closed or Cancelled → will be Removed",
          "PO/item value in rupees. Dashboard value analysis converts this to crores.",
          "Customer PO number (can be blank)",
          "SA/Contract number — first 2 chars used for Havells/SC rules (can be blank)"
        ),
        Example              = c(
          "doc001", "12345", "Closed",
          "1704067200000", "SAP", "Closed", "1250000", "PO-1001", "4500001111"
        ),
        stringsAsFactors = FALSE
      )

      wb <- openxlsx::createWorkbook()

      # Data sheet
      openxlsx::addWorksheet(wb, "item_data")
      openxlsx::writeData(wb, "item_data", template)
      openxlsx::setColWidths(wb, "item_data", cols = 1:9, widths = c(12,12,10,22,10,10,14,14,18))
      header_style <- openxlsx::createStyle(
        fgFill = "#006495", fontColour = "white",
        textDecoration = "bold", halign = "center"
      )
      openxlsx::addStyle(wb, "item_data", header_style, rows = 1, cols = 1:9, gridExpand = TRUE)

      # Instructions sheet
      openxlsx::addWorksheet(wb, "INSTRUCTIONS")
      openxlsx::writeData(wb, "INSTRUCTIONS", instructions)
      openxlsx::setColWidths(wb, "INSTRUCTIONS", cols = 1:3, widths = c(25, 75, 20))
      openxlsx::addStyle(wb, "INSTRUCTIONS", header_style, rows = 1, cols = 1:3, gridExpand = TRUE)

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$ds_grn_template <- downloadHandler(
    filename = function() "procuregraph_grn_template.xlsx",
    content  = function(file) {
      template <- data.frame(
        `_id`                   = c("grn001","grn002","grn003"),
        `buyerId`               = c("12345","67890","11111"),
        `creationDate_epoch_ms` = c(
          as.numeric(as.POSIXct("2024-11-10", tz="UTC")) * 1000,
          as.numeric(as.POSIXct("2024-11-25", tz="UTC")) * 1000,
          as.numeric(as.POSIXct("2024-12-05", tz="UTC")) * 1000
        ),
        `customerGrnNo`         = c("GRN-5001","GRN-5002","GRN-5003"),
        `poId`                  = c("PO-1001","PO-1002","PO-1003"),
        `itemId`                = c("ITEM-AAA","ITEM-BBB","ITEM-CCC"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      instructions <- data.frame(
        Column               = c(
          "_id", "buyerId", "creationDate_epoch_ms",
          "customerGrnNo", "poId", "itemId"
        ),
        Description          = c(
          "Unique GRN document ID (any string)",
          "Plant/buyer ID — must match plantId in mapping Excel",
          "GRN creation date as Unix epoch in MILLISECONDS, OR YYYY-MM-DD string",
          "Customer GRN number",
          "PO reference linked to this GRN",
          "Item reference linked to this GRN"
        ),
        Example              = c(
          "grn001", "12345", "1704067200000",
          "GRN-5001", "PO-1001", "ITEM-AAA"
        ),
        stringsAsFactors = FALSE
      )

      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, "grn_data")
      openxlsx::writeData(wb, "grn_data", template)
      openxlsx::setColWidths(wb, "grn_data", cols = 1:6, widths = c(12,12,22,16,14,14))
      header_style <- openxlsx::createStyle(
        fgFill = "#f59e0b", fontColour = "white",
        textDecoration = "bold", halign = "center"
      )
      openxlsx::addStyle(wb, "grn_data", header_style, rows = 1, cols = 1:6, gridExpand = TRUE)

      openxlsx::addWorksheet(wb, "INSTRUCTIONS")
      openxlsx::writeData(wb, "INSTRUCTIONS", instructions)
      openxlsx::setColWidths(wb, "INSTRUCTIONS", cols = 1:3, widths = c(25, 75, 20))
      openxlsx::addStyle(wb, "INSTRUCTIONS",
                         openxlsx::createStyle(fgFill="#f59e0b",fontColour="white",textDecoration="bold"),
                         rows=1, cols=1:3, gridExpand=TRUE)

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Return reactives for use in main server
  list(
    mode     = reactive(input$data_source_mode %||% "mongodb"),
    item_raw = item_data_raw,
    grn_raw  = grn_data_raw
  )
}
