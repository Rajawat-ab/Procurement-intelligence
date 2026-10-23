# ProcureGraph Disaster Recovery and Rebuild Specification

Version documented: ProcureGraph v6.1 cloud-compatible  
Purpose: This document is the functional blueprint for rebuilding ProcureGraph from scratch with same-to-same functionality, and then extending it into a more advanced version.

## 1. Product Overview

ProcureGraph is a secure R Shiny procurement intelligence application used to monitor PO integration, GRN performance, removed/excluded records, buyer-level opportunity, custom analytics, AI insights, commercial licensing, email reporting, and daily automated PDF reports.

The system converts raw procurement line-item and GRN data into business-ready dashboards by joining it with an Excel mapping workbook. It classifies each line item as `Consider` or `Remove`, calculates integration metrics, highlights company and buyer opportunities, and supports exports for operational follow-up.

Core users:

- Procurement operations leaders.
- Integration/business teams tracking SAP, Manual, EOC adoption.
- Account/business POCs receiving performance alerts.
- Commercial users evaluating paid ProcureGraph features.
- Developers/support team maintaining the dashboard and scheduled reports.

## 2. Technology Stack

Current implementation:

- Language/framework: R Shiny.
- UI: Shiny HTML tags, Tailwind CDN classes, Material Symbols, Font Awesome icons.
- Data processing: dplyr, tidyr, purrr, lubridate, stringr.
- Tables: DT/DataTables.
- Charts: ggplot2 and plotly.
- Database: MongoDB via mongolite.
- Excel input/output: readxl and openxlsx.
- AI: Ollama local API and Groq OpenAI-compatible API.
- Email: curl command to Outlook/Office 365 SMTP.
- Scheduling: Windows Task Scheduler using PowerShell.
- Reports: R script generating daily PDFs and HTML email summaries.

Required R packages:

`shiny`, `shinyjs`, `dplyr`, `tidyr`, `DT`, `mongolite`, `readxl`, `lubridate`, `purrr`, `htmltools`, `memoise`, `cachem`, `stringr`, `curl`, `plotly`, `ggplot2`, `scales`, `httr`, `jsonlite`, `openxlsx`.

## 3. Repository Layout

Current important files:

- `app.R`: Main Shiny app, authentication, UI shell, data pipeline, dashboards, email, exports, commercial page.
- `mod_data_source.R`: MongoDB versus Excel upload data source module.
- `mod_global_filters.R`: Advanced global analytics filters and filter presets.
- `mod_custom_analytics.R`: Custom chart builder with plotly and drill-down.
- `mod_ai_insights.R`: AI chat panel using Ollama or Groq.
- `mod_dashboard_save.R`: Save/load/export/import custom dashboard chart configurations.
- `mod_removed_data.R`: Removed data, missing mapping, buyer opportunity, MoM removed analysis.
- `scripts/daily_procuregraph_reports.R`: Automated daily PDF and email reporting job.
- `scripts/register_daily_report_task.ps1`: Registers Windows scheduled task.
- `scripts/run_daily_report_task.ps1`: Executes daily reporting job and writes logs.
- `DAILY_REPORT_SETUP.md`: Setup notes for automated daily reports.

## 4. Environment Variables

Core app:

- `PROCUREGRAPH_MONGO_CONN`: MongoDB connection string.
- `PROCUREGRAPH_APP_USER`: Login username.
- `PROCUREGRAPH_APP_PASS`: Login password.

Commercial/license:

- `PROCUREGRAPH_LICENSE_TIER`: `trial`, `pro`, or `enterprise`.
- `PROCUREGRAPH_LICENSE_KEY`: License key.
- `PROCUREGRAPH_CUSTOMER_NAME`: Customer/deployment name.
- `PROCUREGRAPH_LICENSE_EXPIRES`: Expiry date in `YYYY-MM-DD`.
- `PROCUREGRAPH_PRO_PRICE`: Monthly price shown on commercial page.
- `PROCUREGRAPH_SALES_CONTACT`: Sales/support contact email.

Daily reports:

- `PROCUREGRAPH_MAPPING_FILE`: Full path to mapping Excel workbook.
- `PROCUREGRAPH_REPORT_DIR`: Output folder for generated daily reports.
- `PROCUREGRAPH_EMAIL_FROM`: Sender email.
- `PROCUREGRAPH_EMAIL_TO`: Comma-separated recipient emails.
- `PROCUREGRAPH_EMAIL_PASS`: Outlook app password.
- `PROCUREGRAPH_SMTP_HOST`: Usually `smtp.office365.com`.
- `PROCUREGRAPH_SMTP_PORT`: Usually `587`.

## 5. Authentication

The app starts with a custom login screen. Access is allowed only when:

- `PROCUREGRAPH_APP_USER` is configured.
- `PROCUREGRAPH_APP_PASS` is configured.
- Entered username exactly matches `PROCUREGRAPH_APP_USER`.
- Entered password exactly matches `PROCUREGRAPH_APP_PASS`.

On logout:

- User is marked unauthenticated.
- Cached processed data is cleared.
- Cache timestamp is cleared.

## 6. Data Sources

ProcureGraph supports two modes.

### MongoDB Mode

Item data is fetched from:

- Database: `procurement`
- Collection: `item`
- Date filter: `creationDate` between selected start and end dates.

Projected item fields:

- `_id`
- `buyerId` from `buyer.id`
- `status`
- `creationDate`
- `source`
- `stage`
- `histPoCustomerPoNo` from `history.po.customerPoNo`
- `histPoCustomerSANumber` from `history.po.customerSANumber`
- `amount`

GRN data is fetched from:

- Database: `procurement`
- Collection: `grn`

Projected GRN fields:

- `_id`
- `buyerId`
- `creationDate`
- `customerGrnNo`
- `poId`
- `itemId`

### Excel Mode

Excel mode allows the app to run without MongoDB, useful for cloud deployments or offline demos.

Item upload accepts `.xlsx` or `.xls`.

Canonical item columns:

- `_id`
- `buyerId`
- `status`
- `creationDate`
- `source`
- `stage`
- `amount`
- `histPoCustomerPoNo`
- `histPoCustomerSANumber`

Supported aliases include common variants such as `buyer_id`, `BuyerID`, `PlantId`, `creationDate_epoch`, `customerPoNo`, `customerSANumber`, `poAmount`, and `value`.

GRN upload accepts `.xlsx` or `.xls`.

Canonical GRN columns:

- `_id`
- `buyerId`
- `creationDate`
- `customerGrnNo`
- `poId`
- `itemId`

Creation dates may be Unix epoch milliseconds or valid date strings. The app normalizes dates to epoch milliseconds internally.

Excel templates are downloadable from the sidebar for both item and GRN data.

## 7. Mapping Workbook

The user uploads one Excel mapping workbook through the sidebar.

Required sheet: `PID-CID`

Purpose: Map buyer/plant IDs to company IDs.

Required fields or accepted aliases:

- `plantId`: aliases include `PlantId`, `Plant ID`, `plant_id`.
- `companyId`: aliases include `CompanyId`, `Company ID`, `company_id`.

Required sheet: `150 CID`

Purpose: Map company IDs to names, integration status, GRN eligibility, and Business POC.

Fields or accepted aliases:

- `companyId`: `Company ID`, `CompanyID`, `company_id`, `companyId`, `COMPANY ID`.
- `companyName`: `Company Name`, `CompanyName`, `company_name`, `COMPANY NAME`.
- `Final Status`: `Integration Status`, `IntegrationStatus`, `integration_status`, `Final Status`, `INTEGRATION STATUS`.
- `GRN`: `GRN Status`, `GRNStatus`, `grn_status`, `GRN`, `GRN STATUS`.
- `BusinessPOC`: `Business POC`, `BusinessPoc`, `business_poc`, `Business POC Email`, `POC Email`.

Mapping warnings must show if:

- `plantId` missing.
- `companyId` missing.
- Company ID values are NA.
- Integration Status is missing or all NA.
- GRN Status is missing or all NA.
- Any company IDs from `PID-CID` do not match `150 CID`.

## 8. Data Processing Pipeline

On `LOAD DATA`, the system:

1. Reads selected date range.
2. Loads raw item data from MongoDB or Excel.
3. Ensures `_id` exists.
4. Normalizes amount into numeric `amount`.
5. Calculates `Amount_Crore = amount / 10000000`.
6. Converts `creationDate` epoch milliseconds to `CreationDate`.
7. Creates `Month = YYYY-MM`.
8. Creates `Consider Month` as `Consider` if `CreationDate` lies inside the selected date range, otherwise `Ignore`.
9. Joins buyer ID to `PID-CID` mapping.
10. Joins company ID to `150 CID`.
11. Fills unmatched `PID-CID` and `Client Name` as `Unknown`.
12. Applies all business rules.
13. Produces `Remove(Y/N)` as final inclusion flag.

Final processed item output columns:

- `_id`
- `buyer.id`
- `history.po.customerPoNo`
- `history.po.customerSANumber`
- `status`
- `creationDate`
- `amount`
- `Amount_Crore`
- `source`
- `stage`
- `Stage`
- `CreationDate`
- `Month`
- `Consider Month`
- `PID-CID`
- `Client Name`
- `Consider Source`
- `Consider Stage`
- `Final Status`
- `RemoveHavellsCapex`
- `RemoveSC`
- `Remove(Y/N)`
- `GRN`

## 9. Business Rules

### Consider Source

`Consider Source` equals the source when `source` is one of:

- `EOC`
- `Manual`
- `SAP`

Otherwise it is `Ignore`.

### Consider Stage

`Consider Stage` is `Ignore` if `Stage` is:

- `Closed`
- `Cancelled`

Otherwise it is `Consider`.

### Final Status

`Final Status` comes from mapping workbook. Missing values become `Ignore`.

Tracked statuses:

- `Live`
- `Live - CBB`
- `No Integration`
- `PR Int. - Punchout`
- `Ignore`

### GRN

`GRN` comes from mapping workbook. Missing values become `Ignore`.

### Havells Capex Rule

If `PID-CID == 1211`:

- If first two characters of `history.po.customerSANumber` are `45`, `RemoveHavellsCapex = Consider`.
- Otherwise `RemoveHavellsCapex = Ignore`.

For all other companies: `RemoveHavellsCapex = Consider`.

### SC Rule

If `PID-CID == 8853`:

- For buyer IDs `9550`, `9557`, `25976`, `26023`, `26186`, `26187`:
  - If first two characters of `history.po.customerSANumber` are `71`, `RemoveSC = Ignore`.
  - Otherwise `RemoveSC = Consider`.
- For all other buyers under `8853`, `RemoveSC = Ignore`.

For all other companies: `RemoveSC = Consider`.

### Remove(Y/N)

`Remove(Y/N) = Remove` if any of these are true:

- `Consider Source == Ignore`
- `Consider Stage == Ignore`
- `Consider Month == Ignore`
- `Final Status == Ignore`
- `RemoveHavellsCapex == Ignore`
- `RemoveSC == Ignore`
- `GRN == Ignore`

Otherwise `Remove(Y/N) = Consider`.

## 10. Integration Definitions

Integrated line items are those where:

- `source == SAP`
- or `source == Manual`

Non-integrated/opportunity line items are generally represented by:

- `source == EOC`

Core formula:

`Integration % = (SAP + Manual) / (SAP + Manual + EOC) * 100`

CBB formula used in pivots:

`CBB % = Manual / Total * 100`

Final formula used in pivots:

`Final % = (SAP + Manual) / Total * 100`

Value integration formula:

`Value Integration % = (SAP value + Manual value) / Total value * 100`

Amounts are converted to crores.

## 11. Global Shell and Controls

Sidebar:

- Upload mapping workbook.
- Open missing PID-CID report.
- Select data source: MongoDB or Excel.
- Upload item Excel in Excel mode.
- Upload GRN Excel in Excel mode.
- Download item and GRN templates.
- Select start date and end date.
- Date presets: Last 7D, Last 30D, Last 90D, Month TD.
- Load data.
- Logout.

Header:

- Mobile menu button.
- Global search over client, source, stage, and buyer ID.
- Integration filter:
  - All MOM
  - Integrated
  - Non-Integrated
- Last refreshed timestamp.
- Email key insights button.
- Business POC alert button.
- User avatar.

Navigation pages:

- Dashboard
- PO Integration
- GRN Analysis
- GRN 2.0
- Removed Data
- Raw Reports
- Commercial
- Advanced Analytics

## 12. Dashboard Page

The Dashboard page gives the main executive overview.

Required cards/widgets:

- Top 5 integrated-volume companies.
- No Integration target list.
- Companies by integration status.
- Integration rate with ABFRL.
- Integration rate without ABFRL.
- Count pivot without ABFRL.
- Count pivot with ABFRL.
- Value pivot without ABFRL.
- Value pivot with ABFRL.

ABFRL exclusion is done by removing `PID-CID == 9802`.

Pivot tables include rows:

- `Live`
- `Live - CBB`
- `No Integration`
- `PR Int. - Punchout`
- `Grand Total`

Pivot columns:

- `EOC`
- `Manual`
- `SAP`
- `Grand Total`

Exports:

- `pivot_without_<date>.csv`
- `pivot_with_<date>.csv`
- `value_pivot_without_<date>.csv`
- `value_pivot_with_<date>.csv`

## 13. PO Integration Page

Purpose: Month-on-month PO integration tracking by company, by value, and by month.

Company monthly table:

- Excludes `PID-CID == 9802`.
- Groups by `PID-CID`, `Client Name`, and `Month`.
- Calculates SAP, Manual, EOC, Total, Integration % per month.
- Adds overall SAP, Manual, EOC, Total, Overall Integration %.
- Displays month columns in readable format such as `Jan 2026 SAP`.
- Uses color indicators:
  - Green for integration >= 85%.
  - Orange for 70% to 85%.
  - Red for < 70%.

Value monthly table:

- Excludes `PID-CID == 9802`.
- Groups by company and month.
- Calculates SAP Value Cr, Manual Value Cr, EOC Value Cr, Total Value Cr, Value Integration %.
- Adds overall value totals and overall value integration %.

MoM chart:

- Uses `ggplot2`.
- Shows monthly Integration %.
- Has threshold lines at 85% target and 70% minimum.

Month table:

- Month.
- Line items.
- Companies.
- MoM Growth.

Exports:

- `po_company_monthly_<date>.xlsx`
- `po_value_company_monthly_<date>.csv`
- `po_month_<date>.csv`
- `po_detail_<date>.csv`

## 14. GRN Analysis Page

GRN records are loaded separately and joined to the same mapping workbook.

GRN filtering:

- GRN records must map to companies where processed item data has `GRN == Y`.
- Exclude ABFRL: `PID-CID != 9802`.

GRN integrated statuses:

- `Live`
- `Live - CBB`
- `PR Int. - Punchout`

Eligible GRN accounts:

- From mapping workbook `150 CID`.
- `GRN == Y`.
- `companyId != 9802`.
- Final Status in GRN integrated statuses.

PO base for GRN:

- Uses valid `Consider` PO line items.
- Excludes `PID-CID == 9802`.
- Final Status in GRN integrated statuses.

Cards:

- Total GRN records.
- GRN companies.
- GRN match rate = GRN records / eligible PO line items.
- Eligible accounts.
- PO accounts.
- PO line items.

Company table:

- `PID-CID`
- `Client Name`
- `Final Status`
- `PO Volume Account`
- `GRN Volume Account`
- `PO Line Items`
- `GRN Records`
- `GRN %`
- SAP
- Manual
- EOC

Monthly table:

- Month.
- GRN eligible accounts.
- PO volume accounts.
- GRN volume accounts.
- PO line items.
- GRN records.
- GRN %.
- GRN MoM Growth.

Detail table:

- `PID-CID`
- `Client Name`
- `buyerId`
- `customerGrnNo`
- `poId`
- `itemId`
- `GrnDate`
- `Month`

Exports:

- `grn_company_<date>.csv`
- `grn_month_<date>.csv`
- `grn_detail_<date>.csv`

## 15. GRN 2.0 Page

Purpose: Analyze relationship between POs and GRNs.

PO relationship table groups GRN records by PO ID.

Fields:

- `PO ID`
- `PID-CID`
- `Client Name`
- `Buyer IDs`
- `First GRN Date`
- `Last GRN Date`
- `First Month`
- `Last Month`
- `GRN Records`
- `Distinct GRNs`
- `Distinct Items`
- `GRN Relationship`
- `GRN Numbers`

GRN relationship values:

- `Missing PO ID` when PO ID is missing.
- `Single GRN` when distinct GRNs <= 1.
- `Multiple GRNs` when distinct GRNs > 1.

Monthly GRN 2.0 table:

- Month.
- POs With GRNs.
- Single-GRN POs.
- Multi-GRN POs.
- GRN Records.
- Distinct GRNs.
- Avg GRNs / PO.

Cards:

- Total POs.
- Single-GRN POs.
- Multi-GRN POs.
- Average GRNs per PO.

Exports:

- `grn2_po_relationship_<date>.csv`
- `grn2_month_<date>.csv`

## 16. Removed Data Page

Purpose: Identify all removed buyer IDs and explain why line items were removed.

Removed data:

- Any row where `Remove(Y/N) == Remove`.

PunchIn removed data:

- Removed rows where `Final Status == PR Int. - Punchout`.

Missing mapping data:

- Rows where `PID-CID` is missing/unknown or `Client Name` is missing/unknown.

Removal reason priority:

1. Missing PID-CID mapping.
2. Source ignored.
3. Closed / Cancelled stage.
4. Outside selected month.
5. Missing / ignored final status.
6. Havells Capex rule.
7. SC rule.
8. GRN rule.
9. Other removal rule.

Buyer summary:

- Buyer ID.
- Client Name.
- PID-CID.
- Total Removed Records.
- PunchIn Removed Records.
- SAP.
- Manual.
- EOC.
- Top Removal Reason.

Removed MoM summary:

- Buyer ID.
- Client Name.
- PID-CID.
- First Month.
- Latest Month.
- Active Months.
- Total Removed Records.
- Latest Month Records.
- Previous Month Records.
- MoM Change.
- Trend.
- Pattern.

Pattern classification:

- `Short-term 1-2 months` when active months <= 2.
- `Continuous / rising` when trend is increasing.
- `Recurring` otherwise.

Missing PID-CID opportunity table:

- Buyer ID.
- Total Records.
- Removed Records.
- PunchIn Records.
- SAP.
- Manual.
- EOC.
- Latest Month.
- Latest Month Records.
- Previous Month Records.
- MoM Change.
- Trend.

Exports:

- `removed_all_buyers_<date>.csv`
- `removed_all_detail_<date>.csv`
- `missing_pidcid_opportunity_<date>.csv`
- `removed_mom_by_buyer_<date>.csv`

## 17. Raw Reports Page

Purpose: Show raw processed line-level data and explain formulas/columns.

Must include:

- Mapping warnings.
- Expandable Column Reference & Formulas section.
- Raw Data table with all processed columns.
- Export raw CSV.

Column reference must explain:

- MongoDB source columns.
- Calculated columns.
- Excel mapping joins.
- Consider/Remove rules.

Raw export:

- `raw_data_<date>.csv`

## 18. Advanced Analytics Page

Sections:

- Data source status.
- Mapping warnings.
- Global filter panel.
- Data health monitor.
- Benchmark summary.
- Opportunity radar.
- Dashboard save/load.
- Custom chart builder.
- AI insights panel.

### Global Filters

Filter dimensions:

- Date range.
- Consider Source.
- Client Name.
- Source.
- Final Status.

Filters narrow each other dynamically. A filter preset system must support:

- Save preset.
- Load preset.
- Delete preset.
- Reset filters.

Presets are stored in session memory.

### Data Health Monitor

Cards:

- Valid rows.
- Removal rate.
- Unknown clients.
- No Integration rows.

### Benchmark Summary

Shows:

- Latest month.
- Latest integration %.
- Best company.
- Momentum readout versus previous month.
- Active companies and valid line items in latest visible month.

### Opportunity Radar

Uses companies with `Grand_Total >= 10`.

Formula:

`Opportunity_Score = (100 - Integration_Pct) * log1p(Grand_Total)`

Shows top 5 companies by opportunity score.

## 19. Custom Chart Builder

Datasets:

- Valid Line Items (Consider).
- Processed Data (All).
- Company Summary.

Controls:

- Data source.
- X-axis category.
- Y-axis value or Count.
- Chart type.
- Optional group by.
- Chart title.
- Subtitle.
- Show data labels.
- Generate chart.
- Save config.
- Clear drill-down.

Chart types:

- Bar.
- Horizontal Bar.
- Stacked Bar.
- Line.
- Area.
- Pie.
- Donut.
- Scatter.

Drill-down:

- Clicking a bar/slice/point opens a second chart.
- Drill-down filters on clicked category.
- Preferred breakdown dimension is `Final Status`, then `Consider Source`, then first available categorical field.

Plot exports:

- Plotly modebar supports PNG download.
- Filename starts with `procuregraph_`.

## 20. Dashboard Save/Load

Storage mode auto-detected:

- Local filesystem: `~/.procuregraph/dashboards/*.json`.
- Cloud/read-only filesystem: reactive in-memory storage.

Operations:

- Save chart config by name.
- Load saved config into chart builder.
- Delete selected config after confirmation.
- Export all configs as one JSON bundle.
- Import JSON bundle.

Saved config fields:

- dataset.
- x_axis.
- y_axis.
- chart_type.
- group_by.
- show_labels.
- title.
- subtitle.

Export filename:

- `procuregraph_dashboards_<timestamp>.json`

## 21. AI Insights

AI backends:

- Auto: try Ollama first, otherwise Groq.
- Ollama.
- Groq API.

Ollama:

- Host: `http://localhost:11434`.
- Model: `gemma2:2b`.
- Endpoint: `/api/chat`.

Groq:

- Endpoint: `https://api.groq.com/openai/v1/chat/completions`.
- Default model: `llama-3.1-8b-instant`.
- Other selectable models:
  - `llama-3.3-70b-versatile`
  - `gemma2-9b-it`
  - `mixtral-8x7b-32768`

AI data context includes:

- Definition of integrated sources.
- Total line items.
- Integrated items.
- Integration rate.
- Source mix.
- MoM integration trend.
- High-performing companies.
- At-risk companies.

AI chat features:

- Analyze Data button.
- Free-text question input.
- Quick prompts:
  - Top actions.
  - At-risk company.
  - Executive summary.
- Clear chat.
- Backend status badge.
- Loading indicator.

Groq API key is entered in the UI and not stored by the app.

## 22. Email Features

### Key Insights Email

The header email button opens a modal for Outlook/Office 365 SMTP.

Inputs:

- Sender email.
- Recipient email.
- Outlook app password.

SMTP:

- Host: `smtp.office365.com`.
- Port: `587`.
- TLS.

Email includes:

- HTML executive report.
- Top 5 integration companies.
- No Integration target list.
- PO integration account count.
- GRN integration account count.
- GRN summary.

Subject:

- `ProcureGraph Insights - <date range>`

Logs:

- `logs/email_smtp.log`

Passwords and auth payloads must be redacted in logs.

### Business POC Alerts

Requires `BusinessPOC` column in mapping workbook.

Recipients are companies with valid Business POC emails.

Performance tier:

- Excellent: integration >= 85%.
- Good: integration >= 70% and < 85%.
- Critical: integration < 70%.

Each company receives a tiered HTML email with:

- Company name.
- Integrated items.
- Total items.
- Integration %.
- Tone and recommended actions based on tier.
- Report period.
- Generated timestamp.

Subjects:

- Excellent: `Integration Performance Update`.
- Good: `Integration Review Required`.
- Critical: `URGENT: Integration Performance Alert`.

## 23. Commercial Page

Purpose: Turn ProcureGraph into a commercial/paid procurement intelligence product.

License active when:

- Tier is `pro` or `enterprise`.
- License key is non-empty.
- Expiry date is empty or not expired.

Shows:

- License status.
- Customer.
- Tier.
- Expiry.
- Pricing cards: Trial, Pro, Enterprise.
- Sellable Pro features.
- Activation setup environment variables.

Downloads:

- License request JSON.
- Pro executive pack Excel, only when license active.

License request export:

- App name.
- Requested tier.
- Current tier.
- Customer name.
- Generated timestamp.
- Machine hint.
- Contact.

Executive pack sheets:

- Executive Summary.
- Monthly Trend.
- Opportunity Pipeline.
- Action Plan.

Executive pack opportunity priority:

- Critical: Integration < 50 and Grand Total >= 25.
- High: Integration < 70.
- Medium: Integration < 85.
- Maintain: otherwise.

## 24. Daily Automated Reports

Script:

- `scripts/daily_procuregraph_reports.R`

Generated daily date ranges:

- MTD.
- Last Month.
- Last 3 Months.

For each range:

- Fetch MongoDB item data.
- Fetch MongoDB GRN data.
- Load mapping workbook.
- Process line items with same core rules.
- Generate PDF report.

PDF output folder:

- `PROCUREGRAPH_REPORT_DIR/YYYY-MM-DD/`

PDF pages:

1. Executive summary.
2. PO integration count view.
3. PO integration value view in crores.
4. GRN and priority action summary.

Email body contains:

- PO Integration Item Wise.
- PO Integration Value Wise.
- GRN Integration.
- Attached PDF list.

Scheduled task:

- Default task name: `ProcureGraph Daily PDF Reports`.
- Default time: daily 10:00.
- Logs: `logs/daily_report_task.log`.

## 25. Caching and Performance

MongoDB processed data cache:

- Duration: 5 minutes.
- Cache is used only in MongoDB mode.
- Cache clears when date range, mapping file, or data source mode changes.

Pivot computations:

- Count and value pivots are memoised with in-memory cache.
- Cache max age: 60 seconds.

## 26. UI and Responsiveness

The UI is a dashboard shell with:

- Fixed dark sidebar on desktop.
- Sticky white header.
- Mobile sidebar overlay and hamburger menu.
- Responsive single-column behavior on mobile.
- DataTables with horizontal scrolling.

Brand styling:

- Primary red: `#da191e`.
- Deep navy: `#0e1d28`.
- Sidebar navy: `#1c2b36`.
- Procurement blue: `#006495`.
- Success green: `#10b981`.
- Warning orange: `#f97316` / `#f59e0b`.

Icons:

- Material Symbols are used throughout navigation and action buttons.

## 27. Rebuild Acceptance Checklist

A rebuilt ProcureGraph is functionally equivalent only if all of these pass:

- Login works using environment variables.
- Mapping workbook upload reads `PID-CID` and `150 CID`.
- Mapping warnings match current behavior.
- MongoDB mode fetches item and GRN data by date.
- Excel mode reads item and GRN uploads and templates download.
- `Remove(Y/N)` matches all current business rules.
- Amounts convert to crores.
- Global search filters buyer ID, client, source, and stage.
- Integrated/non-integrated header filter works.
- Dashboard pivots match current calculations.
- ABFRL exclusion uses `PID-CID != 9802`.
- PO page monthly volume and value tables match current outputs.
- GRN Analysis uses eligible accounts and GRN statuses correctly.
- GRN 2.0 classifies Single GRN, Multiple GRNs, and Missing PO ID correctly.
- Removed Data page outputs buyer, reason, missing mapping, and MoM tables.
- Raw Reports page displays all processed data and formula reference.
- Advanced filters narrow each other and presets work.
- Custom chart builder supports all chart types and drill-down.
- Dashboard config save/load/export/import works locally and in cloud mode.
- AI works with Ollama locally and Groq in cloud mode.
- Key Insights email sends through Outlook SMTP and logs safely.
- POC alert emails send tiered company-specific messages.
- Commercial license status and gated executive pack work.
- Daily report scripts generate three PDFs and email summary.

## 28. Recommended Advanced Version

An advanced rebuild should preserve all above behavior and add:

- Real user management with roles: admin, analyst, viewer, commercial.
- Database-backed saved dashboards and filter presets.
- Audit log for login, data loads, exports, emails, and AI usage.
- Background job queue for emails and report generation.
- Central configuration screen instead of environment-only setup.
- API layer for item, GRN, mapping, dashboards, and reports.
- Automated test suite for all business rules.
- Versioned mapping workbook import history.
- Data quality dashboard showing mapping gaps, invalid dates, invalid amounts, duplicate buyer mappings.
- Role-based access to Commercial and Email features.
- Scheduled report management from UI.
- PDF and Excel report templates with customer branding.
- Secure secret storage for SMTP/API keys.
- Optional SSO for enterprise deployments.
- Deployment using Docker with separate app, worker, and database services.

## 29. Suggested Future Architecture

Frontend:

- React or modern Shiny if staying in R.
- Componentized dashboard pages.
- Reusable table, card, chart, filter, and modal components.

Backend:

- API service for data processing and report generation.
- Worker service for PDF/email jobs.
- Database for users, saved dashboards, presets, mapping versions, logs.

Data layer:

- MongoDB for procurement raw data.
- PostgreSQL for ProcureGraph app metadata.
- Object storage for generated reports.

Analytics:

- Keep business-rule engine as a tested module.
- Store processed snapshots for repeatable historical reporting.
- Add semantic layer for AI so prompts use stable metric definitions.

Security:

- Hashed passwords or SSO.
- Role-based permissions.
- Encrypted secret storage.
- Download and email audit trails.

## 30. Disaster Recovery Steps

To restore the current system quickly:

1. Install R and required packages.
2. Place all app files in one ProcureGraph directory.
3. Configure `.Renviron` or hosting environment variables.
4. Confirm MongoDB connection string.
5. Start Shiny app from the directory containing `app.R`.
6. Login using configured credentials.
7. Upload the mapping workbook.
8. Select date range and load data.
9. Validate dashboard totals against known previous report.
10. Register daily report scheduled task if needed.
11. Send test email to internal recipient.
12. Export one raw CSV and one PO monthly workbook to verify downloads.

## 31. Notes for Development Team

The most important part of ProcureGraph is not the UI. It is the exact business logic that decides which rows are considered and how integration is calculated. Rebuild that first, test it with fixed sample data, and only then rebuild dashboards.

Treat these as protected rules:

- Valid sources are only `EOC`, `Manual`, and `SAP`.
- Integrated means `SAP` or `Manual`.
- ABFRL exclusion is `PID-CID == 9802`.
- Havells and SC rules must match exactly.
- GRN eligibility depends on both mapping `GRN == Y` and allowed final statuses.
- Missing mapping must be visible as an opportunity, not hidden.

