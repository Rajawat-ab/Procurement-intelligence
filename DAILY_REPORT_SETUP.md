# ProcureGraph Daily PDF Reports

This setup generates three 4-page PDFs every day:

- MTD
- Last Month
- Last 3 Months

The PDFs are saved in a dated report folder and emailed as attachments. The email body is an eye-catching HTML summary with only these blocks:

- PO Integration Item Wise
- PO Integration Value Wise
- GRN Integration

## 1. Configure Environment

Add these values to `.Renviron` in the Procure Graph folder:

```text
PROCUREGRAPH_MAPPING_FILE=C:/path/to/your/mapping.xlsx
PROCUREGRAPH_REPORT_DIR=C:/ProcureGraphReports

PROCUREGRAPH_EMAIL_FROM=your.email@moglix.com
PROCUREGRAPH_EMAIL_TO=recipient1@moglix.com,recipient2@moglix.com
PROCUREGRAPH_EMAIL_PASS=your_outlook_app_password
PROCUREGRAPH_SMTP_HOST=smtp.office365.com
PROCUREGRAPH_SMTP_PORT=587
```

`PROCUREGRAPH_MONGO_CONN` should already exist in `.Renviron`.

## 2. Test Manually

From PowerShell:

```powershell
cd "C:\Users\abhishek.rajawat\OneDrive - moglix.com\My Automations\ProcureGraph\Procure Graph"
Rscript ".\scripts\daily_procuregraph_reports.R"
```

The report folder will be:

```text
PROCUREGRAPH_REPORT_DIR/YYYY-MM-DD/
```

## 3. Register Daily 10 AM Task

From PowerShell:

```powershell
cd "C:\Users\abhishek.rajawat\OneDrive - moglix.com\My Automations\ProcureGraph\Procure Graph"
powershell -ExecutionPolicy Bypass -File ".\scripts\register_daily_report_task.ps1"
```

If `Rscript.exe` is not on PATH, pass the full path:

```powershell
powershell -ExecutionPolicy Bypass -File ".\scripts\register_daily_report_task.ps1" -RscriptPath "C:\Program Files\R\R-4.4.0\bin\Rscript.exe"
```

## 4. Test Scheduled Task Immediately

```powershell
Start-ScheduledTask -TaskName "ProcureGraph Daily PDF Reports"
```

Logs are written to:

```text
logs/daily_report_task.log
```

## Report Pages

Each PDF has four pages:

1. Executive summary
2. PO integration count view
3. PO integration value view in crores
4. GRN and priority action summary
