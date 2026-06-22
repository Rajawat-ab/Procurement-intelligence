param(
  [string]$TaskName = "ProcureGraph Daily PDF Reports",
  [string]$RunAt = "10:00",
  [string]$RscriptPath = "Rscript.exe"
)

$ErrorActionPreference = "Stop"

$ProjectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$ScriptPath = Join-Path $ProjectDir "scripts\daily_procuregraph_reports.R"
$RunnerPath = Join-Path $ProjectDir "scripts\run_daily_report_task.ps1"
$LogDir = Join-Path $ProjectDir "logs"
$LogFile = Join-Path $LogDir "daily_report_task.log"

if (!(Test-Path $ScriptPath)) {
  throw "Report script not found: $ScriptPath"
}

if (!(Test-Path $RunnerPath)) {
  throw "Task runner not found: $RunnerPath"
}

if (!(Test-Path $LogDir)) {
  New-Item -ItemType Directory -Path $LogDir | Out-Null
}

$ActionArgs = "-NoProfile -ExecutionPolicy Bypass -File `"$RunnerPath`" -RscriptPath `"$RscriptPath`""
$Action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument $ActionArgs
$Trigger = New-ScheduledTaskTrigger -Daily -At $RunAt
$Settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -MultipleInstances IgnoreNew -ExecutionTimeLimit (New-TimeSpan -Hours 2)
$Principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

Register-ScheduledTask `
  -TaskName $TaskName `
  -Action $Action `
  -Trigger $Trigger `
  -Settings $Settings `
  -Principal $Principal `
  -Description "Generates MTD, Last Month, and Last 3 Months ProcureGraph PDF reports and emails them." `
  -Force | Out-Null

Write-Host "Registered task: $TaskName"
Write-Host "Schedule: Daily at $RunAt"
Write-Host "Project: $ProjectDir"
Write-Host "Log: $LogFile"
Write-Host ""
Write-Host "To test immediately:"
Write-Host "Start-ScheduledTask -TaskName '$TaskName'"
