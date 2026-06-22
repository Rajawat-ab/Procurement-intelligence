param(
  [string]$RscriptPath = ""
)

$ErrorActionPreference = "Stop"

$ProjectDir = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$ReportScript = Join-Path $ProjectDir "scripts\daily_procuregraph_reports.R"
$LogDir = Join-Path $ProjectDir "logs"
$LogFile = Join-Path $LogDir "daily_report_task.log"

if (!(Test-Path $LogDir)) {
  New-Item -ItemType Directory -Path $LogDir -Force | Out-Null
}

function Write-TaskLog {
  param([string]$Message)
  $stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
  Add-Content -Path $LogFile -Value "[$stamp] $Message"
}

try {
  Write-TaskLog "Starting ProcureGraph daily report task."
  Write-TaskLog "ProjectDir: $ProjectDir"
  Write-TaskLog "ReportScript: $ReportScript"

  if (!(Test-Path $ReportScript)) {
    throw "Report script not found: $ReportScript"
  }

  if ([string]::IsNullOrWhiteSpace($RscriptPath)) {
    $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
    if ($cmd) {
      $RscriptPath = $cmd.Source
    }
  }

  if ([string]::IsNullOrWhiteSpace($RscriptPath)) {
    $candidates = @()
    foreach ($root in @("C:\Program Files\R", "C:\Program Files (x86)\R")) {
      if (Test-Path $root) {
        $candidates += Get-ChildItem -Path $root -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue |
          Select-Object -ExpandProperty FullName
      }
    }
    if ($candidates.Count -gt 0) {
      $RscriptPath = $candidates | Sort-Object -Descending | Select-Object -First 1
    }
  }

  if ([string]::IsNullOrWhiteSpace($RscriptPath) -or !(Test-Path $RscriptPath)) {
    throw "Rscript.exe was not found. Re-register the task with -RscriptPath 'C:\Program Files\R\R-x.x.x\bin\Rscript.exe'."
  }

  Write-TaskLog "Using Rscript: $RscriptPath"
  Set-Location $ProjectDir

  $previousErrorActionPreference = $ErrorActionPreference
  $ErrorActionPreference = "Continue"
  $output = & $RscriptPath $ReportScript 2>&1
  $exitCode = $LASTEXITCODE
  $ErrorActionPreference = $previousErrorActionPreference
  if ($output) {
    Add-Content -Path $LogFile -Value $output
  }
  Write-TaskLog "Rscript exit code: $exitCode"

  if ($exitCode -ne 0) {
    exit $exitCode
  }

  Write-TaskLog "ProcureGraph daily report task completed successfully."
  exit 0
}
catch {
  Write-TaskLog "ERROR: $($_.Exception.Message)"
  exit 1
}
