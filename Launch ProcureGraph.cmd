@echo off
setlocal
cd /d "%~dp0"

rem Find Rscript: per-user R install first, then Program Files, then PATH
set "RS="
for /d %%R in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%R\bin\Rscript.exe" set "RS=%%R\bin\Rscript.exe"
if not defined RS for /d %%R in ("%ProgramFiles%\R\R-*") do if exist "%%R\bin\Rscript.exe" set "RS=%%R\bin\Rscript.exe"
if not defined RS for %%X in (Rscript.exe) do set "RS=%%~$PATH:X"
if not defined RS (
    echo R was not found. Install R or update this file with the path to Rscript.exe.
    pause
    exit /b 1
)

rem Use the BuyerOS launcher login instead of ProcureGraph's own (local only)
set "PROCUREGRAPH_LOCAL_SSO=1"
start "ProcureGraph (close to stop)" /min "%RS%" -e "shiny::runApp('.', host='127.0.0.1', port=5080, launch.browser=TRUE)"
exit /b 0
