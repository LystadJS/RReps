@echo off
setlocal EnableExtensions DisableDelayedExpansion
pushd "%~dp0.." || exit /b 1

for %%R in (Rscript.exe) do set "rscript=%%~$PATH:R"
if defined rscript goto launch

for %%K in ("HKCU\SOFTWARE\R-core\R" "HKLM\SOFTWARE\R-core\R") do (
    for /f "tokens=2,*" %%A in ('reg.exe query "%%~K" /v InstallPath 2^>nul') do (
        if /i "%%A"=="REG_SZ" if exist "%%B\bin\Rscript.exe" (
            set "rscript=%%B\bin\Rscript.exe"
            goto launch
        )
    )
)

set "rscript=%ProgramFiles%\R\R-4.5.3\bin\Rscript.exe"
if exist "%rscript%" goto launch
for /f "delims=" %%R in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
    if exist "%ProgramFiles%\R\%%R\bin\Rscript.exe" (
        set "rscript=%ProgramFiles%\R\%%R\bin\Rscript.exe"
        goto launch
    )
)
echo Rscript.exe was not found. Install R 4.5.3 or add its bin directory to PATH. 1>&2
popd
exit /b 1

:launch
if "%~1"=="" goto default
"%rscript%" --vanilla %*
goto finish

:default
"%rscript%" --vanilla run.R

:finish
set "result=%errorlevel%"
popd
exit /b %result%
