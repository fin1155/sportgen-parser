@echo off
cd /d "%~dp0"
where Rscript >nul 2>nul
if errorlevel 1 (
  echo Rscript was not found. Install R from https://cran.r-project.org/ and run install_packages.R.
  pause
  exit /b 1
)
Rscript launch_app.R
