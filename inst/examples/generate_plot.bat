@echo off
cd /d "%~dp0"
Rscript "generate_plot.R"
echo.
pause
