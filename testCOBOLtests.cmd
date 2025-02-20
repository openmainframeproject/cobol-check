@echo off
setlocal enabledelayedexpansion

if "%1"=="" (
echo Usage: runCOBOLtest.cmd [jar-version] like ./runCOBOLtest.cmd 0.2.15
exit /b 1
)

for %%f in (src\main\cobol\*) do (
java -jar .\bin\cobol-check-%1.jar -p %%~nf
rem print the file name and pause for user input
echo 'We ran: ' %%~nf ', Press any key to continue or Ctrl+C to exit'
pause
)

endlocal
