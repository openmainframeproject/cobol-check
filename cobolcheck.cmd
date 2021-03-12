echo off

echo "======> running jar with args %*"

java -jar bin\cobol-check-0.0.5.jar %*
if errorlevel 8 goto end
:loop
cobc -x CC##99.CBL && CC##99.exe
shift
set pname=%2
if "%pname%"=="" goto end
if "%pname:~0,1%"=="-" goto end
goto loop
:end

