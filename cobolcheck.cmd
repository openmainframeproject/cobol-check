echo off
java -jar bin\cobol-check-0.0.4.jar %*
:loop
cobc -x %2T.CBL && %2T.exe 
shift
set pname=%2
if "%pname%"=="" goto end
if "%pname:~0,1%"=="-" goto end 
goto loop     
:end
