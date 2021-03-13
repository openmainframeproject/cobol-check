echo off
del CC##99*
java -jar bin\cobol-check-0.0.5.jar %*
if errorlevel 8 goto end
cobc -x CC##99.CBL && CC##99.exe
:end