@echo off
@REM del CC##99*
cobc -x --conf=c:/udvikler/gnucobol/config/ibm.conf CC##99.CBL && CC##99.exe
java -jar bin\cobol-check-0.1.0.jar %*