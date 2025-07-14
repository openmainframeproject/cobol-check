@echo off
rem Windows - compile and execute a test Cobol program
rem
rem  Assumptions:
rem
rem GnuCOBOL 3.+ is installed and on the path. Its executable or alias or symlink is named "cobc".
%~d1
cd %~p1
cobc -xj %*