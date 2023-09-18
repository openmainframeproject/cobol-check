@echo off
:: Windows - compile and execute a test Cobol program
::
::  Assumptions:
::
:: GnuCOBOL 2.2 or later is installed and on the path.
:: Its executable or alias or symlink is named "cobc".

cobc -xj %* && %~n1