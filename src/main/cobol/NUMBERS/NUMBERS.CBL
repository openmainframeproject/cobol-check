       IDENTIFICATION DIVISION.
       PROGRAM-ID.  NUMBERS.
      *****************************************************************
      * Program to exercise symbolic relations like ">=" and "!=".
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  WS-FIELD-1           PIC S9(5)V99 COMP-3.
           05  WS-FIELD-2           PIC S9(5)V99 COMP-3.
       PROCEDURE DIVISION.
           GOBACK.
