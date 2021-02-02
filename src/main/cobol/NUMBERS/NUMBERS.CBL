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
           05  WS-FIELD-2           PIC S9(5)V99 comp-3.
           05  ws-field-3           pic s9(16) comp.
           05  ws-field-4           pic s9(16) comp-4.
       PROCEDURE DIVISION.
           GOBACK.
