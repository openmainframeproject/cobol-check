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
           05  ws-field-1           PIC S9(11)V9(07) COMP-3.
           05  WS-FIELD-2           PIC S9(11)V9(07) comp-3.
           05  ws-field-3           pic s9(16) comp.
           05  ws-field-4           pic s9(16) comp-4.
           05  ws-display-field     pic s9(5)v99.
           05  ws-num-occ           pic 9(04) occurs 10.
       77   CHAR-CT                      PIC S9(3) COMP.   
       PROCEDURE DIVISION.
           GOBACK.
      