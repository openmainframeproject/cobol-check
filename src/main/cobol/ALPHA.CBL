       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ALPHA.
      *****************************************************************
      * Program to exercise EXPECT statements.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  WS-FIELD-1           PIC X(80).
           05  ws-Field-2           PIC X(80).
           05  ws-table-1.
               10  ws-table-1-entry occurs 5
                       indexed by table-1-ix.
                   15  ws-thing-1   pic x(5).
                   15  ws-thing-2   pic x(5).
           05  ws-table-2.
               10  ws-table-2-entry occurs 5
                       indexed by table-2-ix.
                   15  ws-thing-3   pic x(5).
                   15  ws-thing-4   pic x(5).
           05  ws-display-numeric   pic 999.
       PROCEDURE DIVISION.
           GOBACK.
      