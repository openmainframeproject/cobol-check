       IDENTIFICATION DIVISION.
       PROGRAM-ID.  NUMBERS2.
      *****************************************************************
      * Program to exercise DECIMAL-POINT IS COMMA
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  VALUE-1           PIC Z.ZZ9,99.
           05  VALUE-2           PIC ZZ.ZZZ,ZZ.

       PROCEDURE DIVISION.

       100-ASSIGN.
           MOVE 10 TO VALUE-1
           .

       999-END.
           GOBACK.