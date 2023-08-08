       IDENTIFICATION DIVISION.
       PROGRAM-ID.  RETURNCODE.
      /****************************************************************
      * Program to exercise different mock statements and edge cases.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  VALUE-1           PIC X(80).
           05  VALUE-2           PIC X(80).

       PROCEDURE DIVISION.

       000-MAKE-CALL.
           MOVE "arg1" to VALUE-1
           MOVE "arg2" to VALUE-2
           CALL 'PROG1'
           .