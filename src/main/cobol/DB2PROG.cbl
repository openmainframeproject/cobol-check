       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DB2PROG.
      *****************************************************************
      * Program to exercise DB2 instructions
      ***************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  FILLER.
           05  WS-FIELD-1           PIC X(80).
           05  ws-Field-2           PIC X(80).
       
       
       EXEC SQL INCLUDE SQLCA  END-EXEC.
       EXEC SQL INCLUDE TEXEM  END-EXEC.

       PROCEDURE DIVISION.
       0000-MAIN.

           PERFORM 1000-SELECT
           GOBACK.

       1000-SELECT.
           EXEC SQL
                      SELECT FIRST_NAME,
                         LAST_NAME,
                         TMS_CREA
                      INTO
                         :FIRST-NAME,
                         :LAST-NAME,
                         :TMS-CREA
                      FROM TEXEM
           END-EXEC.

           EVALUATE SQLCODE
             WHEN ZEROES 
                MOVE 'GOOD JOB' TO WS-FIELD-1
           
             WHEN -100
                MOVE 'NOT FOUND' TO WS-FIELD-1
           
             WHEN OTHER
                MOVE 'THIS IS BAD' TO WS-FIELD-1
           END-EVALUATE.