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

           EXEC SQL  
              DECLARE NAME-CUR CURSOR FOR  
              SELECT FIRST_NAME, LAST_NAME FROM TEXEM
           END-EXEC.

       LINKAGE SECTION.
       
       COPY COPY002.

       PROCEDURE DIVISION.
       0000-MAIN.

           PERFORM 1000-SELECT
           PERFORM 2000-OPEN-CURSOR.
           PERFORM 3000-FETCH-SQL
              UNTIL SQLCODE = 100.

           PERFORM 4000-CLOSE-CURSOR. 
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
           

       2000-OPEN-CURSOR.
           EXEC SQL
              OPEN NAME-CUR
           END-EXEC.
           EVALUATE SQLCODE
             WHEN ZEROES 
                MOVE 'CURS OPENED' TO WS-FIELD-1
           
             WHEN OTHER
                MOVE 'THIS IS BAD' TO WS-FIELD-1
           END-EVALUATE.

       3000-FETCH-SQL.
           EXEC SQL
              CLOSE NAME-CUR 
           END-EXEC.
           EVALUATE SQLCODE
             WHEN ZEROES 
                MOVE 'GOOD JOB' TO WS-FIELD-1
           
             WHEN -100
                MOVE 'NOT FOUND' TO WS-FIELD-1
           
             WHEN OTHER
                MOVE 'THIS IS BAD' TO WS-FIELD-1
           END-EVALUATE.

       4000-CLOSE-CURSOR.
           EXEC SQL
              CLOSE NAME-CUR 
           END-EXEC.
           EVALUATE SQLCODE
             WHEN ZEROES 
                MOVE 'CURS CLOSED' TO WS-FIELD-1
           
             WHEN OTHER
                MOVE 'THIS IS BAD' TO WS-FIELD-1
           END-EVALUATE.
