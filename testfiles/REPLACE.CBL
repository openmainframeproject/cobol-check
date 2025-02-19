      **********************************************************************
      * AUTHOR:    T. N. KRAMER
      * DATE:      14 FEB 2025
      * PURPOSE:   DEMONSTRATE REPLACE STATEMENT
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPLDEMO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-OMEGA                PIC X.
       77  WS-SUBPROGRAM-NAME      PIC X(08).
       PROCEDURE DIVISION.

           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-OMEGA TO UT-ACTUAL
           MOVE 'Y' TO UT-EXPECTED
           .
       3000-DYNAMIC-CALL.
           MOVE 'A' TO WS-ALPHA
           MOVE 'Z' TO WS-OMEGA
      *    CALL WS-SUBPROGRAM-NAME
      *        USING WS-ALPHA WS-OMEGA
               MOVE "B" TO WS-ALPHA
               MOVE "Y" TO WS-OMEGA
           .
       9999-END.

           .
