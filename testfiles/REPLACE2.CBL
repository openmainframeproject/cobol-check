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
       77  WS-GAMMA                PIC X.
       77  UT-OMEGA                PIC X.
       77  UT-GAMMA                PIC X.
       77  WS-SUBPROGRAM-NAME      PIC X(08).
       77 UT-COMPARE-DEFAULT        PIC X VALUE 'N'.
       77 UT-NORMAL-COMPARE         PIC X VALUE 'N'.
       77 UT-ACTUAL                 PIC X.
       77 UT-EXPECTED               PIC X.
       77 UT-TEST-CASE-COUNT   PIC 9(4) VALUE 0.
       77 UT-TEST-CASE-COUNT  PIC 9(4) VALUE 0.

       REPLACE TRAILING ==ACTUAL== BY ==EXPECTED==.

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
       REPLACE TRAILING ==ALPHA== BY ==GAMMA==.



       3001-DYNAMIC-CALL
           MOVE 'A' TO WS-ALPHA
           MOVE 'Z' TO WS-OMEGA
      *    CALL WS-SUBPROGRAM-NAME
      *        USING WS-ALPHA WS-OMEGA
               MOVE "B" TO WS-ALPHA
               MOVE "Y" TO WS-OMEGA
                 .
       REPLACE LEADING ==:WS:== BY ==UT==.
       3002-DYNAMIC-CALL
           MOVE 'A' TO WS-ALPHA
           MOVE 'Z' TO WS-OMEGA
      *    CALL WS-SUBPROGRAM-NAME
      *        USING WS-ALPHA WS-OMEGA
               MOVE "B" TO :WS:-EXPECTED
               MOVE "Y" TO :WS:-OMEGA
                       .
         REPLACE ==Bruce== BY ==CAITLIN==
                ==PETER== BY ==Phil==.
       3002-DYNAMIC-CALL
               MOVE 'PETER' TO WS-ALPHA
               MOVE 'BRUCE' TO WS-OMEGA

               PERFORM INC-:XXXX:-SKRIV

               REPLACE ==:XXXX:== BY ==FI01==.
       INC-:XXXX:-SKRIV SECTION.
           ADD 1 TO UT-TEST-CASE-COUNT
      *  do some writing
              .
       9999-END.
           .