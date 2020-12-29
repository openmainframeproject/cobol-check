      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      07 AUG 2014
      * PURPOSE:   SAMPLE PROGRAM THAT WORKS WITH DATES.
      *
      * USAGE:     INVDATE
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVDATE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
           COPY DATETIME.
       01  WS-NEXT-INVOICE-DATE PIC X(8).
       01  WS-QUOTIENT          PIC S9(4) COMP.
       01  WS-REMAINDER         PIC S9(4) COMP.
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'UNIT TESTS FOR INVDATE.CBL'
           DISPLAY SPACE



           MOVE "IT DETERMINES THE NEXT INVOICE DATE IN A 30-DAY MONTH"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE "20150405" TO WS-CURRENT-DATE
           PERFORM 2000-NEXT-INVOICE-DATE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-NEXT-INVOICE-DATE TO UT-ACTUAL
           MOVE "20150430"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "IT DETERMINES THE NEXT INVOICE DATE IN A 31-DAY MONTH"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE "20150705" TO WS-CURRENT-DATE
           PERFORM 2000-NEXT-INVOICE-DATE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-NEXT-INVOICE-DATE TO UT-ACTUAL
           MOVE "20150731"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "IT DETERMINES THE NEXT INVOICE DATE IN FEB, NON LEAP"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE "20150205" TO WS-CURRENT-DATE
           PERFORM 2000-NEXT-INVOICE-DATE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-NEXT-INVOICE-DATE TO UT-ACTUAL
           MOVE "20150228"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE "IT DETERMINES THE NEXT INVOICE DATE IN FEB, LEAP"
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE "20160205" TO WS-CURRENT-DATE
           PERFORM 2000-NEXT-INVOICE-DATE
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-NEXT-INVOICE-DATE TO UT-ACTUAL
           MOVE "20160229"
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.
           SET DO-NOT-SET-CURRENT-DATE TO TRUE

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACES TO UT-FILE-INFORMATION

           .
       UT-END.


           .
       0000-MAIN.

           PERFORM 0500-INITIALIZE
           PERFORM 1000-PROCESS-INVOICES
           GOBACK


           .
       0500-INITIALIZE.


           .
       1000-PROCESS-INVOICES.
           PERFORM 2000-NEXT-INVOICE-DATE


           .
       2000-NEXT-INVOICE-DATE.
           EVALUATE TRUE
               WHEN FEBRUARY
                    PERFORM 2100-HANDLE-FEBRUARY
               WHEN 30-DAY-MONTH
                    MOVE 30 TO WS-CURRENT-DAY
               WHEN OTHER
                    MOVE 31 TO WS-CURRENT-DAY
           END-EVALUATE
           MOVE WS-CURRENT-DATE TO WS-NEXT-INVOICE-DATE


           .
       2100-HANDLE-FEBRUARY.
           DIVIDE 4 INTO WS-CURRENT-YEAR
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAINDER
           END-DIVIDE
           IF  WS-REMAINDER EQUAL ZERO
               MOVE 29 TO WS-CURRENT-DAY
           ELSE
               MOVE 28 TO WS-CURRENT-DAY
           END-IF


           .
       9999-END.

           .
