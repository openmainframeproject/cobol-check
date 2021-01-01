      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      25 JUN 2015
      * PURPOSE:   DEMONSTRATE MOCKING OF CALL STATEMENTS.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLDEMO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       77  WS-ALPHA                PIC X.
       77  WS-OMEGA                PIC X.
       77  WS-SUBPROGRAM-NAME      PIC X(08).
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'DEMONSTRATE CALL STATEMENT MOCKS'
           DISPLAY SPACE


           MOVE 'Mock behavior of basic CALL statement'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'NOTREALNAME USING WS-ALPHA WS-OMEGA'
            TO UT-MOCK-FIND-CALL-TOKENS
           SET UT-FIND-CALL-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           PERFORM 1000-BASIC-CALL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-ALPHA TO UT-ACTUAL
           MOVE 'B'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-OMEGA TO UT-ACTUAL
           MOVE 'Y'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           SET UT-FIND-CICS-MOCK TO TRUE
           MOVE '"NOTREALNAME" USING WS-ALPHA WS-OMEGA'
            TO UT-MOCK-FIND-CALL-TOKENS
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
           MOVE 0001
            TO UT-EXPECTED-ACCESSES
           MOVE UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
            TO UT-ACTUAL-ACCESSES
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-VERIFY-EXACT TO TRUE
           PERFORM UT-ASSERT-ACCESSES

           PERFORM UT-AFTER
           MOVE 'Mock behavior of classic CALL statement'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'NOTREALNAME USING WS-ALPHA WS-OMEGA'
            TO UT-MOCK-FIND-CALL-TOKENS
           SET UT-FIND-CALL-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           PERFORM 2000-CLASSIC-CALL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-ALPHA TO UT-ACTUAL
           MOVE 'B'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-OMEGA TO UT-ACTUAL
           MOVE 'Y'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'Mock CALL to dynamic subprogram'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'WS-SUBPROGRAM-NAME USING WS-ALPHA WS-OMEGA'
            TO UT-MOCK-FIND-CALL-TOKENS
           SET UT-FIND-CALL-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           PERFORM 3000-DYNAMIC-CALL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-ALPHA TO UT-ACTUAL
           MOVE 'B'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-OMEGA TO UT-ACTUAL
           MOVE 'Y'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL



           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACES TO UT-FILE-INFORMATION

           .
       UT-END.

           GOBACK


           .
       1000-BASIC-CALL.
           MOVE 'A' TO WS-ALPHA
           MOVE 'Z' TO WS-OMEGA
      *    CALL 'NOTREALNAME'
      *        USING WS-ALPHA WS-OMEGA
      *    END-CALL
               MOVE "B" TO WS-ALPHA
               MOVE "Y" TO WS-OMEGA


           .
       2000-CLASSIC-CALL.
           MOVE 'A' TO WS-ALPHA
           MOVE 'Z' TO WS-OMEGA
      *    CALL 'NOTREALNAME'
      *        USING WS-ALPHA WS-OMEGA
               MOVE "B" TO WS-ALPHA
               MOVE "Y" TO WS-OMEGA


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
