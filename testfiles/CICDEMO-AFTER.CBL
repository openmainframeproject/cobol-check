      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      10 JAN 2015
      * PURPOSE:   DEMONSTRATE MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSDEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01  WS-RECORD.
           05  WS-KEY        PIC X(5).
           05  WS-FIELD-2    PIC X(5).
           05  WS-FIELD-3    PIC X(5).
           05  WS-FIELD-4    PIC X(5).

       LINKAGE SECTION.
           COPY DFHEIBLK.
       01  DFHCOMMAREA PIC X.

       PROCEDURE DIVISION USING DFHEIBLK, DFHCOMMAREA.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'DEMONSTRATE CICS COMMAND MOCKS'
           DISPLAY SPACE


           MOVE 'Mock behavior of EXEC CICS READ DATASET'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'READ DATASET("MYFILE") RIDFLD("AAAAA") INTO(WS-RECORD)'
            TO UT-MOCK-FIND-CICS-KEYWORDS
           SET UT-FIND-CICS-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           PERFORM 0100-READ-DATASET
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-FIELD-3 TO UT-ACTUAL
           MOVE 'CCCCC'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE EIBRESP TO UT-ACTUAL-NUMERIC
           MOVE ZERO TO UT-EXPECTED-NUMERIC
           SET UT-COMPARE-NUMERIC TO TRUE
           PERFORM UT-ASSERT-EQUAL
           SET UT-FIND-CICS-MOCK TO TRUE
           MOVE 'READ DATASET("MYFILE") RIDFLD("AAAAA") INTO(WS-RECORD)'
            TO UT-MOCK-FIND-CICS-KEYWORDS
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
           MOVE 0001
            TO UT-EXPECTED-ACCESSES
           MOVE UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
            TO UT-ACTUAL-ACCESSES
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-VERIFY-EXACT TO TRUE
           PERFORM UT-ASSERT-ACCESSES


           PERFORM UT-AFTER
           MOVE 'Mock behavior of EXEC CICS WRITE DATASET'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'WRITE DATASET("YOURFILE") RIDFLD("AAAAA") FROM(WS-RECOR
      -    'D)'
            TO UT-MOCK-FIND-CICS-KEYWORDS
           SET UT-FIND-CICS-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           PERFORM 0200-WRITE-DATASET
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE EIBRESP TO UT-ACTUAL-NUMERIC
           MOVE ZERO TO UT-EXPECTED-NUMERIC
           SET UT-COMPARE-NUMERIC TO TRUE
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


           .
       0100-READ-DATASET.

      *    EXEC CICS READ
      *         DATASET('MYFILE')
      *         RIDFLD('AAAAA')
      *         INTO(WS-RECORD)
      *    END-EXEC
           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
           MOVE ZERO TO EIBRESP
           MOVE ZERO TO EIBRESP2
               MOVE "AAAAABBBBBCCCCCDDDDDEEEEE" TO WS-RECORD
      *    END-EXEC


           .
       0200-WRITE-DATASET.

      *    EXEC CICS WRITE
      *         DATASET('YOURFILE')
      *         RIDFLD('AAAAA')
      *         FROM(WS-RECORD)
      *    END-EXEC
           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
           MOVE ZERO TO EIBRESP
           MOVE ZERO TO EIBRESP2
      *    END-EXEC


           .
       9999-END.

           .
