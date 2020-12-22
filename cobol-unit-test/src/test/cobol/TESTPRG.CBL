      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      30 JUN 2015
      * PURPOSE:   DEMONSTRATE PARAGRAPH MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOCKDEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-INPUT-FILE-STATUS.

       SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(200).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(200).

       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .

       01  WS-FILE-STATUS.
           05  WS-INPUT-FILE-STATUS   PIC X(02).
           05  WS-OUTPUT-FILE-STATUS  PIC X(02).
           05  WS-OTHER-FILE-STATUS   PIC X(02).

       01  WS-RECORD-AREAS.
           05  WS-INPUT-RECORD        PIC X(200).
           05  WS-OUTPUT-RECORD       PIC X(200).

       01  WS-TEST-AREAS.
           05  WS-FIELD-A             PIC X(04) VALUE 'ORIG'.
           05  WS-FIELD-B             PIC X(04) VALUE 'ORIG'.

       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'DEMONSTRATE FILE MOCKS'
           DISPLAY SPACE



           MOVE 'IT MOCKS PARAGRAPH 2000-PARA-B'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE '2000-PARA-B' TO UT-MOCK-FIND-PARA-NAME
           SET UT-FIND-PARA-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           MOVE  ZERO  TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
           PERFORM 2000-PARA-B
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-FIELD-A TO UT-ACTUAL
           MOVE 'B2'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-FIELD-B TO UT-ACTUAL
           MOVE 'B3'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           SET UT-FIND-PARA-MOCK TO TRUE
           MOVE '2000-PARA-B'
            TO UT-MOCK-FIND-PARA-NAME
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
           MOVE 0001
            TO UT-EXPECTED-ACCESSES
           MOVE UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
            TO UT-ACTUAL-ACCESSES
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-VERIFY-EXACT TO TRUE
           PERFORM UT-ASSERT-ACCESSES
           END-IF

           PERFORM UT-AFTER
           MOVE 'IT MOCKS PARAGRAPH 1000-PARA-A'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE '1000-PARA-A' TO UT-MOCK-FIND-PARA-NAME
           SET UT-FIND-PARA-MOCK TO TRUE
           PERFORM UT-SET-MOCK
           MOVE  ZERO  TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
           PERFORM 1000-PARA-A
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-FIELD-A TO UT-ACTUAL
           MOVE 'A2'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-FIELD-B TO UT-ACTUAL
           MOVE 'A3'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL
           SET UT-FIND-PARA-MOCK TO TRUE
           MOVE '1000-PARA-A'
            TO UT-MOCK-FIND-PARA-NAME
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
           MOVE 0001
            TO UT-EXPECTED-ACCESSES
           MOVE UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
            TO UT-ACTUAL-ACCESSES
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-VERIFY-NO-MORE-THAN TO TRUE
           PERFORM UT-ASSERT-ACCESSES
           END-IF
           PERFORM UT-AFTER
            COPY ZUTZCPD     .
           .
       UT-BEFORE.
               MOVE SPACES TO INPUT-RECORD
               MOVE 'ORIG' TO WS-FIELD-A
               MOVE 'ORIG' TO WS-FIELD-B

           .
       UT-AFTER.

           .
       UT-INITIALIZE.
           MOVE SPACES TO UT-FILE-INFORMATION

           .
       UT-END.



           .
       0100-OPEN-INPUT.
           OPEN INPUT INPUT-FILE


           .
       0200-READ-INPUT-FILE.
           READ INPUT-FILE


           .
       1000-PARA-A.
      *    MOVE 'A1' TO WS-FIELD-A
      *
           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
               MOVE "A2" TO WS-FIELD-A
               MOVE "A3" TO WS-FIELD-B
      *

           .
       2000-PARA-B.
      *    MOVE 'B1' TO WS-FIELD-B
      *
           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)
               MOVE "B2" TO WS-FIELD-A
               MOVE "B3" TO WS-FIELD-B
      *


           .
       9999-END.

           .
