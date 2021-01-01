       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SAMPLE.
      *****************************************************************
      * TRIVIAL PROGRAM TO EXERCISE ZUTZCPC.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       01  FILLER.
           05  WS-MESSAGE-TYPE          PIC X(08) VALUE SPACES.
           05  WS-MESSAGE               PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'GREETING AND FAREWELL (FAREWELL WILL FAIL)'
           DISPLAY SPACE

           MOVE 'IT RETURNS HELLO, WORLD! AS GREETING'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'GREETING' TO WS-MESSAGE-TYPE
           PERFORM 2000-SPEAK
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-MESSAGE TO UT-ACTUAL
           MOVE 'HELLO, WORLD!'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'IT RETURNS GOODBYE, CRUEL WORLD! AS FAREWELL'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           MOVE 'FAREWELL' TO WS-MESSAGE-TYPE
           PERFORM 2000-SPEAK
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE WS-MESSAGE TO UT-ACTUAL
           MOVE 'GOODBYE, CRUEL WORLD!'
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


           .
       2000-SPEAK.
           IF WS-MESSAGE-TYPE IS EQUAL TO 'GREETING'
               MOVE 'HELLO, WORLD!' TO WS-MESSAGE
           END-IF
           IF WS-MESSAGE-TYPE IS EQUAL TO 'FAREWELL'
               MOVE 'SEE YOU LATER, ALLIGATOR!' TO WS-MESSAGE
           END-IF


           .
       9999-END.

           .
