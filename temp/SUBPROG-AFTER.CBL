      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      21 JAN 2015
      * PURPOSE:   DEMONSTRATE UNIT TESTING A CALLED SUBPROGRAM
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
            COPY ZUTZCWS     .
       LINKAGE SECTION.
       01  LS-ARGUMENTS.
           05  LS-ARG-1      PIC X(80).
           05  LS-ARG-2      PIC X(80).
       PROCEDURE DIVISION USING BY REFERENCE LS-ARGUMENTS.
           PERFORM UT-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'DEMONSTRATE UNIT TESTING A CALLED SUBPROGRAM'
           DISPLAY SPACE


           MOVE 'IT RETURNS VALUE A TO THE CALLER'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           PERFORM 1000-RETURN-VALUE-A
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE LS-ARG-2 TO UT-ACTUAL
           MOVE 'This is value A'
                       TO UT-EXPECTED
           SET UT-COMPARE-DEFAULT TO TRUE
           PERFORM UT-ASSERT-EQUAL

           PERFORM UT-AFTER
           MOVE 'IT RETURNS VALUE B TO THE CALLER'
               TO UT-TEST-CASE-NAME
           PERFORM UT-BEFORE
           PERFORM 1000-RETURN-VALUE-A
           ADD 1 TO UT-TEST-CASE-COUNT
           SET UT-NORMAL-COMPARE TO TRUE
           MOVE LS-ARG-2 TO UT-ACTUAL
           MOVE 'This is value A'
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

           DISPLAY 'Enter SUBPROG'
           MOVE 'Production run' TO LS-ARG-1
           GOBACK

           .
       1000-RETURN-VALUE-A.
           MOVE 'This is value A' TO LS-ARG-2

           .
       1000-RETURN-VALUE-B.
           MOVE 'This is value B' TO LS-ARG-2

           .
       9999-END.

           .
