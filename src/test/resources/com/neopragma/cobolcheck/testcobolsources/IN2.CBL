      * Test Cobol source: Main program with no WORKING-STORAGE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  W-GROUP-1.
           05  W-FIELD-1 PIC X(03) VALUE "FOO".
           05  FILLER    PIC X(01) VALUE "-".
           05  W-FIELD-2 PIC X(03) VALUE "XXX".
       PROCEDURE DIVISION.
       100-APPEND-TEXT.
           MOVE "ZZZ" TO W-FIELD-2
           DISPLAY W-GROUP-1 UPON CONSOLE.
           .
       200-NOT-UNDER-TEST.
           DISPLAY "Should not see this." UPON CONSOLE
           .
