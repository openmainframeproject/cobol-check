       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FILECOPY.
      *****************************************************************
      * Copy one sequential file to another.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INFILE"
               ORGANIZATION SEQUENTIAL
               ACCESS MODE SEQUENTIAL
               FILE STATUS INPUT-FILE-STATUS.
           SELECT
               OUTPUT-FILE
               ASSIGN TO "OUTFILE"
               ORGANIZATION SEQUENTIAL
               ACCESS MODE SEQUENTIAL
               FILE STATUS IS
                   OUTPUT-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
      * This defines our input file
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 40 CHARACTERS
           RECORDING MODE F
           DATA RECORD IS INPUT-RECORD.
      * Layout of an input record
       01  INPUT-RECORD.
           05  IN-FIELD-1         PIC X(10).
           05  FILLER             PIC X(20).
           05  IN-FIELD-2.
               COPY FSTEST.
      * This defines our output file
       FD  OUTPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 40 CHARACTERS
           RECORDING MODE F
           DATA RECORD IS OUTPUT-RECORD.
       01  OUTPUT-RECORD.
           COPY
              OUTREC.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  INPUT-FILE-STATUS  PIC XX.
               88  INPUT-OK       VALUE '00'.
               88  END-OF-FILE    VALUE '10'.
               88  FILE-NOT-FOUND VALUE '35'.
           05  OUTPUT-FILE-STATUS PIC XX.
               88  OUTPUT-OK      VALUE '00'.
           05  WS-COUNT           PIC S9(5) COMP-3.
           05  WS-COUNT-FORMATTED PIC ZZ,ZZ9.
           05  WS-ERROR-MESSAGE   PIC X(60).
       PROCEDURE DIVISION.
           PERFORM 1000-INITIALIZE
           PERFORM 5000-PROCESS
           PERFORM 8000-HOUSEKEEPING
           GOBACK
           .
       1000-INITIALIZE.
           OPEN INPUT INPUT-FILE
           EVALUATE TRUE
               WHEN INPUT-OK
                   CONTINUE
               WHEN FILE-NOT-FOUND
                   MOVE 'Input file not found'
                     TO WS-ERROR-MESSAGE
               WHEN OTHER
                   STRING "Unexpected input file status on open "
                       DELIMITED BY SIZE
                       INPUT-FILE-STATUS
                       DELIMITED BY SIZE
                     INTO WS-ERROR-MESSAGE
                   PERFORM 9999-ABORT
           END-EVALUATE

           OPEN OUTPUT OUTPUT-FILE
           EVALUATE TRUE
               WHEN OUTPUT-OK
                   CONTINUE
               WHEN OTHER
                   STRING "Unexpected output file status on open "
                       DELIMITED BY SIZE
                       OUTPUT-FILE-STATUS
                       DELIMITED BY SIZE
                     INTO WS-ERROR-MESSAGE
                   PERFORM 9999-ABORT
           END-EVALUATE

           MOVE ZERO TO WS-COUNT
           .

       5000-PROCESS.
           READ INPUT-FILE
           PERFORM WITH TEST BEFORE
                   UNTIL END-OF-FILE
               PERFORM 5200-PREPARE-OUTPUT-RECORD
               PERFORM 5400-WRITE-OUTPUT-RECORD
               READ INPUT-FILE
           END-PERFORM
           .

       5200-PREPARE-OUTPUT-RECORD.
           MOVE SPACES TO OUTPUT-RECORD
           MOVE IN-FIELD-1 TO OUT-FIELD-1
           MOVE IN-FIELD-2 TO OUT-FIELD-2
           MOVE "Good" TO OUT-FIELD-3
           .

       5400-WRITE-OUTPUT-RECORD.
           WRITE OUTPUT-RECORD
           IF NOT OUTPUT-OK
               STRING "Unexpected output file status on write "
                   DELIMITED BY SIZE
                   OUTPUT-FILE-STATUS
                   DELIMITED BY SIZE
                 INTO WS-ERROR-MESSAGE
               PERFORM 9999-ABORT
           END-IF
           ADD 1 TO WS-COUNT
           .

       8000-HOUSEKEEPING.
           CLOSE OUTPUT-FILE.
           CLOSE INPUT-FILE
           MOVE WS-COUNT TO WS-COUNT-FORMATTED
           DISPLAY "Records processed: " WS-COUNT-FORMATTED
           .
       9999-ABORT.
           DISPLAY WS-ERROR-MESSAGE
           GOBACK
           .
