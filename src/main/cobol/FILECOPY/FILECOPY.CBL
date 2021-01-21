       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FILECOPY.
      *****************************************************************
      * Copy one sequential file to another.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INFILE
               ORGANIZATION SEQUENTIAL
               ACCESS MODE SEQUENTIAL
               FILE STATUS INPUT-FILE-STATUS.
           SELECT
               OUTPUT-FILE
               ASSIGN TO OUTFILE
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
       01  INPUT-RECORD           PIC X(40).
      * This defines our output file
       FD  OUTPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 40 CHARACTERS
           RECORDING MODE F
           DATA RECORD IS OUTPUT-RECORD.
       01  OUTPUT-RECORD           PIC X(40).
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  INPUT-FILE-STATUS  PIC XX.
           05  OUTPUT-FILE-STATUS PIC XX.
           05  WS-COUNT           PIC S9(5) COMP-3.
       PROCEDURE DIVISION.


       100-Dont-Just-Stand-There.
           move 5 to ws-count
           .

       9999-END.
           .
