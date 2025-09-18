       ID DIVISION.
       PROGRAM-ID.         LV88TEST.
       AUTHOR.             TNP.
       DATE-WRITTEN.       12.05.2025.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       FD  KONTI
             LABEL RECORD IS STANDARD
             RECORDING MODE F
             BLOCK CONTAINS 0 RECORDS
             DATA RECORD IS KONTI-RECORD.
       DATA DIVISION.
       FILE SECTION.
       01  KONTI-RECORD.
         02 WS-88-LEVELS PIC X.
                   88 level-88-value-1  VALUE 'A'.
                   88 level-88-value-2  VALUE 'B'.
                   88 level-88-value-3  VALUE 'C'.
                   88 level-88-value-4  VALUE 'D'.
                   88 level-88-ALL      VALUE 'D',
                                       'C',
                                       'B',
                                       'A'.

            02 WS-88-LEVELS-NUMERIC PIC 99.
            88 WS-88-NUMERIC-LOW
                VALUE  0
                through 9.
            88 WS-88-NUMERIC-MID-ODD
                VALUE 11, 13, 15, 17, 19,
                        21, 23, 25, 27, 29,
                        31, 33, 35, 37, 39,
                        41, 43, 45, 47, 49.
            88 WS-88-NUMERIC-MID-EVEN
                 VALUE 12, 14, 16, 18,
                        22, 24, 26, 28,
                        32, 34, 36, 38,
                        42, 44, 46, 48.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  WORK-FIELDS.
         03  WS-PROG-NAME          PIC X(8) VALUE 'LV88TEST'.
       77  WS-OMEGA                PIC X.
       77  WS-SUBPROGRAM-NAME      PIC X(08).

       01 WS-88-LEVELS PIC X.
            88 level-88-value-1  VALUE 'A'.
            88 level-88-value-2  VALUE 'B'.
            88 level-88-value-3  VALUE 'C'.
            88 level-88-value-4  VALUE 'D'.
            88 level-88-ALL      VALUE 'D',
                                'C',
                                'B',
                                'A'.

       01 WS-88-LEVELS-NUMERIC PIC 99.
            88 WS-88-NUMERIC-LOW
                VALUE  0
                through 9.
            88 WS-88-NUMERIC-MID-ODD
                VALUE 11, 13, 15, 17, 19,
                        21, 23, 25, 27, 29,
                        31, 33, 35, 37, 39,
                        41, 43, 45, 47, 49.
            88 WS-88-NUMERIC-MID-EVEN
                 VALUE 12, 14, 16, 18,
                        22, 24, 26, 28,
                        32, 34, 36, 38,
                        42, 44, 46, 48.


       PROCEDURE DIVISION.
           PERFORM 001-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TEST SUITE:"
           DISPLAY
           'DEMONSTRATE WS 88 STATEMENT are parsed correctly'
           DISPLAY SPACES
           .

       001-INITIALIZE SECTION.
           MOVE 'Z' TO WS-OMEGA
           MOVE 'LV88SUB ' TO WS-SUBPROGRAM-NAME
           .

