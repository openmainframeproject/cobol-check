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
           SELECT LINEFILE ASSIGN TO UT-S-LINEFILE
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS STATUS-LINEFILE.

       DATA DIVISION.
       FILE SECTION.
       FD     LINEFILE
              BLOCK CONTAINS 0 RECORDS
              RECORD CONTAINS 160 CHARACTERS
              LABEL RECORD STANDARD
              RECORDING F
              DATA RECORD   LINIE1.
       01  LINIE1.
         02 FD-88-LEVELS PIC X.
                   88 FD-88-value-1  VALUE 'W'.
                   88 FD-88-value-2  VALUE 'X'.
                   88 FD-88-value-3  VALUE 'Y'.
                   88 FD-88-value-4  VALUE 'Z'.
                   88 FD-88-ALL      VALUE 'Z',
                                       'Y',
                                       'X',
                                       'W'.

         02 FD-88-LEVELS-NUMERIC PIC 99.
            88 FD-88-NUMERIC-LOW
                VALUE  0
                through 9.
            88 FD-88-NUMERIC-MEDIUM
                               VALUE  10
                              through 49.
            88 FD-88-NUMERIC-HIGH
                               VALUE 50
                             through 99.
            88 FD-88-NUMERIC-MED-ODD
                VALUE 11, 13, 15, 17, 19,
                        21, 23, 25, 27, 29,
                        31, 33, 35, 37, 39,
                        41, 43, 45, 47, 49.
            88 FD-88-NUMERIC-MED-EVEN
                 VALUE 12, 14, 16, 18,
                        22, 24, 26, 28,
                        32, 34, 36, 38,
                        42, 44, 46, 48.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01     STATUS-LINEFILE      PIC 99 VALUE ZERO.
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
            88 SW-88-NUMERIC-LOW
                VALUE  0
                through 9.
            88 SW-88-NUMERIC-MEDIUM
                            VALUE  10
                            through 49.
            88 SW-88-NUMERIC-HIGH
                            VALUE 50
                            through 99.
            88 SW-88-NUMERIC-MED-ODD
                VALUE 11, 13, 15, 17, 19,
                        21, 23, 25, 27, 29,
                        31, 33, 35, 37, 39,
                        41, 43, 45, 47, 49.
            88 SW-88-NUMERIC-MED-EVEN
                 VALUE 12, 14, 16, 18,
                        22, 24, 26, 28,
                        32, 34, 36, 38,
                        42, 44, 46, 48.


       PROCEDURE DIVISION.
           PERFORM 001-INITIALIZE

           DISPLAY SPACE
           DISPLAY "TESTING 88-LEVELS IN FD and WORKING-STORAGE"
           DISPLAY
           'DEMONSTRATE WS 88 STATEMENT are parsed correctly'
           DISPLAY SPACES
           .

       001-INITIALIZE SECTION.
           MOVE 'Z' TO WS-OMEGA
           MOVE 'LV88SUB ' TO WS-SUBPROGRAM-NAME
           OPEN OUTPUT LINEFILE
           .

