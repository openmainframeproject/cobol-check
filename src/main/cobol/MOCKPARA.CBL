       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MOCKPARA.
      /****************************************************************
      * Program to exercise mock of paragraphs
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
                03 NUMERIC-FIELDS.
                  05  NUMERIC-1            PIC s9(4) comp.
                  05  NUMERIC-2            PIC s9(4) comp.
                  05  NUMERIC-3            PIC s9(9) comp.
                03 TEXT-FIELDS.
                  05  TEXT-1               PIC X(12).
                  05  TEXT-2               PIC X(12).
                  05  TEXT-3               PIC X(12).

       PROCEDURE DIVISION.

      *-----------------------------------------------------------------
       100-COMPUTE-VALUE SECTION.
      *-----------------------------------------------------------------
           PERFORM 300-FETCH-CURRENT-NUMERIC

           COMPUTE NUMERIC-3 = NUMERIC-1 - NUMERIC-2
           end-compute
           .

       201-DUMMY SECTION.
            CONTINUE
            .

       210-MOVE-TEXT-TO-2 SECTION.
           MOVE TEXT-1 to TEXT-2
           .

       220-MOVE-TEXT-TO-3 SECTION.
           MOVE TEXT-1 to TEXT-2
           .

       300-FETCH-CURRENT-NUMERIC.
           move 500 to NUMERIC-2
           .
       310-SKIP-CURRENT-NUMERIC.
           CONTINUE
           .