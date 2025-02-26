       ID DIVISION.
       PROGRAM-ID.         BIPM012.
       AUTHOR.             (TNP).
       DATE-WRITTEN.       20.01.2025.
      *--------------+------+-------------------------------------------
      *
      *     FUNKTION UNITTEST VALIDATION - REPLACE
      *
      *--------------+------+-------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  WORK-FIELDS-1.
          03 MOVE-IDX               PIC S9(4) COMP.
          03 WS-BALANCE-X           PIC S9(11)V9(2) VALUE 0 COMP.
          03 WS-NUMBER-OF-MOVE      PIC S9(07)    COMP-3.

          03 WS-DATE-BEFORE    PIC X(10).
          03 WS-DATE-AFTER     PIC X(10).
      *-----------------------------------------------------------------
       01  BIPM012-PARM.
            07 INPUT-DATA.
               10 USERNO                       PIC 9(03).
            07 OUTPUT-DATA.
               10 MAX-HEIGHT        usage COMP-3 PIC S9(5).
               10 MAX-LENGTH              PIC S9(07) usage COMP-3.

      *-----------------------------------------------------------------

            REPLACE ==:BDSIXXX:== BY ==BIPM012==.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
           MOVE ZERO TO MAX-LENGTH          IN :BDSIXXX:-PARM
           .
       100-MOVE-DATA SECTION.
      *-----------------------------------------------------------------
           MOVE WS-DATE-BEFORE TO WS-DATE-AFTER
          .