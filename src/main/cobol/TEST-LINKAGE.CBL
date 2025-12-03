      *****************************************************************
      * TEST PROGRAM FOR LINKAGE SECTION, PROCEDURE DIVISION USING
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-LINKAGE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER          PIC 9(04)  VALUE 0.
       01 WS-WORK-AREA        PIC X(20).
       LINKAGE SECTION.
       01 L-IOPCB.
          05 L-DBD-NAME       PIC X(08).
          05 L-SEGMENT-LEVEL  PIC X(02).
          05 L-STATUS-CODE    PIC X(02).
       01 IMSVAR01            PIC X(100).
       01 IMSVAR02            PIC X(200).
       01 IMSVAR03            PIC X(300).
       01 IMSVAR04            PIC X(400).
      *****************************************************************
      * P R O C E D U R E D I V I S I O N *
      *****************************************************************
       PROCEDURE DIVISION USING L-IOPCB
                                IMSVAR01
                                IMSVAR02
                                IMSVAR03
                                IMSVAR04.
       0000-MAIN.
           ADD 1 TO WS-COUNTER
           MOVE 'MAIN ENTRY' TO WS-WORK-AREA.
           ENTRY 'DLITCBL' USING L-IOPCB
              IMSVAR01
              IMSVAR02
              IMSVAR03
              IMSVAR04.
       0100-ENTRY-POINT.
           ADD 10 TO WS-COUNTER
           MOVE 'ENTRY POINT' TO WS-WORK-AREA.