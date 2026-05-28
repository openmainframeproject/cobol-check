       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-FILTER.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEST-VAR       PIC X(10) VALUE SPACES.                     ABCD12
       01 WS-COUNTER        PIC 9(3)  VALUE 0.
       01 WS-RESULT         PIC 9(3)  VALUE 0.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           EJECT
           
           MOVE "HELLO" TO WS-TEST-VAR
           MOVE 10 TO WS-COUNTER                                        TEST1234
           
           SKIP1
           
           PERFORM CALCULATE-RESULT
           
           EJECT
           
           DISPLAY "Program completed successfully"
           .
           
       PROGRAM-END.
           STOP RUN.
           
       CALCULATE-RESULT.
           SKIP2
           COMPUTE WS-RESULT = WS-COUNTER * 2
           
           EJECT
           
           IF WS-RESULT > 15
               DISPLAY "Result is greater than 15: " WS-RESULT
           ELSE
               DISPLAY "Result is 15 or less: " WS-RESULT
           END-IF
           .