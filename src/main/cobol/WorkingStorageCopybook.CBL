       IDENTIFICATION DIVISION.
       PROGRAM-ID. WS-COPYBOOK.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY  WS_Copybook.

       PROCEDURE DIVISION.
           DISPLAY "Checking if the number is Even or Odd: " WS-NUMBER.
           PERFORM CLASSIFY.
           DISPLAY "The number is " WS-RESULT.
           STOP RUN.

       CLASSIFY.
           DIVIDE WS-NUMBER BY 2 GIVING WS-NUMBER REMAINDER WS-REMAIN.
           IF WS-REMAIN = 0
               MOVE "Even" TO WS-RESULT
           ELSE
               MOVE "Odd" TO WS-RESULT
           END-IF.

