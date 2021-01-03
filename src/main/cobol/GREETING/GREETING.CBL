       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GREETING.
      *****************************************************************
      * Trivial program to exercise CobolCheck.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  FILLER             PIC X VALUE 'G'.
               88  MESSAGE-IS-GREETING  VALUE 'G'.
               88  MESSAGE-IS-FAREWELL  VALUE 'F'.
           05  WS-MESSAGE          PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.

       2000-SPEAK.
           IF MESSAGE-IS-GREETING
               MOVE 'Hello, World!' TO WS-MESSAGE
           END-IF  
           IF MESSAGE-IS-FAREWELL
               MOVE 'See you later, alligator!' TO WS-MESSAGE
           END-IF    
           .    

       9999-END.
           .
