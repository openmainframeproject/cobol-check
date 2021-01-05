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
           05  WS-FRIEND           PIC X(10) VALUE SPACES.
           05  WS-GREETING.
               10  FILLER          PIC X(07) VALUE 'Hello, '.
               10  WS-USER-NAME    PIC X(10) VALUE SPACES.
               10  FILLER          PIC X     VALUE '!'.
           05  WS-FAREWELL.
               10  FILLER          PIC X(15) VALUE 'See you later, '.
               10  WS-USER-NAME    PIC X(09) VALUE SPACES.
               10  FILLER          PIC X     VALUE '!'.
       PROCEDURE DIVISION.

           ACCEPT WS-FRIEND.

       2000-SPEAK.
           IF MESSAGE-IS-GREETING
               IF WS-FRIEND EQUALS SPACES
                   MOVE 'World' TO WS-USER-NAME OF WS-GREETING
               ELSE
                   MOVE WS-FRIEND TO WS-USER-NAME OF WS-GREETING
               END-IF
           END-IF  
           IF MESSAGE-IS-FAREWELL
               IF WS-FRIEND EQUALS SPACES
                   MOVE 'alligator!' TO WS-USER-NAME OF WS-FAREWELL
               ELSE
                   MOVE WS-FRIEND TO WS-USER-NAME OF WS-FAREWELL
               END-IF
           END-IF    
           .    

       9999-END.
           .
