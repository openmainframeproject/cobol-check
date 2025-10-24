       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GREETING.
      *****************************************************************
      * Trivial program to exercise CobolCheck.
      * With regards to REPLACE statement.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           REPLACE  ==:PROGRAM:==  BY ==REPDEMO3==
                    ==:REQUEST:==  BY ==UPDATE-MY-ADVANCED-REQUEST==
                    ==:RESPONSE:== BY ==UPDATE-MY-ADVANCED-RESPONSE==.

123456 01  FILLER.
           05  WS-COUNT           PIC S9(5) COMP-3.
           05  FILLER             PIC X VALUE 'G'.
               88  MESSAGE-IS-GREETING  VALUE 'G'.
               88  MESSAGE-IS-FAREWELL  VALUE 'F'.
       01  WS-FRIEND           PIC X(10) VALUE SPACES.
       01  WS-GREETING.
               10  FILLER          PIC X(07) VALUE 'Hello, '.
               10  WS-USER-NAME    PIC X(05) VALUE SPACES.
               10  FILLER          PIC X     VALUE '!'.
       01  WS-FAREWELL.
               10  FILLER          PIC X(15) VALUE 'See you later, '.
               10  WS-USER-NAME    PIC X(09) VALUE SPACES.
               10  FILLER          PIC X     VALUE '!'.

       01  :program:-param         PIC X(10) VALUE "program".
       01  :request:-data       PIC X(10) VALUE "data data".
       01  :response:-element      PIC X(10) VALUE ".response.".

       PROCEDURE DIVISION.

           ACCEPT WS-FRIEND.


       2000-SPEAK.
           IF MESSAGE-IS-GREETING
               IF WS-FRIEND EQUAL SPACES
                   MOVE 'World' TO WS-USER-NAME OF WS-GREETING
               ELSE
                   MOVE WS-FRIEND TO WS-USER-NAME OF WS-GREETING
               END-IF
           END-IF
           IF MESSAGE-IS-FAREWELL
               IF WS-FRIEND EQUAL SPACES
                   MOVE 'alligator!' TO WS-USER-NAME OF WS-FAREWELL
               ELSE
                   MOVE WS-FRIEND TO WS-USER-NAME OF WS-FAREWELL
               END-IF
           END-IF
           .

       3000-SAY-NO-MORE.
              DISPLAY WS-GREETING :PROGRAM:-param
              DISPLAY WS-FAREWELL :PROGRAM:-param :REQUEST:-data
              DISPLAY :PROGRAM:-param :REQUEST:-data :RESPONSE:-element
           .

       9999-END.
           .
