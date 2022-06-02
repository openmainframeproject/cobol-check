       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ALPHA.
      *****************************************************************
      * Program to exercise EXPECT statements.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  VALUE-1           PIC X(80).
           05  VALUE-2           PIC X(80).
           05  VALUE-3           PIC X(80).
           05  TEMP              PIC X(80).

       PROCEDURE DIVISION.

       000-START SECTION.
           MOVE "Value1" to VALUE-1
           MOVE "Value2" to VALUE-2
           PERFORM 100-WELCOME
           PERFORM 200-GOODBYE
           PERFORM 300-CHANGE-1
           PERFORM 400-CHANGE-2
           PERFORM 500-SWITCH
           PERFORM 999-END
           EXIT SECTION.

       100-WELCOME SECTION
           .
           MOVE "Hello" to VALUE-1
           MOVE "Hi" to VALUE-2
              EXIT SECTION
              .

       200-GOODBYE SECTION.
      * Comment for testing purposes
           MOVE "Bye" to VALUE-1
           MOVE "See you" to VALUE-2
              EXIT SECTION
          .

       300-CHANGE-1
      *Comment for testing purposes
       .
           EVALUATE VALUE-1
           WHEN "Hello"
              MOVE "Bye" TO VALUE-1
           WHEN "Bye"
              MOVE "Hello" TO VALUE-1
           WHEN OTHER
              EXIT PARAGRAPH
           END-EVALUATE.

       400-CHANGE-2 SECTION
          .
           EVALUATE VALUE-2
           WHEN "Hi"
              MOVE "See you" TO VALUE-2
              EXIT SECTION
           WHEN OTHER
              MOVE "Hi" TO VALUE-2
              EXIT SECTION
           END-EVALUATE
          .

       410-PLACEHOLDER SECTION.
           CONTINUE
           .

       500-SWITCH.
           MOVE VALUE-2 TO TEMP
           MOVE VALUE-1 TO VALUE-2
           MOVE TEMP TO VALUE-1.

       600-MAKE-CALL.
           MOVE "arg1" to VALUE-1
           MOVE "arg2" to VALUE-2
           CALL 'PROG1'
           .

       700-MAKE-CALL.
           MOVE "arg1" to VALUE-1
           MOVE "arg2" to VALUE-2
           CALL VALUE-2 USING VALUE-1
           END-CALL.

       800-MAKE-CALL.
           MOVE "arg1" to VALUE-1
           MOVE "arg2" to VALUE-2
           MOVE "arg3" to VALUE-3
           CALL 'PROG3' USING 
              BY CONTENT VALUE-1,
              BY VALUE VALUE-2,  
              VALUE-3.
           CALL 'PROG3' USING 
              BY CONTENT VALUE-1,
              BY VALUE VALUE-2,  
              VALUE-3
          CALL 'PROG3' USING VALUE-1.

       999-END.
           GOBACK
           .