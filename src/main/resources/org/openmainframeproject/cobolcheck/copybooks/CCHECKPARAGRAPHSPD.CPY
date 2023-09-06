      * CCHECKPARAGRAPHSPD.CPY
       ==UT==CHECK-EXPECTATION.
      *****************************************************************
      * COMPARE EXPECTED AND ACTUAL VALUES FOR EQUALITY.
      *****************************************************************
           MOVE ==UT==TEST-CASE-COUNT TO ==UT==TEST-CASE-NUMBER
           PERFORM ==UT==COMPARE
           IF ==UT==UNMOCK-FAILED = 0
               IF ==UT==COMPARE-PASSED 
                   PERFORM ==UT==DISPLAY-PASSED
               ELSE 
                   PERFORM ==UT==DISPLAY-FAILED
               END-IF
           ELSE 
               PERFORM ==UT==DISPLAY-MOCK-FAILED
           END-IF
           .

       ==UT==COMPARE.
           SET ==UT==COMPARE-FAILED TO TRUE
           IF ==UT==COMPARE-88-LEVEL
               IF ==UT==COMPARE-88-LEVEL
                   IF (==UT==ACTUAL-88-VALUE
                       AND ==UT==EXPECTED-88-VALUE)
                   OR (NOT ==UT==ACTUAL-88-VALUE
                       AND NOT ==UT==EXPECTED-88-VALUE)
                       SET ==UT==COMPARE-PASSED TO TRUE
                   END-IF
               ELSE
                   IF ==UT==ACTUAL IS EQUAL TO ==UT==EXPECTED
                       SET ==UT==COMPARE-PASSED TO TRUE
                   END-IF
               END-IF
           ELSE
               EVALUATE TRUE ALSO TRUE
                   WHEN ==UT==NUMERIC-COMPARE ALSO ==UT==RELATION-EQ
                       IF ==UT==ACTUAL-NUMERIC
                               EQUAL ==UT==EXPECTED-NUMERIC
                           SET ==UT==COMPARE-PASSED TO TRUE
                       END-IF
                   WHEN ==UT==NUMERIC-COMPARE ALSO ==UT==RELATION-GT
                       IF ==UT==ACTUAL-NUMERIC
                               GREATER ==UT==EXPECTED-NUMERIC
                           SET ==UT==COMPARE-PASSED TO TRUE
                       END-IF
                   WHEN ==UT==NUMERIC-COMPARE ALSO ==UT==RELATION-GE
                       IF ==UT==ACTUAL-NUMERIC
                               GREATER OR EQUAL ==UT==EXPECTED-NUMERIC
                           SET ==UT==COMPARE-PASSED TO TRUE
                       END-IF
                   WHEN ==UT==NUMERIC-COMPARE ALSO ==UT==RELATION-LT
                       IF ==UT==ACTUAL-NUMERIC
                               LESS ==UT==EXPECTED-NUMERIC
                           SET ==UT==COMPARE-PASSED TO TRUE
                       END-IF
                   WHEN ==UT==NUMERIC-COMPARE ALSO ==UT==RELATION-LE
                       IF ==UT==ACTUAL-NUMERIC
                               LESS OR EQUAL ==UT==EXPECTED-NUMERIC
                           SET ==UT==COMPARE-PASSED TO TRUE
                       END-IF
                   WHEN ==UT==ALPHANUMERIC-COMPARE ALSO ==UT==RELATION-EQ
                       IF ==UT==ACTUAL
                               EQUAL ==UT==EXPECTED
                           SET ==UT==COMPARE-PASSED TO TRUE
                        END-IF
                    WHEN ==UT==ALPHANUMERIC-COMPARE ALSO ==UT==RELATION-GT
                        IF ==UT==ACTUAL
                                GREATER ==UT==EXPECTED
                            SET ==UT==COMPARE-PASSED TO TRUE
                        END-IF
                    WHEN ==UT==ALPHANUMERIC-COMPARE ALSO ==UT==RELATION-GE
                        IF ==UT==ACTUAL
                                GREATER OR EQUAL ==UT==EXPECTED
                            SET ==UT==COMPARE-PASSED TO TRUE
                        END-IF
                    WHEN ==UT==ALPHANUMERIC-COMPARE ALSO ==UT==RELATION-LT
                        IF ==UT==ACTUAL
                                LESS ==UT==EXPECTED
                            SET ==UT==COMPARE-PASSED TO TRUE
                        END-IF
                    WHEN ==UT==ALPHANUMERIC-COMPARE ALSO ==UT==RELATION-LE
                        IF ==UT==ACTUAL
                                LESS OR EQUAL ==UT==EXPECTED
                            SET ==UT==COMPARE-PASSED TO TRUE
                        END-IF
                    WHEN OTHER
                        IF ==UT==ACTUAL EQUAL ==UT==EXPECTED
                            SET ==UT==COMPARE-PASSED TO TRUE
                        END-IF
               END-EVALUATE
           END-IF
           PERFORM ==UT==REVERSE-RESULT
           .

       ==UT==REVERSE-RESULT.
           IF ==UT==REVERSE-COMPARE
               IF ==UT==COMPARE-PASSED
                   SET ==UT==COMPARE-FAILED TO TRUE
               ELSE
                   SET ==UT==COMPARE-PASSED TO TRUE
               END-IF
           END-IF
           .                

       ==UT==DISPLAY-PASSED.
           ADD 1 TO ==UT==NUMBER-PASSED
           DISPLAY ==UT==PASSED 
                   ==UT==TEST-CASE-NUMBER '. ' 
                   ==UT==TEST-CASE-NAME
           .              

       ==UT==DISPLAY-FAILED.
           ADD 1 TO ==UT==NUMBER-FAILED
           DISPLAY ==UT==FAILED 
                   ==UT==TEST-CASE-NUMBER '. ' 
                   ==UT==TEST-CASE-NAME
           IF ==UT==NUMERIC-COMPARE
               DISPLAY '    EXPECTED ' ==UT==EXPECTED-NUMERIC
               DISPLAY '         WAS ' ==UT==ACTUAL-NUMERIC
           ELSE
               MOVE ZERO TO ==UT==EXPECTED-TRIM
               INSPECT FUNCTION REVERSE(==UT==EXPECTED)
                   TALLYING ==UT==EXPECTED-TRIM
                   FOR LEADING SPACES
               SUBTRACT ==UT==EXPECTED-TRIM
                   FROM LENGTH OF ==UT==EXPECTED
                   GIVING ==UT==EXPECTED-TRIM
               MOVE ZERO TO ==UT==ACTUAL-TRIM
               INSPECT FUNCTION REVERSE(==UT==ACTUAL)
                   TALLYING ==UT==ACTUAL-TRIM
                   FOR LEADING SPACES
               SUBTRACT ==UT==ACTUAL-TRIM
                   FROM LENGTH OF ==UT==ACTUAL
                   GIVING ==UT==ACTUAL-TRIM

               DISPLAY '    EXPECTED <'
                   ==UT==EXPECTED(1:==UT==EXPECTED-TRIM)
                       '>, WAS <' ==UT==ACTUAL(1:==UT==ACTUAL-TRIM) '>'
           END-IF            
           MOVE 4 TO ==UT==RETCODE
           .

       ==UT==DISPLAY-MOCK-FAILED.
              ADD 1 TO ==UT==NUMBER-FAILED
              DISPLAY ==UT==FAILED 
                     ==UT==TEST-CASE-NUMBER '. ' 
                     ==UT==TEST-CASE-NAME
              DISPLAY " SOME CALLS WERE NOT MOCKED."
              .

       ==UT==ASSERT-ACCESSES.
      *****************************************************************
      * COMPARE THE NUMBER OF ACCESSES TO A MOCK WITH THE EXPECTED
      * NUMBER OF ACCESSES.
      *****************************************************************
           MOVE ==UT==TEST-CASE-COUNT TO ==UT==TEST-CASE-NUMBER
           MOVE ==UT==ACTUAL-ACCESSES TO ==UT==ACTUAL-ACCESSES-FMT
           MOVE ==UT==EXPECTED-ACCESSES TO ==UT==EXPECTED-ACCESSES-FMT
           IF ==UT==EXPECTED-ACCESSES IS EQUAL TO 1
              MOVE SPACES TO ==UT==LABEL-EXPECTED-ACCESS-PL
           ELSE
              MOVE ==UT==CONST-ES TO ==UT==LABEL-EXPECTED-ACCESS-PL
           END-IF

           SET ==UT==VERIFY-FAILED TO TRUE
           EVALUATE TRUE
               WHEN ==UT==VERIFY-AT-LEAST
                    MOVE ==UT==VERIFY-AT-LEAST-STRING TO
                        ==UT==LABEL-VERIFY-COMPARE
                    IF ==UT==ACTUAL-ACCESSES IS GREATER THAN OR EQUAL TO
                            ==UT==EXPECTED-ACCESSES
                        SET ==UT==VERIFY-PASSED TO TRUE
                    END-IF
               WHEN ==UT==VERIFY-NO-MORE-THAN
                    MOVE ==UT==VERIFY-NO-MORE-THAN-STRING TO
                        ==UT==LABEL-VERIFY-COMPARE
                    IF ==UT==ACTUAL-ACCESSES IS LESS THAN OR EQUAL TO
                            ==UT==EXPECTED-ACCESSES
                        SET ==UT==VERIFY-PASSED TO TRUE
                    END-IF
               WHEN OTHER
                    MOVE ==UT==VERIFY-EXACT-STRING TO
                        ==UT==LABEL-VERIFY-COMPARE
                    IF ==UT==ACTUAL-ACCESSES IS EQUAL TO
                            ==UT==EXPECTED-ACCESSES
                        SET ==UT==VERIFY-PASSED TO TRUE
                    END-IF
           END-EVALUATE

           MOVE FUNCTION TRIM(==UT==EXPECTED-ACCESSES-FMT) 
              TO ==UT==EXPECTED-ACCESSES-DISP
           MOVE FUNCTION TRIM(==UT==ACTUAL-ACCESSES-FMT) 
              TO ==UT==ACTUAL-ACCESSES-DISP
              
           IF ==UT==UNMOCK-FAILED = 0
               IF ==UT==VERIFY-PASSED
                   ADD 1 TO ==UT==NUMBER-PASSED
                   STRING
                        ==UT==PASSED   DELIMITED BY SIZE
                        ==UT==TEST-CASE-NUMBER '. ' DELIMITED BY SIZE
                        'VERIFY ' ==UT==LABEL-VERIFY-COMPARE DELIMITED BY '  '
                        ' ' DELIMITED BY SIZE
                        ==UT==EXPECTED-ACCESSES-DISP DELIMITED BY SPACE
                        ' ' DELIMITED BY SIZE
                        ==UT==LABEL-EXPECTED-ACCESS ' ' DELIMITED BY SIZE
                        'TO ' ==UT==MOCK-OPERATION DELIMITED BY SIZE
                       INTO ==UT==DISPLAY-MESSAGE
                   END-STRING
                   DISPLAY ==UT==DISPLAY-MESSAGE
    
               ELSE
                   ADD 1 TO ==UT==NUMBER-FAILED
                   MOVE SPACES TO ==UT==DISPLAY-MESSAGE
                   STRING
                       ==UT==FAILED                   DELIMITED BY SIZE
                       ==UT==TEST-CASE-NUMBER         DELIMITED BY SIZE
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE
                       ==UT==MOCK-OPERATION           DELIMITED BY SIZE
                       INTO ==UT==DISPLAY-MESSAGE
                   END-STRING
                   DISPLAY ==UT==DISPLAY-MESSAGE
                   MOVE SPACES TO ==UT==DISPLAY-MESSAGE
                   STRING
                       '   EXPECTED '                 DELIMITED BY SIZE
                       ==UT==LABEL-VERIFY-COMPARE DELIMITED BY '  '
                       ' ' DELIMITED BY SIZE
                       ==UT==EXPECTED-ACCESSES-DISP   DELIMITED BY SPACE
                       ' '                          DELIMITED BY SIZE
                       ==UT==LABEL-EXPECTED-ACCESS    DELIMITED BY SPACE
                       ', WAS '                       DELIMITED BY SIZE
                       ==UT==ACTUAL-ACCESSES-DISP     DELIMITED BY SIZE
                       INTO ==UT==DISPLAY-MESSAGE
                   END-STRING
                   DISPLAY ==UT==DISPLAY-MESSAGE
                   MOVE 4 TO ==UT==RETCODE
               END-IF
           ELSE 
               PERFORM ==UT==DISPLAY-MOCK-FAILED 
           END-IF         
           .

       ==UT==LOOKUP-FILE.
      *****************************************************************
      * LOOK UP A FILE SPECIFICATION.
      *****************************************************************
           PERFORM VARYING ==UT==FILE-IX FROM 1 BY 1
               UNTIL ==UT==FILE-IX GREATER ==UT==FILE-MAX
               OR ==UT==INTERNAL-FILENAME(==UT==FILE-IX)
                EQUAL ==UT==MOCK-FIND-FILENAME
             CONTINUE    
           END-PERFORM
           SET ==UT==FILE-IX DOWN BY 1
           .
       ==UT==INITIALIZE.
           MOVE SPACES TO ==UT==FILE-INFORMATION
           MOVE 4 to RETURN-CODE

           .
       ==UT==END.

      * CCHECKPARAGRAPHSPD.CPY END
