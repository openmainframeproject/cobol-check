       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  FILECOPY.                                                   
      *****************************************************************         
      * Copy one sequential file to another.                                    
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
       01  INPUT-RECORD.                                                        
           05  IN-FIELD-1         PIC X(10).                                    
           05  FILLER             PIC X(20).                                    
           05  IN-FIELD-2         PIC X(10).                                    
       01  OUTPUT-RECORD.                                                       
           05  OUT-FIELD-1         PIC X(5).                                    
           05  OUT-FIELD-2     PIC X(16).                                       
           05  OUT-FIELD-3     PIC X(14).                                       
      *    COPY OUTREC2.                                                        
           05  FILLER              PIC X(5).                                    
      * CCHECKWS.CPY                                                            
       01  FILLER PIC X(32) VALUE '******* COBOLCHECK 0.0.1 *******'.           
       01  UT-FIELDS.                                                           
           05  UT-CONST-ES               PIC X(02) VALUE 'ES'.                  
           05  UT-LABEL-EXPECTED-ACCESS.                                        
               10  FILLER                PIC X(06) VALUE 'ACCESS'.              
               10  UT-LABEL-EXPECTED-ACCESS-PL                                  
                                         PIC X(02) VALUE SPACES.                
           05  UT-DISPLAY-MESSAGE     PIC X(256) VALUE SPACES.                  
           05  FILLER                    PIC X(02) VALUE SPACES.                
               88  UT-RELATION-EQ               VALUE SPACES, 'EQ'.             
               88  UT-RELATION-GE               VALUE 'GE'.                     
               88  UT-RELATION-GT               VALUE 'GT'.                     
               88  UT-RELATION-LE               VALUE 'LE'.                     
               88  UT-RELATION-LT               VALUE 'LT'.                     
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  UT-NORMAL-COMPARE            VALUE 'N'.                      
               88  UT-REVERSE-COMPARE           VALUE 'Y'.                      
           05  FILLER                    PIC X(01) VALUE '1'.                   
               88  UT-VERIFY-EXACT              VALUE '1'.                      
               88  UT-VERIFY-AT-LEAST           VALUE '2'.                      
               88  UT-VERIFY-NO-MORE-THAN       VALUE '3'.                      
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  UT-VERIFY-PASSED      VALUE 'Y'.                             
               88  UT-VERIFY-FAILED      VALUE SPACES.                          
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  UT-ALPHANUMERIC-COMPARE VALUE SPACE.                         
               88  UT-NUMERIC-COMPARE    VALUE 'N'.                             
               88  UT-COMPARE-88-LEVEL   VALUE '8'.                             
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  UT-COMPARE-PASSED     VALUE 'Y'.                             
               88  UT-COMPARE-FAILED     VALUE SPACES.                          
           05  UT-EXPECTED               PIC X(256) VALUE SPACES.               
           05  UT-ACTUAL                 PIC X(256) VALUE SPACES.               
           05  UT-EXPECTED-NUMERIC-X.                                           
               10  UT-EXPECTED-NUMERIC   PIC S9(11)V9(7)                        
                                            COMP-3 VALUE ZERO.                  
           05  UT-ACTUAL-NUMERIC-X.                                             
               10  UT-ACTUAL-NUMERIC     PIC S9(11)V9(7)                        
                                            COMP-3 VALUE ZERO.                  
           05  FILLER                    PIC X     VALUE 'F'.                   
               88 UT-EXPECTED-88-VALUE             VALUE 'T', FALSE 'F'.        
           05  FILLER                    PIC X     VALUE 'F'.                   
               88 UT-ACTUAL-88-VALUE               VALUE 'T', FALSE 'F'.        
           05  UT-EXPECTED-ACCESSES      PIC 9(04) VALUE ZERO.                  
           05  UT-ACTUAL-ACCESSES        PIC 9(04) VALUE ZERO.                  
           05  UT-EXPECTED-ACCESSES-FMT  PIC Z,ZZ9.                             
           05  UT-ACTUAL-ACCESSES-FMT    PIC Z,ZZ9.                             
           05  UT-FAILED                 PIC X(11)  VALUE "**** FAIL: ".        
           05  UT-PASSED                 PIC X(11)  VALUE "     PASS: ".        
           05  UT-TEST-CASE-NAME         PIC X(80)  VALUE SPACES.               
           05  UT-TEST-CASE-NUMBER       PIC ZZ9.                               
           05  UT-RETCODE                PIC 9(4)   VALUE ZERO.                 
           05  UT-TEST-CASE-COUNT        PIC 9(4)   VALUE ZERO.                 
           05  UT-NUMBER-PASSED          PIC 9(4)   VALUE ZERO.                 
           05  UT-NUMBER-FAILED          PIC 9(4)   VALUE ZERO.                 
           05  UT-EXPECTED-TRIM          PIC S9(5) COMP-3 VALUE ZERO.           
           05  UT-ACTUAL-TRIM            PIC S9(5) COMP-3 VALUE ZERO.           
       01  UT-MOCKS.                                                            
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  UT-MOCK-FOUND                   VALUE 'Y'.                   
               88  UT-MOCK-NOT-FOUND               VALUE SPACES.                
           05  UT-MOCK-FIND-TYPE         PIC X(04).                             
               88  UT-FIND-FILE-MOCK     VALUE 'FILE'.                          
               88  UT-FIND-CALL-MOCK     VALUE 'CALL'.                          
               88  UT-FIND-CICS-MOCK     VALUE 'CICS'.                          
               88  UT-FIND-SQL-MOCK      VALUE 'SQL'.                           
               88  UT-FIND-PARA-MOCK     VALUE 'PARA'.                          
           05  UT-MOCK-FIND-FILENAME     PIC X(31).                             
           05  UT-MOCK-FIND-PARA-NAME    PIC X(31).                             
           05  UT-MOCK-FIND-OPERATION    PIC X(04).                             
           05  UT-MOCK-FIND-CALL-TOKENS.                                        
               10  UT-MOCK-FIND-CALL-TOKEN OCCURS 25 PIC X(31).                 
           05  UT-MOCK-FIND-CICS-KEYWORDS.                                      
               10  UT-MOCK-FIND-CICS-KEYWORD OCCURS 25 PIC X(31).               
           05  UT-MOCK-SET-RECORD        PIC X(2048).                           
           05  UT-MOCK-SET-FILE-STATUS   PIC X(02).                             
           05  UT-MOCK-MAX               PIC 9(02) VALUE 10.                    
           05  UT-MOCK-COUNT             PIC 9(02) VALUE ZERO.                  
           05  UT-MOCK OCCURS 20 INDEXED BY UT-MOCK-IX.                         
               10  UT-MOCK-TYPE          PIC X(04).                             
                   88  UT-MOCK-FILE          VALUE 'FILE'.                      
                   88  UT-MOCK-CALL          VALUE 'CALL'.                      
                   88  UT-MOCK-CICS          VALUE 'CICS'.                      
                   88  UT-MOCK-SQL           VALUE 'SQL'.                       
                   88  UT-MOCK-PARA          VALUE 'PARA'.                      
               10  UT-MOCK-ACCESS-COUNT    PIC 9(04) VALUE ZERO.                
               10  UT-MOCK-RECORD        PIC X(8192).                           
               10  UT-MOCK-DATA          PIC X(806).                            
               10  UT-MOCK-FILE-DATA REDEFINES UT-MOCK-DATA.                    
                   15  UT-MOCK-FILENAME       PIC X(31).                        
                   15  UT-MOCK-OPERATION      PIC X(20).                        
                   15  UT-MOCK-FILE-STATUS    PIC X(02).                        
                   15  FILLER                 PIC X(753).                       
               10  UT-MOCK-CALL-DATA REDEFINES UT-MOCK-DATA.                    
                   15  UT-MOCK-CALL-TOKENS-KEY PIC X(806).                      
               10  UT-MOCK-CICS-DATA REDEFINES UT-MOCK-DATA.                    
                   15  UT-MOCK-CICS-KEYWORDS-KEY PIC X(806).                    
               10  UT-MOCK-PARA-DATA REDEFINES UT-MOCK-DATA.                    
                   15  UT-MOCK-PARA-NAME  PIC X(31).                            
                   15  FILLER             PIC X(775).                           
               10  UT-MOCK-SQL-DATA REDEFINES UT-MOCK-DATA.                     
                   15  FILLER             PIC X(806).                           
       01  UT-FILES.                                                            
           05  UT-FILE-MAX                   PIC 9(02) VALUE 10.                
           05  UT-FILE-COUNT                 PIC 9(02) VALUE ZERO.              
           05  FILLER                        PIC X(01) VALUE SPACE.             
               88  UT-FILENAME-MATCHED       VALUE 'Y'.                         
               88  UT-FILENAME-NOT-MATCHED   VALUE 'N'.                         
           05  UT-FILE-INFORMATION.                                             
               10  FILLER OCCURS 10 INDEXED BY UT-FILE-IX.                      
                   15  UT-INTERNAL-FILENAME      PIC X(31).                     
                   15  UT-RECORD-FIELD-NAME      PIC X(31).                     
                   15  UT-FILE-STATUS-FIELD-NAME PIC X(31).                     
                                                                                
      * CCHECKWS.CPY END                                                        
       01  FILLER.                                                              
           05  INPUT-FILE-STATUS  PIC XX.                                       
               88  INPUT-OK       VALUE '00'.                                   
               88  END-OF-FILE    VALUE '10'.                                   
               88  FILE-NOT-FOUND VALUE '35'.                                   
           05  OUTPUT-FILE-STATUS PIC XX.                                       
               88  OUTPUT-OK      VALUE '00'.                                   
           05  WS-COUNT           PIC S9(5) COMP-3.                             
           05  WS-COUNT-FORMATTED PIC ZZ,ZZ9.                                   
           05  WS-ERROR-MESSAGE   PIC X(60).                                    
       PROCEDURE DIVISION.                                                      
           PERFORM UT-INITIALIZE                                                
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Tests for a sequential file copy program"                   
           MOVE "Output fields are populated from the input record"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE "Alpha" TO IN-FIELD-1                                          
            MOVE "Beta" TO IN-FIELD-2                                           
            PERFORM 5200-PREPARE-OUTPUT-RECORD                                  
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE IN-FIELD-1 OF INPUT-RECORD TO UT-ACTUAL                         
           MOVE OUT-FIELD-1                                                     
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE OUT-FIELD-2 OF OUTPUT-RECORD TO UT-ACTUAL                       
           MOVE IN-FIELD-2                                                      
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
      * CCHECKPD.CPY                                                            
           DISPLAY SPACE                                                        
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER                       
           DISPLAY UT-TEST-CASE-NUMBER ' TEST CASES WERE EXECUTED'              
           MOVE UT-NUMBER-PASSED TO UT-TEST-CASE-NUMBER                         
           DISPLAY UT-TEST-CASE-NUMBER ' PASSED'                                
           MOVE UT-NUMBER-FAILED TO UT-TEST-CASE-NUMBER                         
           DISPLAY UT-TEST-CASE-NUMBER ' FAILED'                                
           DISPLAY "================================================="          
           MOVE UT-RETCODE TO RETURN-CODE                                       
           GOBACK.                                                              
                                                                                
       UT-CHECK-EXPECTATION.                                                    
      *****************************************************************         
      * COMPARE EXPECTED AND ACTUAL VALUES FOR EQUALITY.                        
      *****************************************************************         
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER                       
           PERFORM UT-COMPARE                                                   
           IF UT-COMPARE-PASSED                                                 
               PERFORM UT-DISPLAY-PASSED                                        
           ELSE                                                                 
               PERFORM UT-DISPLAY-FAILED                                        
           END-IF                                                               
           .                                                                    
                                                                                
       UT-COMPARE.                                                              
           SET UT-COMPARE-FAILED TO TRUE                                        
           IF UT-COMPARE-88-LEVEL                                               
               IF UT-COMPARE-88-LEVEL                                           
                   IF (UT-ACTUAL-88-VALUE                                       
                       AND UT-EXPECTED-88-VALUE)                                
                   OR (NOT UT-ACTUAL-88-VALUE                                   
                       AND NOT UT-EXPECTED-88-VALUE)                            
                       SET UT-COMPARE-PASSED TO TRUE                            
                   END-IF                                                       
               ELSE                                                             
                   IF UT-ACTUAL IS EQUAL TO UT-EXPECTED                         
                       SET UT-COMPARE-PASSED TO TRUE                            
                   END-IF                                                       
               END-IF                                                           
           ELSE                                                                 
               EVALUATE TRUE ALSO TRUE                                          
                   WHEN UT-NUMERIC-COMPARE ALSO UT-RELATION-EQ                  
                       IF UT-ACTUAL-NUMERIC                                     
                               EQUAL UT-EXPECTED-NUMERIC                        
                           SET UT-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN UT-NUMERIC-COMPARE ALSO UT-RELATION-GT                  
                       IF UT-ACTUAL-NUMERIC                                     
                               GREATER UT-EXPECTED-NUMERIC                      
                           SET UT-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN UT-NUMERIC-COMPARE ALSO UT-RELATION-GE                  
                       IF UT-ACTUAL-NUMERIC                                     
                               GREATER OR EQUAL UT-EXPECTED-NUMERIC             
                           SET UT-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN UT-NUMERIC-COMPARE ALSO UT-RELATION-LT                  
                       IF UT-ACTUAL-NUMERIC                                     
                               LESS UT-EXPECTED-NUMERIC                         
                           SET UT-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN UT-NUMERIC-COMPARE ALSO UT-RELATION-LE                  
                       IF UT-ACTUAL-NUMERIC                                     
                               LESS OR EQUAL UT-EXPECTED-NUMERIC                
                           SET UT-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN UT-ALPHANUMERIC-COMPARE ALSO UT-RELATION-EQ             
                       IF UT-ACTUAL                                             
                               EQUAL UT-EXPECTED                                
                           SET UT-COMPARE-PASSED TO TRUE                        
                        END-IF                                                  
                    WHEN UT-ALPHANUMERIC-COMPARE ALSO UT-RELATION-GT            
                        IF UT-ACTUAL                                            
                                GREATER UT-EXPECTED                             
                            SET UT-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN UT-ALPHANUMERIC-COMPARE ALSO UT-RELATION-GE            
                        IF UT-ACTUAL                                            
                                GREATER OR EQUAL UT-EXPECTED                    
                            SET UT-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN UT-ALPHANUMERIC-COMPARE ALSO UT-RELATION-LT            
                        IF UT-ACTUAL                                            
                                LESS UT-EXPECTED                                
                            SET UT-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN UT-ALPHANUMERIC-COMPARE ALSO UT-RELATION-LE            
                        IF UT-ACTUAL                                            
                                LESS OR EQUAL UT-EXPECTED                       
                            SET UT-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN OTHER                                                  
                        IF UT-ACTUAL EQUAL UT-EXPECTED                          
                            SET UT-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
               END-EVALUATE                                                     
           END-IF                                                               
           PERFORM UT-REVERSE-RESULT                                            
           .                                                                    
                                                                                
       UT-REVERSE-RESULT.                                                       
           IF UT-REVERSE-COMPARE                                                
               IF UT-COMPARE-PASSED                                             
                   SET UT-COMPARE-FAILED TO TRUE                                
               ELSE                                                             
                   SET UT-COMPARE-PASSED TO TRUE                                
               END-IF                                                           
           END-IF                                                               
           .                                                                    
                                                                                
       UT-DISPLAY-PASSED.                                                       
           ADD 1 TO UT-NUMBER-PASSED                                            
           DISPLAY UT-PASSED                                                    
                   UT-TEST-CASE-NUMBER '. '                                     
                   UT-TEST-CASE-NAME                                            
           .                                                                    
                                                                                
       UT-DISPLAY-FAILED.                                                       
           ADD 1 TO UT-NUMBER-FAILED                                            
           DISPLAY UT-FAILED                                                    
                   UT-TEST-CASE-NUMBER '. '                                     
                   UT-TEST-CASE-NAME                                            
           IF UT-NUMERIC-COMPARE                                                
               DISPLAY '    EXPECTED ' UT-EXPECTED-NUMERIC                      
                       ', WAS ' UT-ACTUAL-NUMERIC                               
           ELSE                                                                 
               MOVE ZERO TO UT-EXPECTED-TRIM                                    
               INSPECT FUNCTION REVERSE(UT-EXPECTED)                            
                   TALLYING UT-EXPECTED-TRIM                                    
                   FOR LEADING SPACES                                           
               SUBTRACT UT-EXPECTED-TRIM                                        
                   FROM LENGTH OF UT-EXPECTED                                   
                   GIVING UT-EXPECTED-TRIM                                      
               MOVE ZERO TO UT-ACTUAL-TRIM                                      
               INSPECT FUNCTION REVERSE(UT-ACTUAL)                              
                   TALLYING UT-ACTUAL-TRIM                                      
                   FOR LEADING SPACES                                           
               SUBTRACT UT-ACTUAL-TRIM                                          
                   FROM LENGTH OF UT-ACTUAL                                     
                   GIVING UT-ACTUAL-TRIM                                        
                                                                                
               DISPLAY '    EXPECTED <'                                         
                   UT-EXPECTED(1:UT-EXPECTED-TRIM)                              
                       '>, WAS <' UT-ACTUAL(1:UT-ACTUAL-TRIM) '>'               
           END-IF                                                               
           MOVE 4 TO UT-RETCODE                                                 
           .                                                                    
                                                                                
       UT-ASSERT-ACCESSES.                                                      
      *****************************************************************         
      * COMPARE THE NUMBER OF ACCESSES TO A MOCK WITH THE EXPECTED              
      * NUMBER OF ACCESSES.                                                     
      *****************************************************************         
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER                       
           MOVE UT-ACTUAL-ACCESSES TO UT-ACTUAL-ACCESSES-FMT                    
           MOVE UT-EXPECTED-ACCESSES TO UT-EXPECTED-ACCESSES-FMT                
           IF UT-EXPECTED-ACCESSES IS EQUAL TO 1                                
              MOVE SPACES TO UT-LABEL-EXPECTED-ACCESS-PL                        
           ELSE                                                                 
              MOVE UT-CONST-ES TO UT-LABEL-EXPECTED-ACCESS-PL                   
           END-IF                                                               
                                                                                
           SET UT-VERIFY-FAILED TO TRUE                                         
           EVALUATE TRUE                                                        
               WHEN UT-VERIFY-AT-LEAST                                          
                    IF UT-ACTUAL-ACCESSES IS GREATER THAN OR EQUAL TO           
                            UT-EXPECTED-ACCESSES                                
                        SET UT-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
               WHEN UT-VERIFY-NO-MORE-THAN                                      
                    IF UT-ACTUAL-ACCESSES IS LESS THAN OR EQUAL TO              
                            UT-EXPECTED-ACCESSES                                
                        SET UT-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
               WHEN OTHER                                                       
                    IF UT-ACTUAL-ACCESSES IS EQUAL TO                           
                            UT-EXPECTED-ACCESSES                                
                        SET UT-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
           END-EVALUATE                                                         
                                                                                
           IF UT-VERIFY-PASSED                                                  
               ADD 1 TO UT-NUMBER-PASSED                                        
               DISPLAY UT-PASSED                                                
                       UT-TEST-CASE-NUMBER '. '                                 
                      'VERIFY ' UT-EXPECTED-ACCESSES-FMT SPACE                  
                      UT-LABEL-EXPECTED-ACCESS                                  
           ELSE                                                                 
               ADD 1 TO UT-NUMBER-FAILED                                        
               MOVE SPACES TO UT-DISPLAY-MESSAGE                                
               IF UT-MOCK-FILE(UT-MOCK-IX)                                      
                   STRING                                                       
                       UT-FAILED                      DELIMITED BY SIZE         
                       UT-TEST-CASE-NUMBER            DELIMITED BY SIZE         
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE         
                       UT-MOCK-OPERATION(UT-MOCK-IX)  DELIMITED BY SPACE        
                       ' ON '                         DELIMITED BY SIZE         
                       UT-MOCK-FILENAME(UT-MOCK-IX)   DELIMITED BY SPACE        
                       ' | EXPECTED '                 DELIMITED BY SIZE         
                       UT-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE         
                       SPACE                          DELIMITED BY SIZE         
                       UT-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE        
                       ', WAS '                       DELIMITED BY SIZE         
                       UT-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE         
                       INTO UT-DISPLAY-MESSAGE                                  
                   END-STRING                                                   
               ELSE                                                             
                   STRING                                                       
                       UT-FAILED                      DELIMITED BY SIZE         
                       UT-TEST-CASE-NUMBER            DELIMITED BY SIZE         
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE         
                       UT-MOCK-CICS-KEYWORDS-KEY(UT-MOCK-IX)                    
                                                      DELIMITED BY SIZE         
                       INTO UT-DISPLAY-MESSAGE                                  
                   END-STRING                                                   
                   DISPLAY UT-DISPLAY-MESSAGE                                   
                   MOVE SPACES TO UT-DISPLAY-MESSAGE                            
                   STRING                                                       
                       '   EXPECTED '                 DELIMITED BY SIZE         
                       UT-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE         
                       SPACE                          DELIMITED BY SIZE         
                       UT-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE        
                       ', WAS '                       DELIMITED BY SIZE         
                       UT-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE         
                       INTO UT-DISPLAY-MESSAGE                                  
                   END-STRING                                                   
               END-IF                                                           
               DISPLAY UT-DISPLAY-MESSAGE                                       
               MOVE 4 TO UT-RETCODE                                             
           END-IF                                                               
           .                                                                    
                                                                                
       UT-SET-MOCK.                                                             
      *****************************************************************         
      * CREATE OR UPDATE A MOCK SPECIFICATION.                                  
      *****************************************************************         
           EVALUATE TRUE                                                        
               WHEN UT-FIND-FILE-MOCK                                           
                    PERFORM UT-SET-FILE-MOCK                                    
               WHEN UT-FIND-CALL-MOCK                                           
                    PERFORM UT-SET-CALL-MOCK                                    
               WHEN UT-FIND-CICS-MOCK                                           
                    PERFORM UT-SET-CICS-MOCK                                    
               WHEN UT-FIND-PARA-MOCK                                           
                    PERFORM UT-SET-PARA-MOCK                                    
           END-EVALUATE                                                         
           .                                                                    
                                                                                
       UT-SET-FILE-MOCK.                                                        
           PERFORM UT-LOOKUP-MOCK                                               
           IF UT-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO UT-MOCK-COUNT                                           
               SET UT-MOCK-IX TO UT-MOCK-COUNT                                  
               SET UT-MOCK-FILE(UT-MOCK-IX) TO TRUE                             
               MOVE UT-MOCK-FIND-FILENAME                                       
                    TO UT-MOCK-FILENAME(UT-MOCK-IX)                             
               MOVE UT-MOCK-FIND-OPERATION                                      
                    TO UT-MOCK-OPERATION(UT-MOCK-IX)                            
           END-IF                                                               
           MOVE UT-MOCK-SET-RECORD                                              
                TO UT-MOCK-RECORD(UT-MOCK-IX)                                   
           MOVE UT-MOCK-SET-FILE-STATUS                                         
                TO UT-MOCK-FILE-STATUS(UT-MOCK-IX)                              
           .                                                                    
                                                                                
       UT-SET-CALL-MOCK.                                                        
           PERFORM UT-LOOKUP-MOCK                                               
           IF UT-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO UT-MOCK-COUNT                                           
               SET UT-MOCK-IX TO UT-MOCK-COUNT                                  
               MOVE UT-MOCK-FIND-CALL-TOKENS                                    
                   TO UT-MOCK-CALL-TOKENS-KEY(UT-MOCK-IX)                       
           END-IF                                                               
           .                                                                    
                                                                                
       UT-SET-CICS-MOCK.                                                        
           PERFORM UT-LOOKUP-MOCK                                               
           IF UT-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO UT-MOCK-COUNT                                           
               SET UT-MOCK-IX TO UT-MOCK-COUNT                                  
               MOVE UT-MOCK-FIND-CICS-KEYWORDS                                  
                   TO UT-MOCK-CICS-KEYWORDS-KEY(UT-MOCK-IX)                     
           END-IF                                                               
           .                                                                    
                                                                                
       UT-SET-PARA-MOCK.                                                        
           PERFORM UT-LOOKUP-MOCK                                               
           IF UT-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO UT-MOCK-COUNT                                           
               SET UT-MOCK-IX TO UT-MOCK-COUNT                                  
               MOVE UT-MOCK-FIND-PARA-NAME                                      
                   TO UT-MOCK-PARA-NAME(UT-MOCK-IX)                             
           END-IF                                                               
           .                                                                    
                                                                                
       UT-LOOKUP-MOCK.                                                          
      *****************************************************************         
      * LOOK UP A MOCK SPECIFICATION.                                           
      *****************************************************************         
           SET UT-MOCK-NOT-FOUND TO TRUE                                        
           PERFORM VARYING UT-MOCK-IX FROM 1 BY 1                               
               UNTIL UT-MOCK-IX IS GREATER THAN UT-MOCK-MAX                     
                  OR UT-MOCK-FOUND                                              
               EVALUATE TRUE                                                    
                   WHEN UT-FIND-FILE-MOCK                                       
                       IF UT-MOCK-FIND-FILENAME IS EQUAL TO                     
                              UT-MOCK-FILENAME(UT-MOCK-IX)                      
                       AND UT-MOCK-FIND-OPERATION IS EQUAL TO                   
                              UT-MOCK-OPERATION(UT-MOCK-IX)                     
                           SET UT-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
                   WHEN UT-FIND-CALL-MOCK                                       
                       IF UT-MOCK-FIND-CALL-TOKENS IS EQUAL TO                  
                              UT-MOCK-CALL-TOKENS-KEY(UT-MOCK-IX)               
                           SET UT-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
                   WHEN UT-FIND-CICS-MOCK                                       
                       IF UT-MOCK-FIND-CICS-KEYWORDS IS EQUAL TO                
                              UT-MOCK-CICS-KEYWORDS-KEY(UT-MOCK-IX)             
                           SET UT-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
                   WHEN UT-FIND-PARA-MOCK                                       
                       IF UT-MOCK-FIND-PARA-NAME IS EQUAL TO                    
                              UT-MOCK-PARA-NAME(UT-MOCK-IX)                     
                           SET UT-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
               END-EVALUATE                                                     
           END-PERFORM                                                          
           SET UT-MOCK-IX DOWN BY 1                                             
           .                                                                    
                                                                                
       UT-LOOKUP-FILE.                                                          
      *****************************************************************         
      * LOOK UP A FILE SPECIFICATION.                                           
      *****************************************************************         
           PERFORM VARYING UT-FILE-IX FROM 1 BY 1                               
               UNTIL UT-FILE-IX GREATER UT-FILE-MAX                             
               OR UT-INTERNAL-FILENAME(UT-FILE-IX)                              
                EQUAL UT-MOCK-FIND-FILENAME                                     
             CONTINUE                                                           
           END-PERFORM                                                          
           SET UT-FILE-IX DOWN BY 1                                             
           .                                                                    
       UT-BEFORE.                                                               
           .                                                                    
       UT-AFTER.                                                                
           .                                                                    
       UT-INITIALIZE.                                                           
           MOVE SPACES TO UT-FILE-INFORMATION                                   
                                                                                
           .                                                                    
       UT-END.                                                                  
                                                                                
      * CCHECKPD.CPY END                                                        
           PERFORM 1000-INITIALIZE                                              
           PERFORM 5000-PROCESS                                                 
           PERFORM 8000-HOUSEKEEPING                                            
           GOBACK                                                               
           .                                                                    
       1000-INITIALIZE.                                                         
      *    OPEN INPUT INPUT-FILE                                                
           EVALUATE TRUE                                                        
               WHEN INPUT-OK                                                    
                   CONTINUE                                                     
               WHEN FILE-NOT-FOUND                                              
                   MOVE 'Input file not found'                                  
                     TO WS-ERROR-MESSAGE                                        
               WHEN OTHER                                                       
                   STRING "Unexpected input file status on open "               
                       DELIMITED BY SIZE                                        
                       INPUT-FILE-STATUS                                        
                       DELIMITED BY SIZE                                        
                     INTO WS-ERROR-MESSAGE                                      
                   PERFORM 9999-ABORT                                           
           END-EVALUATE                                                         
      *    OPEN OUTPUT OUTPUT-FILE                                              
           EVALUATE TRUE                                                        
               WHEN OUTPUT-OK                                                   
                   CONTINUE                                                     
               WHEN OTHER                                                       
                   STRING "Unexpected output file status on open "              
                       DELIMITED BY SIZE                                        
                       OUTPUT-FILE-STATUS                                       
                       DELIMITED BY SIZE                                        
                     INTO WS-ERROR-MESSAGE                                      
                   PERFORM 9999-ABORT                                           
           END-EVALUATE                                                         
           MOVE ZERO TO WS-COUNT                                                
           .                                                                    
       5000-PROCESS.                                                            
      *    READ INPUT-FILE                                                      
           PERFORM WITH TEST BEFORE                                             
                   UNTIL END-OF-FILE                                            
               PERFORM 5200-PREPARE-OUTPUT-RECORD                               
               PERFORM 5400-WRITE-OUTPUT-RECORD                                 
      *        READ INPUT-FILE                                                  
           END-PERFORM                                                          
           .                                                                    
       5200-PREPARE-OUTPUT-RECORD.                                              
           MOVE SPACES TO OUTPUT-RECORD                                         
           MOVE IN-FIELD-1 TO OUT-FIELD-1                                       
           MOVE IN-FIELD-2 TO OUT-FIELD-2                                       
           MOVE "Good" TO OUT-FIELD-3                                           
           .                                                                    
       5400-WRITE-OUTPUT-RECORD.                                                
      *    WRITE OUTPUT-RECORD                                                  
           IF NOT OUTPUT-OK                                                     
               STRING "Unexpected output file status on write "                 
                   DELIMITED BY SIZE                                            
                   OUTPUT-FILE-STATUS                                           
                   DELIMITED BY SIZE                                            
                 INTO WS-ERROR-MESSAGE                                          
               PERFORM 9999-ABORT                                               
           END-IF                                                               
           ADD 1 TO WS-COUNT                                                    
           .                                                                    
       8000-HOUSEKEEPING.                                                       
      *    CLOSE OUTPUT-FILE.                                                   
      *    CLOSE INPUT-FILE                                                     
           MOVE WS-COUNT TO WS-COUNT-FORMATTED                                  
           DISPLAY "Records processed: " WS-COUNT-FORMATTED                     
           .                                                                    
       9999-ABORT.                                                              
           DISPLAY WS-ERROR-MESSAGE                                             
           GOBACK                                                               
           .                                                                    
