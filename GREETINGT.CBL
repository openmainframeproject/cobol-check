       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  GREETING.                                                   
      *****************************************************************         
      * Trivial program to exercise CobolCheck.                                 
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
      * ZUTZCWS.CPY                                                             
       01  FILLER PIC X(16) VALUE '******* COBCHECK'.                           
       01  FILLER PIC X(16) VALUE '******* 0.1.0'.                              
       01  CC-FIELDS.                                                           
           05  CC-CONST-ES               PIC X(02) VALUE 'ES'.                  
           05  CC-LABEL-EXPECTED-ACCESS.                                        
               10  FILLER                PIC X(06) VALUE 'ACCESS'.              
               10  CC-LABEL-EXPECTED-ACCESS-PL                                  
                                         PIC X(02) VALUE SPACES.                
           05  CC-DISPLAY-MESSAGE        PIC X(256) VALUE SPACES.               
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-NORMAL-COMPARE     VALUE 'N'.                             
               88  CC-REVERSE-COMPARE    VALUE 'Y'.                             
           05  FILLER                    PIC X(01) VALUE '1'.                   
               88  CC-VERIFY-EXACT                 VALUE '1'.                   
               88  CC-VERIFY-AT-LEAST              VALUE '2'.                   
               88  CC-VERIFY-NO-MORE-THAN          VALUE '3'.                   
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-VERIFY-PASSED      VALUE 'Y'.                             
               88  CC-VERIFY-FAILED      VALUE SPACES.                          
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-COMPARE-DEFAULT    VALUE SPACE.                           
               88  CC-COMPARE-NUMERIC    VALUE 'N'.                             
               88  CC-COMPARE-88-LEVEL   VALUE '8'.                             
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-COMPARE-PASSED     VALUE 'Y'.                             
               88  CC-COMPARE-FAILED     VALUE SPACES.                          
           05  CC-EXPECTED               PIC X(256) VALUE SPACES.               
           05  CC-ACTUAL                 PIC X(256) VALUE SPACES.               
           05  CC-EXPECTED-NUMERIC       PIC S9(08) VALUE ZERO.                 
           05  CC-ACTUAL-NUMERIC         PIC S9(08) VALUE ZERO.                 
           05  FILLER                    PIC X     VALUE 'F'.                   
               88 CC-EXPECTED-88-VALUE             VALUE 'T', FALSE 'F'.        
           05  FILLER                    PIC X     VALUE 'F'.                   
               88 CC-ACTUAL-88-VALUE               VALUE 'T', FALSE 'F'.        
           05  CC-EXPECTED-ACCESSES      PIC 9(04) VALUE ZERO.                  
           05  CC-ACTUAL-ACCESSES        PIC 9(04) VALUE ZERO.                  
           05  CC-EXPECTED-ACCESSES-FMT  PIC Z,ZZ9.                             
           05  CC-ACTUAL-ACCESSES-FMT    PIC Z,ZZ9.                             
           05  CC-FAILED                 PIC X(11)  VALUE "**** FAIL: ".        
           05  CC-PASSED                 PIC X(11)  VALUE "     PASS: ".        
           05  CC-TEST-CASE-NAME         PIC X(80)  VALUE SPACES.               
           05  CC-TEST-CASE-NUMBER       PIC ZZ9.                               
           05  CC-RETCODE                PIC 9(4)   VALUE ZERO.                 
           05  CC-TEST-CASE-COUNT        PIC 9(4)   VALUE ZERO.                 
           05  CC-NUMBER-PASSED          PIC 9(4)   VALUE ZERO.                 
           05  CC-NUMBER-FAILED          PIC 9(4)   VALUE ZERO.                 
           05  CC-EXPECTED-TRIM          PIC S9(5) COMP-3 VALUE ZERO.           
           05  CC-ACTUAL-TRIM            PIC S9(5) COMP-3 VALUE ZERO.           
       01  CC-MOCKS.                                                            
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-MOCK-FOUND                   VALUE 'Y'.                   
               88  CC-MOCK-NOT-FOUND               VALUE SPACES.                
           05  CC-MOCK-FIND-TYPE         PIC X(04).                             
               88  CC-FIND-FILE-MOCK     VALUE 'FILE'.                          
               88  CC-FIND-CALL-MOCK     VALUE 'CALL'.                          
               88  CC-FIND-CICS-MOCK     VALUE 'CICS'.                          
               88  CC-FIND-SQL-MOCK      VALUE 'SQL'.                           
               88  CC-FIND-PARA-MOCK     VALUE 'PARA'.                          
           05  CC-MOCK-FIND-FILENAME     PIC X(31).                             
           05  CC-MOCK-FIND-PARA-NAME    PIC X(31).                             
           05  CC-MOCK-FIND-OPERATION    PIC X(04).                             
           05  CC-MOCK-FIND-CALL-TOKENS.                                        
               10  CC-MOCK-FIND-CALL-TOKEN OCCURS 25 PIC X(31).                 
           05  CC-MOCK-FIND-CICS-KEYWORDS.                                      
               10  CC-MOCK-FIND-CICS-KEYWORD OCCURS 25 PIC X(31).               
           05  CC-MOCK-SET-RECORD        PIC X(2048).                           
           05  CC-MOCK-SET-FILE-STATUS   PIC X(02).                             
           05  CC-MOCK-MAX               PIC 9(02) VALUE 10.                    
           05  CC-MOCK-COUNT             PIC 9(02) VALUE ZERO.                  
           05  CC-MOCK OCCURS 20 INDEXED BY CC-MOCK-IX.                         
               10  CC-MOCK-TYPE          PIC X(04).                             
                   88  CC-MOCK-FILE          VALUE 'FILE'.                      
                   88  CC-MOCK-CALL          VALUE 'CALL'.                      
                   88  CC-MOCK-CICS          VALUE 'CICS'.                      
                   88  CC-MOCK-SQL           VALUE 'SQL'.                       
                   88  CC-MOCK-PARA          VALUE 'PARA'.                      
               10  CC-MOCK-ACCESS-COUNT    PIC 9(04) VALUE ZERO.                
               10  CC-MOCK-RECORD        PIC X(8192).                           
               10  CC-MOCK-DATA          PIC X(806).                            
               10  CC-MOCK-FILE-DATA REDEFINES CC-MOCK-DATA.                    
                   15  CC-MOCK-FILENAME       PIC X(31).                        
                   15  CC-MOCK-OPERATION      PIC X(20).                        
                   15  CC-MOCK-FILE-STATUS    PIC X(02).                        
                   15  FILLER                 PIC X(753).                       
               10  CC-MOCK-CALL-DATA REDEFINES CC-MOCK-DATA.                    
                   15  CC-MOCK-CALL-TOKENS-KEY PIC X(806).                      
               10  CC-MOCK-CICS-DATA REDEFINES CC-MOCK-DATA.                    
                   15  CC-MOCK-CICS-KEYWORDS-KEY PIC X(806).                    
               10  CC-MOCK-PARA-DATA REDEFINES CC-MOCK-DATA.                    
                   15  CC-MOCK-PARA-NAME  PIC X(31).                            
                   15  FILLER             PIC X(775).                           
               10  CC-MOCK-SQL-DATA REDEFINES CC-MOCK-DATA.                     
                   15  FILLER             PIC X(806).                           
       01  CC-FILES.                                                            
           05  CC-FILE-MAX                   PIC 9(02) VALUE 10.                
           05  CC-FILE-COUNT                 PIC 9(02) VALUE ZERO.              
           05  FILLER                        PIC X(01) VALUE SPACE.             
               88  CC-FILENAME-MATCHED       VALUE 'Y'.                         
               88  CC-FILENAME-NOT-MATCHED   VALUE 'N'.                         
           05  CC-FILE-INFORMATION.                                             
               10  FILLER OCCURS 10 INDEXED BY CC-FILE-IX.                      
                   15  CC-INTERNAL-FILENAME      PIC X(31).                     
                   15  CC-RECORD-FIELD-NAME      PIC X(31).                     
                   15  CC-FILE-STATUS-FIELD-NAME PIC X(31).                     
                                                                                
      * ZUTZCWS.CPY END                                                         
       01  FILLER.                                                              
           05  WS-COUNT           PIC S9(5) COMP-3.                             
           05  FILLER             PIC X VALUE 'G'.                              
               88  MESSAGE-IS-GREETING  VALUE 'G'.                              
               88  MESSAGE-IS-FAREWELL  VALUE 'F'.                              
       01  WS-FRIEND           PIC X(10) VALUE SPACES.                          
       01  WS-GREETING.                                                         
               10  FILLER          PIC X(07) VALUE 'Hello, '.                   
               10  WS-USER-NAME    PIC X(06) VALUE SPACES.                      
               10  FILLER          PIC X     VALUE '!'.                         
       01  WS-FAREWELL.                                                         
               10  FILLER          PIC X(15) VALUE 'See you later, '.           
               10  WS-USER-NAME    PIC X(09) VALUE SPACES.                      
               10  FILLER          PIC X     VALUE '!'.                         
       PROCEDURE DIVISION.                                                      
           PERFORM CC-INITIALIZE                                                
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Greeting returns the appropriate message based on me        
      -    "ssage type"                                                         
           MOVE "When message type is greeting it returns 'Hello, World!        
      -    "'"                                                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            SET MESSAGE-IS-GREETING TO TRUE                                     
            PERFORM 2000-SPEAK                                                  
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           MOVE WS-GREETING TO CC-ACTUAL                                        
           MOVE "Hello, World!"                                                 
               TO CC-EXPECTED                                                   
           SET CC-COMPARE-DEFAULT TO TRUE                                       
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           MOVE "try numerical compare"                                         
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            ADD 1 TO WS-COUNT                                                   
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-COMPARE-NUMERIC TO TRUE                                       
           SET CC-NORMAL-COMPARE TO TRUE                                        
           MOVE WS-COUNT TO CC-ACTUAL-NUMERIC                                   
           MOVE 1 TO CC-EXPECTED-NUMERIC                                        
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           MOVE "try 88 level compare"                                          
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-COMPARE-88-LEVEL TO TRUE                                      
           IF MESSAGE-IS-GREETING                                               
               SET CC-ACTUAL-88-VALUE TO TRUE                                   
               MOVE 'TRUE' TO CC-ACTUAL                                         
           ELSE                                                                 
               SET CC-ACTUAL-88-VALUE TO FALSE                                  
               MOVE 'FALSE' TO CC-ACTUAL                                        
           END-IF                                                               
           SET CC-EXPECTED-88-VALUE TO FALSE                                    
           IF CC-EXPECTED-88-VALUE                                              
               MOVE 'TRUE' TO CC-EXPECTED                                       
           ELSE                                                                 
               MOVE 'FALSE' TO CC-EXPECTED                                      
           END-IF                                                               
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           MOVE "When message type is farewell it returns See you later,        
      -    " alligator!"                                                        
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            SET MESSAGE-IS-FAREWELL TO TRUE                                     
            PERFORM 2000-SPEAK                                                  
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           MOVE WS-FAREWELL TO CC-ACTUAL                                        
           MOVE "See you later, alligator!"                                     
               TO CC-EXPECTED                                                   
           SET CC-COMPARE-DEFAULT TO TRUE                                       
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           MOVE "Message type greeting is not true"                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            SET MESSAGE-IS-GREETING TO TRUE                                     
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-COMPARE-88-LEVEL TO TRUE                                      
           SET CC-REVERSE-COMPARE TO TRUE                                       
           IF MESSAGE-IS-GREETING                                               
               SET CC-ACTUAL-88-VALUE TO TRUE                                   
               MOVE 'TRUE' TO CC-ACTUAL                                         
           ELSE                                                                 
               SET CC-ACTUAL-88-VALUE TO FALSE                                  
               MOVE 'FALSE' TO CC-ACTUAL                                        
           END-IF                                                               
           SET CC-EXPECTED-88-VALUE TO FALSE                                    
           IF CC-EXPECTED-88-VALUE                                              
               MOVE 'TRUE' TO CC-EXPECTED                                       
           ELSE                                                                 
               MOVE 'FALSE' TO CC-EXPECTED                                      
           END-IF                                                               
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Greeting includes the user name when it is provided"        
           MOVE "When message type is greeting it returns Hello, James!"        
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            SET MESSAGE-IS-GREETING TO TRUE                                     
            MOVE "James" TO WS-FRIEND                                           
            PERFORM 2000-SPEAK                                                  
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           MOVE WS-GREETING TO CC-ACTUAL                                        
           MOVE "Hello, James!"                                                 
               TO CC-EXPECTED                                                   
           SET CC-COMPARE-DEFAULT TO TRUE                                       
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           MOVE "When message type is farewell it returns Goodbye, James        
      -    "!"                                                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            SET MESSAGE-IS-FAREWELL TO TRUE                                     
            MOVE "James" TO WS-FRIEND                                           
            PERFORM 2000-SPEAK                                                  
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           MOVE WS-FAREWELL TO CC-ACTUAL                                        
           MOVE "Goodbye, James!"                                               
               TO CC-EXPECTED                                                   
           SET CC-COMPARE-DEFAULT TO TRUE                                       
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
           MOVE "User name for greeting and farewell are consistent"            
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            SET MESSAGE-IS-GREETING TO TRUE                                     
            MOVE "Henry" TO WS-FRIEND                                           
            PERFORM 2000-SPEAK                                                  
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           MOVE WS-USER-NAME OF WS-GREETING TO CC-ACTUAL                        
           MOVE "Henry"                                                         
               TO CC-EXPECTED                                                   
           SET CC-COMPARE-DEFAULT TO TRUE                                       
           PERFORM CC-ASSERT-EQUAL                                              
           PERFORM CC-AFTER                                                     
      * ZUTZCPD.CPY                                                             
           DISPLAY SPACE                                                        
           MOVE CC-TEST-CASE-COUNT TO CC-TEST-CASE-NUMBER                       
           DISPLAY CC-TEST-CASE-NUMBER ' TEST CASES WERE EXECUTED'              
           MOVE CC-NUMBER-PASSED TO CC-TEST-CASE-NUMBER                         
           DISPLAY CC-TEST-CASE-NUMBER ' PASSED'                                
           MOVE CC-NUMBER-FAILED TO CC-TEST-CASE-NUMBER                         
           DISPLAY CC-TEST-CASE-NUMBER ' FAILED'                                
           DISPLAY "================================================="          
           MOVE CC-RETCODE TO RETURN-CODE                                       
           GOBACK.                                                              
                                                                                
       CC-ASSERT-EQUAL.                                                         
      *****************************************************************         
      * COMPARE EXPECTED AND ACTUAL VALUES FOR EQUALITY.                        
      *****************************************************************         
           MOVE CC-TEST-CASE-COUNT TO CC-TEST-CASE-NUMBER                       
           PERFORM CC-COMPARE                                                   
           IF CC-COMPARE-PASSED                                                 
               PERFORM CC-DISPLAY-PASSED                                        
           ELSE                                                                 
               PERFORM CC-DISPLAY-FAILED                                        
           END-IF                                                               
           .                                                                    
                                                                                
       CC-COMPARE.                                                              
           SET CC-COMPARE-FAILED TO TRUE                                        
           IF CC-COMPARE-NUMERIC                                                
               IF CC-ACTUAL-NUMERIC IS EQUAL TO CC-EXPECTED-NUMERIC             
                   SET CC-COMPARE-PASSED TO TRUE                                
               END-IF                                                           
           ELSE                                                                 
               IF CC-COMPARE-88-LEVEL                                           
                   IF (CC-ACTUAL-88-VALUE                                       
                       AND CC-EXPECTED-88-VALUE)                                
                   OR (NOT CC-ACTUAL-88-VALUE                                   
                       AND NOT CC-EXPECTED-88-VALUE)                            
                       SET CC-COMPARE-PASSED TO TRUE                            
                   END-IF                                                       
               ELSE                                                             
                   IF CC-ACTUAL IS EQUAL TO CC-EXPECTED                         
                       SET CC-COMPARE-PASSED TO TRUE                            
                   END-IF                                                       
               END-IF                                                           
           END-IF                                                               
           PERFORM CC-REVERSE-RESULT                                            
           .                                                                    
                                                                                
       CC-REVERSE-RESULT.                                                       
           IF CC-REVERSE-COMPARE                                                
               IF CC-COMPARE-PASSED                                             
                   SET CC-COMPARE-FAILED TO TRUE                                
               ELSE                                                             
                   SET CC-COMPARE-PASSED TO TRUE                                
               END-IF                                                           
           END-IF                                                               
           .                                                                    
                                                                                
       CC-DISPLAY-PASSED.                                                       
           ADD 1 TO CC-NUMBER-PASSED                                            
           DISPLAY CC-PASSED                                                    
                   CC-TEST-CASE-NUMBER '. '                                     
                   CC-TEST-CASE-NAME                                            
           .                                                                    
                                                                                
       CC-DISPLAY-FAILED.                                                       
           ADD 1 TO CC-NUMBER-FAILED                                            
           DISPLAY CC-FAILED                                                    
                   CC-TEST-CASE-NUMBER '. '                                     
                   CC-TEST-CASE-NAME                                            
           IF CC-COMPARE-NUMERIC                                                
               DISPLAY '    EXPECTED ' CC-EXPECTED-NUMERIC                      
                       ', WAS ' CC-ACTUAL-NUMERIC                               
           ELSE                                                                 
               MOVE ZERO TO CC-EXPECTED-TRIM                                    
               INSPECT FUNCTION REVERSE(CC-EXPECTED)                            
                   TALLYING CC-EXPECTED-TRIM                                    
                   FOR LEADING SPACES                                           
               SUBTRACT CC-EXPECTED-TRIM                                        
                   FROM LENGTH OF CC-EXPECTED                                   
                   GIVING CC-EXPECTED-TRIM                                      
               MOVE ZERO TO CC-ACTUAL-TRIM                                      
               INSPECT FUNCTION REVERSE(CC-ACTUAL)                              
                   TALLYING CC-ACTUAL-TRIM                                      
                   FOR LEADING SPACES                                           
               SUBTRACT CC-ACTUAL-TRIM                                          
                   FROM LENGTH OF CC-ACTUAL                                     
                   GIVING CC-ACTUAL-TRIM                                        
                                                                                
               DISPLAY '    EXPECTED <'                                         
                   CC-EXPECTED(1:CC-EXPECTED-TRIM)                              
                       '>, WAS <' CC-ACTUAL(1:CC-ACTUAL-TRIM) '>'               
           END-IF                                                               
           MOVE 4 TO CC-RETCODE                                                 
           .                                                                    
                                                                                
       CC-ASSERT-ACCESSES.                                                      
      *****************************************************************         
      * COMPARE THE NUMBER OF ACCESSES TO A MOCK WITH THE EXPECTED              
      * NUMBER OF ACCESSES.                                                     
      *****************************************************************         
           MOVE CC-TEST-CASE-COUNT TO CC-TEST-CASE-NUMBER                       
           MOVE CC-ACTUAL-ACCESSES TO CC-ACTUAL-ACCESSES-FMT                    
           MOVE CC-EXPECTED-ACCESSES TO CC-EXPECTED-ACCESSES-FMT                
           IF CC-EXPECTED-ACCESSES IS EQUAL TO 1                                
              MOVE SPACES TO CC-LABEL-EXPECTED-ACCESS-PL                        
           ELSE                                                                 
              MOVE CC-CONST-ES TO CC-LABEL-EXPECTED-ACCESS-PL                   
           END-IF                                                               
                                                                                
           SET CC-VERIFY-FAILED TO TRUE                                         
           EVALUATE TRUE                                                        
               WHEN CC-VERIFY-AT-LEAST                                          
                    IF CC-ACTUAL-ACCESSES IS GREATER THAN OR EQUAL TO           
                            CC-EXPECTED-ACCESSES                                
                        SET CC-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
               WHEN CC-VERIFY-NO-MORE-THAN                                      
                    IF CC-ACTUAL-ACCESSES IS LESS THAN OR EQUAL TO              
                            CC-EXPECTED-ACCESSES                                
                        SET CC-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
               WHEN OTHER                                                       
                    IF CC-ACTUAL-ACCESSES IS EQUAL TO                           
                            CC-EXPECTED-ACCESSES                                
                        SET CC-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
           END-EVALUATE                                                         
                                                                                
           IF CC-VERIFY-PASSED                                                  
               ADD 1 TO CC-NUMBER-PASSED                                        
               DISPLAY CC-PASSED                                                
                       CC-TEST-CASE-NUMBER '. '                                 
                      'VERIFY ' CC-EXPECTED-ACCESSES-FMT SPACE                  
                      CC-LABEL-EXPECTED-ACCESS                                  
           ELSE                                                                 
               ADD 1 TO CC-NUMBER-FAILED                                        
               MOVE SPACES TO CC-DISPLAY-MESSAGE                                
               IF CC-MOCK-FILE(CC-MOCK-IX)                                      
                   STRING                                                       
                       CC-FAILED                      DELIMITED BY SIZE         
                       CC-TEST-CASE-NUMBER            DELIMITED BY SIZE         
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE         
                       CC-MOCK-OPERATION(CC-MOCK-IX)  DELIMITED BY SPACE        
                       ' ON '                         DELIMITED BY SIZE         
                       CC-MOCK-FILENAME(CC-MOCK-IX)   DELIMITED BY SPACE        
                       ' | EXPECTED '                 DELIMITED BY SIZE         
                       CC-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE         
                       SPACE                          DELIMITED BY SIZE         
                       CC-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE        
                       ', WAS '                       DELIMITED BY SIZE         
                       CC-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE         
                       INTO CC-DISPLAY-MESSAGE                                  
                   END-STRING                                                   
               ELSE                                                             
                   STRING                                                       
                       CC-FAILED                      DELIMITED BY SIZE         
                       CC-TEST-CASE-NUMBER            DELIMITED BY SIZE         
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE         
                       CC-MOCK-CICS-KEYWORDS-KEY(CC-MOCK-IX)                    
                                                      DELIMITED BY SIZE         
                       INTO CC-DISPLAY-MESSAGE                                  
                   END-STRING                                                   
                   DISPLAY CC-DISPLAY-MESSAGE                                   
                   MOVE SPACES TO CC-DISPLAY-MESSAGE                            
                   STRING                                                       
                       '   EXPECTED '                 DELIMITED BY SIZE         
                       CC-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE         
                       SPACE                          DELIMITED BY SIZE         
                       CC-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE        
                       ', WAS '                       DELIMITED BY SIZE         
                       CC-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE         
                       INTO CC-DISPLAY-MESSAGE                                  
                   END-STRING                                                   
               END-IF                                                           
               DISPLAY CC-DISPLAY-MESSAGE                                       
               MOVE 4 TO CC-RETCODE                                             
           END-IF                                                               
           .                                                                    
                                                                                
       CC-SET-MOCK.                                                             
      *****************************************************************         
      * CREATE OR UPDATE A MOCK SPECIFICATION.                                  
      *****************************************************************         
           EVALUATE TRUE                                                        
               WHEN CC-FIND-FILE-MOCK                                           
                    PERFORM CC-SET-FILE-MOCK                                    
               WHEN CC-FIND-CALL-MOCK                                           
                    PERFORM CC-SET-CALL-MOCK                                    
               WHEN CC-FIND-CICS-MOCK                                           
                    PERFORM CC-SET-CICS-MOCK                                    
               WHEN CC-FIND-PARA-MOCK                                           
                    PERFORM CC-SET-PARA-MOCK                                    
           END-EVALUATE                                                         
           .                                                                    
                                                                                
       CC-SET-FILE-MOCK.                                                        
           PERFORM CC-LOOKUP-MOCK                                               
           IF CC-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO CC-MOCK-COUNT                                           
               SET CC-MOCK-IX TO CC-MOCK-COUNT                                  
               SET CC-MOCK-FILE(CC-MOCK-IX) TO TRUE                             
               MOVE CC-MOCK-FIND-FILENAME                                       
                    TO CC-MOCK-FILENAME(CC-MOCK-IX)                             
               MOVE CC-MOCK-FIND-OPERATION                                      
                    TO CC-MOCK-OPERATION(CC-MOCK-IX)                            
           END-IF                                                               
           MOVE CC-MOCK-SET-RECORD                                              
                TO CC-MOCK-RECORD(CC-MOCK-IX)                                   
           MOVE CC-MOCK-SET-FILE-STATUS                                         
                TO CC-MOCK-FILE-STATUS(CC-MOCK-IX)                              
           .                                                                    
                                                                                
       CC-SET-CALL-MOCK.                                                        
           PERFORM CC-LOOKUP-MOCK                                               
           IF CC-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO CC-MOCK-COUNT                                           
               SET CC-MOCK-IX TO CC-MOCK-COUNT                                  
               MOVE CC-MOCK-FIND-CALL-TOKENS                                    
                   TO CC-MOCK-CALL-TOKENS-KEY(CC-MOCK-IX)                       
           END-IF                                                               
           .                                                                    
                                                                                
       CC-SET-CICS-MOCK.                                                        
           PERFORM CC-LOOKUP-MOCK                                               
           IF CC-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO CC-MOCK-COUNT                                           
               SET CC-MOCK-IX TO CC-MOCK-COUNT                                  
               MOVE CC-MOCK-FIND-CICS-KEYWORDS                                  
                   TO CC-MOCK-CICS-KEYWORDS-KEY(CC-MOCK-IX)                     
           END-IF                                                               
           .                                                                    
                                                                                
       CC-SET-PARA-MOCK.                                                        
           PERFORM CC-LOOKUP-MOCK                                               
           IF CC-MOCK-FOUND                                                     
               CONTINUE                                                         
           ELSE                                                                 
               ADD 1 TO CC-MOCK-COUNT                                           
               SET CC-MOCK-IX TO CC-MOCK-COUNT                                  
               MOVE CC-MOCK-FIND-PARA-NAME                                      
                   TO CC-MOCK-PARA-NAME(CC-MOCK-IX)                             
           END-IF                                                               
           .                                                                    
                                                                                
       CC-LOOKUP-MOCK.                                                          
      *****************************************************************         
      * LOOK UP A MOCK SPECIFICATION.                                           
      *****************************************************************         
           SET CC-MOCK-NOT-FOUND TO TRUE                                        
           PERFORM VARYING CC-MOCK-IX FROM 1 BY 1                               
               UNTIL CC-MOCK-IX IS GREATER THAN CC-MOCK-MAX                     
                  OR CC-MOCK-FOUND                                              
               EVALUATE TRUE                                                    
                   WHEN CC-FIND-FILE-MOCK                                       
                       IF CC-MOCK-FIND-FILENAME IS EQUAL TO                     
                              CC-MOCK-FILENAME(CC-MOCK-IX)                      
                       AND CC-MOCK-FIND-OPERATION IS EQUAL TO                   
                              CC-MOCK-OPERATION(CC-MOCK-IX)                     
                           SET CC-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
                   WHEN CC-FIND-CALL-MOCK                                       
                       IF CC-MOCK-FIND-CALL-TOKENS IS EQUAL TO                  
                              CC-MOCK-CALL-TOKENS-KEY(CC-MOCK-IX)               
                           SET CC-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
                   WHEN CC-FIND-CICS-MOCK                                       
                       IF CC-MOCK-FIND-CICS-KEYWORDS IS EQUAL TO                
                              CC-MOCK-CICS-KEYWORDS-KEY(CC-MOCK-IX)             
                           SET CC-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
                   WHEN CC-FIND-PARA-MOCK                                       
                       IF CC-MOCK-FIND-PARA-NAME IS EQUAL TO                    
                              CC-MOCK-PARA-NAME(CC-MOCK-IX)                     
                           SET CC-MOCK-FOUND TO TRUE                            
                           CONTINUE                                             
                       END-IF                                                   
               END-EVALUATE                                                     
           END-PERFORM                                                          
           SET CC-MOCK-IX DOWN BY 1                                             
           .                                                                    
                                                                                
       CC-LOOKUP-FILE.                                                          
      *****************************************************************         
      * LOOK UP A FILE SPECIFICATION.                                           
      *****************************************************************         
           PERFORM VARYING CC-FILE-IX FROM 1 BY 1                               
               UNTIL CC-FILE-IX GREATER CC-FILE-MAX                             
               OR CC-INTERNAL-FILENAME(CC-FILE-IX)                              
                EQUAL CC-MOCK-FIND-FILENAME                                     
             CONTINUE                                                           
           END-PERFORM                                                          
           SET CC-FILE-IX DOWN BY 1                                             
           .                                                                    
       CC-BEFORE.                                                               
           .                                                                    
       CC-AFTER.                                                                
           .                                                                    
       CC-INITIALIZE.                                                           
           MOVE SPACES TO CC-FILE-INFORMATION                                   
                                                                                
           .                                                                    
       CC-END.                                                                  
                                                                                
      * ZUTZCPD.CPY END                                                         
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
