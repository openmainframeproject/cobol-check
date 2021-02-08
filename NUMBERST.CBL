       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  NUMBERS.                                                    
      *****************************************************************         
      * Program to exercise symbolic relations like ">=" and "!=".              
      *****************************************************************         
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
      * CCHECKWS.CPY                                                            
       01  FILLER PIC X(32) VALUE '******* COBOLCHECK 0.0.1 *******'.           
       01  CC-FIELDS.                                                           
           05  CC-CONST-ES               PIC X(02) VALUE 'ES'.                  
           05  CC-LABEL-EXPECTED-ACCESS.                                        
               10  FILLER                PIC X(06) VALUE 'ACCESS'.              
               10  CC-LABEL-EXPECTED-ACCESS-PL                                  
                                         PIC X(02) VALUE SPACES.                
           05  CC-DISPLAY-MESSAGE     PIC X(256) VALUE SPACES.                  
           05  FILLER                    PIC X(02) VALUE SPACES.                
               88  CC-RELATION-EQ               VALUE SPACES, 'EQ'.             
               88  CC-RELATION-GE               VALUE 'GE'.                     
               88  CC-RELATION-GT               VALUE 'GT'.                     
               88  CC-RELATION-LE               VALUE 'LE'.                     
               88  CC-RELATION-LT               VALUE 'LT'.                     
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-NORMAL-COMPARE            VALUE 'N'.                      
               88  CC-REVERSE-COMPARE           VALUE 'Y'.                      
           05  FILLER                    PIC X(01) VALUE '1'.                   
               88  CC-VERIFY-EXACT              VALUE '1'.                      
               88  CC-VERIFY-AT-LEAST           VALUE '2'.                      
               88  CC-VERIFY-NO-MORE-THAN       VALUE '3'.                      
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-VERIFY-PASSED      VALUE 'Y'.                             
               88  CC-VERIFY-FAILED      VALUE SPACES.                          
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-ALPHANUMERIC-COMPARE VALUE SPACE.                         
               88  CC-NUMERIC-COMPARE    VALUE 'N'.                             
               88  CC-COMPARE-88-LEVEL   VALUE '8'.                             
           05  FILLER                    PIC X(01) VALUE SPACES.                
               88  CC-COMPARE-PASSED     VALUE 'Y'.                             
               88  CC-COMPARE-FAILED     VALUE SPACES.                          
           05  CC-EXPECTED               PIC X(256) VALUE SPACES.               
           05  CC-ACTUAL                 PIC X(256) VALUE SPACES.               
           05  CC-EXPECTED-NUMERIC-X.                                           
               10  CC-EXPECTED-NUMERIC   PIC S9(11)V9(7)                        
                                            COMP-3 VALUE ZERO.                  
           05  CC-ACTUAL-NUMERIC-X.                                             
               10  CC-ACTUAL-NUMERIC     PIC S9(11)V9(7)                        
                                            COMP-3 VALUE ZERO.                  
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
                                                                                
      * CCHECKWS.CPY END                                                        
       01  FILLER.                                                              
           05  ws-field-1           PIC S9(11)V9(07) COMP-3.                    
           05  WS-FIELD-2           PIC S9(11)V9(07) comp-3.                    
           05  ws-field-3           pic s9(16) comp.                            
           05  ws-field-4           pic s9(16) comp-4.                          
           05  ws-display-field     pic s9(5)v99.                               
       PROCEDURE DIVISION.                                                      
           PERFORM CC-INITIALIZE                                                
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Verify Cobol Check handles numeric relations properl        
      -    "y"                                                                  
           MOVE "Equal sign with literal compare"                               
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 25.74 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Equal sign with literal compare (should fail)"                 
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 25.75 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not equal sign with literal compare"                           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 25.75 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not equal sign with literal compare (should fail)"             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 25.74 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not-equal sign with literal compare"                           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 13.6 TO WS-FIELD-1                                             
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 25.74 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not-equal sign with literal compare (should fail)"             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 13.6 TO WS-FIELD-1                                             
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 13.6 TO CC-EXPECTED-NUMERIC                                     
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not not-equal sign with literal compare"                       
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 13.6 TO WS-FIELD-1                                             
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 13.6 TO CC-EXPECTED-NUMERIC                                     
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not not-equal sign with literal compare (should fail)"         
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 13.7 TO WS-FIELD-1                                             
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 13.6 TO CC-EXPECTED-NUMERIC                                     
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Equal sign with ield compare"                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Equal sign with field compare (should fail)"                   
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.75 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not equal sign with field compare"                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.75 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not equal sign with field compare (should fail)"               
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not-equal sign with field compare"                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.75 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not-equal sign with field compare (should fail)"               
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not not-equal sign with field compare"                         
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not not-equal sign with field compare (should fail)"           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 25.75 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than sign with literal compare"                           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.068 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than sign with literal compare (should fail)"             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.066 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not less-than sign with literal compare"                       
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.068 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.068 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not less-than sign with literal compare (should fail)"         
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.066 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than sign with field compare"                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 416.071 TO WS-FIELD-1                                          
            MOVE 416.072 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than sign with field compare (should fail)"               
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 416.072 TO WS-FIELD-1                                          
            MOVE 416.072 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not less-than sign with field compare"                         
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 416.072 TO WS-FIELD-1                                          
            MOVE 416.071 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not less-than sign with field compare (should fail)"           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 416.071 TO WS-FIELD-1                                          
            MOVE 416.072 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than sign with literal compare"                        
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 17.01 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than sign with literal compare (should fail)"          
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 9.805 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 10 TO CC-EXPECTED-NUMERIC                                       
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than sign with literal compare"                    
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 107.701 TO WS-FIELD-1                                          
            MOVE 107.701 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than sign with literal compare (should fail        
      -    ")"                                                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 107.702 TO WS-FIELD-1                                          
            MOVE 107.701 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than sign with field compare"                          
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 1766.03145 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than sign with field compare (should fail)"            
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 1766.03143 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than sign with field to field compare"             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 1766.03144 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than sign with field compare (should fail)"        
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 1766.03145 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare when        
      -    " greater"                                                           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.066 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare when        
      -    " equal"                                                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare (sho        
      -    "uld fail)"                                                          
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 9.805 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 10 TO CC-EXPECTED-NUMERIC                                       
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to sign with literal compare         
      -    "when less"                                                          
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.066 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to sign with literal compare         
      -    "when equal (should fail)"                                           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare when        
      -    " greater (should fail)"                                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 13.45 TO WS-FIELD-1                                            
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE 13.44 TO CC-EXPECTED-NUMERIC                                    
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to-sign with field compare when e        
      -    "qual"                                                               
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to-sign with field compare when g        
      -    "reater"                                                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Greater-than-or-equal-to-sign with field compare when l        
      -    "ess (should fail)"                                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.061 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en less"                                                            
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.063 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en equal (should fail)"                                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en greater (should fail)"                                           
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-LT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than-or-equal-to-sign with field compare when equa        
      -    "l"                                                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than-or-equal-to-sign with field compare when less        
      -    ""                                                                   
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.063 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Less-than-or-equal-to-sign with field compare when grea        
      -    "ter (should fail)"                                                  
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-REVERSE-COMPARE TO TRUE                                       
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not less-than-or-equal-to-sign with field compare when         
      -    "greater"                                                            
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en equal (should fail)"                                             
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en less (should fail)"                                              
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.063 TO WS-FIELD-2                                          
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO CC-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO CC-EXPECTED-NUMERIC                               
           SET CC-RELATION-GT TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
           MOVE "Display Numeric field equals literal"                          
               TO CC-TEST-CASE-NAME                                             
           PERFORM CC-BEFORE                                                    
            MOVE 123.45 TO WS-DISPLAY-FIELD                                     
           ADD 1 TO CC-TEST-CASE-COUNT                                          
           SET CC-NORMAL-COMPARE TO TRUE                                        
           SET CC-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-DISPLAY-FIELD TO CC-ACTUAL-NUMERIC                           
           MOVE 123.45 TO CC-EXPECTED-NUMERIC                                   
           SET CC-RELATION-EQ TO TRUE                                           
           PERFORM CC-CHECK-EXPECTATION                                         
           PERFORM CC-AFTER                                                     
      * CCHECKPD.CPY                                                            
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
                                                                                
       CC-CHECK-EXPECTATION.                                                    
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
           IF CC-COMPARE-88-LEVEL                                               
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
           ELSE                                                                 
               EVALUATE TRUE ALSO TRUE                                          
                   WHEN CC-NUMERIC-COMPARE ALSO CC-RELATION-EQ                  
                       IF CC-ACTUAL-NUMERIC                                     
                               EQUAL CC-EXPECTED-NUMERIC                        
                           SET CC-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN CC-NUMERIC-COMPARE ALSO CC-RELATION-GT                  
                       IF CC-ACTUAL-NUMERIC                                     
                               GREATER CC-EXPECTED-NUMERIC                      
                           SET CC-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN CC-NUMERIC-COMPARE ALSO CC-RELATION-GE                  
                       IF CC-ACTUAL-NUMERIC                                     
                               GREATER OR EQUAL CC-EXPECTED-NUMERIC             
                           SET CC-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN CC-NUMERIC-COMPARE ALSO CC-RELATION-LT                  
                       IF CC-ACTUAL-NUMERIC                                     
                               LESS CC-EXPECTED-NUMERIC                         
                           SET CC-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN CC-NUMERIC-COMPARE ALSO CC-RELATION-LE                  
                       IF CC-ACTUAL-NUMERIC                                     
                               LESS OR EQUAL CC-EXPECTED-NUMERIC                
                           SET CC-COMPARE-PASSED TO TRUE                        
                       END-IF                                                   
                   WHEN CC-ALPHANUMERIC-COMPARE ALSO CC-RELATION-EQ             
                       IF CC-ACTUAL                                             
                               EQUAL CC-EXPECTED                                
                           SET CC-COMPARE-PASSED TO TRUE                        
                        END-IF                                                  
                    WHEN CC-ALPHANUMERIC-COMPARE ALSO CC-RELATION-GT            
                        IF CC-ACTUAL                                            
                                GREATER CC-EXPECTED                             
                            SET CC-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN CC-ALPHANUMERIC-COMPARE ALSO CC-RELATION-GE            
                        IF CC-ACTUAL                                            
                                GREATER OR EQUAL CC-EXPECTED                    
                            SET CC-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN CC-ALPHANUMERIC-COMPARE ALSO CC-RELATION-LT            
                        IF CC-ACTUAL                                            
                                LESS CC-EXPECTED                                
                            SET CC-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN CC-ALPHANUMERIC-COMPARE ALSO CC-RELATION-LE            
                        IF CC-ACTUAL                                            
                                LESS OR EQUAL CC-EXPECTED                       
                            SET CC-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
                    WHEN OTHER                                                  
                        IF CC-ACTUAL EQUAL CC-EXPECTED                          
                            SET CC-COMPARE-PASSED TO TRUE                       
                        END-IF                                                  
               END-EVALUATE                                                     
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
           IF CC-NUMERIC-COMPARE                                                
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
                                                                                
      * CCHECKPD.CPY END                                                        
           GOBACK.                                                              
