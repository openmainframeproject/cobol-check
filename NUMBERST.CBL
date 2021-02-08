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
           05  ws-field-1           PIC S9(11)V9(07) COMP-3.                    
           05  WS-FIELD-2           PIC S9(11)V9(07) comp-3.                    
           05  ws-field-3           pic s9(16) comp.                            
           05  ws-field-4           pic s9(16) comp-4.                          
           05  ws-display-field     pic s9(5)v99.                               
       PROCEDURE DIVISION.                                                      
           PERFORM UT-INITIALIZE                                                
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Verify Cobol Check handles numeric relations properl        
      -    "y"                                                                  
           MOVE "Equal sign with literal compare"                               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 25.74 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Equal sign with literal compare (should fail)"                 
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 25.75 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not equal sign with literal compare"                           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 25.75 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not equal sign with literal compare (should fail)"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 25.74 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not-equal sign with literal compare"                           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 13.6 TO WS-FIELD-1                                             
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 25.74 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not-equal sign with literal compare (should fail)"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 13.6 TO WS-FIELD-1                                             
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 13.6 TO UT-EXPECTED-NUMERIC                                     
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not not-equal sign with literal compare"                       
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 13.6 TO WS-FIELD-1                                             
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 13.6 TO UT-EXPECTED-NUMERIC                                     
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not not-equal sign with literal compare (should fail)"         
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 13.7 TO WS-FIELD-1                                             
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 13.6 TO UT-EXPECTED-NUMERIC                                     
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Equal sign with ield compare"                                  
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Equal sign with field compare (should fail)"                   
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.75 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not equal sign with field compare"                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.75 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not equal sign with field compare (should fail)"               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not-equal sign with field compare"                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.75 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not-equal sign with field compare (should fail)"               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not not-equal sign with field compare"                         
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.74 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not not-equal sign with field compare (should fail)"           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 25.75 TO WS-FIELD-1                                            
            MOVE 25.74 TO WS-FIELD-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than sign with literal compare"                           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.068 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than sign with literal compare (should fail)"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.066 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not less-than sign with literal compare"                       
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.068 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.068 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not less-than sign with literal compare (should fail)"         
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.066 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than sign with field compare"                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 416.071 TO WS-FIELD-1                                          
            MOVE 416.072 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than sign with field compare (should fail)"               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 416.072 TO WS-FIELD-1                                          
            MOVE 416.072 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not less-than sign with field compare"                         
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 416.072 TO WS-FIELD-1                                          
            MOVE 416.071 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not less-than sign with field compare (should fail)"           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 416.071 TO WS-FIELD-1                                          
            MOVE 416.072 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than sign with literal compare"                        
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 17.01 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than sign with literal compare (should fail)"          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 9.805 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 10 TO UT-EXPECTED-NUMERIC                                       
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than sign with literal compare"                    
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 107.701 TO WS-FIELD-1                                          
            MOVE 107.701 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than sign with literal compare (should fail        
      -    ")"                                                                  
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 107.702 TO WS-FIELD-1                                          
            MOVE 107.701 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than sign with field compare"                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 1766.03145 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than sign with field compare (should fail)"            
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 1766.03143 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than sign with field to field compare"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 1766.03144 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than sign with field compare (should fail)"        
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 1766.03145 TO WS-FIELD-1                                       
            MOVE 1766.03144 TO WS-FIELD-2                                       
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare when        
      -    " greater"                                                           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.066 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare when        
      -    " equal"                                                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare (sho        
      -    "uld fail)"                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 9.805 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 10 TO UT-EXPECTED-NUMERIC                                       
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to sign with literal compare         
      -    "when less"                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.066 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to sign with literal compare         
      -    "when equal (should fail)"                                           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 18.067 TO WS-FIELD-1                                           
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 18.067 TO UT-EXPECTED-NUMERIC                                   
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to sign with literal compare when        
      -    " greater (should fail)"                                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 13.45 TO WS-FIELD-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE 13.44 TO UT-EXPECTED-NUMERIC                                    
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to-sign with field compare when e        
      -    "qual"                                                               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to-sign with field compare when g        
      -    "reater"                                                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Greater-than-or-equal-to-sign with field compare when l        
      -    "ess (should fail)"                                                  
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.061 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en less"                                                            
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.063 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en equal (should fail)"                                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en greater (should fail)"                                           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-LT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than-or-equal-to-sign with field compare when equa        
      -    "l"                                                                  
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than-or-equal-to-sign with field compare when less        
      -    ""                                                                   
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.063 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Less-than-or-equal-to-sign with field compare when grea        
      -    "ter (should fail)"                                                  
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-REVERSE-COMPARE TO TRUE                                       
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not less-than-or-equal-to-sign with field compare when         
      -    "greater"                                                            
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.063 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en equal (should fail)"                                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.062 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Not greater-than-or-equal-to-sign with field compare wh        
      -    "en less (should fail)"                                              
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 475.062 TO WS-FIELD-1                                          
            MOVE 475.063 TO WS-FIELD-2                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-FIELD-1 TO UT-ACTUAL-NUMERIC                                 
           MOVE WS-FIELD-2 TO UT-EXPECTED-NUMERIC                               
           SET UT-RELATION-GT TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           PERFORM UT-AFTER                                                     
           MOVE "Display Numeric field equals literal"                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE                                                    
            MOVE 123.45 TO WS-DISPLAY-FIELD                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-NUMERIC-COMPARE TO TRUE                                       
           MOVE WS-DISPLAY-FIELD TO UT-ACTUAL-NUMERIC                           
           MOVE 123.45 TO UT-EXPECTED-NUMERIC                                   
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
           GOBACK.                                                              
