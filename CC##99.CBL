       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  ALPHA.                                                      
      *****************************************************************         
      * Program to exercise EXPECT statements.                                  
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
           05  UT-LABEL-VERIFY-COMPARE                                          
                                         PIC X(12) VALUE SPACES.                
           05  UT-VERIFY-COMPARE-STRINGS.                                       
               10  UT-VERIFY-EXACT-STRING                                       
                                         PIC X(05) VALUE 'EXACT'.               
               10  UT-VERIFY-AT-LEAST-STRING                                    
                                         PIC X(08) VALUE 'AT LEAST'.            
               10  UT-VERIFY-NO-MORE-THAN-STRING                                
                                         PIC X(12) VALUE 'NO MORE THAN'.        
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
           05  UT-TEST-SUITE-NAME        PIC X(80)  VALUE SPACES.               
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
           05  UT-MOCK-OPERATION         PIC X(30).                             
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
           05  VALUE-1           PIC X(80).                                     
           05  VALUE-2           PIC X(80).                                     
           05  VALUE-3           PIC X(80).                                     
           05  TEMP              PIC X(80).                                     
                                                                                
       01  UT-MOCKS-GENERATED.                                                  
           05  UT-2-0-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-0-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-0-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-2-0-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-0-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-0-2-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 100-WELCOME".                                 
           05  UT-2-2-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-2-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-2-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-2-2-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-2-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-2-2-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 100-WELCOME".                                 
           05  UT-2-3-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-3-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-3-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-2-3-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-3-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-3-2-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 100-WELCOME".                                 
           05  UT-2-4-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-4-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-4-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-2-4-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-2-4-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-2-4-2-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 100-WELCOME".                                 
           05  UT-5-0-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-0-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-0-1-MOCK-NAME        PIC X(40)                              
                   VALUE "CALL 'PROG1'".                                        
           05  UT-5-1-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-1-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-1-1-MOCK-NAME        PIC X(40)                              
                   VALUE "CALL 'PROG1'".                                        
           05  UT-5-3-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-3-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-3-1-MOCK-NAME        PIC X(40)                              
                   VALUE "CALL VALUE-2".                                        
           05  UT-5-4-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-4-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-4-1-MOCK-NAME        PIC X(40)                              
                   VALUE "CALL 'PROG3'".                                        
           05  UT-5-5-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-5-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-5-1-MOCK-NAME        PIC X(40)                              
                   VALUE "CALL 'PROG3'".                                        
           05  UT-5-5-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-5-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-5-2-MOCK-NAME        PIC X(40)                              
                   VALUE "PARAGRAPH 800-MAKE-CALL".                             
           05  UT-5-6-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-5-6-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-5-6-1-MOCK-NAME        PIC X(40)                              
                   VALUE "CALL 'PROG1'".                                        
           05  UT-6-0-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-0-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-0-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-6-0-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-0-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-0-2-MOCK-NAME        PIC X(40)                              
                   VALUE "PARAGRAPH 300-CHANGE-1".                              
           05  UT-6-2-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-2-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-2-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-6-3-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-3-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-3-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 000-START".                                   
           05  UT-6-3-2-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-3-2-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-3-2-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 100-WELCOME".                                 
           05  UT-6-3-3-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-3-3-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-3-3-MOCK-NAME        PIC X(40)                              
                   VALUE "PARAGRAPH 500-SWITCH".                                
           05  UT-6-4-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-4-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-4-1-MOCK-NAME        PIC X(40)                              
                   VALUE "PARAGRAPH 500-SWITCH".                                
           05  UT-6-5-1-MOCK-COUNT       PIC 9(02) VALUE ZERO.                  
           05  UT-6-5-1-MOCK-EXPECTED    PIC 9(02) VALUE ZERO.                  
           05  UT-6-5-1-MOCK-NAME        PIC X(40)                              
                   VALUE "SECTION 100-WELCOME".                                 
       PROCEDURE DIVISION.                                                      
           PERFORM UT-INITIALIZE                                                
      *============= "Before and after tests 1" =============*                  
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Before and after tests 1"                                   
           MOVE "Before and after tests 1"                                      
               TO UT-TEST-SUITE-NAME                                            
      *-------- "Before sets value"                                             
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Before sets value"                                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "prepare"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "After sets value"                                              
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "After sets value"                                              
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "clean"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Before happens before Testcase"                                
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Before happens before Testcase"                                
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            MOVE "during" TO VALUE-1                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "during"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "After happens after Testcase (part 1)"                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "After happens after Testcase (part 1)"                         
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            MOVE "during" TO VALUE-2                                            
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "during"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "After happens after Testcase (part 2)"                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "After happens after Testcase (part 2)"                         
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "clean"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *============= "Before and after tests 2" =============*                  
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Before and after tests 2"                                   
           MOVE "Before and after tests 2"                                      
               TO UT-TEST-SUITE-NAME                                            
      *-------- "Performs of mocked items does not influence count"             
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Performs of mocked items does not influence count"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           MOVE 0 TO UT-2-0-1-MOCK-EXPECTED                                     
           MOVE UT-2-0-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-2-0-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-2-0-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE 0 TO UT-2-0-2-MOCK-EXPECTED                                     
           MOVE UT-2-0-2-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-2-0-2-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-2-0-2-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Only global mocks apply in before/after (part 1)"              
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Only global mocks apply in before/after (part 1)"              
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            MOVE "arg3" TO VALUE-3                                              
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "GlobalMockedSection1"                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "before2"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-3 TO UT-ACTUAL                                            
           MOVE "arg3"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Only global mocks apply in before/after (part 2)"              
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Only global mocks apply in before/after (part 2)"              
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "GlobalMockedSection1"                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "before2"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-3 TO UT-ACTUAL                                            
           MOVE "GlobalMockedSection2"                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Only global mocks apply in before/after (part 3)"              
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Only global mocks apply in before/after (part 3)"              
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "GlobalMockedSection1"                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "before2"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-3 TO UT-ACTUAL                                            
           MOVE "GlobalMockedSection2"                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *============= "Before and after tests 3" =============*                  
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Before and after tests 3"                                   
           MOVE "Before and after tests 3"                                      
               TO UT-TEST-SUITE-NAME                                            
      *-------- "Before proceeds after from last testcase (part 1)"             
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Before proceeds after from last testcase (part 1)"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "before"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Before proceeds after from last testcase (part 2)"             
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Before proceeds after from last testcase (part 2)"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "before"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *============= "General tests" =============*                             
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "General tests"                                              
           MOVE "General tests"                                                 
               TO UT-TEST-SUITE-NAME                                            
      *-------- "Welcome section performs as intended"                          
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Welcome section performs as intended"                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Goodbye section performs as intended"                          
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Goodbye section performs as intended"                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 200-GOODBYE                                                 
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Bye"                                                           
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "See you"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Change-1 changes hello to bye"                                 
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Change-1 changes hello to bye"                                 
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
            PERFORM 300-CHANGE-1                                                
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Bye"                                                           
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Change-1 changes bye to hello"                                 
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Change-1 changes bye to hello"                                 
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 200-GOODBYE                                                 
            PERFORM 300-CHANGE-1                                                
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "See you"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Change-2 changes hi to see you"                                
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Change-2 changes hi to see you"                                
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
            PERFORM 400-CHANGE-2                                                
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "See you"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Change-2 changes see you to hi"                                
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Change-2 changes see you to hi"                                
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 200-GOODBYE                                                 
            PERFORM 400-CHANGE-2                                                
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Bye"                                                           
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Switches values"                                               
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Switches values"                                               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
            PERFORM 500-SWITCH                                                  
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *============= "Mock Call statements test" =============*                 
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Mock Call statements test"                                  
           MOVE "Mock Call statements test"                                     
               TO UT-TEST-SUITE-NAME                                            
      *-------- "Simple call mock works"                                        
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Simple call mock works"                                        
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 600-MAKE-CALL                                               
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "From mocked PROG1"                                             
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "arg2"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 1 TO UT-5-1-1-MOCK-EXPECTED                                     
           MOVE UT-5-1-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-5-1-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-5-1-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Simple global call mock works"                                 
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Simple global call mock works"                                 
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 600-MAKE-CALL                                               
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Global PROG1"                                                  
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "arg2"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 1 TO UT-5-0-1-MOCK-EXPECTED                                     
           MOVE UT-5-0-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-5-0-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-5-0-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Call mock with argument works"                                 
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Call mock with argument works"                                 
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 700-MAKE-CALL                                               
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "From mocked PROG2"                                             
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "arg2"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 1 TO UT-5-3-1-MOCK-EXPECTED                                     
           MOVE UT-5-3-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-5-3-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-5-3-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Call mock with content reference for arguments work"           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Call mock with content reference for arguments work"           
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 800-MAKE-CALL                                               
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "content"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "value"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-3 TO UT-ACTUAL                                            
           MOVE "reference"                                                     
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 2 TO UT-5-4-1-MOCK-EXPECTED                                     
           MOVE UT-5-4-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-5-4-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-5-4-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Paragraph mock is called and call mock is ignored"             
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Paragraph mock is called and call mock is ignored"             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            MOVE "arg2" TO VALUE-2                                              
            MOVE "arg3" TO VALUE-3                                              
            PERFORM 800-MAKE-CALL                                               
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "overwritten"                                                   
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "arg2"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-3 TO UT-ACTUAL                                            
           MOVE "arg3"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 1 TO UT-5-5-2-MOCK-EXPECTED                                     
           MOVE UT-5-5-2-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-5-5-2-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-5-5-2-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE 0 TO UT-5-5-1-MOCK-EXPECTED                                     
           MOVE UT-5-5-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-5-5-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-5-5-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Global call mock is not overwritten by local call mock         
      * (Should fail)"                                                          
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Global call mock is not overwritten by local call mock         
      -    "(Should fail)"                                                      
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 600-MAKE-CALL                                               
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Global PROG1"                                                  
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *============= "Mock Sections And Paragraphs" =============*              
           DISPLAY "TESTSUITE:"                                                 
           DISPLAY "Mock Sections And Paragraphs"                               
           MOVE "Mock Sections And Paragraphs"                                  
               TO UT-TEST-SUITE-NAME                                            
      *-------- "Global mock behaves as intended"                               
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Global mock behaves as intended"                               
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 000-START                                                   
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 1 TO UT-6-0-1-MOCK-EXPECTED                                     
           MOVE UT-6-0-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-6-0-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-6-0-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Local mock overwrites global mock"                             
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Local mock overwrites global mock"                             
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 000-START                                                   
            PERFORM 000-START                                                   
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "This is"                                                       
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "a mock"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 2 TO UT-6-2-1-MOCK-EXPECTED                                     
           MOVE UT-6-2-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-6-2-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-6-2-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-AT-LEAST TO TRUE                                       
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Multiple local mocks behaves as intended (First Verify         
      * should fail)"                                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Multiple local mocks behaves as intended (First Verify         
      -    "should fail)"                                                       
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 000-START                                                   
            PERFORM 000-START                                                   
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "I'll leave VALUE-2 alone"                                      
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "a mock"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 5 TO UT-6-3-1-MOCK-EXPECTED                                     
           MOVE UT-6-3-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-6-3-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-6-3-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE 10 TO UT-6-3-2-MOCK-EXPECTED                                    
           MOVE UT-6-3-2-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-6-3-2-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-6-3-2-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-NO-MORE-THAN TO TRUE                                   
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE 0 TO UT-6-3-3-MOCK-EXPECTED                                     
           MOVE UT-6-3-3-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-6-3-3-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-6-3-3-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Empty local mock makes section do nothing"                     
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Empty local mock makes section do nothing"                     
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
            PERFORM 500-SWITCH                                                  
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE 1 TO UT-6-4-1-MOCK-EXPECTED                                     
           MOVE UT-6-4-1-MOCK-COUNT TO UT-ACTUAL-ACCESSES                       
           MOVE UT-6-4-1-MOCK-EXPECTED TO UT-EXPECTED-ACCESSES                  
           MOVE UT-6-4-1-MOCK-NAME TO UT-MOCK-OPERATION                         
           SET UT-VERIFY-EXACT TO TRUE                                          
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           PERFORM UT-ASSERT-ACCESSES                                           
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Local and global mocks can be used together"                   
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Local and global mocks can be used together"                   
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 000-START                                                   
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Mock"                                                          
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Mockity-"                                                      
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "If no local or global mock run source code"                    
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "If no local or global mock run source code"                    
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "Hello"                                                         
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      *-------- "Global paragraph mock works"                                   
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-BEFORE-EACH                                               
           MOVE "Global paragraph mock works"                                   
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-INITIALIZE-MOCK-COUNT                                     
            PERFORM 100-WELCOME                                                 
            PERFORM 300-CHANGE-1                                                
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-1 TO UT-ACTUAL                                            
           MOVE "MOCKED"                                                        
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           ADD 1 TO UT-TEST-CASE-COUNT                                          
           SET UT-NORMAL-COMPARE TO TRUE                                        
           SET UT-ALPHANUMERIC-COMPARE TO TRUE                                  
           MOVE VALUE-2 TO UT-ACTUAL                                            
           MOVE "Hi"                                                            
               TO UT-EXPECTED                                                   
           SET UT-RELATION-EQ TO TRUE                                           
           PERFORM UT-CHECK-EXPECTATION                                         
           MOVE SPACES                                                          
               TO UT-TEST-CASE-NAME                                             
           PERFORM UT-AFTER-EACH                                                
      * CCHECKRESULTPD.CPY                                                      
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
                                                                                
      * CCHECKRESULTPD.CPY END                                                  
       UT-BEFORE-EACH.                                                          
      *****************************************************************         
      *This is performed before each Test Case                                  
      *****************************************************************         
            EVALUATE UT-TEST-SUITE-NAME                                         
                WHEN "Before and after tests 1"                                 
           PERFORM UT-BEFORE-EACH-BRANCH-1                                      
                WHEN "Before and after tests 2"                                 
           PERFORM UT-BEFORE-EACH-BRANCH-2                                      
                WHEN "Before and after tests 3"                                 
           PERFORM UT-BEFORE-EACH-BRANCH-3                                      
            END-EVALUATE                                                        
       .                                                                        
                                                                                
       UT-AFTER-EACH.                                                           
      *****************************************************************         
      *This is performed after each Test Case                                   
      *****************************************************************         
            EVALUATE UT-TEST-SUITE-NAME                                         
                WHEN "Before and after tests 1"                                 
           PERFORM UT-AFTER-EACH-BRANCH-1                                       
                WHEN "Before and after tests 2"                                 
           PERFORM UT-AFTER-EACH-BRANCH-2                                       
                WHEN "Before and after tests 3"                                 
           PERFORM UT-AFTER-EACH-BRANCH-3                                       
            END-EVALUATE                                                        
       .                                                                        
                                                                                
       UT-BEFORE-EACH-BRANCH-1.                                                 
      *****************************************************************         
      *Called before Test Cases in Test Suite:                                  
      *"Before and after tests 1"                                               
      *****************************************************************         
                MOVE "prepare" TO VALUE-1                                       
       .                                                                        
                                                                                
       UT-BEFORE-EACH-BRANCH-2.                                                 
      *****************************************************************         
      *Called before Test Cases in Test Suite:                                  
      *"Before and after tests 2"                                               
      *****************************************************************         
                MOVE "before1" TO VALUE-1                                       
                MOVE "before2" TO VALUE-2                                       
                PERFORM 000-START                                               
       .                                                                        
                                                                                
       UT-BEFORE-EACH-BRANCH-3.                                                 
      *****************************************************************         
      *Called before Test Cases in Test Suite:                                  
      *"Before and after tests 3"                                               
      *****************************************************************         
                MOVE "before" TO VALUE-1                                        
       .                                                                        
                                                                                
       UT-AFTER-EACH-BRANCH-1.                                                  
      *****************************************************************         
      *Called after Test Cases in Test Suite:                                   
      *"Before and after tests 1"                                               
      *****************************************************************         
                MOVE "clean" TO VALUE-2                                         
       .                                                                        
                                                                                
       UT-AFTER-EACH-BRANCH-2.                                                  
      *****************************************************************         
      *Called after Test Cases in Test Suite:                                   
      *"Before and after tests 2"                                               
      *****************************************************************         
                MOVE "clean up" TO VALUE-3                                      
                PERFORM 100-WELCOME                                             
       .                                                                        
                                                                                
       UT-AFTER-EACH-BRANCH-3.                                                  
      *****************************************************************         
      *Called after Test Cases in Test Suite:                                   
      *"Before and after tests 3"                                               
      *****************************************************************         
                MOVE "after" TO VALUE-1                                         
       .                                                                        
                                                                                
       UT-INITIALIZE-MOCK-COUNT.                                                
      *****************************************************************         
      *Sets all global mock counters and expected count to 0                    
      *****************************************************************         
           MOVE 0 TO UT-2-0-1-MOCK-COUNT                                        
           MOVE 0 TO UT-2-0-1-MOCK-EXPECTED                                     
           MOVE 0 TO UT-2-0-2-MOCK-COUNT                                        
           MOVE 0 TO UT-2-0-2-MOCK-EXPECTED                                     
           MOVE 0 TO UT-5-0-1-MOCK-COUNT                                        
           MOVE 0 TO UT-5-0-1-MOCK-EXPECTED                                     
           MOVE 0 TO UT-6-0-1-MOCK-COUNT                                        
           MOVE 0 TO UT-6-0-1-MOCK-EXPECTED                                     
           MOVE 0 TO UT-6-0-2-MOCK-COUNT                                        
           MOVE 0 TO UT-6-0-2-MOCK-EXPECTED                                     
           .                                                                    
                                                                                
      *****************************************************************         
      *Paragraphs called when mocking                                           
      *****************************************************************         
       2-0-1-MOCK.                                                              
      *****************************************************************         
      *Global mock of: SECTION: 000-START                                       
      *In testsuite: "Before and after tests 2"                                 
      *****************************************************************         
           ADD 1 TO UT-2-0-1-MOCK-COUNT                                         
                MOVE "GlobalMockedSection1" TO VALUE-1                          
       .                                                                        
                                                                                
       2-0-2-MOCK.                                                              
      *****************************************************************         
      *Global mock of: SECTION: 100-WELCOME                                     
      *In testsuite: "Before and after tests 2"                                 
      *****************************************************************         
           ADD 1 TO UT-2-0-2-MOCK-COUNT                                         
                MOVE "GlobalMockedSection2" TO VALUE-3                          
       .                                                                        
                                                                                
       2-2-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 000-START                                        
      *In testsuite: "Before and after tests 2"                                 
      *In testcase: "Only global mocks apply in before/after (part 1)"          
      *****************************************************************         
           ADD 1 TO UT-2-2-1-MOCK-COUNT                                         
                MOVE "LocalMockedSection1" TO VALUE-1                           
       .                                                                        
                                                                                
       2-2-2-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 100-WELCOME                                      
      *In testsuite: "Before and after tests 2"                                 
      *In testcase: "Only global mocks apply in before/after (part 1)"          
      *****************************************************************         
           ADD 1 TO UT-2-2-2-MOCK-COUNT                                         
                MOVE "LocalMockedSection2" TO VALUE-3                           
       .                                                                        
                                                                                
       2-3-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 000-START                                        
      *In testsuite: "Before and after tests 2"                                 
      *In testcase: "Only global mocks apply in before/after (part 2)"          
      *****************************************************************         
           ADD 1 TO UT-2-3-1-MOCK-COUNT                                         
                MOVE "LocalMockedSection1" TO VALUE-1                           
       .                                                                        
                                                                                
       2-3-2-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 100-WELCOME                                      
      *In testsuite: "Before and after tests 2"                                 
      *In testcase: "Only global mocks apply in before/after (part 2)"          
      *****************************************************************         
           ADD 1 TO UT-2-3-2-MOCK-COUNT                                         
                MOVE "LocalMockedSection2" TO VALUE-3                           
       .                                                                        
                                                                                
       2-4-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 000-START                                        
      *In testsuite: "Before and after tests 2"                                 
      *In testcase: "Only global mocks apply in before/after (part 3)"          
      *****************************************************************         
           ADD 1 TO UT-2-4-1-MOCK-COUNT                                         
                MOVE "LocalMockedSection1" TO VALUE-1                           
       .                                                                        
                                                                                
       2-4-2-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 100-WELCOME                                      
      *In testsuite: "Before and after tests 2"                                 
      *In testcase: "Only global mocks apply in before/after (part 3)"          
      *****************************************************************         
           ADD 1 TO UT-2-4-2-MOCK-COUNT                                         
                MOVE "LocalMockedSection2" TO VALUE-3                           
       .                                                                        
                                                                                
       5-0-1-MOCK.                                                              
      *****************************************************************         
      *Global mock of: CALL: 'PROG1'                                            
      *In testsuite: "Mock Call statements test"                                
      *****************************************************************         
           ADD 1 TO UT-5-0-1-MOCK-COUNT                                         
                MOVE "Global PROG1" TO VALUE-1                                  
       .                                                                        
                                                                                
       5-1-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: CALL: 'PROG1'                                             
      *In testsuite: "Mock Call statements test"                                
      *In testcase: "Simple call mock works"                                    
      *****************************************************************         
           ADD 1 TO UT-5-1-1-MOCK-COUNT                                         
                MOVE "From mocked PROG1" TO VALUE-1                             
       .                                                                        
                                                                                
       5-3-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: CALL: VALUE-2                                             
      *With args: REFERENCE VALUE-1                                             
      *In testsuite: "Mock Call statements test"                                
      *In testcase: "Call mock with argument works"                             
      *****************************************************************         
           ADD 1 TO UT-5-3-1-MOCK-COUNT                                         
                MOVE "From mocked PROG2" TO VALUE-1                             
       .                                                                        
                                                                                
       5-4-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: CALL: 'PROG3'                                             
      *With args: CONTENT VALUE-1, VALUE VALUE-2, REFERENCE VALUE-3             
      *In testsuite: "Mock Call statements test"                                
      *In testcase: "Call mock with content reference for arguments work        
      * "                                                                       
      *****************************************************************         
           ADD 1 TO UT-5-4-1-MOCK-COUNT                                         
                MOVE "content" TO VALUE-1                                       
                MOVE "value" TO VALUE-2                                         
                MOVE "reference" TO VALUE-3                                     
       .                                                                        
                                                                                
       5-5-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: CALL: 'PROG3'                                             
      *With args: CONTENT VALUE-1, VALUE VALUE-2, REFERENCE VALUE-3             
      *In testsuite: "Mock Call statements test"                                
      *In testcase: "Paragraph mock is called and call mock is ignored"         
      *****************************************************************         
           ADD 1 TO UT-5-5-1-MOCK-COUNT                                         
                MOVE "content" TO VALUE-1                                       
                MOVE "value" TO VALUE-2                                         
                MOVE "reference" TO VALUE-3                                     
       .                                                                        
                                                                                
       5-5-2-MOCK.                                                              
      *****************************************************************         
      *Local mock of: PARAGRAPH: 800-MAKE-CALL                                  
      *In testsuite: "Mock Call statements test"                                
      *In testcase: "Paragraph mock is called and call mock is ignored"         
      *****************************************************************         
           ADD 1 TO UT-5-5-2-MOCK-COUNT                                         
             MOVE "overwritten" TO VALUE-1                                      
       .                                                                        
                                                                                
       5-6-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: CALL: 'PROG1'                                             
      *In testsuite: "Mock Call statements test"                                
      *In testcase: "Global call mock is not overwritten by local call m        
      * ock (Should fail)"                                                      
      *****************************************************************         
           ADD 1 TO UT-5-6-1-MOCK-COUNT                                         
                MOVE "Local PROG1" TO VALUE-1                                   
       .                                                                        
                                                                                
       6-0-1-MOCK.                                                              
      *****************************************************************         
      *Global mock of: SECTION: 000-START                                       
      *In testsuite: "Mock Sections And Paragraphs"                             
      *****************************************************************         
           ADD 1 TO UT-6-0-1-MOCK-COUNT                                         
           PERFORM 100-WELCOME                                                  
           PERFORM 500-SWITCH                                                   
       .                                                                        
                                                                                
       6-0-2-MOCK.                                                              
      *****************************************************************         
      *Global mock of: PARAGRAPH: 300-CHANGE-1                                  
      *In testsuite: "Mock Sections And Paragraphs"                             
      *****************************************************************         
           ADD 1 TO UT-6-0-2-MOCK-COUNT                                         
              EVALUATE VALUE-1                                                  
              WHEN "Hello"                                                      
                MOVE "MOCKED" TO VALUE-1                                        
              WHEN OTHER                                                        
                MOVE "mocked" TO VALUE-1                                        
              END-EVALUATE                                                      
       .                                                                        
                                                                                
       6-2-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 000-START                                        
      *In testsuite: "Mock Sections And Paragraphs"                             
      *In testcase: "Local mock overwrites global mock"                         
      *****************************************************************         
           ADD 1 TO UT-6-2-1-MOCK-COUNT                                         
                MOVE "This is" TO VALUE-1                                       
                MOVE "a mock" TO VALUE-2                                        
       .                                                                        
                                                                                
       6-3-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 000-START                                        
      *In testsuite: "Mock Sections And Paragraphs"                             
      *In testcase: "Multiple local mocks behaves as intended (First Ver        
      * ify should fail)"                                                       
      *****************************************************************         
           ADD 1 TO UT-6-3-1-MOCK-COUNT                                         
                MOVE "This is" TO VALUE-1                                       
                MOVE "a mock" TO VALUE-2                                        
                PERFORM 100-WELCOME                                             
                PERFORM 100-WELCOME                                             
                PERFORM 100-WELCOME                                             
       .                                                                        
                                                                                
       6-3-2-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 100-WELCOME                                      
      *In testsuite: "Mock Sections And Paragraphs"                             
      *In testcase: "Multiple local mocks behaves as intended (First Ver        
      * ify should fail)"                                                       
      *****************************************************************         
           ADD 1 TO UT-6-3-2-MOCK-COUNT                                         
                MOVE "I'll leave VALUE-2 alone" TO VALUE-1                      
       .                                                                        
                                                                                
       6-3-3-MOCK.                                                              
      *****************************************************************         
      *Local mock of: PARAGRAPH: 500-SWITCH                                     
      *In testsuite: "Mock Sections And Paragraphs"                             
      *In testcase: "Multiple local mocks behaves as intended (First Ver        
      * ify should fail)"                                                       
      *****************************************************************         
           ADD 1 TO UT-6-3-3-MOCK-COUNT                                         
       .                                                                        
                                                                                
       6-4-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: PARAGRAPH: 500-SWITCH                                     
      *In testsuite: "Mock Sections And Paragraphs"                             
      *In testcase: "Empty local mock makes section do nothing"                 
      *****************************************************************         
           ADD 1 TO UT-6-4-1-MOCK-COUNT                                         
       .                                                                        
                                                                                
       6-5-1-MOCK.                                                              
      *****************************************************************         
      *Local mock of: SECTION: 100-WELCOME                                      
      *In testsuite: "Mock Sections And Paragraphs"                             
      *In testcase: "Local and global mocks can be used together"               
      *****************************************************************         
           ADD 1 TO UT-6-5-1-MOCK-COUNT                                         
                MOVE "Mockity-" TO VALUE-1                                      
                MOVE "Mock" TO VALUE-2                                          
       .                                                                        
                                                                                
      * CCHECKPARAGRAPHSPD.CPY                                                  
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
                    MOVE UT-VERIFY-AT-LEAST-STRING TO                           
                        UT-LABEL-VERIFY-COMPARE                                 
                    IF UT-ACTUAL-ACCESSES IS GREATER THAN OR EQUAL TO           
                            UT-EXPECTED-ACCESSES                                
                        SET UT-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
               WHEN UT-VERIFY-NO-MORE-THAN                                      
                    MOVE UT-VERIFY-NO-MORE-THAN-STRING TO                       
                        UT-LABEL-VERIFY-COMPARE                                 
                    IF UT-ACTUAL-ACCESSES IS LESS THAN OR EQUAL TO              
                            UT-EXPECTED-ACCESSES                                
                        SET UT-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
               WHEN OTHER                                                       
                    MOVE UT-VERIFY-EXACT-STRING TO                              
                        UT-LABEL-VERIFY-COMPARE                                 
                    IF UT-ACTUAL-ACCESSES IS EQUAL TO                           
                            UT-EXPECTED-ACCESSES                                
                        SET UT-VERIFY-PASSED TO TRUE                            
                    END-IF                                                      
           END-EVALUATE                                                         
                                                                                
           IF UT-VERIFY-PASSED                                                  
               ADD 1 TO UT-NUMBER-PASSED                                        
               DISPLAY UT-PASSED                                                
                       UT-TEST-CASE-NUMBER '. '                                 
                      'VERIFY ' UT-LABEL-VERIFY-COMPARE                         
                      UT-EXPECTED-ACCESSES-FMT SPACE                            
                      UT-LABEL-EXPECTED-ACCESS ' '                              
                      'TO ' UT-MOCK-OPERATION                                   
                                                                                
           ELSE                                                                 
               ADD 1 TO UT-NUMBER-FAILED                                        
               MOVE SPACES TO UT-DISPLAY-MESSAGE                                
               STRING                                                           
                   UT-FAILED                      DELIMITED BY SIZE             
                   UT-TEST-CASE-NUMBER            DELIMITED BY SIZE             
                   '. VERIFY ACCESSES TO '        DELIMITED BY SIZE             
                   UT-MOCK-OPERATION           DELIMITED BY SIZE                
                   INTO UT-DISPLAY-MESSAGE                                      
               END-STRING                                                       
               DISPLAY UT-DISPLAY-MESSAGE                                       
               MOVE SPACES TO UT-DISPLAY-MESSAGE                                
               STRING                                                           
                   '   EXPECTED '                 DELIMITED BY SIZE             
                   UT-LABEL-VERIFY-COMPARE ' ' DELIMITED BY SIZE                
                   UT-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE             
                   SPACE                          DELIMITED BY SIZE             
                   UT-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE            
                   ', WAS '                       DELIMITED BY SIZE             
                   UT-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE             
                   INTO UT-DISPLAY-MESSAGE                                      
               END-STRING                                                       
               DISPLAY UT-DISPLAY-MESSAGE                                       
               MOVE 4 TO UT-RETCODE                                             
           END-IF                                                               
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
                                                                                
      * CCHECKPARAGRAPHSPD.CPY END                                              
                                                                                
       000-START SECTION.                                                       
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Before and after tests 2"                                 
                   ALSO "Only global mocks apply in before/after (part 1        
      -    ")"                                                                  
                    PERFORM 2-2-1-MOCK                                          
                WHEN "Before and after tests 2"                                 
                   ALSO "Only global mocks apply in before/after (part 2        
      -    ")"                                                                  
                    PERFORM 2-3-1-MOCK                                          
                WHEN "Before and after tests 2"                                 
                   ALSO "Only global mocks apply in before/after (part 3        
      -    ")"                                                                  
                    PERFORM 2-4-1-MOCK                                          
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO "Local mock overwrites global mock"                     
                    PERFORM 6-2-1-MOCK                                          
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO "Multiple local mocks behaves as intended (First        
      -    " Verify should fail)"                                               
                    PERFORM 6-3-1-MOCK                                          
                WHEN "Before and after tests 2"                                 
                   ALSO ANY                                                     
                    PERFORM 2-0-1-MOCK                                          
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO ANY                                                     
                    PERFORM 6-0-1-MOCK                                          
           WHEN OTHER                                                           
           MOVE "Value1" to VALUE-1                                             
           MOVE "Value2" to VALUE-2                                             
           PERFORM 100-WELCOME                                                  
           PERFORM 200-GOODBYE                                                  
           PERFORM 300-CHANGE-1                                                 
           PERFORM 400-CHANGE-2                                                 
           PERFORM 500-SWITCH                                                   
           PERFORM 999-END                                                      
            END-EVALUATE                                                        
           EXIT SECTION.                                                        
                                                                                
       100-WELCOME SECTION.                                                     
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Before and after tests 2"                                 
                   ALSO "Only global mocks apply in before/after (part 1        
      -    ")"                                                                  
                    PERFORM 2-2-2-MOCK                                          
                WHEN "Before and after tests 2"                                 
                   ALSO "Only global mocks apply in before/after (part 2        
      -    ")"                                                                  
                    PERFORM 2-3-2-MOCK                                          
                WHEN "Before and after tests 2"                                 
                   ALSO "Only global mocks apply in before/after (part 3        
      -    ")"                                                                  
                    PERFORM 2-4-2-MOCK                                          
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO "Multiple local mocks behaves as intended (First        
      -    " Verify should fail)"                                               
                    PERFORM 6-3-2-MOCK                                          
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO "Local and global mocks can be used together"           
                    PERFORM 6-5-1-MOCK                                          
                WHEN "Before and after tests 2"                                 
                   ALSO ANY                                                     
                    PERFORM 2-0-2-MOCK                                          
           WHEN OTHER                                                           
           MOVE "Hello" to VALUE-1                                              
           MOVE "Hi" to VALUE-2                                                 
            END-EVALUATE                                                        
              EXIT SECTION                                                      
              .                                                                 
                                                                                
       200-GOODBYE SECTION.                                                     
      * Comment for testing purposes                                            
           MOVE "Bye" to VALUE-1                                                
           MOVE "See you" to VALUE-2                                            
              EXIT SECTION                                                      
          .                                                                     
                                                                                
       300-CHANGE-1.                                                            
      *Comment for testing purposes                                             
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO ANY                                                     
                    PERFORM 6-0-2-MOCK                                          
           WHEN OTHER                                                           
           EVALUATE VALUE-1                                                     
           WHEN "Hello"                                                         
              MOVE "Bye" TO VALUE-1                                             
           WHEN "Bye"                                                           
              MOVE "Hello" TO VALUE-1                                           
           WHEN OTHER                                                           
            END-EVALUATE                                                        
              EXIT PARAGRAPH                                                    
           END-EVALUATE.                                                        
                                                                                
       400-CHANGE-2.                                                            
           EVALUATE VALUE-2                                                     
           WHEN "Hi"                                                            
              MOVE "See you" TO VALUE-2                                         
           WHEN OTHER                                                           
              MOVE "Hi" TO VALUE-2                                              
           END-EVALUATE                                                         
          .                                                                     
                                                                                
       500-SWITCH.                                                              
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO "Multiple local mocks behaves as intended (First        
      -    " Verify should fail)"                                               
                    PERFORM 6-3-3-MOCK                                          
                WHEN "Mock Sections And Paragraphs"                             
                   ALSO "Empty local mock makes section do nothing"             
                    PERFORM 6-4-1-MOCK                                          
           WHEN OTHER                                                           
           MOVE VALUE-2 TO TEMP                                                 
           MOVE VALUE-1 TO VALUE-2                                              
           MOVE TEMP TO VALUE-1                                                 
            END-EVALUATE                                                        
           .                                                                    
                                                                                
       600-MAKE-CALL.                                                           
           MOVE "arg1" to VALUE-1                                               
           MOVE "arg2" to VALUE-2                                               
      *    CALL 'PROG1'.                                                        
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Call statements test"                                
                   ALSO "Simple call mock works"                                
                    PERFORM 5-1-1-MOCK                                          
                WHEN "Mock Call statements test"                                
                   ALSO "Global call mock is not overwritten by local ca        
      -    "ll mock (Should fail)"                                              
                    PERFORM 5-6-1-MOCK                                          
                WHEN "Mock Call statements test"                                
                   ALSO ANY                                                     
                    PERFORM 5-0-1-MOCK                                          
            END-EVALUATE                                                        
           .                                                                    
                                                                                
       700-MAKE-CALL.                                                           
           MOVE "arg1" to VALUE-1                                               
           MOVE "arg2" to VALUE-2                                               
      *    CALL VALUE-2 USING VALUE-1 END-CALL.                                 
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Call statements test"                                
                   ALSO "Call mock with argument works"                         
                    PERFORM 5-3-1-MOCK                                          
            END-EVALUATE                                                        
           .                                                                    
                                                                                
       800-MAKE-CALL.                                                           
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Call statements test"                                
                   ALSO "Paragraph mock is called and call mock is ignor        
      -    "ed"                                                                 
                    PERFORM 5-5-2-MOCK                                          
           WHEN OTHER                                                           
           MOVE "arg1" to VALUE-1                                               
           MOVE "arg2" to VALUE-2                                               
           MOVE "arg3" to VALUE-3                                               
      *    CALL 'PROG3' USING  BY CONTENT VALUE-1, BY VALUE VALUE-2, VAL        
      * UE-3.                                                                   
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Call statements test"                                
                   ALSO "Call mock with content reference for arguments         
      -    "work"                                                               
                    PERFORM 5-4-1-MOCK                                          
                WHEN "Mock Call statements test"                                
                   ALSO "Paragraph mock is called and call mock is ignor        
      -    "ed"                                                                 
                    PERFORM 5-5-1-MOCK                                          
            END-EVALUATE                                                        
      *    CALL 'PROG3' USING  BY CONTENT VALUE-1, BY VALUE VALUE-2, VAL        
      * UE-3                                                                    
            EVALUATE UT-TEST-SUITE-NAME                                         
                   ALSO UT-TEST-CASE-NAME                                       
                WHEN "Mock Call statements test"                                
                   ALSO "Call mock with content reference for arguments         
      -    "work"                                                               
                    PERFORM 5-4-1-MOCK                                          
                WHEN "Mock Call statements test"                                
                   ALSO "Paragraph mock is called and call mock is ignor        
      -    "ed"                                                                 
                    PERFORM 5-5-1-MOCK                                          
            END-EVALUATE                                                        
      *   CALL 'PROG3' USING VALUE-1                                            
            END-EVALUATE                                                        
           .                                                                    
                                                                                
       999-END.                                                                 
           GOBACK                                                               
           .                                                                    
