      * CCHECKRESULTPD.CPY
           DISPLAY SPACE
           MOVE ==UT==TEST-CASE-COUNT TO ==UT==TEST-CASE-NUMBER
           DISPLAY ==UT==TEST-CASE-NUMBER ' TEST CASES WERE EXECUTED'
           MOVE ==UT==NUMBER-PASSED TO ==UT==TEST-CASE-NUMBER
           DISPLAY ==UT==TEST-CASE-NUMBER ' PASSED'
           MOVE ==UT==NUMBER-FAILED TO ==UT==TEST-CASE-NUMBER
           DISPLAY ==UT==TEST-CASE-NUMBER ' FAILED'
           MOVE ==UT==NUMBER-UNMOCK-CALL TO ==UT==TEST-CASE-NUMBER
           DISPLAY ==UT==TEST-CASE-NUMBER ' CALLS NOT MOCKED'
           DISPLAY "================================================="
           MOVE ==UT==RETCODE TO RETURN-CODE
           GOBACK.

      * CCHECKRESULTPD.CPY END