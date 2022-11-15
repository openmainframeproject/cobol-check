           EXEC SQL DECLARE TEXE2 TABLE
           ( FIRST_NAME                  CHAR(10) NOT NULL,
             LAST_NAME                   CHAR(10) NOT NULL,
             WALLET                      INT(10) NOT NULL,
             TMS_CREA                    TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TEXEM2                             *
      ******************************************************************
       01  TEXE2.
           10 FIRST-NAME           PIC X(10).
           10 LAST-NAME            PIC X(10).
           10 WALLET               PIC 9(8)V99 COMP-3.
           10 TMS-CREA             PIC X(26).