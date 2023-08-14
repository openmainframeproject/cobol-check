           EXEC SQL DECLARE TEXEM TABLE
           ( FIRST_NAME                  CHAR(10) NOT NULL,
             LAST_NAME                   CHAR(10) NOT NULL,
             TMS_CREA                    TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TEXEMP                             *
      ******************************************************************
       01  TEXEM.
           10 FIRST-NAME           PIC X(10).
           10 LAST-NAME            PIC X(10).
           10 TMS-CREA             PIC X(26).