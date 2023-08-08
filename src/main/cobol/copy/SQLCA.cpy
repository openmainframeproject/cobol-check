       01  SQLCA.
           03  SQLCAID         PIC X(8)          VALUE "SQLCA   ".
           03  SQLCABC         PIC S9(9) USAGE BINARY VALUE 136.
           03  SQLCODE         PIC S9(9) USAGE BINARY VALUE 0.
           03  SQLERRM.
               05  SQLERRML    PIC S9(4) USAGE BINARY.
               05  SQLERRMC    PIC X(70).
           03  SQLERRP         PIC X(8).
           03  SQLERRD         PIC S9(9) USAGE BINARY OCCURS 6.
           03  SQLWARN.
               05  SQLWARN0    PIC X.
               05  SQLWARN1    PIC X.
               05  SQLWARN2    PIC X.
               05  SQLWARN3    PIC X.
               05  SQLWARN4    PIC X.
               05  SQLWARN5    PIC X.
               05  SQLWARN6    PIC X.
               05  SQLWARN7    PIC X.
               05  SQLWARN8    PIC X.
               05  SQLWARN9    PIC X.
               05  SQLWARN10   PIC X.
               05  SQLWARNA    REDEFINES SQLWARN10 PIC X.
           03  SQLSTATE        PIC X(5).
           03  FILLER          PIC X(21).
