ID DIVISION.
  PROGRAM-ID.   CHARGE ACCOUNT PROGRAM.
  AUTHOR.    EVANS, LEVY, & POLEN.
  REMARKS.
     PROGRAM TAKES CHARGE ACCOUNT TRANSACTIONS, AND PREPARES TWO
     OUTPUTS.
      1. A LIST OF CUSTOMERS AND TOTAL CHARGES.
      2. A LIST OF BAD ACCOUNT NUMBERS IN TRANSACTIONS.
ENVIRONMENT DIVISION.
  CONFIGURATION SECTION.
   OBJECT-COMPUTER.   PDP-10.
  INPUT-OUTPUT SECTION.
   FILE-CONTROL.
     SELECT NAME-FILE ASSIGN DSK RECORDING MODE ASCII.
     SELECT TRANS-FILE ASSIGN DSK RECORDING MODE ASCII.
     SELECT LIST-FILE ASSIGN DSK RECORDING MODE ASCII.
     SELECT WORK-FILE ASSIGN DSK RECORDING MODE IS ASCII.
     SELECT SORT-FILE ASSIGN DSK,DSK,DSK.
        SELECT SORT-F2 ASSIGN DSK,DSK,DSK.
DATA DIVISION.
  FILE SECTION.
   FD NAME-FILE VALUE ID 'NAMFILCUS'.
   01 NAME-RECORD.
     02 ID-INFO-N PIC X(57).
     02 ZIP PIC 9.
     02 ACCT-NO-N PIC 9(5).
   FD TRANS-FILE VALUE ID 'TRANS DEK'.
     01 TRANS-RECORD.
       02 ACCT-NO-T  PIC 9(5).
       02 AMT.
         03 AMT-H PIC 99999.
         03 FILLER PIC X.
         03 AMT-L PIC 99.

   FD WORK-FILE VALUE OF ID 'WORK  DAT'.
        01 WORK-REC  PIC X(13).
SD     SORT-FILE.
01 SORT-REC.
        05 S-R1 PIC 9(5).
        05 FILLER PIC X(8).
SD     SORT-F2.
  01  SORT-R2.
        05 FILLER PIC X(58).
        05 S-R2 PIC 9(5).
   FD LIST-FILE VALUE ID 'OUTPUT   '.
     01 OUTPUT-RECORD.
       02 ID-INFO-L  PIC X(57).
       02 ZIP-BIG PIC 9(5).
       02 ACCT-NO-L  PIC 9(5).
       02 TOT-AMT PIC 9(6).99.

  WORKING-STORAGE SECTION.
   77 TOT-AMT-1  PIC 999999V99 COMP.
   77 LAST-ACT-T    PIC 9(5)   COMP.
   77 LAST-ACT-N    PIC 9(5)   COMP.
   77 COUNT         PIC 9(3)   COMP.
   77 GRAND-TOT     PIC 9(6)V99  COMP.
   01 WORK-AMT.
     02 AMT-H PIC 99999.
     02 AMT-L PIC 99.
   01 WORKER REDEFINES WORK-AMT PIC 99999V99.

PROCEDURE DIVISION.
INIT-SORT.
        SORT SORT-FILE ON ASCENDING KEY S-R1,
          USING TRANS-FILE GIVING WORK-FILE.
MSTR-SORT.
        SORT SORT-F2 ON ASCENDING KEY S-R2
        USING NAME-FILE OUTPUT PROCEDURE HOUSKEEP THRU
          OP-PROC-EXIT.
   HOUSKEEP.
     OPEN OUTPUT LIST-FILE, INPUT WORK-FILE.
     MOVE 0 TO LAST-ACT-T, TOT-AMT, GRAND-TOT, COUNT.
        RETURN SORT-F2 INTO NAME-RECORD AT END GO TO NO-NAMES.
        SET LAST-ACT-N TO ACCT-NO-N.

READ-TRANS.
        READ WORK-FILE INTO TRANS-RECORD AT END GO TO ERROR-TRANS-1.
        IF LAST-ACT-T >
     ACCT-NO-T  GO TO ERROR-TRANS.
     IF ACCT-NO-T = 99999 GO TO DONE-DONE.
     SET LAST-ACT-T TO ACCT-NO-T.

   COMPARE-POINT.
     IF ACCT-NO-N < ACCT-NO-T  GO TO WRITE-LINE.
     IF ACCT-NO-N = ACCT-NO-T  GO TO ACCUM.
     IF COUNT >0 GO TO BAD-RECORD.  DISPLAY ' '. DISPLAY ' '.
     DISPLAY 'THE FOLLOWING ARE UNMATCHED RECORDS'.
     DISPLAY " ".

   BAD-RECORD. ADD 1 TO COUNT. 
                  GO TO READ-TRANS.
ACCUM.
      MOVE CORRESPONDING AMT TO WORK-AMT.
     ADD WORKER TO TOT-AMT-1. GO TO READ-TRANS.
   WRITE-LINE.
     MOVE ID-INFO-N TO ID-INFO-L.
     MOVE ACCT-NO-N TO ACCT-NO-L.
     ADD 87100, ZIP GIVING ZIP-BIG.
      MOVE TOT-AMT-1 TO TOT-AMT.
     ADD TOT-AMT-1 TO GRAND-TOT.  WRITE OUTPUT-RECORD.
        RETURN SORT-F2 INTO NAME-RECORD AT END 
          MOVE HIGH-VALUES TO ACCT-NO-N.
*    IF LAST-ACT-N NOT < ACCT-NO-N GO TO ERROR-NAMES.
     SET LAST-ACT-N TO ACCT-NO-N. GO TO COMPARE-POINT.

   NO-NAMES. DISPLAY 'NO NAME FILE RECORDS.'.  STOP RUN.
   ERROR-TRANS. DISPLAY 'CARD OUT OF SEQUENCE'.  STOP RUN.
   ERROR-NAMES. DISPLAY 'CUSTOMER NAME FILE OUT OF SEQUENCE'.  STOP RUN.

   DONE-DONE.
     DISPLAY " ".
     IF COUNT = 0 DISPLAY 'THERE WERE NO UNMATCHED TRANSACTIONS'
      ELSE DISPLAY 'THERE WERE ',COUNT, ' UNMATCHED TRANSACTIONS.'.
     DISPLAY ' '.  DISPLAY ' '.
     DISPLAY 'THE GRAND TOTAL FOR THE TRANSACTIONS WAS $ ',GRAND-TOT.
     DISPLAY ' '.  DISPLAY ' '.
     DISPLAY 'CHARGE EOJ'.
     STOP RUN.
   ERROR-TRANS-1.  DISPLAY 'NO 99999 CARD AT END OF DECK'.
      STOP RUN.
OP-PROC-EXIT.  EXIT.
 