ID DIVISION.
PROGRAM-ID. COBOLSAMPLE.
DATE-COMPILED. TODAY.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT USAGE-FILE ASSIGN TO DSK.
        SELECT ORDERS-FILE ASSIGN TO DSK.
        SELECT RECEIPTS-FILE ASSIGN TO DSK.
        SELECT INV-FILE ASSIGN TO DSK.
        SELECT TRANS-FILE ASSIGN TO DSK.
DATA DIVISION.
FILE SECTION.
FD USAGE-FILE VALUE OF ID IS 'USAGE    '.
01 USAGE-RECORD USAGE IS DISPLAY-7.
        02 ID-1 PICTURE X(7).
        02 WEIGHT-1 PICTURE 9(6).
FD ORDERS-FILE VALUE OF ID IS 'ORDERS   '.
01 ORDERS-RECORD USAGE IS DISPLAY-7.
        02 ID-2 PIC X(7).
        02 QTY-2 PIC 999.
FD RECEIPTS-FILE VALUE OF ID IS 'RECEIP   '.
01 RECEIPTS-RECORD USAGE IS DISPLAY-7.
        02 ID-3 PIC X(7).
        02 QTY-3 PIC 999.
        02 WEIGHT-3 PIC 9(6).
FD INV-FILE VALUE OF ID IS 'INV      '.
01 INV-RECORD USAGE IS DISPLAY-7.
        02 ID-4 PIC X(7).
        02 QTY-ON-HAND PIC 999.
        02 QTY-ON-ORDER PIC 999.
        02 WEIGHT-ON-HAND PIC 9(6).
FD TRANS-FILE VALUE OF ID IS 'TRANS DAT'.
01 TRANS-RECORD USAGE IS DISPLAY-7.
        03 ID-N PIC X(7).
        03 TR-CD PIC X.
        03 TRANS-1.
                04 WEIGHT-1 PIC 9(6).
        03 TRANS-2 REDEFINES TRANS-1.
                04 QTY-2 PIC 999.
        03 TRANS-3 REDEFINES TRANS-1.
                04 QTY-3 PIC 999.
                04 WEIGHT-3 PIC 9(6).
WORKING-STORAGE SECTION.
01 REC-ERR USAGE IS DISPLAY-7.
        02 RE-1 PIC X(14) VALUE 'RECORDS AFTER '.
        02 ID-E PIC X(7).
        02 RE-2 PIC X(16) VALUE ' NOT IN INV FILE'.
01 RE1 PIC X(6) VALUE 'USAGE '.
01 RE2 PIC X(7) VALUE 'ORDERS '.
01 RE3 PIC X(9) VALUE 'RECEIPTS '.
PROCEDURE DIVISION.
PAR-1.
        OPEN INPUT USAGE-FILE, ORDERS-FILE, RECEIPTS-FILE,
                INV-FILE, OUTPUT TRANS-FILE.
        READ USAGE-FILE AT END MOVE HIGH-VALUES TO ID-1.
        READ ORDERS-FILE AT END MOVE HIGH-VALUE TO ID-2.
        READ RECEIPTS-FILE AT END MOVE HIGH-VALUES TO ID-3.
PAR-2.
        IF ID-1 < ID-4 GO TO ERR-1.
        IF ID-1 > ID-4 GO TO PAR-3.
        MOVE 'U' TO TR-CD.
        MOVE ID-1 TO ID-N.
        MOVE WEIGHT-1 IN USAGE-RECORD TO TRANS-1 IN TRANS-RECORD.
        WRITE TRANS-RECORD.
        READ USAGE-FILE AT END MOVE HIGH-VALUES TO ID-1.
        GO TO PAR-2.

PAR-3.

        IF ID-2 < ID-4 GO TO ERR-2.
        IF ID-2 > ID-4 GO TO PAR-4.
        MOVE ID-2 TO ID-N.
        MOVE 'O' TO TR-CD.
        MOVE QTY-2 IN ORDERS-RECORD TO QTY-2 IN
        TRANS-RECORD.
        WRITE TRANS-RECORD.
        READ ORDERS-FILE AT END MOVE HIGH-VALUES TO ID-2.
        GO TO PAR-3.

PAR-4.

        IF ID-3 < ID-4 GO TO ERR-3.
        IF ID-3 > ID-4 GO TO END-PASS.
        MOVE ID-3 TO ID-N
        MOVE 'R' TO TR-CD
        MOVE WEIGHT-3 IN RECEIPTS-RECORD TO WEIGHT-3 IN TRANS-RECORD.
        MOVE QTY-3 IN RECEIPTS-RECORD TO QTY-3 IN TRANS-RECORD.
        WRITE TRANS-RECORD.
        READ RECEIPTS-FILE AT END MOVE HIGH-VALUES TO ID-3.
        GO TO PAR-4.

END-PASS.

        READ INV-FILE AT END GO TO PAR-6.
        GO TO PAR-2.

PAR-6.

      IF ID-1 NOT = HIGH-VALUES MOVE ID-1 TO ID-E , DISPLAY RE1,REC-ERR.
      IF ID-2 NOT = HIGH-VALUES MOVE ID-2 TO ID-E , DISPLAY RE2,REC-ERR.
      IF ID-3 NOT = HIGH-VALUES MOVE ID-3 TO ID-E , DISPLAY RE3,REC-ERR.

CLOSE-STOP.
        CLOSE USAGE-FILE, ORDERS-FILE, RECEIPTS-FILE, TRANS-FILE.
        STOP RUN.

ERR-1.
        DISPLAY ID-1, ' APPEARS ON USAGE-FILE AND IS NOT IN INV.'
        READ USAGE-FILE AT END MOVE HIGH-VALUES TO ID-1.
        GO TO PAR-2.

ERR-2.
        DISPLAY ID-2, ' APPEARS ON ORDERS-FILE AND IS NOT IN INV.'
        READ ORDERS-FILE AT END MOVE HIGH-VALUES TO ID-2.
        GO TO PAR-3.

ERR-3.

        DISPLAY ID-3, ' APPEARS ON RECEIPTS-FILE AND IS NOT IN INV.'
        READ RECEIPTS-FILE AT END MOVE HIGH-VALUES TO ID-3.
        GO TO PAR-4.
