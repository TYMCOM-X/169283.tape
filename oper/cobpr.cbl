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
        SELECT NEW-INV ASSIGN TO DSK.
        SELECT TRANS-FILE ASSIGN TO DSK.
DATA DIVISION.
FILE SECTION.
FD USAGE-FILE VALUE OF ID IS 'USAGE    '.
01 USAGE-RECORD USAGE IS DISPLAY-7.
        02 ID-1 PICTURE X(7).
        02 WEIGHT-1 PICTURE 9(6).
FD ORDERS-FILE VALUE OF ID IS 'ORDERS   '.
01 RECEIPTS-RECORD USAGE IS DISPLAY-7.
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
FD NEW-INV VALUE OF ID IS 'INV   NEW'.
01 NEW-INV-RECORD USAGE IS DISPLAY-7 PIC X(19).
FD TRANS-FILE VALUE OF ID IS 'TRAN     '.
        O1 TRAN-REC USAGE IS DISPLAY-7.
             02 TRAN-CODE PIC 9 .
             02 TRAN-REC PIC 9(6).
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
                INV-FILE, OUTPUT NEW-INV, TRANS-FILE.
        READ RECEIPTS-FILE AT END MOVE HIGH-VALUES TO ID-1.
        READ ORDERS-FILE AT END MOVE HIGH-VALUE TO ID-2.
        READ USAGE-FILE AT END MOVE HIGH-VALUES TO ID-3.
        READ INV-FILE AT END MOVE HIGH-VALUES TO ID-4.
PAR-2.
        IF ID-1 < ID-4 GO TO ERR-1.
        IF ID-1 > ID-4 GO TO PAR-3 ELSE SUBTRACT 1 FROM QTY-ON-HAND.
        SUBTRACT WEIGHT-1 FROM WEIGHT-ON-HAND.
PAR-2A.
        READ USAGE-FILE AT END MOVE HIGH-VALUES TO ID-1.
        GO TO PAR-2.

PAR-3.
        IF ID-2 < ID-4 GO TO ERR-2.
        IF ID-2 > ID-4 GO TO PAR-4 ELSE ADD QTY-2 TO QTY-ON-ORDER.
PAR-3A.
        READ ORDERS-FILE AT END MOVE HIGH-VALUES TO ID-2.
        GO TO PAR-3.

PAR-4.
        IF ID-3 < ID-4 GO TO ERR-3.
        IF ID-3 > ID-4 GO TO PAR-5 ELSE ADD QTY-3 TO QTY-ON-HAND.
        SUBTRACT QTY-3 FROM QTY-ON-ORDER, ADD
                WEIGHT-3 TO WEIGHT-ON-HAND.

PAR-4A.
        READ RECEIPTS-FILE AT END MOVE HIGH-VALUES TO ID-3.
        GO TO PAR-4.

PAR-5.
        WRITE NEW-INV-RECORD FROM INV-RECORD.
        READ INV-FILE AT END GO TO PAR-6.
        GO TO PAR-2.

PAR-6.
        IF ID-1 NOT = HIGH-VALUES MOVE ID-1 TO ID-E, DISPLAY RE1,REC-ERR.
        IF ID-2 NOT = HIGH-VALUES MOVE ID-2 TO ID-E, DISPLAY RE2,REC-ERR.
        IF ID-3 NOT = HIGH-VALUES MOVE ID-3 TO ID-E, DISPLAY RE3,REC-ERR.
        CLOSE USAGE-FILE, ORDERS-FILE, RECEIPTS-FILE, INV-FILE, NEW-INV.
        STOP RUN.

ERR-1.
        DISPLAY ID-1,' APPEARS ON USAGE-FILE AND IS NOT IN INV.'
        GO TO PAR-2A.
ERR-2.
        DISPLAY ID-2, ' APPEARS ON ORDERS-FILE AND IS NOT IN INF.'
        GO TO PAR-3A.
ERR-3.
        DISPLAY ID-3, ' APPEARS ON RECEIPTS-FILE AND IS NOT IN INV.'
        GO TO PAR-4A.
   