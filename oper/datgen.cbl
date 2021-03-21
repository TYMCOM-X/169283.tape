IDENTIFICATION DIVISION.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT OUTFIL ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD  OUTFIL
        BLOCK CONTAINS 10 RECORDS
        VALUE OF ID IS 'DATFILDAT'.
01  OUTREC.
        05 RECNUM PIC X(6).
        05 NUMNUM PIC 9999.
        05 RSTKEY PIC X(15).
        05 RECTIT PIC X(14).
        05 NUMTIT PIC 9999.
        05 RECFIL PIC X(67).
WORKING-STORAGE SECTION.
77  WORK-1 PIC S9(5) COMP VALUE ZERO.
PROCEDURE DIVISION.
FIRST-PARA.
        OPEN OUTPUT OUTFIL.
LOOP.
        ADD 1 TO WORK-1.
        IF WORK-1 > 2999   CLOSE OUTFIL, STOP RUN.
        MOVE WORK-1 TO NUMNUM, NUMTIT.
        MOVE 'RECORD NUMBER ' TO RECTIT.
        MOVE 'C-123-' TO RECNUM.
        MOVE SPACE TO RECFIL.
        WRITE OUTREC.
        GO TO LOOP.
   