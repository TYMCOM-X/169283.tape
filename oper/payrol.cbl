
IDENTIFICATION DIVISION.

PROGRAM-ID.     'PAYROLL'.
AUTHOR.         'LARRY SMITH'.
DATE-WRITTEN.   'MAY 14,1973'.

ENVIRONMENT DIVISION.

INPUT-OUTPUT SECTION.

FILE-CONTROL.

    SELECT PAYFIL, ASSIGN TO DSK, RECORDING MODE IS ASCII.
    SELECT PAYRPT, ASSIGN TO DSK, RECORDING MODE IS ASCII.

DATA DIVISION.

FILE SECTION.

FD  PAYFIL, VALUE OF ID IS 'PAYFIL   '.
01  PAYREC.
    02   LNAME     PIC X(20).
    02   FNAME     PIC X(15).
    02   HOURS     PIC 99V9.
    02   RATE      PIC 99V999.
    02   SSN.
         03   SSN1 PIC 999.
         03   SSN2 PIC 99.
         03   SSN3 PIC 9999.
FD  PAYRPT, VALUE OF ID IS 'PAYRPT   '.
01  RPTREC    PIC X(70).

WORKING-STORAGE SECTION.

77  AMOUNT    PIC 9(6)V99.
77  TOTAMT    PIC 9(7)V99.
01  TITLE.
    02   FILLER    PIC X(26) VALUE SPACES.
    02   FILLER    PIC X(14) VALUE 'PAYROLL REPORT'.
01  HEAD1.
    02   FILLER    PIC X(35) VALUE
         '  SOC-SEC    FIRST          LAST   '.
    02   FILLER    PIC X(35) VALUE
         '            HOURS  HOURLY   DOLLARS'.
01  HEAD2.
    02   FILLER    PIC X(35) VALUE
         '             NAME           NAME   '.
    02   FILLER    PIC X(35) VALUE
         '           WORKED   RATE     EARNED'.
01  D-TAIL.
    02   SSNA      PIC 999.
    02   FILLER    PIC X VALUE '-'.
    02   SSNB      PIC 99.
    02   FILLER    PIC X VALUE '-'.
    02   SSNC      PIC 9999.
    02   FILLER    PIC XX VALUE SPACES.
    02   NAME1     PIC X(15).
    02   NAME2     PIC X(20).
    02   FLD1      PIC ZZ.9.
    02   FLD2      PIC ZZZZ.999.
    02   FLD3      PIC $$$$$$$.99.
01  TOTAL.
    02   FILLER    PIC X(54) VALUE SPACES.
    02   FILLER    PIC X(5) VALUE 'TOTAL'.
    02   FLD4      PIC $$$$$$$$.99.

PROCEDURE DIVISION.

INITIALIZE.
    OPEN INPUT PAYFIL.
    OPEN OUTPUT PAYRPT.
HEADINGS.
    WRITE RPTREC FROM TITLE BEFORE ADVANCING 3.
    WRITE RPTREC FROM HEAD1.
    WRITE RPTREC FROM HEAD2 BEFORE ADVANCING 2.
DETAILS.
    READ PAYFIL, AT END GO TO TOTALS.
    COMPUTE AMOUNT = HOURS * RATE  ROUNDED.
    ADD AMOUNT TO TOTAMT.
    MOVE SSN1 TO SSNA.
    MOVE SSN2 TO SSNB.
    MOVE SSN3 TO SSNC.
    MOVE FNAME TO NAME1.
    MOVE LNAME TO NAME2.
    MOVE HOURS TO FLD1.
    MOVE RATE TO FLD2.
    MOVE AMOUNT TO FLD3.
    WRITE RPTREC FROM D-TAIL.
    GO TO DETAILS.
TOTALS.
    MOVE TOTAMT TO FLD4.
    WRITE RPTREC FROM TOTAL AFTER ADVANCING 1.
    CLOSE PAYFIL PAYRPT.
    STOP RUN.
