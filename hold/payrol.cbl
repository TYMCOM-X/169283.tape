IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL.
AUTHOR. WK SELPH.
DATE-WRITTEN. MAY 11, 1973.
REMARKS.  ASSIGNMENT TWO.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. DECSYSTEM-10.
OBJECT-COMPUTER. DECSYSTEM-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT PAYFIL ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT YTDMST ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT YTDOUT ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT PAYREP ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT CHECKS ASSIGN TO DSK
        RECORDING MODE IS ASCII.
I-O-CONTROL.
SAME RECORD AREA FOR YTDMST YTDOUT.
DATA DIVISION.
FILE SECTION.
FD PAYFIL
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PAYFILDAT".
01 PAYIN.
   03 SSNO     PIC 9(9).
   03 LNAME    PIC X(15).
   03 FNAME    PIC X(10).
   03 HOURS    PIC 99V9.
   03 RATE     PIC 99V999.
FD YTDMST
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "YTDMSTHIS".
01 MSTIN.
   03 SSNO     PIC 9(9).
   03 EMPLOYEE-NO   PIC 9(6).
   03 DEPT-NO  PIC 9999.
   03 EXEMPTIONS   PIC 99.
03   YTD-GROSS-WH  PIC 9(5)V99.
   03 YTD-FIT-WH   PIC 9999V99.
   03 YTD-FICA-WH   PIC 999V99.
   03 YTD-INS-WH   PIC 999V99.
FD YTDOUT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "YTDOUTHIS".
01 YTDREC   PIC X(44).
FD PAYREP
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PAYREPDAT".
01 PAYREP-REC   PIC X(70).
FD CHECKS
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PAYCHECKS".
01 CHECKS-REC PIC IS X(70).
WORKING-STORAGE SECTION.
77 BEG-CHK-NO           PIC 9(6) VALUE ZERO.
77 CUR-DATE             PIC X(20) VALUE SPACES.
77 END-PAY-PD   PIC X(20)   VALUE SPACES.
77 GROSS        PIC S9999V99   VALUE ZEROES.
77 FICA         PIC S9999V99   VALUE ZEROES.
77 FIT          PIC S9999V99   VALUE ZEROES.
77 INS          PIC S9999V99   VALUE ZEROES.
77 NET          PIC S9999V99   VALUE ZEROES.
01 HEAD-1.
   03 FILLER    PIC X(30)   VALUE SPACES.
   03 FILLER    PIC X(40)   VALUE "PAYROLL REPORT".
01 HEAD-2.
   03 FILLER     PIC X(30)    VALUE "SOC-SEC        FIRST     LAST ".
   03 FILLER      PIC X(30)    VALUE "           HOURS   HOURLY     ".
   03   FILLER    PIC X(10)   VALUE "   GROSS  ".
01 HEAD-3.
   03 FILLER      PIC X(30)    VALUE "NUMBER        NAME      NAME ".
   03 FILLER      PIC X(27)    VALUE "           WKD      RATE".
   03 FILLER     PIC X(10)    VALUE "      PAY ".
01 HEAD-4.
   03 FILLER    PIC X(30)   VALUE "           FIT-WH        FICA-".
   03 FILLER     PIC X(30)    VALUE "WH       INS-WH    NET".
01 PLINE-1.
   03 FILLER     PIC X   VALUE SPACE.
   03 SSNO       PIC 999B99B9999.
   03 FILLER     PIC XXX   VALUE SPACES.
   03 FNAME      PIC X(10)   VALUE SPACES.
   03 LNAME      PIC X(15)   VALUE SPACES.
   03 HOURS      PIC ZZZ.9.
   03 FILLER      PIC XXX    VALUE SPACES.
   03 RATE      PIC ZZZZ9.999.
   03 GROSSPAY   PIC ZZ,ZZZ.99.
01 PLINE-2.
   03 FILLER     PIC X(8)   VALUE SPACES.
   03 FIT-WH     PIC ZZ,ZZZ.99.
   03 FILLER     PIC X(6)   VALUE SPACES.
   03 FICA-WH    PIC Z,ZZZ.99.
   03 INS-WH     PIC ZZ,ZZZ,ZZZ.99.
   03 FILLER     PIC XXX     VALUE SPACES.
   03 NETPAY     PIC ZZ,ZZZ.99.
01 CLINE-1.
   03 FILLER     PIC X   VALUE SPACE.
   03 MSG-1      PIC X(10)   VALUE SPACES.
   03 MSG-2     PIC ZZZZZ9  VALUE ZEROES.
   03 FILLER     PIC X(18)   VALUE SPACES.
   03 MSG-3      PIC X(15)   VALUE SPACES.
   03 CK-DATE    PIC X(20)   VALUE SPACES.
01 CLINE-2.
   03 FILLER     PIC X(11)    VALUE " PAY TO:   ".
   03 FNAME     PIC X(15)   VALUE SPACES.
   03 LNAME     PIC X(31)   VALUE SPACES.
   03 NETPAY     PIC $*,***,***.99.
PROCEDURE DIVISION.
STARTER.
        DISPLAY "ENTER BEGINNING CHECK NUMBER:  " WITH NO ADVANCING.
        ACCEPT BEG-CHK-NO.
        DISPLAY "ENTER CURRENT DATE:  " WITH NO ADVANCING.
        ACCEPT CUR-DATE.
        DISPLAY "ENTER ENDING PAY PERIOD DATE:  " WITH NO ADVANCING.
        ACCEPT END-PAY-PD.
        OPEN INPUT PAYFIL, YTDMST
            OUTPUT YTDOUT, PAYREP, CHECKS.
DO-HEADERS.
        
        WRITE PAYREP-REC FROM HEAD-1 AFTER ADVANCING 4.
        WRITE PAYREP-REC FROM HEAD-2 AFTER ADVANCING 2.
        WRITE PAYREP-REC FROM HEAD-3 AFTER ADVANCING 1.
        WRITE PAYREP-REC FROM HEAD-4 AFTER ADVANCING 2.
READ-EM.
        READ YTDMST, AT END GO TO END-OF-JOB.
        READ PAYFIL, AT END GO TO END-OF-JOB.
DO-PAY.
        COMPUTE GROSS ROUNDED  = HOURS IN PAYIN * RATE IN PAYIN.
        COMPUTE FICA ROUNDED = GROSS * .04.
        COMPUTE FIT ROUNDED = ( GROSS - ( EXEMPTIONS * 13)) * .15.
        COMPUTE INS = 5.40 * EXEMPTIONS.
                IF INS > 14.00, MOVE 14.00 TO INS.
        COMPUTE NET = GROSS - FICA - INS - FIT.
BUILDPAY.
        MOVE CORRESPONDING PAYIN TO PLINE-1.
        MOVE CORRESPONDING PAYIN TO CLINE-2.
        MOVE GROSS TO GROSSPAY.
        MOVE NET TO NETPAY IN PLINE-2, NETPAY IN CLINE-2.
        EXAMINE SSNO IN PLINE-1 REPLACING ALL " " BY "-".
        WRITE PAYREP-REC FROM PLINE-1 AFTER ADVANCING 2.
        MOVE FIT TO FIT-WH.
        MOVE FICA TO FICA-WH.
        MOVE INS TO INS-WH.
        WRITE PAYREP-REC FROM PLINE-2 AFTER ADVANCING 1.
DO-CHECKS.
        MOVE " CHECK NO." TO MSG-1.
        MOVE BEG-CHK-NO TO MSG-2.
        MOVE "CURRENT DATE: " TO MSG-3.
        MOVE CUR-DATE TO CK-DATE.
        WRITE CHECKS-REC FROM CLINE-1 AFTER ADVANCING 7.
        MOVE "EMP NO.  " TO MSG-1.
        MOVE EMPLOYEE-NO TO MSG-2.
        MOVE "PAY PERIOD:   " TO MSG-3.
        MOVE END-PAY-PD TO CK-DATE.
        WRITE CHECKS-REC FROM CLINE-1 AFTER ADVANCING 1.
        WRITE CHECKS-REC FROM CLINE-2 AFTER ADVANCING 3.
        ADD 1 TO BEG-CHK-NO.
DO-TYD.
        ADD GROSS TO YTD-GROSS-WH.
        ADD FIT TO YTD-FIT-WH.
        ADD FICA TO YTD-FICA-WH.
        ADD INS TO YTD-INS-WH.
        WRITE YTDREC FROM MSTIN.
        GO TO READ-EM.
END-OF-JOB.
        CLOSE PAYFIL, YTDMST, YTDOUT, PAYREP, CHECKS.
        STOP RUN.
    