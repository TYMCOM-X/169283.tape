IDENTIFICATION DIVISION.
PROGRAM-ID. TWO.
AUTHOR. K. PETZOLD.
INSTALLATION. DALLAS DISTRICT.
DATE-WRITTEN. 25 APRIL, 1973.
DATE-COMPILED. 25 APRIL, 1973.
REMARKS. ASSIGNMENT NUMBER TWO.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT PAYTRN ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT YTDMAS ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT OUTMAS ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT PAYREP ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT PAYCHK ASSIGN TO DSK
        RECORDING MODE IS ASCII.
I-O-CONTROL.
SAME RECORD AREA FOR YTDMAS, OUTMAS.
DATA DIVISION.
FILE SECTION.
FD PAYTRN
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PAYTRNDAT".
01 PAYTRNIN.
        02 SOC-SEC-NO PIC IS 9(9).
        02 LNAME PIC IS X(15).
        02 FNAME PIC IS X(10).
        02 HRSWKD PIC IS 99V9.
        02 HOURLYRATE PIC IS 99V999.
FD YTDMAS
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "YTDMASHIS".
01 MASRECIN.
        02 SOC-SEC-NO PIC IS 9(9).
        02 EMPLOYEE-NO PIC IS 9(6).
        02 DEPT-NO PIC IS 9999.
        02 NUMBER-EXEMPTIONS PIC IS 99.
        02 YTD-GROSS-WH PIC IS 9(5)V99.
        02 YTD-FIT-WH PIC IS 9999V99.
        02 YTD-FICA-WH PIC IS 999V99.
        02 YTD-INS-WH PIC IS 999V99.
FD OUTMAS
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "OUTMASHIS".
01 MASRECOUT PIC IS X(44).
FD PAYREP
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PAYREPOUT".
01 REGISTER-REC PIC IS X(70).
FD PAYCHK
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PAYCHKOUT".
01 CHECKS-REC PIC IS X(70).
WORKING-STORAGE SECTION.
77 BEG-CHK-NO           PIC 9(6) VALUE ZERO.
77 CUR-DATE             PIC X(20) VALUE SPACES.
77 END-PAY-PER          PIC X(20) VALUE SPACES.
77 GROSS-WS             PIC S9999V99 VALUE ZEROES COMP.
77 FICA-WS              PIC S9999V99 VALUE ZEROES COMP.
77 FIT-WS               PIC S9999V99 VALUE ZEROES COMP.
77 INS-WS               PIC S9999V99 VALUE ZEROES COMP.
77 NET-WS               PIC S9999V99 VALUE ZEROES COMP.
77 TEMP                 PIC 9(5)V99 VALUE ZEROES COMP.
77 TEMP1                PIC 999V99 VALUE ZEROES COMP.
*
01 PAYHD-1.
        02 FILLER       PIC X(30) VALUE SPACES.
        02 FILLER       PIC X(40) VALUE "PAYROLL REPORT".
01 PAYHD-2.
        02 FILLER       PIC X(30) VALUE "  SOC-SEC          FIRST      ".
        02 FILLER       PIC X(30) VALUE "    LAST       HOURS   HOURLY ".
        02 FILLER       PIC X(10) VALUE "  GROSS".
01 PAYHD-3.
        02 FILLER       PIC X(30) VALUE "  NUMBER           NAME       ".
        02 FILLER       PIC X(30) VALUE "    NAME        WKD     RATE  ".
        02 FILLER       PIC X(10) VALUE "   PAY".
01 PAYHD-4.
        02 FILLER       PIC X(30) VALUE "           FIT-WH        FICA-".
        02 FILLER       PIC X(30) VALUE "WH       INS-WH     NET".
01 PAY-LN1.
        02 FILLER       PIC X     VALUE SPACE.
        02 SOC-SEC-NO   PIC 999B99B9999.
        02 FILLER       PIC XXX VALUE SPACE.
        02 FNAME        PIC X(15) VALUE SPACES.
        02 LNAME        PIC X(11) VALUE SPACES.
        02 HRSWKD       PIC Z(6).99.
        02 HOURLYRATE   PIC ZZZ9.999.
        02 GROSSPAY     PIC ZZ,ZZZ.99.
01 PAY-LN2.
        02 FILLER       PIC X(8) VALUE SPACES.
        02 FIT-WH       PIC ZZ,ZZZ.99.
        02 FILLER       PIC X(6) VALUE SPACES.
        02 FICA-WH      PIC Z,ZZZ.99.
        02 INS-WH       PIC ZZ,ZZZ,ZZZ.99.
        02 NETPAY       PIC Z,ZZZ,ZZZ.99.
01 CHK-LN1.
        02 FILLER       PIC X VALUE SPACE.
        02 CHK-EMP-MSG  PIC X(10) VALUE SPACES.
        02 CHK-EMP-NO   PIC Z(5)9 VALUE ZEROES.
        02 FILLER       PIC X(18) VALUE SPACES.
        02 MSG-P        PIC X(15) VALUE SPACES.
        02 CK-DATE      PIC X(20) VALUE SPACES.
01 CHK-LN2.
        02 FILLER       PIC X(11) VALUE " PAY TO:   ".
        02 FNAME        PIC X(15) VALUE SPACES.
        02 LNAME        PIC X(31) VALUE SPACES.
        02 NETPAY       PIC $*,***,***.99.
*
PROCEDURE DIVISION.
HSKPG.
        DISPLAY "ENTER BEGINNING CHECK NUMBER: " WITH NO ADVANCING.
        ACCEPT BEG-CHK-NO.
        DISPLAY "ENTER CURRENT DATE: " WITH NO ADVANCING.
        ACCEPT CUR-DATE.
        DISPLAY "ENTER ENDING PAY PERIOD DATE: " WITH NO ADVANCING.
        ACCEPT END-PAY-PER.
*
        OPEN INPUT PAYTRN, YTDMAS
            OUTPUT OUTMAS, PAYREP, PAYCHK.
*
HDG-RTN.
        
        WRITE REGISTER-REC FROM PAYHD-1 AFTER 4.
        WRITE REGISTER-REC FROM PAYHD-2 AFTER 2.
        WRITE REGISTER-REC FROM PAYHD-3 AFTER 1.
        WRITE REGISTER-REC FROM PAYHD-4 AFTER 2.
*
RD-FILES.
        READ YTDMAS, AT END GO TO EOJ-RTN.
        READ PAYTRN, AT END GO TO EOJ-RTN.
*
COMPUTEPAY.
        COMPUTE GROSS-WS ROUNDED  = HRSWKD IN PAYTRNIN * HOURLYRATE IN PAYTRNIN.
        COMPUTE FICA-WS ROUNDED = GROSS-WS * .04.
        COMPUTE FIT-WS ROUNDED = ( GROSS-WS - ( NUMBER-EXEMPTIONS * 13)) * .15.
        COMPUTE INS-WS = 5.40 * NUMBER-EXEMPTIONS.
                IF INS-WS > 14.00, MOVE 14.00 TO INS-WS.
        COMPUTE NET-WS = GROSS-WS - FICA-WS - INS-WS - FIT-WS.
*
BUILDPAY.
        MOVE CORRESPONDING PAYTRNIN TO PAY-LN1.
        MOVE CORRESPONDING PAYTRNIN TO CHK-LN2.
        MOVE GROSS-WS TO GROSSPAY.
        MOVE NET-WS TO NETPAY IN PAY-LN2, NETPAY IN CHK-LN2.
        EXAMINE SOC-SEC-NO IN PAY-LN1  REPLACING ALL " " BY "-".
*
        WRITE REGISTER-REC FROM PAY-LN1 AFTER 2.
*
        MOVE FIT-WS TO FIT-WH.
        MOVE FICA-WS TO FICA-WH.
        MOVE INS-WS TO INS-WH.
*
        WRITE REGISTER-REC FROM PAY-LN2 AFTER 1.
*
BUILDCHK.
        MOVE "CHECK NO. " TO CHK-EMP-MSG.
        MOVE BEG-CHK-NO TO CHK-EMP-NO.
        MOVE "CURRENT DATE: " TO MSG-P.
        MOVE CUR-DATE TO CK-DATE.
*
        WRITE CHECKS-REC FROM CHK-LN1 AFTER 7.
*
        MOVE "EMP NO.   " TO CHK-EMP-MSG.
        MOVE EMPLOYEE-NO TO CHK-EMP-NO.
        MOVE "PAY PERIOD: " TO MSG-P.
        MOVE END-PAY-PER TO CK-DATE.
*
        WRITE CHECKS-REC FROM CHK-LN1 AFTER 1.
        WRITE CHECKS-REC FROM CHK-LN2 AFTER 3.
*
        ADD 1 TO BEG-CHK-NO.
*
BUILDMAS.
        ADD GROSS-WS TO YTD-GROSS-WH.
        ADD FIT-WS TO YTD-FIT-WH.
        ADD FICA-WS TO YTD-FICA-WH.
        ADD INS-WS TO YTD-INS-WH.
*
        WRITE MASRECOUT FROM MASRECIN.
        GO TO RD-FILES.
*
EOJ-RTN.
        CLOSE PAYTRN, YTDMAS, OUTMAS, PAYREP, PAYCHK.
        STOP RUN.
    