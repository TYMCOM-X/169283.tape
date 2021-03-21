IDENTIFICATION DIVISION.
PROGRAM-ID. RCB2.
AUTHOR. CHRIS BUSCH.
INSTALLATION. HOUSTON.
DATE-WRITTEN. MAY 17,1973.
DATE-COMPILED. MAY 17,1973.
SECURITY. HUSH HUSH SWEET CHARLOTTE.
REMARKS. COBOL PROG. ASSIGNMENT #2; PAYROLL SYSTEM.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT MASTER-IN ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT MASTER-OUT ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT TRANS ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT PAY-REPORT ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT CHECKS ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD MASTER-IN
        VALUE OF IDENTIFICATION IS "MASTER   ".
01 MASTER-IN-REC.
           02 SOC-SEC.
              03 SS1          PIC 9(3).
              03 SS2          PIC 99.
             03  SS3          PIC 9(4).
           02 EMP-NUM         PIC 9(6).
           02 DEPT-NUM        PIC 9(4).
           02 NO-EXEMPT       PIC 99.
           02 YTD-GROSS       PIC S9(5)V99.
           02 YTD-FIT         PIC S9(4)V99.
           02 YTD-FICA        PIC S9(3)V99.
           02 YTD-INS         PIC S9(3)V99.
FD TRANS
        VALUE OF IDENTIFICATION IS "TRANS    ".
01 TRANS-REC.
           02 T-SOC-SEC       PIC 9(9).
             02 LAST-NAME       PIC X(15).
           02 FIRST-NAME      PIC X(10).
           02 HOURS           PIC S9(2)V9.
           02 RATE            PIC S9(2)V999.
FD MASTER-OUT
        VALUE OF IDENTIFICATION IS "NEWMAS   ".
01 MASTER-OUT-REC     PIC X(44).
FD PAY-REPORT
        VALUE OF IDENTIFICATION IS "PAYREP   ".
01 REPORT-LINE         PIC X(70).
FD CHECKS
        VALUE OF IDENTIFICATION IS "CHECKS   ".
01 CHECKLINE         PIC X(70).
WORKING-STORAGE SECTION.
77 GROSS    PIC S9(5)V99.
77 FIT-WH       PIC S9(4)V99.
77 FICA-WH      PIC S9(3)V99.
77 INS-WH       PIC S9(2)V99.
77 NET-PAY      PIC S9(5)V99.
77 CHECK-NUM    PIC S9(5).
01 CHAR-CNTL      PIC 9 VALUE 1.
01 HEADING1.
   02 FILLER    PIC X(28) VALUE SPACES.
   02 FILLER    PIC X(14) VALUE "PAYROLL REPORT".
01 HEADING2     PIC X(66) VALUE
   " SOC-SEC          FIRST          LAST       HOURS   HOURLY   GROSS".
01 HEADING3     PIC X(65) VALUE
   " NUMBER           NAME           NAME        WKD     RATE     PAY".
01 HEADING4     PIC X(54) VALUE
   "          FIT-WH          FICA-WH       INS-WH     NET".
01 DETAIL1.
   02 D-SOC-SEC.
      03 D-SS1    PIC XXX.
      03 FILLER   PIC X VALUE "-".
      03 D-SS2    PIC XX.
      03 FILLER   PIC X VALUE "-".
      03 D-SS3    PIC XXXX.
   02 FILLER      PIC XXX VALUE SPACES.
   02 D-FIRST-NAME PIC X(10).
   02 FILLER      PIC X(5) VALUE SPACES.
   02 D-LAST-NAME PIC X(15).
   02 D-HOURS     PIC ZZ.9.
   02 FILLER      PIC X VALUE SPACE.
   02 FILLER      PIC XXX VALUE SPACES.
   02 D-RATE      PIC ZZ.999.
   02 D-GROSS-PAY PIC ZZ,ZZZ.99.
01 DETAIL2.
   02 FILLER      PIC X(8) VALUE SPACES.
   02 D-FIT-WH    PIC Z,ZZZ.99.
   02 FILLER      PIC X(10) VALUE SPACES.

  02 D-FICA-WH    PIC ZZZ.99.
  02 FILLER       PIC X(8) VALUE SPACES.
  02 D-INS-WH     PIC ZZ.99.
  02 FILLER       PIC XXX VALUE SPACES.
  02 D-NET-PAY    PIC ZZ,ZZZ.99.
01 DETAIL3.
   02 FILLER          PIC X(10) VALUE "CHECK NO. ".
   02 D-CHECK-NUM     PIC ZZ999.
   02 FILLER          PIC X(18) VALUE SPACES.
   02 FILLER          PIC X(15) VALUE "CURRENT DATE:  ".
   02 CURDATE         PIC X(18) VALUE SPACES.
01 DETAIL4.
   02 FILLER          PIC X(9) VALUE "EMP NO.  ".
   02 D-EMP-NUM       PIC ZZZ999.
   02 FILLER          PIC X(18) VALUE SPACES.
   02 FILLER          PIC X(15) VALUE "PAY PERIOD:    ".
   02 D-EFF-DATE      PIC X(18) VALUE SPACES.
01 DETAIL5.
   02 FILLER          PIC X(10) VALUE "PAY TO:   ".
   02 D-C-FIRST-NAME  PIC X(10).
   02 FILLER          PIC X(4) VALUE SPACES.
   02 D-C-LAST-NAME   PIC X(15).
   02 FILLER          PIC X(15) VALUE SPACES.
   02 D-C-NET-PAY     PIC $**,**9.99.
01 EOF-MASTER         PIC XXX VALUE SPACES.
01 EOF-TRANS          PIC XXX VALUE SPACES.
PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
INITIALIZATION.
    OPEN INPUT  MASTER-IN TRANS.
     OPEN OUTPUT  MASTER-OUT PAY-REPORT CHECKS.
     DISPLAY "ENTER BEGINNING CHECK NUMBER."
     ACCEPT CHECK-NUM.
     DISPLAY "ENTER CURRENT DATE".
     ACCEPT CURDATE.
     DISPLAY "ENTER PAY PERIOD DATE".
     ACCEPT D-EFF-DATE.
     PERFORM HEADING-ROUT.
     SUBTRACT 1 FROM CHECK-NUM.
MAIN-LINE.
     PERFORM READ-MASTER.
NEW-TRAN.
     PERFORM READ-TRANS.
     IF SOC-SEC NOT= T-SOC-SEC  GO TO ERROR-ROUTINE.
     ADD 1 TO CHECK-NUM.
     COMPUTE  GROSS = HOURS * RATE.
     COMPUTE  FIT-WH = (GROSS - (NO-EXEMPT * 13)) * .15 .
     COMPUTE  FICA-WH = GROSS * .04
     COMPUTE  INS-WH = NO-EXEMPT * 5.40 .
     IF INS-WH > 14.00  MOVE 14.00 TO INS-WH ELSE NEXT SENTENCE.
     COMPUTE  NET-PAY = GROSS - FIT-WH - FICA-WH - INS-WH.
     ADD FIT-WH TO YTD-FIT.
     ADD FICA-WH TO YTD-FICA.
     ADD INS-WH TO YTD-INS.
     ADD GROSS TO YTD-GROSS.
     MOVE  SS1       TO        D-SS1.
     MOVE  SS2       TO        D-SS2.
     MOVE  SS3       TO        D-SS3.
     MOVE  FIRST-NAME TO D-FIRST-NAME.
     MOVE  LAST-NAME TO D-LAST-NAME.
     MOVE HOURS TO D-HOURS.
     MOVE RATE TO D-RATE.
     MOVE GROSS TO D-GROSS-PAY.
     MOVE DETAIL1 TO REPORT-LINE.
     PERFORM  REPORT-PRINT.
     MOVE FIT-WH TO D-FIT-WH.
     MOVE FICA-WH TO D-FICA-WH.
     MOVE INS-WH TO D-INS-WH.
     MOVE NET-PAY TO D-NET-PAY.
     MOVE DETAIL2 TO REPORT-LINE.
     MOVE  2 TO  CHAR-CNTL.
     PERFORM  REPORT-PRINT.
     MOVE CHECK-NUM TO D-CHECK-NUM.
     MOVE EMP-NUM TO D-EMP-NUM.
     MOVE FIRST-NAME TO D-C-FIRST-NAME.
     MOVE LAST-NAME TO D-C-LAST-NAME.
     MOVE NET-PAY TO D-C-NET-PAY.
     MOVE DETAIL3 TO CHECKLINE.
     PERFORM CHECK-PRINT.
     MOVE DETAIL4 TO CHECKLINE.
     MOVE  3  TO CHAR-CNTL.
     PERFORM CHECK-PRINT.
     MOVE DETAIL5 TO CHECKLINE.
     MOVE  7  TO CHAR-CNTL.
     PERFORM CHECK-PRINT.
     MOVE  3  TO  CHAR-CNTL.
     PERFORM CHECK-PRINT.
     PERFORM WRITE-MASTER.
     GO TO MAIN-LINE.
READ-MASTER.
   READ MASTER-IN AT END  MOVE 999 TO EOF-MASTER,GO TO EOF-ROUT.
READ-TRANS.
   READ TRANS AT END  MOVE 999 TO EOF-TRANS,GO TO EOF-ROUT.
WRITE-MASTER.
   WRITE MASTER-OUT-REC  FROM MASTER-IN-REC.
EOF-ROUT.
   IF EOF-MASTER = 999 AND EOF-TRANS = 999 GO TO SHUTDOWN.
     IF EOF-MASTER=999 GO TO NEW-TRAN.
     ELSE  GO TO MAIN-LINE.
ERROR-ROUTINE.
   DISPLAY "FILES NOT REALLY IN 1 TO 1 SEQUENTIAL CORRESPONDENCE.".
   DISPLAY T-SOC-SEC," NOT EQUAL TO ",SOC-SEC.
   GO TO MAIN-LINE.
SHUTDOWN.
   CLOSE  MASTER-IN MASTER-OUT TRANS PAY-REPORT CHECKS.
   STOP RUN.
REPORT-PRINT.
   WRITE REPORT-LINE BEFORE ADVANCING CHAR-CNTL  LINES.
   MOVE SPACES TO REPORT-LINE.
   MOVE 1  TO CHAR-CNTL.
CHECK-PRINT.
   WRITE CHECKLINE  BEFORE ADVANCING CHAR-CNTL LINES.
   MOVE SPACES TO CHECKLINE.
   MOVE  1  TO   CHAR-CNTL.
HEADING-ROUT.
   MOVE 2 TO CHAR-CNTL.
   MOVE HEADING1 TO REPORT-LINE.
   PERFORM REPORT-PRINT.
   MOVE HEADING2 TO REPORT-LINE.
   PERFORM REPORT-PRINT.
   MOVE  2 TO CHAR-CNTL.
   MOVE HEADING3 TO REPORT-LINE.
   PERFORM REPORT-PRINT.
   MOVE  2 TO CHAR-CNTL.
   MOVE HEADING4 TO REPORT-LINE.
   PERFORM REPORT-PRINT.
    