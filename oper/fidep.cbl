IDENTIFICATION DIVISION.
PROGRAM-ID.    FIDEP.
AUTHOR.    J.M. ZIKRATCH.
INSTALLATION.    PRESS-ENTERPRISE CO.
DATE-WRITTEN.    JANUARY 1973.
REMARKS.          FIXTURE INVENTORY SYSTEM

     FIXTURE INVENTORY DEPRECIATION CALCULATION

     THIS PROGRAM CALCULATES THE CURRENT DEPRECIATION
     AMOUNT FOR EACH ITEM THAT HAS NOT YET OUTLIVED ITS
     YEARS OF USEFUL LIFE.  ANY ITEM ON FILE WHICH DOES
     NOT SHOW ANY DEPRECIATION RESERVE (E.G., AN ITEM
     WHICH HAS BEEN ADDED TO THE FILE SINCE THE        
     PROGRAM WAS LAST RUN) WILL HAVE ITS ENTIRE RESERVE
     TO-DATE CALCULATED.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER.    IBM-1130.
OBJECT-COMPUTER.    PDP-10.
SPECIAL-NAMES.    CONSOLE IS TYPEWRITER
                  CONSOLE IS KEYBOARD.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
     SELECT FIXED-ASSET-MASTER-FILE
*        ASSIGN TO DF-1-5000-X
         ASSIGN TO DSK
         ACCESS IS SEQUENTIAL.
     SELECT NEW-FIXED-ASSET-MASTER
*        ASSIGN TO DF-2-5000-X
         ASSIGN TO DSK
         ACCESS IS SEQUENTIAL.
DATA DIVISION.
FILE SECTION.
FD  FIXED-ASSET-MASTER-FILE
         VALUE OF ID IS 'FILTSTDAT'
         LABEL RECORDS ARE STANDARD
         RECORD CONTAINS 80 CHARACTERS
         DATA RECORD IS ASSET-MASTER-FILE-RECORD.
01  ASSET-MASTER-FILE-RECORD.
     05  ASSET-ITEM-NO           PICTURE 9(4) COMPUTATIONAL.
     05  DESCRIPTION             PICTURE X(30).
     05  SERIAL-NO-OR-DIMENSIONS PICTURE X(9).
     05  LOCATION-NO             PICTURE 9(4) COMPUTATIONAL.
     05  FILLER                  PICTURE X(8).
     05  GENERAL-LEDGER-ACCT-NO  PICTURE XXX.
     05  METHOD-OF-DEPRECIATION  PICTURE X.
     05  YEARS-OF-LIFE           PICTURE 9(4) COMPUTATIONAL.
     05  DATE-ACQUIRED.
         10  YEAR-ACQUIRED       PICTURE 99.
         10  MONTH-ACQUIRED      PICTURE 99.
         10  DAY-ACQUIRED        PICTURE 99.
     05  DATE-SOLD               PICTURE 9(6) COMPUTATIONAL.
     05  USED-ITEM-INDICATOR     PICTURE X.
         88  ITEM-IS-USED        VALUE "U".
     05  PURCHASE-COST           PICTURE 9(9) COMPUTATIONAL.
     05  REPLACEMENT-COST        PICTURE 9(9) COMPUTATIONAL.
     05  SALVAGE-VALUE           PICTURE 9(9) COMPUTATIONAL.
     05  AMOUNT-SOLD-FOR         PICTURE 9(9) COMPUTATIONAL.
     05  DEPRECIATION-RESERVE    PICTURE 9(9) COMPUTATIONAL.
     05  CURRENT-DEPRECIATION    PICTURE 9(9) COMPUTATIONAL.

FD  NEW-FIXED-ASSET-MASTER
         VALUE OF ID IS 'NFIXAS   '
         RECORD CONTAINS 80 CHARACTERS
         LABEL RECORDS ARE STANDARD
         DATA RECORD IS NEW-ASSET-MASTER-RECORD.
01  NEW-ASSET-MASTER-RECORD      PICTURE X(80).

WORKING-STORAGE SECTION.
77  MONTHS-OF-LIFE PIC 999 COMP.
77  MONTHS-OF-LIFE-LEFT PIC 999 COMP.
77  DEPRECIATION-RATE PIC 9V9999 COMP.
77  DECLINING-FACTOR PIC 99V99 COMP.
77  MONTHLY-DEPRECIATION PIC 9(9)V999 COMP.
77   RUN-DATE PICTURE X(8).
77  EFFECTIVE-COST PIC 9(9) COMP.
77  SUM-OF-LIFE-DIGITS PIC 9(6) COMP.
77  NO-MONTHS-DEPRECIATED PIC 9(4) COMP.
77  NO-OF-MONTHS PIC 9(4) COMP.
77  TEMP-CURRENT-DEPRECIATION PIC 9(9)V999 COMP.
77   WORK-YEAR-ACQUIRED PICTURE 99 COMP.
77   WORK-MONTH-ACQUIRED PICTURE 99 COMP.
01   EFFECTIVE-DATE.
     05  EFFECTIVE-MONTH PICTURE 99.
     05  FILLER PICTURE X.
     05  EFFECTIVE-DAY PICTURE 99.
     05  FILLER PICTURE X.
     05  EFFECTIVE-YEAR PICTURE 99.

01   WORK-DATE.
     05  WORK-YEAR PIC 99 COMP.
     05  WORK-MONTH PIC 99 COMP.
     05  WORK-DAY PIC 99 COMP.
01  PREVIOUS-WORK-DATE.
     05  PREVIOUS-WORK-MONTH PICTURE 99.
     05  PREVIOUS-WORK-DAY PICTURE 99.
     05  PREVIOUS-WORK-YEAR PICTURE 99.
01  FIXED-ASSET-MASTER-HEADER.
     05  F-A-PARAMETERS OCCURS 12 TIMES
         INDEXED BY F-A-P-INDEX.
         10  F-A-RUN-DATE PICTURE X(6).
         10  F-A-EFFECTIVE-DATE.
             15  F-A-EFFECTIVE-MONTH PIC 99.
             15  F-A-EFFECTIVE-DAY PIC 99.
             15  F-A-EFFECTIVE-YEAR PIC 99.
     05  FILLER PICTURE X(176).

PROCEDURE DIVISION.
OPEN-FILES.
     OPEN INPUT FIXED-ASSET-MASTER-FILE
         OUTPUT NEW-FIXED-ASSET-MASTER.
GET-RUN-DATE.
     DISPLAY "ENTER TODAY" QUOTE "S DATE AS MM-DD-YY"
         UPON TYPEWRITER.
     ACCEPT RUN-DATE FROM KEYBOARD.
GET-EFFECTIVE-DATE.
     DISPLAY "ENTER EFFECTIVE DATE AS MM-DD-YY"
         UPON TYPEWRITER.
     ACCEPT EFFECTIVE-DATE FROM KEYBOARD.
     IF EFFECTIVE-YEAR NOT NUMERIC
         OR EFFECTIVE-YEAR NOT POSITIVE
         OR EFFECTIVE-YEAR LESS THAN 60
         GO TO BAD-DATE.
     IF EFFECTIVE-MONTH NOT NUMERIC
         OR EFFECTIVE-MONTH NOT POSITIVE
         OR EFFECTIVE-MONTH GREATER THAN 13
         GO TO BAD-DATE.
     MOVE EFFECTIVE-YEAR TO WORK-YEAR.
     MOVE EFFECTIVE-MONTH TO WORK-MONTH.
     MOVE EFFECTIVE-DAY TO WORK-DAY.
     GO TO OPEN-FILES.
BAD-DATE.
     DISPLAY "INVALID DATE ENTERED"
         UPON TYPEWRITER.
     GO TO GET-EFFECTIVE-DATE.
READ-PARM-INFO.
     READ FIXED-ASSET-MASTER-FILE INTO FIXED-ASSET-MASTER-HEADER       
                AT END DISPLAY "OLD MASTER EMPTY"
                        UPON TYPEWRITER
                GO TO E-O-J.
     MOVE F-A-EFFECTIVE-DATE (1) TO PREVIOUS-WORK-DATE.
     PERFORM MOVE-PARM-FIELDS THRU MOVE-P-F-X VARYING F-A-P-INDEX
         FROM 12 BY -1 UNTIL F-A-P-INDEX LESS THAN 2.
     MOVE EFFECTIVE-YEAR TO F-A-EFFECTIVE-YEAR (1).
     MOVE EFFECTIVE-MONTH TO F-A-EFFECTIVE-MONTH (1).
     MOVE EFFECTIVE-DAY TO F-A-EFFECTIVE-DAY (1).
     WRITE NEW-ASSET-MASTER-RECORD FROM FIXED-ASSET-MASTER-HEADER
         INVALID KEY DISPLAY "BAD WRITE TO NEW MASTER"
         UPON TYPEWRITER
         GO TO E-O-J.
READ-A-RECORD.
     READ FIXED-ASSET-MASTER-FILE
         AT END GO TO E-O-J.
     IF DATE-SOLD NOT = ZERO
         GO TO WRITE-BACK-RECORD.
     IF YEAR-ACQUIRED IS GREATER THAN WORK-YEAR
         GO TO WRITE-BACK-RECORD.
     MOVE YEAR-ACQUIRED TO WORK-YEAR-ACQUIRED.
     MOVE MONTH-ACQUIRED TO WORK-MONTH-ACQUIRED.
     COMPUTE MONTHS-OF-LIFE = YEARS-OF-LIFE * 12.
     IF DEPRECIATION-RESERVE = ZERO
         MOVE YEAR-ACQUIRED TO WORK-YEAR-ACQUIRED
         MOVE MONTH-ACQUIRED TO WORK-MONTH-ACQUIRED
         COMPUTE MONTHS-OF-LIFE-LEFT = MONTHS-OF-LIFE
         GO TO COMPUTE-MONTHS-OF-DEPRECIATION.
     MOVE PREVIOUS-WORK-YEAR TO WORK-YEAR-ACQUIRED.
     COMPUTE WORK-MONTH-ACQUIRED = PREVIOUS-WORK-MONTH + 1.
     IF YEAR-ACQUIRED = PREVIOUS-WORK-YEAR
         COMPUTE NO-OF-MONTHS-DEPRECIATED =
             PREVIOUS-WORK-MONTH - MONTH-ACQUIRED + 1
     ELSE
         COMPUTE NO-OF-MONTHS-DEPRECIATED =
             (13 - MONTH-ACQUIRED)
             + (PREVIOUS-WORK-MONTH)
             + (PREVIOUS-WORK-YEAR - YEAR-ACQUIRED - 1) * 12.
     COMPUTE MONTHS-OF-LIFE-LEFT = (YEARS-OF-LIFE * 12)
         - NO-MONTHS-DEPRECIATED.
     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE
         GO TO WRITE-BACK-RECORD.
COMPUTE-MONTHS-OF-DEPRECIATION.
     IF WORK-YEAR-ACQUIRED = WORK-YEAR
         COMPUTE NO-OF-MONTHS = WORK-MONTH
         - WORK-MONTH-ACQUIRED + 1
         ELSE
        COMPUTE NO-OF-MONTHS = (13 - WORK-MONTH-ACQUIRED)
             + (WORK-MONTH) + ( (WORK-YEAR -
             WORK-YEAR-ACQUIRED - 1) * 12).

     MOVE ZERO TO CURRENT-DEPRECIATION TEMP-CURRENT-DEPRECIATION.
     IF METHOD-OF-DEPRECIATION = "S"
         GO TO STRAIGHT-LINE.
     IF METHOD-OF-DEPRECIATION = "D"
         MOVE 2 TO DECLINING-FACTOR
         GO TO DECLINING-BALANCE.
     IF METHOD-OF-DEPRECIATION = "H"
         MOVE 1.5 TO DECLINING-FACTOR
         GO TO DECLINING-BALANCE.
     IF METHOD-OF-DEPRECIATION = "Y"
         GO TO SUM-OF-YEARS-DIGITS.
     DISPLAY "INVALID METH. OF DEPR. = " 
         METHOD-OF-DEPRECIATION
         " FOR ASSET " ASSET-ITEM-NO
         UPON TYPEWRITER.
     GO TO WRITE-BACK-RECORD.

STRAIGHT-LINE.
     COMPUTE DEPRECIATION-RATE ROUNDED = 1 / MONTHS-OF-LIFE.
     COMPUTE MONTHLY-DEPRECIATION ROUNDED =
         (PURCHASE-COST - SALVAGE-VALUE)
         * DEPRECIATION-RATE.
     PERFORM ACCUMULATE-ST-LINE-DEPR THRU
         ACCUMULATE-S-L-D-X NO-OF-MONTHS TIMES.
     GO TO ADD-DEPR-TO-RESERVE.

DECLINING-BALANCE.
     COMPUTE DEPRECIATION-RATE ROUNDED = DECLINING-FACTOR
         / MONTHS-OF-LIFE.
     PERFORM ACCUMULATE-DECL-DEPR THRU ACCUMULATE-D-D-X
         NO-OF-MONTHS TIMES.
     GO TO WRITE-BACK-RECORD.

SUM-OF-YEARS-DIGITS.
     COMPUTE SUM-OF-LIFE-DIGITS = MONTHS-OF-LIFE
         * (MONTHS-OF-LIFE + 1) / 2.
     COMPUTE EFFECTIVE-COST = PURCHASE-COST - SALVAGE-VALUE.
     PERFORM ACCUMULATE-S-Y-D-DEPR THRU ACCUMULATE-S-Y-D-D-X
         NO-OF-MONTHS TIMES.
     GO TO ADD-DEPR-TO-RESERVE.

ADD-DEPR-TO-RESERVE.
     ADD TEMP-CURRENT-DEPRECIATION TO CURRENT-DEPRECIATION
         ROUNDED.
     ADD CURRENT-DEPRECIATION TO DEPRECIATION-RESERVE.

WRITE-BACK-RECORD.
     WRITE NEW-ASSET-MASTER-RECORD FROM ASSET-MASTER-FILE-RECORD
         INVALID KEY DISPLAY "NEW MASTER OVERFLOW"
         UPON TYPEWRITER.
     GO TO READ-A-RECORD.

E-O-J.
     CLOSE FIXED-ASSET-MASTER-FILE
         NEW-FIXED-ASSET-MASTER.
     DISPLAY "EOJ--FIDEP" UPON TYPEWRITER.
     STOP RUN.

PERFORMED-ROUTINES SECTION.
ACCUMULATE-ST-LINE-DEPR.
     ADD MONTHLY-DEPRECIATION TO TEMP-CURRENT-DEPRECIATION.
     SUBTRACT 1 FROM MONTHS-OF-LIFE-LEFT.
     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE
         GO TO ADD-DEPR-TO-RESERVE.
ACCUMULATE-S-L-D-X.    EXIT.

ACCUMULATE-DECL-DEPR.
     COMPUTE MONTHLY-DEPRECIATION ROUNDED = DEPRECIATION-RATE
         * (PURCHASE-COST - SALVAGE-VALUE
         - DEPRECIATION-RESERVE).
     ADD MONTHLY-DEPRECIATION TO CURRENT-DEPRECIATION ROUNDED.
     ADD MONTHLY-DEPRECIATION TO DEPRECIATION-RESERVE ROUNDED.
     SUBTRACT 1 FROM MONTHS-OF-LIFE-LEFT.
     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE
         GO TO WRITE-BACK-RECORD.
ACCUMULATE-D-D-X.    EXIT.

MOVE-PARM-FIELDS.
     MOVE F-A-PARAMETERS (F-A-P-INDEX - 1) TO
         F-A-PARAMETERS (F-A-P-INDEX).
MOVE-P-F-X.    EXIT.

ACCUMULATE-S-Y-D-DEPR.
     COMPUTE TEMP-CURRENT-DEPRECIATION ROUNDED =
         TEMP-CURRENT-DEPRECIATION + (EFFECTIVE-COST * MONTHS-OF-LIFE-LEFT)
         / SUM-OF-LIFE-DIGITS.
     SUBTRACT 1 FROM MONTHS-OF-LIFE-LEFT.
     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE
         GO TO ADD-DEPR-TO-RESERVE.
ACCUMULATE-S-Y-D-D-X.    EXIT.
  