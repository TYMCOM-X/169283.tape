IDENTIFICATION DIVISION.
PROGRAM-ID. FORCST.
AUTHOR. B.DEPIETTO.
INSTALLATION. TYMCOM X.
DATE-WRITTEN. APRIL 15,1973.
DATE-COMPILED. APRIL 15,1973.
SECURITY. NONE.
REMARKS. TEST PROGRAM.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT DATAB ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD      DATAB
        VALUE OF ID IS "DATAB DAT".
01 INPUT-REC.
        02 NINE PIC IS 99999.
        02 TEN PIC IS 99999.
        02 PRODUCTS PIC IS 99999.
WORKING-STORAGE SECTION.
77      COUNTER PIC IS 9 VALUE 1.
77      RATE PIC IS V99 VALUE ZEROES.
77      NINESUM PIC IS 99999 VALUE ZEROES DISPLAY.
77      TENSUM PIC IS 99999 VALUE ZEROES DISPLAY.
77      PRODSUM PIC IS 99999 VALUE ZEROES DISPLAY.
77      TOTAL PIC IS 999999 VALUE ZEROES DISPLAY.
77      GRANDSUM PIC IS 99999 VALUE ZEROES DISPLAY.
77      MONTH PIC IS 9 VALUE ZEROES DISPLAY.
PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
        DISPLAY "ENTER GROWTH RATE:" WITH NO ADVANCING.
        ACCEPT RATE.
        OPEN INPUT DATAB.
        READ DATAB AT END GO TO OH-THREE-FIVE.
        DISPLAY "THRE MONTH ROLLING FORECAST".
        DISPLAY "MONTH   NINE   TEN   PRODUCTS  TOTAL".
        COMPUTE TOTAL = NINE + TEN + PRODUCTS.
OH-THREE-FIVE.
        DISPLAY MONTH,NINE,TEN,PRODUCTS,TOTAL.
        COMPUTE NINE = NINE * ( 1 + RATE ).
        COMPUTE TEN = TEN * ( 1 + RATE ).
        COMPUTE PRODUCTS = PRODUCTS * ( 1 + RATE ).
        ADD NINE TO NINESUM.
        ADD TEN TO TENSUM.
        ADD PRODUCTS TO PRODSUM.
        ADD 1 TO COUNTER.
        SET MONTH UP BY 1.
        IF COUNTER NOT EQUAL TO 4 GO TO OH-THREE-FIVE.
        COMPUTE GRANDSUM = NINESUM + TENSUM + PRODSUM.
        DISPLAY "TOTAL "NINESUM,TENSUM,PRODSUM,GRANDSUM.
        STOP RUN.
  