IDENTIFICATION DIVISION.
PROGRAM-ID. BUD.
AUTHOR. A. CONSULTANT.
INSTALLATION. TYMSHARE.
DATE-WRITTEN. APRIL-14-1973.
SECURITY. NONE.
REMARKS. **DEMONSTRATION COBOL PROGRAM- WRITTEN USING COBAID**.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
77 PRINT-HEADING PIC X(72) VALUE SPACES.
77 GROWTH PIC 9.99 .
01 PERIOD-DATA OCCURS 3 TIMES INDEXED BY MONTH.
        02 MONTH-TITLE PIC X(10).
        02 TYMCOM-IX PIC 9999.99 DISPLAY.
        02 FILLER PIC X(5).
        02 TYMCOM-X PIC 9999.99 DISPLAY.
        02 FILLER PIC X(5).
        02 PRODUCT PIC 9999.99 DISPLAY.
        02 FILLER PIC X(6).
        02 TOTAL PIC 9(7) DISPLAY.
01 FINAL-REC.
        02 FINAL-TITLE PIC X(10) VALUE ' TOTAL    '.
        02 FINAL-IX PIC 9(6).
        02 FINAL-X PIC 9(6).
        02 FINAL-PRODUCT PIC 9(6).
        02 FINAL-TOTAL PIC 9(9).
PROCEDURE DIVISION.
INPUT-DATA.
        DISPLAY 'ENTER GROWTH RATE :' WITH NO ADVANCING.
        ACCEPT GROWTH.
        DISPLAY 'ENTER FIRST MONTH TYMCOM-IX AMOUNT :'WITH NO ADVANCING.
        ACCEPT TYMCOM-IX (1).
        DISPLAY 'ENTER FIRSTMONTH TYMCOM-X AMOUNT :'WITH NO ADVANCING.
        ACCEPT TYMCOM-X (1).
        DISPLAY 'ENTER FIRST MONTH PRODUCT AMOUNT :'WITH NO ADVANCING.
        ACCEPT PRODUCT (1).
PRINT-HEADING-SECTION.
        MOVE '               QUARTERLY FORECAST REPORT' TO PRINT-HEADING.
        DISPLAY PRINT-HEADING.
        DISPLAY PRINT-HEADING.
FIRST-MONTH.
        MOVE 'MONTH 1    ' TO MONTH-TITLE (1).
        COMPUTE TOTAL (1)=TYMCOM-X(1) + TYMCOM-X (1) + PRODUCT (1).
        SET MONTH TO 1.
NEXT-MONTHS.
        MULTIPLY TYMCOM-IX (MONTH) BY GROWTH GIVING TYMCOM-IX (MONTH + 1).
        MULTIPLY TYMCOM-X (MONTH) BY GROWTH GIVING TYMCOM-X (MONTH + 1).
        MULTIPLY PRODUCT (MONTH) BY GROWTH GIVING PRODUCT (MONTH + 1).
        MOVE 'MONTH 2   ' TO MONTH-TITLE (2).
        MOVE 'MONTH 3   ' TO MONTH-TITLE (3).
        SET MONTH UP BY 1.
        IF MONTH LESS THAN 3 GO TO NEXT-MONTHS.
        SET MONTH TO 1.
PRODUCE-REPORT.
        COMPUTE FINAL-IX=FINAL-IX + TYMCOM-IX (MONTH).
        COMPUTE FINAL-X=FINAL-X + TYMCOM-X (MONTH).
        COMPUTE FINAL-PRODUCT=FINAL-PRODUCT + PRODUCT (MONTH).
        COMPUTE FINAL-TOTAL = FINAL-TOTAL + TOTAL (MONTH).
        SET MONTH UP BY 1.
        IF MONTH LESS THAN 4 GO TO PRODUCE-REPORT.
        SET MONTH TO 1.
PRINT-OUT.
        MOVE '   TOTAL  ' TO FINAL-TITLE.
        DISPLAY PERIOD-DATA (MONTH).
        SET MONTH UP BY 1.
        IF MONTH LESS THAN 4 GO TO PRINT-OUT.
        DISPLAY FINAL-REC.
END-OF-JOB.
        STOP RUN.
 