IDENTIFICATION DIVISION.
PROGRAM-ID. CLASS1.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT INVENTORY-FILE ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT VENDOR-FILE ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT VENDOR-REPORT ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD  INVENTORY-FILE
        VALUE OF IDENTIFICATION IS
01  INVENTORY-REC PIC X(14).
FD  VENDOR-FILE
        VALUE OF IDENTIFICATION IS
01  VENDOR-REC PIC X(23).
FD  VENDOR-REPORT
        VALUE OF IDENTIFICATION IS
01  REPORT-REC PIC X(50).
WORKING-STORAGE SECTION.
77  NUMERIC-TOTAL PIC S9(6)V99 COMP.
01  VENDOR-TABLE.
       03 VNDR-TBL OCCURS 50 TIMES INDEXED BY I.
        05 VENDOR-NAME PIC X(15).
        05 ITEM-NUMBER PIC XXX.
        05 PRICE-PER   PIC 9(3)V99.
        05 QUANTITY    PIC 9(4).
PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
        MOVE ZEROES TO VENDOR-TABLE.
        OPEN INPUT INVENTORY-FILE.
        OPEN INPUT VENDOR-FILE.
        OPEN OUTPUT VENDOR-REPORT.
 