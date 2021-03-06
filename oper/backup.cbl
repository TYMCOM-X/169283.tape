IDENTIFICATION DIVISION.
PROGRAM-ID. VENDOR.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. DECSYSTEM-10.
OBJECT-COMPUTER. PDP-10.
SPECIAL-NAMES.
     CHANNEL (1) IS TOP-OF-PAGE.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
     SELECT VENDOR-FILE ASSIGN TO DSK
     RECORDING MODE IS ASCII.
     SELECT INVENTORY-FILE ASSIGN TO DSK
     RECORDING MODE IS ASCII.
     SELECT REPORT-FILE ASSIGN TO DSK
     RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD VENDOR-FILE
     LABEL RECORDS ARE STANDARD
     VALUE OF IDENTIFICATION IS "VENDOR   ".
01 VENDOR-REC DISPLAY-7.
     05 NAME PIC X(15).
     05 ITEM-NUMBER PIC 999.
     05 UNIT-PRICE PIC 9(3)V99.
FD INVENTORY-FILE
     VALUE OF IDENTIFICATION IS "NVNTRY   ".
01 INVENTORY-REC DISPLAY-7.
     05 ITEM-NUMBER PIC 999.
     05 QTY-ON-HAND PIC 9(4).
     05 MINIMUM-QTY PIC 999.
     05 REORDER-QTY PIC 9(4).
FD REPORT-FILE
     VALUE OF IDENTIFICATION IS "REPORTVEN".
01 REPORT-REC1 PIC X(72).
WORKING-STORAGE SECTION.
77 IDX-TOTAL PIC 9(5) VALUE ZERO.
01 VENDOR-TABLE.
     05 VENDOR-TBL OCCURS 50 TIMES, INDEXED BY IDX.
        10 NAME PIC X(15).
        10 ITEM-NUMBER PIC 999.
        10 UNIT-PRICE PIC 9(3)V99.
        10 REORDER-QTY PIC 9(4).
01 VENDOR-NAME PIC X(15) VALUE SPACE.
01 VENDOR-RECOUT.
     05 FILLER PIC X(7) VALUE SPACES.
     05 NAME PIC X(15).
     05 FILLER PIC X(9) VALUE SPACES.
     05 ITEM-NUMBER PIC 999.
     05 FILLER PIC X(9) VALUE SPACES.
     05 REORDER-QTY PIC ZZZ9.
     05 FILLER PIC X(9) VALUE SPACES.
     05 AMOUNT PIC $(8).99.
     05 FILLER PIC X(5) VALUE SPACES.
01 INV-REC.
     05 ITEM-NUMBER PIC 999.
     05 QTY-ON-HAND PIC 9(4).
     05 MINIMUM-QTY PIC 999.
     05 REORDER-QTY PIC 9(4).
01 HDG-1 PIC X(47) VALUE
     '                                 SIMPLE COMPANY'.
01 HDG-2 PIC X(47) VALUE
     '                                 VENDOR  REPORT'.
01 HDG-3 PIC X(34) VALUE
     '            VENDOR            ITEM'.
01 HDG-4 PIC X(72) VALUE
     '             NAME             NUMBER     QUANTITY           AMOUNT'.
PROCEDURE DIVISION.
HOUSE-KEEP.
     OPEN INPUT VENDOR-FILE.
     OPEN INPUT INVENTORY-FILE.
     OPEN OUTPUT REPORT-FILE.
     MOVE ZERO TO IDX.
     PERFORM BUILD-VENDOR-TABLE THRU BUILD-EXIT.
MAIN-PROGRAM.
     READ INVENTORY-FILE AT END CLOSE INVENTORY-FILE,
        MOVE ZERO TO IDX, GO TO WRITE-REPORT.
    MOVE CORRESPONDING INVENTORY-REC TO INV-REC.
     IF QTY-ON-HAND IN INV-REC  NOT LESS THAN MINIMUM-QTY IN INV-REC  GO TO MAIN-PROGRAM.
    MOVE ZERO TO IDX.
SEARCH-VENDOR.
     ADD 1 TO IDX.
     IF ITEM-NUMBER IN INV-REC NOT EQUAL ITEM-NUMBER IN VENDOR-TBL (IDX),
     GO TO SEARCH-VENDOR.
     MOVE REORDER-QTY IN INVENTORY-REC TO REORDER-QTY IN VENDOR-TBL (IDX).
     GO TO MAIN-PROGRAM.
WRITE-REPORT.
    ADD 1 TO IDX.
     IF IDX GREATER IDX-TOTAL, GO TO WRITE-EXIT.
     IF REORDER-QTY IN VENDOR-TBL (IDX) EQUAL ZERO, GO TO WRITE-REPORT.
IF NAME IN VENDOR-TBL (IDX) EQUAL VENDOR-NAME, GO TO REPORT-LINES.
WRITE REPORT-REC1 FROM HDG-1 AFTER ADVANCING TOP-OF-PAGE.
WRITE REPORT-REC1 FROM HDG-2 AFTER ADVANCING 2 LINES.
WRITE REPORT-REC1 FROM HDG-3 AFTER ADVANCING 2 LINES.
WRITE REPORT-REC1 FROM HDG-4 AFTER ADVANCING 1 LINE.
MOVE NAME IN VENDOR-TBL (IDX) TO VENDOR-NAME.
REPORT-LINES.
     MOVE CORRESPONDING VENDOR-TBL (IDX) TO VENDOR-RECOUT.
     COMPUTE AMOUNT = REORDER-QTY IN VENDOR-TBL (IDX) * UNIT-PRICE IN VENDOR-TBL (IDX).
     WRITE REPORT-REC1 FROM VENDOR-RECOUT AFTER ADVANCING 2 LINES.
     GO TO WRITE-REPORT.
WRITE-EXIT.
     STOP RUN.
BUILD-VENDOR-TABLE.
     ADD 1 TO IDX.
     READ VENDOR-FILE INTO VENDOR-TBL (IDX),
        AT END CLOSE VENDOR-FILE, GO TO BUILD-EXIT.
     MOVE ZERO TO REORDER-QTY IN VENDOR-TBL (IDX).
     GO TO BUILD-VENDOR-TABLE.
BUILD-EXIT.
     MOVE IDX TO IDX-TOTAL.
     MOVE ZERO TO IDX.
   