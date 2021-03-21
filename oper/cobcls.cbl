IDENTIFICATION DIVISION.
PROGRAM-ID. COBCLS.
AUTHOR. AL GOLDSTEIN.
INSTALLATION. TYMSHARE.
DATE-WRITTEN. JUNE 8, 1973.
SECURITY. NONE.
REMARKS. PORTFOLIO REPORTING SYSTEM.
 ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT OUTFILE ASSIGN TO DSK
        RECORDING MODE IS ASCII.
 DATA DIVISION.
FILE SECTION.
FD OUTFILE
        VALUE OF IDENTIFICATION IS "OUTFILDAT"
01 OUTREC
                02 STOCK
                02 NO-OF-SHARES PIC ZZ,ZZZ,ZZ9.99
                02 PRICE-PER-SHARE PIC $$$$.$$
                02 TOTAL-VALUE PIC $$,$$$,$$$.$$
   WORKING-STORAGE SECTION.
01 HEADING
        02 FILLER PIC X(5) VALUE 'STOCK'
        02 FILLER PIC X(10) VALUE SPACES.
        O2 FILLER PIC X(6) VALUE 'SHARES'.
        02 FILLER PIC X(6) VALUE SPACES.
        02 FILLER PIC X(5) VALUE 'PRICE'.
        02 FILLER PIC X(11) VALUE SPACES.
        02 FILLER PIC X(5) VALUE 'VALUE'.
O1 DATALINE
        02 STOCK PIC X(10)
        02 FILLER PIC X(5) VALUE SPACES.
        02 NO-OF-SHARES-WS PIC ZZ,ZZZ,ZZ9.99
        02 FILLER PIC X(5) VALUE SPACES.
        O2 PRICE-PER-SHARE-WS PIC $$$$.$$

 PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
 