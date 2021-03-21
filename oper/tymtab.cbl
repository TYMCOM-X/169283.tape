IDENTIFICATION DIVISION.
PROGRAM-ID. 'TYMTAB'.
REMARKS. THIS PROGRAM IS A BENCHMARK FOR
         A TYMTAB MODEL.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT REVFILE ASSIGN TO DSK
         RECORDING MODE IS ASCII.
        SELECT COSTFILE ASSIGN TO DSK
         RECORDING MODE IS ASCII.
        SELECT OUTFILE ASSIGN TO DSK
         RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD  REVFILE  VALUE OF ID IS 'REV   DAT'.
01  REVREC  PIC S9(6).
FD  COSTFILE  VALUE OF ID IS 'COST  DAT'.
01  COSTREC  PIC S9(6).
FD  OUTFILE  VALUE OF ID IS 'OUTRPTDAT'.
01  OUTREC.
        05 OUT-MARGIN   PIC X(15).
        05 OUT-JAN      PIC X(7).
        05 OUT-JAN-ED REDEFINES OUT-JAN PIC ZZZ,ZZ9.
        05 FILLER       PIC X.
        05 OUT-FEB      PIC X(9).
        05 OUT-FEB-ED REDEFINES OUT-FEB PIC Z,ZZZ,ZZ9.
        05 FILLER       PIC XX.
        05 OUT-MAR      PIC X(7).
        05 OUT-MAR-ED REDEFINES OUT-MAR PIC ZZZ,ZZ9.
        05 FILLER       PIC XX.
        05 OUT-QTR      PIC X(11).
        05 OUT-QTR-ED REDEFINES OUT-QTR PIC ZZZ,ZZZ,ZZ9.
WORKING-STORAGE SECTION.
01 HEADING-1.
        05 FILLER PIC X(29) VALUE SPACES.
        05 FILLER PIC X(11) VALUE 'ABC COMPANY'.
        05 FILLER PIC X(14) VALUE SPACES.
01 HEADING-2.
        05 FILLER PIC X(25) VALUE SPACES.
        05 FILLER PIC X(19) VALUE 'OPERATING STATEMENT'.
        05 FILLER PIC X(10) VALUE SPACES.
01 HEADING-3.
        05 FILLER PIC X(25) VALUE SPACES.
        05 FILLER PIC X(19) VALUE 'FIRST QUARTER, 1972'.
        05 FILLER PIC X(10) VALUE SPACES.
01 QTRREV       PIC S9(7) VALUE ZEROES COMP.
01 QTRCOST      PIC S9(7) VALUE ZEROES COMP.
01 QTROVERHEAD  PIC S9(7) VALUE ZEROES COMP.
01 QTROTHER     PIC S9(7) VALUE ZEROES COMP.
01 QTRTOTCOST   PIC S9(7) VALUE ZEROES COMP.
01 QTRPROFIT    PIC S9(7) VALUE ZEROES COMP.
01 QTRPERCENT-AREA.
        05 QTRPERCENT   PIC S99V999 COMP VALUE ZEROES.
01 TABLE.
        05 HOLDERS OCCURS 3 TIMES INDEXED BY IDX.
                10 REVENUE      PIC S9(6) COMP.
                10 COST         PIC S9(6) COMP.
                10 OVERHEAD     PIC S9(6) COMP.
                10 OTHER        PIC S9(6) COMP.
                10 TOTCOST      PIC S9(7) COMP.
                10 PROFIT       PIC S9(6) COMP.
                10 PERCENT      PIC S99V999  COMP.
PROCEDURE DIVISION.
HSE-KEEP.
        OPEN INPUT REVFILE, COSTFILE.
        OPEN OUTPUT OUTFILE.
        SET IDX TO 0.
WRITE-HEADINGS.
        WRITE OUTREC FROM HEADING-1.
        WRITE OUTREC FROM HEADING-2.
        WRITE OUTREC FROM HEADING-3 BEFORE ADVANCING 4 LINES.
        MOVE SPACES TO OUTREC.
        MOVE 'JANUARY' TO OUT-JAN.
        MOVE ' FEBRUARY' TO OUT-FEB.
        MOVE '  MARCH' TO OUT-MAR.
        MOVE '1ST QUARTER' TO OUT-QTR.
        WRITE OUTREC BEFORE ADVANCING 2 LINES.
        MOVE SPACES TO OUTREC.
READ-DATA.
        SET IDX UP BY 1.
        READ REVFILE INTO REVENUE (IDX) AT END GO TO COMPUTE-DATA.
        READ COSTFILE INTO COST (IDX) AT END GO TO COMPUTE-DATA.
        IF IDX EQUAL TO 3 GO TO COMPUTE-DATA.
        GO TO READ-DATA.
COMPUTE-DATA.
        SET IDX TO 1.
CD1.    MULTIPLY COST (IDX) BY .5 GIVING OVERHEAD (IDX) ROUNDED.
        MULTIPLY REVENUE (IDX) BY .2 GIVING OTHER (IDX) ROUNDED.
        ADD COST (IDX),OVERHEAD (IDX),OTHER (IDX) TO TOTCOST (IDX).
        SUBTRACT TOTCOST (IDX) FROM REVENUE (IDX) GIVING
          PROFIT (IDX).
        COMPUTE PERCENT (IDX)  = 100*(PROFIT (IDX)/
           REVENUE (IDX)).
        ADD .5 TO PERCENT (IDX).
        ADD REVENUE (IDX)       TO QTRREV.
        ADD COST (IDX)          TO QTRCOST.
        ADD OVERHEAD (IDX)      TO QTROVERHEAD.
        ADD OTHER (IDX)         TO QTROTHER.
        ADD TOTCOST (IDX)       TO QTRTOTCOST.
        ADD PROFIT (IDX)        TO QTRPROFIT.
        IF IDX EQUAL TO 3 GO TO WRITE-DATA.
        SET IDX UP BY 1.
        GO TO CD1.
WRITE-DATA.
        MOVE 'SALES REVENUE' TO OUT-MARGIN.
        MOVE REVENUE (1) TO OUT-JAN-ED.
        MOVE REVENUE (2) TO OUT-FEB-ED.
        MOVE REVENUE (3) TO OUT-MAR-ED.
        MOVE QTRREV      TO OUT-QTR-ED.
        WRITE OUTREC BEFORE ADVANCING 2 LINES.
        MOVE 'COST OF SALES' TO OUT-MARGIN.
        MOVE COST (1) TO OUT-JAN-ED.
        MOVE COST (2) TO OUT-FEB-ED.
        MOVE COST (3) TO OUT-MAR-ED.
        MOVE QTRCOST  TO OUT-QTR-ED.
        WRITE OUTREC.
        MOVE 'OVERHEAD     ' TO OUT-MARGIN.
        MOVE OVERHEAD (1) TO OUT-JAN-ED.
        MOVE OVERHEAD (2) TO OUT-FEB-ED.
        MOVE OVERHEAD (3) TO OUT-MAR-ED.
        MOVE QTROVERHEAD  TO OUT-QTR-ED.
        WRITE OUTREC.
        MOVE 'OTHER COSTS' TO OUT-MARGIN.
        MOVE OTHER (1) TO OUT-JAN-ED.
        MOVE OTHER (2) TO OUT-FEB-ED.
        MOVE OTHER (3) TO OUT-MAR-ED.
        MOVE QTROTHER  TO OUT-QTR-ED.
        WRITE OUTREC BEFORE ADVANCING 2 LINES.
        MOVE 'TOTAL COSTS' TO OUT-MARGIN.
        MOVE TOTCOST (1) TO OUT-JAN-ED.
        MOVE TOTCOST (2) TO OUT-FEB-ED.
        MOVE TOTCOST (3) TO OUT-MAR-ED.
        MOVE QTRTOTCOST  TO OUT-QTR-ED.
        WRITE OUTREC BEFORE ADVANCING 2 LINES.
        MOVE 'PROFIT     ' TO OUT-MARGIN.
        MOVE PROFIT (1) TO OUT-JAN-ED.
        MOVE PROFIT (2) TO OUT-FEB-ED.
        MOVE PROFIT (3) TO OUT-MAR-ED.
        MOVE QTRPROFIT  TO OUT-QTR-ED.
        WRITE OUTREC.
        MOVE '% OF SALES' TO OUT-MARGIN.
        MOVE PERCENT (1) TO OUT-JAN-ED.
        MOVE PERCENT (2) TO OUT-FEB-ED.
        MOVE PERCENT (3) TO OUT-MAR-ED.
        COMPUTE QTRPERCENT = 100*(QTRPROFIT/QTRREV)
        ADD .5 TO QTRPERCENT.
        MOVE QTRPERCENT  TO OUT-QTR-ED.
        WRITE OUTREC.
        PERFORM DASHES THRU DASHES-EXIT.
EOJ.
        CLOSE REVFILE,COSTFILE,OUTFILE.
        STOP RUN.
DASHES.
        MOVE SPACES TO OUT-MARGIN.
        MOVE '-------' TO OUT-JAN,OUT-MAR.
        MOVE ' --------' TO OUT-FEB.
        MOVE '-----------' TO OUT-QTR.
        WRITE OUTREC.
        WRITE OUTREC.
DASHES-EXIT.  EXIT.
  