IDENTIFICATION DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 PRICE-TABLE PIC 99 OCCURS 50 TIMES INDEXED BY IND.
01 T-MODEL-NUMB PIC 99 VALUE IS 15.
01 P-MODEL-NUMB PIC 99 VALUE IS 15.
PROCEDURE DIVISION.
START.
        SEARCH PRICE-TABLE AT END GO TO PAR3
        WHEN T-MODEL-NUMB = P-MODEL-NUMB GO TO PAR3.
PAR3.
        STOP RUN.
   