IDENTIFICATION DIVISION.
PROGRAM-ID.  OCCRS.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01      SUB-VALUES.
	02      A       PICTURE 9(5).
	02      B       PICTURE 9(5).
	02      C       PICTURE 9(5).

01  MATRIX.
        02  MAT1    OCCURS 2.
                03  MAT2     OCCURS 7500.
                        04  MATF   PICTURE 9.
                        04  MATP      PICTURE 9(5).

PROCEDURE DIVISION.
FIRST-PARA.
        MOVE MATF (B,C) TO A.
        STOP RUN.
LAST-PARA.
    