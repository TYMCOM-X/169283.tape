IDENTIFICATION DIVISION.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT IN-FILE, ASSIGN TO DSK,
                RECORDING MODE IS BINARY.
DATA DIVISION.
FILE SECTION.
FD      IN-FILE
                VALUE OF ID IS 'FTNCBLDAT'
        BLOCK CONTAINS 64 RECORDS.
01      IN-REC.
        02      A-TEXT PIC X(5) DISPLAY-7.
        02 N-BIN COMP-1.
PROCEDURE DIVISION.
SETUP.
        OPEN INPUT IN-FILE.
LOOP.
        READ IN-FILE AT END GO TO EOJ.
        DISPLAY A-TEXT"   "N-BIN.
        GO TO LOOP.
EOJ.
        DISPLAY 'ALL DONE'.
        CLOSE IN-FILE.
        STOP RUN.
 