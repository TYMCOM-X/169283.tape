IDENTIFICATION DIVISION.
PROGRAM-ID. CASTFD.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT INFILE ASSIGN DSK.
DATA DIVISION.
FILE SECTION.
FD INFILE VALUE OF ID IS "ABCDEF123".
01      INFILE-REC PIC X(80).
PROCEDURE DIVISION.
FIRST-PARA.
        OPEN INPUT INFILE.
        STOP RUN.
