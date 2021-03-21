ID DIVISION.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT IP-FILE ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT OP-FILE ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD IP-FILE
        VALUE OF ID IS "ACCNEWDAT".
01 IP-REC   PIC X(176).
FD OP-FILE
        VALUE OF ID IS "ACCNW1DAT".
01  OP-REC   PIC X(176).
PROCEDURE DIVISION.
START.
        OPEN INPUT IP-FILE.
        OPEN OUTPUT OP-FILE.
BEGIN.
        READ IP-FILE AT END GO TO EOJ.
        DISPLAY IP-REC.
        GO TO BEGIN.
EOJ.
        CLOSE IP-FILE, OP-FILE.
        STOP RUN.
