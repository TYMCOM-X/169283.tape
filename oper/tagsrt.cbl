IDENTIFICATION DIVISION.
PROGRAM-ID. TAGSRT.
AUTHOR. CHUCK MCCOMAS.
REMARKS. THE FOLLOWING IS A PROGRAM THAT PERFORMS A TAG SORT.
        IT CAN BE USED BY ANY PROGRAMMER WHO NEEDS TO USE THIS
        CAPABILITY AS LONG AS HE ADJUSTS THE RECORD SIZES, FILE
        NAMES, AND RECORDING MODES TO MATCH HIS FILES.
ENVIRONMENT DIVISION.
I-O SECTION.
FILE-CONTROL.
        SELECT INPUT-FILE ASSIGN DSK.
        SELECT RANDOM-INPUT ASSIGN DSK
           ACCESS RANDOM
           ACTUAL KEY SORT-RECORD
           FILE LIMIT 10000.
        SELECT SORT-FILE ASSIGN DSK, DSK, DSK.
        SELECT OUTPUT-FILE ASSIGN DSK.
DATA DIVISION.
FILE SECTION.
FD  INPUT-FILE VALUE OF ID IS 'WHTEVRDAT'
        BLOCK CONTAINS 8 RECORDS.
01  INPUT-RECORD.
        05 INPUT-KEY-1 PIC X(4).
        05 FILLER PIC X(4).
        05 INPUT-KEY-2 PIC X(4).
        05 FILLER PIC X(68).
FD  RANDOM-INPUT  VALUE OF ID IS 'WHTEVRDAT'
        BLOCK CONTAINS 8 RECORDS.
01  RANDOM-RECORD PIC X(80).
SD  SORT-FILE.
01  SORT-RECORD PIC 9(10) COMP.
FD  OUTPUT-FILE VALUE OF ID IS 'SORTEDDAT'
        BLOCK CONTAINS 8 RECORDS.
01  OUTPUT-RECORD PIC X(80).
PROCEDURE DIVISION.
MAIN SECTION.
START.
        SORT SORT-FILE ON ASCENDING KEYS INPUT-KEY-1, INPUT-KEY-2,
           INPUT PROCEDURE TAG-SORT-INPUT
           OUTPUT PROCEDURE TAG-SORT-OUTPUT.
        STOP RUN.
TAG-SORT-INPUT SECTION.
START.
        OPEN INPUT INPUT-FILE.
        MOVE 1 TO SORT-RECORD.
LOOP.
        READ INPUT-FILE AT END GO TO LOOP-END.
        RELEASE SORT-RECORD.
        SET SORT-RECORD UP BY 1.
        GO TO LOOP.
LOOP-END.
        CLOSE INPUT-FILE.
TAG-SORT-OUTPUT SECTION.
START.
        OPEN INPUT RANDOM-INPUT
             OUTPUT OUTPUT-FILE.
LOOP.
        RETURN SORT-FILE AT END GO TO LOOP-END.
        READ RANDOM-INPUT
           INVALID KEY DISPLAY "?", GO TO LOOP-END.
        WRITE OUTPUT-RECORD FROM RANDOM-RECORD.
        GO TO LOOP.
LOOP-END.
        CLOSE OUTPUT-FILE, RANDOM-INPUT.
  