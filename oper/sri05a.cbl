IDENTIFICATION  DIVISION.
PROGRAM-ID.  'ACT3005A'.
REMARKS. PROGRAM LISTS INPUT TAPE.
ENVIRONMENT  DIVISION.
CONFIGURATION  SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT  SECTION.
FILE-CONTROL.
     SELECT PRINT-1  ASSIGN DSK,
      RECORDING MODE IS ASCII.
     SELECT NEW-TAPE ASSIGN DSK,
      RECORDING MODE IS ASCII.
DATA  DIVISION.
FILE  SECTION.
FD   PRINT-1
         VALUE OF IDENTIFICATION IS "PRNT3BDAT"
     LABEL RECORDS ARE STANDARD
     DATA RECORD IS PRINT.
01   PRINT.
     03 FILLER                   PICTURE X(01).
     03 PRINT-DATA               PICTURE X(80).
FD   NEW-TAPE
         VALUE OF IDENTIFICATION IS "TOUT3ADAT"
     LABEL RECORDS ARE STANDARD
     BLOCK CONTAINS 40 RECORDS
     RECORD CONTAINS 080 CHARACTERS
     DATA RECORD IS NEW-RECORD.
01   NEW-RECORD                  PICTURE X(080).
WORKING-STORAGE  SECTION.
77   CC                          PICTURE 9(01).
77   LINE-CTR                    PICTURE 9(02)   VALUE 60.
PROCEDURE  DIVISION.
START.
     OPEN INPUT  NEW-TAPE
          OUTPUT PRINT-1.
AGAIN.
     READ NEW-TAPE AT END GO TO EOJ.
     ADD 1 TO LINE-CTR.
     IF LINE-CTR < 55
        MOVE 0 TO CC
     ELSE
        MOVE 0 TO LINE-CTR
        MOVE 1 TO CC.
     MOVE NEW-RECORD TO PRINT-DATA.
     WRITE PRINT AFTER ADVANCING CC.
     GO TO AGAIN.
EOJ.
     CLOSE NEW-TAPE
           PRINT-1.
     STOP RUN.
 