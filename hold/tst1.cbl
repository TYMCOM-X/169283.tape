IDENTIFICATION DIVISION.
PROGRAM-ID. TST1.
AUTHOR. ME .
INSTALLATION. HERE.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT XYZ ASSIGN TO DSK
        FILE-LIMITS ARE 1 THRU 100,
        ACCESS RANDOM
        ACTUAL KEY IS ACT-KEY,
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.

FD  XYZ
        VALUE OF IDENTIFICATION IS "TST1  DAT",
        BLOCK CONTAINS 1 RECORD.
01 XYZ-REC PICTURE X(80).

WORKING-STORAGE SECTION.
77      ACT-KEY PICTURE 9(5) COMP.
PROCEDURE DIVISION.
FIRST-PARA.
        OPEN I-O XYZ.
        MOVE 1 TO ACT-KEY.
SECOND-P.
        READ XYZ, INVALID KEY STOP RUN.
        ADD 1 TO ACT-KEY.
        DISPLAY ACT-KEY, XYZ-REC.
        GO TO SECOND-P.
   