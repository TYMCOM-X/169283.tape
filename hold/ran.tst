 IDENTIFICATION DIVISION.
 PROGRAM-ID. NAME.
AUTHOR. XXX.
 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
        SELECT DATAX ASSIGN DSK
        FILE-LIMIT IS 1 THRU 5
        ACCESS MODE IS RANDOM
        ACTUAL KEY IS PARTX
        RECORDING MODE IS ASCII.
        SELECT SORTFILE ASSIGN DSK, DSK, DSK.
        SELECT XYZ ASSIGN DSK
        RECORDING MODE IS ASCII.
 DATA DIVISION.
 FILE SECTION.
 FD DATAX BLOCK CONTAINS 1 RECORDS
        VALUE OF IDENTIFICATION IS "DATA1X   ".
 01 RECORDX.
        02 FILLER PIC X.
        02 BODE PIC XX.
        02 PART PIC XXXX.
FD SORTFILE VALUE OF ID "A23456B89" LABEL RECORDS ARE STANDARD.
01 REC-SORT PIC X(80).
FD XYZ
        VALUE OF ID "ABCDEFBII"
        LABEL RECORDS ARE STANDARD.
01 REC-XYZ PIC X(80).
 WORKING-STORAGE SECTION.
 77 PARTX PIC S99 COMP.
77 REC-B COMP-1.
77 A PIC X(8).
77 B PIC A(8).
77 C PIC X(70) VALUE IS "VVVVVVVVVVVVBBBBBBBBBBBBNNNNNNNNNNNNMMMMMMMXXXXXXXXXXX".
 PROCEDURE DIVISION.
 PAR1.
        OPEN I-O DATAX.
 PAR2.
        DISPLAY 'ENTER RECORD #  '.
        ACCEPT PARTX.
        READ DATAX INVALID KEY GO TO ERRORX.
        DISPLAY RECORDX.
        GO TO PAR2.
 ERRORX.
        DISPLAY 'ERROR- ....'
        DISPLAY 'ENTERED #',PARTX.
        GO TO PAR2.
        IF A > B NEXT SENTENCE.
        IF A < B NEXT SENTENCE.
        IF A = B NEXT SENTENCE.
        IF A GREATER B NEXT SENTENCE.
        IF A GREATER THAN B NEXT SENTENCE.
        IF A LESS THAN B NEXT SENTENCE.
        IF A LESS B NEXT SENTENCE.
        IF A EQUAL B NEXT SENTENCE.
        IF A EQUALS B NEXT SENTENCE.
 