IDENTIFICATION DIVISION.
PROGRAM-ID. PROG1.
AUTHOR. N M KIRKPATRICK.
DATE-WRITTEN. APRIL 4 1973.
DATE-COMPILED. TODAY.
SECURITY. TRIVIAL.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT INMAST ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT OUTMAST ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT SORTER ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD INMAST
        BLOCK CONTAINS 1 RECORD
        RECORD CONTAINS 60 CHARACTERS
        LABEL RECORDS ARE OMITTED
        DATA RECORDS ARE MAST-REC.
        01 MAST-REC.
          02 NUMBER.
          02 NAME.
          02 DATE.
          02 COST.
          02 QUANTITY.
FD OUTMAST
        BLOCK CONTAINS 1 RECORD
        RECORD CONTAINS 60 CHARACTERS
        LABEL RECORDS ARE OMITTED
        DATA RECORDS ARE N-MAST-R.
        01 N-MAST-R.
          02 NUMBER.
          02 NAME.
          02 DATE.
          02 COST.
          02 QUANTITY.
SD SORTER
        BLOCK CONTAINS 1 RECORD
        LABEL RECORDS OARE OMITTED
        DATA RECORD IS SORTER-REC.
        01 SORTER-REC.
          02 NUMBER PIC A.
          02 NAME PIC X.
          02 DATE PIC.
          02 COST PIC.
          02 QUANITY PIC.
FD PRINTF
        BLOCK CONTAINS 1 RECORD
        RECORD CONTAINS 132 CHARACTERS
        LABEL RECORDS ARE OMITTED.
        01 PRINT-REC.
          02 NUMBER PIC 9(3).
          02 BLNK PIC X(3).
          02 NAME PIC X(32).
          02 BLNK PIC X(3).
          02 DATE PIC X(5).
          02 BLNK PIC X(4).
          02 COST PIC 9(5V2).
          02 BLNK PIC X(3).
          02 QUANTITY PIC 9(4).
          02 BLNK PIC X(40).
WORKING-STORAGE SECTION.
01 PRINT-LINE.
  02 NUMBER PIC 9(2).
  02 FILLER PIC X(3).
  02 NAME PIC X(32).
  02 FILLER PIC X(3)
  02 DATE PIC X(59.
  02 FILLER PIC S(3).
  02 COST PIC 9(4).
  02 FILLER PIC X(3).
  02 QUANITY PIC 9(5).
  02 FILLER PIC X(3).
        01 RANGE1.
          02 FILLER PIC 999 VALUE 149.
          02 FILLER PIC 999 VALUE 199.
          02 FILLER PIC 999 VALUE 240.
          02 FILLER PIC 999 VALUE 299.
          02 FILLER PIC 999 VALUE 399.
          02 FILLER PIC 999 VALUE 599.
          02 FILLER PIC 999 VALUE 699.
          02 FILLER PIC 999 VALUE 799.
        01 RANGE2 REDEFINES RANGE1.
          02 RANGE-SUB OCCURS 9 TIMES PIC 999..
  02 TOTAL-COST PIC 9(5).
  