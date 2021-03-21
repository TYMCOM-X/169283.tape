  IDENTIFICATION DIVISION.
  PROGRAM-ID. PROG.
  ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
  FILE CONTROL.
      SELECT ERIC ASSIGN TO DSK.
  DATA DIVISION.
  FILE SECTION.
  FD ERIC.
      LABEL RECORDS ARE STANDARD
      VALUE OF ID IS "ERIC.DAT"
      RECORD CONTAINS 1 CHARACTER
      DATA RECORD IS GEORGE.
  01 GEORGE PICTURE X(1)
  WORKING-STORAGE SECTION.
  01 REC-COUNT PICTURE 99999 COMPUTATIONAL VALUE ZERO.
  PROCEDURE DIVISION.
  START.
      OPEN OUTPUT ERIC.
  MEXICO.
      MOVE "A" TO GEORGE.
      WRITE GEORGE.
      ADD 1 TO REC-COUNT.
      IF REC-COUNT=20000 GO TO EOJ.
      GO TO MEXICO.
  EOJ.
      STOP RUN.
    