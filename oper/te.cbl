IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-EDIT.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
77 UE PICTURE 9999V99.
77 E PICTURE $$$$$.99.
PROCEDURE DIVISION.
EDIT-PROCEDURE.
DISPLAY "ENTER UE:".  ACCEPT UE.
DISPLAY UE. MOVE UE TO E. DISPLAY E. STOP RUN.
  