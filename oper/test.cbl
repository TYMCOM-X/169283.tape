IDENTIFICATION DIVISION.
PROGRAM-ID.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
COPY SELLIB.
COPY SELLIB REPLACING MNF WITH ODSF.
WORKING-STORAGE SECTION.
77      MNF-KEY PIC 9(5) COMP.
77      PNF-KEY PIC 9(5) COMP.
77      POSF-KEY        PIC 9(5) COMP.
77      ODSF-KEY        PIC 9(5) COMP.
77      PPSF-KEY        PIC 9(5) COMP.
77      CPEP-KEY    PIC 9(5)        COMP.
PROCEDURE DIVISION.
INITIALZATION-PARA.
        DISPLAY 'MADE IT TO PROGRAM STARTUP'.
        STOP RUN.
   