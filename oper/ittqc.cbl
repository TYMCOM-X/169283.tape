IDENTIFICATION DIVISION.
PROGRAM-ID. ITTQC.
AUTHOR. ADEY.
INSTALLATION. PDP-10.
DATE-WRITTEN. 7/17/73.
REMARKS. PRELIM.PROG.
    ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT INPUT-MASTER ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT OUT-CODE1. ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT OUT-CODE2. ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT OUT-CODE4. ASSIGN TO DSK
        RECORDING MODE IS ASCII.
 DATA DIVISION.
FILE SECTION.
FD INPUT-MASTER
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "INPUTMAST"
01 RECD-IN
                02 D1 PIC 9.
                02 D2 PIC 9.
                02 ITRNC PIC 9(4).
                02 OS-ANSBK PIC 9(20)
                02 DEST-SELCT PIC 9(16).
                02 CALL-HOLD PIC 9(5).
                02 FILLER PIC 9(5).
                02 LN-ADRSS PIC 9(4).
                02 FILLER PIC 9(8).
                02 DATE-ALL PIC 9(8).
                        05 DATE PIC 9(4).
                                07 MO PIC 9(2).
                                07 DY PIC 9(2).
                02 SUBSC-SWTCH PIC 9.
                02 OUT-TRNK-NR PIC 9(4).
                02 ROUT-IND PIC 9.
                02 DSCNT-TIME PIC 9(4).
                02 ELAPS-TIME PIC 9(5).
                02 IN-TRNK PIC 9(3).
                02 FILLER PIC 9(32).
FD OUT-CODE1.
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "OUTCODE1 "
        DATA RECORDS ARE CODE1-RECD.
01 CODE1-RECD.
                02 D1 PIC 9.
                02 D2 PIC 9.
                02 ITRNC PIC 9(4).
                02 0S-ANSBK PIC 9(20).
                02 DEST-SELCT PIC 9(16).
                        05 CNTY-SLECT 9(4).
                        05 FILLER 9(12).
                02 LN-ADRESS PIC 9(4).
                02 DATE-ALL PIC 9(8).
                        05 DATE PIC 9(4).
                                07 MO PIC 9(12).
                                07 DY PIC 9(2).
                        05 PRECD-SLCT PIC 9(4).
                                07 HR PIC 9(2).
                                07 MN PIC 9(2).
                02 SUBSC-SWTCH PIC 9.
                02 OUT-TRNK-NR PIC 9(4).
                02 TRNK-SWTCH PIC 9.
                02 ROUT-IND PIC 9.
                02 DSCNT-TIME PIC 9(4).
                02 ELAPS-TIME PIC 9(5).
                02 IN-TRNK PIC 9(3).
FD OUT-CODE2.
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "OUTCODE2 "
        DATA RECORDS ARE CODE2-RECD.
01 CODE2-RECD.
                02 D1 PIC 9.
                02 D2 PIC 9.
                02 ITRNC PIC 9(4).
                02 0S-ANSBK PIC 9(20).
                02 DEST-SELCT PIC 9(16).
                        05 CNTY-SLECT 9(4).
                        05 FILLER 9(12).
                02 LN-ADRESS PIC 9(4).
                02 DATE-ALL PIC 9(8).
                        05 DATE PIC 9(4).
                                07 MO PIC 9(12).
                                07 DY PIC 9(2).
                        05 PRECD-SLCT PIC 9(4).
                                07 HR PIC 9(2).
                                07 MN PIC 9(2).
                02 SUBSC-SWTCH PIC 9.
                02 OUT-TRNK-NR PIC 9(4).
                02 TRNK-SWTCH PIC 9.
                02 ROUT-IND PIC 9.
                02 DSCNT-TIME PIC 9(4).
                02 ELAPS-TIME PIC 9(5).
                02 IN-TRNK PIC 9(3).
FD OUT-CODE4.
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "OUTCODE4 "
        DATA RECORDS ARE CODE4-RECD.
01 CODE4-RECD.
                02 D1 PIC 9.
                02 D2 PIC 9.
                02 ITRNC PIC 9(4).
                02 0S-ANSBK PIC 9(20).
                02 DEST-SELCT PIC 9(16).
                        05 CNTY-SLECT 9(4).
                        05 FILLER 9(12).
                02 LN-ADRESS PIC 9(4).
                02 DATE-ALL PIC 9(8).
                        05 DATE PIC 9(4).
                                07 MO PIC 9(12).
                                07 DY PIC 9(2).
                        05 PRECD-SLCT PIC 9(4).
                                07 HR PIC 9(2).
                                07 MN PIC 9(2).
                02 SUBSC-SWTCH PIC 9.
                02 OUT-TRNK-NR PIC 9(4).
                02 TRNK-SWTCH PIC 9.
                02 ROUT-IND PIC 9.
                02 DSCNT-TIME PIC 9(4).
                02 ELAPS-TIME PIC 9(5).
                02 IN-TRNK PIC 9(3).
   