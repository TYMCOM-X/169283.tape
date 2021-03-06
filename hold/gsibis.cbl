IDENTIFICATION DIVISION.
PROGRAM-ID. GSI.
AUTHOR. BISIAUX.
REMARKS. CHANGEMENT DU NO DE CLIENT SUR FICHIER RELEVES.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT FICIN ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT FICOUT ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT FICMVT ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT FICANO ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD FICIN
        RECORD CONTAINS 40
        LABEL RECORD STANDARD
        VALUE OF IDENTIFICATION IS "FICIN DAT".
01 ENTREE PIC X(40).
FD FICOUT
        RECORD CONTAINS 40
        LABEL RECORD STANDARD
        VALUE OF IDENTIFICATION IS "FICOUTDAT".
01 SORTIE PIC X(40).
FD FICMVT
        RECORD CONTAINS 80
        LABEL RECORD STANDARD
        VALUE OF IDENTIFICATION IS "FICMVTDAT".
01 DONMVT PIC X(80).
FD FICANO
        RECORD CONTAINS 133
        LABEL RECORD STANDARD
        VALUE OF IDENTIFICATION IS "FICANODAT".
01 IMP.
                02 SAUT PIC X.
                02 LIGNE PIC X(132).
WORKING-STORAGE SECTION.
77 CTRLIGNE PIC S99 COMP VALUE 70.
77 CTRPAGE PIC S9(6) COMP VALUE 0.
77 FINMVT PIC X VALUE LOW-VALUE.
77 FINPERM PIC X VALUE LOW-VALUE.
77 CTRMVT PIC S9(9) VALUE 0.
77 CTRPER PIC S9(9) VALUE 0.
01 MOUV.
        02 CARMVT PIC XX.
        02 SOCMVT PIC XXX.
        02 ANCMVT PIC X(12).
        02 NOUMVT PIC X(12).
        02 FILLER PIC X(51).
01 PERM.
        02 FILLER PIC XX.
        02 SOCPERM PIC XXX.
        02 CPTPERM PIC X(12).
        02 FILLER PIC X(23).
01 LIGTIT1.
        02 FILLER PIC X VALUE "1".
        02 FILLER PIC X(18) VALUE "   ETS. S.S. WHITE".
        02 FILLER PIC X(60) VALUE SPACES.
        02 FILLER PIC X(5) VALUE "DATE ".
        02 DATPARM PIC X(8).
        02 FILLER PIC X(20) VALUE SPACES.
        02 FILLER PIC X(5) VALUE "PAGE".
        02 NUMPAGE PIC Z(5)9.
        02 FILLER PIC X(10) VALUE SPACES.
01 LIGTIT2.
        02 FILLER PIC X VALUE "-".
        02 FILLER PIC X(42) VALUE SPACES.
        02 FILLER PIC X(46) VALUE "MODIFICATION DU CODE CLIENT
-       "AU FICHIER COMPTES".
01 LIGTIT3.
        02 FILLER PIC X VALUE "-".
        02 FILLER PIC X(60) VALUE SPACES.
        02 FILLER PIC X(38) VALUE "STE      ANCIEN CODE      NO
-       "UVEAU CODE".
01 LIGBAN.
        02 FILLER PIC X(4) VALUE "0   ".
        02 LIBEL PIC X(57).
        02 S-STE PIC XXX.
        02 FILLER PIC X(6) VALUE SPACES.
        02 S-ANCOD PIC X(12).
        02 FILLER PIC X(6) VALUE SPACES.
        02 S-NOCOD PIC X(12).
PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
        OPEN INPUT FICIN FICMVT OUTPUT FICOUT FICANO.
LECPERM.
        IF FINPERM = HIGH-VALUE GO TO SLECPERM.
        READ FICIN INTO PERM AT END MOVE HIGH-VALUE TO FINPERM
        SOCPERM CPTPERM.
        ADD 1 TO CTRPER.
FLECPERM. EXIT.
LECMVT.
        IF FINMVT = HIGH-VALUE GO TO FLECMVT.
        READ FICMVT INTO MOUV AT END MOVE HIGH-VALUE TO FINMVT SOCMVT
        ANCMVT.
        ADD 1 TO CTRMVT.
FLECMVT. EXIT.
SLECPERM.
        IF FINMVT = HIGH-VALUE GO TO FINTRA.
S1.     IF FINPERM = HIGH-VALUE AND FINMVT = HIGH-VALUE
        GO TO FINTRA.
        IF FINPERM = HIGH-VALUE GO TO COMPARE.
        IF FINMVT = HIGH-VALUE 
        PERFORM ECRPERM THRU FECRPERM
        PERFORM LECPERM THRU FLECPERM
        GO TO S1.
COMPARE.
        IF CARMVT NOT = "40"
        DISPLAY "CODE CARTE INVALIDE" CARMVT "COMPTE " ANCMVT
        PERFORM LECMVT THRU FLECMVT GO TO S1.
        IF SOCMVT < SOCPERM
        MOVE "MODIFICATION IMPOSSIBLE - CODE CLIENT INEXISTANT"
        TO LIBEL
        PERFORM ECRLIST THRU FECRLIST
        PERFORM LECMVT THRU FLECMVT
        GO TO S1.
        IF SOCMVT > SOCPERM 
        PERFORM ECRPERM THRU FECRPERM
        PERFORM LECPERM THRU FLECPERM
        GO TO S1.
        IF ANCMVT < CPTPERM
        MOVE "MODIFICATION IMPOSSIBLE - CODE CLIENT INEXISTANT" 
         TO LIBEL
        PERFORM ECRLIST THRU FECRLIST
        PERFORM LECMVT  THRU FLECMVT
        GO TO S1.
        IF ANCMVT > CPTPERM
        PERFORM ECRPERM THRU FECRPERM
        PERFORM LECPERM THRU FLECPERM
        GO TO S1.
        MOVE NOUMVT TO CPTPERM.
        MOVE "MODIFICATION EFFECTUEE " TO LIBEL.
        PERFORM ECRLIST THRU FECRLIST.
        PERFORM ECRPERM THRU FECRPERM.
        PERFORM LECPERM THRU FLECPERM.
        PERFORM LECMVT  THRU FLECMVT.
        GO TO S1.
ECRPERM.
        WRITE SORTIE FROM PERM.
FECRPERM.
        EXIT.
ECRLIST.
        IF CTRLIGNE > 60 PERFORM TITRE THRU FTITRE.
        MOVE SOCMVT TO S-STE.
        MOVE ANCMVT TO S-ANCOD.
        MOVE NOUMVT TO S-NOCOD. MOVE LIGBAN TO IMP.
        PERFORM ECRLIG THRU FECRLIG.
FECRLIST.
        EXIT.
ECRLIG.
        IF SAUT = "-" WRITE IMP AFTER 3
        ADD 3 TO CTRLIGNE.
        IF SAUT = "0" WRITE IMP AFTER 2
        ADD 2 TO CTRLIGNE.
FECRLIG. EXIT.
TITRE.
        MOVE 0 TO CTRLIGNE.
        ADD 1 TO CTRPAGE.
        MOVE CTRPAGE TO NUMPAGE.
        MOVE LIGTIT1 TO IMP.
        PERFORM ECRLIG THRU FECRLIG.
        MOVE LIGTIT2 TO IMP.
        PERFORM ECRLIG THRU FECRLIG.
        MOVE LIGTIT3 TO IMP.
        PERFORM ECRLIG THRU FECRLIG.
FTITRE. EXIT.
FINTRA.
        DISPLAY "NOMBRE ENR. FICHIER " CTRPER.
        DISPLAY "NOMBRE ENR. MVT. " CTRMVT.
        MOVE SPACE TO S-STE S-ANCOD S-NOCOD.
        MOVE "FIN DE MODIFICATION " TO LIBEL.
        MOVE LIGBAN TO IMP.
        PERFORM ECRLIG THRU FECRLIG.
        CLOSE FICIN FICOUT FICMVT FICANO.
        STOP RUN.
                                                                 