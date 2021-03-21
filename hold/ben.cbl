IDENTIFICATION DIVISION.
PROGRAM-ID. BEN.
AUTHOR. BENBELAID.
INSTALLATION. PDP-10.
DATE-WRITTEN. 04-10-73.
DATE-COMPILED. SAME.
REMARKS. AUCUNE.
   ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT FILEENTER ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT FICAR ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT FIBAN ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT FICHIMP ASSIGN TO DSK
        RECORDING MODE IS ASCII.
  DATA DIVISION.
FILE SECTION.
FD FICAR
        RECORD CONTAINS 80
        LABEL RECORDS ARE OMITTED
        VALUE OF IDENTIFICATION IS "FICAR DAT".
01 ENTREE PICTURE X(80).
FD FILEENTER
        BLOCK CONTAINS 100
        LABEL RECORDS ARE OMITTED
        VALUE OF IDENTIFICATION IS FILEENTER.
01 CARENTR.
        02 COLONNE PICTURE X OCCURS 80.
FD FIBAN
        BLOCK CONTAINS 100
        RECORD CONTAINS 80
        LABEL RECORDS ARE OMITTED
        VALUE OF IDENTIFICATION IS FIBAN.
01 SORTIE.
        02 ENRGT PICTURE X(80).
FD FICHIMP
        RECORD CONTAINS 132
        LABEL RECORDS ARE OMITTED
        VALUE OF IDENTIFICATION IS FICHIMP.
01 LISTE PICTURE X(132).
  WORKING-STORAGE SECTION.
77 MAJMAX PICTURE 999 COMPUTATIONAL VALUE IS ZERO.
77 I PICTURE 999 COMPUTATIONAL.
77 JMAX PICTURE 999 COMPUTATIONAL.
77 K PICTURE 999 COMPUTATIONAL.
77 KMAX PICTURE 999 COMPUTATIONAL VALUE IS 1.
77 J PICTURE 999 COMPUTATIONAL.
77 L PICTURE 999 COMPUTATIONAL.
77 M PICTURE 999 COMPUTATIONAL.
77 AIG PICTURE 9 COMPUTATIONAL-3 VALUE IS ZERO.
01 TABA.
02 LIGNE OCCURS 100.
03 COLON PICTURE X OCCURS 80.
01 TABS.
02 S PICTURE 99 OCCURD 80 COMPUTATIONAL.
01 TABINITR.
02 INITR PICTURE 999 OCCURS 100 COMPUTATIONAL.
01 TABR.
02 R PICTURE 999 OCCURS 100 COMPUTATIONAL.
01 TITRE.
02 FILLER PICTURE X(43) VALUE SPACES.
02 LIBELLE PICTURE X(46) VALUE 'LISTE DES CARTES ERRONNEES'.
02 FILLER PICTURE X(43) VALUE SPACES.
01 LIGCART.
02 FILLER PICTURE X VALUE IS SPACE.
02 ZON PICTURE X(80).
02 FILLER PICTURE X(51) VALUE IS SPACE.
    PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
INITIALISATION SECTION.
OPEN INPUT FICAR FILEENTR OUTPUT FIBAN FICHIMP.
LECAR. READ FICAR AT END GOTO ETIQ1.
 COMPUTE MAJMAX=MAJMAX+1
 IF MAJMAX IS GREATER THAN 100
 DISPLAY 'DEPASSEMENT DE CAPACITE DU TABLEAU A' UPON CONSOLE
 STOP RUN.
 MOVE ENTREE TO LIGNE(MAJMAX)
 MOVE MAJMAX TO INITR(MAJMAX)
 GOTO LECAR.
ETIQ1. CLOSE FICAR.
TRAITEMENT SECTION.
 PERFORM CONSTS THRU FINCONSTS VARYING I FROM 1 BY 1
 UNTIL I IS GREATER THAN 80.
LEC. READ FILEENTER AT END GOTO FIN.
 MOVE MAJMAX TO JMAX
 MOVE TABINITR TO TABR
 MOVE 1 TO K.
ETIQ2. MOVE 1 TO J
 MOVE ZERO TO M
 MOVE S(K) TO L.
ETIQ3. MOVE R(J) TO I
 IF COLONNE(L) IS EQUAL TO COLON(I L)
 OR COLON(I L) IS EQUAL TO SPACE
 GOTO ETIQ8
 ELSE GOTO ETIQ9.
ETIQ8. ADD 1 TO M
 MOVE I TO R(M).
ETIQ9. ADD 1 TO J
 IF J IS GREATER THAN JMAX
 GOTO ETIQ4
 ELSE GOTO ETIQ3.
ETIQ4. IF M IS EQUAL TO ZERO
 GOTO ETIQ5.
 MOVE M TO JMAX
 ADD 1 TO K
 IF K EQUAL TO KMAX
 GOTO ETIQ6
 ELSE GOTO ETIQ2.
ETIQ6. IF DEPCAP OR AIG IS EQUAL TO ZERO
 WRITE LISTE FROM TITRE AFTER ADVANCING O LINES.
 MOVE CARENTR TO ZON
 MOVE 1 TO AIG
 WRITE LISTE FROM LIGCART AFTER ADVANCING 2 LINES
 GOTO LEC.
ETIQ5. WRITE SORTIE FROM CARENTR
 GOTO LEC.
FIN. IF AIG IS EQUAL  TO ZERO
 DISPLAY 'AUCUNE CARTE FAUSSE' UPON CONSOLE.
 CLOSE FICAR FIBAN FICHIMP FILEENTR
 STOP RUN.
CONSTS. MOVE ZERO TO J.
STS1. ADD 1 TO J
 IF J IS GREATER THAN MAJMAX
 GOTO FINCONSTS.
 IF COLON(J I) IS EQUAL TO SPACE
 GOTO STS1.
 MOVE 1 TO S(KMAX)
 ADD 1 TO KMAX.
FINCONSTS.EXIT.
   