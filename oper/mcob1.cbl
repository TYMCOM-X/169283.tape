       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIRB-1.
       AUTHOR. F D WROBLEWSKI  MERCHANTS MUTUAL INS CO BUFFALO.
       REMARKS. MERCHANTS MUTUAL INS CO BUFFALO NEW YORK.
000050         5 TAPES FOR THE BUREAU, LOG 2 BUREAU
000060         TAPE WILL ALSO BE INPUT TO A SORT
000070         FOR TABLOS.
000080 ENVIRONMENT DIVISION.
000090 CONFIGURATION SECTION.
000100 SOURCE-COMPUTER. H-200.
000110 OBJECT-COMPUTER. H-200 WITH SUPERVISOR MEMORY
000120         ADDRESS 1340 THRU 40000 DISPLAY-CONSOLE 07
000130         ACCEPT-CARD-READER E.
000140 SPECIAL-NAMES. PAGE IS HEAD-OF-FORM.
000150 INPUT-OUTPUT SECTION.
000160 FILE-CONTROL.
000170     SELECT D/O-FILE ASSIGN TO DSK,RECORDING MODE ASCII.
000180     SELECT TAPE-FILE ,ASSIGN TO DSK,RECORDING MODE ASCII.
000190     SELECT FILE-56, ASSIGN TO DSK, RECORDING MODE ASCII.
000200     SELECT FILE-7, ASSIGN TO DSK,RECORDING MODE ASCII.
000210     SELECT FILE-19,  ASSIGN TO DSK, RECORDING MODE ASCII.
000220     SELECT FILE-71,  ASSIGN TO DSK, RECORDING MODE ASCII.
000230     SELECT PRINT-FILE, ASSIGN TO DSK, RECORDING MODE ASCII.
000240 DATA DIVISION.
000250 FILE SECTION.
000260 FD  D/O-FILE,VALUE OF IDENTIFICATION IS "LOSS  DAT".
000320 01  D/O-REC.
000330     02  FILLER PICTURE X(8).
000340     02  D-CLAIM.
000350         03  D-CL-HI PICTURE 9.
000360         03  D-CL-LO PICTURE 9(6).
000370     02  D-AGT  PICTURE 99.
000380     02  FILLER PICTURE XX.
000390     02  D-ISS-MO PICTURE 99.
000400     02  D-ISS-YR.
000410         03  ISS-YR-HI PICTURE 9.
000420         03  ISS-YR-LO PICTURE 9.
000430     02  D-ACC-MO-YR.
000440         03  D-ACC-MO PICTURE 99.
000450         03  D-ACC-YR-HI PICTURE 9.
000460         03  D-ACC-YR-LO PICTURE 9.
000470     02  D-COV.
000480         03  FILLER PICTURE 9.
000490         03  D-COV-LO PICTURE 9.
000500     02  D-WIND PICTURE 9.
000510     02  D-CL-CNT PICTURE X.
000520     02  D-COL2930.
000530         03  D-ACC-CNT PICTURE X.
000540         03  D-LIMIT PICTURE X.
000550     02  D-CLASS1.
000560         03  D-CL-1-2 PICTURE 99.
000570         03  FILLER PICTURE 99.
000580     02  D-CLASS2 REDEFINES D-CLASS1.
000590         03  FILLER PICTURE 9.
000600         03  D-CL-2-3 PICTURE 99.
000610         03  FILLER PICTURE 9.
000620     02  D-ST PICTURE 99.
000630     02  D-CITY.
000640         03  D-CITY1 PICTURE 99.
000650         03  D-SYM PICTURE 9.
000660     02  D-L/A PICTURE 9.
000670     02  D-AGE PICTURE 99.
000680     02  D-AGE1 REDEFINES D-AGE.
000690         03  FILLER PICTURE 9.
000700         03  D-LOAGE PICTURE 9.
000710     02  D-TERR PICTURE 99.
000720     02  FILLER PICTURE X(11).
000730     02  D-INDEM PICTURE IS S9(5)V99.
000740     02  D-EXP PICTURE S999.
000750     02  D-DISP-MO.
000760         03  FILLER PICTURE IS 9.
000770         03  D-DISP-LO PICTURE 9.
000780     02  FILLER PICTURE 9.
000790     02  D-DISP-YR PICTURE 9.
000800     02  FILLER PICTURE XX.
000810     02  D-POL PICTURE 9(6).
000820     02  FILLER PICTURE X(11).
000830     02  D-CNP PICTURE 9.
000840     02  D-ADD PICTURE 9.
000850     02  FILLER PICTURE X(7).
000860 FD  TAPE-FILE
000861     RECORDING MODE IS BCD
000870     BLOCK CONTAINS 10 RECORDS
000880     RECORD CONTAINS 80 CHARACTERS
000890     LABEL RECORDS ARE STANDARD-80
000900     VALUE OF IDENTIFICATION IS @LOSSESPAID@
000910     DATA RECORD IS TAPE-REC.
000920 01  TAPE-REC.
000930     02  T-CONO PICTURE 9999.
000940     02  T-KIND PICTURE 9.
000950     02  T-ACCT-MO PICTURE X.
000960     02  T-ACCT-YR PICTURE 9.
000970     02  T-LINE PICTURE 99.
000980     02  FILLER PICTURE XX.
000990     02  T-EFF-YR PICTURE 9.
001000     02  FILLER PICTURE XXX.
001010     02  T-STATE PICTURE 99.
001020     02  T-TERRM.
001030         03  FILLER PICTURE X.
001040         03  T-TERR PICTURE XX.
001050     02  T-CLASS-2 PICTURE 9999.
001060     02  T-CLASS PICTURE 99.
001070     02  T-LIMITS PICTURE X.
001080     02  FILLER PICTURE X(10).
001090     02  T-TYPE PICTURE 9.
001100     02  T-TRANS PICTURE 9.
001110     02  T-ACC-MOYR PICTURE 9999.
001120     02  T-AMOUNT PICTURE S9(5)V99.
001130     02  FILLER PICTURE X.
001140     02  T-NO-CL PICTURE S9.
001150     02  FILLER PICTURE X(14).
001160     02  T-AGE PICTURE XX.
001170     02  T-POLNUM PICTURE X(6).
001180     02  T-NUMBER PICTURE X(6).
001190 FD  PRINT-FILE
001200     LABEL RECORDS ARE OMITTED
001210     DATA RECORD IS PRINT-REC.
001220 01  PRINT-REC PICTURE X(75).
001230 FD  FILE-56
001231     RECORDING MODE IS BCD
001240     BLOCK CONTAINS 10 RECORDS
001250     RECORD CONTAINS 80 CHARACTERS
001260     LABEL RECORDS ARE STANDARD-80
001270     VALUE OF IDENTIFICATION IS @GLPDLOSSES@
001280     DATA RECORD IS 56-REC.
001290 01  56-REC.
001300     02  56-CONST PICTURE 9(7).
001310     02  56-CL-FILL PICTURE 999.
001320     02  56-CLAIM PICTURE 9(7).
001330     02  FILLER PICTURE XX.
001340     02  56-CLASS PICTURE 9999.
001350     02  56-STATE PICTURE 99.
001360     02  FILLER PICTURE X.
001370     02  56-TERR PICTURE 99.
001380     02  56-ACCMOYR PICTURE 9999.
001390     02  56-COV PICTURE 9.
001400     02  56-TRANS PICTURE 9.
001410     02  56-CR PICTURE X.
001420     02  56-CLCNT PICTURE 9.
001430     02  56-COST PICTURE 9(6)V99.
001440     02  56-EXP1  REDEFINES 56-COST.
001450         03  FILLER PICTURE 999.
001460         03  56-EXP PICTURE 999.
001470         03  FILLER PICTURE V99.
001480     02  56-ANAL PICTURE 9.
001490     02  56-LINE PICTURE 99.
001500     02  56-ISSMO PICTURE 99.
001510     02  56-ISSYR PICTURE 9.
001520     02  56-CLASS2 PICTURE X.
001525     02  56-SYM PICTURE 9.
001530     02  FILLER PICTURE X(28).
001540 FD  FILE-7
001541     RECORDING MODE IS BCD
001550     BLOCK CONTAINS 10 RECORDS
001560     RECORD CONTAINS 80 CHARACTERS
001570     LABEL RECORDS ARE STANDARD-80
001580     VALUE OF IDENTIFICATION IS @GLASDETAIL@
001590     DATA RECORD IS 7-REC.
001600 01  7-REC.
001610     02  7-CONST PICTURE 9(7).
001620     02  7-CL-FILL PICTURE 9.
001630     02  7-CLAIM PICTURE 9(7).
001640     02  FILLER PICTURE X(10).
001650     02  7-CLASS PICTURE 99.
001660     02  7-STATE PICTURE 99.
001670     02  7-TERR PICTURE 99.
001680     02  7-TRANS PICTURE 9.
001690     02  FILLER PICTURE X(7).
001700     02  7-CR PICTURE X.
001710     02  7-ACCYR PICTURE 9.
001720     02  7-CLCNT PICTURE 9.
001730     02  7-COST PICTURE 9(5)V99.
001740     02  7-LINE PICTURE 999.
001750     02  FILLER PICTURE X(28).
001760 FD  FILE-19
001761     RECORDING MODE IS BCD
001770     BLOCK CONTAINS 10 RECORDS
001780     RECORD CONTAINS 80 CHARACTERS
001790     LABEL RECORDS ARE STANDARD-80
001800     VALUE OF IDENTIFICATION IS @BURGDETAIL@
001810     DATA RECORD IS 19-REC.
001820 01  19-REC.
001830     02  19-CONST PICTURE 9(7).
001840     02  19-CL-FILL PICTURE 9.
001850     02  19-CLAIM PICTURE 9(7).
001860     02  FILLER PICTURE X(6).
001870     02  19-FORMA PICTURE 9.
001880     02  19-FORMB PICTURE 9.
001890     02  FILLER PICTURE XXXX.
001900     02  19-STATE PICTURE 99.
001910     02  19-TERR PICTURE 99.
001920     02  19-TRANS PICTURE 9.
001930     02  FILLER PICTURE X(7).
001940     02  19-CR PICTURE X.
001950     02  19-ACCYR PICTURE 9.
001960     02  19-CLCNT PICTURE 9.
001970     02  19-COST PICTURE 9(5)V99.
001980     02  19-LINE PICTURE 999.
001990     02  FILLER PICTURE X(28).
002000 FD  FILE-71
002001     RECORDING MODE IS BCD
002010     BLOCK CONTAINS 10 RECORDS
002020     RECORD CONTAINS 80 CHARACTERS
002030     LABEL RECORDS ARE STANDARD-80
002040     VALUE OF IDENTIFICATION IS @MPPDLOSSES@
002050     DATA RECORD IS 71-REC.
002060 01  71-REC.
002070     02  71-TYPE1 PICTURE 9.
002080     02  71-CALYR PICTURE 9.
002090     02  71-CO PICTURE 9999.
002100     02  71-ST PICTURE 99.
002110     02  71-TERR PICTURE 99.
002120     02  71-TRANS PICTURE 9.
002130     02  71-COL2930 PICTURE 99.
002140     02  71-CLASS PICTURE 9999.
002150     02  71-LOAGE PICTURE 9.
002160     02  FILLER PICTURE X.
002170     02  71-TYPE2 PICTURE 9.
002180     02  FILLER PICTURE X(15).
002190     02  71-COST PICTURE 9(8)V99.
002200     02  71-CR PICTURE X.
002210     02  FILLER PICTURE XXXXX.
002220     02  71-CL PICTURE 9.
002230     02  71-CR2 PICTURE X.
002240     02  FILLER PICTURE X(6).
002250     02  71-QTR PICTURE 9.
002260     02  71-ACCYR-HI PICTURE 9.
002270     02  71-ACCYR-LO PICTURE 9.
002280     02  71-ZERO PICTURE 999.
002290     02  71-CLAIM PICTURE 9(7).
002300     02  FILLER PICTURE X.
002310     02  71-ACCMO PICTURE 99.
002320     02  71-ISSMO PICTURE 99.
002330     02  71-ISSYR PICTURE 9.
002340     02  71-CODE PICTURE 99.
002350 WORKING-STORAGE SECTION.
002360 77  FILL1 PICTURE IS X(22) VALUE @   02  010304     R01 @.
002370 77  FILL2 PICTURE X(24) VALUE @0080001000109SS 1 E    R@.
002380 77  FILL3 PICTURE IS X(9) VALUE @      8  @.
002390 77  FILL4 PICTURE IS X(28) VALUE SPACES.
002400 77  FILL5 PICTURE IS X(12) VALUE @000802001602@.
002410 77  FILL6 PICTURE IS X(48) VALUE SPACES.
002420 77  FILL7 PICTURE IS X(20) VALUE @LOSSESPAIDLOSSESPAID@.
002430 77  FILL8 PICTURE IS X(9) VALUE SPACES.
002440 77  FILL9 PICTURE IS X(9) VALUE @TABLOS00B@.
002450 01  MM-HEAD.
002460     02  FILLER PICTURE X(23) VALUE SPACES.
002470     02  FILLER PICTURE X(30) VALUE IS
002480         @MERCHANTS MUTUAL INSURANCE CO.@.
002490     02  FILLER PICTURE X(22) VALUE SPACES.
002500 01  NH-HEAD.
002510     02  FILLER PICTURE X(22) VALUE SPACES.
002520     02  FILLER PICTURE X(31) VALUE IS
002530         @NEW HAMPSHIRE INSURANCE COMPANY@.
002540     02  FILLER PICTURE X(22) VALUE SPACES.
002550 01  56-HEAD.
002560     02  FILLER PICTURE X(25) VALUE
002570         @        GENERAL LIABILITY@.
002580     02  FILLER PICTURE X(25) VALUE
002590         @ PAID LOSSES AND ALLOCATE@.
002600     02  FILLER PICTURE X(25) VALUE
002610         @D CLAIM EXPENSE          @.
002620 01  7-HEAD.
002630     02  FILLER PICTURE X(20) VALUE SPACES.
002640     02  FILLER PICTURE X(34) VALUE IS
002650         @GLASS INSURANCE EXPERIENCE FOR THE@.
002660     02  FILLER PICTURE X(21) VALUE SPACES.
002670 01  19-HEAD.
002680     02  FILLER PICTURE X(18) VALUE SPACES.
002690     02  FILLER PICTURE X(37) VALUE IS
002700         @BURGLARY INSURANCE EXPERIENCE FOR THE@.
002710     02  FILLER PICTURE X(20) VALUE SPACES.
002720 01  71-HEAD.
002730     02  FILLER PICTURE X(18) VALUE SPACES.
002740     02  FILLER PICTURE X(38) VALUE
002750         @SPECIAL MULTI PERIL EXPERIENCE FOR THE@.
002760     02  FILLER PICTURE X(19) VALUE SPACES.
002770 01  QTR-HEAD.
002780     02  FILLER PICTURE X(29) VALUE SPACES.
002790     02  QTR-NO PICTURE XXX VALUE IS SPACES.
002800     02  FILLER PICTURE X(12) VALUE @ QUARTER 197@.
002810     02  QTR-YR PICTURE 9.
002820     02  FILLER PICTURE X(30) VALUE SPACES.
002830 01  56-LINE1.
002840     02  FILLER PICTURE X(34) VALUE SPACES.
002850     02  FILLER PICTURE X(5) VALUE @CLAIM@.
002860     02  FILLER PICTURE X(25) VALUE IS
002870         @         LOSSES PAID     @.
002880     02  FILLER PICTURE X(11) VALUE @CARD       @.
002890 01  56-LINE2.
002900     02  FILLER PICTURE X(25) VALUE
002910         @      CO.NO.    COV      @.
002920     02  FILLER PICTURE X(25) VALUE
002930         @ST       COUNT           @.
002940     02  FILLER PICTURE X(25) VALUE
002950         @   COST       COUNT      @.
002960 01  7-19-LINE1.
002970     02  FILLER PICTURE X(29) VALUE SPACES.
002980     02  FILLER PICTURE X(15) VALUE IS
002990         @CLAIM          @.
003000     02  FILLER PICTURE X(11) VALUE
003010         @LOSSES PAID@.
003020     02  FILLER PICTURE X(20) VALUE IS
003030         @       CARD         @.
003040 01  7-19-LINE2.
003050     02  FILLER PICTURE X(25) VALUE
003060         @        CO.NO.     ST    @.
003070     02  FILLER PICTURE X(25) VALUE
003080         @    COUNT               C@.
003090     02  FILLER PICTURE X(25) VALUE
003100         @OST        COUNT         @.
003110 01  SW-1 PICTURE 9.
003120 01  SW-2 PICTURE 9.
003130 01  SW-A PICTURE 9.
003140 01  SW-B PICTURE 9.
003150 01  SW-C PICTURE 9.
003160 01  DETAIL-56.
003170     02  FILLER PICTURE X(7) VALUE SPACES.
003180     02  D56-CONO PICTURE 9999.
003190     02  FILLER PICTURE X(7) VALUE SPACES.
003200     02  D56-COV PICTURE 9.
003210     02  FILLER PICTURE X(6) VALUE SPACES.
003220     02  D56-ST PICTURE 99.
003230     02  FILLER PICTURE X(6) VALUE SPACES.
003240     02  D56-CLCNT PICTURE ZZ,ZZZCR.
003250     02  FILLER PICTURE XXXX VALUE SPACES.
003260     02  D56-COST PICTURE Z,ZZZ,ZZZ.ZZCR.
003270     02  FILLER PICTURE XXXX VALUE SPACES.
003280     02  D56-CDCNT PICTURE ZZ,ZZZ.
003290     02  FILLER PICTURE X(6) VALUE SPACES.
003300 01  DETAIL-7-19.
003310     02  FILLER PICTURE X(8) VALUE SPACES.
003320     02  D719-CONO PICTURE 9999.
003330     02  FILLER PICTURE X(7) VALUE SPACES.
003340     02  D719-ST PICTURE 99.
003350     02  FILLER PICTURE X(7) VALUE SPACES.
003360     02  D719-CLCNT PICTURE ZZ,ZZZCR.
003370     02  FILLER PICTURE X(5) VALUE SPACES.
003380     02  D719-COST PICTURE Z,ZZZ,ZZZ.ZZCR.
003390     02  FILLER PICTURE X(5) VALUE SPACES.
003400     02  D719-CDCNT PICTURE ZZ,ZZZ.
003410     02  FILLER PICTURE X(9) VALUE SPACES.
003420 01  CON-FILE.
003430     02  FILLER PICTURE X VALUE IS SPACES.
003440     02  ST-CONO PICTURE IS XXXX.
003450     02  FILLER PICTURE IS X VALUE IS @:@.
003460 01  CD-COUNT12 PICTURE 9(5).
003470 01  CD-COUNT56 PICTURE 9(5).
003480 01  CD-COUNT7 PICTURE 9(5).
003490 01  CD-COUNT19 PICTURE 9(5).
003500 01  CD-COUNT71 PICTURE 9(5).
003510 01  NOTHING PICTURE IS 9 VALUE IS ZERO.
003520 01  ACCUMX.
003530     02  HI-YR PICTURE 9.
003540     02  LO-YR PICTURE 9.
003550     02  MONTH PICTURE 99.
003560 01  ACCUM1 REDEFINES ACCUMX PICTURE IS S9999.
003570 01  CON-CARD.
003580     02  CON-FILL.
003590         03  CON-CONO PICTURE 9999.
003600         03  CON-MO.
003610         04  CON-MOHI PICTURE 9.
003620         04  CON-MOLO PICTURE 9.
003630         03  CON-YR PICTURE 9.
003640     02  CON-NOTPS PICTURE 99.
003650     02  CON-FLAG PICTURE X.
003660     02  FILLER PICTURE X(70).
003670 01  ST-1.
003680     02  FILLER PICTURE X(42) VALUE IS
003690         @060708091013181920212829313234373839444547@.
003700 01  ST-1A REDEFINES ST-1.
003710     02  ST-TAB PICTURE 99 OCCURS 21 TIMES.
003720 01  SUB PICTURE 9.
003730 01  SUB-ST PICTURE 99.
003740 01  A-56.
003750     02  56-A OCCURS 3 TIMES.
003760         03  A-CL56 PICTURE S9(5) OCCURS 21 TIMES.
003770         03  A-COST56 PICTURE S9(7)V99 OCCURS 21 TIMES.
003780         03  A-CD56 PICTURE 9(5) OCCURS 21 TIMES.
003790 01  A-7.
003800     02  A-CL7 PICTURE S9(5) OCCURS 21 TIMES.
003810     02  A-COST7 PICTURE S9(7)V99 OCCURS 21 TIMES.
003820     02  A-CD7 PICTURE 9(5) OCCURS 21 TIMES.
003830 01  A-19.
003840     02  A-CL19 PICTURE S9(5) OCCURS 21 TIMES.
003850     02  A-COST19 PICTURE S9(7)V99 OCCURS 21 TIMES.
003860     02  A-CD19 PICTURE 9(5) OCCURS 21 TIMES.
003870 01  A-71.
003880     02  A-CL71 PICTURE S9(5) OCCURS 21 TIMES.
003890     02  A-COST71 PICTURE S9(7)V99 OCCURS 21 TIMES.
003900     02  A-CD71 PICTURE 9(5) OCCURS 21 TIMES.
003910 01  TOT-56.
003920     02  TOT-CL56 PICTURE S9(5) OCCURS 3 TIMES.
003930     02  TOT-COST56 PICTURE S9(7)V99 OCCURS 3 TIMES.
003940     02  TOT-CD56 PICTURE 9(5) OCCURS 3 TIMES.
003950 01  TOT-7.
003960     02  TOT-CL7 PICTURE S9(5).
003970     02  TOT-COST7 PICTURE S9(7)V99.
003980     02  TOT-CD7 PICTURE 9(5).
003990 01  TOT-19.
004000     02  TOT-CL19 PICTURE S9(5).
004010     02  TOT-COST19 PICTURE S9(7)V99.
004020     02  TOT-CD19 PICTURE 9(5).
004030 01  TOT-71.
004040     02  TOT-CL71 PICTURE S9(5).
004050     02  TOT-COST71 PICTURE S9(7)V99.
004060     02  TOT-CD71 PICTURE 9(5).
004070 PROCEDURE DIVISION.
004080 DECLARATIVES.
004090 A SECTION. USE AFTER STANDARD BEGINNING LABEL
004100         PROCEDURE ON TAPE-FILE, FILE-56, FILE-7, FILE-19,
004110         FILE-71.
004120 A-PARA.
004130     MOVE SPACES TO *BLOCK-COUNT.
004140     MOVE CON-FILE TO *FILE-NO.
004150     MOVE SPACES TO *DATE-WRITTEN *PURGE-DATE
004160         *LABEL-EXCESS.
004170 B SECTION. USE AFTER STANDARD ENDING LABEL
004180         PROCEDURE ON TAPE-FILE.
004190 B-PARA.
004200     MOVE CD-COUNT12 TO *BLOCK-COUNT.
004210 C SECTION. USE AFTER STANDARD ENDING LABEL
004220         PROCEDURE ON FILE-56.
004230 C-PARA.
004240     MOVE CD-COUNT56 TO *BLOCK-COUNT.
004250 D SECTION. USE AFTER STANDARD ENDING LABEL
004260         PROCEDURE ON FILE-7.
004270 D-PARA.
004280     MOVE CD-COUNT7 TO *BLOCK-COUNT.
004290 E SECTION. USE AFTER STANDARD ENDING LABEL
004300         PROCEDURE ON FILE-19.
004310 E-PARA.
004320     MOVE CD-COUNT19 TO *BLOCK-COUNT.
004330 F SECTION. USE AFTER STANDARD ENDING LABEL
004340         PROCEDURE ON FILE-71.
004350 F-PARA.
004360     MOVE CD-COUNT71 TO *BLOCK-COUNT.
004370 END DECLARATIVES.
004380 G SECTION.
004390 LETS-GO.
004400     IF *HALT-NAME IS EQUAL TO @SORTIT@ GO TO SORT-IT.
004410 START-1.
004420     ACCEPT CON-CARD.
004430     IF CON-FLAG IS UNEQUAL TO @*@ DISPLAY
004440         @ CK CONST CARD @ STOP 0101 GO TO START-1.
004450     MOVE CON-CONO TO ST-CONO.
004460     OPEN OUTPUT PRINT-FILE, TAPE-FILE, FILE-56, FILE-7, FILE-19.
004470     OPEN OUTPUT FILE-71.
004480 OPEN-IP.
004490     OPEN INPUT D/O-FILE.
004500 R-1.
004510     READ D/O-FILE AT END GO TO CK-NO-TPS.
004520     IF D-COV IS EQUAL TO 01 OR 02 GO TO CK-12ST.
004530     IF D-COV IS EQUAL TO 05 OR 06 GO TO START56.
004540     IF D-COV IS EQUAL TO 07 GO TO START7.
004550     IF D-COV IS EQUAL TO 19 GO TO START19.
004560     IF D-COV IS EQUAL TO 71 GO TO START71.
004570     GO TO R-1.
004580 CK-12ST.
004590     IF D-ST IS EQUAL TO 20 GO TO R-1.
004600     MOVE SPACES TO TAPE-REC.
004610     MOVE CON-CONO TO T-CONO.
004620     MOVE 2 TO T-KIND.
004630     MOVE D-DISP-YR TO T-ACCT-YR.
004640     MOVE CON-MOLO TO T-ACCT-MO.
004650     IF CON-MO IS EQUAL TO 12 MOVE @?@ TO T-ACCT-MO.
004660     MOVE 41 TO T-LINE.
004670     IF D-AGT IS UNEQUAL TGO TO CONT-12.
004680     MOVE D-ISS-MO TO MONTH.
004690     MOVE ISS-YR-LO TO LO-YR.
004700     MOVE ISS-YR-HI TO HI-YR.
004710     SUBTRACT 6107 FROM ACCUM1.
004720     IF ACCUM1 IS NEGATIVE GO TO CONT-12.
004730     MOVE 49 TO T-LINE.
004740 CONT-12.
004750     MOVE ISS-YR-LO TO T-EFF-YR.
004760     MOVE D-ST TO T-STATE.
004770     MOVE SPACES TO T-TERRM.
004780     MOVE D-CITY1 TO T-TERR.
004790     MOVE D-TERR TO T-CLASS.
004800     MOVE D-CLASS1 TO T-CLASS-2.
004810     MOVE D-LIMIT TO T-LIMITS.
004820     IF D-COV-LO IS EQUAL TO 2 MOVE 3 TO T-TYPE GO TO MV-M012.
004830     IF D-L/A IS EQUAL TO 7 MOVE 4 TO T-TYPE GO TO MV-M012.
004840     IF D-L/A IS EQUAL TO 4 MOVE 5 TO T-TYPE GO TO MV-M012.
004850     IF D-L/A IS EQUAL TO 5 MOVE 2 TO T-TYPE GO TO MV-M012.
004860     IF D-L/A IS EQUAL TO 9 MOVE 9 TO T-TYPE ELSE MOVE 1
004870         TO T-TYPE.
004880 MV-M012.
004890     MOVE D-ACC-MO-YR TO T-ACC-MOYR.
004900     MOVE 4 TO T-TRANS.
004910     MOVE D-CL-CNT TO T-NO-CL THRU @\@.
004920     MOVE D-CL-LO TO T-NUMBER.
004930     MOVE D-POL TO T-POLNUM.
004940     MOVE D-AGE TO T-AGE.
004950     IF 1 IS UNEQUAL TO D-ADD AND D-CNP ADD NOTHING
004960         TO D-INDEM MOVE D-INDEM TO T-AMOUNT ADD 1
004970         TO CD-COUNT12 WRITE TAPE-REC.
004980     IF D-EXP IS UNEQUAL TO SPACES MOVE SPACE TO T-NO-CL
004990         ADD NOTHING TO D-EXP MOVE D-EXP TO T-AMOUNT
005000         ADD 1 TO CD-COUNT12 MOVE 6 TO T-TRANS
005010         WRITE TAPE-REC.
005020     GO TO R-1.
005030 START56.
005040     MOVE SPACES TO 56-REC.
005050     MOVE CON-FILL TO 56-CONST.
005060     MOVE ZEROS TO 56-CL-FILL.
005070     MOVE D-CLAIM TO 56-CLAIM.
005080     MOVE D-CLASS1 TO 56-CLASS.
005090     MOVE D-ST TO 56-STATE.
005100     MOVE D-CITY1 TO 56-TERR.
005110     MOVE D-ACC-MO-YR TO 56-ACCMOYR.
005120     IF D-COV IS EQUAL TO 06 MOVE 3 TO 56-COV MOVE D-L/A
005130         TO 56-ANAL GO TO NEXT-CK56.
005140     IF D-L/A IS EQUAL TO 5 MOVE 2 TO 56-COV ELSE MOVE 1
005150         TO 56-COV.
005160 NEXT-CK56.
005170     MOVE 11 TO 56-LINE.
005180     MOVE D-ISS-MO TO 56-ISSMO.
005190     MOVE ISS-YR-LO TO 56-ISSYR.
005200     MOVE D-LIMIT TO 56-CLASS2.
005205     MOVE D-SYM TO 56-SYM.
005210     MOVE D-CL-CNT TO 56-CLCNT THRU @\@.
005220     MOVE 4 TO 56-TRANS.
005230     IF 1 IS EQUAL TO D-ADD OR D-CNP GO TO CK-EXP56.
005240     IF D-INDEM IS NEGATIVE MOVE @:@ TO 56-CR.
005250     ADD 1 TO CD-COUNT56.
005260     MOVE D-INDEM TO 56-COST. MOVE SPACES TO 56-COST THRU @^@.
005270     MOVE 1 TO SW-1. WRITE 56-REC.
005280 CK-EXP56.
005290     IF D-EXP IS EQUAL TO SPACES GO TO CAL-56.
005300     MOVE SPACE TO 56-CLCNT.
005310     MOVE 6 TO 56-TRANS.
005320     ADD 1 TO CD-COUNT56.
005330     MOVE 1 TO SW-2.
005340     MOVE SPACE TO 56-CR.
005350     IF D-EXP IS NEGATIVE MOVE @:@ TO 56-CR.
005360     MOVE D-EXP TO 56-COST. MOVE SPACES TO 56-COST THRU @^@.
005370     WRITE 56-REC.
005380 CAL-56.
005390     IF SW-1 IS UNEQUAL TO 1 GO TO CK-SW2.
005400     MOVE 0 TO SW-1. MOVE 56-COV TO SUB.
005410     MOVE 00 TO SUB-ST.
005420 SUB-ADD1.
005430     ADD 01 TO SUB-ST.
005440     IF D-ST IS UNEQUAL TO ST-TAB (SUB-ST) GO TO SUB-ADD1.
005450     ADD 1 TO A-CD56 (SUB, SUB-ST).
005460     MOVE D-INDEM TO D-CL-CNT THRU @^@.
005470     ADD D-CL-CNT TO A-CL56 (SUB, SUB-ST).
005480     ADD D-INDEM TO A-COST56 (SUB, SUB-ST).
005490     ADD 1 TO TOT-CD56 (SUB).
005500     ADD D-CL-CNT TO TOT-CL56 (SUB).
005510     ADD D-INDEM TO TOT-COST56 (SUB).
005520 CK-SW2.
005530     IF SW-2 IS UNEQUAL TO 1 GO TO R-1.
005540     MOVE 0 TO SW-2. MOVE 56-COV TO SUB.
005550     MOVE 00 TO SUB-ST.
005560 ADD-SUB2.
005570     ADD 1 TO SUB-ST.
005580     IF D-ST IS UNEQUAL TO ST-TAB (SUB-ST) GO TO ADD-SUB2.
005590     ADD 1 TO A-CD56 (SUB, SUB-ST).
005600     ADD D-EXP TO A-COST56 (SUB, SUB-ST).
005610     ADD 1 TO TOT-CD56 (SUB).
005620     ADD D-EXP TO TOT-COST56 (SUB).
005630     GO TO R-1.
005640 START7.
005650     IF 1 IS EQUAL TO D-ADD OR D-CNP GO TO R-1.
005660     MOVE SPACES TO 7-REC.
005670     MOVE CON-FILL TO 7-CONST.
005680     MOVE 0 TO 7-CL-FILL.
005690     MOVE D-CLAIM TO 7-CLAIM.
005700     MOVE D-CL-1-2 TO 7-CLASS.
005710     MOVE D-ST TO 7-STATE.
005720     MOVE D-CITY1  TO 7-TERR.
005730     MOVE 9 TO 7-TRANS.
005740     MOVE @ 59@ TO 7-LINE.
005750     MOVE D-ACC-YR-LO TO 7-ACCYR.
005760     MOVE D-CL-CNT TO 7-CLCNT THRU @\@.
005770     IF D-INDEM IS NEGATIVE MOVE @:@ TO 7-CR.
005780     ADD 1 TO CD-COUNT7.
005790     MOVE D-INDEM TO 7-COST. MOVE SPACES TO 7-COST THRU @^@.
005800     WRITE 7-REC.
005810     MOVE 00 TO SUB-ST.
005820 SUB-ADD7.
005830     ADD 01 TO SUB-ST.
005840     IF D-ST IS UNEQUAL TO ST-TAB (SUB-ST) GO TO SUB-ADD7.
005850     ADD 1 TO A-CD7 (SUB-ST).
005860     MOVE D-INDEM TO D-CL-CNT THRU @^@.
005870     ADD D-INDEM TO A-COST7 (SUB-ST).
005880     ADD D-CL-CNT TO A-CL7 (SUB-ST).
005890     ADD D-CL-CNT TO TOT-CL7.
005900     ADD D-INDEM TO TOT-COST7.
005910     ADD 1 TO TOT-CD7.
005920     GO TO R-1.
005930 START19.
005940     IF 1 IS EQUAL TO D-ADD OR D-CNP GO TO R-1.
005950     MOVE SPACES TO 19-REC.
005960     MOVE CON-FILL TO 19-CONST.
005970     MOVE 0 TO 19-CL-FILL.
005980     MOVE D-CLAIM TO 19-CLAIM.
005990     MOVE D-ACC-CNT TO 19-FORMA.
006000     MOVE D-LIMIT TO 19-FORMB.
006010     MOVE D-ST TO 19-STATE.
006020     MOVE D-CITY1 TO 19-TERR.
006030     MOVE 9 TO 19-TRANS.
006040     IF D-INDEM IS NEGATIVE MOVE @:@ TO 19-CR.
006050     MOVE D-ACC-YR-LO TO 19-ACCYR.
006060     MOVE D-CL-CNT TO 19-CLCNT THRU @\@.
006070     MOVE D-INDEM TO 19-COST. MOVE SPACES TO 19-COST THRU @^@.
006080     MOVE @199@ TO 19-LINE.
006090     ADD 1 TO CD-COUNT19. WRITE 19-REC.
006100     MOVE 00 TO SUB-ST.
006110 SUB-ADD19.
006120     ADD 01 TO SUB-ST.
006130     IF D-ST IS UNEQUAL TO ST-TAB (SUB-ST) GO TO SUB-ADD19.
006140     ADD 1 TO A-CD19 (SUB-ST).
006150     MOVE D-INDEM TO D-CL-CNT THRU @^@.
006160     ADD D-INDEM TO A-COST19 (SUB-ST).
006170     ADD D-CL-CNT TO A-CL19 (SUB-ST).
006180     ADD D-CL-CNT TO TOT-CL19.
006190     ADD D-INDEM TO TOT-COST19.
006200     ADD 1 TO TOT-CD19.
006210     GO TO R-1.
006220 START71.
006230     IF D-ST IS EQUAL TO 29 GO TO R-1.
006240     IF D-CNP IS EQUAL TO 1 AND D-EXP IS EQUAL TO SPACES
006250         GO TO R-1.
006260     IF (D-ADD IS EQUAL TO 1) AND (D-COL2930 IS UNEQUAL TO 67
006270         AND 68) GO TO R-1.
006280     MOVE SPACES TO 71-REC.
006290     MOVE ZEROS TO 71-ZERO.
006300     MOVE 9 TO 71-TYPE1.
006310     MOVE CON-YR TO 71-CALYR.
006320     MOVE CON-CONO TO 71-CO.
006330     MOVE D-ST TO 71-ST.
006340     MOVE D-CITY1 TO 71-TERR.
006350     MOVE D-COL2930 TO 71-COL2930.
006360     MOVE D-CLASS1 TO 71-CLASS.
006370     MOVE D-LOAGE TO 71-LOAGE.
006380     MOVE 1 TO 71-TYPE2.
006390     MOVE ZEROS TO 71-CODE.
006400     IF D-WIND IS EQUAL TO 9 MOVE 69 TO 71-CODE.
006410     IF D-WIND IS EQUAL TO 1 MOVE 80 TO 71-CODE.
006420     MOVE ISS-YR-LO TO 71-ISSYR.
006430     MOVE D-ISS-MO TO 71-ISSMO.
006440     MOVE D-ACC-MO TO 71-ACCMO.
006450     MOVE D-CLAIM TO 71-CLAIM.
006460     MOVE D-ACC-YR-HI TO 71-ACCYR-HI.
006470     MOVE D-ACC-YR-LO TO 71-ACCYR-LO.
006480     IF CON-MO IS EQUAL TO 03 MOVE 1 TO 71-QTR.
006490     IF CON-MO IS EQUAL TO 06 MOVE 2 TO 71-QTR.
006500     IF CON-MO IS EQUAL TO 09 MOVE 3 TO 71-QTR.
006510     IF CON-MO IS EQUAL TO 12 MOVE 4 TO 71-QTR.
006520     MOVE D-CL-CNT TO 71-CL THRU @\@.
006530     MOVE 2 TO 71-TRANS.
006540     IF 1 IS EQUAL TO D-ADD OR D-CNP GO TO CK-EXP71.
006550     IF D-INDEM IS NEGATIVE MOVE @:@ TO 71-CR, 71-CR2.
006560     ADD 1 TO CD-COUNT71.
006570     MOVE D-INDEM TO 71-COST. MOVE SPACES TO 71-COST
006580         THRU @^@.
006590     MOVE 1 TO SW-1.
006600     WRITE 71-REC.
006610 CK-EXP71.
006620     IF D-EXP IS EQUAL TO SPACES GO TO CAL-71.
006630     IF D-COL2930 IS UNEQUAL TO 67 AND 68 GO TO CAL-71.
006640     MOVE SPACES TO 71-CL.
006650     MOVE 6 TO 71-TRANS.
006660     ADD 1 TO CD-COUNT71.
006670     MOVE 1 TO SW-2.
006680     MOVE SPACES TO 71-CR, 71-CR2.
006690     IF D-EXP IS NEGATIVE MOVE @:@ TO 71-CR, 71-CR2.
006700     MOVE D-EXP TO 71-COST. MOVE SPACES TO 71-COST
006710         THRU @^@.
006720     WRITE 71-REC.
006730 CAL-71.
006740     IF SW-1 IS UNEQUAL TO 1 GO TO CK-SW271.
006750     MOVE 0 TO SW-1.
006760     MOVE 00 TO SUB-ST.
006770 SUB-ADD71.
006780     ADD 01 TO SUB-ST.
006790     IF D-ST IS UNEQUAL TO ST-TAB (SUB-ST) GO TO SUB-ADD71.
006800     ADD 1 TO A-CD71 (SUB-ST).
006810     MOVE D-INDEM TO D-CL-CNT THRU @^@.
006820     ADD D-INDEM TO A-COST71 (SUB-ST).
006830     ADD D-CL-CNT TO A-CL71 (SUB-ST).
006840     ADD D-CL-CNT TO TOT-CL71.
006850     ADD D-INDEM TO TOT-COST71.
006860     ADD 1 TO TOT-CD71.
006870 CK-SW271.
006880     IF SW-2 IS UNEQUAL TO 1 GO TO R-1.
006890     MOVE 0 TO SW-2.
006900     MOVE 00 TO SUB-ST.
006910 ADD-SUB71.
006920     ADD 01 TO SUB-ST.
006930     IF D-ST IS UNEQUAL TO ST-TAB (SUB-ST) GO TO ADD-SUB71.
006940     ADD 1 TO A-CD71 (SUB-ST).
006950     ADD D-EXP TO A-COST71 (SUB-ST).
006960     ADD 1 TO TOT-CD71.
006970     ADD D-EXP TO TOT-COST71.
006980     GO TO R-1.
006990 CK-NO-TPS.
007000     CLOSE D/O-FILE.
007010     SUBTRACT 01 FROM CON-NOTPS.
007020     IF CON-NOTPS IS UNEQUAL TO 00 DISPLAY
007030         @ MOUNT NEXT D/O TAPE HIT RUN ****** @
007040         STOP 0101 GO TO OPEN-IP.
007050     CLOSE TAPE-FILE, FILE-56, FILE-7, FILE-19.
007060     CLOSE FILE-71.
007070     DISPLAY @  @. DISPLAY @  @.
007080     DISPLAY @ REMOVE LOG 1 SAVE, MOUNT WORK ***@.
007090     DISPLAY @ REMOVE LOG 3 SAVE, LABEL COV 5, 6 MOUNT WORK@.
007100     DISPLAY @ REMOVE LOG 4 SAVE, LABEL COV 7,  MOUNT WORK@.
007110     DISPLAY @ REMOVE LOG 5 SAVE, LABEL COV 19  @.
007120     DISPLAY @ REMOVE LOG 6 SAVE, LABEL COV 71  @.
007130     DISPLAY @ LOG 2 WILL SORT, ** SAVE UNSORTED LOG 2 **@.
007140     DISPLAY @ HIT RUN  4 RPTS WILL PRINT,THEN LOG 2 WILL SORT@.
007150     STOP 0202.
007160 PRINT-HEADING.
007170     MOVE SPACES TO PRINT-REC. WRITE PRINT-REC BEFORE
007180         ADVANCING HEAD-OF-FORM.
007190     IF CON-CONO IS EQUAL TO 6095 WRITE PRINT-REC FROM
007200         NH-HEAD BEFORE ADVANCING 2 LINES.
007210     IF CON-CONO IS EQUAL TO 5779 WRITE PRINT-REC FROM
007220         MM-HEAD BEFORE ADVANCING 2 LINES.
007230     IF SW-A IS EQUAL TO 1 GO TO CK-SWB.
007240     WRITE PRINT-REC FROM 56-HEAD BEFORE ADVANCING 2 LINES.
007250     GO TO PRINT-QTR.
007260 CK-SWB.
007270     IF SW-B IS EQUAL TO 1 GO TO CK-SWC.
007280     MOVE 1 TO SW-B.
007290     WRITE PRINT-REC FROM 7-HEAD BEFORE ADVANCING 2 LINES.
007300     GO TO PRINT-QTR.
007310 CK-SWC.
007320     IF SW-C IS EQUAL TO 1 GO TO CK-SWD.
007330     MOVE 1 TO SW-C.
007340     WRITE PRINT-REC FROM 19-HEAD BEFORE ADVANCING 2 LINES.
007350     GO TO PRINT-QTR.
007360 CK-SWD.
007370     WRITE PRINT-REC FROM 71-HEAD BEFORE ADVANCING 2 LINES.
007380 PRINT-QTR.
007390     MOVE CON-YR TO QTR-YR.
007400     IF CON-MO IS EQUAL TO 03 MOVE @1ST@ TO QTR-NO.
007410     IF CON-MO IS EQUAL TO 06 MOVE @2ND@ TO QTR-NO.
007420     IF CON-MO IS EQUAL TO 09 MOVE @3RD@ TO QTR-NO.
007430     IF CON-MO IS EQUAL TO 12 MOVE @4TH@ TO QTR-NO.
007440     WRITE PRINT-REC FROM QTR-HEAD BEFORE ADVANCING 3 LINES.
007450     IF SW-A IS EQUAL TO 1 GO TO HEAD-719.
007460     MOVE 1 TO SW-A.
007470     WRITE PRINT-REC FROM 56-LINE1.
007480     WRITE PRINT-REC FROM 56-LINE2 BEFORE ADVANCING 2 LINES.
007490     GO TO END-HEADING.
007500 HEAD-719.
007510     WRITE PRINT-REC FROM 7-19-LINE1.
007520     WRITE PRINT-REC FROM 7-19-LINE2 BEFORE ADVANCING 2 LINES.
007530 END-HEADING. EXIT.
007540 PRINT56.
007550     MOVE 00 TO SUB-ST.
007560     MOVE 1 TO SUB.
007570 TRY-56.
007580     ADD 1 TO SUB-ST.
007590     IF SUB-ST IS EQUAL TO 22 GO TO PRINT-TOTAL-56.
007600     IF A-CL56 (SUB, SUB-ST) IS UNEQUAL TO ZEROS GO TO
007610         PRINT-LINE-56.
007620     IF A-COST56 (SUB, SUB-ST) IS UNEQUAL TO ZEROS GO TO
007630         PRINT-LINE-56.
007640     IF A-CD56 (SUB, SUB-ST) IS UNEQUAL TO ZEROS GO TO
007650         PRINT-LINE-56.
007660     GO TO TRY-56.
007670 PRINT-LINE-56.
007680     MOVE SPACES TO DETAIL-56.
007690     MOVE CON-CONO TO D56-CONO.
007700     MOVE SUB TO D56-COV.
007710     MOVE ST-TAB (SUB-ST) TO D56-ST.
007720     MOVE A-CL56 (SUB, SUB-ST) TO D56-CLCNT.
007730     MOVE A-COST56 (SUB, SUB-ST) TO D56-COST.
007740     MOVE A-CD56 (SUB, SUB-ST) TO D56-CDCNT.
007750     WRITE PRINT-REC FROM DETAIL-56.
007760     GO TO TRY-56.
007770 PRINT-TOTAL-56.
007780     MOVE SPACES TO DETAIL-56.
007790     WRITE PRINT-REC FROM DETAIL-56 BEFORE ADVANCING 2 LINES.
007800     MOVE TOT-CL56 (SUB) TO D56-CLCNT.
007810     MOVE TOT-COST56 (SUB) TO D56-COST.
007820     MOVE TOT-CD56 (SUB) TO D56-CDCNT.
007830     WRITE PRINT-REC FROM DETAIL-56 BEFORE ADVANCING 3 LINES.
007840     IF SUB IS EQUAL TO 3 GO TO PRINT7.
007850     ADD 1 TO SUB. MOVE 00 TO SUB-ST. GO TO TRY-56.
007860 PRINT7.
007870     PERFORM PRINT-HEADING THRU END-HEADING.
007880     MOVE 00 TO SUB-ST.
007890 TRY-7.
007900     ADD 1 TO SUB-ST.
007910     IF SUB-ST IS EQUAL TO 22 GO TO PRINT-TOTAL-7.
007920     IF A-CL7 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
007930         PRINT-LINE-7.
007940     IF A-COST7 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
007950         PRINT-LINE-7.
007960     IF A-CD7 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
007970         PRINT-LINE-7.
007980     GO TO TRY-7.
007990 PRINT-LINE-7.
008000     MOVE SPACES TO DETAIL-7-19.
008010     MOVE CON-CONO TO D719-CONO.
008020     MOVE ST-TAB (SUB-ST) TO D719-ST.
008030     MOVE A-CL7 (SUB-ST) TO D719-CLCNT.
008040     MOVE A-COST7 (SUB-ST) TO D719-COST.
008050     MOVE A-CD7 (SUB-ST) TO D719-CDCNT.
008060     WRITE PRINT-REC FROM DETAIL-7-19.
008070     GO TO TRY-7.
008080 PRINT-TOTAL-7.
008090     MOVE SPACES TO DETAIL-7-19.
008100     WRITE PRINT-REC FROM DETAIL-7-19 BEFORE
008110     ADVANCING 2 LINES.
008120     MOVE TOT-CL7 TO D719-CLCNT.
008130     MOVE TOT-COST7 TO D719-COST.
008140     MOVE TOT-CD7 TO D719-CDCNT.
008150     WRITE PRINT-REC FROM DETAIL-7-19 BEFORE
008160         ADVANCING HEAD-OF-FORM.
008170 PRINT19.
008180     PERFORM PRINT-HEADING THRU END-HEADING.
008190     MOVE 00 TO SUB-ST.
008200 TRY-19.
008210     ADD 1 TO SUB-ST.
008220     IF SUB-ST IS EQUAL TO 22 GO TO PRINT-TOTAL-19.
008230     IF A-CL19 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
008240         PRINT-LINE-19.
008250     IF A-COST19 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
008260         PRINT-LINE-19.
008270     IF A-CD19 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
008280         PRINT-LINE-19.
008290     GO TO TRY-19.
008300 PRINT-LINE-19.
008310     MOVE SPACES TO DETAIL-7-19.
008320     MOVE CON-CONO TO D719-CONO.
008330     MOVE ST-TAB (SUB-ST) TO D719-ST.
008340     MOVE A-CL19 (SUB-ST) TO D719-CLCNT.
008350     MOVE A-COST19 (SUB-ST) TO D719-COST.
008360     MOVE A-CD19 (SUB-ST) TO D719-CDCNT.
008370     WRITE PRINT-REC FROM DETAIL-7-19.
008380     GO TO TRY-19.
008390 PRINT-TOTAL-19.
008400     MOVE SPACES TO DETAIL-7-19.
008410     WRITE PRINT-REC FROM DETAIL-7-19 BEFORE
008420         ADVANCING 2 LINES.
008430     MOVE TOT-CL19 TO D719-CLCNT.
008440     MOVE TOT-COST19 TO D719-COST.
008450     MOVE TOT-CD19 TO D719-CDCNT.
008460     WRITE PRINT-REC FROM DETAIL-7-19 BEFORE
008470         ADVANCING HEAD-OF-FORM.
008480 PRINT-71.
008490     PERFORM PRINT-HEADING THRU END-HEADING.
008500     MOVE 00 TO SUB-ST.
008510 TRY-71.
008520     ADD 1 TO SUB-ST.
008530     IF SUB-ST IS EQUAL TO 22 GO TO PRINT-TOTAL-71.
008540     IF A-CL71 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
008550         PRINT-LINE-71.
008560     IF A-COST71 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
008570         PRINT-LINE-71.
008580     IF A-CD71 (SUB-ST) IS UNEQUAL TO ZEROS GO TO
008590         PRINT-LINE-71.
008600     GO TO TRY-71.
008610 PRINT-LINE-71.
008620     MOVE SPACES TO DETAIL-7-19.
008630     MOVE CON-CONO TO D719-CONO.
008640     MOVE ST-TAB (SUB-ST) TO D719-ST.
008650     MOVE A-CL71 (SUB-ST) TO D719-CLCNT.
008660     MOVE A-COST71 (SUB-ST) TO D719-COST.
008670     MOVE A-CD71 (SUB-ST) TO D719-CDCNT.
008680     WRITE PRINT-REC FROM DETAIL-7-19.
008690     GO TO TRY-71.
008700 PRINT-TOTAL-71.
008710     MOVE SPACES TO DETAIL-7-19.
008720     WRITE PRINT-REC FROM DETAIL-7-19 BEFORE
008730     ADVANCING 2 LINES.
008740     MOVE TOT-CL71 TO D719-CLCNT.
008750     MOVE TOT-COST71 TO D719-COST.
008760     MOVE TOT-CD71 TO D719-CDCNT.
008770     WRITE PRINT-REC FROM DETAIL-7-19 BEFORE
008780     ADVANCING HEAD-OF-FORM.
008790     CLOSE PRINT-FILE.
008800     DISPLAY @ HEY   DID YOU MOUNT WORK TAPES ON 1,3,4, @.
008810     DISPLAY @ ***IF SO HIT RUN*** @.
008820     DISPLAY @ ** DONT FORGET TO SAVE UNSORTED LOG 2**@.
008830     DISPLAY @ AFTER PRE SORT, LABEL IT COV 1, 2 **@.
008840     STOP 0303.
008850 SORT-IT.
008860     MOVE @/SORTF@ TO *PROGRAM.
008870     MOVE 01 TO *SEGMENT.
008880     STOP RUN.
999999 END COBOL
 +t[