// JOB CE681526                                                                 
//  OPTION  LOG,DUMP,LIST,LISTX                                                 
// EXEC COBOL                                                                   
000010                                                                  CE681526
000020 IDENTIFICATION DIVISION.                                         CE681526
000030                                                                  CE681526
000040 PROGRAM-ID.                                                      CE681526
000050     ^CE681526^.                                                          
000060 AUTHOR.                                                          CE681526
000070     GLAD.                                                        CE681526
000080 DATE-WRITTEN.                                                    CE681526
000090     AUGUST 1969.                                                 CE681526
000100 REMARKS.                                                         CE681526
000110     THIS PROGRAM UPDATES THE INVENTORY MASTER FILE BY PROCESSING CE681526
000120       B/M ISSUES, REQUISITIONS AND SHORTAGES,  26,31,33 CARDS.   CE681526
000130       A DAILY TRANSACTION                                        CE681526
000140     LISTING IS PRODUCED SHOWING ALL PROCESSED ACTIVITY PLUS      CE681526
000150     CURRENT QUANTITY LEVELS.  SHORTAGE CARDS AND NOTIFICATION    CE681526
000160     CARDS ARE PUNCHED.  AN ^OTHER^ FILE IS CREATED ON DISK       CE681526
000170     CONTAINING THE FOLLOWING ---  EXCEPTION LISTING CARDS FOR    CE681526
000180     ALL NON-PROCESSED TRANSACTIONS -  BUG CARDS FOR NON-PROCESSEDCE681526
000190       26 CARDS - B/M ISSUE CARDS FOR ALL PROCESSED               CE681526
000200     B/M ISSUE INPUT AND INV ADJ CARDS.                                   
000220                     ALL PROCESSED TRANSACTIONS ARE TAGGED        CE681526
000230     WITH A 1 AND REWRITTEN.   TOTALS ARE WRITTEN ON TAPE         CE681526
000240     FOR PRINTING IN FOLLOWING PROGRAM.                           CE681526
000250                                                                  CE681526
000260                                                                  CE681526
000270 ENVIRONMENT DIVISION.                                            CE681526
000280                                                                  CE681526
000290 INPUT-OUTPUT SECTION.                                            CE681526
000300 FILE-CONTROL.                                                    CE681526
000310     SELECT  TRANSFILE   ASSIGN TO ^SYS023^ DIRECT-ACCESS 2314.   CE681526
000320                                                                  CE681526
000330     SELECT  PRINTFILE   ASSIGN TO ^SYS005^ UNIT-RECORD 1403.     CE681526
000340                                                                  CE681526
000350     SELECT  CARDFILE   ASSIGN TO ^SYS006^ UNIT-RECORD 2540P      CE681526
000360           RESERVE NO ALTERNATE AREA.                             CE681526
000370                                                                  CE681526
000380     SELECT  OTHERFILE   ASSIGN TO ^SYS033^ DIRECT-ACCESS 2314    CE681526
000390         RESERVE NO ALTERNATE AREA.                               CE681526
000400                                                                  CE681526
000410     SELECT TAPEFILE  ASSIGN TO ^SYS009^  UTILITY 2400                    
000420         RESERVE NO ALTERNATE AREA.                               CE681526
000430                                                                  CE681526
000440 DATA DIVISION.                                                   CE681526
000450 FILE SECTION.                                                    CE681526
000460                                                                  CE681526
000470 FD TRANSFILE                                                     CE681526
000480     RECORDING MODE IS F                                          CE681526
000490     RECORD CONTAINS 81 CHARACTERS                                CE681526
000500     BLOCK CONTAINS 4 RECORDS                                     CE681526
000510     LABEL RECORDS ARE STANDARD                                   CE681526
000520     DATA RECORDS ARE  TRAN-26                                    CE681526
000530             TRAN-31  TRAN-33                                     CE681526
000540                       DATE-CARD.                                 CE681526
000550                                                                  CE681526
000560                                                                  CE681526
000570                                                                  CE681526
000580                                                                  CE681526
000590 01  TRAN-26.                                                     CE681526
000600         03  CARD-CODE       PICTURE XX.                          CE681526
000610         03  CAT-NO          PICTURE X]10[.                       CE681526
000620         03  CHK-DIGIT       PICTURE X.                           CE681526
000630         03  T26-WO          PICTURE X]5[.                        CE681526
000640         03  T26-DWG-NO      PICTURE X]15[.                       CE681526
000650         03  T26-PARENT-NO   PICTURE X]10[.                       CE681526
000660         03  T26-P-CKD       PICTURE X.                           CE681526
000670         03  T26-QTY-ASSY    PICTURE XXX.                         CE681526
000680         03  T26-REQ-QTY     PICTURE S9]6[.                       CE681526
000690         03  T26-PHASE       PICTURE XX.                          CE681526
000700         03  T26-U-MEAS      PICTURE XX.                          CE681526
000710         03  T26-SEC         PICTURE XX.                          CE681526
000720         03  T26-LOC         PICTURE XX.                          CE681526
000730         03  T26-CD-SEQ      PICTURE XXX.                         CE681526
000740         03  T26-LC          PICTURE X.                           CE681526
000750         03  T26-TSP-CYC     PICTURE XXX.                         CE681526
000760         03  FILLER          PICTURE X.                           CE681526
000770         03  T26-ISS-QTY     PICTURE S9]6[.                       CE681526
000780         03  TM-DATE.                                             CE681526
000790             05  TM-YR       PICTURE XX.                          CE681526
000800             05  TM-DAY      PICTURE XXX.                         CE681526
000810         03  PROC-TAG        PICTURE X.                           CE681526
000820                                                                  CE681526
000830 01  TRAN-31.                                                     CE681526
000840         03  FILLER          PICTURE X]13[.                       CE681526
000850         03  T31-CHRG-NO.                                         CE681526
000860           04    T31-CDS.                                         CE681526
000870             05  T31-CTL     PICTURE X.                           CE681526
000880             05  T31-DEPT    PICTURE XXXX.                        CE681526
000890             05  T31-SUB     PICTURE XXX.                         CE681526
000900          04 CHRG-CD.                                             CE681526
000910             05  T31-CHRG.                                        CE681526
000920                 07  POS-123 PICTURE XXX.                         CE681526
000930                 07  FILLER  PICTURE X]5[.                        CE681526
000940             05  T31-CODE    PICTURE X.                           CE681526
000950         03  T31-REQ.                                             CE681526
000960             05  FILLER      PICTURE X.                           CE681526
000970             05  T31-REQ-2   PICTURE X]5[.                        CE681526
000980         03  T31-RC          PICTURE XX.                          CE681526
000990         03  T31-DWG-NO      PICTURE X]15[.                       CE681526
001000         03  T31-UNIT        PICTURE XX.                          CE681526
001010         03  T31-REQ-QTY     PICTURE S9]7[.                       CE681526
001020         03  T31-ISS-QTY     PICTURE S9]7[.                       CE681526
001030         03  T31-UNPL        PICTURE X.                           CE681526
001040         03  T31-U-COMM      PICTURE XXXX.                                
001060         03  T31-BATCH       PICTURE X.                           CE681526
001070         03  FILLER          PICTURE X]6[.                        CE681526
001080                                                                  CE681526
001090                                                                  CE681526
001100                                                                  CE681526
001110 01  DATE-CARD.                                                   CE681526
001120         03  FILLER          PICTURE X]13[.                       CE681526
001130         03  MDATE-CARD      PICTURE X]5[.                        CE681526
001140         03  MDATE-RED REDEFINES MDATE-CARD  PICTURE 9]5[.        CE681526
001150         03  FILLER          PICTURE X]63[.                       CE681526
001160                                                                  CE681526
001170 01  TRAN-33.                                                     CE681526
001180         03  FILLER          PICTURE X]13[.                       CE681526
001190         03  T33-CHRG-NO.                                         CE681526
001200          04 T33-CDS.                                             CE681526
001210             05  T33-CTL     PICTURE X.                           CE681526
001220             05  T33-DEPT    PICTURE XXXX.                        CE681526
001230             05  T33-SUB     PICTURE XXX.                         CE681526
001240          04 FILLER.                                              CE681526
001250             05  T33-CHRG.                                        CE681526
001260                 07  POS-123 PICTURE XXX.                         CE681526
001270                 07  FILLER  PICTURE X]5[.                        CE681526
001280             05  T33-CODE    PICTURE X.                           CE681526
001290         03  T33-REQ.                                             CE681526
001300             05  FILLER      PICTURE X.                           CE681526
001310             05  T33-REQ-2   PICTURE X]5[.                        CE681526
001320         03  T33-RC          PICTURE XX.                          CE681526
001330         03  T33-U-MEAS      PICTURE XX.                          CE681526
001340         03  T33-6P-CODE     PICTURE XX.                          CE681526
001350         03  T33-REQ-QTY     PICTURE S9]7[.                       CE681526
001360         03  T33-ISS-TO-DATE PICTURE S9]7[.                       CE681526
001370         03  T33-REM-QTY     PICTURE S9]7[.                       CE681526
001380         03  T33-UNPL        PICTURE X.                           CE681526
001390         03  T33-ISS-QTY     PICTURE S9]7[.                       CE681526
001399         03  T33-U-COMM      PICTURE XXXX.                                
001400         03  FILLER          PICTURE X]6[.                                
001410                                                                  CE681526
001420                                                                  CE681526
001430 FD  OTHERFILE                                                    CE681526
001440     RECORDING MODE IS F                                          CE681526
001450     RECORD CONTAINS 94 CHARACTERS                                CE681526
001460     BLOCK CONTAINS 4 RECORDS                                     CE681526
001470     LABEL RECORDS ARE STANDARD                                   CE681526
001480     DATA RECORD IS OTHER-REC.                                    CE681526
001490                                                                  CE681526
001500                                                                  CE681526
001510                                                                  CE681526
001520 01  OTHER-REC.                                                   CE681526
001530         03  FILLER          PICTURE X]94[.                       CE681526
001540                                                                  CE681526
001550                                                                  CE681526
001560 FD  PRINTFILE                                                    CE681526
001570     RECORDING MODE IS F                                          CE681526
001580     RECORD CONTAINS 133 CHARACTERS                               CE681526
001590     LABEL RECORDS ARE OMITTED                                    CE681526
001600     DATA RECORD IS PRINT-REC.                                    CE681526
001610                                                                  CE681526
001620 01  PRINT-REC.                                                   CE681526
001630         03  FILLER          PICTURE X]15[.                       CE681526
001640         03  PRINT-REC-X     PICTURE X]118[.                      CE681526
001650                                                                  CE681526
001660                                                                  CE681526
001670                                                                  CE681526
001680 FD  CARDFILE                                                     CE681526
001690     RECORDING MODE IS F                                          CE681526
001700     RECORD CONTAINS 81 CHARACTERS                                CE681526
001710     LABEL RECORDS ARE OMITTED                                    CE681526
001720     DATA RECORD IS CARD-REC.                                     CE681526
001730                                                                  CE681526
001740 01  CARD-REC.                                                    CE681526
001750         03  FILLER          PICTURE X]81[.                       CE681526
001760                                                                  CE681526
001770 FD  TAPEFILE                                                     CE681526
001780     RECORDING MODE F                                             CE681526
001790     RECORD CONTAINS  61 CHARACTERS                               CE681526
001800     LABEL RECORDS ARE STANDARD                                   CE681526
001810     DATA RECORD IS TAPE-TOT-REC.                                 CE681526
001820                                                                  CE681526
001830 01  TAPE-TOT-REC.                                                CE681526
001840     02  FILLER              PICTURE X]61[.                       CE681526
001850                                                                  CE681526
001860                                                                  CE681526
001870 WORKING-STORAGE SECTION.                                         CE681526
001880                                                                  CE681526
001890 77  TURN-ON                 PICTURE X   VALUE ^1^.               CE681526
001900 77  TURN-OFF                PICTURE X   VALUE ^0^.               CE681526
001910 77  PAGE-NO                 PICTURE 999 VALUE ZERO.              CE681526
001920 77  LINE-CT                 PICTURE 99  VALUE ZERO.              CE681526
001930 77  EXT-VALUE COMPUTATIONAL-3 PICTURE S9]8[V999 VALUE ZERO.      CE681526
001940 77  PREV-CAT                PICTURE X]10[.                       CE681526
001950 77  SHORT-QTY               PICTURE  9]7[.                       CE681526
001960 77  REVERSE-ISS-QTY COMPUTATIONAL-3 PICTURE S9]7[.               CE681526
001970 77  REVERSE-ALLOC  COMPUTATIONAL-3  PICTURE S9]7[.               CE681526
001980                                                                  CE681526
001990 77  CARRIAGE-CONTROL        PICTURE X.                           CE681526
002000 77  STACKER-SELECT          PICTURE X.                           CE681526
002010 77  OPER-REPLY              PICTURE X.                           CE681526
002020 77  PROCESSED-SWT           PICTURE X    VALUE ^0^.              CE681526
002030     88  PROCESSED-SWT-ON                VALUE ^1^.               CE681526
002040 77  NON-PROCESS-SWT         PICTURE X   VALUE ^0^.               CE681526
002050     88  NON-PROCESS-SWT-ON              VALUE ^1^.               CE681526
002060 77  INITIAL-SWT             PICTURE X   VALUE ^1^.               CE681526
002070     88  INITIAL-SWT-ON  VALUE ^1^.                               CE681526
002080 77  SHORT-SWT               PICTURE X   VALUE ^0^.               CE681526
002090     88  SHORT-SWT-ON    VALUE ^1^.                               CE681526
002100                                                                  CE681526
002110 77  CHG-ACT-DATE-SWT        PICTURE X   VALUE ^0^.               CE681526
002120     88  CHG-ACT-DATE-SWT-ON     VALUE ^1^.                       CE681526
002130 77  FIRST-TIME-PRINT-SWT    PICTURE X   VALUE ^1^.               CE681526
002140     88  FIRST-TIME-PRINT-SWT-ON VALUE ^1^.                       CE681526
002150                                                                  CE681526
002160 01  SAVE-FIELD.                                                  CE681526
002170     02  SAVE-FLD-NO         PICTURE XXX.                         CE681526
002180     02  SAVE-FLD-NO-RED REDEFINES SAVE-FLD-NO PICTURE 999.       CE681526
002190 01  EXCEPT-AREA.                                                 CE681526
002200         03  EX-REC-CODE     PICTURE X.                           CE681526
002210         03  EX-CAT-NO       PICTURE X]11[.                       CE681526
002220         03  EX-MSG-CODE     PICTURE XX.                          CE681526
00        03  EX-CARD         PICTURE X]80[.                       CE681526
002240                                                                  CE681526
002250                                                                  CE681526
002260                                                                  CE681526
002270 01  BUG-AREA  REDEFINES EXCEPT-AREA.                             CE681526
002280         03  BUG-REC-CODE    PICTURE X.                           CE681526
002290         03  BUG-CAT-NO      PICTURE X]11[.                       CE681526
002300         03  BUG-MSG-CODE    PICTURE XX.                          CE681526
002310         03  BUG-CARD        PICTURE X]80[.                       CE681526
002320                                                                  CE681526
002330                                                                  CE681526
002340 01  BM-ISS-AREA  REDEFINES EXCEPT-AREA.                          CE681526
002350         03  BM-REC-CODE     PICTURE X.                           CE681526
002360         03  BM-CAT-NO       PICTURE X]11[.                       CE681526
002370         03  BM-MSG-CODE     PICTURE XX.                          CE681526
002380         03  BM-CARD         PICTURE X]80[.                       CE681526
002390                                                                  CE681526
002400 01  CYCLE-ADJ-CARD.                                              CE681526
002410         03  OTHER-REC-CODE  PICTURE X.                           CE681526
002420         03  OTHER-CAT-NO    PICTURE X]11[.                       CE681526
002430         03  OTHER-MSG-CODE  PICTURE XX.                          CE681526
002440         03  CA-CARD-CD      PICTURE XX.                          CE681526
002450         03  CA-CAT-NO       PICTURE X]10[.                       CE681526
002460         03  CA-CHK-DIG      PICTURE X.                           CE681526
002470         03  CA-LEDGER       PICTURE XXX.                         CE681526
002480         03  CA-SOURCE       PICTURE X]6[.                        CE681526
002490         03  FILLER          PICTURE X]12[.                       CE681526
002500         03  CA-RC           PICTURE XX.                          CE681526
002510         03  CA-U-COST       PICTURE S9]6[V999.                   CE681526
002520         03  CA-EXT-VAL      PICTURE S9]6[V99.                    CE681526
002530         03  CA-SIGN         PICTURE X.                           CE681526
002540         03  FILLER          PICTURE X]14[.                       CE681526
002550         03  CA-ADJ-QTY      PICTURE S9]7[.                       CE681526
002560         03  CA-DATE         PICTURE X]5[.                        CE681526
002570                                                                  CE681526
002990                                                                  CE681526
003000 01  MEMO-STK-CARD  REDEFINES CYCLE-ADJ-CARD.                     CE681526
003010         03  FILLER          PICTURE X]14[.                       CE681526
003020         03  MS-CAT          PICTURE X]10[.                       CE681526
003030         03  FILLER          PICTURE X]9[.                        CE681526
003040         03  MS-WO           PICTURE XXXX.                        CE681526
003050         03  FILLER          PICTURE X]10[.                       CE681526
003060         03  MS-LOC          PICTURE XX.                          CE681526
003070         03  FILLER          PICTURE X]15[.                       CE681526
003080         03  MS-U-COST       PICTURE S9]6[V999.                   CE681526
003090         03  MS-OH-QTY       PICTURE S9]6[V9.                     CE681526
003100         03  MS-EXT-VAL      PICTURE S9]6[V99.                    CE681526
003110         03  FILLER          PICTURE X]6[.                        CE681526
003120                                                                  CE681526
003130 01  SHORT-CARD-OUT.                                              CE681526
003140         03  FILLER          PICTURE X.                           CE681526
003150         03  SHORT-CODE      PICTURE XX.                          CE681526
003160         03  SHORT-CAT-NO    PICTURE X]10[.                       CE681526
003170         03  SHORT-CHK-DIG   PICTURE X.                           CE681526
003180         03  SHORT-CHARGE.                                        CE681526
003190          05 SHORT-CTL       PICTURE X.                           CE681526
003200          05 SHORT-DEPT      PICTURE XXXX.                        CE681526
003210          05 SHORT-SUB       PICTURE XXX.                         CE681526
003220          05 SHORT-CHRG-NO   PICTURE X]8[.                        CE681526
003230          05 SHORT-CHRG-CODE PICTURE X.                           CE681526
003240         03  SHORT-REQ-NO    PICTURE X]6[.                        CE681526
003250         03  SHORT-RC        PICTURE XX.                          CE681526
003260         03  SHORT-U-MEAS    PICTURE XX.                          CE681526
003270         03  SHORT-GROUP.                                         CE681526
003271           05  SHORT-GROUP-1     PICTURE X.                       CE681526
003272           05  SHORT-GROUP-2     PICTURE X.                       CE681526
003280         03  SHORT-REQUIRED  PICTURE S9]7[.                       CE681526
003290         03  SHORT-ISS-TO-DA PICTURE S9]7[.                       CE681526
003300         03  SHORT-REMAIN    PICTURE  9]7[.                       CE681526
003310         03  SHORT-UNPL      PICTURE X.                           CE681526
003320         03  SHORT-ISS-QTY   PICTURE S9]7[.                       CE681526
003340         03  SHORT-U-COMM    PICTURE XXXX.                                
003350         03  SHORT-MYEAR     PICTURE XX.                          CE681526
003360         03  SHORT-MDAY      PICTURE XXX.                         CE681526
003370                                                                  CE681526
003380 01  HD-LINE-1.                                                   CE681526
003390     02  FILLER              PICTURE X]10[ VALUE ^ PAGE NO. ^.    CE681526
003400     02  HD-PAGE             PICTURE ZZ9.                         CE681526
003410     02  FILLER              PICTURE X]112[ VALUE    ^            CE681526
003420-       ^                 M I N C O M  D I V I S I O N,  C A M A RCE681526
003430-       ^ I L L O                                   ^.            CE681526
003440     02  HD-MDY-DATE         PICTURE X]8[.                        CE681526
003450                                                                  CE681526
003460 01  HD-LINE-2.                                                   CE681526
003470     02  FILLER              PICTURE X]65[  VALUE    ^ REPORT NO. CE681526
003480-       ^6815-28                          INVENTORY CONTROL - ^.  CE681526
003490     02  FILLER              PICTURE X]63[  VALUE    ^DAILY TRANSACE681526
003500-       ^CTION LISTING                               M-DATE ^.    CE681526
003510     02  HD-MDATE            PICTURE X]5[.                        CE681526
003520 01  HD-LINE-3.                                                   CE681526
003530     02  FILLER              PICTURE X]67[   VALUE   ^ CC     CATACE681526
003540-       ^LOG     CHARGE   CHARGE   LAST    ON      AL-       ON ^.CE681526
003550     02  FILLER              PICTURE X]28[   VALUE   ^     IN     CE681526
003560-       ^OUTSIDE    RE-  ^.                                       CE681526
003570                                                                  CE681526
003580                                                                  CE681526
003590 01  HD-LINE-4.                                                   CE681526
003600     02  FILLER              PICTURE X]67[   VALUE   ^         NUMCE681526
003610-       ^BER     NUMBER   SOURCE    ACT   HAND   LOCATED   ORDER^.CE681526
003620     02  FILLER              PICTURE X]28[   VALUE   ^   TRANSIT  CE681526
003630-       ^PROCESS  EVALUAT^.                                       CE681526
003640                                                                  CE681526
003650 01  DETAIL-LINE.                                                 CE681526
003660     02  FILLER              PICTURE X.                           CE681526
003670     02  D-CARD              PICTURE XX.                          CE681526
003680     02  FILLER              PICTURE X.                           CE681526
003690     02  D-CAT               PICTURE X]14[.                       CE681526
003700     02  FILLER              PICTURE X.                           CE681526
003710     02  D-CHRG-NO           PICTURE X]8[.                        CE681526
003720     02  FILLER              PICTURE X.                           CE681526
003730     02  D-CHRG-SO.                                               CE681526
003740         03  D-CHRG-SO-1     PICTURE XX.                          CE681526
003750         03  D-CHRG-SO-2     PICTURE X]6[.                        CE681526
003760     02  FILLER              PICTURE X.                           CE681526
003770     02  D-LAST-ACT          PICTURE X]5[.                        CE681526
003780     02  FILLER              PICTURE X.                           CE681526
003790     02  D-ON-HAND           PICTURE ZZZZZZZ-.                    CE681526
003800     02  FILLER              PICTURE X.                           CE681526
003810     02  D-ALLOC             PICTURE ZZZZZZZ-.                    CE681526
003820     02  FILLER              PICTURE X.                           CE681526
003830     02  D-ON-ORDER          PICTURE ZZZZZZZ-.                    CE681526
003840     02  FILLER              PICTURE X.                           CE681526
003850     02  D-IN-TRANS          PICTURE ZZZZZZZ-.                    CE681526
003860     02  FILLER              PICTURE X.                           CE681526
003870     02  D-OUT-PROC          PICTURE ZZZZZZZ-.                    CE681526
003880     02  FILLER              PICTURE X.                           CE681526
003890     02  D-RE-VAL            PICTURE ZZZZZZZ-.                    CE681526
003900     02  FILLER              PICTURE X]7[.                        CE681526
003910                                                                  CE681526
003920 01  DET-CAT-TOTAL  REDEFINES DETAIL-LINE.                        CE681526
003930     02  FILLER              PICTURE X]18[.                       CE681526
003940     02  DT-MSG-A            PICTURE X]25[.                       CE681526
003950     02  DT-ON-HAND          PICTURE ZZZZZZZ-.                    CE681526
003960     02  DT-ALLOC            PICTURE ZZZZZZZZ-.                   CE681526
003970     02  DT-ON-ORDER         PICTURE ZZZZZZZZ-.                   CE681526
003980     02  DT-IN-TRANS         PICTURE ZZZZZZZZ-.                   CE681526
003990     02  DT-OUT-PROC         PICTURE ZZZZZZZZ-.                   CE681526
004000     02  DT-RE-VAL           PICTURE ZZZZZZZZ-.                   CE681526
004010     02  FILLER              PICTURE X.                           CE681526
004020     02  DT-MSG              PICTURE X]6[.                        CE681526
004030                                                                  CE681526
004040                                                                  CE681526
004050 01  PM-WORK COPY ^PMWK3M^.                                               
004060                                                                  CE681526
004070                                                                  CE681526
004080 01  PROC-ACCUMS  COMPUTATIONAL-3.                                CE681526
004090     02  PR-I26-ON-HAND      PICTURE S9]7[ VALUE ZEROS.           CE681526
004100     02  PR-I26-ALLOC        PICTURE S9]7[ VALUE ZEROS.           CE681526
004110     02  PR-I26-PL-USG       PICTURE S9]7[ VALUE ZEROS.           CE681526
004120     02  PR-I26-UNPL         PICTURE S9]7[ VALUE ZEROS.           CE681526
004130     02  PR-I26-REC-CT       PICTURE S9]5[ VALUE ZEROS.           CE681526
004140     02  PR-I31-ON-HAND      PICTURE S9]7[ VALUE ZEROS.           CE681526
004150     02  PR-I31-ALLOC        PICTURE S9]7[ VALUE ZEROS.           CE681526
004160     02  PR-I31-REVAL        PICTURE S9]7[ VALUE ZEROS.           CE681526
004170     02  PR-I31-PL-USG       PICTURE S9]7[ VALUE ZEROS.           CE681526
004180     02  PR-I31-UNPL         PICTURE S9]7[ VALUE ZEROS.           CE681526
004190     02  PR-I31-REC-CT       PICTURE S9]5[ VALUE ZEROS.           CE681526
004200     02  PR-I33-ON-HAND      PICTURE S9]7[ VALUE ZEROS.           CE681526
004210     02  PR-I33-ALLOC        PICTURE S9]7[ VALUE ZEROS.           CE681526
004220     02  PR-I33-PL-USG       PICTURE S9]7[ VALUE ZEROS.           CE681526
004230     02  PR-I33-UNPL         PICTURE S9]7[ VALUE ZEROS.           CE681526
004240     02  PR-I33-REC-CT       PICTURE S9]5[ VALUE ZEROS.           CE681526
004250 01  NON-PROC-ACCUMS  COMPUTATIONAL-3.                            CE681526
004260     02  NP-I26-ON-HAND      PICTURE S9]7[ VALUE ZEROS.           CE681526
004270     02  NP-I26-ALLOC        PICTURE S9]7[ VALUE ZEROS.           CE681526
004280     02  NP-I26-PL-USG       PICTURE S9]7[ VALUE ZEROS.           CE681526
004290     02  NP-I26-UNPL         PICTURE S9]7[ VALUE ZEROS.           CE681526
004300     02  NP-I26-REC-CT       PICTURE S9]5[ VALUE ZEROS.           CE681526
004310     02  NP-I31-ON-HAND      PICTURE S9]7[ VALUE ZEROS.           CE681526
004320     02  NP-I31-ALLOC        PICTURE S9]7[ VALUE ZEROS.           CE681526
004330     02  NP-I31-REVAL        PICTURE S9]7[ VALUE ZEROS.           CE681526
004340     02  NP-I31-PL-USG       PICTURE S9]7[ VALUE ZEROS.           CE681526
004350     02  NP-I31-UNPL         PICTURE S9]7[ VALUE ZEROS.           CE681526
004360     02  NP-I31-REC-CT       PICTURE S9]5[ VALUE ZEROS.           CE681526
004370     02  NP-I33-ON-HAND      PICTURE S9]7[ VALUE ZEROS.           CE681526
004380     02  NP-I33-ALLOC        PICTURE S9]7[ VALUE ZEROS.           CE681526
004390     02  NP-I33-PL-USG       PICTURE S9]7[ VALUE ZEROS.           CE681526
004400     02  NP-I33-UNPL         PICTURE S9]7[ VALUE ZEROS.           CE681526
004410     02  NP-I33-REC-CT       PICTURE S9]5[ VALUE ZEROS.           CE681526
004420                                                                  CE681526
004430 01  MISC-ACCUMS  COMPUTATIONAL-3.                                CE681526
004440     02  CYC-ADJ-ACCUM       PICTURE S9]7[ VALUE ZEROS.           CE681526
004450     02  CYC-ADJ-CT          PICTURE S9]5[ VALUE ZEROS.           CE681526
004460     02  SHORT-ACCUM         PICTURE S9]7[ VALUE ZEROS.           CE681526
004470     02  SHORT-CT            PICTURE S9]5[ VALUE ZEROS.           CE681526
004480     02  USAGE-ACCUM         PICTURE S9]7[ VALUE ZEROS.           CE681526
004490     02  USAGE-CT            PICTURE S9]5[ VALUE ZEROS.           CE681526
004500 01  EDIT-CAT.                                                    CE681526
004510     02  ED-CAT1             PICTURE XX.                          CE681526
004520     02  FILLER              PICTURE X   VALUE ^-^.               CE681526
004530     02  ED-CAT2             PICTURE XXXX.                        CE681526
004540     02  FILLER              PICTURE X   VALUE ^-^.               CE681526
004550     02  ED-CAT3             PICTURE XXXX.                        CE681526
004560     02  FILLER              PICTURE XX  VALUE ^  ^.              CE681526
004570                                                                  CE681526
004580 01  MDATER                  PICTURE X]5[.                        CE681526
004590 01  DATE-AREA.                                                   CE681526
004600     02 FILLER               PICTURE X]13[  VALUE ^00           ^.CE681526
004610     02  MDATE.                                                   CE681526
004620         03  MDALF.                                               CE681526
004630             05  MDATE-YEAR  PICTURE XX.                          CE681526
004640             05  MDATE-DAY   PICTURE XXX.                         CE681526
004650         03  MDRED REDEFINES MDALF   PICTURE S9]5[.               CE681526
004660     02  FILLER              PICTURE X]62[     VALUE SPACES.      CE681526
004670 01  MDNUM       PICTURE S9]5[  COMPUTATIONAL-3  VALUE ZERO.      CE681526
004680 01  W-ACTIVITY-SAVE         PICTURE S9]5[.                       CE681526
004690                                                                  CE681526
004700 01  REPORT-DATE.                                                 CE681526
004710     02  R-MONTH.                                                 CE681526
004720         03  POS-1           PICTURE X.                           CE681526
004730         03  POS-2           PICTURE X.                           CE681526
004740     02  FILLER              PICTURE X.                           CE681526
004750     02  R-DAY               PICTURE XX.                          CE681526
004760      02  FILLER              PICTURE X.                          CE681526
004770     02  R-YEAR              PICTURE XX.                          CE681526
004780                                                                  CE681526
004790                                                                  CE681526
004800                                                                  CE681526
004810 PROCEDURE DIVISION.                                              CE681526
004820                                                                  CE681526
004830 OPEN-INITIALIZE.                                                 CE681526
004840     OPEN   I-O  TRANSFILE                                        CE681526
004850          OUTPUT PRINTFILE  TAPEFILE                              CE681526
004860                 CARDFILE  OTHERFILE.                             CE681526
004870                                                                  CE681526
004880     ENTER LINKAGE.                                               CE681526
004890     CALL ^BMPOPEN^ USING PM-WORK.                                CE681526
004900     ENTER COBOL.                                                 CE681526
004910                                                                  CE681526
004920     ENTER LINKAGE.                                               CE681526
004930     CALL ^MDYDATE^ USING REPORT-DATE.                            CE681526
004940     ENTER COBOL.                                                 CE681526
004950                                                                  CE681526
004960     MOVE REPORT-DATE TO HD-MDY-DATE.                             CE681526
004970                                                                  CE681526
004980     MOVE SPACES TO CYCLE-ADJ-CARD  BUG-AREA  BM-ISS-AREA.        CE681526
004990                                                                  CE681526
005000     MOVE SPACES TO DETAIL-LINE  DET-CAT-TOTAL                    CE681526
005010         SHORT-CARD-OUT.                                          CE681526
005020                                                                  CE681526
005030     PERFORM READ-A-TRANS.                                        CE681526
005040     IF  CARD-CODE NOT EQUAL TO ^00^ OR                           CE681526
005050         MDATE-RED NOT NUMERIC   DISPLAY ^NO DATE^ UPON           CE681526
005060         CONSOLE  GO TO CLOSE-OUT.                                CE681526
005070     MOVE  MDATE-CARD TO MDATE  HD-MDATE                          CE681526
005080     MOVE MDRED TO MDNUM.                                         CE681526
005090                                                                  CE681526
005100 WRITE-DATE.                                                      CE681526
005110     MOVE ZEROS TO EXCEPT-AREA.                                   CE681526
005120     MOVE DATE-AREA TO EX-CARD.                                   CE681526
005130     MOVE EXCEPT-AREA TO OTHER-REC.                               CE681526
005140     PERFORM WRITE-OTHER-REC.                                     CE681526
005150                                                                  CE681526
005160 READ-A-TRANS.                                                    CE681526
005170     READ TRANSFILE  AT END GO TO END-OF-JOB.                     CE681526
005172     IF CARD-CODE ' ^06^ OR                                               
005174        CARD-CODE ' ^29^ OR                                               
005176        CARD-CODE ' ^32^  GO TO READ-A-TRANS.                             
005180 AFTER-READ.                                                      CE681526
005190     IF INITIAL-SWT-ON    MOVE TURN-OFF TO INITIAL-SWT            CE681526
005200         GO TO SET-PREV.                                          CE681526
005210     IF CAT-NO EQUAL TO PREV-CAT                                  CE681526
005220         GO TO EQUAL-CATS.                                        CE681526
005230     IF PROCESSED-SWT-ON  NEXT SENTENCE ELSE GO TO BY-PUT.        CE681526
005240         ENTER LINKAGE.                                           CE681526
005250         CALL ^BMPPUT^  USING PM-WORK.                            CE681526
005260         ENTER COBOL.                                             CE681526
005270         PERFORM DET-TOT-ROUTINE.                                 CE681526
005280 BY-PUT.                                                          CE681526
005290     MOVE TURN-OFF TO PROCESSED-SWT.                              CE681526
005300 SET-PREV.                                                        CE681526
005310     MOVE CAT-NO TO PREV-CAT.                                     CE681526
005320                                                                  CE681526
005330 GET-MASTER.                                                      CE681526
005340     MOVE CAT-NO TO MWA-SYMKEY.                                   CE681526
005350     ENTER LINKAGE.                                               CE681526
005360     CALL ^BMPSTKY^  USING PM-WORK.                               CE681526
005370     ENTER COBOL.                                                 CE681526
005380     MOVE M-CAT1  TO ED-CAT1.                                     CE681526
005390     MOVE M-CAT2  TO ED-CAT2.                                     CE681526
005400     MOVE M-CAT3  TO ED-CAT3.                                     CE681526
005410     MOVE M-ACTIVITY-DATE TO W-ACTIVITY-SAVE.                     CE681526
005420                                                                  CE681526
005430 EQUAL-CATS.                                                      CE681526
005440     IF M-CAT-NO NOT EQUAL TO CAT-NO                              CE681526
005450         MOVE ^01^ TO EX-MSG-CODE                                 CE681526
005460         GO TO   FIND-ERR-CARD.                                   CE681526
005470                                                                  CE681526
005480                                                                  CE681526
005490     IF M-DEL-TG EQUAL TO ^TT^                                    CE681526
005500         MOVE ^02^ TO EX-MSG-CODE                                 CE681526
005510         GO TO  FIND-ERR-CARD.                                    CE681526
005520                                                                  CE681526
005530 CHK-CARD-CODE.                                                   CE681526
005570     IF CARD-CODE EQUAL TO ^26^  GO TO PROCESS-26.                CE681526
005580     IF CARD-CODE EQUAL TO ^31^  GO TO PROCESS-31.                CE681526
005590     IF CARD-CODE EQUAL TO ^33^  GO TO PROCESS-33.                CE681526
005600 BAD-CARD.                                                        CE681526
005610     MOVE ^10^ TO EX-MSG-CODE.                                    CE681526
005620     PERFORM ERROR-ROUTINE.                                       CE681526
005630     GO TO READ-A-TRANS.                                          CE681526
005640                                                                  CE681526
005650 FIND-ERR-CARD.                                                   CE681526
005660     IF CARD-CODE ' ^31^                                          CE681526
005670         PERFORM NON-PROCESS-31                                   CE681526
005680         GO TO READ-A-TRANS.                                      CE681526
005690     IF CARD-CODE ' ^33^                                          CE681526
005700         PERFORM NON-PROCESS-33                                   CE681526
005710         GO TO READ-A-TRANS.                                      CE681526
005720                                                                  CE681526
005730     IF CARD-CODE ' ^26^  NEXT SENTENCE                           CE681526
005740         ELSE GO TO BAD-CARD.                                     CE681526
005750     IF T26-ISS-QTY NUMERIC                                       CE681526
005752         MULTIPLY -1 BY T26-ISS-QTY                               09127226
005760         ADD T26-ISS-QTY TO NP-I26-ON-HAND                        CE681526
005770         ADD T26-ISS-QTY TO NP-I26-PL-USG                         CE681526
005772         MULTIPLY -1 BY T26-ISS-QTY.                              09127226
005780     ADD 1 TO NP-I26-REC-CT.                                      CE681526
005790     MOVE EX-MSG-CODE TO BUG-MSG-CODE                             CE681526
005800     PERFORM ERROR-ROUTINE.                                       CE681526
005810     PERFORM BUG-CARD-ROUTINE.                                    CE681526
005820     GO TO READ-A-TRANS.                                          CE681526
005830                                                                  CE681526
005840                                                                  CE681526
005850 PROCESS-26.                                                      CE681526
005860     IF T26-ISS-QTY NOT NUMERIC  ADD 1 TO NP-I26-REC-CT           CE681526
005870         MOVE ^20^ TO EX-MSG-CODE  BUG-MSG-CODE                   CE681526
005880         PERFORM ERROR-ROUTINE                                    CE681526
005890         PERFORM BUG-CARD-ROUTINE                                 CE681526
005900             GO TO READ-A-TRANS.                                  CE681526
005910     MOVE TRAN-26 TO BM-CARD.                                     CE681526
005920     MOVE ^3^ TO BM-REC-CODE.                                     CE681526
005930     MOVE     CAT-NO TO BM-CAT-NO.                                CE681526
005940     MOVE BM-ISS-AREA TO OTHER-REC.                               CE681526
005950     PERFORM WRITE-OTHER-REC.                                     CE681526
005980                                                                  CE681526
005990     SUBTRACT T26-ISS-QTY FROM M-ON-HAND-QTY.                     CE681526
006000     ADD T26-ISS-QTY TO M-PLAN-QTY.                               CE681526
006010     ADD T26-ISS-QTY TO PR-I26-PL-USG.                            CE681526
006020     IF  T26-REQ-QTY IS GREATER THAN T26-ISS-QTY                  CE681526
006030         SUBTRACT T26-ISS-QTY FROM T26-REQ-QTY GIVING SHORT-QTY   CE681526
006040         PERFORM SHORT-CARD-ROUTINE.                              CE681526
006050                                                                  CE681526
006060     IF T26-REQ-QTY IS LESS THAN T26-ISS-QTY                      CE681526
006070         SUBTRACT T26-REQ-QTY FROM M-ALLOC-QTY                    CE681526
006080         MULTIPLY -1 BY T26-REQ-QTY                               CE681526
006090         MOVE T26-REQ-QTY TO D-ALLOC                              CE681526
006100         ADD T26-REQ-QTY TO PR-I26-ALLOC                          CE681526
006110         MULTIPLY -1 BY T26-REQ-QTY                               CE681526
006120         MULTIPLY -1 BY T26-ISS-QTY                               CE681526
006130             GO TO FORM-ISS.                                      CE681526
006140     SUBTRACT T26-ISS-QTY FROM M-ALLOC-QTY.                       CE681526
006150     MULTIPLY -1 BY T26-ISS-QTY.                                  CE681526
006160     ADD T26-ISS-QTY TO PR-I26-ALLOC.                             CE681526
006170     MOVE T26-ISS-QTY TO D-ALLOC.                                 CE681526
006180                                                                  CE681526
006190 FORM-ISS.                                                        CE681526
006200     MOVE T26-ISS-QTY TO D-ON-HAND.                               CE681526
006210     ADD  T26-ISS-QTY TO PR-I26-ON-HAND.                          CE681526
006220     ADD 1 TO PR-I26-REC-CT.                                      CE681526
006230     MULTIPLY -1 BY T26-ISS-QTY.                                  CE681526
006240     MOVE CARD-CODE TO D-CARD.                                    CE681526
006250     MOVE EDIT-CAT TO D-CAT.                                      CE681526
006260     MOVE T26-WO TO D-CHRG-NO.                                    CE681526
006270     MOVE W-ACTIVITY-SAVE TO D-LAST-ACT.                          CE681526
006280     MOVE MDNUM TO M-ACTIVITY-DATE.                               CE681526
006290     PERFORM PRINT-REPORT.                                        CE681526
006300     PERFORM REWRITE-TRANS.                                       CE681526
006310     GO TO READ-A-TRANS.                                          CE681526
006320                                                                  CE681526
006330                                                                  CE681526
006340                                                                  CE681526
006350                                                                  CE681526
006360 PROCESS-31.                                                      CE681526
006370     IF T31-UNPL NOT EQUAL TO ^1^  AND                            CE681526
006380        T31-UNPL NOT EQUAL TO ^ ^    MOVE ^22^ TO EX-MSG-CODE     CE681526
006390         PERFORM NON-PROCESS-31                                   CE681526
006400         GO TO READ-A-TRANS.                                      CE681526
006410     IF T31-CODE EQUAL TO ^0^ OR                                  CE681526
006420        T31-CODE EQUAL TO ^1^ OR                                  CE681526
006430        T31-CODE EQUAL TO ^7^ OR                                  CE681526
006440        T31-CODE EQUAL TO ^9^    NEXT SENTENCE  ELSE              CE681526
006450         MOVE ^23^ TO EX-MSG-CODE  PERFORM NON-PROCESS-31         CE681526
006460             GO TO READ-A-TRANS.                                  CE681526
006470     IF T31-REQ-QTY NOT NUMERIC OR                                CE681526
006480        T31-ISS-QTY NOT NUMERIC    MOVE ^20^ TO EX-MSG-CODE       CE681526
006490         PERFORM NON-PROCESS-31                                   CE681526
006500         GO TO READ-A-TRANS.                                      CE681526
006510     IF T31-REQ-QTY ' ZERO  OR                                    CE681526
006520         T31-ISS-QTY ' ZERO  GO TO CHK-CODE-31.                   CE681526
006530     IF T31-REQ-QTY POSITIVE  AND                                 CE681526
006540        T31-ISS-QTY POSITIVE   OR                                 CE681526
006550        T31-REQ-QTY NEGATIVE  AND                                 CE681526
006560        T31-ISS-QTY NEGATIVE                                      CE681526
006570         NEXT SENTENCE   ELSE      MOVE ^21^ TO EX-MSG-CODE       CE681526
006580             PERFORM NON-PROCESS-31                               CE681526
006590             GO TO READ-A-TRANS.                                  CE681526
006600     IF T31-ISS-QTY NEGATIVE  AND T31-ISS-QTY # T31-REQ-QTY       CE681526
006610         MOVE ^27^ TO EX-MSG-CODE  PERFORM NON-PROCESS-31         CE681526
006620         GO TO READ-A-TRANS.                                      CE681526
006630                                                                  CE681526
006640 CHK-CODE-31.                                                     CE681526
006650     IF T31-CODE EQUAL TO ^1^  AND                                CE681526
006660        T31-CHRG EQUAL TO SPACES   MOVE ^24^ TO EX-MSG-CODE       CE681526
006670         PERFORM NON-PROCESS-31                                   CE681526
006680         GO TO READ-A-TRANS.                                      CE681526
006690     MOVE TURN-OFF TO SHORT-SWT.                                  CE681526
006700     MOVE ZERO TO SHORT-QTY.                                      CE681526
006710     IF T31-ISS-QTY IS LESS THAN T31-REQ-QTY                      CE681526
006720         MOVE TURN-ON TO SHORT-SWT.                               CE681526
006730                                                                  CE681526
006740     IF T31-UNPL EQUAL TO ^1^                                     CE681526
006750         GO TO UNPLANNED-31.                                      CE681526
006760     IF T31-CODE EQUAL TO ^0^                                     CE681526
006770         NEXT SENTENCE  ELSE                                      CE681526
006780         GO TO ADD-TO-PLANNED.                                    CE681526
006790     IF POS-123 OF T31-CHRG EQUAL TO ^028^ OR                     CE681526
006800         POS-123 OF T31-CHRG ' ^099^  MOVE ^25^ TO EX-MSG-CODE    CE681526
006810         PERFORM NON-PROCESS-31                                   CE681526
006820         GO TO READ-A-TRANS.                                      CE681526
006840                                                                  CE681526
006850 ADD-TO-PLANNED.                                                  CE681526
006860     ADD T31-ISS-QTY TO M-PLAN-QTY.                               CE681526
006870     ADD T31-ISS-QTY TO PR-I31-PL-USG.                            CE681526
006890 TEST-SHORT-SWT.                                                  CE681526
006900     IF SHORT-SWT-ON                                              CE681526
006910         SUBTRACT T31-ISS-QTY FROM T31-REQ-QTY GIVING SHORT-QTY   CE681526
006920         PERFORM SHORT-CARD-ROUTINE                               CE681526
006930         ADD SHORT-QTY TO M-ALLOC-QTY                             CE681526
006940         ADD SHORT-QTY TO PR-I31-ALLOC.                           CE681526
006950     GO TO PRE-PRINT-31.                                          CE681526
006960                                                                  CE681526
006970 UNPLANNED-31.                                                    CE681526
006980     ADD T31-ISS-QTY TO PR-I31-UNPL.                              CE681526
006990     IF T31-CODE EQUAL TO ^9^                                     CE681526
007000         NEXT SENTENCE ELSE GO TO CHK-CODE-7.                     CE681526
007010     IF T31-ISS-QTY IS POSITIVE                                   CE681526
007020         ADD T31-ISS-QTY TO M-S-O-QTY.                            CE681526
007040     GO TO TEST-SHORT-SWT.                                        CE681526
007050                                                                  CE681526
007060 CHK-CODE-7.                                                      CE681526
007070     IF T31-CODE EQUAL TO ^7^ OR                                  CE681526
007080        T31-CODE EQUAL TO ^1^  OR                                 CE681526
007090         T31-CHRG ' ^09921   ^  OR T31-CHRG ' ^09975   ^          CE681526
007100         NEXT SENTENCE  ELSE GO TO CODE-0.                        CE681526
007110     IF T31-ISS-QTY IS POSITIVE                                   CE681526
007120         ADD T31-ISS-QTY TO M-DPT-PROJ-QTY.                       CE681526
007130     GO TO TEST-SHORT-SWT.                                        CE681526
007140                                                                  CE681526
007150 CODE-0.                                                          CE681526
007160     IF T31-CHRG IS EQUAL TO ^09954   ^  OR                       CE681526
007170        T31-CHRG IS EQUAL TO ^09955   ^  OR                       CE681526
007180        T31-CHRG IS EQUAL TO ^09956   ^  OR                       CE681526
007190         T31-CHRG IS EQUAL TO ^09957   ^ OR                       CE681526
007200        T31-CHRG IS EQUAL TO ^09958   ^  OR                       CE681526
007210        T31-CHRG IS EQUAL TO ^09959   ^  OR                       CE681526
007220        T31-CHRG IS EQUAL TO ^09960   ^  OR                       CE681526
007230        T31-CHRG IS EQUAL TO ^09967   ^  OR                       CE681526
007240        T31-CHRG IS EQUAL TO ^09973   ^  OR                       CE681526
007250        T31-CHRG IS EQUAL TO ^09978   ^                           CE681526
007270         GO TO PRE-PRINT-31.                                      CE681526
007280     IF POS-123 OF T31-CHRG NOT EQUAL TO ^028^                    CE681526
007290         GO TO TRY-9911.                                          CE681526
007300     IF T31-ISS-QTY IS POSITIVE                                   CE681526
007310         ADD T31-ISS-QTY TO M-WARRANTY-QTY.                       CE681526
007330     GO TO TEST-SHORT-SWT.                                        CE681526
007340                                                                  CE681526
007350 TRY-9911.                                                        CE681526
007360     IF T31-CHRG NOT EQUAL TO ^09911   ^ AND                      CE681526
007370        T31-CHRG NOT EQUAL TO ^09912   ^ AND                      CE681526
007380        T31-CHRG NOT EQUAL TO ^09914   ^ AND                      CE681526
007390        T31-CHRG NOT EQUAL TO ^09915   ^ AND                      CE681526
007400        T31-CHRG NOT EQUAL TO ^09916   ^ AND                      CE681526
007410        T31-CHRG NOT EQUAL TO ^09918   ^ AND                      CE681526
007420        T31-CHRG NOT EQUAL TO ^09961   ^ AND                      CE681526
007430        T31-CHRG NOT EQUAL TO ^09963   ^ AND                      CE681526
007440        T31-CHRG NOT EQUAL TO ^09971   ^                          CE681526
007450         GO TO TRY-9966.                                          CE681526
007460     IF T31-ISS-QTY IS POSITIVE                                   CE681526
007470         ADD T31-ISS-QTY TO M-W-O-QTY.                            CE681526
007490     GO TO PRE-PRINT-31.                                          CE681526
007500                                                                  CE681526
007510 TRY-9966.                                                        CE681526
007520     IF T31-CHRG NOT EQUAL TO ^09966   ^                          CE681526
007530         GO TO TRY-9968.                                          CE681526
007540     IF T31-ISS-QTY IS POSITIVE                                   CE681526
007550         ADD T31-ISS-QTY TO M-WARRANTY-QTY.                       CE681526
007560     PERFORM MEMO-STK-ROUTINE.                                    CE681526
007580     GO TO PRE-PRINT-31.                                          CE681526
007590 TRY-9968.                                                        CE681526
007600     IF T31-CHRG NOT EQUAL TO ^09968   ^ AND                      CE681526
007610        T31-CHRG NOT EQUAL TO ^09983   ^                          CE681526
007620         GO TO TRY-9962.                                          CE681526
007640     IF T31-CHRG EQUAL TO ^09983   ^                              CE681526
007650         PERFORM  ADJ-CARD-ROUTINE.                               CE681526
007660     GO TO PRE-PRINT-31.                                          CE681526
007670                                                                  CE681526
007680 TRY-9962.                                                        CE681526
007690     IF T31-CHRG EQUAL TO ^09962   ^                              CE681526
007700         ADD  T31-ISS-QTY TO M-RE-EVALU-QTY                       CE681526
007710         ADD  T31-ISS-QTY TO PR-I31-REVAL                         CE681526
007720         MOVE T31-ISS-QTY TO D-RE-VAL                             CE681526
007740         GO TO PRE-PRINT-31.                                      CE681526
007750                                                                  CE681526
007760     IF POS-123 OF T31-CHRG EQUAL TO ^099^                        CE681526
007770         MOVE ^26^ TO EX-MSG-CODE                                 CE681526
007780         PERFORM NON-PROCESS-31                                   CE681526
007790         SUBTRACT T31-ISS-QTY FROM PR-I31-UNPL                    CE681526
007800         GO TO READ-A-TRANS.                                      CE681526
007810                                                                  CE681526
007820 OTHER-31.                                                        CE681526
007830     IF T31-ISS-QTY POSITIVE                                      CE681526
007840         ADD T31-ISS-QTY TO M-W-O-QTY.                            CE681526
007860     GO TO TEST-SHORT-SWT.                                        CE681526
007870                                                                  CE681526
007880                                                                  CE681526
007890 PRE-PRINT-31.                                                    CE681526
007900     ADD 1 TO PR-I31-REC-CT.                                      CE681526
007910     SUBTRACT T31-ISS-QTY FROM M-ON-HAND-QTY.                     CE681526
007920     MULTIPLY T31-ISS-QTY BY -1 GIVING REVERSE-ISS-QTY.           CE681526
007930     ADD REVERSE-ISS-QTY TO PR-I31-ON-HAND.                       CE681526
007940                                                                  CE681526
007950                                                                  CE681526
007960     MOVE CARD-CODE TO D-CARD.                                    CE681526
007970     MOVE EDIT-CAT TO D-CAT.                                      CE681526
007980     IF T31-CODE EQUAL TO ^7^                                     CE681526
007990         MOVE T31-CDS TO D-CHRG-NO   ELSE                         CE681526
008000         MOVE T31-CHRG TO D-CHRG-NO.                              CE681526
008010     MOVE T31-CODE TO D-CHRG-SO-1.                                CE681526
008020     MOVE T31-REQ TO D-CHRG-SO-2.                                 CE681526
008030     MOVE W-ACTIVITY-SAVE TO D-LAST-ACT.                          CE681526
008040     MOVE MDNUM TO M-ACTIVITY-DATE.                               CE681526
008050                                                                  CE681526
008060     MOVE REVERSE-ISS-QTY TO D-ON-HAND.                           CE681526
008070     MOVE SHORT-QTY TO D-ALLOC.                                   CE681526
008080     PERFORM PRINT-REPORT.                                        CE681526
008090     PERFORM REWRITE-TRANS.                                       CE681526
008100     GO TO READ-A-TRANS.                                          CE681526
008110                                                                  CE681526
008120 NON-PROCESS-31.                                                  CE681526
008130     ADD 1 TO NP-I31-REC-CT.                                      CE681526
008140     PERFORM ERROR-ROUTINE.                                       CE681526
008150     IF T31-REQ-QTY  NOT NUMERIC                                  CE681526
008160         MOVE ZEROS TO T31-REQ-QTY.                               CE681526
008170     IF T31-ISS-QTY  NOT NUMERIC                                  CE681526
008180         MOVE ZEROS TO T31-ISS-QTY.                               CE681526
008190     IF T31-ISS-QTY ) T31-REQ-QTY                                 CE681526
008200         SUBTRACT T31-ISS-QTY FROM T31-REQ-QTY GIVING SHORT-QTY   CE681526
008210         ADD SHORT-QTY TO NP-I31-ALLOC.                           CE681526
008220     MULTIPLY T31-ISS-QTY BY -1  GIVING REVERSE-ISS-QTY.          CE681526
008230     ADD REVERSE-ISS-QTY TO NP-I31-ON-HAND.                       CE681526
008240     IF T31-UNPL ' ^1^                                            CE681526
008250         ADD T31-ISS-QTY TO NP-I31-UNPL  ELSE                     CE681526
008260         ADD T31-ISS-QTY TO NP-I31-PL-USG.                        CE681526
008270     IF T31-UNPL ' ^1^   AND                                      CE681526
008280         T31-CHRG ' ^09962   ^                                    CE681526
008290         ADD T31-ISS-QTY TO NP-I31-REVAL.                         CE681526
008300                                                                  CE681526
008310                                                                  CE681526
008320                                                                  CE681526
008330 PROCESS-33.                                                      CE681526
008340                                                                  CE681526
008350     IF T33-REM-QTY NOT NUMERIC  OR                               CE681526
008360        T33-ISS-QTY NOT NUMERIC   MOVE ^20^ TO EX-MSG-CODE        CE681526
008370         PERFORM NON-PROCESS-33                                   CE681526
008380         GO TO READ-A-TRANS.                                      CE681526
008390     IF T33-ISS-QTY ' ZERO OR                                     CE681526
008400         T33-REM-QTY ' ZERO  GO TO CHK-CODE-33.                   CE681526
008410     IF  T33-ISS-QTY POSITIVE  AND                                CE681526
008420         T33-REM-QTY POSITIVE   OR                                CE681526
008430         T33-ISS-QTY NEGATIVE  AND                                CE681526
008440         T33-REM-QTY NEGATIVE                                     CE681526
008450         NEXT SENTENCE ELSE   MOVE ^21^ TO EX-MSG-CODE            CE681526
008460             PERFORM NON-PROCESS-33                               CE681526
008470             GO TO READ-A-TRANS.                                  CE681526
008480                                                                  CE681526
008490     IF T33-ISS-QTY NEGATIVE  AND                                 CE681526
008500         T33-ISS-QTY # T33-REM-QTY                                CE681526
008510         MOVE ^28^ TO EX-MSG-CODE   PERFORM NON-PROCESS-33        CE681526
008520         GO TO READ-A-TRANS.                                      CE681526
008530                                                                  CE681526
008540                                                                  CE681526
008550 CHK-CODE-33.                                                     CE681526
008560     IF T33-CODE NOT EQUAL TO ^0^ AND                             CE681526
008570        T33-CODE NOT EQUAL TO ^1^ AND                             CE681526
008580        T33-CODE NOT EQUAL TO ^7^ AND                             CE681526
008590        T33-CODE NOT EQUAL TO ^9^   MOVE ^23^ TO EX-MSG-CODE      CE681526
008600             PERFORM NON-PROCESS-33                               CE681526
008610             GO TO READ-A-TRANS.                                  CE681526
008620                                                                  CE681526
008630     IF T33-UNPL NOT EQUAL TO ^1^ AND                             CE681526
008640        T33-UNPL NOT EQUAL TO ^ ^ MOVE ^22^ TO EX-MSG-CODE        CE681526
008650             PERFORM NON-PROCESS-33                               CE681526
008660             GO TO READ-A-TRANS.                                  CE681526
008670     MOVE TURN-OFF TO SHORT-SWT.                                  CE681526
008680     SUBTRACT T33-ISS-QTY FROM M-ON-HAND-QTY.                     CE681526
008690     IF  T33-ISS-QTY IS GREATER THAN T33-REM-QTY                  CE681526
008700         MULTIPLY T33-REM-QTY BY -1 GIVING REVERSE-ALLOC          CE681526
008710         SUBTRACT T33-REM-QTY FROM M-ALLOC-QTY  ELSE              CE681526
008720         SUBTRACT T33-ISS-QTY FROM M-ALLOC-QTY                    CE681526
008730         MULTIPLY T33-ISS-QTY BY -1 GIVING REVERSE-ALLOC.         CE681526
008740                                                                  CE681526
008750     IF T33-REM-QTY    IS GREATER THAN  T33-ISS-QTY               CE681526
008760         NEXT SENTENCE  ELSE  GO TO  TEST-PLAN-33.                CE681526
008770                                                                  CE681526
008780     SUBTRACT  T33-ISS-QTY FROM  T33-REM-QTY GIVING  SHORT-QTY.   CE681526
008790     IF SHORT-QTY IS POSITIVE                                     CE681526
008800         MOVE TURN-ON TO SHORT-SWT.                               CE681526
008810                                                                  CE681526
008820 TEST-PLAN-33.                                                    CE681526
008830     IF T33-UNPL EQUAL TO ^1^                                     CE681526
008840         GO TO UNPLANNED-33.                                      CE681526
008850                                                                  CE681526
008860     ADD T33-ISS-QTY TO M-PLAN-QTY.                               CE681526
008870     ADD T33-ISS-QTY TO PR-I33-PL-USG.                            CE681526
008910     GO TO TEST-SHORT-33.                                         CE681526
008920                                                                  CE681526
008930 UNPLANNED-33.                                                    CE681526
008940     ADD T33-ISS-QTY TO PR-I33-UNPL.                              CE681526
008950     IF T33-CODE NOT EQUAL TO ^9^                                 CE681526
008960         GO TO TRY-CODE-7.                                        CE681526
008970                                                                  CE681526
008980     IF T33-ISS-QTY IS POSITIVE                                   CE681526
008990         ADD T33-ISS-QTY TO M-S-O-QTY.                            CE681526
009000                                                                  CE681526
009020     GO TO TEST-SHORT-33.                                         CE681526
009030                                                                  CE681526
009040 TRY-CODE-7.                                                      CE681526
009050     IF T33-CODE NOT EQUAL TO ^1^ AND                             CE681526
009060        T33-CODE NOT EQUAL TO ^7^                                 CE681526
009070         GO TO TRY-028.                                           CE681526
009080     IF T33-ISS-QTY IS POSITIVE                                   CE681526
009090         ADD T33-ISS-QTY TO M-DPT-PROJ-QTY.                       CE681526
009100                                                                  CE681526
009110     GO TO TEST-SHORT-33.                                         CE681526
009120                                                                  CE681526
009130 TRY-028.                                                         CE681526
009140     IF POS-123 OF T33-CHRG EQUAL TO ^028^                        CE681526
009150         NEXT SENTENCE   ELSE  GO TO OTHER-33.                    CE681526
009160     IF T33-ISS-QTY IS POSITIVE                                   CE681526
009170         ADD T33-ISS-QTY TO M-WARRANTY-QTY.                       CE681526
009190     GO TO TEST-SHORT-33.                                         CE681526
009200                                                                  CE681526
009210 OTHER-33.                                                        CE681526
009220     IF T33-ISS-QTY POSITIVE                                      CE681526
009230         ADD T33-ISS-QTY TO M-W-O-QTY.                            CE681526
009250                                                                  CE681526
009260                                                                  CE681526
009270 TEST-SHORT-33.                                                   CE681526
009280     IF SHORT-SWT-ON                                              CE681526
009290         PERFORM SHORT-CARD-ROUTINE.                              CE681526
009300                                                                  CE681526
009310                                                                  CE681526
009320     ADD 1 TO PR-I33-REC-CT.                                      CE681526
009330     MOVE CARD-CODE TO D-CARD.                                    CE681526
009340     MOVE EDIT-CAT TO D-CAT.                                      CE681526
009350     IF T33-CODE EQUAL TO ^7^                                     CE681526
009360         MOVE T33-CDS TO D-CHRG-NO  ELSE                          CE681526
009370         MOVE T33-CHRG TO D-CHRG-NO.                              CE681526
009380     MOVE T33-CODE TO D-CHRG-SO-1.                                CE681526
009390     MOVE T33-REQ TO D-CHRG-SO-2.                                 CE681526
009400     MOVE W-ACTIVITY-SAVE TO D-LAST-ACT.                          CE681526
009410     MOVE MDNUM TO M-ACTIVITY-DATE.                               CE681526
009420     MULTIPLY T33-ISS-QTY BY -1 GIVING REVERSE-ISS-QTY.           CE681526
009430     MOVE REVERSE-ISS-QTY TO D-ON-HAND.                           CE681526
009440     ADD REVERSE-ISS-QTY TO PR-I33-ON-HAND.                       CE681526
009450     MOVE REVERSE-ALLOC TO D-ALLOC.                               CE681526
009460     ADD REVERSE-ALLOC TO PR-I33-ALLOC.                           CE681526
009470                                                                  CE681526
009480     PERFORM PRINT-REPORT.                                        CE681526
009490     PERFORM REWRITE-TRANS.                                       CE681526
009500     GO TO READ-A-TRANS.                                          CE681526
009510                                                                  CE681526
009520 NON-PROCESS-33.                                                  CE681526
009530     ADD 1  TO NP-I33-REC-CT.                                     CE681526
009540     PERFORM ERROR-ROUTINE.                                       CE681526
009550     IF T33-ISS-QTY NOT NUMERIC                                   CE681526
009560         MOVE ZEROS TO T33-ISS-QTY.                               CE681526
009570     IF T33-REM-QTY NOT NUMERIC                                   CE681526
009580         MOVE ZEROS TO T33-ISS-QTY.                               CE681526
009590     MULTIPLY T33-ISS-QTY BY -1 GIVING REVERSE-ISS-QTY.           CE681526
009600     ADD REVERSE-ISS-QTY TO NP-I33-ON-HAND.                       CE681526
009610                                                                  CE681526
009620     IF T33-REM-QTY # T33-ISS-QTY                                 CE681526
009630         MULTIPLY T33-REM-QTY BY -1 GIVING REVERSE-ALLOC          CE681526
009640         ELSE                                                     CE681526
009650         MULTIPLY T33-ISS-QTY BY -1 GIVING REVERSE-ALLOC.         CE681526
009660     ADD REVERSE-ALLOC TO NP-I33-ALLOC.                           CE681526
009670                                                                  CE681526
009680     IF T33-UNPL ' ^1^                                            CE681526
009690         ADD T33-ISS-QTY TO NP-I33-UNPL  ELSE                     CE681526
009700         ADD T33-ISS-QTY TO NP-I33-PL-USG.                        CE681526
009710                                                                  CE681526
009720                                                                  CE681526
009730 ERROR-ROUTINE.                                                   CE681526
009740     MOVE CAT-NO TO EX-CAT-NO.                                    CE681526
009750     MOVE ^1^ TO EX-REC-CODE.                                     CE681526
009760     MOVE TRAN-26 TO EX-CARD.                                     CE681526
009770     MOVE EXCEPT-AREA TO OTHER-REC.                               CE681526
009780     PERFORM WRITE-OTHER-REC.                                     CE681526
009790                                                                  CE681526
009800                                                                  CE681526
009810 BUG-CARD-ROUTINE.                                                CE681526
009820     MOVE ^2^ TO BUG-REC-CODE.                                    CE681526
009830     MOVE BUG-AREA TO OTHER-REC.                                  CE681526
009840     PERFORM WRITE-OTHER-REC.                                     CE681526
009850     MOVE SPACES TO BUG-AREA.                                     CE681526
009860                                                                  CE681526
009870 ADJ-CARD-ROUTINE.                                                CE681526
009880     MOVE CAT-NO TO  OTHER-CAT-NO  CA-CAT-NO.                     CE681526
009890     MOVE ^4^ TO OTHER-REC-CODE.                                  CE681526
009900     MOVE ^35^ TO CA-CARD-CD.                                     CE681526
009910     MOVE ^35^ TO OTHER-MSG-CODE.                                 CE681526
009920     MOVE CHK-DIGIT TO CA-CHK-DIG.                                CE681526
009930     MOVE M-LEDGER-CD TO CA-LEDGER.                               CE681526
009940     MOVE T31-REQ TO CA-SOURCE.                                   CE681526
009950     MOVE T31-RC TO CA-RC.                                        CE681526
009960     MOVE M-UNIT-COST TO CA-U-COST.                               CE681526
009970     MULTIPLY T31-ISS-QTY BY M-UNIT-COST GIVING EXT-VALUE.        CE681526
009980     MOVE EXT-VALUE TO CA-EXT-VAL.                                CE681526
009990     MOVE MDATE TO CA-DATE.                                       CE681526
010000     MOVE T31-ISS-QTY TO CA-ADJ-QTY.                              CE681526
010010                                                                  CE681526
010020     MOVE CYCLE-ADJ-CARD TO OTHER-REC.                            CE681526
010030     PERFORM WRITE-OTHER-REC.                                     CE681526
010040                                                                  CE681526
010050     ADD  1 TO CYC-ADJ-CT.                                        CE681526
010060     ADD  T31-ISS-QTY TO CYC-ADJ-ACCUM.                           CE681526
010070     MOVE SPACES TO CYCLE-ADJ-CARD.                               CE681526
010080                                                                  CE681526
010340                                                                  CE681526
011000                                                                  CE681526
011010 SHORT-CARD-ROUTINE.                                              CE681526
011020     MOVE SPACES TO SHORT-CARD-OUT.                               CE681526
011030     MOVE ^33^ TO SHORT-CODE.                                     CE681526
011040     MOVE CAT-NO TO SHORT-CAT-NO.                                 CE681526
011050     MOVE CHK-DIGIT TO SHORT-CHK-DIG.                             CE681526
011060     MOVE SPACES TO SHORT-GROUP.                                  CE681526
011062      IF M-MAKE-PURCH ' ^M^                                       CE681526
011063        MOVE M-GROUP-CD TO SHORT-GROUP                            CE681526
011064        ELSE MOVE M-BUYER-CD TO SHORT-GROUP-2.                    CE681526
011070     MOVE M-UNIT-MEAS TO SHORT-U-MEAS.                            CE681526
011080     MOVE SHORT-QTY TO SHORT-REMAIN.                              CE681526
011090     ADD SHORT-QTY TO SHORT-ACCUM.                                CE681526
011100     IF CARD-CODE NOT EQUAL TO ^26^ NEXT SENTENCE ELSE            CE681526
011110         MOVE T26-ISS-QTY TO SHORT-ISS-TO-DA                      CE681526
011120         MOVE T26-REQ-QTY TO SHORT-REQUIRED                       CE681526
011130         MOVE T26-WO TO SHORT-CHRG-NO                             CE681526
011140         MOVE ^0^ TO SHORT-CHRG-CODE                              CE681526
011150         MOVE ^666666^ TO SHORT-REQ-NO                            CE681526
011160         MOVE ^20^ TO SHORT-RC                                    CE681526
011170         ADD   SHORT-QTY  TO SHORT-ACCUM.                         CE681526
011180                                                                  CE681526
011190     IF CARD-CODE NOT EQUAL TO ^31^ NEXT SENTENCE ELSE            CE681526
011200         MOVE T31-ISS-QTY TO SHORT-ISS-TO-DA                      CE681526
011210         MOVE T31-REQ-QTY TO SHORT-REQUIRED                       CE681526
011220         MOVE T31-UNPL TO SHORT-UNPL                              CE681526
011230         MOVE T31-RC TO SHORT-RC                                  CE681526
011240         MOVE T31-REQ TO SHORT-REQ-NO                             CE681526
011250         MOVE T31-CHRG-NO TO SHORT-CHARGE                         CE681526
011250         MOVE T31-U-COMM TO SHORT-U-COMM                                  
011260         ADD    SHORT-QTY TO SHORT-ACCUM.                         CE681526
011270                                                                  CE681526
011280     IF CARD-CODE NOT EQUAL TO ^33^ NEXT SENTENCE  ELSE           CE681526
011290         MOVE T33-CHRG-NO TO SHORT-CHARGE                         CE681526
011300         MOVE T33-REQ  TO SHORT-REQ-NO                            CE681526
011310         MOVE T33-RC TO SHORT-RC                                  CE681526
011320         MOVE T33-UNPL TO SHORT-UNPL                              CE681526
011330         MOVE T33-REQ-QTY TO SHORT-REQUIRED                       CE681526
011340         MOVE T33-ISS-TO-DATE TO SHORT-ISS-TO-DA                  CE681526
011350         ADD T33-ISS-QTY TO SHORT-ISS-TO-DA                       CE681526
011352         MOVE T33-U-COMM TO SHORT-U-COMM                                  
011360         ADD    SHORT-QTY TO SHORT-ACCUM.                         CE681526
011380     MOVE SHORT-CARD-OUT TO CARD-REC.                             CE681526
011390     MOVE ^V^ TO STACKER-SELECT.                                  CE681526
011400     PERFORM PUNCH-CARD.                                          CE681526
011410                                                                  CE681526
011420     ADD 1 TO SHORT-CT.                                           CE681526
011430                                                                  CE681526
011440                                                                  CE681526
011450 DET-TOT-ROUTINE.                                                 CE681526
011460     MOVE ^ ^ TO CARRIAGE-CONTROL.                                CE681526
011470     MOVE M-ON-HAND-QTY TO DT-ON-HAND.                            CE681526
011480     MOVE M-ALLOC-QTY TO DT-ALLOC.                                CE681526
011490     MOVE M-ON-ORDER-QTY TO DT-ON-ORDER.                          CE681526
011500     MOVE M-IN-TRANS-QTY TO DT-IN-TRANS.                          CE681526
011510     MOVE M-OUT-PROC-QTY TO DT-OUT-PROC.                          CE681526
011520     MOVE M-RE-EVALU-QTY TO DT-RE-VAL.                            CE681526
011530     MOVE ^--------INV BAL--------^ TO DT-MSG-A.                  CE681526
011540     IF M-ON-HAND-QTY NEGATIVE OR                                 CE681526
011550        M-ALLOC-QTY NEGATIVE OR                                   CE681526
011560        M-ON-ORDER-QTY NEGATIVE OR                                CE681526
011570        M-IN-TRANS-QTY NEGATIVE OR                                CE681526
011580        M-OUT-PROC-QTY NEGATIVE OR                                CE681526
011590        M-RE-EVALU-QTY NEGATIVE                                   CE681526
011600         MOVE ^CR BAL^ TO DT-MSG  ELSE                            CE681526
011610             MOVE SPACES TO DT-MSG.                               CE681526
011620     MOVE  DET-CAT-TOTAL TO PRINT-REC-X.                          CE681526
011630     PERFORM PRINT-A-LINE.                                        CE681526
011640     ADD 2 TO LINE-CT.                                            CE681526
011650     PERFORM PRINT-A-LINE.                                        CE681526
011660     MOVE SPACES TO PRINT-REC  DETAIL-LINE.                       CE681526
011670                                                                  CE681526
011680                                                                  CE681526
011690 PRINT-REPORT.                                                    CE681526
011700     IF FIRST-TIME-PRINT-SWT-ON                                   CE681526
011710         MOVE TURN-OFF TO FIRST-TIME-PRINT-SWT                    CE681526
011720         PERFORM HEADING-ROUTINE  THRU  HEAD-3-4.                 CE681526
011730                                                                  CE681526
011740     IF LINE-CT # 50                                              CE681526
011750         MOVE ZERO TO LINE-CT                                     CE681526
011760         PERFORM HEADING-ROUTINE  THRU  HEAD-3-4.                 CE681526
011770     ADD 1 TO LINE-CT.                                            CE681526
011780     MOVE DETAIL-LINE TO PRINT-REC-X.                             CE681526
011790     MOVE ^ ^ TO CARRIAGE-CONTROL.                                CE681526
011800     PERFORM PRINT-A-LINE.                                        CE681526
011810     MOVE SPACES TO DETAIL-LINE.                                  CE681526
011820                                                                  CE681526
011830 HEADING-ROUTINE.                                                 CE681526
011840     ADD 1 TO PAGE-NO.                                            CE681526
011850     MOVE PAGE-NO TO HD-PAGE.                                     CE681526
011860     MOVE ^1^ TO CARRIAGE-CONTROL.                                CE681526
011870     MOVE HD-LINE-1 TO PRINT-REC.                                 CE681526
011880     PERFORM PRINT-A-LINE.                                        CE681526
011890     MOVE HD-LINE-2 TO PRINT-REC.                                 CE681526
011900     MOVE ^0^ TO CARRIAGE-CONTROL.                                CE681526
011910     PERFORM PRINT-A-LINE.                                        CE681526
011920 HEAD-3-4.                                                        CE681526
011930     MOVE HD-LINE-3 TO PRINT-REC-X.                               CE681526
011940     PERFORM PRINT-A-LINE.                                        CE681526
011950     MOVE HD-LINE-4 TO PRINT-REC-X.                               CE681526
011960     MOVE ^ ^ TO CARRIAGE-CONTROL.                                CE681526
011970     PERFORM PRINT-A-LINE.                                        CE681526
011980     ADD 6 TO LINE-CT.                                            CE681526
011990     PERFORM PRINT-A-LINE.                                        CE681526
012000                                                                  CE681526
012010 PRINT-A-LINE.                                                    CE681526
012020     WRITE PRINT-REC AFTER ADVANCING CARRIAGE-CONTROL.            CE681526
012030     MOVE SPACES TO PRINT-REC.                                    CE681526
012040                                                                  CE681526
012050                                                                  CE681526
012060 WRITE-OTHER-REC.                                                 CE681526
012070     WRITE OTHER-REC.                                             CE681526
012080     MOVE SPACES TO OTHER-REC.                                    CE681526
012090                                                                  CE681526
012100 PUNCH-CARD.                                                      CE681526
012110     WRITE CARD-REC AFTER STACKER-SELECT.                         CE681526
012120     MOVE SPACES TO CARD-REC.                                     CE681526
012130                                                                  CE681526
012140 REWRITE-TRANS.                                                   CE681526
012150     MOVE ^1^ TO PROC-TAG.                                        CE681526
012160     IF M-STATUS ' ^3^ MOVE ^03^ TO EX-MSG-CODE                   CE681526
012170         PERFORM ERROR-ROUTINE.                                   CE681526
012180     REWRITE TRAN-26.                                             CE681526
012190     MOVE MDNUM TO M-USED-DATE.                                   CE681526
012200     MOVE TURN-ON TO PROCESSED-SWT.                               CE681526
012210                                                                  CE681526
012220                                                                  CE681526
012230                                                                  CE681526
012240 MEMO-STK-ROUTINE.                                                CE681526
012250     MOVE ^4^ TO OTHER-REC-CODE.                                  CE681526
012260     MOVE CAT-NO TO OTHER-CAT-NO  MS-CAT.                         CE681526
012270     MOVE ^70^ TO OTHER-MSG-CODE.                                 CE681526
012280     MOVE ^9966^ TO MS-WO.                                        CE681526
012290     MOVE  T31-RC     TO MS-LOC.                                  CE681526
012300     MOVE   T31-ISS-QTY  TO MS-OH-QTY.                            CE681526
012310     MOVE M-UNIT-COST TO MS-U-COST.                               CE681526
012320     MULTIPLY M-UNIT-COST BY T31-ISS-QTY GIVING MS-EXT-VAL.       CE681526
012330     MOVE MEMO-STK-CARD TO OTHER-REC.                             CE681526
012340     PERFORM WRITE-OTHER-REC.                                     CE681526
012350     MOVE SPACES TO MEMO-STK-CARD.                                CE681526
012360                                                                  CE681526
012370 WRITE-TAPE.                                                      CE681526
012380     WRITE TAPE-TOT-REC.                                          CE681526
012390                                                                  CE681526
012400 END-OF-JOB.                                                      CE681526
012410     IF PROCESSED-SWT-ON NEXT SENTENCE  ELSE                      CE681526
012420         GO TO FINISHED-DETAILS.                                  CE681526
012430     ENTER LINKAGE.                                               CE681526
012440     CALL ^BMPPUT^  USING PM-WORK.                                CE681526
012450     ENTER COBOL.                                                 CE681526
012460     PERFORM DET-TOT-ROUTINE.                                     CE681526
012470 FINISHED-DETAILS.                                                CE681526
012480     MOVE PROC-ACCUMS TO TAPE-TOT-REC.                            CE681526
012490     PERFORM WRITE-TAPE.                                          CE681526
012500     MOVE NON-PROC-ACCUMS TO TAPE-TOT-REC.                        CE681526
012510     PERFORM WRITE-TAPE.                                          CE681526
012520     MOVE MISC-ACCUMS TO TAPE-TOT-REC.                            CE681526
012530     PERFORM WRITE-TAPE.                                          CE681526
012540                                                                  CE681526
012550 CLOSE-OUT.                                                       CE681526
012560     CLOSE  TRANSFILE  PRINTFILE CARDFILE  OTHERFILE              CE681526
012570         TAPEFILE.                                                CE681526
012580     ENTER LINKAGE.                                               CE681526
012590     CALL ^BMPCLOSE^  USING PM-WORK.                              CE681526
012600     ENTER COBOL.                                                 CE681526
012610                                                                  CE681526
012620     STOP RUN.                                                    CE681526
/*                                                                              
    ZhXm