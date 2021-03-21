// JOB CAR3090N 12332NIGAM        A    1077       COMPILE-CATAL                 
// OPTION CATAL                                                                 
   PHASE CAR3090N,*                                                             
// EXEC COBOL                                                                   
000000 IDENTIFICATION DIVISION.                                         T3090   
000010 PROGRAM-ID. 'T3090'.                                             T3090   
000020 AUTHOR. CAGNEY FRANCE, STANFORD OPTNER AND ASSOCIATES INC.       T3090   
000030 INSTALLATION. LOS ANGELES TRAFFIC DEPARTMENT, DATA SERVICE BUREAUT3090   
000040     CITY OF LOS ANGELES, JUNE 1969.                              T3090   
000050 REMARKS.  THIS PROGRAM PRINTS ALL TRAFFIC II WEEKLY              T3090   
000060     REPORTS FOR WHICH REQUESTS HAVE BEEN SUBMITTED.              T3090   
000070 ENVIRONMENT DIVISION.                                            T3090   
000080 CONFIGURATION SECTION.                                           T3090   
000090 SOURCE-COMPUTER. IBM-360.                                        T3090   
000100 OBJECT-COMPUTER. IBM-360.                                        T3090   
000110 INPUT-OUTPUT SECTION.                                            T3090   
000120 FILE-CONTROL.                                                    T3090   
000130     SELECT REPORT-FILE ASSIGN TO 'SYS020' UTILITY 2314.          KM120870
000140                                                                  KM120870
000150     SELECT VOLFILE ASSIGN TO 'SYS021' DIRECT-ACCESS 2314         KM120870
000152             RESERVE NO ALTERNATE AREA                            KM120870
000160             ACCESS IS RANDOM                                     T3090   
000170             ORGANIZATION IS INDEXED                              T3090   
000180             RECORD KEY IS VOL-REC-KEY                            T3090   
000190             SYMBOLIC KEY IS VOL-SYM-KEY.                         T3090   
000200     SELECT PRINT-FILE ASSIGN TO 'SYS012' UNIT-RECORD 1403.       KM120870
000210     SELECT ISFILE ASSIGN TO 'SYS023' DIRECT-ACCESS 2314          KM120870
000212             RESERVE NO ALTERNATE AREA                            KM120870
000220             ACCESS IS RANDOM                                     T3090   
000230             ORGANIZATION IS INDEXED                              T3090   
000240             RECORD KEY IS IS-REC-KEY                             T3090   
000250             SYMBOLIC KEY IS SYM-KEY.                             T3090   
000260     SELECT STRAT-FILE ASSIGN TO 'SYS024' DIRECT-ACCESS 2314      KM120870
000262         RESERVE NO ALTERNATE AREA                                KM120870
000270         ACCESS IS RANDOM                                         T3090   
000280         ORGANIZATION IS INDEXED                                  T3090   
000290         RECORD KEY IS ST-REC-KEY                                 T3090   
000300         SYMBOLIC KEY IS ST-SYM-KEY.                              T3090   
000302     SELECT JURIS-FILE ASSIGN TO 'SYS025' UTILITY 2314.           KM120171
000310 DATA DIVISION.                                                   T3090   
000320 FILE SECTION.                                                    T3090   
000330 FD  PRINT-FILE                                                   T3090   
000340     LABEL RECORDS ARE OMITTED                                    T3090   
000350     RECORDING MODE IS F                                          T3090   
000360     RECORD CONTAINS 133 CHARACTERS                               T3090   
000370                                                                  KM120870
000380     DATA RECORD IS PRINT-REC.                                    T3090   
000390 01  PRINT-REC.                                                   T3090   
000400     02 FILLER       PICTURE X.                                   T3090   
000410     02 PRINT-LINE           PICTURE  X(132).                     T3090   
000420     02 PRINTX  REDEFINES PRINT-LINE.                             T3090   
000430        03 FILLER            PICTURE XX.                          T3090   
000440        03 PRINT-LINE-SUB OCCURS 3 TIMES.                         T3090   
000450          05 PRINT-LINE-SUB0.                                     T3090   
000460           07 PRINT-LINE-SUB1 PICTURE X(27).                      T3090   
000470           07 PRINT-LINE-SUB2 PICTURE XXXX.                       T3090   
000480        03 PRINT-LINE-SUB-L   PICTURE X(37).                      T3090   
000490     02  T23-PRINT REDEFINES PRINTX.                              T3090   
000500         05 T23-P-C          PICTURE X(7).                        T3090   
000510         05 T23-P-C3 OCCURS 125 TIMES.                            T3090   
000520            07 T23-P-INC     PICTURE X.                           T3090   
000530     02  T23-PRINT-1 REDEFINES T23-PRINT.                         T3090   
000540         05 T23-P-NAM OCCURS 3 TIMES.                             T3090   
000550            07 T23-P-NAME1   PICTURE X(40).                       T3090   
000560            07 FILLER        PICTURE X(4).                        T3090   
000570     02  T23-P-SUMMARY REDEFINES T23-PRINT-1.                     T3090   
000580         05 FILLER           PICTURE X(18).                       T3090   
000590         05 T23-SUM-SEC OCCURS 2 TIMES.                           T3090   
000600           06 T23-S-SEC.                                          T3090   
000610            07 T23-S-CONST   PICTURE X(12).                       T3090   
000620            07 T23-S1        PICTURE ZZZ.                         T3090   
000630            07 FILLER        PICTURE X.                           T3090   
000640            07 T23-S2        PICTURE ZZZ.                         T3090   
000650            07 FILLER        PICTURE X.                           T3090   
000660            07 T23-S3        PICTURE ZZZ.                         T3090   
000670            07 FILLER        PICTURE XXX.                         T3090   
000680            07 T23-S4        PICTURE Z,ZZZ.                       T3090   
000690            07 FILLER        PICTURE XX.                          T3090   
000700            07 T23-S5        PICTURE ZZZ.                         T3090   
000710            07 FILLER        PICTURE X.                           T3090   
000720            07 T23-S6        PICTURE ZZZ.                         T3090   
000730            07 FILLER        PICTURE X(17).                       T3090   
000740     02  T23-FINAL-S REDEFINES T23-P-SUMMARY.                     T3090   
000750         05 FILLER           PICTURE X(9).                        T3090   
000760         05 T23-FINAL-1      PICTURE X(40).                       T3090   
000770         05 T23-FINAL-2      PICTURE ZZZ,ZZZ.                     T3090   
000780         05 FILLER REDEFINES T23-FINAL-2.                         T3090   
000790            07 T23-X1        PICTURE XX.                          T3090   
000800            07 T23-X2        PICTURE ZZ.ZZ.                       T3090   
000810         05 FILLER           PICTURE X(75).                       T3090   
000820     02  P-10-LINEA REDEFINES T23-FINAL-S.                        T3090   
000830        05 T10-ITEM-1-2-3.                                        T3090   
000840           07 FILLER         PICTURE X(7).                        T3090   
000850           07 ITEM-1-2 OCCURS 2 TIMES.                            T3090   
000860              09 ITEM-1-CON  PICTURE X(12).                       T3090   
000870              09 FILLER      PICTURE XX.                          T3090   
000880              09 ITEM-1-TOT  PICTURE ZZZZ.                        T3090   
000890              09 FILLER      PICTURE X(17).                       T3090   
000900           07 ITEM-3.                                             T3090   
000910              09 FILLER      PICTURE XX.                          T3090   
000920              09 ITEM-3-CON  PICTURE X(9).                        T3090   
000930              09 FILLER      PICTURE XXX.                         T3090   
000940              09 ITEM-3-A-1  PICTURE ZZZ.                         T3090   
000950              09 FILLER      PICTURE X.                           T3090   
000960              09 ITEM-3-A-2  PICTURE ZZZ.                         T3090   
000970              09 FILLER      PICTURE X.                           T3090   
000980              09 ITEM-3-A-O  PICTURE ZZZ.                         T3090   
000990              09 FILLER      PICTURE XX.                          T3090   
001000              09 ITEM-3-A-T  PICTURE ZZZZ.                        T3090   
001010              09 FILLER      PICTURE X(6).                        T3090   
001020              09 ITEM-3-D-1  PICTURE ZZZ.                         T3090   
001030              09 FILLER      PICTURE X.                           T3090   
001040              09 ITEM-3-D-2  PICTURE ZZZ.                         T3090   
001050              09 FILLER      PICTURE X.                           T3090   
001060              09 ITEM-3-D-O  PICTURE ZZZ.                         T3090   
001070              09 FILLER      PICTURE XX.                          T3090   
001080              09 ITEM-3-D-T  PICTURE ZZZZ.                        T3090   
001090              09 FILLER      PICTURE X.                           T3090   
001100           07 ITEM-3-C REDEFINES ITEM-3.                          T3090   
001110              09 FILLER      PICTURE X(14).                       T3090   
001120              09 ITEM-3-C1   PICTURE X(41).                       T3090   
001130     02 P-10-LINEB REDEFINES P-10-LINEA.                          T3090   
001140        05 T10-ITEM-4-5-6.                                        T3090   
001150           07 FILLER         PICTURE X(7).                        T3090   
001160           07 ITEM-4-CON     PICTURE X(12).                       T3090   
001170           07 ITEM-4-ACC     PICTURE ZZZ.                         T3090   
001180           07 FILLER         PICTURE X(6).                        T3090   
001190           07 ITEM-4-PER     PICTURE ZZZ.                         T3090   
001200           07 FILLER         PICTURE X(10).                       T3090   
001210           07 ITEM-5-CON     PICTURE X(15).                       T3090   
001220           07 ITEM-5-ACC     PICTURE ZZZ.                         T3090   
001230           07 FILLER         PICTURE X(20).                       T3090   
001240           07 ITEM-6-CON     PICTURE X(6).                        T3090   
001250           07 ITEM-6-LOC     PICTURE XXX.                         T3090   
001260           07 FILLER         PICTURE XX.                          T3090   
001270           07 ITEM-6-DIR     PICTURE XX.                          T3090   
001280           07 FILLER         PICTURE XX.                          T3090   
001290           07 ITEM-6-VOL     PICTURE ZZ,ZZZ.                      T3090   
001300        07 ITEM-6-C          PICTURE XX.                          T3090   
001310        07 FILLER            PICTURE X.                           T3090   
001320           07 ITEM-6-MO      PICTURE XX.                          T3090   
001330           07 ITEM-6-C1      PICTURE X.                           T3090   
001340           07 ITEM-6-DA      PICTURE XX.                          T3090   
001350           07 ITEM-6-C2      PICTURE X.                           T3090   
001360           07 ITEM-6-YR      PICTURE XX.                          T3090   
001370           07 FILLER         PICTURE X(21).                       T3090   
001380     02 P-10-LINEC REDEFINES P-10-LINEB.                          T3090   
001390        05 T10-ITEM-7-8.                                          T3090   
001400           07 ITEM-7-H       PICTURE X(80).                       T3090   
001410           07 ITEM-FIELDS REDEFINES ITEM-7-H.                     T3090   
001420              09 ITEM-7-CON  PICTURE X(26).                       T3090   
001430              09 ITEM-7-MEAN PICTURE ZZ.ZZZ.                              
001440              09 FILLER      PICTURE XX.                          T3090   
001450              09 ITEM-7-DEV  PICTURE ZZ.ZZZ.                              
001460              09 FILLER      PICTURE XXX.                         T3090   
001470              09 ITEM-7-C-VA PICTURE ZZ.ZZZ.                              
001480              09 FILLER      PICTURE XX.                          T3090   
001490         09 ITEM-7-HERE.                                          T3090   
001500           11 ITEM-7-A-VA      PICTURE ZZZZ.ZZ.                           
001510           11 FILLER           PICTURE XXX.                               
001520         09 ITEM-7-HERE-A REDEFINES ITEM-7-HERE.                  T3090   
001530            11 ITEM-7-A-VAM  PICTURE ZZZ.ZZ.                      T3090   
001540            11 FILLER        PICTURE X(4).                        T3090   
001550              09 ITEM-7-DIFF PICTURE +ZZZ.ZZ.                             
001560              09 FILLER      PICTURE X(12).                               
001570           07 ITEM-8-CON     PICTURE X(29).                       T3090   
001580           07 ITEM-8-CON-1   PICTURE X(23).                       T3090   
001590           07 ITEM-8-FIELD REDEFINES ITEM-8-CON-1.                T3090   
001600              09 ITEM-8-N    PICTURE X.                           T3090   
001610              09 FILLER      PICTURE XXXX.                        T3090   
001620              09 ITEM-8-S    PICTURE X.                           T3090   
001630              09 FILLER      PICTURE XXXX.                        T3090   
001640              09 ITEM-8-E    PICTURE X.                           T3090   
001650              09 FILLER      PICTURE XXXX.                        T3090   
001660              09 ITEM-8-W    PICTURE X.                           T3090   
001670              09 FILLER      PICTURE X(7).                        T3090   
001680     02 T10-PRI-DET REDEFINES P-10-LINEC.                         T3090   
001690        05 T10-DR-NO1        PICTURE X(6).                        T3090   
001700        05 FILLER            PICTURE XX.                          T3090   
001710        05 T10-MO1           PICTURE XX.                          T3090   
001720        05 P-T10-1           PICTURE X.                           T3090   
001730        05 T10-DA1           PICTURE XX.                          T3090   
001740        05 P-T10-2           PICTURE X.                           T3090   
001750        05 T10-YR1           PICTURE XX.                          T3090   
001760        05 FILLER            PICTURE X.                           T3090   
001770        05 P-T10-DAY         PICTURE XX.                          T3090   
001780        05 FILLER            PICTURE X.                           T3090   
001790        05 T10-HR1           PICTURE XX.                          T3090   
001800        05 P-T10-3           PICTURE X.                           T3090   
001810        05 T10-M1            PICTURE XX.                          T3090   
001820        05 FILLER            PICTURE X.                           T3090   
001830        05 T10-INJ-A1        PICTURE ZZ.                          T3090   
001840        05 FILLER            PICTURE X.                           T3090   
001850        05 T10-INJ-B1        PICTURE ZZ.                          T3090   
001860        05 FILLER            PICTURE X.                           T3090   
001870        05 T10-INJ-C1        PICTURE ZZ.                          T3090   
001880        05 FILLER            PICTURE X.                           T3090   
001890        05 T10-INJ-K1        PICTURE ZZ.                          T3090   
001900        05 FILLER            PICTURE XX.                          T3090   
001910        05 P-VEH-TYPE        PICTURE XXXX.                        T3090   
001920        05 FILLER            PICTURE X.                           T3090   
001930        05 T10-CLASS1        PICTURE Z.                           T3090   
001940        05 FILLER            PICTURE X.                           T3090   
001950        05 T10-COND1         PICTURE ZZ.                          T3090   
001960        05 FILLER            PICTURE X.                           T3090   
001970        05 P-ACC-TYPE        PICTURE X(7).                        T3090   
001980        05 FILLER            PICTURE X.                           T3090   
001990        05 P-10-ACTION       PICTURE X(8).                        T3090   
002000        05 FILLER            PICTURE X.                           T3090   
002010        05 P-10-IS           PICTURE XXX.                         T3090   
002020        05 FILLER            PICTURE X.                           T3090   
002030        05 P-PT-IMPACT       PICTURE X(8).                        T3090   
002040        05 OLD-PT-IMPACT REDEFINES P-PT-IMPACT.                   T3090   
002050           07 IMPACT-1       PICTURE X(4).                        T3090   
002060           07 IMPACT-2       PICTURE X(4) JUSTIFIED RIGHT.        T3090   
002070        05 OLD-IMPACT-1 REDEFINES OLD-PT-IMPACT.                  T3090   
002080           07 INTER-PLUS-1   PICTURE ZZ99.                        T3090   
002090           07 NEW-IMP        PICTURE XXXX.                        T3090   
002100           07 NEW-IMP-D REDEFINES NEW-IMP.                        T3090   
002110              09 NEW-IMP-1   PICTURE X.                           T3090   
002120              09 NEW-IMP-2   PICTURE X.                           T3090   
002130              09 NEW-IMP-3   PICTURE X.                           T3090   
002140              09 NEW-IMP-4   PICTURE X.                           T3090   
002150        05 FILLER            PICTURE XX.                          T3090   
00       05 P-10-DIR          PICTURE XXXX.                        T3090   
002170        05 FILLER   REDEFINES P-10-DIR.                           T3090   
002180           07 FILLER         PICTURE XX.                          T3090   
002190           07 P-10-SP        PICTURE X.                           T3090   
002200           07 P-10-NO            PICTURE 9.                       T3090   
002210        05 FILLER            PICTURE XX.                          T3090   
002220        05 DIR-ANL1          PICTURE 999.                         T3090   
002230        05 FILLER            PICTURE X.                           T3090   
002240        05 P-CONTRI-CIR      PICTURE X(10).                       T3090   
002250        05 FILLER            PICTURE XX.                          T3090   
002260        05 PRI-CODE1         PICTURE X(6).                        T3090   
002270        05 FILLER            PICTURE X.                           T3090   
002280        05 P-GP-CODE         PICTURE X(7).                        T3090   
002290        05 FILLER            PICTURE X.                           T3090   
002300        05 P-WEATHER         PICTURE X.                           T3090   
002310        05 FILLER            PICTURE X.                           T3090   
002320        05 P-LIGHT           PICTURE X(5).                        T3090   
002330        05 FILLER            PICTURE X.                           T3090   
002340        05 T10-AGE1          PICTURE ZZ.                          T3090   
002350        05 FILLER            PICTURE X.                           T3090   
002360        05 P-RD-CND          PICTURE X.                           T3090   
002370        05 FILLER            PICTURE X.                           T3090   
002380        05 SPEC-CIR1         PICTURE XX.                          T3090   
002390 FD  REPORT-FILE                                                  T3090   
002400     LABEL RECORDS ARE STANDARD                                   T3090   
002410     RECORDING MODE IS F                                          T3090   
002420     RECORD CONTAINS 290 CHARACTERS                               T3090   
002430     BLOCK CONTAINS 25 RECORDS                                    T3090   
002440     DATA RECORD IS REPORT-REC.                                   T3090   
002450 01  REPORT-REC.                                                  T3090   
002460     02  T10-DETAIL.                                              T3090   
002470         05 T10-REQUESTER    PICTURE X(15).                       T3090   
002480         05 T10-SEARCH-DATES PICTURE X(12).                       T3090   
002490         05 FILLER           PICTURE XX.                          T3090   
002500         05 T10-IS-CODE.                                          T3090   
002510            07 T10-IS-1      PICTURE X(5).                        T3090   
002520            07 T10-IS-2      PICTURE X(5).                        T3090   
002530         05 DOW              PICTURE X.                           T3090   
002540         05 D-O-W REDEFINES DOW PICTURE 9.                        T3090   
002550         05 T10-DATE.                                             T3090   
002560            07 T10-YR        PICTURE XX.                          T3090   
002570            07 T10-MO        PICTURE XX.                          T3090   
002580            07 T10-DA        PICTURE XX.                          T3090   
002590         05 T10-TIME.                                             T3090   
002600            07 T10-HR        PICTURE XX.                          T3090   
002610            07 T10-M         PICTURE XX.                          T3090   
002620         05 T10-DR-NO        PICTURE X(6).                        T3090   
002630         05 PRI-CODE         PICTURE X(6).                        T3090   
002640         05 GP-CODE          PICTURE XX.                          T3090   
002650         05 GROUP-CODE REDEFINES  GP-CODE PICTURE 99.             T3090   
002660         05 LOCAT-CODE       PICTURE 999.                         T3090   
002670         05 LOCATION-CODE REDEFINES LOCAT-CODE.                   T3090   
002680            07 INTERSECTION  PICTURE 9.                           T3090   
002690            07 DIRECTION-1   PICTURE 9.                           T3090   
002700            07 DIRECTION-2   PICTURE 9.                           T3090   
002710         05 LOC-A REDEFINES LOCATION-CODE.                        T3090   
002720            07 FILLER        PICTURE X.                           T3090   
002730            07 DISTA         PICTURE 99.                          T3090   
002740         05 PRI-CODE-1       PICTURE 9.                           T3090   
002750         05 PRI-CODE-2       PICTURE 9.                           T3090   
002760         05 SEC-FEET         PICTURE 9999.                        T3090   
002770         05 SEC-CODE-1       PICTURE 9.                           T3090   
002780         05 SEC-CODE-2       PICTURE 9.                           T3090   
002790         05 DIR-ANL          PICTURE 999.                         T3090   
002800         05 LIGHT            PICTURE X.                           T3090   
002810         05 RD-CONDITION     PICTURE X.                           T3090   
002820         05 SPEC-CIR         PICTURE XX.                          T3090   
002830         05 WEATHER          PICTURE 9.                           T3090   
002840         05 T10-AT-IS        PICTURE 9.                           T3090   
002850         05 T10-INJ-A        PICTURE S99.                         T3090   
002860         05 T10-INJ-B        PICTURE S99.                         T3090   
002870         05 T10-INJ-C        PICTURE S99.                         T3090   
002880         05 T10-INJ-K        PICTURE S99.                         T3090   
002890         05 T10-ACC-TYPE     PICTURE 99.                          T3090   
002900         05 T10-A-SEV        PICTURE 9.                           T3090   
002910         05 T10-REVERSE-ST-C PICTURE 9.                           T3090   
002920         05 FILLER           PICTURE X(4).                        T3090   
002930         05 T10-PARTY OCCURS 9 TIMES.                             T3090   
002940           06 T10-PARTY-ENTRY.                                    T3090   
002950            07 PARTY-NO      PICTURE 9.                           T3090   
002960            07 VEH-ACTION    PICTURE 99.                          T3090   
002970            07 T10-DIR       PICTURE 9.                           T3090   
002980            07 VEH-TYPE      PICTURE 99.                          T3090   
002990            07 T10-CLASS     PICTURE 9.                           T3090   
003000            07 T10-COND      PICTURE 99.                          T3090   
003010            07 T10-AGE       PICTURE 99.                          T3090   
003020            07 PARTY-INJ     PICTURE 9.                           T3090   
003030            07 CONTRIB-CIR   PICTURE 99.                          T3090   
003040            07 FILLER        PICTURE XX.                          T3090   
003050            07 PED-ACTION    PICTURE 99.                          T3090   
003060         05 FILLER           PICTURE X(8).                        T3090   
003070     02 T12-19-DETAILS REDEFINES T10-DETAIL.                      T3090   
003080        03 T12-REQUESTER     PICTURE X(11).                       T3090   
003090        03 FILLER            PICTURE XXXX.                        T3090   
003100        03 T12-SEARCH-DATES.                                      T3090   
003110     05 S-FR-MO              PICTURE XX.                          T3090   
003120     05 S-FR-DA              PICTURE XX.                          T3090   
003130     05 S-FR-YR              PICTURE XX.                          T3090   
003140     05 S-TO-MO              PICTURE XX.                          T3090   
003150     05 S-TO-DA              PICTURE XX.                          T3090   
003160     05 S-TO-YR              PICTURE XX.                          T3090   
003170        03 T12-SEARCH-TIMES.                                      T3090   
003180     05 S-FR-HR              PICTURE 99.                          T3090   
003190     05 S-FR-M               PICTURE 99.                          T3090   
003200     05 S-TO-HR              PICTURE 99.                          T3090   
003210     05 S-TO-M               PICTURE 99.                          T3090   
003220        03 T12-CAT-LIMIT     PICTURE XX.                          T3090   
003230        03 T12-CAT-CODES OCCURS 14 TIMES.                         T3090   
003240           05 CAT            PICTURE 999.                         T3090   
003250        03 T12-AT-CODE       PICTURE X.                           T3090   
003260        03 T12-I-S           PICTURE S999.                        T3090   
003270        03 T12-NON-I-S       PICTURE S999.                        T3090   
003280        03 T12-FROM          PICTURE X(5).                        T3090   
003290        03 T12-TO            PICTURE X(5).                        T3090   
003300        03 T12-FROM-NAME     PICTURE X(30).                       T3090   
003310        03 T12-TO-NAME       PICTURE X(30).                       T3090   
003320        03 T19-FLD.                                               T3090   
003330           05 T19-SEG-NO     PICTURE X(5).                        T3090   
003340           05 T19-SEG-CODE   PICTURE X(5).                        T3090   
003350           05 T19-SEG-NAME   PICTURE X(30).                       T3090   
003360        03 FILLER            PICTURE X(74).                       T3090   
003370     02 SORT-KEY.                                                 T3090   
003380        05 REPORT-CODE       PICTURE 99.                          T3090   
003390     05 REQ-NO.                                                   KM021971
003392        07 FILLER            PICTURE X(02).                       KM021971
003394        07 REQ-NO-3          PICTURE X(01).                       KM021971
003400        05 TOT-CAT-ACC       PICTURE S999.                        T3090   
003410        05 REC-CODE          PICTURE 9.                           T3090   
003420        05 DTANCE.                                                T3090   
003430           07 DISTANCE       PICTURE 999.                         T3090   
003440           07 FILLER         PICTURE XX.                          T3090   
003450        05 T20-SEG-NO REDEFINES DTANCE   PICTURE X(5).            T3090   
003460        05 FILLER            PICTURE X(6).                        T3090   
003470 FD  ISFILE                                                       T3090   
003480     LABEL RECORDS ARE STANDARD                                   T3090   
003490     RECORDING MODE IS F                                          T3090   
003500     RECORD CONTAINS 300 CHARACTERS                               T3090   
003510     BLOCK CONTAINS 10 RECORDS                                    KM120171
003520     DATA RECORD IS ISFILE-REC.                                   T3090   
003530 01  ISFILE-REC.                                                  T3090   
003540     03 IS-DELETE-CODE       PICTURE X.                           T3090   
003550     03 IS-REC-KEY.                                               T3090   
003560        05 IS-KEY-ST-1       PICTURE X(5).                        T3090   
003570        05 IS-KEY-ST-2       PICTURE X(5).                        T3090   
003580     03 IS-NAME.                                                  T3090   
003590        05 IS-NAME-1         PICTURE X(30).                       T3090   
003600        05 IS-NAME-2         PICTURE X(30).                       T3090   
003610     03 IS-CLASSIFICATION.                                        T3090   
003620        05 IS-CODES.                                              T3090   
003630           07 IS-CLASS-CODE  PICTURE 9.                           T3090   
003640           07 IS-NO-LEGS     PICTURE 9.                           T3090   
003650           07 IS-CONTROL     PICTURE 9.                           T3090   
003660           07 IS-SIGNAL-CTRL PICTURE 9.                           T3090   
003670           07 IS-SIGNAL-CHAR PICTURE 99.                          T3090   
003680        05 IS-DATE.                                               T3090   
003690           07 IS-MO          PICTURE XX.                          T3090   
003700           07 IS-DA          PICTURE XX.                          T3090   
003710           07 IS-YR          PICTURE XX.                          T3090   
003720     03 IS-APPROACH-CHARS.                                        T3090   
003730        05 IS-LEG-CHAR OCCURS 4 TIMES.                            T3090   
003740           07 IS-NO-LANES    PICTURE X.                           T3090   
003750           07 IS-TURN        PICTURE X.                           T3090   
003760           07 IS-CTRL-SIGN   PICTURE X.                           T3090   
003770           07 IS-MAST-ARM    PICTURE X.                           T3090   
003780           07 IS-T-SIG-CTRL  PICTURE X.                           T3090   
003790           07 IS-LT-SIGN-REG PICTURE X.                           T3090   
003800           07 IS-LT-PED-REG  PICTURE X.                           T3090   
003810           07 IS-BUS-ZONE    PICTURE X.                           T3090   
003820           07 IS-SIG-APPR    PICTURE X.                           T3090   
003830           07 IS-MED-TYPE    PICTURE X.                           T3090   
003840           07 IS-MED-WDTH    PICTURE X.                           T3090   
003850           07 IS-PK-STP-RSTR PICTURE X.                           T3090   
003860           07 IS-DIR-FLOW    PICTURE X.                           T3090   
003870           07 IS-SEC-S-LOC   PICTURE X.                           T3090   
003880     03 IS-CORDINATES.                                            T3090   
003890        05 IS-X-CORD         PICTURE X(6).                        T3090   
003900        05 IS-X-CORD-N REDEFINES IS-X-CORD PICTURE 9(6).          T3090   
003910        05 IS-Y-CORD         PICTURE X(6).                        T3090   
003920        05 IS-Y-CORD-N REDEFINES IS-Y-CORD PICTURE 9(6).          T3090   
003930     03 IS-SEGMENT-NO.                                            T3090   
003940        05 IS-N-LEG          PICTURE X(5).                        T3090   
003950        05 IS-E-LEG          PICTURE X(5).                        T3090   
003960        05 IS-S-LEG          PICTURE X(5).                        T3090   
003970        05 IS-W-LEG          PICTURE X(5).                        T3090   
003980     03 IS-NEXT-IS.                                               T3090   
003990        05 IS-NEXT-LEG OCCURS 4 TIMES.                            T3090   
004000           07 IS-NEXT-IS-CODE.                                    T3090   
004010              09 IS-NEXT-ST-1    PICTURE X(5).                    T3090   
004020              09 IS-NEXT-ST-2    PICTURE X(5).                    T3090   
004030           07 IS-NEXT-IS-DIST    PICTURE 9999.                    T3090   
004040           07 IS-NEXT-IS-SLOPE   PICTURE X.                       T3090   
004050     03 IS-HAZARDOUS-ID.                                          T3090   
004060        05 IS-YEAR OCCURS 4 TIMES.                                T3090   
004070           07 IS-YEAR-1      PICTURE 9.                           T3090   
004080           07 IS-YEAR-2      PICTURE 9.                           T3090   
004090           07 IS-YEAR-3      PICTURE 9.                           T3090   
004100           07 IS-YEAR-4      PICTURE 9.                           T3090   
004110     03 IS-DEAD-END          PICTURE X.                           T3090   
004120     03 IS-VACATED           PICTURE X.                           T3090   
004130     03 IS-OTHER-IS.                                              T3090   
004140        05 IS-JOG            PICTURE X.                           T3090   
004150        05 IS-OTHER-ST.                                           T3090   
004160           07 IS-OTHER-ST-1  PICTURE X(5).                        T3090   
004170           07 IS-OTHER-ST-2  PICTURE X(5).                        T3090   
004180     03 IS-ASSUME-EW         PICTURE X.                           T3090   
004190     03 IS-MAINT             PICTURE XXXX.                        T3090   
004200     03 IS-POL-DIST          PICTURE XXXX.                        T3090   
004210     03 IS-TR-DIST           PICTURE X.                           T3090   
004220     03 FILLER               PICTURE X(10).                       T3090   
004230     03 IS-STRAF             PICTURE 999.                         T3090   
004240     03 FILLER               PICTURE X(17).                       T3090   
004250 FD  VOLFILE                                                      T3090   
004260     LABEL RECORDS ARE STANDARD                                   T3090   
004270     RECORDING MODE IS F                                          T3090   
004280     RECORD CONTAINS 110 CHARACTERS                               T3090   
004290                                                                  KM120870
004300     DATA RECORD IS VOLFILE-REC.                                  T3090   
004310 01  VOLFILE-REC.                                                 T3090   
004320     03 VOL-DELETE-CODE      PICTURE X.                           T3090   
004330     03 VOL-REC-KEY.                                              T3090   
004340        05 VOL-REC-ST.                                            T3090   
004350           07 VOL-REC-ST-1   PICTURE X(5).                        T3090   
004360           07 VOL-REC-ST-2   PICTURE X(5).                        T3090   
004370        05 VOL-REC-INDEX     PICTURE XXX.                         T3090   
004380        05 VOL-REC-INDEX-N REDEFINES VOL-REC-INDEX   PICTURE S999.T3090   
004390     03 VOL-LATEST           PICTURE XXX.                         T3090   
004400     03 VOL-LATEST-N REDEFINES VOL-LATEST    PICTURE S999.        T3090   
004410     03 VOL-OLDEST           PICTURE XXX.                         T3090   
004420     03 VOL-OLDEST-N REDEFINES VOL-OLDEST    PICTURE S999.        T3090   
004430     03 VOL-REC-CODE         PICTURE 9.                           T3090   
004440     03 VOL-DATE.                                                 T3090   
004450        05 VOL-YR        PICTURE 99.                                      
004460        05 VOL-MO        PICTURE 99.                                      
004470        05 VOL-DA        PICTURE 99.                                      
004480     03 VOL-D-O-W            PICTURE X.                           T3090   
004490     03 VOL-LOC.                                                  T3090   
004500        05 VOL-LOC-1         PICTURE X.                           T3090   
004510        05 VOL-LOC-2         PICTURE X.                           T3090   
004520        05 VOL-LOC-3         PICTURE X.                           T3090   
004530     03 VOL-DIST             PICTURE XX.                          T3090   
004540     03 VOL-REV-CODE         PICTURE X.                           T3090   
004550     03 VOL-SPEC-CODE        PICTURE X.                           T3090   
004560     03 VOL-AM-PM-PEAK.                                           T3090   
004570        05 VOL-PEAK OCCURS 2 TIMES.                               T3090   
004580           07 VOL-N-E.                                            T3090   
004590              09 V-NE-HR.                                         T3090   
004600                 11 V-NE-H   PICTURE XX.                          T3090   
004610                 11 V-NE-M   PICTURE X.                           T3090   
004620              09 V-NE-VOL    PICTURE 9999.                        T3090   
004630           07 VOL-S-W.                                            T3090   
004640              09 V-SW-HR.                                         T3090   
004650                 11 V-SW-H   PICTURE XX.                          T3090   
004660                 11 V-SW-M   PICTURE X.                           T3090   
004670              09 V-SW-VOL    PICTURE 9999.                        T3090   
004680           07 VOL-TOT.                                            T3090   
004690              09 V-TOT-HR.                                        T3090   
004700                 11 V-TOT-H  PICTURE XX.                          T3090   
004710                 11 V-TOT-M  PICTURE X.                           T3090   
004720              09 V-TOT-VOL   PICTURE 9999.                        T3090   
004730     03 VOL-TOTALS-HR.                                            T3090   
004740        05 VOL-TOTAL-NE      PICTURE X(5).                        T3090   
004750        05 VOL-TOTAL-NE-N REDEFINES VOL-TOTAL-NE PICTURE S9(5).   T3090   
004760        05 VOL-TOTAL-SW      PICTURE X(5).                        T3090   
004770        05 VOL-TOTAL-SW-N REDEFINES VOL-TOTAL-SW PICTURE S9(5).   T3090   
004780        05 VOL-TOTAL-TOT     PICTURE X(5).                        T3090   
004790        05 VOL-TOTAL-TOT-N REDEFINES VOL-TOTAL-TOT PICTURE S9(5). T3090   
004800     03 VOL-PHY-DESC.                                             T3090   
004810        05 VOL-MED           PICTURE XXX.                         T3090   
004820        05 VOL-ADJ-FAC REDEFINES VOL-MED     PICTURE  9V99.       T3090   
004830        05 VOL-RD-WIDTH      PICTURE XXXX.                        T3090   
004840        05 VOL-NO-LA         PICTURE XX.                          T3090   
004850     03 FILLER               PICTURE X(9).                        T3090   
004860 FD  STRAT-FILE                                                   T3090   
004870     RECORD CONTAINS 200 CHARACTERS                               T3090   
004880                                                                  KM120870
004890     RECORDING MODE IS F                                          T3090   
004900     LABEL RECORDS ARE STANDARD                                   T3090   
004910     DATA RECORD IS STRAT-REC.                                    T3090   
004920 01  STRAT-REC.                                                   T3090   
004930     03 ST-DELETE-CODE       PICTURE X.                           T3090   
004940     03 ST-REC-KEY           PICTURE 999.                         T3090   
004950     03 ST-TYPE              PICTURE 9.                           T3090   
004960     03 ST-NO-IN.                                                 T3090   
004970        05 ST-TOT-NO         PICTURE 9(5).                        T3090   
004980        05 ST-NO-WITH-VOL    PICTURE 9(5).                        T3090   
004990     03 ST-NO-ACC.                                                T3090   
005000        05 ST-TOT-ACC        PICTURE 9(5).                        T3090   
005010        05 ST-INJ-FAT        PICTURE 9(5).                        T3090   
005020     03 ST-DATA.                                                  T3090   
005030        05 PER-IS-MILE.                                           T3090   
005040           07 ST-M-1         PICTURE 99V999.                      T3090   
005050           07 ST-SD-1        PICTURE 99V999.                      T3090   
005060           07 ST-CV-1        PICTURE 99V999.                      T3090   
005070           07 ST-M-2         PICTURE 99V999.                      T3090   
005080           07 ST-SD-2        PICTURE 99V999.                      T3090   
005090           07 ST-CV-2        PICTURE 99V999.                      T3090   
005100        05 PER-MV-MVM.                                            T3090   
005110           07 ST-M-3         PICTURE 99V999.                      T3090   
005120           07 ST-SD-3        PICTURE 99V999.                      T3090   
005130           07 ST-CV-3        PICTURE 99V999.                      T3090   
005140           07 ST-M-4         PICTURE 99V999.                      T3090   
005150           07 ST-SD-4        PICTURE 99V999.                      T3090   
005160           07 ST-CV-4        PICTURE 99V999.                      T3090   
005170     03 FILLER               PICTURE X(25).                       T3090   
005180     03 IS-STRATIFICATION    PICTURE X(61).                       T3090   
005190     03 SEG-STRATIFICATION REDEFINES IS-STRATIFICATION            T3090   
005200                             PICTURE X(61).                       T3090   
005210     03 FILLER               PICTURE X(29).                       T3090   
005211 FD  JURIS-FILE                                                   KM120171
005212     LABEL RECORDS ARE STANDARD                                   KM120171
005213     RECORDING MODE IS F                                          KM120171
005214     DATA RECORD IS J.                                            KM120171
005215 01  J.                                                           KM120171
005216     03 J-CODE                   PICTURE X(01).                   KM120171
005217     03 J-NAME                   PICTURE X(20).                   KM120171
005218     03 FILLER                   PICTURE X(59).                   KM120171
005220 WORKING-STORAGE SECTION.                                         T3090   
005222 77  ANT         COMPUTATIONAL   PICTURE S99     VALUE ZERO.      KM120271
005230 77  TX-1            PICTURE 9 VALUE 1.                           T3090   
005240 77  TX-2            PICTURE 9 VALUE 1.                           T3090   
005250 77  T81-REQ-HOLD    PICTURE XXX.                                 T3090   
005260 77  HDR-CTR         PICTURE 99.                                  T3090   
005270 77  PAGE-NO         PICTURE 999   VALUE ZEROES.                  T3090   
005280 77  LINE-NO         PICTURE 99    VALUE ZEROES.                  T3090   
005290 77  WORK-ACC        PICTURE S9(6).                               T3090   
005300 77  EXT-SW          PICTURE 9     VALUE ZERO.                    T3090   
005310 77  SW-81           PICTURE 9     VALUE 1.                       T3090   
005320 77  SW-10           PICTURE 9     VALUE 1.                       T3090   
005330 77  SW-12           PICTURE 9     VALUE 1.                       T3090   
005340 77  SW-19           PICTURE 9     VALUE 1.                       T3090   
005350 77  SW-20           PICTURE 9     VALUE 1.                       T3090   
005360 77  SW-23           PICTURE 9     VALUE 1.                       T3090   
005370 77  T12-REQ-HOLD    PICTURE XXX.                                 T3090   
005380 77  T19-REQ-HOLD    PICTURE XXX.                                 T3090   
005390 77  T12-T-LOC       PICTURE S9999 VALUE ZEROES.                  T3090   
005400 77  T12-T-CAT-ACC   PICTURE S9(5) VALUE ZEROES.                  T3090   
005410 77  T19-T-SEG       PICTURE S9999 VALUE ZEROES.                  T3090   
005420 77  T19-T-CAT-ACC   PICTURE S9(5) VALUE ZEROES.                  T3090   
005430 77  F               PICTURE 9.                                   T3090   
005440 77  L               PICTURE 99.                                  T3090   
005450 77  K               PICTURE 99.                                  T3090   
005460 77  CAT-SW          PICTURE 9     VALUE 1.                       T3090   
005470 77  CTR             PICTURE 99.                                  T3090   
005480 77  C               PICTURE 99.                                  T3090   
005490 77  E               PICTURE 999.                                 T3090   
005500 77  M               PICTURE 99.                                  T3090   
005510 77  N               PICTURE 99.                                  T3090   
005520 77  O               PICTURE 999.                                 T3090   
005530 77  P               PICTURE 99.                                  T3090   
005540 77  Q               PICTURE 99.                                  T3090   
005550 77  R               PICTURE 99.                                  T3090   
005560 77  FT              PICTURE 999.                                 T3090   
005570 77  SW10A           PICTURE     9.                               T3090   
005580 77  T10-IS-HOLD     PICTURE     X(10).                           T3090   
005590 77  PRTY-CTR        PICTURE     9     VALUE ZERO.                T3090   
005600 77  INTER-SWITCH    PICTURE     9     VALUE ZERO.                T3090   
005610 77  DIRECTION-HOLD  PICTURE     XXXX JUSTIFIED RIGHT.            T3090   
005620 77  AT-S            PICTURE     9     VALUE ZERO.                T3090   
005630 77  G               PICTURE     99.                              T3090   
005640 77  H               PICTURE     99.                              T3090   
005650 77  VOL-LATEST-HOLD PICTURE     S999.                            T3090   
005660 77  VOL-OLDEST-HOLD PICTURE     S999.                            T3090   
005670 77  REV-SWITCH      PICTURE     9     VALUE ZERO.                T3090   
005680                                                                  KM021971
005690 77  L-CTR                   PICTURE 9.                           T3090   
005700 77  T20-SEG-HOLD            PICTURE X(5).                        T3090   
005710 77  ADT-CTR                 PICTURE S99     VALUE ZEROES.        T3090   
005720 77  D-CTR                   PICTURE 99.                          T3090   
005730 77  DESC-SW                 PICTURE 9       VALUE 1.             T3090   
005740 77  SUBSCRIP-H-2            PICTURE 99.                          T3090   
005750 77  10-SW                   PICTURE 9 VALUE ZERO.                T3090   
005760 77  23-SW                   PICTURE 9.                           T3090   
005770 77  ITEM-7-HOLD             PICTURE S9999.                       T3090   
005780 77  ITEM-7-VOL              PICTURE 9(6).                        T3090   
005790 77  ITEM-7-HO               PICTURE S9999.                       T3090   
005800 77  ITEM-7-HOO              PICTURE S9999V9.                     T3090   
005801 77  SW-0-INDEX          PICTURE 9.                               AN121472
005802 01  T10-REQ-HOLD.                                                KM021971
005804     03 FILLER               PICTURE X(02).                       KM021971
005806     03 T10-REQ-HOLD-3       PICTURE X(01).                       KM021971
005807 01  NAME-TABLE.                                                  KM120171
005808     03 NT-ELEMENTS  OCCURS 20.                                   KM120171
005809        05 NT-CITY               PICTURE X(01).                   KM120171
005810        05 NT-NAME               PICTURE X(20).                   KM120171
005815 01  ST-SYM-KEY                  PICTURE 9(03).                   KM120171
005820 01  SYM-KEY.                                                     T3090   
005830     03 SYM-KEY-ST-1         PICTURE X(5).                        T3090   
005840     03 SYM-KEY-ST-2         PICTURE X(5).                        T3090   
005850 01  VOL-SYM-KEY.                                                 T3090   
005860     03 VOL-SYM-ST-CODE.                                          T3090   
005870        05 VOL-SYM-ST-1      PICTURE X(5).                        T3090   
005880        05 VOL-SYM-ST-2      PICTURE X(5).                        T3090   
005890     03 VOL-SYM-INDEX        PICTURE XXX.                         T3090   
005900     03 VOL-SYM-INDEX-N REDEFINES VOL-SYM-INDEX  PICTURE  999.    T3090   
005910 01  COMPUTERS-DATE.                                              T3090   
005920                                                                  KM120870
005930     03 DATE.                                                     T3090   
005940        05 MO                PICTURE 99.                          T3090   
005942        05 FILLER            PICTURE X.                           KM010871
005950        05 DA                PICTURE 99.                          T3090   
005952        05 FILLER            PICTURE X.                           KM010871
005960        05 YR                PICTURE 99.                          T3090   
005970                                                                  KM010871
005980 01  CAT-LIMIT-HOLD.                                              T3090   
005990     03 A                    PICTURE 999.                         T3090   
006000     03 B                    PICTURE 999.                         T3090   
006010 01  S                       PICTURE XX.                          T3090   
006020 01  T REDEFINES S           PICTURE 99.                          T3090   
006030 01  HDR.                                                         T3090   
006040     03 FILLER               PICTURE X.                           T3090   
006050     03 FILLER               PICTURE XXX   VALUE 'T-'.            T3090   
006060     03 H-NO                 PICTURE ZZ.                          T3090   
006070     03 FILLER               PICTURE X(49)   VALUE SPACES.        KM021971
006080     03 H-SYSTEM                 PICTURE X(13).                   KM120171
006090                                                                  KM120171
006095     03 ANT-HDR                  PICTURE X(20).                   KM120171
006100     03 FILLER                   PICTURE X(37)   VALUE SPACES.    KM120171
006110     03 FILLER               PICTURE X(5)  VALUE 'PAGE '.         T3090   
006120     03 HP                   PICTURE ZZZ.                         T3090   
006130 01  HDR2-3.                                                      T3090   
006140     03 FILLER               PICTURE X(45) VALUE SPACES.          T3090   
006150     03 HDR-2-3-TITLE        PICTURE X(40).                       T3090   
006160     03 FILLER               PICTURE X(48) VALUE SPACES.          T3090   
006170 01  HDR4.                                                        T3090   
006180     03 FILLER               PICTURE X(10) VALUE SPACES.          T3090   
006190     03 HDR4-TITLE           PICTURE X(20)                        T3090   
006200                 VALUE 'STREET CODES/NAMES: '.                    T3090   
006210     03 HDR4-ST1             PICTURE X(5).                        T3090   
006220     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006230     03 HDR4-ST2             PICTURE X(5).                        T3090   
006240     03 FILLER               PICTURE XX    VALUE SPACES.          T3090   
006250     03 HDR4-N1              PICTURE X(30).                       T3090   
006260     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006270     03 HDR4-N2              PICTURE X(30).                       T3090   
006280     03 FILLER               PICTURE X(14) VALUE '   REQUESTER: '.T3090   
006290     03 HDR4-REQ             PICTURE X(15).                       T3090   
006300 01  HDR5.                                                        T3090   
006310     03 FILLER               PICTURE X(10) VALUE SPACES.          T3090   
006320     03 FILLER               PICTURE X(14) VALUE 'SEARCH DATES: '.T3090   
006330     03 HDR5-MO-FR           PICTURE XX.                          T3090   
006340     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006350     03 HDR5-DA-FR           PICTURE XX.                          T3090   
006360     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006370     03 HDR5-YR-FR           PICTURE XX.                          T3090   
006380     03 FILLER               PICTURE X(6)  VALUE '  TO  '.        T3090   
006390     03 HDR5-MO-TO           PICTURE XX.                          T3090   
006400     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006410     03 HDR5-DA-TO           PICTURE XX.                          T3090   
006420     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006430     03 HDR5-YR-TO           PICTURE XX.                          T3090   
006440     03 FILLER               PICTURE X(61) VALUE SPACES.          T3090   
006450     03 FILLER               PICTURE X(10) VALUE 'RUN DATE: '.    T3090   
006460     03 HDR5-R-MO            PICTURE XX.                          T3090   
006470     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006480     03 HDR5-R-DA            PICTURE XX.                          T3090   
006490     03 FILLER               PICTURE X     VALUE '-'.             T3090   
006500     03 HDR5-R-YR            PICTURE XX.                          T3090   
006510     03 FILLER               PICTURE X(8)  VALUE SPACES.          T3090   
006520 01  HDR4-A.                                                      T3090   
006530     03 FILLER               PICTURE X(10) VALUE SPACES.          T3090   
006540     03 FILLER               PICTURE X(15)                        T3090   
006550                 VALUE 'STREET ROUTE:  '.                         T3090   
006560     03 HDR4-A-ST-C          PICTURE X(5).                        T3090   
006570     03 FILLER               PICTURE XXX   VALUE ' - '.           T3090   
006580     03 HDR4-A-ST-N          PICTURE X(30).                       T3090   
006590     03 FILLER               PICTURE X(44) VALUE SPACES.          T3090   
006600     03 FILLER               PICTURE X(11) VALUE 'REQUESTER: '.   T3090   
006610     03 HDR4-A-REQ           PICTURE X(15).                       T3090   
006620 01  T10-DET-P-HOLD.                                              T3090   
006630     03 IMPACT-LINE2-H.                                           T3090   
006640        05 IMPCT-1           PICTURE X(4).                        T3090   
006650        05 IMPCT-2           PICTURE X(4).                        T3090   
006660     03 NEW-IMP-HOLD REDEFINES IMPACT-LINE2-H.                    T3090   
006670        05 IMPCT-H           PICTURE ZZ99.                        T3090   
006680        05 IMPCT-H-1         PICTURE X.                           T3090   
006690        05 IMPCT-H-2         PICTURE X.                           T3090   
006700        05 IMPCT-H-3         PICTURE X.                           T3090   
006710        05 IMPCT-H-4         PICTURE X.                           T3090   
006720     03 T10LINE2-DIR-H       PICTURE X(4).                        T3090   
006730     03 T10LINE2-PR-GP-H     PICTURE X(7).                        T3090   
006740     03 T10LINE2-LIGHT-H     PICTURE X(5).                        T3090   
006750 01  ST-VOL-HOLD.                                                 T3090   
006760     03 ST-V-HOLD OCCURS 2 TIMES.                                 T3090   
006770       04 ST-V-H.                                                 T3090   
006780        05 ST-V-LOC          PICTURE XXX.                         T3090   
006790        05 ST-V-DIR          PICTURE XX.                          T3090   
006800        05 ST-V-V                    PICTURE  9(5).               T3090   
006810        05 ST-V-SIGN         PICTURE XX.                          T3090   
006820 01  T10-DAY-TABLE.                                               T3090   
006830     03 T10-DAY-TAB  PICTURE X(14) VALUE 'MOTUWETHFRSASU'.        T3090   
006840     03 T10-DAY-ENTRY REDEFINES T10-DAY-TAB OCCURS 7 TIMES        T3090   
006850                             PICTURE XX.                          T3090   
006860 01  T10-V-TYPE-TABLE.                                            T3090   
006870     03 T10-V-TAB.                                                T3090   
006880        05 FILLER    PICTURE X(4) VALUE 'UNK.'.                   T3090   
006890        05 FILLER    PICTURE X(4) VALUE 'AUTO'.                   T3090   
006900        05 FILLER    PICTURE X(4) VALUE 'AUTO'.                   T3090   
006910        05 FILLER    PICTURE X(4) VALUE 'AUTO'.                   T3090   
006920        05 FILLER    PICTURE X(4) VALUE 'AUTO'.                   T3090   
006930        05 FILLER    PICTURE X(4) VALUE 'TRCK'.                   T3090   
006940        05 FILLER    PICTURE X(4) VALUE 'AUTO'.                   T3090   
006950        05 FILLER    PICTURE X(4) VALUE 'AMBU'.                   T3090   
006960        05 FILLER    PICTURE X(4) VALUE 'BUS '.                   T3090   
006970        05 FILLER    PICTURE X(4) VALUE 'TRCK'.                   T3090   
006980        05 FILLER    PICTURE X(4) VALUE 'TRCK'.                   T3090   
006990        05 FILLER    PICTURE X(4) VALUE 'CYCL'.                   T3090   
007000        05 FILLER    PICTURE X(4) VALUE 'CYCL'.                   T3090   
007010        05 FILLER    PICTURE X(4) VALUE 'CYCL'.                   T3090   
007020        05 FILLER    PICTURE X(4) VALUE 'CYCL'.                   T3090   
007030        05 FILLER    PICTURE X(4)  VALUE 'TRCK'.                  T3090   
007040        05 FILLER    PICTURE X(4) VALUE 'TRAN'.                   T3090   
007050        05 FILLER    PICTURE X(4) VALUE 'BIKE'.                   T3090   
007060        05 FILLER    PICTURE X(4) VALUE 'AUTO'.                   T3090   
007070        05 FILLER    PICTURE X(4) VALUE 'ANML'.                   T3090   
007080        05 FILLER    PICTURE X(4) VALUE 'OTHR'.                   T3090   
007090     03 T10-V-ENTRY REDEFINES T10-V-TAB OCCURS 21 TIMES           T3090   
007100                     PICTURE XXXX.                                T3090   
007110 01  T10-CIR-TABLE.                                               T3090   
007120     03 T10-CIR-TAB.                                              T3090   
007130        05 FILLER    PICTURE X(10) VALUE '   NONE   '.            T3090   
007140        05 FILLER    PICTURE X(10) VALUE '  SPEED   '.            T3090   
007150        05 FILLER    PICTURE X(10) VALUE 'YIELD R/W '.            T3090   
007160        05 FILLER    PICTURE X(10) VALUE 'WRONG SIDE'.            T3090   
007170        05 FILLER    PICTURE X(10) VALUE ' RAN STOP '.            T3090   
007180        05 FILLER    PICTURE X(10) VALUE 'RAN SIGNAL'.            T3090   
007190        05 FILLER    PICTURE X(10) VALUE 'IMPR PASS '.            T3090   
007200        05 FILLER    PICTURE X(10) VALUE 'TOO CLOSE '.            T3090   
007210        05 FILLER    PICTURE X(10) VALUE 'IMPR TURN '.            T3090   
007220        05 FILLER    PICTURE X(10) VALUE 'IMPR DRIVE'.            T3090   
007230        05 FILLER    PICTURE X(10) VALUE '  OTHER   '.            T3090   
007240        05 FILLER    PICTURE X(10) VALUE 'LANE CHG  '.            T3090   
007250     03 T10-CIR-ENTRY REDEFINES T10-CIR-TAB OCCURS 12 TIMES       T3090   
007260                             PICTURE X(10).                       T3090   
007270 01  T10-A-TYPE-TABLE.                                            T3090   
007280     03 T10-A-TYPE-TAB.                                           T3090   
007290        05 FILLER    PICTURE X(7)  VALUE '  ROR  '.               T3090   
007300       05 FILLER     PICTURE X(7)  VALUE 'OVER TN'.               T3090   
007310        05 FILLER    PICTURE X(7)  VALUE '  PED  '.               T3090   
007320        05 FILLER    PICTURE X(7)  VALUE '       '.               T3090   
007330        05 FILLER    PICTURE X(7)  VALUE 'PRK-VEH'.               T3090   
007340        05 FILLER    PICTURE X(7)  VALUE 'NON VEH'.               T3090   
007350        05 FILLER    PICTURE X(7)  VALUE 'FIX OBJ'.               T3090   
007360        05 FILLER    PICTURE X(7)  VALUE 'OTH OBJ'.               T3090   
007370        05 FILLER    PICTURE X(7)  VALUE 'NONCOLL'.               T3090   
007380       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007390       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007400       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007410       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007420       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007430       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007440       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007450       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007460       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007470       05 FILLER     PICTURE X(7)  VALUE '       '.               T3090   
007480        05 FILLER    PICTURE X(7)  VALUE '  RA   '.               T3090   
007490        05 FILLER    PICTURE X(7)  VALUE ' RA-LT '.               T3090   
007500        05 FILLER    PICTURE X(7)  VALUE ' RA-RT '.               T3090   
007510        05 FILLER    PICTURE X(7)  VALUE '  LT   '.               T3090   
007520        05 FILLER    PICTURE X(7)  VALUE ' LT-LT '.               T3090   
007530        05 FILLER    PICTURE X(7)  VALUE ' LT-RT '.               T3090   
007540        05 FILLER    PICTURE X(7)  VALUE ' RE/SS '.               KM120870
007550        05 FILLER    PICTURE X(7)  VALUE ' RE-LT '.               T3090   
007560        05 FILLER    PICTURE X(7)  VALUE ' RE-RT '.               T3090   
007570        05 FILLER    PICTURE X(7)  VALUE 'HEAD ON'.               T3090   
007580        05 FILLER    PICTURE X(7)  VALUE ' RE/SS '.               KM120870
007590       05 FILLER     PICTURE X(7)  VALUE ' SS-LT '.               T3090   
007600       05 FILLER     PICTURE X(7)  VALUE ' SS-RT '.               T3090   
007610       05 FILLER     PICTURE X(7)  VALUE 'U-TURN '.               T3090   
007620       05 FILLER     PICTURE X(7)  VALUE 'EN DRWY'.               T3090   
007630       05 FILLER     PICTURE X(7)  VALUE 'LV DRWY'.               T3090   
007640       05 FILLER     PICTURE X(7)  VALUE ' OTHER '.               T3090   
007650     03 T10-A-TYPE-ENTRY REDEFINES T10-A-TYPE-TAB OCCURS 36 TIMES T3090   
007660                             PICTURE X(7).                        T3090   
007670 01  PRI-GP-TABLE.                                                T3090   
007680     03 PRI-GP-TAB.                                               T3090   
007690        05 FILLER    PICTURE X(14) VALUE ' DRUNK DRIVING'.        T3090   
007700        05 FILLER    PICTURE X(14) VALUE ' SPEED        '.        T3090   
007710        05 FILLER    PICTURE X(14) VALUE 'SIGNAL  LIGHT '.        T3090   
007720        05 FILLER    PICTURE X(14) VALUE ' STOP   SIGN  '.        T3090   
007730        05 FILLER    PICTURE X(14) VALUE ' WRONG  SIDE  '.        T3090   
007740        05 FILLER    PICTURE X(14) VALUE '  R/W    LT   '.        T3090   
007750        05 FILLER    PICTURE X(14) VALUE '  R/W  STP/YLD'.        T3090   
007760        05 FILLER    PICTURE X(14) VALUE '  R/W   OTHER '.        T3090   
007770        05 FILLER    PICTURE X(14) VALUE '  R/W    PED. '.        T3090   
007780        05 FILLER    PICTURE X(14) VALUE '  PED.  VIOL. '.        T3090   
007790        05 FILLER    PICTURE X(14) VALUE 'IMPROP.  TURN '.        T3090   
007800        05 FILLER    PICTURE X(14) VALUE 'FOL TOO CLOSE '.        T3090   
007810        05 FILLER    PICTURE X(14) VALUE 'UNSAFE CHANGE '.        T3090   
007820        05 FILLER    PICTURE X(14) VALUE ' START/BACKING'.        T3090   
007830        05 FILLER    PICTURE X(14) VALUE ' OTHER   VIOL '.        T3090   
007840        05 FILLER    PICTURE X(14) VALUE 'NON-APPIMPR DR'.        T3090   
007850        05 FILLER    PICTURE X(14) VALUE 'UNKNOWN       '.        T3090   
007860     03 FILLER REDEFINES PRI-GP-TAB OCCURS 17 TIMES.              T3090   
007870        05 GP-LINE1          PICTURE X(7).                        T3090   
007880        05 GP-LINE2          PICTURE X(7).                        T3090   
007890 01  DIR-TABLE.                                                   T3090   
007900     03 DIR-TAB PICTURE XXXX VALUE 'NESW'.                        T3090   
007910     03 DIR-ENTRY REDEFINES DIR-TAB OCCURS 4 TIMES PICTURE X.     T3090   
007920 01  T10-V-ACT-TABLE.                                             T3090   
007930     03 T10-V-ACT-TAB.                                            T3090   
007940        05 FILLER    PICTURE X(8)  VALUE 'STRAIGHT'.              T3090   
007950        05 FILLER    PICTURE X(8)  VALUE 'OVERTAKE'.              T3090   
007960        05 FILLER    PICTURE X(8)  VALUE '   RT   '.              T3090   
007970        05 FILLER    PICTURE X(8)  VALUE '   LT   '.              T3090   
007980        05 FILLER    PICTURE X(8)  VALUE ' U TURN '.              T3090   
007990        05 FILLER    PICTURE X(8)  VALUE 'SLW/STOP'.              T3090   
008000        05 FILLER    PICTURE X(8)  VALUE 'START LN'.              T3090   
008010        05 FILLER    PICTURE X(8)  VALUE 'STRT PRK'.              T3090   
008020        05 FILLER    PICTURE X(8)  VALUE 'BACKING '.              T3090   
008030        05 FILLER    PICTURE X(8)  VALUE 'STOPPED '.              T3090   
008040        05 FILLER    PICTURE X(8)  VALUE ' PARKED '.              T3090   
008050        05 FILLER    PICTURE X(8)  VALUE ' OTHER  '.              T3090   
008060        05 FILLER    PICTURE X(8)  VALUE ' UNKNOWN'.              T3090   
008070     03 T10-V-ACT-ENTRY REDEFINES T10-V-ACT-TAB                   T3090   
008080                     OCCURS 13 TIMES PICTURE X(8).                T3090   
008090 01  T10-PED-ACT-TABLE.                                           T3090   
008100     03 T10-PED-ACT-TAB.                                          T3090   
008110        05 FILLER    PICTURE X(8)  VALUE '   01   '.              T3090   
008120        05 FILLER    PICTURE X(8)  VALUE '   02   '.              T3090   
008130        05 FILLER    PICTURE X(8)  VALUE '   03   '.              T3090   
008140        05 FILLER    PICTURE X(8)  VALUE '   04   '.              T3090   
008150        05 FILLER    PICTURE X(8)  VALUE '   05   '.              T3090   
008160        05 FILLER    PICTURE X(8)  VALUE '   06   '.              T3090   
008170        05 FILLER    PICTURE X(8)  VALUE '   07   '.              T3090   
008180        05 FILLER    PICTURE X(8)  VALUE '   08   '.              T3090   
008190        05 FILLER    PICTURE X(8)  VALUE '   09   '.              T3090   
008200        05 FILLER    PICTURE X(8)  VALUE '   10   '.              T3090   
008210        05 FILLER    PICTURE X(8)  VALUE '   11   '.              T3090   
008220        05 FILLER    PICTURE X(8)  VALUE '   12   '.              T3090   
008230        05 FILLER    PICTURE X(8)  VALUE '   13   '.              T3090   
008240     03 T10-PED-ACT-ENTRY REDEFINES T10-PED-ACT-TAB               T3090   
008250            OCCURS 13 TIMES  PICTURE X(8).                        T3090   
008260 01  ITEM-8-LEG-TABLE.                                            T3090   
008270     03 ITEM-8-L-TAB.                                             T3090   
008280        05 FILLER    PICTURE X(21) VALUE '2 LEGS (L)           '. T3090   
008290        05 FILLER    PICTURE X(21) VALUE '3 LEGS (T)           '. T3090   
008300        05 FILLER    PICTURE X(21) VALUE '3 LEGS (Y)           '. T3090   
008310        05 FILLER    PICTURE X(21) VALUE '4 LEGS (+)           '. T3090   
008320        05 FILLER    PICTURE X(21) VALUE '4 LEG JOGGED         '. T3090   
008330        05 FILLER    PICTURE X(21) VALUE '4 LEG NON-STANDARD   '. T3090   
008340        05 FILLER    PICTURE X(21) VALUE 'MORE THAN 4 LEGS     '. T3090   
008350        05 FILLER    PICTURE X(21) VALUE 'DEAD END OR CITY LINE'. T3090   
008360        05 FILLER    PICTURE X(21) VALUE 'GRADE SEPARATION     '. T3090   
008370     03 ITEM-8-L-ENTRY REDEFINES ITEM-8-L-TAB                     T3090   
008380                     OCCURS  9 TIMES PICTURE X(21).               T3090   
008390 01  ITEM-8-IS-TABLE.                                             T3090   
008400     03 ITEM-8-I-TAB.                                             T3090   
008410        05 FILLER    PICTURE X(21) VALUE 'NONE                 '. T3090   
008420         05 FILLER   PICTURE X(21) VALUE 'SIGNAL               '. T3090   
008430        05 FILLER    PICTURE X(21) VALUE 'STOP SIGNS - PARTIAL '. T3090   
008440        05 FILLER    PICTURE X(21) VALUE 'STOP SIGNS - ALL LEGS'. T3090   
008450        05 FILLER    PICTURE X(21) VALUE 'YIELD SIGN           '. T3090   
008460     03 ITEM-8-I-ENTRY REDEFINES ITEM-8-I-TAB                     T3090   
008470                     OCCURS 5 TIMES PICTURE X(21).                T3090   
008480 01  ITEM-8-SIG-TABLE.                                            T3090   
008490     03 ITEM-8-S-TAB.                                             T3090   
008500        05 FILLER    PICTURE X(21) VALUE 'NON-SIGNALIZED       '. T3090   
008510        05 FILLER    PICTURE X(21) VALUE 'FIXED TIME           '. T3090   
008520        05 FILLER    PICTURE X(21) VALUE 'SEMI ACTUATED        '. T3090   
008530        05 FILLER    PICTURE X(21) VALUE 'FULLY ACTUATED       '. T3090   
008540         05 FILLER   PICTURE X(21) VALUE 'OTHER                '. T3090   
008550     03 ITEM-8-S-ENTRY REDEFINES ITEM-8-S-TAB                     T3090   
008560                     OCCURS 5  TIMES PICTURE X(21).               T3090   
008570 01  ITEM-8-APP-TABLE.                                            T3090   
008580     03 ITEM-8-A-TAB.                                             T3090   
008590        05 FILLER    PICTURE X(21) VALUE 'NO. THRU APPR. LANES:'. T3090   
008600        05 FILLER    PICTURE X(21) VALUE 'TYPE OF TURN LANE:   '. T3090   
008610        05 FILLER    PICTURE X(21) VALUE 'TYPE OF CONTROL SIGN:'. T3090   
008620        05 FILLER    PICTURE X(21) VALUE 'SIZE OF MAST ARM IND:'. T3090   
008630        05 FILLER    PICTURE X(21) VALUE 'TURN SIGNAL CONTROL: '. T3090   
008640        05 FILLER    PICTURE X(21) VALUE 'LT PERM. SIGN REG.:  '. T3090   
008650        05 FILLER    PICTURE X(21) VALUE 'LT PEDESTAL SIGN REG:'. T3090   
008660        05 FILLER    PICTURE X(21) VALUE 'BUS ZONE:            '. T3090   
008670        05 FILLER    PICTURE X(21) VALUE 'PED SIG. ACROSS APPR:'. T3090   
008680        05 FILLER    PICTURE X(21) VALUE 'MEDIAN TYPE:         '. T3090   
008690        05 FILLER    PICTURE X(21) VALUE 'MEDIAN WIDTH:        '. T3090   
008700        05 FILLER    PICTURE X(21) VALUE 'PARK./STOP. RESTRICT:'. T3090   
008710        05 FILLER    PICTURE X(21) VALUE 'DIRECTION FLOW:      '. T3090   
008720        05 FILLER    PICTURE X(21) VALUE 'SEC. SIGNAL LOCATION:'. T3090   
008730     03 ITEM-8-A-ENTRY REDEFINES ITEM-8-A-TAB                     T3090   
008740                     OCCURS 14 TIMES PICTURE X(21).               T3090   
008750 01  T10-SUM-WORK.                                                T3090   
008760       02 T10-SUM-LINEA-WORK.                                     T3090   
008770     03 T10-SUM-LINEA OCCURS 9 TIMES.                             T3090   
008780         05 T10-LINEA-FLD OCCURS 10 TIMES PICTURE 9999.           T3090   
008790       02 T10-SUM-LINEB-WORK.                                     T3090   
008800     03 T10-SUM-LINEB OCCURS 7 TIMES.                             T3090   
008810     05  T10-LINEB-FLD OCCURS 3 TIMES PICTURE 999.                T3090   
008820        05 T10-V1.                                                T3090   
008830           07 T10-V1A        PICTURE X.                           T3090   
008840           07 T10-V1B        PICTURE X.                           T3090   
008850           07 T10-V1C        PICTURE X.                           T3090   
008860        05 T10-V2            PICTURE XX.                          T3090   
008870         05 T10-V3           PICTURE 9(5).                        T3090   
008880        05 T10-V3A           PICTURE XX.                          T3090   
008890        05 T10-V4-M          PICTURE XX.                          T3090   
008900        05 T10-V4-D          PICTURE XX.                          T3090   
008910        05 T10-V4-Y          PICTURE XX.                          T3090   
008920       02 T10-SUM-LINEC-WORK.                                     T3090   
008930     03 T10-SUM-LINEC OCCURS 4 TIMES.                             T3090   
008940         05 T10-LINEC-C.                                          T3090   
008950           07 LINEC-MEAN     PICTURE 99V999.                              
008960           07 LINEC-DEV      PICTURE 99V999.                              
008970           07 LINEC-CVA      PICTURE 99V999.                              
008980     03 T10-SUM-LNEC1-C.                                          T3090   
008990     05 T10-SUM-LINEC1 OCCURS 5 TIMES                             T3090   
009000                             PICTURE 99.                          T3090   
009010     03 T10-SUM-LINEC2.                                           T3090   
009020        04 FILLER OCCURS 4 TIMES.                                 T3090   
009030        05 T10-S-LINEC3 OCCURS 14 TIMES PICTURE X.                T3090   
009040 01  ITEM-8-CLASS-TABLE.                                          T3090   
009050     02 ITEM-8-CLASS-TAB.                                         T3090   
009060     03 FILLER   PICTURE X(21) VALUE '    LOCAL - LOCAL    '.     T3090   
009070     03 FILLER   PICTURE X(21) VALUE 'COLLECTOR - LOCAL    '.     T3090   
009080     03 FILLER   PICTURE X(21) VALUE 'COLLECTOR - COLLECTOR'.     T3090   
009090     03 FILLER   PICTURE X(21) VALUE 'MINOR ART - LOCAL    '.     T3090   
009100     03 FILLER   PICTURE X(21) VALUE 'MINOR ART - COLLECTOR'.     T3090   
009110     03 FILLER   PICTURE X(21) VALUE 'MINOR ART - MINOR ART'.     T3090   
009120     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - LOCAL    '.     T3090   
009130     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - COLLECTOR'.     T3090   
009140     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - MINOR ART'.     T3090   
009150     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - PRIM. ART'.     T3090   
009160     02 ITEM-8-C-ENTRY REDEFINES ITEM-8-CLASS-TAB                 T3090   
009170                      OCCURS 10 TIMES PICTURE X(21).              T3090   
009180 01  MONTH-TABLE.                                                 T3090   
009190     03 FILLER       PICTURE 999 VALUE 031.                       T3090   
009200     03 FILLER       PICTURE 999 VALUE 059.                       T3090   
009210     03 FILLER       PICTURE 999 VALUE 090.                       T3090   
009220     03 FILLER       PICTURE 999 VALUE 120.                       T3090   
009230     03 FILLER       PICTURE 999 VALUE 151.                       T3090   
009240     03 FILLER       PICTURE 999 VALUE 181.                       T3090   
009250     03 FILLER       PICTURE 999 VALUE 212.                       T3090   
009260     03 FILLER       PICTURE 999 VALUE 243.                       T3090   
009270     03 FILLER       PICTURE 999 VALUE 273.                       T3090   
009280     03 FILLER       PICTURE 999 VALUE 304.                       T3090   
009290     03 FILLER       PICTURE 999 VALUE 334.                       T3090   
009300 01  FILLER REDEFINES MONTH-TABLE.                                T3090   
009310     03 MO-ENTRY OCCURS 11 TIMES PICTURE 999.                     T3090   
009320 01  T-D-HOLD        PICTURE X(12).                               T3090   
009330 01  FILLER REDEFINES T-D-HOLD.                                   T3090   
009340     03 C-M          PICTURE 99.                                  T3090   
009350     03 C-D          PICTURE 99.                                  T3090   
009360     03 C-Y          PICTURE 99.                                  T3090   
009370     03 C-M1         PICTURE 99.                                  T3090   
009380     03 C-D1         PICTURE 99.                                  T3090   
009390     03 C-Y1         PICTURE 99.                                  T3090   
009400 01  D-TOTAL.                                                     T3090   
009410     03 TO-D-TOT     PICTURE 9999.                                T3090   
009420     03 FR-D-TOT     PICTURE 9999.                                T3090   
009430     03 YR-D-TOT     PICTURE 9999.                                T3090   
009440     03 TND          PICTURE 9999.                                T3090   
009450     03 MO-CT        PICTURE 99.                                  T3090   
009460 01  STR-SUM.                                                     T3090   
009470     03 IF-AC-SM             PICTURE 999 VALUE ZERO.              T3090   
009480     03 T-AC-SM              PICTURE 999 VALUE ZERO.              T3090   
009490 01   DIR-VEH-C          PICTURE 9 VALUE ZERO.                    T3090   
009500 01  DIR-VEH-HOLD    PICTURE 9 VALUE ZERO.                        T3090   
009510 01  SUPL-RD-ON      PICTURE 9 VALUE ZERO.                        T3090   
009520 01  DIR-ST-SW       PICTURE 9 VALUE ZERO.                        T3090   
009522 01  DATE-2YRS       PICTURE 9(6).                                        
009523 01  FILLER REDEFINES DATE-2YRS.                                          
009524     03 YRS-2        PICTURE 99.                                          
009525     03 MOS-2        PICTURE 99.                                          
009526     03 DAS-2        PICTURE 99.                                          
009530 PROCEDURE DIVISION.                                              T3090   
009540     ENTER LINKAGE.                                               T3090   
009550     CALL 'TODAY' USING COMPUTERS-DATE.                           KM120870
009560     ENTER COBOL.                                                 T3090   
009562     MOVE MO TO HDR5-R-MO.                                        KM021971
009564     MOVE DA TO HDR5-R-DA.                                        KM021971
009566     MOVE YR TO HDR5-R-YR.                                        KM021971
009570     OPEN INPUT  REPORT-FILE                                      KM120171
009580                 VOLFILE                                          KM120171
009590                 ISFILE                                           KM120171
009595                 STRAT-FILE                                       KM120171
009600                 JURIS-FILE                                       KM120171
009610          OUTPUT PRINT-FILE.                                      KM120171
009615     PERFORM LOAD-NAME-TABLE THRU LNT-EXIT.                       KM120171
009620 START-PROCESSING.                                                T3090   
009630     READ REPORT-FILE AT END GO TO EOJ.                           T3090   
009640 PROCESS-REPORT.                                                  T3090   
009650     IF REPORT-CODE > 10 GO TO EOJ.                               T3090   
009660     IF REPORT-CODE = 10 PERFORM PRINT-10 THRU PRINT-10-X.        T3090   
009670 READ-NEXT-REC.                                                   T3090   
009680     GO TO START-PROCESSING.                                      T3090   
009690 EOJ.                                                             T3090   
009700     IF 10-SW = 1 MOVE ZERO TO 10-SW MOVE 2 TO SW10A              T3090   
009710         ADD 1 TO PAGE-NO PERFORM SUM-10 THRU SUM-10-X.           T3090   
009720     MOVE SPACES TO PRINT-LINE.                                   T3090   
009730     WRITE PRINT-REC AFTER ADVANCING 0 LINES.                     T3090   
009740     DISPLAY 'T3090 COMPLETED NORMALLY'.                          T3090   
009750     CLOSE REPORT-FILE                                            KM120171
009760           PRINT-FILE                                             KM120171
009770           VOLFILE                                                KM120171
009780           ISFILE                                                 KM120171
009790           STRAT-FILE                                             KM120171
009795           JURIS-FILE.                                            KM120171
009800     STOP RUN.                                                    T3090   
009810 PRINT-10.                                                        T3090   
009820     IF SW-10 NOT = 1 GO TO T10B. MOVE ZERO TO SW-10.             T3090   
009830         MOVE 1 TO PAGE-NO MOVE 10 TO HDR-CTR.                    T3090   
009840         MOVE REQ-NO TO T10-REQ-HOLD. PERFORM INIT THRU INIT-X.   T3090   
009850 T10A.   MOVE 1 TO SW10A MOVE 0 TO LINE-NO.                       T3090   
009860         MOVE T10-IS-CODE TO T10-IS-HOLD.                         T3090   
009870     PERFORM HEADER THRU HEADER-X.                                T3090   
009880     MOVE T10-SEARCH-DATES TO T-D-HOLD.                           T3090   
009890     PERFORM NO-D-RTN THRU NO-D-RTN-X.                            T3090   
009900     MOVE 1 TO 10-SW.                                             T3090   
009910 T10B.                                                            T3090   
009920     IF T10-IS-CODE = T10-IS-HOLD GO TO TB.                       T3090   
009930 TA. MOVE 2 TO SW10A ADD 1 TO  PAGE-NO.                           T3090   
009940     PERFORM SUM-10 THRU SUM-10-X.                                T3090   
009950     IF REQ-NO = T10-REQ-HOLD ADD 1 TO PAGE-NO GO TO T10A.        T3090   
009960       MOVE REQ-NO TO T10-REQ-HOLD MOVE 1 TO PAGE-NO GO TO T10A.  T3090   
009970 TB. IF REQ-NO = T10-REQ-HOLD GO TO T10C ELSE GO TO TA.           T3090   
009980 T10C.                                                            T3090   
009990     IF LINE-NO > 40 ADD 1 TO PAGE-NO                             T3090   
010000         MOVE 1 TO SW10A PERFORM HEADER THRU HEADER-X.            T3090   
010010     PERFORM T10-DET THRU T10-DET-X.                              T3090   
010020 PRINT-10-X. EXIT.                                                T3090   
010030 T10-DET.                                                         T3090   
010040     MOVE 0 TO PRTY-CTR. MOVE SPACES TO PRINT-LINE, T10-DET-P-HOLDT3090   
010050     MOVE T10-CLASS (1) TO T10-CLASS1.                            T3090   
010060     MOVE '-' TO P-T10-1, P-T10-2  MOVE ':' TO P-T10-3.           T3090   
010070     IF DOW > '0' AND DOW < '8' MOVE T10-DAY-ENTRY (D-O-W)        KM120870
010080         TO P-T10-DAY ELSE MOVE SPACES TO P-T10-DAY.              T3090   
010090     MOVE DIR-ANL   TO DIR-ANL1   MOVE PRI-CODE TO PRI-CODE1.     T3090   
010100     MOVE SPEC-CIR  TO SPEC-CIR1.                                 T3090   
010110     MOVE T10-DR-NO TO T10-DR-NO1 MOVE T10-MO TO T10-MO1.         T3090   
010120     MOVE T10-DA    TO T10-DA1    MOVE T10-YR TO T10-YR1.         T3090   
010130     MOVE T10-HR    TO T10-HR1    MOVE T10-M  TO T10-M1.          T3090   
010140     IF WEATHER = 1 MOVE 'C' TO P-WEATHER.                        T3090   
010150     IF WEATHER = 2 MOVE 'R' TO P-WEATHER.                        T3090   
010160     IF WEATHER = 3 MOVE 'F' TO P-WEATHER.                        T3090   
010170     IF WEATHER = 4 MOVE 'O' TO P-WEATHER.                        T3090   
010180     IF LIGHT   = 1 MOVE ' DAY ' TO P-LIGHT MOVE SPACES TO        T3090   
010190         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (1, 3).          T3090   
010200     IF LIGHT   = 2 MOVE 'DUSK/' TO P-LIGHT MOVE 'DAWN ' TO       T3090   
010210         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (2, 3).          T3090   
010220     IF LIGHT = 3, MOVE 'DARK ' TO P-LIGHT, MOVE '     ' TO       T3090   
010230         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (3, 3).          T3090   
010240     IF LIGHT = 4, MOVE 'DARK/' TO P-LIGHT, MOVE 'ST LT' TO       T3090   
010250         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (3, 3).          T3090   
010260     IF LIGHT = 5, MOVE ' DAY ' TO P-LIGHT, MOVE 'CLOUD' TO       T3090   
010270         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (4, 3).          T3090   
010280         ADD 1 TO T10-LINEB-FLD (5, 3).                           T3090   
010290 T0. IF RD-CONDITION = 1 MOVE 'D' TO P-RD-CND.                    T3090   
010300     IF RD-CONDITION = 2 MOVE 'W' TO P-RD-CND.                    T3090   
010310     IF RD-CONDITION = 3 MOVE 'I' TO P-RD-CND.                    T3090   
010320     IF RD-CONDITION = 4 MOVE 'O' TO P-RD-CND.                    T3090   
010330 T1. MOVE T10-AGE (1) TO T10-AGE1.                                T3090   
010340     IF PED-ACTION (1) NOT = ZERO MOVE 'PED ' TO P-VEH-TYPE ELSE  T3090   
010350     IF VEH-TYPE (1) < 21 ADD 1 TO VEH-TYPE (1) MOVE VEH-TYPE (1) T3090   
010360     TO SUBSCRIP-H-2 MOVE T10-V-ENTRY (SUBSCRIP-H-2) TO           T3090   
010370     P-VEH-TYPE.                                                  T3090   
010380     MOVE T10-COND (1) TO T10-COND1.                              T3090   
010390     IF CONTRIB-CIR (1) > 00 AND CONTRIB-CIR (1) < 13 MOVE        T3090   
010400     CONTRIB-CIR (1) TO SUBSCRIP-H-2 MOVE T10-CIR-ENTRY           T3090   
010410     (SUBSCRIP-H-2) TO P-CONTRI-CIR.                              T3090   
010420 T2. IF GP-CODE > '00' AND GP-CODE < '18' MOVE GP-LINE1           T3090   
010430         (GROUP-CODE) TO P-GP-CODE MOVE GP-LINE2 (GROUP-CODE)     T3090   
010440         TO T10LINE2-PR-GP-H.                                     T3090   
010450     MOVE T10-INJ-A TO T10-INJ-A1 MOVE T10-INJ-B TO T10-INJ-B1.   T3090   
010460     MOVE T10-INJ-C TO T10-INJ-C1 MOVE T10-INJ-K TO T10-INJ-K1.   T3090   
010470     IF T10-A-SEV = 1 ADD 1 TO T10-LINEB-FLD (3, 1).              T3090   
010480     IF T10-A-SEV = 2 ADD 1 TO T10-LINEB-FLD (4, 1).              T3090   
010490     IF T10-A-SEV = 3 ADD 1 TO T10-LINEB-FLD (5, 1).              T3090   
010500     IF T10-A-SEV = 4 ADD 1 TO T10-LINEB-FLD (1, 1).              T3090   
010510     IF T10-A-SEV = 5 ADD 1 TO T10-LINEB-FLD (6, 1).              T3090   
010520                      ADD T10-INJ-A TO T10-LINEB-FLD (3, 2).      T3090   
010530                      ADD T10-INJ-B TO T10-LINEB-FLD (4, 2).      T3090   
010540                      ADD T10-INJ-C TO T10-LINEB-FLD (5, 2).      T3090   
010550                      ADD T10-INJ-K TO T10-LINEB-FLD (1, 2).      T3090   
010560      ADD 1 TO T10-LINEB-FLD (7, 1)  ADD T10-INJ-A T10-INJ-B      T3090   
010570      T10-INJ-C T10-INJ-K TO T10-LINEB-FLD (7, 2).                T3090   
010580 T3. IF PED-ACTION (1) NOT = 00 AND PED-ACTION (1) < 14           T3090   
010590         MOVE PED-ACTION (1) TO SUBSCRIP-H-2                      T3090   
010600        MOVE T10-PED-ACT-ENTRY (SUBSCRIP-H-2) TO P-10-ACTION      T3090   
010610        ELSE IF PED-ACTION (1) = 00 AND VEH-ACTION (1) > 00 AND   T3090   
010620        VEH-ACTION (1) < 14 MOVE VEH-ACTION (1) TO SUBSCRIP-H-2   T3090   
010630       MOVE T10-V-ACT-ENTRY (SUBSCRIP-H-2) TO P-10-ACTION.        T3090   
010640 T4. IF T10-ACC-TYPE > 00 AND T10-ACC-TYPE < 37                   T3090   
010650         MOVE T10-A-TYPE-ENTRY (T10-ACC-TYPE) TO P-ACC-TYPE.      T3090   
010660     IF PRI-CODE-1 NOT = 9 GO TO NEW-IMPACT-RTN.                  T3090   
010670     IF INTERSECTION NOT NUMERIC MOVE   ZERO TO INTERSECTION.     T3090   
010680     IF DIRECTION-1  NOT NUMERIC MOVE   ZERO TO DIRECTION-1.      T3090   
010690     IF DIRECTION-2  NOT NUMERIC MOVE   ZERO TO DIRECTION-2.      T3090   
010700     IF INTERSECTION = 0 MOVE 'I/S ' TO IMPACT-1 MOVE 1 TO        T3090   
010710         INTER-SWITCH ELSE MOVE 0 TO INTER-SWITCH GO TO NON-INTER.T3090   
010720     MOVE SPACES TO DIRECTION-HOLD.                               T3090   
010730     IF DIRECTION-2 = 5 MOVE 'PED ' TO DIRECTION-HOLD.            T3090   
010740     IF T10-REVERSE-ST-C = 1 GO TO INTER-A.                       T3090   
010750     IF DIRECTION-1 = 1  MOVE 'NB-2'  TO IMPACT-2.                T3090   
010760     IF DIRECTION-1 = 2  MOVE 'EB-1'  TO IMPACT-2.                T3090   
010770     IF DIRECTION-1 = 3  MOVE 'SB-2'  TO IMPACT-2.                T3090   
010780     IF DIRECTION-1 = 4  MOVE 'WB-1'  TO IMPACT-2.                T3090   
010790     IF DIRECTION-2 = 1  MOVE 'NB-2'  TO DIRECTION-HOLD.          T3090   
010800     IF DIRECTION-2 = 2  MOVE 'EB-1'  TO DIRECTION-HOLD.          T3090   
010810     IF DIRECTION-2 = 3  MOVE 'SB-2'  TO DIRECTION-HOLD.          T3090   
010820     IF DIRECTION-2 = 4  MOVE 'WB-1'  TO DIRECTION-HOLD.          T3090   
010830     GO TO INTER-B.                                               T3090   
010840 INTER-A.                                                         T3090   
010850     IF DIRECTION-1 = 1 MOVE 'NB-1' TO IMPACT-2.                  T3090   
010860     IF DIRECTION-1 = 2 MOVE 'EB-2' TO IMPACT-2.                  T3090   
010870     IF DIRECTION-1 = 3 MOVE 'SB-1' TO IMPACT-2.                  T3090   
010880     IF DIRECTION-1 = 4 MOVE 'WB-2' TO IMPACT-2.                  T3090   
010890     IF DIRECTION-2 = 1 MOVE 'NB-1' TO DIRECTION-HOLD.            T3090   
010900     IF DIRECTION-2 = 2 MOVE 'EB-2' TO DIRECTION-HOLD.            T3090   
010910     IF DIRECTION-2 = 3 MOVE 'SB-1' TO DIRECTION-HOLD.            T3090   
010920     IF DIRECTION-2 = 4 MOVE 'WB-2' TO DIRECTION-HOLD.            T3090   
010930 INTER-B.                                                         T3090   
010940     IF DIRECTION-HOLD = SPACE GO TO ACCIDENT-TYPE ELSE IF        T3090   
010950     DIRECTION-1 = DIRECTION-2 NEXT SENTENCE ELSE GO TO           T3090   
010960     ACCIDENT-TYPE.                                               T3090   
010970     IF T10-REVERSE-ST-C = 1 GO TO INTER-C.                       T3090   
010980     IF DIRECTION-2 = 4 MOVE 'WB-1' TO DIRECTION-HOLD.            T3090   
010990     IF DIRECTION-2 = 1 MOVE 'NB-2' TO DIRECTION-HOLD.            T3090   
011000     IF DIRECTION-2 = 2 MOVE 'EB-1' TO DIRECTION-HOLD.            T3090   
011010     IF DIRECTION-2 = 3 MOVE 'SB-2' TO DIRECTION-HOLD.            T3090   
011020     GO TO ACCIDENT-TYPE.                                         T3090   
011030 INTER-C.                                                         T3090   
011040     IF DIRECTION-2 = 1 MOVE 'NB-1' TO DIRECTION-HOLD.            T3090   
011050     IF DIRECTION-2 = 2 MOVE 'EB-2' TO DIRECTION-HOLD.            T3090   
011060     IF DIRECTION-2 = 3 MOVE 'SB-1' TO DIRECTION-HOLD.            T3090   
011070     IF DIRECTION-2 = 4 MOVE 'WB-2' TO DIRECTION-HOLD.            T3090   
011080     GO TO ACCIDENT-TYPE.                                         T3090   
011090 NON-INTER.                                                       T3090   
011100     IF INTERSECTION = 1 MOVE ' PED' TO IMPACT-1.                 T3090   
011110     IF INTERSECTION = 2 MOVE ' DEP' TO IMPACT-1                  T3090   
011120         MOVE 'N/E' TO IMPACT-2.                                  T3090   
011130     IF INTERSECTION = 3 MOVE ' APP' TO IMPACT-1                  T3090   
011140         MOVE 'S/W' TO IMPACT-2.                                  T3090   
011150     IF INTERSECTION = 4 MOVE ' DEP' TO IMPACT-1                  T3090   
011160         MOVE 'S/W' TO IMPACT-2.                                  T3090   
011170     IF INTERSECTION = 5 MOVE ' APP' TO IMPACT-1                  T3090   
011180         MOVE 'N/E' TO IMPACT-2.                                  T3090   
011190     IF INTERSECTION = 6 MOVE '  HO' TO IMPACT-1                  T3090   
011200         MOVE 'N/E' TO IMPACT-2.                                  T3090   
011210     IF INTERSECTION = 7 MOVE '  HO' TO IMPACT-1                  T3090   
011220         MOVE 'S/W' TO IMPACT-2.                                  T3090   
011230     IF INTERSECTION = 8 OR INTERSECTION = 9 MOVE ' OTHER' TO     T3090   
011240         P-PT-IMPACT.                                             T3090   
011250     IF T10-REVERSE-ST-C = 0 MOVE ' 1ST' TO DIRECTION-HOLD ELSE   T3090   
011260     MOVE ' 2ND' TO DIRECTION-HOLD.                               T3090   
011270     IF DIRECTION-2 = ZERO AND DIRECTION-1 = ZERO NEXT SENTENCE   T3090   
011280     ELSE GO TO NON-INTER-B.                                      T3090   
011290     IF T10-REVERSE-ST-C = 0 GO TO NON-INTER-A.                   T3090   
011300     IF INTERSECTION = 2  ADD 1 TO T10-LINEA-FLD (4, 7).          T3090   
011310     IF INTERSECTION = 3  ADD 1 TO T10-LINEA-FLD (5, 3).          T3090   
011320     IF INTERSECTION = 4  ADD 1 TO T10-LINEA-FLD (5, 7).          T3090   
011330     IF INTERSECTION = 5  ADD 1  TO T10-LINEA-FLD (4, 3).         T3090   
011340     IF INTERSECTION = 6  ADD 1  TO T10-LINEA-FLD (4, 7).         T3090   
011350     IF INTERSECTION = 7  ADD 1  TO T10-LINEA-FLD (5, 3).         T3090   
011360     GO TO ACCIDENT-TYPE.                                         T3090   
011370 NON-INTER-A.                                                     T3090   
011380     IF INTERSECTION = 2  ADD 1  TO T10-LINEA-FLD (2, 7).         T3090   
011390     IF INTERSECTION = 3  ADD 1  TO T10-LINEA-FLD (3, 3).         T3090   
011400     IF INTERSECTION = 4  ADD 1  TO T10-LINEA-FLD (3, 7).         T3090   
011410     IF INTERSECTION = 5  ADD 1  TO T10-LINEA-FLD (2, 3).         T3090   
011420     IF INTERSECTION = 6  ADD 1  TO T10-LINEA-FLD (2, 7).         T3090   
011430     IF INTERSECTION = 7  ADD 1  TO T10-LINEA-FLD (3, 3).         T3090   
011440     GO TO ACCIDENT-TYPE.                                         T3090   
011450 NON-INTER-B.                                                     T3090   
011460     IF DIRECTION-1 = ZERO AND DIRECTION-2 = 1 NEXT SENTENCE ELSE T3090   
011470     GO TO NON-INTER-D.                                           T3090   
011480     IF T10-REVERSE-ST-C = 0 GO TO NON-INTER-C.                   T3090   
011490     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (4, 8).           T3090   
011500     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (5, 4).           T3090   
011510     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (5, 8).           T3090   
011520     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (4, 4).           T3090   
011530     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (4, 8).           T3090   
011540     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (5, 8).           T3090   
011550     GO TO ACCIDENT-TYPE.                                         T3090   
011560 NON-INTER-C.                                                     T3090   
011570     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (2, 8).           T3090   
011580     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (3, 4).           T3090   
011590     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (3, 8).           T3090   
011600     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (2, 4).           T3090   
011610     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (2, 8).           T3090   
011620     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (3, 4).           T3090   
011630     GO TO ACCIDENT-TYPE.                                         T3090   
011640 NON-INTER-D.                                                     T3090   
011650     IF T10-REVERSE-ST-C = 0 GO TO NON-INTER-E.                   T3090   
011660     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (4, 9).           T3090   
011670     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (5, 5).           T3090   
011680     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (5, 9).           T3090   
011690     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (4, 5).           T3090   
011700     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (4, 9).           T3090   
011710     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (5, 5).           T3090   
011720     GO TO ACCIDENT-TYPE.                                         T3090   
011730 NON-INTER-E.                                                     T3090   
011740     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (2, 9).           T3090   
011750     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (3, 5).           T3090   
011760     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (2, 5).           T3090   
011770     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (2, 9).           T3090   
011780     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (3, 9).           T3090   
011790     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (3, 5).           T3090   
011800     GO TO ACCIDENT-TYPE.                                         T3090   
011810 NEW-IMPACT-RTN.                                                  T3090   
011820     IF T10-AT-IS = 1 MOVE 'YES' TO P-10-IS ELSE                  T3090   
011830     MOVE ' NO' TO P-10-IS.                                       T3090   
011840     IF T10-REVERSE-ST-C NOT = 0 GO TO NEW-IMP-RTN-R.             T3090   
011850         MOVE LOCAT-CODE TO INTER-PLUS-1.                         T3090   
011860         IF PRI-CODE-1 > 0 AND PRI-CODE-1 < 5 MOVE                T3090   
011870             DIR-ENTRY (PRI-CODE-1) TO NEW-IMP-2 ELSE MOVE SPACE  T3090   
011880             TO NEW-IMP-2. MOVE SPACE TO NEW-IMP-1.               T3090   
011890     IF PRI-CODE-2 > 0 AND PRI-CODE-2 < 5 MOVE                    T3090   
011900             DIR-ENTRY (PRI-CODE-2) TO NEW-IMP-4 ELSE MOVE SPACE  T3090   
011910             TO NEW-IMP-4. MOVE '/' TO NEW-IMP-3.                 T3090   
011920         MOVE SEC-FEET TO IMPCT-H.                                T3090   
011930         IF SEC-CODE-1 > 0 AND SEC-CODE-1 < 5 MOVE                T3090   
011940            DIR-ENTRY (SEC-CODE-1) TO IMPCT-H-2 ELSE MOVE SPACE   T3090   
011950             TO IMPCT-H-2. MOVE SPACE TO IMPCT-H-1.               T3090   
011960         IF SEC-CODE-2 > 0 AND SEC-CODE-2 < 5 MOVE                T3090   
011970             DIR-ENTRY (SEC-CODE-2) TO IMPCT-H-4 ELSE MOVE SPACE  T3090   
011980             TO IMPCT-H-4. MOVE    '/' TO IMPCT-H-3.              T3090   
011990     IF T10-AT-IS = 1 GO TO A-PIS.                                T3090   
012000     GO TO A-PX.                                                  T3090   
012010 NEW-IMP-RTN-R.                                                   T3090   
012020     MOVE SEC-FEET TO INTER-PLUS-1.                               T3090   
012030         MOVE LOCAT-CODE TO IMPCT-H.                              T3090   
012040     MOVE SPACE TO NEW-IMP-1, IMPCT-H-1.                          T3090   
012050     MOVE '/' TO NEW-IMP-3, IMPCT-H-3.                            T3090   
012060     IF SEC-CODE-1 > 0 AND SEC-CODE-1 < 5                         T3090   
012070         MOVE DIR-ENTRY (SEC-CODE-1) TO NEW-IMP-2 ELSE MOVE SPACE T3090   
012080             TO NEW-IMP-2.                                        T3090   
012090     IF SEC-CODE-2 > 0 AND SEC-CODE-2 < 5                         T3090   
012100         MOVE DIR-ENTRY (SEC-CODE-2) TO NEW-IMP-4 ELSE MOVE SPACE T3090   
012110             TO NEW-IMP-4.                                        T3090   
012120     IF PRI-CODE-1 > 0 AND PRI-CODE-1 < 5                         T3090   
012130         MOVE DIR-ENTRY (PRI-CODE-1) TO IMPCT-H-2 ELSE MOVE SPACE T3090   
012140             TO IMPCT-H-2.                                        T3090   
012150     IF PRI-CODE-2 > 0 AND PRI-CODE-2 < 5                         T3090   
012160         MOVE DIR-ENTRY (PRI-CODE-2) TO IMPCT-H-4 ELSE MOVE SPACE T3090   
012170         TO IMPCT-H-4. IF T10-AT-IS = 1 GO TO A-PIS.              T3090   
012180 A-PX.                                                            T3090   
012190     PERFORM ST-ACC-ON THRU ST-ACC-ON-X.                          T3090   
012200     IF T10-REVERSE-ST-C = 1 GO TO REV-A-D.                       T3090   
012210     IF DIR-ANL NOT = 300 GO TO X9.                               T3090   
012220     IF SEC-FEET < 101 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)     T3090   
012230         ADD 1 TO T10-LINEA-FLD (2 7) GO TO A-P.                  T3090   
012240     IF SEC-FEET < 101 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)     T3090   
012250         ADD 1 TO T10-LINEA-FLD (3 7) GO TO A-P.                  T3090   
012260     IF SEC-FEET < 201 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)     T3090   
012270         ADD 1 TO T10-LINEA-FLD (2 8) GO TO A-P.                  T3090   
012280     IF SEC-FEET < 201 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)     T3090   
012290         ADD 1 TO T10-LINEA-FLD (3 8) GO TO A-P.                  T3090   
012300     IF SEC-FEET > 200 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)     T3090   
012310         ADD 1 TO T10-LINEA-FLD (2 9) GO TO A-P.                  T3090   
012320     IF SEC-FEET > 200 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)     T3090   
012330         ADD 1 TO T10-LINEA-FLD (3 9) GO TO A-P.                  T3090   
012340 X9. IF T10-DIR (1) = ZERO GO TO A-P.                             T3090   
012350     IF SEC-CODE-1 < 1 OR SEC-CODE-1 > 4 GO TO A-P.               T3090   
012360     IF SEC-FEET > 100 GO TO X9-A.                                T3090   
012370     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012380         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012390         ADD 1 TO T10-LINEA-FLD (2 7)    GO TO A-P.               T3090   
012400     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012410         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012420         ADD 1 TO T10-LINEA-FLD (3 7)    GO TO A-P.               T3090   
012430     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
012440         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012450         ADD 1 TO T10-LINEA-FLD (2 3)    GO TO A-P.               T3090   
012460     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
012470         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012480         ADD 1 TO T10-LINEA-FLD (3 3)    GO TO A-P.               T3090   
012490 X9-A.                                                            T3090   
012500     IF SEC-FEET > 200 GO TO S2.                                  T3090   
012510     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012520         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012530         ADD 1 TO T10-LINEA-FLD (2 8)    GO TO A-P.               T3090   
012540     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012550         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012560         ADD 1 TO T10-LINEA-FLD (3 8)    GO TO A-P.               T3090   
012570     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
012580         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012590         ADD 1 TO T10-LINEA-FLD (2 4)    GO TO A-P.               T3090   
012600     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
012610         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012620         ADD 1 TO T10-LINEA-FLD (3 4)    GO TO A-P.               T3090   
012630 S2.                                                              T3090   
012640     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012650         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012660         ADD 1 TO T10-LINEA-FLD (2 9)    GO TO A-P.               T3090   
012670     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012680         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012690         ADD 1 TO T10-LINEA-FLD (3 9)    GO TO A-P.               T3090   
012700     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
012710         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012720         ADD 1 TO T10-LINEA-FLD (2 5)    GO TO A-P.               T3090   
012730     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
012740         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012750         ADD 1 TO T10-LINEA-FLD (3 5)    GO TO A-P.               T3090   
012760 REV-A-D.                                                         T3090   
012770     IF DIR-ANL NOT = 300 GO TO REV-CONT.                         T3090   
012780     IF SEC-FEET < 101 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)     T3090   
012790         ADD 1 TO T10-LINEA-FLD (4 7) GO TO A-P.                  T3090   
012800     IF SEC-FEET < 101 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)     T3090   
012810         ADD 1 TO T10-LINEA-FLD (5 7) GO TO A-P.                  T3090   
012820     IF SEC-FEET < 201 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)     T3090   
012830         ADD 1 TO T10-LINEA-FLD (4 8) GO TO A-P.                  T3090   
012840     IF SEC-FEET < 201 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)     T3090   
012850         ADD 1 TO T10-LINEA-FLD (5 8) GO TO A-P.                  T3090   
012860     IF SEC-FEET > 200 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)     T3090   
012870         ADD 1 TO T10-LINEA-FLD (4 9) GO TO A-P.                  T3090   
012880     IF SEC-FEET > 200 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)     T3090   
012890         ADD 1 TO T10-LINEA-FLD (5 9) GO TO A-P.                  T3090   
012900 REV-CONT.                                                        T3090   
012910     IF T10-DIR (1) = ZERO GO TO A-P.                             T3090   
012920     IF SEC-CODE-1 < 1 OR SEC-CODE-1 > 4 GO TO A-P.               T3090   
012930     IF SEC-FEET > 100 GO TO REV-CONT-1.                          T3090   
012940     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012950         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
012960         ADD 1 TO T10-LINEA-FLD (4 7)    GO TO A-P.               T3090   
012970     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
012980         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
012990         ADD 1 TO T10-LINEA-FLD (5 7)    GO TO A-P.               T3090   
013000     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
013010         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
013020         ADD 1 TO T10-LINEA-FLD (4 3)    GO TO A-P.               T3090   
013030     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
013040         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
013050         ADD 1 TO T10-LINEA-FLD (5 3)    GO TO A-P.               T3090   
013060 REV-CONT-1.                                                      T3090   
013070     IF SEC-FEET > 200 GO TO REV-CONT-2.                          T3090   
013080     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
013090         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
013100         ADD 1 TO T10-LINEA-FLD (4 8)    GO TO A-P.               T3090   
013110     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
013120         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
013130         ADD 1 TO T10-LINEA-FLD (5 8)    GO TO A-P.               T3090   
013140     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
013150         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
013160         ADD 1 TO T10-LINEA-FLD (4 4)    GO TO A-P.               T3090   
013170     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
013180         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
013190         ADD 1 TO T10-LINEA-FLD (5 4)    GO TO A-P.               T3090   
013200 REV-CONT-2.                                                      T3090   
013210     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
013220         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
013230         ADD 1 TO T10-LINEA-FLD (4 9)    GO TO A-P.               T3090   
013240     IF T10-DIR (1) = SEC-CODE-1 AND                              T3090   
013250         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
013260         ADD 1 TO T10-LINEA-FLD (5 9)    GO TO A-P.               T3090   
013270     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
013280         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)                       T3090   
013290         ADD 1 TO T10-LINEA-FLD (4 5)    GO TO A-P.               T3090   
013300     IF T10-DIR (1) NOT = SEC-CODE-1 AND                          T3090   
013310         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)                       T3090   
013320         ADD 1 TO T10-LINEA-FLD (5 5)    GO TO A-P.               T3090   
013330 A-P.                                                             T3090   
013340     MOVE DIR-VEH-HOLD TO T10-DIR (1).                            T3090   
013350 A-PIS.                                                           T3090   
013360     PERFORM RIGHT-DIR THRU RIGHT-DIR-X.                          T3090   
013370 ACCIDENT-TYPE.                                                   T3090   
013380     IF T10-AT-IS = 1 GO TO SEV-CK.                               T3090   
013390     IF PRI-CODE-1 NOT = 9 GO TO SEV-CK-NEW.                      T3090   
013400     IF DIRECTION-1 > ZERO OR DIRECTION-2 > 1 GO TO CK-SE.        T3090   
013410     IF INTERSECTION NOT = 3 AND INTERSECTION NOT = 5 GO TO CK-SE.T3090   
013420     GO TO IC.                                                    T3090   
013430 SEV-CK-NEW.                                                      T3090   
013440     IF SEC-FEET > 0200 GO TO CK-SE.                              T3090   
013450     IF DIR-ANL = 300 GO TO CK-SE.                                T3090   
013460     IF DIR-VEH-C = ZERO GO TO CK-SE.                             T3090   
013470     IF SEC-CODE-1 < 1 OR SEC-CODE-1 > 4 GO TO CK-SE.             T3090   
013480     IF DIR-VEH-C = SEC-CODE-1 GO TO CK-SE.                       T3090   
013490 IC.                                                              T3090   
013500     IF T10-ACC-TYPE = 26 OR T10-ACC-TYPE = 27 OR T10-ACC-TYPE =  T3090   
013510         28 OR T10-ACC-TYPE = 30 OR T10-ACC-TYPE = 31 OR          T3090   
013520         T10-ACC-TYPE = 32 GO TO SEV-CK ELSE GO TO CK-SE.         T3090   
013530 SEV-CK.                                                          T3090   
013540     IF T10-A-SEV = 1 OR T10-A-SEV = 2 OR T10-A-SEV = 3           T3090   
013550         OR T10-A-SEV = 4 ADD 1 TO IF-AC-SM.                      T3090   
013560     ADD 1 TO T-AC-SM.                                            T3090   
013570 CK-SE.                                                           T3090   
013580     IF T10-AT-IS = 1 GO TO ACCIDENT-TYPE-A.                      T3090   
013590     IF T10-ACC-TYPE > 25 AND T10-ACC-TYPE < 29 ADD 1 TO          T3090   
013600         T10-LINEA-FLD (1 2) GO TO W-F-R.                         T3090   
013610     IF T10-ACC-TYPE > 29 AND T10-ACC-TYPE < 33 ADD 1 TO          T3090   
013620         T10-LINEA-FLD (2 2) GO TO W-F-R.                         T3090   
013630     IF T10-ACC-TYPE = 29 ADD 1 TO T10-LINEA-FLD (3 2)            T3090   
013640         GO TO W-F-R.                                             T3090   
013650     IF T10-ACC-TYPE = 01 ADD 1 TO T10-LINEA-FLD (4 2)            T3090   
013660         GO TO W-F-R.                                             T3090   
013670     IF T10-ACC-TYPE = 07 ADD 1 TO T10-LINEA-FLD (5 2)            T3090   
013680         GO TO W-F-R.                                             T3090   
013690     IF T10-ACC-TYPE = 05 ADD 1 TO T10-LINEA-FLD (6 2)            T3090   
013700         GO TO W-F-R.                                             T3090   
013710     IF T10-ACC-TYPE = 03 ADD 1 TO T10-LINEA-FLD (7 2)            T3090   
013720         GO TO W-F-R.                                             T3090   
013730     ADD 1 TO T10-LINEA-FLD (8 2) GO TO W-F-R.                    T3090   
013740 ACCIDENT-TYPE-A.                                                 T3090   
013750     IF T10-ACC-TYPE > 19 AND T10-ACC-TYPE < 23 ADD 1 TO          T3090   
013760         T10-LINEA-FLD (1 1) GO TO W-F-R.                         T3090   
013770     IF T10-ACC-TYPE > 22 AND T10-ACC-TYPE < 26 ADD 1 TO          T3090   
013780         T10-LINEA-FLD (2 1) GO TO W-F-R.                         T3090   
013790     IF T10-ACC-TYPE > 25 AND T10-ACC-TYPE < 29 ADD 1  TO         T3090   
013800         T10-LINEA-FLD (3 1) GO TO W-F-R.                         T3090   
013810     IF T10-ACC-TYPE > 29 AND T10-ACC-TYPE < 33 ADD 1 TO          T3090   
013820         T10-LINEA-FLD (4 1) GO TO W-F-R.                         T3090   
013830     IF T10-ACC-TYPE = 29 ADD 1 TO T10-LINEA-FLD (5 1)            T3090   
013840         GO TO W-F-R.                                             T3090   
013850     IF T10-ACC-TYPE = 03 ADD 1 TO T10-LINEA-FLD (6 1)            T3090   
013860         GO TO W-F-R.                                             T3090   
013870     ADD 1 TO T10-LINEA-FLD (7 1) GO TO W-F-R.                    T3090   
013880 W-F-R.                                                           T3090   
013890     WRITE PRINT-REC AFTER ADVANCING 2 LINES.                     T3090   
013900     MOVE SPACES TO PRINT-LINE ADD 2 TO PRTY-CTR.                 T3090   
013910     IF PRI-CODE-1 NOT = 9 GO TO W-S-R.                           T3090   
013920     MOVE DIRECTION-HOLD TO IMPACT-2.                             T3090   
013930     IF INTER-SWITCH = ZERO COMPUTE INTER-PLUS-1 =                T3090   
013940         ((DISTA + 1) * 100).                                     T3090   
013950     GO TO F-2-R.                                                 T3090   
013960 W-S-R.                                                           T3090   
013970     MOVE IMPACT-LINE2-H TO P-PT-IMPACT.                          T3090   
013980 F-2-R.                                                           T3090   
013990     MOVE T10LINE2-PR-GP-H TO P-GP-CODE.                          T3090   
014000     MOVE T10LINE2-LIGHT-H TO P-LIGHT.                            T3090   
014010     IF PARTY-NO (2) = 2 NEXT SENTENCE ELSE                       T3090   
014020     WRITE PRINT-REC AFTER ADVANCING 1 LINES MOVE SPACES TO       T3090   
014030     PRINT-LINE  ADD 1  TO PRTY-CTR GO TO T10-DET-1A.             T3090   
014040     MOVE  T10-PARTY-ENTRY (2) TO T10-PARTY-ENTRY (1).            T3090   
014050 S6.                                                              T3090   
014060     PERFORM RIGHT-DIR THRU RIGHT-DIR-X.                          T3090   
014070 S4. MOVE T10-CLASS (1) TO T10-CLASS1.                            T3090   
014080     PERFORM T1.                                                  T3090   
014090     PERFORM T3.                                                  T3090   
014100     WRITE PRINT-REC AFTER ADVANCING 1 LINES MOVE SPACES TO       T3090   
014110         PRINT-LINE  ADD 1 TO PRTY-CTR.                           T3090   
014120 S7. IF PARTY-NO (3) NOT = 3 GO TO T10-DET-1A.                    T3090   
014130     MOVE T10-PARTY-ENTRY (3) TO T10-PARTY-ENTRY (1).             T3090   
014140     PERFORM S6 THRU S4.                                          T3090   
014150     IF PARTY-NO (4) NOT = 4 GO TO T10-DET-1A.                    T3090   
014160     MOVE T10-PARTY-ENTRY (4) TO T10-PARTY-ENTRY (1).             T3090   
014170     PERFORM S6 THRU S4.                                          T3090   
014180     IF PARTY-NO (5) NOT = 5 GO TO T10-DET-1A.                    T3090   
014190     MOVE T10-PARTY-ENTRY (5) TO T10-PARTY-ENTRY (1).             T3090   
014200     PERFORM S6 THRU S4.                                          T3090   
014210     IF PARTY-NO (6) NOT = 6 GO TO T10-DET-1A.                    T3090   
014220     MOVE T10-PARTY-ENTRY (6) TO T10-PARTY-ENTRY (1).             T3090   
014230     PERFORM S6 THRU S4.                                          T3090   
014240     IF PARTY-NO (7) NOT = 7 GO TO T10-DET-1A.                    T3090   
014250     MOVE T10-PARTY-ENTRY (7) TO T10-PARTY-ENTRY (1).             T3090   
014260     PERFORM S6 THRU S4.                                          T3090   
014270     IF PARTY-NO (8) NOT = 8 GO TO T10-DET-1A.                    T3090   
014280     MOVE T10-PARTY-ENTRY (8) TO T10-PARTY-ENTRY (1).             T3090   
014290     PERFORM S6 THRU S4.                                          T3090   
014300     IF PARTY-NO (9) NOT = 9 GO TO T10-DET-1A.                    T3090   
014310     MOVE T10-PARTY-ENTRY (9) TO T10-PARTY-ENTRY (1).             T3090   
014320     PERFORM S6 THRU S4.                                          T3090   
014330 T10-DET-1A.                                                      T3090   
014340     MOVE ZERO TO INTER-SWITCH, AT-S.                             T3090   
014350     MOVE SPACES TO  T10-DET-P-HOLD.                              T3090   
014360     ADD PRTY-CTR TO LINE-NO.                                     T3090   
014370     MOVE ZERO TO PRTY-CTR.                                       T3090   
014380 T10-DET-X. EXIT.                                                 T3090   
014390 RIGHT-DIR.                                                       T3090   
014400     IF T10-DIR (1) = ZERO AND T10-DATE < '690701'                T3090   
014410         MOVE SPACE TO P-10-DIR GO TO RIGHT-DIR-X.                T3090   
014420     IF T10-DIR (1) = ZERO OR T10-DIR (1) > 4                     T3090   
014430     MOVE 'UNK.' TO P-10-DIR GO TO RIGHT-DIR-X.                   T3090   
014440     IF T10-DIR (1) = 1 MOVE 'NB- ' TO P-10-DIR.                  T3090   
014450     IF T10-DIR (1) = 2 MOVE 'EB- ' TO P-10-DIR.                  T3090   
014460     IF T10-DIR (1) = 3 MOVE 'SB- ' TO P-10-DIR.                  T3090   
014470     IF T10-DIR (1) = 4 MOVE 'WB- ' TO P-10-DIR.                  T3090   
014480                                                                  KM010771
014490                                                                  KM010771
014500                                                                  KM010771
014510                                                                  KM010771
014520                                                                  KM010771
014530                                                                  KM010771
014540     IF T10-AT-IS = 1 GO TO RIGHT-DIR-IS.                         T3090   
014550     IF PRI-CODE-1 = PRI-CODE-2 AND SEC-CODE-1 = SEC-CODE-2       T3090   
014560         NEXT SENTENCE ELSE GO TO RIGHT-DIR-ST.                   T3090   
014570     IF LOCAT-CODE LESS THAN 011 NEXT SENTENCE ELSE               T3090   
014580     MOVE SPACE TO P-10-SP GO TO RIGHT-DIR-X.                     T3090   
014590 RIGHT-DIR-ST.                                                    T3090   
014600     IF T10-REVERSE-ST-C = 1 MOVE 2 TO P-10-NO ELSE               T3090   
014610     MOVE 1 TO P-10-NO.                                           T3090   
014620     GO TO RIGHT-DIR-X.                                           T3090   
014630 RIGHT-DIR-IS.                                                    T3090   
014640     IF T10-REVERSE-ST-C = 1 GO TO RIGHT-DIR-IS-REV.              T3090   
014650     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND                  T3090   
014660     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 1 TO P-10-NO.        T3090   
014670     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND                  T3090   
014680     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 2 TO P-10-NO.        T3090   
014690     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND                  T3090   
014700     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 2 TO P-10-NO.        T3090   
014710     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND                  T3090   
014720     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 1 TO P-10-NO.        T3090   
014730     GO TO RIGHT-DIR-X.                                           T3090   
014740 RIGHT-DIR-IS-REV.                                                T3090   
014750     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND                  T3090   
014760     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 2 TO P-10-NO.        T3090   
014770     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND                  T3090   
014780     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 1 TO P-10-NO.        T3090   
014790     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND                  T3090   
014800     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 1 TO P-10-NO.        T3090   
014810     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND                  T3090   
014820     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 2 TO P-10-NO.        T3090   
014830 RIGHT-DIR-X.  EXIT.                                              T3090   
014840 HEADER.                                                          T3090   
014850     MOVE ZERO TO LINE-NO.                                        T3090   
014860     IF HDR-CTR = 19 MOVE 21 TO H-NO.          IF HDR-CTR = 23    T3090   
014870     MOVE 22 TO H-NO ELSE MOVE HDR-CTR TO H-NO. MOVE PAGE-NO TO HPT3090   
014872     PERFORM ANT-LOOKUP VARYING ANT FROM 1 BY 1 UNTIL ANT > 20 OR KM120171
014874                     NT-CITY (ANT) = T10-REQ-HOLD-3.              KM120171
014876     IF ANT > 20 MOVE SPACES TO ANT-HDR       ELSE                KM120171
014878        MOVE NT-NAME (ANT) TO ANT-HDR.                            KM120171
014880            WRITE PRINT-REC FROM HDR AFTER ADVANCING 0 LINES.     T3090   
014890     IF HDR-CTR = 10 PERFORM H-10 THRU H-10-X.                    T3090   
014900 HEADER-X. EXIT.                                                  T3090   
014905 ANT-LOOKUP. EXIT.                                                KM120271
014910 SUM-10.                                                          T3090   
014920     MOVE 0 TO 10-SW.                                             T3090   
014930     PERFORM HEADER THRU HEADER-X.                                T3090   
014940     COMPUTE T10-LINEA-FLD (8 1) = T10-LINEA-FLD (1 1) +          T3090   
014950       T10-LINEA-FLD (2 1) + T10-LINEA-FLD (3 1) +                T3090   
014960       T10-LINEA-FLD (4 1) + T10-LINEA-FLD (5 1) +                T3090   
014970       T10-LINEA-FLD (6 1) + T10-LINEA-FLD (7 1).                 T3090   
014980     COMPUTE T10-LINEA-FLD (9 2) = T10-LINEA-FLD (1 2) +          T3090   
014990       T10-LINEA-FLD (2 2) + T10-LINEA-FLD (3 2) +                T3090   
015000       T10-LINEA-FLD (4 2) + T10-LINEA-FLD (5 2) +                T3090   
015010       T10-LINEA-FLD (6 2) + T10-LINEA-FLD (7 2) +                T3090   
015020       T10-LINEA-FLD (8 2).                                       T3090   
015030     COMPUTE T10-LINEA-FLD (6 3) = T10-LINEA-FLD (2 3) +          T3090   
015040       T10-LINEA-FLD (3 3) + T10-LINEA-FLD (4 3) +                T3090   
015050       T10-LINEA-FLD (5 3).                                       T3090   
015060     COMPUTE T10-LINEA-FLD (6 4) = T10-LINEA-FLD (2 4) +          T3090   
015070       T10-LINEA-FLD (3 4) + T10-LINEA-FLD (4 4) +                T3090   
015080       T10-LINEA-FLD (5 4).                                       T3090   
015090     COMPUTE T10-LINEA-FLD (6 5) = T10-LINEA-FLD (2 5) +          T3090   
015100       T10-LINEA-FLD (3 5) + T10-LINEA-FLD (4 5) +                T3090   
015110       T10-LINEA-FLD (5 5).                                       T3090   
015120     COMPUTE T10-LINEA-FLD (2 6) = T10-LINEA-FLD (2 3) +          T3090   
015130       T10-LINEA-FLD (2 4) + T10-LINEA-FLD (2 5).                 T3090   
015140     COMPUTE T10-LINEA-FLD (3 6) = T10-LINEA-FLD (3 3) +          T3090   
015150       T10-LINEA-FLD (3 4) + T10-LINEA-FLD (3 5).                 T3090   
015160     COMPUTE T10-LINEA-FLD (4 6) = T10-LINEA-FLD (4 3) +          T3090   
015170       T10-LINEA-FLD (4 4) + T10-LINEA-FLD (4 5).                 T3090   
015180     COMPUTE T10-LINEA-FLD (5 6) = T10-LINEA-FLD (5 3) +          T3090   
015190       T10-LINEA-FLD (5 4) + T10-LINEA-FLD (5 5).                 T3090   
015200     COMPUTE T10-LINEA-FLD (6 6) = T10-LINEA-FLD (6 3) +          T3090   
015210       T10-LINEA-FLD (6 4) + T10-LINEA-FLD (6 5).                 T3090   
015220     COMPUTE T10-LINEA-FLD (6 7) = T10-LINEA-FLD (2 7) +          T3090   
015230       T10-LINEA-FLD (3 7) + T10-LINEA-FLD (4 7) +                T3090   
015240       T10-LINEA-FLD (5 7).                                       T3090   
015250     COMPUTE T10-LINEA-FLD (6 8) = T10-LINEA-FLD (2 8) +          T3090   
015260       T10-LINEA-FLD (3 8) + T10-LINEA-FLD (4 8) +                T3090   
015270       T10-LINEA-FLD (5 8).                                       T3090   
015280     COMPUTE T10-LINEA-FLD (6 9) = T10-LINEA-FLD (2 9) +          T3090   
015290       T10-LINEA-FLD (3 9) + T10-LINEA-FLD (4 9) +                T3090   
015300       T10-LINEA-FLD (5 9).                                       T3090   
015310     COMPUTE T10-LINEA-FLD (2 10) = T10-LINEA-FLD (2 7) +         T3090   
015320       T10-LINEA-FLD (2 8) + T10-LINEA-FLD (2 9).                 T3090   
015330     COMPUTE T10-LINEA-FLD (3 10) = T10-LINEA-FLD (3 7) +         T3090   
015340       T10-LINEA-FLD (3 8) + T10-LINEA-FLD (3 9).                 T3090   
015350     COMPUTE T10-LINEA-FLD (4 10) = T10-LINEA-FLD (4 7) +         T3090   
015360       T10-LINEA-FLD (4 8) + T10-LINEA-FLD (4 9).                 T3090   
015370     COMPUTE T10-LINEA-FLD (5 10) = T10-LINEA-FLD (5 7) +         T3090   
015380       T10-LINEA-FLD (5 8) + T10-LINEA-FLD (5 9).                 T3090   
015390     COMPUTE T10-LINEA-FLD (6 10) = T10-LINEA-FLD (6 7) +         T3090   
015400       T10-LINEA-FLD (6 8) + T10-LINEA-FLD (6 9).                 T3090   
015410     PERFORM SUM-10-LINEA THRU SUM-10-LINEA-X VARYING             T3090   
015420         G FROM 1 BY 1 UNTIL G GREATER THAN 9.                    T3090   
015430                                                                  T3090   
015440     MOVE '4. ACCIDENT SEVERITY:             5. LIGHTING CONDITIONT3090   
015450-         'S:               6. TRAFFIC VOLUME:' TO PRINT-LINE.    T3090   
015460     WRITE PRINT-REC AFTER ADVANCING 2 LINES.                     T3090   
015470     MOVE '     DESCRIPTION ACCIDENTS PERSONS     DESCRIPTION    AT3090   
015480-         'CCIDENTS              STREET  LOC  DIR VOLUME    DATE' T3090   
015490          TO PRINT-LINE.                                          T3090   
015500     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
015510     PERFORM TRA-VOL THRU TRA-VOL-X.                              T3090   
015520     PERFORM SUM-10-LINEB THRU SUM-10-LINEB-X VARYING G FROM 1    T3090   
015530         BY 1 UNTIL G GREATER THAN 7.                             T3090   
015540     MOVE '                                                       T3090   
015550-    '                 NOTE: * = (6 HR TOTAL X EXPANSION FACTOR)' T3090   
015560         TO PRINT-LINE.                                           T3090   
015570     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
015580     MOVE '                                                       T3090   
015590-    '                       **= (P.M. PEAK HR TOTAL X 10)'       T3090   
015600         TO PRINT-LINE.                                           T3090   
015610     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
015620     MOVE '7. ACCIDENT SUMMARY:                                   T3090   
015630-         '                         8. INTERSECTION DESCRIPTION:' T3090   
015640          TO PRINT-LINE.                                          T3090   
015650     WRITE PRINT-REC AFTER ADVANCING 2 LINES.                     T3090   
015660     MOVE '                           STRATIFICATION DATA         T3090   
015670-         '                         GENERAL' TO PRINT-LINE.       T3090   
015680     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
015690     PERFORM STRAF-RD THRU STRAF-RD-X.                            T3090   
015700     PERFORM SUM-10-LINEC THRU SUM-10-LINEC-X.                    T3090   
015710     MOVE '   B. ACCIDENT RATE PER                                T3090   
015720-         '                         APPROACH CHARACTERISTICS:'    T3090   
015730          TO PRINT-LINE.                                          T3090   
015740     MOVE '   ** L E G **   ' TO ITEM-8-CON-1.                    T3090   
015750     WRITE PRINT-REC AFTER ADVANCING 2 LINES.                     T3090   
015760     MOVE '      MILLION VEHICLES: ' TO PRINT-LINE.               T3090   
015770     MOVE 'N    S    E    W' TO ITEM-8-CON-1.                     T3090   
015780     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
015790     PERFORM SUM-10-LINEC-1 THRU SUM-10-LINEC-1-X VARYING         T3090   
015800         H FROM 1 BY 1 UNTIL H GREATER THAN 14.                   T3090   
015810     PERFORM INIT THRU INIT-X.                                    T3090   
015820 SUM-10-X. EXIT.                                                  T3090   
015830 INIT.                                                            T3090   
015840     MOVE ZEROES TO T10-SUM-LINEA-WORK T10-SUM-LINEB-WORK         T3090   
015850         T10-SUM-LINEC-WORK.                                      T3090   
015860     PERFORM INIT-1X THRU INIT-1X-X VARYING G FROM 1 BY 1         T3090   
015870         UNTIL G GREATER THAN 7.                                  T3090   
015880     MOVE ZEROES TO STR-SUM.                                      T3090   
015890 INIT-X. EXIT.                                                    T3090   
015900 INIT-1X.                                                         T3090   
015910     MOVE SPACES TO T10-V1 (G), T10-V2 (G), T10-V4-M (G),         T3090   
015920         T10-V4-D (G), T10-V4-Y (G), T10-V3A (G).                 T3090   
015930     MOVE ZEROES TO T10-V3 (G).                                   T3090   
015940 INIT-1X-X. EXIT.                                                 T3090   
015950 SUM-10-LINEA. MOVE SPACES TO PRINT-LINE.                         T3090   
015960     MOVE T10-LINEA-FLD (G 1) TO ITEM-1-TOT (1).                  T3090   
015970     MOVE T10-LINEA-FLD (G 2) TO ITEM-1-TOT (2).                  T3090   
015980     MOVE T10-LINEA-FLD (G 3) TO ITEM-3-A-1.                      T3090   
015990     MOVE T10-LINEA-FLD (G 4) TO ITEM-3-A-2.                      T3090   
016000     MOVE T10-LINEA-FLD (G 5) TO ITEM-3-A-O.                      T3090   
016010     MOVE T10-LINEA-FLD (G 6) TO ITEM-3-A-T.                      T3090   
016020     MOVE T10-LINEA-FLD (G 7) TO ITEM-3-D-1.                      T3090   
016030     MOVE T10-LINEA-FLD (G 8) TO ITEM-3-D-2.                      T3090   
016040     MOVE T10-LINEA-FLD (G 9) TO ITEM-3-D-O.                      T3090   
016050     MOVE T10-LINEA-FLD (G 10) TO ITEM-3-D-T.                     T3090   
016060     IF G = 1 MOVE 'RIGHT ANGLE ' TO ITEM-1-CON (1)               T3090   
016070              MOVE 'REAR END    ' TO ITEM-1-CON (2)               T3090   
016080              MOVE '100 200 OVER TOTAL     100 200 OVER TOTAL'    T3090   
016090                 TO ITEM-3-C1.                                    T3090   
016100     IF G = 2 MOVE 'LEFT TURN   ' TO ITEM-1-CON (1)               T3090   
016110              MOVE 'SIDE SWIPE  ' TO ITEM-1-CON (2)               T3090   
016120              MOVE '1ST - N/E'    TO ITEM-3-CON.                  T3090   
016130     IF G = 3 MOVE 'REAR END    ' TO ITEM-1-CON (1)               T3090   
016140              MOVE 'HEAD ON     ' TO ITEM-1-CON (2)               T3090   
016150              MOVE '1ST - S/W'    TO ITEM-3-CON.                  T3090   
016160     IF G = 4 MOVE 'SIDE SWIPE  ' TO ITEM-1-CON (1)               T3090   
016170              MOVE 'RAN OFF ROAD' TO ITEM-1-CON (2)               T3090   
016180              MOVE '2ND - N/E'    TO ITEM-3-CON.                  T3090   
016190     IF G = 5 MOVE 'HEAD ON     ' TO ITEM-1-CON (1)               T3090   
016200              MOVE 'FIXED OBJECT' TO ITEM-1-CON (2)               T3090   
016210              MOVE '2ND - S/W'    TO ITEM-3-CON.                  T3090   
016220     IF G = 6 MOVE 'PEDESTRIAN  ' TO ITEM-1-CON (1)               T3090   
016230              MOVE 'PARKED VEH. ' TO ITEM-1-CON (2)               T3090   
016240              MOVE '  TOTAL  '    TO ITEM-3-CON.                  T3090   
016250     IF G = 7 MOVE 'MISC.       ' TO ITEM-1-CON (1)               T3090   
016260              MOVE 'PEDESTRIAN  ' TO ITEM-1-CON (2).              T3090   
016270     IF G = 8 MOVE '   TOTAL    ' TO ITEM-1-CON (1)               T3090   
016280              MOVE 'MISC.       ' TO ITEM-1-CON (2).              T3090   
016290     IF G = 9 MOVE '   TOTAL    ' TO ITEM-1-CON (2).              T3090   
016300     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
016310 SUM-10-LINEA-X. EXIT.                                            T3090   
016320 SUM-10-LINEB.                                                    T3090   
016330     MOVE SPACES TO PRINT-LINE.                                   T3090   
016340     MOVE T10-LINEB-FLD (G 1) TO ITEM-4-ACC.                      T3090   
016350     MOVE T10-LINEB-FLD (G 2) TO ITEM-4-PER.                      T3090   
016360     MOVE T10-LINEB-FLD (G 3) TO ITEM-5-ACC.                      T3090   
016370     MOVE T10-V1 (G) TO ITEM-6-LOC.                               T3090   
016380     MOVE T10-V2 (G) TO ITEM-6-DIR.                               T3090   
016390     MOVE T10-V3 (G) TO ITEM-6-VOL.                               T3090   
016400     MOVE T10-V3A (G) TO ITEM-6-C.                                T3090   
016410     MOVE T10-V4-M (G) TO ITEM-6-MO.                              T3090   
016420     MOVE T10-V4-D (G) TO ITEM-6-DA.                              T3090   
016430     MOVE T10-V4-Y (G) TO ITEM-6-YR.                              T3090   
016440     IF G = 1 MOVE 'FATALITY    ' TO ITEM-4-CON                   T3090   
016450              MOVE 'DAY            ' TO ITEM-5-CON                T3090   
016460              MOVE '1ST   ' TO ITEM-6-CON.                        T3090   
016470     IF G = 2 MOVE 'INJURY      ' TO ITEM-4-CON                   T3090   
016480              MOVE 'DUSK-DAWN      ' TO ITEM-5-CON                T3090   
016490              MOVE '1ST   ' TO ITEM-6-CON.                        T3090   
016500     IF G = 3 MOVE '  A         ' TO ITEM-4-CON                   T3090   
016510              MOVE 'DARK           ' TO ITEM-5-CON.               T3090   
016520     IF G = 4 MOVE '  B         ' TO ITEM-4-CON                   T3090   
016530              MOVE 'DAY-DARK-CLOUDY' TO ITEM-5-CON                T3090   
016540              MOVE '2ND   ' TO ITEM-6-CON.                        T3090   
016550     IF G = 5 MOVE '  C         ' TO ITEM-4-CON                   T3090   
016560              MOVE '   TOTAL       ' TO ITEM-5-CON                T3090   
016570              MOVE '2ND   ' TO ITEM-6-CON.                        T3090   
016580     IF G = 6 MOVE 'PROPERTY    ' TO ITEM-4-CON.                  T3090   
016590     IF G = 7 MOVE '   TOTAL    ' TO ITEM-4-CON.                  T3090   
016600     IF G = 1 AND T10-V4-M (1) NOT = SPACE                        T3090   
016610         MOVE '-' TO ITEM-6-C1, ITEM-6-C2.                        T3090   
016620     IF G = 4 AND T10-V4-M (4) NOT = SPACE                        T3090   
016630         MOVE '-' TO ITEM-6-C1, ITEM-6-C2.                        T3090   
016640     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
016650 SUM-10-LINEB-X. EXIT.                                            T3090   
016660 W-SPACE-1.                                                       T3090   
016670     MOVE ZERO TO ITEM-7-VOL.                                     T3090   
016680     ADD 1 TO G.                                                  T3090   
016690     IF G = 1 PERFORM 7A GO TO W-SPACE-2.                         T3090   
016700     IF G = 2 PERFORM 7B GO TO W-SPACE-2.                         T3090   
016710     IF G = 3 PERFORM 7C GO TO W-SPACE-2.                         T3090   
016720     IF G = 4 PERFORM 7D PERFORM 7D-A GO TO W-SPACE-2.            T3090   
016730     IF G = 5 PERFORM 7E                                          T3090   
016740     MOVE T-AC-SM TO ITEM-7-HO                                    T3090   
016750     SUBTRACT LINEC-MEAN (2) FROM ITEM-7-HO  GIVING ITEM-7-DIFF   T3090   
016760     MOVE LINEC-MEAN (2) TO ITEM-7-MEAN                           T3090   
016770     MOVE LINEC-DEV (2) TO ITEM-7-DEV                             T3090   
016780     MOVE LINEC-CVA (2) TO ITEM-7-C-VA                            T3090   
016790         MOVE T-AC-SM  TO ITEM-7-A-VA GO TO W-SPACE-2.            T3090   
016800 W-SPACE-2.                                                       T3090   
016810     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
016820     MOVE SPACES TO PRINT-LINE.                                   T3090   
016830 W-SPACE-1-X. EXIT.                                               T3090   
016840 7A. MOVE '     DESCRIPTION                STANDARD  CRITICAL ACTUT3090   
016850-         'AL DIFFERENCE ' TO ITEM-7-H.                           T3090   
016860 7B. MOVE '                           MEAN DEVIATION VALUE    VALUT3090   
016870-         'E  (ACTUAL VS MEAN)' TO ITEM-7-H.                      T3090   
016880 7C. MOVE '   A. NO. OF ACCIDENTS: ' TO ITEM-7-CON.               T3090   
016890 7D. MOVE '      1. INJ + FAT      ' TO ITEM-7-CON.               T3090   
016900 7E. MOVE '      2. ALL ACCIDENTS  ' TO ITEM-7-CON.               T3090   
016910 7D-A.                                                            T3090   
016920     MOVE IF-AC-SM TO ITEM-7-HOLD.                                T3090   
016930             MOVE ITEM-7-HOLD TO ITEM-7-A-VA.                     T3090   
016940     SUBTRACT LINEC-MEAN (1) FROM ITEM-7-HOLD GIVING ITEM-7-DIFF. T3090   
016950     MOVE LINEC-MEAN (1) TO ITEM-7-MEAN.                          T3090   
016960     MOVE LINEC-DEV (1) TO ITEM-7-DEV.                            T3090   
016970     MOVE LINEC-CVA (1) TO ITEM-7-C-VA.                           T3090   
016980 7D-B.                                                            T3090   
016990     IF IF-AC-SM = ZERO GO TO 7D-B-X.                             T3090   
017000     IF T10-V3 (1) = ZERO AND T10-V3 (2) = ZERO GO TO 7D-B-X.     T3090   
017010     IF T10-V3 (4) = ZERO AND T10-V3 (5) = ZERO GO TO 7D-B-X.     T3090   
017020     ADD T10-V3 (1) T10-V3 (2) T10-V3 (4) T10-V3 (5) GIVING       T3090   
017030         ITEM-7-VOL.                                              T3090   
017040     IF ITEM-7-VOL = ZERO GO TO 7D-B-X.                           T3090   
017050     IF TND = ZERO GO TO 7D-B-X.                                  T3090   
017060     COMPUTE ITEM-7-HOO ROUNDED =                                 T3090   
017070         (IF-AC-SM / ((TND * ITEM-7-VOL) / 1000000)).             KM120870
017080     MOVE ITEM-7-HOO TO ITEM-7-A-VAM.                             T3090   
017090     SUBTRACT LINEC-MEAN (3) FROM ITEM-7-HOO GIVING ITEM-7-DIFF.  T3090   
017100     MOVE LINEC-MEAN (3) TO ITEM-7-MEAN.                          T3090   
017110     MOVE LINEC-DEV (3) TO ITEM-7-DEV.                            T3090   
017120     MOVE LINEC-CVA (3) TO ITEM-7-C-VA.                           T3090   
017130 7D-B-X. EXIT.                                                    T3090   
017140 7D-C.                                                            T3090   
017150     IF T-AC-SM = ZERO GO TO 7D-C-X.                              T3090   
017160     IF ITEM-7-VOL = ZERO GO TO 7D-C-X.                           T3090   
017170     IF TND = ZERO GO TO 7D-C-X.                                  T3090   
017180     COMPUTE ITEM-7-HOO ROUNDED =                                 T3090   
017190         (T-AC-SM / ((TND * ITEM-7-VOL) / 1000000)).              KM120870
017200     MOVE ITEM-7-HOO TO ITEM-7-A-VAM.                             T3090   
017210     SUBTRACT LINEC-MEAN (4) FROM ITEM-7-HOO GIVING ITEM-7-DIFF.  T3090   
017220     MOVE LINEC-MEAN (4) TO ITEM-7-MEAN.                          T3090   
017230     MOVE LINEC-DEV (4) TO ITEM-7-DEV.                            T3090   
017240     MOVE LINEC-CVA (4) TO ITEM-7-C-VA.                           T3090   
017250 7D-C-X. EXIT.                                                    T3090   
017260 SUM-10-LINEC-1.                                                  T3090   
017270     MOVE SPACES TO PRINT-LINE.                                   T3090   
017280     IF H = 1 PERFORM 7D MOVE ZERO TO ITEM-7-VOL                  T3090   
017290         PERFORM 7D-B THRU 7D-B-X.                                T3090   
017300     IF H = 2 PERFORM 7E PERFORM 7D-C THRU 7D-C-X.                T3090   
017310     MOVE ITEM-8-A-ENTRY (H) TO ITEM-8-CON.                       T3090   
017320     MOVE T10-S-LINEC3 (1 H) TO ITEM-8-N.                         T3090   
017330     MOVE T10-S-LINEC3 (2 H) TO ITEM-8-E.                         T3090   
017340     MOVE T10-S-LINEC3 (3 H) TO ITEM-8-S.                         T3090   
017350     MOVE T10-S-LINEC3 (4 H) TO ITEM-8-W.                         T3090   
017360     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
017370 SUM-10-LINEC-1-X. EXIT.                                          T3090   
017380 SUM-10-LINEC.                                                    T3090   
017390     MOVE SPACES TO PRINT-LINE. MOVE ZERO TO G.                   T3090   
017400     IF T10-SUM-LINEC1 (1) NOT EQUAL TO ZERO                      T3090   
017410         MOVE T10-SUM-LINEC1 (1) TO S ADD 1 TO T                  T3090   
017420         MOVE ITEM-8-C-ENTRY (T) TO ITEM-8-CON-1.                 T3090   
017430         MOVE 'CLASS CODE:' TO ITEM-8-CON.                        T3090   
017440         PERFORM W-SPACE-1 THRU W-SPACE-1-X.                      T3090   
017450 S1. IF T10-SUM-LINEC1 (2) > ZERO                                 T3090   
017460     MOVE T10-SUM-LINEC1 (2) TO S                                 T3090   
017470         MOVE ITEM-8-L-ENTRY (T) TO ITEM-8-CON-1.                 T3090   
017480         MOVE 'NUMBER OF LEGS:'      TO ITEM-8-CON.               T3090   
017490         PERFORM W-SPACE-1 THRU W-SPACE-1-X.                      T3090   
017500     IF T10-SUM-LINEC1 (3) EQUAL TO ZERO OR T10-SUM-LINEC1 (3) >  T3090   
017510         '04' GO TO LINEC-A.                                      T3090   
017520         MOVE T10-SUM-LINEC1 (3) TO S ADD 1 TO T                  T3090   
017530         MOVE ITEM-8-I-ENTRY (T) TO ITEM-8-CON-1.                 T3090   
017540 LINEC-A.                                                         T3090   
017550         MOVE 'TYPE OF I/S CONTROL:' TO ITEM-8-CON.               T3090   
017560         PERFORM W-SPACE-1 THRU W-SPACE-1-X.                      T3090   
017570     IF T10-SUM-LINEC1 (4) EQUAL TO ZERO OR T10-SUM-LINEC1 (4) >  T3090   
017580         '04' GO TO LINEC-B.                                      T3090   
017590         MOVE T10-SUM-LINEC1 (4) TO S ADD 1 TO T                  T3090   
017600         MOVE ITEM-8-S-ENTRY (T) TO ITEM-8-CON-1.                 T3090   
017610 LINEC-B.                                                         T3090   
017620     MOVE 'TYPE OF SIGNAL CONTROL:   ' TO ITEM-8-CON.             T3090   
017630         PERFORM W-SPACE-1 THRU W-SPACE-1-X.                      T3090   
017640     IF T10-SUM-LINEC1 (5) NOT EQUAL TO ZERO                      T3090   
017650         MOVE T10-SUM-LINEC1 (5) TO ITEM-8-CON-1.                 T3090   
017660     MOVE 'SIGNAL OP CHARACTERISTICS:' TO ITEM-8-CON.             T3090   
017670         PERFORM W-SPACE-1 THRU W-SPACE-1-X.                      T3090   
017680 SUM-10-LINEC-X. EXIT.                                            T3090   
017690 TRA-VOL.                                                         T3090   
017691     MOVE 0 TO SW-0-INDEX.                                        AN121472
17695      MOVE 9 TO REV-SWITCH.                                                
017700     MOVE SPACES TO ST-V-H (1) ST-V-H (2).                        T3090   
017710     MOVE ZEROES TO ST-V-V (1)  ST-V-V (2).                       T3090   
017720     MOVE T10-IS-HOLD TO VOL-SYM-ST-CODE.                         T3090   
017730     MOVE ZEROES TO VOL-SYM-INDEX.                                T3090   
017740     READ VOLFILE INVALID KEY GO TO TRA-VOL-X.                    T3090   
017750     IF VOL-DELETE-CODE = HIGH-VALUE GO TO TRA-VOL-X.             T3090   
017760         MOVE VOL-LATEST-N TO VOL-LATEST-HOLD.                    T3090   
017770     MOVE VOL-OLDEST-N TO VOL-OLDEST-HOLD.                        T3090   
017771     IF  VOL-OLDEST-HOLD = 0                                      AN121472
017771     AND VOL-LATEST-HOLD = 0                                      AN121472
017771         MOVE 1 TO SW-0-INDEX.                                    AN121472
017772     IF VOL-D-O-W = '6' OR VOL-D-O-W = '7' GO TO TRA-VOL-D.       KM121070
017773     IF DATE-2YRS = ZERO NEXT SENTENCE ELSE                               
017774         IF VOL-DATE < DATE-2YRS GO TO TRA-VOL-D.                         
017780 T5. IF VOL-LOC-3 = '5' MOVE 'ND'  TO ST-V-DIR (1)                T3090   
017790     MOVE VOL-TOTAL-TOT-N TO ST-V-V (1).                          AN122072
017800     IF VOL-LOC-3 = '1' MOVE  'NB' TO ST-V-DIR (1)                T3090   
017810               MOVE VOL-TOTAL-NE-N TO ST-V-V (1).                 T3090   
017820     IF VOL-LOC-3 = '2' MOVE  'EB' TO ST-V-DIR (1)                T3090   
017830               MOVE VOL-TOTAL-NE-N TO ST-V-V (1).                 T3090   
017840     IF VOL-LOC-3 = '3' MOVE  'SB' TO ST-V-DIR (1)                T3090   
017850               MOVE VOL-TOTAL-SW-N TO ST-V-V (1).                 T3090   
017860     IF VOL-LOC-3 = '4' MOVE  'WB' TO ST-V-DIR (1)                T3090   
017870               MOVE VOL-TOTAL-SW-N TO ST-V-V (1).                 T3090   
017880     IF VOL-LOC-1 = '1' MOVE 'N/O' TO ST-V-LOC (1).               T3090   
017890     IF VOL-LOC-1 = '2' MOVE 'E/O' TO ST-V-LOC (1).               T3090   
017900     IF VOL-LOC-1 = '3' MOVE 'S/O' TO ST-V-LOC (1).               T3090   
017910     IF VOL-LOC-1 = '4' MOVE 'W/O' TO ST-V-LOC (1).               T3090   
017920     IF VOL-LOC-1 = '5' MOVE 'AT ' TO ST-V-LOC (1).               T3090   
017930     IF VOL-LOC-1 = '6' MOVE 'AT ' TO ST-V-LOC (1).               T3090   
017940     IF VOL-LOC-3 NOT = ZERO GO TO TRA-VOL-A.                     T3090   
017950     IF VOL-LOC-1 = '1' MOVE 'NB' TO ST-V-DIR (1)                 T3090   
017960         MOVE 'SB' TO ST-V-DIR (2)                                T3090   
017970         MOVE 'N/O' TO ST-V-LOC (1), ST-V-LOC (2).                T3090   
017980     IF VOL-LOC-1 = '2' MOVE 'EB' TO ST-V-DIR (1)                 T3090   
017990         MOVE 'WB' TO ST-V-DIR (2)                                T3090   
018000         MOVE 'E/O' TO ST-V-LOC (1), ST-V-LOC (2).                T3090   
018010     IF VOL-LOC-1 = '3' MOVE 'NB' TO ST-V-DIR (1)                 T3090   
018020         MOVE 'SB' TO ST-V-DIR (2)                                T3090   
018030         MOVE 'S/O' TO ST-V-LOC (1), ST-V-LOC (2).                T3090   
018040     IF VOL-LOC-1 = '4' MOVE 'EB' TO ST-V-DIR (1)                 T3090   
018050         MOVE 'WB' TO ST-V-DIR (2)                                T3090   
018060         MOVE 'W/O' TO ST-V-LOC (1), ST-V-LOC (2).                T3090   
018070     IF VOL-LOC-1 = '5' MOVE 'NB' TO ST-V-DIR (1)                 T3090   
018080         MOVE 'SB' TO ST-V-DIR (2)                                T3090   
018090         MOVE 'AT ' TO ST-V-LOC (1), ST-V-LOC (2).                T3090   
018100     IF VOL-LOC-1 = '6' MOVE 'EB' TO ST-V-DIR (1)                 T3090   
018110         MOVE 'WB' TO ST-V-DIR (2)                                T3090   
018120         MOVE 'AT ' TO ST-V-LOC (1), ST-V-LOC (2).                T3090   
018130               MOVE VOL-TOTAL-NE-N TO ST-V-V (1).                 T3090   
018140               MOVE VOL-TOTAL-SW-N TO ST-V-V (2).                 T3090   
018150 TRA-VOL-A.                                                       T3090   
018160     IF VOL-REC-CODE = 1 GO TO TRA-VOL-B.                         T3090   
018170     IF ST-V-V (1) = ZERO GO TO TVA                               T3090   
018180                       ELSE MULTIPLY ST-V-V (1) BY VOL-ADJ-FAC    T3090   
018190     GIVING ST-V-V (1) ROUNDED MOVE '*' TO ST-V-SIGN (1).         T3090   
018200     IF ST-V-V (2) = ZERO GO TO TVA ELSE                          T3090   
018210         MULTIPLY ST-V-V (2) BY VOL-ADJ-FAC                       T3090   
018220     GIVING ST-V-V (2) ROUNDED MOVE '*' TO ST-V-SIGN (2).         T3090   
018230 TRA-VOL-B.                                                       T3090   
018240     IF VOL-REV-CODE = '1' MOVE 1 TO REV-SWITCH GO TO TRA-VOL-C.  T3090   
018245     MOVE ZERO TO REV-SWITCH.                                             
018250     MOVE ST-V-LOC (1) TO T10-V1 (1).                             T3090   
018260     MOVE ST-V-LOC (2) TO T10-V1 (2).                             T3090   
018270     MOVE ST-V-DIR (1) TO T10-V2  (1).                            T3090   
018280     MOVE ST-V-DIR (2) TO T10-V2  (2).                            T3090   
018290     MOVE ST-V-V   (1) TO T10-V3  (1).                            T3090   
018300     MOVE ST-V-V   (2) TO T10-V3  (2).                            T3090   
018310     MOVE ST-V-SIGN (1) TO T10-V3A (1).                           T3090   
018320     MOVE ST-V-SIGN (2) TO T10-V3A (2).                           T3090   
018330     MOVE VOL-YR  TO  T10-V4-Y (1).                               T3090   
018340     MOVE VOL-MO  TO  T10-V4-M (1).                               T3090   
018350     MOVE VOL-DA  TO  T10-V4-D (1).                               T3090   
018360     GO TO TRA-VOL-D.                                             T3090   
018370 TVA.  COMPUTE ST-V-V (1) ROUNDED =                               T3090   
018380         ((V-NE-VOL (2) + V-SW-VOL (2)) * 10) / 2.                T3090   
018390       MOVE ST-V-V (1) TO ST-V-V (2).                             T3090   
018400       MOVE '**' TO ST-V-SIGN (1) ST-V-SIGN (2).                  T3090   
018410     GO TO TRA-VOL-B.                                             T3090   
018420 TRA-VOL-C.                                                       T3090   
018430     MOVE ST-V-LOC (1) TO T10-V1 (4).                             T3090   
018440     MOVE ST-V-LOC (2) TO T10-V1 (5).                             T3090   
018450     MOVE ST-V-DIR (1) TO   T10-V2  (4).                          T3090   
018460     MOVE ST-V-DIR (2) TO   T10-V2  (5).                          T3090   
018470     MOVE ST-V-V   (1) TO   T10-V3  (4).                          T3090   
018480     MOVE ST-V-V   (2) TO   T10-V3  (5).                          T3090   
018490     MOVE ST-V-SIGN (1) TO  T10-V3A (4).                          T3090   
018500     MOVE ST-V-SIGN (2) TO  T10-V3A (5).                          T3090   
018510     MOVE VOL-YR  TO  T10-V4-Y (4).                               T3090   
018520     MOVE VOL-MO  TO  T10-V4-M (4).                               T3090   
018530     MOVE VOL-DA  TO  T10-V4-D (4).                               T3090   
018540 TRA-VOL-D.                                                       T3090   
018541     IF SW-0-INDEX = 1 GO TO TRA-VOL-E.                           AN121472
018550     IF VOL-LATEST-HOLD = VOL-SYM-INDEX-N MOVE ZERO               T3090   
018560         TO VOL-LATEST-HOLD, VOL-OLDEST-HOLD, REV-SWITCH,         T3090   
018570     MOVE SPACES TO ST-VOL-HOLD                                   T3090   
018580     MOVE ZEROES TO ST-V-V (1)  ST-V-V (2) GO TO TRA-VOL-X.       T3090   
018581 TRA-VOL-E.                                                       AN121472
018582     MOVE 0 TO SW-0-INDEX.                                        AN121472
018590     MOVE SPACES TO ST-VOL-HOLD                                   T3090   
018600     MOVE ZEROES TO ST-V-V (1) ST-V-V (2) MOVE VOL-LATEST-HOLD TO T3090   
018610         VOL-SYM-INDEX-N.                                         T3090   
018620 TC. READ VOLFILE INVALID KEY GO TO TRA-VOL-ERROR.                T3090   
018630     IF VOL-DELETE-CODE = HIGH-VALUE GO TO TCA.                   T3090   
018632     IF VOL-D-O-W = '6' OR VOL-D-O-W = '7' GO TO TCA.             KM121070
018633     IF DATE-2YRS = ZERO NEXT SENTENCE ELSE                               
018635         IF VOL-DATE < DATE-2YRS GO TO TCA.                               
018640     IF VOL-REV-CODE = REV-SWITCH GO TO TCA.                      T3090   
018645     IF REV-SWITCH = 9 GO TO T5.                                          
018650     MOVE ZERO TO VOL-LATEST-HOLD, VOL-SYM-INDEX-N GO TO T5.      T3090   
018660 TCA.                                                             T3090   
018670     IF VOL-LATEST-HOLD = VOL-OLDEST-HOLD MOVE VOL-LATEST-HOLD    T3090   
018680         TO VOL-SYM-INDEX-N GO TO TRA-VOL-D.                      T3090   
018690     SUBTRACT 1 FROM VOL-LATEST-HOLD GIVING VOL-SYM-INDEX-N.      T3090   
018700     MOVE VOL-SYM-INDEX-N TO VOL-LATEST-HOLD.                     T3090   
018710     IF VOL-SYM-INDEX-N = ZERO MOVE ZERO TO VOL-LATEST-HOLD       T3090   
018720         GO TO TRA-VOL-D  ELSE GO TO TC.                          T3090   
018730 TRA-VOL-ERROR.                                                   T3090   
018740     DISPLAY  VOLFILE-REC.                                        T3090   
018750     DISPLAY  VOL-SYM-KEY.                                        T3090   
018760     GO TO TCA.                                                   T3090   
018770 TRA-VOL-X. EXIT.                                                 T3090   
018780 H1. MOVE S-FR-MO TO HDR5-MO-FR MOVE S-FR-DA TO HDR5-DA-FR.       T3090   
018790     MOVE S-FR-YR TO HDR5-YR-FR MOVE S-TO-MO TO HDR5-MO-TO.       T3090   
018800     MOVE S-TO-DA TO HDR5-DA-TO MOVE S-TO-YR TO HDR5-YR-TO.       T3090   
018810     MOVE MO TO HDR5-R-MO MOVE DA TO HDR5-R-DA.                   T3090   
018820     MOVE YR TO HDR5-R-YR.                                        T3090   
018830     WRITE PRINT-REC FROM HDR5 AFTER ADVANCING 1 LINES.           T3090   
018840 H-10.                                                            T3090   
018850     IF SW10A = 1 MOVE '        TRAFFIC  ACCIDENT  REPORT       ' T3090   
018860         TO HDR-2-3-TITLE ELSE MOVE '     ACCIDENT LOCATION ANALYST3090   
018870-            'IS REPORT  ' TO HDR-2-3-TITLE.                      T3090   
018880     WRITE PRINT-REC FROM HDR2-3 AFTER ADVANCING 2 LINES.         T3090   
018890     IF SW10A = 2 GO TO H-10-A.                                   T3090   
018900     MOVE T10-IS-CODE TO SYM-KEY.                                 T3090   
018910     READ ISFILE INVALID KEY MOVE 'INVALID INTERSECTION CODE' TO  T3090   
018920         HDR4-N1 MOVE SPACES TO HDR4-N2 GO TO V1.                 T3090   
018930     MOVE IS-NAME-1 TO HDR4-N1  MOVE IS-NAME-2 TO HDR4-N2.        T3090   
018940     MOVE IS-APPROACH-CHARS TO T10-SUM-LINEC2.                    T3090   
018950     MOVE IS-CLASS-CODE TO T10-SUM-LINEC1 (1).                    T3090   
018960     MOVE IS-NO-LEGS TO T10-SUM-LINEC1 (2).                       T3090   
018970     MOVE IS-CONTROL TO T10-SUM-LINEC1 (3).                       T3090   
018980     MOVE IS-SIGNAL-CTRL TO T10-SUM-LINEC1 (4).                   T3090   
018990     MOVE IS-SIGNAL-CHAR TO T10-SUM-LINEC1 (5).                   T3090   
019000     MOVE IS-STRAF TO ST-SYM-KEY.                                 T3090   
019010 V1. MOVE T10-REQUESTER TO HDR4-REQ.                              T3090   
019020     MOVE T10-IS-1 TO HDR4-ST1 MOVE T10-IS-2 TO HDR4-ST2.         T3090   
019030     WRITE PRINT-REC FROM HDR4 AFTER ADVANCING 2 LINES.           T3090   
019040     MOVE T10-SEARCH-DATES TO T12-SEARCH-DATES.                   T3090   
019050     PERFORM H1.                                                  T3090   
019060     MOVE SPACES TO PRINT-LINE.                                   T3090   
019070     MOVE ' DIV.                       SEVERITY             ******T3090   
019080-         '*** ACCIDENT DIAGRAM ********  DIR  ' TO PRINT-LINE.   T3090   
019090     MOVE '                                  A      ' TO ITEM-3-C1T3090   
019100     WRITE PRINT-REC AFTER ADVANCING 3 LINES.                     T3090   
019110     MOVE 'RECORD       ACCIDENT     INJURY  FAT   VEHICLE    ACC T3090   
019120-         '  VEH/PED      POINT OF        ANL C' TO PRINT-LINE.   T3090   
019130     MOVE 'ONTRIB.       DMV-CAUSE           G      ' TO ITEM-3-C1T3090   
019140     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
019150     MOVE 'NUMBER     DATE  DAY TIME  A  B  C     TYPE C CND  TYPET3090   
019160-         '  ACTIONS  I/S  IMPACT   DIR  CODE  ' TO PRINT-LINE.   T3090   
019170     MOVE 'CIRCUM      CODE   GROUP  W LIGHT E RC SC' TO ITEM-3-C1T3090   
019180     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
019190     MOVE SPACES TO PRINT-LINE  GO TO H-10-X.                     T3090   
019200 H-10-A.                                                          T3090   
019210     WRITE PRINT-REC FROM HDR4 AFTER ADVANCING 2 LINES.           T3090   
019220     WRITE PRINT-REC FROM HDR5 AFTER ADVANCING 1 LINES.           T3090   
019230     MOVE '                              *   *   *   *   I N T E RT3090   
019240-         ' S E C T I O N   S U M M A R Y   *   *   *   *'        T3090   
019250         TO PRINT-LINE.                                           T3090   
019260     WRITE PRINT-REC AFTER ADVANCING 3 LINES.                     T3090   
019270     MOVE '1. INTERSECTION ACCIDENT TYPES:   2. NON-INTERSECTION AT3090   
019280-         'CCIDENT TYPES:   3. NON-INTERSECTION ACCIDENT LOCATIONST3090   
019290-         ':' TO PRINT-LINE.                                      T3090   
019300     WRITE PRINT-REC AFTER ADVANCING 2 LINES.                     T3090   
019310     MOVE '     DESCRIPTION   ACCIDENTS           DESCRIPTION     T3090   
019320-         '                      STREET  LOC   ' TO PRINT-LINE.   T3090   
019330     MOVE '  A P P R O A C H       D E P A R T U R E' TO ITEM-3-C1T3090   
019340     WRITE PRINT-REC AFTER ADVANCING 1 LINES.                     T3090   
019350 H-10-X. EXIT.                                                    T3090   
019360 STRAF-RD.                                                        T3090   
019370     READ STRAT-FILE INVALID KEY GO TO STRAF-RD-X.                T3090   
019380     IF ST-DELETE-CODE = HIGH-VALUE GO TO STRAF-RD-X.             T3090   
019390     MOVE ST-M-1 TO LINEC-MEAN (2).                               T3090   
019400     MOVE ST-SD-1 TO LINEC-DEV (2).                               T3090   
019410     MOVE ST-CV-1 TO LINEC-CVA (2).                               T3090   
019420     MOVE ST-M-2 TO LINEC-MEAN (1).                               T3090   
019430     MOVE ST-SD-2 TO LINEC-DEV (1).                               T3090   
019440     MOVE ST-CV-2 TO LINEC-CVA (1).                               T3090   
019450     MOVE ST-M-3 TO LINEC-MEAN (4).                               T3090   
019460     MOVE ST-SD-3 TO LINEC-DEV (4).                               T3090   
019470     MOVE ST-CV-3 TO LINEC-CVA (4).                               T3090   
019480     MOVE ST-M-4 TO LINEC-MEAN (3).                               T3090   
019490     MOVE ST-SD-4 TO LINEC-DEV (3).                               T3090   
019500     MOVE ST-CV-4 TO LINEC-CVA (3).                               T3090   
019510 STRAF-RD-X. EXIT.                                                T3090   
019520 NO-D-RTN.                                                        T3090   
019522     MOVE C-M1 TO MOS-2. MOVE C-D1 TO DAS-2.                              
019523     MOVE C-Y1 TO YRS-2.                                                  
019524     IF T-D-HOLD = ZERO  MOVE MO TO MOS-2                                 
019526         MOVE DA TO DAS-2  MOVE YR TO YRS-2.                              
019527     SUBTRACT 4 FROM YRS-2.                                       AN122072
019530     IF T-D-HOLD = ZEROES MOVE 2190 TO TND GO TO NO-D-RTN-X.      T3090   
019540     IF C-M1 = 01 MOVE C-D1 TO TO-D-TOT GO TO NO-D-1.             T3090   
019550     SUBTRACT 1 FROM C-M1 GIVING MO-CT.                           T3090   
019560     MOVE MO-ENTRY (MO-CT) TO TO-D-TOT.                           T3090   
019570     ADD C-D1 TO TO-D-TOT.                                        T3090   
019580 NO-D-1.                                                          T3090   
019590     IF C-M = 01 MOVE C-D TO FR-D-TOT GO TO NO-D-2.               T3090   
019600     SUBTRACT 1 FROM C-M GIVING MO-CT.                            T3090   
019610     MOVE MO-ENTRY (MO-CT) TO FR-D-TOT.                           T3090   
019620     ADD C-D TO FR-D-TOT.                                         T3090   
019630 NO-D-2.                                                          T3090   
019640     COMPUTE YR-D-TOT =  (365 * (C-Y1 - C-Y)).                    T3090   
019650     IF FR-D-TOT > TO-D-TOT SUBTRACT 365 FROM YR-D-TOT,           T3090   
019660     COMPUTE TND = (((365 + TO-D-TOT) - FR-D-TOT) + YR-D-TOT)     T3090   
019670     GO TO NO-D-RTN-X.                                            T3090   
019680     COMPUTE TND = ((TO-D-TOT - FR-D-TOT) + YR-D-TOT).            T3090   
019690 NO-D-RTN-X. EXIT.                                                T3090   
019700 ST-ACC-ON.                                                       T3090   
019710     MOVE T10-DIR (1) TO DIR-VEH-HOLD.                            T3090   
019720     MOVE ZERO TO DIR-ST-SW.                                      T3090   
019730     MOVE T10-DIR (1) TO DIR-VEH-C.                               T3090   
019740     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019750     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019760     MOVE T10-DIR (2) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019770     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019780     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019790     MOVE T10-DIR (3) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019800     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019810     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019820     MOVE T10-DIR (4) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019830     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019840     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019850     MOVE T10-DIR (5) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019860     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019870     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019880     MOVE T10-DIR (6) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019890     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019900     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019910     MOVE T10-DIR (7) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019920     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019930     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019940     MOVE T10-DIR (8) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019950     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019960     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
019970     MOVE T10-DIR (9) TO DIR-VEH-C, T10-DIR (1).                  T3090   
019980     PERFORM CK-DIR-V THRU CK-DIR-V-X.                            T3090   
019990     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.                         T3090   
020000     MOVE ZERO TO T10-DIR (1), DIR-VEH-C.                         T3090   
020010 ST-ACC-ON-X.   EXIT.                                             T3090   
020020 CK-DIR-V.                                                        T3090   
020030     IF (SEC-CODE-1 = 1 OR SEC-CODE-1 = 3) AND                    KM120870
020032        (DIR-VEH-C = 1 OR DIR-VEH-C = 3)                          KM120870
020040     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.                        T3090   
020050     IF (SEC-CODE-1 = 2 OR SEC-CODE-1 = 4) AND                    KM120870
020052        (DIR-VEH-C = 2 OR DIR-VEH-C = 4)                          KM120870
020060     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.                        T3090   
020070 CK-DIR-V-X.   EXIT.                                              T3090   
020080 LOAD-NAME-TABLE.                                                 KM120171
020090     MOVE ZERO TO ANT.                                            KM120171
020100     MOVE SPACES TO NAME-TABLE.                                   KM120171
020110 LNT-LOOP.                                                        KM120171
020120     READ JURIS-FILE AT END GO TO LNT-EXIT.                       KM120171
020130     IF J-CODE = '*'                                              KM120171
020140        MOVE J-NAME TO H-SYSTEM                                   KM120171
020150        GO TO LNT-LOOP.                                           KM120171
020160     ADD 1 TO ANT.                                                KM120171
020170     IF ANT > 20                                                  KM120171
020180        GO TO LNT-EXIT.                                           KM120171
020190     MOVE J-CODE TO NT-CITY (ANT).                                KM120171
020200     MOVE J-NAME TO NT-NAME (ANT).                                KM120171
020210     GO TO LNT-LOOP.                                              KM120171
020220 LNT-EXIT.                                                        KM120171
020230     EXIT.                                                        KM120171
/*                                                                              
// LBLTYP NSD(05)                                                               
// EXEC LNKEDT                                                                  
/*                                                                              
/&                                                                              
    $ [