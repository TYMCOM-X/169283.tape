000001                                                                  FAAFIN  
000002    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000003-    7:10		PAGE 1                                                 FAAFIN  
000004    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000005                                                                  FAAFIN  
000006    0001    IDENTIFICATION  DIVISION.                             FAAFIN  
000007    0002    PROGRAM-ID.     "BB01A".                              FAAFIN  
000008    0003    AUTHOR.          WESTERN REGION PROGRAMMING STAFF.    FAAFIN  
000009    0004    INSTALLATION.    FAA, WESTERN REGION.                 FAAFIN  
000010    0005    DATE-WRITTEN.    16 MAR 1972.                         FAAFIN  
000011    0006    DATE-COMPILED. 04-JUN-73.                             FAAFIN  
000012    0007    REMARKS.                                              FAAFIN  
000013    0008        PURPOSE.                                          FAAFIN  
000014    0009        VALIDATE SIP-6 CARD INPUT AND PREPARE BATCH LISTINFAAFIN  
000015-    G                                                            FAAFIN  
000016    0010        SHOWING CODING ERRORS AND TOTALS.                 FAAFIN  
000017    0011        REVISIONS.                                        FAAFIN  
000018    0012        01-20-73                                          FAAFIN  
000019    0013        PROGRAMMER KEN ELSING, FAA WESTERN REGION.        FAAFIN  
000020    0014        MODIFY EDIT AND CHANGE HEADINGS.                  FAAFIN  
000021    0015    ENVIRONMENT  DIVISION.                                FAAFIN  
000022    0016    CONFIGURATION  SECTION.                               FAAFIN  
000023    0017    SOURCE-COMPUTER. DECsystem-10.                        FAAFIN  
000024    0018    OBJECT-COMPUTER. PDP-10.                              FAAFIN  
000025    0019    INPUT-OUTPUT  SECTION.                                FAAFIN  
000026    0020    FILE-CONTROL.                                         FAAFIN  
000027    0021        SELECT  TRANS   ASSIGN DSK,                       FAAFIN  
000028    0022          RECORDING MODE IS ASCII.                        FAAFIN  
000029    0023                                                          FAAFIN  
000030    0024        SELECT  PRINT   ASSIGN DSK,                       FAAFIN  
000031    0025          RECORDING MODE IS ASCII.                        FAAFIN  
000032    0026                                                          FAAFIN  
000033    0027    DATA  DIVISION.                                       FAAFIN  
000034    0028    FILE  SECTION.                                        FAAFIN  
000035    0029    FD   TRANS                                            FAAFIN  
000036    0030        RECORD  CONTAINS   80  CHARACTERS                 FAAFIN  
000037    0031        LABEL   RECORDS   ARE  STANDARD                   FAAFIN  
000038    0032           VALUE OF IDENTIFICATION IS "TRANS DAT"         FAAFIN  
000039    0033        DATA    RECORD    IS   REC-IN.                    FAAFIN  
000040    0034    01   REC-IN          PICTURE X(80).                   FAAFIN  
000041    0035    FD   PRINT                                            FAAFIN  
000042    0036        RECORD  CONTAINS  133  CHARACTERS                 FAAFIN  
000043    0037        LABEL RECORDS ARE STANDARD                        FAAFIN  
000044    0038           VALUE OF IDENTIFICATION IS "PRINT DAT"         FAAFIN  
000045    0039           DATA RECORD IS PRINT-LINE.                     FAAFIN  
000046    0040    01   PRINT-LINE.                                      FAAFIN  
000047    0041        03  FILLER          PICTURE X.                    FAAFIN  
000048    0042        03  PRINT-IO        PICTURE X(132).               FAAFIN  
000049    0043        03  DETAIL-LINE REDEFINES PRINT-IO.               FAAFIN  
000050    0044            05  FILLER          PICTURE X(3).             FAAFIN  
000051    0045            05  BATCH-RPT       PICTURE X(3).             FAAFIN  
000052    0046            05  FILLER          PICTURE X(3).             FAAFIN  
000053    0047            05  REG-RPT         PICTURE X.                FAAFIN  
000054    0048            05  CST-CEN-RPT     PICTURE X(4).             FAAFIN  
000055    0049            05  FILLER          PICTURE X(4).             FAAFIN  
000056    0050            05  LOCATION-RPT    PICTURE X(4).             FAAFIN  
000057    0051            05  FILLER          PICTURE XX.               FAAFIN  
000058    0052            05  FAC-TYPE-RPT    PICTURE X(5).             FAAFIN  
000059    0053            05  FILLER          PICTURE XX.               FAAFIN  
000060    0054            05  FSNA-RPT        PICTURE X(4).             FAAFIN  
000061    0055            05  DASH1-RPT       PICTURE X.                FAAFIN  
000062    0056            05  FSNB-RPT        PICTURE X(3).             FAAFIN  
000063    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000064-    7:10		PAGE 1-1                                               FAAFIN  
000065    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000066                                                                  FAAFIN  
000067    0057            05  DASH2-RPT       PICTURE X.                FAAFIN  
000068    0058            05  FSNC-RPT        PICTURE X(3).             FAAFIN  
000069    0059            05  FSND-RPT        PICTURE X.                FAAFIN  
000070    0060            05  FILLER          PICTURE X.                FAAFIN  
000071    0061            05  EXCESS-FLAG-RPT PICTURE X.                FAAFIN  
000072    0062            05  FILLER          PICTURE XX.               FAAFIN  
000073    0063            05  EQUIP-RPT       PICTURE X(14).            FAAFIN  
000074    0064            05  FILLER          PICTURE XX.               FAAFIN  
000075    0065            05  OWN-RPT         PICTURE X.                FAAFIN  
000076    0066            05  FILLER          PICTURE X(3).             FAAFIN  
000077    0067            05  ASS-CODE-RPT    PICTURE XX.               FAAFIN  
000078    0068            05  FILLER          PICTURE X(3).             FAAFIN  
000079    0069            05  QUANTITY-RPT    PICTURE XX.               FAAFIN  
000080    0070            05  FILLER          PICTURE X.                FAAFIN  
000081    0071            05  UNIT-VALUE-RPT  PICTURE Z,ZZZ,ZZZ.99.     FAAFIN  
000082    0072            05  TOTAL-VALUE-RPT PICTURE ZZZ,ZZZ,ZZZ.99.   FAAFIN  
000083    0073            05  FILLER          PICTURE X(3).             FAAFIN  
000084    0074            05  TRANS-CODE-RPT  PICTURE XX.               FAAFIN  
000085    0075            05  FILLER          PICTURE X(3).             FAAFIN  
000086    0076            05  MONTH-RPT       PICTURE X.                FAAFIN  
000087    0077            05  DASH3-RPT       PICTURE X.                FAAFIN  
000088    0078            05  YEAR-RPT        PICTURE XX.               FAAFIN  
000089    0079            05  FILLER          PICTURE X(3).             FAAFIN  
000090    0080            05  DOC-NUM-RPT     PICTURE X(5).             FAAFIN  
000091    0081            05  FILLER          PICTURE X(10).            FAAFIN  
000092    0082        03  TOTAL-LINE-RPT REDEFINES PRINT-IO.            FAAFIN  
000093    0083            05  FILLER          PICTURE X(19).            FAAFIN  
000094    0084            05  T-1             PICTURE X(8).             FAAFIN  
000095    0085            05  T-2             PICTURE X(6).             FAAFIN  
000096    0086            05  T-3             PICTURE X(8).             FAAFIN  
000097    0087            05  FILLER          PICTURE X(10).            FAAFIN  
000098    0088            05  T-4             PICTURE X(8).             FAAFIN  
000099    0089            05  FILLER          PICTURE X(6).             FAAFIN  
000100    0090            05  DECR-CNT-RPT    PICTURE ZZ,ZZZ.           FAAFIN  
000101    0091            05  FILLER          PICTURE X(3).             FAAFIN  
000102    0092            05  DECR-AMT-RPT    PICTURE ZZZ,ZZZ,ZZZ.ZZCR. FAAFIN  
000103    0093            05  FILLER          PICTURE X(3).             FAAFIN  
000104    0094            05  T-5             PICTURE X(8).             FAAFIN  
000105    0095            05  FILLER          PICTURE X(3).             FAAFIN  
000106    0096            05  INCR-CNT-RPT    PICTURE ZZ,ZZZ.           FAAFIN  
000107    0097            05  FILLER          PICTURE XX.               FAAFIN  
000108    0098            05  INCR-AMT-RPT    PICTURE ZZZ,ZZZ,ZZZ.ZZCR. FAAFIN  
000109    0099            05  FILLER          PICTURE X(4).             FAAFIN  
000110    0100    WORKING-STORAGE  SECTION.                             FAAFIN  
000111    0101    77   PRT-CTRL   PICTURE X.                            FAAFIN  
000112    0102    77   ERROR-IND           PICTURE X   VALUE "0".       FAAFIN  
000113    0103    77   FIRST-RCD-IND       PICTURE X   VALUE "0".       FAAFIN  
000114    0104    77   END-JOB-IND         PICTURE X   VALUE "0".       FAAFIN  
000115    0105    77   WS-REGION-HOLD      PICTURE X   VALUE SPACE.     FAAFIN  
000116    0106    77   WS-BATCH-HOLD       PICTURE X(3)  VALUE SPACE.   FAAFIN  
000117    0107    77   WS-DOC-NUM          PICTURE X(5)  VALUE SPACE.   FAAFIN  
000118    0108    77   AC-HOLD             PICTURE XX  VALUE SPACE.     FAAFIN  
000119    0109    77   TR-HOLD             PICTURE XX  VALUE SPACE.     FAAFIN  
000120    0110    77   SUB         PICTURE S99 USAGE IS COMPUTATIONAL.  FAAFIN  
000121    0111    77   LINE-CTR        PICTURE S99         COMPUTATIONALFAAFIN  
000122-       VALUE 0.                                                  FAAFIN  
000123    0112    77   PAGE-CTR        PICTURE S9(3)       COMPUTATIONALFAAFIN  
000124-       VALUE 0.                                                  FAAFIN  
000125    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000126-    7:10		PAGE 1-2                                               FAAFIN  
000127    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000128                                                                  FAAFIN  
000129    0113    77   DOC-DECR-CTR    PICTURE S9(5)       COMPUTATIONALFAAFIN  
000130-       VALUE 0.                                                  FAAFIN  
000131    0114    77   DOC-INCR-CTR    PICTURE S9(5)       COMPUTATIONALFAAFIN  
000132-       VALUE 0.                                                  FAAFIN  
000133    0115    77   BATCH-DECR-CTR  PICTURE S9(5)       COMPUTATIONALFAAFIN  
000134-       VALUE 0.                                                  FAAFIN  
000135    0116    77   BATCH-INCR-CTR  PICTURE S9(5)       COMPUTATIONALFAAFIN  
000136-       VALUE 0.                                                  FAAFIN  
000137    0117    77   BATCH-CTR       PICTURE S9(5)       COMPUTATIONALFAAFIN  
000138-       VALUE 0.                                                  FAAFIN  
000139    0118    77   REGION-CTR      PICTURE S9(5)       COMPUTATIONALFAAFIN  
000140-       VALUE 0.                                                  FAAFIN  
000141    0119    77   OV-CTR          PICTURE S9(5)       COMPUTATIONALFAAFIN  
000142-       VALUE 0.                                                  FAAFIN  
000143    0120    77   DOC-DECR-AMT    PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000144-       VALUE 0.                                                  FAAFIN  
000145    0121    77   DOC-INCR-AMT    PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000146-       VALUE 0.                                                  FAAFIN  
000147    0122    77   BATCH-DECR-AMT  PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000148-       VALUE 0.                                                  FAAFIN  
000149    0123    77   BATCH-INCR-AMT  PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000150-       VALUE 0.                                                  FAAFIN  
000151    0124    77   BATCH-AMT       PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000152-       VALUE 0.                                                  FAAFIN  
000153    0125    77   REGION-AMT      PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000154-       VALUE 0.                                                  FAAFIN  
000155    0126    77   OV-AMT          PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000156-       VALUE 0.                                                  FAAFIN  
000157    0127    77   TEMP-TOTAL      PICTURE S9(9)V99    COMPUTATIONALFAAFIN  
000158-       VALUE 0.                                                  FAAFIN  
000159    0128    77   UNIT-VALUE-WS   PICTURE S9(9)V99  COMPUTATIONAL  FAAFIN  
000160-     VALUE 0.                                                    FAAFIN  
000161    0129    77   QUAN-WS         PICTURE S9(3)     COMPUTATIONAL  FAAFIN  
000162-     VALUE 0.                                                    FAAFIN  
000163    0130    01   WORK-CONSTANTS.                                  FAAFIN  
000164    0131        03  HYPHEN      PICTURE X   VALUE  "-".           FAAFIN  
000165    0132        03  DOCMNT      PICTURE X(8)   VALUE "DOCUMENT".  FAAFIN  
000166    0133        03  TOTL        PICTURE X(8)   VALUE "TOTAL   ".  FAAFIN  
000167    0134        03  INCR        PICTURE X(8)   VALUE "INCREASE".  FAAFIN  
000168    0135        03  DECR        PICTURE X(8)   VALUE "DECREASE".  FAAFIN  
000169    0136        03  ITEM        PICTURE X(6)   VALUE "ITEM  ".    FAAFIN  
000170    0137        03  BATCH       PICTURE X(8)   VALUE "  BATCH ".  FAAFIN  
000171    0138        03  TVAL        PICTURE X(8)   VALUE "  VALUE ".  FAAFIN  
000172    0139        03  TCNT        PICTURE X(8)   VALUE "  COUNT ".  FAAFIN  
000173    0140        03  TNET        PICTURE X(8)   VALUE "   NET  ".  FAAFIN  
000174    0141    01   TRANS-INPUT-WS              PICTURE X(80) VALUE SFAAFIN  
000175-    PACES.                                                       FAAFIN  
000176    0142    01   TR-IN-WS-X REDEFINES TRANS-INPUT-WS.             FAAFIN  
000177    0143        03  FILL-BB                 PICTURE XX.           FAAFIN  
000178    0144        03  REGION-IN               PICTURE X.            FAAFIN  
000179    0145        03  CST-CEN-CODE-IN         PICTURE X(4).         FAAFIN  
000180    0146        03  LOCATION-IN             PICTURE X(4).         FAAFIN  
000181    0147        03  FAC-ID-IN.                                    FAAFIN  
000182    0148            05  SYSTEM-IN           PICTURE X.            FAAFIN  
000183    0149            05  CATEGORY-IN         PICTURE X.            FAAFIN  
000184    0150            05  FAC-TYPE-IN.                              FAAFIN  
000185    0151                07  FAC-TYPE-1ST    PICTURE X.            FAAFIN  
000186    0152                07  FAC-TYPE-2ND    PICTURE X.            FAAFIN  
000187    0153            05  M-CODE              PICTURE X.            FAAFIN  
000188    0154        03  FSN-IN.                                       FAAFIN  
000189    0155            05  FSNA-IN             PICTURE X(4).         FAAFIN  
000190    0156            05  FSNB-IN             PICTURE X(3).         FAAFIN  
000191    0157            05  FSNLAST-4-IN.                             FAAFIN  
000192    0158                07  FSNC-IN         PICTURE X(3).         FAAFIN  
000193    0159                07  FSND-IN         PICTURE X.            FAAFIN  
000194    0160        03  EQUIPMENT-IN            PICTURE X(14).        FAAFIN  
000195    0161        03  OWN-IN                  PICTURE X.            FAAFIN  
000196    0162        03  ASSET-CODE-IN           PICTURE XX.           FAAFIN  
000197    0163        03  QUANTITY-IN             PICTURE XX.           FAAFIN  
000198    0164        03  QUANTITY-IN-9 REDEFINES QUANTITY-IN PICTURE 99FAAFIN  
000199-    .                                                            FAAFIN  
000200    0165        03  UNIT-VALUE-IN           PICTURE 9(7)V99.      FAAFIN  
000201    0166        03  UNIT-VALUE-X REDEFINES UNIT-VALUE-IN PICTURE XFAAFIN  
000202-    (9).                                                         FAAFIN  
000203    0167        03  TOTAL-VALUE-IN          PICTURE 9(7)V99.      FAAFIN  
000204    0168        03  TOTAL-VALUE-X REDEFINES TOTAL-VALUE-IN PICTUREFAAFIN  
000205-     X(9).                                                       FAAFIN  
000206    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000207-    7:10		PAGE 1-3                                               FAAFIN  
000208    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000209                                                                  FAAFIN  
000210    0169        03  TRANS-CODE-IN           PICTURE XX.           FAAFIN  
000211    0170        03  ACTION-DATE-IN.                               FAAFIN  
000212    0171            05  MONTH-IN            PICTURE X.            FAAFIN  
000213    0172            05  YEAR-IN             PICTURE XX.           FAAFIN  
000214    0173        03  DOC-NUM-IN              PICTURE X(5).         FAAFIN  
000215    0174        03  BATCH-NUM-IN            PICTURE X(3).         FAAFIN  
000216    0175        03  FILLER                  PICTURE X(3).         FAAFIN  
000217    0176    01   HEAD-1.                                          FAAFIN  
000218    0177        03  FILLER      PICTURE X(16)  VALUE "RIN_  BB01ARFAAFIN  
000219-    "1   ".                                                      FAAFIN  
000220    0178        03  REGION-HDR  PICTURE X(21)  VALUE SPACES.      FAAFIN  
000221    0179        03  FILLER      PICTURE X(60)  VALUE "     PERSONAFAAFIN  
000222-    "L PROPERTY INVENTORY BATCH LISTING              ".          FAAFIN  
000223    0181        03  DATE-HDR    PICTURE X(11)  VALUE SPACES.      FAAFIN  
000224    0182        03  FILLER      PICTURE X(20)  VALUE "            FAAFIN  
00    "   PAGE ".                                                  FAAFIN  
000226    0183        03  PAGE-HDR    PICTURE ZZ.                       FAAFIN  
000227    0184        03  FILLER      PICTURE X     VALUE SPACE.        FAAFIN  
000228    0185    01   HEAD-2.                                          FAAFIN  
000229    0186        03  FILLER      PICTURE X(20) VALUE "  BATCH  COSTFAAFIN  
000230-    "     LO".                                                   FAAFIN  
000231    0187        03  FILLER      PICTURE X(20) VALUE "CA  FAC    FEFAAFIN  
000232-    "D STK  ".                                                   FAAFIN  
000233    0188        03  FILLER      PICTURE X(20) VALUE "NO     EQUIP FAAFIN  
000234-    " TYPE  ".                                                   FAAFIN  
000235    0189        03  FILLER      PICTURE X(20) VALUE "    O ASSET QFAAFIN  
000236-    "UAN    ".                                                   FAAFIN  
000237    0190        03  FILLER      PICTURE X(3) VALUE SPACE.         FAAFIN  
000238    0191        03  FILLER      PICTURE X(20) VALUE "UNIT       SUFAAFIN  
000239-    "MMARY  ".                                                   FAAFIN  
000240    0192        03  FILLER      PICTURE X(20) VALUE " TRS  ACTION FAAFIN  
000241-    " DOCMT ".                                                   FAAFIN  
000242    0193        03  FILLER      PICTURE X(9) VALUE SPACE.         FAAFIN  
000243    0194    01   HEAD-3.                                          FAAFIN  
000244    0195        03  FILLER      PICTURE X(20) VALUE "    NO    CENFAAFIN  
000245-    "TER  TI".                                                   FAAFIN  
000246    0196        03  FILLER      PICTURE X(20) VALUE "ON   TYPE  FCFAAFIN  
000247-    "LT OR I".                                                   FAAFIN  
000248    0197        03  FILLER      PICTURE X(20) VALUE "NST      SERIFAAFIN  
000249-    "AL  NO ".                                                   FAAFIN  
000250    0198        03  FILLER      PICTURE X(20) VALUE "    W  CODE TFAAFIN  
000251-    "ITY    ".                                                   FAAFIN  
000252    0199        03  FILLER      PICTURE X(3) VALUE SPACE.         FAAFIN  
000253    0200        03  FILLER      PICTURE X(20) VALUE "PRICE      VAFAAFIN  
000254-    "LUE    ".                                                   FAAFIN  
000255    0201        03  FILLER      PICTURE X(20) VALUE "  CD   M YR  FAAFIN  
000256-    "    NO ".                                                   FAAFIN  
000257    0202        03  FILLER      PICTURE X(9) VALUE SPACE.         FAAFIN  
000258    0203    01   ERROR-LN        PICTURE X(132)  VALUE SPACES.    FAAFIN  
000259    0204    01   ERROR-LINE REDEFINES ERROR-LN.                   FAAFIN  
000260    0205        03  FILLER      PICTURE X(3).                     FAAFIN  
000261    0206        03  BTCH-ERR    PICTURE XXX.                      FAAFIN  
000262    0207        03  FILLER      PICTURE XXX.                      FAAFIN  
000263    0208        03  CC-ERR.                                       FAAFIN  
000264    0209            05  REG-ERR PICTURE X.                        FAAFIN  
000265    0210            05  CST-ERR PICTURE X(4).                     FAAFIN  
000266    0211        03  FILLER      PICTURE X(4).                     FAAFIN  
000267    0212        03  LOC-ERR     PICTURE X(4).                     FAAFIN  
000268    0213        03  FILLER      PICTURE XX.                       FAAFIN  
000269    0214        03  FACTPE-ERR  PICTURE X(5).                     FAAFIN  
000270    0215        03  FILLER      PICTURE XX.                       FAAFIN  
000271    0216        03  FSN.                                          FAAFIN  
000272    0217            05  FSNA-ERR PICTURE X(4).                    FAAFIN  
000273    0218            05  FILLER   PICTURE X.                       FAAFIN  
000274    0219            05  FSNB-ERR PICTURE XXX.                     FAAFIN  
000275    0220            05  FILLER   PICTURE X.                       FAAFIN  
000276    0221            05  FSNC-ERR PICTURE XXX.                     FAAFIN  
000277    0222            05  FSND-ERR PICTURE X.                       FAAFIN  
000278    0223        03  FILLER       PICTURE X(4).                    FAAFIN  
000279    0224        03  EQUIP-ERR    PICTURE X(14).                   FAAFIN  
000280    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000281-    7:10		PAGE 1-4                                               FAAFIN  
000282    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000283                                                                  FAAFIN  
000284    0225        03  FILLER     PICTURE   XX.                      FAAFIN  
000285    0226        03  OWN-ERR      PICTURE X.                       FAAFIN  
000286    0227        03  FILLER       PICTURE XXX.                     FAAFIN  
000287    0228        03  AC-ERR      PICTURE XX.                       FAAFIN  
000288    0229        03  FILLER      PICTURE XXX.                      FAAFIN  
000289    0230        03  QUAN-ERR    PICTURE XX.                       FAAFIN  
000290    0231        03  FILLER      PICTURE XXX.                      FAAFIN  
000291    0232        03  PRICE-ERR   PICTURE X(10).                    FAAFIN  
000292    0233        03  FILLER      PICTURE X(3).                     FAAFIN  
000293    0234        03  VALUE-ERR   PICTURE X(11).                    FAAFIN  
000294    0235        03  FILLER      PICTURE X(3).                     FAAFIN  
000295    0236        03  TR-CD-ERR   PICTURE XX.                       FAAFIN  
000296    0237        03  FILLER      PICTURE XXX.                      FAAFIN  
000297    0238        03  ACT-DTE-ERR PICTURE XXXX.                     FAAFIN  
000298    0239        03  FILLER      PICTURE X(6).                     FAAFIN  
000299    0240        03  DOC-NO-ERR  PICTURE X(5).                     FAAFIN  
000300    0241        03  ERR-FLG     PICTURE X(10).                    FAAFIN  
000301    0242    01   ADD-TBLS.                                        FAAFIN  
000302    0243        03  FILLER      PICTURE X(40)   VALUE             FAAFIN  
000303    0244            "0A0B0C0D0E0F0G00010203040506070851535557".   FAAFIN  
000304    0245    01   ADDS        REDEFINES  ADD-TBLS.                 FAAFIN  
000305    0246        03  INCR-TBL    OCCURS  37  TIMES  PICTURE XX.    FAAFIN  
000306    0247    01   DELETES.                                         FAAFIN  
000307    0248        03  FILLER      PICTURE X(44)   VALUE             FAAFIN  
000308    0249            "1A1B1C1D1F1G1H1I1J1K10111214151718212325271M"FAAFIN  
000309-    .                                                            FAAFIN  
000310    0250    01   SUB-DR REDEFINES DELETES.                        FAAFIN  
000311    0251        03  DECR-TBL    OCCURS  22  TIMES  PICTURE XX.    FAAFIN  
000312    0252    01   ASSET-CODE-TABLE.                                FAAFIN  
000313    0253        03  FILLER  VALUE  "11" PICTURE  XX.              FAAFIN  
000314    0254        03  FILLER  VALUE  "12" PICTURE  XX.              FAAFIN  
000315    0255        03  FILLER  VALUE  "13" PICTURE  XX.              FAAFIN  
000316    0256        03  FILLER  VALUE  "14" PICTURE  XX.              FAAFIN  
000317    0257        03  FILLER  VALUE  "15" PICTURE  XX.              FAAFIN  
000318    0258        03  FILLER  VALUE  "16" PICTURE  XX.              FAAFIN  
000319    0259        03  FILLER  VALUE  "20" PICTURE  XX.              FAAFIN  
000320    0260        03  FILLER  VALUE  "41" PICTURE  XX.              FAAFIN  
000321    0261        03  FILLER  VALUE  "42" PICTURE  XX.              FAAFIN  
000322    0262        03  FILLER  VALUE  "43" PICTURE  XX.              FAAFIN  
000323    0263        03  FILLER  VALUE  "44" PICTURE  XX.              FAAFIN  
000324    0264        03  FILLER  VALUE  "45" PICTURE  XX.              FAAFIN  
000325    0265        03  FILLER  VALUE  "46" PICTURE  XX.              FAAFIN  
000326    0266        03  FILLER  VALUE  "61" PICTURE  XX.              FAAFIN  
000327    0267        03  FILLER  VALUE  "62" PICTURE  XX.              FAAFIN  
000328    0268        03  FILLER  VALUE  "63" PICTURE  XX.              FAAFIN  
000329    0269        03  FILLER  VALUE  "64" PICTURE  XX.              FAAFIN  
000330    0270        03  FILLER  VALUE  "81" PICTURE  XX.              FAAFIN  
000331    0271        03  FILLER  VALUE  "82" PICTURE  XX.              FAAFIN  
000332    0272        03  FILLER  VALUE  "83" PICTURE  XX.              FAAFIN  
000333    0273    01   G1  REDEFINES ASSET-CODE-TABLE.                  FAAFIN  
000334    0274        03  AC OCCURS 20 TIMES    PICTURE XX.             FAAFIN  
000335    0275    01   TABLE-B.                                         FAAFIN  
000336    0276        03  REGION-TABLE.                                 FAAFIN  
000337    0277            05  FILLER  PICTURE X(22) VALUE "4WESTERN REGIFAAFIN  
000338-    "ON       ".                                                 FAAFIN  
000339    0278            05  FILLER  PICTURE X(22) VALUE "DROCKY MOUNTAFAAFIN  
000340-    "IN REGION".                                                 FAAFIN  
000341    0279            05  FILLER  PICTURE X(22) VALUE "SNORTHWEST REFAAFIN  
000342-    "GION     ".                                                 FAAFIN  
000343    0280            05  FILLER  PICTURE X(22) VALUE "ENEW ENGLAND FAAFIN  
000344-    "REGION   ".                                                 FAAFIN  
000345    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000346-    7:10		PAGE 1-5                                               FAAFIN  
000347    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000348                                                                  FAAFIN  
000349    0281            05  FILLER  PICTURE X(22) VALUE "CGREAT LAKES FAAFIN  
000350-    "REGION   ".                                                 FAAFIN  
000351    0282            05  FILLER  PICTURE X(22) VALUE "1EASTERN REGIFAAFIN  
000352-    "ON       ".                                                 FAAFIN  
000353    0283            05  FILLER  PICTURE X(22) VALUE "2SOUTHWEST REFAAFIN  
000354-    "GION     ".                                                 FAAFIN  
000355    0284            05  FILLER  PICTURE X(22) VALUE "3CENTRAL REGIFAAFIN  
000356-    "ON       ".                                                 FAAFIN  
000357    0285            05  FILLER  PICTURE X(22) VALUE "5ALASKAN REGIFAAFIN  
000358-    "ON       ".                                                 FAAFIN  
000359    0286            05  FILLER  PICTURE X(22) VALUE "6PACIFIC REGIFAAFIN  
000360-    "ON       ".                                                 FAAFIN  
000361    0287            05  FILLER  PICTURE X(22) VALUE "7SOUTHERN REGFAAFIN  
000362-    "ION      ".                                                 FAAFIN  
000363    0288        03  REG-TBL REDEFINES REGION-TABLE                FAAFIN  
000364    0289                    OCCURS 11 TIMES PICTURE X(22).        FAAFIN  
000365    0290    01   REG-BRKDWN  PICTURE X(22)   VALUE SPACES.        FAAFIN  
000366    0291    01   R-BRK REDEFINES REG-BRKDWN.                      FAAFIN  
000367    0292        03  REG-CODE-TBL        PICTURE X.                FAAFIN  
000368    0293        03  REG-NAME-TBL        PICTURE X(21).            FAAFIN  
000369    0294    01   NUMERIC-CHECK-AREA.                              FAAFIN  
000370    0295        03  NUMB-TEST-9     PICTURE X(10) VALUE "000000000FAAFIN  
000371-    "0".                                                         FAAFIN  
000372    0296        03  N-T-9 REDEFINES NUMB-TEST-9.                  FAAFIN  
000373    0297            05  NINE-TEST   PICTURE X(9).                 FAAFIN  
000374    0298            05  FILLER      PICTURE X.                    FAAFIN  
000375    0299        03  NUMB-TEST-5     PICTURE X(6)  VALUE "000000". FAAFIN  
000376    0300        03  N-T-5 REDEFINES NUMB-TEST-5.                  FAAFIN  
000377    0301            05  FIVE-TEST   PICTURE X(5).                 FAAFIN  
000378    0302            05  FILLER      PICTURE X.                    FAAFIN  
000379    0303        03  NUMB-TEST-4     PICTURE X(5)  VALUE "00000".  FAAFIN  
000380    0304        03  N-T-4 REDEFINES NUMB-TEST-4.                  FAAFIN  
000381    0305            05  FOUR-TEST   PICTURE X(4).                 FAAFIN  
000382    0306            05  FILLER      PICTURE X.                    FAAFIN  
000383    0307        03  NUMB-TEST-3     PICTURE X(4)  VALUE "0000".   FAAFIN  
000384    0308        03  N-T-3 REDEFINES NUMB-TEST-3.                  FAAFIN  
000385    0309            05  THREE-TEST  PICTURE X(3).                 FAAFIN  
000386    0310            05  FILLER      PICTURE X.                    FAAFIN  
000387    0311    PROCEDURE  DIVISION.                                  FAAFIN  
000388    0312        OPEN INPUT  TRANS,                                FAAFIN  
000389    0313             OUTPUT PRINT.                                FAAFIN  
000390    0314        DISPLAY "KEY IN TODAYS DATE - IE_ JAN 10 1973" UPOFAAFIN  
000391-    N CONSOLE.                                                   FAAFIN  
000392    0315        ACCEPT DATE-HDR.                                  FAAFIN  
000393    0316    010-READ-INPUT.                                       FAAFIN  
000394    0317        READ TRANS INTO TRANS-INPUT-WS AT END             FAAFIN  
000395    0318            MOVE "1" TO END-JOB-IND                       FAAFIN  
000396    0319            GO TO 350-INPUT-AT-END.                       FAAFIN  
000397    0320        IF FIRST-RCD-IND = "0"                            FAAFIN  
000398    0321            PERFORM 280-FIND-REGION-HDR THRU 290-FIND-REG-FAAFIN  
000399-    EXIT                                                         FAAFIN  
000400    0322            PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXFAAFIN  
000401-    IT                                                           FAAFIN  
000402    0323            MOVE "1" TO FIRST-RCD-IND                     FAAFIN  
000403    0324            MOVE REGION-IN TO WS-REGION-HOLD              FAAFIN  
000404    0325            MOVE BATCH-NUM-IN TO WS-BATCH-HOLD            FAAFIN  
000405    0326            MOVE DOC-NUM-IN TO WS-DOC-NUM.                FAAFIN  
000406    0327    020-COMPARE-INPUT-TO-HOLDS.                           FAAFIN  
000407    0328        IF WS-REGION-HOLD NOT = REGION-IN                 FAAFIN  
000408    0329            PERFORM 260-END-REGION-RTN THRU 270-END-REGIONFAAFIN  
000409-    -EXIT                                                        FAAFIN  
000410    0330            GO TO 030-SET-UP-PRINT-LINE.                  FAAFIN  
000411    0331        IF WS-BATCH-HOLD NOT = BATCH-NUM-IN               FAAFIN  
000412    0332            PERFORM 240-END-BATCH-RTN THRU 250-BATCH-RTN-EFAAFIN  
000413-    XIT                                                          FAAFIN  
000414    0333            MOVE ZEROS TO PAGE-CTR LINE-CTR               FAAFIN  
000415    0334            PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXFAAFIN  
000416-    IT                                                           FAAFIN  
000417    0335            GO TO 030-SET-UP-PRINT-LINE.                  FAAFIN  
000418    0336        IF WS-DOC-NUM NOT = DOC-NUM-IN                    FAAFIN  
000419    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000420-    7:10		PAGE 1-6                                               FAAFIN  
000421    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000422                                                                  FAAFIN  
000423    0337            PERFORM 220-END-DOC-RTN THRU 230-END-DOC-EXIT.FAAFIN  
000424    0338    030-SET-UP-PRINT-LINE.                                FAAFIN  
000425    0339        MOVE REGION-IN       TO REG-RPT.                  FAAFIN  
000426    0340        MOVE CST-CEN-CODE-IN TO CST-CEN-RPT.              FAAFIN  
000427    0341        MOVE LOCATION-IN     TO LOCATION-RPT.             FAAFIN  
000428    0342        MOVE FAC-ID-IN       TO FAC-TYPE-RPT.             FAAFIN  
000429    0343        MOVE FSNA-IN         TO FSNA-RPT.                 FAAFIN  
000430    0344        MOVE FSNB-IN         TO FSNB-RPT.                 FAAFIN  
000431    0345        MOVE FSNC-IN         TO FSNC-RPT.                 FAAFIN  
000432    0346        MOVE FSND-IN         TO FSND-RPT.                 FAAFIN  
000433    0347        MOVE HYPHEN          TO DASH1-RPT, DASH2-RPT, DASHFAAFIN  
000434-    3-RPT.                                                       FAAFIN  
000435    0348        MOVE EQUIPMENT-IN    TO EQUIP-RPT.                FAAFIN  
000436    0349        MOVE OWN-IN          TO OWN-RPT.                  FAAFIN  
000437    0350        MOVE ASSET-CODE-IN TO ASS-CODE-RPT.               FAAFIN  
000438    0351        MOVE QUANTITY-IN     TO QUANTITY-RPT.             FAAFIN  
000439    0352        MOVE UNIT-VALUE-IN   TO UNIT-VALUE-RPT.           FAAFIN  
000440    0353        MOVE TRANS-CODE-IN   TO TRANS-CODE-RPT.           FAAFIN  
000441    0354        MOVE MONTH-IN        TO MONTH-RPT.                FAAFIN  
000442    0355        MOVE YEAR-IN         TO YEAR-RPT.                 FAAFIN  
000443    0356        MOVE DOC-NUM-IN      TO DOC-NUM-RPT.              FAAFIN  
000444    0357        MOVE BATCH-NUM-IN    TO BATCH-RPT.                FAAFIN  
000445    0358        MOVE CST-CEN-CODE-IN TO FOUR-TEST.                FAAFIN  
000446    0359        IF NUMB-TEST-4 IS NOT NUMERIC                     FAAFIN  
000447    0360            MOVE "****" TO CST-ERR                        FAAFIN  
000448    0361             MOVE "1" TO ERROR-IND.                       FAAFIN  
000449    0362        IF LOCATION-IN IS NOT ALPHABETIC                  FAAFIN  
000450    0363            MOVE "****" TO LOC-ERR                        FAAFIN  
000451    0364             MOVE "1" TO ERROR-IND.                       FAAFIN  
000452    0365        IF SYSTEM-IN = SPACE OR                           FAAFIN  
000453    0366            CATEGORY-IN = SPACE OR                        FAAFIN  
000454    0367            FAC-TYPE-2ND = SPACE OR                       FAAFIN  
000455    0368            M-CODE = SPACE                                FAAFIN  
000456    0369            GO TO 040-FACILITY-CODE-ERROR.                FAAFIN  
000457    0370        IF FAC-TYPE-1ST < "0" OR FAC-TYPE-1ST > "9"       FAAFIN  
000458    0371            GO TO 040-FACILITY-CODE-ERROR   ELSE          FAAFIN  
000459    0372            GO TO 050-FSN-CHECK.                          FAAFIN  
000460    0373    040-FACILITY-CODE-ERROR.                              FAAFIN  
000461    0374        MOVE "*****" TO FACTPE-ERR.                       FAAFIN  
000462    0375        MOVE "1" TO ERROR-IND.                            FAAFIN  
000463    0376    050-FSN-CHECK.                                        FAAFIN  
000464    0377        IF FSNA-IN = "FCLT"                               FAAFIN  
000465    0378            GO TO 060-EQP-CK.                             FAAFIN  
000466    0379        IF FSNA-IN = "INST"                               FAAFIN  
000467    0380            GO TO 070-CHG-CK.                             FAAFIN  
000468    0381        MOVE FSNA-IN TO FOUR-TEST.                        FAAFIN  
000469    0382        IF NUMB-TEST-4 NOT NUMERIC                        FAAFIN  
000470    0383            MOVE "****" TO FSNA-ERR                       FAAFIN  
000471    0384            MOVE "1" TO ERROR-IND.                        FAAFIN  
000472    0385        GO TO 080-CHECK-MID-3.                            FAAFIN  
000473    0386    060-EQP-CK.                                           FAAFIN  
000474    0387        IF FSNB-IN NOT = "EQP"                            FAAFIN  
000475    0388            PERFORM 090-FSN-MID-3-ERROR.                  FAAFIN  
000476    0389        GO TO 120-LAST-FOUR-SPACES.                       FAAFIN  
000477    0390    070-CHG-CK.                                           FAAFIN  
000478    0391        IF FSNB-IN NOT = "CHG"                            FAAFIN  
000479    0392            PERFORM 090-FSN-MID-3-ERROR.                  FAAFIN  
000480    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000481-    7:10		PAGE 1-7                                               FAAFIN  
000482    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000483                                                                  FAAFIN  
000484    0393        GO TO 120-LAST-FOUR-SPACES.                       FAAFIN  
000485    0394    080-CHECK-MID-3.                                      FAAFIN  
000486    0395        IF FSNB-IN = "WE0" OR "RM0" OR "NW0"              FAAFIN  
000487    0396            GO TO 100-CHECK-LAST-4FSN.                    FAAFIN  
000488    0397        MOVE FSNB-IN TO THREE-TEST.                       FAAFIN  
000489    0398        IF NUMB-TEST-3 IS NOT NUMERIC                     FAAFIN  
000490    0399            GO TO 090-FSN-MID-3-ERROR.                    FAAFIN  
000491    0400        GO TO 100-CHECK-LAST-4FSN.                        FAAFIN  
000492    0401    090-FSN-MID-3-ERROR.                                  FAAFIN  
000493    0402        MOVE "***" TO FSNB-ERR.                           FAAFIN  
000494    0403        MOVE "1" TO ERROR-IND.                            FAAFIN  
000495    0404    100-CHECK-LAST-4FSN.                                  FAAFIN  
000496    0405        IF FSND-IN < "?" OR FSND-IN > "I"                 FAAFIN  
000497    0406            GO TO 110-CHECK-ALL4-NUMERIC.                 FAAFIN  
000498    0407        MOVE "*" TO EXCESS-FLAG-RPT.                      FAAFIN  
000499    0408        NOTE ---> THIS NEXT COMPARE IS FOR A 12/0 PUNCH NOFAAFIN  
000500-    T A SPACE.                                                   FAAFIN  
000501    0409        IF FSND-IN = "?"                                  FAAFIN  
000502    0410            MOVE "0" TO FSND-RPT.                         FAAFIN  
000503    0411        IF FSND-IN = "A"                                  FAAFIN  
000504    0412            MOVE "1" TO FSND-RPT.                         FAAFIN  
000505    0413        IF FSND-IN = "C"                                  FAAFIN  
000506    0414            MOVE "3" TO FSND-RPT.                         FAAFIN  
000507    0415            MOVE "2" TO FSND-RPT.                         FAAFIN  
000508    0416        IF FSND-IN = "B"                                  FAAFIN  
000509    0417        IF FSND-IN = "D"                                  FAAFIN  
000510    0418            MOVE "4" TO FSND-RPT.                         FAAFIN  
000511    0419        IF FSND-IN = "E"                                  FAAFIN  
000512    0420            MOVE "5" TO FSND-RPT.                         FAAFIN  
000513    0421        IF FSND-IN = "F"                                  FAAFIN  
000514    0422            MOVE "6" TO FSND-RPT.                         FAAFIN  
000515    0423        IF FSND-IN = "G"                                  FAAFIN  
000516    0424            MOVE "7" TO FSND-RPT.                         FAAFIN  
000517    0425        IF FSND-IN = "H"                                  FAAFIN  
000518    0426            MOVE "8" TO FSND-RPT.                         FAAFIN  
000519    0427        IF FSND-IN = "I"                                  FAAFIN  
000520    0428            MOVE "9" TO FSND-RPT.                         FAAFIN  
000521    0429        MOVE FSNC-IN TO THREE-TEST.                       FAAFIN  
000522    0430        IF NUMB-TEST-3 IS NOT NUMERIC                     FAAFIN  
000523    0431            MOVE "***" TO FSNC-ERR                        FAAFIN  
000524    0432            MOVE "1" TO ERROR-IND.                        FAAFIN  
000525    0433        GO TO 125-CK-OWN-FIELD.                           FAAFIN  
000526    0434    110-CHECK-ALL4-NUMERIC.                               FAAFIN  
000527    0435        MOVE FSNLAST-4-IN TO FOUR-TEST.                   FAAFIN  
000528    0436        IF NUMB-TEST-4 IS NOT NUMERIC                     FAAFIN  
000529    0437            MOVE "***" TO FSNC-ERR                        FAAFIN  
000530    0438            MOVE "*" TO FSND-ERR                          FAAFIN  
000531    0439            GO TO 110-CHECK-ALL4-NUMERIC.                 FAAFIN  
000532    0440            MOVE "1" TO ERROR-IND.                        FAAFIN  
000533    0441    120-LAST-FOUR-SPACES.                                 FAAFIN  
000534    0442        IF FSNA-IN = "FCLT" OR "INST"                     FAAFIN  
000535    0443            NEXT SENTENCE  ELSE                           FAAFIN  
000536    0444            GO TO 125-CK-OWN-FIELD.                       FAAFIN  
000537    0445        IF FSNLAST-4-IN = SPACES                          FAAFIN  
000538    0446            GO TO 125-CK-OWN-FIELD.                       FAAFIN  
000539    0447        MOVE "***" TO FSNC-ERR.                           FAAFIN  
000540    0448        MOVE "*" TO FSND-ERR.                             FAAFIN  
000541    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000542-    7:10		PAGE 1-8                                               FAAFIN  
000543    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000544                                                                  FAAFIN  
000545    0449        MOVE "1" TO ERROR-IND.                            FAAFIN  
000546    0450    125-CK-OWN-FIELD.                                     FAAFIN  
000547    0451        IF OWN-IN < "1" OR                                FAAFIN  
000548    0452            OWN-IN > "9"                                  FAAFIN  
000549    0453            MOVE "*" TO OWN-ERR                           FAAFIN  
000550    0454            MOVE "1" TO ERROR-IND.                        FAAFIN  
000551    0455        MOVE 1 TO SUB.                                    FAAFIN  
000552    0456    130-ASSET-CODE-LOOKUP.                                FAAFIN  
000553    0457        MOVE AC (SUB) TO AC-HOLD.                         FAAFIN  
000554    0458        IF ASSET-CODE-IN = AC-HOLD                        FAAFIN  
000555    0459            GO TO 140-QTY-CHECK.                          FAAFIN  
000556    0460        ADD 1 TO SUB.                                     FAAFIN  
000557    0461        IF SUB > 20                                       FAAFIN  
000558    0462            MOVE "**" TO AC-ERR                           FAAFIN  
000559    0463            MOVE "1" TO ERROR-IND                         FAAFIN  
000560    0464            GO TO 140-QTY-CHECK   ELSE                    FAAFIN  
000561    0465            GO TO 130-ASSET-CODE-LOOKUP.                  FAAFIN  
000562    0466    140-QTY-CHECK.                                        FAAFIN  
000563    0467        IF FSNA-IN = "FCLT" OR "INST"                     FAAFIN  
000564    0468            MOVE SPACE TO DASH2-RPT                       FAAFIN  
000565    0469            MOVE " 0" TO QUANTITY-RPT                     FAAFIN  
000566    0470            MOVE "01" TO QUANTITY-IN                      FAAFIN  
000567    0471            MOVE ZEROS TO UNIT-VALUE-IN                   FAAFIN  
000568    0472            GO TO 145-TOTAL-VALUE-CK.                     FAAFIN  
000569    0473        IF QUANTITY-IN < "01" OR                          FAAFIN  
000570    0474            QUANTITY-IN > "99"                            FAAFIN  
000571    0475            MOVE "**" TO QUAN-ERR                         FAAFIN  
000572    0476            MOVE ZEROS TO QUANTITY-IN                     FAAFIN  
000573    0477            MOVE "1" TO ERROR-IND.                        FAAFIN  
000574    0478        IF UNIT-VALUE-X = SPACES                          FAAFIN  
000575    0479            MOVE "000000000" TO UNIT-VALUE-X.             FAAFIN  
000576    0480        EXAMINE UNIT-VALUE-X REPLACING LEADING SPACES BY ZFAAFIN  
000577-    EROS.                                                        FAAFIN  
000578    0481        MOVE UNIT-VALUE-X TO NINE-TEST.                   FAAFIN  
000579    0482        IF NUMB-TEST-9 IS NOT NUMERIC                     FAAFIN  
000580    0483            MOVE ZEROS TO UNIT-VALUE-IN                   FAAFIN  
000581    0484            MOVE "**********" TO PRICE-ERR                FAAFIN  
000582    0485            MOVE "1" TO ERROR-IND.                        FAAFIN  
000583    0486    145-TOTAL-VALUE-CK.                                   FAAFIN  
000584    0487        IF TOTAL-VALUE-X = SPACES                         FAAFIN  
000585    0488            MOVE "000000000" TO TOTAL-VALUE-X.            FAAFIN  
000586    0489        EXAMINE TOTAL-VALUE-X REPLACING LEADING SPACES BY FAAFIN  
000587-    ZEROS.                                                       FAAFIN  
000588    0490        MOVE TOTAL-VALUE-X TO NINE-TEST.                  FAAFIN  
000589    0491        IF NUMB-TEST-9 IS NOT NUMERIC                     FAAFIN  
000590    0492            MOVE ZEROS TO TOTAL-VALUE-IN                  FAAFIN  
000591    0493            MOVE "***********" TO VALUE-ERR               FAAFIN  
000592    0494            MOVE "1" TO ERROR-IND.                        FAAFIN  
000593    0495    150-ACTION-DATE-CK.                                   FAAFIN  
000594    0496        IF MONTH-IN = "O" OR "N" OR "D"                   FAAFIN  
000595    0497            GO TO 155-YEAR-CK.                            FAAFIN  
000596    0498        IF MONTH-IN < "0" OR                              FAAFIN  
000597    0499            MONTH-IN > "9"                                FAAFIN  
000598    0500            MOVE "****" TO ACT-DTE-ERR                    FAAFIN  
000599    0501            MOVE "1" TO ERROR-IND.                        FAAFIN  
000600    0502    155-YEAR-CK.                                          FAAFIN  
000601    0503        IF YEAR-IN < "00" OR                              FAAFIN  
000602    0504            YEAR-IN > "99"                                FAAFIN  
000603    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000604-    7:10		PAGE 1-9                                               FAAFIN  
000605    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000606                                                                  FAAFIN  
000607    0505            MOVE "****" TO ACT-DTE-ERR                    FAAFIN  
000608    0506            MOVE "1" TO ERROR-IND.                        FAAFIN  
000609    0507    160-DOC-NUM-CK.                                       FAAFIN  
000610    0508        MOVE DOC-NUM-IN TO FIVE-TEST.                     FAAFIN  
000611    0509        IF NUMB-TEST-5 IS NOT NUMERIC                     FAAFIN  
000612    0510            MOVE "*****" TO DOC-NO-ERR                    FAAFIN  
000613    0511            MOVE "1" TO ERROR-IND.                        FAAFIN  
000614    0512        IF BATCH-NUM-IN = SPACES                          FAAFIN  
000615    0513            MOVE "***" TO BTCH-ERR                        FAAFIN  
000616    0514            MOVE "1" TO ERROR-IND.                        FAAFIN  
000617    0515        MOVE 1 TO SUB.                                    FAAFIN  
000618    0516    165-MOVE-FIELDS-TO-COMP-WS.                           FAAFIN  
000619    0517        IF FSNA-IN = "FCLT" OR "INST"                     FAAFIN  
000620    0518            MOVE TOTAL-VALUE-IN TO TEMP-TOTAL             FAAFIN  
000621    0519            MOVE TOTAL-VALUE-IN TO TOTAL-VALUE-RPT        FAAFIN  
000622    0520            MOVE 01 TO QUAN-WS                            FAAFIN  
000623    0521            GO TO 170-CHECK-IF-INCR.                      FAAFIN  
000624    0522        MOVE UNIT-VALUE-IN TO UNIT-VALUE-WS.              FAAFIN  
000625    0523        MOVE QUANTITY-IN-9 TO QUAN-WS.                    FAAFIN  
000626    0524        MULTIPLY UNIT-VALUE-WS BY QUAN-WS GIVING TEMP-TOTAFAAFIN  
000627-    L.                                                           FAAFIN  
000628    0525        MOVE TEMP-TOTAL TO TOTAL-VALUE-RPT.               FAAFIN  
000629    0526    170-CHECK-IF-INCR.                                    FAAFIN  
000630    0527        MOVE INCR-TBL (SUB) TO TR-HOLD.                   FAAFIN  
000631    0528        IF TRANS-CODE-IN = TR-HOLD                        FAAFIN  
000632    0529            GO TO 190-INCR-ROUTINE.                       FAAFIN  
000633    0530        IF SUB = 20                                       FAAFIN  
000634    0531        MOVE 1 TO SUB                                     FAAFIN  
000635    0532            GO TO 180-CHECK-IF-DECR   ELSE                FAAFIN  
000636    0533            ADD 1 TO SUB                                  FAAFIN  
000637    0534            GO TO 170-CHECK-IF-INCR.                      FAAFIN  
000638    0535    180-CHECK-IF-DECR.                                    FAAFIN  
000639    0536        MOVE DECR-TBL (SUB) TO TR-HOLD.                   FAAFIN  
000640    0537        IF TRANS-CODE-IN = TR-HOLD                        FAAFIN  
000641    0538            GO TO 200-DECR-ROUTINE.                       FAAFIN  
000642    0539        IF SUB = 22                                       FAAFIN  
000643    0540            MOVE "**" TO TR-CD-ERR                        FAAFIN  
000644    0541            MOVE "1" TO ERROR-IND                         FAAFIN  
000645    0542            GO TO 210-PRINT-DOC  ELSE                     FAAFIN  
000646    0543            ADD 1 TO SUB                                  FAAFIN  
000647    0544            GO TO 180-CHECK-IF-DECR.                      FAAFIN  
000648    0545    190-INCR-ROUTINE.                                     FAAFIN  
000649    0546        ADD QUAN-WS TO DOC-INCR-CTR.                      FAAFIN  
000650    0547        ADD QUAN-WS TO BATCH-INCR-CTR.                    FAAFIN  
000651    0548        ADD QUAN-WS TO BATCH-CTR.                         FAAFIN  
000652    0549        ADD QUAN-WS TO REGION-CTR.                        FAAFIN  
000653    0550        ADD QUAN-WS TO OV-CTR.                            FAAFIN  
000654    0551        ADD TEMP-TOTAL TO BATCH-INCR-AMT.                 FAAFIN  
000655    0552        ADD TEMP-TOTAL TO DOC-INCR-AMT.                   FAAFIN  
000656    0553        ADD TEMP-TOTAL TO BATCH-AMT.                      FAAFIN  
000657    0554        ADD TEMP-TOTAL TO REGION-AMT.                     FAAFIN  
000658    0555        ADD TEMP-TOTAL TO OV-AMT.                         FAAFIN  
000659    0556        GO TO 210-PRINT-DOC.                              FAAFIN  
000660    0557    200-DECR-ROUTINE.                                     FAAFIN  
000661    0558        SUBTRACT QUAN-WS FROM DOC-DECR-CTR.               FAAFIN  
000662    0559        SUBTRACT QUAN-WS FROM BATCH-DECR-CTR.             FAAFIN  
000663    0560        ADD QUAN-WS TO BATCH-CTR.                         FAAFIN  
000664    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000665-    7:10		PAGE 1-10                                              FAAFIN  
000666    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000667                                                                  FAAFIN  
000668    0561        ADD QUAN-WS TO REGION-CTR.                        FAAFIN  
000669    0562        ADD QUAN-WS TO OV-CTR.                            FAAFIN  
000670    0563        SUBTRACT TEMP-TOTAL FROM DOC-DECR-AMT.            FAAFIN  
000671    0564        SUBTRACT TEMP-TOTAL FROM BATCH-DECR-AMT.          FAAFIN  
000672    0565        SUBTRACT TEMP-TOTAL FROM REGION-AMT.              FAAFIN  
000673    0566        SUBTRACT TEMP-TOTAL FROM BATCH-AMT.               FAAFIN  
000674    0567        SUBTRACT TEMP-TOTAL FROM OV-AMT.                  FAAFIN  
000675    0568    210-PRINT-DOC.                                        FAAFIN  
000676    0569        MOVE "0" TO PRT-CTRL.                             FAAFIN  
000677    0570        ADD 2 TO LINE-CTR.                                FAAFIN  
000678    0571        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000679    0572        IF ERROR-IND = "1"                                FAAFIN  
000680    0573            MOVE "**********" TO ERR-FLG                  FAAFIN  
000681    0574            ADD 1 TO LINE-CTR                             FAAFIN  
000682    0575            MOVE ERROR-LINE TO PRINT-IO                   FAAFIN  
000683    0576            MOVE SPACES TO ERROR-LINE                     FAAFIN  
000684    0577            MOVE " " TO ERROR-IND                         FAAFIN  
000685    0578            MOVE " " TO PRT-CTRL                          FAAFIN  
000686    0579            PERFORM 320-WRITE-A-LINE.                     FAAFIN  
000687    0580        PERFORM 330-CHECK-OVERFLOW THRU 340-WRITE-EXIT.   FAAFIN  
000688    0581        GO TO 010-READ-INPUT.                             FAAFIN  
000689    0582    220-END-DOC-RTN.                                      FAAFIN  
000690    0583        MOVE DOCMNT TO T-1.                               FAAFIN  
000691    0584        MOVE TOTL TO T-3.                                 FAAFIN  
000692    0585        MOVE DECR TO T-4.                                 FAAFIN  
000693    0586        MOVE INCR TO T-5.                                 FAAFIN  
000694    0587        MOVE DOC-DECR-CTR TO DECR-CNT-RPT.                FAAFIN  
000695    0588        MOVE DOC-DECR-AMT TO DECR-AMT-RPT.                FAAFIN  
000696    0589        MOVE DOC-INCR-CTR TO INCR-CNT-RPT.                FAAFIN  
000697    0590        MOVE DOC-INCR-AMT TO INCR-AMT-RPT.                FAAFIN  
000698    0591        MOVE ZEROS TO DOC-DECR-CTR DOC-DECR-AMT           FAAFIN  
000699    0592                      DOC-INCR-CTR DOC-INCR-AMT.          FAAFIN  
000700    0593        MOVE "-" TO PRT-CTRL.                             FAAFIN  
000701    0594        ADD 3 TO LINE-CTR.                                FAAFIN  
000702    0595        PERFORM 320-WRITE-A-LINE THRU 340-WRITE-EXIT.     FAAFIN  
000703    0596        IF END-JOB-IND = "1"                              FAAFIN  
000704    0597            MOVE " " TO PRT-CTRL                          FAAFIN  
000705    0598            PERFORM 320-WRITE-A-LINE.                     FAAFIN  
000706    0599        MOVE DOC-NUM-IN TO WS-DOC-NUM.                    FAAFIN  
000707    0600    230-END-DOC-EXIT.                                     FAAFIN  
000708    0601        EXIT.                                             FAAFIN  
000709    0602    240-END-BATCH-RTN.                                    FAAFIN  
000710    0603        PERFORM 220-END-DOC-RTN THRU 230-END-DOC-EXIT.    FAAFIN  
000711    0604        MOVE BATCH TO T-1.                                FAAFIN  
000712    0605        MOVE TOTL TO T-3.                                 FAAFIN  
000713    0606        MOVE DECR TO T-4.                                 FAAFIN  
000714    0607        MOVE INCR TO T-5.                                 FAAFIN  
000715    0608        MOVE BATCH-DECR-CTR TO DECR-CNT-RPT.              FAAFIN  
000716    0609        MOVE BATCH-DECR-AMT TO DECR-AMT-RPT.              FAAFIN  
000717    0610        MOVE BATCH-INCR-CTR TO INCR-CNT-RPT.              FAAFIN  
000718    0611        MOVE BATCH-INCR-AMT TO INCR-AMT-RPT.              FAAFIN  
000719    0612        MOVE "0" TO PRT-CTRL.                             FAAFIN  
000720    0613        ADD 2 TO LINE-CTR.                                FAAFIN  
000721    0614        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000722    0615        MOVE BATCH TO T-1.                                FAAFIN  
000723    0616        MOVE TVAL TO T-3.                                 FAAFIN  
000724    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000725-    7:10		PAGE 1-11                                              FAAFIN  
000726    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000727                                                                  FAAFIN  
000728    0617        MOVE BATCH-AMT TO INCR-AMT-RPT.                   FAAFIN  
000729    0618        ADD 2 TO LINE-CTR.                                FAAFIN  
000730    0619        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000731    0620        MOVE BATCH TO T-1.                                FAAFIN  
000732    0621        MOVE ITEM TO T-2.                                 FAAFIN  
000733    0622        MOVE TCNT TO T-3.                                 FAAFIN  
000734    0623        MOVE BATCH-CTR TO INCR-CNT-RPT.                   FAAFIN  
000735    0624        ADD 2 TO LINE-CTR.                                FAAFIN  
000736    0625        PERFORM 320-WRITE-A-LINE THRU 340-WRITE-EXIT.     FAAFIN  
000737    0626        MOVE ZEROS TO BATCH-DECR-CTR, BATCH-DECR-AMT, BATCFAAFIN  
000738-    H-INCR-AMT                                                   FAAFIN  
000739    0627               BATCH-INCR-CTR, BATCH-AMT, BATCH-CTR.      FAAFIN  
000740    0628        MOVE BATCH-NUM-IN TO WS-BATCH-HOLD.               FAAFIN  
000741    0629    250-BATCH-RTN-EXIT.                                   FAAFIN  
000742    0630        EXIT.                                             FAAFIN  
000743    0631    260-END-REGION-RTN.                                   FAAFIN  
000744    0632        PERFORM 240-END-BATCH-RTN THRU 250-BATCH-RTN-EXIT.FAAFIN  
000745    0633        MOVE TNET TO T-1.                                 FAAFIN  
000746    0634        MOVE TVAL TO T-3.                                 FAAFIN  
000747    0635        MOVE REGION-AMT TO INCR-AMT-RPT.                  FAAFIN  
000748    0636        MOVE "0" TO PRT-CTRL.                             FAAFIN  
000749    0637        ADD 2 TO LINE-CTR.                                FAAFIN  
000750    0638        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000751    0639        MOVE TOTL TO T-1.                                 FAAFIN  
000752    0640        MOVE ITEM TO T-2.                                 FAAFIN  
000753    0641        MOVE TCNT TO T-3.                                 FAAFIN  
000754    0642        MOVE REGION-CTR TO INCR-CNT-RPT.                  FAAFIN  
000755    0643        ADD 2 TO LINE-CTR.                                FAAFIN  
000756    0644        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000757    0645        MOVE ZEROS TO REGION-AMT, REGION-CTR, LINE-CTR, PAFAAFIN  
000758-    GE-CTR.                                                      FAAFIN  
000759    0646        IF END-JOB-IND = "1"                              FAAFIN  
000760    0647            NEXT SENTENCE  ELSE                           FAAFIN  
000761    0648            MOVE ZEROS TO PAGE-CTR                        FAAFIN  
000762    0649            PERFORM 280-FIND-REGION-HDR THRU 290-FIND-REG-FAAFIN  
000763-    EXIT.                                                        FAAFIN  
000764    0650        PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXIT. FAAFIN  
000765    0651        MOVE REGION-IN TO WS-REGION-HOLD.                 FAAFIN  
000766    0652    270-END-REGION-EXIT.                                  FAAFIN  
000767    0653        EXIT.                                             FAAFIN  
000768    0654    280-FIND-REGION-HDR.                                  FAAFIN  
000769    0655        MOVE 1 TO SUB.                                    FAAFIN  
000770    0656    285-START-HDR-LOOKUP.                                 FAAFIN  
000771    0657        MOVE REG-TBL (SUB) TO REG-BRKDWN.                 FAAFIN  
000772    0658        IF REGION-IN = REG-CODE-TBL                       FAAFIN  
000773    0659            MOVE REGION-IN TO WS-REGION-HOLD              FAAFIN  
000774    0660            MOVE REG-NAME-TBL TO REGION-HDR               FAAFIN  
000775    0661            GO TO 290-FIND-REG-EXIT.                      FAAFIN  
000776    0662        IF SUB > 10                                       FAAFIN  
000777    0663            MOVE "*" TO REG-ERR                           FAAFIN  
000778    0664            MOVE "1" TO ERROR-IND                         FAAFIN  
000779    0665            GO TO 290-FIND-REG-EXIT   ELSE                FAAFIN  
000780    0666            ADD 1 TO SUB                                  FAAFIN  
000781    0667            GO TO 285-START-HDR-LOOKUP.                   FAAFIN  
000782    0668    290-FIND-REG-EXIT.                                    FAAFIN  
000783    0669        EXIT.                                             FAAFIN  
000784    0670    300-HEADER-ROUTINE.                                   FAAFIN  
000785    0671        ADD 1 TO PAGE-CTR.                                FAAFIN  
000786    0672        MOVE PAGE-CTR TO PAGE-HDR.                        FAAFIN  
000787    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000788-    7:10		PAGE 1-12                                              FAAFIN  
000789    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000790                                                                  FAAFIN  
000791    0673        MOVE "1" TO PRT-CTRL.                             FAAFIN  
000792    0674        MOVE HEAD-1 TO PRINT-IO.                          FAAFIN  
000793    0675        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000794    0676        MOVE "0" TO PRT-CTRL.                             FAAFIN  
000795    0677        MOVE HEAD-2 TO PRINT-IO.                          FAAFIN  
000796    0678        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000797    0679        MOVE " " TO PRT-CTRL.                             FAAFIN  
000798    0680        MOVE HEAD-3 TO PRINT-IO.                          FAAFIN  
000799    0681        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000800    0682    310-HDR-RTN-EXIT.                                     FAAFIN  
000801    0683        EXIT.                                             FAAFIN  
000802    0684    320-WRITE-A-LINE.                                     FAAFIN  
000803    0685        WRITE PRINT-LINE AFTER ADVANCING 2 LINES.         FAAFIN  
000804    0686        MOVE SPACES TO PRINT-IO.                          FAAFIN  
000805    0687    330-CHECK-OVERFLOW.                                   FAAFIN  
000806    0688        IF LINE-CTR > 46                                  FAAFIN  
000807    0689            MOVE ZEROS TO LINE-CTR                        FAAFIN  
000808    0690            PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXFAAFIN  
000809-    IT.                                                          FAAFIN  
000810    0691    340-WRITE-EXIT.                                       FAAFIN  
000811    0692        EXIT.                                             FAAFIN  
000812    0693    350-INPUT-AT-END.                                     FAAFIN  
000813    0694        PERFORM 260-END-REGION-RTN THRU 270-END-REGION-EXIFAAFIN  
000814-    T.                                                           FAAFIN  
000815    0695        MOVE "OVERALL " TO T-1.                           FAAFIN  
000816    0696        MOVE TOTL TO T-3.                                 FAAFIN  
000817    0697        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000818    0698        MOVE TNET TO T-1.                                 FAAFIN  
000819    0699        MOVE TVAL TO T-3.                                 FAAFIN  
000820    0700        MOVE OV-AMT TO INCR-AMT-RPT.                      FAAFIN  
000821    0701        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000822    0702        MOVE TOTL TO T-1.                                 FAAFIN  
000823    0703        MOVE ITEM TO T-2.                                 FAAFIN  
000824    0704        MOVE TCNT TO T-3.                                 FAAFIN  
000825    0705        MOVE OV-CTR TO INCR-CNT-RPT.                      FAAFIN  
000826    0706        PERFORM 320-WRITE-A-LINE.                         FAAFIN  
000827    0707        CLOSE TRANS, PRINT.                               FAAFIN  
000828    0708        DISPLAY "EXCELLENT PROGRAMMING JOB" UPON CONSOLE. FAAFIN  
000829    0709        DISPLAY "END BB01A" UPON CONSOLE.                 FAAFIN  
000830    0710        STOP RUN.                                         FAAFIN  
000831    0711                                                          FAAFIN  
000832    0712                                                          FAAFIN  
000833    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  1FAAFIN  
000834-    7:10		PAGE W-1                                               FAAFIN  
000835    FAA1.CBL    04-JUN-73  14:36                                  FAAFIN  
000836                                                                  FAAFIN  
000837 WARNINGS:                                                        FAAFIN  
000838                                                                  FAAFIN  
000839    0204  REDEFINITION IS NOT THE SAME SIZE AS THE REDEFINED ITEM FAAFIN  
000840    0245  REDEFINITION IS NOT THE SAME SIZE AS THE REDEFINED ITEM FAAFIN  
000841    0524  MOST SIGNIFICANT DIGITS TRUNCATED ON TEMP-TOTAL         FAAFIN  
000842    0575  RIGHT-MOST TRUNCATION ON PRINT-IO                       FAAFIN  
000843    0672  MOST SIGNIFICANT DIGITS TRUNCATED ON PAGE-HDR           FAAFIN  
000844                                                                  FAAFIN  
000845                                                                  FAAFIN  
000846 NO FATAL ERRORS, 5 WARNINGS                                      FAAFIN  
000847 a@tù