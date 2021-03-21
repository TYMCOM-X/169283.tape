000010 IDENTIFICATION DIVISION.                                         ARBL0000
000020 PROGRAM-ID.                                                      ARBL0001
000030     A226.                                                        MO A0002
000040 AUTHOR.                                                          ARBL0003
000050     MIKE ORBAN.                                                  ARBL0004
000060 DATE-WRITTEN.                                                    ARBL0005
000070     MARCH 15, 1972.                                              ARBL0006
000075 DATE-COMPILED.  11/16/72.                                            0007
000080 REMARKS.                                                         ARBL0008
000090     THIS PROGRAM WILL RECALCULATE ALL CHARGES FOR PAID TCHR      ARBL0009
000100     L.C. AND SECONDARY MAGS ONLY.  TERM 4 ACCOUNTS WITH ABOVE    ARBL0010
000110     CRITERIA WILL BE DISPLAYED BUT NOT RECALCULATED.             ARBL0011
000120 ENVIRONMENT DIVISION.                                            ARBL0012
000130 CONFIGURATION SECTION.                                           ARBL0013
000140 SOURCE-COMPUTER.                                                 ARBL0014
000150     IBM-370-145.                                                 MO A0015
000160 OBJECT-COMPUTER.                                                 ARBL0016
000170     IBM-370-145.                                                 MO A0017
000180 INPUT-OUTPUT SECTION.                                            ARBL0018
000190 FILE-CONTROL.                                                    ARBL0019
000200     SELECT INPUT-MASTER-FILE ASSIGN TO SYS010-UT-2400-S-IPMAST.  MO A0020
000210     SELECT OUTPUT-MASTER-FILE ASSIGN TO SYS012-UT-2400-S-OPMAST. MO A0021
000220 I-O-CONTROL.                                                     ARBL0022
000230     APPLY WRITE-ONLY ON OUTPUT-MASTER-FILE.                      ARBL0023
000240 DATA DIVISION.                                                   ARBL0024
000250 FILE SECTION.                                                    ARBL0025
000260 FD  INPUT-MASTER-FILE                                            ARBL0026
001531     RECORD CONTAINS 109 TO 164 CHARACTERS                            0027
000270     BLOCK CONTAINS  1360  CHARACTERS                                 0028
           RECORDING MODE IS V                                              0029
000280     LABEL RECORDS ARE STANDARD                                   ARBL0030
000290     DATA RECORDS ARE  INPUT-TEACHER-MASTER, INPUT-SCHOOL-MASTER. ARBL0031
000300 01  INPUT-TEACHER-MASTER        SYNC.                            ARBL0032
000310     02  ID-NUMBER.                                               ARBL0033
000320         03  ZIP-CODE            PICTURE  9[5].                   ARBL0034
000330         03  CODE-ZIP REDEFINES ZIP-CODE.                         ARBL0035
000340             04  ZIP-HIGH        PICTURE X.                       ARBL0036
000350             04  FILLER          PICTURE X[4].                    ARBL0037
000360         03  SCH-NUMBER          PICTURE  999.                    ARBL0038
000370         03  EDITION             PICTURE  99.                     ARBL0039
000380         03  TCH-NUMBER          PICTURE  999.                    ARBL0040
000390         03  CHECK-DIGIT         PICTURE     9.                   ARBL0041
000400     02  REST-OF-MASTERI.                                         ARBL0042
000410         03  CURRENT-COPYI.                                       ARBL0043
000420             04  STUD-COPYI      PICTURE    9[4].                 ARBL0044
000430             04  XSTUDCOPYI REDEFINES STUD-COPYI  PICTURE X[4].   ARBL0045
000440             04  PAID-TCHI       PICTURE    99.                   ARBL0046
000450             04  XPAIDTCHI REDEFINES PAID-TCHI  PICTURE XX.       ARBL0047
000460             04  RECSI           PICTURE    99.                   ARBL0048
000470             04  FILMI           PICTURE    99.                   ARBL0049
000480             04  XTCI            PICTURE    99.                   ARBL0050
000490         03  AMOUNT-PAIDI  PICTURE S9[4]V99 COMP-3 SYNC.          MO A0051
000500          03 TERM-IN             PICTURE    9.                    ARBL0052
000510             88 ITERM1   VALUE 1.                                 ARBL0053
000520             88 ITERM2   VALUE 2.                                 ARBL0054
000530             88 ITERM3   VALUE 3.                                 ARBL0055
000540             88 ITERM4   VALUE 4.                                 ARBL0056
000550             88 ITERM5   VALUE 5.                                 ARBL0057
000560             88 ITERM6   VALUE 6.                                 ARBL0058
000570             88 ITERM7   VALUE 7.                                 ARBL0059
000580             88 ITERM8   VALUE 8.                                 ARBL0060
000590         03  TERMX REDEFINES TERM-IN    PICTURE X.                ARBL0061
000600         03  GRADE-API           PICTURE  XX.                     ARBL0062
000610         03  TEACHER-NAMEI       PICTURE  X[25].                  ARBL0063
000620         03  PROMO-KEYI          PICTURE  X[5].                   ARBL0064
000630         03  BILL-TOI            PICTURE    9.                    ARBL0065
000640             88 IBILL1   VALUE 1.                                 ARBL0066
000650             88 IBILL2   VALUE 2.                                 ARBL0067
000660             88 IBILL3   VALUE 3.                                 ARBL0068
000670             88 IBILL4   VALUE 4.                                 ARBL0069
000680             88 IBILL5   VALUE 5.                                 ARBL0070
000690             88 IBILL6   VALUE 6.                                 ARBL0071
000700         03  BOARD-NUMBERI       PICTURE  X[5].                   ARBL0072
000710         03  ISSUE-DATESI.                                        ARBL0073
000720             04  BEGINI.                                          ARBL0074
000730                 05  ISSI        PICTURE    99.                   ARBL0075
000740                 05  YEARI       PICTURE    9.                    ARBL0076
000750             04  EXPIREI.                                         ARBL0077
000760                 05  ISSI        PICTURE    99.                   ARBL0078
000770                 05  YEARI       PICTURE   9.                     ARBL0079
000780     03  SPEC-ISS-CHECK REDEFINES  ISSUE-DATESI.                  ARBL0080
000790         04  BGNI.                                                ARBL0081
000800                 05  II        PICTURE  XX.                       ARBL0082
000810                 05  YYI            PICTURE  X.                   ARBL0083
000820             04  EXPI.                                            ARBL0084
000830                 05  II            PICTURE  XX.                   ARBL0085
000840                 05  YYI       PICTURE  X.                        ARBL0086
000850         03  CHARGESI        COMP-3 SYNC.                             0087
000860             04  FIRST-YR-TERMI  PICTURE  S9[4]V99.               ARBL0088
000870             04  SECOND-SEMI     PICTURE  S9[4]V99.               ARBL0089
000880             04  POSTALI         PICTURE  S9[3]V99.               ARBL0090
000890             04  OTHERI          PICTURE  S9[3]V99.               ARBL0091
000900             04  BALANCE-DUEI    PICTURE  S9[4]V99.               ARBL0092
000910         03  REVISION-ONEI.                                       ARBL0093
000920             04  REV1-STUDI      PICTURE     9[4].                ARBL0094
000930             04  REV1-TCHI       PICTURE       99.                ARBL0095
000940             04  REV1-RECI       PICTURE       99.                ARBL0096
000950             04  REV1-FILMI      PICTURE       99.                ARBL0097
000960             04  REV1-ISSUEI.                                     ARBL0098
000970                 05  REV1-ISSI   PICTURE    99.                   ARBL0099
000980                 05  REV1-YEARI  PICTURE        9.                ARBL0100
000990         03  REVISION-TWOI.                                       ARBL0101
001000             04  REV2-STUDI      PICTURE     9[4].                ARBL0102
001010             04  REV2-TCHI       PICTURE       99.                ARBL0103
001020             04  REV2-RECI       PICTURE       99.                ARBL0104
001030             04  REV2-FILMI      PICTURE       99.                ARBL0105
001040             04  REV2-ISSUEI.                                     ARBL0106
001050                 05  REV2-ISSI   PICTURE    99.                   ARBL0107
001060                 05  REV2-YEARI  PICTURE        9.                ARBL0108
001070         03  REVISION-THREEI.                                     ARBL0109
001080             04  REV3-STUDI      PICTURE     9[4].                ARBL0110
001090             04  REV3-TCHI       PICTURE       99.                ARBL0111
001100             04  REV3-RECI       PICTURE       99.                ARBL0112
001110             04  REV3-FILMI      PICTURE       99.                ARBL0113
001120             04  REV3-ISSUEI.                                     ARBL0114
001130                 05  REV3-ISSI   PICTURE    99.                   ARBL0115
001140                 05  REV3-YEARI  PICTURE        9.                ARBL0116
001150         03  TYPE-MASTERI        PICTURE A.                       ARBL0117
001160         03  MASTER-HOLD-CODEI   PICTURE X.                       ARBL0118
001170         03  LAST-SEMESTERI.                                      ARBL0119
001180             04  LAST-STUDI      PICTURE   9[4].                  ARBL0120
001190             04  LAST-TCHI       PICTURE    99.                   ARBL0121
001200             04  LAST-XTCI       PICTURE    99.                   ARBL0122
001210             04  LAST-RECI       PICTURE    99.                   ARBL0123
001220         03  TRANS-INFOI.                                         ARBL0124
001230             04  MTRANS-CODEI    PICTURE    X.                    ARBL0125
001240             04  MBATCH-NUMBERI  PICTURE   X[6].                  ARBL0126
001250             04  MMAIL-NUMBERI   PICTURE    XX.                   ARBL0127
001260         03  FILLER              PICTURE   X[11].                 ARBL0128
001270 01  INPUT-SCHOOL-MASTER    SYNC.                                 ARBL0129
001280     02  ID-NUMBER.                                               ARBL0130
001290         03  ZIP-CODE            PICTURE  9[5].                   ARBL0131
001300         03  SCH-NUMBER          PICTURE  999.                    ARBL0132
001310         03  BLANKS              PICTURE  X[5].                   ARBL0133
001320             88  SCH-RECORD      VALUE SPACES.                    ARBL0134
001330         03  CHECK-DIGIT         PICTURE        9.                ARBL0135
001340     02  REST-OF-MASTERIS.                                        ARBL0136
001350         03  SCHOOL-NAMEI        PICTURE      X[20].              ARBL0137
001360         03  STREET-ADDRESSI     PICTURE      X[20].              ARBL0138
001370         03  CITY-NAMEI          PICTURE      X[20].              ARBL0139
001380         03  STATE-ALPHAI        PICTURE       XX.                ARBL0140
001390         03  STATE-CODEI         PICTURE       XX.                ARBL0141
001400         03  SR-CODEI            PICTURE       XX.                ARBL0142
001410         03  COUNTYI             PICTURE      XXX.                ARBL0143
001420         03  PP-ZONEI            PICTURE        X.                ARBL0144
001430         03  NEW-IDI             PICTURE      X[8].               ARBL0145
001440         03  HOLDI                         PICTURE  X.            ARBL0146
001450         03  SEXI                     PICTURE  X.                 ARBL0147
001460         03  TYPESCHI                     PICTURE  XX.            ARBL0148
001470         03  SCHSIZEI                        PICTURE  X[4].       ARBL0149
001480         03  STRANS-INFOI.                                        ARBL0150
001490             04  STRANS-CODEI    PICTURE       X.                 ARBL0151
001500             04  SBATCH-NUMBERI  PICTURE      X[6].               ARBL0152
001510             04  SMAIL-NUMBERI   PICTURE       XX.                ARBL0153
001520 FD  OUTPUT-MASTER-FILE                                           ARBL0154
001540     LABEL RECORDS ARE STANDARD                                   ARBL0155
001550     DATA RECORDS ARE OUTPUT-TECHER-MASTER, OUTPUT-SCHOOL-MASTER  ARBL0156
001531     RECORD CONTAINS 109 TO 164 CHARACTERS                            0157
000270     BLOCK CONTAINS  1360  CHARACTERS                                 0158
001560     RECORDING MODE IS V.                                             0159
001570 01  OUTPUT-TEACHER-MASTER       PICTURE X[164]      SYNC.        ARBL0160
001580 01  OUTPUT-SCHOOL-MASTER        PICTURE X[109]      SYNC.        ARBL0161
001590 WORKING-STORAGE SECTION.                                         ARBL0162
001600 77  ERR-INC       PICTURE S99  COMP  SYNC  VALUE ZEROS.          MO A0163
001610 77  LINE-COUNT    PICTURE S99  COMP  SYNC       VALUE ZEROS.     MO A0164
001620 77  RECORDS-IN    PICTURE S9[11]  COMP-3  SYNC  VALUE ZEROS.     MO A0165
001630 77  RECORDS-OUT   PICTURE S9[11]  COMP-3 SYNC  VALUE ZEROS.      MO A0166
001640 77  PAGE-CTR      PIC S9[3]  COMP-3 SYNC  VALUE ZEROS.           MO A0167
001650 77  SCHOOL-CTR  PIC S9[11]  COMP-3 SYNC  VALUE ZEROS.            MO A0168
001660 77  TEACHER-CTR PIC S9[11]  COMP-3 SYNC  VALUE ZEROS.            MO A0169
001670 77  FIRST-RECORD                PICTURE X       VALUE :Y:.       ARBL0170
001680 77  TOTAL-ADJUSTED-RECORDS  PIC S9[11]  COMP-3 SYNC  VALUE ZEROS.    0171
001700 77  SINGLES-COPY      COMP-3    PICTURE S9[8]   VALUE ZEROS.     ARBL0172
001710 77  EDITION-INDEX     COMP      PICTURE S99     VALUE ZEROS.     ARBL0173
001720 77  RATE-INDEX        COMP      PICTURE S99     VALUE ZEROS.     ARBL0174
001730 77  TERM4-REC-CTR  PIC S9[12]  COMP-3 SYNC VALUE ZEROS.          MO A0175
001750 77  TCH-CHARGE  PIC S9[4]V99  COMP-3 SYNC  VALUE ZEROS.          MO A0176
001760 01  RATE-TABLE-A    COMP-3  SYNC.                                MO A0177
001770     02  ED08.                                                    ARBL0178
001780         03  START1          PICTURE  999    VALUE   013.         ARBL0179
001790         03  STOP1           PICTURE  999    VALUE   043.         ARBL0180
001800         03  START2          PICTURE  999    VALUE   053.         ARBL0181
001810         03  STOP2           PICTURE  999    VALUE   083.         ARBL0182
001820         03  SSEM            PICTURE  9V99   VALUE   0.00.        ARBL0183
001830         03  SYEARLY         PICTURE 99V99   VALUE   1.00.        ARBL0184
001840         03  STERM       PICTURE 9V999   VALUE  .125.             ARBL0185
001850         03  TSEM            PICTURE  9V99   VALUE   0.00.        ARBL0186
001860         03, TYEARLY         PICTURE  9V99   VALUE   0.00.        ARBL0187
001870         03  TTERM           PICTURE  9V99   VALUE   0.00.        ARBL0188
001880         03  MIN             PICTURE   99    VALUE    00.         ARBL0189
001890         03  MAX             PICTURE   99    VALUE    00.         ARBL0190
001900         03  SINGLE1-4       PICTURE  99V99  VALUE  00.00.        ARBL0191
001910         03  SINGLE5-9       PICTURE 99V99   VALUE  00.00.        ARBL0192
001920         03  YEAR2           PICTURE  9V99   VALUE   0.00.        ARBL0193
001930         03  YEAR3           PICTURE 99V99   VALUE   00.00.       ARBL0194
001940         03  SSTERM          PICTURE 9V999   VALUE   .000.        ARBL0195
001950         03  STTERM          PICTURE  V999   VALUE   .000.        ARBL0196
001960         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0197
001970     02  ED10T017-28-29.                                          ARBL0198
001980         03  START1          PICTURE  999    VALUE   013.         ARBL0199
001990         03  STOP1           PICTURE  999    VALUE   143.         ARBL0200
002000         03  START2          PICTURE  999    VALUE   153.         ARBL0201
002010         03  STOP2           PICTURE  999    VALUE   283.         ARBL0202
002020         03  SSEM            PICTURE 9V99    VALUE 0.40.          ARBL0203
002030         03  SYEARLY         PICTURE 99V99   VALUE 0.70.          ARBL0204
002040         03  STERM           PICTURE 9V999   VALUE   .025.        ARBL0205
002050         03  TSEM            PICTURE  9V99   VALUE   2.50.        ARBL0206
002060         03, TYEARLY         PICTURE  9V99   VALUE   5.00.        ARBL0207
002070         03  TTERM           PICTURE  V999   VALUE   .178.        ARBL0208
002080         03  MIN             PICTURE   99    VALUE   24.          ARBL0209
002090         03  MAX             PICTURE   99    VALUE   28.          ARBL0210
002100         03 SINGLE1-4        PICTURE 99V99   VALUE 2.00.          ARBL0211
002110         03 SINGLE5-9        PICTURE 99V99   VALUE 2.00.          ARBL0212
002120         03  YEAR2           PICTURE  9V99   VALUE   8.50.        ARBL0213
002130         03  YEAR3           PICTURE 99V99   VALUE   11.50.       ARBL0214
002140         03  SSTERM          PICTURE 9V999   VALUE .071.          ARBL0215
002150         03  STTERM          PICTURE  V999   VALUE   .178.        ARBL0216
002160         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0217
002170     02  ED18-19.                                                 ARBL0218
002180         03  START1          PICTURE  999    VALUE   013.         ARBL0219
002190         03  STOP1           PICTURE  999    VALUE   143.         ARBL0220
002200         03  START2          PICTURE  999    VALUE   153.         ARBL0221
002210         03  STOP2           PICTURE  999    VALUE   283.         ARBL0222
002220         03  SSEM            PICTURE  9V99   VALUE   0.60.        ARBL0223
00        03  SYEARLY         PICTURE 99V99   VALUE   1.00.        ARBL0224
002240         03  STERM           PICTURE 9V999   VALUE .035.          ARBL0225
002250         03  TSEM            PICTURE  9V99   VALUE   2.50.        ARBL0226
002260         03, TYEARLY         PICTURE  9V99   VALUE   5.00.        ARBL0227
002270         03  TTERM           PICTURE  V999   VALUE   .178.        ARBL0228
002280         03  MIN             PICTURE   99    VALUE   24.          ARBL0229
002290         03  MAX             PICTURE   99    VALUE   28.          ARBL0230
002300         03 SINGLE1-4        PICTURE 99V99   VALUE 2.00.          ARBL0231
002310         03 SINGLE5-9        PICTURE 99V99   VALUE 2.00.          ARBL0232
002320         03  YEAR2           PICTURE  9V99   VALUE   8.50.        ARBL0233
002330         03  YEAR3           PICTURE 99V99   VALUE   11.50.       ARBL0234
002340         03  SSTERM          PICTURE 9V999   VALUE .071.          ARBL0235
002350         03  STTERM          PICTURE  V999   VALUE   .178.        ARBL0236
002360         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0237
002370     02  ED20-21.                                                 ARBL0238
002380         03  START1          PICTURE  999    VALUE   013.         ARBL0239
002390         03  STOP1           PICTURE  999    VALUE   143.         ARBL0240
002400         03  START2          PICTURE  999    VALUE   153.         ARBL0241
002410         03  STOP2           PICTURE  999    VALUE   283.         ARBL0242
002420         03  SSEM            PICTURE 9V99    VALUE 0.80.          ARBL0243
002430         03  SYEARLY         PICTURE 99V99   VALUE 1.35.          ARBL0244
002440         03  STERM           PICTURE 9V999   VALUE .048.          ARBL0245
002450         03  TSEM            PICTURE  9V99   VALUE   2.50.        ARBL0246
002460         03, TYEARLY         PICTURE  9V99   VALUE   5.00.        ARBL0247
002470         03  TTERM           PICTURE  V999   VALUE   .178.        ARBL0248
002480         03  MIN             PICTURE   99    VALUE   24.          ARBL0249
002490         03  MAX             PICTURE   99    VALUE   28.          ARBL0250
002500         03 SINGLE1-4        PICTURE 99V99   VALUE 2.20.          ARBL0251
002510         03 SINGLE5-9        PICTURE 99V99   VALUE 2.20.          ARBL0252
002520         03  YEAR2           PICTURE  9V99   VALUE   8.50.        ARBL0253
002530         03  YEAR3           PICTURE 99V99   VALUE   11.50.       ARBL0254
002540         03  SSTERM          PICTURE 9V999   VALUE .078.          ARBL0255
002550         03  STTERM          PICTURE  V999   VALUE   .178.        ARBL0256
002560         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0257
002570     02  ED30-31.                                                 ARBL0258
002580         03  START1          PICTURE  999    VALUE   013.         ARBL0259
002590         03  STOP1           PICTURE  999    VALUE   043.         ARBL0260
002600         03  START2          PICTURE  999    VALUE   053.         ARBL0261
002610         03  STOP2           PICTURE  999    VALUE   083.         ARBL0262
002620         03  SSEM            PICTURE  9V99   VALUE   0.85.        ARBL0263
002630         03  SYEARLY         PICTURE 99V99   VALUE   1.50.        ARBL0264
002640         03  STERM           PICTURE 9V999   VALUE .187.          ARBL0265
002650         03  TSEM            PICTURE 9V99    VALUE 3.00.          ARBL0266
002660         03  TYEARLY         PICTURE 9V99    VALUE 6.00.          ARBL0267
002670         03  TTERM           PICTURE V999    VALUE .750.          ARBL0268
002680         03  MIN             PICTURE   99    VALUE   07.          ARBL0269
002690         03  MAX             PICTURE   99    VALUE   08.          ARBL0270
002700         03 SINGLE1-4        PICTURE 99V99   VALUE 2.60.          ARBL0271
002710         03 SINGLE5-9        PICTURE 99V99   VALUE 2.60.          ARBL0272
002720         03  YEAR2           PICTURE  9V99   VALUE   8.50.        ARBL0273
002730         03  YEAR3           PICTURE 99V99   VALUE   11.50.       ARBL0274
002740         03  SSTERM          PICTURE 9V999   VALUE .325.          ARBL0275
002750         03  STTERM          PICTURE V999    VALUE .750.          ARBL0276
002760         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0277
002770     02  ED32.                                                    ARBL0278
002780         03  START1          PICTURE  999    VALUE   013.         ARBL0279
002790         03  STOP1           PICTURE  999    VALUE   053.         ARBL0280
002800         03  START2          PICTURE  999    VALUE   053.         ARBL0281
002810         03  STOP2           PICTURE  999    VALUE   103.         ARBL0282
002820         03  SSEM            PICTURE  9V99   VALUE   1.00.        ARBL0283
002830         03  SYEARLY         PICTURE 99V99   VALUE   1.50.        ARBL0284
002840         03  STERM           PICTURE 9V999   VALUE .150.          ARBL0285
002850         03  TSEM            PICTURE  9V99   VALUE   3.00.        ARBL0286
002860         03, TYEARLY         PICTURE  9V99   VALUE   6.00.        ARBL0287
002870         03  TTERM           PICTURE V999    VALUE .666.          ARBL0288
002880         03  MIN             PICTURE   99    VALUE   08.          ARBL0289
002890         03  MAX             PICTURE   99    VALUE   09.          ARBL0290
002900         03 SINGLE1-4        PICTURE 99V99   VALUE 2.40.          ARBL0291
002910         03 SINGLE5-9        PICTURE 99V99   VALUE 2.40.          ARBL0292
002920         03  YEAR2           PICTURE  9V99   VALUE   9.50.        ARBL0293
002930         03  YEAR3           PICTURE 99V99   VALUE   12.50.       ARBL0294
002940         03  SSTERM          PICTURE 9V999   VALUE   .240.        ARBL0295
002950         03  STTERM          PICTURE V999    VALUE .666.          ARBL0296
002960         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0297
002970     02  ED40-41.                                                 ARBL0298
002980         03  START1          PICTURE  999    VALUE   013.         ARBL0299
002990         03  STOP1           PICTURE  999    VALUE   143.         ARBL0300
003000         03  START2          PICTURE  999    VALUE   153.         ARBL0301
003010         03  STOP2           PICTURE  999    VALUE   283.         ARBL0302
003020         03  SSEM            PICTURE 9V99    VALUE 1.10.          ARBL0303
003030         03  SYEARLY         PICTURE 99V99   VALUE 1.85.          ARBL0304
003040         03  STERM           PICTURE 9V999   VALUE .066.          ARBL0305
003050         03  TSEM            PICTURE 9V99    VALUE 3.00.          ARBL0306
003060         03  TYEARLY         PICTURE 9V99    VALUE 6.00.          ARBL0307
003070         03  TTERM           PICTURE V999    VALUE .214.          ARBL0308
003080         03  MIN                 PICTURE 99   VALUE 25.           ARBL0309
003090         03  MAX             PICTURE   99    VALUE   28.          ARBL0310
003100         03 SINGLE1-4        PICTURE 99V99   VALUE 2.60.          ARBL0311
003110         03 SINGLE5-9        PICTURE 99V99  VALUE 2.60.           ARBL0312
003120         03  YEAR2           PICTURE  9V99   VALUE   9.50.        ARBL0313
003130         03  YEAR3           PICTURE 99V99   VALUE   12.50.       ARBL0314
003140         03  SSTERM          PICTURE 9V999   VALUE .092.          ARBL0315
003150         03  STTERM          PICTURE V999    VALUE .214.          ARBL0316
003160         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0317
003170     02  ED42-43.                                                 ARBL0318
003180         03  START1          PICTURE  999    VALUE   013.         ARBL0319
003190         03  STOP1           PICTURE  999    VALUE   143.         ARBL0320
003200         03  START2          PICTURE  999    VALUE   153.         ARBL0321
003210         03  STOP2           PICTURE  999    VALUE   283.         ARBL0322
003220         03  SSEM            PICTURE 9V99    VALUE 1.10.          ARBL0323
003230         03  SYEARLY         PICTURE 99V99   VALUE 1.85.          ARBL0324
003240         03  STERM           PICTURE 9V999   VALUE .066.          ARBL0325
003250         03  TSEM            PICTURE 9V99    VALUE 3.00.          ARBL0326
003260         03  TYEARLY         PICTURE 9V99    VALUE 6.00.          ARBL0327
003270         03  TTERM           PICTURE V999    VALUE .214.          ARBL0328
003280         03  MIN                 PICTURE 99   VALUE 25.           ARBL0329
003290         03  MAX             PICTURE   99    VALUE   28.          ARBL0330
003300         03 SINGLE1-4        PICTURE 99V99   VALUE 2.60.          ARBL0331
003310         03 SINGLE5-9        PICTURE 99V99   VALUE 2.60.          ARBL0332
003320         03  YEAR2           PICTURE  9V99   VALUE   9.50.        ARBL0333
003330         03  YEAR3           PICTURE 99V99   VALUE   12.50.       ARBL0334
003340         03  SSTERM          PICTURE 9V999   VALUE .092.          ARBL0335
003350         03  STTERM          PICTURE V999    VALUE .214.          ARBL0336
003360         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0337
003370     02  ED22-27-50-51.                                           ARBL0338
003380         03  START1          PICTURE  999    VALUE   013.         ARBL0339
003390         03  STOP1           PICTURE  999    VALUE   143.         ARBL0340
003400         03  START2          PICTURE  999    VALUE   153.         ARBL0341
003410         03  STOP2           PICTURE  999    VALUE   283.         ARBL0342
003420         03  SSEM            PICTURE 9V99    VALUE 1.10.          ARBL0343
003430         03  SYEARLY         PICTURE 99V99   VALUE 1.85.          ARBL0344
003440         03  STERM           PICTURE 9V999   VALUE .066.          ARBL0345
003450         03  TSEM            PICTURE 9V99    VALUE  3.00.         ARBL0346
003460         03  TYEARLY         PICTURE 9V99    VALUE  6.00.         ARBL0347
003470         03  TTERM           PICTURE V999    VALUE  .214.         ARBL0348
003480         03  MIN             PICTURE   99    VALUE   25.          ARBL0349
003490         03  MAX             PICTURE   99    VALUE   28.          ARBL0350
003500         03 SINGLE1-4        PICTURE 99V99   VALUE 2.60.          ARBL0351
003510         03 SINGLE5-9        PICTURE 99V99   VALUE 2.60.          ARBL0352
003520         03  YEAR2           PICTURE  9V99   VALUE   9.50.        ARBL0353
003530         03  YEAR3           PICTURE 99V99   VALUE   12.50.       ARBL0354
003540         03  SSTERM          PICTURE 9V999   VALUE  .092.         ARBL0355
003550         03  STTERM          PICTURE V999    VALUE  .214.         ARBL0356
003560         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0357
003570     02  ED34.                                                    ARBL0358
003580         03  START1          PICTURE  999    VALUE   013.         ARBL0359
003590         03  STOP1           PICTURE  999    VALUE   053.         ARBL0360
003600         03  START2          PICTURE  999    VALUE   063.         ARBL0361
003610         03  STOP2           PICTURE  999    VALUE   103.         ARBL0362
003620         03  SSEM            PICTURE  9V99   VALUE   2.50.        ARBL0363
003630         03  SYEARLY         PICTURE 99V99   VALUE   5.00.        ARBL0364
003640         03  STERM       PICTURE  9V999  VALUE .500.              ARBL0365
003650         03  TSEM            PICTURE  9V99   VALUE   2.50.        ARBL0366
003660         03, TYEARLY         PICTURE  9V99   VALUE   5.00.        ARBL0367
003670         03  TTERM       PICTURE  V999   VALUE .500.              ARBL0368
003680         03  MIN             PICTURE   99    VALUE   08.          ARBL0369
003690         03  MAX             PICTURE   99    VALUE   10.          ARBL0370
003700         03  SINGLE1-4       PICTURE 99V99   VALUE   5.00.        ARBL0371
003710         03  SINGLE5-9       PICTURE 99V99   VALUE   5.00.        ARBL0372
003720         03  YEAR2           PICTURE  9V99   VALUE   8.50.        ARBL0373
003730         03  YEAR3           PICTURE 99V99   VALUE   12.00.       ARBL0374
003740         03  SSTERM          PICTURE 9V999   VALUE   .000.        ARBL0375
003750         03  STTERM          PICTURE  V999   VALUE   .500.        ARBL0376
003760         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0377
003770     02  ED33.                                                    ARBL0378
003780         03  START1          PICTURE  999    VALUE   013.         ARBL0379
003790         03  STOP1           PICTURE  999    VALUE   053.         ARBL0380
003800         03  START2          PICTURE  999    VALUE   053.         ARBL0381
003810         03  STOP2           PICTURE  999    VALUE   093.         ARBL0382
003820         03  SSEM            PICTURE  9V99   VALUE   0.00.        ARBL0383
003830         03  SYEARLY         PICTURE 99V99   VALUE   0.00.        ARBL0384
003840         03  STERM           PICTURE 9V999   VALUE   .000.        ARBL0385
003850         03  TSEM            PICTURE  9V99   VALUE   3.00.        ARBL0386
003860         03, TYEARLY         PICTURE  9V99   VALUE   6.00.        ARBL0387
003870         03  TTERM           PICTURE  V999   VALUE   .666.        ARBL0388
003880         03  MIN             PICTURE   99    VALUE   08.          ARBL0389
003890         03  MAX             PICTURE   99    VALUE   10.          ARBL0390
003900         03  SINGLE1-4       PICTURE 99V99   VALUE   6.00.        ARBL0391
003910         03  SINGLE5-9       PICTURE 99V99   VALUE   6.00.        ARBL0392
003920         03  YEAR2           PICTURE  9V99   VALUE   9.50.        ARBL0393
003930         03  YEAR3           PICTURE 99V99   VALUE   12.50.       ARBL0394
003940         03  SSTERM          PICTURE 9V999   VALUE   .000.        ARBL0395
003950         03  STTERM          PICTURE  V999   VALUE   .666.        ARBL0396
003960         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0397
003970     02  ED66-85.                                                 ARBL0398
003980         03  START1          PICTURE  999    VALUE   013.         ARBL0399
003990         03  STOP1           PICTURE  999    VALUE   053.         ARBL0400
004000         03  START2          PICTURE  999    VALUE   053.         ARBL0401
004010         03  STOP2           PICTURE  999    VALUE   093.         ARBL0402
004020         03  SSEM            PICTURE  9V99   VALUE   0.00.        ARBL0403
004030         03  SYEARLY         PICTURE 99V99   VALUE   1.00.        ARBL0404
004040         03  STERM           PICTURE 9V999   VALUE .111.          ARBL0405
004050         03  TSEM            PICTURE  9V99   VALUE   0.00.        ARBL0406
004060         03, TYEARLY         PICTURE  9V99   VALUE   2.00.        ARBL0407
004070         03  TTERM           PICTURE  9V99  VALUE  0.22.          ARBL0408
004080         03  MIN             PICTURE   99    VALUE   08.          ARBL0409
004090         03  MAX             PICTURE   99    VALUE   09.          ARBL0410
004100         03  SINGLE1-4           PICTURE 99V99   VALUE 2.00.      ARBL0411
004110         03  SIN7LE5-9           PICTURE 99V99   VALUE 2.00.      ARBL0412
004120         03  YEAR2           PICTURE  9V99   VALUE   3.50.        ARBL0413
004130         03  YEAR3           PICTURE 99V99   VALUE   05.00.       ARBL0414
004140         03  SSTERM          PICTURE 9V999   VALUE .222.          ARBL0415
004150         03  STTERM          PICTURE V999    VALUE .222.          ARBL0416
004160         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0417
004170     02  ED87-93.                                                 ARBL0418
004180         03  START1          PICTURE  999    VALUE   ZERO.        ARBL0419
004190         03  STOP1           PICTURE  999    VALUE   ZERO.        ARBL0420
004200         03  START2          PICTURE  999    VALUE   ZERO.        ARBL0421
004210         03  STOP2           PICTURE  999    VALUE   ZERO.        ARBL0422
004220         03  SSEM            PICTURE  9V99   VALUE   ZERO.        ARBL0423
004230         03  SYEARLY         PICTURE 99V99   VALUE   ZERO.        ARBL0424
004240         03  STERM           PICTURE 9V999   VALUE   ZERO.        ARBL0425
004250         03  TSEM            PICTURE  9V99   VALUE   ZERO.        ARBL0426
004260         03, TYEARLY         PICTURE  9V99   VALUE   ZERO.        ARBL0427
004270         03  TTERM           PICTURE  9V99   VALUE   ZERO.        ARBL0428
004280         03  MIN             PICTURE   99    VALUE   ZERO.        ARBL0429
004290         03  MAX             PICTURE   99    VALUE   ZERO.        ARBL0430
004300         03  SINGLE1-4       PICTURE 99V99   VALUE   ZERO.        ARBL0431
004310         03  SINGLE5-9       PICTURE 99V99   VALUE   ZERO.        ARBL0432
004320         03  YEAR2           PICTURE  9V99   VALUE   ZERO.        ARBL0433
004330         03  YEAR3           PICTURE 99V99   VALUE   ZERO.        ARBL0434
004340         03  SSTERM          PICTURE 9V999   VALUE   ZERO.        ARBL0435
004350         03  STTERM          PICTURE  V999   VALUE   ZERO.        ARBL0436
004360         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0437
004370     02  ED-38.                                                   ARBL0438
004380         03  START1          PICTURE 999     VALUE 013.           ARBL0439
004390         03  STOP1           PICTURE 999     VALUE 073.           ARBL0440
004400         03  START2          PICTURE 999     VALUE 083.           ARBL0441
004410         03  STOP2           PICTURE 999     VALUE 143.           ARBL0442
004420         03  SSEM            PICTURE 9V99    VALUE 8.00.          ARBL0443
004430         03  SYEARLY         PICTURE 99V99   VALUE 14.00.         ARBL0444
004440         03  STERM           PICTURE 9V999   VALUE 1.000.         ARBL0445
004450         03  TSEM            PICTURE 9V99    VALUE 0.00.          ARBL0446
004460         03  TYEARLY         PICTURE 9V99    VALUE 0.00.          ARBL0447
004470         03  TTERM           PICTURE 9V99    VALUE 0.00.          ARBL0448
004480         03  MIN             PICTURE 99      VALUE 14.            ARBL0449
004490         03  MAX             PICTURE 99      VALUE 14.            ARBL0450
004500         03  SINGLE1-4       PICTURE 99V99   VALUE 14.00.         ARBL0451
004510         03  SINGLE5-9       PICTURE 99V99   VALUE 14.00.         ARBL0452
004520         03  YEAR2           PICTURE 9V99    VALUE 0.00.          ARBL0453
004530         03  YEAR3           PICTURE 99V99   VALUE 00.00.         ARBL0454
004540         03  SSTERM          PICTURE 9V999   VALUE 1.000.         ARBL0455
004550         03  STTERM          PICTURE V999    VALUE .000.          ARBL0456
004560         03  FILLER      PICTURE 9[3]        VALUE ZERO.          ARBL0457
004570 01  RATE-TABLE-B REDEFINES RATE-TABLE-A.                         ARBL0458
004580     02  RATE-TABLE  OCCURS 14 TIMES COMP-3  SYNC.                MO A0459
004590         03  FILLER      PICTURE  999.                            ARBL0460
004600         03  FILLER      PICTURE  999.                            ARBL0461
004610         03  FILLER      PICTURE  999.                            ARBL0462
004620         03  END4                PICTURE 999.                     ARBL0463
004630         03  FILLER      PICTURE  9V99.                           ARBL0464
004640         03  FILLER      PICTURE  99V99.                          ARBL0465
004650         03  FILLER          PICTURE 9V999.                       ARBL0466
004660         03  FILLER      PICTURE  9V99.                           ARBL0467
004670         03  FILLER      PICTURE  9V99.                           ARBL0468
004680         03  FILLER      PICTURE  9V99.                           ARBL0469
004690         03  FILLER      PICTURE  99.                             ARBL0470
004700         03  FILLER      PICTURE  99.                             ARBL0471
004710         03  FILLER      PICTURE 99V99.                           ARBL0472
004720         03  FILLER     PICTURE  99V99.                           ARBL0473
004730         03  FILLER      PICTURE 9V99.                            ARBL0474
004740         03  FILLER      PICTURE  99V99.                          ARBL0475
004750         03  FILLER      PICTURE  9V999.                          ARBL0476
004760         03  FILLER      PICTURE  V999.                           ARBL0477
004770         03  FILLER      PICTURE 9[3].                            ARBL0478
004780 01  STORE-RATE-AREA    COMP-3 SYNC.                              MO A0479
004790     02  BEGIN-1ST               PICTURE  999.                    ARBL0480
004800     02  EXPIRE-1ST              PICTURE  999.                    ARBL0481
004810     02  BEGIN-2ND               PICTURE  999.                    ARBL0482
004820     02  EXPIRE-2ND              PICTURE  999.                    ARBL0483
004830     02  STUD-SEM                PICTURE  9V99.                   ARBL0484
004840     02  STUD-YEAR               PICTURE  99V99.                  ARBL0485
004850     02  STUD-TERM               PICTURE  9V999.                  ARBL0486
004860     02  TCH-SEM                 PICTURE  9V99.                   ARBL0487
004870     02  TCH-YEAR                PICTURE  9V99.                   ARBL0488
004880     02  TCH-TERM                PICTURE V999.                    ARBL0489
004890     02  MINIMUM                 PICTURE    99.                   ARBL0490
004900     02  MAXIMUM                 PICTURE    99.                   ARBL0491
004910     02  SINGLE-RATE1-4      PICTURE 99V99.                       ARBL0492
004920     02  SINGLE-RATE5-9      PICTURE 99V99.                       ARBL0493
004930     02  YEAR2-RATE              PICTURE  9V99.                   ARBL0494
004940     02  YEAR3-RATE              PICTURE 99V99.                   ARBL0495
004950     02  SINGLE-STERM            PICTURE  9V999.                  ARBL0496
004960     02  SINGLE-TTERM            PICTURE  V999.                   ARBL0497
004970     02  FILLER              PICTURE 9[3].                        ARBL0498
004980 01  EDITION-TABLE-A   SYNC.                                      ARBL0499
004990     02  ED01                    PICTURE 99  VALUE   99.          ARBL0500
005000     02  ED02                    PICTURE 99  VALUE   99.          ARBL0501
005010     02  ED03                    PICTURE 99  VALUE   99.          ARBL0502
005020     02  ED04                    PICTURE 99  VALUE   99.          ARBL0503
005030     02  ED05                    PICTURE 99  VALUE   99.          ARBL0504
005040     02  ED06                    PICTURE 99  VALUE   99.          ARBL0505
005050     02  ED07                    PICTURE 99  VALUE   99.          ARBL0506
005060     02  ED08                    PICTURE 99  VALUE   01.          ARBL0507
005070     02  ED09                    PICTURE 99  VALUE   99.          ARBL0508
005080     02  ED10                    PICTURE 99  VALUE   02.          ARBL0509
005090     02  ED11                    PICTURE 99  VALUE   02.          ARBL0510
005100     02  ED12                    PICTURE 99  VALUE   02.          ARBL0511
005110     02  ED13                    PICTURE 99  VALUE   02.          ARBL0512
005120     02  ED14                    PICTURE 99  VALUE   02.          ARBL0513
005130     02  ED15                    PICTURE 99  VALUE   02.          ARBL0514
005140     02  ED16                    PICTURE 99  VALUE   02.          ARBL0515
005150     02  ED17                    PICTURE 99  VALUE   02.          ARBL0516
005160     02  ED18                    PICTURE 99  VALUE   03.          ARBL0517
005170     02  ED19                    PICTURE 99  VALUE   03.          ARBL0518
005180     02  ED20                    PICTURE 99  VALUE   04.          ARBL0519
005190     02  ED21                    PICTURE 99  VALUE   04.          ARBL0520
005200     02  ED22                    PICTURE 99  VALUE   09.          ARBL0521
005210     02  ED23                    PICTURE 99  VALUE   09.          ARBL0522
005220     02  ED24                    PICTURE 99  VALUE   09.          ARBL0523
005230     02  ED25                    PICTURE 99  VALUE   09.          ARBL0524
005240     02  ED26                    PICTURE 99  VALUE   09.          ARBL0525
005250     02  ED27                    PICTURE 99  VALUE   09.          ARBL0526
005260     02  ED28                    PICTURE 99  VALUE   02.          ARBL0527
005270     02  ED29                    PICTURE 99  VALUE   02.          ARBL0528
005280     02  ED30                    PICTURE 99  VALUE   05.          ARBL0529
005290     02  ED31                    PICTURE 99  VALUE   05.          ARBL0530
005300     02  ED32                    PICTURE 99  VALUE   06.          ARBL0531
005310     02  ED33                    PICTURE 99  VALUE   11.          ARBL0532
005320     02  ED34                    PICTURE 99  VALUE   10.          ARBL0533
005330     02  ED35                    PICTURE 99  VALUE   99.          ARBL0534
005340     02  ED36                    PICTURE 99  VALUE   99.          ARBL0535
005350     02  ED37                    PICTURE 99  VALUE   99.          ARBL0536
005360     02  ED38                    PICTURE 99  VALUE   14.          ARBL0537
005370     02  ED39                    PICTURE 99  VALUE   99.          ARBL0538
005380     02  ED40                    PICTURE 99  VALUE   07.          ARBL0539
005390     02  ED41                    PICTURE 99  VALUE   07.          ARBL0540
005400     02  ED42                    PICTURE 99  VALUE   08.          ARBL0541
005410     02  ED43                    PICTURE 99  VALUE   08.          ARBL0542
005420     02  ED44                    PICTURE 99  VALUE   99.          ARBL0543
005430     02  ED45                    PICTURE 99  VALUE   99.          ARBL0544
005440     02  ED46                    PICTURE 99  VALUE   99.          ARBL0545
005450     02  ED47                    PICTURE 99  VALUE   99.          ARBL0546
005460     02  ED48                    PICTURE 99  VALUE   99.          ARBL0547
005470     02  ED49                    PICTURE 99  VALUE   99.          ARBL0548
005480     02  ED50                    PICTURE 99  VALUE   09.          ARBL0549
005490     02  ED51                    PICTURE 99  VALUE   09.          ARBL0550
005500     02  ED52                    PICTURE 99  VALUE   99.          ARBL0551
005510     02  ED53                    PICTURE 99  VALUE   99.          ARBL0552
005520     02  ED54                    PICTURE 99  VALUE   99.          ARBL0553
005530     02  ED55                    PICTURE 99  VALUE   99.          ARBL0554
005540     02  ED56                    PICTURE 99  VALUE   99.          ARBL0555
005550     02  ED57                    PICTURE 99  VALUE   99.          ARBL0556
005560     02  ED58                    PICTURE 99  VALUE   99.          ARBL0557
005570     02  ED59                    PICTURE 99  VALUE   99.          ARBL0558
005580     02  ED60                    PICTURE 99  VALUE   99.          ARBL0559
005590     02  ED61                    PICTURE 99  VALUE   99.          ARBL0560
005600     02  ED62                    PICTURE 99  VALUE   99.          ARBL0561
005610     02  ED63                    PICTURE 99  VALUE   99.          ARBL0562
005620     02  ED64                    PICTURE 99  VALUE   99.          ARBL0563
005630     02  ED65                    PICTURE 99  VALUE   99.          ARBL0564
005640     02  ED66                    PICTURE 99  VALUE   12.          ARBL0565
005650     02  ED67                    PICTURE 99  VALUE   12.          ARBL0566
005660     02  ED68                    PICTURE 99  VALUE   12.          ARBL0567
005670     02  ED69                    PICTURE 99  VALUE   12.          ARBL0568
005680     02  ED70                    PICTURE 99  VALUE   12.          ARBL0569
005690     02  ED71                    PICTURE 99  VALUE   12.          ARBL0570
005700     02  ED72                    PICTURE 99  VALUE   12.          ARBL0571
005710     02  ED73                    PICTURE 99  VALUE   12.          ARBL0572
005720     02  ED74                PICTURE 99      VALUE 99.            ARBL0573
005730     02  ED75                    PICTURE 99  VALUE   99.          ARBL0574
005740     02  ED76                    PICTURE 99  VALUE   12.          ARBL0575
005750     02  ED77                    PICTURE 99  VALUE   12.          ARBL0576
005760     02  ED78                    PICTURE 99  VALUE   12.          ARBL0577
005770     02  ED79                    PICTURE 99  VALUE   99.          ARBL0578
005780     02  ED80                    PICTURE 99  VALUE   12.          ARBL0579
005790     02  ED81                    PICTURE 99  VALUE   12.          ARBL0580
005800     02  ED82                    PICTURE 99  VALUE   12.          ARBL0581
005810     02  ED83                    PICTURE 99  VALUE   99.          ARBL0582
005820     02  ED84                PICTURE 99      VALUE 99.            ARBL0583
005830     02  ED85                    PICTURE 99  VALUE   99.          ARBL0584
005840     02  ED86                    PICTURE 99 VALUE 12.             ARBL0585
005850     02  ED87                    PICTURE 99  VALUE   99.          ARBL0586
005860     02  ED88                    PICTURE 99  VALUE   99.          ARBL0587
005870     02  ED89                    PICTURE 99  VALUE   99.          ARBL0588
005880     02  ED90                    PICTURE 99  VALUE   99.          ARBL0589
005890     02  ED91                    PICTURE 99  VALUE   99.          ARBL0590
005900     02  ED92                    PICTURE 99  VALUE   99.          ARBL0591
005910     02  ED93                    PICTURE 99  VALUE   99.          ARBL0592
005920     02  ED94                    PICTURE 99  VALUE   99.          ARBL0593
005930     02  ED95                    PICTURE 99  VALUE   99.          ARBL0594
005940                                                                  ARBL0595
005950 01  EDITION-TABLE-B  REDEFINES EDITION-TABLE-A.                  ARBL0596
005960     02  EDITION-TABLE   PICTURE  99   OCCURS  95  TIMES.         ARBL0597
005970 01  PRINTER           SYNC.                                      ARBL0598
005980     02  CC  PICTURE X VALUE SPACES.                              ARBL0599
005990     02  DATAREA  PICTURE X[132]  VALUE SPACES.                   ARBL0600
006000 01  HDR1    SYNC.                                                ARBL0601
006010     02  FILLER              PICTURE X[3]    VALUE SPACES.        ARBL0602
006020     02  DATE-CONSTANT       PICTURE X[5]    VALUE :DATE :.       ARBL0603
006030     02  RPT-DATE            PICTURE X[8]    VALUE SPACES.        ARBL0604
006040     02  FILLER              PICTURE X[12]   VALUE SPACES.        ARBL0605
006050     02  RPT-NAME            PICTURE X[80]   VALUE                ARBL0606
006060     :    M A G A Z I N E    M A S T E R       C O R R E C T I O  ARBL0607
006070-        :N   L I S T I N G   :.                                  ARBL0608
006080     02  FILLER              PICTURE X[10]   VALUE SPACES.        ARBL0609
006090     02  PAGE-CONSTANT       PICTURE X[5]    VALUE :PAGE :.       ARBL0610
006100     02  PAGE-EDIT           PICTURE ZZZ.                         ARBL0611
006110     02  FILLER              PICTURE X[7]    VALUE SPACES.        ARBL0612
006120 01  HDR2    SYNC.                                                ARBL0613
006130     02  FILLER                  PICTURE X[5]    VALUE SPACES.    ARBL0614
006140     02  FILLER                  PICTURE X[30]   VALUE            ARBL0615
006150         :ZIP   SCH ED TCH    1ST  SEM  :.                        ARBL0616
006160     02  FILLER                  PICTURE X[45]   VALUE            ARBL0617
006170         :        2ND  SEM         AMT PAID            :.         ARBL0618
006180     02  FILLER                  PICTURE X[46]   VALUE            ARBL0619
006190         :BAL-DUE     TERM     STUD   PDTCH  TYPE  CMTS :.        ARBL0620
006200     02  FILLER                  PICTURE X[7]    VALUE SPACES.    ARBL0621
006210 01  DETAIL-LINE       SYNC.                                      ARBL0622
006220     02  FILLER                  PICTURE X[5]    VALUE SPACES.    ARBL0623
006230     02  ZIPP                    PICTURE X[5]    VALUE SPACES.    ARBL0624
006240     02  FILLER                  PICTURE X       VALUE SPACES.    ARBL0625
006250     02  SCHP                    PICTURE XXX     VALUE SPACES.    ARBL0626
006260     02  FILLER                  PICTURE X       VALUE SPACES.    ARBL0627
006270     02  EDP                     PICTURE XX      VALUE SPACES.    ARBL0628
006280     02  FILLER                  PICTURE X       VALUE SPACES.    ARBL0629
006290     02  TCHP                    PICTURE XXX     VALUE SPACES.    ARBL0630
006300     02  FILLER                  PICTURE X[5]    VALUE SPACES.    ARBL0631
006310     02  1SEMP         PICTURE Z,ZZZ.99CR.                        ARBL0632
006320     02  FILLER                  PICTURE X[7]     VALUE SPACES.   ARBL0633
006330     02  2SEMP         PICTURE Z,ZZZ.99CR.                        ARBL0634
006340     02  FILLER                  PICTURE X[7]     VALUE SPACES.   ARBL0635
006350     02  AMT-PDP                 PICTURE Z,ZZZ.99CR.              ARBL0636
006360     02  FILLER                  PICTURE X[9]    VALUE SPACES.    ARBL0637
006370     02  BAL-DUEP                PICTURE Z,ZZZ.99CR.              ARBL0638
006380     02  FILLER                  PICTURE X[5]    VALUE SPACES.    ARBL0639
006390     02  TERMP                   PICTURE X       VALUE SPACES.    ARBL0640
006400     02  FILLER                  PICTURE X[7]    VALUE SPACES.    ARBL0641
006410     02  S-COPYP             PICTURE ZZZ9.                        ARBL0642
006420     02  FILLER              PICTURE X[5]    VALUE SPACES.        ARBL0643
006430     02  P-TCHP              PICTURE Z9.                          ARBL0644
006440     02  FILLER              PICTURE X[5]    VALUE SPACES.        ARBL0645
006450     02  TYPEP               PICTURE X       VALUE SPACES.        ARBL0646
006460     02  FILLER              PICTURE XX      VALUE SPACES.        ARBL0647
006470     02  FILLERCOM           PICTURE X[12]   VALUE SPACES.        ARBL0648
006480 01  TOTALS-LINE REDEFINES DETAIL-LINE.                           ARBL0649
006490     02  FILLER                  PICTURE X.                       ARBL0650
006500     02  TOTAL-LINE-ID           PICTURE X[30].                   ARBL0651
006510     02  FILLER                  PICTURE XX.                      ARBL0652
006520     02  TOTAL-LINE-COUNT        PICTURE ZZZ,ZZZ,ZZZ,ZZ9.         ARBL0653
006530     02  FILLER                  PICTURE X[85].                   ARBL0654
006540 01  TEACHER-MASTER-WORK     SYNC.                                ARBL0655
006550     02  ID-NUMBER.                                               ARBL0656
006560         03  ZIP-CODE            PICTURE   9[5].                  ARBL0657
006570             03  SCH-NUMBER      PICTURE 999.                     ARBL0658
006580         03  EDITION             PICTURE    99.                   ARBL0659
006590         03  TCH-NUMBER          PICTURE   999.                   ARBL0660
006600         03  CHECK-DIGIT         PICTURE  9.                      ARBL0661
006610     02  REST-OF-MASTER-T.                                        ARBL0662
006620         03  CURRENT-COPY.                                        ARBL0663
006630             04  STUD-COPY       PICTURE    9[4].                 ARBL0664
006640             04  XSTUDCOPY REDEFINES STUD-COPY  PICTURE X[4].     ARBL0665
006650             04  PAID-TCH        PICTURE    99.                   ARBL0666
006660             04  XPAIDTCH REDEFINES PAID-TCH  PICTURE XX.         ARBL0667
006670             04  RECS            PICTURE    99.                   ARBL0668
006680             04  FILM            PICTURE    99.                   ARBL0669
006690             04  XTC             PICTURE    99.                   ARBL0670
006700         03  CURRENT-BLANK  REDEFINES CURRENT-COPY.               ARBL0671
006710             04  STUD-BLANK              PICTURE  X[4].           ARBL0672
006720             04  TCH-BLANK               PICTURE   XX.            ARBL0673
006730             04  REC-BLANK               PICTURE   XX.            ARBL0674
006740             04  FILM-BLANK              PICTURE   XX.            ARBL0675
006750                 04  SPECIALE REDEFINES FILM-BLANK.               ARBL0676
006760                     05  1ST-SPEC  PICTURE X.                     ARBL0677
006770                     05  2ND-SPEC  PICTURE X.                     ARBL0678
006780             04  XTC-BLANK               PICTURE   XX.            ARBL0679
006790         03  AMOUNT-PAID    PICTURE S9[4]V99 COMP-3 SYNC.         MO A0680
006800         03  TERM                PICTURE X.                       ARBL0681
006810             88  TERM1   VALUE :1:.                               ARBL0682
006820             88  TERM2   VALUE :2:.                               ARBL0683
006830             88  TERM3   VALUE :3:.                               ARBL0684
006840             88  TERM4   VALUE :4:.                               ARBL0685
006850             88  TERM5   VALUE :5:.                               ARBL0686
006860             88  TERM6   VALUE :6:.                               ARBL0687
006870             88  TERM7   VALUE :7:.                               ARBL0688
006880             88  TERM8   VALUE :8:.                               ARBL0689
006890         03  GRADE-AP            PICTURE  XX.                     ARBL0690
006900         03  TEACHER-NAME        PICTURE  X[25].                  ARBL0691
006910         03  PROMO-KEY           PICTURE  X[5].                   ARBL0692
006920         03  BILL-TO             PICTURE  X.                      ARBL0693
006930             88  BILL1   VALUE :1:.                               ARBL0694
006940             88  BILL2   VALUE :2:.                               ARBL0695
006950             88  BILL3   VALUE :3:.                               ARBL0696
006960             88  BILL4   VALUE :4:.                               ARBL0697
006970             88  BILL5   VALUE :5:.                               ARBL0698
006980             88  BILL6   VALUE :6:.                               ARBL0699
006990         03  BOARD-NUMBER        PICTURE  X[5].                   ARBL0700
007000         03  ISSUE-DATES.                                         ARBL0701
007010             04  BEGINN.                                          ARBL0702
007020                 05  ISS         PICTURE   99.                    ARBL0703
007030                 05  YEAR        PICTURE    9.                    ARBL0704
007040         04  BEGINNN REDEFINES  BEGINN.                           ARBL0705
007050             05  BEGIN           PICTURE 999.                     ARBL0706
007060             04  EXPIREE.                                         ARBL0707
007070                 05  ISS         PICTURE   99.                    ARBL0708
007080                 05  YEAR        PICTURE   9.                     ARBL0709
007090         04  EXPIREEE  REDEFINES  EXPIREE.                        ARBL0710
007100             05  EXPIRE          PICTURE 999.                     ARBL0711
007110         03  CHARGES         COMPUTATIONAL-3.                     ARBL0712
007120             04  FIRST-YR-TERM   PICTURE  S9[4]V99.               ARBL0713
007130             04  SECOND-SEM      PICTURE  S9[4]V99.               ARBL0714
007140             04  POSTAL          PICTURE  S9[3]V99.               ARBL0715
007150             04  OTHER           PICTURE  S9[3]V99.               ARBL0716
007160             04  BALANCE-DUE     PICTURE  S9[4]V99.               ARBL0717
007170         03  REVISION-ONE.                                        ARBL0718
007180             04  REV1-STUD       PICTURE     9[4].                ARBL0719
007190             04  REV1-TCH        PICTURE       99.                ARBL0720
007200             04  REV1-REC        PICTURE       99.                ARBL0721
007210             04  REV1-FILM       PICTURE       99.                ARBL0722
007220             04  REV1-ISSUE.                                      ARBL0723
007230                 05  REV1-ISS    PICTURE       99.                ARBL0724
007240                 05  REV1-YEAR   PICTURE        9.                ARBL0725
007250         03  REVISION-ONE-BLANK  REDEFINES REVISION-ONE           ARBL0726
007260                         PICTURE X[13].                           ARBL0727
007270         03  REVISION-TWO.                                        ARBL0728
007280             04  REV2-STUD       PICTURE     9[4].                ARBL0729
007290             04  REV2-TCH        PICTURE       99.                ARBL0730
007300             04  REV2-REC        PICTURE       99.                ARBL0731
007310             04  REV2-FILM       PICTURE       99.                ARBL0732
007320             04  REV2-ISSUE.                                      ARBL0733
007330                 05  REV2-ISS    PICTURE       99.                ARBL0734
007340                 05  REV2-YEAR   PICTURE        9.                ARBL0735
007350         03  REVISION-TWO-BLANK  REDEFINES  REVISION-TWO          ARBL0736
007360                          PICTURE X[13].                          ARBL0737
007370         03  REVISION-THREE.                                      ARBL0738
007380             04  REV3-STUD       PICTURE     9[4].                ARBL0739
007390             04  REV3-TCH        PICTURE       99.                ARBL0740
007400             04  REV3-REC        PICTURE       99.                ARBL0741
007410             04  REV3-FILM       PICTURE       99.                ARBL0742
007420             04  REV3-ISSUE.                                      ARBL0743
007430                 05  REV3-ISS    PICTURE       99.                ARBL0744
007440                 05  REV3-YEAR   PICTURE        9.                ARBL0745
007450         03  REVISION-THREE-BLANK REDEFINES  REVISION-THREE       ARBL0746
007460                         PICTURE  X[13].                          ARBL0747
007470         03  TYPE-MASTER         PICTURE X.                       ARBL0748
007480         03  MASTER-HOLD-CODE    PICTURE X.                       ARBL0749
007490         03  LAST-SEMESTER.                                       ARBL0750
007500             04  LAST-STUD       PICTURE   X[4].                  ARBL0751
007510       04  LAST-STUN REDEFINES LAST-STUD PICTURE 9[4].            ARBL0752
007520             04  LAST-TCH        PICTURE    XX.                   ARBL0753
007530       04  LAST-TCN  REDEFINES LAST-TCH  PICTURE 99.              ARBL0754
007540             04  LAST-XTC        PICTURE    XX.                   ARBL0755
007550             04  LAST-REC        PICTURE    XX.                   ARBL0756
007560         03  LASN REDEFINES LAST-SEMESTER.                        ARBL0757
007570         04  LASN-STUD  PICTURE 9[4].                             ARBL0758
007580         04  LASN-TCH   PICTURE 99.                               ARBL0759
007590         04  FILLER     PICTURE X[4].                             ARBL0760
007600         03  TRANS-INFO.                                          ARBL0761
007610             04  MTRANS-CODE     PICTURE    X.                    ARBL0762
007620             04  MBATCH-NUMBER   PICTURE   X[6].                  ARBL0763
007630             04  MMAIL-NUMBER    PICTURE  XX.                     ARBL0764
003351         03  SOURCE-KEY          PICTURE X[5].                    MO080765
003352         03  SOURCE-YR           PICTURE X.                       MO080766
003353         03  BONUS-INDIC         PICTURE X.                       MO080767
003354         03  FILLER              PICTURE X[4].                    MO080768
007650 01  SCHOOL-MASTER-WORK      SYNC.                                ARBL0769
007660     02  ID-NUMBER.                                               ARBL0770
007670         03  ZIP-CODE            PICTURE      9[5].               ARBL0771
007680         03  SCH-NUMBER          PICTURE      999.                ARBL0772
007690         03  ED-TCH-BLANK        PICTURE   X[5].                  ARBL0773
007700         03  CHECK-DIGIT         PICTURE        9.                ARBL0774
007710     02  REST-OF-MASTER-S.                                        ARBL0775
007720         03  SCHOOL-NAME         PICTURE      X[20].              ARBL0776
007730         03  STREET-ADDRESS      PICTURE      X[20].              ARBL0777
007740         03  CITY-NAME           PICTURE      X[20].              ARBL0778
007750         03  STATE-ALPHA         PICTURE       XX.                ARBL0779
007760         03  STATE-CODE          PICTURE       XX.                ARBL0780
007770         03  SR-CODE             PICTURE       XX.                ARBL0781
007780         03  COUNTY              PICTURE      XXX.                ARBL0782
007790         03  PP-ZONE             PICTURE        X.                ARBL0783
007800         03  NEW-ZIP             PICTURE X[8].                    ARBL0784
007810         03  HOLD-SCH            PICTURE          X.              ARBL0785
007820         03  SEX                   PICTURE  X.                    ARBL0786
007830         03  TYPESCH                 PICTURE  XX.                 ARBL0787
007840         03  SCHSIZE                         PICTURE  X[4].       ARBL0788
007850         03  STRANS-INFO.                                         ARBL0789
007860             04  STRANS-CODE     PICTURE       X.                 ARBL0790
007870             04  SBATCH-NUMBER   PICTURE      X[6].               ARBL0791
007880             04  SMAIL-NUMBER    PICTURE       XX.                ARBL0792
007890 01  ERROR-TABLE       SYNC.                                      ARBL0793
007900     02  ERROR-MSGS.                                              ARBL0794
007910     03  EM01 PICTURE X[27]  VALUE :01.1ST REC IPMAST NOT SCHL.:. ARBL0795
007920     03  EM02 PICTURE X[27]  VALUE :02.SAVE IP CALL PROG DEPT .:. ARBL0796
007930     03  EM03 PICTURE X[27]  VALUE :03.END OF COACH AP CORRECT.:. ARBL0797
007940     02  ERR-MSGS REDEFINES ERROR-MSGS.                           ARBL0798
007950         03  MSG     PICTURE X[27] OCCURS 03 TIMES.               ARBL0799
007960 01  CONSOLE-OUTPUT-AREA     SYNC.                                ARBL0800
007970     02  FILLER              PICTURE X[10]   VALUE :.A230S   .:.  ARBL0801
007980     02  CONSOLE-MSG         PICTURE X[60]   VALUE SPACES.        ARBL0802
007990 01  COMRG-INFO        SYNC.                                      ARBL0803
008000     02  COM-VARDATE         PICTURE X[8]    VALUE SPACES.        ARBL0804
008010     02  COM-DATE            PICTURE X[6]    VALUE SPACES.        ARBL0805
008020     02  COM-JULDAY          PICTURE X[3]    VALUE SPACES.        ARBL0806
008030     02  COM-PROGNAME        PICTURE X[8]    VALUE SPACES.        ARBL0807
008040     02  COM-PREPASS         PICTURE X[11]   VALUE SPACES.        ARBL0808
008050     02  COM-UPSI            PICTURE X[8]    VALUE SPACES.        ARBL0809
008060     02  COM-TIME            PICTURE X[8]    VALUE SPACES.        ARBL0810
008070     02  FILLER              PICTURE X[4]    VALUE SPACES.        ARBL0811
008080 PROCEDURE DIVISION.                                              ARBL0812
008090 HSK.                                                             ARBL0813
008100     OPEN INPUT  INPUT-MASTER-FILE                                ARBL0814
008110         OUTPUT OUTPUT-MASTER-FILE.                               ARBL0815
008120     MOVE SPACES TO PRINTER,                                      ARBL0816
008130         TEACHER-MASTER-WORK, SCHOOL-MASTER-WORK.                 MO A0817
008140     MOVE SPACES TO DETAIL-LINE.                                  ARBL0818
008150 GET-DATE.                                                        ARBL0819
008160     ENTER LINKAGE.                                               ARBL0820
008170     CALL :GETCOMRG: USING COMRG-INFO.                            ARBL0821
008180     ENTER COBOL.                                                 ARBL0822
008190     MOVE COM-VARDATE TO RPT-DATE.                                ARBL0823
008200 HEADING-ROUTINE.                                                 ARBL0824
008210     ADD 1 TO PAGE-CTR.                                           ARBL0825
008220     MOVE PAGE-CTR TO PAGE-EDIT.                                  ARBL0826
008230     MOVE HDR1 TO PRINTER.                                        ARBL0827
008240     MOVE :1: TO CC.                                              ARBL0828
008250     PERFORM W-REC.                                               ARBL0829
008260     MOVE HDR2 TO PRINTER.                                        ARBL0830
008270     MOVE :0: TO CC.                                              ARBL0831
008280     PERFORM W-REC.                                               ARBL0832
008290     MOVE :0: TO CC.                                              ARBL0833
008300     PERFORM W-REC.                                               ARBL0834
008310     MOVE ZEROS TO LINE-COUNT.                                    ARBL0835
008320 READ-MASTER.                                                     ARBL0836
008330     READ INPUT-MASTER-FILE AT END GO TO EOJ.                     ARBL0837
008340     ADD 1 TO RECORDS-IN.                                         ARBL0838
008350 START-WITH-SCHOOL-RECORD.                                        ARBL0839
008360     IF FIRST-RECORD IS EQUAL TO :Y: AND NOT SCH-RECORD           ARBL0840
008370         MOVE 01 TO ERR-INC  PERFORM ERR-LOOKUP                   ARBL0841
008380         MOVE 02 TO ERR-INC GO TO ERR-LOOKUP.                     ARBL0842
008390     MOVE :N: TO FIRST-RECORD.                                    ARBL0843
008400 TST-IF-SCHOOL.                                                   ARBL0844
008410     IF SCH-RECORD                                                ARBL0845
008420         MOVE INPUT-SCHOOL-MASTER TO SCHOOL-MASTER-WORK           ARBL0846
008430         GO TO WRITE-SCHOOL-RECORD.                               ARBL0847
008440 MOVE-TCH-TO-WORKAREA.                                            ARBL0848
008450     MOVE INPUT-TEACHER-MASTER TO TEACHER-MASTER-WORK.            ARBL0849
       SELECT-SCOPE.                                                        0850
           IF EDITION OF TEACHER-MASTER-WORK IS > 50 OR                     0851
              EDITION OF TEACHER-MASTER-WORK IS > 51 NEXT SENTENCE          0852
               ELSE GO TO SELECT-34.                                        0853
           IF TYPE-MASTER IS > :A: AND ISS OF BEGINN IS \ 5                 0854
               NEXT SENTENCE ELSE GO TO WRITE-TEACHER-RECORD.               0855
           MOVE ISSUE-DATES TO FILLERCOM.                                   0856
           GO TO SET-UP-PRINTER.                                            0857
       SELECT-34.                                                           0858
           IF EDITION OF TEACHER-MASTER-WORK IS > 34                        0859
               AND TYPE-MASTER IS > :A: NEXT SENTENCE                       0860
               ELSE GO TO SELECT-AM-LIBSUB.                                 0861
           IF BILL6 AND YEAR OF BEGINN IS > 9 NEXT SENTENCE                 0862
               ELSE GO TO WRITE-TEACHER-RECORD.                             0863
           MOVE 1 TO YEAR OF BEGINN.                                        0864
           MOVE ISSUE-DATES TO FILLERCOM.                                   0865
           GO TO SET-UP-PRINTER.                                            0866
       SELECT-AM-LIBSUB.                                                MO090867
           IF EDITION OF TEACHER-MASTER-WORK IS > 45                    MO090868
               NEXT SENTENCE ELSE GO TO WRITE-TEACHER-RECORD.           MO090869
           IF FIRST-YR-TERM IS > 0045.00 AND                            MO090870
               PAID-TCH IS > 1                                              0871
               MOVE 1 TO RECS                                           MO090872
               ADD 1 TO TERM4-REC-CTR                                   MO090873
           MOVE REC-BLANK TO FILLERCOM                                      0874
               GO TO SET-UP-PRINTER.                                    MO090875
           IF FIRST-YR-TERM IS > 0090.00 AND                            MO090876
               PAID-TCH IS > 2                                              0877
               MOVE 2 TO RECS                                           MO090878
               ADD 1 TO TERM4-REC-CTR                                   MO090879
           MOVE REC-BLANK TO FILLERCOM                                      0880
               GO TO SET-UP-PRINTER.                                    MO090881
           GO TO WRITE-TEACHER-RECORD.                                  MO090882
004640 SET-UP-PRINTER.                                                      0883
004650     IF LINE-COUNT IS \ 56 PERFORM HEADING-ROUTINE.                   0884
004660     MOVE ZIP-CODE OF TEACHER-MASTER-WORK TO ZIPP.                    0885
004670     MOVE SCH-NUMBER OF TEACHER-MASTER-WORK TO SCHP.                  0886
004680     MOVE EDITION OF TEACHER-MASTER-WORK TO EDP.                      0887
004690     MOVE TCH-NUMBER OF TEACHER-MASTER-WORK TO TCHP.                  0888
004700     MOVE TERM TO TERMP.                                              0889
004710     MOVE FIRST-YR-TERM TO 1SEMP.                                     0890
004720     MOVE SECOND-SEM TO 2SEMP.                                        0891
004730     MOVE AMOUNT-PAID TO AMT-PDP.                                     0892
004740     MOVE BALANCE-DUE TO BAL-DUEP.                                    0893
004750     MOVE STUD-COPY TO S-COPYP.                                       0894
004760     MOVE PAID-TCH TO P-TCHP.                                         0895
004770     MOVE TYPE-MASTER TO TYPEP.                                       0896
004790     ADD 1 TO TOTAL-ADJUSTED-RECORDS.                                 0897
004800     MOVE DETAIL-LINE TO PRINTER.                                     0898
004810     MOVE SPACES TO DETAIL-LINE.                                      0899
004820     MOVE : : TO CC.                                                  0900
009360 W-REC.                                                           ARBL0901
009370     ENTER LINKAGE.                                               ARBL0902
009380     CALL :PRTSPOOL: USING PRINTER.                               ARBL0903
009390     ENTER COBOL.                                                 ARBL0904
009400     ADD 1 TO LINE-COUNT.                                         ARBL0905
009410     MOVE SPACES TO DATAREA.                                      ARBL0906
009420 WRITE-TEACHER-RECORD.                                            ARBL0907
009430     WRITE OUTPUT-TEACHER-MASTER FROM TEACHER-MASTER-WORK.        ARBL0908
009440     MOVE SPACES TO TEACHER-MASTER-WORK.                          MO N0909
009450     ADD 1 TO RECORDS-OUT.                                        ARBL0910
009460     ADD 1 TO TEACHER-CTR.                                        ARBL0911
009470     MOVE ZEROS TO TCH-CHARGE.                                    ARBL0912
009480     GO TO READ-MASTER.                                           ARBL0913
009490 WRITE-SCHOOL-RECORD.                                             ARBL0914
009500     WRITE OUTPUT-SCHOOL-MASTER FROM SCHOOL-MASTER-WORK.          ARBL0915
009510     MOVE SPACES TO SCHOOL-MASTER-WORK.                           MO A0916
009520     ADD 1 TO RECORDS-OUT.                                        ARBL0917
009530     ADD 1 TO SCHOOL-CTR.                                         ARBL0918
009540     GO TO READ-MASTER.                                           ARBL0919
009550 EOJ.                                                             ARBL0920
009560     PERFORM HEADING-ROUTINE.                                     ARBL0921
009570     MOVE SPACES TO TOTALS-LINE.                                  ARBL0922
009580     MOVE :RECORDS IN: TO TOTAL-LINE-ID.                          ARBL0923
009590     MOVE RECORDS-IN TO TOTAL-LINE-COUNT.                         ARBL0924
009600     MOVE TOTALS-LINE TO PRINTER.                                 ARBL0925
009610     MOVE :0: TO CC.                                              ARBL0926
009620     PERFORM W-REC.                                               ARBL0927
009630     MOVE :RECORDS OUT: TO TOTAL-LINE-ID.                         ARBL0928
009640     MOVE RECORDS-OUT TO TOTAL-LINE-COUNT.                        ARBL0929
009650     MOVE TOTALS-LINE TO PRINTER.                                 ARBL0930
009660     PERFORM W-REC.                                               ARBL0931
009670     MOVE :SCHOOLS OUT:  TO TOTAL-LINE-ID                         ARBL0932
009680     MOVE SCHOOL-CTR TO TOTAL-LINE-COUNT.                         ARBL0933
009690     MOVE TOTALS-LINE TO PRINTER.                                 ARBL0934
009700     PERFORM W-REC.                                               ARBL0935
009710     MOVE :TEACHERS OUT: TO TOTAL-LINE-ID.                        ARBL0936
009720     MOVE TEACHER-CTR TO TOTAL-LINE-COUNT.                        ARBL0937
009730     MOVE TOTALS-LINE TO PRINTER.                                 ARBL0938
009740     PERFORM W-REC.                                               ARBL0939
009750     MOVE :TOTAL ADJUSTED RECORDS: TO TOTAL-LINE-ID.              ARBL0940
009760     MOVE TOTAL-ADJUSTED-RECORDS TO TOTAL-LINE-COUNT.             ARBL0941
009770     MOVE TOTALS-LINE TO PRINTER.                                 ARBL0942
009780     PERFORM W-REC.                                               ARBL0943
009790     MOVE :TOTAL AM    ACCTS: TO TOTAL-LINE-ID.                   ARBL0944
009800     MOVE TERM4-REC-CTR TO TOTAL-LINE-COUNT.                      ARBL0945
009810     MOVE TOTALS-LINE TO PRINTER.                                 ARBL0946
009820     PERFORM W-REC.                                               ARBL0947
009830 CLOSE-FILES.                                                     ARBL0948
009840     CLOSE  INPUT-MASTER-FILE, OUTPUT-MASTER-FILE.                ARBL0949
009850     MOVE 03 TO ERR-INC PERFORM ERR-LOOKUP.                       ARBL0950
009860     ENTER LINKAGE.                                               ARBL0951
009870     CALL :UNITCLOS:.                                             ARBL0952
009880     ENTER COBOL.                                                 ARBL0953
009890     STOP RUN.                                                    ARBL0954
009900 ERR-LOOKUP.                                                      ARBL0955
009910     MOVE MSG [ERR-INC] TO CONSOLE-MSG.                           ARBL0956
009920     ENTER LINKAGE.                                               ARBL0957
009930     CALL :CONSPOOL: USING CONSOLE-OUTPUT-AREA.                   ARBL0958
009940     ENTER COBOL.                                                 ARBL0959
009950 ABEND.                                                           ARBL0960
009960     ENTER LINKAGE.                                               ARBL0961
009970     CALL :DUMPSTOP:.                                             ARBL0962
009980     ENTER COBOL.                                                 ARBL0963
009990 THATS-ALL-FOLKS.                                                 ARBL0964
                                                                                                                                                                                                                                      
f`H¯