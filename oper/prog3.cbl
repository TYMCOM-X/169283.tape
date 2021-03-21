000010 IDENTIFICATION DIVISION.                                         A2300000
000050 PROGRAM-ID.                                                      A2300001
000090     A230.                                                        A2300002
000130 AUTHOR.                                                          A2300003
000170     MIKE ORBAN.                                                  A2300004
000210 DATE-WRITTEN.                                                    A2300005
000250     MARCH 15, 1972.                                              A2300006
000255 DATE-COMPILED.  11/30/72.                                        A2300007
000290 REMARKS.                                                         A2300008
000330     THIS PROGRAM WILL QUALIFY ALL ACTIVE COACH AND SINGLE        A2300009
000370     FORECAST ACCOUNTS.                                           A2300010
000410 ENVIRONMENT DIVISION.                                            A2300011
000450 CONFIGURATION SECTION.                                           A2300012
000490 SOURCE-COMPUTER.                                                 A2300013
000530     IBM-370-145.                                                 A2300014
000570 OBJECT-COMPUTER.                                                 A2300015
000610     IBM-370-145.                                                 A2300016
000650 INPUT-OUTPUT SECTION.                                            A2300017
000690 FILE-CONTROL.                                                    A2300018
000730     SELECT  INPUT-MASTER-FILE ASSIGN SYS010-UT-2400-S.           A2300019
000770     SELECT  OUTPUT-MASTER-FILE ASSIGN SYS012-UT-2400-S.          A2300020
000810 I-O-CONTROL.                                                     A2300021
000850     APPLY WRITE-ONLY ON OUTPUT-MASTER-FILE.                      A2300022
000890 DATA DIVISION.                                                   A2300023
000930 FILE SECTION.                                                    A2300024
000970 FD  INPUT-MASTER-FILE                                            A2300025
001010     BLOCK CONTAINS 1364 CHARACTERS                               A2300026
001050     LABEL RECORDS ARE STANDARD                                   A2300027
001090     DATA RECORDS ARE  INPUT-TEACHER-MASTER, INPUT-SCHOOL-MASTER  A2300028
001130     RECORDING V.                                                 A2300029
001170 01  INPUT-TEACHER-MASTER SYNC.                                   A2300030
001210     02  ID-NUMBER.                                               A2300031
001250         03  ZIP-CODE            PICTURE  9[5].                   A2300032
001290         03  CODE-ZIP REDEFINES ZIP-CODE.                         A2300033
001330             04  ZIP-HIGH        PICTURE X.                       A2300034
001370             04  FILLER          PICTURE X[4].                    A2300035
001410         03  SCH-NUMBER          PICTURE  999.                    A2300036
001450         03  EDITION             PICTURE  99.                     A2300037
001490         03  TCH-NUMBER          PICTURE  999.                    A2300038
001530         03  CHECK-DIGIT         PICTURE     9.                   A2300039
001570     02  REST-OF-MASTERI.                                         A2300040
001610         03  CURRENT-COPYI.                                       A2300041
001650             04  STUD-COPYI      PICTURE    9[4].                 A2300042
001690             04  XSTUDCOPYI REDEFINES STUD-COPYI  PICTURE X[4].   A2300043
001730             04  PAID-TCHI       PICTURE    99.                   A2300044
001770             04  XPAIDTCHI REDEFINES PAID-TCHI  PICTURE XX.       A2300045
001810             04  RECSI           PICTURE    99.                   A2300046
001850             04  FILMI           PICTURE    99.                   A2300047
001890             04  XTCI            PICTURE    99.                   A2300048
001930         03  AMOUNT-PAIDI  PICTURE S9[4]V99 COMPUTATIONAL-3.      A2300049
001970          03 TERM-IN             PICTURE    9.                    A2300050
002010             88 ITERM1   VALUE 1.                                 A2300051
002050             88 ITERM2   VALUE 2.                                 A2300052
002090             88 ITERM3   VALUE 3.                                 A2300053
002130             88 ITERM4   VALUE 4.                                 A2300054
002170             88 ITERM5   VALUE 5.                                 A2300055
002210             88 ITERM6   VALUE 6.                                 A2300056
002250             88 ITERM7   VALUE 7.                                 A2300057
002290             88 ITERM8   VALUE 8.                                 A2300058
002330         03  TERMX REDEFINES TERM-IN    PICTURE X.                A2300059
002370         03  GRADE-API           PICTURE  XX.                     A2300060
002410         03  TEACHER-NAMEI       PICTURE  X[25].                  A2300061
002450         03  PROMO-KEYI          PICTURE  X[5].                   A2300062
002490         03  BILL-TOI            PICTURE    9.                    A2300063
002530             88 IBILL1   VALUE 1.                                 A2300064
002570             88 IBILL2   VALUE 2.                                 A2300065
002610             88 IBILL3   VALUE 3.                                 A2300066
002650             88 IBILL4   VALUE 4.                                 A2300067
002690             88 IBILL5   VALUE 5.                                 A2300068
002730             88 IBILL6   VALUE 6.                                 A2300069
002770         03  BOARD-NUMBERI       PICTURE  X[5].                   A2300070
002810         03  ISSUE-DATESI.                                        A2300071
002850             04  BEGINI.                                          A2300072
002890                 05  ISSI        PICTURE    99.                   A2300073
002930                 05  YEARI       PICTURE    9.                    A2300074
002970             04  EXPIREI.                                         A2300075
003010                 05  ISSI        PICTURE    99.                   A2300076
003050                 05  YEARI       PICTURE   9.                     A2300077
003090     03  SPEC-ISS-CHECK REDEFINES  ISSUE-DATESI.                  A2300078
003130         04  BGNI.                                                A2300079
003170                 05  II        PICTURE  XX.                       A2300080
003210                 05  YYI            PICTURE  X.                   A2300081
003250             04  EXPI.                                            A2300082
003290                 05  II            PICTURE  XX.                   A2300083
003330                 05  YYI       PICTURE  X.                        A2300084
003370         03  CHARGESI        COMPUTATIONAL-3.                     A2300085
003410             04  FIRST-YR-TERMI  PICTURE  S9[4]V99.               A2300086
003450             04  SECOND-SEMI     PICTURE  S9[4]V99.               A2300087
003490             04  POSTALI         PICTURE  S9[3]V99.               A2300088
003530             04  OTHERI          PICTURE  S9[3]V99.               A2300089
003570             04  BALANCE-DUEI    PICTURE  S9[4]V99.               A2300090
003610         03  REVISION-ONEI.                                       A2300091
003650             04  REV1-STUDI      PICTURE     9[4].                A2300092
003690             04  REV1-TCHI       PICTURE       99.                A2300093
003730             04  REV1-RECI       PICTURE       99.                A2300094
003770             04  REV1-FILMI      PICTURE       99.                A2300095
003810             04  REV1-ISSUEI.                                     A2300096
003850                 05  REV1-ISSI   PICTURE    99.                   A2300097
003890                 05  REV1-YEARI  PICTURE        9.                A2300098
003930         03  REVISION-TWOI.                                       A2300099
003970             04  REV2-STUDI      PICTURE     9[4].                A2300100
004010             04  REV2-TCHI       PICTURE       99.                A2300101
004050             04  REV2-RECI       PICTURE       99.                A2300102
004090             04  REV2-FILMI      PICTURE       99.                A2300103
004130             04  REV2-ISSUEI.                                     A2300104
004170                 05  REV2-ISSI   PICTURE    99.                   A2300105
004210                 05  REV2-YEARI  PICTURE        9.                A2300106
004250         03  REVISION-THREEI.                                     A2300107
004290             04  REV3-STUDI      PICTURE     9[4].                A2300108
004330             04  REV3-TCHI       PICTURE       99.                A2300109
004370             04  REV3-RECI       PICTURE       99.                A2300110
004410             04  REV3-FILMI      PICTURE       99.                A2300111
004450             04  REV3-ISSUEI.                                     A2300112
004490                 05  REV3-ISSI   PICTURE    99.                   A2300113
004530                 05  REV3-YEARI  PICTURE        9.                A2300114
004570         03  TYPE-MASTERI        PICTURE A.                       A2300115
004610         03  MASTER-HOLD-CODEI   PICTURE X.                       A2300116
004650         03  LAST-SEMESTERI.                                      A2300117
004690             04  LAST-STUDI      PICTURE   9[4].                  A2300118
004730             04  LAST-TCHI       PICTURE    99.                   A2300119
004770             04  LAST-XTCI       PICTURE    99.                   A2300120
004810             04  LAST-RECI       PICTURE    99.                   A2300121
004850         03  TRANS-INFOI.                                         A2300122
004890             04  MTRANS-CODEI    PICTURE    X.                    A2300123
004930             04  MBATCH-NUMBERI  PICTURE   X[6].                  A2300124
004970             04  MMAIL-NUMBERI   PICTURE    XX.                   A2300125
005010         03  FILLER              PICTURE   X[11].                 A2300126
005050 01                      INPUT-SCHOOL-MASTER SYNC.                A2300127
005090     02  ID-NUMBER.                                               A2300128
005130         03  ZIP-CODE            PICTURE  9[5].                   A2300129
005170         03  SCH-NUMBER          PICTURE  999.                    A2300130
005210         03  BLANKS              PICTURE  X[5].                   A2300131
005250             88  SCH-RECORD      VALUE SPACES.                    A2300132
005290         03  CHECK-DIGIT         PICTURE        9.                A2300133
005330     02  REST-OF-MASTERIS.                                        A2300134
005370         03  SCHOOL-NAMEI        PICTURE      X[20].              A2300135
005410         03  STREET-ADDRESSI     PICTURE      X[20].              A2300136
005450         03  CITY-NAMEI          PICTURE      X[20].              A2300137
005490         03  STATE-ALPHAI        PICTURE       XX.                A2300138
005530         03  STATE-CODEI         PICTURE       XX.                A2300139
005570         03  SR-CODEI            PICTURE       XX.                A2300140
005610         03  COUNTYI             PICTURE      XXX.                A2300141
005650         03  PP-ZONEI            PICTURE        X.                A2300142
005690         03  NEW-IDI             PICTURE      X[8].               A2300143
005730         03  HOLDI                         PICTURE  X.            A2300144
005770         03  SEXI                     PICTURE  X.                 A2300145
005810         03  TYPESCHI                     PICTURE  XX.            A2300146
005850         03  SCHSIZEI                        PICTURE  X[4].       A2300147
005890         03  STRANS-INFOI.                                        A2300148
005930             04  STRANS-CODEI    PICTURE       X.                 A2300149
005970             04  SBATCH-NUMBERI  PICTURE      X[6].               A2300150
006010             04  SMAIL-NUMBERI   PICTURE       XX.                A2300151
006050 FD  OUTPUT-MASTER-FILE                                           A2300152
006090         BLOCK CONTAINS 1364 CHARACTERS                           A2300153
006130     LABEL RECORDS ARE STANDARD                                   A2300154
006170     DATA RECORDS ARE OUTPUT-TECHER-MASTER, OUTPUT-SCHOOL-MASTER  A2300155
006210     RECORDING MODE IS V.                                         A2300156
006250 01  OUTPUT-TEACHER-MASTER       PICTURE X[164] SYNC.             A2300157
006290 01  OUTPUT-SCHOOL-MASTER        PICTURE X[109] SYNC.             A2300158
006330 WORKING-STORAGE SECTION.                                         A2300159
006331 77  MODULE-ID  PIC X[24]  VALUE :A230SANS 11/23/72 01****:.      A2300160
006350 77  SINGLES-CTR  PIC S9[9]  COMP-3  SYNC  VALUE <0.              A2300161
006370 77  ERR-INC    COMPUTATIONAL PICTURE S99  VALUE ZEROS SYNC.      A2300162
006410 77  LINE-COUNT COMP  PIC S99  SYNC  VALUE <0.                    A2300163
006450 77  RECORDS-IN COMPUTATIONAL-3  PICTURE S9[11]  VALUE ZEROS.     A2300164
006490 77  RECORDS-OUT COMPUTATIONAL-3 PICTURE S9[11]  VALUE ZEROS.     A2300165
006530 77  PAGE-CTR    COMPUTATIONAL-3 PICTURE S9[3]   VALUE ZEROS.     A2300166
006570 77  SCHOOL-CTR  COMPUTATIONAL-3 PICTURE S9[11]  VALUE ZEROS.     A2300167
006610 77  TEACHER-CTR COMPUTATIONAL-3 PICTURE S9[11]  VALUE ZEROS.     A2300168
006650 77  FIRST-RECORD                PICTURE X       VALUE :Y:.       A2300169
006690 77  TOTAL-ADJUSTED-RECORDS      COMPUTATIONAL-3                  A2300170
006730         PICTURE S9[11] VALUE ZEROS.                              A2300171
006770 01  PRINTER SYNC.                                                A2300172
006810     02  CC  PICTURE X VALUE SPACES.                              A2300173
006850     02  DATAREA  PICTURE X[132]  VALUE SPACES.                   A2300174
006890 01  HDR1 SYNC.                                                   A2300175
006930     02  FILLER              PICTURE X[3]    VALUE SPACES.        A2300176
006970     02  DATE-CONSTANT       PICTURE X[5]    VALUE :DATE :.       A2300177
007010     02  RPT-DATE            PICTURE X[8]    VALUE SPACES.        A2300178
007050     02  FILLER              PICTURE X[12]   VALUE SPACES.        A2300179
007090     02  RPT-NAME            PICTURE X[80]   VALUE                A2300180
007130     :    M A G A Z I N E    G R A D E   A P   C O R R E C T I O  A2300181
007170-        :N   L I S T I N G   :.                                  A2300182
007210     02  FILLER              PICTURE X[10]   VALUE SPACES.        A2300183
007250     02  PAGE-CONSTANT       PICTURE X[5]    VALUE :PAGE :.       A2300184
007290     02  PAGE-EDIT           PICTURE ZZZ.                         A2300185
007330     02  FILLER              PICTURE X[7]    VALUE SPACES.        A2300186
007370 01  HDR2 SYNC.                                                   A2300187
007410     02  FILLER                  PICTURE X[5]    VALUE SPACES.    A2300188
007450     02  FILLER                  PICTURE X[30]   VALUE            A2300189
007490         :ZIP   SCH ED TCH    1ST  SEM  :.                        A2300190
007530     02  FILLER                  PICTURE X[45]   VALUE            A2300191
007570         :        2ND  SEM         AMT PAID            :.         A2300192
007610     02  FILLER                  PICTURE X[46]   VALUE            A2300193
007650         :BAL-DUE     TERM     STUD   PDTCH  TYPE  GAP  :.        A2300194
007690     02  FILLER                  PICTURE X[7]    VALUE SPACES.    A2300195
007730 01  DETAIL-LINE SYNC.                                            A2300196
007770     02  FILLER                  PICTURE X[5]    VALUE SPACES.    A2300197
007810     02  ZIPP                    PICTURE X[5]    VALUE SPACES.    A2300198
007850     02  FILLER                  PICTURE X       VALUE SPACES.    A2300199
007890     02  SCHP                    PICTURE XXX     VALUE SPACES.    A2300200
007930     02  FILLER                  PICTURE X       VALUE SPACES.    A2300201
007970     02  EDP                     PICTURE XX      VALUE SPACES.    A2300202
008010     02  FILLER                  PICTURE X       VALUE SPACES.    A2300203
008050     02  TCHP                    PICTURE XXX     VALUE SPACES.    A2300204
008090     02  FILLER                  PICTURE X[5]    VALUE SPACES.    A2300205
008130     02  1SEMP         PICTURE Z,ZZZ.99CR.                        A2300206
008170     02  FILLER                  PICTURE X[7]     VALUE SPACES.   A2300207
008210     02  2SEMP         PICTURE Z,ZZZ.99CR.                        A2300208
008250     02  FILLER                  PICTURE X[7]     VALUE SPACES.   A2300209
008290     02  AMT-PDP                 PICTURE Z,ZZZ.99CR.              A2300210
008330     02  FILLER                  PICTURE X[9]    VALUE SPACES.    A2300211
008370     02  BAL-DUEP                PICTURE Z,ZZZ.99CR.              A2300212
008410     02  FILLER                  PICTURE X[5]    VALUE SPACES.    A2300213
008450     02  TERMP                   PICTURE X       VALUE SPACES.    A2300214
008490     02  FILLER                  PICTURE X[7]    VALUE SPACES.    A2300215
008530     02  S-COPYP             PICTURE ZZZ9.                        A2300216
008570     02  FILLER              PICTURE X[5]    VALUE SPACES.        A2300217
008610     02  P-TCHP              PICTURE Z9.                          A2300218
008650     02  FILLER              PICTURE X[5]    VALUE SPACES.        A2300219
008690     02  TYPEP               PICTURE X       VALUE SPACES.        A2300220
008730     02  FILLER              PICTURE XX      VALUE SPACES.        A2300221
008770     02  FILLERCOM           PICTURE X[12]   VALUE SPACES.        A2300222
008810 01  TOTALS-LINE REDEFINES DETAIL-LINE SYNC.                      A2300223
00    02  FILLER                  PICTURE X.                       A2300224
008890     02  TOTAL-LINE-ID           PICTURE X[30].                   A2300225
008930     02  FILLER                  PICTURE XX.                      A2300226
008970     02  TOTAL-LINE-COUNT        PICTURE ZZZ,ZZZ,ZZZ,ZZ9.         A2300227
009010     02  FILLER                  PICTURE X[85].                   A2300228
009050 01  TEACHER-MASTER-WORK SYNC.                                    A2300229
009090     02  ID-NUMBER.                                               A2300230
009130         03  ZIP-CODE            PICTURE   9[5].                  A2300231
009170             03  SCH-NUMBER      PICTURE 999.                     A2300232
009210         03  EDITION             PICTURE    99.                   A2300233
009250         03  TCH-NUMBER          PICTURE   999.                   A2300234
009290         03  CHECK-DIGIT         PICTURE  9.                      A2300235
009330     02  REST-OF-MASTER-T.                                        A2300236
009370         03  CURRENT-COPY.                                        A2300237
009410             04  STUD-COPY       PICTURE    9[4].                 A2300238
009450             04  XSTUDCOPY REDEFINES STUD-COPY  PICTURE X[4].     A2300239
009490             04  PAID-TCH        PICTURE    99.                   A2300240
009530             04  XPAIDTCH REDEFINES PAID-TCH  PICTURE XX.         A2300241
009570             04  RECS            PICTURE    99.                   A2300242
009610             04  FILM            PICTURE    99.                   A2300243
009650             04  XTC             PICTURE    99.                   A2300244
009690         03  CURRENT-BLANK  REDEFINES CURRENT-COPY.               A2300245
009730             04  STUD-BLANK              PICTURE  X[4].           A2300246
009770             04  TCH-BLANK               PICTURE   XX.            A2300247
009810             04  REC-BLANK               PICTURE   XX.            A2300248
009850             04  FILM-BLANK              PICTURE   XX.            A2300249
009890                 04  SPECIALE REDEFINES FILM-BLANK.               A2300250
009930                     05  1ST-SPEC  PICTURE X.                     A2300251
009970                     05  2ND-SPEC  PICTURE X.                     A2300252
010010             04  XTC-BLANK               PICTURE   XX.            A2300253
010050         03  AMOUNT-PAID    PICTURE S9[4]V99 COMPUTATIONAL-3.     A2300254
010090         03  TERM                PICTURE X.                       A2300255
010130             88  TERM1   VALUE :1:.                               A2300256
010170             88  TERM2   VALUE :2:.                               A2300257
010210             88  TERM3   VALUE :3:.                               A2300258
010250             88  TERM4   VALUE :4:.                               A2300259
010290             88  TERM5   VALUE :5:.                               A2300260
010330             88  TERM6   VALUE :6:.                               A2300261
010370             88  TERM7   VALUE :7:.                               A2300262
010410             88  TERM8   VALUE :8:.                               A2300263
010450         03  GRADE-AP            PICTURE  XX.                     A2300264
010490         03  TEACHER-NAME        PICTURE  X[25].                  A2300265
010530         03  PROMO-KEY           PICTURE  X[5].                   A2300266
010570         03  BILL-TO             PICTURE  X.                      A2300267
010610             88  BILL1   VALUE :1:.                               A2300268
010650             88  BILL2   VALUE :2:.                               A2300269
010690             88  BILL3   VALUE :3:.                               A2300270
010730             88  BILL4   VALUE :4:.                               A2300271
010770             88  BILL5   VALUE :5:.                               A2300272
010810             88  BILL6   VALUE :6:.                               A2300273
010850         03  BOARD-NUMBER        PICTURE  X[5].                   A2300274
010890         03  ISSUE-DATES.                                         A2300275
010930             04  BEGINN.                                          A2300276
010970                 05  ISS         PICTURE   99.                    A2300277
011010                 05  YEAR        PICTURE    9.                    A2300278
011050         04  BEGINNN REDEFINES  BEGINN.                           A2300279
011090             05  BEGIN           PICTURE 999.                     A2300280
011130             04  EXPIREE.                                         A2300281
011170                 05  ISS         PICTURE   99.                    A2300282
011210                 05  YEAR        PICTURE   9.                     A2300283
011250         04  EXPIREEE  REDEFINES  EXPIREE.                        A2300284
011290             05  EXPIRE          PICTURE 999.                     A2300285
011330         03  CHARGES         COMPUTATIONAL-3.                     A2300286
011370             04  FIRST-YR-TERM   PICTURE  S9[4]V99.               A2300287
011410             04  SECOND-SEM      PICTURE  S9[4]V99.               A2300288
011450             04  POSTAL          PICTURE  S9[3]V99.               A2300289
011490             04  OTHER           PICTURE  S9[3]V99.               A2300290
011530             04  BALANCE-DUE     PICTURE  S9[4]V99.               A2300291
011570         03  REVISION-ONE.                                        A2300292
011610             04  REV1-STUD       PICTURE     9[4].                A2300293
011650             04  REV1-TCH        PICTURE       99.                A2300294
011690             04  REV1-REC        PICTURE       99.                A2300295
011730             04  REV1-FILM       PICTURE       99.                A2300296
011770             04  REV1-ISSUE.                                      A2300297
011810                 05  REV1-ISS    PICTURE       99.                A2300298
011850                 05  REV1-YEAR   PICTURE        9.                A2300299
011890         03  REVISION-ONE-BLANK  REDEFINES REVISION-ONE           A2300300
011930                         PICTURE X[13].                           A2300301
011970         03  REVISION-TWO.                                        A2300302
012010             04  REV2-STUD       PICTURE     9[4].                A2300303
012050             04  REV2-TCH        PICTURE       99.                A2300304
012090             04  REV2-REC        PICTURE       99.                A2300305
012130             04  REV2-FILM       PICTURE       99.                A2300306
012170             04  REV2-ISSUE.                                      A2300307
012210                 05  REV2-ISS    PICTURE       99.                A2300308
012250                 05  REV2-YEAR   PICTURE        9.                A2300309
012290         03  REVISION-TWO-BLANK  REDEFINES  REVISION-TWO          A2300310
012330                          PICTURE X[13].                          A2300311
012370         03  REVISION-THREE.                                      A2300312
012410             04  REV3-STUD       PICTURE     9[4].                A2300313
012450             04  REV3-TCH        PICTURE       99.                A2300314
012490             04  REV3-REC        PICTURE       99.                A2300315
012530             04  REV3-FILM       PICTURE       99.                A2300316
012570             04  REV3-ISSUE.                                      A2300317
012610                 05  REV3-ISS    PICTURE       99.                A2300318
012650                 05  REV3-YEAR   PICTURE        9.                A2300319
012690         03  REVISION-THREE-BLANK REDEFINES  REVISION-THREE       A2300320
012730                         PICTURE  X[13].                          A2300321
012770         03  TYPE-MASTER         PICTURE X.                       A2300322
012810         03  MASTER-HOLD-CODE    PICTURE X.                       A2300323
012850         03  LAST-SEMESTER.                                       A2300324
012890             04  LAST-STUD       PICTURE   X[4].                  A2300325
012930       04  LAST-STUN REDEFINES LAST-STUD PICTURE 9[4].            A2300326
012970             04  LAST-TCH        PICTURE    XX.                   A2300327
013010       04  LAST-TCN  REDEFINES LAST-TCH  PICTURE 99.              A2300328
013050             04  LAST-XTC        PICTURE    XX.                   A2300329
013090             04  LAST-REC        PICTURE    XX.                   A2300330
013130         03  LASN REDEFINES LAST-SEMESTER.                        A2300331
013170         04  LASN-STUD  PICTURE 9[4].                             A2300332
013210         04  LASN-TCH   PICTURE 99.                               A2300333
013250         04  FILLER     PICTURE X[4].                             A2300334
013290         03  TRANS-INFO.                                          A2300335
013330             04  MTRANS-CODE     PICTURE    X.                    A2300336
013370             04  MBATCH-NUMBER   PICTURE   X[6].                  A2300337
013410             04  MMAIL-NUMBER    PICTURE  XX.                     A2300338
013450         03  FILLER              PICTURE   X[11].                 A2300339
013490 01  SCHOOL-MASTER-WORK SYNC.                                     A2300340
013530     02  ID-NUMBER.                                               A2300341
013570         03  ZIP-CODE            PICTURE      9[5].               A2300342
013610         03  SCH-NUMBER          PICTURE      999.                A2300343
013650         03  ED-TCH-BLANK        PICTURE   X[5].                  A2300344
013690         03  CHECK-DIGIT         PICTURE        9.                A2300345
013730     02  REST-OF-MASTER-S.                                        A2300346
013770         03  SCHOOL-NAME         PICTURE      X[20].              A2300347
013810         03  STREET-ADDRESS      PICTURE      X[20].              A2300348
013850         03  CITY-NAME           PICTURE      X[20].              A2300349
013890         03  STATE-ALPHA         PICTURE       XX.                A2300350
013930         03  STATE-CODE          PICTURE       XX.                A2300351
013970         03  SR-CODE             PICTURE       XX.                A2300352
014010         03  COUNTY              PICTURE      XXX.                A2300353
014050         03  PP-ZONE             PICTURE        X.                A2300354
014090         03  NEW-ZIP             PICTURE X[8].                    A2300355
014130         03  HOLD-SCH            PICTURE          X.              A2300356
014170         03  SEX                   PICTURE  X.                    A2300357
014210         03  TYPESCH                 PICTURE  XX.                 A2300358
014250         03  SCHSIZE                         PICTURE  X[4].       A2300359
014290         03  STRANS-INFO.                                         A2300360
014330             04  STRANS-CODE     PICTURE       X.                 A2300361
014370             04  SBATCH-NUMBER   PICTURE      X[6].               A2300362
014410             04  SMAIL-NUMBER    PICTURE       XX.                A2300363
014450 01  ERROR-TABLE SYNC.                                            A2300364
014490     02  ERROR-MSGS.                                              A2300365
014530     03  EM01 PICTURE X[27]  VALUE :01.1ST REC IPMAST NOT SCHL.:. A2300366
014570     03  EM02 PICTURE X[27]  VALUE :02.SAVE IP CALL PROG DEPT .:. A2300367
014610     03  EM03 PICTURE X[27]  VALUE :03.END OF COACH AP CORRECT.:. A2300368
014650     02  ERR-MSGS REDEFINES ERROR-MSGS.                           A2300369
014690         03  MSG     PICTURE X[27] OCCURS 03 TIMES.               A2300370
014730 01  CONSOLE-OUTPUT-AREA SYNC.                                    A2300371
014770     02  FILLER              PICTURE X[10]   VALUE :.A230S   .:.  A2300372
014810     02  CONSOLE-MSG         PICTURE X[60]   VALUE SPACES.        A2300373
014850 01  COMRG-INFO SYNC.                                             A2300374
014890     02  COM-VARDATE         PICTURE X[8]    VALUE SPACES.        A2300375
014930     02  COM-DATE            PICTURE X[6]    VALUE SPACES.        A2300376
014970     02  COM-JULDAY          PICTURE X[3]    VALUE SPACES.        A2300377
015010     02  COM-PROGNAME        PICTURE X[8]    VALUE SPACES.        A2300378
015050     02  COM-PREPASS         PICTURE X[11]   VALUE SPACES.        A2300379
015090     02  COM-UPSI            PICTURE X[8]    VALUE SPACES.        A2300380
015130     02  COM-TIME            PICTURE X[8]    VALUE SPACES.        A2300381
015170     02  FILLER              PICTURE X[4]    VALUE SPACES.        A2300382
015210 PROCEDURE DIVISION.                                              A2300383
015250 HSK.                                                             A2300384
015290     OPEN INPUT  INPUT-MASTER-FILE                                A2300385
015330         OUTPUT OUTPUT-MASTER-FILE.                               A2300386
015370     MOVE SPACES TO PRINTER,                                      A2300387
015371         SCHOOL-MASTER-WORK  TEACHER-MASTER-WORK.                 A2300388
015450     MOVE SPACES TO DETAIL-LINE.                                  A2300389
015490 GET-DATE.                                                        A2300390
015530     CALL :GETCOMRG: USING COMRG-INFO.                            A2300391
015570     MOVE COM-VARDATE TO RPT-DATE.                                A2300392
015610 HEADING-ROUTINE.                                                 A2300393
015650     ADD 1 TO PAGE-CTR.                                           A2300394
015690     MOVE PAGE-CTR TO PAGE-EDIT.                                  A2300395
015730     MOVE HDR1 TO PRINTER.                                        A2300396
015770     MOVE :1: TO CC.                                              A2300397
015810     PERFORM W-REC.                                               A2300398
015850     MOVE HDR2 TO PRINTER.                                        A2300399
015890     MOVE :0: TO CC.                                              A2300400
015930     PERFORM W-REC.                                               A2300401
015970     MOVE :0: TO CC.                                              A2300402
016010     PERFORM W-REC.                                               A2300403
016050     MOVE ZEROS TO LINE-COUNT.                                    A2300404
016090 READ-MASTER.                                                     A2300405
016130     READ INPUT-MASTER-FILE AT END GO TO EOJ.                     A2300406
016170     ADD 1 TO RECORDS-IN.                                         A2300407
016210 START-WITH-SCHOOL-RECORD.                                        A2300408
016250     IF FIRST-RECORD IS EQUAL TO :Y: AND NOT SCH-RECORD           A2300409
016290         MOVE 01 TO ERR-INC  PERFORM ERR-LOOKUP                   A2300410
016330         MOVE 02 TO ERR-INC GO TO ERR-LOOKUP.                     A2300411
016370     MOVE :N: TO FIRST-RECORD.                                    A2300412
016410 TST-IF-SCHOOL.                                                   A2300413
016450     IF SCH-RECORD                                                A2300414
016490         MOVE INPUT-SCHOOL-MASTER TO SCHOOL-MASTER-WORK           A2300415
016530         GO TO WRITE-SCHOOL-RECORD.                               A2300416
016570 MOVE-TCH-TO-WORKAREA.                                            A2300417
016610     MOVE INPUT-TEACHER-MASTER TO TEACHER-MASTER-WORK.            A2300418
016620     IF TYPE-MASTER IS EQUAL TO :A: NEXT SENTENCE ELSE            A2300419
016630         GO TO WRITE-TEACHER-RECORD.                              A2300420
016650 SELECT-COACH.                                                    A2300421
016660     IF EDITION OF TEACHER-MASTER-WORK IS > :34: NEXT SENTENCE    A2300422
016661         ELSE GO TO SELECT-FORECAST.                              A2300423
016810     IF TYPESCH IS > :6E: OR                                      A2300424
016850        TYPESCH IS > :8E: OR                                      A2300425
016890        TYPESCH IS > :CE: NEXT SENTENCE ELSE                      A2300426
016930         GO TO CONT-CHK-SCHL-AP.                                  A2300427
016970     IF GRADE-AP IS > :8Q:                                        A2300428
017010         GO TO WRITE-TEACHER-RECORD ELSE                          A2300429
017050         MOVE :8Q: TO GRADE-AP GO TO SET-UP-PRINTER.              A2300430
017090 CONT-CHK-SCHL-AP.                                                A2300431
017130     IF TYPESCH IS > : S: NEXT SENTENCE ELSE                      A2300432
017170         GO TO TST-J3Q.                                           A2300433
017210     IF GRADE-AP IS > :1Q:   GO TO WRITE-TEACHER-RECORD.          A2300434
017250     MOVE :1Q: TO GRADE-AP GO TO SET-UP-PRINTER.                  A2300435
017290 TST-J3Q.                                                         A2300436
017330     IF TYPESCH IS > : J: NEXT SENTENCE ELSE                      A2300437
017370         GO TO TST-JS2Q.                                          A2300438
017410     IF GRADE-AP IS > :3Q: GO TO WRITE-TEACHER-RECORD.            A2300439
017450     MOVE :3Q: TO GRADE-AP GO TO SET-UP-PRINTER.                  A2300440
017490 TST-JS2Q.                                                        A2300441
017530     IF TYPESCH IS > :JS: NEXT SENTENCE ELSE                      A2300442
017570         GO TO TST-E8Q.                                           A2300443
017610     IF GRADE-AP IS > :2Q: GO TO WRITE-TEACHER-RECORD.            A2300444
017650     MOVE :2Q: TO GRADE-AP GO TO SET-UP-PRINTER.                  A2300445
017690 TST-E8Q.                                                         A2300446
017730     IF TYPESCH IS > : E: NEXT SENTENCE ELSE                      A2300447
017770         GO TO TST-C4Q.                                           A2300448
017810     IF GRADE-AP IS > :8Q: GO TO WRITE-TEACHER-RECORD.            A2300449
017850     MOVE :8Q: TO GRADE-AP GO TO SET-UP-PRINTER.                  A2300450
017890 TST-C4Q.                                                         A2300451
017930     IF TYPESCH IS > : C: NEXT SENTENCE ELSE                      A2300452
017970         GO TO TST-JCBQ.                                          A2300453
018010     IF GRADE-AP IS > :4Q: GO TO WRITE-TEACHER-RECORD.            A2300454
018050     MOVE :4Q: TO GRADE-AP GO TO SET-UP-PRINTER.                  A2300455
018090 TST-JCBQ.                                                        A2300456
018130     IF TYPESCH IS > :JC: NEXT SENTENCE ELSE                      A2300457
018170         GO TO TST-CLAQ.                                          A2300458
018210     IF GRADE-AP IS > :BQ: GO TO WRITE-TEACHER-RECORD.            A2300459
018250     MOVE :BQ: TO GRADE-AP GO TO SET-UP-PRINTER.                  A2300460
018290 TST-CLAQ.                                                        A2300461
018330     IF TYPESCH IS > :CL: NEXT SENTENCE ELSE                      A2300462
018370         GO TO WRITE-TEACHER-RECORD.                              A2300463
018410     IF  GRADE-AP IS > :AQ: GO TO WRITE-TEACHER-RECORD.           A2300464
018450     MOVE :AQ: TO GRADE-AP.                                       A2300465
018454 SELECT-FORECAST.                                                 A2300466
018455     IF EDITION OF TEACHER-MASTER-WORK IS EQUAL TO :33:           A2300467
018456         NEXT SENTENCE ELSE GO TO WRITE-TEACHER-RECORD.           A2300468
018461     EXAMINE STUD-BLANK REPLACING ALL : : BY :0:.                 A2300469
018462     EXAMINE TCH-BLANK  REPLACING ALL : : BY :0:.                 A2300470
018463     EXAMINE REC-BLANK  REPLACING ALL : : BY :0:.                 A2300471
018464     COMPUTE SINGLES-CTR > STUD-COPY < PAID-TCH < RECS.           A2300472
018466     IF SINGLES-CTR IS EQUAL TO 1                                 A2300473
018467             MOVE :1A: TO GRADE-AP                                A2300474
018468             GO TO SET-UP-PRINTER.                                A2300475
018469     GO TO WRITE-TEACHER-RECORD.                                  A2300476
018490 SET-UP-PRINTER.                                                  A2300477
018530     IF LINE-COUNT IS \ 56 PERFORM HEADING-ROUTINE.               A2300478
018570     MOVE ZIP-CODE OF TEACHER-MASTER-WORK TO ZIPP.                A2300479
018610     MOVE SCH-NUMBER OF TEACHER-MASTER-WORK TO SCHP.              A2300480
018650     MOVE EDITION OF TEACHER-MASTER-WORK TO EDP.                  A2300481
018690     MOVE TCH-NUMBER OF TEACHER-MASTER-WORK TO TCHP.              A2300482
018730     MOVE TERM TO TERMP.                                          A2300483
018770     MOVE FIRST-YR-TERM TO 1SEMP.                                 A2300484
018810     MOVE SECOND-SEM TO 2SEMP.                                    A2300485
018850     MOVE AMOUNT-PAID TO AMT-PDP.                                 A2300486
018890     MOVE BALANCE-DUE TO BAL-DUEP.                                A2300487
018930     MOVE STUD-COPY TO S-COPYP.                                   A2300488
018970     MOVE PAID-TCH TO P-TCHP.                                     A2300489
019010     MOVE TYPE-MASTER TO TYPEP.                                   A2300490
019050     MOVE GRADE-AP OF TEACHER-MASTER-WORK TO FILLERCOM.           A2300491
019090     ADD 1 TO TOTAL-ADJUSTED-RECORDS.                             A2300492
019130     MOVE DETAIL-LINE TO PRINTER.                                 A2300493
019170     MOVE SPACES TO DETAIL-LINE.                                  A2300494
019210     MOVE : : TO CC.                                              A2300495
019250 W-REC.                                                           A2300496
019290     CALL :PRTSPOOL: USING PRINTER.                               A2300497
019330     ADD 1 TO LINE-COUNT.                                         A2300498
019370     MOVE SPACES TO DATAREA.                                      A2300499
019410 WRITE-TEACHER-RECORD.                                            A2300500
019450     WRITE OUTPUT-TEACHER-MASTER FROM TEACHER-MASTER-WORK.        A2300501
019490     MOVE SPACES TO TEACHER-MASTER-WORK.                          A2300502
019500     MOVE ZEROS TO SINGLES-CTR.                                   A2300503
019530     ADD 1 TO RECORDS-OUT.                                        A2300504
019570     ADD 1 TO TEACHER-CTR.                                        A2300505
019610     GO TO READ-MASTER.                                           A2300506
019650 WRITE-SCHOOL-RECORD.                                             A2300507
019690     WRITE OUTPUT-SCHOOL-MASTER FROM SCHOOL-MASTER-WORK.          A2300508
019730     MOVE SPACES TO SCHOOL-MASTER-WORK.                           A2300509
019770     ADD 1 TO RECORDS-OUT.                                        A2300510
019810     ADD 1 TO SCHOOL-CTR.                                         A2300511
019850     GO TO READ-MASTER.                                           A2300512
019890 EOJ.                                                             A2300513
019930     PERFORM HEADING-ROUTINE.                                     A2300514
019970     MOVE SPACES TO TOTALS-LINE.                                  A2300515
020010     MOVE :RECORDS IN: TO TOTAL-LINE-ID.                          A2300516
020050     MOVE RECORDS-IN TO TOTAL-LINE-COUNT.                         A2300517
020090     MOVE TOTALS-LINE TO PRINTER.                                 A2300518
020130     MOVE :0: TO CC.                                              A2300519
020170     PERFORM W-REC.                                               A2300520
020210     MOVE :RECORDS OUT: TO TOTAL-LINE-ID.                         A2300521
020250     MOVE RECORDS-OUT TO TOTAL-LINE-COUNT.                        A2300522
020290     MOVE TOTALS-LINE TO PRINTER.                                 A2300523
020330     PERFORM W-REC.                                               A2300524
020370     MOVE :SCHOOLS OUT:  TO TOTAL-LINE-ID                         A2300525
020410     MOVE SCHOOL-CTR TO TOTAL-LINE-COUNT.                         A2300526
020450     MOVE TOTALS-LINE TO PRINTER.                                 A2300527
020490     PERFORM W-REC.                                               A2300528
020530     MOVE :TEACHERS OUT: TO TOTAL-LINE-ID.                        A2300529
020570     MOVE TEACHER-CTR TO TOTAL-LINE-COUNT.                        A2300530
020610     MOVE TOTALS-LINE TO PRINTER.                                 A2300531
020650     PERFORM W-REC.                                               A2300532
020690     MOVE :TOTAL ADJUSTED RECORDS: TO TOTAL-LINE-ID.              A2300533
020730     MOVE TOTAL-ADJUSTED-RECORDS TO TOTAL-LINE-COUNT.             A2300534
020770     MOVE TOTALS-LINE TO PRINTER.                                 A2300535
020810     PERFORM W-REC.                                               A2300536
020850 CLOSE-FILES.                                                     A2300537
020890     CLOSE  INPUT-MASTER-FILE, OUTPUT-MASTER-FILE.                A2300538
020930     MOVE 03 TO ERR-INC PERFORM ERR-LOOKUP.                       A2300539
020970     CALL :UNITCLOS:.                                             A2300540
021010     STOP RUN.                                                    A2300541
021050 ERR-LOOKUP.                                                      A2300542
021090     MOVE MSG [ERR-INC] TO CONSOLE-MSG.                           A2300543
021130     CALL :CONSPOOL: USING CONSOLE-OUTPUT-AREA.                   A2300544
021170 ABEND.                                                           A2300545
021210     CALL :DUMPSTOP:.                                             A2300546
021250 THATS-ALL-FOLKS.                                                 A2300547
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        }Cz