000010 IDENTIFICATION DIVISION.                                         A9990000
000090     P999.                                                        A9990002
000130 AUTHOR.                                                          A9990003
000170     SUSAN GERBER.                                                A9990004
000210 DATE-WRITTEN.                                                    A9990005
000250     FEBRUARY 1969.                                               A9990006
000260 DATE-COMPILED.  02/07/73.                                        A9990007
000290 REMARKS.                                                         A9990008
000330     THE PURPOSE OF THIS PROGRAM IS TO DROP OR PASS SCHOOL AND    A9990009
000370     TEACHER RECORDS WITH MODIFICATION OF FIELDS, AND WRITE       A9990010
000410     SELECTED SCHOOLS AND TEACHERS TO A DP TAPE.                  A9990011
000420     CONVERTED TO ANS BY M. ORBAN  02/07/73.                      A9990012
000450 ENVIRONMENT DIVISION.                                            A9990013
000490 CONFIGURATION SECTION.                                           A9990014
000530 SOURCE-COMPUTER.                                                 A9990015
000570     COPY CONFIG.                                                 A9990016
000610 OBJECT-COMPUTER.                                                 A9990017
000650     COPY CONFIG2.                                                A9990018
000690 INPUT-OUTPUT SECTION.                                            A9990019
000730 FILE-CONTROL.                                                    A9990020
000770                                                                  A9990021
000810     SELECT MAG-MASTER-FILE                                       A9990022
000850         ASSIGN TO SYS010-UT-2400-S.                              A9990023
000890     SELECT PURGED-FILE                                           A9990024
000930         ASSIGN TO SYS011-UT-2400-S.                              A9990025
000970     SELECT DP-FILE                                               A9990026
001010         ASSIGN TO SYS012-UT-2400-S.                              A9990027
001130 DATA DIVISION.                                                   A9990028
001170 FILE SECTION.                                                    A9990029
001210 FD  MAG-MASTER-FILE                                              A9990030
001250     BLOCK CONTAINS 1504 CHARACTERS                               A9990031
001290     LABEL RECORDS ARE STANDARD                                   A9990032
001330     DATA RECORDS ARE TEACHER-REC, SCHOOL-REC RECORDING V.        A9990033
001370 01  TEACHER-REC SYNC.                                            A9990034
001410     02  ID-NUMB-T.                                               A9990035
001450         03  ZIP-CODE-XT PICTURE X[5].                            A9990036
001490         03  SCH-NUMB-T  PICTURE XXX.                             A9990037
001530         03  ED-T        PICTURE XX.                              A9990038
001570         03  TCH-NUMB-T  PIC S999.                                A9990039
001610         03  CK-DIGIT-T  PICTURE X.                               A9990040
001650     02  CURR-COPY-X.                                             A9990041
001690         03  STUD-CP-X   PICTURE X[4].                            A9990042
001730         03  PD-TCH-CP-X PICTURE XX.                              A9990043
001770         03  RECS-X      PICTURE XX.                              A9990044
001810         03  FILM-X      PICTURE XX.                              A9990045
001850         03  XTC-X       PICTURE XX.                              A9990046
001890     02  CURR-COPY REDEFINES CURR-COPY-X.                         A9990047
001930         03  STUD-CP     PICTURE 9[4].                            A9990048
001970         03  PD-TCH-CP   PICTURE 99.                              A9990049
002010         03  RECS        PICTURE 99.                              A9990050
002050         03  FILM        PICTURE 99.                              A9990051
002090         03  XTC         PICTURE 99.                              A9990052
002130     02  AMT-PD          PICTURE S9[4]V99 COMPUTATIONAL-3.        A9990053
002170     02  TERM-X          PICTURE X.                               A9990054
002210     02  TERM REDEFINES TERM-X PICTURE 9.                         A9990055
002250     02  GRADE-AP        PICTURE XX.                              A9990056
002290     02  TCH-NAME        PICTURE X[25].                           A9990057
002330     02  PROMO-KEY       PICTURE X[5].                            A9990058
002370     02  BILL-TO                 PICTURE X.                       A9990059
002410     02  BOARD-NUMB      PICTURE X[5].                            A9990060
002450     02  ISSUE-DATES-X.                                           A9990061
002490         03  B-DATE-X.                                            A9990062
002530             04  B-ISS-X PICTURE XX.                              A9990063
002570             04  B-YR-X  PICTURE X.                               A9990064
002610         03  EX-DATE-X.                                           A9990065
002650             04  EX-ISS-X PICTURE XX.                             A9990066
002690             04  EX-YR-X PICTURE X.                               A9990067
002730     02  ISSUE-DATES REDEFINES ISSUE-DATES-X.                     A9990068
002770         03  B-DATE.                                              A9990069
002810             04  B-ISS   PICTURE 99.                              A9990070
002850             04  B-YR    PICTURE 9.                               A9990071
002890         03  EXP-DATE.                                            A9990072
002930             04  EX-ISS  PICTURE 99.                              A9990073
002970             04  EX-YR   PICTURE 9.                               A9990074
003010     02  CHARGES         COMPUTATIONAL-3.                         A9990075
003050         03  FST-YR-TERM PICTURE S9[4]V99.                        A9990076
003090         03  SEC-SEM     PICTURE S9[4]V99.                        A9990077
003130         03  POSTAL      PICTURE S999V99.                         A9990078
003170         03  OTHER       PICTURE S999V99.                         A9990079
003210         03  BAL-DUE     PICTURE S9[4]V99.                        A9990080
003250     02  REVISION-1      PICTURE X[13].                           A9990081
003290     02  REVISION-2      PICTURE X[13].                           A9990082
003330     02  REVISION-3      PICTURE X[13].                           A9990083
003370     02  TYPE-MASTER     PICTURE X.                               A9990084
003410     02  MAST-HOLD-CD    PICTURE X.                               A9990085
003450     02  LAST-SEMES-X.                                            A9990086
003490         03  L-STUD-CP-X PICTURE X[4].                            A9990087
003530         03  L-TCH-CP-X  PICTURE XX.                              A9990088
003570         03  L-XTC-X     PICTURE XX.                              A9990089
003610         03  L-REC-X     PICTURE XX.                              A9990090
003650     02  LAST-SEMES REDEFINES LAST-SEMES-X.                       A9990091
003690         03  L-STUD-CP   PICTURE 9[4].                            A9990092
003730         03  L-TCH-CP    PICTURE 99.                              A9990093
003770         03  L-XTC       PICTURE 99.                              A9990094
003810         03  L-REC       PICTURE 99.                              A9990095
003850     02  TRANS-INFO.                                              A9990096
003890         03  TR-INFO-1   PICTURE X.                               A9990097
003930         03  BATCH-NO    PICTURE X[6].                            A9990098
003970         03  TR-INFO-2   PICTURE XX.                              A9990099
004010     02  FILLER-1        PICTURE X[11].                           A9990100
004050 01  SCHOOL-REC SYNC.                                             A9990101
004090     02  ID-NUMB-S.                                               A9990102
004130         03  ZIP-CODE-XS PICTURE X[5].                            A9990103
004170         03  ZIP-CODE-S REDEFINES ZIP-CODE-XS PICTURE 9[5].       A9990104
004210         03  SCH-NUMB-S  PICTURE 999.                             A9990105
004250         03  ED-S        PICTURE XX.                              A9990106
004290         03  TCH-NUMB-S  PICTURE XXX.                             A9990107
004330         03  CK-DIGIT-S  PICTURE X.                               A9990108
004370     02  SCH-INFO1       PICTURE X[70].                           A9990109
004410     02  NEW-ID          PICTURE X[8].                            A9990110
004450     02  SCH-INFO2       PICTURE XX.                              A9990111
004490     02  SCH-TYPE        PICTURE XX.                              A9990112
004530     02  FILLER              PICTURE X[5].                        A9990113
004570     02  BATCH-NUMBER        PICTURE X[6].                        A9990114
004610     02  FILLER              PICTURE XX.                          A9990115
004650 FD  PURGED-FILE                                                  A9990116
004690     BLOCK CONTAINS 1404 CHARACTERS                               A9990117
004730     LABEL RECORDS ARE STANDARD                                   A9990118
004770     DATA RECORDS ARE P-TEACHER-REC, P-SCHOOL-REC RECORDING V.    A9990119
004810 01  P-TEACHER-REC SYNC.                                          A9990120
004850     02  P-ID-NUMB-T.                                             A9990121
004890         03  P-ZIP-CODE-T PICTURE X[5].                           A9990122
004930         03  REST-ID-T.                                           A9990123
004970             04  P-SCH-T         PICTURE XXX.                     A9990124
005010             04  P-ED-T          PICTURE XX.                      A9990125
005050             04  P-TCH-T     PIC 999.                             A9990126
005090             04  P-CHKDIG-T      PICTURE X.                       A9990127
005130     02  P-CURR-CP.                                               A9990128
005170         03  P-STUD-CP   PICTURE 9[4].                            A9990129
005210         03  P-TCH-CP    PICTURE 99.                              A9990130
005250         03  P-RECS      PICTURE 99.                              A9990131
005290         03  P-FILM      PICTURE 99.                              A9990132
005330         03  P-XTC       PICTURE 99.                              A9990133
005370     02  P-AMT-PD        PICTURE S9[4]V99 COMPUTATIONAL-3.        A9990134
005410     02  P-TERM          PICTURE 9.                               A9990135
005450     02  P-GRADE-AP      PICTURE XX.                              A9990136
005490     02  P-TCH-NAME      PICTURE X[25].                           A9990137
005530     02  P-PROMO-KEY     PICTURE X[5].                            A9990138
005570     02  P-BILL-TO               PICTURE X.                       A9990139
005610     02  P-BOARD-NUMB    PICTURE X[5].                            A9990140
005650     02  P-ISSUE-DATES.                                           A9990141
005690         03  PISS-START          PICTURE XX.                      A9990142
005730         03  PISS-START-YR       PICTURE X.                       A9990143
005770         03  PISS-STOP           PICTURE XX.                      A9990144
005810         03  PISS-STOP-YR        PICTURE X.                       A9990145
005850     02  P-CHARGES       COMPUTATIONAL-3.                         A9990146
005890                                                                  A9990147
005930         03  P-FST-YR    PICTURE S9[4]V99.                        A9990148
005970         03  P-SEC-SEM   PICTURE S9[4]V99.                        A9990149
006010         03  P-POSTAL    PICTURE S999V99.                         A9990150
006050         03  P-OTHER     PICTURE S999V99.                         A9990151
006090         03  P-BAL-DUE   PICTURE S9[4]V99.                        A9990152
006130     02  P-REVISION-1    PICTURE X[13].                           A9990153
006170     02  P-REVISION-2    PICTURE X[13].                           A9990154
006210     02  P-REVISION-3    PICTURE X[13].                           A9990155
006250     02  P-TYPE-MASTER   PICTURE X.                               A9990156
006290     02  P-MAST-HOLD-CD  PICTURE X.                               A9990157
006330     02  P-LAST-SEMES.                                            A9990158
006370         03  P-L-ST-CP   PICTURE 9[4].                            A9990159
006410         03  P-L-TCH-CP  PICTURE 99.                              A9990160
006450         03  P-L-XTC     PICTURE 99.                              A9990161
006490         03  P-L-REC     PICTURE 99.                              A9990162
006530     02  P-TRANS-INFO.                                            A9990163
006570         03  P-TR-INFO-1 PICTURE X.                               A9990164
006610         03  P-BATCH-NO  PICTURE X[6].                            A9990165
006650         03  P-TR-INFO-2 PICTURE XX.                              A9990166
006690     02  P-SOURCE-CODE.                                           A9990167
006730         03  P-SOURCE1           PICTURE X[5].                    A9990168
006770         03  P-SOURCE2           PICTURE X.                       A9990169
006810         03  FILLER-2    PICTURE X[5].                            A9990170
006850 01  P-SCHOOL-REC SYNC.                                           A9990171
006890     02  P-ID-NUMB-S.                                             A9990172
006930         03  P-ZIP-CODE-S PICTURE X[5].                           A9990173
006970         03  REST-ID-S   PICTURE X[9].                            A9990174
007010     02  REST-SCH        PICTURE X[95].                           A9990175
007050 FD  DP-FILE                                                      A9990176
007090     BLOCK CONTAINS 1404 CHARACTERS                               A9990177
007130     LABEL RECORDS ARE STANDARD                                   A9990178
007170     DATA RECORDS ARE DP-TEACHER-REC, DP-SCHOOL-REC RECORDING V.  A9990179
007210 01  DP-TEACHER-REC SYNC.                                         A9990180
007250     02  DP-ID-NUMP-T    PICTURE X[14].                           A9990181
007290     02  DP-CURR-CP      PICTURE X[12].                           A9990182
007330     02  DP-AMT-PD       PICTURE S9[4]V99 COMPUTATIONAL-3.        A9990183
007370     02  DP-TERM         PICTURE X.                               A9990184
007410     02  DP-GRADE-AP     PICTURE XX.                              A9990185
007450     02  DP-TCH-NAME     PICTURE X[25].                           A9990186
007490     02  DP-PROMO-KEY    PICTURE X[5].                            A9990187
007530     02  DP-BILL-TO      PICTURE 9.                               A9990188
007570     02  DP-BOARD-NUMB   PICTURE X[5].                            A9990189
007610     02  DP-ISSUE-DATES  PICTURE X[6].                            A9990190
007650     02  DP-CHARGES      COMPUTATIONAL-3.                         A9990191
007690         03  DP-FST-YR-TM PICTURE S9[4]V99.                       A9990192
007730         03  DP-SEC-SEM  PICTURE S9[4]V99.                        A9990193
007770         03  DP-POSTAL   PICTURE S999V99.                         A9990194
007810         03  DP-OTHER    PICTURE S999V99.                         A9990195
007850         03  DP-BAL-DUE  PICTURE S9[4]V99.                        A9990196
007890     02  DP-REVISION-1   PICTURE X[13].                           A9990197
007930     02  DP-REVISION-2   PICTURE X[13].                           A9990198
007970     02  DP-REVISION-3   PICTURE X[13].                           A9990199
008010     02  DP-TYPE-MASTER  PICTURE X.                               A9990200
008050     02  DP-MAST-HOLD-CD PICTURE X.                               A9990201
008090     02  DP-L-SEMES      PICTURE X[10].                           A9990202
008130     02  DP-TRANS-INFO   PICTURE X[9].                            A9990203
008170     02  FILLER-3        PICTURE X[11].                           A9990204
008210 01  DP-SCHOOL-REC SYNC.                                          A9990205
008250     02  DP-ID-NUMB-S    PICTURE X[14].                           A9990206
008290     02  DP-REST-SCH     PICTURE X[95].                           A9990207
008610 WORKING-STORAGE SECTION.                                         A9990208
008611 77  MODULE-ID  PIC X[24]  VALUE :A999SANS 02/07/73 01****:.      A9990209
008650 77  AOWW-CTR    PICTURE S9[7]   COMPUTATIONAL-3  VALUE ZEROS.    A9990210
008690 77  SINGLES-COUNTER             PICTURE 9[4] VALUE ZEROS.        A9990211
008730 77  DROP-CK             PICTURE 9       VALUE ZEROS.             A9990212
008770 77  TIMES-THRU          PICTURE 9       VALUE ZEROS.             A9990213
008810 77  RECORDS-IN          PICTURE 9[8]    VALUE ZEROS.             A9990214
008850 77  RECORDS-OUT         PICTURE 9[8]    VALUE ZEROS.             A9990215
008890 77  RECORDS-DP          PICTURE 9[8]    VALUE ZEROS.             A9990216
008930 77  EDIT-ALL            PICTURE ZZ,ZZZ,ZZZ.                      A9990217
008970 77  END-IT              PICTURE 9       VALUE ZEROS.             A9990218
009010 01  SCH-REC-HOLD        PICTURE X[109]  VALUE SPACES SYNC.       A9990219
009050 01  ZIP-CODE-XS--Q000.                                           A9990220
009090 02  ZIP-CODE-XS--R000 PICTURE S9[5].                             A9990221
009090 01  PRINTLINE.                                                   A9990222
009091     02  CC      PIC X           VALUE SPACES.                    A9990223
009092     02  DATAREA PIC X[132]      VALUE SPACES.                    A9990224
00PROCEDURE DIVISION.                                              A9990225
009170 BEGIN-RUN.                                                       A9990226
009210     OPEN INPUT MAG-MASTER-FILE,                                  A9990227
009250         OUTPUT PURGED-FILE  DP-FILE.                             A9990228
009290     MOVE SPACES TO DP-TEACHER-REC DP-SCHOOL-REC  PRINTLINE       A9990229
009330         P-TEACHER-REC P-SCHOOL-REC.                              A9990230
009370                                                                  A9990231
009410                                                                  A9990232
009450 PROCESS-DATA.                                                    A9990233
009490     READ MAG-MASTER-FILE AT END GO TO CLOSE-FILES.               A9990234
009530     IF ED-T NOT EQUAL TO :  : GO TO PROCESS-DATA.                A9990235
009570     ADD 1 TO RECORDS-IN.                                         A9990236
009610 SCH-RTE.                                                         A9990237
009650     IF END-IT EQUAL TO 1 GO TO CL-FILES.                         A9990238
009690     MOVE 1 TO TIMES-THRU.                                        A9990239
009730     MOVE 0 TO DROP-CK.                                           A9990240
009770     MOVE ZIP-CODE-XS TO ZIP-CODE-XS--Q000 IF ZIP-CODE-XS--R000   A9990241
009810     NOT NUMERIC MOVE 1 TO DROP-CK.                               A9990242
009850     IF SCH-NUMB-S GREATER THAN 710 MOVE 1 TO DROP-CK.            A9990243
009890     IF NEW-ID NOT EQUAL TO SPACES MOVE 2 TO DROP-CK,             A9990244
009930         ADD 1 TO RECORDS-DP, GO TO TCH-READ.                     A9990245
009970     IF SCH-TYPE EQUAL TO :DP: MOVE 3 TO DROP-CK.                 A9990246
010010     GO TO HOLD-SCH-RTE.                                          A9990247
010050 TCH-READ.                                                        A9990248
010090     READ MAG-MASTER-FILE AT END GO TO CLOSE-FILES.               A9990249
010130     ADD 1 TO RECORDS-IN.                                         A9990250
010170     IF ED-T EQUAL TO :  : GO TO DROP-TEST.                       A9990251
010370     IF DROP-CK EQUAL TO 2 ADD 1 TO RECORDS-DP, GO TO TCH-READ.   A9990252
010410     IF DROP-CK EQUAL TO 1 GO TO TCH-RTE.                         A9990253
010450     IF TIMES-THRU EQUAL TO 1 GO TO SCH-TEST.                     A9990254
010490     IF DROP-CK EQUAL TO 3 GO TO DP-TEACHER.                      A9990255
010530                                                                  A9990256
010570 TCH-RTE.                                                         A9990257
010610     IF TYPE-MASTER EQUAL TO :K: ADD 1 TO RECORDS-DP,             A9990258
010650         GO TO TCH-READ.                                          A9990259
010690     IF TYPE-MASTER EQUAL TO :C:                                  A9990260
010730         ADD 1 TO RECORDS-DP, GO TO TCH-READ.                     A9990261
010770     IF DROP-CK EQUAL TO 1 AND TIMES-THRU EQUAL TO 1              A9990262
010810         GO TO SCH-TEST.                                          A9990263
010850     EXAMINE ISSUE-DATES-X REPLACING ALL : : BY :0:.              A9990264
010890*    THIS IS A YEARLY CHECK WHICH MUST BE CHANGED YEARLY.         A9990265
010891*  * * * * * * * * * * * * * * * * * * * * * * * * * ** * * * * * A9990266
010890     IF TYPE-MASTER EQUAL TO :A: AND EX-YR EQUAL TO 3 MOVE :E:    A9990267
010930         TO P-TYPE-MASTER ELSE MOVE TYPE-MASTER TO P-TYPE-MASTER. A9990268
010970         MOVE ZEROS TO P-AMT-PD, P-FST-YR, P-SEC-SEM,             A9990269
011010         P-POSTAL, P-OTHER, P-BAL-DUE.                            A9990270
011050     NOTE  PREVIOUS TEST DETERMINES IF ACTIVE IN CURRENT YEAR     A9990271
011090     OR A CARRY OVER RECORD IN ORDER TO SET UP OUTPUT RECORD      A9990272
011130     INFO ACCORDINGLY                                             A9990273
011170     CURRENT YEAR DIGIT TEST MUST REFLECT CURRENT YEAR.           A9990274
011210     EXAMINE TERM-X REPLACING ALL : : BY :0:.                     A9990275
011250     EXAMINE CURR-COPY-X REPLACING ALL : : BY :0:.                A9990276
011290     EXAMINE LAST-SEMES-X REPLACING ALL : : BY :0:.               A9990277
011330     IF P-TYPE-MASTER NOT EQUAL TO :E: GO TO REG-TCH-MOVES.       A9990278
011370     NOTE THE PREVIOUS TEST CHECKS FOR EXPIRES AND IF EXPIRE      A9990279
011410         RECORD THE FOLLOWING STATEMENTS MODIFY THE VARIOUS       A9990280
011450         SELECTED FIELDS AND OMITS THE REG-TCH-MOVES ROUTINE.     A9990281
011460*    CURRENT YEAR EXPIRE LOGIC ROUTINE  EXPIRES ONLY              A9990282
011490     MOVE STUD-CP TO P-L-ST-CP.                                   A9990283
011530     MOVE PD-TCH-CP TO P-L-TCH-CP.                                A9990284
011570     MOVE RECS TO P-L-REC                                         A9990285
011610     MOVE XTC TO P-L-XTC.                                         A9990286
011650     MOVE TERM TO P-TERM.                                         A9990287
011690     IF TERM EQUAL TO 7 MOVE 1 TO P-TERM.                         A9990288
011730     IF TERM IS EQUAL TO 2  MOVE 3 TO P-TERM.                     A9990289
011930     MOVE SPACES TO P-REVISION-1                                  A9990290
011970         P-TRANS-INFO, P-REVISION-2, P-REVISION-3.                A9990291
012010     MOVE ZEROS TO SINGLES-COUNTER.                               A9990292
012050     ADD STUD-CP, PD-TCH-CP GIVING SINGLES-COUNTER.               A9990293
012090     IF TERM EQUAL TO 4 OR [[ED-T EQUAL TO :32: OR ED-T EQUAL TO  A9990294
012130     :33:]  AND SINGLES-COUNTER EQUAL TO 0001]  MOVE ISSUE-DATES  A9990295
012170      TO P-ISSUE-DATES,                                           A9990296
012210         ELSE MOVE :003003: TO P-ISSUE-DATES.                     A9990297
012250     GO TO GENERAL-TCH-MOVES.                                     A9990298
012290 REG-TCH-MOVES.                                                   A9990299
012291*  CARRYOVER LOGIC ROUTINES  *************    *****  ****  ****   A9990300
012330     MOVE LAST-SEMES TO P-LAST-SEMES.                             A9990301
012370     MOVE TERM TO P-TERM.                                         A9990302
012450     MOVE REVISION-1 TO P-REVISION-1.                             A9990303
012490     MOVE REVISION-2 TO P-REVISION-2.                             A9990304
012530     MOVE REVISION-3 TO P-REVISION-3.                             A9990305
012570     MOVE TRANS-INFO TO P-TRANS-INFO.                             A9990306
012610     MOVE SPACES TO TR-INFO-2.                                    A9990307
012650     MOVE ISSUE-DATES TO P-ISSUE-DATES.                           A9990308
012690                                                                  A9990309
012730 GENERAL-TCH-MOVES.                                               A9990310
013010     MOVE SPACES TO P-BOARD-NUMB.                                 A9990311
013050     MOVE ID-NUMB-T TO P-ID-NUMB-T.                               A9990312
013090     MOVE CURR-COPY TO P-CURR-CP.                                 A9990313
013100     MOVE GRADE-AP TO P-GRADE-AP.                                 A9990314
013130     MOVE TCH-NAME TO P-TCH-NAME.                                 A9990315
013170     MOVE BILL-TO TO P-BILL-TO.                                   A9990316
013210     MOVE MAST-HOLD-CD TO P-MAST-HOLD-CD.                         A9990317
013250     MOVE SPACES TO P-PROMO-KEY.                                  A9990318
013290     MOVE FILLER-1 TO P-SOURCE-CODE.                              A9990319
014130     WRITE P-TEACHER-REC.                                         A9990320
014170     ADD 1 TO RECORDS-OUT.                                        A9990321
014210     MOVE 0 TO DROP-CK.                                           A9990322
014250     GO TO TCH-READ.                                              A9990323
014290                                                                  A9990324
014330 HOLD-SCH-RTE.                                                    A9990325
014370     MOVE SPACES TO BATCH-NUMBER.                                 A9990326
014410     MOVE SCHOOL-REC TO SCH-REC-HOLD.                             A9990327
014450     GO TO TCH-READ.                                              A9990328
014490 SCH-TEST.                                                        A9990329
014530     MOVE 0 TO TIMES-THRU.                                        A9990330
014570     IF DROP-CK EQUAL TO 3 GO TO DP-SCHOOL.                       A9990331
014610     MOVE SCH-REC-HOLD TO P-SCHOOL-REC.                           A9990332
014650     WRITE P-SCHOOL-REC.                                          A9990333
014690     ADD 1 TO RECORDS-OUT.                                        A9990334
014730     MOVE SPACES TO P-SCHOOL-REC.                                 A9990335
014770     IF END-IT EQUAL TO 1 GO TO CL-FILES.                         A9990336
014810     IF ED-T EQUAL TO :  : GO TO SCH-RTE ELSE GO TO TCH-RTE.      A9990337
014850 DROP-TEST.                                                       A9990338
014890     IF TIMES-THRU EQUAL TO 0 OR DROP-CK EQUAL TO 2               A9990339
014930         GO TO SCH-RTE.                                           A9990340
014970     IF DROP-CK EQUAL TO 1                                        A9990341
015010         ADD 1 TO RECORDS-DP, GO TO SCH-RTE.                      A9990342
015050     GO TO SCH-TEST.                                              A9990343
015090 DP-SCHOOL.                                                       A9990344
015130     WRITE DP-SCHOOL-REC FROM SCH-REC-HOLD.                       A9990345
015170     ADD 1 TO RECORDS-DP.                                         A9990346
015210     MOVE SPACES TO DP-SCHOOL-REC.                                A9990347
015250     IF END-IT EQUAL TO 1 GO TO CL-FILES.                         A9990348
015290     IF ED-T EQUAL TO :  : GO TO SCH-RTE.                         A9990349
015330 DP-TEACHER.                                                      A9990350
015370     WRITE DP-TEACHER-REC FROM TEACHER-REC.                       A9990351
015410     ADD 1 TO RECORDS-DP.                                         A9990352
015450     GO TO TCH-READ.                                              A9990353
015930 CLOSE-FILES.                                                     A9990354
015970     MOVE 1 TO END-IT.                                            A9990355
016010     GO TO DROP-TEST.                                             A9990356
       W-REC.                                                           A9990357
           CALL :PRTSPOOL: USING PRINTLINE.                             A9990358
           MOVE SPACES TO PRINTLINE.                                    A9990359
016050 CL-FILES.                                                        A9990360
016090     MOVE RECORDS-IN TO EDIT-ALL.                                 A9990361
016130     MOVE :0: TO CC.                                              A9990362
016131     MOVE :TOTAL RECORDS IN : TO DATAREA.                         A9990363
016132     PERFORM W-REC.                                               A9990364
016133     MOVE EDIT-ALL TO DATAREA.                                    A9990365
016134     PERFORM W-REC.                                               A9990366
016170     MOVE RECORDS-OUT TO EDIT-ALL.                                A9990367
           MOVE :TOTAL PURGED FILE: TO DATAREA.                         A9990368
           PERFORM W-REC.                                               A9990369
           MOVE EDIT-ALL TO DATAREA.                                    A9990370
           PERFORM W-REC.                                               A9990371
016250     MOVE RECORDS-DP TO EDIT-ALL.                                 A9990372
           MOVE :TOTAL DP FILE : TO DATAREA.                            A9990373
           PERFORM W-REC.                                               A9990374
           MOVE EDIT-ALL TO DATAREA.                                    A9990375
           PERFORM W-REC.                                               A9990376
           CLOSE MAG-MASTER-FILE  PURGED-FILE  DP-FILE.                 A9990377
016450                                                                  A9990378
016490     CALL :UNITCLOS:.                                             A9990379
016530     STOP RUN.                                                    A9990380
                                                                                                                                                                                                        ! ­