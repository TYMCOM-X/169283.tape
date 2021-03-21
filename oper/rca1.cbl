       IDENTIFICATION DIVISION.                                         LIS00010
       PROGRAM-ID. :BR2035:.                                            LIS00020
       AUTHOR. C W WALKER.                                              LIS00030
       INSTALLATION. RCA RECORD DIVISION.                               LIS00040
       ENVIRONMENT DIVISION.                                            LIS00050
       CONFIGURATION SECTION.                                           LIS00060
       INPUT-OUTPUT SECTION.                                            LIS00070
       FILE-CONTROL.                                                    LIS00080
           SELECT CHANGES   ASSIGN TO UT-2311-S-SYS105.                 LIS00090
           SELECT TUNE-INV   ASSIGN TO DA-2301-R-SYS010                 LIS00100
                             ACCESS MODE IS RANDOM                      LIS00110
                             NOMINAL KEY IS TUNE-ACT-KEY.               LIS00120
           SELECT SELECTION-INV ASSIGN TO DA-2302-R-SYS020              LIS00130
                                ACCESS MODE IS RANDOM                   LIS00140
                                NOMINAL KEY IS SEL-ACT-KEY.             LIS00150
           SELECT TRANS-FILE    ASSIGN TO UT-2311-S-SYS102.             LIS00160
           SELECT LIST-NOTCE ASSIGN TO UT-2311-S-SYS104.                LIS00170
           SELECT INDEX-SRT ASSIGN TO UT-2311-S-SORTOUT.                LIS00180
           SELECT CONI-FILE ASSIGN TO UT-2311-S-SYS035.                 LIS00190
                                                                        LIS00200
       DATA DIVISION.                                                   LIS00210
       FILE SECTION.                                                    LIS00220
       FD  CONI-FILE                                                    LIS00230
           RECORDING MODE F                                             LIS00240
           LABEL RECORDS OMITTED                                        LIS00250
           DATA RECORD IS CONI-REC.                                     LIS00260
       01  CONI-REC.                                                    LIS00270
           05 TODAYS-DATE.                                              LIS00280
               07 T-MO    PICTURE XX.                                   LIS00290
               07 FILLER  PICTURE X.                                    LIS00300
               07 T-DA    PICTURE XX.                                   LIS00310
               07 FILLER  PICTURE X.                                    LIS00320
               07 T-YR    PICTURE XX.                                   LIS00330
           05 FILLER        PICTURE X[72].                              LIS00340
       FD  INDEX-SRT                                                    LIS00350
           BLOCK CONTAINS 100 RECORDS                                   LIS00360
           DATA RECORD IS INDEX-SRT-REC.                                LIS00370
           01  INDEX-SRT-REC.                                           LIS00380
               03 SERIAL-SORT     PICTURE X[8].                         LIS00390
               03 FILLER PICTURE X[11].                                 LIS00400
       FD  CHANGES RECORDING MODE F                                     LIS00410
           BLOCK CONTAINS 3 RECORDS                                     LIS00420
           LABEL RECORDS STANDARD                                       LIS00430
           DATA RECORD IS CHG-REC.                                      LIS00440
       01  CHG-REC.                                                     LIS00450
           03  SELCTN-NO-C     PICTURE  X[10].                          LIS00460
           03  TUNE-NO-C       PICTURE X[9].                            LIS00470
           03  SELCTN-TITLE-C  PICTURE  X[100].                         LIS00480
           03  ARTIST-NAME-C   PICTURE  X[60].                          LIS00490
           03  CURR-DATE-C     PICTURE  9[6].                           LIS00500
           03  REL-DATE-C      PICTURE  9[4].                           LIS00510
           03  C-SIDE      PICTURE XX.                                  LIS00520
           03  C-POST      PICTURE XX.                                  LIS00530
           03  C-MIN       PICTURE XX.                                  LIS00540
           03 C-SEC        PICTURE XX.                                  LIS00550
           03  REL-TYPE-C      PICTURE  X.                              LIS00560
           03  MAP-CODE-C         PICTURE X[4].                         LIS00570
           03  CHG-CODE-C      PICTURE  XX.                             LIS00580
           03  OLD-INFO-C.                                              LIS00590
             05 OLD-CTIL1        PICTURE X[40].                         LIS00600
             05 OLD-CTIL2        PICTURE X[40].                         LIS00610
             05 OLD-CTIL3        PICTURE X[40].                         LIS00620
           03 OLD-INFO-AGAIN REDEFINES OLD-INFO-C.                      LIS00630
             05 OLD-CAR1         PICTURE X[60].                         LIS00640
             05 OLD-CAR2         PICTURE X[60].                         LIS00650
           03  NEW-INFO-C.                                              LIS00660
             05 NEW-CTIL1        PICTURE X[40].                         LIS00670
             05 NEW-CTIL2        PICTURE X[40].                         LIS00680
             05 NEW-CTIL3        PICTURE X[40].                         LIS00690
           03  NEW-INFO-AGAIN REDEFINES NEW-INFO-C.                     LIS00700
             05 NEW-CART1        PICTURE X[60].                         LIS00710
             05 NEW-CART2        PICTURE X[60].                         LIS00720
                                                                        LIS00730
       FD  TUNE-INV RECORDING F                                         LIS00740
                    RECORD CONTAINS 3179 CHARACTERS                     LIS00750
                    LABEL RECORDS STANDARD                              LIS00760
                    DATA RECORDS ARE TUNE-REC, TYP1, TYP2.              LIS00770
                                                                        LIS00780
       01  TUNE-REC.                                                    LIS00790
           03 REC-ID-1          PICTURE XXX.                            LIS00800
           03 TUNE-DETAIL OCCURS 8 TIMES.                               LIS00810
              05 TUNE-SERIAL    PICTURE X[8].                           LIS00820
              05 SONG-NUMBER    PICTURE X[9].                           LIS00830
             05 SONG-FLAG      PICTURE X.                               LIS00840
              05 TUNE-TITLE     PICTURE X[40].                          LIS00850
              05 ARTIST-NAME    PICTURE X[60].                          LIS00860
              05 STAT           PICTURE X.                              LIS00870
              05 DATE-AREA.                                             LIS00880
                 07 DATE-REC OCCURS 5 TIMES                             LIS00890
                                PICTURE 9[6] COMPUTATIONAL-3.           LIS00900
              05 TAKE           PICTURE XX.                             LIS00910
              05 R-TIME         PICTURE 9[4].                           LIS00920
              05 A-R-PROD       PICTURE X[20].                          LIS00930
              05 JOB-NO         PICTURE X[8].                           LIS00940
              05 CREDITS.                                               LIS00950
                 07 T-CHAR OCCURS 200 TIMES PICTURE X.                  LIS00960
              05 CONT-POINT     PICTURE 9[4].                           LIS00970
              05 SELCTN-POINT OCCURS 5 TIMES.                           LIS00980
                 07 SELP-REC    PICTURE 9[3].                           LIS00990
                 07 SELP-DET    PICTURE 9.                              LIS01000
                                                                        LIS01010
       01  TYP1.                                                        LIS01020
           03 FILLER            PICTURE XXX.                            LIS01030
           03 T-INDX            PICTURE X[2850].                        LIS01040
           03 T-INDX-DUM REDEFINES T-INDX.                              LIS01050
              05 T-INDX-DETAIL OCCURS 150 TIMES.                        LIS01060
                 07 T-I-SERIAL  PICTURE X[8].                           LIS01070
                 07 FILLER      PICTURE X[7].                           LIS01080
                07 T-I-POINT  PICTURE 9[4].                             LIS01090
           03 FILLER            PICTURE X[326].                         LIS01100
                                                                        LIS01110
       01  TYP2.                                                        LIS01120
           02 FILLER            PICTURE XXX.                            LIS01130
           02 M-DETAIL OCCURS 8 TIMES.                                  LIS01140
           03 MSTR-SERIAL       PICTURE X[8].                           LIS01150
           03 FILLER            PICTURE X[2].                           LIS01160
           03 MSTR-SELCTN       PICTURE X[10].                          LIS01170
           03 MSTR-SIDE         PICTURE X[2].                           LIS01180
           03 MSTR-TUNES OCCURS 15 TIMES.                               LIS01190
              05 MSTR-TUNE-SER  PICTURE X[8].                           LIS01200
              05 FILLER         PICTURE X[2].                           LIS01210
              05 MSTR-BAND      PICTURE 99.                             LIS01220
              05 MSTR-ART       PICTURE X[2].                           LIS01230
           03 MSTR-IND          PICTURE X.                              LIS01240
           03 FILLER            PICTURE X[164].                         LIS01250
                                                                        LIS01260
       FD  SELECTION-INV RECORDING F                                    LIS01270
                         RECORD CONTAINS 4714 CHARACTERS                LIS01280
                         LABEL RECORDS STANDARD                         LIS01290
                         DATA RECORDS ARE SEL-REC, IND1.                LIS01300
                                                                        LIS01310
       01  SEL-REC.                                                     LIS01320
           03 REC-ID-2          PICTURE X[3].                           LIS01330
           03 REC-DETAIL OCCURS 7 TIMES.                                LIS01340
            04 BLOCK-1.                                                 LIS01350
              05 SELCTN-NO      PICTURE X[10].                          LIS01360
              05 SOURCE-SEL     PICTURE X[10].                          LIS01370
              05 SELCTN-TITLE.                                          LIS01380
                 07 TITLE1      PICTURE X[60].                          LIS01390
                 07 TITLE2      PICTURE X[40].                          LIS01400
             05 NEW-SEL-FLAG  PICTURE X.                                LIS01410
             05 NO-BDS   PICTURE 9[4] COMPUTATIONAL-3.                  LIS01420
             05 SINGL-FADE-AREA REDEFINES NO-BDS.                       LIS01430
               07 SINGL-FADE OCCURS 2 TIMES PICTURE X.                  LIS01440
               07 FILLER PICTURE X.                                     LIS01450
             05 PROJ-REL-DATE PICTURE 9[4].                             LIS01460
              05 ART-NAME       PICTURE X[60].                          LIS01470
              05 REL-DATE       PICTURE 9[4].                           LIS01480
              05 REL-TYPE       PICTURE X.                              LIS01490
              05 PRODUCER       PICTURE X[20].                          LIS01500
              05 MAP.                                                   LIS01510
                 07 FILLER      PICTURE X[3].                           LIS01520
                 07 MEDIUM      PICTURE 9.                              LIS01530
              05 CLAS           PICTURE 9[4].                           LIS01540
              05 PRICE          PICTURE 9[4].                           LIS01550
              05 REV-NO         PICTURE 99.                             LIS01560
            04 BLOCK-2.                                                 LIS01570
              05 TUNE  OCCURS 34 TIMES.                                 LIS01580
                 07 POST        PICTURE 9[4] COMPUTATIONAL-3.           LIS01590
                 07 MASTER-NO.                                          LIS01600
                  09 TUNE-POINT PICTURE 9[4].                           LIS01610
                  09 TIME       PICTURE 9999.                           LIS01620
                 07 GAP         PICTURE 99.                             LIS01630
              05 CONT-S-POINT   PICTURE 9[4].                           LIS01640
                                                                        LIS01650
       01  IND1.                                                        LIS01660
           03 FILLER            PICTURE XXX.                            LIS01670
           03 S-INDX.                                                   LIS01680
              05 S-INDX-DETAIL OCCURS 200 TIMES.                        LIS01690
                 07 S-I-SELCTN  PICTURE X[10].                          LIS01700
                 07 FILLER      PICTURE X[9].                           LIS01710
           03 FILLER            PICTURE X[911].                         LIS01720
                                                                        LIS01730
       FD  TRANS-FILE RECORDING F                                       LIS01740
                      BLOCK 10 RECORDS                                  LIS01750
                      RECORD CONTAINS 80 CHARACTERS                     LIS01760
                      LABEL RECORDS OMITTED                             LIS01770
                      DATA RECORDS TUNE-TRANS, SEL-TRANS.               LIS01780
                                                                        LIS01790
       01  TUNE-TRANS.                                                  LIS01800
           02 IMAGE.                                                    LIS01810
           03 T-SOURCE          PICTURE X[2].                           LIS01820
           03 T-TRANS           PICTURE 9.                              LIS01830
           03 T-JOB-NO          PICTURE X[8].                           LIS01840
           03 T-CARD            PICTURE 9.                              LIS01850
           03 T-CARD1.                                                  LIS01860
              05 T-REC-DATE.                                            LIS01870
                 07 DATE1       PICTURE 99.                             LIS01880
                 07 DATE2       PICTURE 99.                             LIS01890
                 07 DATE3       PICTURE 99.                             LIS01900
              05 T-ENG          PICTURE X[20].                          LIS01910
              05 T-REP          PICTURE X[20].                          LIS01920
              05 FILLER         PICTURE X[22].                          LIS01930
           03 T-CARD2 REDEFINES T-CARD1.                                LIS01940
              05 T-ARTIST       PICTURE X[58].                          LIS01950
              05 T-AR-NO  PICTURE 9[5].                                 LIS01960
              05 FILLER         PICTURE X[5].                           LIS01970
           03 T-CARD3 REDEFINES T-CARD1.                                LIS01980
              05 T-SERIAL       PICTURE X[8].                           LIS01990
              05 T-CODE         PICTURE X[3].                           LIS02000
              05 T-DATA         PICTURE X[57].                          LIS02010
              05 T-DUM REDEFINES T-DATA.                                LIS02020
                 07 T-FIN       PICTURE X.                              LIS02030
                 07 T-TAKE      PICTURE 99.                             LIS02040
                 07 T-TIME-1    PICTURE X[4].                           LIS02050
                 07 FILLER      PICTURE X[50].                          LIS02060
       01  SEL-TRANS.                                                   LIS02070
           03 FILLER            PICTURE X[3].                           LIS02080
           03 S-SELCTN          PICTURE X[10].                          LIS02090
           03 S-CARD            PICTURE 9.                              LIS02100
           03 FILLER            PICTURE X[66].                          LIS02110
       FD  LIST-NOTCE RECORDING V                                       LIS02120
                 LABEL RECORDS OMITTED                                  LIS02130
                 DATA RECORDS R1, R2.                                   LIS02140
                                                                        LIS02150
       01  R1.                                                          LIS02160
           03 R1-ID             PICTURE XX.                             LIS02170
           03 BLK-1             PICTURE X[227].                         LIS02180
           03 ALBUMS.                                                   LIS02190
              05 MSTRS OCCURS 4 TIMES.                                  LIS02200
                 07 SIDE-MSTR   PICTURE X[8].                           LIS02210
                 07 INTRO       PICTURE 9[2].                           LIS02220
                 07 FADE        PICTURE 9.                              LIS02230
                 07 FILLER      PICTURE X[3].                           LIS02240
            07 NO-SELNS    PICTURE 99.                             LIS02250
              05 FILLER         PICTURE X[4].                           LIS02260
           03 ST-Q REDEFINES ALBUMS.                                    LIS02270
              05 S8-INDNT       PICTURE 99.                             LIS02280
              05 S8-OUTDNT      PICTURE 99.                             LIS02290
              05 PROGRAMS OCCURS 4 TIMES.                               LIS02300
                 07 NOSLS       PICTURE 99.                             LIS02310
                 07 GAP-T OCCURS 7 TIMES PICTURE 99.                    LIS02320
           03 CASETE REDEFINES ALBUMS.                                  LIS02330
              05 FILLER         PICTURE X[4].                           LIS02340
              05 PROG OCCURS 2 TIMES.                                   LIS02350
                 07 NSELECTS    PICTURE 99.                             LIS02360
                 07 GPS-T OCCURS 15 TIMES PICTURE 99.                   LIS02370
           03 R1-CONT           PICTURE X.                              LIS02380
                                                                        LIS02390
       01  R2.                                                          LIS02400
           03 R2-ID             PICTURE XX.                             LIS02410
           03 R2-DETAIL         PICTURE X[373].                         LIS02420
           03 R2-CONT           PICTURE X.                              LIS02430
           03 R2-FROM           PICTURE X[10].                          LIS02440
                                                                        LIS02450
       WORKING-STORAGE SECTION.                                         LIS02460
           77 N                 PICTURE 9999 VALUE 0.                   LIS02470
           77  CNT-HOLD        PICTURE 999  VALUE 0.                    LIS02480
           77 DIRECTOR         PICTURE 99 VALUE 0.                      LIS02490
           77 CNT      PICTURE 99 VALUE 0.                              LIS02500
           77 I                 PICTURE 9999 VALUE 0.                   LIS02510
           77 J                 PICTURE 9999 VALUE 0.                   LIS02520
           77 K                 PICTURE 9999 VALUE 0.                   LIS02530
           77 L                 PICTURE 9999 VALUE 0.                   LIS02540
           77 A                 PICTURE 9999 VALUE 0.                   LIS02550
           77 B                 PICTURE 9999 VALUE 0.                   LIS02560
           77 C                 PICTURE 9999 VALUE 0.                   LIS02570
           77 NS-CNT            PICTURE S9999 COMPUTATIONAL VALUE 0.    LIS02580
           77 BAD-CNT           PICTURE S9999 COMPUTATIONAL VALUE 0.    LIS02590
           77 TUNE-ACT-KEY      PICTURE S9[8] COMPUTATIONAL.            LIS02600
           77 SEL-ACT-KEY       PICTURE S9[8] COMPUTATIONAL.            LIS02610
           77 CS-SW             PICTURE 9 VALUE 0.                      LIS02620
           77 NEW-SER           PICTURE X[8] VALUE SPACES.              LIS02630
           77 T-UPDATE          PICTURE 9      VALUE 0.                 LIS02640
           77 S-UPDATE          PICTURE 9      VALUE 0.                 LIS02650
           77 DET               PICTURE 9      VALUE 1.                 LIS02660
           77 DETS              PICTURE 9      VALUE 1.                 LIS02670
           77 HALF              PICTURE S9999 COMPUTATIONAL.            LIS02680
           77 T-LEVEL           PICTURE 9999   VALUE 1.                 LIS02690
           77 TR-CNT            PICTURE 9999 VALUE 0.                   LIS02700
           77 ERR-SW            PICTURE 99 VALUE 0.                     LIS02710
           77 TE-CNT            PICTURE 9[4] VALUE 0.                   LIS02720
           77 ERR-CODE          PICTURE 99 VALUE ZERO.                  LIS02730
           77 F-ERR             PICTURE X[3] VALUE SPACES.              LIS02740
           77 M-ERR-NO          PICTURE 9[6].                           LIS02750
           77 ALB               PICTURE 9 VALUE 0.                      LIS02760
           77 H-SIDE            PICTURE 99 VALUE 0.                     LIS02770
           77 CRE               PICTURE 999 VALUE 0.                    LIS02780
           77 OVER-SW           PICTURE 9  VALUE 0.                     LIS02790
           77 NT                PICTURE 99 VALUE 1.                     LIS02800
           77 TRI-SER           PICTURE X[8] VALUE HIGH-VALUE.          LIS02810
           77 MED-SW            PICTURE 9  VALUE 0.                     LIS02820
           77 H-MED             PICTURE X.                              LIS02830
           77 DIST              PICTURE 9  VALUE 0.                     LIS02840
           77 END-SW            PICTURE 9 VALUE 0.                      LIS02850
           77 NN                PICTURE 9 VALUE 0.                      LIS02860
           77 FOUND             PICTURE 9 VALUE 0.                      LIS02870
           77 TI-SW             PICTURE 9 VALUE 0.                      LIS02880
           77 SIDE            PICTURE 99 VALUE 0.                       LIS02890
           77 KNT     PICTURE 999.                                      LIS02900
           77 SERIAL-HOLD          PICTURE 9[8].                        LIS02910
           77 TIMEHLD    PICTURE  99.                                   LIS02920
           77 BC PICTURE 999 VALUE 0.                                   LIS02930
           77 BD  PICTURE 999 VALUE 0.                                  LIS02940
           77 B1 PICTURE 99 VALUE 0.                                    LIS02950
           77 C1 PICTURE 99 VALUE 0.                                    LIS02960
                                                                        LIS02970
       01  N-B-A     PICTURE 9[4].                                      LIS02980
       01  NO-BANDS-AREA REDEFINES N-B-A.                               LIS02990
           03 NO-BANDS OCCURS 4 TIMES PICTURE 9.                        LIS03000
       01  CSS-MSGS.                                                    LIS03010
           03 VAR1         PICTURE X[8].                                LIS03020
           03 VAR2         PICTURE X[8].                                LIS03030
           03 VAR3         PICTURE X[8].                                LIS03040
           03 VAR4         PICTURE X[8].                                LIS03050
           03 RET-CD       PICTURE 9999.                                LIS03060
       01  DT.                                                          LIS03070
           03 D-MO              PICTURE 99.                             LIS03080
           03 D-DA              PICTURE 99.                             LIS03090
           03 YR                PICTURE 99.                             LIS03100
           03 FILLER            PICTURE XX.                             LIS03110
       01  DT2 REDEFINES DT.                                            LIS03120
           03 J-DAT      PICTURE 9[5].                                  LIS03130
           03 FILLER     PICTURE X[3].                                  LIS03140
       01  DT3 REDEFINES DT2 PICTURE X[8].                              LIS03150
       01  JULIAN-DATE    PICTURE 9[5] COMPUTATIONAL-3.                 LIS03160
       01  REL-AREA.                                                    LIS03170
           03 REL-TYPE-SAVE   PICTURE X.                                LIS03180
               88 OPP-UNA VALUE IS :3: :6:.                             LIS03190
       01  PAR    PICTURE   X[24].                                      LIS03200
       01  ERR-REASON  PICTURE X[10].                                   LIS03210
       01  CONT-CNV.                                                    LIS03220
           03  C-REL    PICTURE 999.                                    LIS03230
           03 C-DET   PICTURE 9.                                        LIS03240
       01  NO-MORE-SWITCH  PICTURE 9.                                   LIS03250
             88 NO-MORE-INDEXES VALUE IS 1.                             LIS03260
       01  NEW-TUNE-SWITCH-AREA.                                        LIS03270
           03 NEW-TUNE-SWITCH  PICTURE  9 VALUE ZERO.                   LIS03280
             88 NEW-TUNES VALUE IS 1.                                   LIS03290
       01  NINETY-NINE   PICTURE 99 VALUE 99 COMPUTATIONAL-3.           LIS03300
       01  POST-CNV  PICTURE 9999 COMPUTATIONAL-3.                      LIS03310
       01  ZERO-VALUE   PICTURE 99 VALUE 0 COMPUTATIONAL-3.             LIS03320
       01  PHOLD1.                                                      LIS03330
           03  POST-HOLD1.                                              LIS03340
               05 SIDE-HOLD1    PICTURE 99.                             LIS03350
               05 SLOT-HOLD1   PICTURE 99.                              LIS03360
           03  MSTHLD1.                                                 LIS03370
               05 TUNP1      PICTURE 9[4].                              LIS03380
               05 STIM1     PICTURE 9[4].                               LIS03390
           03 GAPHLD1       PICTURE 99.                                 LIS03400
       01  PHOLD2.                                                      LIS03410
           03  POST-HOLD2.                                              LIS03420
               05 SIDE-HOLD2    PICTURE 99.                             LIS03430
               05 SLOT-HOLD2   PICTURE 99.                              LIS03440
           03  MSTHLD2.                                                 LIS03450
               05 TUNP2      PICTURE 9[4].                              LIS03460
               05 STIM2     PICTURE 9[4].                               LIS03470
           03 GAPHLD2       PICTURE 99.                                 LIS03480
       01  SEL-CNV.                                                     LIS03490
           03  SEL-REL    PICTURE 9[3].                                 LIS03500
           03  SEL-DET     PICTURE 9.                                   LIS03510
       01  MST-S-HLD1.                                                  LIS03520
           03 M-T-S1       PICTURE X[8].                                LIS03530
           03  FILLER     PICTURE XX.                                   LIS03540
           03 M-B1         PICTURE 99.                                  LIS03550
           03 M-A1         PICTURE XX.                                  LIS03560
       01  MST-S-HLD2.                                                  LIS03570
           03 M-T-S2       PICTURE X[8].                                LIS03580
           03 FILLER PICTURE XX.                                        LIS03590
           03 M-B2         PICTURE 99.                                  LIS03600
           03 M-A2         PICTURE XX.                                  LIS03610
       01  LIST-SWITCH        PICTURE XXX.                              LIS03620
               88 LIST-ONLY VALUE IS :SET:.                             LIS03630
                                                                        LIS03640
       01  LIMITS-SECT.                                                 LIS03650
           03 H-ENG             PICTURE X[20].                          LIS03660
           03 NT-HOLD1          PICTURE X[19].                          LIS03670
           03 NT-HOLD2          PICTURE X[19].                          LIS03680
           03 NT-CNT OCCURS 10 TIMES PICTURE 999.                       LIS03690
           03 NX OCCURS 10 TIMES PICTURE 999.                           LIS03700
           03 T-LIMIT-DUM.                                              LIS03710
              05 U-TUNE OCCURS 10 TIMES PICTURE X[8].                   LIS03720
           03 FIRST-OPEN.                                               LIS03730
              05 FIRST-OPEN-REC PICTURE 9[3].                           LIS03740
              05 FIRST-DET      PICTURE 9.                              LIS03750
           03 LAST-OPEN.                                                LIS03760
              05 LAST-OPEN-REC  PICTURE 9[3].                           LIS03770
              05 LAST-DET       PICTURE 9.                              LIS03780
           03 S-LIMIT-DUM.                                              LIS03790
              05 S-LAST         PICTURE 999.                            LIS03800
              05 FILLER         PICTURE X[7].                           LIS03810
           03 FS-AREA       PICTURE 9[5].                               LIS03820
           03 FS-AREA2   REDEFINES FS-AREA.                             LIS03830
             05 FS-ZERO     PICTURE 9.                                  LIS03840
             05 FIRST-SEL.                                              LIS03850
                07 FIRST-SEL-REC  PICTURE 9[3].                         LIS03860
                07 FIRST-S-DET    PICTURE 9.                            LIS03870
           03 LAST-SEL.                                                 LIS03880
              05 LAST-SEL-REC   PICTURE 9[3].                           LIS03890
              05 LAST-S-DET     PICTURE 9.                              LIS03900
           03 POS1              PICTURE 9[4].                           LIS03910
           03 POS2 REDEFINES POS1.                                      LIS03920
              05 POSITN         PICTURE 99.                             LIS03930
              05 SLOT           PICTURE 99.                             LIS03940
           03 TUNE-SAVE.                                                LIS03950
              05 TS-REC         PICTURE 9[3].                           LIS03960
              05 TS-DET         PICTURE 9.                              LIS03970
           03 TUN-CNV-DUM.                                              LIS03980
              05 TP-CNV.                                                LIS03990
                 07 TP-REL      PICTURE 9[3].                           LIS04000
                 07 TP-DET      PICTURE 9.                              LIS04010
              05 TP-BND         PICTURE 99.                             LIS04020
              05 TP-BND-C REDEFINES TP-BND PICTURE XX.                  LIS04030
           03 TUN-CNV REDEFINES TUN-CNV-DUM PICTURE 9[6].               LIS04040
           03 FIRST-SEL-S.                                              LIS04050
              05 ACT-POINT      PICTURE 9[4].                           LIS04060
              05 ZERO-POINT     PICTURE 9[2].                           LIS04070
           03 FIRST-SEL-SAVE REDEFINES FIRST-SEL-S PICTURE 9[6].        LIS04080
           03 TIM-DUM.                                                  LIS04090
              05 MIN1           PICTURE 99.                             LIS04100
              05 SEC1           PICTURE 99.                             LIS04110
                                                                        LIS04120
       01  SEL-CARD-AREA.                                               LIS04130
           03 FILLER            PICTURE X[3].                           LIS04140
           03 SEL-SELCTN        PICTURE X[10].                          LIS04150
           03 SEL-CARD-NO       PICTURE 9.                              LIS04160
           03 S-CARD0.                                                  LIS04170
               05 S-SALES-CODE       PICTURE 9[4].                      LIS04180
               05 S-PRICE          PICTURE 9[4].                        LIS04190
               05 S-PROJ-REL-DATE      PICTURE 9[4].                    LIS04200
               05 FILLER            PICTURE X[54].                      LIS04210
           03 S-CARD1 REDEFINES S-CARD0.                                LIS04220
              05 S-SOURCE       PICTURE X[10].                          LIS04230
              05 S-TYPE         PICTURE X[3].                           LIS04240
              05 S-MAP          PICTURE 9[4].                           LIS04250
              05 S-MA-S REDEFINES S-MAP PICTURE X[4].                   LIS04260
              05 S-MA-N REDEFINES S-MAP OCCURS 4 TIMES PICTURE 9.       LIS04270
              05 S-PROD         PICTURE X[20].                          LIS04280
              05 FILLER         PICTURE X[29].                          LIS04290
           03 S-CARD2 REDEFINES S-CARD0.                                LIS04300
              05 S-TITLE        PICTURE X[66].                          LIS04310
           03 S-CARD4 REDEFINES S-CARD0.                                LIS04320
              05 S-ARTIST.                                              LIS04330
                 07 ART-FIL     PICTURE X[9].                           LIS04340
                 07 FILLER      PICTURE X[57].                          LIS04350
           03 S-CARD6 REDEFINES S-CARD0.                                LIS04360
              05 S-SIDE         PICTURE 9[2].                           LIS04370
              05 S-POSTN        PICTURE 9[2].                           LIS04380
              05 S-TAPE         PICTURE X[8].                           LIS04390
              05 S-T REDEFINES S-TAPE.                                  LIS04400
                 07 FILLER      PICTURE X[3].                           LIS04410
                 07 M-DIG       PICTURE X.                              LIS04420
                 07 FILLER      PICTURE X[4].                           LIS04430
              05 S-BAND         PICTURE 9[2].                           LIS04440
              05 S-BAND-C REDEFINES S-BAND PICTURE X[2].                LIS04450
             05 F-AREA PICTURE X[4].                                    LIS04460
             05 T-AREA REDEFINES F-AREA.                                LIS04470
                07 S-MIN          PICTURE 9[2].                         LIS04480
                07 S-MIN-C REDEFINES S-MIN PICTURE X[2].                LIS04490
                07 S-SEC          PICTURE 9[2].                         LIS04500
                07 S-SEC-C REDEFINES S-SEC PICTURE X[2].                LIS04510
              05 S-GMIN         PICTURE 9[2].                           LIS04520
              05 S-GMIN-S REDEFINES S-GMIN PICTURE X[2].                LIS04530
              05 S-GSEC         PICTURE 9[2].                           LIS04540
              05 S-GSEC-S REDEFINES S-GSEC PICTURE X[2].                LIS04550
              05 FILLER         PICTURE X[44].                          LIS04560
           03 FILLER            PICTURE X[67].                          LIS04570
                                                                        LIS04580
       01  TUNE-AREA.                                                   LIS04590
           03 FILLER            PICTURE XX.                             LIS04600
           03 TU-TRANS          PICTURE X.                              LIS04610
           03 JOB-NO-1          PICTURE X[8].                           LIS04620
           03 TU-CARD           PICTURE X.                              LIS04630
           03 TU-CARD1.                                                 LIS04640
              05 DATE-REC-1     PICTURE 9[6].                           LIS04650
              05 DATE-REC-C REDEFINES DATE-REC-1 PICTURE X[6].          LIS04660
              05 ENGINEER-1.                                            LIS04670
                 07 ENG-CHAR OCCURS 20 TIMES PICTURE X.                 LIS04680
              05 A-R-PROD-1     PICTURE X[20].                          LIS04690
              05 FILLER         PICTURE X[22].                          LIS04700
           03 TU-CARD2 REDEFINES TU-CARD1.                              LIS04710
              05 ARTIST-NAME-1  PICTURE X[58].                          LIS04720
              05 ARTIST-NO-1    PICTURE 9[5].                           LIS04730
              05 FILLER         PICTURE X[5].                           LIS04740
           03 TU-CARD3 REDEFINES TU-CARD1.                              LIS04750
              05 SERIAL-1       PICTURE X[8].                           LIS04760
              05 D-CODE-1       PICTURE XX.                             LIS04770
              05 D-SEQ-1        PICTURE X.                              LIS04780
              05 DAT-1.                                                 LIS04790
                 07 DATA-C OCCURS 57 TIMES PICTURE X.                   LIS04800
              05 T-TUM REDEFINES DAT-1.                                 LIS04810
                 07 F-TAKE-1    PICTURE X.                              LIS04820
                 07 TAKE-1      PICTURE XX.                             LIS04830
                 07 R-TIME-1    PICTURE X[4].                           LIS04840
                 07 FILLER      PICTURE X[50].                          LIS04850
                                                                        LIS04860
       01  FL-AREA.                                                     LIS04870
           03 FILLER            PICTURE X[3].                           LIS04880
           03 FIN-SELCTN        PICTURE X[10].                          LIS04890
           03 FIN-REL           PICTURE 9[4].                           LIS04900
           03 FIN-CLASS         PICTURE 9[4].                           LIS04910
           03 FIN-PR            PICTURE 9[4].                           LIS04920
           03 FILLER            PICTURE X[122].                         LIS04930
                                                                        LIS04940
       01  TUNE-INDEX.                                                  LIS04950
           03 TI-DETAIL OCCURS 150 TIMES ASCENDING KEY TI-SERIAL        LIS04960
                 INDEXED BY TI-CNT.                                     LIS04970
              05 TI-SERIAL      PICTURE X[8].                           LIS04980
              05 TI-HOLD         PICTURE X.                             LIS04990
              05 TUN-TYPE       PICTURE X.                              LIS05000
              05 TI-LAST-ACT   PICTURE 9[5]  COMPUTATIONAL-3.           LIS05010
              05 TI-STATUS     PICTURE X.                               LIS05020
              05 TI-TYPE        PICTURE 9.                              LIS05030
              05 TI-POINT.                                              LIS05040
                 07 TI-REL      PICTURE 999.                            LIS05050
                 07 TI-REC      PICTURE 9.                              LIS05060
                                                                        LIS05070
       01  NEW-TUNE-INDEX.                                              LIS05080
           03 NT-DETAIL.                                                LIS05090
              05 NT-SERIAL      PICTURE X[8].                           LIS05100
              05 NT-HOLD        PICTURE X.                              LIS05110
              05 FILLER         PICTURE X[1].                           LIS05120
              05 NT-LAST-ACT.                                           LIS05130
                 07 NT-MO       PICTURE 99.                             LIS05140
                 07 NT-DA       PICTURE 99.                             LIS05150
              05 NT-TYPE        PICTURE X.                              LIS05160
              05 NT-POINT.                                              LIS05170
                 07 NT-REC      PICTURE 999.                            LIS05180
                 07 NT-DET      PICTURE 9.                              LIS05190
                                                                        LIS05200
       01  SELCTN-AREA.                                                 LIS05210
           03 MAST1-1           PICTURE X[8].                           LIS05220
           03 FILLER            PICTURE X[2].                           LIS05230
           03 MAST2-1.                                                  LIS05240
                 07 INDENT-1    PICTURE 9[2].                           LIS05250
                 07 OUTDENT-1   PICTURE 9[2].                           LIS05260
                 07 FILLER      PICTURE X[6].                           LIS05270
           03 TUNES-1 OCCURS 24 TIMES.                                  LIS05280
              05 TUNE-NO-1      PICTURE X[8].                           LIS05290
              05 FILLER         PICTURE X[2].                           LIS05300
              05 TIME-1         PICTURE 9[4].                           LIS05310
              05 POST-1         PICTURE 9[4] COMPUTATIONAL-3.           LIS05320
              05 GAP-1          PICTURE 99.                             LIS05330
                                                                        LIS05340
       01  TUNE-SUSP-AREA.                                              LIS05350
           03 TS-DUM.                                                   LIS05360
              05 SUSP1          PICTURE X[96] VALUE SPACES.             LIS05370
              05 SUSP2          PICTURE X[96] VALUE SPACES.             LIS05380
           03 TS-DUM2 REDEFINES TS-DUM.                                 LIS05390
              05 SUSP-TUNE OCCURS 24 TIMES PICTURE X[8].                LIS05400
                                                                        LIS05410
       01  SEL-INDEX.                                                   LIS05420
           02 SEL-DUM.                                                  LIS05430
           03 SI-DETAIL OCCURS 400 TIMES ASCENDING KEY SI-SELCTN        LIS05440
                 INDEXED BY SI-CNT.                                     LIS05450
              05 SI-SELCTN      PICTURE X[10].                          LIS05460
              05 SI-LAST-ACT  PICTURE 9[5] COMPUTATIONAL-3.             LIS05470
              05 FILLER   PICTURE X.                                    LIS05480
              05 SI-TYPE        PICTURE 9.                              LIS05490
              05 SI-POINT.                                              LIS05500
                 07 SI-REL      PICTURE 999.                            LIS05510
                 07 SI-REC      PICTURE 9.                              LIS05520
           02 S-DUM REDEFINES SEL-DUM.                                  LIS05530
           03 S-INDX-1          PICTURE X[3800].                        LIS05540
           03 S-INDX-2          PICTURE X[3800].                        LIS05550
                                                                        LIS05560
       01  NEW-SEL-INDEX.                                               LIS05570
           03 NS-DETAIL OCCURS 20 TIMES.                                LIS05580
              05 NS-SELCTN      PICTURE X[10].                          LIS05590
              05 NS-LAST-ACT PICTURE 9[5] COMPUTATIONAL-3.              LIS05600
              05 FILLER PICTURE X.                                      LIS05610
              05 NS-TYPE        PICTURE 9.                              LIS05620
              05 NS-POINT.                                              LIS05630
                 07 NS-REC      PICTURE 999.                            LIS05640
                 07 NS-DET      PICTURE 9.                              LIS05650
                                                                        LIS05660
       01  TRANS-TABLE-AREA.                                            LIS05670
           02 TAB-DUM.                                                  LIS05680
           03 TAB-TRANS OCCURS 50 TIMES.                                LIS05690
              05 TAB-SOURCE     PICTURE XX.                             LIS05700
              05 TAB-TRANSTN    PICTURE X.                              LIS05710
              05 TAB-JOB-NO     PICTURE X[8].                           LIS05720
              05 TAB-CARD-1     PICTURE X.                              LIS05730
              05 TAB-CARD1.                                             LIS05740
                 07 TAB-SERIAL  PICTURE X[8].                           LIS05750
                 07 FILLER      PICTURE X[60].                          LIS05760
              05 TAB-ERR-NO     PICTURE 9[6].                           LIS05770
              05 TAB-FIRST      PICTURE X[4].                           LIS05780
              05 TAB-CNT        PICTURE 9.                              LIS05790
              05 E-DUM OCCURS 8 TIMES.                                  LIS05800
                 07 TAB-ERR     PICTURE X[3].                           LIS05810
                 07 TAB-CRCTN   PICTURE 9[4].                           LIS05820
           02 TAB-DUM-2 REDEFINES TAB-DUM.                              LIS05830
           03 DUM-TAB OCCURS 50 TIMES.                                  LIS05840
              05 FILLER         PICTURE X[3].                           LIS05850
              05 TAB-SELCTN     PICTURE X[10].                          LIS05860
              05 TAB-CARD       PICTURE X.                              LIS05870
              05 FILLER         PICTURE X[133].                         LIS05880
           02 TUNE-CARD1        PICTURE X[147] VALUE SPACES.            LIS05890
           02 TUNE-CARD2        PICTURE X[147] VALUE SPACES.            LIS05900
                                                                        LIS05910
                                                                        LIS05920
       01  CODE-TABLE.                                                  LIS05930
           03 CT-DUM            PICTURE X[56] VALUE :  ARTIWRPRPUVLVAMLMLIS05940
      -                                  :AMCAGACCDNOENSUTFSAAADICSCT:. LIS05950
           03 CT-DUM2 REDEFINES CT-DUM.                                 LIS05960
              05 COD OCCURS 28 TIMES PICTURE XX.                        LIS05970
           03 CT-DUM3           PICTURE X[56] VALUE :0000000315091307140LIS05980
      -                               :812040605101116171819202223:.    LIS05990
           03 CT-DUM4 REDEFINES CT-DUM3.                                LIS06000
              05 COD-NUM OCCURS 21 TIMES.                               LIS06010
                 07 COD-1       PICTURE X.                              LIS06020
                 07 COD-2       PICTURE X.                              LIS06030
                                                                        LIS06040
       01  CRED-CHAR-AREA.                                              LIS06050
           03 C-DUM.                                                    LIS06060
              05 CHAR OCCURS 260 TIMES PICTURE X.                       LIS06070
           03 C-DUM2 REDEFINES C-DUM.                                   LIS06080
              05 CRED-1         PICTURE X[200].                         LIS06090
              05 CRED-2         PICTURE X[60].                          LIS06100
       01  TUNE-HOLDER.                                                 LIS06110
           03 FILLER       PICTURE X[3].                                LIS06120
           03  TUNE-REC-HOLD.                                           LIS06130
             05 FILLER           PICTURE X[3179].                       LIS06140
                                                                        LIS06150
       PROCEDURE DIVISION.                                              LIS06160
       HSKEEPING-PAR.                                                   LIS06170
           OPEN I-O TUNE-INV.                                           LIS06180
           OPEN INPUT CONI-FILE.                                        LIS06190
           MOVE SPACES TO DT3.                                          LIS06200
           READ CONI-FILE AT END CLOSE CONI-FILE.                       LIS06210
           MOVE T-DA TO D-DA.                                           LIS06220
           MOVE T-MO TO D-MO.                                           LIS06230
           MOVE T-YR TO YR.                                             LIS06240
           CALL :ASMDATCV: USING DT2.                                   LIS06250
           MOVE J-DAT TO JULIAN-DATE.                                   LIS06260
           OPEN OUTPUT CHANGES.                                         LIS06270
           OPEN I-O SELECTION-INV.                                      LIS06280
           OPEN INPUT TRANS-FILE.                                       LIS06290
           OPEN OUTPUT LIST-NOTCE.                                      LIS06300
           MOVE 0 TO TUNE-ACT-KEY, SEL-ACT-KEY, H-MED.                  LIS06310
           READ TUNE-INV INVALID KEY GO TO HSK-ERR.                     LIS06320
           PERFORM HSK010 VARYING N FROM 1 BY 1 UNTIL N > 11.           LIS06330
           PERFORM HSK020 VARYING N FROM 1 BY 1 UNTIL N > 11.           LIS06340
           MOVE T-INDX TO TUNE-INDEX.                                   LIS06350
           MOVE TI-POINT [1] TO FIRST-OPEN.                             LIS06360
           MOVE TI-POINT [2] TO LAST-OPEN.                              LIS06370
           MOVE :033: TO REC-ID-1.                                      LIS06380
           REWRITE TYP1 INVALID KEY GO TO HSK-ERR.                      LIS06390
           MOVE 1 TO T-LEVEL.                                           LIS06400
           READ SELECTION-INV INVALID KEY GO TO HSK-ERR.                LIS06410
           MOVE S-INDX TO S-INDX-1.                                     LIS06420
           MOVE SI-LAST-ACT [1] TO FS-AREA.                             LIS06430
           MOVE SI-POINT [1] TO LAST-SEL.                               LIS06440
           MOVE SI-SELCTN [1] TO S-LIMIT-DUM.                           LIS06450
           MOVE SPACES TO SI-SELCTN [1].                                LIS06460
           ADD 1 TO SEL-ACT-KEY.                                        LIS06470
           READ SELECTION-INV INVALID KEY GO TO HSK-ERR.                LIS06480
           MOVE S-INDX TO S-INDX-2.                                     LIS06490
           MOVE HIGH-VALUE TO TAB-SOURCE [1], TAB-TRANSTN [1],          LIS06500
                 TAB-JOB-NO [1].                                        LIS06510
           GO TO TRANS-READ.                                            LIS06520
                                                                        LIS06530
       HSK-ERR.                                                         LIS06540
           MOVE :0: TO ERR-REASON.                                      LIS06550
           MOVE :HSKEEPING-PAR: TO PAR.                                 LIS06560
           GO TO ERR-HALT.                                              LIS06570
       HSK010.                                                          LIS06580
           IF T-I-SERIAL [N] NOT > SPACES                               LIS06590
                MOVE T-I-SERIAL [N] TO U-TUNE [N]                       LIS06600
           ELSE MOVE HIGH-VALUES TO U-TUNE [N].                         LIS06610
           MOVE SPACES TO T-I-SERIAL [N].                               LIS06620
       HSK020.                                                          LIS06630
           MOVE 0 TO NT-CNT [N].                                        LIS06640
                                                                        LIS06650
       TRANS-READ.                                                      LIS06660
           GO TO TR010.                                                 LIS06670
       TR010.                                                           LIS06680
           IF LIST-ONLY MOVE SPACES TO LIST-SWITCH.                     LIS06690
           READ TRANS-FILE AT END GO TO X010.                           LIS06700
           IF T-SOURCE NOT > TAB-SOURCE [1]     GO TO CHNG.             LIS06710
           IF T-JOB-NO NOT > TAB-JOB-NO [1] GO TO CHNG.                 LIS06720
           IF T-TRANS NOT > TAB-TRANSTN [1] GO TO CHNG.                 LIS06730
       SEQ-TRANS.                                                       LIS06740
           IF S-SELCTN NOT > TAB-SELCTN [TR-CNT] GO TO CHNG.            LIS06750
           ADD 1 TO TR-CNT.                                             LIS06760
           IF TR-CNT \ 50 GO TO TAB-OVER.                               LIS06770
           MOVE SEL-TRANS TO TAB-TRANS [TR-CNT].                        LIS06780
           GO TO TRANS-READ.                                            LIS06790
       TAB-OVER.                                                        LIS06800
           MOVE 1 TO OVER-SW.                                           LIS06810
       CHNG.                                                            LIS06820
           GO TO A051.                                                  LIS06830
       A051.                                                            LIS06840
           ALTER CHNG TO PROCEED TO A052.                               LIS06850
           GO TO A053                                                   LIS06860
       A052.                                                            LIS06870
            GO TO SEL-SEQNCE.                                           LIS06880
       A053.                                                            LIS06890
           IF OVER-SW > 2 MOVE 0 TO OVER-SW.                            LIS06900
           IF OVER-SW > 0 MOVE 0 TO ERR-SW                              LIS06910
                 ELSE MOVE 2 TO OVER-SW.                                LIS06920
           MOVE 1 TO TR-CNT.                                            LIS06930
           MOVE SEL-TRANS TO TAB-TRANS [TR-CNT].                        LIS06940
           GO TO TRANS-READ.                                            LIS06950
                                                                        LIS06960
       SEL-SEQNCE.                                                      LIS06970
           GO TO B020.                                                  LIS06980
       B020.                                                            LIS06990
           ALTER SEL-SEQNCE TO PROCEED TO B030.                         LIS07000
       B030.                                                            LIS07010
           IF TAB-SOURCE [1] > :KD: GO TO FINAL-LN.                     LIS07020
           IF TAB-TRANSTN [1] > 7 GO TO ADDER.                          LIS07030
           IF TAB-TRANSTN [1] > 5 GO TO LIST-GENERATOR.                 LIS07040
           MOVE TAB-TRANS [1] TO SEL-CARD-AREA.                         LIS07050
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS07060
           IF TAB-TRANSTN [1] > 2 GO TO SEL-DELETE.                     LIS07070
           IF TAB-TRANSTN [1] > 4 GO TO SEL-CHNG.                       LIS07080
           IF DETS > 0 GO TO NEW-SEL.                                   LIS07090
           DISPLAY :SELECTION : SEL-SELCTN :NOT FOUND : UPON CONSOLE.   LIS07100
           STOP RUN.                                                    LIS07110
       NEW-SEL.                                                         LIS07120
           MOVE 0 TO ERR-SW, A, B, N-B-A.                               LIS07130
           MOVE FIRST-SEL-REC TO SEL-ACT-KEY.                           LIS07140
           READ SELECTION-INV INVALID KEY  MOVE :NEW SEL: TO PAR        LIS07150
           MOVE FIRST-SEL-REC TO ERR-REASON                             LIS07160
           GO TO ERR-HALT.                                              LIS07170
           MOVE 1 TO N.                                                 LIS07180
           MOVE 0 TO J.                                                 LIS07190
           MOVE FIRST-S-DET TO I.                                       LIS07200
           MOVE I TO DETS.                                              LIS07210
           MOVE TUNE-POINT [I, 1] TO ACT-POINT.                         LIS07220
       N100.                                                            LIS07230
           IF N \ TR-CNT GO TO END-SEL.                                 LIS07240
           MOVE TAB-TRANS [N] TO SEL-CARD-AREA.                         LIS07250
           IF SEL-CARD-NO > 0                                           LIS07260
           MOVE S-SALES-CODE TO CLAS [DETS]                             LIS07270
           MOVE S-PRICE TO PRICE [DETS]                                 LIS07280
           MOVE S-PROJ-REL-DATE TO REL-DATE [DETS]                      LIS07290
           ADD 1 TO N GO TO N100.                                       LIS07300
           GO TO N1, N2, N3, N4, N5, N6 DEPENDING ON SEL-CARD-NO.       LIS07310
       N1.                                                              LIS07320
           MOVE S-MA-N [4] TO ALB.                                      LIS07330
           MOVE S-SOURCE TO SOURCE-SEL [DETS].                          LIS07340
           MOVE 1 TO REL-TYPE [DETS].                                   LIS07350
           IF S-TYPE > :SPE: MOVE 2 TO REL-TYPE [DETS].                 LIS07360
           IF S-TYPE > :OPP: MOVE 3 TO REL-TYPE [DETS].                 LIS07370
           IF S-TYPE > :PRO: MOVE 4 TO REL-TYPE [DETS].                 LIS07380
           IF S-TYPE > :SNG: MOVE 5 TO REL-TYPE [DETS].                 LIS07390
           IF S-TYPE > :UNA: MOVE 6 TO REL-TYPE [DETS].                 LIS07400
           MOVE REL-TYPE [DETS] TO REL-TYPE-SAVE.                       LIS07410
           MOVE SPACES TO SELCTN-TITLE [DETS].                          LIS07420
           MOVE S-MAP TO MAP [DETS].                                    LIS07430
           MOVE S-PROD TO PRODUCER [DETS].                              LIS07440
           MOVE SEL-SELCTN TO SELCTN-NO [DETS].                         LIS07450
           ADD 1 TO N.                                                  LIS07460
           GO TO N100.                                                  LIS07470
       N2.                                                              LIS07480
           MOVE S-TITLE TO SELCTN-TITLE [DETS].                         LIS07490
           MOVE SPACES TO TITLE2 [DETS].                                LIS07500
           ADD 1 TO N.                                                  LIS07510
           GO TO N100.                                                  LIS07520
       N3.                                                              LIS07530
           MOVE S-TITLE TO TITLE2 [DETS].                               LIS07540
           ADD 1 TO N.                                                  LIS07550
           GO TO N100.                                                  LIS07560
       N4.                                                              LIS07570
           MOVE S-ARTIST TO ART-NAME [DETS].                            LIS07580
           ADD 1 TO N.                                                  LIS07590
           GO TO N100.                                                  LIS07600
       N5.                                                              LIS07610
           ADD 1 TO N.                                                  LIS07620
           GO TO N100.                                                  LIS07630
       N6.                                                              LIS07640
           IF S-POSTN > 99 ADD 1 TO N, GO TO N100.                      LIS07650
           IF S-POSTN > 0 GO TO N6-0CHK.                                LIS07660
           ADD 1 TO J.                                                  LIS07670
       N6-3.                                                            LIS07680
           MOVE 0 TO FOUND.                                             LIS07690
           MOVE S-TAPE TO SERIAL-1.                                     LIS07700
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS07710
           IF ERR-CODE \ 0 GO TO THATS-ALL.                             LIS07720
           IF FOUND > 0 GO TO NO-TUNE.                                  LIS07730
           IF S-POSTN > 0 GO TO DUPL-MSTR.                              LIS07740
           MOVE TI-POINT [TI-CNT] TO TP-CNV.                            LIS07750
           IF S-BAND-C > SPACES MOVE 0 TO S-BAND.                       LIS07760
           MOVE S-BAND TO TP-BND.                                       LIS07770
           IF S-BAND NOT > 0 GO TO MSTR-LOOK.                           LIS07780
       N6-35.                                                           LIS07790
           MOVE TP-CNV TO TUNE-POINT [I, J].                            LIS07800
           ADD 1 TO A.                                                  LIS07810
           COMPUTE TIME [I, J] > S-MIN * 60 < S-SEC.                    LIS07820
           MOVE 0 TO GAP [I, J].                                        LIS07830
           IF ALB IS LESS THAN 5 GO TO N6-POS.                          LIS07840
       N6-4.                                                            LIS07850
           IF S-POSTN > 0 ADD 1 TO J.                                   LIS07860
           IF S-GMIN-S > SPACES MOVE 0 TO S-GMIN.                       LIS07870
           IF S-GSEC-S > SPACES MOVE 0 TO S-GSEC.                       LIS07880
           COMPUTE GAP [I, J] > S-GMIN * 60 < S-GSEC.                   LIS07890
       N6-POS.                                                          LIS07900
           MOVE S-SIDE TO POSITN.                                       LIS07910
           MOVE S-POSTN TO SLOT.                                        LIS07920
           MOVE POS1 TO POST [I, J].                                    LIS07930
           ADD 1 TO N.                                                  LIS07940
           GO TO N100.                                                  LIS07950
       N6-0CHK.                                                         LIS07960
           IF ALB \ 5 GO TO N6-4.                                       LIS07970
           GO TO N6-3.                                                  LIS07980
                                                                        LIS07990
       MSTR-LOOK.                                                       LIS08000
           IF TUN-TYPE [TI-CNT] > :M: GO TO ML010.                      LIS08010
       ML010.                                                           LIS08020
           IF TUNE-ACT-KEY > TP-REL GO TO ML020.                        LIS08030
           MOVE TP-REL TO TUNE-ACT-KEY.                                 LIS08040
           READ TUNE-INV INVALID KEY MOVE :ML010: TO PAR                LIS08050
           MOVE TP-REL TO ERR-REASON                                    LIS08060
           GO TO ERR-HALT.                                              LIS08070
       ML020.                                                           LIS08080
           IF MSTR-TUNE-SER [TP-DET, S-BAND] > SPACES GO TO ML020-ERR.  LIS08090
           IF S-POSTN > 0 GO TO N6-35.                                  LIS08100
           MOVE MSTR-TUNE-SER [TP-DET, S-BAND] TO SERIAL-1.             LIS08110
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS08120
           IF ERR-CODE \ 0 GO TO THATS-ALL.                             LIS08130
           IF FOUND > 0 GO TO NO-TUNE.                                  LIS08140
           MOVE 0 TO S-BAND, TP-BND.                                    LIS08150
           MOVE SERIAL-1 TO S-TAPE.                                     LIS08160
           MOVE TI-POINT [TI-CNT]  TO TP-CNV.                           LIS08170
           GO TO N6-35.                                                 LIS08180
       ML020-ERR.                                                       LIS08190
           DISPLAY :NO MASTR-TUNE FOR : SEL-SELCTN UPON CONSOLE.        LIS08200
           STOP RUN.                                                    LIS08210
       NO-TUNE.                                                         LIS08220
           IF S-POSTN > 0 GO TO VAL-MSTR.                               LIS08230
       NO-1.                                                            LIS08240
           GO TO ML020-ERR.                                             LIS08250
       VAL-MSTR.                                                        LIS08260
           IF S-MIN-C > :UN: MOVE 1 TO NO-BANDS [S-SIDE].               LIS08270
           IF B NOT > 0 MOVE A TO GAP [I, B].                           LIS08280
           IF ALB > 1 AND REL-TYPE [I] > 5 AND M-DIG > :M:              LIS08290
               GO TO VM-END.                                            LIS08300
           ADD 1 TO J.                                                  LIS08310
           MOVE S-SIDE TO POSITN.                                       LIS08320
           MOVE S-POSTN TO SLOT.                                        LIS08330
           MOVE POS1 TO POST [I, J].                                    LIS08340
           MOVE S-TAPE TO MASTER-NO [I, J].                             LIS08350
           MOVE 0 TO A.                                                 LIS08360
           MOVE J TO B.                                                 LIS08370
           ADD 1 TO N.                                                  LIS08380
           IF ALB > 1 AND REL-TYPE [I] > 5 MOVE :M: TO M-DIG            LIS08390
               GO TO N6-3.                                              LIS08400
           GO TO N100.                                                  LIS08410
       VM-END.                                                          LIS08420
           MOVE :S: TO M-DIG.                                           LIS08430
           GO TO N100.                                                  LIS08440
                                                                        LIS08450
       DUPL-MSTR.                                                       LIS08460
           DISPLAY :DUPL MSTR-SIDE FOR: SEL-SELCTN UPON CONSOLE.        LIS08470
           STOP RUN.                                                    LIS08480
       END-SEL.                                                         LIS08490
           COMPUTE BC > TR-CNT - 2.                                     LIS08500
           COMPUTE CNT > J < 1.                                         LIS08510
           IF CNT NOT > 36                                              LIS08520
             MOVE ZERO TO POST [DETS, CNT].                             LIS08530
           MOVE A TO GAP [I, B].                                        LIS08540
           MOVE 0 TO REV-NO [DETS].                                     LIS08550
           IF ALB ) 5 AND ALB \ 1                                       LIS08560
           MOVE N-B-A TO NO-BDS [DETS].                                 LIS08570
           MOVE BLOCK-1 [DETS] TO BLK-1.                                LIS08580
           MOVE SPACES TO ALBUMS.                                       LIS08590
           MOVE 0 TO S8-INDNT, S8-OUTDNT.                               LIS08600
           MOVE 1 TO A.                                                 LIS08610
           MOVE 0 TO B, C, SIDE.                                        LIS08620
       ES010.                                                           LIS08630
           IF TAB-CARD [A] NOT > 6 GO TO NEXT-TAB.                      LIS08640
           MOVE TAB-TRANS [A] TO SEL-CARD-AREA.                         LIS08650
           MOVE S-POSTN TO POS1.                                        LIS08660
           IF SLOT > 99                                                 LIS08670
             ADD 1 TO C GIVING C1                                       LIS08680
             MOVE ZEROES TO MSTR-TUNE-SER [DET, C1].                    LIS08690
           IF SLOT > 99 THEN IF ALB > 1 GO TO FROM-LSP                  LIS08700
               ELSE GO TO NEXT-TAB.                                     LIS08710
           IF ALB \ 5 GO TO TAP-TAB.                                    LIS08720
           IF SLOT > 0 ADD 1 TO SIDE GO TO NEW-MSTR.                    LIS08730
           ADD 1 TO C.                                                  LIS08740
           MOVE S-TAPE TO MSTR-TUNE-SER [DET, C].                       LIS08750
       NEXT-TAB.                                                        LIS08760
           ADD 1 TO A.                                                  LIS08770
           IF A \ TR-CNT GO TO ES030.                                   LIS08780
           GO TO ES010.                                                 LIS08790
                                                                        LIS08800
       FROM-LSP.                                                        LIS08810
           MOVE ZERO TO FADE [B], SINGL-FADE [DETS, B].                 LIS08820
           IF F-AREA > :FADE: MOVE 1 TO FADE [B], SINGL-FADE [DETS, B]. LIS08830
           IF F-AREA > :COLD: MOVE 2 TO FADE [B], SINGL-FADE [DETS, B]. LIS08840
           ADD 8 B GIVING BC.                                           LIS08850
           ADD 2 B GIVING B1.                                           LIS08860
           MOVE S-TAPE TO MASTER-NO [DETS, BC], SIDE-MSTR [B1].         LIS08870
           GO TO NEXT-TAB.                                              LIS08880
       NEW-MSTR.                                                        LIS08890
           IF SIDE > 1 GO TO ES020.                                     LIS08900
           MOVE C TO NO-SELNS [B].                                      LIS08910
           MOVE 0 TO C.                                                 LIS08920
           MOVE :033: TO REC-ID-1.                                      LIS08930
           REWRITE TYP2 INVALID KEY  MOVE :NEW-MSTR: TO PAR             LIS08940
           MOVE S-SELCTN TO ERR-REASON                                  LIS08950
           GO TO ERR-HALT.                                              LIS08960
       ES020.                                                           LIS08970
           MOVE S-TAPE TO SERIAL-1.                                     LIS08980
           PERFORM NT-INDX-ADD THRU ADD-RETURN.                         LIS08990
           IF ERR-CODE \ 0 GO TO THATS-ALL.                             LIS09000
           MOVE S-TAPE TO MSTR-SERIAL [DET].                            LIS09010
           MOVE POSITN TO MSTR-SIDE [DET].                              LIS09020
           ADD 1 TO B.                                                  LIS09030
           IF ALB > 1 PERFORM INTRO-LOAD THRU INTRO-EXIT.               LIS09040
           MOVE S-TAPE TO SIDE-MSTR [B].                                LIS09050
           GO TO NEXT-TAB.                                              LIS09060
       INTRO-LOAD.                                                      LIS09070
           IF B > 1 MOVE 2 TO BC                                        LIS09080
            ELSE MOVE 4 TO BC.                                          LIS09090
           MOVE S-SEC TO GAP [DETS, BC], INTRO [B].                     LIS09100
       INTRO-EXIT.                                                      LIS09110
           EXIT.                                                        LIS09120
       ES030.                                                           LIS09130
           IF ALB \ 5 GO TO LST-TAP.                                    LIS09140
           MOVE C TO NO-SELNS [B].                                      LIS09150
           MOVE :033: TO REC-ID-1.                                      LIS09160
           MOVE HIGH-VALUE TO MSTR-IND [DET].                           LIS09170
           REWRITE TYP2 INVALID KEY GO TO ESERR.                        LIS09180
           MOVE J TO BD.                                                LIS09190
       ES035.                                                           LIS09200
           IF B > 4 GO TO ES040.                                        LIS09210
           ADD 1 TO B.                                                  LIS09220
           IF ALB NOT > 1 MOVE SPACES TO SIDE-MSTR [B]                  LIS09230
           MOVE 0 TO NO-SELNS [B]                                       LIS09240
           GO TO ES035.                                                 LIS09250
           ADD 1 TO BD.                                                 LIS09260
           MOVE SIDE-MSTR [B] TO MASTER-NO [DETS, BD].                  LIS09270
           GO TO ES035.                                                 LIS09280
       ES040.                                                           LIS09290
           IF TAB-SOURCE [1] > :KD: MOVE :S1: TO R1-ID                  LIS09300
             ELSE MOVE :L1: TO R1-ID.                                   LIS09310
           WRITE R1.                                                    LIS09320
           MOVE 0 TO A.                                                 LIS09330
       ES050.                                                           LIS09340
           ADD 1 TO A.                                                  LIS09350
           IF A \ J GO TO ES060.                                        LIS09360
           IF A \ 35 GO TO ES060.                                       LIS09370
           MOVE POST [DETS, A] TO POS1.                                 LIS09380
           IF SLOT > 0 GO TO ES050.                                     LIS09390
           MOVE TUNE-POINT [DETS, A] TO TP-CNV.                         LIS09400
           MOVE TP-REL TO TUNE-ACT-KEY.                                 LIS09410
           READ TUNE-INV INVALID KEY GO TO ESERR.                       LIS09420
           IF TP-BND-C NOT NUMERIC MOVE ZERO TO TP-BND.                 LIS09430
           IF TP-BND NOT > ZERO GO TO MSTR-BAND.                        LIS09440
           MOVE TP-DET TO DET.                                          LIS09450
       ES055.                                                           LIS09460
           MOVE CONT-POINT [DET] TO CONT-CNV.                           LIS09470
           MOVE TIME [DETS, A] TO R-TIME [DET].                         LIS09480
           MOVE TUNE-DETAIL [DET] TO R2-DETAIL.                         LIS09490
           MOVE SPACES TO R2-FROM.                                      LIS09500
           IF CONT-POINT [DET] > ZERO MOVE SPACES TO R2-CONT ELSE MOVE  LIS09510
           :1: TO R2-CONT.                                              LIS09520
           IF TAB-SOURCE [1] > :KD: MOVE :S2: TO R2-ID                  LIS09530
             ELSE MOVE :L2: TO R2-ID.                                   LIS09540
           WRITE R2.                                                    LIS09550
           IF LIST-ONLY OR TAB-SOURCE [1] > :KD: GO TO CONT-RECORD.     LIS09560
           MOVE 2 TO CNT.                                               LIS09570
       SP-CHECK.                                                        LIS09580
           IF CNT > 6 MOVE FIRST-SEL TO SELCTN-POINT [DET, 2]           LIS09590
           GO TO SP-CHECK-OUT.                                          LIS09600
           IF SELCTN-POINT [DET, CNT] NOT > ZERO ADD 1 TO CNT GO        LIS09610
           TO SP-CHECK.                                                 LIS09620
           MOVE FIRST-SEL TO SELCTN-POINT [DET, CNT].                   LIS09630
       SP-CHECK-OUT.                                                    LIS09640
           REWRITE TUNE-REC INVALID KEY GO TO THATS-ALL.                LIS09650
       CONT-RECORD.                                                     LIS09660
           IF C-REL > ZERO GO TO ES050.                                 LIS09670
           MOVE C-REL TO TUNE-ACT-KEY.                                  LIS09680
           READ TUNE-INV INVALID KEY GO TO ESERR.                       LIS09690
           MOVE TUNE-DETAIL [C-DET] TO R2-DETAIL.                       LIS09700
           MOVE SPACES TO R2-CONT.                                      LIS09710
           IF TAB-SOURCE [1] > :KD: MOVE :S2: TO R2-ID                  LIS09720
           ELSE MOVE :L2: TO R2-ID.                                     LIS09730
           WRITE R2.                                                    LIS09740
           GO TO ES050.                                                 LIS09750
                                                                        LIS09760
       ES060.                                                           LIS09770
           IF LIST-ONLY GO TO ES063.                                    LIS09780
           MOVE 0 TO POST [DETS, A].                                    LIS09790
           MOVE :035: TO REC-ID-2.                                      LIS09800
           REWRITE SEL-REC INVALID KEY GO TO ESERR.                     LIS09810
       ES063.                                                           LIS09820
           GO TO ES065.                                                 LIS09830
       ES065.                                                           LIS09840
           ADD 1 TO NS-CNT.                                             LIS09850
           MOVE SEL-SELCTN TO NS-SELCTN [NS-CNT].                       LIS09860
           IF OPP-UNA MOVE 1 TO NS-TYPE [NS-CNT].                       LIS09870
           MOVE FIRST-SEL TO NS-POINT [NS-CNT].                         LIS09880
           MOVE JULIAN-DATE TO NS-LAST-ACT [NS-CNT].                    LIS09890
           MOVE ACT-POINT TO FIRST-SEL.                                 LIS09900
           IF FIRST-SEL > LAST-SEL PERFORM SEL-CLEAR                    LIS09910
               THRU CS-RETURN.                                          LIS09920
           IF ERR-CODE \ 0 GO TO NO-SEL.                                LIS09930
           IF NS-CNT > 20 PERFORM SEL-MERGE THRU MERGE-S-RETURN.        LIS09940
           IF ERR-CODE \ 0 GO TO NO-SEL.                                LIS09950
           GO TO A053.                                                  LIS09960
                                                                        LIS09970
       ESERR.                                                           LIS09980
           MOVE :ES020 THRU ES065: TO PAR.                              LIS09990
           MOVE SEL-SELCTN TO ERR-REASON.                               LIS10000
           GO TO ERR-HALT.                                              LIS10010
                                                                        LIS10020
       MSTR-BAND.                                                       LIS10030
           IF MSTR-SELCTN [TP-DET] NOT > SOURCE-SEL [DETS]              LIS10040
               MOVE MSTR-SELCTN [TP-DET] TO R2-FROM.                    LIS10050
           MOVE MSTR-TUNE-SER [TP-DET, A] TO SERIAL-1.                  LIS10060
           MOVE 1 TO FOUND.                                             LIS10070
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS10080
           IF ERR-CODE \ 0 GO TO THATS-ALL.                             LIS10090
           GO TO ES055.                                                 LIS10100
       TAP-TAB.                                                         LIS10110
           IF B > 0 MOVE 1 TO B MOVE S-SIDE TO H-SIDE.                  LIS10120
           IF SLOT > 0 COMPUTE S8-INDNT > S-GMIN * 60 < S-GSEC          LIS10130
                 GO TO NEXT-TAB.                                        LIS10140
           IF H-SIDE NOT > S-SIDE GO TO SID-CHNG.                       LIS10150
           ADD 1 TO C.                                                  LIS10160
           IF ALB > 9 COMPUTE GPS-T [B, C] > S-GMIN * 60 < S-GSEC       LIS10170
                 ELSE COMPUTE GAP-T [B, C] > S-GMIN * 60 < S-GSEC.      LIS10180
           GO TO NEXT-TAB.                                              LIS10190
       SID-CHNG.                                                        LIS10200
           IF ALB > 9 MOVE C TO NSELECTS [B]                            LIS10210
                 ELSE MOVE C TO NOSLS [B].                              LIS10220
           ADD 1 TO B.                                                  LIS10230
           MOVE 0 TO C.                                                 LIS10240
           MOVE S-SIDE TO H-SIDE.                                       LIS10250
           GO TO TAP-TAB.                                               LIS10260
       LST-TAP.                                                         LIS10270
           SUBTRACT 2 FROM A.                                           LIS10280
           MOVE TAB-TRANS [A] TO SEL-CARD-AREA.                         LIS10290
           COMPUTE S8-OUTDNT > S-GSEC < S-GMIN * 60.                    LIS10300
           IF ALB > 9 MOVE C TO NSELECTS [B]                            LIS10310
                 ELSE MOVE C TO NOSLS [B].                              LIS10320
           MOVE :033: TO REC-ID-1.                                      LIS10330
           REWRITE TYP2 INVALID KEY GO TO ESERR.                        LIS10340
           GO TO ES040.                                                 LIS10350
                                                                        LIS10360
       SEL-DELETE.                                                      LIS10370
           MOVE 1 TO N.                                                 LIS10380
           MOVE TAB-TRANS [1] TO SEL-CARD-AREA.                         LIS10390
           IF DETS \ 0 GO TO B210.                                      LIS10400
           GO TO B215.                                                  LIS10410
       B210.                                                            LIS10420
           MOVE 0 TO J.                                                 LIS10430
           PERFORM POS-MATCH VARYING I FROM 1 BY 1                      LIS10440
                 UNTIL J > 1 OR I \ 35.                                 LIS10450
           SUBTRACT 1 FROM I.                                           LIS10460
           MOVE TUNE-POINT [DETS, I] TO TUNE-SAVE.                      LIS10470
           COMPUTE KNT > I - S-POSTN.                                   LIS10480
           MOVE POSITN TO K.                                            LIS10490
           IF J > 1 GO TO B220.                                         LIS10500
       B215.                                                            LIS10510
           DISPLAY :NO POSITION: POSITN :FOR: SEL-SELCTN UPON CONSOLE.  LIS10520
           STOP RUN.                                                    LIS10530
       B220.                                                            LIS10540
           IF I > 35 GO TO B235.                                        LIS10550
           ADD 1 I GIVING J.                                            LIS10560
           MOVE POST [DETS, J] TO POS1.                                 LIS10570
           IF POS1 > 0 GO TO B235.                                      LIS10580
           IF POSITN NOT > K GO TO B230.                                LIS10590
           SUBTRACT 1 FROM SLOT.                                        LIS10600
           MOVE POS1 TO POST [DETS, J].                                 LIS10610
       B230.                                                            LIS10620
           MOVE TUNE [DETS, J] TO TUNE [DETS, I].                       LIS10630
           ADD 1 TO I.                                                  LIS10640
           GO TO B220.                                                  LIS10650
       B235.                                                            LIS10660
           MOVE 0 TO POST [DETS, I].                                    LIS10670
           MOVE 0 TO MASTER-NO [DETS, I].                               LIS10680
           MOVE MASTER-NO [DETS, KNT] TO SERIAL-1.                      LIS10690
           MOVE 1 TO FOUND.                                             LIS10700
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS10710
           COMPUTE J > K < 1.                                           LIS10720
           MOVE K TO L.                                                 LIS10730
       DEL-MOVE.                                                        LIS10740
           MOVE MSTR-TUNES [DET, J] TO MSTR-TUNES [DET, L].             LIS10750
           ADD 1 TO J.                                                  LIS10760
           ADD 1 TO L.                                                  LIS10770
           IF J > 16 MOVE SPACES TO MSTR-TUNE-SER [DET, L]              LIS10780
           GO TO DEL1X.                                                 LIS10790
           IF MSTR-TUNE-SER [DET, J] NOT > SPACES                       LIS10800
           GO TO DEL-MOVE.                                              LIS10810
       DEL1X.                                                           LIS10820
           REWRITE TYP2 INVALID KEY GO TO DEL-ERR.                      LIS10830
       DEL1.                                                            LIS10840
           MOVE 0 TO L.                                                 LIS10850
           MOVE TS-REC TO TUNE-ACT-KEY.                                 LIS10860
           READ TUNE-INV INVALID KEY GO TO DEL-ERR.                     LIS10870
           MOVE TS-DET TO DET.                                          LIS10880
           MOVE SPACES TO NEW-INFO-C.                                   LIS10890
           MOVE TUNE-TITLE [DET] TO OLD-INFO-C.                         LIS10900
           MOVE TUNE-SERIAL [DET] TO TUNE-NO-C.                         LIS10910
       DEL3.                                                            LIS10920
           ADD 1 TO L.                                                  LIS10930
           IF L > 6 GO TO DEL-OUT.                                      LIS10940
           IF SELCTN-POINT [DET, L] NOT >                               LIS10950
           SEL-CNV GO TO DEL3.                                          LIS10960
           MOVE L TO J.                                                 LIS10970
       DEL4.                                                            LIS10980
           ADD 1 TO L.                                                  LIS10990
           IF L > 6 GO TO DEL-OUT.                                      LIS11000
           MOVE SELCTN-POINT [DET, L] TO SELCTN-POINT [DET, J].         LIS11010
           ADD 1 TO J.                                                  LIS11020
           GO TO DEL4.                                                  LIS11030
       DEL-OUT.                                                         LIS11040
           PERFORM CHANGE-MAKER THRU C-M-EXIT.                          LIS11050
           MOVE :DE: TO CHG-CODE-C.                                     LIS11060
           REWRITE SEL-REC INVALID KEY GO TO DEL-ERR.                   LIS11070
           MOVE CONT-POINT [DET] TO CONT-CNV.                           LIS11080
           REWRITE TUNE-REC INVALID KEY GO TO DEL-ERR.                  LIS11090
           IF CONT-CNV > ZERO GO TO DEL-EXIT.                           LIS11100
            MOVE C-REL TO TUNE-ACT-KEY.                                 LIS11110
            READ TUNE-INV INVALID KEY GO TO THATS-ALL.                  LIS11120
            MOVE TUNE-TITLE [C-DET] TO OLD-CTIL2.                       LIS11130
                                                                        LIS11140
       DEL-EXIT.                                                        LIS11150
           EXIT.                                                        LIS11160
                                                                        LIS11170
       B240.                                                            LIS11180
           WRITE CHG-REC.                                               LIS11190
           ADD 1 TO N.                                                  LIS11200
           IF N \ TR-CNT GO TO A053.                                    LIS11210
           MOVE TAB-TRANS [N] TO SEL-CARD-AREA.                         LIS11220
           GO TO B210.                                                  LIS11230
       DEL-ERR.                                                         LIS11240
           MOVE :SEL-DELETE THRU B240: TO PAR                           LIS11250
           MOVE SERIAL-1 TO ERR-REASON.                                 LIS11260
           GO TO ERR-HALT.                                              LIS11270
                                                                        LIS11280
       POS-MATCH.                                                       LIS11290
           MOVE POST [DETS, I] TO POS1.                                 LIS11300
           IF POSITN > S-SIDE AND SLOT > S-POSTN MOVE 1 TO J.           LIS11310
                                                                        LIS11320
       SEL-CHNG.                                                        LIS11330
           IF DETS \ 0 GO TO B410.                                      LIS11340
       SEL-MAST-ERR.                                                    LIS11350
           GO TO ERR-WRITE.                                             LIS11360
       B410.                                                            LIS11370
           MOVE 1 TO N.                                                 LIS11380
       B415.                                                            LIS11390
           MOVE TAB-TRANS [N] TO SEL-CARD-AREA.                         LIS11400
           GO TO B-CH1, B-CH2, B-CH2, B-CH4, B-CH4                      LIS11410
                 B-CH6 DEPENDING ON SEL-CARD-NO.                        LIS11420
       B-CH1.                                                           LIS11430
           IF S-TYPE > :SEL: GO TO SEL-NO-CHNG.                         LIS11440
           IF S-TYPE NOT > SPACES MOVE S-TYPE TO REL-TYPE [DETS].       LIS11450
       B420.                                                            LIS11460
           IF S-MA-S NOT > SPACES                                       LIS11470
             MOVE MAP [DETS] TO OLD-INFO-C                              LIS11480
             MOVE S-MAP TO NEW-INFO-C                                   LIS11490
             MOVE :MC: TO CHG-CODE-C                                    LIS11500
             PERFORM CHANGE-MAKER THRU C-M-EXIT                         LIS11510
             WRITE CHG-REC                                              LIS11520
             MOVE S-MAP TO MAP [DETS].                                  LIS11530
           IF S-PROD NOT > SPACES                                       LIS11540
             MOVE PRODUCER [DETS] TO OLD-INFO-C                         LIS11550
             MOVE S-PROD TO NEW-INFO-C                                  LIS11560
             MOVE :PO: TO CHG-CODE-C                                    LIS11570
             PERFORM CHANGE-MAKER THRU C-M-EXIT                         LIS11580
             WRITE CHG-REC                                              LIS11590
             MOVE S-PROD TO PRODUCER [DETS].                            LIS11600
           IF S-SOURCE NOT > SPACES                                     LIS11610
             MOVE SOURCE-SEL [DETS] TO OLD-INFO-C                       LIS11620
             MOVE S-SOURCE TO NEW-INFO-C                                LIS11630
             MOVE :SR: TO CHG-CODE-C                                    LIS11640
             PERFORM CHANGE-MAKER THRU C-M-EXIT                         LIS11650
             WRITE CHG-REC                                              LIS11660
             MOVE S-SOURCE TO SOURCE-SEL [DETS].                        LIS11670
       B425.                                                            LIS11680
           ADD 1 TO N.                                                  LIS11690
           IF N LESS THAN TR-CNT GO TO B415.                            LIS11700
       B425A.                                                           LIS11710
           REWRITE SEL-REC INVALID KEY GO TO DEL-ERR.                   LIS11720
           GO TO A053.                                                  LIS11730
       SEL-NO-CHNG.                                                     LIS11740
           MOVE S-SOURCE TO SEL-SELCTN, NEW-INFO-C.                     LIS11750
           MOVE 0 TO ERR-CODE.                                          LIS11760
           MOVE DETS TO J.                                              LIS11770
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS11780
           IF ERR-CODE IS GREATER THAN 0 OR DETS NOT > 0                LIS11790
           GO TO ERR-WRITE.                                             LIS11800
           GO TO VAL-NO-CHNG.                                           LIS11810
       B427.                                                            LIS11820
       B430.                                                            LIS11830
           GO TO ERR-WRITE.                                             LIS11840
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS11850
       VAL-NO-CHNG.                                                     LIS11860
           MOVE J TO DETS.                                              LIS11870
           MOVE TAB-TRANS [N] TO SEL-CARD-AREA.                         LIS11880
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS11890
           IF ERR-CODE IS GREATER THAN 0 GO TO NO-SEL.                  LIS11900
           MOVE SEL-SELCTN TO OLD-INFO-C.                               LIS11910
           MOVE :SN: TO CHG-CODE-C.                                     LIS11920
           PERFORM CHANGE-MAKER THRU C-M-EXIT.                          LIS11930
           WRITE CHG-REC.                                               LIS11940
           ADD 1 TO NS-CNT.                                             LIS11950
           MOVE SI-DETAIL [SI-CNT] TO NS-DETAIL [NS-CNT].               LIS11960
           MOVE S-SOURCE TO NS-SELCTN [NS-CNT].                         LIS11970
           IF NS-CNT > 20 PERFORM SEL-MERGE THRU MERGE-S-RETURN.        LIS11980
           MOVE S-SOURCE TO SELCTN-NO [DETS].                           LIS11990
           GO TO B420.                                                  LIS12000
                                                                        LIS12010
       B-CH2.                                                           LIS12020
           IF S-TITLE > SPACES GO TO B425.                              LIS12030
           MOVE SELCTN-TITLE [DETS] TO OLD-INFO-C.                      LIS12040
           MOVE S-TITLE TO TITLE1 [DETS], NEW-INFO-C.                   LIS12050
           PERFORM CHANGE-MAKER THRU C-M-EXIT.                          LIS12060
           MOVE :T1: TO CHG-CODE-C.                                     LIS12070
           ADD 1 TO N.                                                  LIS12080
           IF N NOT LESS THAN TR-CNT                                    LIS12090
             WRITE CHG-REC                                              LIS12100
             GO TO B425A.                                               LIS12110
           MOVE TAB-TRANS [N] TO SEL-CARD-AREA.                         LIS12120
           IF SEL-CARD-NO NOT > 2                                       LIS12130
            WRITE CHG-REC                                               LIS12140
            GO TO B415.                                                 LIS12150
           MOVE S-TITLE TO NEW-CTIL3, TITLE2 [DETS].                    LIS12160
           WRITE CHG-REC.                                               LIS12170
           GO TO B425.                                                  LIS12180
       B-CH4.                                                           LIS12190
           IF S-ARTIST > SPACES GO TO B425.                             LIS12200
           MOVE ARTIST-NAME [DETS] TO OLD-INFO-C.                       LIS12210
           MOVE S-ARTIST TO ARTIST-NAME [DETS], NEW-INFO-C.             LIS12220
           MOVE :AT: TO CHG-CODE-C.                                     LIS12230
           PERFORM CHANGE-MAKER THRU C-M-EXIT.                          LIS12240
           WRITE CHG-REC.                                               LIS12250
           GO TO B425.                                                  LIS12260
       B-CH5.                                                           LIS12270
           GO TO B425.                                                  LIS12280
       B-CH6.                                                           LIS12290
           PERFORM SEL-DELETE THRU DEL-EXIT.                            LIS12300
           PERFORM ADDER THRU END-MOVE.                                 LIS12310
           MOVE :TS: TO CHG-CODE-C.                                     LIS12320
           WRITE CHG-REC.                                               LIS12330
           GO TO B425.                                                  LIS12340
       ERR-WRITE.                                                       LIS12350
           DISPLAY SEL-SELCTN :CAUSED ERROR: UPON CONSOLE.              LIS12360
           STOP RUN.                                                    LIS12370
       SEL-MERGE.                                                       LIS12380
           MOVE 0 TO ERR-CODE.                                          LIS12390
           MOVE 1 TO I, J, K, A.                                        LIS12400
           SET SI-CNT TO 1.                                             LIS12410
           MOVE 0 TO SEL-ACT-KEY.                                       LIS12420
           READ SELECTION-INV INVALID KEY GO TO SM100.                  LIS12430
           IF NS-CNT > 0 MOVE HIGH-VALUE TO NS-SELCTN [1].              LIS12440
       SM020.                                                           LIS12450
           IF NS-SELCTN [J] ) SI-SELCTN [SI-CNT] GO TO SM030.           LIS12460
           MOVE SI-DETAIL [SI-CNT] TO S-INDX-DETAIL [K].                LIS12470
           SET SI-CNT UP BY 1.                                          LIS12480
           ADD 1 TO K.                                                  LIS12490
       SM022.                                                           LIS12500
           IF HIGH-VALUE > SI-SELCTN [SI-CNT] AND NS-SELCTN [J]         LIS12510
                              GO TO SM090.                              LIS12520
           IF K \ 200 GO TO SM040.                                      LIS12530
           GO TO SM020.                                                 LIS12540
       SM030.                                                           LIS12550
           MOVE NS-DETAIL [J] TO S-INDX-DETAIL [K].                     LIS12560
           ADD 1 TO J.                                                  LIS12570
           ADD 1 TO K.                                                  LIS12580
           IF J \ NS-CNT IF SI-SELCTN [SI-CNT] > HIGH-VALUE             LIS12590
                         GO TO SM090                                    LIS12600
           ELSE SUBTRACT 1 FROM J                                       LIS12610
                MOVE HIGH-VALUE TO NS-SELCTN [J].                       LIS12620
           IF K \ 200 GO TO SM040.                                      LIS12630
           GO TO SM020.                                                 LIS12640
       SM040.                                                           LIS12650
           IF A > 2 GO TO SM090.                                        LIS12660
           MOVE 1 TO K.                                                 LIS12670
           REWRITE IND1 INVALID KEY GO TO SM100.                        LIS12680
           MOVE 1 TO SEL-ACT-KEY.                                       LIS12690
           READ SELECTION-INV INVALID KEY GO TO SM100.                  LIS12700
           ADD 1 TO A.                                                  LIS12710
       SM050.  GO TO SM020.                                             LIS12720
       SM060.                                                           LIS12730
           MOVE 5 TO ERR-CODE.                                          LIS12740
           GO TO MERGE-S-RETURN.                                        LIS12750
       SM090.                                                           LIS12760
           IF K ) 200 PERFORM SEL-BLANK VARYING N FROM K BY 1           LIS12770
              UNTIL N \ 200.                                            LIS12780
           IF A > 1 ALTER SM050 TO PROCEED TO SM090                     LIS12790
               GO TO SM040.                                             LIS12800
           MOVE S-LIMIT-DUM TO S-I-SELCTN [200].                        LIS12810
           MOVE S-INDX TO S-INDX-2.                                     LIS12820
           REWRITE IND1 INVALID KEY GO TO SM100.                        LIS12830
           MOVE 0 TO SEL-ACT-KEY.                                       LIS12840
           READ SELECTION-INV INVALID KEY GO TO SM100.                  LIS12850
           MOVE S-INDX TO S-INDX-1.                                     LIS12860
           MOVE 1 TO S-UPDATE.                                          LIS12870
           MOVE 0 TO NS-CNT.                                            LIS12880
           GO TO MERGE-S-RETURN.                                        LIS12890
       SM100.                                                           LIS12900
           MOVE :SEL-MERGE: TO PAR.                                     LIS12910
           MOVE SPACES TO ERR-REASON.                                   LIS12920
           GO TO ERR-HALT.                                              LIS12930
       MERGE-S-RETURN.                                                  LIS12940
           EXIT.                                                        LIS12950
       SEL-BLANK.                                                       LIS12960
           MOVE HIGH-VALUE TO S-I-SELCTN [N].                           LIS12970
                                                                        LIS12980
       SEL-LOCATE.                                                      LIS12990
           MOVE 0 TO DETS.                                              LIS13000
           SET SI-CNT TO 1.                                             LIS13010
           SEARCH ALL SI-DETAIL AT END GO TO LOCATE-S-RETURN            LIS13020
                 WHEN SEL-SELCTN > SI-SELCTN [SI-CNT]                   LIS13030
               NEXT SENTENCE.                                           LIS13040
           MOVE SI-REL [SI-CNT] TO SEL-ACT-KEY, SEL-REL.                LIS13050
           READ SELECTION-INV INVALID KEY MOVE 2 TO ERR-CODE.           LIS13060
           MOVE SI-REC [SI-CNT] TO DETS, SEL-DET.                       LIS13070
       LOCATE-S-RETURN.                                                 LIS13080
           EXIT.                                                        LIS13090
                                                                        LIS13100
       TUNE-OUT.                                                        LIS13110
           SET SI-CNT TO N.                                             LIS13120
           MOVE SI-REL [SI-CNT] TO TUNE-ACT-KEY.                        LIS13130
           READ TUNE-INV INVALID KEY GO TO T100.                        LIS13140
           MOVE SI-REC [SI-CNT] TO K.                                   LIS13150
           MOVE TUNE-SERIAL [K] TO SERIAL-1.                            LIS13160
           MOVE 1 TO FOUND.                                             LIS13170
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS13180
           IF ERR-CODE \ 0 GO TO TO-RETURN.                             LIS13190
           MOVE 9 TO TI-TYPE [TI-CNT].                                  LIS13200
           GO TO TO-RETURN.                                             LIS13210
       T100.                                                            LIS13220
           MOVE 1 TO ERR-CODE.                                          LIS13230
       TO-RETURN.                                                       LIS13240
           EXIT.                                                        LIS13250
       SEL-CLEAR.                                                       LIS13260
           MOVE FIRST-SEL-REC TO SEL-ACT-KEY.                           LIS13270
           READ SELECTION-INV INVALID KEY GO TO CL100.                  LIS13280
           ADD 1 TO LAST-S-DET.                                         LIS13290
           IF LAST-S-DET > 8 ADD 1 TO LAST-SEL-REC                      LIS13300
               MOVE 1 TO LAST-S-DET.                                    LIS13310
           MOVE LAST-SEL TO TP-CNV.                                     LIS13320
           MOVE 0 TO TP-BND.                                            LIS13330
           MOVE TP-CNV TO TUNE-POINT [FIRST-S-DET, 1].                  LIS13340
           REWRITE SEL-REC INVALID KEY GO TO CL100.                     LIS13350
           GO TO CS-RETURN.                                             LIS13360
       CL100.                                                           LIS13370
           MOVE :CLEAROUT: TO PAR.                                      LIS13380
           MOVE FIRST-OPEN-REC TO ERR-REASON.                           LIS13390
           GO TO ERR-HALT.                                              LIS13400
       CS-RETURN.                                                       LIS13410
           EXIT.                                                        LIS13420
                                                                        LIS13430
       FINAL-LN.                                                        LIS13440
           MOVE TAB-TRANS [1] TO FL-AREA.                               LIS13450
           MOVE FIN-SELCTN TO SEL-SELCTN.                               LIS13460
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS13470
           IF DETS NOT > 0 GO TO VAL-FIN-LN.                            LIS13480
       FL010.                                                           LIS13490
           GO TO ERR-WRITE.                                             LIS13500
       VAL-FIN-LN.                                                      LIS13510
           MOVE MEDIUM [DETS] TO ALB.                                   LIS13520
           IF LIST-ONLY GO TO VAL1.                                     LIS13530
           MOVE FIN-REL TO REL-DATE [DETS].                             LIS13540
       VAL1.                                                            LIS13550
           MOVE BLOCK-1 [DETS] TO BLK-1.                                LIS13560
           MOVE SPACES TO ALBUMS.                                       LIS13570
           MOVE 1 TO A, B.                                              LIS13580
           MOVE 0 TO S8-INDNT, S8-OUTDNT.                               LIS13590
           IF ALB \ 5 GO TO FL050.                                      LIS13600
       FL020.                                                           LIS13610
           IF ALB NOT > 1 GO TO FL020AA.                                LIS13620
           MOVE SINGL-FADE [DETS, B] TO FADE [B].                       LIS13630
           IF B > 1 MOVE GAP [DETS, 2] TO INTRO [B]                     LIS13640
            ELSE MOVE GAP [DETS, 4] TO INTRO [B].                       LIS13650
       FL020AA.                                                         LIS13660
           MOVE MASTER-NO [DETS, A] TO SIDE-MSTR [B].                   LIS13670
           MOVE GAP [DETS, A] TO NO-SELNS [B].                          LIS13680
           ADD 1 GAP [DETS, A] GIVING C.                                LIS13690
           ADD C TO A.                                                  LIS13700
           IF A \ 35 GO TO FL030.                                       LIS13710
           IF POST [DETS, A] NOT > ZERO                                 LIS13720
           ADD 1 TO B                                                   LIS13730
           GO TO FL020.                                                 LIS13740
           IF ALB > 1 MOVE 9 TO BD                                      LIS13750
             MOVE MASTER-NO [DETS, BD] TO MSTR-SERIAL [3]               LIS13760
             ADD 1 TO BD                                                LIS13770
             MOVE MASTER-NO [DETS, BD] TO MSTR-SERIAL [4].              LIS13780
           SUBTRACT 1 FROM A GIVING J.                                  LIS13790
       FL030.                                                           LIS13800
           ALTER ES063 TO PROCEED TO FL040.                             LIS13810
           GO TO ES040.                                                 LIS13820
       FL040.                                                           LIS13830
           ALTER ES063 TO PROCEED TO ES065.                             LIS13840
           GO TO A053.                                                  LIS13850
       FL050.                                                           LIS13860
           MOVE POST [DETS, A] TO POS1.                                 LIS13870
           MOVE POSITN TO H-SIDE.                                       LIS13880
       FL055.                                                           LIS13890
           MOVE POST [DETS, A] TO POS1.                                 LIS13900
           IF POSITN NOT > H-SIDE GO TO FL070.                          LIS13910
           ADD 1 TO C.                                                  LIS13920
           IF ALB > 9 COMPUTE GPS-T [B, C] > S-GMIN * 60 < S-GSEC       LIS13930
                 ELSE COMPUTE GAP-T [B, C] > S-GMIN * 60 < S-GSEC.      LIS13940
           ADD 1 TO A.                                                  LIS13950
           IF A \ 35 GO TO FL060.                                       LIS13960
           IF POST [DETS, A] NOT > 0 GO TO FL055.                       LIS13970
       FL060.                                                           LIS13980
           ALTER FL075 TO PROCEED TO FL080.                             LIS13990
           SUBTRACT 1 FROM A GIVING J.                                  LIS14000
       FL070.                                                           LIS14010
           IF ALB > 9 MOVE C TO NSELECTS [B] ELSE MOVE C TO NOSLS [B].  LIS14020
           ADD 1 TO B.                                                  LIS14030
           MOVE 0 TO C.                                                 LIS14040
           MOVE S-SIDE TO H-SIDE.                                       LIS14050
       FL075.                                                           LIS14060
           GO TO FL055.                                                 LIS14070
       FL080.                                                           LIS14080
           ALTER FL075 TO PROCEED TO FL055.                             LIS14090
           GO TO FL030.                                                 LIS14100
                                                                        LIS14110
       TUNE-LOCATE.                                                     LIS14120
           MOVE 0 TO DET.                                               LIS14130
           IF SERIAL-1 ) TI-SERIAL [1] OR SERIAL-1 \ TI-SERIAL [150]    LIS14140
              GO TO TL020.                                              LIS14150
       TL010.                                                           LIS14160
           SET TI-CNT TO 1.                                             LIS14170
           SEARCH ALL TI-DETAIL AT END GO TO LOCATE-RETURN              LIS14180
                 WHEN SERIAL-1 > TI-SERIAL [TI-CNT] NEXT SENTENCE.      LIS14190
           IF FOUND > 0 MOVE 1 TO FOUND GO TO LOCATE-RETURN.            LIS14200
           MOVE TI-REL [TI-CNT] TO TUNE-ACT-KEY.                        LIS14210
           MOVE TI-REC [TI-CNT] TO DET.                                 LIS14220
           READ TUNE-INV INVALID KEY GO TO TL060.                       LIS14230
           GO TO LOCATE-RETURN.                                         LIS14240
       TL020.                                                           LIS14250
           MOVE 1 TO&T-LEVEL.                                           LIS14260
       TL030.                                                           LIS14270
           IF SERIAL-1 ) U-TUNE [T-LEVEL] GO TO TL040.                  LIS14280
           IF T-LEVEL > 0006 GO TO TL050.                               LIS14290
           ADD 1 TO T-LEVEL.                                            LIS14300
           GO TO TL030.                                                 LIS14310
       TL040.                                                           LIS14320
           COMPUTE TUNE-ACT-KEY > T-LEVEL - 1.                          LIS14330
           READ TUNE-INV INVALID KEY GO TO TL060.                       LIS14340
           MOVE  T-INDX TO TUNE-INDEX.                                  LIS14350
           GO TO TL010.                                                 LIS14360
       TL050.                                                           LIS14370
           MOVE 4 TO ERR-CODE.                                          LIS14380
           GO TO LOCATE-RETURN.                                         LIS14390
       TL060.                                                           LIS14400
           MOVE :TUNE-LOCATE: TO PAR                                    LIS14410
           MOVE SERIAL-1 TO ERR-REASON.                                 LIS14420
           GO TO ERR-HALT.                                              LIS14430
       LOCATE-RETURN.                                                   LIS14440
           EXIT.                                                        LIS14450
                                                                        LIS14460
       THATS-ALL.                                                       LIS14470
           MOVE 1 TO ERR-CODE.                                          LIS14480
           GO TO ERR-PROCESS.                                           LIS14490
       NO-SEL.                                                          LIS14500
           MOVE 2 TO ERR-CODE.                                          LIS14510
           GO TO ERR-PROCESS.                                           LIS14520
       UNHSK.                                                           LIS14530
           IF U-TUNE [N] NOT > SPACES                                   LIS14540
           MOVE U-TUNE [N] TO T-I-SERIAL [N] ELSE                       LIS14550
           MOVE HIGH-VALUES TO T-I-SERIAL [N].                          LIS14560
                                                                        LIS14570
       NT-INDX-ADD.                                                     LIS14580
           MOVE SERIAL-1 TO NT-SERIAL.                                  LIS14590
           IF NOT NEW-TUNES MOVE 1 TO NEW-TUNE-SWITCH                   LIS14600
           OPEN OUTPUT INDEX-SRT.                                       LIS14610
           MOVE :M: TO NT-TYPE.                                         LIS14620
           IF CS-SW > 0 MOVE FIRST-OPEN TO NT-POINT                     LIS14630
                 ELSE MOVE TUNE-SAVE TO NT-POINT.                       LIS14640
           MOVE JULIAN-DATE TO NT-LAST-ACT.                             LIS14650
           MOVE 1 TO NT-HOLD.                                           LIS14660
           WRITE INDEX-SRT-REC FROM NT-DETAIL.                          LIS14670
           MOVE FIRST-OPEN-REC TO TUNE-ACT-KEY, K.                      LIS14680
           READ TUNE-INV INVALID KEY GO TO NA999.                       LIS14690
           MOVE FIRST-DET TO N.                                         LIS14700
           MOVE SELCTN-POINT [N, 1] TO FIRST-OPEN.                      LIS14710
           IF FIRST-OPEN NOT > LAST-OPEN GO TO NA050.                   LIS14720
           PERFORM CLEAROUT THRU CLEAR-RETURN.                          LIS14730
           IF ERR-CODE \ 0 GO TO NA999.                                 LIS14740
           MOVE K TO TUNE-ACT-KEY.                                      LIS14750
           READ TUNE-INV INVALID KEY GO TO NA999.                       LIS14760
       NA050.                                                           LIS14770
           MOVE N TO DET.                                               LIS14780
           GO TO ADD-RETURN.                                            LIS14790
       NA999.                                                           LIS14800
           MOVE :NT-INDX-ADD: TO PAR.                                   LIS14810
           MOVE SERIAL-1 TO ERR-REASON.                                 LIS14820
           GO TO ERR-HALT.                                              LIS14830
       ADD-RETURN.                                                      LIS14840
           EXIT.                                                        LIS14850
                                                                        LIS14860
       ADD-FIND.                                                        LIS14870
           MOVE 0 TO N.                                                 LIS14880
           IF J > 20 MOVE 2 TO K.                                       LIS14890
           IF POST [DETS, J] >  99 MOVE 1 TO K.                         LIS14900
           IF TUNE-POINT [DETS, J] > 0 MOVE 1 TO K.                     LIS14910
       ERR-PROCESS.                                                     LIS14920
           GO TO X016.                                                  LIS14930
       X010.                                                            LIS14940
           MOVE 1 TO END-SW.                                            LIS14950
           ALTER TRANS-READ TO PROCEED TO X016.                         LIS14960
           GO TO CHNG.                                                  LIS14970
       LIST-GENERATOR.                                                  LIS14980
           MOVE 0 TO TP-BND.                                            LIS14990
           MOVE TAB-TRANS [1] TO FL-AREA.                               LIS15000
           MOVE FIN-SELCTN TO SEL-SELCTN.                               LIS15010
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS15020
           MOVE :SET: TO LIST-SWITCH.                                   LIS15030
           GO TO VAL-FIN-LN.                                            LIS15040
       CLEAROUT.                                                        LIS15050
           MOVE FIRST-OPEN-REC TO TUNE-ACT-KEY.                         LIS15060
           READ TUNE-INV INVALID KEY GO TO C100.                        LIS15070
           MOVE LAST-DET TO DET.                                        LIS15080
           IF LAST-DET > 8 ADD 1 TO LAST-OPEN-REC                       LIS15090
                           MOVE 1 TO LAST-DET                           LIS15100
           ELSE ADD 1 TO LAST-DET.                                      LIS15110
           MOVE LAST-OPEN TO SELCTN-POINT [DET, 1].                     LIS15120
           REWRITE TYP1 INVALID KEY GO TO C100.                         LIS15130
           GO TO CLEAR-RETURN.                                          LIS15140
       C100.                                                            LIS15150
                   MOVE :CLEAROUT: TO PAR                               LIS15160
           MOVE SPACES TO ERR-REASON                                    LIS15170
           GO TO ERR-HALT.                                              LIS15180
       CLEAR-RETURN.                                                    LIS15190
           EXIT.                                                        LIS15200
       TUNE-BLANK.                                                      LIS15210
           MOVE HIGH-VALUE TO T-I-SERIAL [N].                           LIS15220
       ADDER.                                                           LIS15230
           IF TAB-TRANSTN [1] NOT > :4:                                 LIS15240
             MOVE SPACES TO OLD-INFO-C.                                 LIS15250
           MOVE TAB-TRANS [1] TO SEL-CARD-AREA.                         LIS15260
           PERFORM SEL-LOCATE THRU LOCATE-S-RETURN.                     LIS15270
           PERFORM CHANGE-MAKER THRU C-M-EXIT.                          LIS15280
           MOVE 0 TO FOUND.                                             LIS15290
           MOVE S-TAPE TO SERIAL-1.                                     LIS15300
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS15310
           MOVE TI-POINT [TI-CNT] TO TP-CNV.                            LIS15320
           IF S-BAND-C > SPACES MOVE ZERO TO S-BAND.                    LIS15330
           MOVE S-BAND TO TP-BND.                                       LIS15340
           IF S-BAND NOT > ZERO GO TO  MST-FINDER.                      LIS15350
       ADD-BACK.                                                        LIS15360
           MOVE SERIAL-1 TO S-TAPE.                                     LIS15370
           MOVE 0 TO S-BAND, TP-BND.                                    LIS15380
           MOVE TP-REL TO TUNE-ACT-KEY.                                 LIS15390
           READ TUNE-INV INVALID KEY GO TO THATS-ALL.                   LIS15400
           MOVE TP-DET TO DET.                                          LIS15410
           MOVE 2 TO CNT.                                               LIS15420
       NEXT-SELECT-POINT.                                               LIS15430
           IF CNT > 6 MOVE SEL-CNV TO SELCTN-POINT [DET, 2]             LIS15440
           GO TO POINT-OUT.                                             LIS15450
           IF SELP-DET [DET, CNT] NOT > ZERO ADD 1 TO CNT               LIS15460
           GO TO NEXT-SELECT-POINT.                                     LIS15470
           MOVE SEL-CNV TO SELCTN-POINT [DET, CNT].                     LIS15480
       POINT-OUT.                                                       LIS15490
           MOVE TUNE-TITLE [DET] TO NEW-INFO-C.                         LIS15500
           MOVE :AD: TO CHG-CODE-C.                                     LIS15510
           MOVE S-SIDE TO C-SIDE.                                       LIS15520
           MOVE S-POSTN TO C-POST.                                      LIS15530
           MOVE TUNE-SERIAL [DET] TO TUNE-NO-C.                         LIS15540
           MOVE S-MIN-C TO C-MIN.                                       LIS15550
           MOVE S-SEC-C TO C-SEC.                                       LIS15560
           MOVE CONT-POINT [DET] TO CONT-CNV.                           LIS15570
           REWRITE TUNE-REC INVALID KEY GO TO THATS-ALL.                LIS15580
           IF CONT-CNV > ZERO GO TO PO2.                                LIS15590
           MOVE C-REL TO TUNE-ACT-KEY.                                  LIS15600
           READ TUNE-INV INVALID KEY GO TO THATS-ALL.                   LIS15610
           MOVE TUNE-TITLE [C-DET] TO NEW-CTIL2.                        LIS15620
       PO2.                                                             LIS15630
           MOVE S-SIDE TO SIDE-HOLD1.                                   LIS15640
           MOVE S-POSTN TO SLOT-HOLD1.                                  LIS15650
           MOVE TP-CNV TO TUNP1.                                        LIS15660
           IF S-MIN-C > SPACES MOVE 0 TO S-MIN.                         LIS15670
           IF S-SEC-C > SPACES MOVE 0 TO S-SEC.                         LIS15680
           IF S-GMIN-S > SPACES MOVE 0 TO S-GMIN.                       LIS15690
           IF S-GSEC-S > SPACES MOVE 0 TO S-GSEC.                       LIS15700
           MOVE 0 TO CNT.                                               LIS15710
           COMPUTE STIM1 > [S-MIN * 60] < S-SEC.                        LIS15720
           MOVE STIM1 TO TIMEHLD.                                       LIS15730
       FIND-THE-SLOT.                                                   LIS15740
           ADD 1 TO CNT.                                                LIS15750
           MOVE POST [DETS, CNT] TO POS1.                               LIS15760
           IF POSITN IS GREATER THAN SIDE-HOLD1 OR POSITN > ZERO        LIS15770
           MOVE CNT TO CNT-HOLD GO TO MOVE-THE-SLOT.                    LIS15780
           IF POS2 NOT > POST-HOLD1 GO TO FIND-THE-SLOT.                LIS15790
           MOVE CNT TO CNT-HOLD.                                        LIS15800
       MOVE-THE-SLOT.                                                   LIS15810
           MOVE POST [DETS, CNT] TO POS1.                               LIS15820
           MOVE POS1 TO POST-HOLD2.                                     LIS15830
           MOVE MASTER-NO [DETS, CNT] TO MSTHLD2.                       LIS15840
           MOVE GAP [DETS, CNT] TO GAPHLD2.                             LIS15850
           IF SIDE-HOLD2 NOT > S-SIDE GO TO NEXT-MOVE.                  LIS15860
           IF SLOT-HOLD2 NOT > 0                                        LIS15870
           ADD 1 TO SLOT-HOLD2.                                         LIS15880
       NEXT-MOVE.                                                       LIS15890
           MOVE POST-HOLD1 TO POS2.                                     LIS15900
           MOVE POS1 TO POST [DETS, CNT].                               LIS15910
           MOVE MSTHLD1 TO MASTER-NO [DETS, CNT].                       LIS15920
           MOVE GAPHLD1 TO GAP [DETS, CNT].                             LIS15930
           MOVE PHOLD2 TO PHOLD1.                                       LIS15940
           ADD 1 TO CNT.                                                LIS15950
           MOVE POST [DETS, CNT] TO POS1.                               LIS15960
           IF POSITN NOT > 0  GO TO MOVE-THE-SLOT.                      LIS15970
           MOVE POST-HOLD1 TO POS1.                                     LIS15980
           MOVE POS1 TO POST [DETS, CNT].                               LIS15990
           MOVE MSTHLD1 TO MASTER-NO [DETS, CNT].                       LIS16000
           MOVE GAPHLD1 TO GAP [DETS, CNT].                             LIS16010
           COMPUTE KNT > CNT < 1.                                       LIS16020
           MOVE 0 TO POS1.                                              LIS16030
           IF KNT NOT > 36 MOVE POS1 TO POST [DETS, KNT].               LIS16040
           COMPUTE CNT > CNT-HOLD - S-POSTN.                            LIS16050
           MOVE MASTER-NO [DETS, CNT] TO SERIAL-1.                      LIS16060
           MOVE 1 TO FOUND.                                             LIS16070
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS16080
           MOVE S-POSTN TO KNT.                                         LIS16090
           MOVE S-TAPE TO M-T-S1.                                       LIS16100
           MOVE 0 TO M-B1.                                              LIS16110
           MOVE SPACES TO M-A1.                                         LIS16120
       HERE.                                                            LIS16130
           MOVE MSTR-TUNES [DET, KNT] TO MST-S-HLD2.                    LIS16140
           MOVE MST-S-HLD1 TO MSTR-TUNES [DET, KNT]                     LIS16150
           ADD 1 TO KNT.                                                LIS16160
           MOVE MST-S-HLD2 TO MST-S-HLD1.                               LIS16170
           IF KNT > 16 OR MSTR-TUNE-SER [DET, KNT] > SPACES GO TO       LIS16180
           END-MOVE.                                                    LIS16190
           GO TO HERE.                                                  LIS16200
       END-MOVE.                                                        LIS16210
           MOVE MST-S-HLD1 TO MSTR-TUNES [DET, KNT].                    LIS16220
           REWRITE TYP2 INVALID KEY GO TO THATS-ALL.                    LIS16230
       ADDER-EXIT.                                                      LIS16240
           WRITE CHG-REC.                                               LIS16250
           GO TO A053.                                                  LIS16260
       MST-FINDER.                                                      LIS16270
           IF TUNE-ACT-KEY > TP-REL GO TO MF1.                          LIS16280
           MOVE TP-REL TO TUNE-ACT-KEY.                                 LIS16290
           READ TUNE-INV INVALID KEY GO TO THATS-ALL.                   LIS16300
       MF1.                                                             LIS16310
           MOVE MSTR-SERIAL [TP-DET] TO SERIAL-1.                       LIS16320
           PERFORM TUNE-LOCATE THRU LOCATE-RETURN.                      LIS16330
           MOVE TI-POINT [TI-CNT] TO TP-CNV.                            LIS16340
           GO TO ADD-BACK.                                              LIS16350
       CHANGE-MAKER.                                                    LIS16360
           MOVE SEL-SELCTN TO SELCTN-NO-C.                              LIS16370
           MOVE SELCTN-TITLE [DETS] TO SELCTN-TITLE-C.                  LIS16380
           MOVE ART-NAME [DETS] TO ARTIST-NAME-C.                       LIS16390
           MOVE DT TO CURR-DATE-C.                                      LIS16400
           MOVE MAP [DETS] TO MAP-CODE-C.                               LIS16410
           MOVE REL-DATE [DETS] TO REL-DATE-C.                          LIS16420
           MOVE REL-TYPE [DETS] TO REL-TYPE-C.                          LIS16430
       C-M-EXIT.                                                        LIS16440
           EXIT.                                                        LIS16450
       X016.                                                            LIS16460
           IF NS-CNT \ 0 PERFORM SEL-MERGE THRU MERGE-S-RETURN.         LIS16470
           MOVE 0 TO KNT.                                               LIS16480
           IF NEW-TUNES PERFORM TUNE-MERGE THRU MERGE-RETURN.           LIS16490
           MOVE 0 TO TUNE-ACT-KEY, SEL-ACT-KEY.                         LIS16500
           READ TUNE-INV INVALID KEY GO TO X020.                        LIS16510
           MOVE FIRST-OPEN TO T-I-POINT [1].                            LIS16520
           MOVE LAST-OPEN TO T-I-POINT [2].                             LIS16530
           PERFORM UNHSK VARYING N FROM 1 BY 1 UNTIL N > 11.            LIS16540
           MOVE :033: TO REC-ID-1.                                      LIS16550
           REWRITE TYP1 INVALID KEY GO TO X020.                         LIS16560
           READ SELECTION-INV INVALID KEY GO TO NO-SEL.                 LIS16570
           MOVE FS-AREA TO SI-LAST-ACT [1].                             LIS16580
           MOVE LAST-SEL TO SI-POINT [1].                               LIS16590
           MOVE S-LIMIT-DUM TO SI-SELCTN [1].                           LIS16600
           MOVE S-INDX-1 TO S-INDX.                                     LIS16610
           MOVE :035: TO REC-ID-2.                                      LIS16620
           REWRITE IND1 INVALID KEY GO TO NO-SEL.                       LIS16630
           MOVE 1 TO SEL-ACT-KEY.                                       LIS16640
           READ SELECTION-INV INVALID KEY GO TO NO-SEL.                 LIS16650
           MOVE S-INDX-2 TO S-INDX.                                     LIS16660
           MOVE :035: TO REC-ID-2.                                      LIS16670
           REWRITE IND1 INVALID KEY GO TO NO-SEL.                       LIS16680
       X020.                                                            LIS16690
           CLOSE TRANS-FILE, LIST-NOTCE.                                LIS16700
           CLOSE TUNE-INV.                                              LIS16710
           CLOSE SELECTION-INV.                                         LIS16720
           CLOSE CHANGES.                                               LIS16730
           STOP RUN.                                                    LIS16740
       TUNE-MERGE.                                                      LIS16750
           MOVE 11 TO KNT.                                              LIS16760
           MOVE ZERO TO N.                                              LIS16770
       MERGE-READ.                                                      LIS16780
           MOVE N TO TUNE-ACT-KEY.                                      LIS16790
           READ TUNE-INV INVALID KEY GO TO X020.                        LIS16800
       RETURN-INDEX.                                                    LIS16810
           WRITE INDEX-SRT-REC FROM T-INDX-DETAIL [KNT].                LIS16820
           ADD 1 TO KNT.                                                LIS16830
           IF KNT \ 150 GO TO NXT-INDEX-RETURN.                         LIS16840
           IF T-I-SERIAL [KNT] NOT > HIGH-VALUES GO TO RETURN-INDEX.    LIS16850
           ADD 1 N GIVING K.                                            LIS16860
           MOVE T-I-SERIAL [KNT] TO U-TUNE [K].                         LIS16870
           CLOSE INDEX-SRT.                                             LIS16880
           GO TO MERGE-RETURN.                                          LIS16890
       NXT-INDEX-RETURN.                                                LIS16900
           SUBTRACT 1 FROM KNT.                                         LIS16910
           COMPUTE K > N < 1.                                           LIS16920
           MOVE T-I-SERIAL [KNT] TO U-TUNE [K].                         LIS16930
       NXT2.                                                            LIS16940
           MOVE 1 TO KNT.                                               LIS16950
           ADD 1 TO N.                                                  LIS16960
           IF N ) 6 GO TO MERGE-READ.                                   LIS16970
           CLOSE INDEX-SRT.                                             LIS16980
       MERGE-RETURN.                                                    LIS16990
           EXIT.                                                        LIS17000
       ERR-HALT.                                                        LIS17010
           DISPLAY :ERROR IN BR2035: UPON CONSOLE.                      LIS17020
           MOVE :SET: TO VAR1.                                          LIS17030
           MOVE :CONSPOOL: TO VAR2.                                     LIS17040
           MOVE :ON: TO VAR3.                                           LIS17050
           CALL :CSS: USING RET-CD, VAR1, VAR2, VAR3.                   LIS17060
           DISPLAY :ER IN BR2035 IN PARA : PAR UPON CONSOLE.            LIS17070
           MOVE :VP: TO VAR1.                                           LIS17080
           MOVE :CLOSE: TO VAR2.                                        LIS17090
           MOVE :9: TO VAR3.                                            LIS17100
           CALL :CSS: USING RET-CD, VAR1, VAR2, VAR3.                   LIS17110
           MOVE :LOGOUT: TO VAR2.                                       LIS17120
           MOVE :*: TO VAR3.                                            LIS17130
           CALL :CSS: USING RET-CD, VAR1, VAR2, VAR3.                   LIS17140
  h KT