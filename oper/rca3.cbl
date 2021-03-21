       IDENTIFICATION DIVISION.                                         REP00010
       PROGRAM-ID.       :BR2050:.                                      REP00020
       INSTALLATION.      RCA RECORD DIVISION ROCKAWAY NJ.              REP00030
       AUTHOR     .       A ROSEN.                                      REP00040
       DATE WRITTEN.      06/07/71.                                     REP00050
       DATE COMPILED.     TODAY.                                        REP00060
       SECURITY    .       STANDARD.                                    REP00070
       ENVIRONMENT DIVISION.                                            REP00080
       CONFIGURATION SECTION.                                           REP00090
       INPUT-OUTPUT SECTION.                                            REP00100
       FILE-CONTROL.                                                    REP00110
           SELECT REC-IN ASSIGN  UT-2311-S-SYS104 .                     REP00120
           SELECT TABL ASSIGN UT-2311-S-SYS201.                         REP00130
            SELECT REC-OUT ASSIGN UT-2311-S-SYS033.                     REP00140
           SELECT DATE-FILE ASSIGN UT-2311-S-SYS205.                    REP00150
           SELECT CNTRL  ASSIGN DA-2301-R-SYS080                        REP00160
                           ACCESS IS RANDOM                             REP00170
                           NOMINAL KEY IS CNTRL-ACT-KEY.                REP00180
           SELECT CONI-FILE ASSIGN TO UT-2311-S-SYS035.                 REP00190
       DATA DIVISION.                                                   REP00200
       FILE SECTION.                                                    REP00210
       FD  CONI-FILE                                                    REP00220
           RECORDING MODE F                                             REP00230
           LABEL RECORDS OMITTED                                        REP00240
           DATA RECORD IS CONI-REC.                                     REP00250
       01  CONI-REC.                                                    REP00260
           03 REM-LCL      PICTURE XXX.                                 REP00270
           03 FILLER       PICTURE X[5].                                REP00280
           03 TODAYS-DATE      PICTURE X[8].                            REP00290
           03 FILLER           PICTURE X[64].                           REP00300
       FD  CNTRL                                                        REP00310
           RECORDING MODE F                                             REP00320
           LABEL RECORDS STANDARD                                       REP00330
           DATA RECORD IS CNTRL-REC.                                    REP00340
       01  CNTRL-REC.                                                   REP00350
           03 FILLER           PICTURE X[35].                           REP00360
           03 BR2050-DISCON    PICTURE X.                               REP00370
           03 BR2050-KEY-ER    PICTURE X[10].                           REP00380
           03 FILLER           PICTURE X[34].                           REP00390
       FD  REC-IN                                                       REP00400
           RECORDING MODE V                                             REP00410
           LABEL RECORD OMITTED                                         REP00420
           DATA RECORDS ARE R1, R2.                                     REP00430
       01  R1.                                                          REP00440
           03 ID1  PICTURE X.                                           REP00450
           03  ID2 PICTURE X.                                           REP00460
               88 FIRST-REC VALUE IS :1:.                               REP00470
           03 SELECTION PICTURE  X[10].                                 REP00480
           03 FILLER            PICTURE IS X[302].                      REP00490
       01  R2.                                                          REP00500
           03 RECRDID           PICTURE IS XX.                          REP00510
           03 SERLNO            PICTURE IS X[8].                        REP00520
           03 SONG-NUMBER PICTURE X[9].                                 REP00530
           03 NEW-SONG-FLAG PICTURE X.                                  REP00540
           03 TUNTIT            PICTURE IS X[40].                       REP00550
           03 ARTST             PICTURE IS X[60].                       REP00560
           03 FILLER            PICTURE IS X.                           REP00570
           03 RECD-DATE OCCURS 5 TIMES PICTURE 9[6] USAGE               REP00580
           IS COMPUTATIONAL-3.                                          REP00590
           03 TAKE              PICTURE IS 99.                          REP00600
           03 TIME              PICTURE IS 9999.                        REP00610
           03 RCA-A-R           PICTURE IS X[20].                       REP00620
           03 JOB-NO1            PICTURE X[8].                          REP00630
           03 CREDITS           PICTURE IS X[200].                      REP00640
           03 CRED-CHAR REDEFINES CREDITS OCCURS 200 TIMES PICTURE X.   REP00650
           03 CONTIN            PICTURE IS X.                           REP00660
       FD  TABL                                                         REP00670
           RECORDING MODE F                                             REP00680
           LABEL RECORD OMITTED                                         REP00690
           DATA RECORD IS PROD-COD.                                     REP00700
       01  PROD-COD          PICTURE X[25].                             REP00710
       FD  REC-OUT                                                      REP00720
           RECORDING MODE F                                             REP00730
           LABEL RECORD OMITTED                                         REP00740
           DATA RECORD IS PRINTLINE.                                    REP00750
       01 PRINTLINE.                                                    REP00760
           03  PRINT-1.                                                 REP00770
               05 FILLER     PICTURE XX.                                REP00780
               05 FIRST-HALF PICTURE X[64].                             REP00790
               05 SECOND-HALF PICTURE X[67].                            REP00800
           03 PRINT-X REDEFINES PRINT-1.                                REP00810
             05 PRINT-2 OCCURS 133 TIMES PICTURE X.                     REP00820
           03 PRINT-3 REDEFINES PRINT-1.                                REP00830
             05 FILLER          PICTURE XX.                             REP00840
             05 RCA             PICTURE X[38].                          REP00850
             05 L-NOT           PICTURE X[40].                          REP00860
             05 SELNUM          PICTURE X[19].                          REP00870
             05 SN.                                                     REP00880
               07 SN1            PICTURE X[10].                         REP00890
               07 FILLER         PICTURE X[4].                          REP00900
               07 PAG-LBL        PICTURE X[6].                          REP00910
               07 PAG-SPT        PICTURE Z9.                            REP00920
               07 FILLER         PICTURE X[12].                         REP00930
             05 END-DAT REDEFINES SN.                                   REP00940
               07 MT            PICTURE X[5].                           REP00950
               07 YR            PICTURE 9999.                           REP00960
               07 FILLER        PICTURE X[25].                          REP00970
             05 PRO-CHAR REDEFINES SN OCCURS 34 TIMES PICTURE X.        REP00980
           03 PRINT-4 REDEFINES PRINT-1.                                REP00990
             05 FILLER          PICTURE X[13].                          REP01000
             05 CHAR-1          PICTURE X[60].                          REP01010
             05 FILLER          PICTURE X[60].                          REP01020
           03 PRINT-5 REDEFINES PRINT-1.                                REP01030
             05 FILLER          PICTURE X[43].                          REP01040
             05 CLS-CD          PICTURE X[13].                          REP01050
             05 SLS-COD         PICTURE Z999.                           REP01060
             05 FILLER          PICTURE XXXX.                           REP01070
             05 PRS             PICTURE X[7].                           REP01080
             05 DOLS            PICTURE $$9.99.                         REP01090
             05 FILLER          PICTURE X[33].                          REP01100
             05 RE-NV           PICTURE X[6].                           REP01110
             05 RV-NUM          PICTURE 99.                             REP01120
             05 FILLER          PICTURE X[15].                          REP01130
       WORKING-STORAGE SECTION.                                         REP01140
           77 TOT-OUTS        PICTURE 99 VALUE IS 0.                    REP01150
           77 PAG-CNT            PICTURE 99 VALUE 0.                    REP01160
           77 T        PICTURE 99 VALUE 0.                              REP01170
           77 S-CNT        PICTURE 99 VALUE 0.                          REP01180
           77 DIV-SAV            PICTURE 99 VALUE 0.                    REP01190
           77 DIVON-SAV         PICTURE 99 VALUE 0.                     REP01200
           77 MP-SAV           PICTURE XXXX.                            REP01210
           77 PAR-SPOT         PICTURE 999 VALUE IS 0.                  REP01220
           77 TUN-LST         PICTURE X[40].                            REP01230
           77 TKN-NO            PICTURE 999 VALUE 0.                    REP01240
           77 ARTST-STOR          PICTURE X[60].                        REP01250
           77 TUNTIT-STOR          PICTURE X[40].                       REP01260
           77 DIVS                PICTURE 99 VALUE 0.                   REP01270
           77 TTL-TIM          PICTURE 9999 VALUE IS 0.                 REP01280
           77 TUN-INFO         PICTURE 99.                              REP01290
           77 CUT              PICTURE 99.                              REP01300
           77  WRTOT                PICTURE 999.                        REP01310
           77 NO-PRODS          PICTURE 999 VALUE 0.                    REP01320
           77 COMPR             PICTURE 999 VALUE 1.                    REP01330
           77 NO-CONTINS        PICTURE 9 VALUE 0.                      REP01340
           77 SUBTTL-NO         PICTURE 999 VALUE 0.                    REP01350
           77  BGIN-1              PICTURE 999.                         REP01360
           77  STRT-1              PICTURE 999.                         REP01370
           77  LOOK-1               PICTURE 999.                        REP01380
           77  LINE-TEST            PICTURE 9.                          REP01390
           77 ART-COMPARE             PICTURE X[120].                   REP01400
           77  A1                   PICTURE 99.                         REP01410
           77  TOT-ART              PICTURE 99.                         REP01420
           77  A2                   PICTURE 99.                         REP01430
           77  A3                   PICTURE 99.                         REP01440
           77  TUNE-INFO            PICTURE 99.                         REP01450
           77  TOT-MINS             PICTURE 99.                         REP01460
           77  TOT-SECS             PICTURE 99.                         REP01470
           77  CRED-TITL            PICTURE X[23].                      REP01480
           77 REL               PICTURE IS 999.                         REP01490
           77 LOOK              PICTURE IS 999.                         REP01500
           77 STRT            PICTURE IS 999.                           REP01510
           77 BGIN             PICTURE IS 999.                          REP01520
           77 FLAG1             PICTURE IS 9.                           REP01530
           77 DIVISIONS         PICTURE IS 9.                           REP01540
           77 OUTS              PICTURE IS 99.                          REP01550
           77 LIMT             PICTURE IS 99.                           REP01560
           77 TOT-TIM           PICTURE IS 9999.                        REP01570
           77 MINS              PICTURE IS 99.                          REP01580
           77 SECS              PICTURE IS 99.                          REP01590
           77 INDE              PICTURE IS 99.                          REP01600
           77 SUB               PICTURE IS 999 USAGE COMPUTATIONAL-3.   REP01610
           77 SUB1              PICTURE IS 999 USAGE COMPUTATIONAL-3.   REP01620
           77 COMNBR            PICTURE IS 999.                         REP01630
           77 LYRNUM            PICTURE IS 999.                         REP01640
           77 NOTCNT            PICTURE IS 999.                         REP01650
           77  KNT  PICTURE  99 VALUE ZERO.                             REP01660
           77 CNTRL-ACT-KEY  PICTURE S9[8] COMPUTATIONAL.               REP01670
           77 N PICTURE 999 VALUE 0.                                    REP01680
           77 K PICTURE 999 VALUE 0.                                    REP01690
           77 KNTS PICTURE 9 VALUE 0.                                   REP01700
                                                                        REP01710
       01  CSS-MSGS.                                                    REP01720
           03 VAR1         PICTURE X[8].                                REP01730
           03 VAR2         PICTURE X[8].                                REP01740
           03 VAR3         PICTURE X[8].                                REP01750
           03 VAR4         PICTURE X[8].                                REP01760
           03 RET-CD       PICTURE 9999.                                REP01770
       01  END-HOLDER.                                                  REP01780
             05  END-AREA OCCURS 3 TIMES.                               REP01790
               07 END-TYPE PICTURE X[5].                                REP01800
       01  MED-TOT PICTURE X[10].                                       REP01810
       01  MED-CHAR-AREA REDEFINES MED-TOT.                             REP01820
           03 MED-CHAR OCCURS 10 TIMES PICTURE X.                       REP01830
       01  WS-CONTROL-CH.                                               REP01840
           03 WS1 PICTURE XX                                            REP01850
       01  R1-SAV.                                                      REP01860
           03 REC-ID            PICTURE IS XX.                          REP01870
             03  REC-ID2 REDEFINES REC-ID.                              REP01880
               05  ID-SPOT PICTURE X.                                   REP01890
                 88 SUMMARY VALUE IS :S:.                               REP01900
               05 FILLER PICTURE X.                                     REP01910
           03 SEL-NO            PICTURE IS X[10].                       REP01920
           03 PREF REDEFINES SEL-NO OCCURS 10 TIMES PICTURE X.          REP01930
           03 SOURCE-SEL        PICTURE IS X[10].                       REP01940
           03 SEL-TITL          PICTURE IS X[100].                      REP01950
           03 TITL-CHAR REDEFINES SEL-TITL OCCURS 100 TIMES PICTURE X.  REP01960
           03  FILLER     PICTURE X.                                    REP01970
           03  NO-BDS     PICTURE 9[4] COMPUTATIONAL-3.                 REP01980
           03  PROJ-REL-DATE PICTURE 9[4].                              REP01990
           03 ART-NAM           PICTURE IS X[60].                       REP02000
           03 ART-CHAR REDEFINES ART-NAM OCCURS 60 TIMES PICTURE X.     REP02010
           03 REL-MON           PICTURE IS 99.                          REP02020
           03 REL-YR            PICTURE IS 99.                          REP02030
           03 REL-TYPE          PICTURE IS X.                           REP02040
              88 REG VALUE :1:.                                         REP02050
              88 SPEC VALUE :2:.                                        REP02060
              88 PRO VALUE :4:.                                         REP02070
              88 OPP VALUE :3:.                                         REP02080
              88 DUBL VALUE :5:.                                        REP02090
           03 PRODUCER          PICTURE IS X[20].                       REP02100
           03 MAP-CODE.                                                 REP02110
             05 PRODUCT          PICTURE 999.                           REP02120
             05 MEDIUM          PICTURE 9.                              REP02130
               88 SINGLE VALUE 1.                                       REP02140
               88 ALBUM VALUES ARE 3 4 5.                               REP02150
               88 Q-DSK VALUE 5.                                        REP02160
               88 RL-RL VALUE 7.                                        REP02170
               88 QUAD-8 VALUE 6.                                       REP02180
               88 STEREO-8 VALUE 8.                                     REP02190
               88 CASSETTE VALUE 9.                                     REP02200
           03 MP-CD REDEFINES MAP-CODE PICTURE XXXX.                    REP02210
           03 CLASS-CODE        PICTURE IS 9[4].                        REP02220
           03 PRICE           PICTURE IS 99V99.                         REP02230
           03 REV-NO            PICTURE 99.                             REP02240
      03 ALBUMS.                                                   REP02250
              05 MSTRS OCCURS 4 TIMES.                                  REP02260
                07 MASTER-NO    PICTURE IS X[8].                        REP02270
                07 INTRO PICTURE XX.                                    REP02280
                07 FADE PICTURE 9.                                      REP02290
                07 FILLER PICTURE X[3].                                 REP02300
                07 NO-SELNS     PICTURE IS 99.                          REP02310
              05 FILLER         PICTURE IS X[4].                        REP02320
           03 ST-Q REDEFINES ALBUMS.                                    REP02330
              05 INDENT         PICTURE IS 99.                          REP02340
              05 OUTDENT        PICTURE IS 99.                          REP02350
              05 PROGRAMS OCCURS 4 TIMES.                               REP02360
                07 NOSLS        PICTURE IS 99.                          REP02370
                07 GAP OCCURS 7 TIMES PICTURE IS 99.                    REP02380
           03 CASETE REDEFINES ALBUMS.                                  REP02390
              05 INDNT          PICTURE IS 99.                          REP02400
              05 OTDNT          PICTURE IS 99.                          REP02410
              05 PROG OCCURS 2 TIMES.                                   REP02420
                07 NSELECTS     PICTURE IS 99.                          REP02430
                07 GPS OCCURS 15 TIMES PICTURE IS 99.                   REP02440
           03 CONT               PICTURE X.                             REP02450
       01  NUMBR-TST.                                                   REP02460
           03 NUM-TEST.                                                 REP02470
             05 NUM-TESTA          PICTURE X.                           REP02480
             05 NUM-TESTB         PICTURE X.                            REP02490
           03 NUM-TEST1 REDEFINES NUM-TEST PICTURE 99.                  REP02500
       01  PROD-TABL.                                                   REP02510
           03 PROD-ELMNT OCCURS 50 TIMES.                               REP02520
             05 MAP-PROD PICTURE XXX.                                   REP02530
             05 MAP-TRANS        PICTURE X[22].                         REP02540
       01  MONTH-NAME.                                                  REP02550
           03 MNAM  PICTURE IS A[48] VALUE   :JAN FEB MAR APR MAY JUNEJUREP02560
      -    :LYAUG SEPTOCT NOV DEC :.                                    REP02570
           03 TABL-M REDEFINES MNAM.                                    REP02580
             05 MONTH OCCURS 12 TIMES PICTURE A[4].                     REP02590
       01  SAVAREA.                                                     REP02600
           03 ID-D              PICTURE IS XX.                          REP02610
           03 SERIAL            PICTURE IS X[8].                        REP02620
           03 SONG-NUM-SAV PICTURE X[9].                                REP02630
           03 SONG-FLAG-SAV PICTURE X.                                  REP02640
           03 TUNE              PICTURE IS X[40].                       REP02650
           03 TUNE-1 REDEFINES TUNE OCCURS 40 TIMES PICTURE X.          REP02660
           03 ARTIST-1          PICTURE IS X[60].                       REP02670
           03 FILLER            PICTURE IS X.                           REP02680
           03 RDAT OCCURS 5 TIMES PICTURE 9[6] USAGE COMPUTATIONAL-3.   REP02690
           03 TAKE-1            PICTURE IS 99.                          REP02700
           03 TIME-1            PICTURE IS 9999.                        REP02710
           03 RA-R              PICTURE IS X[20].                       REP02720
           03 JOB-NO               PICTURE IS X[8].                     REP02730
           03 CREDS             PICTURE IS X[200].                      REP02740
           03 CONTINUE          PICTURE IS X.                           REP02750
       01  OUTAREA.                                                     REP02760
           03 OUT-LINE OCCURS 58 TIMES PICTURE X[132].                  REP02770
       01  HEAD-LINE.                                                   REP02780
           03 ALB-HD.                                                   REP02790
              05  FILLER        PICTURE XXX.                            REP02800
              05  SD-NAM        PICTURE X[5].                           REP02810
              05  NMBR          PICTURE 9.                              REP02820
              05  FILLER        PICTURE X[3].                           REP02830
              05 SRL-SPACE.                                             REP02840
                 07  SRL PICTURE X[14].                                 REP02850
                 07  FILLER PICTURE X[20].                              REP02860
               05 SRL-AGAIN REDEFINES SRL-SPACE.                        REP02870
                 07 FILLER PICTURE X[10].                               REP02880
                 07 MON-START PICTURE X[5].                             REP02890
                 07 SRL-X.                                              REP02900
                   09 FILLER PICTURE X[3].                              REP02910
                   09 SRL-X4 PICTURE X.                                 REP02920
                   09 FILLER PICTURE X[4].                              REP02930
                07 MON-FINISH PICTURE X.                                REP02940
                 07 FILLER PICTURE X[10].                               REP02950
             05 P-TIM           PICTURE X[4].                           REP02960
             05 FILLER          PICTURE X[3].                           REP02970
             05 P-REC-DT        PICTURE X[8].                           REP02980
             05 FILLER          PICTURE X[3].                           REP02990
             05 P-SERIAL        PICTURE X[6].                           REP03000
             05 FILLER          PICTURE X[10].                          REP03010
             05 P-JB            PICTURE X[6].                           REP03020
             05 FILLER          PICTURE X[6].                           REP03030
             05 P-PUB           PICTURE X[9].                           REP03040
           03 TP-HD REDEFINES ALB-HD.                                   REP03050
              05  FILLER        PICTURE XXX.                            REP03060
              05  PRG-NAM       PICTURE X[8].                           REP03070
              05  NMBR-1        PICTURE 9.                              REP03080
              05  FILLER        PICTURE X[45].                          REP03090
              05  INDT          PICTURE X[7].                           REP03100
              05  INTM          PICTURE 99.                             REP03110
             05 FILLER          PICTURE X[35].                          REP03120
           03 SINGLE-FROM-LINE REDEFINES ALB-HD.                        REP03130
               05 FILLER PICTURE X[7].                                  REP03140
               05 S-F-START PICTURE X[6].                               REP03150
               05 FILLER PICTURE X.                                     REP03160
               05 SINGLE-FROM PICTURE X[8].                             REP03170
               05 S-F-END PICTURE X.                                    REP03180
               05 FILLER PICTURE X[76].                                 REP03190
       01  STOR-AREA.                                                   REP03200
           03 STORE  PICTURE IS X[200].                                 REP03210
           03  STORE-1 REDEFINES STORE.                                 REP03220
             05 STORAGE OCCURS 200 TIMES PICTURE X.                     REP03230
           03 C-L REDEFINES STORE.                                      REP03240
             05 NAMES-1        PICTURE IS X[90].                        REP03250
             05 FILLER         PICTURE IS X[110].                       REP03260
           03 A-C REDEFINES STORE.                                      REP03270
             05 NAMES-2        PICTURE IS X[90].                        REP03280
             05 FILLER         PICTURE IS X[110].                       REP03290
           03 STORE-1 REDEFINES STORE.                                  REP03300
             05 NOTES-1        PICTURE IS X[90].                        REP03310
             05 FILLER         PICTURE IS X[110].                       REP03320
       01  LINE1.                                                       REP03330
           03 CMP            PICTURE IS X[90].                          REP03340
           03 CO1 REDEFINES CMP OCCURS 90 TIMES PICTURE X.              REP03350
           03 LYR              PICTURE IS X[90].                        REP03360
           03 LYR1 REDEFINES LYR OCCURS 90 TIMES PICTURE X.             REP03370
           03 SUBTTL          PICTURE X[90].                            REP03380
           03 SUB-CHAR REDEFINES SUBTTL OCCURS 90 TIMES PICTURE X.      REP03390
           03 CO-ARTST OCCURS 12 TIMES PICTURE X[90].                   REP03400
           03 TKN-FRM               PICTURE X[90].                      REP03410
           03 ARR OCCURS 12 TIMES PICTURE X[90].                        REP03420
           03 COND OCCURS 12 TIMES PICTURE X[90].                       REP03430
           03 AR-C OCCURS 12 TIMES PICTURE X[90].                       REP03440
           03 VOC OCCURS 12 TIMES PICTURE X[90].                        REP03450
           03 INST OCCURS 12 TIMES PICTURE X[90].                       REP03460
           03 NOT1 OCCURS 12 TIMES PICTURE X[90].                       REP03470
           03 RECDENG OCCURS 12 TIMES PICTURE X[90].                    REP03480
           03 RECSTD OCCURS 12 TIMES PICTURE X[90].                     REP03490
           03 MUSCRD  OCCURS 12 TIMES PICTURE X[90].                    REP03500
           03 PUB1              PICTURE X[90].                          REP03510
           03 PUB2 REDEFINES PUB1.                                      REP03520
              05 PB2A           PICTURE X[40].                          REP03530
              05 PB2B           PICTURE X[50].                          REP03540
           03 PUB3 REDEFINES PUB1 OCCURS 90 TIMES PICTURE X.            REP03550
           03 LDVOC OCCURS 12 TIMES PICTURE X[90].                      REP03560
           03 LDINS OCCURS 12 TIMES PICTURE X[90].                      REP03570
           03 PRDCR OCCURS 12 TIMES PICTURE X[90].                      REP03580
       01  LAY-OUT.                                                     REP03590
           03 LAYOUT-1.                                                 REP03600
             05 FILLER          PICTURE IS X[2].                        REP03610
             05 CUT-NUM         PICTURE IS Z9.                          REP03620
             05 DSH             PICTURE IS X.                           REP03630
             05 TITL-1.                                                 REP03640
                 07 FILLER PICTURE X[35].                               REP03650
                 07 S-LABEL PICTURE X[5].                               REP03660
             05 FILLER          PICTURE IS X[1].                        REP03670
             05 ENDWRD PICTURE X[5].                                    REP03680
             05 ENDWRD-NUM REDEFINES ENDWRD.                            REP03690
               07 MIN-1           PICTURE IS Z9.                        REP03700
               07 COL             PICTURE IS X.                         REP03710
               07 SEC-1           PICTURE IS 99.                        REP03720
               07 SEC-2 REDEFINES SEC-1 PICTURE XX.                     REP03730
             05 FILLER          PICTURE IS XX.                          REP03740
             05 REG-DAT.                                                REP03750
               07 RMON            PICTURE IS Z9.                        REP03760
               07 DSH1            PICTURE IS X.                         REP03770
               07 RDAY            PICTURE IS Z9.                        REP03780
               07 DSH3            PICTURE IS X.                         REP03790
               07 RYR             PICTURE IS 99.                        REP03800
             05 NREG-DAT REDEFINES REG-DAT.                             REP03810
               07 PM            PICTURE X[4].                           REP03820
               07 PM-YR         PICTURE 9999.                           REP03830
             05 FILLER          PICTURE IS XX.                          REP03840
             05 SERIAL-1        PICTURE IS X[14].                       REP03850
             05 FILLER          PICTURE IS X[2].                        REP03860
             05 JBNM            PICTURE IS X[10].                       REP03870
             05  FILLER         PICTURE IS X[3].                        REP03880
             05 PUBLSH          PICTURE IS X[40].                       REP03890
             05 PBLSHR REDEFINES PUBLSH OCCURS 40 TIMES PICTURE X.      REP03900
           03 LAYOUT-2 REDEFINES LAYOUT-1.                              REP03910
             05 FILLER          PICTURE X[5].                           REP03920
             05 WRT-CHAR OCCURS 50 TIMES PICTURE X.                     REP03930
             05 FILLER          PICTURE X[77].                          REP03940
           03 LAYOUT-2A REDEFINES LAYOUT-2.                             REP03950
             05 FILLER PICTURE X[5].                                    REP03960
             05 OPEN-PAR PICTURE X.                                     REP03970
             05 FIRST-38 PICTURE X[38].                                 REP03980
             05 CLOSE-PAR PICTURE X.                                    REP03990
             05 FILLER PICTURE X[87].                                   REP04000
           03 LAYOUT-3 REDEFINES LAYOUT-1.                              REP04010
             05 FILLER          PICTURE X[5].                           REP04020
             05 ARTIST-7        PICTURE X[60].                          REP04030
             05 ARTIST-8        PICTURE X[60].                          REP04040
             05 FILLER          PICTURE X[7].                           REP04050
           03 LAYOUT-4 REDEFINES LAYOUT-1.                              REP04060
             05 FILLER          PICTURE X[20].                          REP04070
             05 TIM-TIT         PICTURE X[15].                          REP04080
             05 PROG-NUM        PICTURE Z9.                             REP04090
             05 FILLER          PICTURE X[95].                          REP04100
           03 LAYOUT-5 REDEFINES LAYOUT-1.                              REP04110
             05 FILLER          PICTURE X[3].                           REP04120
             05 REFS OCCURS 42 TIMES.                                   REP04130
                07 TUN-CRED     PICTURE Z9.                             REP04140
                07 COMA         PICTURE X.                              REP04150
             05 FILLER          PICTURE X[3].                           REP04160
           03 LAYOUT-6 REDEFINES LAYOUT-1.                              REP04170
             05 FILLER          PICTURE X[14].                          REP04180
             05 ART-OUT-1       PICTURE X[60].                          REP04190
             05 ART-OUT-1A          PICTURE X[58].                      REP04200
           03 LAYOUT-7 REDEFINES LAYOUT-1.                              REP04210
             05 FILLER          PICTURE X[26].                          REP04220
             05 ART-OUT-2       PICTURE X[60].                          REP04230
             05 ART-OUT-2A        PICTURE X[46].                        REP04240
           03 LAYOUT-8 REDEFINES LAYOUT-1.                              REP04250
             05 FILLER          PICTURE X[34].                          REP04260
             05 ART-OUT-3       PICTURE X[60].                          REP04270
             05 ART-OUT-3A       PICTURE X[38].                         REP04280
           03 LAYOUT-9 REDEFINES LAYOUT-1.                              REP04290
             05 FILLER          PICTURE X[5].                           REP04300
             05 NOT-OUT         PICTURE X[90].                          REP04310
             05 FILLER          PICTURE X[37].                          REP04320
           03 LAYOUT-10 REDEFINES LAYOUT-1.                             REP04330
             05 FILLER          PICTURE X[20].                          REP04340
             05 NOT-OUT-1       PICTURE X[90].                          REP04350
             05 FILLER          PICTURE X[22].                          REP04360
           03 LAYOUT-11 REDEFINES LAYOUT-1.                             REP04370
             05 FILLER          PICTURE X[38].                          REP04380
             05 NOT-OUT-2       PICTURE X[90].                          REP04390
             05 FILLER          PICTURE X[4].                           REP04400
           03 LAYOUT-12 REDEFINES LAYOUT-1.                             REP04410
             05 FILLER          PICTURE X[5].                           REP04420
             05 TITL-OUT-1      PICTURE X[13].                          REP04430
             05 ARR-OUT-1       PICTURE X[60].                          REP04440
             05 FILLER          PICTURE X[54].                          REP04450
           03 LAYOUT-13 REDEFINES LAYOUT-1.                             REP04460
             05 FILLER          PICTURE X[20].                          REP04470
             05 TITL-OUT-2      PICTURE X[13].                          REP04480
             05 ARR-OUT-2       PICTURE X[30].                          REP04490
             05 FILLER          PICTURE X[69].                          REP04500
           03 LAYOUT-14 REDEFINES LAYOUT-1.                             REP04510
             05 FILLER          PICTURE X[38].                          REP04520
             05 TITL-OUT-3      PICTURE X[13].                          REP04530
             05 ARR-OUT-3       PICTURE X[30].                          REP04540
             05 FILLER          PICTURE X[51].                          REP04550
           03 LAYOUT-15 REDEFINES LAYOUT-1.                             REP04560
             05 FILLER          PICTURE X[5].                           REP04570
             05 TITL-OUT-4      PICTURE X[24].                          REP04580
             05 A-C-OUT-1       PICTURE X[30].                          REP04590
             05 FILLER          PICTURE X[73].                          REP04600
           03 LAYOUT-16 REDEFINES LAYOUT-1.                             REP04610
             05 FILLER          PICTURE X[18].                          REP04620
             05 TITL-OUT-5      PICTURE X[24].                          REP04630
             05 A-C-OUT-2       PICTURE X[90].                          REP04640
           03 LAYOUT-17 REDEFINES LAYOUT-1.                             REP04650
             05 FILLER          PICTURE X[38].                          REP04660
             05 TITL-OUT-6      PICTURE X[24].                          REP04670
             05 A-C-OUT-3       PICTURE X[30].                          REP04680
             05 FILLER          PICTURE X[40].                          REP04690
       01  ALT-1.                                                       REP04700
           03 ALT-1-OCCUR OCCURS 12 TIMES PICTURE 99.                   REP04710
       01  ALT-2.                                                       REP04720
           03 ALT-2-OCCUR OCCURS 12 TIMES PICTURE 99.                   REP04730
       01  MAIN.                                                        REP04740
           03 MAIN-ART OCCURS 12 TIMES PICTURE 99.                      REP04750
       01  ART-LINE-1.                                                  REP04760
           03 ART-LINE OCCURS 58 TIMES PICTURE 9.                       REP04770
       01  TUN-MTCH.                                                    REP04780
           03 TUN-MTCH-1 OCCURS 11 TIMES PICTURE 99.                    REP04790
       01  DAT-FIX.                                                     REP04800
           03 DATE-2        PICTURE 9[6].                               REP04810
           03 DATE-3 REDEFINES DATE-2.                                  REP04820
             05 MONTH-1        PICTURE 99.                              REP04830
             05 DAY-1          PICTURE 99.                              REP04840
             05 YEAR-1         PICTURE 99.                              REP04850
       01  CONTROL-CHAR.                                                REP04860
           03  WS-CH1      PICTURE S9[2] COMPUTATIONAL-3 VALUE 0.       REP04870
           03  WS-CH2      PICTURE X VALUE :1:.                         REP04880
       01  PRINT-HOLDER        PICTURE X[133].                          REP04890
       01  ART-SWITCH    PICTURE  9 VALUE ZERO.                         REP04900
            88 ONLY-1-ARTIST VALUE IS 0.                                REP04910
       01  DSHES     PICTURE X[67] VALUE :------------------------------REP04920
      -    :-------------------------------------:.                     REP04930
       01  CHAR-HOLD-AREA.                                              REP04940
           03 CREDS-1 OCCURS 400 TIMES PICTURE X.                       REP04950
       01  CHAR-HOLD-AREA2 REDEFINES CHAR-HOLD-AREA.                    REP04960
           03 1ST-200-CHAR PICTURE X[200].                              REP04970
           03 2ND-200-CHAR PICTURE X[200].                              REP04980
       01  ART-AREA.                                                    REP04990
           03  ART-HOLDER40.                                            REP05000
             05 ART40-1 PICTURE X[40].                                  REP05010
             05 ART40-2 PICTURE X[40].                                  REP05020
             05 ART40-3 PICTURE X[40].                                  REP05030
           03  ART-HOLDER60 REDEFINES ART-HOLDER40.                     REP05040
             05 ART60-1 PICTURE X[60].                                  REP05050
             05 ART60-2 PICTURE X[60].                                  REP05060
       01  ART-ARRAY REDEFINES ART-AREA.                                REP05070
           03 ARTS OCCURS 3 TIMES PICTURE X[40].                        REP05080
                                                                        REP05090
       01  BAND-AREA  PICTURE 9[5].                                     REP05100
       01  B-A REDEFINES BAND-AREA.                                     REP05110
           03  BAND-DUM  PICTURE 9.                                     REP05120
           03  NO-BAND OCCURS 4 TIMES PICTURE 9.                        REP05130
                                                                        REP05140
       01  GAP-SWITCH            PICTURE 9 VALUE ZERO.                  REP05150
            88 GAP-THERE VALUE IS 1.                                    REP05160
                                                                        REP05170
       01  FN                    PICTURE X[8] VALUE :LISTNTR :.         REP05180
       01  FT                    PICTURE X[8] VALUE :DATA    :.         REP05190
       01  RETCD                 PICTURE S9[5] COMPUTATIONAL.           REP05200
                                                                        REP05210
       01  SERIAL-HOLD-AREA.                                            REP05220
           03 A-SPT              PICTURE X VALUE :A:.                   REP05230
           03 SERL-HOLD.                                                REP05240
             05 FILLER           PICTURE X[7].                          REP05250
             05 LAST-CHAR        PICTURE X.                             REP05260
             05 LAST-SPT         PICTURE X.                             REP05270
                                                                        REP05280
       PROCEDURE DIVISION.                                              REP05290
       HOUSEKEEPING SECTION.                                            REP05300
       STATE-CHECK.                                                     REP05310
           CALL :STATE: USING FN, FT, RETCD.                            REP05320
           IF RETCD > 0                                                 REP05330
             STOP RUN.                                                  REP05340
       DO-STRT.                                                         REP05350
           OPEN INPUT REC-IN.                                           REP05360
           OPEN INPUT TABL.                                             REP05370
           MOVE SPACES TO END-AREA [1].                                 REP05380
           MOVE : FADE: TO END-AREA [2].                                REP05390
           MOVE : COLD: TO END-AREA [3].                                REP05400
           OPEN OUTPUT REC-OUT.                                         REP05410
       TABL-READIN.                                                     REP05420
           READ TABL AT END GO TO PAR-1.                                REP05430
           ADD 1 TO NO-PRODS.                                           REP05440
           MOVE PROD-COD TO PROD-ELMNT [NO-PRODS].                      REP05450
           GO TO TABL-READIN.                                           REP05460
       PAR-1.                                                           REP05470
           OPEN INPUT CONI-FILE.                                        REP05480
           READ CONI-FILE AT END CLOSE CONI-FILE.                       REP05490
           IF REM-LCL > :LCL: MOVE WS-CH1 TO WS-CONTROL-CH ELSE         REP05500
               MOVE WS-CH2 TO WS-CONTROL-CH.                            REP05510
           OPEN I-O CNTRL.                                              REP05520
           MOVE 0 TO CNTRL-ACT-KEY.                                     REP05530
           READ CNTRL INVALID KEY GO TO CNTRL-ERR.                      REP05540
           PERFORM CLEAN-OUT.                                           REP05550
           IF BR2050-DISCON > 1 GO TO ER-RECOVERY.                      REP05560
           IF REM-LCL > :REM: MOVE 0 TO BR2050-DISCON ELSE              REP05570
           MOVE 1 TO BR2050-DISCON.                                     REP05580
           REWRITE CNTRL-REC INVALID KEY GO TO CNTRL-ERR.               REP05590
       PAGE-HEAD SECTION.                                               REP05600
       01-LINE1-1.                                                      REP05610
           CLOSE TABL.                                                  REP05620
       01-LINE1-2.                                                      REP05630
           READ REC-IN INTO R1 AT END GO TO END-PAR.                    REP05640
           PERFORM CONTROL-CHECK.                                       REP05650
       RESTRT-POINT.                                                    REP05660
           MOVE R1 TO R1-SAV.                                           REP05670
           IF MEDIUM NOT > 1 AND MEDIUM ) 6 MOVE NO-BDS TO BAND-AREA.   REP05680
           MOVE MP-CD TO MP-SAV.                                        REP05690
           PERFORM PAGE-TOP.                                            REP05700
           MOVE SPACES TO PRINTLINE.                                    REP05710
           WRITE PRINTLINE.                                             REP05720
           IF NOT SUMMARY MOVE :LISTING NOTICE: TO L-NOT                REP05730
           ELSE MOVE :PRODUCT SUMMARY: TO L-NOT.                        REP05740
           MOVE :RCA RECORDS: TO RCA.                                   REP05750
           MOVE :SELECTION NUMBER @: TO SELNUM.                         REP05760
           MOVE SEL-NO TO SN1.                                          REP05770
           ADD 1 TO PAG-CNT.                                            REP05780
           MOVE PAG-CNT TO PAG-SPT.                                     REP05790
           MOVE :PAGE@ : TO PAG-LBL.                                    REP05800
           WRITE PRINTLINE.                                             REP05810
           MOVE SPACES TO PRINTLINE.                                    REP05820
       01-LINE1-3.                                                      REP05830
           IF SOURCE-SEL NOT > SPACES MOVE :TAKEN FROM       @:         REP05840
           TO SELNUM.                                                   REP05850
           MOVE SOURCE-SEL TO SN1.                                      REP05860
           WRITE PRINTLINE.                                             REP05870
           MOVE SPACES TO PRINTLINE.                                    REP05880
           MOVE :TITLE    @: TO RCA.                                    REP05890
       04-LINE5.                                                        REP05900
           MOVE 1 TO STRT. MOVE 14 TO BGIN.                             REP05910
           MOVE 67 TO LOOK.                                             REP05920
       05-SEARCH.                                                       REP05930
           IF TITL-CHAR [LOOK] > SPACE GO TO 06-MOVE.                   REP05940
           SUBTRACT 1 FROM LOOK.                                        REP05950
           GO TO 05-SEARCH.                                             REP05960
       06-MOVE.                                                         REP05970
           MOVE TITL-CHAR [STRT] TO PRINT-2 [BGIN].                     REP05980
           ADD 1 TO STRT. ADD 1 TO BGIN.                                REP05990
           IF STRT NOT \ LOOK GO TO 06-MOVE.                            REP06000
       07-DATE-CONV.                                                    REP06010
           MOVE :ISSUE DATE       @: TO SELNUM.                         REP06020
           MOVE TODAYS-DATE TO SN.                                      REP06030
           IF REV-NO NOT > 0 MOVE :REV =: TO RE-NV.                     REP06040
           IF REV-NO NOT > 0 MOVE REV-NO TO RV-NUM.                     REP06050
           WRITE PRINTLINE.                                             REP06060
           MOVE SPACES TO PRINTLINE.                                    REP06070
       08-LINE6.                                                        REP06080
           MOVE 100 TO LOOK.                                            REP06090
           MOVE 13 TO BGIN.                                             REP06100
           PERFORM 06-MOVE.                                             REP06110
       MAPCOD.                                                          REP06120
           MOVE :MAP CODE         @: TO SELNUM.                         REP06130
           MOVE MP-SAV TO SN.                                           REP06140
           WRITE PRINTLINE.                                             REP06150
           MOVE SPACES TO PRINTLINE.                                    REP06160
           GO TO 011-LINE9.                                             REP06170
       PREFIX-CONV.                                                     REP06180
           IF PRODUCT > MAP-PROD [COMPR] MOVE MAP-TRANS [COMPR] TO      REP06190
           SN.                                                          REP06200
           IF PRODUCT > MAP-PROD [COMPR] GO TO FIN-CHK.                 REP06210
           ADD 1 TO  COMPR.                                             REP06220
           IF COMPR NOT \ NO-PRODS GO TO PREFIX-CONV.                   REP06230
           GO TO FIN-CHK.                                               REP06240
       FIN-CHK.                                                         REP06250
           MOVE 1 TO COMPR.                                             REP06260
           MOVE 25 TO LOOK.                                             REP06270
       NON-SPC-SRCH.                                                    REP06280
           IF PRO-CHAR [LOOK] NOT > SPACE GO TO MED-MOVE.               REP06290
           SUBTRACT 1 FROM LOOK.                                        REP06300
           GO TO NON-SPC-SRCH.                                          REP06310
       MED-MOVE.                                                        REP06320
           ADD 2 TO LOOK.                                               REP06330
           IF ALBUM  MOVE :ALBUM: TO MED-TOT.                           REP06340
           IF STEREO-8 MOVE :STEREO-8: TO MED-TOT.                      REP06350
           IF QUAD-8 MOVE :Q-8: TO MED-TOT.                             REP06360
           IF Q-DSK MOVE :QUADRADISC: TO MED-TOT.                       REP06370
           IF CASSETTE MOVE :CASSETTE: TO MED-TOT.                      REP06380
           IF SINGLE MOVE :SINGLE: TO MED-TOT.                          REP06390
           IF RL-RL MOVE :REEL-REEL: TO MED-TOT.                        REP06400
           MOVE 1 TO BGIN.                                              REP06410
       MED-CHAR-MOVE.                                                   REP06420
           MOVE MED-CHAR [BGIN] TO PRO-CHAR [LOOK].                     REP06430
           ADD 1 TO BGIN.                                               REP06440
           ADD 1 TO LOOK.                                               REP06450
           IF BGIN ) 11 GO TO MED-CHAR-MOVE.                            REP06460
       011-LINE9.                                                       REP06470
           MOVE :ARTIST   @: TO RCA.                                    REP06480
           MOVE ART-NAM TO CHAR-1.                                      REP06490
           IF SUMMARY MOVE :FINAL RELEASE    @: TO SELNUM               REP06500
           ELSE MOVE :PROJ RELEASE     @: TO SELNUM.                    REP06510
           IF REL-MON NOT > 0 MOVE MONTH [REL-MON] TO MT.               REP06520
           IF REL-YR NOT > 0 ADD 1900 REL-YR GIVING YR.                 REP06530
           IF REL-MON > 0 AND REL-YR > 0 PERFORM PREFIX-CONV THRU       REP06540
           MED-CHAR-MOVE.                                               REP06550
           WRITE PRINTLINE.                                             REP06560
           MOVE SPACES TO PRINTLINE.                                    REP06570
       011-LINE10.                                                      REP06580
           IF REL-MON NOT > 0 AND REL-YR NOT > 0 PERFORM PREFIX-CONV    REP06590
           THRU MED-CHAR-MOVE.                                          REP06600
           WRITE PRINTLINE.                                             REP06610
           MOVE SPACES TO PRINTLINE.                                    REP06620
       011-LINE11.                                                      REP06630
           MOVE :A+R REP  @: TO RCA.                                    REP06640
           MOVE PRODUCER TO CHAR-1.                                     REP06650
           IF CLASS-CODE NOT > 0 MOVE :SALES CODE @ : TO  CLS-CD.       REP06660
           IF CLASS-CODE NOT > 0 MOVE CLASS-CODE TO SLS-COD.            REP06670
           IF PRICE NOT > 0 MOVE :PRICE @: TO PRS.                      REP06680
           IF PRICE NOT > 0 MOVE PRICE TO DOLS.                         REP06690
           IF SPEC MOVE :SPECIAL: TO SN.                                REP06700
           IF OPP MOVE :OPPORTUNITY: TO SN.                             REP06710
           IF PRO MOVE :PROMOTIONAL: TO SN.                             REP06720
           IF DUBL MOVE :REG AND DJ PROM: TO SN.                        REP06730
           MOVE 1 TO DIVISIONS, DIVS.                                   REP06740
           WRITE PRINTLINE.                                             REP06750
           MOVE SPACES TO PRINTLINE.                                    REP06760
       UNDR-LIN.                                                        REP06770
           MOVE DSHES TO FIRST-HALF.                                    REP06780
           MOVE DSHES TO SECOND-HALF.                                   REP06790
           WRITE PRINTLINE.                                             REP06800
           MOVE SPACES TO PRINTLINE.                                    REP06810
           MOVE 2 TO S-CNT.                                             REP06820
       SIDE-HEAD.                                                       REP06830
           MOVE SPACES TO HEAD-LINE.                                    REP06840
           IF ALBUM OR RL-RL  OR SINGLE GO TO ALB-HD.                   REP06850
           MOVE :PROGRAM: TO PRG-NAM.                                   REP06860
           MOVE DIVISIONS TO NMBR-1.                                    REP06870
           GO TO COL-HD.                                                REP06880
       ALB-HD.                                                          REP06890
           MOVE :SIDE: TO SD-NAM.                                       REP06900
           MOVE DIVISIONS TO NMBR.                                      REP06910
           IF ALBUM OR RL-RL OR SINGLE MOVE MASTER-NO [DIVISIONS] TO    REP06920
           SRL, SERL-HOLD.                                              REP06930
           IF Q-DSK                                                     REP06940
            MOVE LAST-CHAR TO LAST-SPT MOVE :-: TO LAST-CHAR            REP06950
            MOVE SERIAL-HOLD-AREA TO SRL.                               REP06960
           IF NOT SINGLE AND MEDIUM ) 6 AND BAND-AREA NOT > ZERO        REP06970
            PERFORM NO-BAND-CHECK.                                      REP06980
           IF NOT DUBL GO TO COL-HD.                                    REP06990
           IF DIVISIONS > 1 MOVE MASTER-NO [1] TO SRL SRL-X             REP07000
           ELSE MOVE MASTER-NO [2] TO SRL SRL-X.                        REP07010
           MOVE :M: TO SRL-X4.                                          REP07020
           MOVE :[MON-: TO MON-START.                                   REP07030
           MOVE :]: TO MON-FINISH.                                      REP07040
           GO TO COL-HD.                                                REP07050
       NO-BAND-CHECK.                                                   REP07060
           IF NO-BAND [NMBR] > 1                                        REP07070
            MOVE :    [: TO MON-START                                   REP07080
            MOVE :NO-BANDS: TO SRL-X                                    REP07090
            MOVE :]: TO MON-FINISH.                                     REP07100
                                                                        REP07110
       COL-HD.                                                          REP07120
             MOVE :TIME: TO P-TIM.                                      REP07130
           MOVE :REC DATE: TO P-REC-DT.                                 REP07140
           MOVE :SERIAL: TO P-SERIAL.                                   REP07150
           MOVE :JOB NO: TO P-JB.                                       REP07160
           MOVE :PUBLISHER: TO P-PUB.                                   REP07170
       MOVE-1.                                                          REP07180
           MOVE 1 TO OUTS.                                              REP07190
       M1.                                                              REP07200
           MOVE SPACES TO OUT-LINE [OUTS].                              REP07210
           MOVE HEAD-LINE TO OUT-LINE [OUTS].                           REP07220
           ADD 1 TO OUTS.                                               REP07230
           MOVE SPACES TO OUT-LINE [OUTS].                              REP07240
           MOVE SPACES TO HEAD-LINE.                                    REP07250
           MOVE SPACES TO LAY-OUT.                                      REP07260
       M2. IF NOT SINGLE GO TO SX.                                      REP07270
           ADD 1 TO S-CNT.                                              REP07280
           IF MSTRS [S-CNT] > SPACES GO TO M3.                          REP07290
             MOVE :[FROM@: TO S-F-START                                 REP07300
             MOVE :]: TO S-F-END                                        REP07310
             MOVE MSTRS [S-CNT] TO SINGLE-FROM                          REP07320
             PERFORM M1.                                                REP07330
       M3.                                                              REP07340
           IF DIVISIONS > 1 AND INTRO [1] NOT > :00:                    REP07350
           MOVE 1 TO T                                                  REP07360
           MOVE INTRO [1] TO SEC-2 PERFORM INTRO-WRIT                   REP07370
           GO TO SX.                                                    REP07380
           IF DIVISIONS > 2 AND NOT DUBL AND INTRO [2] NOT              REP07390
           > :00: MOVE 2 TO T                                           REP07400
           MOVE INTRO [2] TO SEC-2 PERFORM INTRO-WRIT                   REP07410
           GO TO SX.                                                    REP07420
           IF DIVISIONS > 2 AND DUBL                                    REP07430
           AND INTRO [3] NOT > :00:                                     REP07440
           MOVE INTRO [3] TO SEC-2 MOVE 3 TO T                          REP07450
           PERFORM INTRO-WRIT.                                          REP07460
           GO TO SX.                                                    REP07470
       INTRO-WRIT.                                                      REP07480
           IF INTRO [T] > :99: MOVE :INSTR: TO S-LABEL.                 REP07490
           IF S-LABEL NOT > :INSTR: AND INTRO [T] NOT > SPACES          REP07500
             MOVE :@: TO COL MOVE INTRO [T] TO SEC-2                    REP07510
             MOVE :INTRO: TO S-LABEL.                                   REP07520
           PERFORM 016-WRT-LINE.                                        REP07530
           MOVE SPACES TO OUT-LINE [OUTS].                              REP07540
       SX.  IF NOT ALBUM AND DIVISIONS > 1 MOVE :@: TO COL.             REP07550
           IF SINGLE OR RL-RL AND DIVISIONS > 1 MOVE SPACES TO COL.     REP07560
           IF CASSETTE AND DIVISIONS > 1 MOVE INDENT TO SEC-1.          REP07570
           IF STEREO-8 AND DIVISIONS > 1 MOVE INDENT TO SEC-1.          REP07580
           IF QUAD-8 AND DIVISIONS > 1 MOVE INDENT TO SEC-1.            REP07590
           MOVE LAY-OUT TO HEAD-LINE.                                   REP07600
           MOVE SPACES TO LAY-OUT.                                      REP07610
           MOVE HEAD-LINE TO OUT-LINE [OUTS].                           REP07620
           ADD 1 TO OUTS.                                               REP07630
           IF ALBUM OR RL-RL  OR SINGLE MOVE NO-SELNS [DIVS] TO LIMT.   REP07640
           IF QUAD-8 OR STEREO-8 MOVE NOSLS [DIVS] TO LIMT.             REP07650
           IF CASSETTE MOVE NSELECTS [DIVS] TO LIMT.                    REP07660
           MOVE 0 TO TOT-TIM.                                           REP07670
           PERFORM 012-TUNREAD THRU NXT-ONE   VARYING CUT FROM 1 BY 1   REP07680
           UNTIL CUT \ LIMT.                                            REP07690
           GO TO SIDE-OVER.                                             REP07700
       012-TUNREAD.                                                     REP07710
           MOVE SPACES TO ART-AREA.                                     REP07720
           MOVE ZERO TO GAP-SWITCH.                                     REP07730
           READ REC-IN INTO R2 AT END GO TO END-PAR.                    REP07740
           MOVE R2 TO SAVAREA.                                          REP07750
           MOVE ARTIST-1 TO ART60-1.                                    REP07760
           MOVE CREDS TO 1ST-200-CHAR.                                  REP07770
           IF CONTINUE NOT > :1: GO TO 013-ON.                          REP07780
           READ REC-IN INTO R2 AT END GO TO END-PAR.                    REP07790
           MOVE CREDITS TO 2ND-200-CHAR.                                REP07800
           MOVE ARTST TO ART60-2.                                       REP07810
       013-ON.                                                          REP07820
           PERFORM TUN-CLEAR.                                           REP07830
           MOVE SPACES TO LAY-OUT.                                      REP07840
           MOVE CUT TO CUT-NUM.                                         REP07850
           MOVE :-: TO DSH.                                             REP07860
           MOVE TUNE   TO TITL-1.                                       REP07870
           ADD TIME-1 TO TOT-TIM.                                       REP07880
           DIVIDE 60 INTO TIME-1 GIVING MINS.                           REP07890
           COMPUTE SECS > [TIME-1 -  [MINS * 60]].                      REP07900
           MOVE MINS TO MIN-1.                                          REP07910
           MOVE SECS TO SEC-1.                                          REP07920
           MOVE :@: TO COL.                                             REP07930
           MOVE 1 TO INDE .                                             REP07940
           MOVE JOB-NO TO JBNM.                                         REP07950
       DATE-CONV.                                                       REP07960
           MOVE RDAT [INDE]  TO DATE-2.                                 REP07970
           IF DAY-1 > 0 AND MONTH-1 > 0                                 REP07980
            MOVE YEAR-1 TO RYR.                                         REP07990
           IF DAY-1 \ 0 AND NOT > 99 MOVE DAY-1 TO RDAY.                REP08000
           IF DAY-1 \ 0 AND NOT > 99 MOVE MONTH-1 TO RMON.              REP08010
           IF MONTH-1 \ 0 AND NOT > 99 MOVE YEAR-1 TO RYR.              REP08020
           IF DAY-1 > 0 AND MONTH-1 \ 0  MOVE MONTH-1 TO RDAY.          REP08030
           IF DAY-1 \ 0 AND NOT > 99 MOVE :-: TO DSH1.                  REP08040
           IF MONTH-1 \ 0 AND NOT > 99 MOVE :-: TO DSH3.                REP08050
           IF MONTH-1 > 99 AND YEAR-1 NOT > 0 MOVE :P.M.: TO PM.        REP08060
           IF MONTH-1 > 99 AND YEAR-1 NOT > 0 ADD 1900 YEAR-1 GIVING    REP08070
           PM-YR.                                                       REP08080
       SERIAL-MOVE.                                                     REP08090
           MOVE SERIAL TO SERIAL-1.                                     REP08100
           MOVE 1 TO SUB, SUB1.                                         REP08110
           MOVE SPACES TO STOR-AREA.                                    REP08120
       CREDIT-MOVE.                                                     REP08130
           MOVE CREDS-1 [SUB] TO NUM-TESTA.                             REP08140
           ADD 1 TO SUB.                                                REP08150
           MOVE CREDS-1 [SUB] TO NUM-TESTB.                             REP08160
       CHAR-MOVE.                                                       REP08170
           ADD 1 TO SUB.                                                REP08180
           IF CREDS-1 [SUB] > :$: GO TO END-CHK.                        REP08190
       MOV-1.                                                           REP08200
           MOVE CREDS-1 [SUB] TO STORAGE [SUB1].                        REP08210
           ADD 1 TO SUB1.                                               REP08220
           GO TO CHAR-MOVE.                                             REP08230
       END-CHK.                                                         REP08240
           COMPUTE REL > SUB < 1.                                       REP08250
           IF CREDS-1 [REL] NOT > :$: GO TO MOV-1.                      REP08260
           GO TO 1CHK 2CHK 3CHK 4CHK 5CHK 6CHK 7CHK 8CHK 9CHK           REP08270
           10CHK 11CHK 12CHK 13CHK 14CHK 15CHK 16CHK 17CHK              REP08280
           18CHK 19CHK DEPENDING ON NUM-TEST1.                          REP08290
       1CHK.                                                            REP08300
           MOVE NOTES-1 TO CMP.                                         REP08310
           MOVE SUB1 TO COMNBR.                                         REP08320
           GO TO NXTONE.                                                REP08330
       2CHK.                                                            REP08340
           MOVE NAMES-1 TO LYR.                                         REP08350
           MOVE SUB1 TO LYRNUM.                                         REP08360
           GO TO NXTONE.                                                REP08370
       3CHK.                                                            REP08380
           MOVE NOTES-1 TO CMP.                                         REP08390
           MOVE SUB1 TO COMNBR.                                         REP08400
           GO TO NXTONE.                                                REP08410
       4CHK.                                                            REP08420
           MOVE NAMES-2 TO ARR [CUT].                                   REP08430
           GO TO NXTONE.                                                REP08440
       5CHK.                                                            REP08450
           MOVE NAMES-2 TO COND [CUT].                                  REP08460
           GO TO NXTONE.                                                REP08470
       6CHK.                                                            REP08480
           MOVE NAMES-2 TO AR-C [CUT].                                  REP08490
           GO TO NXTONE.                                                REP08500
       7CHK.                                                            REP08510
           MOVE NAMES-2 TO VOC [CUT].                                   REP08520
           GO TO NXTONE.                                                REP08530
       8CHK.                                                            REP08540
           MOVE NAMES-2 TO INST [CUT].                                  REP08550
           GO TO NXTONE.                                                REP08560
       9CHK.                                                            REP08570
           MOVE NOTES-1 TO PUB1.                                        REP08580
           MOVE SUB1 TO NOTCNT.                                         REP08590
           GO TO NXTONE.                                                REP08600
       10CHK.                                                           REP08610
           MOVE NOTES-1 TO NOT1 [CUT].                                  REP08620
           GO TO NXTONE.                                                REP08630
       11CHK.                                                           REP08640
           MOVE NAMES-2 TO RECDENG [CUT].                               REP08650
           GO TO NXTONE.                                                REP08660
       12CHK.                                                           REP08670
           MOVE NAMES-2 TO MUSCRD [CUT].                                REP08680
           GO TO  NXTONE.                                               REP08690
       13CHK.                                                           REP08700
           MOVE NOTES-1 TO LDVOC [CUT].                                 REP08710
           GO TO NXTONE.                                                REP08720
       14CHK.                                                           REP08730
           MOVE NOTES-1 TO LDINS [CUT].                                 REP08740
           GO TO NXTONE.                                                REP08750
       15CHK.                                                           REP08760
           MOVE NAMES-2 TO PRDCR [CUT].                                 REP08770
           GO TO NXTONE.                                                REP08780
       16CHK.                                                           REP08790
           MOVE NOTES-1 TO SUBTTL.                                      REP08800
           MOVE SUB1 TO SUBTTL-NO.                                      REP08810
           GO TO NXTONE.                                                REP08820
       17CHK.                                                           REP08830
           MOVE NOTES-1 TO TKN-FRM.                                     REP08840
           MOVE SUB1 TO TKN-NO.                                         REP08850
           GO TO NXTONE.                                                REP08860
       18CHK.                                                           REP08870
           MOVE NOTES-1 TO CO-ARTST [CUT].                              REP08880
           GO TO NXT-ONE.                                               REP08890
       19CHK.                                                           REP08900
           MOVE NOTES-1 TO RECSTD [CUT].                                REP08910
       NXTONE.                                                          REP08920
           MOVE 1 TO SUB1.                                              REP08930
           MOVE SPACES TO STOR-AREA.                                    REP08940
           ADD 2 TO SUB.                                                REP08950
           IF SUB ) 401 AND CREDS-1 [SUB] NOT > : : GO TO CREDIT-MOVE.  REP08960
       014-ON.                                                          REP08970
           IF NOTCNT > 0 GO TO 016-WRT-LINE.                            REP08980
           IF NOTCNT \ 40 GO TO PUB-MV.                                 REP08990
           MOVE PB2A TO PUBLSH.                                         REP09000
           GO TO 016-WRT-LINE.                                          REP09010
       PUB-MV.                                                          REP09020
           MOVE 1 TO STRT, BGIN.                                        REP09030
           MOVE 40 TO LOOK.                                             REP09040
       SPC-SRCH.                                                        REP09050
           IF PUB3 [LOOK] > SPACE GO TO 015-MOVE.                       REP09060
           SUBTRACT 1 FROM LOOK.                                        REP09070
           GO TO SPC-SRCH.                                              REP09080
       015-MOVE.                                                        REP09090
           MOVE PUB3 [STRT] TO PBLSHR [BGIN].                           REP09100
           ADD 1 TO STRT.                                               REP09110
           ADD 1 TO BGIN.                                               REP09120
           IF STRT NOT \ LOOK GO TO 015-MOVE.                           REP09130
       016-WRT-LINE.                                                    REP09140
           MOVE LAY-OUT TO OUT-LINE [OUTS].                             REP09150
           ADD 1 TO OUTS.                                               REP09160
           MOVE SPACES TO LAY-OUT.                                      REP09170
       017-WRITER-LINE.                                                 REP09180
           IF CONTINUE NOT > :1: GO TO TAP-CONT.                        REP09190
           IF TUNTIT > SPACES GO TO TAP-CONT.                           REP09200
           MOVE TUNTIT TO TITL-1.                                       REP09210
           IF NOTCNT ) 41 GO TO 018-WRT-LINE.                           REP09220
       MOV-COMP.                                                        REP09230
           MOVE 1 TO BGIN.                                              REP09240
           MOVE NOTCNT TO LOOK.                                         REP09250
           IF LOOK \ 80 MOVE 80 TO LOOK.                                REP09260
           PERFORM 015-MOVE.                                            REP09270
       018-WRT-LINE.                                                    REP09280
           PERFORM SUB-1.                                               REP09290
           MOVE 1 TO NOTCNT.                                            REP09300
       TAP-CONT.                                                        REP09310
           IF TUNE NOT > TUN-LST GO TO SBTTL-IN.                        REP09320
           MOVE :CONTINUED: TO TITL-1.                                  REP09330
           IF NOTCNT \ 40 PERFORM MOV-COMP.                             REP09340
           MOVE 1 TO NOTCNT.                                            REP09350
           PERFORM SUB-1.                                               REP09360
       SBTTL-IN.                                                        REP09370
           MOVE TUNE TO TUN-LST.                                        REP09380
           IF SUBTTL-NO \ 40 GO TO SUB-BK.                              REP09390
           IF SUBTTL-NO > 0 GO TO SHW-CRED.                             REP09400
           MOVE SUBTTL TO TITL-1.                                       REP09410
           GO TO SUB-1.                                                 REP09420
       SUB-BK.                                                          REP09430
           MOVE 40 TO LOOK-1.                                           REP09440
       SUB-2.                                                           REP09450
           IF SUB-CHAR [LOOK-1] > SPACE GO TO SUB-3.                    REP09460
           SUBTRACT 1 FROM LOOK-1.                                      REP09470
           GO TO SUB-2.                                                 REP09480
       SUB-3.                                                           REP09490
           MOVE 1 TO STRT-1, BGIN-1.                                    REP09500
       SUB-4.                                                           REP09510
           MOVE SUB-CHAR [BGIN-1] TO WRT-CHAR [STRT-1].                 REP09520
           ADD 1 TO STRT-1.                                             REP09530
           ADD 1 TO BGIN-1.                                             REP09540
           IF BGIN-1 NOT \ LOOK-1 GO TO SUB-4.                          REP09550
       SUB-5.                                                           REP09560
           IF NOTCNT ) 41 GO TO SUB-1.                                  REP09570
           PERFORM MOV-COMP.                                            REP09580
           MOVE 1 TO NOTCNT.                                            REP09590
       SUB-1.                                                           REP09600
           IF INDE ) 5 ADD 1 TO INDE.                                   REP09610
           IF RDAT [INDE] NOT > 0 AND INDE ) 5 PERFORM DATE-CONV.       REP09620
           PERFORM 016-WRT-LINE.                                        REP09630
       SUB-6.                                                           REP09640
           IF SUBTTL-NO ) 41 GO TO SHW-CRED.                            REP09650
           MOVE 1 TO STRT-1.                                            REP09660
           IF SUBTTL-NO \ 75 MOVE 75 TO SUBTTL-NO.                      REP09670
           MOVE SUBTTL-NO TO LOOK-1.                                    REP09680
           PERFORM SUB-4.                                               REP09690
           PERFORM SUB-1.                                               REP09700
       SHW-CRED.                                                        REP09710
           IF TKN-NO > 0 GO TO 019-LINE2.                               REP09720
           MOVE :[FROM : TO TITL-1.                                     REP09730
           MOVE TKN-FRM TO SUBTTL.                                      REP09740
           MOVE 34 TO LOOK-1.                                           REP09750
           PERFORM SUB-2 THRU SUB-3.                                    REP09760
           MOVE 1 TO BGIN-1.                                            REP09770
           MOVE 7 TO STRT-1.                                            REP09780
           PERFORM SUB-4.                                               REP09790
           IF TKN-NO ) 35 ADD 7 TKN-NO GIVING LOOK-1.                   REP09800
           IF TKN-NO ) 35 MOVE :]: TO WRT-CHAR [LOOK-1].                REP09810
           PERFORM SUB-5 THRU SUB-1.                                    REP09820
           IF TKN-NO ) 35 GO TO 019-LINE2.                              REP09830
           MOVE TKN-NO TO LOOK-1.                                       REP09840
           IF LOOK-1 \ 90 MOVE 90 TO LOOK-1.                            REP09850
           MOVE 1 TO STRT-1.                                            REP09860
           SUBTRACT BGIN-1 FROM LOOK-1 GIVING PAR-SPOT.                 REP09870
           PERFORM SUB-4.                                               REP09880
           ADD 1 TO PAR-SPOT.                                           REP09890
           MOVE :]: TO WRT-CHAR [PAR-SPOT].                             REP09900
           PERFORM 016-WRT-LINE.                                        REP09910
       019-LINE2.                                                       REP09920
           ADD COMNBR LYRNUM GIVING WRTOT.                              REP09930
           IF WRTOT \ 40 GO TO BREAKDOWN. IF WRTOT > 0 GO TO ON-5.      REP09940
           MOVE 40 TO LOOK-1.                                           REP09950
       PAR-MOVE.                                                        REP09960
           MOVE :[: TO WRT-CHAR [1].                                    REP09970
           MOVE 2 TO STRT-1.                                            REP09980
           MOVE 1 TO BGIN-1.                                            REP09990
       COMP-MOVE.                                                       REP10000
           IF CO1 [BGIN-1] > :[: OR :]: MOVE : : TO CO1 [BGIN-1].       REP10010
           MOVE CO1  [BGIN-1] TO WRT-CHAR [STRT-1].                     REP10020
           ADD 1 TO STRT-1.                                             REP10030
           ADD 1 TO BGIN-1.                                             REP10040
           IF BGIN-1 NOT \ COMNBR AND LOOK-1 GO TO COMP-MOVE.           REP10050
           SUBTRACT 1 FROM STRT-1.                                      REP10060
       CHK-LYR.                                                         REP10070
           IF LYRNUM > 0 MOVE :]: TO WRT-CHAR [STRT-1]   ELSE MOVE :-:  REP10080
           TO WRT-CHAR [STRT-1].                                        REP10090
           IF LYRNUM > 0 GO TO ON-5.                                    REP10100
           MOVE 1 TO BGIN-1.                                            REP10110
           ADD 1 TO STRT-1.                                             REP10120
       MOVE-LYR.                                                        REP10130
           IF LYR1 [BGIN-1] > :[: OR :]: MOVE : : TO LYR1 [BGIN-1].     REP10140
           MOVE LYR1 [BGIN-1] TO WRT-CHAR [STRT-1].                     REP10150
           ADD 1 TO BGIN-1.                                             REP10160
           ADD 1 TO STRT-1.                                             REP10170
           IF BGIN-1 NOT \ LYRNUM GO TO MOVE-LYR.                       REP10180
           MOVE :]: TO WRT-CHAR [STRT-1].                               REP10190
           GO TO ON-5.                                                  REP10200
           MOVE ZERO TO KNTS.                                           REP10210
       BREAKDOWN.                                                       REP10220
           IF LYRNUM NOT > 0 GO TO BRK2.                                REP10230
           MOVE CMP TO FIRST-38.                                        REP10240
           MOVE 0 TO WRTOT.                                             REP10250
           MOVE :[: TO OPEN-PAR.                                        REP10260
           MOVE :-: TO CLOSE-PAR.                                       REP10270
           PERFORM ON-5 THRU DUM-5.                                     REP10280
           MOVE 1 TO STRT-1.                                            REP10290
           MOVE 39 TO BGIN-1.                                           REP10300
           MOVE COMNBR TO LOOK-1.                                       REP10310
           GO TO COMP-MOVE.                                             REP10320
       BRK2.                                                            REP10330
           COMPUTE REL > COMNBR < 1.                                    REP10340
           MOVE :-: TO CO1 [REL].                                       REP10350
           MOVE 1 TO BGIN-1.                                            REP10360
           ADD 2 TO COMNBR.                                             REP10370
       COMBINE.                                                         REP10380
           IF LYR1 [BGIN-1] > :[: OR :]: MOVE : : TO LYR1 [BGIN-1].     REP10390
           MOVE LYR1 [BGIN-1] TO CO1 [COMNBR].                          REP10400
           ADD 1 TO BGIN-1.                                             REP10410
           ADD 1 TO COMNBR.                                             REP10420
           IF BGIN-1 NOT \ LYRNUM GO TO COMBINE.                        REP10430
       CARRY-ON.                                                        REP10440
           MOVE 40 TO LOOK-1.                                           REP10450
       ON-3.                                                            REP10460
           IF CO1 [LOOK-1] > :-: GO TO ON-4.                            REP10470
           SUBTRACT 1 FROM LOOK-1.                                      REP10480
           GO TO ON-3.                                                  REP10490
       ON-4.                                                            REP10500
           PERFORM PAR-MOVE THRU COMP-MOVE.                             REP10510
       ON-5.                                                            REP10520
           IF ALBUM OR SINGLE OR RL-RL  GO TO DUM-5.                    REP10530
           IF GAP-THERE GO TO DUM-5.                                    REP10540
           MOVE 1 TO GAP-SWITCH.                                        REP10550
           IF STEREO-8  OR QUAD-8 MOVE GAP [DIVS, CUT] TO SEC-1.        REP10560
           IF CASSETTE MOVE GPS [DIVS, CUT] TO SEC-1.                   REP10570
           IF STEREO-8 AND DIVS > 4 AND CUT > LIMT                      REP10580
           MOVE OUTDENT TO SEC-1.                                       REP10590
           IF QUAD-8 AND DIVS > 4 AND CUT > LIMT                        REP10600
           MOVE OUTDENT TO SEC-1.                                       REP10610
           IF CASSETTE AND DIVS > 2 AND CUT > LIMT                      REP10620
           MOVE OTDNT TO SEC-1.                                         REP10630
           IF STEREO-8 OR QUAD-8 OR CASSETTE  ADD SEC-1 TO TOT-TIM      REP10640
            MOVE :@: TO COL.                                            REP10650
           IF  SEC-1 > 0 MOVE SPACES TO SEC-2, COL.                     REP10660
       DUM-5.                                                           REP10670
           IF INDE ) 5 ADD 1 TO INDE .                                  REP10680
           IF RDAT [INDE]  NOT > 0 AND INDE ) 4   PERFORM DATE-CONV.    REP10690
           IF NOTCNT \ 40 PERFORM MOV-COMP.                             REP10700
           PERFORM 016-WRT-LINE.                                        REP10710
       THIRD-LINE.                                                      REP10720
           MOVE 0 TO LINE-TEST.                                         REP10730
           IF WRTOT )  41 GO TO INDX-CHK.                               REP10740
           MOVE WRTOT TO LOOK-1.                                        REP10750
           MOVE 1 TO STRT-1.                                            REP10760
           PERFORM COMP-MOVE.                                           REP10770
           ADD 1 TO STRT-1.                                             REP10780
           MOVE :]: TO WRT-CHAR [STRT-1].                               REP10790
           MOVE 1 TO LINE-TEST.                                         REP10800
       INDX-CHK.                                                        REP10810
           IF INDE  \ 4 GO TO PRNT-CHK.                                 REP10820
           ADD 1 TO INDE .                                              REP10830
           IF RDAT [INDE]  > ZERO MOVE 5 TO INDE GO TO PRNT-CHK.        REP10840
           PERFORM DATE-CONV.                                           REP10850
           MOVE 1 TO LINE-TEST.                                         REP10860
       PRNT-CHK.                                                        REP10870
           IF LINE-TEST > 1 PERFORM 016-WRT-LINE.                       REP10880
           MOVE SPACES TO TITL-1.                                       REP10890
           ADD 1 TO KNTS.                                               REP10900
           IF KNTS NOT \ 3 AND ARTS [KNTS] NOT > SPACES                 REP10910
             MOVE ARTS [KNTS] TO TITL-1                                 REP10920
             MOVE 1 TO LINE-TEST, ART-LINE [OUTS].                      REP10930
           IF INDE ) 5 OR TITL-1 NOT > SPACES GO TO INDX-CHK.           REP10940
           MOVE 0 TO LINE-TEST.                                         REP10950
           IF CUT > 1 MOVE ART-AREA TO ART-COMPARE                      REP10960
             GO TO NXT-ONE.                                             REP10970
           IF ART-AREA NOT > ART-COMPARE                                REP10980
             ADD 1 TO TOT-ART                                           REP10990
             MOVE 1 TO ART-SWITCH.                                      REP11000
       NXT-ONE.                                                         REP11010
           MOVE SPACES TO ART-AREA.                                     REP11020
           MOVE ZERO TO KNTS.                                           REP11030
           PERFORM 016-WRT-LINE.                                        REP11040
       TUN-CLEAR.                                                       REP11050
           MOVE SPACES TO PUB1, CMP, LYR, SUBTTL.                       REP11060
           MOVE ZERO TO COMNBR, LYRNUM, NOTCNT, NO-CONTINS, SUBTTL-NO.  REP11070
           MOVE 0 TO TKN-NO.                                            REP11080
       ZERO-MOVE.                                                       REP11090
           MOVE ZERO TO ART-LINE [BGIN].                                REP11100
       SIDE-OVER.                                                       REP11110
            IF NOT ONLY-1-ARTIST PERFORM ZERO-MOVE VARYING BGIN FROM 1  REP11120
           BY 1  UNTIL BGIN > OUTS.                                     REP11130
           MOVE ZERO TO ART-SWITCH.                                     REP11140
           IF NOT SINGLE GO TO SIDE-ON1.                                REP11150
           MOVE SPACES TO LAY-OUT.                                      REP11160
           MOVE FADE [DIVISIONS] TO N.                                  REP11170
           ADD 1 TO N.                                                  REP11180
           IF N NOT > 1 MOVE :END: TO S-LABEL.                          REP11190
           MOVE END-AREA [N] TO ENDWRD.                                 REP11200
           PERFORM 020-WRT-LINE.                                        REP11210
           PERFORM 016-WRT-LINE.                                        REP11220
       SIDE-ON1.                                                        REP11230
           MOVE OUTS TO TUNE-INFO. MOVE 1 TO INDE .                     REP11240
           IF ALBUM OR SINGLE OR RL-RL GO TO ART-LISTING.               REP11250
           IF DIVISIONS > 1 ADD INDENT TO TOT-TIM.                      REP11260
           DIVIDE 60 INTO TOT-TIM GIVING MINS.                          REP11270
           COMPUTE SECS > [TOT-TIM - [MINS * 60]].                      REP11280
           ADD TOT-TIM TO TTL-TIM.                                      REP11290
           MOVE :TIMING PROGRAM : TO TIM-TIT.                           REP11300
           MOVE DIVISIONS TO PROG-NUM.                                  REP11310
           MOVE MINS TO MIN-1.                                          REP11320
           MOVE SECS TO SEC-1. MOVE :@: TO COL.                         REP11330
           PERFORM 016-WRT-LINE.                                        REP11340
           PERFORM 016-WRT-LINE.                                        REP11350
           IF CASSETTE AND DIVS ) 2 GO TO ART-LISTING.                  REP11360
           IF STEREO-8 AND DIVS ) 4 GO TO ART-LISTING.                  REP11370
           IF QUAD-8 AND DIVS ) 4 GO TO ART-LISTING.                    REP11380
           MOVE :TOTAL: TO TIM-TIT.                                     REP11390
           DIVIDE 60 INTO TTL-TIM GIVING TOT-MINS.                      REP11400
           COMPUTE TOT-SECS > [TTL-TIM - [TOT-MINS * 60]].              REP11410
           MOVE TOT-MINS TO MIN-1.                                      REP11420
           MOVE TOT-SECS TO SEC-1.                                      REP11430
           MOVE :@: TO COL.                                             REP11440
           PERFORM 016-WRT-LINE.                                        REP11450
       ART-LISTING.                                                     REP11460
           IF TOT-ART \ 1 MOVE 1 TO INDE GO TO NOTE-LIST.               REP11470
           MOVE ART-COMPARE TO ART-AREA.                                REP11480
           MOVE :ARTIST: TO TITL-OUT-5.                                 REP11490
           MOVE 0 TO KNTS.                                              REP11500
       AL1.                                                             REP11510
           ADD 1 TO KNTS.                                               REP11520
           IF KNTS NOT \ 3 AND ARTS [KNTS] NOT > SPACES                 REP11530
             MOVE ARTS [KNTS] TO A-C-OUT-2 PERFORM 020-WRT-LINE         REP11540
           ELSE MOVE 1 TO INDE GO TO NOTE-LIST.                         REP11550
           GO TO AL1.                                                   REP11560
       020-WRT-LINE.                                                    REP11570
           IF OUTS \ 51 PERFORM CONT-PG THRU MOVE-UP.                   REP11580
           PERFORM 016-WRT-LINE.                                        REP11590
       CO-ART.                                                          REP11600
           MOVE 1 TO INDE.                                              REP11610
           MOVE :CO-ARTIST : TO CRED-TITL.                              REP11620
       CO-ART1.                                                         REP11630
           IF CO-ARTST [1] > SPACES OR CO-ARTST [1] NOT > CO-ARTST      REP11640
           [INDE] GO TO CO-MOVE.                                        REP11650
           ADD 1 TO INDE.                                               REP11660
           IF INDE NOT \ LIMT GO TO CO-ART1.                            REP11670
           MOVE CO-ARTST [1] TO A-C-OUT-1.                              REP11680
           MOVE CRED-TITL TO TITL-OUT-4.                                REP11690
           PERFORM 020-WRT-LINE.                                        REP11700
           GO TO ON-11.                                                 REP11710
       CO-MOVE.                                                         REP11720
           MOVE 1 TO INDE.                                              REP11730
       ON-30.                                                           REP11740
           MOVE CO-ARTST [INDE] TO NOT1 [INDE].                         REP11750
           ADD 1 TO INDE.                                               REP11760
           IF INDE NOT \ LIMT GO TO ON-30.                              REP11770
           PERFORM ARR-LST-COMP.                                        REP11780
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP11790
           GO TO ON-11.                                                 REP11800
       NOTE-LIST.                                                       REP11810
           IF NOT1 [1] NOT > NOT1 [INDE] OR NOT1 [1] > SPACES GO TO     REP11820
           NOTE-LIST-1.                                                 REP11830
           ADD 1 TO INDE .                                              REP11840
           IF INDE  NOT \ LIMT GO TO NOTE-LIST.                         REP11850
           MOVE NOT1 [1] TO A-C-OUT-2.                                  REP11860
           MOVE 1 TO INDE .                                             REP11870
           PERFORM 020-WRT-LINE.                                        REP11880
           GO TO CO-ART.                                                REP11890
       NOTE-LIST-1.                                                     REP11900
           MOVE 0 TO INDE .                                             REP11910
           MOVE 1 TO LOOK, STRT, BGIN.                                  REP11920
       ON-7.                                                            REP11930
           ADD 1 TO INDE .                                              REP11940
           IF INDE  \ LIMT GO TO ON-10.                                 REP11950
           MOVE INDE  TO LOOK.                                          REP11960
       ON-8.                                                            REP11970
           IF NOT1 [INDE]  > SPACES GO TO ON-7.                         REP11980
           IF NOT1 [INDE]  > NOT1 [LOOK] GO TO MATCH-1.                 REP11990
           ADD 1 TO LOOK.                                               REP12000
           IF LOOK NOT \ LIMT GO TO ON-8 ELSE GO TO ON-9.               REP12010
       MATCH-1.                                                         REP12020
           MOVE LOOK TO TUN-MTCH-1 [STRT].                              REP12030
           IF INDE  NOT > LOOK MOVE SPACES TO NOT1 [LOOK].              REP12040
           ADD 1 TO LOOK.                                               REP12050
           ADD 1 TO STRT.                                               REP12060
           IF LOOK NOT \ LIMT GO TO ON-8.                               REP12070
       ON-9.                                                            REP12080
           MOVE 1 TO STRT.                                              REP12090
           MOVE TUN-MTCH-1 [BGIN] TO TUN-CRED [BGIN].                   REP12100
           ADD 1 TO BGIN.                                               REP12110
           IF TUN-MTCH-1 [BGIN] > 0 GO TO ON-10.                        REP12120
           COMPUTE REL > BGIN - 1.                                      REP12130
           MOVE :,: TO COMA [REL].                                      REP12140
           GO TO ON-9.                                                  REP12150
       ON-10.                                                           REP12160
           MOVE ZEROES TO TUN-MTCH.                                     REP12170
       RIT-NOT.                                                         REP12180
           IF INDE \ LIMT GO TO CO-ART.                                 REP12190
           IF NOT1 [INDE] > SPACES GO TO CO-ART.                        REP12200
           IF BGIN \ 5 PERFORM 020-WRT-LINE.                            REP12210
           MOVE NOT1 [INDE] TO A-C-OUT-2.                               REP12220
           MOVE :NOTES: TO TITL-OUT-5.                                  REP12230
           PERFORM 020-WRT-LINE.                                        REP12240
           MOVE 1 TO BGIN.                                              REP12250
           GO TO ON-7.                                                  REP12260
       ON-11. MOVE 1 TO INDE .                                          REP12270
           MOVE :ARRANGED BY : TO CRED-TITL.                            REP12280
       ARRANG-LST.                                                      REP12290
           IF ARR [1] > SPACES OR ARR [1] NOT > ARR [INDE]  GO TO ARRMV.REP12300
           ADD 1 TO INDE .                                              REP12310
           IF INDE  NOT \ LIMT GO TO ARRANG-LST.                        REP12320
           MOVE CRED-TITL TO TITL-OUT-5.                                REP12330
           MOVE ARR [1] TO A-C-OUT-2.                                   REP12340
           PERFORM 020-WRT-LINE.                                        REP12350
           GO TO COND-LST.                                              REP12360
       ARRMV.                                                           REP12370
           MOVE 1 TO INDE .                                             REP12380
       ON-12.                                                           REP12390
           MOVE SPACES TO NOT1 [INDE] .                                 REP12400
           MOVE ARR [INDE]  TO NOT1 [INDE] .                            REP12410
           ADD 1 TO INDE .                                              REP12420
           IF INDE  NOT \ LIMT GO TO ON-12.                             REP12430
       ARR-LST-COMP.                                                    REP12440
           MOVE 0 TO INDE .                                             REP12450
           MOVE 1 TO LOOK, STRT, BGIN.                                  REP12460
       PERF-MOV.                                                        REP12470
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP12480
       COND-LST.                                                        REP12490
           MOVE 1 TO INDE .                                             REP12500
           MOVE :CONDUCTED BY: TO CRED-TITL.                            REP12510
       CO-LST-1.                                                        REP12520
           IF COND [1] > SPACES OR COND [1] NOT > COND [INDE]  GO TO    REP12530
           CON-MV.                                                      REP12540
           ADD 1 TO INDE .                                              REP12550
           IF INDE  NOT \ LIMT GO TO CO-LST-1.                          REP12560
           MOVE COND [1] TO A-C-OUT-2.                                  REP12570
           MOVE CRED-TITL TO TITL-OUT-5.                                REP12580
           PERFORM 020-WRT-LINE.                                        REP12590
           GO TO A-C-LST.                                               REP12600
       CON-MV.                                                          REP12610
           MOVE 1 TO INDE .                                             REP12620
       ON-13.                                                           REP12630
           MOVE COND [INDE]  TO NOT1 [INDE] .                           REP12640
           ADD 1 TO INDE .                                              REP12650
           IF INDE  NOT \ LIMT GO TO ON-13.                             REP12660
           PERFORM ARR-LST-COMP.                                        REP12670
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP12680
       A-C-LST.                                                         REP12690
           MOVE 1 TO INDE . MOVE :ARRANGED + CONDUCTED BY: TO CRED-TITL.REP12700
       A-C-LST-1.                                                       REP12710
           IF AR-C [1] > SPACES OR AR-C [1] NOT > AR-C [INDE]  GO TO    REP12720
           A-C-MV.                                                      REP12730
           ADD 1 TO INDE .                                              REP12740
           IF INDE  NOT \ LIMT GO TO A-C-LST-1.                         REP12750
           MOVE AR-C [1] TO A-C-OUT-2.                                  REP12760
           MOVE :ARRANGED + CONDUCTED BY: TO TITL-OUT-5.                REP12770
           PERFORM 020-WRT-LINE.                                        REP12780
           GO TO VOC-AC.                                                REP12790
       A-C-MV.                                                          REP12800
           MOVE 1 TO INDE .                                             REP12810
       ON-14.                                                           REP12820
           MOVE AR-C [INDE]  TO NOT1 [INDE] .                           REP12830
           ADD 1 TO INDE .                                              REP12840
           IF INDE  NOT \ LIMT GO TO ON-14.                             REP12850
           PERFORM ARR-LST-COMP.                                        REP12860
       PERF-MOV-2.                                                      REP12870
           PERFORM ON-7 THRU ON-10.                                     REP12880
           IF NOT1 [INDE]  > SPACES GO TO VOC-AC.                       REP12890
           IF BGIN \ 5 PERFORM 020-WRT-LINE.                            REP12900
           MOVE NOT1 [INDE] TO A-C-OUT-2.                               REP12910
           MOVE CRED-TITL TO TITL-OUT-5.                                REP12920
           PERFORM 020-WRT-LINE.                                        REP12930
           MOVE 1 TO BGIN.                                              REP12940
           IF INDE  ) LIMT GO TO PERF-MOV-2.                            REP12950
       VOC-AC.                                                          REP12960
           MOVE 1 TO INDE .                                             REP12970
           MOVE :VOCAL ACCOMPANIMENT BY: TO CRED-TITL.                  REP12980
       VOC-AC-1.                                                        REP12990
           IF VOC [1] > SPACES OR VOC [1] NOT > VOC [INDE]  GO TO VO-MV.REP13000
           ADD 1 TO INDE .                                              REP13010
           IF INDE  NOT \ LIMT GO TO VOC-AC-1.                          REP13020
           MOVE VOC [1] TO A-C-OUT-2.                                   REP13030
           MOVE CRED-TITL TO TITL-OUT-5.                                REP13040
           PERFORM 020-WRT-LINE.                                        REP13050
           GO TO INS-AC.                                                REP13060
       VO-MV.                                                           REP13070
           MOVE 1 TO INDE .                                             REP13080
       ON-15.                                                           REP13090
           MOVE VOC [INDE]  TO NOT1 [INDE] .                            REP13100
           ADD 1 TO INDE .                                              REP13110
           IF INDE  NOT \ LIMT GO TO ON-15.                             REP13120
           PERFORM ARR-LST-COMP.                                        REP13130
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP13140
       INS-AC.                                                          REP13150
           MOVE 1 TO INDE .                                             REP13160
           MOVE :INST. ACCOMPANIMENT BY: TO CRED-TITL.                  REP13170
       INS-AC-1.                                                        REP13180
           IF INST [1] > SPACES OR INST [1] NOT > INST [INDE]  GO TO    REP13190
           INS-MOV.                                                     REP13200
           ADD 1 TO INDE .                                              REP13210
           IF INDE  NOT \ LIMT GO TO INS-AC-1.                          REP13220
           MOVE INST [1] TO A-C-OUT-2.                                  REP13230
           MOVE CRED-TITL TO TITL-OUT-5.                                REP13240
           PERFORM 020-WRT-LINE.                                        REP13250
           GO TO REC-STD.                                               REP13260
       INS-MOV.                                                         REP13270
           MOVE 1 TO INDE .                                             REP13280
       ON-16.                                                           REP13290
           MOVE INST [INDE]  TO NOT1 [INDE] .                           REP13300
           ADD 1 TO INDE .                                              REP13310
           IF INDE  NOT \ LIMT GO TO ON-16.                             REP13320
           PERFORM ARR-LST-COMP.                                        REP13330
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP13340
       REC-STD.                                                         REP13350
           MOVE 1 TO INDE .                                             REP13360
           MOVE :RECORDING STUDIO : TO CRED-TITL.                       REP13370
       REC-STD-1.                                                       REP13380
           IF RECSTD [1] > SPACES OR RECSTD [1] NOT > RECSTD [INDE]     REP13390
           GO TO RS-MOV.                                                REP13400
           ADD 1 TO INDE .                                              REP13410
           IF INDE  NOT \ LIMT GO TO REC-STD-1.                         REP13420
           MOVE RECSTD [1] TO A-C-OUT-2.                                REP13430
           MOVE CRED-TITL TO TITL-OUT-5.                                REP13440
           PERFORM 020-WRT-LINE.                                        REP13450
           GO TO REC-ENG.                                               REP13460
       RS-MOV.                                                          REP13470
           MOVE 1 TO INDE .                                             REP13480
       ON-165.                                                          REP13490
           MOVE RECSTD [INDE]  TO NOT1 [INDE] .                         REP13500
           ADD 1 TO INDE .                                              REP13510
           IF INDE  NOT \ LIMT GO TO ON-165.                            REP13520
           PERFORM ARR-LST-COMP.                                        REP13530
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP13540
       REC-ENG.                                                         REP13550
           MOVE 1 TO INDE .                                             REP13560
           MOVE :RECORDING ENGINEER : TO CRED-TITL.                     REP13570
       REC-ENG-1.                                                       REP13580
           IF RECDENG [1] > SPACES OR RECDENG [1] NOT > RECDENG [INDE]  REP13590
           GO TO RE-MOV.                                                REP13600
           ADD 1 TO INDE .                                              REP13610
           IF INDE  NOT \ LIMT GO TO REC-ENG-1.                         REP13620
           MOVE RECDENG [1] TO A-C-OUT-2.                               REP13630
           MOVE CRED-TITL TO TITL-OUT-5.                                REP13640
           PERFORM 020-WRT-LINE.                                        REP13650
           GO TO MUS-CRD.                                               REP13660
       RE-MOV.                                                          REP13670
           MOVE 1 TO INDE .                                             REP13680
       ON-17.                                                           REP13690
           MOVE RECDENG [INDE]  TO NOT1 [INDE] .                        REP13700
           ADD 1 TO INDE .                                              REP13710
           IF INDE  NOT \ LIMT GO TO ON-17.                             REP13720
           PERFORM ARR-LST-COMP.                                        REP13730
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP13740
       MUS-CRD.                                                         REP13750
           MOVE 1 TO INDE .                                             REP13760
           MOVE :MUSICAL COORDINATOR: TO CRED-TITL.                     REP13770
       MUS-CRD-1.                                                       REP13780
           IF MUSCRD [1] > SPACES OR MUSCRD [1] NOT > MUSCRD [INDE]  GO REP13790
           TO MUS-MOV.                                                  REP13800
           ADD 1 TO INDE .                                              REP13810
           IF INDE  NOT \ LIMT GO TO MUS-CRD-1.                         REP13820
           MOVE MUSCRD [2] TO A-C-OUT-2.                                REP13830
           MOVE CRED-TITL TO TITL-OUT-5.                                REP13840
           PERFORM 020-WRT-LINE.                                        REP13850
           GO TO PRO-CRD.                                               REP13860
       MUS-MOV.                                                         REP13870
           MOVE 1 TO INDE .                                             REP13880
       ON-18. MOVE MUSCRD [INDE]  TO NOT1 [INDE] .                      REP13890
           ADD 1 TO INDE .                                              REP13900
           IF INDE  NOT \ LIMT GO TO ON-18.                             REP13910
           PERFORM ARR-LST-COMP.                                        REP13920
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP13930
       PRO-CRD.                                                         REP13940
           MOVE 1 TO INDE.                                              REP13950
           MOVE :PRODUCED BY: TO CRED-TITL.                             REP13960
       PRO-CRD-1.                                                       REP13970
           IF PRDCR [1] > SPACES OR PRDCR [1] NOT > PRDCR [INDE] GO TO  REP13980
           PRO-MOV.                                                     REP13990
           ADD 1 TO INDE.                                               REP14000
           IF INDE NOT \ LIMT GO TO PRO-CRD-1.                          REP14010
           MOVE PRDCR [1] TO A-C-OUT-2.                                 REP14020
           MOVE CRED-TITL TO TITL-OUT-5.                                REP14030
           PERFORM 020-WRT-LINE.                                        REP14040
           GO TO LD-VOC.                                                REP14050
       PRO-MOV.                                                         REP14060
           MOVE 1 TO INDE.                                              REP14070
       ON-19.                                                           REP14080
           MOVE PRDCR [INDE] TO NOT1 [INDE].                            REP14090
           ADD 1 TO INDE.                                               REP14100
           IF INDE NOT \ LIMT GO TO ON-19.                              REP14110
           PERFORM ARR-LST-COMP.                                        REP14120
           PERFORM PERF-MOV-2 THRU VOC-AC.                              REP14130
       LD-VOC.                                                          REP14140
           MOVE 1 TO INDE .                                             REP14150
       LD-VOC-1.                                                        REP14160
           MOVE LDVOC [INDE]  TO NOT1 [INDE] .                          REP14170
           ADD 1 TO INDE .                                              REP14180
           IF INDE  NOT \ LIMT GO TO LD-VOC-1.                          REP14190
           MOVE 1 TO INDE .                                             REP14200
           PERFORM NOTE-LIST THRU ON-11.                                REP14210
       LD-INS.                                                          REP14220
           MOVE LDINS [INDE]  TO NOT1 [INDE] .                          REP14230
           ADD 1 TO INDE .                                              REP14240
           IF INDE  NOT \ LIMT GO TO LD-INS.                            REP14250
           MOVE 1 TO INDE .                                             REP14260
           PERFORM NOTE-LIST THRU ON-11.                                REP14270
       WRIT-PAGE.                                                       REP14280
           ADD OUTS TO TOT-OUTS.                                        REP14290
           IF TOT-ART IS LESS THAN 9 SUBTRACT LIMT FROM TOT-OUTS.       REP14300
           IF TOT-OUTS \ 51 PERFORM PAG-CHANG.                          REP14310
           MOVE 1 TO KNT.                                               REP14320
           PERFORM BLANK-LINE THRU BLANK-EXIT.                          REP14330
           WRITE PRINTLINE FROM OUT-LINE [1].                           REP14340
           PERFORM COM-PAG THRU C-RETURN VARYING LOOK FROM 2 BY 1       REP14350
           UNTIL LOOK > OUTS.                                           REP14360
           GO TO CLEAN-OUT.                                             REP14370
       COM-PAG.                                                         REP14380
           IF ART-LINE [LOOK] NOT > 1 WRITE PRINTLINE FROM OUT-LINE     REP14390
           [LOOK] GO TO C-RETURN.                                       REP14400
           MOVE OUT-LINE [LOOK] TO LAYOUT-1.                            REP14410
           IF REG-DAT NOT > SPACES MOVE SPACES TO ARTIST-7              REP14420
            MOVE LAYOUT-1 TO OUT-LINE [LOOK]                            REP14430
            WRITE PRINTLINE FROM OUT-LINE [LOOK].                       REP14440
       C-RETURN.                                                        REP14450
           EXIT.                                                        REP14460
       PAG-CHANG.                                                       REP14470
           PERFORM PAGE-TOP.                                            REP14480
           MOVE SPACES TO PRINTLINE.                                    REP14490
           WRITE PRINTLINE.                                             REP14500
           MOVE :RCA RECORDS: TO RCA.                                   REP14510
           IF NOT SUMMARY MOVE :LISTING NOTICE-CONTINUED: TO L-NOT      REP14520
           ELSE MOVE :PRODUCT SUMMARY: TO L-NOT.                        REP14530
           MOVE :SELECTION NUMBER @: TO SELNUM.                         REP14540
           MOVE SEL-NO TO SN1.                                          REP14550
           ADD 1 TO PAG-CNT.                                            REP14560
           MOVE PAG-CNT TO PAG-SPT.                                     REP14570
           MOVE :PAGE@ : TO PAG-LBL.                                    REP14580
           WRITE PRINTLINE.                                             REP14590
           MOVE SPACES TO PRINTLINE.                                    REP14600
           MOVE DIVS TO DIV-SAV.                                        REP14610
           MOVE DIVISIONS TO DIVON-SAV.                                 REP14620
           PERFORM 01-LINE1-3 THRU UNDR-LIN.                            REP14630
           MOVE DIV-SAV TO DIVS.                                        REP14640
           MOVE DIVON-SAV TO DIVISIONS.                                 REP14650
           MOVE OUTS TO TOT-OUTS.                                       REP14660
       CLEAN-OUT.                                                       REP14670
           MOVE SPACES TO LINE1, ART-COMPARE.                           REP14680
           MOVE ZEROES TO ALT-1, ALT-2, MAIN, ART-LINE-1, TUN-MTCH,     REP14690
           TUN-INFO, TOT-MINS, TOT-SECS.                                REP14700
           MOVE 1 TO TOT-ART, A1, A2, A3.                               REP14710
       NXT-SIDE.                                                        REP14720
           ADD 1 TO DIVISIONS.                                          REP14730
           ADD 1 TO DIVS.                                               REP14740
           IF DIVS \ 4 GO TO NXT-ALBM.                                  REP14750
           IF ALBUM     AND MASTER-NO [DIVS] NOT > SPACES               REP14760
           GO TO SIDE-HEAD.                                             REP14770
           IF RL-RL AND MASTER-NO [DIVS] NOT > SPACES GO TO SIDE-HEAD.  REP14780
           IF SINGLE AND DIVS ) 3 GO TO SIDE-HEAD.                      REP14790
           IF STEREO-8 OR QUAD-8                                        REP14800
            PERFORM 2-PAGE THRU 2-PAGE-EXIT.                            REP14810
           IF QUAD-8 AND DIVS ) 5 GO TO SIDE-HEAD.                      REP14820
           IF STEREO-8 AND DIVS ) 5 GO TO SIDE-HEAD.                    REP14830
           IF CASSETTE AND DIVS ) 3 GO TO SIDE-HEAD.                    REP14840
       NXT-ALBM.                                                        REP14850
           MOVE 0 TO TOT-OUTS,  TTL-TIM, PAG-CNT.                       REP14860
           GO TO 01-LINE1-2.                                            REP14870
                                                                        REP14880
       2-PAGE.                                                          REP14890
           IF PAG-CNT \ 1 OR DIVS NOT > 3 GO TO 2-PAGE-EXIT.            REP14900
           PERFORM PAG-CHANG.                                           REP14910
           MOVE 0 TO OUTS, TOTAL-OUTS.                                  REP14920
       2-PAGE-EXIT.                                                     REP14930
           EXIT.                                                        REP14940
                                                                        REP14950
       CONT-PG.        IF TUN-INFO < TOT-OUTS \ 51 PERFORM PAG-CHANG.   REP14960
           MOVE 1 TO KNT.                                               REP14970
           PERFORM BLANK-LINE THRU BLANK-EXIT.                          REP14980
           WRITE PRINTLINE FROM OUT-LINE [1].                           REP14990
           PERFORM COM-PAG THRU C-RETURN VARYING LOOK FROM 2 BY 1       REP15000
           UNTIL LOOK > TUNE-INFO < 1.                                  REP15010
           PERFORM PAG-CHANG.                                           REP15020
           MOVE 1 TO BGIN-1.                                            REP15030
       MOVE-UP.                                                         REP15040
           MOVE OUT-LINE [LOOK] TO OUT-LINE [BGIN-1].                   REP15050
           ADD 1 TO LOOK.                                               REP15060
           ADD 1 TO BGIN-1.                                             REP15070
           IF LOOK ) 51 GO TO MOVE-UP.                                  REP15080
           MOVE BGIN-1 TO OUTS.                                         REP15090
           MOVE 0 TO TOT-OUTS.                                          REP15100
       END-PAR.                                                         REP15110
           MOVE SPACES TO PRINTLINE.                                    REP15120
           MOVE WS-CONTROL-CH TO PRINT-1.                               REP15130
           WRITE PRINTLINE.                                             REP15140
           MOVE 0 TO CNTRL-ACT-KEY.                                     REP15150
           READ CNTRL INVALID KEY GO TO CNTRL-ERR.                      REP15160
           MOVE ZEROS TO BR2050-DISCON BR2050-KEY-ER.                   REP15170
           REWRITE CNTRL-REC INVALID KEY GO TO CNTRL-ERR.               REP15180
           CLOSE CNTRL.                                                 REP15190
           CLOSE REC-IN.                                                REP15200
           CLOSE REC-OUT.                                               REP15210
           STOP RUN.                                                    REP15220
       PAGE-TOP.                                                        REP15230
           MOVE PRINT-1 TO PRINT-HOLDER.                                REP15240
           MOVE SPACES TO PRINT-1.                                      REP15250
           MOVE WS-CONTROL-CH TO PRINT-1.                               REP15260
           WRITE PRINTLINE.                                             REP15270
           MOVE PRINT-HOLDER TO PRINT-1.                                REP15280
       BLANK-LINE.                                                      REP15290
           MOVE PRINT-1 TO PRINT-HOLDER                                 REP15300
           MOVE SPACES TO PRINT-1.                                      REP15310
       WRITE-BLANK-LINE.                                                REP15320
           WRITE PRINTLINE.                                             REP15330
           SUBTRACT 1 FROM KNT.                                         REP15340
           IF KNT NOT > ZERO GO TO WRITE-BLANK-LINE                     REP15350
           ELSE MOVE PRINT-HOLDER TO PRINT-1.                           REP15360
       BLANK-EXIT.                                                      REP15370
           EXIT.                                                        REP15380
       CNTRL-WRITE.                                                     REP15390
           MOVE 0 TO CNTRL-ACT-KEY.                                     REP15400
           READ CNTRL INVALID KEY GO TO CNTRL-ERR.                      REP15410
           MOVE SELECTION TO BR2050-KEY-ER.                             REP15420
           REWRITE CNTRL-REC INVALID KEY GO TO CNTRL-ERR.               REP15430
           MOVE 10 TO CNTRL-ACT-KEY.                                    REP15440
           READ CNTRL INVALID KEY GO TO CNTRL-ERR.                      REP15450
           GO TO CNTRL-WR-RETURN.                                       REP15460
       CNTRL-ERR.                                                       REP15470
           MOVE :SET: TO VAR1.                                          REP15480
           MOVE :CONSPOOL: TO VAR2.                                     REP15490
           MOVE :ON: TO VAR3.                                           REP15500
           CALL :CSS: USING RET-CD, VAR1, VAR2, VAR3.                   REP15510
           DISPLAY :ERROR IN PROGRAM BR2050: UPON CONSOLE.              REP15520
           DISPLAY :WHILE PROCESSING CNTRL FILE: UPON CONSOLE.          REP15530
           MOVE :VP: TO VAR1.                                           REP15540
           MOVE :CLOSE: TO VAR2.                                        REP15550
           MOVE :9: TO VAR3.                                            REP15560
           CALL :CSS: USING RET-CD, VAR1, VAR2, VAR3.                   REP15570
           MOVE :LOGOUT: TO VAR3.                                       REP15580
           MOVE :*: TO VAR3.                                            REP15590
           CALL :CSS: USING RET-CD, VAR1, VAR2, VAR3.                   REP15600
                                                                        REP15610
                                                                        REP15620
       CNTRL-WR-RETURN.                                                 REP15630
           EXIT.                                                        REP15640
       ER-RECOVERY.                                                     REP15650
           READ REC-IN AT END GO TO CNTRL-ERR.                          REP15660
           IF SELECTION LESS THAN BR2050-KEY-ER                         REP15670
             GO TO ER-RECOVERY.                                         REP15680
           MOVE : THERE HAS BEEN A DISCONNECT SINCE THE LAST TIME YOU WEREP15690
      -     :RE ON: TO PRINTLINE.                                       REP15700
           WRITE PRINTLINE.                                             REP15710
           MOVE : PRINTING WILL START FROM THE LAST TRANSACTION PRINTED:REP15720
           TO PRINTLINE.                                                REP15730
           WRITE PRINTLINE.                                             REP15740
           GO TO RESTRT-POINT.                                          REP15750
       CONTROL-CHECK.                                                   REP15760
           IF FIRST-REC AND REM-LCL > :LCL:                             REP15770
             PERFORM CNTRL-WRITE THRU CNTRL-WR-RETURN.                  REP15780
    lPGÝ