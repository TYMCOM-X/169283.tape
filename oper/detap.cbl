000000 IDENTIFICATION DIVISION.                                         00  INPT
000010 PROGRAM-ID.                                                      00  INPT
000020         INPUTC.                                                  00  INPT
000030 AUTHOR.                                                          00  INPT
000040     S L POLLACK.                                                 00  INPT
000050 INSTALLATION.                                                    00  INPT
000060     INFORMATION MANAGEMENT INC.                                  00  INPT
000070 DATE-WRITTEN.                                                    00  INPT
000080     JULY 1967.                                                   00  INPT
000090 SECURITY.                                                        00  INPT
000100     COMPANY CONFIDENTIAL.                                        00  INPT
000110 REMARKS.                                                         00  INPT
000120     IMI/DETAP DECISION TABLE PROCESSOR.                          00  INPT
000140                                                                  00  INPT
000150                                                                  00  INPT
000200     8/25/72- IMPLEMENTED IPARS D30 AND D31.                      00  INPT
000205     2/12/73- CONVERTED TO PDP-10.                                 PDP-10
000210                                                                  00  INPT
005000 ENVIRONMENT DIVISION.                                            00  INPT
005010 CONFIGURATION SECTION.                                           00  INPT
005015 SPECIAL-NAMES.  CHANNEL (1) IS TOP-OF-PAGE.                      PDP-10
005020 SOURCE-COMPUTER. IBM-360.                                        00  INPT
005030 OBJECT-COMPUTER. PDP-10.                                         PDP-10
005040                                                                  00  INPT
005050                                                                  00  INPT
006000 INPUT-OUTPUT SECTION.                                            03  IORT
006010 FILE-CONTROL.                                                    03  IORT
006020     SELECT  FILE-IN                                              03  IORT
006030     ASSIGN TO DSK RECORDING MODE IS ASCII.                       PDP-10
006040     SELECT  LIST-OUT                                             03  IORT
006050     ASSIGN TO DSK RECORDING MODE IS ASCII.                       PDP-10
006060     SELECT  PUNCH-OUT                                            03  IORT
006070     ASSIGN TO DSK RECORDING MODE IS ASCII.                       PDP-10
006080     SELECT  FORMAT-WORK-FILE                                     03  IORT
006090     ASSIGN TO DSK RECORDING MODE IS ASCII.                       PDP-10
006100                                                                  03  IORT
006110                                                                  03  IORT
010000 DATA DIVISION.                                                   00  INPT
010010 FILE SECTION.                                                    03  IORT
010020 FD  FILE-IN                                                      03  IORT
010030     VALUE OF IDENTIFICATION IS SOURCE-ID                         PDP-10
010040     RECORD CONTAINS 80 CHARACTERS                                03  IORT
010050                                                                  PDP-10
010060                                                                  PDP-10
010070     DATA   RECORD  IS INPUTREC.                                  03  IORT
010080 01  INPUTREC.                                                    03  IORT
010090     03  FILLER                  PICTURE X(80).                   03  IORT
010100                                                                  03  IORT
010110                                                                  03  IORT
011000 FD  LIST-OUT                                                     03  IORT
011005     BLOCK CONTAINS 20 RECORDS                                     PDP-10
011010     VALUE OF IDENTIFICATION IS LISTING-ID                        PDP-10
011020                                                                  PDP-10
011030                                                                  PDP-10
011040                                                                  PDP-10
011050     DATA RECORD IS LIST-REC.                                     PDP-10
011060 01  LIST-REC                    PICTURE X(132).                  PDP-10
012000 FD  PUNCH-OUT                                                    03  IORT
012005     BLOCK CONTAINS 20 RECORDS                                     PDP-10
012010     VALUE OF IDENTIFICATION IS OUTPUT-ID                         PDP-10
012020     RECORD CONTAINS 80 CHARACTERS                                03  IORT
012030                                                                  PDP-10
012040                                                                  PDP-10
012050     DATA RECORD IS PUNCHOUT.                                     03  IORT
012060 01  PUNCHOUT.                                                    03  IORT
012070     03  FILLER                  PICTURE X(80).                   03  IORT
012080                                                                  03  IORT
012090                                                                  03  IORT
013000 FD  FORMAT-WORK-FILE                                             03  IORT
013005      BLOCK CONTAINS 3 RECORDS                                     PDP-10
013010     VALUE OF IDENTIFICATION IS WORK-FL-ID                        PDP-10
013020     RECORD CONTAINS 660 CHARACTERS                               03  IORT
013030                                                                  PDP-10
013040                                                                  PDP-10
013050     DATA RECORD IS FORMAT-WORK-FILE-RECORD.                      PDP-10
013060 01  FORMAT-WORK-FILE-RECORD.                                     03  IORT
013070     03  FILLER                  PICTURE X(660).                  03  IORT
013080                                                                  03  IORT
013090                                                                  03  IORT
020000 WORKING-STORAGE SECTION.                                         00  INPT
020005 77  VERS-LEVEL-DT               PICTURE X(24)                    00  INPT
020006                         VALUE " DETAP/IMI V4-1 08/25/72".        00  INPT
020010 77  X                           PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020020 77  Y                           PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020030 77  Z                           PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020040 77  Q1                          PICTURE X.                       00  INPT
020050 77  Q2                          PICTURE X.                       00  INPT
020060 77  QX                          PICTURE S9(5)   COMPUTATIONAL    00  INPT
020070                         VALUE ZERO.                              00  INPT
020080 77  CNTR-1                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020090 77  CNTR-2                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020100 77  CNTR-3                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020110 77  CNTR-4                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020120 77  CNTR-5                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020130 77  CNTR-6                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020140 77  CNTR-7                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020150 77  CNTR-8                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020160 77  CNTR-9                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020170 77  LEFT-1                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020180 77  LEFT-2                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020190 77  XR-A                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020200 77  XR-B                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020210 77  LIMITED-ENTRY-IND           PICTURE S9      COMPUTATIONAL    00  INPT
020220                         VALUE +2.                                00  INPT
020230 77  TYPEID                      PICTURE X.                       00  INPT
020240 77  TEST-ID                     PICTURE X(5)                     00  INPT
020245                         VALUE SPACE.                             00  INPT
020270 77  NACTNS                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020290 77  SPEC-NAME                   PICTURE X(9)                     00  INPT
020300                         VALUE "DETAP-IMI".                       00  INPT
020310 77  NEXT-IND                    PICTURE X.                       00  INPT
020320 77  ELSE-IND                    PICTURE X.                       00  INPT
020330 77  RLID                        PICTURE XX                       00  INPT
020340                         VALUE "RL".                              00  INPT
020360 77  DETAP-IND                   PICTURE X.                       00  INPT
020370 77  EXTNS                       PICTURE S9      COMPUTATIONAL.   00  INPT
020380 77  APROW                       PICTURE S9(2)   COMPUTATIONAL.   00  INPT
020390 77  AROW                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020400 77  CROW                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020420 77  TCOL                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020430 77  TBLNO-1                     PICTURE 9(5)                     00  INPT
020440                         VALUE ZERO.                              00  INPT
020450 77  TBLNO-2             REDEFINES TBLNO-1                        00  INPT
020460                                 PICTURE X(5).                    00  INPT
020470 77  ELEM                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020500 77  ELEM2                       PICTURE S9(3)   COMPUTATIONAL.   00  INPT
020510 77  SHELFINDEX                  PICTURE S9(2)   COMPUTATIONAL.   00  INPT
020520 77  STUBINDEX                   PICTURE S9      COMPUTATIONAL.   00  INPT
020530 77  ROWSIZE                     PICTURE S9(2)   COMPUTATIONAL    00  INPT
020540                         VALUE +50.                               00  INPT
020570 77  INTSIZE                     PICTURE S9(3)   COMPUTATIONAL    00  INPT
020580                         VALUE +377.                              00  INPT
020630 77  TBLMT1                      PICTURE S9(3)   COMPUTATIONAL    00  INPT
020640                         VALUE +100.                              00  INPT
020650 77  TBLMT2                      PICTURE S9(3)   COMPUTATIONAL    00  INPT
020660                         VALUE +101.                              00  INPT
020670 77  DETAP-NAME-1                PICTURE X(39)                    00  INPT
020680                         VALUE                                    00  INPT
020690            "             DETAP PREPROCESSOR LISTING".            00  INPT
020700 77  DETAP-NAME-2                PICTURE X(50)                    00  INPT
020710                         VALUE                                    00  INPT
020720            "            PROPERTY OF IMI & TYMSHARE INC.       ".  PDP-10
020750 77  CHARACTER-HOR-DITTO         PICTURE X                        00  INPT
020760                         VALUE ":".                               PDP-10
020790 77  WARNING                     PICTURE X(19)                    00  INPT
020800                         VALUE "******** WARNING  -".             00  INPT
020810 77  TERM-INAL                   PIC X(19)                        00  INPT
020820                         VALUE "*** TERMINAL ERROR-".             00  INPT
020850 77  ELSERROR                    PICTURE X(39)                    00  INPT
020860                         VALUE                                    00  INPT
020870            "NO ACTIONS FOR ELSE RULE-BEWARE LINKAGE".            00  INPT
020880 77  NAMERROR                    PICTURE X(27)                    00  INPT
020890                         VALUE " TABLE NAME IS NOT PRESENT ".     00  INPT
020900 77  PRMERROR                    PICTURE X(35)                    00  INPT
020910                         VALUE                                    00  INPT
020920            " PARAMETERS STATED ARE NOT ACCURATE".                00  INPT
020930 77  UNKERROR                    PICTURE X(48)                    00  INPT
020940                         VALUE                                    00  INPT
020950            " UNKNOWN ERROR- CHECK STATEMENTS BEFORE THIS ONE".   00  INPT
020960 77  STBERROR                    PICTURE X(41)                    00  INPT
020970                         VALUE                                    00  INPT
020980            " NO STUB EXISTS-THERE MUST BE ONE PRESENT".          00  INPT
020990 77  EXTERROR                    PICTURE X(35)                    00  INPT
021000                         VALUE                                    00  INPT
021010            "TOO MANY RULE EXTENSIONS-SEE MANUAL".                00  INPT
021020 77  RULERROR-1                  PICTURE X(29)                    00  INPT
021030                         VALUE "RL-CARDS WRONG CHECK AND REDO".   00  INPT
021060 77  FLERROR                     PICTURE X(39)                    00  INPT
021070                         VALUE                                    00  INPT
021080            "END OF FILE DETECTED IN MIDDLE OF TABLE".            00  INPT
021090 77  READERROR                   PICTURE X(50)                    00  INPT
021100                         VALUE                                    00  INPT
021110            "CARD ABOVE WAS IN ERROR DURING READ-CHECK AND REDO". 00  INPT
021120 77  LMTERROR                    PICTURE X(41)                    00  INPT
021130                         VALUE                                    00  INPT
021140            "SIZE OF INTERNAL TABLES HAS BEEN EXCEEDED".          00  INPT
021150 77  REDUNERROR                  PICTURE X(47)                    00  INPT
021160                         VALUE                                    00  INPT
021170            "REDUNDANT RULES PRESENT     - RULE XXX AND YYY.".    00  INPT
021180 77  CONTRERROR                  PICTURE X(47)                    00  INPT
021190                         VALUE                                    00  INPT
021200            "CONTRADICTORY RULES PRESENT - RULE XXX AND YYY.".    00  INPT
021210 77  ROWENERROR                  PICTURE X(40)                    00  INPT
021220                         VALUE                                    00  INPT
021230            "COND. AND/OR ACTN. ROW HAS MIXED ENTRIES".           00  INPT
021240 77  RLATER                      PICTURE X(54)                    00  INPT
021250                         VALUE                                    00  INPT
021260        "NO RULES OR ACTIONS FOUND-THIS IS NOT A DECISION TABLE". 00  INPT
021270 77  NATNPR                      PICTURE X(18)                    00  INPT
021280                         VALUE "NO ACTIONS PRESENT".              00  INPT
021290 77  NOCONDACTN                  PICTURE X(40)                    00  INPT
021300                         VALUE                                    00  INPT
021310            "NO ENTRIES FOR ABOVE COND. OR ACTION ROW".           00  INPT
021320 77  PSCOND                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021330 77  DOIND                       PICTURE X                        00  INPT
021340                         VALUE "1".                               00  INPT
021350 77  PSACTN                      PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021360 77  STKINDX                     PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021370 77  SUBX                        PICTURE S9(3)   COMPUTATIONAL    00  INPT
021380                         VALUE +1.                                00  INPT
021390 77  OPEN-IO                     PICTURE X                        00  INPT
021400                         VALUE "A".                               00  INPT
021410 77  OPEN-PUNCH                  PICTURE X                        00  INPT
021420                         VALUE "B".                               00  INPT
021430 77  READ-INPUT                  PICTURE X                        00  INPT
021440                         VALUE "C".                               00  INPT
021450 77  WRITE-LIST                  PICTURE X                        00  INPT
021460                         VALUE "D".                               00  INPT
021470 77  WRITE-PUNCH                 PICTURE X                        00  INPT
021480                         VALUE "E".                               00  INPT
021490 77  EJECT-PAGE                  PICTURE X                        00  INPT
021500                         VALUE "9".                               00  INPT
021510 77  CLOSE-IO                    PICTURE X                        00  INPT
021520                         VALUE "F".                               00  INPT
021540                                                                  00  INPT
021550                                                                  00  INPT
021560 77  TMPID                       PICTURE X.                       00  INPT
021570 77  CURRELEM                    PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021580 77  PASTELEM                    PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021590 77  TESTELEM                    PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021600 77  PERFORM-NUMBER              PICTURE S9      COMPUTATIONAL.   00  INPT
021610 77  BND-FIRST-TIME              PICTURE X                        00  INPT
021620                         VALUE ZERO.                              00  INPT
021630 77  CONTRA-SW                   PICTURE X                        00  INPT
021640                         VALUE ZERO.                         00  INPT
021650 77  SEQNUM-SAVE                 PICTURE X(6)                     00  INPT
021660                         VALUE ZERO.                              00  INPT
021670 77  SUBS                        PICTURE S9(3)   COMPUTATIONAL.   00  INPT
021680 77  RULE-PROC-SW                PICTURE X                        00  INPT
021690                         VALUE ZERO.                              00  INPT
021700 77  RLNUM                       PICTURE X                        00  INPT
021710                         VALUE "1".                               00  INPT
021720 77  OP-SET-NE                   PICTURE X(4)                     00  INPT
021730                         VALUE "N  B".                            00  INPT
021740 77  TEMP-TEST                   PICTURE X.                       00  INPT
021750 77  TEMP-PLUG                   PICTURE X.                       00  INPT
021760 77  OP-SET-G                    PICTURE X(4)                     00  INPT
021770                         VALUE "Y$*B".                            00  INPT
021780 77  OP-SET-L                    PICTURE X(4)                     00  INPT
021790                         VALUE "Y*$B".                            00  INPT
021800 77  OP-SET-GE                   PICTURE X(4)                     00  INPT
021810                         VALUE "N*$B".                            00  INPT
021820 77  OP-SET-LE                   PICTURE X(4)                     00  INPT
021830                         VALUE "N$*B".                            00  INPT
021840 77  OP-SET-STD                  PICTURE X(4)                     00  INPT
021850                         VALUE "Y** ".                            00  INPT
021860 77  READFLIN-SKIP-SW            PICTURE X                        00  INPT
021870                         VALUE ZERO.                              00  INPT
021880 77  COMB-MADE-SW                PICTURE X                        00  INPT
021890                         VALUE ZERO.                              00  INPT
021900 77  P-SW                        PICTURE X                        00  INPT
021910                         VALUE ZERO.                              00  INPT
021920 77  H1                          PICTURE X                        00  INPT
021930                         VALUE ZERO.                              00  INPT
021940 77  H2                          PICTURE X                        00  INPT
021950                         VALUE ZERO.                              00  INPT
021960 77  NO-ENT-ON-STUB              PICTURE X                        00  INPT
021970                         VALUE ZERO.                              00  INPT
021980 77  WRITEIND                    PICTURE X                        00  INPT
021990                         VALUE "1".                               00  INPT
022000 77  TRACEALL                    PICTURE X                        00  INPT
022010                         VALUE ZERO.                              00  INPT
022020                                                                  00  INPT
030000 77  VER-LEV-DT-01               PICTURE X(18)                    01  DCMP
030001                         VALUE "V4-1 08/25/72 DCMP".              01  DCMP
030020 77  RELATED-FOUND               PICTURE X                        01  DCMP
030030                         VALUE ZERO.                              01  DCMP
030040 77  ELEMENT1                    PICTURE X.                       01  DCMP
030050 77  ELEMENT2                    PICTURE X                        01  DCMP
030060                         VALUE ZERO.                              01  DCMP
030070 77  ELEMENT3                    PICTURE X                        01  DCMP
030080                         VALUE ZERO.                              01  DCMP
030090 77  ELEMENT4                    PICTURE X                        01  DCMP
030100                         VALUE ZERO.                              01  DCMP
030110 77  LIMIT-1                     PICTURE S9(7)   COMPUTATIONAL.   01  DCMP
030120 77  COL                         PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030130 77  ROW                         PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030140 77  NCNT                        PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030150 77  YCNT                        PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030160 77  CCNT                        PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030170 77  LSCNT                       PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030180 77  NSCNT                       PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030190 77  KTH-COND                    PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030200 77  CNTER                       PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030210 77  TROW                        PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030230 77  TEMP-CNT                    PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030330 77  NXTSIND                     PICTURE X                        01  DCMP
030340                         VALUE "1".                               01  DCMP
030350 77  DXN-IND                     PICTURE X                        01  DCMP
030360                         VALUE "2".                               01  DCMP
030370 77  ACTNIND                     PICTURE X                        01  DCMP
030380                         VALUE "3".                               01  DCMP
030390 77  ELS-IND                     PICTURE X                        01  DCMP
030400                         VALUE "4".                               01  DCMP
030410 77  Y1-IND                      PICTURE X.                       01  DCMP
030420 77  N1-IND                      PICTURE X.                       01  DCMP
030490 77  DESTACK-IND                 PICTURE X.                       01  DCMP
030530 77  BRANCH                      PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
030540 77  SKIP-GEN-SW                 PICTURE X                        01  DCMP
030550                         VALUE ZERO.                              01  DCMP
030560 77  HOLDER-1                    PICTURE S9(7)   COMPUTATIONAL    01  DCMP
030570                         VALUE ZERO.                              01  DCMP
030580 77  HOLDER-2                    PICTURE S9(7)   COMPUTATIONAL    01  DCMP
030590                         VALUE ZERO.                              01  DCMP
030600                                                                  01  DCMP
030610                                                                  01  DCMP
040000 77  VER-LEV-DT-02               PICTURE X(18)                    02  DCOB
040010                         VALUE "V4-1 08/25/72 DCOB".              02  DCOB
040020 77  START-STUB                  PICTURE S9(2)   COMPUTATIONAL.   02  DCOB
040030 77  END-STUB                    PICTURE S9(2)   COMPUTATIONAL.   02  DCOB
040070 77  ACTION-SUB                  PICTURE S9(3)   COMPUTATIONAL.   02  DCOB
040080 77  COND-SUB                    PICTURE S9(3)   COMPUTATIONAL.   02  DCOB
040090 77  COND-LST-SUB                PICTURE S9(3)   COMPUTATIONAL.   02  DCOB
040110 77  ACT-LST-SUB                 PICTURE S9(3)   COMPUTATIONAL.   02  DCOB
040130 77  NEXT-SUB                    PICTURE S9(3)   COMPUTATIONAL.   02  DCOB
040240 77  MAX                         PICTURE S9(3)   COMPUTATIONAL    02  DCOB
040250                         VALUE ZERO.                              02  DCOB
040255 77  SPILL-OPEN-SW               PICTURE X                        02  DCOB
040256                         VALUE ZERO.                              02  DCOB
040260 77  DETAP-TRACE-1               PICTURE X(21)                    02  DCOB
040270                         VALUE "DISPLAY  DETAP TRACE*".           02  DCOB
040300 77  DETAP-ELSE                  PICTURE X(38)                    02  DCOB
040310                         VALUE                                    02  DCOB
040320            "DISPLAY  ELSE RULE NONE SPECIFIED-TBL=".             02  DCOB
040330 77  STOPRUN                     PICTURE X(9)                     02  DCOB
040340                         VALUE "STOP RUN.".                       02  DCOB
040350 77  PERFRM-SENT                 PICTURE X(35)                    02  DCOB
040360                         VALUE                                    02  DCOB
040370            "PERFORM ATXXXXXYYY THRU ATXXXXXYYY.".                02  DCOB
040380 77  DETAP-TRACE-3               PICTURE X(5)                     02  DCOB
040390                         VALUE "*ELSE".                           02  DCOB
040400 77  NOACTFRULE                  PICTURE X(45)                    02  DCOB
040410                         VALUE                                    02  DCOB
040420            "******** WARNING  -NO ACTIONS FOR THIS RULE. ".      02  DCOB
040440 77  ACTNRULES                   PICTURE X.                       02  DCOB
040450 77  DEXITIND                    PICTURE X.                       02  DCOB
040460 77  OUT-EL-SW                   PICTURE X                        02  DCOB
040470                         VALUE ZERO.                              02  DCOB
040500                                                                  02  DCOB
045000 77  VER-LEV-DT-03               PICTURE X(18)                    03  IORT
045001                         VALUE "V4-1 08/25/72 IORT".              03  IORT
045005 77  PUNCH-OPEN-IND              PICTURE X                        03  IORT
045010                         VALUE SPACE.                             03  IORT
045020 77  LASTSEQ                     PICTURE 9(6)                     03  IORT
045030                         VALUE ZERO.                              03  IORT
045040 77  SEQIND                      PICTURE X                        03  IORT
045050                         VALUE SPACE.                             03  IORT
045060 77  LCNTER                      PICTURE S9(2)   COMPUTATIONAL.   03  IORT
045070 77  SEQ-SW                  PIC X   VALUE "N".                   03  IORT
045080                                                                  03  IORT
050000 77  VER-LEV-DT-04               PICTURE X(18)                    04  FRMT
050001                         VALUE "V4-1 08/25/72 FRMT".              04  FRMT
050005 77  LINECT                      PICTURE S9(3) COMP               PDP-10
050010                         VALUE +001.                              04  FRMT
050020 77  STUB-CON-SW-1               PICTURE X                        04  FRMT
050030                         VALUE ZERO.                              04  FRMT
050040 77  STUB-CON-SW-2               PICTURE X                        04  FRMT
050050                         VALUE ZERO.                              04  FRMT
050060 77  WS-XM-DUM-CHAR              PICTURE X                        04  FRMT
050070                         VALUE "$".                               04  FRMT
050080 77  WS-XM-SW                    PICTURE X                        04  FRMT
050090                         VALUE ZERO.                              04  FRMT
050100 77  INIT-BOX-IND                PICTURE S9(3) COMP               PDP-10
050110                         VALUE ZERO.                              04  FRMT
050140 77  SAVE-COMMENT-FLAG           PICTURE X(2)                     04  FRMT
050150                         VALUE SPACE.                             04  FRMT
050160 77  SKIP-READ-SW                PICTURE X                        04  FRMT
050170                         VALUE ZERO.                              04  FRMT
050180 77  UTILITY-SUB                 PICTURE S9(2)   COMPUTATIONAL    04  FRMT
050190                         VALUE ZERO.                              04  FRMT
050200 77  WS-GO-TO                    PICTURE X                        04  FRMT
050210                         VALUE ZERO.                              04  FRMT
050215 77  WS-TYPE-IO-JUST-DONE        PICTURE X                        04  FRMT
050216                         VALUE ZERO.                              04  FRMT
050220 77  PRINT-WORK-132              PICTURE X(132).                  PDP-10
050230 77  TALLX                       PICTURE S9(5) COMP.              PDP-10
070000 01  TEMP-FIELD.                                                  00  INPT
070010     03  TEMP-FIELD-1-3.                                          00  INPT
070020         05  TEMP-FIELD-1        PICTURE X.                       00  INPT
070030         05  FILLER              PICTURE X(2).                    00  INPT
070040     03  TEMP-FIELD-4-ON         PICTURE X(57).                   00  INPT
070080                                                                  00  INPT
070090 01  OPTIONS-MSSG.                                                00  INPT
070100     03  FILLER                  PICTURE X(17)                    00  INPT
070110                         VALUE "***OPTIONS ARE...".               00  INPT
070120     03  OM-PRT                  PICTURE X(2)                     00  INPT
070130                         VALUE "YS".                              00  INPT
070140     03  FILLER                  PICTURE X(4)                     00  INPT
070150                         VALUE "PRT,".                            00  INPT
070160     03  OM-CMP                  PICTURE X(2)                     00  INPT
070170                         VALUE "NO".                              00  INPT
070180     03  FILLER                  PICTURE X(4)                     00  INPT
070190                         VALUE "CMP,".                            00  INPT
070200     03  OM-RCP                  PICTURE X(2)                     00  INPT
070210                         VALUE "YS".                              00  INPT
070220     03  FILLER                  PICTURE X(4)                     00  INPT
070230                         VALUE "RCP,".                            00  INPT
070240     03  OM-FMT                  PICTURE X(2)                     00  INPT
070250                         VALUE "YS".                              00  INPT
070260     03  FILLER                  PICTURE X(4)                     00  INPT
070270                         VALUE "FMT,".                            00  INPT
070280     03  OM-EXT                  PICTURE X(2)                     00  INPT
070290                         VALUE "YS".                              00  INPT
070300     03  FILLER                  PICTURE X(4)                     00  INPT
070310                         VALUE "LIN,".                            00  INPT
070320     03  OM-OVF                  PICTURE X(2)                     00  INPT
070330                         VALUE "YS".                              00  INPT
070340     03  FILLER                  PICTURE X(4)                     00  INPT
070350                         VALUE "OVF,".                            00  INPT
070360     03  OM-RPL                  PICTURE X(2)                     00  INPT
070370                         VALUE "YS".                              00  INPT
070380     03  FILLER                  PICTURE X(4)                     00  INPT
070390                         VALUE "RPL,".                            00  INPT
070400     03  OM-DCP                  PICTURE X(2)                     00  INPT
070410                         VALUE "YS".                              00  INPT
070420     03  FILLER                  PICTURE X(4)                     00  INPT
070430                         VALUE "DCP,".                            00  INPT
070440     03  OM-DIT                  PICTURE X(2)                     00  INPT
070450                         VALUE "YS".                              00  INPT
070460     03  FILLER                  PICTURE X(4)                     00  INPT
070470                         VALUE "DIT,".                            00  INPT
070480     03  OM-BND                  PICTURE X(2)                     00  INPT
070490                         VALUE "YS".                              00  INPT
070500     03  CONST-BND               PICTURE X(3)                     00  INPT
070510                         VALUE "BND".                             00  INPT
070520     03  OM-PERIOD               PICTURE X                        00  INPT
070530                         VALUE ".".                               00  INPT
070531 01  OPTIONS-MSSG-2.                                              00  INPT
070532     03  FILLER                  PICTURE X(17)                    00  INPT
070533                         VALUE "***              ".               00  INPT
070534     03  OM-SEQ                  PICTURE X(2)                     00  INPT
070535                         VALUE "YS".                              00  INPT
070536     03  FILLER                  PICTURE X(4)                     00  INPT
070537                         VALUE "SEQ.".                            00  INPT
070538     03  FILLER                  PICTURE X(57)                    00  INPT
070539                         VALUE   SPACES.                          00  INPT
070540 01  TN-ARRAY-GR.                                                 00  INPT
070550     03  TNARRAY                 PICTURE S9 COMP OCCURS 50.       PDP-10
070560                                                                  00  INPT
070570 01  TABLE-HEADER.                                                00  INPT
070580     03  SEQNUM                  PICTURE 9(6).                    00  INPT
070590     03  SEQNUM-X        REDEFINES SEQNUM                         00  INPT
070600                                 PICTURE X(6).                    00  INPT
070610     03  FILLER                  PICTURE X.                       00  INPT
070620     03  IDENT.                                                   00  INPT
070630         05  RL-ID.                                               00  INPT
070640             07  CTLCOL          PICTURE X.                       00  INPT
070650             07  COLM-9          PICTURE X.                       00  INPT
070660         05  FILLER-RL.                                           00  INPT
070670             07  RL-NUM          PICTURE X.                       00  INPT
070680             07  FILLER          PICTURE X(2).                    00  INPT
070690     03  FILLERA.                                                 00  INPT
070700         05  SASTR               PICTURE X(2).                    00  INPT
070710         05  FILLER              PICTURE X(3).                    00  INPT
070720     03  TABLE-NAME              PICTURE X(32).                   00  INPT
070730     03  FILLER                  PICTURE X(2).                    00  INPT
070740     03  TABLE-NO                PICTURE X(5).                    00  INPT
070750     03  FILLER                  PICTURE X.                       00  INPT
070760     03  PARAMETERS              PICTURE X(9).                    00  INPT
070770     03  FILLER                  PICTURE X.                       00  INPT
070780     03  OPTIMIZATION            PICTURE X.                       00  INPT
070790     03  FILLER                  PICTURE X.                       00  INPT
070800     03  PERFORM-NO              PICTURE X.                       00  INPT
070810     03  PERFORM-NO-N    REDEFINES PERFORM-NO                     00  INPT
070820                                 PICTURE 9.                       00  INPT
070830     03  FILLER                  PICTURE X(2).                    00  INPT
070840     03  IDENTIFIER              PICTURE X(8).                    00  INPT
070850 01  TABLE-DUMMY         REDEFINES TABLE-HEADER.                  00  INPT
070860     03  FILLER                  PICTURE X(7).                    00  INPT
070870     03  RL2-ID-SLOT             PICTURE X(3).                    00  INPT
070880     03  CARD-FIELD              PICTURE X(62).                   00  INPT
070890     03  FILLER                  PICTURE X(8).                    00  INPT
070900 01  SCAN-FORMAT         REDEFINES TABLE-DUMMY.                   00  INPT
070910     03  FILLER                  PICTURE X(9).                    00  INPT
070920     03  CRDCOL                  PICTURE X                        00  INPT
070930                         OCCURS 63.                               00  INPT
070940     03  FILLER                  PICTURE X(8).                    00  INPT
070950 01  TABLE-CARD          REDEFINES SCAN-FORMAT.                   00  INPT
070960     03  FILLER                  PICTURE X(9).                    00  INPT
070970     03  ROW-CARD                PICTURE X(63).                   00  INPT
070980     03  FILLER                  PICTURE X(8).                    00  INPT
070990 01  NOTE-CARD-IN        REDEFINES TABLE-CARD.                    00  INPT
071000     03  FIRST-PRT               PICTURE X(7).                    00  INPT
071010     03  SECOND-PRT              PICTURE X(65).                   00  INPT
071020     03  FILLER                  PICTURE X(8).                    00  INPT
071030 01  AFILLER             REDEFINES NOTE-CARD-IN.                  00  INPT
071040     03  FILLER                  PICTURE X(7).                    00  INPT
071050     03  COND-SECT.                                               00  INPT
071060         05  IDEN12.                                              00  INPT
071070             07  IDEN9.                                           00  INPT
071080                 08  IDEN8.                                       00  INPT
071090                     09  IDEN7.                                   00  INPT
071100                         10  IDEN6                                00  INPT
071110                                 PICTURE X(6).                    00  INPT
071120                         10  FILLER                               00  INPT
071130                                 PICTURE X.                       00  INPT
071140                     09  FILLER  PICTURE X.                       00  INPT
071150                 08  FILLER      PICTURE X.                       00  INPT
071160             07  FILLER          PICTURE X(3).                    00  INPT
071170         05  FILLER              PICTURE X(12).                   00  INPT
071180     03  FILLER                  PICTURE X(49).                   00  INPT
071190 01  BFILLER             REDEFINES AFILLER.                       00  INPT
071200     03  FILLER                  PICTURE X(7).                    00  INPT
071210     03  ACTN-SECT.                                               00  INPT
071220         05  CODN-SECT-17.                                        00  INPT
071230             07  ACTN-SECT-14    PICTURE X(14).                   00  INPT
071240             07  FILLER          PICTURE X(3).                    00  INPT
071250         05  FILLER              PICTURE X(4).                    00  INPT
071260     03  FILLER                  PICTURE X(52).                   00  INPT
071270 01  CFILLER             REDEFINES BFILLER.                       00  INPT
071280     03  FILLER                  PICTURE X(7).                    00  INPT
071290     03  FILE-SECT               PICTURE X(12).                   00  INPT
071300     03  FILLER                  PICTURE X(61).                   00  INPT
071310 01  IFILLER             REDEFINES CFILLER.                       00  INPT
071320     03  FILLER                  PICTURE X(8).                    00  INPT
071330     03  INIT-FIELD-9.                                            00  INPT
071340         05  FILLER              PICTURE X.                       00  INPT
071350         05  INIT-FIELD          PICTURE X(63).                   00  INPT
071360     03  FILLER                  PICTURE X(8).                    00  INPT
071370 01  JFILLER             REDEFINES IFILLER.                       00  INPT
071380     03  FILLER                  PICTURE X(13).                   00  INPT
071390     03  IMM-ALL                 PICTURE X(3).                    00  INPT
071400     03  FILLER                  PICTURE X(5).                    00  INPT
071410     03  RNGETP                  PICTURE X(3).                    00  INPT
071420     03  FILLER                  PICTURE X(56).                   00  INPT
071430 01  KFILLER             REDEFINES JFILLER.                       00  INPT
071440     03  FILLER                  PICTURE X(25).                   00  INPT
071450     03  LNECNT                  PICTURE X(2).                    00  INPT
071460     03  LNE1NO          REDEFINES LNECNT                         00  INPT
071470                                 PICTURE 9(2).                    00  INPT
071480     03  FILLER                  PICTURE X(53).                   00  INPT
071490 01  DOPTS-CARD          REDEFINES KFILLER.                       00  INPT
071500     03  FILLER                  PICTURE X(7).                    00  INPT
071510     03  DOPT-1                  PICTURE X(7).                    00  INPT
071520     03  FILLER                  PICTURE X.                       00  INPT
071530     03  DOPT-2.                                                  00  INPT
071540         05  DOPT-OPT            PICTURE X(6)                     00  INPT
071550                         OCCURS 10.                               00  INPT
071560     03  FILLER                  PICTURE X(5).                    00  INPT
071561 01  TABLE-HDR-RD        REDEFINES DOPTS-CARD.                    00  INPT
071562     03  FILLER                  PICTURE X(51).                   00  INPT
071563     03  TABLE-HDR-EXAMIN        PICTURE X(19).                   00  INPT
071564     03  FILLER                  PICTURE X(10).                   00  INPT
071570                                                                  00  INPT
071580                                                                  00  INPT
071590 01  DOPT-TEST.                                                   00  INPT
071600     03  DOPTYN                  PICTURE X(2).                    00  INPT
071610     03  DOPTXXX.                                                 00  INPT
071620         05  EL-1                PICTURE X.                       00  INPT
071630         05  EL-2                PICTURE X.                       00  INPT
071640         05  EL-3                PICTURE X.                       00  INPT
071650     03  DOPTCOMMA               PICTURE X.                       00  INPT
071660                                                                  00  INPT
071670                                                                  00  INPT
071680 01  INARRAY-VECTOR.                                              00  INPT
071690     03  INARRAY                 PICTURE S9(3)   COMPUTATIONAL    00  INPT
071700                         OCCURS 50.                               00  INPT
071710                                                                  00  INPT
071720                                                                  00  INPT
071730 01  TEMP-VECTOR.                                                 00  INPT
071740     03  TEMP-COLM               PICTURE S9(3)   COMPUTATIONAL    00  INPT
071750                         OCCURS 100.                              00  INPT
071760 01  TEMP-GEN-SWITCHES.                                           PDP-10
071770     03  TEMP-SW-VECTOR.                                          00  INPT
071780         05  GEN-SW              PICTURE X                        00  INPT
071790                         OCCURS 50.                               00  INPT
071800     03  FILLER                  PICTURE X(6).                    00  INPT
071810     03  SHIFT-WORK-FIELD.                                        00  INPT
071820         05  S-R-GEN.                                             00  INPT
071830             07  S-R-SHIFTED     PICTURE X(63).                   00  INPT
071840             07  FILLER          PICTURE X(3).                    00  INPT
071850         05  S-R-ONCE    REDEFINES S-R-GEN.                       00  INPT
071860             07  S-R-SPACE-1     PICTURE X.                       00  INPT
071870             07  S-R-1-BODY      PICTURE X(63).                   00  INPT
071880             07  FILLER          PICTURE X(2).                    00  INPT
071890         05  S-R-TRICE   REDEFINES S-R-ONCE.                      00  INPT
071900             07  S-R-SPACE-3     PICTURE X(3).                    00  INPT
071910             07  S-R-3-BODY      PICTURE X(63).                   00  INPT
071920     03  FILLER                  PICTURE X(78).                   00  INPT
071930                                                                  00  INPT
071940                                                                  00  INPT
071950 01  SCAN-FIELD.                                                  00  INPT
071960     03  SCAN-FIELD-1-ON.                                         00  INPT
071970         05  SCAN-1-THRU-3       PICTURE X(3).                    00  INPT
071980         05  SCAN-4-ON.                                           00  INPT
071990             07  FILLER          PICTURE X(22).                   00  INPT
072000             07  ELEMENT-26-ON.                                   00  INPT
072010                 08  FILLER      PICTURE X(5).                    00  INPT
072020                 08  ELEMENT-31-ON                                00  INPT
072030                                 PICTURE X(33).                   00  INPT
072040     03  FILLER                  PICTURE X(17)                    00  INPT
072050                         VALUE SPACE.                             00  INPT
072060 01  SCAN-COL            REDEFINES SCAN-FIELD.                    00  INPT
072070     03  ELEMENT                 PICTURE X                        00  INPT
072080                         OCCURS 63.                               00  INPT
072090     03  FILLER                  PICTURE X(17).                   00  INPT
072100 01  TESTNUM             REDEFINES SCAN-COL.                      00  INPT
072110     03  NUM-BER                 PIC 9(2).                        00  INPT
072120     03  NUMBR           REDEFINES NUM-BER                        00  INPT
072130                                 PICTURE X(2).                    00  INPT
072140     03  FILLER                  PICTURE X(78).                   00  INPT
072150 01  TBL-FIELD           REDEFINES TESTNUM.                       00  INPT
072160     03  NAMEFIELD.                                               00  INPT
072170         05  SCAN-FIELD-1-THRU-18                                 00  INPT
072180                                 PICTURE X(18).                   00  INPT
072190         05  FILLER              PICTURE X(14).                   00  INPT
072200     03  FILLER                  PICTURE X(48).                   00  INPT
072210                                                                  00  INPT
072220                                                                  00  INPT
072230 01  PARAMS.                                                      00  INPT
072240     03  CONDS                   PICTURE 9(2).                    00  INPT
072250     03  FILLER                  PICTURE X.                       00  INPT
072260     03  ACTNS                   PICTURE 9(2).                    00  INPT
072270     03  FILLER                  PICTURE X.                       00  INPT
072280     03  RULES                   PICTURE 9(2).                    00  INPT
072290     03  FILLER                  PICTURE X.                       00  INPT
072300                                                                  00  INPT
072310                                                                  00  INPT
072320 01  RULES-FORMAT.                                                00  INPT
072330     03  REXTENSIONS             PICTURE S9(3)   COMPUTATIONAL    00  INPT
072340                         OCCURS 6.                                00  INPT
072350     03  EXTINDEX                PICTURE S9      COMPUTATIONAL.   00  INPT
072360     03  NRULES                  PICTURE S9(3)   COMPUTATIONAL.   00  INPT
072370     03  RLENGTH                 PICTURE S9(3)   COMPUTATIONAL    00  INPT
072380                         OCCURS 50.                               00  INPT
072390                                                                  00  INPT
072400                                                                  00  INPT
072410                                                                  00  INPT
072420 01  OUTSTACK.                                                    00  INPT
072425     03  BENTRY                  PICTURE X(14)                    00  INPT
072430                         OCCURS 200.                              00  INPT
072435 01  OVER-LAY-SH         REDEFINES OUTSTACK.                      00  INPT
072440     03  INIT-STRUCTURE.                                          00  INPT
072445         05  INITIAL-VECTOR.                                      00  INPT
072450             07  ININDEX         PICTURE S9(3).                   PDP-10
072455             07  IN-ITIAL        PIC X(62)                        00  INPT
072460                         OCCURS 45.                               00  INPT
072465             07  FILLER          PICTURE X(7).                    PDP-10
072470 01  SPILL-STRUCTURE     REDEFINES OVER-LAY-SH.                   00  INPT
072475     03  SPILL-PIECES.                                            00  INPT
072480         05  SPILL               PICTURE X(660)                   00  INPT
072485                         OCCURS 4.                                00  INPT
072490         05  FILLER              PICTURE X(160).                  00  INPT
072500     03  COUNT-PIECES    REDEFINES SPILL-PIECES.                  00  INPT
072510         05  SPILT               PICTURE X(660).                  00  INPT
072520         05  FILLER              PICTURE X(2140).                 00  INPT
072530 01  S-MATRIXQ-AREA      REDEFINES SPILL-STRUCTURE.               00  INPT
072540     03  FILLER                  PICTURE X(2800).                 00  INPT
072550                                                                  00  INPT
072560 01  REST-OF-OUTSTACK.                                            00  INPT
072570     03  FILLER                  PICTURE X(3302).                 00  INPT
072580 01  REST-OF-OVER-LAY-SH REDEFINES REST-OF-OUTSTACK.              00  INPT
072590     03  FILLER                  PICTURE X(302).                  00  INPT
072600     03  RULE-SHELVES.                                            00  INPT
072610         05  SHELF       OCCURS 50.                               00  INPT
072620             07  SHELF1-3.                                        00  INPT
072630                 08  SHELF1      PICTURE X.                       00  INPT
072640                 08  SHELF2-3    PICTURE X(2).                    00  INPT
072650             07  SHELF4-ON       PICTURE X(57).                   00  INPT
072660 01  REST-OF-SPILL-STRUCTURE                                      00  INPT
072670                         REDEFINES REST-OF-OVER-LAY-SH.           00  INPT
072680     03  FILLER                  PICTURE X(2478).                 00  INPT
072690     03  COUNT-IN-SPILL          PICTURE S9(3).                   PDP-10
072700     03  SPILL-INDEX             PICTURE S9(3).                   PDP-10
072710     03  FILLER                  PICTURE X(819).                  PDP-10
072720 01  REST-OF-S-MATRIXQ   REDEFINES REST-OF-SPILL-STRUCTURE.       00  INPT
072730     03  FILLER                  PICTURE X(302).                  00  INPT
072740     03  S-MATRIXQ.                                               00  INPT
072750         05  S-MATRIX.                                            00  INPT
072760             07  S-BROAD OCCURS 50.                               00  INPT
072770                 08  S           PICTURE X                        00  INPT
072780                         OCCURS 50.                               00  INPT
072790     03  FILLER                  PICTURE X(500).                  00  INPT
072800                                                                  00  INPT
072810                                                                  00  INPT
072820                                                                  00  INPT
072830 01  STRING-FIELD.                                                00  INPT
072840     03  STR-ING                 PIC X                            00  INPT
072850                         OCCURS 300.                              00  INPT
072860                                                                  00  INPT
072870 01  STUB-STACK.                                                  00  INPT
072880     03  STUBIES                 PICTURE X(60)                    00  INPT
072890                         OCCURS 5.                                00  INPT
072900     03  STUB6                   PICTURE X(60).                   00  INPT
072910 01  STUB-STACK-BND      REDEFINES STUB-STACK.                    00  INPT
072920     03  STUB-1-BND.                                              00  INPT
072930         05  STUB-1-BND-COL      PICTURE X                        00  INPT
072940                         OCCURS 360.                              PDP-10  
072950                                                                  PDP-10  
072960                                                                  00  INPT
072970                                                                  00  INPT
072980 01  ERROR-MSG.                                                   00  INPT
072990     03  TYPEMSG                 PICTURE X(19).                   00  INPT
073000     03  ERROR-PRINT             PICTURE X(61).                   00  INPT
073010     03  FILLER1         REDEFINES ERROR-PRINT.                   00  INPT
073020         05  FILLER              PICTURE X(35).                   00  INPT
073030         05  RULE-1              PICTURE 9(3).                    00  INPT
073040         05  FILLER              PICTURE X(5).                    00  INPT
073050         05  RULE-2              PICTURE 9(3).                    00  INPT
073060         05  FILLER              PICTURE X(15).                   00  INPT
073070     03  FILLER2         REDEFINES FILLER1.                       00  INPT
073080         05  ERROR-INTRO         PICTURE X(9).                    00  INPT
073090         05  ERROR-BODY          PICTURE X(34).                   00  INPT
073100         05  ERROR-RULE-NUMBER   PICTURE Z(3).                    00  INPT
073110         05  ERROR-TRAIL         PICTURE X(15).                   00  INPT
073115                                                                  00  INPT
073120                                                                  00  INPT
073125 01  CONDITION-MATRIX.                                            00  INPT
073128     03  FILLERC         OCCURS 50.                               00  INPT
073132         05  CMATRIX             PICTURE X                        00  INPT
073136                         OCCURS 50.                               00  INPT
073138                                                                  00  INPT
073140 01  ACTION-MATRIX.                                               00  INPT
073150     03  FILLERA         OCCURS 50.                               00  INPT
073160         05  AMATRIX             PICTURE S9(3)   COMPUTATIONAL    00  INPT
073170                         OCCURS 50.                               00  INPT
073180                                                                  00  INPT
073190                                                                  00  INPT
073200 01  SOME-CONSTANTS.                                              00  INPT
073210     03  SECT-NAME               PICTURE X(8)                     00  INPT
073220                         VALUE "SECTION.".                        00  INPT
073230     03  SECTXXXX        REDEFINES SECT-NAME.                     00  INPT
073240         05  SECT                PICTURE X                        00  INPT
073250                         OCCURS 8.                                00  INPT
073310                                                                  00  INPT
073320                                                                  00  INPT
073330 01  INDEXES.                                                     00  INPT
073340     03  CNDINDEX                PICTURE S9(3)   COMPUTATIONAL.   00  INPT
073350     03  ATNINDEX                PICTURE S9(3)   COMPUTATIONAL.   00  INPT
073360     03  CPTRINDEX               PICTURE S9(3)   COMPUTATIONAL.   00  INPT
073370     03  APTRINDEX               PICTURE S9(3)   COMPUTATIONAL.   00  INPT
073400                                                                  00  INPT
073410                                                                  00  INPT
073490                                                                  00  INPT
073500 01  FREQ-VECTOR.                                                 00  INPT
073510     03  FREQ                    PICTURE S9(3)   COMPUTATIONAL    00  INPT
073520                         OCCURS 50.                               00  INPT
073530                                                                  00  INPT
073540 01  CONDSTK-VECTOR.                                              00  INPT
073550     03  CONDSTK         OCCURS 100.                              00  INPT
073560         05  CONDCOLX            PICTURE X                        00  INPT
073570                         OCCURS 60.                               00  INPT
073580                                                                  00  INPT
073590 01  ACTNSTK-VECTOR.                                              00  INPT
073600     03  ACTNSTK                 PICTURE X(60)                    00  INPT
073610                         OCCURS 100.                              00  INPT
073620                                                                  00  INPT
073630 01  CSTUBPTR-VECTOR.                                             00  INPT
073640     03  CSTUBPTR                PICTURE S9(3)   COMPUTATIONAL    00  INPT
073650                         OCCURS 101.                              00  INPT
073660                                                                  00  INPT
073670 01  ASTUBPTR-VECTOR.                                             00  INPT
073680     03  ASTUBPTR                PICTURE S9(3)   COMPUTATIONAL    00  INPT
073690                         OCCURS 101.                              00  INPT
073700                                                                  00  INPT
073710 01  CONDLST-VECTOR.                                              00  INPT
073720     03  CONDLST                 PICTURE S9(3)   COMPUTATIONAL    00  INPT
073730                         OCCURS 100.                              00  INPT
073740                                                                  00  INPT
073750 01  ACTNLST-VECTOR.                                              00  INPT
073760     03  ACTNLST                 PICTURE S9(3)   COMPUTATIONAL    00  INPT
073770                         OCCURS 100.                              00  INPT
073780                                                                  00  INPT
073790 01  ACTION-POINTER-VECTOR.                                       00  INPT
073800     03  ACTNPTR                 PICTURE S9(3)   COMPUTATIONAL    00  INPT
073810                         OCCURS 50.                               00  INPT
073820                                                                  00  INPT
073825 01  INDARRAY-VECTOR.                                             00  INPT
073828     03  PFRMINDX                PICTURE S9(3)   COMPUTATIONAL.   00  INPT
073832     03  INDARRAY                PICTURE S9                       00  INPT
073836                         OCCURS 50.                               00  INPT
073838                                                                  00  INPT
073840                                                                  00  INPT
073845                                                                  00  INPT
073850 01  SUFFIX-PARA-NAME-NOTE.                                       00  INPT
073860     03  SUFFIX-VALUE.                                            00  INPT
073870         05  FILLER              PICTURE X(3)                     00  INPT
073880                         VALUE "-DT".                             00  INPT
073890         05  SUFFIX-NUMBER       PICTURE 9(2)                     00  INPT
073900                         VALUE 01.                                00  INPT
073910         05  FILLER              PICTURE X(6)                     00  INPT
073920                         VALUE ". NOTE".                          00  INPT
073930     03  SUFFIX-OCCURS   REDEFINES SUFFIX-VALUE.                  00  INPT
073940         05  SUFFIXER            PICTURE X                        00  INPT
073950                         OCCURS 11.                               00  INPT
073960                                                                  00  INPT
073970                                                                  00  INPT
073980                                                                  00  INPT
073990 01  BUFFER.                                                      00  INPT
073992     03  FILLER                  PICTURE X(72)                    00  INPT
073993                         VALUE                                    00  INPT
073994             " START OF DETAP/IMI PREPROCESSOR RUN.".             00  INPT
073997     03  BUFF-IDEN               PICTURE X(8)                     00  INPT
073998                         VALUE "DETAPIMI".                        00  INPT
074000 01  TYPE-1A-RECORD      REDEFINES BUFFER.                        00  INPT
074010     03  TYPE-RECORD             PICTURE X.                       00  INPT
074020     03  SUB-TYPE                PICTURE X.                       00  INPT
074030     03  FILLER                  PICTURE X(78).                   00  INPT
074040 01  TYPE-4-RECORD       REDEFINES TYPE-1A-RECORD.                00  INPT
074050     03  FILLER                  PICTURE X(4).                    00  INPT
074060     03  ACTIONS                 PICTURE 9(3).                    00  INPT
074070     03  FILLER                  PICTURE X(73).                   00  INPT
074080 01  TYPE-6A-RECORD      REDEFINES TYPE-4-RECORD.                 00  INPT
074090     03  FILLER                  PICTURE X(3).                    00  INPT
074100     03  LABEL-TYPE              PICTURE X(2).                    00  INPT
074110     03  IN-LABEL                PICTURE X(3).                    00  INPT
074120     03  LABEL-1         REDEFINES IN-LABEL                       00  INPT
074130                                 PICTURE 9(3).                    00  INPT
074140     03  FILLER                  PICTURE X(72).                   00  INPT
074150 01  TYPE-6B-RECORD      REDEFINES TYPE-6A-RECORD.                00  INPT
074160     03  FILLER                  PICTURE X(3).                    00  INPT
074170     03  CONDITION               PICTURE 9(3).                    00  INPT
074180     03  GO-TO-LABEL             PICTURE X(3).                    00  INPT
074185     03  Y-BRANCH        REDEFINES GO-TO-LABEL                    00  INPT
074186                                 PICTURE 9(3).                    00  INPT
074190     03  ELSE-LABEL              PICTURE X(3).                    00  INPT
074195     03  N-BRANCH        REDEFINES ELSE-LABEL                     00  INPT
074196                                 PICTURE 9(3).                    00  INPT
074200     03  TYPE-GO-TO              PICTURE X.                       00  INPT
074205     03  Y-BRANCH-IND    REDEFINES TYPE-GO-TO                     00  INPT
074206                                 PICTURE 9.                       00  INPT
074210     03  TYPE-ELSE               PICTURE X.                       00  INPT
074215     03  N-BRANCH-IND    REDEFINES TYPE-ELSE                      00  INPT
074216                                 PICTURE 9.                       00  INPT
074217     03  NO-TYPE         REDEFINES N-BRANCH-IND                   00  INPT
074218                                 PICTURE X.                       00  INPT
074220     03  FILLER                  PICTURE X(66).                   00  INPT
074230 01  PRINT-LINE          REDEFINES TYPE-6B-RECORD.                00  INPT
074240     03  TSEQ-NO                 PICTURE 9(6).                    00  INPT
074250     03  TSEQ-NO-X       REDEFINES TSEQ-NO                        00  INPT
074260                                 PICTURE X(6).                    00  INPT
074270     03  FILLER                  PICTURE X(5).                    00  INPT
074280     03  B-MARG.                                                  00  INPT
074290         05  FILLER              PICTURE X(19).                   00  INPT
074300         05  RULENO              PICTURE 9(5).                    00  INPT
074310         05  FILLER              PICTURE X(37).                   00  INPT
074320     03  TIDENT-FIELD            PICTURE X(8).                    00  INPT
074330 01  NOTE-CARD-OUT       REDEFINES PRINT-LINE.                    00  INPT
074340     03  NOTE-BODY-PARTS.                                         00  INPT
074350         05  PART-ONE            PICTURE X(7).                    00  INPT
074360         05  SPACE-OUT           PICTURE X(4).                    00  INPT
074370         05  PART-TWO            PICTURE X(65).                   00  INPT
074380     03  PART-FLAG-BND           PICTURE X(4).                    00  INPT
074390 01  ID-OUT              REDEFINES NOTE-CARD-OUT.                 00  INPT
074400     03  FILLER                  PICTURE X(72).                   00  INPT
074410     03  IDENT-OUT               PICTURE X(8).                    00  INPT
074420 01  TYP-DUM             REDEFINES ID-OUT.                        00  INPT
074430     03  BDUM                    PICTURE X(20).                   00  INPT
074440     03  FILLER                  PICTURE X(60).                   00  INPT
074450 01  LABEL-LINE          REDEFINES TYP-DUM.                       00  INPT
074460     03  FILLER                  PICTURE X(7).                    00  INPT
074470     03  SECTION-OUT.                                             00  INPT
074480         05  LABEL-OUTS.                                          00  INPT
074490             07  PREFIX          PICTURE X(2).                    00  INPT
074500             07  TNUMB           PICTURE 9(5).                    00  INPT
074510             07  POSTFIX         PICTURE 9(3).                    00  INPT
074520             07  PERIOD          PICTURE X.                       00  INPT
074530         05  FILLER              PICTURE X(54).                   00  INPT
074540     03  FILLER                  PICTURE X(8).                    00  INPT
074550 01  FFILLER             REDEFINES LABEL-LINE.                    00  INPT
074560     03  FILLER                  PICTURE X(7).                    00  INPT
074570     03  NOTE-COL                PICTURE X                        00  INPT
074580                         OCCURS 50.                               00  INPT
074590     03  FILLER                  PICTURE X(23).                   00  INPT
074600 01  FFFILLER            REDEFINES FFILLER.                       00  INPT
074610     03  FILLER                  PICTURE X(46).                   00  INPT
074620     03  VERS-LEVEL-ID           PICTURE X(24).                   00  INPT
074630     03  FILLER                  PICTURE X(10).                   00  INPT
074640 01  WS-CARD             REDEFINES FFFILLER.                      00  INPT
074650     03  INIT-CARD-COL-1-7.                                       00  INPT
074660         05  FILLER              PICTURE X(7).                    00  INPT
074670     03  INIT-CARD-COL-8-72.                                      00  INPT
074680         05  WS-CC8-9            PICTURE X(2).                    00  INPT
074690         05  FILLER              PICTURE X(63).                   00  INPT
074700     03  INIT-CARD-COL-73-80.                                     00  INPT
074710         05  FILLER              PICTURE X(8).                    00  INPT
074720 01  WS-CARD1            REDEFINES WS-CARD.                       00  INPT
074730     03  FILLER                  PICTURE X(7).                    00  INPT
074740     03  WS-CC8-10               PICTURE X(3).                    00  INPT
074750     03  FILLER                  PICTURE X(70).                   00  INPT
074760 01  WS-CARD2            REDEFINES WS-CARD1.                      00  INPT
074770     03  FILLER                  PICTURE X(7).                    00  INPT
074780     03  WS-CC8-11               PICTURE X(4).                    00  INPT
074790     03  FILLER                  PICTURE X(69).                   00  INPT
074800 01  WS-CARD3            REDEFINES WS-CARD2.                      00  INPT
074810     03  FILLER                  PICTURE X(7).                    00  INPT
074820     03  WS-CC8-13.                                               00  INPT
074830         05  WS-CC8-12           PICTURE X(5).                    00  INPT
074840         05  FILLER              PICTURE X.                       00  INPT
074850     03  FILLER                  PICTURE X(67).                   00  INPT
074860 01  WS-CARD4            REDEFINES WS-CARD3.                      00  INPT
074870     03  FILLER                  PICTURE X(7).                    00  INPT
074880     03  WS-CC8-21               PICTURE X(14).                   00  INPT
074890     03  FILLER                  PICTURE X(59).                   00  INPT
074900 01  WS-CARD5            REDEFINES WS-CARD4.                      00  INPT
074910     03  FILLER                  PICTURE X(7).                    00  INPT
074920     03  WS-CC8-24               PICTURE X(17).                   00  INPT
074930     03  FILLER                  PICTURE X(56).                   00  INPT
074940 01  WS-CARD6            REDEFINES WS-CARD5.                      00  INPT
074950     03  FILLER                  PICTURE X(9).                    00  INPT
074960     03  WS-CC10-72              PICTURE X(63).                   00  INPT
074970     03  FILLER                  PICTURE X(8).                    00  INPT
074980 01  WS-CARD7            REDEFINES WS-CARD6.                      00  INPT
074982     03  FILLER                  PICTURE X(17).                   00  INPT
074983     03  WS-CC18-50              PICTURE X(33).                   00  INPT
074984     03  FILLER                  PICTURE X(30).                   00  INPT
074985 01  WS-CARD8            REDEFINES WS-CARD7.                      00  INPT
074990     03  WS-CC                   PICTURE X                        00  INPT
074995                         OCCURS 80.                               00  INPT
074996 01  WS-CARD9            REDEFINES WS-CARD8.                      00  INPT
074997     03  FILLER                  PICTURE X(7).                    00  INPT
074998     03  WS-CC8-15               PICTURE X(8).                    00  INPT
074999     03  FILLER                  PICTURE X(65).                   00  INPT
075640                                                                  00  INPT
075650                                                                  00  INPT
075660                                                                  00  INPT
       01  WS-STANDRD-FMT-SHIFT    REDEFINES WS-CARD9.
           03   FILLER              PICTURE X(6).
           03   STANDRD-FMT-SHIFT   PICTURE X(66).
           03   FILLER              PICTURE X(8).


       01  TYMSHARE-FILE-NAME-STUFF.
           03  SOURCE-ID.
                05  SOURCE-ID-NAME   PICTURE X(6).
                05  SOURCE-ID-EXT    PICTURE X(3).
           03  OUTPUT-ID.
                05  OUTPUT-ID-NAME      PICTURE X(6).
                05  OUTPUT-ID-EXT       PICTURE X(3).
           03  LISTING-ID.
                05  LISTING-ID-NAME     PICTURE X(6).
                05  LISTING-ID-EXT      PICTURE X(3).
           03  WORK-FL-ID.
                05  WORK-FL-ID-NAME     PICTURE X(6).
                05  WORK-FL-ID-EXT      PICTURE X(3).
           03  TEMPORARY-NAME.
                05  TN-CHAR   PICTURE X   OCCURS 10 TIMES.
           03  PROGRAM-NAME.
                05  PN-CHAR   PICTURE X   OCCURS 9 TIMES.
           03 EXT-PERIOD-FOUND  PICTURE X.
                88  EXT-PER-FOUND  VALUE "X".
           03  I              PICTURE S99;  COMP.
           03  J              PICTURE S99;  COMP.
075670 01  IO-BLOCK.                                                    00  INPT
075680     03  TYPE-IO                 PICTURE X.                       00  INPT
075690     03  RESULT                  PICTURE X.                       00  INPT
075700     03  LINECNT                 PICTURE S9(2)   COMPUTATIONAL    00  INPT
075705                         VALUE +55.                               00  INPT
075720     03  ERR-RULE-01             PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075730     03  ERR-RULE-02             PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075740     03  ERROR-IND               PICTURE X.                       00  INPT
075750     03  BND-IND-4-REPL          PICTURE X                        00  INPT
075760                         VALUE SPACE.                             00  INPT
075770                                                                  00  INPT
075780                                                                  00  INPT
075790 01  CONTROL-RECORD.                                              00  INPT
075810     03  TBLNAME                 PICTURE X(32).                   00  INPT
075840     03  NBR-CONDS               PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075850     03  NBR-ACTIONS             PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075860     03  NBR-RULES               PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075865     03  OUTINDEX                PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075866     03  DXNUM                   PICTURE S9(3)   COMPUTATIONAL.   00  INPT
075870     03  ELSERULE                PICTURE X.                       00  INPT
075880     03  NOPUNCH                 PICTURE X.                       00  INPT
075890     03  PRINT-PUNCH             PICTURE X.                       00  INPT
075900     03  TBLNO                   PICTURE X(5)                     00  INPT
075905                         VALUE ZERO.                              00  INPT
075910     03  TBLNUM          REDEFINES TBLNO                          00  INPT
075920                                 PICTURE 9(5).                    00  INPT
075940     03  TRACE-IND               PICTURE X.                       00  INPT
075950     03  OPTION-OPTIMIZ          PICTURE X                        00  INPT
075955                         VALUE "C".                               00  INPT
076000     03  SEQUENCE-NO             PICTURE 9(6).                    00  INPT
076010     03  SEQUENCE-NO-X   REDEFINES SEQUENCE-NO                    00  INPT
076020                                 PICTURE X(6).                    00  INPT
076030     03  INCREMENT               PICTURE S9                       PDP-10
076035                         VALUE +2.                                00  INPT
076040     03  IDENTFIELD              PICTURE X(8).                    00  INPT
076050                                                                  00  INPT
076060                                                                  00  INPT
076070                                                                  00  INPT
076080                                                                  00  INPT
076090                                                                  00  INPT
076130                                                                  00  INPT
076135                                                                  00  INPT
076150                                                                  00  INPT
076160                                                                  00  INPT
076170 01  DOPTS-SWS.                                                   00  INPT
076180     03  DOPTS-SW-RCP            PICTURE X                        00  INPT
076190                         VALUE "1".                               00  INPT
076200     03  DOPTS-SW-FMT            PICTURE X                        00  INPT
076210                         VALUE "1".                               00  INPT
076220     03  DOPTS-SW-EXT            PICTURE X                        00  INPT
076230                         VALUE "2".                               00  INPT
076240     03  DOPTS-SW-OVF            PICTURE X                        00  INPT
076250                         VALUE "2".                               00  INPT
076260     03  DOPTS-SW-RPL            PICTURE X                        00  INPT
076270                         VALUE "1".                               00  INPT
076280     03  DOPTS-SW-DCP            PICTURE X                        00  INPT
076290                         VALUE "1".                               00  INPT
    076300     03  P-D-SW                  PICTURE X                        00  INPT
076310                         VALUE ZERO.                              00  INPT
076320     03  DOPT-IN-RPL-SW          PICTURE X                        00  INPT
076330                         VALUE "1".                               00  INPT
076340     03  DOPTS-SW-DIT            PICTURE X                        00  INPT
076350                         VALUE "1".                               00  INPT
076353     03  DOPTS-SW-SEQ            PICTURE X                        00  INPT
076354                         VALUE "1".                               00  INPT
076360                                                                  00  INPT
076370                                                                  00  INPT
076380 01  EOF-RECAP-MSSGS.                                             00  INPT
076390     03  EOF-TERM-MSSG.                                           00  INPT
076400         05  FILLER              PICTURE X(7)                     00  INPT
076410                         VALUE "****** ".                         00  INPT
076420         05  TERM-ERR-COUNT      PICTURE 9(5)                     00  INPT
076430                         VALUE ZERO.                              00  INPT
076440         05  FILLER              PICTURE X(55)                    00  INPT
076450                         VALUE                                    00  INPT
076460       " TERMINAL ERRORS IN ABOVE TABLE. COMPILE FILE CANCELED.". 00  INPT
076470         05  FILLER              PICTURE X(13)                    00  INPT
076480                         VALUE "************ ".                   00  INPT
076490     03  EOF-WARN-MSSG.                                           00  INPT
076500         05  FILLER              PICTURE X(7)                     00  INPT
076510                         VALUE "****** ".                         00  INPT
076520         05  WARN-ERR-COUNT      PICTURE 9(5)                     00  INPT
076530                         VALUE ZERO.                              00  INPT
076540         05  FILLER              PICTURE X(55)                    00  INPT
076550                         VALUE                                    00  INPT
076560       " WARNING MESSAGES IN ABOVE TABLE. *********************". 00  INPT
076570         05  FILLER              PICTURE X(13)                    00  INPT
076580                         VALUE "************ ".                   00  INPT
076590                                                                  00  INPT
076600                                                                  00  INPT
076610 01  BADDOPTCD.                                                   00  INPT
076620     03  FILLER                  PICTURE X(20)                    00  INPT
076630                         VALUE "D*OPTS CARD OPTION, ".            00  INPT
076640     03  BADDOPTNAME             PICTURE X(6).                    00  INPT
076650     03  FILLER                  PICTURE X(29)                    00  INPT
076660                         VALUE " NOT VALID. DROPPED REMAINDER".   00  INPT
076670                                                                  00  INPT
076680                                                                  00  INPT
076690 01  GLE-SWS.                                                     00  INPT
076700     03  GLE-SW-NOT              PICTURE X                        00  INPT
076710                         VALUE SPACE.                             00  INPT
076720     03  GLE-SW-L                PICTURE X                        00  INPT
076730                         VALUE SPACE.                             00  INPT
076740     03  GLE-SW-G                PICTURE X                        00  INPT
076750                         VALUE SPACE.                             00  INPT
076760                                                                  00  INPT
076770                                                                  00  INPT
076780 01  OP-SET.                                                      00  INPT
076790     03  OP-EQ                   PICTURE X                        00  INPT
076795                         VALUE SPACE.                             00  INPT
076800     03  OP-LT                   PICTURE X                        00  INPT
076805                         VALUE SPACE.                             00  INPT
076810     03  OP-GT                   PICTURE X                        00  INPT
076815                         VALUE SPACE.                             00  INPT
076820     03  OP-FLAG                 PICTURE X                        00  INPT
076825                         VALUE SPACE.                             00  INPT
076830                                                                  00  INPT
076840                                                                  00  INPT
076850                                                                  00  INPT
100000 01  LABEL-STACK.                                                 01  DCMP
100010     03  LSTACK                  PICTURE S9(3)   COMPUTATIONAL    01  DCMP
100020                         OCCURS 50.                               01  DCMP
100030                                                                  01  DCMP
100040 01  COND-NODE-STACK.                                             01  DCMP
100050     03  COND-NODE-COMP.                                          01  DCMP
100060         05  FILLER-C-N-C                                         01  DCMP
100070                         OCCURS 150.                              01  DCMP
100080             07  NSTACK          PICTURE S9(3)   COMPUTATIONAL.   01  DCMP
100090     03  COND-NODE-SWS.                                           01  DCMP
100100         05  FILLER-C-N-S                                         01  DCMP
100110                         OCCURS 150.                              01  DCMP
100120             07  YIND            PICTURE X.                       01  DCMP
100130             07  ARRAY-SW        PICTURE X.                       01  DCMP
100140                                                                  01  DCMP
100150 01  COND-ARRAY-VECTOR.                                           01  DCMP
100160     03  CARRAY                  PICTURE S9(3)   COMPUTATIONAL    01  DCMP
100170                         OCCURS 50.                               01  DCMP
100180                                                                  01  DCMP
100190 01  N-ARRAY-VECTOR.                                              01  DCMP
100200     03  NARRAY                  PICTURE S9(3)   COMPUTATIONAL    01  DCMP
100210                         OCCURS 50.                               01  DCMP
100220                                                                  01  DCMP
100230 01  Y-ARRAY-VECTOR.                                              01  DCMP
100240     03  YARRAY                  PICTURE S9(3)   COMPUTATIONAL    01  DCMP
100250                         OCCURS 50.                               01  DCMP
100260                                                                  01  DCMP
100270 01  N-CNT-VECTOR.                                                01  DCMP
100280     03  N-CNT                   PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100290                         OCCURS 50.                               01  DCMP
100300                                                                  01  DCMP
100310 01  Y-CNT-VECTOR.                                                01  DCMP
100320     03  Y-CNT                   PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100330                         OCCURS 50.                               01  DCMP
100340                                                                  01  DCMP
100350 01  WE-DASH-COUNT-VECTOR.                                        01  DCMP
100360     03  WE-DSH-CNT              PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100370                         OCCURS 50.                               01  DCMP
100380                                                                  01  DCMP
100385 01  W-DASH-COUNT-VECTOR.                                         01  DCMP
100386     03  W-DASH-CNT              PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100387                         OCCURS 50.                               01  DCMP
100388                                                                  01  DCMP
100389                                                                  01  DCMP
100390 01  TEMP-COL-VECTOR.                                             01  DCMP
100400     03  TEMP-COL                PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100410                         OCCURS 50.                               01  DCMP
100420 01  TEMP-COL-S999-REDEF REDEFINES TEMP-COL-VECTOR.               01  DCMP
100430     03  TEMP-COL-C-VECTOR.                                       01  DCMP
100440         05  TEMP-COL-C          PICTURE S9(3)   COMPUTATIONAL    01  DCMP
100450                         OCCURS 50.                               01  DCMP
100460                                                                  PDP-10
100470                                                                  PDP-10
100480                                                                  01  DCMP
100490 01  AST-CNT-VECTOR.                                              01  DCMP
100500     03  AST-CNT                 PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100510                         OCCURS 50.                               01  DCMP
100520                                                                  01  DCMP
100530 01  DELTA-VECTOR.                                                01  DCMP
100540     03  DELTA                   PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100550                         OCCURS 50.                               01  DCMP
100560                                                                  01  DCMP
100570 01  UTIL-CTRS.                                                   01  DCMP
100580     03  UTIL-1                  PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100590                         VALUE ZERO.                              01  DCMP
100600     03  UTIL-2                  PICTURE S9(7)   COMPUTATIONAL    01  DCMP
100610                         VALUE ZERO.                              01  DCMP
100660     03  N                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100670                         VALUE ZERO.                              01  DCMP
100680     03  P                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100690                         VALUE ZERO.                              01  DCMP
100700     03  Q                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100710                         VALUE ZERO.                              01  DCMP
100720     03  R                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100730                         VALUE ZERO.                              01  DCMP
100740     03  SU                      PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100750                         VALUE ZERO.                              01  DCMP
100760     03  T                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100770                         VALUE ZERO.                              01  DCMP
100780     03  U                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100790                         VALUE ZERO.                              01  DCMP
100800     03  V                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100810                         VALUE ZERO.                              01  DCMP
100820     03  W                       PICTURE S9(4)   COMPUTATIONAL    01  DCMP
100830                         VALUE ZERO.                              01  DCMP
110000 01  LITS-GRP.                                                    02  DCOB
110010     03  LITERALS.                                                02  DCOB
110020         05  FILLER              PICTURE X(32)                    02  DCOB
110030                         VALUE "IF DEXIT.ELSE GO TO DTATELNEXT S".02  DCOB
110040         05  FILLER              PICTURE X(32)                    02  DCOB
110050                         VALUE "ENTENCESECTION.GO TO DEXIT00000.".02  DCOB
110060     03  LIT-FIELD       REDEFINES LITERALS.                      02  DCOB
110070         05  LIT-CHAR            PICTURE X                        02  DCOB
110080                         OCCURS 64.                               02  DCOB
110090                                                                  02  DCOB
110100 01  WORK-LABEL.                                                  02  DCOB
110110     03  LABEL-CHAR              PICTURE X                        02  DCOB
110120                         OCCURS 33.                               02  DCOB
110130                                                                  02  DCOB
110140                                                                  02  DCOB
110560                                                                  02  DCOB
110570 01  STUB-CHAR.                                                   02  DCOB
110580     03  IN-STUB                 PICTURE X                        02  DCOB
110590                         OCCURS 60.                               02  DCOB
110600                                                                  02  DCOB
110610                                                                  02  DCOB
110620 01  STUB-WORK-AREA.                                              02  DCOB
110630     03  WORK-STUB               PICTURE X                        02  DCOB
110640                         OCCURS 60.                               02  DCOB
110650     03  STUB-CTR                PICTURE S9(2)   COMPUTATIONAL.   02  DCOB
110660                                                                  02  DCOB
110670                                                                  02  DCOB
110680 01  OUTPUT-REC.                                                  02  DCOB
110690     03  OUT-SEQ-NO              PICTURE 9(6).                    02  DCOB
110692     03  OUT-SEQ-NO-X    REDEFINES OUT-SEQ-NO                     02  DCOB
110693                                 PICTURE X(6).                    02  DCOB
110700     03  FILLER                  PICTURE X                        02  DCOB
110710                         VALUE SPACE.                             02  DCOB
110720     03  OUT-FIELD.                                               02  DCOB
110730         05  OUT-PREFIX          PICTURE X(2).                    02  DCOB
110740         05  OUT-TABLE-NO        PICTURE X(5).                    02  DCOB
110750         05  OUT-LABEL           PICTURE X(3).                    02  DCOB
110760         05  OUT-PERIOD          PICTURE X.                       02  DCOB
110770         05  OUT-FILLER          PICTURE X(62).                   02  DCOB
110780     03  OUT-FIELD2      REDEFINES OUT-FIELD.                     02  DCOB
110790         05  OUT-DEXIT           PICTURE X(5).                    02  DCOB
110800         05  OUT-DEX-TABLE       PICTURE X(5).                    02  DCOB
110810         05  OUT-DEX-EXIT        PICTURE X(7).                    02  DCOB
110820         05  FILLER              PICTURE X(56).                   02  DCOB
110830     03  OUT-CHAR-FIELD  REDEFINES OUT-FIELD2.                    02  DCOB
110840         05  OUTPUT-CHAR         PICTURE X                        02  DCOB
110850                         OCCURS 65.                               02  DCOB
110860         05  OUT-IDENT           PICTURE X(8).                    02  DCOB
110870     03  OUT-GO-TO-FIELD REDEFINES OUT-CHAR-FIELD.                02  DCOB
110880         05  FILLER              PICTURE X(4).                    02  DCOB
110890         05  CALOUT              PICTURE X(17).                   02  DCOB
110900         05  FILLERG     REDEFINES CALOUT.                        02  DCOB
110910             07  GOTODEXIT       PICTURE X(11).                   02  DCOB
110920             07  TABLE-NUMBER    PICTURE X(5).                    02  DCOB
110930             07  FILLER          PICTURE X.                       02  DCOB
110935         05  FILLERG1    REDEFINES FILLERG.                       02  DCOB
110936             07  FILLER          PICTURE X(8).                    02  DCOB
110937             07  G-AT-TBLNO      PICTURE X(5).                    02  DCOB
110938             07  G-AT-LBL        PICTURE X(3).                    02  DCOB
110939             07  FILLER          PICTURE X.                       02  DCOB
110940         05  FILLER              PICTURE X(52).                   02  DCOB
110950     03  DISPLAY-OUTR    REDEFINES OUT-GO-TO-FIELD.               02  DCOB
110960         05  FILLER              PICTURE X(4).                    02  DCOB
110970         05  DISPLAY-OUT         PICTURE X(61).                   02  DCOB
110975         05  DISPLAY-OUT-1                                        02  DCOB
110980                         REDEFINES DISPLAY-OUT.                   02  DCOB
111005             07  FILLER          PICTURE X(21).                   02  DCOB
111010             07  DISP-OUT-TBLNM  PICTURE X(32).                   02  DCOB
111015             07  DISP-OUT-MSG.                                    02  DCOB
111020                 08  DISP-OUT-ELS                                 02  DCOB
111025                                 PICTURE X(5).                    02  DCOB
111030                 08  FILLER      PICTURE X(3).                    02  DCOB
111035             07  DISP-OUT-MSG1                                    02  DCOB
111040                         REDEFINES DISP-OUT-MSG.                  02  DCOB
111045                 08  DISP-OUT-RLW                                 02  DCOB
111050                                 PICTURE X(3).                    02  DCOB
111053                 08  DISP-OUT-RLNO                                02  DCOB
111055                                 PICTURE X(3).                    02  DCOB
111060                 08  FILLER      PICTURE X(2).                    02  DCOB
111065         05  FILLER              PICTURE X(8).                    02  DCOB
111070     03  FILLERH         REDEFINES DISPLAY-OUTR.                  02  DCOB
111075         05  FILLER              PICTURE X(14).                   02  DCOB
111080         05  LABELNO-1           PICTURE X(5).                    02  DCOB
111085         05  RULENO-1            PICTURE X(3).                    02  DCOB
111090         05  FILLER              PICTURE X(8).                    02  DCOB
111095         05  LABELNO-2           PICTURE X(5).                    02  DCOB
111100         05  RULENO-2            PICTURE X(3).                    02  DCOB
111105         05  FILLER              PICTURE X(35).                   02  DCOB
111110     03  FILLERI         REDEFINES FILLERH.                       02  DCOB
111115         05  FILLER              PICTURE X(42).                   02  DCOB
111120         05  ELS-DISP-TBLNME     PICTURE X(18).                   02  DCOB
111125         05  FILLER              PICTURE X(13).                   02  DCOB
111126     03  FILLERJ         REDEFINES FILLERI.                       02  DCOB
111127         05  FILLER              PICTURE X(4).                    02  DCOB
111128         05  NOTE-WD             PICTURE X(4).                    02  DCOB
111129         05  FILLER              PICTURE X.                       02  DCOB
111130         05  DICT-WD             PICTURE X(30).                   02  DCOB
111131         05  FILLER              PICTURE X(34).                   02  DCOB
111132                                                                  02  DCOB
111133                                                                  02  DCOB
111140 01  CHARSTRING.                                                  02  DCOB
111150     03  CHAR                    PICTURE X                        02  DCOB
111160                         OCCURS 32.                               02  DCOB
111161 01  CHAR-VERB           REDEFINES CHARSTRING.                    02  DCOB
111162     03  VERB-STOP.                                               02  DCOB
111163         05  VERB-GO-TO          PICTURE X(6).                    02  DCOB
111164         05  FILLER              PICTURE X(2).                    02  DCOB
111165     03  FILLER                  PICTURE X(24).                   02  DCOB
111170                                                                  02  DCOB
111180                                                                  02  DCOB
111270 01  LASTREC.                                                     02  DCOB
111290     03  DUM-A-M                 PICTURE X(4).                    02  DCOB
111300     03  DUMMY1                  PICTURE X(61).                   02  DCOB
111310     03  DUMSCN          REDEFINES DUMMY1                         02  DCOB
111320                                 PICTURE X                        02  DCOB
111330                         OCCURS 61.                               02  DCOB
111350                                                                  02  DCOB
111360                                                                  02  DCOB
120000                                                                  03  IORT
120010                                                                  03  IORT
120020 01  NUMBERS.                                                     03  IORT
120030     03  SEQNUMB                 PICTURE 9(6)                     03  IORT
120040                         VALUE ZERO.                              03  IORT
120050     03  TEST-NUMB       REDEFINES SEQNUMB                        03  IORT
120060                                 PICTURE X(6).                    03  IORT
120070 01  FILLERA             REDEFINES NUMBERS.                       03  IORT
120080     03  SEQNUMB-1               PICTURE 9(3).                    03  IORT
120090     03  SEQNUMB-2               PICTURE 9(3).                    03  IORT
120100                                                                  03  IORT
120110                                                                  03  IORT
130000                                                                  04  FRMT
130005                                                                  04  FRMT
130010 01  WS-WORK-AREAS.                                               04  FRMT
130020     03  WS-ARRAY                PICTURE X(630)                   04  FRMT
130030                         VALUE SPACE.                             04  FRMT
130040     03  WS-FILLER       REDEFINES WS-ARRAY.                      04  FRMT
130050         05  WS-ELEM             PICTURE X                        04  FRMT
130060                         OCCURS 630.                              04  FRMT
130070     03  WS-ELEM-CD      REDEFINES WS-FILLER                      04  FRMT
130080                                 PICTURE X(63)                    04  FRMT
130090                         OCCURS 10.                               04  FRMT
130110     03  WS-NAME-OF-TBL          PICTURE X(33)                    04  FRMT
130120                         VALUE SPACE.                             04  FRMT
130170     03  WS-RULES.                                                04  FRMT
130180         05  WS-RL1S             PICTURE X(630)                   04  FRMT
130190                         VALUE SPACE.                             04  FRMT
130200         05  WS-RL2S             PICTURE X(630)                   04  FRMT
130210                         VALUE SPACE.                             04  FRMT
130220     03  WS-RULE         REDEFINES WS-RULES                       04  FRMT
130230                                 PICTURE X(63)                    04  FRMT
130240                         OCCURS 20.                               04  FRMT
130250     03  WS-FREQ                 PICTURE X(63)                    04  FRMT
130260                         VALUE SPACE.                             04  FRMT
130270     03  WS-MASK                 PICTURE X(660)                   04  FRMT
130280                         VALUE SPACE.                             04  FRMT
130290     03  WS-MK-CHAR      REDEFINES WS-MASK                        04  FRMT
130300                                 PICTURE X                        04  FRMT
130310                         OCCURS 660.                              04  FRMT
130320     03  WS-STUB-IND             PICTURE X                        04  FRMT
130330                         VALUE SPACE.                             04  FRMT
130340     03  WS-1ST-RULE             PICTURE X                        04  FRMT
130350                         VALUE SPACE.                             04  FRMT
130360     03  WS-1ST-COND             PICTURE X                        04  FRMT
130370                         VALUE SPACE.                             04  FRMT
130380     03  WS-1ST-ACT              PICTURE X                        04  FRMT
130390                         VALUE SPACE.                             04  FRMT
130400     03  WS-CORN-IND             PICTURE X                        04  FRMT
130410                         VALUE SPACE.                             04  FRMT
130420     03  WS-ERR-CODE             PICTURE 9(3)                     04  FRMT
130430                         VALUE ZERO.                              04  FRMT
130440                                                                  04  FRMT
130450                                                                  04  FRMT
130460 01  WS-XM-GRP.                                                   04  FRMT
130470     03  WORK-XM-1.                                               04  FRMT
130480         05  WORK-XM-1-4         PICTURE X(4).                    04  FRMT
130490         05  FILLER              PICTURE X(656).                  04  FRMT
130500     03  WORK-XM-2       REDEFINES WORK-XM-1.                     04  FRMT
130510         05  WS-XM-CHAR          PICTURE X                        04  FRMT
130520                         OCCURS 660.                              04  FRMT
130530                                                                  04  FRMT
130540                                                                  04  FRMT
130550 01  WS-SUBSCRIPTS.                                               04  FRMT
130560     03  WS-MK-SUB               PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130570                         VALUE ZERO.                              04  FRMT
130580     03  WS-EL-SUB               PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130590                         VALUE ZERO.                              04  FRMT
130600     03  WS-WK-SUB               PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130610                         VALUE ZERO.                              04  FRMT
130620     03  WS-CC-SUB               PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130630                         VALUE ZERO.                              04  FRMT
130640     03  WS-WR-SUB               PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130650                         VALUE ZERO.                              04  FRMT
130660     03  WS-RL1-SUB              PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130670                         VALUE ZERO.                              04  FRMT
130680     03  WS-RL2-SUB              PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130690                         VALUE ZERO.                              04  FRMT
130700     03  WS-ARY-SIZE             PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130710                         VALUE ZERO.                              04  FRMT
130720     03  WS-MSK-SIZE             PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130730                         VALUE ZERO.                              04  FRMT
130740     03  WS-XM-SUB               PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130750                         VALUE ZERO.                              04  FRMT
130760     03  WS-XM-SIZE              PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130770                         VALUE ZERO.                              04  FRMT
130780     03  WS-NO-PAGES             PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130790                         VALUE ZERO.                              04  FRMT
130820     03  WS-TTL-SIZE             PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130830                         VALUE ZERO.                              04  FRMT
130831 01  SUBSCRIPTS-RD       REDEFINES WS-SUBSCRIPTS.                 04  FRMT
130832     03  WS-FRMT-SUB             PICTURE S9(3)   COMPUTATIONAL    04  FRMT
130833                         OCCURS 13.                               04  FRMT
130840                                                                  04  FRMT
130850                                                                  04  FRMT
130860 01  WS-CONSTANTS.                                                04  FRMT
130870     03  WS-LIM-IND              PICTURE X                        04  FRMT
130880                         VALUE "E".                               04  FRMT
130890     03  WS-VER-CHAR             PICTURE X                        04  FRMT
130900                         VALUE "I".                               04  FRMT
130910     03  WS-HOR-CHAR             PICTURE X                        04  FRMT
130920                         VALUE "-".                               04  FRMT
130930     03  WS-COR-CHAR             PICTURE X                        04  FRMT
130940                         VALUE "*".                               04  FRMT
130950     03  WS-FIL-CHAR             PICTURE X                        04  FRMT
130960                         VALUE "_".                               PDP-10
130962     03  WS-FIL-CHAR-12-0-PUNCHES                                 04  FRMT
130963                         REDEFINES WS-FIL-CHAR                    04  FRMT
130964                                 PICTURE X.                       04  FRMT
130990     03  WS-OP-WK-OP             PICTURE X                        04  FRMT
131000                         VALUE "2".                               04  FRMT
131010     03  WS-WR-WK                PICTURE X                        04  FRMT
131020                         VALUE "3".                               04  FRMT
131030     03  WS-OP-WK-IP             PICTURE X                        04  FRMT
131040                         VALUE "4".                               04  FRMT
131050     03  WS-RD-WK                PICTURE X                        04  FRMT
131060                         VALUE "5".                               04  FRMT
131070     03  WS-CL-WK                PICTURE X                        04  FRMT
131080                         VALUE "6".                               04  FRMT
131110     03  WS-WR-PR                PICTURE X                        04  FRMT
131120                         VALUE "8".                               04  FRMT
131150                                                                  04  FRMT
131160                                                                  04  FRMT
131170 01  WORK-RECORD                 PICTURE X(660)                   04  FRMT
131180                         VALUE SPACE.                             04  FRMT
131190 01  FILLER1F            REDEFINES WORK-RECORD.                   04  FRMT
131200     03  WR-CHAR                 PICTURE X                        04  FRMT
131210                         OCCURS 660.                              04  FRMT
131220 01  FILLER2F            REDEFINES FILLER1F.                      04  FRMT
131230     03  WR-LINE         OCCURS 5.                                04  FRMT
131240         05  WR-LINE-1-3         PICTURE X(3).                    04  FRMT
131250         05  WR-LINE-4-ON        PICTURE X(129).                  04  FRMT
131260                                                                  04  FRMT
011070 01  LISTOUTPUT.                                                  03  IORT
011080     03  CONTROLX                PICTURE X.                       03  IORT
011090     03  FILLER                  PICTURE X(2).                    03  IORT
011100     03  SEQERR                  PICTURE X.                       03  IORT
011110     03  ERR-FLAGS               PICTURE X(3).                    03  IORT
011120     03  SEQUENCE                PICTURE 9(6).                    03  IORT
011130     03  SEQUENCE-X      REDEFINES SEQUENCE                       03  IORT
011140                                 PICTURE X(6).                    03  IORT
011150     03  FILLER                  PICTURE X(8).                    03  IORT
011160     03  OUTPUTD.                                                 03  IORT
011170         05  ERR-AST             PICTURE X(3).                    03  IORT
011180         05  FILLER          PIC X(4).                            03  IORT
011182         05  D-OPTNS         PIC X(7).                            03  IORT
011184         05  FILLER          PIC X(58).                           03  IORT
011186         05  OUTPUTD-IDEN        PICTURE X(8).                    03  IORT
011190     03  FILLER                  PICTURE X.                       03  IORT
011200     03  BND-WORD                PICTURE X(5).                    03  IORT
011210     03  FILLER                  PICTURE X(26).                   03  IORT
011220 01 GET-AT-132CHAR REDEFINES LISTOUTPUT.                          PDP-10
011260     03 FILLER                   PIC X.                           PDP-10
011270     03 PRINT-LINE-132           PIC X(132).                      PDP-10
131270                                                                  04  FRMT
131280 01  PRINT-RECORD                PICTURE X(133)                   04  FRMT
131290                         VALUE SPACE.                             04  FRMT
131300 01  FILLER3F            REDEFINES PRINT-RECORD.                  04  FRMT
131310     03  PR-CAR-CTL              PICTURE X.                       04  FRMT
131320     03  PR-LINE                 PICTURE X(132).                  04  FRMT
131330 01  ERROR-MSG4          REDEFINES FILLER3F.                      04  FRMT
131340     03  FILLER                  PICTURE X.                       04  FRMT
131350     03  TYPEMSG4                PICTURE X(19).                   04  FRMT
131360     03  ERROR-PRINT4.                                            04  FRMT
131370         05  E-P-BODY            PICTURE X(24).                   04  FRMT
131380         05  ERR-MSSG-NUMBER     PICTURE 9(3).                    04  FRMT
131390         05  E-P-END-BODY        PICTURE X(2).                    04  FRMT
131400         05  E-P-END-NAME        PICTURE X(30).                   04  FRMT
131410         05  E-P-END-PERIOD      PICTURE X(54).                   04  FRMT
131420                                                                  04  FRMT
131430                                                                  04  FRMT
131500 01  FORMAT-WORK-FILE-BUFFER.                                     04  FRMT
131510     03  FORMAT-PRINT-BUFFER.                                     04  FRMT
131515         05  FLAG-END-SPILL      PICTURE X(10)                    04  FRMT
131516                         VALUE SPACE.                             04  FRMT
131520         05  FILLER              PICTURE X(123)                   04  FRMT
131525                         VALUE SPACE.                             04  FRMT
131530     03  FILLER                  PICTURE X(527)                   04  FRMT
131540                         VALUE SPACE.                             04  FRMT
131550 01  FORM-WK-BUFF-RD     REDEFINES FORMAT-WORK-FILE-BUFFER.       04  FRMT
131560     03  FORMAT-BUFF             PICTURE X(66)                    04  FRMT
131570                         OCCURS 10.                               04  FRMT
131580                                                                  04  FRMT
131590                                                                  04  FRMT
200000 PROCEDURE DIVISION.                                              00  INPT
200010 A00-START-DETAP.                                                 00  INPT


           NOTE  ***  TYMSHARE CHANGES TO ALLOW USER CHOICE
                OF FILE NAMES.

       GET-NAME.
           MOVE SPACES TO SOURCE-ID, OUTPUT-ID, EXT-PERIOD-FOUND.
       GET-NAME-1.
           DISPLAY SPACE.
           DISPLAY "INPUT FILE: " WITH NO ADVANCING.
           ACCEPT TEMPORARY-NAME.
           MOVE 0 TO I.
           MOVE SPACES TO PROGRAM-NAME.
           PERFORM CONVERT-ID THRU CONVERT-ID-EXIT
                VARYING J FROM 1 BY 1
                UNTIL J > 10.
           IF I > 9 
                DISPLAY "?NAME TOO LONG",
                GO TO GET-NAME-1.
           MOVE PROGRAM-NAME TO SOURCE-ID.

           IF NOT EXT-PER-FOUND
                IF SOURCE-ID-EXT EQUALS SPACES
                    MOVE "CBL" TO SOURCE-ID-EXT.

       GET-NAME-2.
           DISPLAY SPACE.
           DISPLAY "OUTPUT FILE: " WITH NO ADVANCING.
           ACCEPT TEMPORARY-NAME.
           MOVE 0 TO I.
           MOVE SPACES TO PROGRAM-NAME, EXT-PERIOD-FOUND.
           PERFORM CONVERT-ID THRU CONVERT-ID-EXIT
                VARYING J FROM 1 BY 1 
                UNTIL J > 10.
           IF I > 9
                DISPLAY "?NAME TOO LONG",
                GO TO GET-NAME-2.
           IF PROGRAM-NAME IS EQUAL TO SPACES
                MOVE SOURCE-ID-NAME TO OUTPUT-ID-NAME,
           ELSE MOVE PROGRAM-NAME TO OUTPUT-ID.
           IF NOT EXT-PER-FOUND
              IF OUTPUT-ID-EXT IS EQUAL TO SPACES
                    MOVE "CBL" TO OUTPUT-ID-EXT.
           MOVE OUTPUT-ID-NAME TO LISTING-ID-NAME.
           MOVE "TBL" TO LISTING-ID-EXT.
           MOVE OUTPUT-ID-NAME TO WORK-FL-ID-NAME.
           MOVE "TMP" TO WORK-FL-ID-EXT.

           GO TO A00-START-DETAP-2.

       CONVERT-ID.
           MOVE TN-CHAR (J) TO TN-CHAR (1).
           IF TN-CHAR (1) IS NOT EQUAL TO "."
                GO TO CONVERT-ID-2.
           MOVE "X" TO EXT-PERIOD-FOUND.
           IF I IS GREATER THAN 6
                MOVE 10 TO I, J;
           ELSE MOVE 6 TO I.
           GO TO CONVERT-ID-EXIT.

       CONVERT-ID-2.
           IF TN-CHAR (1) IS EQUAL TO SPACE
                 NEXT SENTENCE;
           ELSE IF I IS GREATER THAN 8
                    MOVE 10 TO I, J;
                ELSE ADD 1 TO I,
                     MOVE TN-CHAR (1) TO PN-CHAR (I).

       CONVERT-ID-EXIT.
           EXIT.


       A00-START-DETAP-2.

200020     MOVE OPEN-IO TO TYPE-IO.                                     00  INPT
200030     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
200040                         NOTE OPEN INPUT AND PRINT FILES.         00  INPT
200050     MOVE WRITE-LIST TO TYPE-IO.                                  00  INPT
200060     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
200070                         NOTE WRITE TITLE PRINT LINE.             00  INPT
200080     MOVE EJECT-PAGE TO TYPE-IO.                                  00  INPT
200090     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
200100                         NOTE EJECT PAGE ON PRINT FILE.           00  INPT
200110                                                                  00  INPT
200120 A05-INITIALIZE-FOR-NEXT-TBL.                                     00  INPT
200130     MOVE ZERO TO CNTR-1.                                         00  INPT
200135     MOVE 050 TO CNTR-2.                                          00  INPT
200140     MOVE ZERO TO DETAP-IND.                                      00  INPT
200150     MOVE ZERO TO APROW.                                          00  INPT
200160     MOVE ZERO TO AROW.                                           00  INPT
200170     MOVE ZERO TO CROW.                                           00  INPT
200180     MOVE ZERO TO TBLNO-1.                                        00  INPT
200200     MOVE ZERO TO SHELFINDEX.                                     00  INPT
200250     MOVE SPACE TO CONDSTK-VECTOR.                                00  INPT
200255     MOVE ZERO TO ERROR-IND.                                      00  INPT
200260     MOVE SPACE TO ACTNSTK-VECTOR.                                00  INPT
200270     MOVE ZERO TO ELSERULE.                                       00  INPT
200280     MOVE ZERO TO TRACE-IND.                                      00  INPT
200290     MOVE ZERO TO DXNUM.                                          00  INPT
200300     MOVE ZERO TO SEQUENCE-NO-X.                                  00  INPT
200330     MOVE SPACE TO CONDITION-MATRIX.                              00  INPT
200340     MOVE ZERO TO ININDEX.                                        00  INPT
200350     MOVE ZERO TO INDARRAY-VECTOR.                                00  INPT
200360     MOVE ZERO TO PFRMINDX.                                       00  INPT
200370                                                                  00  INPT
200380 A10-LOOP-TO-CLEAR-VECTORS.                                       00  INPT
200390     ADD 001 TO CNTR-1.                                           00  INPT
200400     ADD 001 TO CNTR-2.                                           00  INPT
200410     MOVE ZERO TO TEMP-COLM (CNTR-1).                             00  INPT
200420     MOVE ZERO TO TEMP-COLM (CNTR-2).                             00  INPT
200430     MOVE ZERO TO CSTUBPTR (CNTR-1).                              00  INPT
200440     MOVE ZERO TO CSTUBPTR (CNTR-2).                              00  INPT
200450     MOVE ZERO TO ASTUBPTR (CNTR-1).                              00  INPT
200460     MOVE ZERO TO ASTUBPTR (CNTR-2).                              00  INPT
200470     MOVE ZERO TO TNARRAY (CNTR-1).                               00  INPT
200480     MOVE CNTR-1 TO ACTNPTR (CNTR-1).                             00  INPT
200490     MOVE CNTR-1 TO ACTNLST (CNTR-1).                             00  INPT
200500     MOVE CNTR-2 TO ACTNLST (CNTR-2).                             00  INPT
200510     MOVE CNTR-1 TO CONDLST (CNTR-1).                             00  INPT
200520     MOVE CNTR-2 TO CONDLST (CNTR-2).                             00  INPT
200530     MOVE CNTR-1 TO INARRAY (CNTR-1).                             00  INPT
200540     MOVE 001 TO FREQ (CNTR-1).                                   00  INPT
200550     IF CNTR-1 LESS THAN 050                                      00  INPT
200560         GO TO A10-LOOP-TO-CLEAR-VECTORS.                         00  INPT
200570     MOVE ZERO TO CSTUBPTR (101).                                 00  INPT
200580     MOVE ZERO TO ASTUBPTR (101).                                 00  INPT
200590     ALTER B55-INITIALIZ-SW                                       00  INPT
200600         TO PROCEED TO B56-CK-FOR-INITIALIZ-CARDS.                00  INPT
200604                                                                  00  INPT
200605 A12-CLEAR-WS.                                                    00  INPT
200610     MOVE 01 TO CNDINDEX.                                         00  INPT
200620     MOVE 01 TO ATNINDEX.                                         00  INPT
200630     MOVE 01 TO CPTRINDEX.                                        00  INPT
200640     MOVE 01 TO APTRINDEX.                                        00  INPT
200650     MOVE 001 TO CSTUBPTR (1).                                    00  INPT
200660     MOVE 001 TO ASTUBPTR (1).                                    00  INPT
200670     MOVE 1 TO EXTINDEX.                                          00  INPT
200710     MOVE ZERO TO CNTR-1.                                         00  INPT
200720     MOVE ZERO TO CNTR-2.                                         00  INPT
200730     MOVE ZERO TO CNTR-3.                                         00  INPT
200740     MOVE ZERO TO CNTR-4.                                         00  INPT
200750     MOVE ZERO TO CNTR-5.                                         00  INPT
200760     MOVE ZERO TO CNTR-6.                                         00  INPT
200770     MOVE ZERO TO CNTR-7.                                         00  INPT
200780     MOVE ZERO TO NOPUNCH.                                        00  INPT
200810     MOVE "C" TO OPTION-OPTIMIZ.                                  00  INPT
200820     MOVE ZERO TO PSACTN.                                         00  INPT
200830     MOVE ZERO TO PSCOND.                                         00  INPT
200840     MOVE ZERO TO STRING-FIELD.                                   00  INPT
200850     MOVE ZERO TO TMPID.                                          00  INPT
200860                                                                  00  INPT
200870 A14-BRANCH-AROUND-HDGS.                                          00  INPT
200880     GO TO A15-DO-HDGS.     NOTE ALTERED IN A15 TO GO TO A19.     00  INPT
200890                                                                  00  INPT
200900 A15-DO-HDGS.                                                     00  INPT
200910     MOVE DETAP-NAME-1 TO BUFFER.                                 00  INPT
200920     MOVE "1" TO NOPUNCH.                                         00  INPT
200925     MOVE "DETAPIMI"  TO  BUFF-IDEN.         NOTE IPAR D30.       00  INPT
200930     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
200940     MOVE DETAP-NAME-2 TO BUFFER.                                 00  INPT
200945     MOVE "DETAPIMI"  TO  BUFF-IDEN.         NOTE IPAR D30.       00  INPT
200950     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
200955     MOVE "DETAPIMI"  TO  BUFF-IDEN.         NOTE IPAR D30.       00  INPT
200960     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
200970     MOVE ZERO TO NOPUNCH.                                        00  INPT
200980     ALTER A14-BRANCH-AROUND-HDGS                                 00  INPT
200990         TO PROCEED TO A19-NEW-TABLE-SWITCH.                      00  INPT
200995     GO TO A25-FIND-NEXT-TABLE.                                   00  INPT
201000                                                                  00  INPT
201010 A19-NEW-TABLE-SWITCH.                                            00  INPT
201015     PERFORM J05-EOF-ERR-RECAP-0 THRU J20-EOF-ERR-RECAP-2.        00  INPT
201020     GO TO A25-FIND-NEXT-TABLE.                                   00  INPT
201030                                                                  00  INPT
201040 A20-INCR-TABLE-NO.                                               00  INPT
201050     ADD 00001 TO TBLNUM.                                         00  INPT
201060     GO TO A05-INITIALIZE-FOR-NEXT-TBL.                           00  INPT
201070                                                                  00  INPT
201080 A25-FIND-NEXT-TABLE.                                             00  INPT
201100     PERFORM A85-READ-INPUT-FILE THRU B45-READ-INPUT-FILE-EXIT.   00  INPT
201110     IF IDEN7 EQUAL TO "PGM END"                                  00  INPT
201120         GO TO A30-PGM-END.                                       00  INPT
201130     IF IDEN8 EQUAL TO "FILE END"                                 00  INPT
201140         GO TO J00-END-OF-FILE.                                   00  INPT
201150     IF IDEN6 EQUAL TO "DETAP "                                   00  INPT
201160         GO TO A40-START-NEW-TABLE.                               00  INPT
201170     IF IDEN6 EQUAL TO "DETAP."                                   00  INPT
201180         GO TO A40-START-NEW-TABLE.                               00  INPT
201190     GO TO A19-NEW-TABLE-SWITCH.                                  00  INPT
201200                                                                  00  INPT
201210 A30-PGM-END.                                                     00  INPT
201220     MOVE 01 TO SUFFIX-NUMBER.                                    00  INPT
201230                                                                   PDP-10
201240                                                                   PDP-10
201250                                                                  00  INPT
201260 A32-RESET-TRACE.                                                 00  INPT
201270     IF TRACEALL  EQUAL TO "1"                                    00  INPT
201280         GO TO A34-RESET-FOR-NEW-PGM.                             00  INPT
201290     MOVE ZERO TO TRACEALL.                                       00  INPT
201310                                                                  00  INPT
201320 A34-RESET-FOR-NEW-PGM.                                           00  INPT
201330     MOVE 2 TO INCREMENT.                                         00  INPT
201340     MOVE "1" TO WRITEIND.                                        00  INPT
201380     MOVE ZERO TO TBLNO.                                          00  INPT
201390     MOVE ZERO TO TBLNO-1.                                        00  INPT
201400     MOVE EJECT-PAGE TO TYPE-IO.                                  00  INPT
201410     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
201420     MOVE 55 TO LINECNT.                                          00  INPT
201430     GO TO A19-NEW-TABLE-SWITCH.                                  00  INPT
201440                                                                  00  INPT
201450                                                                  00  INPT
201460                                                                  00  INPT
201470                                                                  00  INPT
201480 A40-START-NEW-TABLE.                                             00  INPT
201490     IF TRACE-IND NOT EQUAL TO "1"                                00  INPT
201500         MOVE TRACEALL TO TRACE-IND.                              00  INPT
201510     MOVE ZERO TO RULE-PROC-SW.                                   00  INPT
201520     PERFORM H10-OPTIONS-WRITE THRU H15-OPTIONS-WRITE-EXIT.       00  INPT
201530     MOVE "1" TO RLNUM.                                           00  INPT
201540     IF PARAMETERS EQUAL TO SPACE                                 00  INPT
201550         MOVE "1" TO P-SW                                         00  INPT
201560             ELSE MOVE ZERO TO P-SW.                              00  INPT
201570     EXAMINE TABLE-HDR-EXAMIN REPLACING ALL " " BY "0".           00  INPT
201580     IF TABLE-NO NOT NUMERIC                                      00  INPT
201590         MOVE ZERO TO TABLE-NO                                    00  INPT
201600         GO TO A42-CHECK-TBL-HDR-PARAMS.                          00  INPT
201610     IF TABLE-NO EQUAL TO "99999"                                 00  INPT
201620         MOVE TBLNO TO TBLNO-2.                                   00  INPT
201640     IF TABLE-NO NOT EQUAL TO ZERO                                00  INPT
201650         MOVE TBLNUM TO TBLNO-1                                   00  INPT
201660         MOVE TABLE-NO TO TBLNO.                                  00  INPT
201670                                                                  00  INPT
201680 A42-CHECK-TBL-HDR-PARAMS.                                        00  INPT
201690     IF PARAMETERS NOT NUMERIC                                    00  INPT
201700         MOVE ZERO TO PARAMETERS.                                 00  INPT
201710     MOVE PARAMETERS TO PARAMS.                                   00  INPT
201720     IF OPTIMIZATION EQUAL TO "R"                                 00  INPT
201730         MOVE "R" TO OPTION-OPTIMIZ                               00  INPT
201740             ELSE MOVE "C" TO OPTION-OPTIMIZ.                     00  INPT
201750     IF PERFORM-NO NOT NUMERIC                                    00  INPT
201760         MOVE 3 TO PERFORM-NUMBER                                 00  INPT
201770             ELSE MOVE PERFORM-NO-N TO PERFORM-NUMBER.            00  INPT
201780     IF PERFORM-NUMBER EQUAL TO 1                                 00  INPT
201790         MOVE 3 TO PERFORM-NUMBER.                                00  INPT
201800                                                                  00  INPT
201810 A45-FIND-RL-CARDS.                                               00  INPT
201820     PERFORM A85-READ-INPUT-FILE THRU B45-READ-INPUT-FILE-EXIT.   00  INPT
201830     IF IDENT EQUAL TO "TEND."                                    00  INPT
201840         PERFORM H20-RULE-ACTN-ERROR THRU H22-RULE-ACTN-ERR-EXIT  00  INPT
201850         GO TO A20-INCR-TABLE-NO.                                 00  INPT
201860     IF CTLCOL EQUAL TO "*"                                       00  INPT
201870         GO TO A45-FIND-RL-CARDS.                                 00  INPT
201880     IF RL-ID EQUAL TO RLID                                       00  INPT
201890         GO TO A47-CHECK-RL-NO.                                   00  INPT
201900     MOVE WARNING TO TYPEMSG.                                     00  INPT
201910     MOVE "NO $ ON RL2, ASSUME $ IN COL 72." TO ERROR-PRINT.      00  INPT
201920     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
201930     IF CNTR-1 LESS THAN 002                                      00  INPT
201940         GO TO H35-RULE-ERROR.                                    00  INPT
201950     MOVE "$" TO STR-ING (CNTR-1).                                00  INPT
201960     MOVE "1" TO READFLIN-SKIP-SW.                                00  INPT
201970                                 NOTE PREVENTS READING ANOTHER    00  INPT
201980                                   CARD SINCE YOU ALREADY HAVE    00  INPT
201990                                   AN UNPROCESSED ONE.            00  INPT
202000     MOVE TABLE-CARD TO SCAN-FIELD.                               00  INPT
202010     GO TO A65-RULE-SCAN.     NOTE HAVE LAST RL CARD.             00  INPT
202020                                                                  00  INPT
202030 A47-CHECK-RL-NO.                                                 00  INPT
202040     IF RL-NUM NOT EQUAL TO RLNUM                                 00  INPT
202050         GO TO H35-RULE-ERROR.    NOTE HAVE AN RL CARD HERE.      00  INPT
202054                                                                  00  INPT
202055 A48-UPDATE-RLNUM.                                                00  INPT
202060     IF RLNUM EQUAL TO "1"                                        00  INPT
202070         MOVE "2" TO RLNUM                                        00  INPT
202080             ELSE MOVE "1" TO RLNUM.  NOTE READY FOR NEXT RL CARD.00  INPT
202088 A48-UPDATE-RLNUM-EXIT.                                           00  INPT
202089     EXIT.                                                        00  INPT
202090                                                                  00  INPT
202100 A50-RULE-CARD-1.                                                 00  INPT
202110     PERFORM A85-READ-INPUT-FILE THRU B45-READ-INPUT-FILE-EXIT.   00  INPT
202120     IF IDENT EQUAL TO "TEND."                                    00  INPT
202130         PERFORM H20-RULE-ACTN-ERROR THRU H22-RULE-ACTN-ERR-EXIT  00  INPT
202140         GO TO A20-INCR-TABLE-NO.                                 00  INPT
202150     IF CTLCOL EQUAL TO "*"                                       00  INPT
202160         GO TO A50-RULE-CARD-1.                                   00  INPT
202170     IF RL-ID NOT EQUAL TO RLID                                   00  INPT
202180         GO TO H35-RULE-ERROR.                                    00  INPT
202190     IF RL-NUM NOT EQUAL TO RLNUM                                 00  INPT
202200         GO TO H35-RULE-ERROR.                                    00  INPT
202210     PERFORM A48-UPDATE-RLNUM  THRU  A48-UPDATE-RLNUM-EXIT.       00  INPT
202220     IF RULE-PROC-SW EQUAL TO "1"                                 00  INPT
202230         MOVE ZERO TO RULE-PROC-SW                                00  INPT
202240         GO TO A60-RULE-CARD-5.                                   00  INPT
202250                                                                  00  INPT
202260 A53-RULE-CARD-2.                                                 00  INPT
202270     IF CARD-FIELD EQUAL TO SPACE                                 00  INPT
202280         MOVE 060 TO REXTENSIONS (1)                              00  INPT
202290         MOVE ZERO TO TALLY                                       00  INPT
202295         MOVE 0 TO TALLX                                          PDP-10
202300         MOVE 001 TO CNTR-1                                       00  INPT
202305         MOVE "1" TO NO-ENT-ON-STUB                               00  INPT
202310         GO TO A58-RULE-CARD-4.                                   00  INPT
202315     MOVE ZERO TO NO-ENT-ON-STUB.                                 00  INPT
202320     EXAMINE CARD-FIELD TALLYING LEADING " ".                     00  INPT
202325     MOVE TALLY TO TALLX.                                         PDP-10
202330     IF TALLX NOT EQUAL TO 0                                      PDP-10
202340         GO TO A55-RULE-CARD-2A.                                  00  INPT
202350     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
202370     MOVE STBERROR TO ERROR-PRINT.                                00  INPT
202380     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
202390     GO TO A20-INCR-TABLE-NO.    NOTE NO STUB.                    00  INPT
202400                                                                  00  INPT
202410 A55-RULE-CARD-2A.                                                00  INPT
202420     ADD 1 TO TALLX.  NOTE ONLY HERE ON 1ST NON-BLANK-RL CARD.    PDP-10
202430     MOVE TALLX TO REXTENSIONS (1). NOTE 1ST SIGNIF RULE COLUMN.  PDP-10
202440     ADD 1 TO TALLX.                                              PDP-10
202450     MOVE 001 TO CNTR-1.                                          00  INPT
202460                                                                  00  INPT
202470 A56-MOVE-RULES-TO-STRING.                                        00  INPT
202480     MOVE  CRDCOL (TALLX) TO STR-ING (CNTR-1).                    PDP-10
202490     ADD 001 TO CNTR-1.                                           00  INPT
202500     ADD 1 TO TALLX.                                              PDP-10
202510     IF TALLX LESS THAN 64                                        PDP-10
202520         GO TO A56-MOVE-RULES-TO-STRING.                          00  INPT
202530     EXAMINE CARD-FIELD TALLYING ALL "$".                         00  INPT
202540     MOVE TALLY TO TALLX.                                         PDP-10
202550 A58-RULE-CARD-4.                                                 00  INPT
202560     IF TALLX EQUAL TO ZERO                                       PDP-10
202570         MOVE "1" TO RULE-PROC-SW                                 00  INPT
202580         GO   TO A45-FIND-RL-CARDS. NOTE NO $ ON THIS CARD        00  INPT
202590                                SO EXPECT MORE RL CARDS.          00  INPT
202600     GO TO A65-RULE-SCAN.   NOTE HAVE ALL RL CARDS.               00  INPT
202610                                                                  00  INPT
202620 A60-RULE-CARD-5.                                                 00  INPT
202630     IF CARD-FIELD EQUAL TO SPACE                                 00  INPT
202640         GO TO A58-RULE-CARD-4.                                   00  INPT
202650     IF EXTINDEX LESS THAN 5                                      00  INPT
202660         GO TO A62-RULE-CARD-5A.                                  00  INPT
202670     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
202671*       TOO MANY RULE EXTENSIONS                                  00  INPT
202690     MOVE EXTERROR TO ERROR-PRINT.                                00  INPT
202700     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
202710     GO TO A12-CLEAR-WS.                                          00  INPT
202720                                                                  00  INPT
202730 A62-RULE-CARD-5A.                                                00  INPT
202740     ADD 1 TO EXTINDEX.                                           00  INPT
202750     EXAMINE CARD-FIELD TALLYING LEADING " ".                     00  INPT
202755     MOVE TALLY TO TALLX.                                         PDP-10
202760     ADD 1 TO TALLX.                                              PDP-10
202770     MOVE TALLX TO REXTENSIONS (EXTINDEX).                        PDP-10
202780     ADD 1 TO TALLX.                                              PDP-10
202790     GO TO A56-MOVE-RULES-TO-STRING.                              00  INPT
202800                                                                  00  INPT
202810                                                                  00  INPT
202820                                                                  00  INPT
202830 A65-RULE-SCAN.                                                   00  INPT
202840     MOVE SPACE TO STR-ING (1).                                   00  INPT
202850     MOVE ZERO TO CNTR-3.                                         00  INPT
202860     MOVE ZERO TO NRULES.                                         00  INPT
202870     MOVE 001 TO CNTR-2.                                          00  INPT
202880                        NOTE SIGNIF RL CARD PORTION IS STRUNG     00  INPT
202890                       OUT IN 300 CHARACTER FIELD STRING.         00  INPT
202900                        NOTE A66 WILL CALCULATE THE NUMBER OF     00  INPT
202910                          RULES AND THE LENGTH OF EACH AND STOP   00  INPT
202920                          WHEN IT ENCOUNTERS A $ IN THE STRING.   00  INPT
202930                                                                  00  INPT
202940 A66-RULE-SCAN-LOOP.                                              00  INPT
202950     IF STR-ING (CNTR-2) EQUAL TO SPACE                           00  INPT
202960         ADD 001 TO CNTR-2                                        00  INPT
202970         ADD 001 TO CNTR-3                                        00  INPT
202980         GO TO A66-RULE-SCAN-LOOP.                                00  INPT
202990     ADD 01 TO NRULES.                                            00  INPT
203000     IF CNTR-3 GREATER THAN 064                                   00  INPT
203010         SUBTRACT CNTR-3 FROM CNTR-2                              00  INPT
203020         ADD 064 TO CNTR-2                                        00  INPT
203030         MOVE 064 TO CNTR-3.                                      00  INPT
203040     MOVE CNTR-3 TO RLENGTH (NRULES).                             00  INPT
203050     IF STR-ING (CNTR-2) EQUAL TO "$"                             00  INPT
203060         GO TO A68-HAVE-SCANNED-RULES.                            00  INPT
203080     ADD 001 TO CNTR-2.                                           00  INPT
203090     MOVE 001 TO CNTR-3.                                          00  INPT
203100     GO TO A66-RULE-SCAN-LOOP.                                    00  INPT
203110                                                                  00  INPT
203120                                                                  00  INPT
203130 A68-HAVE-SCANNED-RULES.                                          00  INPT
203140     SUBTRACT RLENGTH (NRULES) FROM CNTR-2.                       00  INPT
203150     MOVE STR-ING (CNTR-2) TO Q1.                                 00  INPT
203160     IF Q1 EQUAL TO "E"                                           00  INPT
203170         GO TO A69-HAVE-AN-ELSE.                                  00  INPT
203180     IF Q1 EQUAL TO "L"                                           00  INPT
203190         GO TO A69-HAVE-AN-ELSE.                                  00  INPT
203200     MOVE ZERO TO ELSE-IND.                                       00  INPT
203210     GO TO A70-CHECK-NBR-OF-RULES.                                00  INPT
203220                                                                  00  INPT
203230 A69-HAVE-AN-ELSE.                                                00  INPT
203240     MOVE "1" TO ELSE-IND.                                        00  INPT
203250                                                                  00  INPT
203260 A70-CHECK-NBR-OF-RULES.                                          00  INPT
203270     MOVE NRULES TO NBR-RULES.                                    00  INPT
203280     IF NBR-RULES EQUAL TO ZERO                                   00  INPT
203290         PERFORM H20-RULE-ACTN-ERROR THRU H22-RULE-ACTN-ERR-EXIT  00  INPT
203300         GO TO A20-INCR-TABLE-NO.                                 00  INPT
203310     MOVE ZERO TO NEXT-IND.                                       00  INPT
203320     MOVE ZERO TO CNTR-7.                                         00  INPT
203330                                                                  00  INPT
203340 A75-SETUP-NEXT-ROW-OF-TABLE.                                     00  INPT
203350     MOVE 001 TO CNTR-5.                                          00  INPT
203360     MOVE 001 TO CNTR-6.                                          00  INPT
203370     MOVE   1 TO EXTNS.                                           00  INPT
203380     MOVE  01 TO SHELFINDEX.                                      00  INPT
203400     MOVE SPACE TO TYPEID.                                        00  INPT
203410     MOVE SPACE TO STUB-STACK.                                    00  INPT
203420     MOVE SPACE TO RULE-SHELVES.                                  00  INPT
203430     MOVE ZERO TO CNTR-4.                                         00  INPT
203440     MOVE ZERO TO STUBINDEX.                                      00  INPT
203450     PERFORM B50-READ-A-TABLE-BODY-CARD THRU B90-READ-TBLBOD-EXIT.00  INPT
203460     IF RESULT EQUAL TO "2"                                       00  INPT
203470         MOVE ZERO TO RESULT                                      00  INPT
203480         GO TO E75-SORT-ACTION-MATRIX. NOTE HAVE TEND CARD        00  INPT
203485                                        SO ROWS ARE ALL DONE.     00  INPT
203490                                                                  00  INPT
203500 A77-FIND-NEXT-ELEMENT.                                           00  INPT
203510     MOVE SPACE TO SCAN-FIELD.                                    00  INPT
203520     MOVE ZERO TO CNTR-6.                                         00  INPT
203530                                                                  00  INPT
203540 A78-LOOP-TO-ISOLATE-ELEMENT.                                     00  INPT
203550     ADD 001 TO CNTR-4.                                           00  INPT
203560     ADD 001 TO CNTR-6.                                           00  INPT
203570     MOVE STR-ING (CNTR-4) TO ELEMENT (CNTR-6).                   00  INPT
203580     IF CNTR-6 LESS THAN RLENGTH (SHELFINDEX)                     00  INPT
203590         GO TO A78-LOOP-TO-ISOLATE-ELEMENT.                       00  INPT
203600     PERFORM D90-LEFT-JUSTIFY THRU D93-LEFT-JUSTIFY-EXIT.         00  INPT
203610     IF ELEMENT (1) EQUAL TO CHARACTER-HOR-DITTO                  00  INPT
203620         PERFORM F75-REPLACE-DITTO THRU F77-REPLACE-DITTO-EXIT.   00  INPT
203630     IF SCAN-FIELD EQUAL TO "-"                                   00  INPT
203640         MOVE SPACE TO ELEMENT (1).                               00  INPT
203650     MOVE SCAN-FIELD TO SHELF (SHELFINDEX).                       00  INPT
203660     IF TYPEID EQUAL TO "F"                                       00  INPT
203665         MOVE SHELFINDEX TO CNTR-7                                00  INPT
203670         PERFORM B67A-PERFORM-FOR-FREQ THRU                       00  INPT
203671             B70-CHECK-FREQ-FIGURE.                               00  INPT
203680     IF SHELFINDEX EQUAL TO NRULES                                00  INPT
203690         GO TO A80-HAVE-ALL-ELEMENTS-FOR-ROW.                     00  INPT
203700     ADD 001 TO SHELFINDEX.                                       00  INPT
203710     GO TO A77-FIND-NEXT-ELEMENT.                                 00  INPT
203720                                                                  00  INPT
203730 A80-HAVE-ALL-ELEMENTS-FOR-ROW.                                   00  INPT
203740     GO TO   C00-EDIT-AND-CONVERT-TO-LE                             PDP-10  
203750 A81-LABEL.                                                        PDP-10
203760     GO TO A75-SETUP-NEXT-ROW-OF-TABLE.                           00  INPT
203770                                                                  00  INPT
203780                                                                  00  INPT
203790 A85-READ-INPUT-FILE.                                             00  INPT
203800     IF READFLIN-SKIP-SW EQUAL TO "1"                             00  INPT
203810         MOVE SCAN-FIELD TO BUFFER                                00  INPT
203820         MOVE SPACE TO SCAN-FIELD                                 00  INPT
203830         GO TO A88-HAVE-NEXT-INPUT-REC. NOTE ALREADY HAD A REC.   00  INPT
203840     MOVE ZERO TO RESULT.                                         00  INPT
203850     MOVE READ-INPUT TO TYPE-IO.                                  00  INPT
203860     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
203870                            NOTE READ AN INPUT FILE RECORD.       00  INPT
203880                                                                  00  INPT
203890 A88-HAVE-NEXT-INPUT-REC.                                         00  INPT
203900     MOVE BUFFER TO TABLE-CARD.                                   00  INPT
203910     IF RESULT NOT EQUAL TO "1"                                   00  INPT
203920         GO TO B45-READ-INPUT-FILE-EXIT.                          00  INPT
203930     MOVE IDENT TO TEST-ID.                                       00  INPT
203940     IF IDENT EQUAL TO "TEND"                                     00  INPT
203950         MOVE "TEND." TO IDENT                                    00  INPT
203960         MOVE "TEND." TO TEST-ID                                  00  INPT
203970         GO TO A90-HAVE-TEND-CARD.                                00  INPT
203980     IF IDENT EQUAL TO "TEND."                                    00  INPT
203990         GO TO A90-HAVE-TEND-CARD.                                00  INPT
204000     GO TO A92-WHAT-KIND-OF-CARD.                                 00  INPT
204010                                                                  00  INPT
204020 A90-HAVE-TEND-CARD.                                              00  INPT
204050     MOVE TABLE-CARD TO BUFFER.                                   00  INPT
204060     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
204070                                                                  00  INPT
204080 A92-WHAT-KIND-OF-CARD.                                           00  INPT
204090     IF TEST-ID NOT EQUAL TO "FILE"                               00  INPT
204100         GO TO A94-IS-IT-TABLE-HDR.                               00  INPT
204110     IF FILE-SECT NOT EQUAL TO "FILE END    "                     00  INPT
204120         MOVE SPACE TO TEST-ID.                                   00  INPT
204130                                                                  00  INPT
204140 A94-IS-IT-TABLE-HDR.                                             00  INPT
204150     IF IDEN6 NOT EQUAL TO "DETAP "                               00  INPT
204160         GO TO B12-CK-FOR-OPTION-CARDS.                           00  INPT
204170                                                                  00  INPT
204180 A95-ITS-A-TBL-HDR.                                               00  INPT
204190     MOVE SPACE TO BUFFER.                                        00  INPT
204200     IF WRITEIND NOT EQUAL TO "2"                                 00  INPT
204210         MOVE EJECT-PAGE TO TYPE-IO                               00  INPT
204220         PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.     00  INPT
204230     MOVE ZERO TO DOPT-IN-RPL-SW.                                 00  INPT
204240     MOVE IDENTIFIER TO IDENTFIELD.                               00  INPT
204250     IF SEQNUM-X NOT NUMERIC                                      00  INPT
204260         MOVE SEQNUM-X TO SEQNUM-SAVE                             00  INPT
204270         MOVE INCREMENT TO SEQNUM                                 00  INPT
204280         ADD 000001 TO SEQNUM                                     00  INPT
204290         GO TO A97-CHECK-TBL-NAME.                                00  INPT
204300     MOVE SEQNUM TO SEQUENCE-NO.                                  00  INPT
204310     SUBTRACT INCREMENT FROM SEQUENCE-NO.                         00  INPT
204320     SUBTRACT 000001    FROM SEQUENCE-NO.                         00  INPT
204330                                                                  00  INPT
204340 A97-CHECK-TBL-NAME.                                              00  INPT
204350     MOVE "1" TO DETAP-IND.                                       00  INPT
204380     IF TABLE-NAME NOT EQUAL TO SPACE                             00  INPT
204390         GO TO B00-SET-UP-TABLE-NAME.                             00  INPT
204400     MOVE SPEC-NAME TO TABLE-NAME.                                00  INPT
204410     MOVE WARNING TO TYPEMSG.                                     00  INPT
204420     MOVE NAMERROR TO ERROR-PRINT.                                00  INPT
204430     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
204440                             NOTE TABLE NAME MISSING.             00  INPT
204450                                                                  00  INPT
204460 B00-SET-UP-TABLE-NAME.                                           00  INPT
204470     MOVE TABLE-NAME TO SCAN-FIELD.                               00  INPT
204480     PERFORM D90-LEFT-JUSTIFY THRU D93-LEFT-JUSTIFY-EXIT.         00  INPT
204490     MOVE NAMEFIELD TO TBLNAME.                                   00  INPT
204500     PERFORM F70-BACKSCAN THRU F72-BACKSCAN-EXIT.                 00  INPT
204510     IF CNTR-7 GREATER THAN 025                                   00  INPT
204520         MOVE 025 TO CNTR-7                                       00  INPT
204530         MOVE SPACE TO ELEMENT-26-ON.                             00  INPT
204570 B03-SET-UP-TBL-NAME-NOTE.                                        00  INPT
204580     MOVE SCAN-FIELD-1-ON TO SECTION-OUT.                         00  INPT
204590     MOVE TBLNUM TO SUFFIX-NUMBER.                                00  INPT
204600     MOVE ZERO TO LEFT-1.                                         00  INPT
204610                                                                  00  INPT
204615 B05-MOVE-DT-NOTE.                                                00  INPT
204620     ADD 001 TO LEFT-1.                                           00  INPT
204630     ADD 001 TO CNTR-7.                                           00  INPT
204640     MOVE SUFFIXER (LEFT-1) TO NOTE-COL (CNTR-7).                 00  INPT
204650     IF LEFT-1 LESS THAN 011                                      00  INPT
204660         GO TO B05-MOVE-DT-NOTE.                                  00  INPT
204670     MOVE VERS-LEVEL-DT TO VERS-LEVEL-ID.                         00  INPT
204680                                                                  00  INPT
204690 B10-WRITE-HDR.                                                   00  INPT
204700     ADD INCREMENT TO SEQUENCE-NO.                                00  INPT
204710     MOVE SEQUENCE-NO-X TO TSEQ-NO-X.                             00  INPT
204720     MOVE IDENTFIELD TO TIDENT-FIELD.                             00  INPT
204730     MOVE "1" TO TMPID.                                           00  INPT
204740     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
204750     MOVE ZERO TO TMPID.                                          00  INPT
204760     MOVE TABLE-HEADER TO BUFFER.                                 00  INPT
204770     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
204780                                                                  00  INPT
204790 B12-CK-FOR-OPTION-CARDS.                                         00  INPT
204800     IF IDEN6 NOT EQUAL TO "TRACE"                                00  INPT
204810         GO TO B16-FINAL-TYPE-CARD-CK.                            00  INPT
204820     MOVE "1" TO TRACE-IND.      NOTE TRACE NEXT TABLE SWITCH.    00  INPT
204830     IF RNGETP EQUAL TO "ALL"                                     00  INPT
204840         GO TO B13-TRACE-ALL.                                     00  INPT
204850     IF IMM-ALL EQUAL TO "ALL"                                    00  INPT
204860         GO TO B13-TRACE-ALL.                                     00  INPT
 204870     GO TO B14-CK-TRACE.                                          00  INPT
204880                                                                  00  INPT
204890 B13-TRACE-ALL.                                                   00  INPT
204900     MOVE "1" TO TRACEALL.   NOTE TRACE ALL FOLLOWING TABLES      00  INPT
204910                                  IN PROGRAM.                     00  INPT
204920     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
204930                                                                  00  INPT
204940 B14-CK-TRACE.                                                    00  INPT
204950     IF RNGETP EQUAL TO "NO "                                     00  INPT
204960         GO TO B15-TURN-OFF-TRACE.                                00  INPT
204970     IF RNGETP EQUAL TO " NO"                                     00  INPT
204980         GO TO B15-TURN-OFF-TRACE.                                00  INPT
204990     IF IMM-ALL EQUAL TO "NO "                                    00  INPT
205000         GO TO B15-TURN-OFF-TRACE.                                00  INPT
205010     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
205020                                                                  00  INPT
205030 B15-TURN-OFF-TRACE.                                              00  INPT
205040     MOVE ZERO TO TRACE-IND.                                      00  INPT
205050     MOVE ZERO TO TRACEALL.                                       00  INPT
205070     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
205080                                                                  00  INPT
205090 B16-FINAL-TYPE-CARD-CK.                                          00  INPT
205100     IF IDEN7 EQUAL TO "PGM END"                                  00  INPT
205110         GO TO B42-WRITE-THIS-CARD.                               00  INPT
205120     IF IDEN9 EQUAL TO "FILE END"                                 00  INPT
205130         GO TO  B42-WRITE-THIS-CARD.                              00  INPT
205140     IF IDENT EQUAL TO "D*OPT"                                    00  INPT
205150         GO TO B20-PROCESS-DOPTS.                                 00  INPT
205155     IF IDENT EQUAL TO "OPTIO"                                    00  INPT
205156         GO TO B20-PROCESS-DOPTS.           NOTE IPAR D12.        00  INPT
205160     IF IDEN12 EQUAL TO "PRINT-DETAP"                             00  INPT
205170         GO TO B18-RESET-PUNCH-PRINT-INDS.                        00  INPT
205180     IF IDEN12 EQUAL TO "PUNCH-DETAP"                             00  INPT
205190         GO TO B18-RESET-PUNCH-PRINT-INDS.                        00  INPT
205200     IF IDEN6 EQUAL TO "PBOTH"                                    00  INPT
205210         GO TO B18-RESET-PUNCH-PRINT-INDS.                        00  INPT
205220     IF ACTN-SECT-14 EQUAL TO "COMPILE-DETAP"                     00  INPT
205230         GO TO B18-RESET-PUNCH-PRINT-INDS.                        00  INPT
205240     MOVE SPACE TO TEST-ID.                                       00  INPT
205250     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
205260                                                                  00  INPT
205270 B18-RESET-PUNCH-PRINT-INDS.                                      00  INPT
205300     IF TEST-ID EQUAL TO "PRINT"                                  00  INPT
205310         PERFORM I00-LINE-COUNT THRU I02-LINE-COUNT-EXIT          00  INPT
205320         MOVE "1" TO WRITEIND                                     00  INPT
205330         GO TO B42-WRITE-THIS-CARD.                               00  INPT
205340     IF TEST-ID EQUAL TO "PBOTH"                                  00  INPT
205350         PERFORM I00-LINE-COUNT THRU I02-LINE-COUNT-EXIT          00  INPT
205360         MOVE "3" TO WRITEIND                                     00  INPT
205370         GO TO B19-OPEN-PUNCH.                                    00  INPT
205380     MOVE "2" TO WRITEIND.                                        00  INPT
205390                                                                  00  INPT
205400 B19-OPEN-PUNCH.                                                  00  INPT
205410     MOVE OPEN-PUNCH TO TYPE-IO.                                  00  INPT
205420     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
205430     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
205435                                                                  00  INPT
205440 B20-PROCESS-DOPTS.                                               00  INPT
205450     MOVE ZERO TO SUBS.   NOTE PROCESS D*OPTNS CARD.              00  INPT
205460                                                                  00  INPT
205470 B21-PROCESS-DOPT-LOOP.                                           00  INPT
205480     ADD 001 TO SUBS                                              00  INPT
205482     IF SUBS GREATER THAN 10                                      00  INPT
205483         GO TO B42-WRITE-THIS-CARD.                               00  INPT
205490     MOVE DOPT-OPT (SUBS) TO DOPT-TEST.                           00  INPT
205500     IF DOPT-TEST EQUAL TO SPACE                                  00  INPT
205510         GO TO B42-WRITE-THIS-CARD.                               00  INPT
205520     IF DOPTYN EQUAL TO "YS"                                      00  INPT
205530         MOVE "1" TO P-D-SW                                       00  INPT
205540         GO TO B23-WHAT-KIND-OF-DOPT.                             00  INPT
205550     IF DOPTYN EQUAL TO "NO"                                      00  INPT
205560         MOVE "0" TO P-D-SW                                       00  INPT
205570         GO TO B23-WHAT-KIND-OF-DOPT.                             00  INPT
205580     GO TO B25-DOPT-ERROR.                                        00  INPT
205590                                                                  00  INPT
205600 B23-WHAT-KIND-OF-DOPT.                                           00  INPT
205610     IF DOPTXXX EQUAL TO "RCP"                                    00  INPT
205620         GO TO B27-DOPTS-RCP.                                     00  INPT
205630     IF DOPTXXX EQUAL TO "FMT"                                    00  INPT
205640         GO TO B28-DOPTS-FMT.                                     00  INPT
205650     IF DOPTXXX EQUAL TO "LIN"                                    00  INPT
205660         GO TO B29-DOPTS-LIN.                                     00  INPT
205670     IF DOPTXXX EQUAL TO "OVF"                                    00  INPT
205680         GO TO B30-DOPTS-OVF.                                     00  INPT
205690     IF DOPTXXX EQUAL TO "RPL"                                    00  INPT
205700         GO TO B31-DOPTS-RPL.                                     00  INPT
205710     IF DOPTXXX EQUAL TO "DCP"                                    00  INPT
205720         GO TO B32-DOPTS-DCP.                                     00  INPT
205730     IF DOPTXXX EQUAL TO "PRT"                                    00  INPT
205740         GO TO B33-DOPTS-PRT.                                     00  INPT
205750     IF DOPTXXX EQUAL TO "CMP"                                    00  INPT
205760         GO TO B35-DOPTS-CMP.                                     00  INPT
205770     IF DOPTXXX EQUAL TO "TRC"                                    00  INPT
205780         GO TO B37-DOPTS-TRC.                                     00  INPT
205790     IF DOPTXXX EQUAL TO "DIT"                                    00  INPT
205800         GO TO B38-DOPTS-DIT.                                     00  INPT
205803     IF DOPTXXX EQUAL TO "SEQ"                                    00  INPT
205804         GO TO  B41-DOPTS-SEQ.                                    00  INPT
205810     IF DOPTXXX EQUAL TO CONST-BND                                00  INPT
205820         GO TO B39-DOPTS-BND.                                     00  INPT
205830                                                                  00  INPT
205840 B25-DOPT-ERROR.                                                  00  INPT
205850     MOVE WARNING TO TYPEMSG.                                     00  INPT
205860     MOVE DOPT-TEST TO BADDOPTNAME.                               00  INPT
205870     MOVE BADDOPTCD TO ERROR-PRINT.                               00  INPT
205880     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
205890     MOVE TABLE-HEADER TO BUFFER.                                 00  INPT
205900     GO TO B42-WRITE-THIS-CARD.                                   00  INPT
205910                                                                  00  INPT
205920 B27-DOPTS-RCP.                                                   00  INPT
205930     MOVE P-D-SW TO DOPTS-SW-RCP.                                 00  INPT
205940     MOVE DOPTYN TO OM-RCP.                                       00  INPT
205950     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
205960                                                                  00  INPT
205970 B28-DOPTS-FMT.                                                   00  INPT
205980     MOVE P-D-SW TO DOPTS-SW-FMT.                                 00  INPT
205990     MOVE DOPTYN TO OM-FMT.                                       00  INPT
206000     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206010                                                                  00  INPT
206020 B29-DOPTS-LIN.                                                   00  INPT
206030     MOVE P-D-SW TO DOPTS-SW-EXT.                                 00  INPT
206040     MOVE DOPTYN TO OM-EXT.                                       00  INPT
206050     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206060                                                                  00  INPT
206070 B30-DOPTS-OVF.                                                   00  INPT
206080     MOVE P-D-SW TO DOPTS-SW-OVF.                                 00  INPT
206090     MOVE DOPTYN TO OM-OVF.                                       00  INPT
206100     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206110                                                                  00  INPT
206120 B31-DOPTS-RPL.                                                   00  INPT
206130     MOVE P-D-SW TO DOPTS-SW-RPL.                                 00  INPT
206140     MOVE DOPTYN TO OM-RPL.                                       00  INPT
206150     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206160                                                                  00  INPT
206170 B32-DOPTS-DCP.                                                   00  INPT
206180     MOVE P-D-SW TO DOPTS-SW-DCP.                                 00  INPT
206190     MOVE DOPTYN TO OM-DCP.                                       00  INPT
206200     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206210                                                                  00  INPT
206220 B33-DOPTS-PRT.                                                   00  INPT
206230     MOVE DOPTYN TO OM-PRT.                                       00  INPT
206240     IF P-D-SW EQUAL TO "1"                                       00  INPT
206250         GO TO B34-TURN-ON-PRT.  NOTE FALL THRU TO TURN OFF PRINT.00  INPT
206260     IF WRITEIND EQUAL TO "1"                                     00  INPT
206270         MOVE ZERO TO WRITEIND                                    00  INPT
206280         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206290     IF WRITEIND EQUAL TO "3"                                     00  INPT
206300         MOVE "2" TO WRITEIND.                                    00  INPT
206310     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206320                                                                  00  INPT
206330 B34-TURN-ON-PRT.                                                 00  INPT
206340     IF WRITEIND EQUAL TO "1"                                     00  INPT
206350         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206360     IF WRITEIND EQUAL TO "3"                                     00  INPT
206370         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206380     PERFORM I00-LINE-COUNT THRU I02-LINE-COUNT-EXIT.             00  INPT
206390     IF WRITEIND EQUAL TO ZERO                                    00  INPT
206400         MOVE "1" TO WRITEIND                                     00  INPT
206410         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206430     MOVE "3" TO WRITEIND.                                        00  INPT
206440     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206450                                                                  00  INPT
206460 B35-DOPTS-CMP.                                                   00  INPT
206470     IF P-D-SW EQUAL TO "1"                                       00  INPT
206480         GO TO B36-TURN-ON-PUNCH. NOTE FALL THRU TO TURNOFF PUNCH.00  INPT
206490     IF WRITEIND EQUAL TO "2"                                     00  INPT
206500         MOVE ZERO TO WRITEIND                                    00  INPT
206510         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206520     IF WRITEIND EQUAL TO "3"                                     00  INPT
206530         MOVE "1"  TO WRITEIND.                                   00  INPT
206540     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206550                                                                  00  INPT
206560 B36-TURN-ON-PUNCH.                                               00  INPT
206570     IF  WRITEIND EQUAL TO "2"                                    00  INPT
206580         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206590     IF  WRITEIND EQUAL TO "3"                                    00  INPT
206600         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206610     MOVE OPEN-PUNCH TO TYPE-IO.                                  00  INPT
206620     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
206630     IF  WRITEIND EQUAL TO ZERO                                   00  INPT
206640         MOVE "2" TO WRITEIND                                     00  INPT
206650         GO TO B21-PROCESS-DOPT-LOOP.                             00  INPT
206660     MOVE "3" TO WRITEIND.                                        00  INPT
206670     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206680                                                                  00  INPT
206690 B37-DOPTS-TRC.                                                   00  INPT
206700     MOVE P-D-SW TO TRACEALL.                                     00  INPT
206710     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206720                                                                  00  INPT
206730 B38-DOPTS-DIT.                                                   00  INPT
206740     MOVE P-D-SW TO DOPTS-SW-DIT.                                 00  INPT
206750     MOVE DOPTYN TO OM-DIT.                                       00  INPT
206760     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206770                                                                  00  INPT
206780 B39-DOPTS-BND.                                                   00  INPT
206790     MOVE DOPTYN TO OM-BND.                                       00  INPT
206800     GO TO B21-PROCESS-DOPT-LOOP.                                 00  INPT
206810                                                                  00  INPT
206813 B41-DOPTS-SEQ.                                                   00  INPT
206815     MOVE DOPTYN TO OM-SEQ.                                       00  INPT
206816     MOVE P-D-SW TO DOPTS-SW-SEQ.                                 00  INPT
206817     GO TO  B21-PROCESS-DOPT-LOOP.                                00  INPT
206819                                                                  00  INPT
206820 B42-WRITE-THIS-CARD.                                             00  INPT
206830     IF READFLIN-SKIP-SW EQUAL TO "1"                             00  INPT
206840         MOVE ZERO TO READFLIN-SKIP-SW                            00  INPT
206850         GO TO B44-TAKE-OUTRL2-XS.                                00  INPT
206860     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
206870                                                                  00  INPT
206880 B44-TAKE-OUTRL2-XS.                                              00  INPT
206890     IF RL2-ID-SLOT EQUAL TO "RL2"                                00  INPT
206900         EXAMINE TABLE-HEADER REPLACING ALL "X" BY " ".           00  INPT
206910                                                                  00  INPT
206920 B45-READ-INPUT-FILE-EXIT.                                        00  INPT
206930     EXIT.                                                        00  INPT
206940                                                                  00  INPT
206950                                                                  00  INPT
206960                                                                  00  INPT
206970                                                                  00  INPT
207000 B50-READ-A-TABLE-BODY-CARD.                                      00  INPT
207010     MOVE SPACE TO STRING-FIELD.                                  00  INPT
207020     IF NEXT-IND EQUAL TO "1"                                     00  INPT
207030         MOVE ZERO TO NEXT-IND                                    00  INPT
207040         MOVE SPACE TO SCAN-FIELD                                 00  INPT
207050         GO TO B52-HAVE-NEXT-CARD.                                00  INPT
207060                                                                  00  INPT
207070 B51-READ-A-CARD.                                                 00  INPT
207080     PERFORM A85-READ-INPUT-FILE THRU B45-READ-INPUT-FILE-EXIT.   00  INPT
207090                                                                  00  INPT
207100 B52-HAVE-NEXT-CARD.                                              00  INPT
207230     IF RESULT EQUAL TO "1"                                       00  INPT
207240         GO TO B55-INITIALIZ-SW.                                  00  INPT
207250     IF RESULT NOT EQUAL TO "3"                                   00  INPT
207260         MOVE ZERO TO RESULT                                      00  INPT
207270         GO TO B55-INITIALIZ-SW.                                  00  INPT
207280     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
207300     MOVE FLERROR TO ERROR-PRINT.                                 00  INPT
207310     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
207320     MOVE ZERO TO RESULT.                                         00  INPT
207330     GO TO J00-END-OF-FILE.                                       00  INPT
207340                                                                  00  INPT
207350 B55-INITIALIZ-SW.                                                00  INPT
207360     GO TO B56-CK-FOR-INITIALIZ-CARDS.                            00  INPT
207370                                                                  00  INPT
207380 B56-CK-FOR-INITIALIZ-CARDS.                                      00  INPT
207390     IF CTLCOL EQUAL TO SPACE                                     00  INPT
207400         GO TO B57-INIT-STMT.                                     00  INPT
207410     IF CTLCOL EQUAL TO "I"                                       00  INPT
207420         GO TO B57-INIT-STMT.                                     00  INPT
207430     GO TO B59-FIRST-NON-INIT.                                    00  INPT
207440                                                                  00  INPT
207450 B57-INIT-STMT.                                                   00  INPT
207460     IF COLM-9 NOT EQUAL TO SPACE                                 00  INPT
207470         MOVE INIT-FIELD-9 TO S-R-1-BODY                          00  INPT
207480         MOVE S-R-1-BODY TO INIT-FIELD.                           00  INPT
207490     IF ININDEX LESS THAN 050                                     00  INPT
207500         GO TO B58-FILE-THE-INIT-STMT.                            00  INPT
207510     MOVE WARNING TO TYPEMSG.                                     00  INPT
207520     PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT.             00  INPT
207530     GO TO B51-READ-A-CARD.                                       00  INPT
207540                                                                  00  INPT
207550 B58-FILE-THE-INIT-STMT.                                          00  INPT
207560     ADD 001 TO ININDEX.                                          00  INPT
207570     MOVE INIT-FIELD TO IN-ITIAL (ININDEX).                       00  INPT
207580     GO TO B51-READ-A-CARD.                                       00  INPT
207590                                                                  00  INPT
207600 B59-FIRST-NON-INIT.                                              00  INPT
207610     IF CTLCOL EQUAL TO "*"                                       00  INPT
207620         GO TO B51-READ-A-CARD. NOTE JUST A COMMENT.              00  INPT
207630     ALTER B55-INITIALIZ-SW                                       00  INPT
207640         TO PROCEED TO B60-PROCESS-NONINIT-CARDS.                 00  INPT
207650                                                                  00  INPT
207660 B60-PROCESS-NONINIT-CARDS.                                       00  INPT
207670     IF IDENT EQUAL TO "TBL E"                                    00  INPT
207680         GO TO B62-HAVE-TEND-CARD.                                00  INPT
207690     IF IDENT EQUAL TO "TEND."                                    00  INPT
207700         GO TO B62-HAVE-TEND-CARD.                                00  INPT
207710     IF IDENT EQUAL TO "TEND "                                    00  INPT
207720         GO TO B62-HAVE-TEND-CARD.                                00  INPT
207730     GO TO B64-WHAT-KIND-OF-TBL-CARD.                             00  INPT
207740                                                                  00  INPT
207750 B62-HAVE-TEND-CARD.                                              00  INPT
207760     MOVE "2" TO RESULT.                                          00  INPT
207770     MOVE "1" TO DOPT-IN-RPL-SW.                                  00  INPT
207780     MOVE SEQNUM-X TO SEQUENCE-NO-X.  NOTE TABLE END CARD.        00  INPT
207790     MOVE "TEND" TO IDENT.                                        00  INPT
207800     GO TO B90-READ-TBLBOD-EXIT.                                  00  INPT
207810                                                                  00  INPT
207820 B64-WHAT-KIND-OF-TBL-CARD.                                       00  INPT
207830     IF CTLCOL EQUAL TO "*"                                       00  INPT
207840         GO TO B51-READ-A-CARD.  NOTE COMMENT.                    00  INPT
207850     IF CODN-SECT-17 EQUAL TO "CONDITION SECTION"                 00  INPT
207860         GO TO B51-READ-A-CARD.                                   00  INPT
207870     IF ACTN-SECT-14 EQUAL TO "ACTION SECTION"                    00  INPT
207880         GO TO B51-READ-A-CARD.                                   00  INPT
207890     IF COLM-9 NOT EQUAL TO SPACE                                 00  INPT
207900         GO TO B72-INVALID-TABLE-CARD.                            00  INPT
207910     IF CTLCOL EQUAL TO "C"                                       00  INPT
207920         MOVE CTLCOL TO TYPEID                                    00  INPT
207930         GO TO B74-PROCESS-C-A-OR-NEWF.                           00  INPT
207940     IF CTLCOL EQUAL TO "A"                                       00  INPT
207950         MOVE CTLCOL TO TYPEID                                    00  INPT
207960         GO TO B74-PROCESS-C-A-OR-NEWF.                           00  INPT
207970     IF CTLCOL NOT EQUAL TO "F"                                   00  INPT
207980         GO TO B72-INVALID-TABLE-CARD.                            00  INPT
207990     IF OPTION-OPTIMIZ NOT EQUAL TO "R"                           00  INPT
208000         GO TO B72-INVALID-TABLE-CARD.     NOTE FREQUENCY CARD    00  INPT
208010                                  VALID ONLY WHEN OPTIMIZING      00  INPT
208020                                  RUN TIME.                       00  INPT
208030                                                                  00  INPT
208040 B66-PROCESS-FREQ-CARD.                                           00  INPT
208050     MOVE ZERO TO CNTR-7.                                         00  INPT
208060     EXAMINE ROW-CARD TALLYING UNTIL FIRST ",".                   00  INPT
208065     MOVE TALLY TO TALLX.                                         PDP-10
208070     IF TALLX GREATER THAN 59                                     PDP-10
208080         MOVE CTLCOL TO TYPEID                                    00  INPT
208085         GO TO B74-PROCESS-C-A-OR-NEWF.                           00  INPT
208090                                  NOTE FIXED FORMAT FREQ CARD.    00  INPT
208100     MOVE ROW-CARD TO SCAN-FIELD.                                 00  INPT
208110                                                                  00  INPT
208120 B67-FIND-NEXT-FREQ-FIGURE.                                       00  INPT
208130     ADD 001 TO CNTR-7.                                           00  INPT
208140     PERFORM D90-LEFT-JUSTIFY THRU D93-LEFT-JUSTIFY-EXIT.         00  INPT
208142                                                                  00  INPT
208145 B67A-PERFORM-FOR-FREQ.                                           00  INPT
208150     IF ELEMENT (1) EQUAL TO ","                                  00  INPT
208160         PERFORM D98-SHIFT-RIGHT 2 TIMES                          00  INPT
208170         GO TO B70-CHECK-FREQ-FIGURE.                             00  INPT
208180     IF ELEMENT (2) EQUAL TO SPACE                                00  INPT
208190         GO TO B68-SHIFT-FREQR.                                   00  INPT
208200     IF ELEMENT (2) EQUAL TO ","                                  00  INPT
208210         GO TO B68-SHIFT-FREQR.                                   00  INPT
208220     GO TO B69-CK-ELEM3.                                          00  INPT
208230                                                                  00  INPT
208240 B68-SHIFT-FREQR.                                                 00  INPT
208250     PERFORM D98-SHIFT-RIGHT.                                     00  INPT
208260     MOVE ZERO TO ELEMENT (1).                                    00  INPT
208270                                                                  00  INPT
208280 B69-CK-ELEM3.                                                    00  INPT
208290     IF ELEMENT (3) EQUAL TO SPACE                                00  INPT
208300         GO TO B70-CHECK-FREQ-FIGURE.                             00  INPT
208310     IF ELEMENT (3) EQUAL TO ","                                  00  INPT
208320         GO TO B70-CHECK-FREQ-FIGURE.                             00  INPT
208330     PERFORM D95-SHIFT-LEFT.                                      00  INPT
208340     GO TO B69-CK-ELEM3.                                          00  INPT
208350                                                                  00  INPT
208360 B70-CHECK-FREQ-FIGURE.                                           00  INPT
208370     IF NUMBR NOT NUMERIC                                         00  INPT
208380         MOVE 01 TO NUM-BER.                                      00  INPT
208390     IF NUMBR EQUAL TO ZERO                                       00  INPT
208400         MOVE 01 TO NUM-BER.                                      00  INPT
208410     MOVE NUM-BER TO FREQ (CNTR-7).                               00  INPT
208420     MOVE SPACE TO ELEMENT (3).                                   00  INPT
208430     MOVE SPACE TO ELEMENT (2).                                   00  INPT
208440     MOVE SPACE TO ELEMENT (1).                                   00  INPT
208442                                                                  00  INPT
208445 B70A-PFF-END.                                                    00  INPT
208450     IF CNTR-7 LESS THAN NRULES                                   00  INPT
208460         GO TO B67-FIND-NEXT-FREQ-FIGURE.                         00  INPT
208470     GO TO B51-READ-A-CARD.                                       00  INPT
208480                                                                  00  INPT
208490 B72-INVALID-TABLE-CARD.                                          00  INPT
208500     MOVE WARNING TO TYPEMSG.                                     00  INPT
208510     MOVE READERROR TO ERROR-PRINT.                               00  INPT
208520     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
208530     GO TO B51-READ-A-CARD.                                       00  INPT
208540                                                                  00  INPT
208550 B74-PROCESS-C-A-OR-NEWF.                                         00  INPT
208560     MOVE SPACE TO SCAN-FIELD.                                    00  INPT
208570     MOVE 001 TO CNTR-3.                                          00  INPT
208580                                                                  00  INPT
208590 B75-LOOP-TO-ACCUM-STUB.                                          00  INPT
208600     MOVE CRDCOL (CNTR-3) TO ELEMENT (CNTR-3).                    00  INPT
208610     ADD 001 TO CNTR-3.                                           00  INPT
208620     IF CNTR-3 GREATER THAN  REXTENSIONS (EXTNS)                  00  INPT
208630         GO TO B76-HAVE-STUB.                                     00  INPT
208640     GO TO B75-LOOP-TO-ACCUM-STUB.                                00  INPT
208650                                                                  00  INPT
208660 B76-HAVE-STUB.                                                   00  INPT
208670     PERFORM D90-LEFT-JUSTIFY THRU D93-LEFT-JUSTIFY-EXIT.         00  INPT
208680     ADD 001 TO STUBINDEX.                                        00  INPT
208690     IF STUBINDEX GREATER THAN 005                                00  INPT
208700         GO TO B72-INVALID-TABLE-CARD. NOTE MORE THAN 5           00  INPT
208710                                         STUB CARDS.              00  INPT
208720     MOVE SCAN-FIELD TO STUBIES (STUBINDEX).  NOTE STORE STUB.    00  INPT
208730                                                                  00  INPT
208740 B78-PREPARE-TO-SAVE-ENTRIES.                                     00  INPT
208745     MOVE REXTENSIONS (EXTNS) TO CNTR-7.                          00  INPT
208746     ADD 001 TO CNTR-7.                                           00  INPT
208750     IF EXTNS NOT EQUAL TO 001                                    00  INPT
208760         GO TO B80-SAVE-ENTRY-PORTION.                            00  INPT
208770     IF NO-ENT-ON-STUB EQUAL TO "1"                               00  INPT
208780         GO TO B85-CHECK-FOR-CONTINUATIONS.                       00  INPT
208790                                                                  00  INPT
208800 B80-SAVE-ENTRY-PORTION.                                          00  INPT
208810     MOVE CRDCOL (CNTR-7) TO STR-ING (CNTR-6).                    00  INPT
208820     ADD 001 TO CNTR-6.                                           00  INPT
208830     IF CNTR-7 GREATER THAN 062                                   00  INPT
208840         GO TO B85-CHECK-FOR-CONTINUATIONS.                       00  INPT
208850     ADD 001 TO CNTR-7.                                           00  INPT
208860     GO TO B80-SAVE-ENTRY-PORTION.                                00  INPT
208870                                                                  00  INPT
208880 B85-CHECK-FOR-CONTINUATIONS.                                     00  INPT
208890     PERFORM A85-READ-INPUT-FILE THRU B45-READ-INPUT-FILE-EXIT.   00  INPT
208900     IF IDENT EQUAL TO "TBL E"                                    00  INPT
208910         GO TO B87-NOT-CONTIN-SET-NEXT.                           00  INPT
208920     IF IDENT EQUAL TO "TEND."                                    00  INPT
208930         GO TO B87-NOT-CONTIN-SET-NEXT.                           00  INPT
208940     IF IDENT EQUAL TO "TEND "                                    00  INPT
208950         GO TO B87-NOT-CONTIN-SET-NEXT.                           00  INPT
208960     IF CODN-SECT-17 EQUAL TO "CONDITION SECTION"                 00  INPT
208970         GO TO B85-CHECK-FOR-CONTINUATIONS.                       00  INPT
208980     IF ACTN-SECT-14 EQUAL TO "ACTION SECTION"                    00  INPT
208990         GO TO B85-CHECK-FOR-CONTINUATIONS.                       00  INPT
209000     IF CTLCOL EQUAL TO "*"                                       00  INPT
209010         GO TO B85-CHECK-FOR-CONTINUATIONS.                       00  INPT
209020     IF COLM-9 NOT EQUAL TO SPACE                                 00  INPT
209030         GO TO B72-INVALID-TABLE-CARD.                            00  INPT
209040     IF CTLCOL EQUAL TO SPACE                                     00  INPT
209050         MOVE 001 TO CNTR-6                                       00  INPT
209060         GO TO B74-PROCESS-C-A-OR-NEWF.                           00  INPT
209070     IF CTLCOL NOT EQUAL TO "R"                                   00  INPT
209080         GO TO B87-NOT-CONTIN-SET-NEXT.                           00  INPT
209090     ADD 001 TO EXTNS.                                            00  INPT
209100     IF EXTNS GREATER THAN EXTINDEX                               00  INPT
209110         PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT          00  INPT
209120         GO TO B85-CHECK-FOR-CONTINUATIONS.                       00  INPT
209130     IF EXTNS EQUAL TO 006                                        00  INPT
209140         GO TO B72-INVALID-TABLE-CARD.                            00  INPT
209150     GO TO B78-PREPARE-TO-SAVE-ENTRIES.                           00  INPT
209155                                                                  00  INPT
209160 B87-NOT-CONTIN-SET-NEXT.                                         00  INPT
209165     MOVE "1" TO NEXT-IND.                                        00  INPT
209170                                                                  00  INPT
209175 B90-READ-TBLBOD-EXIT.                                            00  INPT
209180     EXIT.                                                        00  INPT
209185                                                                  00  INPT
209190                                                                  00  INPT
209195                                                                  00  INPT
209200                                                                  00  INPT
209208 C00-EDIT-AND-CONVERT-TO-LE.                                      00  INPT
209210     IF TYPEID EQUAL TO "F"                                       00  INPT
209215         GO TO E50-EDIT-CONVERT-EXIT.                             00  INPT
209220     MOVE ZERO TO CNTR-1.                                         00  INPT
209225     MOVE ZERO TO CNTR-2.                                         00  INPT
209230     MOVE ZERO TO CNTR-3.                                         00  INPT
209235     MOVE ZERO TO LEFT-1.                                         00  INPT
209240     MOVE ZERO TO LEFT-2.                                         00  INPT
209245     MOVE ZERO TO DOIND.                                          00  INPT
209250                                                                  00  INPT
209255 C02-GO-TO-SWITCH.                                                00  INPT
209260     GO TO C04-ACTION-OR-CONDIT.                                  00  INPT
209265                                                                  00  INPT
209270 C04-ACTION-OR-CONDIT.                                            00  INPT
209275     MOVE ZERO TO CNTR-1.                                         00  INPT
209280     IF TYPEID EQUAL TO "A"                                       00  INPT
209285         GO TO D50-PROCESS-ACTION-ROW.                            00  INPT
209290     MOVE ZERO TO BND-FIRST-TIME.                                 00  INPT
209295     MOVE ZERO TO TEMP-SW-VECTOR.                                 00  INPT
209300     IF ELSE-IND EQUAL TO "1"                                     00  INPT
209305         SUBTRACT 001 FROM SHELFINDEX. NOTE DONT WORRY ABOUT      00  INPT
209310                                        ENTRY FOR ELSE RULE.      00  INPT
209315     IF AROW EQUAL TO ZERO                                        00  INPT
209320         GO TO C06-PROCESS-CONDITION-ROW.                         00  INPT
209325     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
209330     MOVE "ACTION BEFORE CONDITION" TO ERROR-PRINT.               00  INPT
209335     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
209340     GO TO A20-INCR-TABLE-NO.                                     00  INPT
209345                                                                  00  INPT
209350 C06-PROCESS-CONDITION-ROW.                                       00  INPT
209355     ADD 001 TO CROW.                                             00  INPT
209360     IF CROW NOT GREATER THAN ROWSIZE                             00  INPT
209365         GO TO C07-HAVE-ROOM-FOR-CONDIT.                          00  INPT
209370     PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT.             00  INPT
209375     GO TO A20-INCR-TABLE-NO.                                     00  INPT
209380                                                                  00  INPT
209390 C07-HAVE-ROOM-FOR-CONDIT.                                        00  INPT
209400     ADD 001 TO PSCOND.                                           00  INPT
209410                                                                  00  INPT
209420 C08-SEARCH-STUB-FOR-COLON-OP.                                    00  INPT
209430     MOVE ZERO TO LEFT-1.                                         00  INPT
209440     MOVE ZERO TO LEFT-2.                                         00  INPT
209450                                                                  00  INPT
209460 C09-COLON-OP-SEARCH-LOOP.                                        00  INPT
209470     ADD 001 TO LEFT-1.                                           00  INPT
209480     IF LEFT-1 GREATER THAN STUBINDEX                             00  INPT
209490         GO TO C10-DONE-WITH-COLON-SEARCH.                        00  INPT
209500     EXAMINE STUBIES (LEFT-1) TALLYING UNTIL FIRST ":".           PDP-10
209510                        NOTE COLON IN QUOTES-CHANGE FOR HN CHAIN. 00  INPT
209515     MOVE TALLY TO TALLX.                                         PDP-10
209520     IF TALLX EQUAL TO 60                                         PDP-10
209530         GO TO C09-COLON-OP-SEARCH-LOOP.                          00  INPT
209540     MOVE TALLX TO LEFT-2.                                        PDP-10
209550                                                                  00  INPT
209560 C10-DONE-WITH-COLON-SEARCH.                                      00  INPT
209570     IF LEFT-2 EQUAL TO ZERO                                      00  INPT
209580         GO TO C12-ANALYZE-ENTRY.                                 00  INPT
209590     MOVE LEFT-1 TO CNTR-8.                 NOTE HAVE COLON       00  INPT
209600                                  OPERATOR IN STUB.               00  INPT
209610     ADD 001 TO LEFT-2.                                           00  INPT
209620     MOVE LEFT-2 TO CNTR-9.                                       00  INPT
209630                           NOTE CNTR-8 HAS INDEX FOR STUBIE       00  INPT
209640                             CONTAINING COLON                     00  INPT
209650                                CNTR-9 HAS POSITION OF COLON      00  INPT
209660                             WITHIN THAT STUBIE.                  00  INPT
209670     GO TO C85-EXPAND-COLON-OP-STUB.                              00  INPT
209680                                                                  00  INPT
209690 C12-ANALYZE-ENTRY.                                               00  INPT
209700     ADD 001 TO CNTR-1.                                           00  INPT
209710     IF CNTR-1 GREATER THAN SHELFINDEX                            00  INPT
209720         MOVE 001 TO CNTR-1                                       00  INPT
209730         GO TO C14-ITS-A-LIMITED-ENTRY-ROW.                       00  INPT
209740                        NOTE SHELFINDEX HAS NUMBER OF RULES       00  INPT
209750                           EXCLUDING ELSE.                        00  INPT
209760     MOVE SHELF (CNTR-1) TO SCAN-FIELD.  NOTE THAT IS THE ENTRY.  00  INPT
209770     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
209780         GO TO C12-ANALYZE-ENTRY.        NOTE ENTRY HAS BEEN      00  INPT
209790                                           LEFT-JUSTIFIED.        00  INPT
209800     MOVE SCAN-FIELD TO STUB6.                                    00  INPT
209810     MOVE "1" TO DOIND.                                           00  INPT
209820     MOVE ZERO TO CNTR-3.                                         00  INPT
209830     PERFORM C98-CK-FOR-LIMITED-ENTRY THRU D01-LE-CK-EXIT.        00  INPT
209840     IF SCAN-FIELD EQUAL TO "NOT"                                 00  INPT
209850         GO TO C12-ANALYZE-ENTRY.   NOTE ISNT CONSIDERED EXTENDED.00  INPT
209860     IF CNTR-3 EQUAL TO 001                                       00  INPT
209870         GO TO C20-ITS-AN-EXT-ENTRY-ROW.                          00  INPT
209880                            NOTE FALL THRU FOR LIMITED ENTRY.     00  INPT
209890     IF ELEMENT (1) EQUAL TO "*"                                  00  INPT
209900         GO TO C12-ANALYZE-ENTRY.                                 00  INPT
209910     IF ELEMENT (1) EQUAL TO "$"                                  00  INPT
209920         GO TO C12-ANALYZE-ENTRY.                                 00  INPT
209930                            NOTE HAVE Y OR N.                     00  INPT
209940     MOVE ZERO TO CNTR-1.                                         00  INPT
209950                                                                  00  INPT
209960 C14-ITS-A-LIMITED-ENTRY-ROW.                                     00  INPT
209965     MOVE SPACE TO STUB6.                     NOTE IPAR D23.      00  INPT
209970     ADD 001 TO CNTR-1.                                           00  INPT
209980     IF CNTR-1 GREATER THAN SHELFINDEX                            00  INPT
209990         GO TO D28-DONE-WITH-CONDIT-ROW.                          00  INPT
210000     MOVE SHELF (CNTR-1) TO SCAN-FIELD.                           00  INPT
210010     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
210020         GO TO C14-ITS-A-LIMITED-ENTRY-ROW.                       00  INPT
210030     MOVE ZERO TO CNTR-3.                                         00  INPT
210040     PERFORM C98-CK-FOR-LIMITED-ENTRY THRU D01-LE-CK-EXIT.        00  INPT
210050     IF CNTR-3 NOT EQUAL TO 002                                   00  INPT
210060         GO TO C15-INVALID-ROW-ENTRY.                             00  INPT
210070     MOVE ELEMENT (1) TO CMATRIX (CROW, CNTR-1).                  00  INPT
210080     GO TO C14-ITS-A-LIMITED-ENTRY-ROW.                           00  INPT
210090                                                                  00  INPT
210100 C15-INVALID-ROW-ENTRY.                                           00  INPT
210110     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
210130     MOVE ROWENERROR TO ERROR-PRINT.                              00  INPT
210140     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
210150     GO TO A20-INCR-TABLE-NO.                                     00  INPT
210160                                                                  00  INPT
210170 C20-ITS-AN-EXT-ENTRY-ROW.                                        00  INPT
210180     IF OM-BND EQUAL TO "NO"                                      00  INPT
210190         GO TO C68-NO-SPECIAL-OP.                                 00  INPT
210200     IF BND-FIRST-TIME EQUAL TO "1"                               00  INPT
210210         GO TO C70-MOVE-STUB.                                     00  INPT
210220     MOVE REXTENSIONS (1) TO SUBX.                                00  INPT
210230     ADD 001 TO SUBX.                                             00  INPT
210240                                                                  00  INPT
210250 C22-ISOL-END-OF-STUB.                                            00  INPT
210260     MOVE STUB-1-BND-COL (SUBX) TO TEMP-TEST.                     00  INPT
210270                                                                  00  INPT
210280 C23-HAVE-END-OF-STUB.                                            00  INPT
210290     IF TEMP-TEST EQUAL TO SPACE                                  00  INPT
210300         GO TO C25-NOT-AT-END.                                    00  INPT
210310     IF TEMP-TEST EQUAL TO "T"                                    00  INPT
210320         GO TO C27-PROC-BOUNDS-THAN.                              00  INPT
210330     IF TEMP-TEST EQUAL TO "E"                                    00  INPT
210340         GO TO C33-PROC-BOUNDS-EQ.                                00  INPT
210350     GO TO C68-NO-SPECIAL-OP.                                     00  INPT
210360                                                                  00  INPT
210370 C25-NOT-AT-END.                                                  00  INPT
210380     SUBTRACT 001 FROM SUBX.                                      00  INPT
210390     IF SUBX GREATER THAN 003                                     00  INPT
210400         GO TO C22-ISOL-END-OF-STUB.                              00  INPT
210410     GO TO C68-NO-SPECIAL-OP.                                     00  INPT
210420                                                                  00  INPT
210430 C27-PROC-BOUNDS-THAN.                                            00  INPT
210440     SUBTRACT 001 FROM SUBX.                                      00  INPT
210450     PERFORM C22-ISOL-END-OF-STUB.                                00  INPT
210460     IF TEMP-TEST EQUAL TO "L"                                    00  INPT
210470         GO TO C31-PROC-BOUNDS-LT.                                00  INPT
210480     IF TEMP-TEST NOT EQUAL TO "G"                                00  INPT
210490         GO TO C68-NO-SPECIAL-OP.                                 00  INPT
210500                                                                  00  INPT
210510 C29-PROC-BOUNDS-GT.                                              00  INPT
210520     MOVE ">" TO TEMP-PLUG.   NOTE GREATER-THAN SYMBOL.           00  INPT
210530     MOVE OP-SET-G TO OP-SET.                                     00  INPT
210540     GO TO C42-PROC-BOUNDS-PLUG-OP.                               00  INPT
210550                                                                  00  INPT
210560 C31-PROC-BOUNDS-LT.                                              00  INPT
210570     MOVE "<" TO TEMP-PLUG.   NOTE LESS-THAN SYMBOL.              00  INPT
210580     MOVE OP-SET-L TO OP-SET.                                     00  INPT
210590     GO TO C42-PROC-BOUNDS-PLUG-OP.                               00  INPT
210600                                                                  00  INPT
210610 C33-PROC-BOUNDS-EQ.                                              00  INPT
210620     SUBTRACT 001 FROM SUBX.                                      00  INPT
210630     PERFORM C22-ISOL-END-OF-STUB.                                00  INPT
210640     IF TEMP-TEST EQUAL TO "L"                                    00  INPT
210650         GO TO C35-PROC-BOUNDS-LE.                                00  INPT
210660     IF TEMP-TEST EQUAL TO "N"                                    00  INPT
210670         GO TO C37-PROC-BOUNDS-NE.                                00  INPT
210680     IF TEMP-TEST EQUAL TO "G"                                    00  INPT
210690         GO TO C39-PROC-BOUNDS-GE.                                00  INPT
210700     GO TO C68-NO-SPECIAL-OP.                                     00  INPT
210710                                                                  00  INPT
210720 C35-PROC-BOUNDS-LE.                                              00  INPT
210730     MOVE ">" TO TEMP-PLUG.       NOTE GREATER-THAN SYMBOL.       00  INPT
210740     MOVE OP-SET-LE TO OP-SET.                                    00  INPT
210750     GO TO C42-PROC-BOUNDS-PLUG-OP.                               00  INPT
210760                                                                  00  INPT
210770 C37-PROC-BOUNDS-NE.                                              00  INPT
210780     MOVE "=" TO TEMP-PLUG.       NOTE EQUAL SIGN.                00  INPT
210790     MOVE OP-SET-NE TO OP-SET.                                    00  INPT
210800     GO TO C42-PROC-BOUNDS-PLUG-OP.                               00  INPT
210810                                                                  00  INPT
210820 C39-PROC-BOUNDS-GE.                                              00  INPT
210830     MOVE "<" TO TEMP-PLUG.       NOTE LESS-THAN SYMBOL.          00  INPT
210840     MOVE OP-SET-GE TO OP-SET.                                    00  INPT
210850                                                                  00  INPT
210860 C42-PROC-BOUNDS-PLUG-OP.                                         00  INPT
210870     SUBTRACT 001 FROM SUBX.                                      00  INPT
210880     PERFORM C22-ISOL-END-OF-STUB.                                00  INPT
210890     IF TEMP-TEST NOT EQUAL TO SPACE                              00  INPT
210900         GO TO C68-NO-SPECIAL-OP.                                 00  INPT
210910     ADD 001 TO SUBX.                                             00  INPT
210920                                                                  00  INPT
210930 C43-BOUNDS-MOVE-IN-OP.                                           00  INPT
210940     MOVE TEMP-PLUG TO STUB-1-BND-COL (SUBX).                     00  INPT
210950                                                                  00  INPT
210960 C44-BOUNDS-MOVE-IN-SPACE.                                        00  INPT
210970     MOVE TEMP-PLUG TO TEMP-TEST.                                 00  INPT
210980     MOVE SPACE TO TEMP-PLUG.                                     00  INPT
210990     ADD 001 TO SUBX.                                             00  INPT
211000     IF SUBX LESS THAN 061                                        00  INPT
211010         PERFORM C43-BOUNDS-MOVE-IN-OP.                           00  INPT
211020                                                                  00  INPT
211030 C45-HAVE-STUB-COMPLETE.                                          00  INPT
211040     IF TEMP-TEST EQUAL TO SPACE                                  00  INPT
211050         GO TO C70-MOVE-STUB.                                     00  INPT
211060     IF BND-FIRST-TIME NOT EQUAL TO ZERO                          00  INPT
211070         GO TO C70-MOVE-STUB.                                     00  INPT
211080     MOVE SHELFINDEX TO CNTR-2.                                   00  INPT
211090                                                                  00  INPT
211100 C47-NORMALIZE-FOR-BND-LOOP.                                      00  INPT
211110     MOVE SHELF (CNTR-2) TO SCAN-FIELD.                           00  INPT
211120     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
211130         GO TO C52-DECR-LOOP-SUB.                                 00  INPT
211140     IF SCAN-1-THRU-3 EQUAL TO "NOT"                              00  INPT
211150         GO TO C52-DECR-LOOP-SUB.                                 00  INPT
211155     IF ELEMENT (1) EQUAL TO QUOTE                                00  INPT
211160         MOVE 001 TO CNTR-2                                       00  INPT
211170         GO TO C52-DECR-LOOP-SUB.                                 00  INPT
211180     IF ELEMENT (1) EQUAL TO "+"                                  00  INPT
211190         MOVE ZERO TO ELEMENT (1).                                00  INPT
211200                                                                  00  INPT
211210 C49-CK-THIS-ENTRY.                                               00  INPT
211220     IF ELEMENT (19) EQUAL TO SPACE                               00  INPT
211230         PERFORM D98-SHIFT-RIGHT                                  00  INPT
211240         MOVE ZERO TO ELEMENT (1)                                 00  INPT
211250         GO TO C49-CK-THIS-ENTRY.                                 00  INPT
211260     PERFORM D95-SHIFT-LEFT.                                      00  INPT
211270     IF SCAN-FIELD-1-THRU-18 NUMERIC                              00  INPT
211280         GO TO C50-REPLACE-ENTRY.                                 00  INPT
211290     MOVE CNTR-2 TO XR-B.                                         00  INPT
211300     PERFORM D18-POSSIBLE THRU D20-BOUNDED-ERROR.                 00  INPT
211310                                                                  00  INPT
211320 C50-REPLACE-ENTRY.                                               00  INPT
211330     MOVE SCAN-FIELD TO SHELF (CNTR-2).                           00  INPT
211340                                                                  00  INPT
211350 C52-DECR-LOOP-SUB.                                               00  INPT
211360     SUBTRACT 001 FROM CNTR-2.                                    00  INPT
211370     IF CNTR-2 GREATER THAN ZERO                                  00  INPT
211380         GO TO C47-NORMALIZE-FOR-BND-LOOP.                        00  INPT
211390     MOVE "1" TO BND-FIRST-TIME.                                  00  INPT
211400                                                                  00  INPT
211410 C54-CHECK-REST-OF-ENTRIES.                                       00  INPT
211420     IF GEN-SW (CNTR-1) EQUAL TO SPACE                            00  INPT
211430         GO TO C56-INCR-CNTR-1.                                   00  INPT
211440     MOVE SHELF (CNTR-1) TO SCAN-FIELD.                           00  INPT
211450     IF SCAN-4-ON NOT EQUAL TO SPACE                              00  INPT
211460         GO TO C58-EXT-ENTRY.                                     00  INPT
211470     IF SCAN-1-THRU-3 EQUAL TO SPACE                              00  INPT
211480         GO TO C56-INCR-CNTR-1.                                   00  INPT
211490     IF SCAN-1-THRU-3 EQUAL TO "*  "                              00  INPT
211500         GO TO C56-INCR-CNTR-1.                                   00  INPT
211510     IF SCAN-1-THRU-3 EQUAL TO "$  "                              00  INPT
211520         GO TO C56-INCR-CNTR-1.                                   00  INPT
211530     IF SCAN-1-THRU-3 EQUAL TO "NOT"                              00  INPT
211540         GO TO C56-INCR-CNTR-1.                                   00  INPT
211550     GO TO C58-EXT-ENTRY.                                         00  INPT
211560                                                                  00  INPT
211570 C56-INCR-CNTR-1.                                                 00  INPT
211580     IF CROW GREATER THAN ROWSIZE                                 00  INPT
211590         PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT          00  INPT
211600         GO TO A20-INCR-TABLE-NO.                                 00  INPT
211610     ADD 001 TO CNTR-1.                                           00  INPT
211620     IF CNTR-1 NOT GREATER THAN SHELFINDEX                        00  INPT
211630         GO TO C54-CHECK-REST-OF-ENTRIES.                         00  INPT
211640     SUBTRACT 001 FROM CROW.                                      00  INPT
211650     GO TO E48-PUT-OUT-LAST-STUB.                                 00  INPT
211660                                                                  00  INPT
211670 C58-EXT-ENTRY.                                                   00  INPT
211680     IF TEMP-TEST EQUAL TO SPACE                                  00  INPT
211690         GO TO C64-DO-LIMIT-TEST.                                 00  INPT
211700                                                                  00  INPT
211710 C60-DENORMALIZE-BNDS.                                            00  INPT
211720     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
211730         GO TO C64-DO-LIMIT-TEST.                                 00  INPT
211740                                                                  00  INPT
211750 C61-DENORMAL-SHIFT.                                              00  INPT
211760     IF ELEMENT (1) EQUAL TO ZERO                                 00  INPT
211770         PERFORM D95-SHIFT-LEFT                                   00  INPT
211780         GO TO C61-DENORMAL-SHIFT.                                00  INPT
211790                                                                  00  INPT
211800 C62-HAVE-SHIFTED.                                                00  INPT
211810     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
211820         MOVE ZERO TO ELEMENT (1).                                00  INPT
211830                                                                  00  INPT
211840 C64-DO-LIMIT-TEST.                                               00  INPT
211850     MOVE SCAN-FIELD TO STUB6.                                    00  INPT
211860     MOVE ZERO TO CNTR-3.                                         00  INPT
211870     PERFORM C98-CK-FOR-LIMITED-ENTRY THRU D01-LE-CK-EXIT.        00  INPT
211880     IF CNTR-3 EQUAL TO 002                                       00  INPT
211890         GO TO C15-INVALID-ROW-ENTRY.                             00  INPT
211900     IF TEMP-TEST NOT EQUAL TO SPACE                              00  INPT
211910         MOVE SHELF (CNTR-1) TO SCAN-FIELD.                       00  INPT
211920     GO TO C20-ITS-AN-EXT-ENTRY-ROW.                              00  INPT
211930                                                                  00  INPT
211940                                                                  00  INPT
211950 C68-NO-SPECIAL-OP.                                               00  INPT
211960     MOVE OP-SET-STD TO OP-SET.                                   00  INPT
211970     MOVE SPACE TO TEMP-TEST.                                     00  INPT
211980                                                                  00  INPT
211990 C70-MOVE-STUB.                                                   00  INPT
212000     PERFORM E00-STUB-MOVE THRU E35-STUB-MOVE-EXIT.               00  INPT
212010     MOVE ZERO TO CNTR-2.                                         00  INPT
212020     MOVE "1" TO BND-FIRST-TIME.                                  00  INPT
212030                                                                  00  INPT
212040 C72-EXPAND-EXT-ENTRY.                                            00  INPT
212050     ADD 001 TO CNTR-2.                                           00  INPT
212060     IF CNTR-2 GREATER THAN SHELFINDEX                            00  INPT
212070         PERFORM D05-CCONV-COMBINER THRU D25-CCONV-COMBINER-EXIT  00  INPT
212080         ADD 001 TO CROW                                          00  INPT
212090         GO TO C56-INCR-CNTR-1.                                   00  INPT
212100     MOVE SHELF (CNTR-2) TO TEMP-FIELD.                           00  INPT
212110     IF TEMP-FIELD-1 EQUAL TO SPACE                               00  INPT
212120         GO TO C72-EXPAND-EXT-ENTRY.                              00  INPT
212130     IF TEMP-FIELD EQUAL TO "NOT"                                 00  INPT
212140         GO TO C78-EXPAND-NOT.                                    00  INPT
212150     IF TEMP-FIELD-1-3 EQUAL TO "*"                               00  INPT
212160         GO TO C74-AST-DOL.                                       00  INPT
212170     IF TEMP-FIELD-1-3 EQUAL TO "$"                               00  INPT
212180         GO TO C74-AST-DOL.                                       00  INPT
212190     GO TO C76-SETUP-OPS.                                         00  INPT
212200                                                                  00  INPT
212210 C74-AST-DOL.                                                     00  INPT
212220     IF TEMP-FIELD-4-ON EQUAL TO SPACE                            00  INPT
212230         MOVE TEMP-FIELD-1 TO GLE-SW-L                            00  INPT
212240         GO TO C82-PLUG-C-MATRIX.                                 00  INPT
212250                                                                  00  INPT
212260 C76-SETUP-OPS.                                                   00  INPT
212270     IF TEMP-FIELD NOT EQUAL TO SCAN-FIELD                        00  INPT
212280         GO TO C80-G-OR-L.                                        00  INPT
212290     MOVE OP-EQ TO GLE-SW-L.                                      00  INPT
212300     MOVE SPACE TO GEN-SW (CNTR-2).                               00  INPT
212310     GO TO C82-PLUG-C-MATRIX.                                     00  INPT
212320                                                                  00  INPT
212330 C78-EXPAND-NOT.                                                  00  INPT
212340     IF OP-EQ EQUAL TO "Y"                                        00  INPT
212350         MOVE "N" TO GLE-SW-L                                     00  INPT
212360             ELSE MOVE "Y" TO GLE-SW-L.                           00  INPT
212370     GO TO C82-PLUG-C-MATRIX.                                     00  INPT
212380                                                                  00  INPT
212390 C80-G-OR-L.                                                      00  INPT
212400     IF SCAN-FIELD GREATER THAN TEMP-FIELD                        00  INPT
212410         MOVE OP-GT TO GLE-SW-L                                   00  INPT
212420             ELSE MOVE OP-LT TO GLE-SW-L.                         00  INPT
212430                                                                  00  INPT
212440 C82-PLUG-C-MATRIX.                                               00  INPT
212450     MOVE GLE-SW-L TO CMATRIX (CROW, CNTR-2).                     00  INPT
212460     GO TO C72-EXPAND-EXT-ENTRY.                                  00  INPT
212470                                                                  00  INPT
212480         NOTE *** C85 THRU C96 HANDLES COLON OPERATOR ***.        00  INPT
212490                                                                  00  INPT
212500 C85-EXPAND-COLON-OP-STUB.                                        00  INPT
212510     MOVE STUBIES (CNTR-8) TO SCAN-FIELD.  NOTE STUB WITH COLON.  00  INPT
212520     MOVE SPACE TO STUB6.                                         00  INPT
212530     MOVE ZERO TO LEFT-1.                                         00  INPT
212540     MOVE SPACE TO GLE-SWS.                                       00  INPT
212550                                                                  00  INPT
212560 C87-LOOP-TO-CK-COLON-OP-ENTS.                                    00  INPT
212570     ADD 001 TO LEFT-1.                                           00  INPT
212580     IF LEFT-1 GREATER THAN SHELFINDEX                            00  INPT
212590         GO TO C89-WHAT-KIND-OF-ENTRIES-FOUND.                    00  INPT
212600     MOVE SHELF (LEFT-1) TO TEMP-FIELD.                           00  INPT
212610     IF TEMP-FIELD EQUAL TO SPACE                                 00  INPT
212620         GO TO C87-LOOP-TO-CK-COLON-OP-ENTS.                      00  INPT
212630     IF TEMP-FIELD-4-ON NOT EQUAL TO SPACE                        00  INPT
212640         GO TO C12-ANALYZE-ENTRY. NOTE INVALID COLON ENTRY -      00  INPT
212650                                       IGNORE COLON OPERATOR.     00  INPT
212660     IF TEMP-FIELD-1-3 EQUAL TO "NOT"                             00  INPT
212670         MOVE "$" TO GLE-SW-NOT                                   00  INPT
212680         GO TO C87-LOOP-TO-CK-COLON-OP-ENTS.                      00  INPT
212690     IF TEMP-FIELD-1-3 EQUAL TO "=  "                             00  INPT
212700         GO TO C87-LOOP-TO-CK-COLON-OP-ENTS.                      00  INPT
212710     IF TEMP-FIELD-1-3 EQUAL TO "<"                               00  INPT
212720         MOVE "$" TO GLE-SW-L                                     00  INPT
212730         GO TO C87-LOOP-TO-CK-COLON-OP-ENTS.                      00  INPT
212740     IF TEMP-FIELD-1-3 EQUAL TO ">"                               00  INPT
212750         MOVE "$" TO GLE-SW-G                                     00  INPT
212760         GO TO C87-LOOP-TO-CK-COLON-OP-ENTS.                      00  INPT
212770     GO TO C12-ANALYZE-ENTRY.                                     00  INPT
212780                        NOTE COLON OPERATION HAS WRONG ENTRY SO   00  INPT
212790                             IGNORE COLON OPERATOR.               00  INPT
212800                                                                  00  INPT
212810 C89-WHAT-KIND-OF-ENTRIES-FOUND.                                  00  INPT
212820     IF GLE-SWS NOT EQUAL TO " $$"                                00  INPT
212830         MOVE ZERO TO LEFT-1                                      00  INPT
212840         GO TO C92-LOOP-TO-CREATE-COLON-STUB.                     00  INPT
212850                             NOTE IF YOU FELL THRU THERE ARE NO   00  INPT
212860                               -NOT-ENTRIES SO CONVERT EQUAL      00  INPT
212870                               SIGNS TO NOTS.                     00  INPT
212880     MOVE ZERO TO SUBX.                                           00  INPT
212890                                                                  00  INPT
212900 C90-CONVERT-EQUALS-TO-NOTS.                                      00  INPT
212910     ADD 001 TO SUBX.                                             00  INPT
212920     IF SHELF1 (SUBX) EQUAL TO "="                                00  INPT
212930         MOVE "NOT" TO SHELF1-3 (SUBX).                           00  INPT
212940     IF SUBX LESS THAN SHELFINDEX                                 00  INPT
212950         GO TO C90-CONVERT-EQUALS-TO-NOTS.                        00  INPT
212960     MOVE ZERO TO LEFT-1.                                         00  INPT
212970                                                                  00  INPT
212980 C92-LOOP-TO-CREATE-COLON-STUB.                                   00  INPT
212990     ADD 001 TO LEFT-1.                                           00  INPT
213000     IF LEFT-1 GREATER THAN SHELFINDEX                            00  INPT
213010         SUBTRACT 001 FROM CROW                                   00  INPT
213020         GO TO E50-EDIT-CONVERT-EXIT.                             00  INPT
213030     MOVE SHELF (LEFT-1) TO TEMP-FIELD.                           00  INPT
213040     IF TEMP-FIELD-1   EQUAL TO SPACE                             00  INPT
213050         GO TO C92-LOOP-TO-CREATE-COLON-STUB.                     00  INPT
213060     IF TEMP-FIELD-1   EQUAL TO "*"                               00  INPT
213070         GO TO C92-LOOP-TO-CREATE-COLON-STUB.                     00  INPT
213080     IF TEMP-FIELD-1-3 EQUAL TO "NOT"                             00  INPT
213090         GO TO C92-LOOP-TO-CREATE-COLON-STUB.                     00  INPT
213100     MOVE "1" TO DOIND.                                           00  INPT
213110     MOVE TEMP-FIELD-1 TO ELEMENT (CNTR-9).                       00  INPT
213120     MOVE SCAN-FIELD TO STUBIES (CNTR-8).                         00  INPT
213130     PERFORM E00-STUB-MOVE THRU E35-STUB-MOVE-EXIT.               00  INPT
213140     MOVE ZERO TO LEFT-2.                                         00  INPT
213150                                                                  00  INPT
213160 C94-FIND-ENTRIES-FOR-THIS-STUB.                                  00  INPT
213170     ADD 001 TO LEFT-2.                                           00  INPT
213180     IF LEFT-2 GREATER THAN SHELFINDEX                            00  INPT
213190         ADD 001 TO CROW                                          00  INPT
213200         GO TO C92-LOOP-TO-CREATE-COLON-STUB.                     00  INPT
213210     MOVE SHELF (LEFT-2) TO S-R-GEN.                              00  INPT
213220     IF S-R-SPACE-1 EQUAL TO SPACE                                00  INPT
213230         GO TO C94-FIND-ENTRIES-FOR-THIS-STUB.                    00  INPT
213240     IF S-R-SPACE-1 EQUAL TO TEMP-FIELD-1                         00  INPT
213250         MOVE "Y" TO GLE-SW-L                                     00  INPT
213260         MOVE "*" TO SHELF (LEFT-2)                               00  INPT
213270         GO TO C96-PLUG-CMATRIX.                                  00  INPT
213280     IF S-R-SPACE-3 EQUAL TO "NOT"                                00  INPT
213290         MOVE "N" TO GLE-SW-L                                     00  INPT
213300         GO TO C96-PLUG-CMATRIX.                                  00  INPT
213310     MOVE "*" TO GLE-SW-L.                                        00  INPT
213320                                                                  00  INPT
213330 C96-PLUG-CMATRIX.                                                00  INPT
213340     MOVE GLE-SW-L TO CMATRIX (CROW, LEFT-2).                     00  INPT
213350     GO TO C94-FIND-ENTRIES-FOR-THIS-STUB.                        00  INPT
213360                                                                  00  INPT
213370                                                                  00  INPT
213380                                                                  00  INPT
213390 C98-CK-FOR-LIMITED-ENTRY.                                        00  INPT
213400     IF SCAN-4-ON NOT EQUAL TO SPACE                              00  INPT
213410         GO TO D00-ADD-1.                                         00  INPT
213420     IF SCAN-1-THRU-3 EQUAL TO "Y  "                              00  INPT
213430         GO TO C99-ADD-1.                                         00  INPT
213440     IF SCAN-1-THRU-3 EQUAL TO "N  "                              00  INPT
213450         GO TO C99-ADD-1.                                         00  INPT
213460     IF SCAN-1-THRU-3 EQUAL TO "*  "                              00  INPT
213470         GO TO C99-ADD-1.                                         00  INPT
213480     IF SCAN-1-THRU-3 EQUAL TO "$  "                              00  INPT
213490         GO TO C99-ADD-1.                                         00  INPT
213500     GO TO D00-ADD-1.                                             00  INPT
213510                                                                  00  INPT
213520 C99-ADD-1.                                                       00  INPT
213530     ADD 001 TO CNTR-3.                                           00  INPT
213540                                                                  00  INPT
213550 D00-ADD-1.                                                       00  INPT
213560     ADD 001 TO CNTR-3.                                           00  INPT
213570                                                                  00  INPT
213580 D01-LE-CK-EXIT.                                                  00  INPT
213590     EXIT.         NOTE C98 THRU D01 CHECKS AN INDIVIDUAL ENTRY   00  INPT
213600                    FOR LIMITED CODES -  IN CNTR-3                00  INPT
213610                          IT RETURNS 2 IF IT FOUND A LIMITED ENTRY00  INPT
213620                                     1 IF IT DID NOT.             00  INPT
213630                                                                  00  INPT
213640                                                                  00  INPT
213650                                                                  00  INPT
213660 D05-CCONV-COMBINER.                                              00  INPT
213670     IF OM-BND EQUAL TO "NO"                                      00  INPT
213680         GO TO D25-CCONV-COMBINER-EXIT.                           00  INPT
213690     IF TEMP-TEST EQUAL TO SPACE                                  00  INPT
213700         GO TO D25-CCONV-COMBINER-EXIT.                           00  INPT
213710     MOVE CPTRINDEX TO SUBS.                                      00  INPT
213720     SUBTRACT 001 FROM CSTUBPTR (SUBS) GIVING CNTR-9.             00  INPT
213730     MOVE CROW TO XR-A.                                           00  INPT
213740     MOVE ZERO TO EL-3.                                           00  INPT
213750                                                                  00  INPT
213760 D07-LOOP-TO-FIND-DUPL-STUBS.                                     00  INPT
213770     SUBTRACT 001 FROM SUBS.                                      00  INPT
213780     SUBTRACT 001 FROM XR-A.                                      00  INPT
213790     IF SUBS LESS THAN 001                                        00  INPT
213800         GO TO D25-CCONV-COMBINER-EXIT.                           00  INPT
213810     SUBTRACT 001 FROM CSTUBPTR (SUBS) GIVING SUBX.               00  INPT
213815      IF CNTR-9 < 1 OR SUBX < 1                                    PDP-10
213816          GO TO D07-LOOP-TO-FIND-DUPL-STUBS.                       PDP-10
213820     IF CONDSTK (CNTR-9) NOT EQUAL TO CONDSTK (SUBX)              00  INPT
213830         GO TO D07-LOOP-TO-FIND-DUPL-STUBS.                       00  INPT
213840                                 NOTE YOU"VE FOUND A DUPLICATE    00  INPT
213850                                   STUB, COMBINE CROWS.           00  INPT
213860     SUBTRACT 001 FROM CNTR-9.                                    00  INPT
213870     SUBTRACT 001 FROM SUBX.                                      00  INPT
213875      IF CNTR-9 < 1 OR SUBX < 1                                    PDP-10
213876          ADD 001 TO CNTR-9                                        PDP-10
213877          GO TO D07-LOOP-TO-FIND-DUPL-STUBS.                       PDP-10
213880     IF CONDSTK (CNTR-9) NOT EQUAL TO CONDSTK (SUBX)              00  INPT
213890         ADD 001 TO CNTR-9                                        00  INPT
213900         GO TO D07-LOOP-TO-FIND-DUPL-STUBS.                       00  INPT
213910     ADD 001 TO CNTR-9.                                           00  INPT
213920     ADD 001 TO SUBX.                                             00  INPT
213930     MOVE 001 TO XR-B.                                            00  INPT
213940                                                                  00  INPT
213950 D10-CK-FOR-COMPATIBLE-ENTRIES.                                   00  INPT
213960     MOVE CMATRIX (CROW, XR-B) TO EL-1.                           00  INPT
213970     MOVE CMATRIX (XR-A, XR-B) TO EL-2.                           00  INPT
213980     IF EL-1 EQUAL TO EL-2                                        00  INPT
213990         GO TO D24-INCR-XR-B.                                     00  INPT
214000     IF EL-1 EQUAL TO SPACE                                       00  INPT
214010         MOVE EL-2 TO EL-1                                        00  INPT
214020         GO TO D18-POSSIBLE.                                      00  INPT
214030     IF EL-2 EQUAL TO SPACE                                       00  INPT
214040         GO TO D18-POSSIBLE.                                      00  INPT
214050     IF EL-1 EQUAL TO "N"                                         00  INPT
214060         GO TO D11-1-IS-N.                                        00  INPT
214070     IF EL-1 EQUAL TO "*"                                         00  INPT
214080         GO TO D12-1-IS-STAR.                                     00  INPT
214090     IF EL-1 EQUAL TO "Y"                                         00  INPT
214100         GO TO D13-1-IS-Y.                                        00  INPT
214110     IF EL-1 EQUAL TO "$"                                         00  INPT
214120         GO TO D14-1-IS-DOL.                                      00  INPT
214130     GO TO D16-PROBABLE.                                          00  INPT
214135                                                                  00  INPT
214140 D11-1-IS-N.                                                      00  INPT
214150     IF EL-2 EQUAL TO "*"                                         00  INPT
214160         GO TO D22-PUT-EL-1-IN-CMATRIX.                           00  INPT
214170                                                                  00  INPT
214180 D12-1-IS-STAR.                                                   00  INPT
214190     IF EL-2 EQUAL TO "N"                                         00  INPT
214200         MOVE "N" TO EL-1                                         00  INPT
214210         GO TO D22-PUT-EL-1-IN-CMATRIX.                           00  INPT
214220                                                                  00  INPT
214230 D13-1-IS-Y.                                                      00  INPT
214240     IF EL-2 EQUAL TO "$"                                         00  INPT
214250         GO TO D22-PUT-EL-1-IN-CMATRIX.                           00  INPT
214260                                                                  00  INPT
214270 D14-1-IS-DOL.                                                    00  INPT
214280     IF EL-2 EQUAL TO "Y"                                         00  INPT
214290         MOVE "Y" TO EL-1                                         00  INPT
214300         GO TO D22-PUT-EL-1-IN-CMATRIX.                           00  INPT
214305                                                                  00  INPT
214310 D16-PROBABLE.                                                    00  INPT
214320     MOVE "PROBABLE" TO ERROR-INTRO.                              00  INPT
214330     PERFORM D20-BOUNDED-ERROR.                                   00  INPT
214340     MOVE "1" TO EL-3.                                            00  INPT
214350     GO TO D24-INCR-XR-B.                                         00  INPT
214360                                                                  00  INPT
214370 D18-POSSIBLE.                                                    00  INPT
214380     MOVE "POSSIBLE" TO ERROR-INTRO.                              00  INPT
214390                                                                  00  INPT
214400 D20-BOUNDED-ERROR.                                               00  INPT
214410     MOVE "CONFLICT OF BOUNDED VALUES, RULE" TO ERROR-BODY.       00  INPT
214420     MOVE XR-B TO ERROR-RULE-NUMBER.                              00  INPT
214430     MOVE "." TO ERROR-TRAIL.                                     00  INPT
214440     MOVE WARNING TO TYPEMSG.                                     00  INPT
214450     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
214460                                                                  00  INPT
214470 D22-PUT-EL-1-IN-CMATRIX.                                         00  INPT
214480     MOVE EL-1 TO CMATRIX (XR-A, XR-B).                           00  INPT
214490                                                                  00  INPT
214500 D24-INCR-XR-B.                                                   00  INPT
214510     ADD 001 TO XR-B.                                             00  INPT
214520     IF XR-B NOT GREATER THAN SHELFINDEX                          00  INPT
214530         GO TO D10-CK-FOR-COMPATIBLE-ENTRIES.                     00  INPT
  214540     IF EL-3 EQUAL TO "1"                                         00  INPT
214550         GO TO D25-CCONV-COMBINER-EXIT.                           00  INPT
214560     SUBTRACT 001 FROM CROW.                                      00  INPT
214570     SUBTRACT 001 FROM CPTRINDEX.                                 00  INPT
214580     MOVE CSTUBPTR (CPTRINDEX) TO CNDINDEX.                       00  INPT
214590                                                                  00  INPT
214600 D25-CCONV-COMBINER-EXIT.                                         00  INPT
214610     EXIT.                                                        00  INPT
214620                                                                  00  INPT
214630                                                                  00  INPT
214640 D28-DONE-WITH-CONDIT-ROW.                                        00  INPT
214650     IF DOIND NOT EQUAL TO ZERO                                   00  INPT
214660         GO TO E48-PUT-OUT-LAST-STUB.                             00  INPT
214670     PERFORM H40-NOACTCOND THRU H42-NOACTCOND-EXIT.               00  INPT
214680     SUBTRACT 001 FROM CROW.                                      00  INPT
214690     SUBTRACT 001 FROM PSCOND.                                    00  INPT
214700     IF CONDS GREATER THAN ZERO                                   00  INPT
214710         SUBTRACT 001 FROM CONDS.                                 00  INPT
214720     GO TO E50-EDIT-CONVERT-EXIT.                                 00  INPT
214730                                                                  00  INPT
214740                                                                  00  INPT
214750 D50-PROCESS-ACTION-ROW.                                          00  INPT
214760     ADD 001 TO AROW.                                             00  INPT
214770     MOVE AROW TO CNTR-7.                                         00  INPT
214780     IF AROW NOT GREATER THAN ROWSIZE                             00  INPT
214790         GO TO D52-HAVE-ROOM-FOR-ACTION.                          00  INPT
214800     PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT.             00  INPT
214810     GO TO A20-INCR-TABLE-NO.                                     00  INPT
214820                                                                  00  INPT
214830 D52-HAVE-ROOM-FOR-ACTION.                                        00  INPT
214840     ADD 001 TO APROW.                                            00  INPT
214850     ADD 001 TO PSACTN.                                           00  INPT
214860                                                                  00  INPT
214870 D54-SEARCH-FOR-ASNS.                                             00  INPT
214880     ADD 001 TO CNTR-1.                                           00  INPT
214890                        NOTE ** START ACTION PROCESSING           00  INPT
214900                               AND ASN CHECK.                     00  INPT
214910     IF CNTR-1 GREATER THAN SHELFINDEX                            00  INPT
214920         GO TO D68-DONE-WITH-ACTION-ROW.                          00  INPT
214930     MOVE SHELF (CNTR-1) TO SCAN-FIELD.                           00  INPT
214940     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
214945         MOVE ZERO TO AMATRIX (AROW, CNTR-1)                      00  INPT
214950         GO TO D54-SEARCH-FOR-ASNS.                               00  INPT
214960     MOVE "1" TO DOIND.                                           00  INPT
214970     MOVE ZERO TO SUBX.                                           00  INPT
214980                                                                  00  INPT
214990 D56-FIND-FIRST-NON-NUM.                                          00  INPT
215000     ADD 001 TO SUBX.                                             00  INPT
215010     MOVE ELEMENT (SUBX) TO DOPTCOMMA.                            00  INPT
215020     IF DOPTCOMMA GREATER THAN "9"                                00  INPT
215030         ADD 059 TO SUBX.                                         00  INPT
215040     IF DOPTCOMMA LESS THAN "0"                                   00  INPT
215050         ADD 059 TO SUBX.                                         00  INPT
215060     IF SUBX LESS THAN 060                                        00  INPT
215070         GO TO D56-FIND-FIRST-NON-NUM.                            00  INPT
215080     IF SUBX EQUAL TO 060                                         00  INPT
215090         GO TO D60-NO-ASN.                                        00  INPT
215100     SUBTRACT 059 FROM SUBX.                                      00  INPT
215110     IF DOPTCOMMA NOT EQUAL TO ")"                                00  INPT
215120         GO TO D66-CHK-ASN-ONLY.                                  00  INPT
215130                                                                  00  INPT
215140 D58-YES-ASN.                                                     00  INPT
215150     IF SUBX LESS THAN 003                                        00  INPT
215160         PERFORM D98-SHIFT-RIGHT                                  00  INPT
215170         MOVE ZERO TO ELEMENT (1)                                 00  INPT
215180         ADD 001 TO SUBX                                          00  INPT
215190         GO TO D58-YES-ASN.                                       00  INPT
215200     IF SUBX EQUAL TO 003                                         00  INPT
215210         GO TO D62-CHECK-X.                                       00  INPT
215220     IF ELEMENT (1) NOT EQUAL TO ZERO                             00  INPT
215230         GO TO D60-NO-ASN.                                        00  INPT
215240     PERFORM D95-SHIFT-LEFT.                                      00  INPT
215250     SUBTRACT 001 FROM SUBX.                                      00  INPT
215260     GO TO D58-YES-ASN.                                           00  INPT
215270                                                                  00  INPT
215280 D60-NO-ASN.                                                      00  INPT
215290     PERFORM D98-SHIFT-RIGHT 3 TIMES.                             00  INPT
215300     MOVE AROW TO NUM-BER.                                        00  INPT
215310                                                                  00  INPT
215320 D62-CHECK-X.                                                     00  INPT
215330     IF SCAN-4-ON NOT EQUAL TO "X"                                00  INPT
215340         GO TO D64-NOT-X.                                         00  INPT
215350     IF LIMITED-ENTRY-IND LESS THAN 2                             00  INPT
215360         GO TO C15-INVALID-ROW-ENTRY.                             00  INPT
215370     MOVE  3  TO LIMITED-ENTRY-IND.                               00  INPT
215380     GO TO D65-DO-AMATRIX.                                        00  INPT
215390                                                                  00  INPT
215400 D64-NOT-X.                                                       00  INPT
215410     IF LIMITED-ENTRY-IND GREATER THAN 2                          00  INPT
215420         GO TO C15-INVALID-ROW-ENTRY.                             00  INPT
215430     MOVE  1  TO LIMITED-ENTRY-IND.                               00  INPT
215440                                                                  00  INPT
215450 D65-DO-AMATRIX.                                                  00  INPT
215460     MOVE NUM-BER TO AMATRIX (AROW CNTR-1).                       00  INPT
215470     MOVE SPACE TO SCAN-1-THRU-3.                                 00  INPT
215480     PERFORM D90-LEFT-JUSTIFY THRU D93-LEFT-JUSTIFY-EXIT.         00  INPT
215490     MOVE SCAN-FIELD TO SHELF (CNTR-1).                           00  INPT
215500     GO TO D54-SEARCH-FOR-ASNS.                                   00  INPT
215501                                                                  00  INPT
215502 D66-CHK-ASN-ONLY.                                                00  INPT
215504     MOVE SUBX TO XR-A.                                           00  INPT
215506                                                                  00  INPT
215508 D67-SCAN-ELEMENTS.                                               00  INPT
215510     IF XR-A GREATER THAN 060                                     00  INPT
215512         GO TO D68-DONE-WITH-SCAN.                                00  INPT
215514     IF ELEMENT (XR-A) EQUAL TO SPACE                             00  INPT
215516         ADD 001 TO XR-A                                          00  INPT
215518         GO TO D67-SCAN-ELEMENTS.                                 00  INPT
215520     GO TO D60-NO-ASN.                                            00  INPT
215522                                                                  00  INPT
215524 D68-DONE-WITH-SCAN.                                              00  INPT
215526     EXAMINE SCAN-FIELD TALLYING LEADING "0".                     00  INPT
215527     MOVE TALLY TO TALLX.                                         PDP-10
215528     SUBTRACT TALLX FROM SUBX GIVING TALLX.                       PDP-10
215530     IF TALLX GREATER THAN 003                                    PDP-10
215532         GO TO D60-NO-ASN.                                        00  INPT
215534     MOVE ")" TO ELEMENT (SUBX).                                  00  INPT
215536     MOVE SUBX TO XR-A.                                           00  INPT
215537     ADD 1 TO XR-A.                                               00  INPT
215538     MOVE "X" TO ELEMENT (XR-A).                                  00  INPT
215540     GO TO D58-YES-ASN.                                           00  INPT
215542                                                                  00  INPT
215546 D68-DONE-WITH-ACTION-ROW.                                        00  INPT
215548     IF DOIND NOT EQUAL TO ZERO                                   00  INPT
215550         GO TO D70-ROW-HAD-ENTRY.                                 00  INPT
215555     PERFORM H40-NOACTCOND THRU H42-NOACTCOND-EXIT.               00  INPT
215560     SUBTRACT 001 FROM AROW.                                      00  INPT
215570     SUBTRACT 001 FROM APROW.                                     00  INPT
215580     SUBTRACT 001 FROM PSACTN.                                    00  INPT
215590     IF ACTNS GREATER THAN ZERO                                   00  INPT
215600         SUBTRACT 001 FROM ACTNS.                                 00  INPT
215610     GO TO E50-EDIT-CONVERT-EXIT.                                 00  INPT
215620                                                                  00  INPT
215630 D70-ROW-HAD-ENTRY.                                               00  INPT
215640     MOVE ZERO TO CNTR-1.                                         00  INPT
215650                                                                  00  INPT
215660 D72-DO-A-ROW.                                                    00  INPT
215670     MOVE AROW TO CNTR-2.                                         00  INPT
215680     MOVE LIMITED-ENTRY-IND TO CNTR-3.                            00  INPT
215690     SUBTRACT 001 FROM CNTR-3.                                    00  INPT
215700     GO TO D74-INCR-CNTR-1.                                       00  INPT
215710                                                                  00  INPT
215720 D73-INCR-AROW.                                                   00  INPT
215730     ADD 001 TO AROW.                                             00  INPT
215740     ADD 001 TO APROW.                                            00  INPT
215750                                                                  00  INPT
215760 D74-INCR-CNTR-1.                                                 00  INPT
215770     ADD 001 TO CNTR-1.                                           00  INPT
215780     IF CNTR-1 NOT GREATER THAN SHELFINDEX                        00  INPT
215790         GO TO D76-EXT-ENT-ACT-ROW.                               00  INPT
215800     SUBTRACT 001 FROM AROW.                                      00  INPT
215810     SUBTRACT 001 FROM APROW.                                     00  INPT
215820     GO TO E48-PUT-OUT-LAST-STUB.                                 00  INPT
215830                                                                  00  INPT
215840 D76-EXT-ENT-ACT-ROW.                                             00  INPT
215850     IF CNTR-3 EQUAL TO 002                                       00  INPT
215860         GO TO E48-PUT-OUT-LAST-STUB.                             00  INPT
215870     MOVE SHELF (CNTR-1) TO SCAN-FIELD.                           00  INPT
215880     MOVE SHELF (CNTR-1) TO STUB6.                                00  INPT
215890     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
215900         GO TO D74-INCR-CNTR-1.                                   00  INPT
215910     PERFORM E00-STUB-MOVE THRU E35-STUB-MOVE-EXIT.               00  INPT
215920     IF CNTR-2 EQUAL TO AROW                                      00  INPT
215930         GO TO D78-LOOK-FOR-SAME-ENTRIES.                         00  INPT
215932     MOVE 001 TO CNTR-6.                                          00  INPT
215933                                                                  00  INPT
215934 D77-CLEAR-AMATRIX-ROW.                                           00  INPT
215935     MOVE ZERO TO AMATRIX (AROW, CNTR-6).                         00  INPT
215936     IF CNTR-6 NOT EQUAL TO SHELFINDEX                            00  INPT
215937         ADD 001 TO CNTR-6                                        00  INPT
215938         GO TO D77-CLEAR-AMATRIX-ROW.  NOTE ONLY NECESSARY FOR    00  INPT
215939                                          EXTENDED ENTRY.         00  INPT
215940     MOVE AMATRIX (CNTR-2, CNTR-1) TO                             00  INPT
215950          AMATRIX (AROW, CNTR-1).                                 00  INPT
215960     MOVE ZERO TO AMATRIX (CNTR-2, CNTR-1).                       00  INPT
215970                                                                  00  INPT
215980 D78-LOOK-FOR-SAME-ENTRIES.                                       00  INPT
215990     MOVE CNTR-1 TO CNTR-4.                                       00  INPT
216000                                                                  00  INPT
216010 D80-LOOKING.                                                     00  INPT
216020     ADD 001 TO CNTR-4.                                           00  INPT
216030     IF CNTR-4 GREATER THAN SHELFINDEX                            00  INPT
216040         GO TO D73-INCR-AROW.                                     00  INPT
216050     IF SHELF (CNTR-4) EQUAL TO SPACE                             00  INPT
216060         GO TO D80-LOOKING.                                       00  INPT
216070     IF SCAN-FIELD NOT EQUAL TO SHELF (CNTR-4)                    00  INPT
216080         GO TO D80-LOOKING.                                       00  INPT
216090                                                                  00  INPT
216100 D82-FOUND-ONE.                                                   00  INPT
216110     IF AROW EQUAL TO CNTR-2                                      00  INPT
216120         GO TO D84-CLEAR-AND-LOOK-AGAIN.                          00  INPT
216130     MOVE AMATRIX (CNTR-2, CNTR-4) TO AMATRIX (AROW, CNTR-4).     00  INPT
216140     MOVE ZERO TO AMATRIX (CNTR-2, CNTR-4).                       00  INPT
216150                                                                  00  INPT
216160 D84-CLEAR-AND-LOOK-AGAIN.                                        00  INPT
216170     MOVE SPACE TO SHELF (CNTR-4).                                00  INPT
216180     GO TO D80-LOOKING.                                           00  INPT
216190                                                                  00  INPT
216200                                                                  00  INPT
216210 D90-LEFT-JUSTIFY.                                                00  INPT
216220     IF SCAN-FIELD EQUAL TO SPACE                                 00  INPT
216230         GO TO D93-LEFT-JUSTIFY-EXIT.                             00  INPT
216240                             NOTE THIS SUBROUTINE LEFT-JUSTIFIES  00  INPT
216250                                ALL ALPHANUMERIC FIELDS.          00  INPT
216260 D92-CK-FOR-DONE.                                                 00  INPT
216270     IF ELEMENT (1) EQUAL TO SPACE                                00  INPT
216280         PERFORM D95-SHIFT-LEFT                                   00  INPT
216290         GO TO D92-CK-FOR-DONE.                                   00  INPT
216300 D93-LEFT-JUSTIFY-EXIT.                                           00  INPT
216310     EXIT.                                                        00  INPT
216320                                                                  00  INPT
216330                                                                  00  INPT
216340                                                                  00  INPT
216350 D95-SHIFT-LEFT.                                                  00  INPT
216360     MOVE SCAN-FIELD TO S-R-ONCE.                                 00  INPT
216370     MOVE S-R-1-BODY TO SCAN-FIELD.                               00  INPT
216380                                                                  00  INPT
216390                                                                  00  INPT
216400                                                                  00  INPT
216410 D98-SHIFT-RIGHT.                                                 00  INPT
216420     MOVE SCAN-FIELD TO S-R-1-BODY.                               00  INPT
216430     MOVE SPACE TO S-R-SPACE-1.                                   00  INPT
216440     MOVE S-R-SHIFTED TO SCAN-FIELD.                              00  INPT
216450                                                                  00  INPT
216460                                                                  00  INPT
216470                                                                  00  INPT
216480                                                                  00  INPT
216490 E00-STUB-MOVE.                                                   00  INPT
216500     MOVE ZERO TO CNTR-6.                                         00  INPT
216510                                                                  00  INPT
216520                        NOTE THIS ROUTINE SETS UP THE CONDITION   00  INPT
216530               AND ACTION STUB STACKS OR TABLES FOR LATER USE     00  INPT
216540                             IT ALSO SETS UP THE POINTERS USED    00  INPT
216550               TO KEEP TRACK OF THE STARTING POINT AND LENGTH     00  INPT
216560               OF EACH ROW STUBS SINCE THEY CAN VARY IN LENGTH.   00  INPT
216570     IF TYPEID EQUAL TO "A"                                       00  INPT
216580         GO TO E20-ACTION-STUB-MOVE.                              00  INPT
216590                                                                  00  INPT
216600 E05-CONDITION-STUB-MOVE.                                         00  INPT
216610     ADD 001 TO CNTR-6.                                           00  INPT
216620     IF CNTR-6 GREATER THAN STUBINDEX                             00  INPT
216630         GO TO E10-CSTUB-MOVE-1.                                  00  INPT
216640     IF STUBIES (CNTR-6) EQUAL TO SPACE                           00  INPT
216650         GO TO E10-CSTUB-MOVE-1.                                  00  INPT
216660     IF CNDINDEX GREATER THAN TBLMT1                              00  INPT
216670             PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT      00  INPT
216675             GO TO A20-INCR-TABLE-NO.                             00  INPT
216680     MOVE STUBIES (CNTR-6) TO CONDSTK (CNDINDEX).                 00  INPT
216690     ADD 001 TO CNDINDEX.                                         00  INPT
216700     GO TO E05-CONDITION-STUB-MOVE.                               00  INPT
216710                                                                  00  INPT
216720 E10-CSTUB-MOVE-1.                                                00  INPT
216730     IF STUB6 EQUAL TO SPACE                                      00  INPT
216740         GO TO E15-CSTUB-MOVE-2.  NOTE NO ENTRY TO BE ADDED       00  INPT
216750                                     TO STUB.                     00  INPT
216760     IF CNDINDEX GREATER THAN TBLMT1                              00  INPT
216770             PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT      00  INPT
216775             GO TO A20-INCR-TABLE-NO.                             00  INPT
216780     MOVE STUB6 TO CONDSTK (CNDINDEX).                            00  INPT
216790     ADD 001 TO CNDINDEX.                                         00  INPT
216800                                                                  00  INPT
216810 E15-CSTUB-MOVE-2.                                                00  INPT
216820     ADD 001 TO CPTRINDEX.                                        00  INPT
216830     IF CPTRINDEX GREATER THAN TBLMT2                             00  INPT
216840         PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT          00  INPT
216845         GO TO A20-INCR-TABLE-NO.                                 00  INPT
216850     MOVE CNDINDEX TO CSTUBPTR (CPTRINDEX).                       00  INPT
216860     GO TO E32-CLEAR-STUB6.                                       00  INPT
216870                                                                  00  INPT
216880 E20-ACTION-STUB-MOVE.                                            00  INPT
216890     ADD 001 TO CNTR-6.                                           00  INPT
216900     IF CNTR-6 GREATER THAN STUBINDEX                             00  INPT
216910         GO TO E25-ASTUB-MOVE-1.                                  00  INPT
216920     IF STUBIES (CNTR-6) EQUAL TO SPACE                           00  INPT
216930         GO TO E25-ASTUB-MOVE-1.                                  00  INPT
216940     IF ATNINDEX GREATER THAN TBLMT1                              00  INPT
216950             PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT      00  INPT
216955             GO TO A20-INCR-TABLE-NO.                             00  INPT
216960     MOVE STUBIES (CNTR-6) TO ACTNSTK (ATNINDEX).                 00  INPT
216970     ADD 001 TO ATNINDEX.                                         00  INPT
216980     GO TO E20-ACTION-STUB-MOVE.                                  00  INPT
216990                                                                  00  INPT
217000 E25-ASTUB-MOVE-1.                                                00  INPT
217010     IF STUB6 EQUAL TO SPACE                                      00  INPT
217020         GO TO E30-ASTUB-MOVE-2.                                  00  INPT
217030     IF ATNINDEX GREATER THAN TBLMT1                              00  INPT
217040             PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT      00  INPT
217045             GO TO A20-INCR-TABLE-NO.                             00  INPT
217050     MOVE STUB6 TO ACTNSTK (ATNINDEX).                            00  INPT
217060     ADD 001 TO ATNINDEX.                                         00  INPT
217070                                                                  00  INPT
217080 E30-ASTUB-MOVE-2.                                                00  INPT
217090     ADD 001 TO APTRINDEX.                                        00  INPT
217100     IF APTRINDEX GREATER THAN TBLMT2                             00  INPT
217110         PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT          00  INPT
217115         GO TO A20-INCR-TABLE-NO.                                 00  INPT
217120     MOVE ATNINDEX TO ASTUBPTR (APTRINDEX).                       00  INPT
217130                                                                  00  INPT
217140 E32-CLEAR-STUB6.                                                 00  INPT
217150     MOVE SPACE TO STUB6.                                         00  INPT
217160                                                                  00  INPT
217170 E35-STUB-MOVE-EXIT.                                              00  INPT
217180     EXIT.                                                        00  INPT
217190                                                                  00  INPT
217200                                                                  00  INPT
217210                                                                  00  INPT
217220 E48-PUT-OUT-LAST-STUB.                                           00  INPT
217230     IF CNTR-3 EQUAL TO 002                                       00  INPT
217240         PERFORM E00-STUB-MOVE THRU E35-STUB-MOVE-EXIT.           00  INPT
217250     MOVE 2 TO LIMITED-ENTRY-IND.                                 00  INPT
217260                                                                  00  INPT
217270 E50-EDIT-CONVERT-EXIT.                                           00  INPT
217280     GO TO A81-LABEL.                                              PDP-10
217290                                                                  00  INPT
217300                                                                  00  INPT
217310                                                                  00  INPT
217320                                                                  00  INPT
217330                                                                  00  INPT
217340 E75-SORT-ACTION-MATRIX.                                          00  INPT
217350     IF PART-FLAG-BND EQUAL TO SPACE                              00  INPT
217360         GO TO E77-FINISH-FORMATTED-TBL.                          00  INPT
217370     MOVE SPACE TO NOTE-BODY-PARTS.                               00  INPT
217380     MOVE WRITE-LIST TO TYPE-IO.                                  00  INPT
217390     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
217400                                                                  00  INPT
217410 E77-FINISH-FORMATTED-TBL.                                        00  INPT
217420     MOVE "TEND" TO SPACE-OUT.                                    00  INPT
217430     IF DOPTS-SW-FMT NOT EQUAL TO "1"                             00  INPT
217440         GO TO E78-PAGE-EJECT-FOR-DCP.                            00  INPT
217450     PERFORM W00-FORMATTED-TABLE THRU Y99-FORMATTED-TABLE-EXIT.   00  INPT
217460                                                                  00  INPT
217470 E78-PAGE-EJECT-FOR-DCP.                                          00  INPT
217480     IF DOPTS-SW-DCP EQUAL TO ZERO                                00  INPT
217490         GO TO E80-INITIALIZE-FOR-AMATRIX.                        00  INPT
217495     IF WRITEIND EQUAL TO "2"                                     00  INPT
217496         GO TO E80-INITIALIZE-FOR-AMATRIX.  NOTE ONLY PUNCHING.   00  INPT
217500     MOVE EJECT-PAGE TO TYPE-IO.                                  00  INPT
217510     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
217520                                                                  00  INPT
217530 E80-INITIALIZE-FOR-AMATRIX.                                      00  INPT
217550     MOVE CROW TO NBR-CONDS.                                      00  INPT
217560     MOVE AROW TO NACTNS.                                         00  INPT
217570     MOVE AROW TO NBR-ACTIONS.                                    00  INPT
217580     MOVE ZERO TO STKINDX.                                        00  INPT
217590     MOVE ZERO TO CNTR-1.                                         00  INPT
217600                                                                  00  INPT
217610                  NOTE *** THIS SECTION DOES THE SORTING OF ALL   00  INPT
217620                    THE ACTIONS (INCLUDING ELSE RULE) AND SETS    00  INPT
217630                    THEM UP IN ASCENDING ORDER FOR USE BY THE     00  INPT
217640                    OPTIMIZATION ROUTINES SO THE ACTION POINTERS  00  INPT
217650                    MAY BE CREATED ***.                           00  INPT
217660                                                                  00  INPT
217670     IF NACTNS NOT EQUAL TO ZERO                                  00  INPT
217680         GO TO E84-SET-UP-RULE-LOOP.                              00  INPT
217690     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
217700     MOVE NATNPR TO ERROR-PRINT.                                  00  INPT
217710     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
217720     GO TO A20-INCR-TABLE-NO.                                     00  INPT
217730                                                                  00  INPT
217780                                                                  00  INPT
217790 E84-SET-UP-RULE-LOOP.                                            00  INPT
217800     ADD 001 TO CNTR-1.                                           00  INPT
217810     IF CNTR-1 GREATER THAN NRULES                                00  INPT
217820         GO TO F00-COUNT-ENTRIES-IN-RULE.                         00  INPT
217830     MOVE ZERO TO CNTR-3.                                         00  INPT
217840                                                                  00  INPT
217850 E86-SET-UP-ACT-VECTOR-SLOTS.                                     00  INPT
217860     ADD 001 TO CNTR-3.                                           00  INPT
217870     IF CNTR-3 GREATER THAN NACTNS                                00  INPT
217880         GO TO E88-SORT-RULE-ACTN-VECTOR.                         00  INPT
217890     MOVE CNTR-3 TO TEMP-COLM (CNTR-3).                           00  INPT
217900     GO TO E86-SET-UP-ACT-VECTOR-SLOTS.                           00  INPT
217910                                                                  00  INPT
217920 E88-SORT-RULE-ACTN-VECTOR.                                       00  INPT
217930     MOVE ZERO TO CNTR-2.                                         00  INPT
217940                                                                  00  INPT
217950 E90-ISOLATE-ACTN-ENT-FOR-RULE.                                   00  INPT
217960     ADD 001 TO CNTR-2.                                           00  INPT
217970     IF CNTR-2 EQUAL TO NACTNS                                    00  INPT
217980         GO TO E94-HAVE-ARRANGED.                                 00  INPT
217990     MOVE AMATRIX (CNTR-2, CNTR-1) TO ELEM2.                      00  INPT
218000     MOVE CNTR-2 TO CNTR-3.                                       00  INPT
218010                                                                  00  INPT
218020 E92-ARRANGE-ACTNS.                                               00  INPT
218030     ADD 001 TO CNTR-3.                                           00  INPT
218040     IF CNTR-3 GREATER THAN NACTNS                                00  INPT
218050         GO TO E90-ISOLATE-ACTN-ENT-FOR-RULE.                     00  INPT
218060     MOVE AMATRIX (CNTR-3, CNTR-1) TO CNTR-4.                     00  INPT
218070     IF ELEM2 NOT GREATER THAN CNTR-4                             00  INPT
218080         GO TO E92-ARRANGE-ACTNS.                                 00  INPT
218090     MOVE CNTR-4 TO AMATRIX (CNTR-2, CNTR-1).                     00  INPT
218100     MOVE ELEM2 TO AMATRIX (CNTR-3, CNTR-1).                      00  INPT
218110     MOVE TEMP-COLM (CNTR-2) TO ELEM2.                            00  INPT
218120     MOVE TEMP-COLM (CNTR-3) TO TEMP-COLM (CNTR-2).               00  INPT
218130     MOVE ELEM2 TO TEMP-COLM (CNTR-3).                            00  INPT
218140     MOVE AMATRIX (CNTR-2, CNTR-1) TO ELEM2.                      00  INPT
218150     GO TO E92-ARRANGE-ACTNS.                                     00  INPT
218160 E94-HAVE-ARRANGED.                                               00  INPT
218170     MOVE ZERO TO CNTR-3.                                         00  INPT
218180                                                                  00  INPT
218190 E96-CONSTRUCT-AMATRIX.                                           00  INPT
218200     ADD 001 TO CNTR-3.                                           00  INPT
218210     IF CNTR-3 GREATER THAN NACTNS                                00  INPT
218220         GO TO E84-SET-UP-RULE-LOOP.                              00  INPT
218230     IF AMATRIX (CNTR-3, CNTR-1) EQUAL TO ZERO                    00  INPT
218240         GO TO E96-CONSTRUCT-AMATRIX.                             00  INPT
218250     MOVE TEMP-COLM (CNTR-3) TO AMATRIX (CNTR-3, CNTR-1).         00  INPT
218260     GO TO E96-CONSTRUCT-AMATRIX.                                 00  INPT
218270                                                                  00  INPT
218280 F00-COUNT-ENTRIES-IN-RULE.                                       00  INPT
218290     MOVE ZERO TO CNTR-1.                                         00  INPT
218300                                                                  00  INPT
218310 F02-RULE-LOOP.                                                   00  INPT
218320     ADD 001 TO CNTR-1.                                           00  INPT
218330     IF CNTR-1 GREATER THAN NRULES                                00  INPT
218340         GO TO F10-TABLE-VALIDATION.                              00  INPT
218350     MOVE ZERO TO TEMP-COLM (CNTR-1).                             00  INPT
218360     MOVE ZERO TO CNTR-2.                                         00  INPT
218370                                                                  00  INPT
218380 F04-COUNT-THIS-RULE.                                             00  INPT
218390     ADD 001 TO CNTR-2.                                           00  INPT
218400     IF CNTR-2 GREATER THAN NACTNS                                00  INPT
218410         GO TO F02-RULE-LOOP.                                     00  INPT
218420     IF AMATRIX (CNTR-2, CNTR-1) NOT EQUAL TO ZERO                00  INPT
218430         ADD 001 TO TEMP-COLM (CNTR-1).                           00  INPT
218440     GO TO F04-COUNT-THIS-RULE.                                   00  INPT
218450                                                                  00  INPT
218460                                                                  00  INPT
218470                                                                  00  INPT
218480                                                                  00  INPT
218490 F10-TABLE-VALIDATION.                                            00  INPT
218500     IF ELSE-IND EQUAL TO "1"                                     00  INPT
218510         SUBTRACT 001 FROM NBR-RULES.                             00  INPT
218520     MOVE SPACE TO S-MATRIX.                                      00  INPT
218530     MOVE ZERO TO X.                                              00  INPT
218540     MOVE ZERO TO Y.                                              00  INPT
218550                                                                  00  INPT
218560 F12-INCR-COND.                                                   00  INPT
218570     ADD 001 TO X.                                                00  INPT
218580     IF X GREATER THAN NBR-CONDS                                  00  INPT
218590         GO TO F25-ANALYZE-S-MATRIX.                              00  INPT
218600     MOVE ZERO TO Y.                                              00  INPT
218610                                                                  00  INPT
218620 F14-INCR-RULE.                                                   00  INPT
218630     ADD 001 TO Y.                                                00  INPT
218640     IF Y GREATER THAN NBR-RULES                                  00  INPT
218650         GO TO F12-INCR-COND.                                     00  INPT
218660     MOVE CMATRIX (X, Y) TO Q1.                                   00  INPT
218670     IF Q1 EQUAL TO "Y"                                           00  INPT
218680         GO TO F16-FOUND-A-Y.                                     00  INPT
218690     IF Q1 EQUAL TO "$"                                           00  INPT
218700         GO TO F16-FOUND-A-Y.                                     00  INPT
218710     GO TO F14-INCR-RULE.                                         00  INPT
218720                                                                  00  INPT
218730 F16-FOUND-A-Y.                                                   00  INPT
218740     MOVE ZERO TO Z.                                              00  INPT
218750                                                                  00  INPT
218760 F18-SCAN-ACROSS-TO-FIND-N.                                       00  INPT
218770     ADD 001 TO Z.                                                00  INPT
218780     IF Z GREATER THAN NBR-RULES                                  00  INPT
218790         GO TO F14-INCR-RULE.                                     00  INPT
218800     MOVE CMATRIX (X, Z) TO Q2.                                   00  INPT
218810     IF Q2 EQUAL TO "N"                                           00  INPT
218820         GO TO F20-FOUND-A-N.                                     00  INPT
218830     IF Q2 EQUAL TO "*"                                           00  INPT
218840         GO TO F20-FOUND-A-N.                                     00  INPT
218850     GO TO F18-SCAN-ACROSS-TO-FIND-N.                             00  INPT
218860                                                                  00  INPT
218870 F20-FOUND-A-N.                                                   00  INPT
218880     IF Z GREATER THAN Y                                          00  INPT
218890         MOVE "1" TO S (Y, Z)                                     00  INPT
218900             ELSE MOVE "1" TO S (Z, Y).                           00  INPT
218910     GO TO F18-SCAN-ACROSS-TO-FIND-N.                             00  INPT
218920                                                                  00  INPT
218931                       NOTE *** F10 THRU F20 DOES A DETAILED      00  INPT
218940                *  CHECK FOR CONTRADICTION AND REDUNDANCY         00  INPT
218950                *  BASED UPON Y-N PAIRS (WHERE Y=$, N=*)          00  INPT
218960                *  IN EACH RULE                                   00  INPT
218970                *      Y = POINTER TO LEFT-HAND RULE IN DOWN-SCAN 00  INPT
218980                *      X = POINTER T CONDITION ROW IN DOWN-SCAN   00  INPT
218990                *      Z = POINTER TO RIGHT-HAND RULE IN          00  INPT
219000                *                                    ACROSS-SCAN  00  INPT
219010                *     Q1 = TEMP HOLD AREA DOWN-SCAN               00  INPT
219020                *     Q2 = TEMP HOLD AREA ACROSS-SCAN             00  INPT
219030                * S(I,J) = SWITCH CELL (50X50 MATRIX)             00  INPT
219040                *             S(I,J) = 1 IF COLUMNS I AND J       00  INPT
219050                *                           HAVE A Y-N PAIR       00  INPT
219060                *             S(I,J) BLANK IF I AND J DO NOT      00  INPT
219070                *                           HAVE A Y-N PAIR.      00  INPT
219080                                                                  00  INPT
219090                                                                  00  INPT
219100                                                                  00  INPT
219110 F25-ANALYZE-S-MATRIX.                                            00  INPT
219120     MOVE ZERO TO Y.                                              00  INPT
219130     MOVE ZERO TO Z.                                              00  INPT
219140                                                                  00  INPT
219150 F27-INCR-Y.                                                      00  INPT
219160     ADD 001 TO Y.                                                00  INPT
219170     IF Y EQUAL TO NBR-RULES                                      00  INPT
219180         GO TO F40-CHECK-FOR-CONTRADICT-FOUND.                    00  INPT
219190     MOVE Y TO Z.                                                 00  INPT
219200                                                                  00  INPT
219210 F29-INCR-Z.                                                      00  INPT
219220     ADD 001 TO Z.                                                00  INPT
219230     IF Z GREATER THAN NBR-RULES                                  00  INPT
219240         GO TO F27-INCR-Y.                                        00  INPT
219250     IF S (Y, Z) EQUAL TO "1"                                     00  INPT
219260         GO TO F29-INCR-Z.  NOTE COLUMNS-HAD-Y-N-PAIR.            00  INPT
219270                                                                  00  INPT
219280 F31-HAVE-NO-Y-N-PAIR-IN-YZ.                                      00  INPT
219290     MOVE ZERO TO X.                                              00  INPT
219310                                                                  00  INPT
219320 F32-LOOP-THRU-ACTNS.                                             00  INPT
219330     ADD 001 TO X.                                                00  INPT
219340     IF X GREATER THAN NACTNS                                     00  INPT
219350         GO TO F36-REDUNDANCY.                                    00  INPT
219360     IF AMATRIX (X, Y) EQUAL TO AMATRIX (X, Z)                    00  INPT
219370         GO TO F32-LOOP-THRU-ACTNS.                               00  INPT
219380                                                                  00  INPT
219390 F34-CONTRADICTION.                                               00  INPT
219400     MOVE CONTRERROR TO ERROR-PRINT.                              00  INPT
219410     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
219450     MOVE "1" TO CONTRA-SW.                                       00  INPT
219460     GO TO F38-COMPLETE-ERROR-MSG.                                00  INPT
219470                                                                  00  INPT
219480 F36-REDUNDANCY.                                                  00  INPT
219490     MOVE REDUNERROR TO ERROR-PRINT.                              00  INPT
219500     MOVE WARNING TO TYPEMSG.                                     00  INPT
219520                                                                  00  INPT
219530 F38-COMPLETE-ERROR-MSG.                                          00  INPT
219540     MOVE Y TO RULE-1.                                            00  INPT
219550     MOVE Z TO RULE-2.                                            00  INPT
219560     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
219570     GO TO F29-INCR-Z.                                            00  INPT
219580                                                                  00  INPT
219590 F40-CHECK-FOR-CONTRADICT-FOUND.                                  00  INPT
219600     IF CONTRA-SW EQUAL TO "1"                                    00  INPT
219610         MOVE ZERO TO CONTRA-SW                                   00  INPT
219620         GO TO A20-INCR-TABLE-NO. NOTE CANCEL TABLE SINCE         00  INPT
219630                                   CONTRADICTION FOUND.           00  INPT
219640     IF ELSE-IND EQUAL TO "1"                                     00  INPT
219650         ADD 001 TO NBR-RULES.                                    00  INPT
219660     IF TRACE-IND EQUAL TO "1"                                    00  INPT
219670         GO TO G00-DECOMPOSE-TABLE.                               00  INPT
219680                                                                  00  INPT
219690                       NOTE *** THE FOLLOWING CODE APPLIES A      00  INPT
219700                      *  PARTIAL OPTIMIZATION OF ACTIONS          00  INPT
219710                      *  BY TAKING ANY THAT ARE DUPLICATES AND    00  INPT
219720                      *  HAVING THEM POINT TO THE REDUNDANT ONES. 00  INPT
219730                                                                  00  INPT
219734                                                                  00  INPT
219735 F41-CYCLE-THRU-TABLE.                                            00  INPT
219740     MOVE ZERO TO CNTR-1.                                         00  INPT
219750     MOVE ZERO TO COMB-MADE-SW.                                   00  INPT
219755         NOTE NBR-RULE HERE INCLUDES ELSE-RULE.                   00  INPT
219760                                                                  00  INPT
219770 F42-INCR-RULE.                                                   00  INPT
219780     ADD 001 TO CNTR-1.                                           00  INPT
219790     IF CNTR-1 EQUAL TO NRULES                                    00  INPT
219800         GO TO F68-CK-FOR-COMB-MADE.                              00  INPT
219810     MOVE TEMP-COLM (CNTR-1) TO ELEM.                             00  INPT
219815         NOTE TEMP-COLM (S) HAS NUMBER OF ACTNS IN RULE S.        00  INPT
219820     IF ELEM EQUAL TO ZERO                                        00  INPT
219830         GO TO F42-INCR-RULE. NOTE NO ACTNS FOR RULE.             00  INPT
219840     MOVE CNTR-1 TO CNTR-2.                                       00  INPT
219850                                                                  00  INPT
219860 F44-INCR-COMPAR-RULE.                                            00  INPT
219870     ADD 001 TO CNTR-2.                                           00  INPT
219880     IF CNTR-2 GREATER THAN NRULES                                00  INPT
219890         GO TO F42-INCR-RULE.                                     00  INPT
219900     MOVE TEMP-COLM (CNTR-2) TO ELEM2.                            00  INPT
219910     IF ELEM2 NOT EQUAL TO ELEM                                   00  INPT
219920         GO TO F44-INCR-COMPAR-RULE.                              00  INPT
219930     MOVE ZERO TO CNTR-3.                                         00  INPT
219940              NOTE HERE BOTH RULES HAVE SAME NUMBER OF ACTIONS    00  INPT
219950               SO CHECK DETAIL.                                   00  INPT
219960                                                                  00  INPT
219970 F46-DETAIL-COMPAR-ACTIONS.                                       00  INPT
219980     ADD 001 TO CNTR-3.                                           00  INPT
219990     IF CNTR-3 GREATER THAN NACTNS                                00  INPT
220000         GO TO F48-HAVE-IDENT-ACTN-SEQS.                          00  INPT
220010     IF AMATRIX (CNTR-3, CNTR-1) NOT EQUAL TO                     00  INPT
220020        AMATRIX (CNTR-3, CNTR-2)                                  00  INPT
220030         GO TO F44-INCR-COMPAR-RULE.                              00  INPT
220040     GO TO F46-DETAIL-COMPAR-ACTIONS.                             00  INPT
220050                                                                  00  INPT
220060 F48-HAVE-IDENT-ACTN-SEQS.                                        00  INPT
220070     MOVE ZERO TO CNTR-3.                                         00  INPT
220080     MOVE ZERO TO Q1.                                             00  INPT
220090     IF CNTR-2 GREATER THAN NBR-RULES                             00  INPT
220100         GO TO F65-SUBSTITUTE-ACTN-PTR.                           00  INPT
220110     IF ELSE-IND NOT EQUAL TO "1"                                 00  INPT
220120         GO TO F50-LOOP-COMPAR-CONDIT-ENTS.                       00  INPT
220130     IF CNTR-2 EQUAL TO NBR-RULES                                 00  INPT
220140         GO TO F65-SUBSTITUTE-ACTN-PTR.  NOTE CANT COMBINE        00  INPT
220150                                            ELSE RULE.            00  INPT
220160                                                                  00  INPT
220170 F50-LOOP-COMPAR-CONDIT-ENTS.                                     00  INPT
220180     ADD 001 TO CNTR-3.                                           00  INPT
220190     IF CNTR-3 GREATER THAN NBR-CONDS                             00  INPT
220200         GO TO F52-AT-MOST-1-DIFF.                                00  INPT
220210     MOVE CMATRIX (CNTR-3, CNTR-1) TO H1.                         00  INPT
220220     MOVE CMATRIX (CNTR-3, CNTR-2) TO H2.                         00  INPT
220230     IF H1 EQUAL TO "$"                                           00  INPT
220240         MOVE "Y" TO H1.                                          00  INPT
220250     IF H1 EQUAL TO "*"                                           00  INPT
220260         MOVE "N" TO H1.                                          00  INPT
220270     IF H2 EQUAL TO "$"                                           00  INPT
220280         MOVE "Y" TO H2.                                          00  INPT
220290     IF H2 EQUAL TO "*"                                           00  INPT
220300         MOVE "N" TO H2.  NOTE FORCE $=Y, *=N.                    00  INPT
220310     IF H1 EQUAL TO H2                                            00  INPT
220320         GO TO F50-LOOP-COMPAR-CONDIT-ENTS.                       00  INPT
220330     IF Q1 EQUAL TO "1"                                           00  INPT
220340         GO TO F65-SUBSTITUTE-ACTN-PTR. NOTE MORE THAN 1 DIFF.    00  INPT
220350     MOVE "1" TO Q1.                                              00  INPT
220360     GO TO F50-LOOP-COMPAR-CONDIT-ENTS.                           00  INPT
220370                                                                  00  INPT
220380 F52-AT-MOST-1-DIFF.                                              00  INPT
220390     IF Q1 EQUAL TO ZERO                                          00  INPT
220400         GO TO F65-SUBSTITUTE-ACTN-PTR. NOTE RULES ARE REDUNDANT. 00  INPT
220410     MOVE CNTR-1 TO Y.                                            00  INPT
220420     MOVE CNTR-2 TO Z.                                            00  INPT
220430     MOVE ZERO TO X.                                              00  INPT
220440                                                                  00  INPT
220450 F54-ANALYZE-FOR-RULE-COMB.                                       00  INPT
220460     ADD 001 TO X.                                                00  INPT
220470     IF X GREATER THAN NBR-CONDS                                  00  INPT
220480         GO TO F42-INCR-RULE.                                     00  INPT
220490     MOVE CMATRIX (X, Y) TO H1.                                   00  INPT
220500     MOVE CMATRIX (X, Z) TO H2.                                   00  INPT
220510     IF H1 EQUAL TO H2                                            00  INPT
220520         GO TO F54-ANALYZE-FOR-RULE-COMB.                         00  INPT
220530     IF H1 EQUAL TO "$"                                           00  INPT
220540         MOVE "Y" TO H1.                                          00  INPT
220550     IF H1 EQUAL TO "*"                                           00  INPT
220560         MOVE "N" TO H1.                                          00  INPT
220570     IF H2 EQUAL TO "$"                                           00  INPT
220580         MOVE "Y" TO H2.                                          00  INPT
220590     IF H2 EQUAL TO "*"                                           00  INPT
220600         MOVE "N" TO H2.                                          00  INPT
220610     IF H1 EQUAL TO SPACE                                         00  INPT
220620         GO TO F60-BLANK-BOTH.                                    00  INPT
220630     IF H2 EQUAL TO SPACE                                         00  INPT
220640         GO TO F60-BLANK-BOTH.                                    00  INPT
220650     IF H1 EQUAL TO "Y"                                           00  INPT
220660         GO TO F58-Y-FND.                                         00  INPT
220670                                                                  00  INPT
220680 F56-N-FND.                                                       00  INPT
220690     IF H2 EQUAL TO "Y"                                           00  INPT
220700         GO TO F60-BLANK-BOTH                                     00  INPT
220710             ELSE GO TO F62-PLUG-IN-ENTRY.                        00  INPT
220720                                                                  00  INPT
220730 F58-Y-FND.                                                       00  INPT
220740     IF H2 EQUAL TO "N"                                           00  INPT
220750         GO TO F60-BLANK-BOTH                                     00  INPT
220760             ELSE GO TO F62-PLUG-IN-ENTRY.                        00  INPT
220770                                                                  00  INPT
220780 F60-BLANK-BOTH.                                                  00  INPT
220790     MOVE SPACE TO H1.                                            00  INPT
220800                                                                  00  INPT
220810 F62-PLUG-IN-ENTRY.                                               00  INPT
220820     MOVE H1 TO CMATRIX (X, Z).                                   00  INPT
220830     MOVE H1 TO CMATRIX (X, Y).                                   00  INPT
220840     MOVE "1" TO COMB-MADE-SW.                                    00  INPT
220850     GO TO F54-ANALYZE-FOR-RULE-COMB.                             00  INPT
220860                                                                  00  INPT
220870                                                                  00  INPT
220880 F65-SUBSTITUTE-ACTN-PTR.                                         00  INPT
220890     MOVE ACTNPTR (CNTR-1) TO ACTNPTR (CNTR-2).                   00  INPT
220900     GO TO F44-INCR-COMPAR-RULE.                                  00  INPT
220910                                                                  00  INPT
220920 F68-CK-FOR-COMB-MADE.                                            00  INPT
220930     IF COMB-MADE-SW EQUAL TO ZERO                                00  INPT
220940         GO TO G00-DECOMPOSE-TABLE                                00  INPT
220950             ELSE GO TO F41-CYCLE-THRU-TABLE.                     00  INPT
220960                                                                  00  INPT
220970                                                                  00  INPT
220980                                                                  00  INPT
220990                                                                  00  INPT
221000 F70-BACKSCAN.                                                    00  INPT
221010     MOVE 060 TO CNTR-7.                                          00  INPT
221020                                                                  00  INPT
221030 F71-BACKSCAN-LOOP.                                               00  INPT
221040     SUBTRACT 001 FROM CNTR-7.                                    00  INPT
221050     IF ELEMENT (CNTR-7) EQUAL TO SPACE                           00  INPT
221060         GO TO F71-BACKSCAN-LOOP.                                 00  INPT
221070                                                                  00  INPT
221080 F72-BACKSCAN-EXIT.                                               00  INPT
221090     EXIT.                                                        00  INPT
221100                                                                  00  INPT
221110                                                                  00  INPT
221120 F75-REPLACE-DITTO.                                               00  INPT
221130     IF SCAN-FIELD NOT EQUAL TO CHARACTER-HOR-DITTO               00  INPT
221140         GO TO F77-REPLACE-DITTO-EXIT.                            00  INPT
221150     IF  SHELFINDEX EQUAL TO 001                                  00  INPT
221160         GO TO F77-REPLACE-DITTO-EXIT.                            00  INPT
221170     IF DOPTS-SW-DIT NOT EQUAL TO "1"                             00  INPT
221180         GO TO F77-REPLACE-DITTO-EXIT.                            00  INPT
221190     MOVE SHELFINDEX TO SUBX.                                     00  INPT
221200     SUBTRACT 001 FROM SUBX.                                      00  INPT
221210                                                                  00  INPT
221220     MOVE SHELF (SUBX) TO SCAN-FIELD.                             00  INPT
221230                                                                  00  INPT
221240 F77-REPLACE-DITTO-EXIT.                                          00  INPT
221250     EXIT.                                                        00  INPT
221260                                                                  00  INPT
222000 G00-DECOMPOSE-TABLE.                                             00  INPT
222010     IF ELSE-IND EQUAL TO "1"                                     00  INPT
222020         SUBTRACT 001 FROM NBR-RULES.                             00  INPT
222030     IF P-SW EQUAL TO "1"                                         00  INPT
222040         MOVE ZERO TO P-SW                                        00  INPT
222050         GO TO G04-CK-NBR-RULES. NOTE NO PARAMS SUPPLIED BY USER  00  INPT
222060                                   SO SUPPRESS MESSAGE.           00  INPT
222070     IF RULES NOT EQUAL TO NBR-RULES                              00  INPT
222080         GO TO G02-PARAM-ERR-MSG.                                 00  INPT
222090     IF ACTNS NOT EQUAL TO PSACTN                                 00  INPT
222100         GO TO G02-PARAM-ERR-MSG.                                 00  INPT
222110     IF CONDS NOT EQUAL TO PSCOND                                 00  INPT
222120         GO TO G02-PARAM-ERR-MSG.                                 00  INPT
222130     GO TO G04-CK-NBR-RULES.                                      00  INPT
222140                                                                  00  INPT
222150 G02-PARAM-ERR-MSG.                                               00  INPT
222160     MOVE WARNING TO TYPEMSG.                                     00  INPT
222170     MOVE PRMERROR TO ERROR-PRINT.                                00  INPT
222180     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
222190                                                                  00  INPT
222200 G04-CK-NBR-RULES.                                                00  INPT
222210     IF NBR-RULES NOT EQUAL TO ZERO                               00  INPT
222220         GO TO G06-SET-UP-SECTION-NAME.                           00  INPT
222230     IF PSACTN NOT GREATER THAN ZERO                              00  INPT
222240         GO TO G06-SET-UP-SECTION-NAME.                           00  INPT
222250     MOVE "NO RULES FOUND." TO ERROR-PRINT.                       00  INPT
222270     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
222280     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
222290     MOVE 001 TO NBR-RULES.                                       00  INPT
222300     MOVE ZERO TO ELSE-IND.                                       00  INPT
222310                                                                  00  INPT
222320 G06-SET-UP-SECTION-NAME.                                         00  INPT
222330     MOVE TBLNAME TO SCAN-FIELD.                                  00  INPT
222340     PERFORM F70-BACKSCAN THRU F72-BACKSCAN-EXIT.                 00  INPT
222350     IF CNTR-7 NOT GREATER THAN 030                               00  INPT
222360         GO TO G10-FORMAT-SECT-NAME.                              00  INPT
222370                                                                  00  INPT
222380 G08-TBL-NAME-TOO-BIG.                                            00  INPT
222390     MOVE "SECTION-NAME TRUNCATED TO 30 CHARACTERS."              00  INPT
222400         TO ERROR-PRINT.                                          00  INPT
222410     MOVE WARNING TO TYPEMSG.                                     00  INPT
222420     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
222430     MOVE SPACE TO ELEMENT-31-ON.                                 00  INPT
222440     MOVE 030 TO CNTR-7.                                          00  INPT
222450                                                                  00  INPT
222460 G10-FORMAT-SECT-NAME.                                            00  INPT
222470     ADD 001 TO CNTR-7.                                           00  INPT
222480     MOVE ZERO TO CNTR-1.                                         00  INPT
222510                                                                  00  INPT
222520 G12-CONST-MOVE-LOOP.                                             00  INPT
222530     ADD 001 TO CNTR-7.                                           00  INPT
222540     ADD 001 TO CNTR-1.                                           00  INPT
222550     IF CNTR-1 GREATER THAN 008                                   00  INPT
222560         GO TO G14-HAVE-SECT-NAME.                                00  INPT
222570     MOVE SECT (CNTR-1) TO ELEMENT (CNTR-7).                      00  INPT
222580     GO TO G12-CONST-MOVE-LOOP.                                   00  INPT
222590                                                                  00  INPT
222600 G14-HAVE-SECT-NAME.                                              00  INPT
222620     MOVE SCAN-FIELD TO SECTION-OUT.                              00  INPT
222630     PERFORM H75-IDENTS-OUT.                                      00  INPT
222640     MOVE "1" TO TMPID.                                           00  INPT
222650     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
222660     MOVE ZERO TO TMPID.                                          00  INPT
222670     IF ININDEX EQUAL TO ZERO                                     00  INPT
222680         GO TO G18-HAVE-DONE-INIT.                                00  INPT
222690     PERFORM H70-PUT-OUT-DT-LBL THRU H72-PUT-OUT-DT-LBL-EXIT.     00  INPT
222700     MOVE ZERO TO CNTR-1.                                         00  INPT
222710                                                                  00  INPT
222720 G16-LOOP-TO-PUT-OUT-INIT.                                        00  INPT
222730     ADD 001 TO CNTR-1.                                           00  INPT
222740     IF CNTR-1 GREATER THAN ININDEX                               00  INPT
222750         GO TO G18-HAVE-DONE-INIT.                                00  INPT
222760     MOVE IN-ITIAL (CNTR-1) TO B-MARG.                            00  INPT
222770     PERFORM H75-IDENTS-OUT.                                      00  INPT
222780     MOVE "1" TO TMPID.                                           00  INPT
222790     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
222800     MOVE ZERO TO TMPID.                                          00  INPT
222810     MOVE SPACE TO B-MARG.                                        00  INPT
222820     GO TO G16-LOOP-TO-PUT-OUT-INIT.                              00  INPT
222830                                                                  00  INPT
222840 G18-HAVE-DONE-INIT.                                              00  INPT
222850     MOVE ZERO TO COUNT-IN-SPILL.                                 00  INPT
222860     MOVE ZERO TO SPILL-INDEX.                                    00  INPT
222870     IF NBR-CONDS EQUAL TO ZERO                                   00  INPT
222880         GO TO G20-HAVE-1ST-LBL.                                  00  INPT
222890     PERFORM H70-PUT-OUT-DT-LBL THRU H72-PUT-OUT-DT-LBL-EXIT.     00  INPT
222900                                                                  00  INPT
222910 G20-HAVE-1ST-LBL.                                                00  INPT
222920     MOVE STKINDX TO OUTINDEX.                                    00  INPT
222926     MOVE STKINDX TO CNTR-4. NOTE SAVE ORIG STKINDX.              00  INPT
222930     MOVE WRITEIND TO PRINT-PUNCH.                                00  INPT
222940     IF NBR-CONDS EQUAL TO ZERO                                   00  INPT
222950         GO TO G35-ACTIONS-OUTPUT.                                00  INPT
222960     PERFORM K00-DECOMPOSE-TABLE THRU                             00  INPT
222970             P99-DECOMPOSE-TABLE-EXIT.                            00  INPT
222976     MOVE CNTR-4 TO STKINDX. NOTE RESTORE ORIG STKINDX.           00  INPT
222980                                                                  00  INPT
222990 G22-ANALYZE-DECOMP-RESULT.                                       00  INPT
223000     IF ERROR-IND EQUAL TO ZERO                                   00  INPT
223010         GO TO G35-ACTIONS-OUTPUT.                                00  INPT
223020     IF ERROR-IND EQUAL TO "8"                                    00  INPT
223030         PERFORM H30-SIZE-ERROR THRU H32-SIZE-ERROR-EXIT          00  INPT
223040         GO TO A20-INCR-TABLE-NO.                                 00  INPT
223050     IF ERROR-IND EQUAL TO "5"                                    00  INPT
223060         GO TO G26-REDUN-ERR.                                     00  INPT
223110     GO TO H45-UNKNOWN-ERROR.                                     00  INPT
223120                                                                  00  INPT
223330 G26-REDUN-ERR.                                                   00  INPT
223440     IF STKINDX EQUAL TO 002                                      00  INPT
223450         MOVE ZERO TO DXNUM.                                      00  INPT
223460     IF NBR-RULES NOT GREATER THAN 002                            00  INPT
223470         GO TO A20-INCR-TABLE-NO.                                 00  INPT
223480     MOVE ERR-RULE-01 TO CNTR-1.                                  00  INPT
223485     MOVE ZERO TO CNTR-2.                                         00  INPT
223490     PERFORM G30-RE-ADJUST THRU G32-RE-ADJUST-EXIT.               00  INPT
223500     SUBTRACT 001 FROM NBR-RULES.                                 00  INPT
223510     GO TO G20-HAVE-1ST-LBL.                                      00  INPT
223520                                                                  00  INPT
223530                                                                  00  INPT
223540                                                                  00  INPT
223550 G30-RE-ADJUST.                                                   00  INPT
223560     ADD 001 TO CNTR-2.                                           00  INPT
223570     IF INARRAY (CNTR-2) NOT EQUAL TO CNTR-1                      00  INPT
223580         GO TO G30-RE-ADJUST.                                     00  INPT
223590     MOVE CNTR-2 TO CNTR-3.                                       00  INPT
223600                                                                  00  INPT
223610 G31-FOUND-IT.                                                    00  INPT
223620     ADD 001 TO CNTR-3.                                           00  INPT
223630     IF CNTR-3 GREATER THAN NBR-RULES                             00  INPT
223640         GO TO G32-RE-ADJUST-EXIT.                                00  INPT
223650     MOVE INARRAY (CNTR-3) TO INARRAY (CNTR-2).                   00  INPT
223660     ADD 001 TO CNTR-2.                                           00  INPT
223670     GO TO G31-FOUND-IT.                                          00  INPT
223680                                                                  00  INPT
223690 G32-RE-ADJUST-EXIT.                                              00  INPT
223700     EXIT.                                                        00  INPT
223710                                                                  00  INPT
223720                                                                  00  INPT
223730                                                                  00  INPT
223740 G35-ACTIONS-OUTPUT.                                              00  INPT
223750     MOVE OUTINDEX TO STKINDX.                                    00  INPT
223760     MOVE ZERO TO CNTR-1.                                         00  INPT
223770     IF NBR-ACTIONS NOT EQUAL TO ZERO                             00  INPT
223780         GO TO G37-LOOP-TO-PUT-OUT-ACTIONS.                       00  INPT
223790     IF ELSE-IND EQUAL TO ZERO                                    00  INPT
223800         GO TO G55-FINISH-UP.                                     00  INPT
223810                                                                  00  INPT
223820 G37-LOOP-TO-PUT-OUT-ACTIONS.                                     00  INPT
223830     ADD 001 TO CNTR-1.                                           00  INPT
223840     IF CNTR-1 GREATER THAN NBR-RULES                             00  INPT
223850         GO TO G43-CHECK-ON-ELSE.                                 00  INPT
223860     MOVE INARRAY (CNTR-1) TO CNTR-7.                             00  INPT
223870     MOVE ACTNPTR (CNTR-7) TO CNTR-6.                             00  INPT
223880     IF TRACE-IND EQUAL TO "1"                                    00  INPT
223890         GO TO G39-TRACE-MSG.                                     00  INPT
223900     IF TNARRAY (CNTR-6) EQUAL TO 1                               00  INPT
223910         GO TO G37-LOOP-TO-PUT-OUT-ACTIONS.                       00  INPT
223920     PERFORM H60-AT-LABEL-OUTPUT THRU H62-AT-LBL-EXIT.            00  INPT
223930     GO TO G41-DO-ACTNS.                                          00  INPT
223940                                                                  00  INPT
223950 G39-TRACE-MSG.                                                   00  INPT
223960     PERFORM H60-AT-LABEL-OUTPUT THRU H62-AT-LBL-EXIT.            00  INPT
223970     MOVE "6" TO TYPE-RECORD.                                     00  INPT
223980     MOVE "A" TO SUB-TYPE.                                        00  INPT
223990     MOVE "DS" TO LABEL-TYPE.                                     00  INPT
224000     MOVE CNTR-7 TO LABEL-1.                                      00  INPT
224010     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224020     MOVE SPACE TO BUFFER.                                        00  INPT
224030     IF TNARRAY (CNTR-6) NOT EQUAL TO 1                           00  INPT
224040         GO TO G41-DO-ACTNS.                                      00  INPT
224050     MOVE "6" TO TYPE-RECORD.                                     00  INPT
224060     MOVE "A" TO SUB-TYPE.                                        00  INPT
224070     MOVE "GT" TO LABEL-TYPE.                                     00  INPT
224080     MOVE CNTR-6 TO LABEL-1.                                      00  INPT
224090     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224100     MOVE SPACE TO BUFFER.                                        00  INPT
224110     GO TO G37-LOOP-TO-PUT-OUT-ACTIONS.                           00  INPT
224120                                                                  00  INPT
224130 G41-DO-ACTNS.                                                    00  INPT
224140     MOVE 1 TO TNARRAY (CNTR-6).                                  00  INPT
224150     MOVE SPACE TO BUFFER.                                        00  INPT
224160     MOVE CNTR-6 TO TCOL.                                         00  INPT
224170     PERFORM G75-PERFORM-ANALYSIS THRU G85-PERF-ANAL-EXIT.        00  INPT
224180     PERFORM H55-GO-TO-DEXIT.                                     00  INPT
224190     GO TO G37-LOOP-TO-PUT-OUT-ACTIONS.                           00  INPT
224200                                                                  00  INPT
224210 G43-CHECK-ON-ELSE.                                               00  INPT
224220     IF ELSE-IND EQUAL TO "1"                                     00  INPT
224230         ADD 001 TO NBR-RULES.                                    00  INPT
224240     MOVE SPACE TO BUFFER.                                        00  INPT
224250     IF ELSERULE NOT EQUAL TO "4"                                 00  INPT
224260         GO TO G50-FINISH-PERFORMS.                               00  INPT
224270     MOVE "6" TO TYPE-RECORD.                                     00  INPT
224280     MOVE "A" TO SUB-TYPE.                                        00  INPT
224290     MOVE "EL" TO LABEL-TYPE.                                     00  INPT
224300     MOVE 001 TO LABEL-1.                                         00  INPT
224310     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224320     MOVE SPACE TO BUFFER.                                        00  INPT
224330     IF TRACE-IND NOT EQUAL TO "1"                                00  INPT
224340         GO TO G45-WHAT-KIND-OF-ELSE.                             00  INPT
224350     MOVE "6" TO TYPE-RECORD.    NOTE TRACE ELSE MSG.             00  INPT
224355     MOVE "A" TO SUB-TYPE.                                        00  INPT
224360     MOVE "DS" TO LABEL-TYPE.                                     00  INPT
224370     MOVE 999 TO LABEL-1.                                         00  INPT
224380     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224390     MOVE SPACE TO BUFFER.                                        00  INPT
224400                                                                  00  INPT
224410 G45-WHAT-KIND-OF-ELSE.                                           00  INPT
224420     IF ELSE-IND EQUAL TO "1"                                     00  INPT
224430         GO TO G47-USER-SUPPLIED-ELSE.                            00  INPT
224440     MOVE "4" TO TYPE-RECORD.          NOTE ELSE STOP RUN MSG.    00  INPT
224450     MOVE "C" TO SUB-TYPE.                                        00  INPT
224460     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224470     MOVE SPACE TO BUFFER.                                        00  INPT
224480     GO TO G50-FINISH-PERFORMS.                                   00  INPT
224490                                                                  00  INPT
224500 G47-USER-SUPPLIED-ELSE.                                          00  INPT
224510     IF TEMP-COLM (NRULES) NOT EQUAL TO ZERO                      00  INPT
224520         GO TO G48-ELSE-HAS-ACTNS.                                00  INPT
224530     MOVE WARNING TO TYPEMSG.                                     00  INPT
224540     MOVE ELSERROR TO ERROR-PRINT.                                00  INPT
224550     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
224560     GO TO G50-FINISH-PERFORMS.                                   00  INPT
224570                                                                  00  INPT
224580 G48-ELSE-HAS-ACTNS.                                              00  INPT
224590     MOVE NRULES TO TCOL.                                         00  INPT
224600     PERFORM G75-PERFORM-ANALYSIS THRU G85-PERF-ANAL-EXIT.        00  INPT
224610     PERFORM H55-GO-TO-DEXIT.                                     00  INPT
224620                                                                  00  INPT
224630                                                                  00  INPT
224640 G50-FINISH-PERFORMS.                                             00  INPT
224650     MOVE ZERO TO PFRMINDX.                                       00  INPT
224660                                                                  00  INPT
224670 G52-LOOP-PERFORMABLES.                                           00  INPT
224680     ADD 001 TO PFRMINDX.                                         00  INPT
224690     IF PFRMINDX GREATER THAN NACTNS                              00  INPT
224700         GO TO G55-FINISH-UP.                                     00  INPT
224710     IF INDARRAY (PFRMINDX) EQUAL TO ZERO                         00  INPT
224720         GO TO G52-LOOP-PERFORMABLES.                             00  INPT
224730     MOVE "6" TO TYPE-RECORD.                                     00  INPT
224740     MOVE "A" TO SUB-TYPE.                                        00  INPT
224750     MOVE "AT" TO LABEL-TYPE.                                     00  INPT
224760     MOVE PFRMINDX  TO LABEL-1.                                   00  INPT
224770     ADD NRULES TO LABEL-1.                                       00  INPT
224780     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224790     MOVE SPACE TO BUFFER.                                        00  INPT
224800     MOVE "4" TO TYPE-RECORD.                                     00  INPT
224810     MOVE "A" TO SUB-TYPE.                                        00  INPT
224820     MOVE PFRMINDX TO ACTIONS.                                    00  INPT
224830     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224840     MOVE SPACE TO BUFFER.                                        00  INPT
224850     GO TO G52-LOOP-PERFORMABLES.                                 00  INPT
224860                                                                  00  INPT
224870                                                                  00  INPT
224880 G55-FINISH-UP.                                                   00  INPT
224890     MOVE "6" TO TYPE-RECORD.                                     00  INPT
224900     MOVE "DE" TO LABEL-TYPE.                                     00  INPT
224910     MOVE "A" TO SUB-TYPE.                                        00  INPT
224920     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
224930     MOVE SPACE TO BUFFER.                                        00  INPT
224940     MOVE STKINDX TO OUTINDEX.                                    00  INPT
224950     IF SPILL-INDEX GREATER THAN ZERO                             00  INPT
224960         PERFORM H90-SPILL-BENTRY THRU H95-SPILL-BENTRY-EXIT.     00  INPT
224970     IF NBR-CONDS NOT EQUAL TO ZERO                               00  INPT
224980         GO TO G60-DECOBOLIZE.                                    00  INPT
224990     IF NBR-ACTIONS EQUAL TO ZERO                                 00  INPT
225000         GO TO A20-INCR-TABLE-NO.                                 00  INPT
225010                                                                  00  INPT
225020 G60-DECOBOLIZE.                                                  00  INPT
225030     IF DOPTS-SW-DCP NOT EQUAL TO ZERO                            00  INPT
225040         GO TO G61-TRANSLATE-DCP.                                 00  INPT
225050     IF WRITEIND LESS THAN "2"                                    00  INPT
225060         GO TO A20-INCR-TABLE-NO.                                 00  INPT
225070                                                                  00  INPT
225080 G61-TRANSLATE-DCP.                                               00  INPT
225090     PERFORM Q00-PRODUCE-COBOL-CODE THRU                          00  INPT
225100             T99-PRODUCE-COBOL-EXIT.                              00  INPT
225110     GO TO A20-INCR-TABLE-NO.                                     00  INPT
225120                                                                  00  INPT
225130                                                                  00  INPT
225140                                                                  00  INPT
225150 G75-PERFORM-ANALYSIS.                                            00  INPT
225160     MOVE ZEROS TO AROW.                                          00  INPT
225170     MOVE ZEROS TO PASTELEM.                                      00  INPT
225180     MOVE ZEROS TO PFRMINDX.                                      00  INPT
225190                                                                  00  INPT
225200 G77-START-SECONDARY-LOOP.                                        00  INPT
225210     ADD 001 TO AROW.                                             00  INPT
225220     IF AROW GREATER THAN NACTNS                                  00  INPT
225230         GO TO G83-SECONDARY-LOOP-3.                              00  INPT
225240     MOVE AMATRIX (AROW, TCOL) TO CURRELEM.                       00  INPT
225250     IF CURRELEM EQUAL TO ZERO                                    00  INPT
225260         GO TO G77-START-SECONDARY-LOOP.                          00  INPT
  225270     IF PASTELEM EQUAL TO ZERO                                    00  INPT
225280         GO TO G79-SECONDARY-LOOP-1.                              00  INPT
225290     IF CURRELEM EQUAL TO TESTELEM                                00  INPT
225300         GO TO G81-SECONDARY-LOOP-2.                              00  INPT
225310     PERFORM G90-PERFORM-OUTPUT THRU H00-PERFORM-OUTPUT-EXIT.     00  INPT
225320                                                                  00  INPT
225330                                                                  00  INPT
225340 G79-SECONDARY-LOOP-1.                                            00  INPT
225350     MOVE CURRELEM TO PASTELEM.                                   00  INPT
225360     MOVE CURRELEM TO TESTELEM.                                   00  INPT
225370                                                                  00  INPT
225380 G81-SECONDARY-LOOP-2.                                            00  INPT
225390     IF PERFORM-NUMBER NOT EQUAL TO ZERO                          00  INPT
225400         ADD 1 TO TESTELEM.                                       00  INPT
225410     ADD 1 TO PFRMINDX.                                           00  INPT
225420     GO TO G77-START-SECONDARY-LOOP.                              00  INPT
225430                                                                  00  INPT
225440 G83-SECONDARY-LOOP-3.                                            00  INPT
225450     IF PFRMINDX EQUAL TO ZERO                                    00  INPT
225460         GO TO G85-PERF-ANAL-EXIT.                                00  INPT
225470     PERFORM G90-PERFORM-OUTPUT THRU H00-PERFORM-OUTPUT-EXIT.     00  INPT
225480                                                                  00  INPT
225490 G85-PERF-ANAL-EXIT.                                              00  INPT
225500     EXIT.                                                        00  INPT
225510                                                                  00  INPT
225520                                                                  00  INPT
225530                                                                  00  INPT
225540 G90-PERFORM-OUTPUT.                                              00  INPT
225550     MOVE ZERO TO CNTR-3.                                         00  INPT
225560     IF PERFORM-NUMBER EQUAL TO ZERO                              00  INPT
225570         GO TO G96-PERFORM-04.                                    00  INPT
225580     IF PFRMINDX LESS THAN PERFORM-NUMBER                         00  INPT
225590         GO TO G96-PERFORM-04.                                    00  INPT
225600     MOVE PASTELEM TO CNTR-2.                                     00  INPT
225610                                                                  00  INPT
225620 G92-PERFORM-01.                                                  00  INPT
225630     ADD 001 TO CNTR-3.                                           00  INPT
225640     IF CNTR-3 GREATER THAN PFRMINDX                              00  INPT
225650         GO TO G94-PERFORM-02.                                    00  INPT
225660     MOVE 1 TO INDARRAY (CNTR-2).                                 00  INPT
225670     ADD 001 TO CNTR-2.                                           00  INPT
225680     GO TO G92-PERFORM-01.                                        00  INPT
225690                                                                  00  INPT
225700 G94-PERFORM-02.                                                  00  INPT
225710     MOVE "4" TO TYPE-RECORD.                                     00  INPT
225720     MOVE "E" TO SUB-TYPE.                                        00  INPT
225730     ADD NRULES TO PASTELEM.                                      00  INPT
225740     MOVE PASTELEM TO Y-BRANCH.                                   00  INPT
225750     ADD NRULES TO TESTELEM.                                      00  INPT
225760     SUBTRACT 1 FROM TESTELEM.                                    00  INPT
225770     MOVE TESTELEM TO N-BRANCH.                                   00  INPT
225780     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
225790     GO TO G99-PERFORM-CLEAR.                                     00  INPT
225800                                                                  00  INPT
225810 G96-PERFORM-04.                                                  00  INPT
225820     IF PFRMINDX EQUAL TO ZERO                                    00  INPT
225830         GO TO G99-PERFORM-CLEAR.                                 00  INPT
225840     ADD 001 TO CNTR-3.                                           00  INPT
225850     MOVE "4" TO TYPE-RECORD.                                     00  INPT
225860     MOVE "A" TO SUB-TYPE.                                        00  INPT
225870     MOVE PASTELEM TO ACTIONS.                                    00  INPT
225880     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
225890                                                                  00  INPT
225900 G98-PERFORM-05.                                                  00  INPT
225910     ADD 1 TO CNTR-3.                                             00  INPT
225920     IF CNTR-3 GREATER THAN PFRMINDX                              00  INPT
225930         GO TO G99-PERFORM-CLEAR.                                 00  INPT
225940     ADD 1 TO PASTELEM.                                           00  INPT
225950     MOVE PASTELEM TO ACTIONS.                                    00  INPT
225960     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
225970     GO TO G98-PERFORM-05.                                        00  INPT
225980                                                                  00  INPT
225990 G99-PERFORM-CLEAR.                                               00  INPT
226000     MOVE SPACES TO BUFFER.                                       00  INPT
226010     MOVE ZERO TO PFRMINDX.                                       00  INPT
226020                                                                  00  INPT
226030 H00-PERFORM-OUTPUT-EXIT.                                         00  INPT
226040     EXIT.                                                        00  INPT
226050                                                                  00  INPT
226060                                                                  00  INPT
226070                                                                  00  INPT
226080                                                                  00  INPT
226090 H10-OPTIONS-WRITE.                                               00  INPT
226100     IF WRITEIND EQUAL TO "1"                                     00  INPT
226110         MOVE "YS" TO OM-PRT                                      00  INPT
226120         MOVE "NO" TO OM-CMP                                      00  INPT
226130         GO TO H12-MOVE-OPTN-MSSG.                                00  INPT
226140     IF WRITEIND EQUAL TO "2"                                     00  INPT
226150         MOVE "NO" TO OM-PRT                                      00  INPT
226160         MOVE "YS" TO OM-CMP                                      00  INPT
226170         GO TO H12-MOVE-OPTN-MSSG.                                00  INPT
226180     MOVE "YS" TO OM-PRT.                                         00  INPT
226190     MOVE "YS" TO OM-CMP.                                         00  INPT
226200                                                                  00  INPT
226210 H12-MOVE-OPTN-MSSG.                                              00  INPT
226220     MOVE OPTIONS-MSSG TO ERROR-MSG.                              00  INPT
226230     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226233     MOVE OPTIONS-MSSG-2 TO ERROR-MSG.                            00  INPT
226234     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226240                                                                  00  INPT
226250 H15-OPTIONS-WRITE-EXIT.                                          00  INPT
226260     EXIT.                                                        00  INPT
226270                                                                  00  INPT
226280                                                                  00  INPT
226290 H20-RULE-ACTN-ERROR.                                             00  INPT
226300     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
226320     MOVE RLATER     TO ERROR-PRINT.                              00  INPT
226330     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226340                                                                  00  INPT
226350 H22-RULE-ACTN-ERR-EXIT.                                          00  INPT
226360     EXIT.                                                        00  INPT
226370                                                                  00  INPT
226380                                                                  00  INPT
226390                                                                  00  INPT
226410                                                                  00  INPT
226500                                                                  00  INPT
226510                                                                  00  INPT
226540                                                                  00  INPT
226550 H30-SIZE-ERROR.                                                  00  INPT
226560     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
226580     MOVE LMTERROR   TO ERROR-PRINT.                              00  INPT
226590     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226600                                                                  00  INPT
226610 H32-SIZE-ERROR-EXIT.                                             00  INPT
226620     EXIT.                                                        00  INPT
226630                                                                  00  INPT
226640 H35-RULE-ERROR.                                                  00  INPT
226650     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
226670     MOVE RULERROR-1 TO ERROR-PRINT.                              00  INPT
226680     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226690     GO TO A20-INCR-TABLE-NO.                                     00  INPT
226700                                                                  00  INPT
226710 H40-NOACTCOND.                                                   00  INPT
226720     MOVE WARNING TO TYPEMSG.                                     00  INPT
226730     MOVE NOCONDACTN TO ERROR-PRINT.                              00  INPT
226740     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226750     MOVE "1" TO DOIND.                                           00  INPT
226760                                                                  00  INPT
226770 H42-NOACTCOND-EXIT.                                              00  INPT
226780     EXIT.                                                        00  INPT
226790                                                                  00  INPT
226800 H45-UNKNOWN-ERROR.                                               00  INPT
226810     MOVE TERM-INAL TO TYPEMSG.                                   00  INPT
226830     MOVE UNKERROR   TO ERROR-PRINT.                              00  INPT
226840     PERFORM H50-ERROR-WRITE THRU H52-ERROR-WRITE-EXIT.           00  INPT
226850     GO TO A20-INCR-TABLE-NO.                                     00  INPT
226860                                                                  00  INPT
226870                                                                  00  INPT
226880 H50-ERROR-WRITE.                                                 00  INPT
226890     MOVE ERROR-MSG TO BUFFER.                                    00  INPT
226900     MOVE "1" TO ERROR-IND.                                       00  INPT
226910     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
226920     MOVE SPACE TO ERROR-MSG.                                     00  INPT
226930                                                                  00  INPT
226940 H52-ERROR-WRITE-EXIT.                                            00  INPT
226950     EXIT.                                                        00  INPT
226960                                                                  00  INPT
226970                                                                  00  INPT
227000 H55-GO-TO-DEXIT.                                                 00  INPT
227010     MOVE "6" TO TYPE-RECORD.                                     00  INPT
227020     MOVE "B" TO SUB-TYPE.                                        00  INPT
227030     MOVE  5  TO Y-BRANCH-IND.                                    00  INPT
227040     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
227050     MOVE SPACE TO BUFFER.                                        00  INPT
227060                                                                  00  INPT
227070                                                                  00  INPT
227080 H60-AT-LABEL-OUTPUT.                                             00  INPT
227090     MOVE "AT" TO LABEL-TYPE.                                     00  INPT
227100     IF TRACE-IND EQUAL TO "1"                                    00  INPT
227110         MOVE CNTR-7 TO LABEL-1                                   00  INPT
227120             ELSE MOVE CNTR-6 TO LABEL-1.                         00  INPT
227130     PERFORM H65-LABEL-WRITE.                                     00  INPT
227140                                                                  00  INPT
227150 H62-AT-LBL-EXIT.                                                 00  INPT
227160     EXIT.                                                        00  INPT
227170                                                                  00  INPT
227180                                                                  00  INPT
227190 H65-LABEL-WRITE.                                                 00  INPT
227200     MOVE "6" TO TYPE-RECORD.                                     00  INPT
227210     MOVE "A" TO SUB-TYPE.                                        00  INPT
227220     PERFORM H80-WRITE-TEMP THRU H82-WRITE-TEMP-EXIT.             00  INPT
227230     MOVE SPACE TO BUFFER.                                        00  INPT
227240                                                                  00  INPT
227250 H70-PUT-OUT-DT-LBL.                                              00  INPT
227280     MOVE "DT00000000." TO LABEL-OUTS.                            00  INPT
227290     MOVE TBLNUM TO TNUMB.                                        00  INPT
227300     MOVE DXNUM TO POSTFIX.                                       00  INPT
227310     ADD 01 TO DXNUM.                                             00  INPT
227320     PERFORM H75-IDENTS-OUT.                                      00  INPT
227330     MOVE "1" TO TMPID.                                           00  INPT
227340     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
227350     MOVE ZERO TO TMPID.                                          00  INPT
227360                                                                  00  INPT
227370 H72-PUT-OUT-DT-LBL-EXIT.                                         00  INPT
227380     EXIT.                                                        00  INPT
227390                                                                  00  INPT
227400                                                                  00  INPT
227410 H75-IDENTS-OUT.                                                  00  INPT
227420     ADD INCREMENT TO SEQUENCE-NO.                                00  INPT
227422     IF  DOPTS-SW-SEQ  EQUAL TO  "0"                              00  INPT
227423         MOVE SPACES TO  TSEQ-NO-X                                00  INPT
227424       ELSE                                                       00  INPT
227430     MOVE SEQUENCE-NO-X TO TSEQ-NO-X.                             00  INPT
227440     MOVE IDENTFIELD TO TIDENT-FIELD.                             00  INPT
227450                                                                  00  INPT
227460                                                                  00  INPT
227470                                                                  00  INPT
227480 H80-WRITE-TEMP.                                                  00  INPT
227490     ADD 001 TO STKINDX.                                          00  INPT
227500     IF STKINDX GREATER THAN INTSIZE                              00  INPT
227510         PERFORM H90-SPILL-BENTRY THRU H95-SPILL-BENTRY-EXIT.     00  INPT
227520     MOVE BDUM TO BENTRY (STKINDX).                               00  INPT
227530                                                                  00  INPT
227540 H82-WRITE-TEMP-EXIT.                                             00  INPT
227550     EXIT.                                                        00  INPT
227560                                                                  00  INPT
227570                                                                  00  INPT
227580                                                                  00  INPT
227590 H90-SPILL-BENTRY.                                                00  INPT
227600     MOVE 001 TO X.                                               00  INPT
227610     IF SPILL-INDEX GREATER THAN ZERO                             00  INPT
227620         GO TO H92-SPILL-IS-OPEN.                                 00  INPT
227630     MOVE "2" TO TYPE-IO.                                         00  INPT
227640     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
227650                                 NOTE OPEN WORK FILE AS OUTPUT.   00  INPT
227660                                                                  00  INPT
227670 H92-SPILL-IS-OPEN.                                               00  INPT
227680     PERFORM H93-DO-WRITE-A-SPILL 7 TIMES.                        00  INPT
227690     MOVE STKINDX TO COUNT-IN-SPILL.                              00  INPT
227700     SUBTRACT 001 FROM COUNT-IN-SPILL.                            00  INPT
227710                                                                  00  INPT
227720 H93-DO-WRITE-A-SPILL.                                            00  INPT
227722     MOVE "3" TO TYPE-IO.                                         00  INPT
227730     MOVE SPILL (X) TO FORMAT-WORK-FILE-BUFFER.                   00  INPT
227740     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
227760     ADD 001 TO X.                                                00  INPT
227770                                                                  00  INPT
227780 H94-DONE-SPILLING.                                               00  INPT
227790     ADD 001 TO SPILL-INDEX.                                      00  INPT
227800     MOVE 001 TO STKINDX.                                         00  INPT
227810                                                                  00  INPT
227820 H95-SPILL-BENTRY-EXIT.                                           00  INPT
227830     EXIT.                                                        00  INPT
227840                                                                  00  INPT
227850                                                                  00  INPT
227860                                                                  00  INPT
227870 I00-LINE-COUNT.                                                  00  INPT
227880     IF LNECNT EQUAL TO SPACE                                     00  INPT
227890         GO TO I02-LINE-COUNT-EXIT.                               00  INPT
227900     IF LNECNT LESS THAN "00"                                     00  INPT
227910         GO TO I02-LINE-COUNT-EXIT.                               00  INPT
227920     IF LNECNT GREATER THAN "99"                                  00  INPT
227930         GO TO I02-LINE-COUNT-EXIT.                               00  INPT
227940     MOVE LNE1NO TO LINECNT.                                      00  INPT
227950                                                                  00  INPT
227960 I02-LINE-COUNT-EXIT.                                             00  INPT
227970     EXIT.                                                        00  INPT
227980                                                                  00  INPT
227990                                                                  00  INPT
228000 I10-WRITE-OUTPUT.                                                00  INPT
228010     IF TYPEMSG EQUAL TO TERM-INAL                                00  INPT
228020         ADD 001 TO TERM-ERR-COUNT.                               00  INPT
228030     IF TYPEMSG EQUAL TO WARNING                                  00  INPT
228040         ADD 001 TO WARN-ERR-COUNT.                               00  INPT
228050     IF SEQNUM-SAVE NOT EQUAL TO ZERO                             00  INPT
228060         MOVE SEQNUM-SAVE TO SEQNUM-X                             00  INPT
228070         MOVE ZERO TO SEQNUM-SAVE.                                00  INPT
228080     IF ERROR-IND EQUAL TO "1"                                    00  INPT
228090         GO TO I15-WRITE-A-LINE.                                  00  INPT
228100     IF DOPT-IN-RPL-SW EQUAL TO "1"                               00  INPT
228110         GO TO I12-DOPTS-IN-RPL-1.                                00  INPT
228120     IF DOPTS-SW-RPL EQUAL TO "0"                                 00  INPT
228130         GO TO I20-BYPASS-WRITE-LIST.                             00  INPT
228140     GO TO I13-TEST-WRITEIND.                                     00  INPT
228150                                                                  00  INPT
228160 I12-DOPTS-IN-RPL-1.                                              00  INPT
228170     IF DOPTS-SW-DCP EQUAL TO "0"                                 00  INPT
228180         GO TO I20-BYPASS-WRITE-LIST.                             00  INPT
228190                                                                  00  INPT
228200 I13-TEST-WRITEIND.                                               00  INPT
228210     IF WRITEIND EQUAL TO "2"                                     00  INPT
228220         GO TO I25-PREPARE-TO-PUNCH.                              00  INPT
228230     IF OP-FLAG EQUAL TO "B"                                      00  INPT
228240         MOVE OP-FLAG TO BND-IND-4-REPL.                          00  INPT
228250     MOVE SPACE TO OP-FLAG.                                       00  INPT
228260                                                                  00  INPT
228270 I15-WRITE-A-LINE.                                                00  INPT
228280     MOVE WRITE-LIST TO TYPE-IO.                                  00  INPT
228290     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
228300                                                                  00  INPT
228310 I20-BYPASS-WRITE-LIST.                                           00  INPT
228320     IF TEST-ID EQUAL TO "TEND."                                  00  INPT
228330         MOVE "1" TO DOPT-IN-RPL-SW                               00  INPT
228340         GO TO I25-PREPARE-TO-PUNCH.                              00  INPT
228350     IF ERROR-IND EQUAL TO "1"                                    00  INPT
228360         GO TO I25-PREPARE-TO-PUNCH.                              00  INPT
228370     IF DOPTS-SW-FMT NOT EQUAL TO "1"                             00  INPT
228380         GO TO I25-PREPARE-TO-PUNCH.                              00  INPT
228390     PERFORM W00-FORMATTED-TABLE THRU                             00  INPT
228400             Y99-FORMATTED-TABLE-EXIT.                            00  INPT
228410                                                                  00  INPT
228420 I25-PREPARE-TO-PUNCH.                                            00  INPT
228430     IF ERROR-IND EQUAL TO "1"                                    00  INPT
228440         MOVE ZERO TO ERROR-IND                                   00  INPT
228450         GO TO I40-CLEAR-BUFFER.                                  00  INPT
228460     IF WRITEIND EQUAL TO "1"                                     00  INPT
228470         GO TO I40-CLEAR-BUFFER.                                  00  INPT
228500     IF DETAP-IND NOT EQUAL TO "1"                                00  INPT
228510         GO TO I35-NOT-REPLICA.                                   00  INPT
228520     IF TMPID EQUAL TO "1"                                        00  INPT
228530         GO TO I35-NOT-REPLICA.                                   00  INPT
228540                                                                  00  INPT
228550 I30-ADJUST-REPLICA.                                              00  INPT
228560     MOVE IDENTIFIER TO IDENT-OUT.                                00  INPT
228570     MOVE FIRST-PRT  TO PART-ONE.                                 00  INPT
228580     MOVE SECOND-PRT TO PART-TWO.                                 00  INPT
228590     MOVE SPACE      TO SPACE-OUT.                                00  INPT
228600                                                                  00  INPT
228610 I35-NOT-REPLICA.                                                 00  INPT
228620     IF TEST-ID EQUAL TO SPACE                                    00  INPT
228630         GO TO I37-TRY-TO-PUNCH.                                  00  INPT
228640     IF TEST-ID EQUAL TO "DETAP"                                  00  INPT
228650         GO TO I37-TRY-TO-PUNCH.                                  00  INPT
228660     IF TEST-ID EQUAL TO "TEND."                                  00  INPT
228670         GO TO I37-TRY-TO-PUNCH.                                  00  INPT
228680     GO TO I40-CLEAR-BUFFER.                                      00  INPT
228690                                                                  00  INPT
228700 I37-TRY-TO-PUNCH.                                                00  INPT
228710     MOVE ZERO TO RESULT.                                         00  INPT
228720     IF NOPUNCH EQUAL TO "1"                                      00  INPT
228730         GO TO I40-CLEAR-BUFFER.                                  00  INPT
228740     MOVE WRITE-PUNCH TO TYPE-IO.                                 00  INPT
228750     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
228760                                                                  00  INPT
228770 I40-CLEAR-BUFFER.                                                00  INPT
228780     MOVE SPACE TO BUFFER.                                        00  INPT
228790                                                                  00  INPT
228800 I45-WRITE-OUTPUT-EXIT.                                           00  INPT
228810     EXIT.                                                        00  INPT
228820                                                                  00  INPT
228830                                                                  00  INPT
228840                                                                  00  INPT
228850                                                                  00  INPT
229000 J00-END-OF-FILE.                                                 00  INPT
229010     IF FILE-SECT NOT EQUAL TO "FILE END"                         00  INPT
229020         GO TO A25-FIND-NEXT-TABLE.                               00  INPT
229030 J05-EOF-ERR-RECAP-0.                                             00  INPT
229040     IF QX EQUAL TO 012                                           00  INPT
229050         GO TO J10-HAVE-QX.                                       00  INPT
229060     IF TERM-ERR-COUNT GREATER THAN ZERO                          00  INPT
229070         MOVE 012 TO QX                                           00  INPT
229080         GO TO J10-HAVE-QX.                                       00  INPT
229090     IF WARN-ERR-COUNT GREATER THAN ZERO                          00  INPT
229100         MOVE 004 TO QX.                                          00  INPT
229110                                                                  00  INPT
229120 J10-HAVE-QX.                                                     00  INPT
229130     IF DOPTS-SW-RCP EQUAL TO ZERO                                00  INPT
229140         GO TO J20-EOF-ERR-RECAP-2.                               00  INPT
229150     IF WARN-ERR-COUNT EQUAL TO ZERO                              00  INPT
229160         GO TO J15-EOF-ERR-RECAP-1.                               00  INPT
229170     MOVE EOF-WARN-MSSG TO BUFFER.                                00  INPT
229180     MOVE "1" TO ERROR-IND.                                       00  INPT
229190     IF WRITEIND NOT EQUAL TO "2"                                 00  INPT
229200         MOVE EJECT-PAGE TO TYPE-IO                               00  INPT
229210         PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.     00  INPT
229220     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
229230                                                                  00  INPT
229240 J15-EOF-ERR-RECAP-1.                                             00  INPT
229250     IF TERM-ERR-COUNT EQUAL TO ZERO                              00  INPT
229260         GO TO J20-EOF-ERR-RECAP-2.                               00  INPT
229270     MOVE EOF-TERM-MSSG TO BUFFER.                                00  INPT
229280     MOVE "1" TO ERROR-IND.                                       00  INPT
229290     IF WARN-ERR-COUNT NOT EQUAL TO ZERO                          00  INPT
229300         GO TO J18-HAVE-EJECTED.                                  00  INPT
229310     IF WRITEIND NOT EQUAL TO "2"                                 00  INPT
229320         MOVE EJECT-PAGE TO TYPE-IO                               00  INPT
229330         PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.     00  INPT
229340                                                                  00  INPT
229350 J18-HAVE-EJECTED.                                                00  INPT
229360     PERFORM I10-WRITE-OUTPUT THRU I45-WRITE-OUTPUT-EXIT.         00  INPT
229370                                                                  00  INPT
229380 J20-EOF-ERR-RECAP-2.                                             00  INPT
229390     MOVE ZERO TO WARN-ERR-COUNT.                                 00  INPT
229400     MOVE ZERO TO TERM-ERR-COUNT.                                 00  INPT
229410                                                                  00  INPT
229420 J25-EOF-STOP-RUN.                                                00  INPT
229430     MOVE " END OF DETAP/IMI DECISION TABLE PREPROCESSOR RUN."    00  INPT
229440         TO BUFFER.                                               00  INPT
229450     MOVE WRITE-LIST TO TYPE-IO.                                  00  INPT
229460     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
229470     MOVE CLOSE-IO TO TYPE-IO.                                    00  INPT
229480     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         00  INPT
229490                                                                  PDP-10
229500      OPEN OUTPUT FORMAT-WORK-FILE.                               PDP-10
229510      CLOSE FORMAT-WORK-FILE WITH DELETE.                         PDP-10


                NOTE  ***  TYMESHARE CHANGES TO ATTEMPT TO RETURN
                        TO COBAID IF THIS PROGRAM WAS CALLED
                        BY THAT PACKAGE;  THIS PROGRAM MUST BE
                        LOADED WITH THE MACRO CODE FILE ASOCDE.

                ENTER MACRO  COBAID .

229515                                                                  PDP-10
229516                                                                  PDP-10
229520     STOP RUN.                                                    00  INPT
400020 K00-DECOMPOSE-TABLE.                                             00  INPT
400120     MOVE OUTINDEX TO STKINDX.                                    01  DCMP
400140     MOVE ZERO TO NSCNT.                                          01  DCMP
400150     MOVE ZERO TO LSCNT.                                          01  DCMP
400170     MOVE ZERO TO LIMIT-1.                                        01  DCMP
400180     MOVE ZERO TO CNTER.                                          01  DCMP
400190     MOVE ZERO TO YCNT.                                           01  DCMP
400200     MOVE ZERO TO CNTR-1.                                         01  DCMP
400210     MOVE 050  TO CNTR-2.                                         01  DCMP
400220     MOVE 100  TO CNTR-3.                                         01  DCMP
400230                                                                  01  DCMP
400240 K05-INITIALIZE.                                                  01  DCMP
400250     ADD 001 TO CNTR-1.                                           01  DCMP
400260     ADD 001 TO CNTR-2.                                           01  DCMP
400270     ADD 001 TO CNTR-3.                                           01  DCMP
400280     MOVE ZERO TO NSTACK (CNTR-1).  NOTE CONDITION-NODE-STACK PT1.01  DCMP
400290     MOVE ZERO TO NSTACK (CNTR-2).                                01  DCMP
400300     MOVE ZERO TO NSTACK (CNTR-3).                                01  DCMP
400310     MOVE ZERO TO CARRAY (CNTR-1).  NOTE CONDITION-ARRAY-VECTOR.  01  DCMP
400320     IF CNTR-1 LESS THAN 050                                      01  DCMP
400330         GO TO K05-INITIALIZE.                                    01  DCMP
400340     MOVE ZERO TO COND-NODE-SWS.    NOTE REST OF CONDITION-NODE   01  DCMP
400345                                      STACK.                      01  DCMP
400350     MOVE ZERO TO CNTR-1.                                         01  DCMP
400360                                                                  01  DCMP
400370 K10-CLEANUP.                                                     01  DCMP
400380     ADD 1 TO CNTR-1.                                             01  DCMP
400390     MOVE ZERO TO N-CNT (CNTR-1).                                 01  DCMP
400400     MOVE ZERO TO Y-CNT (CNTR-1).                                 01  DCMP
400410     MOVE ZERO TO TEMP-COL (CNTR-1).                              01  DCMP
400420     IF CNTR-1 LESS THAN 050                                      01  DCMP
400430         GO TO K10-CLEANUP.      NOTE K10 ALSO PERFORMED.         01  DCMP
400440                                                                  01  DCMP
400450 K10-CLEANUP-EXIT.                                                01  DCMP
400460     EXIT.                                                        01  DCMP
400470                                                                  01  DCMP
400480 K15-MORE-INITIALIZING.                                           01  DCMP
400490     MOVE ZERO TO ELSERULE.                                       01  DCMP
400500                                                                  01  DCMP
400510 K20-NARRAY-INIT.                                                 01  DCMP
400520     MOVE INARRAY-VECTOR TO N-ARRAY-VECTOR.                       01  DCMP
400530     MOVE NBR-RULES TO NCNT.                                      01  DCMP
400550     MOVE ZERO TO CNTR-1.                                         01  DCMP
400560                                                                  01  DCMP
400570 K22-NARRAY-INIT-LOOP.                                            01  DCMP
400580     ADD 001 TO CNTR-1.                                           01  DCMP
400590     MOVE ZERO TO YARRAY (CNTR-1).                                01  DCMP
400600     IF CNTR-1 LESS THAN 050                                      01  DCMP
400610         GO TO K22-NARRAY-INIT-LOOP.                              01  DCMP
400620                                                                  01  DCMP
400630 K25-NARRAY-INIT-EXIT.                                            01  DCMP
400640     EXIT.                                                        01  DCMP
400650                                                                  01  DCMP
400660 K30-STILL-MORE-INIT.                                             01  DCMP
400670     MOVE ZERO TO CCNT.                                           01  DCMP
400680                                                                  01  DCMP
400690 K35-CARRAY-INIT.                                                 01  DCMP
400700     ADD 001 TO CCNT.                                             01  DCMP
400710     MOVE CCNT TO CARRAY (CCNT).                                  01  DCMP
400720     IF CCNT EQUAL TO NBR-CONDS                                   01  DCMP
400730         GO TO K38-CARRAY-INIT-EXIT.                              01  DCMP
400740     GO TO K35-CARRAY-INIT.      NOTE CARRAY (1) CONTAINS 001     01  DCMP
400741                                      CARRAY (2) CONTAINS 002 ETC 01  DCMP
400742                                      CARRAY (NBR-CONDS) CONTAINS 01  DCMP
400743                                        NUMBER OF CONDITIONS IN   01  DCMP
400744                                        TABLE                     01  DCMP
400745                                      ALL CARRAY (X) WHERE X IS   01  DCMP
400746                                        LARGER THAN NBR-CONDS     01  DCMP
400747                                        CONTAIN ZERO.             01  DCMP
400750                                                                  01  DCMP
400760 K38-CARRAY-INIT-EXIT.                                            01  DCMP
400770     EXIT.                                                        01  DCMP
400780                                                                  01  DCMP
400790 K40-ENCORE.                                                      01  DCMP
400800     MOVE ZERO TO DESTACK-IND.                                    01  DCMP
400810     MOVE SPACE TO TYPE-1A-RECORD.                                01  DCMP
400820     IF CCNT EQUAL TO 001                                         01  DCMP
400830         GO TO K55-FINAL-COND-FOUND. NOTE ONLY 1 CONDITION.       01  DCMP
400840     IF NBR-RULES EQUAL TO 001                                    01  DCMP
400850         GO TO N50-ONE-RULE.                                      01  DCMP
400860                                                                  01  DCMP
400870                                                                  01  DCMP
400880 K45-FIND-COND-K.                                                 01  DCMP
400890     PERFORM L00-COND-CALC THRU L55-COND-CALC-EXIT.               01  DCMP
400891                                 NOTE CALCULATE DASH COUNT (WT-EXP01  DCMP
400892                                    AND WT)     ASTERISK COUNT    01  DCMP
400893                                            AND DELTA             01  DCMP
400894                                  FOR EACH CONDITION ROW OF TABLE.01  DCMP
400900     PERFORM L60-COND-SELECT THRU M85-COND-SELECT-EXIT.           01  DCMP
400901                                 NOTE SELECT CONDITION-K ON WHICH 01  DCMP
400902                                  YOU WANT TO DISCRIMINATE THIS   01  DCMP
400903                                  TIME.                           01  DCMP
400905     IF NSCNT GREATER THAN 150                                    01  DCMP
400906         GO TO P96-SIZE-ERROR.                                    01  DCMP
400910     MOVE 001 TO CNTR-1.                                          01  DCMP
400920     PERFORM N10-DECOMPOSE THRU N40-DECOMPOSE-EXIT.               01  DCMP
400930     IF YCNT NOT EQUAL TO ZERO                                    01  DCMP
400940         GO TO K50-HAVE-COND-K.                                   01  DCMP
400950     IF NCNT EQUAL TO ZERO                                        01  DCMP
400960         GO TO P95-TERMINAL-ERROR.                                01  DCMP
400970                                                                  01  DCMP
400980 K50-HAVE-COND-K.                                                 01  DCMP
400990     MOVE SPACE TO TYPE-1A-RECORD.                                01  DCMP
401000     MOVE "6" TO TYPE-RECORD.                                     01  DCMP
401010     MOVE "B" TO SUB-TYPE.                                        01  DCMP
401020     MOVE KTH-COND TO CONDITION.                                  01  DCMP
401030     GO TO N90-COND-CODE-GENERATE.                                01  DCMP
401040 K51-SUCCESSFUL-GEN.                                              01  DCMP
401050     IF CCNT NOT EQUAL TO 001                                     01  DCMP
401060         GO TO K70-RESET-FOR-NEXT-COND-K.                         01  DCMP
401070                                                                  01  DCMP
401080 K55-FINAL-COND-FOUND.                                            01  DCMP
401090     ADD 001 TO NSCNT.               NOTE HAVE LAST CONDITION.    01  DCMP
401100     IF NSCNT GREATER THAN 150                                    01  DCMP
401110         GO TO P96-SIZE-ERROR.                                    01  DCMP
401120     MOVE CARRAY (1) TO NSTACK (NSCNT).                           01  DCMP
401130     MOVE CARRAY (1) TO KTH-COND.                                 01  DCMP
401140     MOVE CARRAY (1) TO ROW.                                      01  DCMP
401150                                                                  01  DCMP
401160 K60-REC-GEN-FOR-FINAL-COND.                                      01  DCMP
401170     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
401180     MOVE KTH-COND TO CONDITION.                                  01  DCMP
401190     MOVE 001 TO CNTR-1.                                          01  DCMP
401200     PERFORM N10-DECOMPOSE THRU N40-DECOMPOSE-EXIT.               01  DCMP
401210     MOVE ZERO TO Y-BRANCH.                                       01  DCMP
401220     MOVE ZERO TO Y-BRANCH-IND.                                   01  DCMP
401230     MOVE ZERO TO N-BRANCH.                                       01  DCMP
401240     MOVE ZERO TO N-BRANCH-IND.                                   01  DCMP
401250     MOVE "6" TO TYPE-RECORD.                                     01  DCMP
401260     MOVE "B" TO SUB-TYPE.                                        01  DCMP
401270     GO TO N75-LAST-CONDITION.                                    01  DCM-
401280                                                                  01  DCMP
401290 K65-DO-NEXT-SUBTABLE.                                            01  DCMP
401300     GO TO P20-BACK-UP-TREE.                                      01  DCMP
401310                                                                  01  DCMP
401320                                                                  01  DCMP
401330                                                                  01  DCMP
401340 K70-RESET-FOR-NEXT-COND-K.                                       01  DCMP
401350     MOVE ZERO TO CNTR-1.                                         01  DCMP
401360     PERFORM K10-CLEANUP THRU K10-CLEANUP-EXIT.                   01  DCMP
401370     GO TO K45-FIND-COND-K.                                       01  DCMP
401371                                                                  01  DCMP
401372                                                                  01  DCMP
401373                                                                  01  DCMP
401374             NOTE L00 THRU L55 CALCULATES THE ASTERISK COUNT,*    01  DCMP
401375              * THE WEIGTED EXPICIT DASH COUNT, THE WEIGHTED *    01  DCMP
401376              * DASH COUNT   AND DELTA                       *    01  DCMP
401377              *  FOR EACH CONDITION ROW IN THE TABLE         .    01  DCMP
401378                                                                  01  DCMP
401379                                                                  01  DCMP
401380 L00-COND-CALC.                                                   01  DCMP
401390     MOVE ZERO TO CNTR-1.                                         01  DCMP
401400                                                                  01  DCMP
401410 L05-LOOP-TO-CLEAR.                                               01  DCMP
401420     ADD 001 TO CNTR-1.                                           01  DCMP
401430     MOVE ZERO TO DELTA (CNTR-1).                                 01  DCMP
401440     MOVE ZERO TO AST-CNT (CNTR-1).                               01  DCMP
401450     MOVE ZERO TO WE-DSH-CNT (CNTR-1).                            01  DCMP
401455     MOVE ZERO TO W-DASH-CNT (CNTR-1).                            01  DCMP
401460     IF CNTR-1 LESS THAN 050                                      01  DCMP
401470         GO TO L05-LOOP-TO-CLEAR.                                 01  DCMP
401480     MOVE ZERO TO ROW.                                            01  DCMP
401490                                                                  01  DCMP
401500 L10-CALC-STATISTICS-LOOP.                                        01  DCMP
401510     ADD 001 TO ROW.                                              01  DCMP
401520     PERFORM L15-CALC-STATS-1ROW THRU L40-END-CALC-STATS-1-ROW.   01  DCMP
401530     IF ROW EQUAL TO CCNT                                         01  DCMP
401540         GO TO L55-COND-CALC-EXIT.  NOTE HAVE DONE ALL COND ROWS. 01  DCMP
401550     GO TO L10-CALC-STATISTICS-LOOP.                              01  DCMP
401560                                                                  01  DCMP
401561                                                                  01  DCMP
401562             NOTE L15 THRU L40 CALCULATES THE ASTERISK COUNT *    01  DCMP
401563              *                               DASH COUNT     *    01  DCMP
401564              *                           AND DELTA          *    01  DCMP
401565              *  FOR A SINGLE CONDITION ROW                  .    01  DCMP
401566                                                                  01  DCMP
401570 L15-CALC-STATS-1ROW.                                             01  DCMP
401580     MOVE ZERO TO UTIL-1.                                         01  DCMP
401590     MOVE ZERO TO UTIL-2.                                         01  DCMP
401600     MOVE CARRAY (ROW) TO TROW.                                   01  DCMP
401610     MOVE ZERO TO COL.                                            01  DCMP
401620                                                                  01  DCMP
401630 L20-CALC-ST-1ROW-COL-CTL.                                        01  DCMP
401640     ADD 001 TO COL.                                              01  DCMP
401650     PERFORM L45-CALC-1-SLOT THRU L50-CALC-1-SLOT-EXIT.           01  DCMP
401660     IF COL EQUAL TO NCNT                                         01  DCMP
401670         GO TO L25-DONE-1ROW.                                     01  DCMP
401680     GO TO L20-CALC-ST-1ROW-COL-CTL.                              01  DCMP
401690                                                                  01  DCMP
401700 L25-DONE-1ROW.                                                   01  DCMP
401710     IF UTIL-1 NOT EQUAL TO ZERO                                  01  DCMP
401720         GO TO L30-CALC-DELTA-FOR-ROW.                            01  DCMP
401730     IF UTIL-2 NOT EQUAL TO ZERO                                  01  DCMP
401740         GO TO L30-CALC-DELTA-FOR-ROW.                            01  DCMP
401750     MOVE 060 TO UTIL-1.         NOTE PLUG A HIGH DELTA-NO Y OR   01  DCMP
401760                                         N IN ROW.                01  DCMP
401770     GO TO L34-PLUG-IN-DELTA.                                     01  DCMP
401780                                                                  01  DCMP
401790 L30-CALC-DELTA-FOR-ROW.                                          01  DCMP
401800     IF AST-CNT (TROW) GREATER THAN ZERO                          01  DCMP
401810         MOVE 001 TO UTIL-1                                       01  DCMP
401820         GO TO L34-PLUG-IN-DELTA.                                 01  DCMP
401821                                 NOTE DELTA IS                    01  DCMP
401822                                   0 FOR ALL "Y" OR ALL "N" ROW   01  DCMP
401823                                   1 * OR $ IN ROW                01  DCMP
401824                                  60 ALL* AND $ ROW               01  DCMP
401825                                2-53 NO * OR $.                   01  DCMP
401826                                                                  01  DCMP
401830     IF UTIL-1 EQUAL TO ZERO                                      01  DCMP
401840         GO TO L36-ALL-YN.                                        01  DCMP
401850     IF UTIL-2 EQUAL TO ZERO                                      01  DCMP
401860         GO TO L36-ALL-YN.                                        01  DCMP
401870                                                                  01  DCMP
401880 L32-NOT-ALL-YN.                                                  01  DCMP
401885     SUBTRACT UTIL-2 FROM UTIL-1.                                 01  DCMP
401890     IF UTIL-1 LESS THAN ZERO                                     01  DCMP
401900         MULTIPLY UTIL-1 BY -0001 GIVING UTIL-1.                  01  DCMP
401910     SUBTRACT UTIL-1 FROM 053 GIVING UTIL-1.                      01  DCMP
401920                                                                  01  DCMP
401930 L34-PLUG-IN-DELTA.                                               01  DCMP
401940     MOVE UTIL-1 TO DELTA (TROW).                                 01  DCMP
401950     GO TO L40-END-CALC-STATS-1-ROW.                              01  DCMP
401960                                                                  01  DCMP
401970 L36-ALL-YN.                                                      01  DCMP
401980     MOVE ZERO TO UTIL-1.                                         01  DCMP
401990     GO TO L34-PLUG-IN-DELTA.                                     01  DCMP
402000                                                                  01  DCMP
402010 L40-END-CALC-STATS-1-ROW.                                        01  DCMP
402020     EXIT.                                                        01  DCMP
402030                                                                  01  DCMP
402040                                                                  01  DCMP
402041             NOTE L45 THRU L50 ANALYSES A SINGLE ENTRY       *    01  DCMP
402042              *    WITHIN A CONDITION ROW                    .    01  DCMP
402043                                                                  01  DCMP
402044                                                                  01  DCMP
402050                                                                  01  DCMP
402060 L45-CALC-1-SLOT.                                                 01  DCMP
402070     MOVE NARRAY (COL) TO TCOL.                                   01  DCMP
402080     MOVE CMATRIX (TROW, TCOL) TO ELEMENT1.                       01  DCMP
402090     IF ELEMENT1 EQUAL TO "Y"                                     01  DCMP
402100         ADD 001 TO UTIL-1                                        01  DCMP
402110         GO TO L50-CALC-1-SLOT-EXIT.                              01  DCMP
402120     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
402130         ADD 001 TO UTIL-2                                        01  DCMP
402140         GO TO L50-CALC-1-SLOT-EXIT.                              01  DCMP
402150     IF ELEMENT1  NOT EQUAL TO SPACE                              01  DCMP
402160         ADD FREQ (TCOL) TO AST-CNT (TROW)                        01  DCMP
402170         GO TO L50-CALC-1-SLOT-EXIT.                              01  DCMP
402180                                  NOTE HAVE SPACE SO CALCULATE    01  DCMP
402190          WEIGHTED-DASH-COUNT AND WEIGHTED-EXPLICIT-DASH-COUNT.   01  DCMP
402195     ADD FREQ (TCOL) TO W-DASH-CNT (TROW).                        01  DCMP
402200     MOVE ZERO TO N.                                              01  DCMP
402210                                                                  01  DCMP
402220 L46-CALC-WEDC-LOOP.                                              01  DCMP
402230     ADD 001 TO N.                                                01  DCMP
402240     IF  N GREATER THAN CCNT                                      01  DCMP
402250         GO TO L50-CALC-1-SLOT-EXIT.                              01  DCMP
402260     MOVE CARRAY (N) TO SU.                                       01  DCMP
402270     MOVE CMATRIX (SU, TCOL) TO ELEMENT1.                         01  DCMP
402280     IF ELEMENT1 EQUAL TO "Y"                                     01  DCMP
402300         GO TO L47-ADD-TO-WEDC.                                   01  DCMP
402310     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
402320         GO TO L47-ADD-TO-WEDC.                                   01  DCMP
402330     GO TO L46-CALC-WEDC-LOOP.                                    01  DCMP
402340                                                                  01  DCMP
402350 L47-ADD-TO-WEDC.                                                 01  DCMP
402360     ADD FREQ (TCOL) TO WE-DSH-CNT (TROW).                        01  DCMP
402370     GO TO L46-CALC-WEDC-LOOP.                                    01  DCMP
402380                                                                  01  DCMP
402390 L50-CALC-1-SLOT-EXIT.                                            01  DCMP
402400     EXIT.                                                        01  DCMP
402410                                                                  01  DCMP
402420 L55-COND-CALC-EXIT.                                              01  DCMP
402430     EXIT.                                                        01  DCMP
402440                                                                  01  DCMP
402450                                                                  01  DCMP
402460                                                                  01  DCMP
402500 L60-COND-SELECT.                                                 01  DCMP
402510     MOVE ZERO TO TEMP-CNT.                                       01  DCMP
402520                                                                  01  DCMP
402530 L65-SET-UP-WE-D-CTS-FOR-TBL.                                     01  DCMP
402540     ADD 001 TO TEMP-CNT.                                         01  DCMP
402550     MOVE CARRAY (TEMP-CNT) TO ROW.                               01  DCMP
402560     MOVE ROW TO N-CNT (TEMP-CNT).                                01  DCMP
402570     MOVE WE-DSH-CNT (ROW) TO Y-CNT (TEMP-CNT).                   01  DCMP
402580     IF TEMP-CNT LESS THAN CCNT                                   01  DCMP
402590         GO TO L65-SET-UP-WE-D-CTS-FOR-TBL.                       01  DCMP
402600     IF TEMP-CNT EQUAL TO 001                                     01  DCMP
402610         GO TO M70-HAVE-A-COND-K-CANDIDATE.                       01  DCMP
402620     MOVE ZERO TO ROW.                                            01  DCMP
402630                                                                  01  DCMP
402640 L70-SORT-WE-D-CTS.                                               01  DCMP
402650     ADD 001 TO ROW.                                              01  DCMP
402660     IF ROW EQUAL TO TEMP-CNT                                     01  DCMP
402670         GO TO L75-HAVE-SORTED-WE-D-CTS.                          01  DCMP
402680     PERFORM M90-SORT-Y-CNT-VECTOR THRU N05-SORT-Y-CNT-EXIT.      01  DCMP
402690     GO TO L70-SORT-WE-D-CTS.                                     01  4CMP
402700                                                                  01  DCMP
402710 L75-HAVE-SORTED-WE-D-CTS.                                        01  DCMP
402720     MOVE Y-CNT (1) TO LIMIT-1.                                   01  DCMP
402730     IF Y-CNT (1) NOT EQUAL TO Y-CNT (2)                          01  DCMP
402740         GO TO M70-HAVE-A-COND-K-CANDIDATE.  NOTE HAVE A SINGLE   01  DCMP
402750                            ROW MINIMUM EXPLICIT DASH COUNT.      01  DCMP
402760     MOVE ZERO TO TROW.                                           01  DCMP
402770     IF Y-CNT (1) NOT EQUAL TO ZERO                               01  DCMP
402780         MOVE ZERO TO CNTR-1                                      01  DCMP
402790         GO TO L80-SET-UP-W-DASH-CTS.                             01  DCMP
402800     GO TO  L95-SET-UP-DELTAS.         NOTE LOW DASH-CT IS ZERO.  01  DCMP
402810                                                                  01  DCMP
402820 L80-SET-UP-W-DASH-CTS.                                           01  DCMP
402830     ADD 001 TO CNTR-1.                                           01  DCMP
402840     MOVE N-CNT (CNTR-1) TO ROW.                                  01  DCMP
402850     IF LIMIT-1 NOT EQUAL TO Y-CNT (CNTR-1)                       01  DCMP
402855         MOVE ZERO TO ROW                                         01  DCMP
402856         SUBTRACT 001 FROM CNTR-1                                 01  DCMP
402857         MOVE CNTR-1 TO TEMP-CNT                                  01  DCMP
402860         GO TO L85-SORT-W-DASH-CTS. NOTE ONLY WANT W-DASH-CTS     01  DCMP
402870                                     FOR THE LOW WE-DASH-CTS.     01  DCMP
402880     MOVE W-DASH-CNT (ROW) TO Y-CNT (CNTR-1).                     01  DCMP
402890     IF CNTR-1 LESS THAN CCNT                                     01  DCMP
402900         GO TO L80-SET-UP-W-DASH-CTS.                             01  DCMP
402910     MOVE ZERO TO ROW.                                            01  DCMP
402915     MOVE CNTR-1 TO TEMP-CNT.                                     01  DCMP
402920                                                                  01  DCMP
402930 L85-SORT-W-DASH-CTS.                                             01  DCMP
402940     ADD 001 TO ROW.                                              01  DCMP
402950     IF ROW EQUAL TO TEMP-CNT                                     01  DCMP
402960         GO TO L90-HAVE-SORTED-W-DASH-CTS.                        01  DCMP
402970     PERFORM M90-SORT-Y-CNT-VECTOR THRU N05-SORT-Y-CNT-EXIT.      01  DCMP
402980     GO TO L85-SORT-W-DASH-CTS.                                   01  DCMP
402990                                                                  01  DCMP
403000 L90-HAVE-SORTED-W-DASH-CTS.                                      01  DCMP
403010     MOVE Y-CNT (1) TO LIMIT-1.                                   01  DCMP
403020     IF Y-CNT (1) NOT EQUAL TO Y-CNT (2)                          01  DCMP
403030         GO TO M70-HAVE-A-COND-K-CANDIDATE. NOTE HAVE A SINGLE    01  DCMP
403040                   ROW WITHIN LOW WEDC WHICH HAS MINIMUM WEIGHTED 01  DCMP
403050                       DASH COUNT.                                01  DCMP
403055     MOVE ZERO TO TROW.                                           01  DCMP
403060                                                                  01  DCMP
403070 L95-SET-UP-DELTAS.                                               01  DCMP
403080     ADD 001 TO TROW.        NOTE ITS ZERO FIRST TIME.            01  DCMP
403090     MOVE N-CNT (TROW) TO CNTER.                                  01  DCMP
403100     IF LIMIT-1 NOT EQUAL TO Y-CNT (TROW)                         01  DCMP
403105         MOVE ZERO TO ROW                                         01  DCMP
403106         SUBTRACT 001 FROM TROW                                   01  DCMP
403107         MOVE TROW TO TEMP-CNT                                    01  DCMP
403110         GO TO M00-SORT-DELTAS.    NOTE ONLY WANT DELTAS          01  DCMP
403120                                    FOR LOW DASH COUNTS.          01  DCMP
403130     MOVE DELTA (CNTER) TO Y-CNT (TROW).                          01  DCMP
403140     IF TROW LESS THAN TEMP-CNT                                   01  DCMP
403150         GO TO L95-SET-UP-DELTAS.                                 01  DCMP
403160     MOVE ZERO TO ROW.                                            01  DCMP
403165     MOVE TROW TO TEMP-CNT.                                       01  DCMP
403170                                                                  01  DCMP
403180 M00-SORT-DELTAS.                                                 01  DCMP
403190     ADD 001 TO ROW.                                              01  DCMP
403200     IF ROW EQUAL TO TEMP-CNT                                     01  DCMP
403210         GO TO M05-HAVE-SORTED-DELTAS.                            01  DCMP
403220     PERFORM M90-SORT-Y-CNT-VECTOR THRU N05-SORT-Y-CNT-EXIT.      01  DCMP
403230     GO TO M00-SORT-DELTAS.                                       01  DCMP
403240                                                                  01  DCMP
403250 M05-HAVE-SORTED-DELTAS.                                          01  DCMP
403260     MOVE Y-CNT (1) TO LIMIT-1.                                   01  DCMP
403270     IF LIMIT-1 EQUAL TO ZERO                                     01  DCMP
403280         GO TO M70-HAVE-A-COND-K-CANDIDATE. NOTE HAVE AT LEAST    01  DCMP
403290                                              ONE ZERO DELTA      01  DCMP
403300                                   MEANING AN ALL-Y OR ALL-N ROW. 01  DCMP
403310     MOVE ZERO TO N.                                              01  DCMP
403320     MOVE ZERO TO RELATED-FOUND.                                  01  DCMP
403330                                                                  01  DCMP
403340 M10-RELATED-TEST.                                                01  DCMP
403350     ADD 001 TO N.                                                01  DCMP
403360                                                                  01  DCMP
403370                                                                  01  DCMP
403380     IF N GREATER THAN TEMP-CNT                                   01  DCMP
403390         GO TO M55-DO-AST-CNT-CHECK.                              01  DCMP
403400     IF Y-CNT (N) GREATER THAN 001                                01  DCMP
403410         GO TO M45-WAS-RELATED-FOUND. NOTE HAVE DONE ALL ROWS     01  DCMP
403420                                   WITH * AND/OR $.               01  DCMP
403430     IF RELATED-FOUND EQUAL TO "2"                                01  DCMP
403440         GO TO M10-RELATED-TEST.                                  01  DCMP
403450     MOVE N TO P.                                                 01  DCMP
403460                                                                  01  DCMP
403470 M15-RELATED-TEST2.                                               01  DCMP
403480     ADD 001 TO P.                                                01  DCMP
403490                                                                  01  DCMP
403500                                                                  01  DCMP
403510     IF P GREATER THAN TEMP-CNT                                   01  DCMP
403520         GO TO M10-RELATED-TEST.                                  01  DCMP
403530     IF Y-CNT (P) NOT GREATER THAN 002                            01  DCMP
403540         GO TO M15-RELATED-TEST2.                                 01  DCMP
403550     MOVE ZERO TO Q.                                              01  DCMP
403560                                                                  01  DCMP
403570 M20-RELATED-TEST3.                                               01  DCMP
403580     ADD 001 TO Q.                                                01  DCMP
403590     IF Q NOT LESS THAN NCNT                                      01  DCMP
403600         GO TO M15-RELATED-TEST2.                                 01  DCMP
403610     MOVE N-CNT (N) TO SU.                                        01  DCMP
403620     MOVE CARRAY (SU) TO SU.                                      01  DCMP
403630     MOVE N-CNT (P) TO T.                                         01  DCMP
403640     MOVE CARRAY (T) TO T.                                        01  DCMP
403650     MOVE NARRAY (Q) TO U.                                        01  DCMP
403660     MOVE CMATRIX (SU, U) TO ELEMENT1.                            01  DCMP
403670     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
403680         GO TO M25-FOUND-Y-N.                                     01  DCMP
403690     IF ELEMENT1 EQUAL TO "Y"                                     01  DCMP
403700         GO TO M25-FOUND-Y-N.                                     01  DCMP
403710     GO TO M20-RELATED-TEST3.                                     01  DCMP
403720                                                                  01  DCMP
403730 M25-FOUND-Y-N.                                                   01  DCMP
403740     MOVE Q TO R.                                                 01  DCMP
403750                                                                  01  DCMP
403760 M30-RELATED-TEST4.                                               01  DCMP
403770     ADD 001 TO R.                                                01  DCMP
403780     IF R GREATER THAN NCNT                                       01  DCMP
403790         GO TO M20-RELATED-TEST3.                                 01  DCMP
403800     MOVE NARRAY (R) TO V.                                        01  DCMP
403810     MOVE CMATRIX (SU, V) TO ELEMENT2.                            01  DCMP
403820     IF ELEMENT1 NOT EQUAL TO ELEMENT2                            01  DCMP
403830         GO TO M30-RELATED-TEST4.                                 01  DCMP
403840     MOVE CMATRIX (T, U) TO ELEMENT3.                             01  DCMP
403850     MOVE CMATRIX (T, V) TO ELEMENT4.                             01  DCMP
403860     IF ELEMENT3 EQUAL TO "Y"                                     01  DCMP
403870         GO TO M35-3-IS-A-Y.                                      01  DCMP
403880     IF ELEMENT3 EQUAL TO "N"                                     01  DCMP
403890         GO TO M36-3-IS-A-N.                                      01  DCMP
403900     GO TO M30-RELATED-TEST4.                                     01  DCMP
403910                                                                  01  DCMP
403920 M35-3-IS-A-Y.                                                    01  DCMP
403930     IF ELEMENT4 EQUAL TO "N"                                     01  DCMP
403940         GO TO M40-MULT-CHK.                                      01  DCMP
403950     GO TO M30-RELATED-TEST4.                                     01  DCMP
403960                                                                  01  DCMP
403970 M36-3-IS-A-N.                                                    01  DCMP
403980     IF ELEMENT4 EQUAL TO "Y"                                     01  DCMP
403990         GO TO M40-MULT-CHK.                                      01  DCMP
404000     GO TO M30-RELATED-TEST4.                                     01  DCMP
404010                                                                  01  DCMP
404020 M40-MULT-CHK.                                                    01  DCMP
404030     IF RELATED-FOUND EQUAL TO "1"                                01  DCMP
404040         MOVE "2" TO RELATED-FOUND                                01  DCMP
404050         GO TO M10-RELATED-TEST.                                  01  DCMP
404060     MOVE N TO W.                                                 01  DCMP
404070     MOVE "1" TO RELATED-FOUND.                                   01  DCMP
404080     GO TO M15-RELATED-TEST2.                                     01  DCMP
404090                                                                  01  DCMP
404100 M45-WAS-RELATED-FOUND.                                           01  DCMP
404110     IF RELATED-FOUND EQUAL TO ZERO                               01  DCMP
404120         MOVE 002 TO N                                            01  DCMP
404130         GO TO M50-MAX-DELTA-CHECK.                               01  DCMP
404140     MOVE N-CNT (W) TO N-CNT (1).                                 01  DCMP
404150     MOVE Y-CNT (W) TO Y-CNT (1).                                 01  DCMP
404160     GO TO M70-HAVE-A-COND-K-CANDIDATE.                           01  DCMP
404170                                                                  01  DCMP
404180 M50-MAX-DELTA-CHECK.                                             01  DCMP
404190     IF Y-CNT (1) GREATER THAN 001                                01  DCMP
404200         GO TO M70-HAVE-A-COND-K-CANDIDATE.                       01  DCMP
404210     MOVE N-CNT (N) TO N-CNT (1).                                 01  DCMP
404220     MOVE Y-CNT (N) TO Y-CNT (1).                                 01  DCMP
404230     ADD 001 TO N.                                                01  DCMP
404240     GO TO M50-MAX-DELTA-CHECK.                                   01  DCMP
404250                                                                  01  DCMP
404260 M55-DO-AST-CNT-CHECK.                                            01  DCMP
404270     MOVE 000 TO TROW.                                            01  DCMP
404280     MOVE 001 TO LIMIT-1.                                         01  DCMP
404290                                                                  01  DCMP
404300 M60-SET-UP-AST-CNTS.                                             01  DCMP
404310     ADD 001 TO TROW.                                             01  DCMP
404320     MOVE N-CNT (TROW) TO CNTER.                                  01  DCMP
404330     IF LIMIT-1 NOT EQUAL TO Y-CNT (TROW)                         01  DCMP
404340         MOVE ZERO TO ROW                                         01  DCMP
404345         SUBTRACT 001 FROM TROW                                   01  DCMP
404346         MOVE TROW TO TEMP-CNT                                    01  DCMP
404350         GO TO M65-SORT-AST-CNTS.                                 01  DCMP
404360     MOVE AST-CNT (CNTER) TO Y-CNT (TROW).                        01  DCMP
404370     IF TROW LESS THAN CCNT                                       01  DCMP
404380         GO TO M60-SET-UP-AST-CNTS.                               01  DCMP
404390     MOVE ZERO TO ROW.                                            01  DCMP
404395     MOVE TROW TO TEMP-CNT.                                       01  DCMP
404400                                                                  01  DCMP
404410 M65-SORT-AST-CNTS.                                               01  DCMP
404420     ADD 001 TO ROW.                                              01  DCMP
404430     IF ROW EQUAL TO TEMP-CNT                                     01  DCMP
404440         GO TO M70-HAVE-A-COND-K-CANDIDATE.                       01  DCMP
404450     PERFORM M90-SORT-Y-CNT-VECTOR THRU N05-SORT-Y-CNT-EXIT.      01  DCMP
404460     GO TO M65-SORT-AST-CNTS.                                     01  DCMP
404470                                                                  01  DCMP
404480 M70-HAVE-A-COND-K-CANDIDATE.                                     01  DCMP
404490     MOVE ZERO TO ROW.                                            01  DCMP
404500                                                                  01  DCMP
404510 M75-SEARCH-KTH-COND.                                             01  DCMP
404520     ADD 001 TO ROW.                                              01  DCMP
404530     MOVE N-CNT (ROW) TO KTH-COND.                                01  DCMP
404540     IF DELTA (KTH-COND) NOT EQUAL TO 060                         01  DCMP
404550         GO TO M80-COND-K-OK.                                     01  DCMP
404560     IF ROW EQUAL TO CCNT                                         01  DCMP
404570         MOVE "1" TO SKIP-GEN-SW                                  01  DCMP
404580         GO TO M80-COND-K-OK.                                     01  DCMP
404590     GO TO M75-SEARCH-KTH-COND.                                   01  DCMP
404600                                                                  01  DCMP
404610 M80-COND-K-OK.                                                   01  DCMP
404620     ADD 001 TO NSCNT.                                            01  DCMP
404630     IF NSCNT GREATER THAN 150                                    01  DCMP
404640         GO TO M85-COND-SELECT-EXIT.                              01  DCMP
404650     MOVE KTH-COND TO NSTACK (NSCNT).                             01  DCMP
404660     MOVE KTH-COND TO ROW.                                        01  DCMP
404670                                                                  01  DCMP
404680 M85-COND-SELECT-EXIT.                                            01  DCMP
404690     EXIT.                                                        01  DCMP
404700                                                                  01  DCMP
404710                                                                  01  DCMP
404720                                                                  01  DCMP
404730 M90-SORT-Y-CNT-VECTOR.                                           01  DCMP
404740     MOVE ROW TO TROW.                                            01  DCMP
404750     ADD 001 TO TROW.   NOTE SORT Y-CNT VECTOR ENTRIES            01  DCMP
404760                         INTO ASCENDING SEQUENCE.                 01  DCMP
404770                                                                  01  DCMP
404780 M95-SORTING.                                                     01  DCMP
404790     MOVE Y-CNT (ROW)  TO HOLDER-1.                               01  DCMP
404800     MOVE Y-CNT (TROW) TO HOLDER-2.                               01  DCMP
404810     IF HOLDER-1 NOT GREATER THAN HOLDER-2                        01  DCMP
404820         GO TO N00-INCREMENT-TROW.                                01  DCMP
404830     MOVE HOLDER-1 TO Y-CNT (TROW).                               01  DCMP
404840     MOVE HOLDER-2 TO Y-CNT (ROW).                                01  DCMP
404850     MOVE N-CNT (ROW) TO HOLDER-1.                                01  DCMP
404860     MOVE N-CNT (TROW) TO N-CNT (ROW).                            01  DCMP
404870     MOVE HOLDER-1 TO N-CNT (TROW).                               01  DCMP
404880                                                                  01  DCMP
404890 N00-INCREMENT-TROW.                                              01  DCMP
404900     ADD 001 TO TROW.                                             01  DCMP
404910     IF TROW GREATER THAN TEMP-CNT                                01  DCMP
404920         GO TO N05-SORT-Y-CNT-EXIT.                               01  DCMP
404930     GO TO M95-SORTING.                                           01  DCMP
404940                                                                  01  DCMP
404950 N05-SORT-Y-CNT-EXIT.                                             01  DCMP
404960     EXIT.                                                        01  DCMP
404970                                                                  01  DCMP
404980                                                                  01  DCMP
404990                                                                  01  DCMP
405000 N10-DECOMPOSE.                                                   01  DCMP
405010     MOVE NCNT TO LIMIT-1.                                        01  DCMP
405020     MOVE ZERO TO YCNT.                                           01  DCMP
405030     MOVE ZERO TO NCNT.                                           01  DCMP
405040     IF DESTACK-IND EQUAL TO "1"                                  01  DCMP
405050         MOVE NSTACK (CNTR-1) TO KTH-COND.                        01  DCMP
405060     MOVE KTH-COND TO ROW.                                        01  DCMP
405070     MOVE ZERO TO CNTER.                                          01  DCMP
405080                                                                  01  DCMP
405090 N15-SPLIT-TABLE.                                                 01  DCMP
405100     ADD 001 TO CNTER.                                            01  DCMP
405110     PERFORM N25-WHICH-SIDE-THIS-RULE THRU N35-WHICH-SIDE-EXIT.   01  DCMP
405120     IF CNTER LESS THAN LIMIT-1                                   01  DCMP
405130         GO TO N15-SPLIT-TABLE.                                   01  DCMP
405140     MOVE CCNT TO LIMIT-1.                                        01  DCMP
405150     IF CCNT EQUAL TO 001                                         01  DCMP
405160         GO TO N40-DECOMPOSE-EXIT.                                01  DCMP
405170     MOVE ZERO TO CCNT.                                           01  DCMP
405180     MOVE COND-ARRAY-VECTOR TO TEMP-COL-C-VECTOR.                 01  DCMP
405190     MOVE ZERO TO CNTER.                                          01  DCMP
405200                                                                  01  DCMP
405210 N20-SETUP-NEW-COND-VECTOR.                                       01  DCMP
405220     ADD 001 TO CNTER.                                            01  DCMP
405225     MOVE TEMP-COL-C (CNTER) TO X.                                01  DCMP
405227     IF X NOT EQUAL TO KTH-COND                                   01  DCMP
405230         ADD 001 TO CCNT                                          01  DCMP
405232         MOVE X TO CARRAY (CCNT).                                 01  DCMP
405240     IF CNTER LESS THAN LIMIT-1                                   01  DCMP
405250         GO TO N20-SETUP-NEW-COND-VECTOR.                         01  DCMP
405260     IF DESTACK-IND NOT EQUAL TO "1"                              01  DCMP
405270         GO TO  N40-DECOMPOSE-EXIT.                               01  DCMP
405280     IF ARRAY-SW (CNTR-1) NOT EQUAL TO "1"                        01  DCMP
405290         GO TO  N40-DECOMPOSE-EXIT.                               01  DCMP
405300     PERFORM N45-YARRAY-SWITCH THRU N46-YARRAY-SWITCH-EXIT.       01  DCMP
405310     GO TO N40-DECOMPOSE-EXIT.                                    01  DCMP
405320                                                                  01  DCMP
405330                                                                  01  DCMP
405340 N25-WHICH-SIDE-THIS-RULE.                                        01  DCMP
405350     MOVE NARRAY (CNTER) TO COL.                                  01  DCMP
405360     MOVE CMATRIX (ROW, COL) TO ELEMENT1.                         01  DCMP
405370     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
405380         GO TO N30-SPLIT-TO-N-SIDE.                               01  DCMP
405390     IF ELEMENT1 EQUAL TO "*"                                     01  DCMP
405400         GO TO N30-SPLIT-TO-N-SIDE.                               01  DCMP
405410                                 NOTE Y AND $ SPLIT TO Y-SIDE     01  DCMP
405420                                      SPACE SPLITS BOTH WAYS.     01  DCMP
405430     ADD 001 TO YCNT.                                             01  DCMP
405440     MOVE COL TO YARRAY (YCNT).                                   01  DCMP
405450     IF ELEMENT1 NOT EQUAL TO SPACE                               01  DCMP
405460         GO TO N35-WHICH-SIDE-EXIT.                               01  DCMP
405480                                                                  01  DCMP
405490 N30-SPLIT-TO-N-SIDE.                                             01  DCMP
405500     ADD 001 TO NCNT                                              01  DCMP
405510     MOVE COL TO NARRAY (NCNT).                                   01  DCMP
   405520                                                                  01  DCMP
405530 N35-WHICH-SIDE-EXIT.                                             01  DCMP
405540     EXIT.                                                        01  DCMP
405550                                                                  01  DCMP
405560 N40-DECOMPOSE-EXIT.                                              01  DCMP
405570     EXIT.                                                        01  DCMP
405580                                                                  01  DCMP
405590                                                                  01  DCMP
405600                                                                  01  DCMP
405610 N45-YARRAY-SWITCH.                                               01  DCMP
405620     MOVE Y-ARRAY-VECTOR TO N-ARRAY-VECTOR.                       01  DCMP
405630     MOVE YCNT TO NCNT.                                           01  DCMP
405640     MOVE ZERO TO YCNT.                                           01  DCMP
405650     IF DESTACK-IND EQUAL TO "1"                                  01  DCMP
405660         GO TO N46-YARRAY-SWITCH-EXIT.                            01  DCMP
405670     MOVE "1" TO  ARRAY-SW (NSCNT).                               01  DCMP
405680                                                                  01  DCMP
405690 N46-YARRAY-SWITCH-EXIT.                                          01  DCMP
405700     EXIT.                                                        01  DCMP
405710                                                                  01  DCMP
405720                                                                  01  DCMP
405730                                                                  01  DCMP
405740 N50-ONE-RULE.                                                    01  DCMP
405750     MOVE ZERO TO Y1-IND.                                         01  DCMP
405760     MOVE ZERO TO N1-IND.                                         01  DCMP
405770     MOVE ZERO TO UTIL-1.                                         01  DCMP
405780     IF NCNT NOT EQUAL TO ZERO                                    01  DCMP
405790         MOVE NARRAY (1) TO COL                                   01  DCMP
405800         GO TO N55-ONE-RULE-LOOP.                                 01  DCMP
405810     MOVE YARRAY (1) TO COL.                                      01  DCMP
405820                                                                  01  DCMP
405830 N55-ONE-RULE-LOOP.                                               01  DCMP
405840     MOVE 001 TO YARRAY (1).                                      01  DCMP
405850     MOVE 001 TO NARRAY (1).                                      01  DCMP
405860     MOVE 001 TO YCNT.                                            01  DCMP
405870     MOVE 001 TO NCNT.                                            01  DCMP
405880     ADD 001 TO UTIL-1.                                           01  DCMP
405890     IF UTIL-1 GREATER THAN CCNT                                  01  DCMP
405900         GO TO N68-ONE-RULE-WINDUP.                               01  DCMP
405910     MOVE "6" TO TYPE-RECORD.                                     01  DCMP
405920     MOVE "B" TO SUB-TYPE.                                        01  DCMP
405930     ADD 001 TO NSCNT.                                            01  DCMP
405940     IF NSCNT GREATER THAN 150                                    01  DCMP
405950         GO TO P96-SIZE-ERROR.                                    01  DCMP
405960     MOVE CARRAY (UTIL-1) TO ROW.                                 01  DCMP
405970     MOVE ROW TO CONDITION.                                       01  DCMP
405980     MOVE ROW TO NSTACK (NSCNT).                                  01  DCMP
405990     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
406000     MOVE CMATRIX (ROW, COL) TO ELEMENT1.                         01  DCMP
406010     IF ELEMENT1 EQUAL TO "Y"                                     01  DCMP
406020         GO TO N60-ONE-RULE-BRANCH-YN-INIT.                       01  DCMP
406030     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
406040         GO TO N60-ONE-RULE-BRANCH-YN-INIT.                       01  DCMP
406050     GO TO N55-ONE-RULE-LOOP.                                     01  DCMP
406060                                                                  01  DCMP
406070 N60-ONE-RULE-BRANCH-YN-INIT.                                     01  DCMP
406080     MOVE UTIL-1 TO UTIL-2.                                       01  DCMP
406090                                                                  01  DCMP
406100 N62-ONE-RULE-BRANCH-YN-LOOP.                                     01  DCMP
406110     ADD 001 TO UTIL-2.                                           01  DCMP
406120     IF UTIL-2 GREATER THAN CCNT                                  01  DCMP
406130         GO TO N68-ONE-RULE-WINDUP.                               01  DCMP
406140     MOVE CMATRIX (UTIL-2, COL) TO ELEMENT2.                      01  DCMP
406150     IF ELEMENT2 EQUAL TO "Y"                                     01  DCMP
406160         GO TO N63-ONE-RULE-TESTYN.                               01  DCMP
406170     IF ELEMENT2 EQUAL TO "N"                                     01  DCMP
406180         GO TO N63-ONE-RULE-TESTYN.                               01  DCMP
406190     GO TO N62-ONE-RULE-BRANCH-YN-LOOP.                           01  DCMP
406200                                                                  01  DCMP
406205 N63-ONE-RULE-TESTYN.                                             01  DCMP
406206     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
406207         GO TO N66-ONE-RULE-TESTN.                                01  DCMP
406208                                                                  01  DCMP
406209                                                                  01  DCMP
406210 N64-ONE-RULE-TESTY.                                              01  DCMP
406220     ALTER O55-IF-CK-NS-ELS-EL-EXIT                               01  DCMP
406221         TO PROCEED TO N65.                                       01  DCMP
406225     GO TO O50-IF-CK-NS-ELS-EL.                                   01  DCMP
406226 N65.                                                             01  DCMP
406230     MOVE "1" TO ARRAY-SW (NSCNT).                                01  DCMP
406240     GO TO N55-ONE-RULE-LOOP.                                     01  DCMP
406250                                                                  01  DCMP
406260 N66-ONE-RULE-TESTN.                                              01  DCMP
406270     ALTER O45-IF-CK-EL-EXIT                                      01  DCMP
406271         TO PROCEED TO N55-ONE-RULE-LOOP.                         01  DCMP
406280     GO TO O40-IF-CK-EL.                                          01  DCMP
406290                                                                  01  DCMP
406300 N68-ONE-RULE-WINDUP.                                             01  DCMP
406310     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
406320         ALTER O75-IF-CK-EL-ELS-AZ-EXIT                           01  DCMP
406321             TO PROCEED TO N69-ONE-RULE-WRITE                     01  DCMP
406330         GO TO O70-IF-CK-EL-ELS-AZ.                               01  DCMP
406340     ALTER O65-IF-CK-AZ-ELS-EL-EXIT                               01  DCMP
406341         TO PROCEED TO N69-ONE-RULE-WRITE.                        01  DCMP
406345     GO TO O60-IF-CK-AZ-ELS-EL.                                   01  DCMP
406350                                                                  01  DCMP
406360 N69-ONE-RULE-WRITE.                                              01  DCMP
406370     PERFORM P05-WRITE-OUT THRU P08-WRITE-OUT-EXIT.               01  DCMP
406380                                                                  01  DCMP
406390 N70-ONE-RULE-EXIT.                                               01  DCMP
406400     GO TO P97-TABLE-END.                                         01  DCMP
406410                                                                  01  DCMP
406420                                                                  01  DCMP
406430                                                                  01  DCMP
406440 N75-LAST-CONDITION.                                              01  DCMP
406450     IF YCNT NOT EQUAL TO ZERO                                    01  DCMP
406460         GO TO N77-LAST-COND-HAS-Y.                               01  DCMP
406470     ALTER O75-IF-CK-EL-ELS-AZ-EXIT                               01  DCMP
406471         TO PROCEED TO N80-LAST-COND-DO-ACTIONS.                  01  DCMP
406480     GO TO O70-IF-CK-EL-ELS-AZ.                                   01  DCMP
406490                                                                  01  DCMP
406500 N77-LAST-COND-HAS-Y.                                             01  DCMP
406510     IF NCNT NOT EQUAL TO ZERO                                    01  DCMP
406520         GO TO N79-LAST-COND-HAS-YN.                              01  DCMP
406530     ALTER O65-IF-CK-AZ-ELS-EL-EXIT                               01  DCMP
406531         TO PROCEED TO N80-LAST-COND-DO-ACTIONS.                  01  DCMP
406540     GO TO O60-IF-CK-AZ-ELS-EL.                                   01  DCMP
406550                                                                  01  DCMP
406560 N79-LAST-COND-HAS-YN.                                            01  DCMP
406570     ALTER O85-IF-CK-AZ-ELS-AZ-EXIT                               01  DCMP
406571         TO PROCEED TO N80-LAST-COND-DO-ACTIONS.                  01  DCMP
406575     GO TO O80-IF-CK-AZ-ELS-AZ.                                   01  DCMP
406580                                                                  01  DCMP
406590 N80-LAST-COND-DO-ACTIONS.                                        01  DCMP
406600     PERFORM O95-PUT-OUT-ACTIONS THRU P00-PUT-OUT-ACTIONS-EXIT.   01  DCMP
406605     IF CNTR-7 EQUAL TO ZERO                                      01  DCMP
406606         GO TO P97-TABLE-END. NOTE NO MORE DT LABELS OR           01  DCMP
406607                                       SUBTABLES.                 01  DCMP
406610                                                                  01  DCMP
406620 N85-LAST-CONDITION-EXIT.                                         01  DCMP
406630     GO TO K65-DO-NEXT-SUBTABLE.                                  01  DCMP
406640                                                                  01  DCMP
406650                                                                  01  DCMP
406660                                                                  01  DCMP
406670 N90-COND-CODE-GENERATE.                                          01  DCMP
406680     MOVE ZERO TO Y1-IND.                                         01  DCMP
406690     MOVE ZERO TO N1-IND.                                         01  DCMP
406700     IF YCNT NOT EQUAL TO 001                                     01  DCMP
406710         GO TO N95-ARRAY-TESTING1.                                01  DCMP
406720     MOVE YARRAY (1) TO CNTR-1.                                   01  DCMP
406730     PERFORM O10-BLANK-TEST THRU O15-BLANK-TEST-EXIT.             01  DCMP
406740     IF CNTR-2 EQUAL TO ZERO                                      01  DCMP
406750         GO TO N95-ARRAY-TESTING1.                                01  DCMP
406760     MOVE "1" TO Y1-IND.                                          01  DCMP
406770                                                                  01  DCMP
406780 N95-ARRAY-TESTING1.                                              01  DCMP
406790     IF NCNT NOT EQUAL TO 001                                     01  DCMP
406800         GO TO O00-WHAT-KIND-OF-IF.                               01  DCMP
406810     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
406820     PERFORM O10-BLANK-TEST THRU O15-BLANK-TEST-EXIT.             01  DCMP
406830     IF CNTR-2 EQUAL TO ZERO                                      01  DCMP
406840         GO TO O00-WHAT-KIND-OF-IF.                               01  DCMP
406850     MOVE "1" TO N1-IND.                                          01  DCMP
406860                                                                  01  DCMP
406870 O00-WHAT-KIND-OF-IF.                                             01  DCMP
406880     IF NCNT EQUAL TO ZERO                                        01  DCMP
406890         ALTER O55-IF-CK-NS-ELS-EL-EXIT                           01  DCMP
406891             TO PROCEED TO O05-COND-CODE-GENERATE-EXIT            01  DCMP
406900         GO TO O50-IF-CK-NS-ELS-EL.                               01  DCMP
406910     IF YCNT EQUAL TO ZERO                                        01  DCMP
406920         ALTER O45-IF-CK-EL-EXIT                                  01  DCMP
406921             TO PROCEED TO O05-COND-CODE-GENERATE-EXIT            01  DCMP
406930         GO TO O40-IF-CK-EL.                                      01  DCMP
406940     ALTER O35-IF-CK-DX-EXIT                                      01  DCMP
406941         TO PROCEED TO O05-COND-CODE-GENERATE-EXIT.               01  DCMP
406945     GO TO O20-IF-CK-DX.                                          01  DCMP
406950                                                                  01  DCMP
406960 O05-COND-CODE-GENERATE-EXIT.                                     01  DCMP
406970     GO TO K51-SUCCESSFUL-GEN.                                    01  DCMP
406980                                                                  01  DCMP
406990                                                                  01  DCMP
407000 O10-BLANK-TEST.                                                  01  DCMP
407010     MOVE ZERO TO CNTR-2.                                         01  DCMP
407020                                                                  01  DCMP
407030 O12-BLANK-TEST-LOOP.                                             01  DCMP
407040     ADD 001 TO CNTR-2.                                           01  DCMP
407050     MOVE CARRAY (CNTR-2) TO CNTR-3.                              01  DCMP
407060     MOVE CMATRIX (CNTR-3, CNTR-1) TO ELEMENT1.                   01  DCMP
407070     IF ELEMENT1 EQUAL TO "Y"                                     01  DCMP
407080         GO TO O14-BLANK-TEST-FOUND-YN.                           01  DCMP
407090     IF ELEMENT1 EQUAL TO "N"                                     01  DCMP
407100         GO TO O14-BLANK-TEST-FOUND-YN.                           01  DCMP
407110     IF CNTR-2 NOT EQUAL TO CCNT                                  01  DCMP
407120         GO TO O12-BLANK-TEST-LOOP.                               01  DCMP
407130     MOVE 001 TO CNTR-2.                                          01  DCMP
407140     GO TO O15-BLANK-TEST-EXIT.                                   01  DCMP
407150                                                                  01  DCMP
407160 O14-BLANK-TEST-FOUND-YN.                                         01  DCMP
407170     MOVE ZERO TO CNTR-2.                                         01  DCMP
407180                                                                  01  DCMP
407190 O15-BLANK-TEST-EXIT.                                             01  DCMP
407200     EXIT.                                                        01  DCMP
407210                                                                  01  DCMP
407220                                                                  01  DCMP
407230                                                                  01  DCMP
407240 O20-IF-CK-DX.                                                    01  DCMP
407250     IF Y1-IND EQUAL TO "1"                                       01  DCMP
407260         GO TO O26-IF-CK-DX-3.                                    01  DCMP
407270     IF N1-IND EQUAL TO "1"                                       01  DCMP
407280         GO TO O22-IF-CK-DX-1.                                    01  DCMP
407290     IF NCNT NOT EQUAL TO 001                                     01  DCMP
407300         GO TO O24-IF-CK-DX-2.                                    01  DCMP
407310     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
407320     GO TO O24-IF-CK-DX-2.                                        01  DCMP
407330                                                                  01  DCMP
407340 O22-IF-CK-DX-1.                                                  01  DCMP
407350     MOVE NXTSIND TO TYPE-GO-TO.                                  01  DCMP
407360     MOVE ACTNIND TO TYPE-ELSE.                                   01  DCMP
407370     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
407380     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
407390     MOVE BRANCH TO N-BRANCH.                                     01  DCMP
407400     PERFORM N45-YARRAY-SWITCH THRU N46-YARRAY-SWITCH-EXIT.       01  DCMP
407410     GO TO O30-IF-CK-DX-5.                                        01  DCMP
407420                                                                  01  DCMP
407430 O24-IF-CK-DX-2.                                                  01  DCMP
407440     MOVE DXN-IND TO TYPE-GO-TO.                                  01  DCMP
407450     MOVE DXNUM TO Y-BRANCH.                                      01  DCMP
407460     PERFORM O93-PUSH-LABEL THRU O94-PUSH-LABEL-EXIT.             01  DCMP
407461     IF LSCNT GREATER THAN 050                                    01  DCMP
407462         GO TO P96-SIZE-ERROR. NOTE TOO MANY DTS.                 01  DCMP
407470     GO TO O32-IF-CK-DX-6.                                        01  DCMP
407480                                                                  01  DCMP
407490 O26-IF-CK-DX-3.                                                  01  DCMP
407500     IF N1-IND NOT EQUAL TO "1"                                   01  DCMP
407510         GO TO O28-IF-CK-DX-4.                                    01  DCMP
407520     ALTER O85-IF-CK-AZ-ELS-AZ-EXIT                               01  DCMP
407521         TO PROCEED TO O27.                                       01  DCMP
407525     GO TO O80-IF-CK-AZ-ELS-AZ.                                   01  DCMP
407526 O27.                                                             01  DCMP
407530     PERFORM O95-PUT-OUT-ACTIONS THRU P00-PUT-OUT-ACTIONS-EXIT.   01  DCMP
407535     IF CNTR-7 EQUAL TO ZERO                                      01  DCMP
407536         GO TO P97-TABLE-END. NOTE NO MORE DT LABELS OR           01  DCMP
407537                                      SUBTABLES.                  01  DCMP
407540     GO TO K65-DO-NEXT-SUBTABLE.                                  01  DCMP
407550                                                                  01  DCMP
407560 O28-IF-CK-DX-4.                                                  01  DCMP
407570     MOVE ACTNIND TO TYPE-GO-TO.                                  01  DCMP
407580     MOVE YARRAY (1) TO CNTR-1.                                   01  DCMP
407590     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
407600     MOVE BRANCH TO Y-BRANCH.                                     01  DCMP
407610                                                                  01  DCMP
407620 O30-IF-CK-DX-5.                                                  01  DCMP
407630     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
407640                                                                  01  DCMP
407650 O32-IF-CK-DX-6.                                                  01  DCMP
407660     PERFORM P05-WRITE-OUT THRU P08-WRITE-OUT-EXIT.               01  DCMP
407670                                                                  01  DCMP
407680 O35-IF-CK-DX-EXIT.                                               01  DCMP
407690     GO TO.                                                       01  DCMP
407700                                                                  01  DCMP
407710                                                                  01  DCMP
407720                                                                  01  DCMP
407730 O40-IF-CK-EL.                                                    01  DCMP
407740     MOVE ELS-IND TO TYPE-GO-TO.                                  01  DCMP
407745         IF SKIP-GEN-SW NOT EQUAL TO "1"                          01  DCMP
407750     MOVE ELS-IND TO ELSERULE.                                    01  DCMP
407760     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
407765     IF SKIP-GEN-SW EQUAL TO "1"                                  01  DCMP
407766         GO TO O41-LAST-IF-SUBTABLE.   NOTE IPAR D21.             01  DCMP
407770     IF N1-IND EQUAL TO ZERO                                      01  DCMP
407780         GO TO O42-IF-CK-EL-1.                                    01  DCMP
407785 O41-LAST-IF-SUBTABLE.                                            01  DCMP
407790     MOVE ACTNIND TO TYPE-ELSE.                                   01  DCMP
407800     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
407810     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
407820     MOVE BRANCH TO N-BRANCH.                                     01  DCMP
407822     IF SKIP-GEN-SW EQUAL TO "1"                                  01  DCMP
407824         MOVE BRANCH TO Y-BRANCH                                  01  DCMP
407826         MOVE ACTNIND TO TYPE-GO-TO                               01  DCMP
407828         MOVE ZERO TO SKIP-GEN-SW.     NOTE IPAR D21.             01  DCMP
407830     PERFORM O95-PUT-OUT-ACTIONS THRU P00-PUT-OUT-ACTIONS-EXIT.   01  DCMP
407835     IF CNTR-7 EQUAL TO ZERO                                      01  DCMP
407836         GO TO P97-TABLE-END. NOTE NO MORE DT LBLS OR SUBTABLES.  01  DCMP
407840     GO TO K65-DO-NEXT-SUBTABLE.                                  01  DCMP
407850                                                                  01  DCMP
407860 O42-IF-CK-EL-1.                                                  01  DCMP
407870     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
407880     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
407890     MOVE BRANCH TO Y-BRANCH.                                     01  DCMP
407900     PERFORM P05-WRITE-OUT THRU P08-WRITE-OUT-EXIT.               01  DCMP
407910                                                                  01  DCMP
407920 O45-IF-CK-EL-EXIT.                                               01  DCMP
407930     GO TO.                                                       01  DCMP
407940                                                                  01  DCMP
407950                                                                  01  DCMP
407960                                                                  01  DCMP
407970 O50-IF-CK-NS-ELS-EL.                                             01  DCMP
407980     MOVE YARRAY (1) TO CNTR-1.                                   01  DCMP
407990     IF NBR-RULES EQUAL TO 001                                    01  DCMP
408000         MOVE ZERO TO CNTR-2                                      01  DCMP
408010         GO TO O52-IF-CK-NS-ELS-EL-1.                             01  DCMP
408020     PERFORM O10-BLANK-TEST THRU O15-BLANK-TEST-EXIT.             01  DCMP
408030                                                                  01  DCMP
408040 O52-IF-CK-NS-ELS-EL-1.                                           01  DCMP
408050     MOVE ELS-IND TO TYPE-ELSE                                    01  DCMP
408055         IF SKIP-GEN-SW NOT EQUAL TO "1"                          01  DCMP
408060     MOVE ELS-IND TO ELSERULE.                                    01  DCMP
408070     IF CNTR-2 EQUAL TO 001                                       01  DCMP
408080         GO TO O54-IF-CK-NS-ELS-EL-2.                             01  DCMP
408090     MOVE NXTSIND TO TYPE-GO-TO.                                  01  DCMP
408100     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
408110     IF NBR-RULES NOT EQUAL TO 001                                01  DCMP
408120         PERFORM N45-YARRAY-SWITCH THRU N46-YARRAY-SWITCH-EXIT.   01  DCMP
408130     PERFORM P05-WRITE-OUT THRU P08-WRITE-OUT-EXIT.               01  DCMP
408140     GO TO O55-IF-CK-NS-ELS-EL-EXIT.                              01  DCMP
408150                                                                  01  DCMP
408160 O54-IF-CK-NS-ELS-EL-2.                                           01  DCMP
408170     MOVE YARRAY (1) TO CNTR-1.                                   01  DCMP
408180     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
408190     MOVE BRANCH TO Y-BRANCH.                                     01  DCMP
408200     MOVE ACTNIND TO TYPE-GO-TO.                                  01  DCMP
408202     IF SKIP-GEN-SW EQUAL TO "1"                                  01  DCMP
408204         MOVE BRANCH TO N-BRANCH                                  01  DCMP
408206         MOVE ACTNIND TO TYPE-ELSE                                01  DCMP
408208         MOVE ZERO TO SKIP-GEN-SW.     NOTE IPAR D21.             01  DCMP
408210     PERFORM O95-PUT-OUT-ACTIONS THRU P00-PUT-OUT-ACTIONS-EXIT.   01  DCMP
408215     IF CNTR-7 EQUAL TO ZERO                                      01  DCMP
408216         GO TO P97-TABLE-END. NOTE NO MORE DT LBLS OR SUBTABLES.  01  DCMP
408220     GO TO K65-DO-NEXT-SUBTABLE.                                  01  DCMP
408230                                                                  01  DCMP
408240 O55-IF-CK-NS-ELS-EL-EXIT.                                        01  DCMP
408250     GO TO.                                                       01  DCMP
408260                                                                  01  DCMP
408270                                                                  01  DCMP
408280                                                                  01  DCMP
408290 O60-IF-CK-AZ-ELS-EL.                                             01  DCMP
408300     IF YCNT GREATER THAN 001                                     01  DCMP
408310         GO TO P92-ERR-SETUP-Y.                                   01  DCMP
408320     MOVE ACTNIND TO TYPE-GO-TO.                                  01  DCMP
408330     MOVE ELS-IND TO TYPE-ELSE.                                   01  DCMP
408340     MOVE ELS-IND TO ELSERULE.                                    01  DCMP
408350     MOVE YARRAY (1) TO CNTR-1.                                   01  DCMP
408360     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
408370     MOVE BRANCH TO Y-BRANCH.                                     01  DCMP
408380                                                                  01  DCMP
408390 O65-IF-CK-AZ-ELS-EL-EXIT.                                        01  DCMP
408400     GO TO.                                                       01  DCMP
408410                                                                  01  DCMP
408420                                                                  01  DCMP
408430                                                                  01  DCMP
408440 O70-IF-CK-EL-ELS-AZ.                                             01  DCMP
408450     IF NCNT GREATER THAN 001                                     01  DCMP
408460         GO TO P93-ERR-SETUP-N.                                   01  DCMP
408470     MOVE ELS-IND TO TYPE-GO-TO.                                  01  DCMP
408480     MOVE ELS-IND TO ELSERULE.                                    01  DCMP
408490     MOVE ACTNIND TO TYPE-ELSE.                                   01  DCMP
408500     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
408510     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
408520     MOVE BRANCH TO N-BRANCH.                                     01  DCMP
408530                                                                  01  DCMP
408540 O75-IF-CK-EL-ELS-AZ-EXIT.                                        01  DCMP
408550     GO TO.                                                       01  DCMP
408560                                                                  01  DCMP
408570                                                                  01  DCMP
408580                                                                  01  DCMP
408590 O80-IF-CK-AZ-ELS-AZ.                                             01  DCMP
408600     IF NCNT GREATER THAN 001                                     01  DCMP
408610         GO TO P94-ERR-SETUP-Y-N.                                 01  DCMP
408620     IF YCNT GREATER THAN 001                                     01  DCMP
408630         GO TO P94-ERR-SETUP-Y-N.                                 01  DCMP
408640     MOVE ACTNIND TO TYPE-GO-TO.                                  01  DCMP
408650     MOVE ACTNIND TO TYPE-ELSE.                                   01  DCMP
408660     MOVE NARRAY (1) TO CNTR-1.                                   01  DCMP
408670     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
408680     MOVE BRANCH TO N-BRANCH.                                     01  DCMP
408690     MOVE YARRAY (1) TO CNTR-1.                                   01  DCMP
408700     PERFORM O90-POINTER-TEST THRU O91-POINTER-TEST-EXIT.         01  DCMP
408710     MOVE BRANCH TO Y-BRANCH.                                     01  DCMP
408720                                                                  01  DCMP
408730 O85-IF-CK-AZ-ELS-AZ-EXIT.                                        01  DCMP
408740     GO TO.                                                       01  DCMP
408750                                                                  01  DCMP
408760                                                                  01  DCMP
408770                                                                  01  DCMP
408780 O90-POINTER-TEST.                                                01  DCMP
408790     IF TRACE-IND NOT EQUAL TO "1"                                01  DCMP
408800         MOVE ACTNPTR (CNTR-1) TO BRANCH                          01  DCMP
408810         GO TO O91-POINTER-TEST-EXIT.                             01  DCMP
408820     MOVE INARRAY (CNTR-1) TO BRANCH.                             01  DCMP
408830                                                                  01  DCMP
408840 O91-POINTER-TEST-EXIT.                                           01  DCMP
408850     EXIT.                                                        01  DCMP
408860                                                                  01  DCMP
408870                                                                  01  DCMP
408880                                                                  01  DCMP
408890 O93-PUSH-LABEL.                                                  01  DCMP
408900     ADD 001 TO LSCNT.                                            01  DCMP
408910     IF LSCNT GREATER THAN 050                                    01  DCMP
408920         GO TO O94-PUSH-LABEL-EXIT.                               01  DCMP
408930     MOVE DXNUM TO LSTACK (LSCNT).                                01  DCMP
408940     ADD 001 TO DXNUM.                                            01  DCMP
408950                                                                  01  DCMP
408960 O94-PUSH-LABEL-EXIT.                                             01  DCMP
408970     EXIT.                                                        01  DCMP
408980                                                                  01  DCMP
408990                                                                  01  DCMP
409000                                                                  01  DCMP
409010 O95-PUT-OUT-ACTIONS.                                             01  DCMP
409020     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
409030     PERFORM P05-WRITE-OUT THRU P08-WRITE-OUT-EXIT.               01  DCMP
409050     MOVE NSCNT TO CNTR-7.                                        01  DCMP
409060                                                                  01  DCMP
409070 O97-PUT-OUT-ACTNS-1.                                             01  DCMP
409080     SUBTRACT 001 FROM CNTR-7.                                    01  DCMP
409090     IF CNTR-7 EQUAL TO ZERO                                      01  DCMP
409100         GO TO P00-PUT-OUT-ACTIONS-EXIT.                          01  DCMP
409110     IF YIND (CNTR-7) EQUAL TO "1"                                01  DCMP
409120         GO TO O97-PUT-OUT-ACTNS-1.                               01  DCMP
409130                                                                  01  DCMP
409140 O98-POP-LABEL.                                                   01  DCMP
409150     MOVE "6" TO TYPE-RECORD.                                     01  DCMP
409160     MOVE "A" TO SUB-TYPE.                                        01  DCMP
409170     MOVE "DT" TO LABEL-TYPE.                                     01  DCMP
409180     MOVE LSTACK (LSCNT) TO LABEL-1.                              01  DCMP
409190     SUBTRACT 001 FROM LSCNT.                                     01  DCMP
409200                                                                  01  DCMP
409210 O99-WRITE-ACTNS.                                                 01  DCMP
409220     PERFORM P05-WRITE-OUT THRU P08-WRITE-OUT-EXIT.               01  DCMP
409230                                                                  01  DCMP
409240 P00-PUT-OUT-ACTIONS-EXIT.                                        01  DCMP
409250     EXIT.                                                        01  DCMP
409260                                                                  01  DCMP
409270                                                                  01  DCMP
409280                                                                  01  DCMP
409290 P05-WRITE-OUT.                                                   01  DCMP
409300     IF SKIP-GEN-SW EQUAL TO "1"                                  01  DCMP
409310         MOVE "0" TO SKIP-GEN-SW                                  01  DCMP
409320         GO TO P07-SKIP-WRITE-OUT.                                01  DCMP
409330     ADD 001 TO STKINDX.                                          01  DCMP
409340     IF STKINDX GREATER THAN INTSIZE                              01  DCMP
409350         PERFORM P10-SPILL-BENTRY THRU P16-SPILL-BENTRY-EXIT.     01  DCMP
409360     MOVE BDUM TO BENTRY (STKINDX).                               01  DCMP
409370                                                                  01  DCMP
409380 P07-SKIP-WRITE-OUT.                                              01  DCMP
409390     MOVE SPACE TO TYPE-1A-RECORD.                                01  DCMP
409400                                                                  01  DCMP
409410 P08-WRITE-OUT-EXIT.                                              01  DCMP
409420     EXIT.                                                        01  DCMP
409430                                                                  01  DCMP
409440                                                                  01  DCMP
409450                                                                  01  DCMP
409460 P10-SPILL-BENTRY.                                                01  DCMP
409470     MOVE STKINDX TO COUNT-IN-SPILL.                              01  DCMP
409480     MOVE 001 TO X.                                               01  DCMP
409490     IF SPILL-INDEX GREATER THAN ZERO                             01  DCMP
409500         GO TO P12-SPILL-IS-OPEN.                                 01  DCMP
409510     MOVE "2" TO TYPE-IO.                                         01  DCMP
409520     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         01  DCMP
409530                         NOTE OPEN FORMAT WORK FILE AS OUTPUT.    01  DCMP
409540                                                                  01  DCMP
409550 P12-SPILL-IS-OPEN.                                               01  DCMP
409560     PERFORM P14-WRITE-SPILL-REC 7 TIMES.                         01  DCMP
409570     MOVE STKINDX TO COUNT-IN-SPILL.                              01  DCMP
409580     SUBTRACT 001 FROM COUNT-IN-SPILL.                            01  DCMP
409590                                                                  01  DCMP
409600 P14-WRITE-SPILL-REC.                                             01  DCMP
409610     MOVE SPILL (X) TO BUFFER.                                    01  DCMP
409620     MOVE "3" TO TYPE-IO.                                         01  DCMP
409630     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         01  DCMP
409640                         NOTE WRITE FORMAT WORK FILE RECORD.      01  DCMP
409650     ADD 001 TO X.                                                01  DCMP
409660                                                                  01  DCMP
409670 P15-HAVE-WRITTEN-SPILL.                                          01  DCMP
409680     ADD 001 TO SPILL-INDEX.                                      01  DCMP
409690     MOVE 001 TO STKINDX.                                         01  DCMP
409700                                                                  01  DCMP
409710 P16-SPILL-BENTRY-EXIT.                                           01  DCMP
409720     EXIT.                                                        01  DCMP
409730                                                                  01  DCMP
409740                                                                  01  DCMP
409750                                                                  01  DCMP
409760 P20-BACK-UP-TREE.                                                01  DCMP
409770     IF NSCNT EQUAL TO ZERO                                       01  DCMP
409780         GO TO P97-TABLE-END.                                     01  DCMP
409790     IF YIND (NSCNT) EQUAL TO "1"                                 01  DCMP
409800         MOVE ZERO TO FILLER-C-N-S (NSCNT)                        01  DCMP
409810         SUBTRACT 001 FROM NSCNT                                  01  DCMP
409820         GO TO P20-BACK-UP-TREE.                                  01  DCMP
409830     PERFORM K20-NARRAY-INIT THRU K25-NARRAY-INIT-EXIT.           01  DCMP
409840     MOVE ZERO TO CCNT.                                           01  DCMP
409850     PERFORM K35-CARRAY-INIT THRU K38-CARRAY-INIT-EXIT.           01  DCMP
409880     MOVE "1" TO DESTACK-IND.                                     01  DCMP
409890     MOVE ZERO TO CNTR-1.                                         01  DCMP
409900                                                                  01  DCMP
409910 P25-BACK-UP-TREE-DECOMP.                                         01  DCMP
409920     ADD 001 TO CNTR-1                                            01  DCMP
409930     PERFORM N10-DECOMPOSE THRU N40-DECOMPOSE-EXIT.               01  DCMP
409940     IF CNTR-1 LESS THAN NSCNT                                    01  DCMP
409950         GO TO P25-BACK-UP-TREE-DECOMP.                           01  DCMP
409960     MOVE ZERO TO DESTACK-IND.                                    01  DCMP
409970     PERFORM N45-YARRAY-SWITCH THRU N46-YARRAY-SWITCH-EXIT.       01  DCMP
409980     MOVE "1" TO YIND (NSCNT).                                    01  DCMP
409990                                                                  01  DCMP
410000 P30-BACK-UP-TREE-EXIT.                                           01  DCMP
410010     IF CCNT EQUAL TO 001                                         01  DCMP
410020         GO TO K55-FINAL-COND-FOUND                               01  DCMP
410030             ELSE GO TO K70-RESET-FOR-NEXT-COND-K.                01  DCMP
410040                                                                  01  DCMP
410050                                                                  01  DCMP
498000 P92-ERR-SETUP-Y.                                                 01  DCMP
498010     MOVE "5" TO ERROR-IND.                                       01  DCMP
498020     MOVE YARRAY (1) TO ERR-RULE-01.                              01  DCMP
498040     MOVE YARRAY (2) TO ERR-RULE-02.                              01  DCMP
498060     GO TO P98-RETURN.                                            01  DCMP
498070                                                                  01  DCMP
498080 P93-ERR-SETUP-N.                                                 01  DCMP
498090     MOVE "5" TO ERROR-IND.                                       01  DCMP
498100     MOVE NARRAY (1) TO ERR-RULE-01.                              01  DCMP
498110     MOVE NARRAY (2) TO ERR-RULE-02.                              01  DCMP
498120     GO TO P98-RETURN.                                            01  DCMP
498130                                                                  01  DCMP
498140 P94-ERR-SETUP-Y-N.                                               01  DCMP
498160     IF NCNT GREATER THAN 001                                     01  DCMP
498170         GO TO P93-ERR-SETUP-N.                                   01  DCMP
498180     GO TO P92-ERR-SETUP-Y.                                       01  DCMP
498190                                                                  01  DCMP
498192 P95-TERMINAL-ERROR.                                              01  DCMP
498193     MOVE "2" TO ERROR-IND.                                       01  DCMP
498194     GO TO P98-RETURN.                                            01  DCMP
498195                                                                  01  DCMP
498200 P96-SIZE-ERROR.                                                  01  DCMP
498210     MOVE "8" TO ERROR-IND.                                       01  DCMP
498220     GO TO P98-RETURN.                                            01  DCMP
498230                                                                  01  DCMP
498240 P97-TABLE-END.                                                   01  DCMP
498250     MOVE ZERO TO ERROR-IND.                                      01  DCMP
498260                                                                  01  DCMP
498270 P98-RETURN.                                                      01  DCMP
498280     IF SPILL-INDEX GREATER THAN ZERO                             01  DCMP
498290         PERFORM P10-SPILL-BENTRY THRU P16-SPILL-BENTRY-EXIT.     01  DCMP
498300     MOVE STKINDX TO OUTINDEX.                                    01  DCMP
498340                                                                  01  DCMP
498350 P99-DECOMPOSE-TABLE-EXIT.                                        01  DCMP
498360     EXIT.                                                        01  DCMP
498370                                                                  01  DCMP
500000 Q00-PRODUCE-COBOL-CODE.                                          00  INPT
500140     MOVE ZERO TO CNTR-1.                                         02  DCOB
500150     MOVE ZERO TO RESULT.                                         02  DCOB
500160     MOVE ZERO TO DEXITIND.                                       02  DCOB
500170     MOVE ZERO TO ACTNRULES.                                      02  DCOB
500200     MOVE ZERO TO ACTION-SUB.                                     02  DCOB
500210     MOVE ZERO TO COND-SUB.                                       02  DCOB
500220     MOVE ZERO TO COND-LST-SUB.                                   02  DCOB
500240     MOVE ZERO TO ACT-LST-SUB.                                    02  DCOB
500260     MOVE ZERO TO OUT-EL-SW.                                      02  DCOB
500270     IF SPILL-INDEX EQUAL TO ZERO                                 02  DCOB
500280         MOVE OUTINDEX TO STKINDX                                 02  DCOB
500290             ELSE MOVE ZERO TO STKINDX.                           02  DCOB
500300     MOVE SPACE TO OUTPUT-REC.                                    02  DCOB
500310                                                                  02  DCOB
500320 Q05-PROCESS-RECORDS.                                             02  DCOB
500330     ADD 001 TO CNTR-1.                                           02  DCOB
500340     IF CNTR-1 GREATER THAN STKINDX                               02  DCOB
500350         GO TO T00-SPILL-BENTRY.                                  02  DCOB
500360                                                                  02  DCOB
500370 Q10-NOW-HAVE-RECORD.                                             02  DCOB
500380     MOVE BENTRY (CNTR-1) TO TYPE-1A-RECORD.                      02  DCOB
500410     IF TYPE-RECORD EQUAL TO "4"                                  02  DCOB
500420         GO TO R80-ACTION-MATRIX.                                 02  DCOB
500430     IF TYPE-RECORD EQUAL TO "6"                                  02  DCOB
500440         GO TO Q15-SOURCE-REC.                                    02  DCOB
500450     GO TO Q05-PROCESS-RECORDS.                                   02  DCOB
500460                                                                  02  DCOB
500470 Q15-SOURCE-REC.                                                  02  DCOB
500480     IF SUB-TYPE EQUAL TO "A"                                     02  DCOB
500490         GO TO Q20-LABEL-RECORD.                                  02  DCOB
500500     GO TO Q70-IF-RECORD.                                         02  DCOB
500510                                                                  02  DCOB
500520 Q20-LABEL-RECORD.                                                02  DCOB
500530     IF LABEL-TYPE EQUAL TO "DS"                                  02  DCOB
500540         GO TO Q30-LABEL-DS.                                      02  DCOB
500550     IF ACTNRULES EQUAL TO "1"                                    02  DCOB
500560         GO TO S90-NO-ACTN-WARN.                                  02  DCOB
500570                                                                  02  DCOB
500580 Q25-WHAT-KIND-OF-LBL.                                            02  DCOB
500590     IF LABEL-TYPE EQUAL TO "GT"                                  02  DCOB
500600             GO TO Q40-LABEL-GT.                                  02  DCOB
500610     IF LABEL-TYPE EQUAL TO "DE"                                  02  DCOB
500620             GO TO Q60-LABEL-DEXIT.                               02  DCOB
500630     IF LABEL-TYPE EQUAL TO "AT"                                  02  DCOB
500640             GO TO Q55-LABEL-AZ.                                  02  DCOB
500650     IF LABEL-TYPE EQUAL TO "DT"                                  02  DCOB
500660             GO TO Q50-LABEL-DX.                                  02  DCOB
500670     IF LABEL-TYPE EQUAL TO "EL"                                  02  DCOB
500680             GO TO Q45-LABEL-EL.                                  02  DCOB
500690                                                                  02  DCOB
500700 Q30-LABEL-DS.                                                    02  DCOB
500710     MOVE DETAP-TRACE-1 TO DISPLAY-OUT.                           02  DCOB
500720     MOVE TBLNAME TO DISP-OUT-TBLNM.                              02  DCOB
500730     IF IN-LABEL EQUAL TO "999"                                   02  DCOB
500740         MOVE DETAP-TRACE-3 TO DISP-OUT-ELS                       02  DCOB
500750         GO TO Q35-PUNC-TRACE-MSG.                                02  DCOB
500760     MOVE "RL." TO DISP-OUT-RLW.                                  02  DCOB
500770     MOVE IN-LABEL TO DISP-OUT-RLNO.                              02  DCOB
500780                                                                  02  DCOB
500790 Q35-PUNC-TRACE-MSG.                                              02  DCOB
500800     MOVE QUOTE TO OUTPUT-CHAR (13).                              02  DCOB
500810     MOVE QUOTE TO OUTPUT-CHAR (64).                              02  DCOB
500820     MOVE "." TO OUTPUT-CHAR (65).                                02  DCOB
500830     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
500840                                                                  02  DCOB
500850 Q40-LABEL-GT.                                                    02  DCOB
500860     MOVE "GO TO AT" TO DISPLAY-OUT.                              02  DCOB
500870     MOVE TBLNO TO G-AT-TBLNO.                                    02  DCOB
500880     MOVE IN-LABEL TO G-AT-LBL.                                   02  DCOB
500890     MOVE "." TO OUTPUT-CHAR (22).                                02  DCOB
500900     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
500910                                                                  02  DCOB
500920 Q45-LABEL-EL.                                                    02  DCOB
500930     MOVE "EL" TO OUT-PREFIX.                                     02  DCOB
500940     MOVE "1" TO OUT-EL-SW.                                       02  DCOB
500950     PERFORM Q65-LABEL-NO-MOVE.                                   02  DCOB
500960     MOVE "001" TO OUT-LABEL.                                     02  DCOB
500970     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
500980                                                                  02  DCOB
500990 Q50-LABEL-DX.                                                    02  DCOB
501000     MOVE "DT" TO OUT-PREFIX.                                     02  DCOB
501010     PERFORM Q65-LABEL-NO-MOVE.                                   02  DCOB
501020     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
501030                                                                  02  DCOB
501040 Q55-LABEL-AZ.                                                    02  DCOB
501050     IF OUT-EL-SW EQUAL TO "1"                                    02  DCOB
501060         PERFORM R25-GO-TO-DEXIT THRU R40-GO-TO-DEXIT-CLEAR.      02  DCOB
501070     MOVE "AT" TO OUT-PREFIX.                                     02  DCOB
501080     PERFORM Q65-LABEL-NO-MOVE.                                   02  DCOB
501090     MOVE "1" TO ACTNRULES.                                       02  DCOB
501100     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
501110                                                                  02  DCOB
501120 Q60-LABEL-DEXIT.                                                 02  DCOB
501130     IF DEXITIND NOT EQUAL TO "1"                                 02  DCOB
501140         GO TO Q05-PROCESS-RECORDS.                               02  DCOB
501150     IF OUT-EL-SW EQUAL TO "1"                                    02  DCOB
501160         MOVE "EXIT." TO CALOUT                                   02  DCOB
501170         PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT  02  DCOB
501180         MOVE ZERO TO OUT-EL-SW.                                  02  DCOB
501190     MOVE ZERO TO DEXITIND.                                       02  DCOB
501200     MOVE "DEXIT" TO OUT-DEXIT.                                   02  DCOB
501210     MOVE TBLNO TO OUT-DEX-TABLE.                                 02  4COB
501220     MOVE "." TO OUT-PERIOD.                                      02  DCOB
501225 Q62-DEXIT-END-PERF.                                              02  DCOB
501230     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
501240                                                                  02  DCOB
501250 Q65-LABEL-NO-MOVE.                                               02  DCOB
501260     MOVE TBLNO TO OUT-TABLE-NO.                                  02  DCOB
501270     MOVE IN-LABEL TO OUT-LABEL.                                  02  DCOB
501280     MOVE "." TO OUT-PERIOD.                                      02  DCOB
501290                                                                  02  DCOB
501300                                                                  02  DCOB
501310                                                                  02  DCOB
501320 Q70-IF-RECORD.                                                   02  DCOB
501330     IF TYPE-GO-TO EQUAL TO "5"                                   02  DCOB
501340         GO TO R25-GO-TO-DEXIT.                                   02  DCOB
501350     MOVE 005 TO X.                                               02  DCOB
501360     IF GO-TO-LABEL NOT EQUAL TO ELSE-LABEL                       02  DCOB
501370         GO TO Q75-ACTUAL-IF.                                     02  DCOB
501380     IF TYPE-GO-TO EQUAL TO TYPE-ELSE                             02  DCOB
501390         MOVE SPACE TO NO-TYPE                                    02  DCOB
501400         GO TO R00-GO-TO-RECORD.                                  02  DCOB
501410                                                                  02  DCOB
501420 Q75-ACTUAL-IF.                                                   02  DCOB
501430     MOVE "IF"  TO CALOUT.                                        02  DCOB
501440     MOVE CONDLST (CONDITION) TO COND-LST-SUB.                    02  DCOB
501450     MOVE CSTUBPTR (COND-LST-SUB) TO COND-SUB.                    02  DCOB
501460     MOVE COND-LST-SUB TO NEXT-SUB.                               02  DCOB
501470     ADD 001 TO NEXT-SUB.                                         02  DCOB
501480     MOVE 007 TO X.                                               02  DCOB
501490                                                                  02  DCOB
501500 Q80-PROCESS-CONDITION.                                           02  DCOB
501510     MOVE CONDSTK (COND-SUB) TO STUB-CHAR.                        02  DCOB
501520     PERFORM S50-STUB-SHIFT THRU S70-STUB-EXIT.                   02  DCOB
501530     MOVE 001 TO Y.                                               02  DCOB
501540     MOVE STUB-CTR TO MAX.                                        02  DCOB
501550     ADD X TO MAX.                                                02  DCOB
501560                                                                  02  DCOB
501570 Q85-SET-X-TO-OUTPUT.                                             02  DCOB
501580     IF MAX NOT GREATER THAN 060                                  02  DCOB
501590         GO TO Q90-S-X-T-O-OK.                                    02  DCOB
501595     IF OUTPUT-REC NOT EQUAL TO SPACE                             02  DCOB
501600         PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT. 02  DCOB
501610     IF STUB-CTR GREATER THAN 56                                  02  DCOB
501620         MOVE 064 TO X                                            02  DCOB
501630         SUBTRACT STUB-CTR FROM X                                 02  DCOB
501640             ELSE MOVE 008 TO X.                                  02  DCOB
501650                                                                  02  DCOB
501660 Q90-S-X-T-O-OK.                                                  02  DCOB
501670     ADD 001 TO X.                                                02  DCOB
501675 Q92-S-X-T-O-END-PERF.                                            02  DCOB
501680     PERFORM S80-STUB-TO-OUTPUT STUB-CTR TIMES.                   02  DCOB
501690     ADD 001 TO COND-SUB.                                         02  DCOB
501700     IF CSTUBPTR (NEXT-SUB) EQUAL TO COND-SUB                     02  DCOB
501710         GO TO Q95-END-COND.                                      02  DCOB
501720     GO TO Q80-PROCESS-CONDITION.                                 02  DCOB
501730                                                                  02  DCOB
501740 Q95-END-COND.                                                    02  DCOB
501750     NOTE THIS PARAGRAPH COMBINES THE ACTION WITH THE CONDITION.  02  DCOB
501760                                                                  02  DCOB
501770 R00-GO-TO-RECORD.                                                02  DCOB
501780     MOVE X TO MAX.                                               02  DCOB
501790     ADD 019 TO MAX.                                              02  DCOB
501800     IF MAX GREATER THAN 059                                      02  DCOB
501810         MOVE 009 TO X                                            02  DCOB
501820         PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT  02  DCOB
501830             ELSE ADD 001 TO X.                                   02  DCOB
501840     IF TYPE-GO-TO EQUAL TO "1"                                   02  DCOB
501850         GO TO R05-GO-TO-01.                                      02  DCOB
501860     MOVE 015 TO Y.                                               02  DCOB
501870     PERFORM S75-MOVE-LITERAL 6 TIMES.                            02  DCOB
501880                                                                  02  DCOB
501890 R05-GO-TO-01.                                                    02  DCOB
501900     PERFORM R47-GO-TO-PREFIX THRU R75-GO-TO-EXIT.                02  DCOB
501910     IF Y NOT EQUAL TO 080                                        02  DCOB
501920         MOVE 001 TO Y                                            02  DCOB
501930         MOVE TBLNO TO WORK-LABEL                                 02  DCOB
501940         PERFORM S85-LABEL-MOVE 5 TIMES.                          02  DCOB
501950     MOVE GO-TO-LABEL TO WORK-LABEL.                              02  DCOB
501960     IF TYPE-GO-TO EQUAL TO "4"                                   02  DCOB
501970         MOVE "001" TO WORK-LABEL.                                02  DCOB
501980     MOVE 001 TO Y.                                               02  DCOB
501990     PERFORM S85-LABEL-MOVE 3 TIMES.                              02  DCOB
502000     IF NO-TYPE EQUAL TO SPACE                                    02  DCOB
502010         GO TO R10-WRITE-GO-TO.                                   02  DCOB
502020     MOVE X TO MAX.                                               02  DCOB
502030     ADD 026 TO MAX.                                              02  DCOB
502040     IF MAX GREATER THAN 059                                      02  DCOB
502050         MOVE 009 TO X                                            02  DCOB
502060         PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT  02  DCOB
502070             ELSE ADD 001 TO X.                                   02  DCOB
502080     MOVE 010 TO Y.                                               02  DCOB
502090     PERFORM S75-MOVE-LITERAL 11 TIMES.                           02  DCOB
502100     MOVE TYPE-ELSE TO TYPE-GO-TO.                                02  DCOB
502110     PERFORM R47-GO-TO-PREFIX THRU R75-GO-TO-EXIT.                02  DCOB
502120     MOVE 001 TO Y.                                               02  DCOB
502130     MOVE TBLNO TO WORK-LABEL.                                    02  DCOB
502140     PERFORM S85-LABEL-MOVE 5 TIMES.                              02  DCOB
502150     MOVE ELSE-LABEL TO WORK-LABEL.                               02  DCOB
502160     IF TYPE-GO-TO EQUAL TO "4"                                   02  DCOB
502170         MOVE "001" TO WORK-LABEL.                                02  DCOB
502180     MOVE 001 TO Y.                                               02  DCOB
502190     PERFORM S85-LABEL-MOVE 3 TIMES.                              02  DCOB
502200                                                                  02  DCOB
502210 R10-WRITE-GO-TO.                                                 02  DCOB
502220     MOVE "." TO OUTPUT-CHAR (X).                                 02  DCOB
502230                                                                  02  DCOB
502240 R15-WRITE-RECORD-OUT.                                            02  DCOB
502250     PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT.     02  DCOB
502260     GO TO Q05-PROCESS-RECORDS.                                   02  DCOB
502270                                                                  02  DCOB
502280                                                                  02  DCOB
502290                                                                  02  DCOB
502300                                                                  02  DCOB
502310                                                                  02  DCOB
502320                                                                  02  DCOB
502330                                                                  02  DCOB
502340                                                                  02  DCOB
502350                                                                  02  DCOB
502360                                                                  02  DCOB
502370                                                                  02  DCOB
502380 R25-GO-TO-DEXIT.                                                 02  DCOB
502390     EXAMINE DUMMY1 TALLYING LEADING SPACES.                      02  DCOB
502395     MOVE TALLY TO TALLX.                                         PDP-10
502400     MOVE ZERO TO CNTR-2.                                         02  DCOB
502410     MOVE ZERO TO OUT-EL-SW.                                      02  DCOB
502420                                                                  02  DCOB
502430 R30-DEXIT-ISOL-LST-VERB.                                         02  DCOB
502440     ADD 001 TO TALLX.                                            PDP-10
502450     ADD 001 TO CNTR-2.                                           02  DCOB
502460     IF CNTR-2 GREATER THAN 008                                   02  DCOB
502470         GO TO R35-DEXIT-CK-LST-VERB.                             02  DCOB
502480     MOVE DUMSCN (TALLX) TO CHAR (CNTR-2).                        PDP-10
502490     GO TO R30-DEXIT-ISOL-LST-VERB.                               02  DCOB
502500                                                                  02  DCOB
502510 R35-DEXIT-CK-LST-VERB.                                           02  DCOB
502520     IF VERB-GO-TO EQUAL TO "GO TO "                              02  DCOB
502530         GO TO R40-GO-TO-DEXIT-CLEAR.                             02  DCOB
502540     IF VERB-STOP EQUAL TO "STOP RUN"                             02  DCOB
502550         GO TO R40-GO-TO-DEXIT-CLEAR.                             02  DCOB
502560     MOVE 005 TO X.                                               02  DCOB
502570     MOVE "GO TO DEXIT00000." TO CALOUT.                          02  DCOB
502580     MOVE "1" TO DEXITIND.                                        02  DCOB
502590     MOVE TBLNO TO TABLE-NUMBER.                                  02  DCOB
502600     PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT.     02  DCOB
502610                                                                  02  DCOB
502620 R40-GO-TO-DEXIT-CLEAR.                                           02  DCOB
502630     MOVE SPACE TO LASTREC.                                       02  DCOB
502640                                                                  02  DCOB
502650 R45-GO-TO-DEXIT-DONE.                                            02  DCOB
502660     GO TO Q05-PROCESS-RECORDS.                                   02  DCOB
502670                                                                  02  DCOB
502671 R47-GO-TO-PREFIX.                                                02  DCOB
502672     IF TYPE-GO-TO EQUAL TO "1"                                   02  DCOB
502673         GO TO R55-GO-TO-NEXT.                                    02  DCOB
502674     IF TYPE-GO-TO EQUAL TO "2"                                   02  DCOB
502675         GO TO R60-GO-TO-DX.                                      02  DCOB
502676     IF TYPE-GO-TO EQUAL TO "3"                                   02  DCOB
502677         GO TO R65-GO-TO-AZ.                                      02  DCOB
502678     IF TYPE-GO-TO EQUAL TO "4"                                   02  DCOB
502679         GO TO R50-GO-TO-EL.                                      02  DCOB
502680 R50-GO-TO-EL.                                                    02  DCOB
502690     MOVE 025 TO Y.                                               02  DCOB
502700     GO TO R70-MOVE-PREFIX.                                       02  DCOB
502710                                                                  02  DCOB
502720 R55-GO-TO-NEXT.                                                  02  DCOB
502730     MOVE 027 TO Y.                                               02  DCOB
502740     PERFORM S75-MOVE-LITERAL 13 TIMES.                           02  DCOB
502750     MOVE 080 TO Y.                                               02  DCOB
502760     GO TO R75-GO-TO-EXIT.                                        02  DCOB
502770                                                                  02  DCOB
502780 R60-GO-TO-DX.                                                    02  DCOB
502790     MOVE 021 TO Y.                                               02  DCOB
502800     GO TO R70-MOVE-PREFIX.                                       02  DCOB
502810                                                                  02  DCOB
502820 R65-GO-TO-AZ.                                                    02  DCOB
502830     MOVE 023 TO Y.                                               02  DCOB
502840                                                                  02  DCOB
502850 R70-MOVE-PREFIX.                                                 02  DCOB
502860     PERFORM S75-MOVE-LITERAL 2 TIMES.                            02  DCOB
502870                                                                  02  DCOB
502880 R75-GO-TO-EXIT.                                                  02  DCOB
502890     EXIT.                                                        02  DCOB
502900                                                                  02  DCOB
502910                                                                  02  DCOB
502920                                                                  02  DCOB
502930 R80-ACTION-MATRIX.                                               02  DCOB
502940     IF SUB-TYPE EQUAL TO "C"                                     02  DCOB
502950         GO TO R95-ELSE-DISPLAY-OUT.                              02  DCOB
502960     IF SUB-TYPE EQUAL TO "D"                                     02  DCOB
502970         GO TO R95-ELSE-DISPLAY-OUT.                              02  DCOB
502980     MOVE ZERO TO ACTNRULES.                                      02  DCOB
502990     IF SUB-TYPE EQUAL TO "E"                                     02  DCOB
503000         GO TO S05-PERFORM-OUT.                                   02  DCOB
503010     MOVE ACTNLST (ACTIONS) TO ACT-LST-SUB.                       02  DCOB
503020     MOVE ASTUBPTR (ACT-LST-SUB) TO ACTION-SUB.                   02  DCOB
503030     MOVE ACT-LST-SUB TO NEXT-SUB.                                02  DCOB
503040     ADD 001 TO NEXT-SUB.                                         02  DCOB
503050     MOVE 004 TO X.                                               02  DCOB
503060                                                                  02  DCOB
503070 R85-ACTION-PROCESS.                                              02  DCOB
503080     MOVE ACTNSTK (ACTION-SUB) TO STUB-CHAR.                      02  DCOB
503090     PERFORM S50-STUB-SHIFT THRU S70-STUB-EXIT.                   02  DCOB
503100     MOVE X TO MAX.                                               02  DCOB
503110     ADD STUB-CTR TO MAX.                                         02  DCOB
503120     PERFORM Q85-SET-X-TO-OUTPUT THRU Q90-S-X-T-O-OK.             02  DCOB
503130     MOVE 001 TO Y.                                               02  DCOB
503140     PERFORM S80-STUB-TO-OUTPUT STUB-CTR TIMES.                   02  DCOB
503150     ADD 001 TO ACTION-SUB.                                       02  DCOB
503160     IF ASTUBPTR (NEXT-SUB) EQUAL TO ACTION-SUB                   02  DCOB
503170         GO TO R90-END-ACTION.                                    02  DCOB
503180     GO TO R85-ACTION-PROCESS.                                    02  DCOB
503190                                                                  02  DCOB
503200 R90-END-ACTION.                                                  02  DCOB
503210     MOVE X TO Y.                                                 02  DCOB
503220     SUBTRACT 001 FROM Y.                                         02  DCOB
503230     IF OUTPUT-CHAR (Y) NOT EQUAL TO "."                          02  DCOB
503240         MOVE "." TO OUTPUT-CHAR (X).                             02  DCOB
503250     PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT.     02  DCOB
503260     GO TO Q05-PROCESS-RECORDS.                                   02  DCOB
503270                                                                  02  DCOB
503280 R95-ELSE-DISPLAY-OUT.                                            02  DCOB
503290     MOVE ZERO TO OUT-EL-SW.                                      02  DCOB
503300     MOVE DETAP-ELSE TO DISPLAY-OUT.                              02  DCOB
503310     MOVE TBLNAME TO ELS-DISP-TBLNME.                             02  DCOB
503320     MOVE 060 TO X.                                               02  DCOB
503330                                                                  02  DCOB
503340 S00-FIND-END-ELSE-DISP.                                          02  DCOB
503350     IF OUTPUT-CHAR (X) EQUAL TO SPACE                            02  DCOB
503360         SUBTRACT 001 FROM X                                      02  DCOB
503370         GO TO S00-FIND-END-ELSE-DISP.                            02  DCOB
503380     ADD 001 TO X.                                                02  DCOB
503390     MOVE QUOTE TO OUTPUT-CHAR (13).                              02  DCOB
503400     MOVE QUOTE TO OUTPUT-CHAR (X).                               02  DCOB
503410     ADD 001 TO X.                                                02  DCOB
503420     MOVE "." TO OUTPUT-CHAR (X).                                 02  DCOB
503430     PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT.     02  DCOB
503440     MOVE STOPRUN TO CALOUT.                                      02  DCOB
503450     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
503460                                                                  02  DCOB
503470 S05-PERFORM-OUT.                                                 02  DCOB
503480     MOVE PERFRM-SENT TO DISPLAY-OUT.                             02  DCOB
503490     MOVE GO-TO-LABEL TO RULENO-1.                                02  DCOB
503500     MOVE ELSE-LABEL TO RULENO-2.                                 02  DCOB
503510     MOVE TBLNO TO LABELNO-1.                                     02  DCOB
503520     MOVE TBLNO TO LABELNO-2.                                     02  DCOB
503530     GO TO R15-WRITE-RECORD-OUT.                                  02  DCOB
503540                                                                  02  DCOB
503550                                                                  02  DCOB
503560                                                                  02  DCOB
503570                                                                  02  DCOB
504000 S50-STUB-SHIFT.                                                  02  DCOB
504010     MOVE 01 TO START-STUB.                                       02  DCOB
504020                                                                  02  DCOB
504030                                                                  02  DCOB
504040                                                                  02  DCOB
504050                                                                  02  DCOB
504060                                                                  02  DCOB
504070     MOVE  61 TO END-STUB.                                        02  DCOB
504080                                                                  02  DCOB
504090 S60-FIND-STUB-END.                                               02  DCOB
504100     SUBTRACT  01 FROM END-STUB.                                  02  DCOB
504110     IF IN-STUB (END-STUB) EQUAL TO SPACE                         02  DCOB
504120         GO TO S60-FIND-STUB-END.                                 02  DCOB
504130     MOVE END-STUB TO STUB-CTR.                                   02  DCOB
504140                                                                  02  DCOB
504150                                                                  02  DCOB
504160     MOVE 001 TO Y.                                               02  DCOB
504170                                                                  02  DCOB
504180 S65-STUB-MOVE.                                                   02  DCOB
504190     MOVE IN-STUB (START-STUB) TO WORK-STUB (Y).                  02  DCOB
504200     IF START-STUB EQUAL TO END-STUB                              02  DCOB
504210         GO TO S70-STUB-EXIT.                                     02  DCOB
504220     ADD  01 TO START-STUB.                                       02  DCOB
504230     ADD 001 TO Y.                                                02  DCOB
504240     GO TO S65-STUB-MOVE.                                         02  DCOB
504250                                                                  02  DCOB
504260 S70-STUB-EXIT.                                                   02  DCOB
504270     EXIT.                                                        02  DCOB
504280                                                                  02  DCOB
504290                                                                  02  DCOB
504300                                                                  02  DCOB
504310                                                                  02  DCOB
504320 S75-MOVE-LITERAL.                                                02  DCOB
504330     MOVE LIT-CHAR (Y) TO OUTPUT-CHAR (X).                        02  DCOB
504340     ADD 001 TO X.                                                02  DCOB
504350     ADD 001 TO Y.                                                02  DCOB
504360                                                                  02  DCOB
504370                                                                  02  DCOB
504380                                                                  02  DCOB
504390                                                                  02  DCOB
504400 S80-STUB-TO-OUTPUT.                                              02  DCOB
504410     MOVE WORK-STUB (Y) TO OUTPUT-CHAR (X).                       02  DCOB
504420     ADD 001 TO X.                                                02  DCOB
504430     ADD 001 TO Y.                                                02  DCOB
504440                                                                  02  DCOB
504450                                                                  02  DCOB
504460                                                                  02  DCOB
504470                                                                  02  DCOB
504480 S85-LABEL-MOVE.                                                  02  DCOB
504490     MOVE LABEL-CHAR (Y) TO OUTPUT-CHAR (X).                      02  DCOB
504500     ADD 001 TO X.                                                02  DCOB
504510     ADD 001 TO Y.                                                02  DCOB
504520                                                                  02  DCOB
504530                                                                  02  DCOB
504540                                                                  02  DCOB
504550                                                                  02  DCOB
504560 S90-NO-ACTN-WARN.                                                02  DCOB
504570     ADD 00001 TO WARN-ERR-COUNT.                                 02  DCOB
504576     MOVE BDUM TO TEMP-FIELD. NOTE SAVE BDUM FROM BUFFER BEFORE   02  DCOB
504577         REUSING IT IN COMBINED VERSION - IT IS RESTORED IN T35   02  DCOB
504578         PARAGRAPH OF COMBINED VERSION.                           02  DCOB
504580     MOVE NOACTFRULE TO BUFFER.                                   02  DCOB
504590     MOVE SECTION-OUT TO LASTREC.                                 02  DCOB
504600     PERFORM T32-PRINT THRU T40-WRITE-OUTPUT-EXIT.                02  DCOB
504610     MOVE ZERO TO ACTNRULES.                                      02  DCOB
504620     GO TO Q25-WHAT-KIND-OF-LBL.                                  02  DCOB
504630                                                                  02  DCOB
504640                                                                  02  DCOB
504650                                                                  02  DCOB
504660 T00-SPILL-BENTRY.                                                02  DCOB
504670     IF SPILL-INDEX EQUAL TO ZERO                                 02  DCOB
504680         GO TO T50-WRAP-UP.                                       02  DCOB
504690     IF SPILL-OPEN-SW NOT EQUAL TO ZERO                           02  DCOB
504700         GO TO T15-READ-SPILL.                                    02  DCOB
504710                                                                  02  DCOB
504720 T05-CLOSE-SPILL.                                                 02  DCOB
504730     MOVE "6" TO TYPE-IO.                                         02  DCOB
504740     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         02  DCOB
504750                         NOTE CLOSE FORMAT WORK FILE.             02  DCOB
504760     MOVE ZERO TO SPILL-OPEN-SW.                                  02  DCOB
504770                                                                  02  DCOB
504780 T10-OPEN-SPILL-IN.                                               02  DCOB
504790     MOVE "4" TO TYPE-IO.                                         02  DCOB
504800     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         02  DCOB
504810                         NOTE OPEN FORMAT WORK FILE AS INPUT.     02  DCOB
504820     MOVE "1" TO SPILL-OPEN-SW.                                   02  DCOB
504830                                                                  02  DCOB
504840 T15-READ-SPILL.                                                  02  DCOB
504850     MOVE ZERO TO X.                                              02  DCOB
504860                                                                  02  DCOB
504870 T16-READING-SPILL.                                               02  DCOB
504880     ADD 001 TO X.                                                02  DCOB
504890     MOVE "5" TO TYPE-IO.                                         02  DCOB
504900     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         02  DCOB
504910                         NOTE READ FORMAT WORK FILE.              02  DCOB
504920     IF FLAG-END-SPILL EQUAL TO "*** FORMAT"                      02  DCOB
504930         GO TO T50-WRAP-UP.                                       02  DCOB
504940     MOVE FORMAT-WORK-FILE-BUFFER TO SPILL (X).                   02  DCOB
504950     IF X LESS THAN 008                                           02  DCOB
504960         GO TO T16-READING-SPILL.                                 02  DCOB
504970                                                                  02  DCOB
504980 T17-HAVE-READ-SPILL.                                             02  DCOB
504990     MOVE COUNT-IN-SPILL TO STKINDX.                              02  DCOB
505000     MOVE ZERO TO CNTR-1.                                         02  DCOB
505010     SUBTRACT 001 FROM SPILL-INDEX.                               02  DCOB
505020     GO TO Q05-PROCESS-RECORDS.                                   02  DCOB
505030                                                                  02  DCOB
505040                                                                  02  DCOB
505050                                                                  02  DCOB
505060                                                                  02  DCOB
505070 T20-WRITE-OUTPUT-REC.                                            02  DCOB
505080     MOVE IDENTFIELD TO OUT-IDENT.                                02  DCOB
505090     ADD INCREMENT TO SEQUENCE-NO.                                02  DCOB
 505094     IF  DOPTS-SW-SEQ  EQUAL TO  "0"                              02  DCOB
505096         MOVE SPACES TO  OUT-SEQ-NO-X                             02  DCOB
505098       ELSE                                                       02  DCOB
505100         MOVE SEQUENCE-NO TO OUT-SEQ-NO.     NOTE IPAR D31.       02  DCOB
505106     MOVE BDUM TO TEMP-FIELD. NOTE YOU WRITE FROM BUFFER SO SAVE  02  DCOB
505107                                IT, BUFFER IS SAME AREA AS BDUM   02  DCOB
505108                                IN COMBINED VERSION.              02  DCOB
505110     MOVE OUTPUT-REC TO BUFFER.                                   02  DCOB
505120     MOVE SECTION-OUT TO LASTREC.                                 02  DCOB
505130     IF PRINT-PUNCH EQUAL TO "2"                                  02  DCOB
505140         GO TO T25-PUNCH.                                         02  DCOB
505150     IF PRINT-PUNCH EQUAL TO "3"                                  02  DCOB
505160         GO TO T25-PUNCH.                                         02  DCOB
505170     GO TO T32-PRINT.                                             02  DCOB
505180                                                                  02  DCOB
505190 T25-PUNCH.                                                       02  DCOB
505200     MOVE WRITE-PUNCH TO TYPE-IO.                                 02  DCOB
505230     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         02  DCOB
505235                         NOTE WRITE PUNCH RECORD.                 02  DCOB
505240                                                                  02  DCOB
505250 T30-CHECK-WRITE.                                                 02  DCOB
505260     IF PRINT-PUNCH NOT EQUAL TO "3"                              02  DCOB
505270         GO TO T35-WRITE-CLEAR.                                   02  DCOB
505280                                                                  02  DCOB
505290 T32-PRINT.                                                       02  DCOB
505300     IF DOPTS-SW-DCP EQUAL TO ZERO                                02  DCOB
505310         GO TO T35-WRITE-CLEAR.                                   02  DCOB
505320     MOVE WRITE-LIST TO TYPE-IO.                                  02  DCOB
505330     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         02  DCOB
505340                         NOTE WRITE PRINT LINE.                   02  DCOB
505350                                                                  02  DCOB
505360 T35-WRITE-CLEAR.                                                 02  DCOB
505370     MOVE SPACE TO OUTPUT-REC.                                    02  DCOB
505376     MOVE TEMP-FIELD TO BUFFER. NOTE RESTORE BDUM TO BUFFER.      02  DCOB
505380                                                                  02  DCOB
505390 T40-WRITE-OUTPUT-EXIT.                                           02  DCOB
505400     EXIT.                                                        02  DCOB
505410                                                                  02  DCOB
505420                                                                  02  DCOB
505430                                                                  02  DCOB
505440                                                                  02  DCOB
505450 T50-WRAP-UP.                                                     02  DCOB
505460     IF SPILL-OPEN-SW EQUAL TO "1"                                02  DCOB
505470         PERFORM T05-CLOSE-SPILL.                                 02  DCOB
505480     IF DEXITIND EQUAL TO "1"                                     02  DCOB
505490         PERFORM Q60-LABEL-DEXIT                                  02  DCOB
505500         PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT. 02  DCOB
505510     IF DUM-A-M NOT EQUAL TO SPACE                                02  DCOB
505520         MOVE "EXIT." TO CALOUT                                   02  DCOB
505530         PERFORM T20-WRITE-OUTPUT-REC THRU T40-WRITE-OUTPUT-EXIT. 02  DCOB
505540     MOVE ZERO TO RESULT.                                         02  DCOB
505550                                                                  02  DCOB
505610 T99-PRODUCE-COBOL-EXIT.                                          02  DCOB
505620     EXIT.                                                        02  DCOB
505630                                                                  02  DCOB
505640                                                                  02  DCOB
505650                                                                  02  DCOB
600010 U00-INPUT-OUTPUT-OPERATION.                                      00  INPT
600060     IF TYPE-IO EQUAL TO "D"                                      03  IORT
600070         GO TO U35-WRITE-LIST-OUT.                                03  IORT
600080     IF TYPE-IO EQUAL TO "C"                                      03  IORT
600090         GO TO U30-READ-INPUT.                                    03  IORT
600100     IF TYPE-IO EQUAL TO "E"                                      03  IORT
600110         GO TO U45-WRITE-PUNCH.                                   03  IORT
600120     IF TYPE-IO EQUAL TO "8"                                      03  IORT
600130         GO TO V30-WRITE-LIST-OUT-FRMT.                           03  IORT
600140     IF TYPE-IO EQUAL TO "9"                                      03  IORT
600150         GO TO U10-PAGE-EJECT.                                    03  IORT
600160     IF TYPE-IO EQUAL TO "5"                                      03  IORT
600170         GO TO V15-READ-FRMT-WORK.                                03  IORT
600180     IF TYPE-IO EQUAL TO "3"                                      03  IORT
600190         GO TO V05-WRITE-FRMT-WORK.                               03  IORT
600200     IF TYPE-IO EQUAL TO "6"                                      03  IORT
600210         GO TO V20-CLOSE-FRMT-WORK.                               03  IORT
600220     IF TYPE-IO EQUAL TO "2"                                      03  IORT
600230         GO TO V00-OPEN-FRMT-WORK-OUT.                            03  IORT
600240     IF TYPE-IO EQUAL TO "4"                                      03  IORT
600250         GO TO V10-OPEN-FRMT-WORK-IN.                             03  IORT
600260     IF TYPE-IO EQUAL TO "A"                                      03  IORT
600270         GO TO U05-OPEN-INPUT-AND-PRINT-FILES.                    03  IORT
600280     IF TYPE-IO EQUAL TO "B"                                      03  IORT
600290         GO TO U25-OPEN-PUNCH.                                    03  IORT
600300     IF TYPE-IO EQUAL TO "F"                                      03  IORT
600310         GO TO U50-CLOSE-FILES-EOJ.                               03  IORT
600500     GO TO V98-RETURN.                                            03  IORT
600510                                                                  03  IORT
600520 U05-OPEN-INPUT-AND-PRINT-FILES.                                  03  IORT
600530     OPEN INPUT FILE-IN.                                          03  IORT
600540     OPEN OUTPUT LIST-OUT.                                        03  IORT
600550                                                                  03  IORT
600560 U10-PAGE-EJECT.                                                  03  IORT
600570     MOVE SPACE TO LISTOUTPUT.                                    03  IORT
600580     MOVE "1" TO CONTROLX.                                        03  IORT
600590     MOVE 01 TO LCNTER.                                           03  IORT
600600                                                                  03  IORT
600610 U15-ACTUALLY-PRINT.                                              03  IORT
600614     MOVE PRINT-LINE-132 TO PRINT-WORK-132.                       PDP-10
600615     IF CONTROLX EQUAL TO SPACE                                   PDP-10
600616         WRITE LIST-REC   FROM PRINT-WORK-132                     PDP-10
600617             AFTER ADVANCING 1 LINES ELSE                         PDP-10
600618     IF CONTROLX EQUAL TO ZERO                                    PDP-10
600619         WRITE LIST-REC   FROM PRINT-WORK-132                     PDP-10
600620             AFTER ADVANCING 2 LINES ELSE                         PDP-10
600621     IF CONTROLX EQUAL TO "-"                                     PDP-10
600622         WRITE LIST-REC   FROM PRINT-WORK-132                     PDP-10
600623             AFTER ADVANCING 3 LINES ELSE                         PDP-10
600624     IF CONTROLX EQUAL TO "1"                                     PDP-10
600625         WRITE LIST-REC   FROM PRINT-WORK-132                     PDP-10
600626             AFTER ADVANCING TOP-OF-PAGE.                         PDP-10
600630                                                                  PDP-10
600631                                                                  PDP-10
600640 U20-CLEANUP-AFTER-PRINT.                                         03  IORT
600650     MOVE SPACE TO LISTOUTPUT.                                    03  IORT
600660     GO TO V98-RETURN.                                            03  IORT
600670                                                                  03  IORT
600680 U25-OPEN-PUNCH.                                                  03  IORT
600685     IF PUNCH-OPEN-IND EQUAL TO "X"                               03  IORT
600686         GO TO V98-RETURN.                                        03  IORT
600690     OPEN OUTPUT PUNCH-OUT.                                       03  IORT
600700     MOVE "X" TO PUNCH-OPEN-IND.                                  03  IORT
600710     GO TO V98-RETURN.                                            03  IORT
600720                                                                  03  IORT
600730 U30-READ-INPUT.                                                  03  IORT
600740     READ FILE-IN AT END                                          03  IORT
600750         MOVE "3" TO RESULT                                       03  IORT
600760         MOVE "999999 FILE END" TO WS-CARD                        03  IORT
600770         GO TO V98-RETURN.                                        03  IORT

                NOTE  ***  TYMSHARE CHANGES TO ADJUST INPUT
                        TO ALLOW FOR DEC STANDARD FORMAT:
                        THIS LINE WAS REPLACED
                                600780     MOVE INPUTREC TO WS-CARD
                        AND REPLACED BY THE TWO LINES
                                MOVE SPACES TO WS-CARD
                                MOVE INPUTREC TO STANDRD-FMT-SHIFT.


                MOVE SPACES TO WS-CARD.
                MOVE INPUTREC TO STANDRD-FMT-SHIFT.

600790     MOVE "1" TO RESULT.                                          03  IORT
600800     EXAMINE TSEQ-NO-X REPLACING ALL " " BY "0".                  03  IORT
600810     IF TSEQ-NO NOT NUMERIC                                       03  IORT
600820         MOVE "*" TO SEQIND                                       03  IORT
600830         GO TO V98-RETURN.                                        03  IORT
600840     IF LASTSEQ GREATER THAN TSEQ-NO                              03  IORT
600850         MOVE "*" TO SEQIND.                                      03  IORT
600860     MOVE TSEQ-NO TO LASTSEQ.                                     03  IORT
600870     GO TO V98-RETURN.                                            03  IORT
600880                                                                  03  IORT
600890 U35-WRITE-LIST-OUT.                                              03  IORT
600900     MOVE BUFFER TO OUTPUTD.                                      03  IORT
600910     MOVE NUMBERS TO SEQUENCE-X.                                  03  IORT
600920     MOVE SEQIND TO SEQERR.                                       03  IORT
600930     IF LCNTER GREATER THAN LINECNT                               03  IORT
600940         MOVE "1" TO CONTROLX                                     03  IORT
600950         MOVE ZERO TO LCNTER.                                     03  IORT
600960     IF ERR-AST EQUAL TO "***"                                    03  IORT
600970         MOVE ERR-AST TO ERR-FLAGS.                               03  IORT
600980     IF BND-IND-4-REPL EQUAL TO "B"                               03  IORT
600990         MOVE "(BND)" TO BND-WORD.                                03  IORT
600992     IF  OUTPUTD-IDEN EQUAL TO "DETAPIMI"  OR  ERR-AST = "***"    03  IORT
600993         OR D-OPTNS EQUAL TO "D*OPTNS"                            03  IORT
600994         MOVE "N" TO SEQ-SW                                       03  IORT
600995       ELSE                                                       03  IORT
600996         MOVE "Y" TO SEQ-SW.          NOTE IPAR D30.              03  IORT
601000     PERFORM U15-ACTUALLY-PRINT.                                  03  IORT
601010     ADD 01 TO LCNTER.                                            03  IORT
601020     IF TEST-NUMB NUMERIC AND SEQ-SW EQUAL TO "Y"                 03  IORT
601030         ADD 000001 TO SEQNUMB.                                   03  IORT
601040     MOVE SPACE TO SEQIND.                                        03  IORT
601050     MOVE SPACE TO BND-IND-4-REPL.                                03  IORT
601060     MOVE SPACE TO LISTOUTPUT.                                    03  IORT
601070     MOVE "1" TO RESULT.                                          03  IORT
601100     GO TO V98-RETURN.                                            03  IORT
601110                                                                  03  IORT
601120 U45-WRITE-PUNCH.                                                 03  IORT

           MOVE SPACES TO PUNCHOUT.
           MOVE STANDRD-FMT-SHIFT TO PUNCHOUT.

                NOTE  ***  TYMSHARE CHANGES TO ADJUST OUTPUT
                        TO DEC STANDARD FORMAT:
                        THE FOLLOWING LINE WAS REPLACED;
                                601130    MOVE BUFFER TO PUNCHOUT
                        BY THE TWO LINES;
                                MOVE SPACES TO PUNCHOUT
                                MOVE STANDRD-FMT-SHIFT TO PUNCHOUT.

601140     WRITE PUNCHOUT.                                              03  IORT
601150     MOVE "1" TO RESULT.                                          03  IORT
601160     GO TO V98-RETURN.                                            03  IORT
601170                                                                  03  IORT
601180 U50-CLOSE-FILES-EOJ.                                             03  IORT
601190     CLOSE FILE-IN.                                               03  IORT
601200     CLOSE LIST-OUT.                                              03  IORT
601210     IF PUNCH-OPEN-IND NOT EQUAL TO SPACE                         03  IORT
601220         CLOSE PUNCH-OUT.                                         03  IORT
601230     MOVE "1" TO RESULT.                                          03  IORT
601240     GO TO V98-RETURN.                                            03  IORT
601250                                                                  03  IORT
601260                                                                  03  IORT
602000 V00-OPEN-FRMT-WORK-OUT.                                          03  IORT
602010     OPEN OUTPUT FORMAT-WORK-FILE.                                03  IORT
602020     GO TO V98-RETURN.                                            03  IORT
602030                                                                  03  IORT
602040 V05-WRITE-FRMT-WORK.                                             03  IORT
602050     MOVE FORMAT-WORK-FILE-BUFFER TO FORMAT-WORK-FILE-RECORD.     03  IORT
602060     WRITE FORMAT-WORK-FILE-RECORD.                               03  IORT
602070     GO TO V98-RETURN.                                            03  IORT
602080                                                                  03  IORT
602090 V10-OPEN-FRMT-WORK-IN.                                           03  IORT
602100     OPEN INPUT FORMAT-WORK-FILE.                                 03  IORT
602110     GO TO V98-RETURN.                                            03  IORT
602120                                                                  03  IORT
602130 V15-READ-FRMT-WORK.                                              03  IORT
602140     READ FORMAT-WORK-FILE AT END                                 03  IORT
602145         MOVE "*** FORMAT" TO FLAG-END-SPILL                      03  IORT
602150         GO TO V98-RETURN.                                        03  IORT
602160     MOVE FORMAT-WORK-FILE-RECORD TO FORMAT-WORK-FILE-BUFFER.     03  IORT
602170     GO TO V98-RETURN.                                            03  IORT
602180                                                                  03  IORT
602190 V20-CLOSE-FRMT-WORK.                                             03  IORT
602200     CLOSE FORMAT-WORK-FILE.                                      03  IORT
602210     GO TO V98-RETURN.                                            03  IORT
602220                                                                  03  IORT
602230 V30-WRITE-LIST-OUT-FRMT.                                         03  IORT
602240     MOVE FORMAT-PRINT-BUFFER TO LISTOUTPUT.                      03  IORT
602250     PERFORM U15-ACTUALLY-PRINT.                                  03  IORT
602255     MOVE SPACE TO LISTOUTPUT.                                    03  IORT
602260     GO TO V98-RETURN.                                            03  IORT
602270                                                                  03  IORT
602280     NOTE ********************************************************03  IORT
602290     *       THE INPUT-OUTPUT-OPERATION ROUTINES PERFORM ALL     *03  IORT
602300     *    I/O FUNCTIONS FOR DETAP -                              *03  IORT
602310     *                                                           *03  IORT
602320     *       THE FUNCTION REQUESTED IS CODED IN THE TYPE-IO      *03  IORT
602330     *    FIELD OF THE IO-BLOCK  AS FOLLOWS                      *03  IORT
602340     *                                                           *03  IORT
602350     *       TYPE-IO     FUNCTION REQUESTED                      *03  IORT
602360     *       -------     -----------------                       *03  IORT
602370     *          2    OPEN FORMATTER MODULE WORK FILE AS OUTPUT   *03  IORT
602380     *          3    WRITE A RECORD ON FORMATTER MODULE WORK FILE*03  IORT
602390     *          4    OPEN FORMATTER MODULE WORK FILE AS INPUT    *03  IORT
602400     *          5    READ FORMATTER MODULE WORK FILE RECORD      *03  IORT
602410     *          6    CLOSE FORMATTER MODULE WORK FILE            *03  IORT
602420     *          8    WRITE A PRINT LINE OF THE FORMATTER MODULE  *03  IORT
602430     *          9    PAGE EJECT ON THE PRINT FILE                *03  IORT
602440     *          A    OPEN THE CARD INPUT AND PRINT FILES         *03  IORT
602450     *          B    OPEN THE CARD PUNCH FILE                    *03  IORT
602460     *          C    READ THE CARD INPUT FILE                    *03  IORT
602470     *          D    WRITE A PRINT LINE NOT OF THE FORMATTER     *03  IORT
602480     *          E    WRITE A PUNCH FILE RECORD                   *03  IORT
602490     *          F    CLOSE ALL FILES FOR END OF JOB              *03  IORT
602500     *                                                           *03  IORT
602510     ************************************************************.03  IORT
602520                                                                  03  IORT
602530                                                                  03  IORT
602540                                                                  03  IORT
699000 V98-RETURN.                                                      03  IORT
699010     MOVE ZERO TO TYPE-IO.                                        03  IORT
699050                                                                  03  IORT
699060 V99-IO-EXIT.                                                     03  IORT
699070     EXIT.                                                        03  IORT
699080                                                                  03  IORT
700020 W00-FORMATTED-TABLE.                                             00  INPT
700080     IF WS-GO-TO EQUAL TO "1"                                     04  FRMT
700090         GO TO W25-RULES-PROCESS.                                 04  FRMT
700100     IF WS-GO-TO EQUAL TO "2"                                     04  FRMT
700110         GO TO W85-CHECK-INIT-CARD.                               04  FRMT
700120     IF WS-GO-TO EQUAL TO "3"                                     04  FRMT
700130         GO TO X10-FORMAT-CARDS.                                  04  FRMT
700140                                                                  04  FRMT
700150 W05-CHECK-FOR-TABLE.                                             04  FRMT
700155     IF WS-CC8-13 NOT EQUAL TO "DETAP "                           04  FRMT
700160         GO TO Y98-RETURN.                                        04  FRMT
700170                             NOTE THIS ROUTINE READS THRU THE     04  FRMT
700180                               INPUT CARDS UNTIL A DETAP TABLE    04  FRMT
700190                               HEADER CARD ("DETAP" IN CC 8-12)   04  FRMT
700200                               IS FOUND.                          04  FRMT
700210     IF DOPTS-SW-EXT EQUAL TO ZERO                                04  FRMT
700220             MOVE "L"  TO WS-LIM-IND.                             04  FRMT
700230     IF DOPTS-SW-EXT EQUAL TO "1"                                 04  FRMT
700240             MOVE "E"  TO WS-LIM-IND.                             04  FRMT
700250     IF DOPTS-SW-OVF EQUAL TO ZERO                                04  FRMT
700260             MOVE ZERO TO LINECT.                                 04  FRMT
700270     IF DOPTS-SW-OVF EQUAL TO "1"                                 04  FRMT
700280             MOVE 001  TO LINECT.                                 04  FRMT
700290     MOVE ZERO TO INIT-BOX-IND.                                   04  FRMT
700300     IF WS-LIM-IND EQUAL TO "L"                                   04  FRMT
700310         MOVE ZERO TO DOPTS-SW-EXT                                04  FRMT
700320             ELSE                                                 04  FRMT
700330                 MOVE "1" TO DOPTS-SW-EXT.                        04  FRMT
700340     IF LINECT EQUAL TO ZERO                                      04  FRMT
700350         MOVE ZERO TO DOPTS-SW-OVF                                04  FRMT
700360             ELSE                                                 04  FRMT
700370                 MOVE "1" TO DOPTS-SW-OVF.                        04  FRMT
700380                                                                  04  FRMT
700390 W10-INITIALIZATION.                                              04  FRMT
700400     IF LINECT NOT EQUAL TO ZERO                                  04  FRMT
700410         MOVE 001 TO LINECT.                                      04  FRMT
700420                             NOTE THIS ROUTINE INITIALIZES        04  FRMT
700430                               WORKING-STORAGE AND OPENS THE WORK 04  FRMT
700440                               FILE FOR THE NEXT TABLE.           04  FRMT
700450     MOVE ZERO TO STUB-CON-SW-1.                                  04  FRMT
700460     MOVE ZERO TO STUB-CON-SW-2.                                  04  FRMT
700470     MOVE SPACE TO WS-WORK-AREAS.                                 04  FRMT
700475     MOVE SPACE TO WORK-RECORD.                                   04  FRMT
700480     MOVE WS-CC18-50 TO WS-NAME-OF-TBL.                           04  FRMT
700490     MOVE ZERO TO UTILITY-SUB.                                    04  FRMT
700500                                                                  04  FRMT
700510 W15-CLEAR-SUBS.                                                  04  FRMT
700520     ADD 01 TO UTILITY-SUB.                                       04  FRMT
700530     MOVE ZERO TO WS-FRMT-SUB (UTILITY-SUB).                      04  FRMT
700540     IF UTILITY-SUB LESS THAN 13                                  04  FRMT
700550         GO TO W15-CLEAR-SUBS.                                    04  FRMT
700590     MOVE 010 TO WS-RL2-SUB.                                      04  FRMT
700600     MOVE WS-OP-WK-OP TO TYPE-IO.                                 04  FRMT
700610     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
700620                             NOTE OPENING OUTPUT WORK-FILE.       04  FRMT
700630                                                                  04  FRMT
700640 W20-READ-RULES.                                                  04  FRMT
700650     MOVE "1" TO WS-GO-TO.                                        04  FRMT
700660     GO TO Y98-RETURN.                                            04  FRMT
700670                             NOTE 1 IN WS-GO-TO MEANS ENTRY       04  FRMT
700680                               ROUTINE WILL GO TO                 04  FRMT
700690                               W25-RULES-PROCESS.                 04  FRMT
700700                                                                  04  FRMT
700710 W25-RULES-PROCESS.                                               04  FRMT
700720     IF WS-CC8-10 EQUAL TO "RL1"                                  04  FRMT
700730         ADD 001 TO WS-RL1-SUB                                    04  FRMT
700740         MOVE SPACE TO WS-CC (10)                                 04  FRMT
700750         MOVE WS-CC10-72 TO WS-RULE (WS-RL1-SUB)                  04  FRMT
700760         GO TO W20-READ-RULES.                                    04  FRMT
700770                             NOTE SAVE "RL1" CARDS TO             04  FRMT
700780                               FORMATTED.                         04  FRMT
700790                             NOTE THESE ROUTINES FOLLOWING READ   04  FRMT
700800                               THE RULE CARDS AND STORE CC 10-72  04  FRMT
700810                               OF THE RL2 CARDS IN CONTIGUOUS 63  04  FRMT
700820                               POSITION AREAS IN "WS-ARRAY" UNTIL 04  FRMT
700830                               A "$" IS ENCOUNTERED DENOTING THE  04  FRMT
700840                               END OF THE LAST RL2 CARD.          04  FRMT
700900     IF WS-CC8-10 EQUAL TO "RL2"                                  04  FRMT
700910         GO TO W30-RL2-FOUND.                                     04  FRMT
700920     IF WS-RL2-SUB EQUAL TO 010                                   04  FRMT
700930         MOVE 100 TO WS-ERR-CODE                                  04  FRMT
700940         GO TO Y20-CANCEL.   NOTE CANCEL FORMATTING SINCE YOU     04  FRMT
700950                               NEVER GOT AN RL2 CARD.             04  FRMT
700960     MOVE "$" TO WS-ELEM (WS-EL-SUB).                             04  FRMT
700970     MOVE "1" TO SKIP-READ-SW.                                    04  FRMT
700980     GO TO W40-FORMAT-MASK.                                       04  FRMT
700990                                                                  04  FRMT
701000 W30-RL2-FOUND.                                                   04  FRMT
701010     ADD 001 TO WS-RL2-SUB.                                       04  FRMT
701020     MOVE SPACE TO WS-CC (10).                                    04  FRMT
701030     MOVE WS-CC10-72 TO WS-RULE (WS-RL2-SUB).                     04  FRMT
701040                             NOTE SAVE "RL2" CARDS TO BE          04  FRMT
701050                               FORMATTED.                         04  FRMT
701060     MOVE 010 TO WS-CC-SUB.                                       04  FRMT
701070                                                                  04  FRMT
701080 W35-LAY-OUT-MASK.                                                04  FRMT
701090     ADD 001 TO WS-EL-SUB.                                        04  FRMT
701100     MOVE WS-CC (WS-CC-SUB) TO WS-ELEM (WS-EL-SUB).               04  FRMT
701110     IF WS-ELEM (WS-EL-SUB) EQUAL TO "$"                          04  FRMT
701120         GO TO W40-FORMAT-MASK.                                   04  FRMT
701130     ADD 001 TO WS-CC-SUB.                                        04  FRMT
701140     IF WS-CC-SUB GREATER THAN 072                                04  FRMT
701150         GO TO W20-READ-RULES.                                    04  FRMT
701160                             NOTE MOVING RL2 CARD CHARACTER-BY-   04  FRMT
701170                               CHARACTER INTO ARRAY UNTIL $       04  FRMT
701180                               ENCOUNTERED OR END OF CARD.        04  FRMT
701190     IF WS-EL-SUB LESS THAN 630                                   04  FRMT
701200         GO TO W35-LAY-OUT-MASK.                                  04  FRMT
701210     MOVE 110 TO WS-ERR-CODE.                                     04  FRMT
701220     GO TO Y20-CANCEL.      NOTE CANCEL FORMATTING SINCE YOU HAVE 04  FRMT
701230                              EXCEEDED SIZE OF MASK.              04  FRMT
701240                                                                  04  FRMT
701250 W40-FORMAT-MASK.                                                 04  FRMT
701260     MOVE WS-EL-SUB TO WS-ARY-SIZE.                               04  FRMT
701270                            NOTE THESE ROUTINES FORMAT A MASK     04  FRMT
701280                              FROM THE RL2 CARDS IN WS-ARRAY      04  FRMT
701290                                 THE CONDITIONS AND ACTIONS (AND  04  FRMT
701300                              THEIR CONTINUATIONS) WILL BE        04  FRMT
701310                              EDITTED INTO THE FORMATTED TABLE    04  FRMT
701320                              USING THIS MASK.                    04  FRMT
701340     MOVE WS-VER-CHAR TO WS-MK-CHAR (1).                          04  FRMT
701350     IF WS-LIM-IND NOT EQUAL TO "L"                               04  FRMT
701360         MOVE WS-VER-CHAR TO WS-MK-CHAR (2)                       04  FRMT
701370             ELSE                                                 04  FRMT
701380                 MOVE WS-FIL-CHAR TO WS-MK-CHAR (2).              04  FRMT
701390     MOVE WS-FIL-CHAR TO WS-MK-CHAR (3).                          04  FRMT
701400     MOVE WS-MASK TO WS-XM-GRP.                                   04  FRMT
701410     MOVE 004 TO WS-MK-SUB.                                       04  FRMT
701420     MOVE 004 TO WS-XM-SUB.                                       04  FRMT
701430     MOVE 001 TO WS-EL-SUB.                                       04  FRMT
701440                                                                  04  FRMT
701450 W45-RULE-SEARCH.                                                 04  FRMT
701460     IF WS-ELEM (WS-EL-SUB) EQUAL TO "X"                          04  FRMT
701470         GO TO W50-GET-RID-OF-DUMMY.                              04  FRMT
701480     IF WS-ELEM (WS-EL-SUB) NOT EQUAL TO SPACE                    04  FRMT
701490         GO TO W55-NOT-A-DUMMY.                                   04  FRMT
701500     IF WS-XM-SW NOT EQUAL TO "1"                                 04  FRMT
701510         GO TO W55-NOT-A-DUMMY.                                   04  FRMT
701520                                                                  04  FRMT
701530 W50-GET-RID-OF-DUMMY.                                            04  FRMT
701540     MOVE "1" TO WS-XM-SW.                                        04  FRMT
701550     MOVE WS-XM-DUM-CHAR TO WS-XM-CHAR (WS-XM-SUB).               04  FRMT
701560     ADD 001 TO WS-XM-SUB.                                        04  FRMT
701570     ADD 001 TO WS-EL-SUB.                                        04  FRMT
701580     GO TO W45-RULE-SEARCH.                                       04  FRMT
701590                                                                  04  FRMT
701600 W55-NOT-A-DUMMY.                                                 04  FRMT
701610     MOVE ZERO TO WS-XM-SW.                                       04  FRMT
701620     IF WS-ELEM (WS-EL-SUB) EQUAL TO SPACE                        04  FRMT
701630         ADD 001 TO WS-MK-SUB                                     04  FRMT
701640         ADD 001 TO WS-EL-SUB                                     04  FRMT
701650         ADD 001 TO WS-XM-SUB                                     04  FRMT
701660         GO TO W45-RULE-SEARCH.                                   04  FRMT
701670     MOVE WS-FIL-CHAR TO WS-MK-CHAR (WS-MK-SUB).                  04  FRMT
701680     MOVE WS-FIL-CHAR TO WS-XM-CHAR (WS-XM-SUB).                  04  FRMT
701690     ADD 001 TO WS-MK-SUB.                                        04  FRMT
701700     ADD 001 TO WS-XM-SUB.                                        04  FRMT
701710     IF WS-LIM-IND NOT EQUAL TO "L"                               04  FRMT
701720         MOVE WS-VER-CHAR TO WS-MK-CHAR (WS-MK-SUB)               04  FRMT
701730         MOVE WS-VER-CHAR TO WS-XM-CHAR (WS-XM-SUB)               04  FRMT
701740         ADD 001 TO WS-XM-SUB                                     04  FRMT
701750         ADD 001 TO WS-MK-SUB.                                    04  FRMT
701760     IF WS-ELEM (WS-EL-SUB) EQUAL TO "$"                          04  FRMT
701770         MOVE WS-VER-CHAR TO WS-MK-CHAR (WS-MK-SUB)               04  FRMT
701780         MOVE WS-VER-CHAR TO WS-XM-CHAR (WS-XM-SUB)               04  FRMT
701790         GO TO W60-FORMAT-HEADER.                                 04  FRMT
701800                             NOTE IF END OF RULES MAKE A DOUBLE   04  FRMT
701810                               LINE AND EXIT.                     04  FRMT
701820     IF WS-STUB-IND EQUAL TO SPACE                                04  FRMT
701830         MOVE WS-VER-CHAR TO WS-MK-CHAR (WS-MK-SUB)               04  FRMT
701840         MOVE WS-VER-CHAR TO WS-XM-CHAR (WS-XM-SUB)               04  FRMT
701850         ADD 001 TO WS-MK-SUB                                     04  FRMT
701860         ADD 001 TO WS-XM-SUB                                     04  FRMT
701870         MOVE "X" TO WS-STUB-IND.                                 04  FRMT
701880                             NOTE IF END OF STUB MAKE DOUBLE LINE.04  FRMT
701890     IF WS-LIM-IND NOT EQUAL TO "L"                               04  FRMT
701900         MOVE WS-FIL-CHAR TO WS-MK-CHAR (WS-MK-SUB)               04  FRMT
701910         MOVE WS-FIL-CHAR TO WS-XM-CHAR (WS-XM-SUB)               04  FRMT
701920         ADD 001 TO WS-MK-SUB                                     04  FRMT
701930         ADD 001 TO WS-XM-SUB.                                    04  FRMT
701940     MOVE SPACE TO WS-ELEM (WS-EL-SUB).                           04  FRMT
701950     GO TO W45-RULE-SEARCH.                                       04  FRMT
701960                                                                  04  FRMT
701970 W60-FORMAT-HEADER.                                               04  FRMT
701980     MOVE WS-MK-SUB TO WS-MSK-SIZE.                               04  FRMT
701990                             NOTE THESE ROUTINES FORMAT AND WRITE 04  FRMT
702000                               THE FORMATTED TABLE HEADINGS -     04  FRMT
702010                                  THEY WRITE THE DOUBLE           04  FRMT
702020                               HORIZONTAL LINE AT THE TOP OF THE  04  FRMT
702030                               TABLE AND PASS ANY INITIALIZATION  04  FRMT
702040                               CARDS.                             04  FRMT
702070                                                                  04  FRMT
702080 W70-COMPUTE-NO-PAGES.                                            04  FRMT
702090     ADD 001 TO WS-NO-PAGES.                                      04  FRMT
702100     ADD 132 TO WS-TTL-SIZE.                                      04  FRMT
702110     IF WS-TTL-SIZE LESS THAN WS-MSK-SIZE                         04  FRMT
702120         GO TO W70-COMPUTE-NO-PAGES.                              04  FRMT
702150                                                                  04  FRMT
702160 W75-WRITE-HEADER.                                                04  FRMT
702170     ADD 001 TO WS-WR-SUB.                                        04  FRMT
702180     MOVE WS-NAME-OF-TBL TO WR-LINE (WS-WR-SUB).                  04  FRMT
702190     IF WS-WR-SUB LESS THAN WS-NO-PAGES                           04  FRMT
702200         GO TO W75-WRITE-HEADER.                                  04  FRMT
702210     MOVE ZERO TO WS-WR-SUB.                                      04  FRMT
702220     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
702230     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
702240                             NOTE WRITE WORK FILE.                04  FRMT
702250                                                                  04  FRMT
702260 W80-PROCESS-INIT-CARDS.                                          04  FRMT
702270     MOVE "2" TO WS-GO-TO.                                        04  FRMT
702280                             NOTE WS-GO-TO EQUAL TO 2 MEANS ENTRY 04  FRMT
702290                               ROUTINE WILL GO TO                 04  FRMT
702300                               W85-CHECK-INIT-CARD.               04  FRMT
702310     IF SKIP-READ-SW EQUAL TO "1"                                 04  FRMT
702320         MOVE ZERO TO SKIP-READ-SW                                04  FRMT
702330         GO TO W85-CHECK-INIT-CARD.                               04  FRMT
702340     GO TO Y98-RETURN.                                            04  FRMT
702350                                                                  04  FRMT
702360 W85-CHECK-INIT-CARD.                                             04  FRMT
702370     IF WS-CC8-9 EQUAL TO "* "                                    04  FRMT
702380         GO TO W90-MOVE-INIT.                                     04  FRMT
702390     IF WS-CC8-24 EQUAL TO "CONDITION SECTION"                    04  FRMT
702400         GO TO W80-PROCESS-INIT-CARDS.                            04  FRMT
702410     IF WS-CC8-21 EQUAL TO "ACTION SECTION"                       04  FRMT
702420         GO TO W80-PROCESS-INIT-CARDS.                            04  FRMT
702430     IF WS-CC (8) NOT EQUAL TO SPACE                              04  FRMT
702440         GO TO X00-PROCESS-RULES.                                 04  FRMT
702450                             NOTE IF NOT INITIALIZATION CARD GO   04  FRMT
702460                               DO THE RULES.                      04  FRMT
702470                                                                  04  FRMT
702480 W90-MOVE-INIT.                                                   04  FRMT
702490     IF INIT-BOX-IND EQUAL TO ZERO                                04  FRMT
702500         MOVE "X" TO WS-CORN-IND                                  04  FRMT
702510         MOVE WS-MSK-SIZE TO INIT-BOX-IND                         04  FRMT
702520         MOVE 070 TO WS-MSK-SIZE                                  04  FRMT
702530         PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.           04  FRMT
702540     MOVE INIT-CARD-COL-8-72 TO WR-LINE-4-ON (1).                 04  FRMT
702550     MOVE WS-VER-CHAR TO WR-LINE-1-3 (1).                         04  FRMT
702560     MOVE WS-VER-CHAR TO WR-CHAR (70).                            04  FRMT
702570     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
702580     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
702590                             NOTE WRITE WORK FILE.                04  FRMT
702600                                                                  04  FRMT
702610 W95-MOVE-INIT-END.                                               04  FRMT
702620     GO TO W80-PROCESS-INIT-CARDS.                                04  FRMT
702630                                                                  04  FRMT
702640 X00-PROCESS-RULES.                                               04  FRMT
702650     IF INIT-BOX-IND NOT EQUAL TO ZERO                            04  FRMT
702660         MOVE "X" TO WS-CORN-IND                                  04  FRMT
702670         PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT            04  FRMT
702680         MOVE INIT-BOX-IND TO WS-MSK-SIZE                         04  FRMT
702690         MOVE ZERO TO INIT-BOX-IND                                04  FRMT
702695         MOVE WS-WR-WK TO TYPE-IO                                 04  FRMT
702696         PERFORM Y30-INPUT-OUTPUT                                 04  FRMT
702697         MOVE WS-WR-WK TO TYPE-IO                                 04  FRMT
702698         PERFORM Y30-INPUT-OUTPUT                                 04  FRMT
702699         MOVE WS-WR-WK TO TYPE-IO                                 04  FRMT
702700         PERFORM Y30-INPUT-OUTPUT.                                04  FRMT
702710     MOVE SPACE TO WORK-RECORD.                                   04  FRMT
702720     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
702730     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
702740                             NOTE WRITE WORK FILE                 04  FRMT
702750                               - A BLANK LINE TO SEPARATE THE     04  FRMT
702760                               INITIALIZATION CARDS FROM THE REST 04  FRMT
702770                               OF THE TABLE.                      04  FRMT
702780     MOVE "X" TO WS-CORN-IND.                                     04  FRMT
702790                             NOTE THIS IS THE TOP LINE SO ADD THE 04  FRMT
702800                               CORNER CHARACTERS.                 04  FRMT
702810     PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.               04  FRMT
702820     IF WS-LIM-IND NOT EQUAL TO "L"                               04  FRMT
702830         PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.           04  FRMT
702840     MOVE WS-RL1S TO WS-ARRAY.                                    04  FRMT
702850     MOVE "1" TO WS-1ST-RULE.                                     04  FRMT
702860     GO TO X40-WRITE-ARRAY.                                       04  FRMT
702870                             NOTE GO FORMAT THE RL1 CARDS.        04  FRMT
702880                                                                  04  FRMT
702890                                                                  04  FRMT
702900     NOTE ********************************************************04  FRMT
702910     *       THESE ROUTINES READ THE TABLE BODY CARDS AND        *04  FRMT
702920     *       TAKE THE FOLLOWING ACTIONS -                        *04  FRMT
702930     *                                                           *04  FRMT
702940     *     * CONDITION ROW, AC-    * WRITES A WORK FILE RE-      *04  FRMT
702950     *       ION ROW, OR STUB        CORD CONTAINING ONLY A      *04  FRMT
702960     *       CONTINUATION CARDS      MASK, EDITS "WS-ARRAY"      *04  FRMT
702970     *                               INTO THE MASK AND WRITES    *04  FRMT
702980     *                               AGAIN, WRITES A HORIZONTAL  *04  FRMT
702990     *                               LINE, MOVES THE CARD INTO   *04  FRMT
703000     *                               THE LOW ORDER END OF        *04  FRMT
703010     *                               "WS-ARRAY"                  *04  FRMT
703020     *                                                           *04  FRMT
703030     *     * RULE CONTINUATION     * PLACE IN "WS-ARRAY" AT      *04  FRMT
703040     *       CARDS                   LEFT-MOST UNUSED LOCATION   *04  FRMT
703050     *                                                           *04  FRMT
703060     *     * CONDITION SECTION,    * PASS                        *04  FRMT
703070     *       ACTION SECTION, AND                                 *04  FRMT
703080     *       COMMENT CARDS                                       *04  FRMT
703090     *                                                           *04  FRMT
703100     *     * RULE FREQUENCY CARD   * SAVE                        *04  FRMT
703110     *                                                           *04  FRMT
703120     *     * TEND CARD             * WRITE A HORIZONTAL LINE,    *04  FRMT
703130     *                               WRITE A TRAILER RECORD,     *04  FRMT
703140     *                               CLOSE THE WORK FILE AND     *04  FRMT
703150     *                               OPEN THE PRINT FILE         *04  FRMT
703160     ************************************************************.04  FRMT
703170                                                                  04  FRMT
703180                                                                  04  FRMT
703190 X05-PROCESS-CARDS.                                               04  FRMT
703200     MOVE "3" TO WS-GO-TO.                                        04  FRMT
703205     GO TO Y98-RETURN.                                            04  FRMT
703210                             NOTE WS-GO-TO EQUAL TO 3 MEANS ENTRY 04  FRMT
703220                               ROUTINE WILL GO TO                 04  FRMT
703230                               X10-FORMAT-CARDS.                  04  FRMT
703240                                                                  04  FRMT
703250 X10-FORMAT-CARDS.                                                04  FRMT
703260     IF WS-CC8-11 EQUAL TO "TEND"                                 04  FRMT
703270         GO TO X40-WRITE-ARRAY.                                   04  FRMT
703280     IF WS-CC8-24 EQUAL TO "CONDITION SECTION"                    04  FRMT
703290         GO TO X05-PROCESS-CARDS.                                 04  FRMT
703300     IF WS-CC8-9  EQUAL TO "* "                                   04  FRMT
703310         MOVE "1" TO STUB-CON-SW-1                                04  FRMT
703320         GO TO X40-WRITE-ARRAY.                                   04  FRMT
703330     IF WS-CC8-9 NOT EQUAL TO "F"                                 04  FRMT
703340         GO TO X15-NOT-FREQ-CD.                                   04  FRMT
703350     EXAMINE BUFFER TALLYING UNTIL FIRST ",".                     04  FRMT
703355     MOVE TALLY TO TALLX.                                         PDP-10
703360     IF TALLX GREATER THAN 69                                     PDP-10
703370         GO TO X35-PROCESS-ACTION.                                04  FRMT
703380     MOVE WS-CC10-72 TO WS-FREQ.                                  04  FRMT
703390     GO TO X05-PROCESS-CARDS.                                     04  FRMT
703400                                                                  04  FRMT
703410 X15-NOT-FREQ-CD.                                                 04  FRMT
703420     IF WS-CC8-21 EQUAL TO "ACTION SECTION"                       04  FRMT
703430         GO TO X05-PROCESS-CARDS.                                 04  FRMT
703440     IF WS-CC8-9  EQUAL TO "R "                                   04  FRMT
703450         GO TO X20-PROCESS-RULE-CONT.                             04  FRMT
703460     IF WS-CC8-9  EQUAL TO SPACE                                  04  FRMT
703470         GO TO X25-PROCESS-STUB-CONT.                             04  FRMT
703480     IF WS-CC8-9  EQUAL TO "C "                                   04  FRMT
703490         GO TO X30-PROCESS-CONDITION.                             04  FRMT
703500     IF WS-CC8-9  EQUAL TO "A "                                   04  FRMT
703510         GO TO X35-PROCESS-ACTION.                                04  FRMT
703520     MOVE 310  TO WS-ERR-CODE.                                    04  FRMT
703530     GO TO Y20-CANCEL.       NOTE CANCEL FORMATTING BECAUSE OF    04  FRMT
703540                               UNIDENTIFIABLE CARD.               04  FRMT
703550                                                                  04  FRMT
703560 X20-PROCESS-RULE-CONT.                                           04  FRMT
703570     IF WS-1ST-COND EQUAL TO SPACE                                04  FRMT
703580         MOVE 300 TO WS-ERR-CODE                                  04  FRMT
703590         GO TO Y20-CANCEL.   NOTE CANCEL FORMATTING BECAUSE OF    04  FRMT
703600                               RULE CONTINUATION CARD BEFORE      04  FRMT
703610                               FIRST CONDITION.                   04  FRMT
703620     ADD 001 TO WS-EL-SUB.                                        04  FRMT
703630     IF WS-EL-SUB GREATER THAN 005                                04  FRMT
703640         MOVE 321 TO WS-ERR-CODE                                  04  FRMT
703650         GO TO Y20-CANCEL.   NOTE CANCEL FORMATTING BECAUSE OF    04  FRMT
703660                               OVER FIVE RULE CONTINUATIONS.      04  FRMT
703670     MOVE WS-CC10-72 TO WS-ELEM-CD (WS-EL-SUB).                   04  FRMT
703680     GO TO X05-PROCESS-CARDS.                                     04  FRMT
703690                                                                  04  FRMT
703700 X25-PROCESS-STUB-CONT.                                           04  FRMT
703710     MOVE "1" TO STUB-CON-SW-1.                                   04  FRMT
703720     IF WS-1ST-COND EQUAL TO SPACE                                04  FRMT
703730         MOVE 330 TO WS-ERR-CODE                                  04  FRMT
703740         GO TO Y20-CANCEL.   NOTE CANCEL FORMATTING BECAUSE OF    04  FRMT
703750                               STUB CONTINUATION BEFORE THE FIRST 04  FRMT
703760                               CONDITION.                         04  FRMT
703770     GO TO X40-WRITE-ARRAY.                                       04  FRMT
703780                                                                  04  FRMT
703790 X30-PROCESS-CONDITION.                                           04  FRMT
703800     IF WS-1ST-ACT NOT EQUAL TO SPACE                             04  FRMT
703810         MOVE 350 TO WS-ERR-CODE                                  04  FRMT
703820         GO TO Y20-CANCEL.   NOTE CANCEL FORMATTING BECAUSE OF    04  FRMT
703830                               CONDITION FOLLOWING ACTIONS.       04  FRMT
703840     IF WS-1ST-COND NOT EQUAL TO SPACE                            04  FRMT
703850         GO TO X40-WRITE-ARRAY.                                   04  FRMT
703860     MOVE 001 TO WS-EL-SUB.                                       04  FRMT
703870     MOVE WS-CC10-72 TO WS-ELEM-CD (1).                           04  FRMT
703880     MOVE "X" TO WS-1ST-COND.                                     04  FRMT
703890     GO TO X05-PROCESS-CARDS.                                     04  FRMT
703900                             NOTE ABOVE 4 LINES FOR FIRST         04  FRMT
703910                               CONDITION ONLY.                    04  FRMT
703920                                                                  04  FRMT
703930 X35-PROCESS-ACTION.                                              04  FRMT
703940     IF WS-1ST-COND EQUAL TO SPACE                                04  FRMT
703950         MOVE "X" TO WS-1ST-COND                                  04  FRMT
703960         MOVE "X" TO WS-1ST-ACT.                                  04  FRMT
703970     IF WS-1ST-ACT EQUAL TO SPACE                                 04  FRMT
703980         MOVE "Y" TO WS-1ST-ACT.                                  04  FRMT
703990                                                                  04  FRMT
704000 X40-WRITE-ARRAY.                                                 04  FRMT
704010     IF STUB-CON-SW-2 EQUAL TO "1"                                04  FRMT
704020         MOVE "0" TO STUB-CON-SW-2                                04  FRMT
704030         GO TO X45-EDIT-TEXT.                                     04  FRMT
704040     MOVE WS-MASK TO WORK-RECORD.                                 04  FRMT
704050     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
704060     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
704070                             NOTE WRITE WORK FILE.                04  FRMT
704080                                                                  04  FRMT
704090 X45-EDIT-TEXT.                                                   04  FRMT
704100     MOVE WS-MASK TO WORK-RECORD.                                 04  FRMT
704110     MOVE 001 TO WS-EL-SUB.                                       04  FRMT
704120     MOVE 001 TO WS-WR-SUB.                                       04  FRMT
704130     MOVE 001 TO WS-XM-SUB.                                       04  FRMT
704140                                                                  04  FRMT
704150 X50-EDIT-LOOP.                                                   04  FRMT
704160     IF SAVE-COMMENT-FLAG EQUAL TO "* "                           04  FRMT
704170         GO TO X60-EDIT-COMMENTS.                                 04  FRMT
704180     IF SAVE-COMMENT-FLAG EQUAL TO "F "                           04  FRMT
704190         MOVE "F" TO WR-CHAR (2).                                 04  FRMT
704200                                                                  04  FRMT
704210 X55-EDIT-LOOP-1-1.                                               04  FRMT
704220     IF WS-XM-CHAR (WS-XM-SUB) EQUAL TO WS-XM-DUM-CHAR            04  FRMT
704230         ADD 001 TO WS-XM-SUB                                     04  FRMT
704240         ADD 001 TO WS-EL-SUB                                     04  FRMT
704250         GO TO X55-EDIT-LOOP-1-1.                                 04  FRMT
704260     GO TO X65-EDIT-LOOP-1.                                       04  FRMT
704270                                                                  04  FRMT
704280 X60-EDIT-COMMENTS.                                               04  FRMT
704290     IF WS-EL-SUB NOT LESS THAN 075                               04  FRMT
704300         GO TO X65-EDIT-LOOP-1.                                   04  FRMT
704310     IF WS-WR-SUB NOT GREATER THAN 004                            04  FRMT
704320         GO TO X65-EDIT-LOOP-1.                                   04  FRMT
704330     MOVE "*" TO WR-CHAR (2).                                     04  FRMT
704340     MOVE SPACE TO WR-CHAR (WS-WR-SUB).                           04  FRMT
704350                                                                  04  FRMT
704360 X65-EDIT-LOOP-1.                                                 04  FRMT
704370     IF WR-CHAR (WS-WR-SUB) EQUAL TO SPACE                        04  FRMT
704380         MOVE WS-ELEM (WS-EL-SUB) TO WR-CHAR (WS-WR-SUB)          04  FRMT
704390         ADD 001 TO WS-EL-SUB.                                    04  FRMT
704400     IF WR-CHAR (WS-WR-SUB) EQUAL TO WS-FIL-CHAR                  04  FRMT
704410         MOVE SPACE TO WR-CHAR (WS-WR-SUB).                       04  FRMT
704420     ADD 001 TO WS-WR-SUB.                                        04  FRMT
704430     ADD 001 TO WS-XM-SUB.                                        04  FRMT
704440     IF SAVE-COMMENT-FLAG NOT EQUAL TO "* "                       04  FRMT
704450         GO TO X67-NOT-A-COMMENT.                                 04  FRMT
704460     IF WS-EL-SUB LESS THAN 73                                    04  FRMT
704470         GO TO X60-EDIT-COMMENTS.                                 04  FRMT
704480                                                                  04  FRMT
704490 X67-NOT-A-COMMENT.                                               04  FRMT
704495     IF WS-WR-SUB NOT GREATER THAN WS-MSK-SIZE                    04  FRMT
704500         GO TO X50-EDIT-LOOP.                                     04  FRMT
704510     IF WS-WR-SUB GREATER THAN 660                                04  FRMT
704520         MOVE 380 TO WS-ERR-CODE                                  04  FRMT
704530         GO TO Y20-CANCEL.   NOTE CANCEL FORMATTING BECAUSE       04  FRMT
704535                               MAXIMUM TABLE SIZE EXCEEDED.       04  FRMT
704540     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
704550     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
704560                             NOTE WRITE WORK FILE.                04  FRMT
704570     IF WS-1ST-RULE EQUAL TO "1"                                  04  FRMT
704580         MOVE "2" TO WS-1ST-RULE                                  04  FRMT
704590         MOVE WS-RL2S TO WS-ARRAY                                 04  FRMT
704600         GO TO X45-EDIT-TEXT.                                     04  FRMT
704610                             NOTE IF FORMATTING RL1 CARDS THEN GO 04  FRMT
704620                               DO THE RL2 CARDS.                  04  FRMT
704630     MOVE SPACES TO WS-ARRAY.                                     04  FRMT
704640                                                                  04  FRMT
704650 X70-WRITE-LINE.                                                  04  FRMT
704660     IF WS-LIM-IND EQUAL TO "L"                                   04  FRMT
704670         GO TO X72-KEEP-WRITING.                                  04  FRMT
704680     IF STUB-CON-SW-1 EQUAL TO ZERO                               04  FRMT
704690         PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.           04  FRMT
704700                                                                  04  FRMT
704710 X72-KEEP-WRITING.                                                04  FRMT
704720     IF STUB-CON-SW-1 EQUAL TO "1"                                04  FRMT
704730         MOVE "1" TO STUB-CON-SW-2                                04  FRMT
704740         MOVE ZERO TO STUB-CON-SW-1.                              04  FRMT
704750     IF WS-1ST-ACT EQUAL TO "Y"                                   04  FRMT
704760         MOVE "X" TO WS-1ST-ACT                                   04  FRMT
704770         PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.           04  FRMT
704780     IF WS-1ST-RULE EQUAL TO "2"                                  04  FRMT
704790         MOVE "3" TO WS-1ST-RULE                                  04  FRMT
704800         PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.           04  FRMT
704810     IF WS-1ST-RULE EQUAL TO "3"                                  04  FRMT
704820         MOVE "4" TO WS-1ST-RULE                                  04  FRMT
704830         GO TO X10-FORMAT-CARDS.                                  04  FRMT
704840     MOVE 001 TO WS-EL-SUB.                                       04  FRMT
704850     MOVE WS-CC10-72 TO WS-ELEM-CD (1).                           04  FRMT
704860     MOVE WS-CC8-9   TO SAVE-COMMENT-FLAG.                        04  FRMT
704870     IF WS-CC8-11 NOT EQUAL TO "TEND"                             04  FRMT
704880         GO TO X05-PROCESS-CARDS.                                 04  FRMT
704890                                                                  04  FRMT
704900 X75-END-OF-TABLE.                                                04  FRMT
704910     MOVE "X" TO WS-CORN-IND.                                     04  FRMT
704920     PERFORM Y00-HORZ-LINE THRU Y10-HORZ-LINE-EXIT.               04  FRMT
704930     MOVE SPACE TO WORK-RECORD.                                   04  FRMT
704940     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
704950     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
704960                             NOTE WRITE WORK FILE.                04  FRMT
704970     MOVE WS-FREQ TO WORK-RECORD.                                 04  FRMT
704980     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
704990     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705000                             NOTE WRITE WORK FILE.                04  FRMT
705010 X80-WRITE-TRAILER.                                               04  FRMT
705020     MOVE "TEND" TO WR-LINE (1).                                  04  FRMT
705030     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
705040     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705050                             NOTE WRITE WORK FILE.                04  FRMT
705060     MOVE ZERO TO WS-WR-SUB.                                      04  FRMT
705070     MOVE ZERO TO WS-XM-SUB.                                      04  FRMT
705080                                                                  04  FRMT
705090 X85-CLOSE-WORK-FILE.                                             04  FRMT
705100     MOVE WS-CL-WK TO TYPE-IO.                                    04  FRMT
705110     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705120                             NOTE CLOSE WORK FILE.                04  FRMT
705130                                                                  04  FRMT
705140 X90-WRITE-OUTPUT.                                                04  FRMT
705150     ADD 001 TO WS-WR-SUB.                                        04  FRMT
705160     ADD 001 TO WS-XM-SUB.                                        04  FRMT
705170     MOVE WS-OP-WK-IP TO TYPE-IO.                                 04  FRMT
705180     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705190                             NOTE OPEN INPUT WORK FILE.           04  FRMT
705200     MOVE WS-RD-WK TO TYPE-IO.                                    04  FRMT
705210     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705220                             NOTE READ WORK FILE.                 04  FRMT
705230                             NOTE THESE ROUTINES PASS THE WORK    04  FRMT
705240                               FILE UP TO 5 TIMES - EACH TIME     04  FRMT
705250                               COPYING A 132 POSITION PORTION OF  04  FRMT
705260                               IT TO THE PRINT FILE               04  FRMT
705270                                 A BLANK HEADING DENOTES THE END. 04  FRMT
705275     MOVE WR-LINE (WS-WR-SUB) TO PR-LINE.                         04  FRMT
705280     IF PR-LINE EQUAL TO SPACE                                    04  FRMT
705290         MOVE ZERO TO WS-GO-TO                                    04  FRMT
705300         MOVE WS-CL-WK TO TYPE-IO                                 04  FRMT
705310         PERFORM Y30-INPUT-OUTPUT                                 04  FRMT
705320         GO TO Y98-RETURN.                                        04  FRMT
705330                             NOTE WS-GO-TO EQUAL TO ZERO MEANS    04  FRMT
705340                               ENTRY ROUTINE WILL GO TO           04  FRMT
705350                               W05-CHECK-FOR-TABLE.               04  FRMT
705360     MOVE "1" TO PR-CAR-CTL.                                      04  FRMT
705370     MOVE WS-WR-PR TO TYPE-IO.                                    04  FRMT
705380     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705390                             NOTE WRITE PRINT LINE RECORD.        04  FRMT
705400     IF LINECT NOT EQUAL TO ZERO                                  04  FRMT
705410         MOVE 001 TO LINECT.                                      04  FRMT
705420     MOVE "-" TO PR-CAR-CTL.                                      04  FRMT
705430     MOVE SPACE TO PR-LINE.                                       04  FRMT
705440                                                                  04  FRMT
705450 X95-PRINT-READ.                                                  04  FRMT
705460     MOVE WS-WR-PR TO TYPE-IO.                                    04  FRMT
705470     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705480                             NOTE WRITE PRINT LINE RECORD.        04  FRMT
705490     MOVE SPACE TO PR-LINE.                                       04  FRMT
705500     MOVE WS-RD-WK TO TYPE-IO.                                    04  FRMT
705510     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705520                             NOTE READ WORK FILE.                 04  FRMT
705530     IF WR-LINE (1) EQUAL TO "TEND"                               04  FRMT
705540         GO TO X85-CLOSE-WORK-FILE.                               04  FRMT
705550     MOVE SPACE TO PR-CAR-CTL.                                    04  FRMT
705560     MOVE WR-LINE (WS-WR-SUB) TO PR-LINE.                         04  FRMT
705570     EXAMINE PR-LINE REPLACING ALL "_" BY " ".                    PDP-10
705580                             NOTE THE ABOVE LINE REPLACES THE FIL 04  FRMT
705590                               CHARACTER  WITH BLANKS             04  FRMT
705600                                  ACTUAL FILL CHAR IS DESCRIBED   04  FRMT
705610                               IN WS-FIL-CHAR AS A 12-0 PUNCH     04  FRMT
705611                               AND WILL NOT PRINT.                04  FRMT
705620     GO TO X95-PRINT-READ.                                        04  FRMT
705630                                                                  04  FRMT
705640                                                                  04  FRMT
705650 Y00-HORZ-LINE.                                                   04  FRMT
705660     MOVE 001 TO WS-WK-SUB.                                       04  FRMT
705670                                                                  04  FRMT
705680 Y05-HORZ-LINE-LOOP.                                              04  FRMT
705690     MOVE WS-HOR-CHAR TO WR-CHAR (WS-WK-SUB).                     04  FRMT
705700     IF WS-WK-SUB LESS THAN WS-MSK-SIZE                           04  FRMT
705710         ADD 001 TO WS-WK-SUB                                     04  FRMT
705720         GO TO Y05-HORZ-LINE-LOOP.                                04  FRMT
705730     IF WS-CORN-IND NOT EQUAL TO SPACE                            04  FRMT
705740         MOVE SPACE TO WS-CORN-IND                                04  FRMT
705750         MOVE  WS-COR-CHAR TO WR-CHAR (1)                         04  FRMT
705760         MOVE  WS-COR-CHAR TO WR-CHAR (WS-MSK-SIZE).              04  FRMT
705770     MOVE WS-WR-WK TO TYPE-IO.                                    04  FRMT
705780     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
705790                             NOTE WRITE WORK FILE.                04  FRMT
705800                                                                  04  FRMT
705810 Y10-HORZ-LINE-EXIT.                                              04  FRMT
705820     EXIT.                                                        04  FRMT
705830                                                                  04  FRMT
705840                                                                  04  FRMT
705850 Y20-CANCEL.                                                      04  FRMT
705860     IF WS-CC8-13 EQUAL TO "DETAP "                               04  FRMT
705870         GO TO W05-CHECK-FOR-TABLE.                               04  FRMT
705880                             NOTE THIS SUBROUTINE IS BRANCHED TO  04  FRMT
705890                               WHEN AN ERROR IS FOUND IN A TABLE  04  FRMT
705900                               MAKING IT IMPOSSIBLE TO CONTINUE   04  FRMT
705910                               FORMATTING THAT TABLE              04  FRMT
705920                                 - IT DISPLAYS THE REASON         04  FRMT
705930                                      CLOSES THE WORK FILE        04  FRMT
705940                                 AND GOES TO W05-CHECK-FOR-TABLE. 04  FRMT
705950     MOVE WARNING TO TYPEMSG4.                                    04  FRMT
705960     MOVE WS-ERR-CODE TO ERR-MSSG-NUMBER.                         04  FRMT
705970     MOVE "FORMATTING ERROR NUMBER " TO E-P-BODY.                 04  FRMT
705980     MOVE WS-NAME-OF-TBL TO E-P-END-NAME.                         04  FRMT
705990     MOVE ". FORMATTING CAN NOT BE COMPLETED." TO E-P-END-PERIOD. 04  FRMT
706000     MOVE WS-WR-PR TO TYPE-IO.                                    04  FRMT
706010     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
706020                             NOTE WRITE PRINT LINE RECORD.        04  FRMT
706030     ADD 00001 TO WARN-ERR-COUNT.                                 04  FRMT
706040     MOVE WS-CL-WK TO TYPE-IO.                                    04  FRMT
706050     PERFORM Y30-INPUT-OUTPUT.                                    04  FRMT
706060                             NOTE CLOSE WORK FILE.                04  FRMT
706070     MOVE ZERO TO WS-GO-TO.                                       04  FRMT
706080     GO TO W05-CHECK-FOR-TABLE.                                   04  FRMT
706090                                                                  04  FRMT
706100 Y30-INPUT-OUTPUT.                                                04  FRMT
706110     IF TYPE-IO EQUAL TO WS-WR-WK                                 04  FRMT
706120         MOVE WORK-RECORD TO  FORMAT-WORK-FILE-BUFFER.            04  FRMT
706130     IF TYPE-IO EQUAL TO WS-WR-PR                                 04  FRMT
706140         MOVE PRINT-RECORD TO FORMAT-WORK-FILE-BUFFER.            04  FRMT
706150                             NOTE THIS SUBROUTINE CALLS THE       04  FRMT
706160                               INPUT/OUTPUT MODULE BEING USED IN  04  FRMT
706170                               CONJUNCTION WITH THE FORMATTER     04  FRMT
706180                             VALUES FOR TYPE-IO ARE AS FOLLOWS    04  FRMT
706190                                                                  04  FRMT
706200                            2  WS-OP-WK-OP   OPEN WORK FILE OUTPUT04  FRMT
706210                            3  WS-WR-WK      WRITE WORK FILE      04  FRMT
706220                            4  WS-OP-WK-IP   OPEN WORK FILE INPUT 04  FRMT
706230                            5  WS-RD-WK      READ WORK FILE       04  FRMT
706240                            6  WS-CL-WK      CLOSE WORK FILE      04  FRMT
706250                            8  WS-WR-PR      WRITE PRINT FILE.    04  FRMT
706255     MOVE TYPE-IO TO WS-TYPE-IO-JUST-DONE.                        04  FRMT
706260     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         04  FRMT
706270     IF WS-TYPE-IO-JUST-DONE EQUAL TO WS-WR-WK                    04  FRMT
706280         MOVE SPACE TO WORK-RECORD.                               04  FRMT
706290     IF WS-TYPE-IO-JUST-DONE EQUAL TO WS-WR-PR                    04  FRMT
706300         PERFORM Y35-PAGE-EJECT-CK THRU Y40-PAGE-EJECT-END        04  FRMT
706310         MOVE SPACE TO PRINT-RECORD.                              04  FRMT
706320     IF WS-TYPE-IO-JUST-DONE EQUAL TO WS-RD-WK                    04  FRMT
706330         MOVE FORMAT-WORK-FILE-BUFFER TO WORK-RECORD.             04  FRMT
706340     MOVE SPACE TO FORMAT-WORK-FILE-BUFFER.                       04  FRMT
706350                                                                  04  FRMT
706360 Y35-PAGE-EJECT-CK.                                               04  FRMT
706370     IF LINECT EQUAL TO ZERO                                      04  FRMT
706380         GO TO Y40-PAGE-EJECT-END.                                04  FRMT
706390     IF LINECT LESS THAN 055                                      04  FRMT
706400         ADD 001 TO LINECT                                        04  FRMT
706410         GO TO Y40-PAGE-EJECT-END.                                04  FRMT
706420     MOVE 001 TO LINECT.                                          04  FRMT
706430     MOVE EJECT-PAGE  TO TYPE-IO.                                 04  FRMT
706440     PERFORM U00-INPUT-OUTPUT-OPERATION THRU V99-IO-EXIT.         04  FRMT
706450                             NOTE TO ENABLE LINE-COUNTER AND      04  FRMT
706460                               EJECT, CHANGE LINECT VALUE IN W-S  04  FRMT
706470                               FROM 0 TO 1.                       04  FRMT
706480                                                                  04  FRMT
706490 Y40-PAGE-EJECT-END.                                              04  FRMT
706500     EXIT.                                                        04  FRMT
799000 Y98-RETURN.                                                      04  FRMT
799065                                                                  04  FRMT
799065     GO TO Y99-FORMATTED-TABLE-EXIT.                              PDP-10
799066 Y99-FORMATTED-TABLE-EXIT.                                        04  FRMT
799070     EXIT.                                                        04  FRMT
799075                                                                  04  FRMT
  
  <	 1G