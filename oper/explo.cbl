       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXPLOD.
       AUTHOR. STONE AND WEBSTER MGMT CONSULTANTS MIS DIVISION.
       DATE-WRITTEN. SPRING OF 1973.
       DATE-COMPILED. 2/28/73.
       REMARKS. THIS PROGRAM ELIMINATES FORM SEQUENCES THAT CONTAIN
               EDIT ERRORS, PRINTS THE ERROR REPORT, EXPLODES THE
               GOOD INPUT INTO THE CREW CHANGE FILE THE EQUIPMENT
               FILE AND THE LEAK DETAIL FILE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CHANNEL (1) IS LINE-1.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DETAIL-FILE-2, ASSIGN TO DSK RECORDING MODE IS ASCII.
           SELECT INPUT-ERROR,  ASSIGN TO DSK RECORDING MODE IS ASCII.
           SELECT LEAK-DETAIL, ASSIGN TO DSK RECORDING MODE IS ASCII.
           SELECT CREW-CHANGE-FL ASSIGN TO DSK RECORDING MODE IS ASCII.
           SELECT ERR-REPORT, ASSIGN TO DSK RECORDING MODE IS ASCII.
           SELECT EQUIPMENT-FL, ASSIGN TO DSK, RECORDING MODE IS ASCII.
       DATA DIVISION.
       FILE SECTION.
       FD  ERR-REPORT VALUE OF IDENTIFICATION IS "ERRPT    "
           RECORD CONTAINS 136 CHARACTERS.
       01  PRNT           PIC X(132).
       01  PRLN1.
           02  FILLER    PIC X(9).
           02 PCRD.
            03 FILLER   PIC X(80).
            03 FILLER   PIC XX.
            03 PERR-MES   PIC X(41).
       01  PRLN2.
           02 PCRD-ID    PIC X(21).
           02 FILLER    PIC X(111).
        FD  DETAIL-FILE-2  VALUE OF IDENTIFICATION IS
             "CNED02   "
                BLOCK CONTAINS 89 CHARACTERS.
        01  LK-REC   PIC X(89).
        01  C-LEAK-DESC-01-REC.
                02  C-KEY.
                03  C-TYPE   PIC X.
                03  C-SERIAL   PIC X(5).
                03  C-NUMB   PIC XX.
                02  CUR-LEAK.
                03  CUR-LEAK-NUMB.
                04  LEAK-ALPHA-CUR   PIC XX.
                04  LEAK-NUMERIC-CUR   PIC X(5).
                02  CUR-LEAK-ID.
                03  CUR-LEAK-DIV   PIC X.
                03  CUR-LEAK-GDIST   PIC X.
                03  CUR-LEAK-GENFORE   PIC XX.
                03  CUR-LEAK-FOREMAN   PIC XX.
                02  OLD-LEAK.
                03  OLD-LEAK-NUMB.
               04  LEAK-ALPHA-OLD    PIC XX.
                04  LEAK-NUMERIC-OLD   PIC X(5).
                03  OLD-LEAK-DIV   PIC X.
                03  OLD-LEAK-GDIST   PIC X.
                03  OLD-LEAK-GENFORE   PIC XX.
                03  OLD-LEAK-FOREMAN   PIC XX.
                02  DATE.
                03  MONTH   PIC XX.
                03  DAY   PIC XX.
                03  YEAR   PIC XX.
                02  PLATE-NUMB.
                03  ALPHA-PLATE-NUMB   PIC X.
                03  NUM-PLATE-NUMB   PIC XX.
                02  LEAK-SOURCE   PIC XX.
                02  LEAK-CLASS   PIC X.
                02  FILLER   PIC X(43).
        01  C-LEAK-DESC-02-REC.
                02  FILLER   PIC X(8).
                02  ADDRESS-1   PIC X(24).
                02  ADDRESS-2   PIC X(24).
                02  ADDRESS-3   PIC X(24).
                02  FILLER PIC X(9).
        01  C-LEAK-DESC-03-THRU-08-REC.
                02  FILLER   PIC X(8).
                02  REPAIR-DESC OCCURS 5 TIMES.
                03  STREET-OR-WALK   PIC X.
                03  OPENING.
                04  WIDTH.
                06  WID1   PIC X.
                06  WID2   PIC X.
                04  LENGTH.
                06  LEN1   PIC X.
                06  LEN2   PIC X.
                03 REPAIR-CODE   PIC X(4).
                03  REPAIR-SIZE.
                05  REP-SZ.
                07  REP-SZ1   PIC X.
                07  REP-SZ2   PIC X.
                05  REP-FRAC.
                07  REP-FRAC1   PIC X.
                07  REP-FRAC2   PIC X.
                02  FILLER   PIC X(16).
       01  JOURNAL-PREC.
           02 JEDUMY.
               03 FILLER       PIC X.
               03  JELEAK-NO.
                   05  JELNO-ALP    PIC XX.
                   05  JEL-NUM     PIC X(5).
               03  PPOINT.
                   05  PPHRS       PIC XXX.
                   05   PPMIN      PIC XX.
               03  EXCVCOR.
                   05  EXCHRS      PIC XXX.
                   05   EXCMIN     PIC XX.
               03  RXCOR.
                   05  RXCHRS      PIC XXX.
                   05  RXCMIN      PIC XX.
               03  BFCOR.
                   05  BFCHRS      PIC XXX.
                   05  BFMIN       PIC XX.
               03  JESIGN          PIC X.
           02  JEDESCRP.
               03  JEPDATE.
                   05  JEPMO       PIC XX.
                   05  JEPDAY      PIC XX.
                   05  JEPYR       PIC XX.
               03  JEPNARRATIVE    PIC X(36).
           02  FILLER          PIC X(18).
       01  JEPREC.
           03  FILLER           PIC X(8).
           03  JEPTM OCCURS 4 TIMES.
               05  JEPHRS       PIC 999.
               05  JEPMIN       PIC 99.
           03  FILLER           PIC X(61).
        01  C-LEAK-DESC-09-THRU-97-REC.
                02  FILLER   PIC X(8).
                02  FREE-FORM-ALPHA-DESC   PIC X(42).
                02  FILLER   PIC  X(39).
        01  C-LEAK-DESC-98-REC.
                02  FILLER   PIC X(8).
                02  CLOSE-DUPL-OF-LEAK-NUMB.
                03  DUPL-LEAK-ALPHA   PIC XX.
                03  DUPL-LEAK-NUM   PIC X(5).
                02  NUMB-DAYS-RPTED-CONST-LEAK  PIC XXX.
                02  CLOSED-BY-CONST   PIC X.
                02  LEAK-CLOSED-BY-FOLLOWUP.
                03  NEW-LEAK-NUMB.
                04  NEW-LEAK-ALPHA   PIC XX.
                04  NEW-LEAK-NUM   PIC X(5).
                02  NO-GAS   PIC X.
                02  DATE-98.
                04  CDMO   PIC XX.
                04  CDDAY   PIC XX.
                04  CDYR   PIC XX.
                02  FILLER   PIC X(56).
        01  C-TIME-SHEET-01-OR-02-REC.
                02  FILLER   PIC X(8).
                02  MEN-ASSIGNED OCCURS 4 TIMES.
                03  MAN-NUMB   PIC X(5).
                03  MAN-IN.
                04  IN-HOUR   PIC XX.
                04  IN-MIN   PIC XX.
                04  IOT-FLAG   PIC X.
                03  MAN-OUT.
                04  OUT-HOUR   PIC XX.
                04  OUT-MIN   PIC XX.
                04  OOT-FLAG   PIC X.
                02  FILLER   PIC X(21).
        01  C-TIME-SHEET-03-REC.
                02  FILLER   PIC X(8).
                02  TDATE.
                03  TMONTH   PIC XX.
                03  TDAY   PIC XX.
                03  TYEAR   PIC XX.
                02  FILLER   PIC X.
                02  C-QP.
                04  SHIFT   PIC X.
                04  DISTRICT   PIC XX.
                04  VEHICLE-NUMB OCCURS 3 TIMES.
               05  VHNBR.
                06  ALPHA-VEH   PIC XXXX.
                06  NUM-VEH   PIC XXXX.
               05  VHTME.
                06  VTIME-IN.
                08  VHRIN  PIC XX.
                08  VMININ  PIC XX.
                06  VIOT-FLAG   PIC X.
                06  VTIME-OUT.
                08  VHROUT  PIC XX.
                08  VMINOUT  PIC XX.
                06  VOOT-FLAG   PIC X.
                02  FILLER   PIC X(17).
        01  D2-RECORDS-NORMAL.
                02  NORMAL-REC   PIC X(80).
                02  FILLER   PIC X(9).
        01  D2-TIME-SHEET-EXTENDED.
                02  D2-KEY.
                04  D2-CTYPE   PIC X.
                04  D2-SERIAL    PIC XXXXX.
                04  D2-CNUMB   PIC XX.
                02  D2-WORK-DETL.
                03  D2-WORK-UNIT OCCURS 3 TIMES.
                06  D2-LEAK-NUMB.
                08  D2-LEAK-NUMB-ALPHA   PIC XX.
                08  D2-LEAK-NUMB-NAM   PIC XXXXX.
                06  D2-WORK-CODE   PIC XX.
                06 D2-START-TIME.
                08  D2-HRS-START   PIC 99.
                08  D2-MIN-START   PIC XX.
                06  D2-FINISH-TIME.
                08  D2-HRS-FINISH   PIC 99.
                08  D2-MIN-FINISH   PIC XX.
                06  FILLER   PIC X(5).
                04  D2-EXT-TIME.
                06  D2-EXT-HRS   PIC S999.
                06  D2-EXT-MIN   PIC S99.
        01  C-EQUIP-MSTR.
                02  C-KEY-TYPE-DATE.
                03  FILLER   PIC X.
                03  EDATE.
                04  EMONTH   PIC XX.
                04  EDAY   PIC XX.
                04  EYEAR   PIC XX.
                03  EM-DIST   PIC XX.
                03  FILLER   PIC X.
                03  E-SHIFT   PIC X.
                02  EQUIP-OUT OCCURS 5 TIMES.
                03  EQUIP-NUMB.
                05  EQN-ALPH   PIC X(4).
                05  EQP-NUMB   PIC X(4).
                03  EQP-TM.
                05  EQUIP-OUT-HRS   PIC XX.
                05  EQUIP-OUT-REASON   PIC XX.
                02  FILLER   PIC X(9).
                02  FILLER   PIC X(44).
        01  JOURNAL-ENTRY-01-EQUIP-SYSTEM.
                02  CKTDX.
                03  FILLER   PIC X.
                03  CARD-DATE.
                04  EQUIP-MONTH   PIC XX.
                04  EQUIP-DAY   PIC XX.
                04  EQUIP-YEAR   PIC XX.
                03  EQUIP-DIV   PIC X.
                03  EQUIP-SHIFT   PIC X.
                03  EQUIP-DIST   PIC XX.
                02  TRUCK-HOURS-CORR.
                03  TRUCK-REPAIR-CORR.
                04  TR-REP-CORR-HRS   PIC XXX.
                04  TR-REP-CORR-MIN   PIC XX.
                03  TRUCK-NO-CREW-CORR.
                04  TR-NO-CREW-CORR-HRS   PIC XXX.
                04  TR-NO-CREW-CORR-MIN   PIC XX.
                03  TRUCK-START-YARD-CORR.
                04  TR-START-YARD-CORR-HRST   PIC XXX.
                04  TR-START-YARD-CORR-MIN   PIC XX.
                03  TRUCK-STANDBY-CORR.
                04  TR-STANDBY-CORR-HRS   PIC XXX.
                04  TR-STANDBY-CORR-MIN   PIC XX.
                03  TRUCK-TRAVEL-CORR.
                04  TR-TRAVEL-CORR-HRS   PIC XXX.
                04  TR-TRAVEL-CORR-MIN   PIC XX.
                03  TRUCK-PRODUC-CORR.
                04  TR-PRODUC-CORR-HRS   PIC XXX.
                04  TR-PRODUC-CORR-MIN   PIC XX.
                03  TRUCK-WAITING-CORR.
                04  TR-WAITING-CORR-HRS   PIC XXX.
                04  TR-WAITING-CORR-MIN  PIC XX.
                03  TRUCK-END-YARD-CORR.
                04  TR-END-YARD-CORR-HRS  PIC XXX.
                04  TR-END-YARD-CORR-MIN  PIC XX.
                03  TRUCK-NUMB-LOC-CORR   PIC 999.
                02  FILLER   PIC X(35).
        01  JE-EQUIP-01.
                02  FILLER   PIC X(11).
                02  JE-01-TIME-CORR OCCURS 8 TIMES.
                04  HRS-EQUIP-JE   PIC 999.
                04  MIN-EQUIP-JE   PIC 99.
                02  FILLER   PIC X(38).
        01  JOURNAL-ENTRY-02-EQUIP-SYSTEM.
                02  FILLER   PIC X(11).
                02  MAN-HRS-TIME-CORR.
                04  MH-STRT-IN-YD.
                06  HRS-STRT-EQUIP-JE-02   PIC XXX.
                06  MIN-STRT-EQUIP-JE-02   PIC XX.
                06  HRS-STND-BY-EQUIP-JE-02   PIC XXX.
                06  MIN-STND-BY-EQUIP-JE-02   PIC XX.
                06  HRS-TRVL-EQUIP-JE-02   PIC XXX.
                06  MIN-TRVL-EQUIP-JE-02   PIC XX.
                06  HRS-PDCTVE-EQUIP-JE-02   PIC XXX.
                06  MIN-PDCTVE-EQUIP-JE-02   PIC XX.
                06  HRS-WTNG-EQUIP-JE-02   PIC XXX.
                06  MIN-WTNG-EQUIP-JE-02   PIC XX.
                06  HRS-ETIY-EQUIP-JE-02   PIC XXX.
                06  MIN-ETIY-EQUIP-JE-02   PIC XX.
                06  HRS-EQUIP-FAIL-EQUIP-JE-02  PIC XXX.
                06  MIN-EQUIP-FAIL-EQUIP-JE-02  PIC XX.
                02  FILLER   PIC X(43).
        01  JE-EQUIP-02.
                02  FILLER   PIC X(11).
                02  JE-02-TIME-CORR OCCURS 7 TIMES.
                04  HRS-EQUIP-JE-02   PIC 999.
                04  MIN-EQUIP-JE-02   PIC 99.
                02  FILLER   PIC X(43).
        FD  INPUT-ERROR VALUE OF IDENTIFICATION IS "INERR    "
                 BLOCK CONTAINS 121 CHARACTERS.
        01  ERROR-REC.
                02  HKEYX.
                03  H-KEY.
                04  H-C-TYPE   PIC X.
                04  H-SERIAL   PIC X(5).
                04  H-CNUM   PIC X(2).
                03  FILLER   PIC XXX.
               02  ERRREC.
                03  CARD-IMAGE PIC X OCCURS 80 TIMES.
                03  ERR-CODE   PIC X OCCURS 30 TIMES.
        01  ERR-REC2.
                03  FILLER   PIC X(11).
                03  EXCD   PIC X(80).
                03  ALL-ERRS  PIC X(30).
        01  ERR-REC3.
                03  EQKEY.
                05  FILLER   PIC X.
                05  EQDATEX.
                07  EQMOX   PIC XX.
                07  EQDAYX   PIC XX.
                07  EQYRX   PIC XX.
                05  FILLER   PIC X.
                05  EQSHIFT   PIC X.
                05  EQDIST   PIC XX.
                03  FILLER   PIC X(110).
        FD  LEAK-DETAIL VALUE OF IDENTIFICATION IS "LKDET    "
                RECORD CONTAINS 60 CHARACTERS.
        01  LK-DETL   PIC X(60).
       01  DLEAK-JE.
           03  FILLER                  PIC X(14).
           03 JE-PNPNT.
               05  JE-PP-HRS           PIC S999.
               05  JE-PP-MIN           PIC S99.
           03 JE-EXCVTE.
               05  JE-EX-HRS           PIC S999.
               05 JE-EX-MIN            PIC S99.
           03  JE-REPR.
               05  JE-RX-HRS           PIC S999.
               05  JE-RX-MIN           PIC S99.
           03  JEBKFL.
               05  JE-BKF-HRS          PIC S999.
               05  JE-BKF-MIN          PIC S99.
       01  DLK-JE.
           03 FILLER                   PIC X(14).
           03  LK-JECOR OCCURS 4 TIMES.
               05  LK-JE-HRS           PIC S999.
               05 LK-JE-MIN            PIC S99.
           03 FILLER                   PIC X(26).
        01  DADDRESS-1.
                02  RCKEY.
                04  RCLEAK.
                06  RCLAPH   PIC XX.
                06  RCLNUM   PIC 9(5).
                04  RR-TYPE   PIC 9.
                02  DLINE1   PIC X(24).
                02  DLINE2   PIC X(24).
                02  FILLER   PIC XXXX.
        01  DADDRESS-2.
                02  FILLER   PIC X(8).
                02  DLINE3   PIC X(24).
                02  FILLER   PIC X(28).
        01  DMAN-REC.
                02  DLEAK-KEY.
                05  DLEAK-NO.
                07  DLEAK-ALPH   PIC XX.
                07  DLEAK-NUMB   PIC 9(5).
                05  DREC-TYPE   PIC 9.
                02  DMREC-DATE.
                04  DMREC-YR   PIC 99.
                04  DMREC-MO   PIC 99.
                04  DMREC-DA   PIC 99.
                02  DMAN-DETAIL OCCURS 2 TIMES.
                04  DMAN-NO   PIC 9(5).
                04  DPINPOINTING.
                06  DPP-HRS   PIC 99.
                06  DPP-MIN   PIC 99.
                04  DEXCAVATING.
                06  DEXC-HRS   PIC 99.
                06  DEXC-MIN   PIC 99.
                04  DREPAIRING.
                06  DREP-HRS   PIC 99.
                06  DREP-MIN   PIC 99.
                04  DBACK-FILLING.
                06  DBKFL-HRS   PIC 99.
                06  DBKFL-MIN   PIC 99.
                02  FILLER   PIC XXXX.
        01  DMRC.
                03  FILLER   PIC X(14).
                03  DMDT OCCURS 2 TIMES.
                05  DMNUM   PIC 9(5).
                05  DMTM OCCURS 4 TIMES.
                07  DMTHR   PIC 99.
                07  DMTMIN   PIC 99.
                03  FILLER   PIC XXXX.
        01  DLEAK-REPAIR-DESC.
                02  FILLER   PIC X(14).
                02  DCARD-NO   PIC 99.
                02  DREPAIR OCCURS 3 TIMES.
               04  DS-OR-W         PIC X.
                04  DHOLE-SZ.
                06  DWIDTH   PIC XX.
                06  DLENGTH   PIC XX.
                04  DREPR-CODE   PIC XXXX.
                04  DPIPE-SZ.
                06  DPIPE-IN   PIC 99.
                06  DPIPE-FRAC   PIC 99.
                02  FILLER   PIC XXXXX.
        01  DNARRATIVE.
                02  FILLER   PIC X(16).
                02  DNARRATIVE-UF   PIC X(42).
                02  DCODEN   PIC X.
                02  FILLER   PIC X.
        FD  EQUIPMENT-FL VALUE OF IDENTIFICATION IS "EQDET    "
                RECORD CONTAINS 86 CHARACTERS.
        01  EQUP-DAILY.
                02  DEQUIP-KEY-2.
                04  DEQUIP-KEY-1.
                06  DEQP-DIV   PIC X.
                06  DEQP-DIST   PIC 99.
                06  DEQP-SHIFTY   PIC 99.
                06  DEQP-REC-TYPE   PIC 9.
                04  DEQUIP-NO.
                06  DEQP-ALPH   PIC XXXX.
                06  DEQP-NUMB   PIC 9999.
                02  DEQUIP-DATE.
                04  DEQP-MO   PIC 99.
                04  DEQP-DAY   PIC 99.
                04  DEQP-YR   PIC 99.
                02  DEQP-NUM-MEN   PIC 99.
           02  DEQPTIMES.
               03 DEQUIP-OUT-BROKE.
                04  DEQP-OUT-BRK-HRS   PIC 99.
                04  DEQP-OUT-BRK-MIN   PIC 99.
               03   DEQUIP-NO-MEN.
                04  DEQP-NO-MEN-HRS   PIC 99.
                04  DEQP-NO-MEN-MIN   PIC 99.
               03   DEQUIP-START.
                04  DEQP-STRT-HRS   PIC 99.
                04  DEQP-STRT-MIN   PIC 99.
               03   DEQUIP-STND-BY.
                04  DEQP-STND-BYHRS   PIC 99.
                04  DEQP-STND-BY-MIN   PIC 99.
               03   DEQUIP-TRVL.
                04  DEQP-TRVL-HRS   PIC 99.
                04  DEQP-TRVL-MIN   PIC 99.
               03   DEQUIP-PROD.
                04  DEQP-PROD-HRS   PIC 99.
                04  DEQP-PROD-MIN   PIC 99.
               03   DEQUIP-WAIT.
                04  DEQP-WAIT-HRS   PIC 99.
                04  DEQP-WAIT-MIN   PIC 99.
               03   DEQUIP-END.
                04  DEQP-END-HRS   PIC 99.             04  DEQP-END-MIN   PIC 99.
                02  DEQP-LOCTNS   PIC 999.
                02  FILLER   PIC X(28).
                02  DEQUIP-EDIT-ERROR   PIC X.
        01  DAILYEQP.
                03  FILLER   PIC X(22).
                03  DLYEQP-TM OCCURS 8 TIMES.
                05  DLYHRS   PIC S99.
                05  DLYMIN   PIC S99.
                03  FILLER  PIC X(32).
        FD  CREW-CHANGE-FL VALUE OF IDENTIFICATION IS "CRWCG    "
                RECORD CONTAINS 20 CHARACTERS.
        01  DCREW-CHANGE.
                02  DCC-KEY.
                04  DCC-MAN-NO   PIC 9(5).
                04  DCC-REC-TYPE   PIC 9.
                02  DCC-TIME.
                04  DCC-HRS   PIC 99.
                04  DCC-MIN   PIC 99.
                02  DCC-SEQ-NO   PIC X(5).
                02  DCC-DIVISION   PIC X.
                02  DCC-DISTRICT   PIC 99.
                02  DCC-SHIFT   PIC 99.
       WORKING-STORAGE SECTION.
        01  DEQUIP-YTD.
               02  DEQKEY.
                   04  DQYDIV     PIC X.
                   04  DQYDIST    PIC 99.
                   04  DQYSHIFT    PIC 99.
                   04 DQYRECTYPE   PIC 9.
                02  DEQUIP-YTD-OUT-BROKE.
                04  DEQP-YOUT-BRK-HRS   PIC S9(7).
                04  DEQP-YOUT-BRK-MIN   PIC S99.
                02  DEQUIP-YTD-NO-MEN.
                04  DEQP-YNO-MEN-HRS   PIC S9(7).
                04  DEQP-YNO-MEN-MIN   PIC S99.
                02  DEQUIP-YTD-STRT.
                04  DEQP-YSTRT-HRS   PIC S9(7).
                04  DEQP-YSTRT-MIN   PIC S99.
                02  DEQUIP-YTD-STND-BY.
                04  DEQP-YSTND-BY-HRS   PIC S9(7).
                04  DEQP-YSTND-BY-MIN   PIC S99.
                02  DEQUIP-YTD-TRVL.
                04  DEQP-YTRVL-HRS   PIC S9(7).
                04  DEQP-YTRVL-MIN   PIC S99.
                02  DEQUIP-YTD-PROD.
                04  DEQP-YPROD   PIC S9(7).
                04  DEQP-YPROD   PIC S99.
                02  DEQUIP-YTD-WAIT.
                04  DEQP-YWAIT-HRS   PIC S9(7).
                04  DEQP-YWAIT-MIN   PIC S99.
                02  DEQUIP-YTD-END.
                04  DEQP-YEND-HRS   PIC S9(7).
                04  DEQP-YEND-MIN   PIC S99.
                02  DEQP-YLOCTNS   PIC S9(7).
        01  QYTD REDEFINES DEQUIP-YTD.
                03  FILLER   PIC X(6).
                03  QYTDTME OCCURS 8 TIMES.
                05  QYTDHRS   PIC S9(7).
                05  QYTDMIN   PIC S99.
                03  FILLER   PIC X(8).
        01  DMAN-HRS.
                02  FILLER   PIC X(6).
                02  DMHR-STRT.
                04  DMHRS-STRT-HRS   PIC S9(7).
                04  DMHRS-STRT-MIN   PIC S99.
                02  DMHRS-STND-BY.
                04  DMHRS-STND-BY-HRS   PIC S9(7).
                04  DMHRS-STND-BY-MIN   PIC S99.
                02  DMHRS-TRVL.
                04  DMHRS-TRVL-HRS   PIC S9(7).
                04  DMHRS-TRVL-MIN   PIC S99.
                02  DMHRS-PROD.
                04  DMHRS-PROD-HRS   PIC S9(7).
                04  DMHRS-PROD-MIN   PIC S99.
                02  DMHRS-WAIT.
                04  DMHRS-WAIT-HRS   PIC S9(7).
                04  DMHRS-WAIT-MIN   PIC S99.
                02  DMHRS-END.
                04  DMHRS-END-HRS   PIC S9(7).
                04  DMHRS-END-MIN   PIC S99.
                02  DMHRS-EQUP-FAIL.
                04  DMHRS-EQP-FL-HRS   PIC S9(7).
                04  DMHRS-EQP-FL-MIN   PIC S99.
                02  FILLER   PIC X(17).
        01  QMHRS REDEFINES DMAN-HRS.
                03  QKEY.
                05  QDIV   PIC X.
                05  QDIST   PIC 99.
                05  QSHIFT   PIC 99.
                05  QREC-TYPE   PIC 9.
                03  QHRSDTL OCCURS 7 TIMES.
                05  QHRS   PIC S9(7).
                05  QMIN   PIC S99.
                03  FILLER   PIC X(17).
        01  DL-FIXED-DATA.
                02  XLEAK-KEY.
                04  XLEAK-NO.
                06  XLEAK-ALPH   PIC XX.
                06  XLEAK-NUMB  PIC 9(5).
                04  XREC-TYPE   PIC 9.
                02  D-LK-ID-CUR.
                03  DDIVISION   PIC 9.
                03  DDISTRICT   PIC 9.
                03  DGEN-FMAN   PIC XX.
                03  DFMAN   PIC XX.
                02  DDATE.
                04  DMO   PIC 99.
                04  DDAY   PIC 99.
                04  DYR   PIC 99.
                02  DPLATE.
                04  DPLATE-ALPH   PIC X.
                04  DPLATE-NO   PIC 99.
                02  DOLD-LEAK.
                04  DOLD-LEAK-ALPH   PIC XX.
                04  DOLD-LEAK-NUMP   PIC 9(5).
                02  DLEAK-SOURCE   PIC XX.
                02  DLEAK-CLASS   PIC X.
                02  DREPORTING.
                04  DCRITERIA   PIC X.
                04  DRPT-CTR   PIC S999.
                02  FILLER   PIC XX.
                02  DCLOSE-CODE   PIC X.
                02  DDUPE-OR-REOPEN-NO.
                04  DNEW-NO-ALPH   PIC XX.
                04  DNEW-NO-NUMB   PIC 9(5).
                02  DDATE-CLOSED.
                04  DCLOS-NO   PIC 99.
                04  DCLOS-DAY   PIC 99.
                04  DCLOS-YR   PIC 99.
                02  CONSTRUCTION-CLOSE   PIC 999.
                02  FILLER   PIC XXX.
                02  DCODENW   PIC X.
       01  SV-LEAK-NUM.
           03  SV-LAPH    PIC XX VALUE IS ZERO.
           03  SV-LNUM    PIC 9(5) VALUE IS ZERO.
       01  SV-DATE.
           03  SV-MO    PIC 99.
           03  SV-DAY    PIC 99.
           03  SV-YR    PIC 99.
       01  SV-LEAK-NUM2.
           03 SV-2ALPHA    PIC XX VALUE IS ZERO.
           03 SV-2NUM    PIC 9(5) VALUE IS ZERO.
       01  SV-EQP PIC X(57).
       01  SV-QP  REDEFINES SV-EQP.
           03  SV-SHIFT    PIC 9.
           03  SV-DISTRICT    PIC 99.
           03  SVQP OCCURS 3 TIMES.
               05  TRUCKNUMB.
                 07  TRK-ALPHA    PIC XXXX.
                 07  TRK-NO    PIC 9999.
               05  TRXDMX.
                 07  TRK-TMIN.
                   09  TRK-HRSIN    PIC 99.
                   09  TRK-MININ    PIC 99.
                   09  TRK-IOTF    PIC X.
                 07  TRK-TMOUT.
                   09  TRK-HRSOUT    PIC 99.
                   09  TRK-MINOUT    PIC 99.
                   09  TRK-OOTF    PIC X.
       01 SVMNDXX      PIC X(248).
       01  SV-MNDETAIL REDEFINES SVMNDXX OCCURS 8 TIMES.
           03  MAN-DTL.
               05  MANNO    PIC 9(5).
               05  TME-IN.
                 07  HRIN    PIC 99.
                 07  MININ    PIC 99.
                 07  IOTF    PIC X.
               05  TME-OUT.
                 07  HROUT    PIC 99.
                 07  MINOUT    PIC 99.
                 07  OOTF    PIC X.
           03  LEAK-TIMES  OCCURS 4 TIMES.
                 07  LKHRS    PIC 99.
                 07  LKMIN    PIC 99.
       01  ADDERS.
           02  START-MINUTES    PIC S99.
           02  FINISH-MINUTES    PIC S99.
           02  NET-MINUTES    PIC S999.
           02  START-HOURS    PIC S9(7).
           02  FINISH-HOURS    PIC S9(7).
           02  NET-HOURS    PIC S9(7).
           02  SVENET-HRS    PIC S9(7).
           02  SVENET-MIN    PIC S99.
       01  WORK-CODEX.
           03 FILLER PIC X(29) VALUE IS "PP01EX02RX03BF04LH00VB00TL00Z".
               03 FILLER PIC X(3) VALUE IS "Z00".
       01  WORK-CODEY  REDEFINES WORK-CODEX OCCURS 8 TIMES.
           03  LK-CDE    PIC XX.
           03  LKNDX    PIC 99.
       01  EQUIP-CODEX.
           02 FILLER PIC X(29) VALUE IS "LH0000YS0301SB0402VB0007YE080".
           02 FILLER PIC X(19)     VALUE IS "6TL0705**0004ZZ0000".
       01  EQUIP-CODEY  REDEFINES EQUIP-CODEY OCCURS 8 TIMES.
           03  EQ-CDE    PIC XX.
           03  EQNDX    PIC 99.
           03  MHNDX    PIC 99.
       01  ERCODEMES.
           03  FILLER PIC X(12) VALUE IS "CODE NUMBER ".
           03  ERCODENO   PIC XX.
           03 FILLER PIC X(12) VALUE IS " IS IN ERROR".
       01  EREWD       PIC Z9.
       01  SV-FINISH.
           03  SV-FHRS    PIC 99.
           03  SV-FMIN    PIC 99.
       01  TRAVELX.
           03  TXHR    PIC S99.
           03  TXMIN    PIC S99.
       01  SVTRTM.
           03  SVTREXTO.
               05  TREXT-HRSO    PIC S99.
               05  TREXT-MINO    PIC S99.
           03  SVTREXTI.
               05  TREXT-HRSI    PIC S99.
               05  TREXT-MINO    PIC S99.
       01  SVQKEY.
           03  FILLER    PIC X.
           03  QDATE.
               05  QMO    PIC 99.
               05  QDAY    PIC 99.
               05  QYR    PIC 99.
           03  FILLER    PIC X.
           03  SVQSHIFT PIC 9.
           03  SVQDIST  PIC 99.
       01  SXKEY  REDEFINES SVQKEY.
           02  SV-KEY.
               03  SV-C-TYPE    PIC X.
               03  SV-SERIAL.
                 05  SV-JCNUM    PIC 99.
                 05  FILLER    PIC 999.
           03   FILLER    PIC XXX.
               03  SV-CNUM    PIC 99.
           02   FILLER    PIC XXX.
       01  HDINGS.
           03 ER-ADF PIC X(21) VALUE IS "LEAK DESCRIPTION     ".
           03 ER-GH  PIC X(21) VALUE IS "DAILY ROUTE SHEET    ".
           03 ER-K   PIC X(21) VALUE IS "EQUIPMENT MASTER LIST".
           03 ER-P   PIC X(21) VALUE IS "LEAK JOURNAL ENTRY   ".
           03 ER-ST  PIC X(21) VALUE IS "TRUCK HOURS JOURNAL  ".
           03 ER-WX  PIC X(21) VALUE IS "MAN HOURS JOURNAL    ".
           03 SYS-M1 PIC X(21) VALUE IS "SYSTEM ERROR         ".
       01  HDNG REDEFINES HDINGS PIC X(21) OCCURS 7 TIMES.
       01  HDTBL PIC X(7) VALUE IS "1234567".
       01  SYSMES.
           03 FILLER PIC X(29) VALUE "AN ERROR HAS OCCURRED IN THE ".
           03 FILLER PIC X(29) VALUE "PROGRAM PRECEEDING, NAMED EDI".
           03 FILLER PIC X(29) VALUE "T. PLEASE CALL SYSTEM PROGRAM".
           03 FILLER PIC XXXX  VALUE "MER.".
       01  EDIT-WORDS.
           02 E1  PIC X(35) VALUE "NVALID CARD CODE NOTHING CHECKED   ".
           02 E2  PIC X(35) VALUE "SEQUENCE ERROR                     ".
           02 E3  PIC X(35) VALUE "MUST BE NUMERIC                    ".
           02 E4  PIC X(35) VALUE "MAN NUMBER MISSING                 ".
           02 E5  PIC X(35) VALUE "HOURS ERROR GREATER THAN 12        ".
           02 E6  PIC X(35) VALUE "MINUTES ERROR GREATER THAN 59      ".
           02 E7  PIC X(35) VALUE "CARD NUMBER INVALID NOTHING CHECKED".
           02 E8  PIC X(35) VALUE "SHIFT EQUAL 1, 2, OR 3             ".
           02 E9  PIC X(35) VALUE "INCOMPLETE SEQUENCE OR MISSING CARD".
           02 EA  PIC X(35) VALUE "INVALID WORK CODE                  ".
           02 EB  PIC X(35) VALUE "NO LEAK NUMBER FOR THIS WORK CODE  ".
           02 EC  PIC X(35) VALUE "TIME INCOMPLETE                    ".
           02 ED  PIC X(35) VALUE "LEAK  JE HAS NO EXPLANATION        ".
           02 EE  PIC X(35) VALUE "OVER TWO SHIFTS ENTERED            ".
           02 EF  PIC X(35) VALUE "EQUIP MASTER EXCEPT. HRS INVALID   ".
           02 EG  PIC X(35) VALUE "EQUIP MASTER EXCEPT. LACKS REASON  ".
           02 EH  PIC X(35) VALUE "HOURS BUT NO LEAK NUMBER           ".
           02 EI  PIC X(35) VALUE "NO LEAK NUMBER OR INVALID NUMBER   ".
           02 EJ  PIC X(35) VALUE "GEOGRAPHICAL DISTRICT NOT 1,2 OR 3 ".
           02 EK  PIC X(35) VALUE "PLATE NUMBER INVALID               ".
           02 EL  PIC X(35) VALUE "STREET OR WALK CODE INVALID        ".
           02 EM PIC X(35) VALUE  "INCOMPLETE ENTRY S OR W OPENING    ".
           02 EN  PIC X(35) VALUE "NO OPENING FOR S OR W CODE         ".
           02 EO  PIC X(35) VALUE "NO REPAIR CODE ENTRY               ".
           02 EP  PIC X(35) VALUE "INVALID FRACTION                   ".
           02 EQ  PIC X(35) VALUE "NO SIZE                            ".
           02 ER  PIC X(35) VALUE "CAN NOT USE A-01 CRD ONLY,USE D CRD".
           02 ES  PIC X(35) VALUE "MUST HAVE START-TIME TRUCK CHANGE  ".
           02 ET  PIC X(35) VALUE "MUST HAVE TIME-OUT ON TRUCK CHANGE ".
           02 EU  PIC X(35) VALUE "MUST HAVE TRUCK NUMBERS IN SEQUENCE".
           02 EV  PIC X(35) VALUE "VEHICLE ENTRY ERROR EMBEDED SPACES ".
           02 EW  PIC X(35) VALUE "DUPLICATE OR WRONG CARD NUMBER     ".
           02 EX  PIC X(35) VALUE "SIGN INVALID OR MISSING            ".
       01  EW-TBL REDEFINES EDIT-WORDS PIC X(35) OCCURS 30 TIMES.
       01  ECDE PIC X(33) VALUE "123456789ABCDEFGHIJKLMNOPQRSTUVWX".
       01  ECD  REDEFINES ECDE  PIC X OCCURS 33 TIMES.
           77 NDX4          PIC 99.
           77 NDX5          PIC 99.
           77 NDX6          PIC 99 VALUE IS 1.
               77 NDX7                   PIC 99.
           77 NDX1          PIC 99.
           77 NDX2          PIC 99.
           77 NDX3          PIC 99.
           77 SV-WCDE       PIC XX.
           77 ED2           PIC Z9.
           77 LN-CTR        PIC 99.
           77 ADJT-SW       PIC 9  VALUE IS ZERO.
           77 LN-ADV        PIC 9.
           77 OVER-24-ERR   PIC X.
           77 ER-CTR        PIC 99 VALUE IS ZERO.
           77 OT-SW         PIC 9  VALUE IS ZERO.
           77 HL-CTYPE      PIC 9  VALUE IS ZERO.
           77 NO-TIME       PIC 9  VALUE IS ZERO.
           77 CRD-SW        PIC 9  VALUE IS ZERO.
           77 TCNDX         PIC 99 VALUE IS 1.
           77 NINES   PIC X(8) VALUE IS "99999999".
       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT  DETAIL-FILE-2
                INPUT-ERROR.
           OPEN OUTPUT
                LEAK-DETAIL,
                CREW-CHANGE-FL,
                EQUIPMENT-FL,
                ERR-REPORT.
           MOVE     ZEROS TO EQUP-DAILY   DCREW-CHANGE,LK-DETL,
               DMAN-HRS, QMHRS.
           MOVE 9 TO SV-SHIFT.
           MOVE  SPACES TO PRNT.
       START.
           PERFORM READ-DETAIL.
       STRT1.
           PERFORM READ-ERROR.
       STRT2.
           IF C-KEY EQUAL TO NINES GO TO EOJ.
           MOVE C-KEY TO SV-KEY.
           IF  H-KEY GREATER THAN C-KEY GO TO SYS-ERRG.
           IF C-TYPE EQUAL TO "A"  OR    C-TYPE EQUAL TO  "D"
               OR C-TYPE EQUAL TO "F" GO TO  LK-DESCRIP.
           IF C-TYPE EQUAL TO "G" OR  C-TYPE  EQUAL TO  "H"
               GO TO  TME-CRD.
           IF C-TYPE EQUAL TO "K"  GO TO EQUIP-MSTR.
           IF C-TYPE EQUAL TO "P"  GO TO  LK-JE.
           IF C-TYPE EQUAL TO  "S"  OR     C-TYPE EQUAL "T"
               GO TO  EQUIP-JE.
           IF C-TYPE  EQUAL TO "W"  OR  C-TYPE  EQUAL TO "X"
               GO TO  MAN-HRS-JE.
           GO TO SYS-ERR.
       SYS-ERR.
           MOVE SYS-M1 TO PCRD-ID.
           MOVE 2 TO LN-ADV.
           PERFORM WRITE-PRINT.
       SYER1.
           MOVE SYSMES TO PRNT.
       SYER2.
           PERFORM WRITE-PRINT.
           MOVE 1 TO LN-ADV.
       SYERA.
           MOVE LK-REC TO PCRD
           PERFORM WRITE-PRINT
       SYER3.
           PERFORM READ-DETAIL
           GO TO STRT2.
       SYS-ERRG.
           PERFORM SYS-ERR.
           MOVE SYSMES TO PRNT.
           PERFORM SYER2 THRU SYERA.
           GO TO STRT1.
       SUBTRACT-RTNE.
           IF START-MINUTES IS GREATER THAN FINISH-MINUTES
               ADD 60 TO FINISH-MINUTES
               SUBTRACT 1 FROM FINISH-HOURS
               SUBTRACT START-MINUTES FROM FINISH-MINUTES
               GIVING NET-MINUTES
                   ELSE SUBTRACT START-MINUTES FROM FINISH-MINUTES
                   GIVING NET-MINUTES.
           IF START-HOURS GREATER THAN FINISH-HOURS
               ADD 12 TO FINISH-HOURS, SUBTRACT START-HOURS FROM
               FINISH-HOURS GIVING NET-HOURS,
                   ELSE SUBTRACT START-HOURS FROM FINISH-HOURS GIVING
                   NET-HOURS.
       ADD-RTN.
           MOVE START-MINUTES TO NET-MINUTES.
           ADD FINISH-MINUTES TO NET-MINUTES.
           IF NET-MINUTES GREATER THAN 59, SUBTRACT 60 FROM NET-MINUTES
                   ADD 1 TO START-HOURS.
           MOVE START-HOURS TO NET-HOURS.
           ADD FINISH-HOURS TO NET-HOURS.
       ADD-RTNE-EX.
           MOVE NET-MINUTES TO FINISH-MINUTES.
           MOVE NET-HOURS TO FINISH-HOURS.
           MOVE ZEROS TO START-MINUTES, START-HOURS.
       EOJ SECTION.
       XWDEQ.
           IF DEQP-DIV IS NOT EQUAL TO ZERO
               PERFORM WRITE-DEQ.
       XWLKD.
           IF RCKEY IS NOT EQUAL TO ZEROS
               PERFORM WRITE-LKDTL.
       EWXFDT.
           IF CRD-SW IS EQUAL TO 1
               PERFORM WRITEFXDT.
       EWQYTD.
           IF DQYDIV IS NOT EQUAL TO ZERO
               PERFORM WRITEYTDQ.
O      XWMHRS.
           IF DEQP-DIV IS NOT EQUAL TO ZERO 
               PERFORM WRITEMHRS.
       EWPRT.
           IF PRNT IS NOT EQUAL TO SPACES
               PERFORM WRITEPRT.
       XWCCHNG.
           IF DCC-KEY IS NOT EQUAL TO ZEROS
               PERFORM WRITECCHG.
           CLOSE
               DETAIL-FILE-2
               INPUT-ERROR
               LEAK-DETAIL
               CREW-CHANGE-FL
               EQUIPMENT-FL
               ERR-REPORT.
           STOP RUN.
       WRITING SECTION.
       WRITE-LKDTL.
           WRITE LK-DETL.
           MOVE ZEROS TO LK-DETL.
       WRITE-DEQ.
           WRITE EQUP-DAILY.
           MOVE ZEROS TO EQUP-DAILY.
       WRITEMHRS.
           PERFORM XWDEQ.
           WRITE EQUP-DAILY FROM DMAN-HRS.
           MOVE ZEROS TO DMAN-HRS.
       WRITECCHG.
           WRITE DCREW-CHANGE.
           MOVE ZEROS TO DCREW-CHANGE.
       WRITEYTDQ.
           PERFORM XWDEQ.
           WRITE EQUP-DAILY FROM DEQUIP-YTD.
           MOVE ZEROS TO DEQUIP-YTD.
       WRITEFXDT.
           PERFORM XWLKD.
           WRITE LK-DETL FROM DL-FIXED-DATA.
           MOVE ZEROS TO DL-FIXED-DATA.
           MOVE ZERO TO CRD-SW.
       WRITEPRT.
           WRITE PRNT.
           MOVE SPACES TO PRNT.
       READ-DETAIL SECTION.
       RDS2.
           MOVE SPACES TO LK-REC.
           READ DETAIL-FILE-2 RECORD,
               AT END MOVE ZEROS TO LK-DETL, MOVE NINES TO C-KEY
               GO TO RD1.
           IF C-TYPE NOT EQUAL TO "G"
              AND C-TYPE NOT EQUAL TO "H"
               GO TO RD-EX.
           IF C-NUMB IS NOT EQUAL TO 3
               GO TO RD-EX.
           IF SV-SHIFT IS EQUAL TO 9
               GO TO RD-EX.
           IF SV-SHIFT IS EQUAL TO SHIFT
               AND  SV-DISTRICT IS EQUAL TO DISTRICT
               GO TO RD-EX.
       RD1.
           IF DEQUIP-KEY-1 IS NOT EQUAL TO ZEROS
               WRITE EQUP-DAILY,  MOVE 1 TO TCNDX,
               MOVE ZEROS TO EQUP-DAILY.
           IF  QKEY IS NOT EQUAL TO ZEROS
               WRITE EQUP-DAILY FROM DMAN-HRS.
               MOVE ZEROS TO DMAN-HRS.
       RD-EX.
           EXIT.
       READ-ERROR SECTION.
       RESSC3.
           IF H-KEY EQUAL TO NINES GO TO REEREX.
           MOVE SPACES TO ERROR-REC.
           READ INPUT-ERROR RECORD,
               AT END MOVE NINES TO H-KEY.
       REEREX.
           EXIT.
       WRITE-PRINT SECTION.
       WRPSC4.
           WRITE PRNT AFTER ADVANCING LN-ADV LINES.
           ADD LN-ADV TO LN-CTR.
           IF LN-CTR GREATER THAN 64 GO TO TOPOFP.
       WRPR1.
           MOVE  SPACES TO PRNT.
           GO TO WRPRX.
       WRITE-LK.
           WRITE LK-DETL.
           MOVE  SPACES TO LK-DETL.
       WRLXX.
           GO TO WRPRX.
       TOPOFP.
           MOVE 1 TO NDX7.
       TPPLP.
           IF NDX7 IS EQUAL TO HDTBL
               MOVE HDNG (NDX7) TO PCRD-ID
               WRITE PRNT AFTER ADVANCING LINE-1
               MOVE 1 TO LN-CTR
               GO TO WRPR1.
           ADD 1 TO NDX7.
           GO TO TPPLP.
       WRPRX.
           EXIT.
       ERR-PRINT.
           MOVE 1 TO LN-ADV.
           MOVE LK-REC TO PCRD.
           PERFORM WRITE-PRINT.
           MOVE EXCD TO PCRD.
           PERFORM WRITE-PRINT.
           MOVE 1 TO NDX3.
           MOVE 1 TO NDX2.
       ERP1.
           IF ERR-CODE (NDX3) IS EQUAL TO SPACES
               OR NDX3 IS GREATER THAN 30 GO TO ERPEX.
       ERP2.
           IF ECD  (NDX2) IS EQUAL TO ERR-CODE (NDX3)
               PERFORM WRITEER
               ADD 1 TO NDX3
               MOVE 1 TO NDX2
               GO TO ERP1.
           IF NDX2 IS GREATER THAN 30 
               GO TO ERPSYSER.
           ADD 1 TO NDX2.
           GO TO ERP2.
       WRITEER.
           MOVE EW-TBL (NDX2) TO PERR-MES.
           PERFORM WRITE-PRINT
       ERPSYSER.
           PERFORM SYER2.
           MOVE ALL-ERRS TO PCRD.
           MOVE EREWD  TO ERCODENO.
           MOVE NDX3 TO ERCODENO.
           MOVE ERCODEMES TO PERR-MES.
           PERFORM WRITE-PRINT.
           ADD 1 TO NDX3.
           MOVE 1 TO NDX2.
           GO TO ERP1.
       ERPEX.
           EXIT.
       LK-DESCRIP SECTION.
       LKD1.
           IF H-SERIAL NOT EQUAL TO C-SERIAL,  AND   H-C-TYPE NOT
               EQUAL TO C-TYPE GO TO LKD2.
           IF HL-CTYPE NOT EQUAL TO 1, MOVE 1 TO HL-CTYPE, MOVE ER-ADF
               TO PCRD-ID, MOVE 2 TO LN-ADV  PERFORM WRITE-PRINT.
       LKD4.
           MOVE LK-REC TO PCRD, MOVE 1 TO LN-ADV PERFORM WRITE-PRINT.
           IF H-KEY NOT EQUAL TO C-KEY  GO TO  LKD3.
           MOVE EXCD TO  PCRD.
           PERFORM ERR-PRINT  THRU ERPEX.
           PERFORM READ-ERROR.
       LKD3.
           PERFORM READ-DETAIL.
           IF SV-C-TYPE EQUAL TO C-TYPE AND C-SERIAL EQUAL TO
               SV-SERIAL GO TO LKD4.
           GO TO STRT2.
       LKD2.
           IF C-NUMB NOT EQUAL TO 1 GO TO LKD2C.
           MOVE CUR-LEAK-NUMB  TO  SV-LEAK-NUM.
           MOVE DATE TO SV-DATE
           IF C-TYPE EQUAL TO "A" GO TO LKD-EX.
           MOVE C-TYPE TO DCODENW.
           MOVE ZERO TO XREC-TYPE.
           MOVE 1 TO CRD-SW.
           MOVE CUR-LEAK-ID TO D-LK-ID-CUR.
           MOVE DATE TO DDATE.
           MOVE OLD-LEAK-NUMB TO DOLD-LEAK.
           MOVE PLATE-NUMB TO DPLATE.
           MOVE LEAK-SOURCE TO  DLEAK-SOURCE.
           MOVE LEAK-CLASS TO  DLEAK-CLASS.
           GO TO LKD-EX.
       LKD2C.
           MOVE C-TYPE TO DCODEN.
           MOVE SV-LEAK-NUM TO  RCLEAK.
           IF C-NUMB NOT EQUAL TO 2,  GO TO LKD3C.
           IF ADDRESS-1 IS EQUAL TO SPACES AND ADDRESS-2 IS EQUAL
               TO SPACES MOVE  3 TO  RR-TYPE MOVE ADDRESS-3 TO
               DLINE3, PERFORM WRITE-LK     GO TO LKD-EX.
           MOVE  2  TO    RR-TYPE.
           MOVE  ADDRESS-1 TO DLINE1.
           MOVE ADDRESS-2 TO DLINE2.
           PERFORM WRITE-LK.
           IF  ADDRESS-3 IS NOT EQUAL TO SPACES, MOVE 3 TO   RR-TYPE
               MOVE ADDRESS-3 TO DLINE3, PERFORM WRITE-LK.
           GO TO LKD-EX.
       LKD3C.
           IF C-NUMB GREATER THAN 8  GO TO LKD9C.
       LKD3C1.
           MOVE SV-MO TO DMREC-MO.
           MOVE SV-DAY TO DMREC-DA.
           MOVE SV-YR  TO DMREC-YR.
       LKD3CA.
           MOVE 1 TO  NDX1,  NDX2.
           MOVE 7 TO RR-TYPE.
       LKD3C2.
           IF REPAIR-DESC (NDX1) IS EQUAL TO SPACES GO TO LKD-EX.
           IF NDX1 IS GREATER THAN 5 GO TO LKD-EX.
           IF  NDX2 IS GREATER THAN 3, PERFORM WRITE-LK, MOVE 1 TO
               NDX2,  PERFORM  LKD3C1.
           MOVE STREET-OR-WALK (NDX1)  TO DS-OR-W (NDX2).
           IF WID1 (NDX1) IS  NOT EQUAL TO SPACES
               AND WID2 (NDX1) IS EQUAL TO SPACES
               MOVE WID1 (NDX1) TO WID2 (NDX1), MOVE SPACES TO
               WID1 (NDX1).
           IF LEN2 (NDX1) IS  NOT EQUAL TO  SPACES
               AND LEN1 (NDX1) IS EQUAL TO SPACES
               MOVE LEN2 (NDX1) TO LEN1 (NDX1), MOVE SPACES TO
               LEN2 (NDX1).
           MOVE OPENING (NDX1)  TO  DHOLE-SZ (NDX2).
           MOVE REPAIR-CODE (NDX1)  TO  DREPR-CODE (NDX2)
           IF  REP-FRAC (NDX1)  IS  NOT EQUAL TO  SPACES,
               MOVE REP-SZ2 (NDX1) TO  REP-FRAC1 (NDX1),
               MOVE REP-SZ1 (NDX1) TO  REP-SZ2 (NDX1),
               REP-FRAC (NDX1).
           IF REP-SZ1 (NDX1) IS NOT EQUAL TO SPACES
               AND REP-SZ2 (NDX1) IS EQUAL TO SPACES
               MOVE REP-SZ1 (NDX1)  TO  REP-SZ2 (NDX1)
               MOVE ZERO TO  REP-SZ1 (NDX1).
           MOVE REPAIR-SIZE (NDX1) TO  DPIPE-SZ (NDX2).
           ADD 1 TO NDX1.
           ADD 1 TO NDX2.
           GO TO LKD3C2.
       LKD9C.
           IF C-NUMB GREATER THAN 97 GO TO  LKD98.
           PERFORM LKD3C1.
           MOVE FREE-FORM-ALPHA-DESC TO DNARRATIVE.
           GO TO LKD-EX.
       LKD98.
           IF C-NUMB IS NOT EQUAL TO 98
               GO TO SYS-ERR.
           IF CLOSE-DUPL-OF-LEAK-NUMB NOT EQUAL TO SPACES,
               MOVE CLOSE-DUPL-OF-LEAK-NUMB TO DDUPE-OR-REOPEN-NO,
               MOVE "D" TO DCLOSE-CODE.
           IF  NUMB-DAYS-RPTED-CONST-LEAK NOT EQUAL TO SPACES
               MOVE NUMB-DAYS-RPTED-CONST-LEAK TO CONSTRUCTION-CLOSE.
           IF CLOSED-BY-CONST NOT EQUAL TO SPACES, MOVE CLOSED-BY-CONST
               TO  DCLOSE-CODE.
           IF LEAK-CLOSED-BY-FOLLOWUP NOT EQUAL TO SPACES,
               MOVE LEAK-CLOSED-BY-FOLLOWUP TO DDUPE-OR-REOPEN-NO.
           IF NO-GAS NOT EQUAL TO SPACES
               MOVE "N" TO DCLOSE-CODE.
           IF DATE   NOT EQUAL TO SPACES MOVE DATE   TO DDATE-CLOSED.
       LKD-EX.
           PERFORM READ-DETAIL.
           IF SV-SERIAL EQUAL TO C-SERIAL AND  C-TYPE EQUAL TO
               SV-C-TYPE,
               GO TO LKD1.
           IF CRD-SW EQUAL TO 1 WRITE LK-DETL FROM  DL-FIXED-DATA,
               MOVE SPACES TO LK-DETL, MOVE ZERO TO CRD-SW.
           GO TO STRT2.
       TME-CRD SECTION.
       TC1.
           IF H-SERIAL NOT EQUAL TO C-SERIAL,  AND  H-C-TYPE NOT
               EQUAL TO C-TYPE MOVE 1 TO NDX2, GO TO TC2.
           IF HL-CTYPE NOT EQUAL TO 2, MOVE 2 TO HL-CTYPE MOVE ER-GH
               TO  PCRD-ID MOVE 2 TO LN-ADV, PERFORM WRITE-PRINT.
       TC3.
           MOVE  LK-REC TO PCRD, MOVE 1 TO LN-ADV, PERFORM WRITE-PRINT.
           IF H-KEY NOT EQUAL TO              C-KEY GO TO TCEQUIPERR.
           MOVE EXCD TO PCRD.
           PERFORM ERR-PRINT THRU ERPEX.
           PERFORM READ-ERROR.
           NOTE
           THIS ROUTINE PUTS OUT A RECORD FOR EQUIPMENT
               REJECTED BY THE EDIT.
       TCEQUIPERR.
           IF C-NUMB IS NOT EQUAL TO 3
               GO TO TC4.
           MOVE 1 TO NDX1.
       TCEQ.
           IF NDX1 IS GREATER THAN 3
               OR VEHICLE-NUMB (NDX1) IS EQUAL TO SPACES
               OR VEHICLE-NUMB (NDX1) IS EQUAL TO ZEROS
               GO TO TC4.
           IF DEQP-DIV   IS NOT EQUAL TO ZERO
               WRITE EQUP-DAILY,
               MOVE ZEROS TO EQUP-DAILY.     NOTE THAT THIS AVOIDS
                          OVER-LAY OF RECORD IN THE HOLE.
           MOVE "M" TO DEQP-DIV.
           MOVE SHIFT TO  DEQP-SHIFTY.
           MOVE  ZERO  TO DEQP-REC-TYPE.
           MOVE  DISTRICT TO DEQP-DIST.
           MOVE "*" TO  DEQUIP-EDIT-ERROR.
           MOVE VHNBR (NDX1) TO DEQUIP-NO.
           MOVE  TDATE TO DEQUIP-DATE.
           WRITE EQUP-DAILY.
           MOVE ZEROS TO EQUP-DAILY.
           ADD 1 TO NDX1.
           GO TO TCEQ.
       TC4.
           PERFORM READ-DETAIL
           IF SV-C-TYPE EQUAL TO C-TYPE AND C-SERIAL EQUAL TO
               SV-SERIAL,GO TO TC3,
               ELSE GO TO STRT2.
       TC2.
           IF C-NUMB NOT EQUAL TO 1
               AND C-NUMB NOT EQUAL TO 2
               GO TO SYS-ERR.
           MOVE C-TYPE TO DCODEN.
           MOVE  ZEROS TO SVMNDXX.
           MOVE 1 TO NDX1, NDX2.
       TC2A.
           IF NDX1 GREATER THAN 4
               OR MAN-NUMB (NDX1) EQUAL TO SPACES, OR
               MAN-NUMB (NDX1) EQUAL TO ZEROS GO TO TC2B.
           MOVE MAN-NUMB (NDX1) TO MANNO (NDX2).
           IF MAN-IN (NDX1) IS NOT EQUAL TO SPACES
               AND MAN-IN (NDX1) IS NOT EQUAL TO ZEROS
               MOVE MAN-IN (NDX1) TO TME-IN (NDX2).
           MOVE IOT-FLAG (NDX1) TO IOTF (NDX2).
           IF MAN-OUT (NDX1) IS NOT EQUAL TO SPACES
               AND MAN-OUT (NDX1) IS NOT EQUAL TO ZEROS
               MOVE MAN-OUT (NDX1) TO TME-OUT (NDX2).
           MOVE OOT-FLAG (NDX1) TO OOTF (NDX2).
           ADD 1 TO NDX1.
           ADD 1 TO NDX2.
           GO TO TC2A.
       TC2B.
           PERFORM READ-DETAIL.
           IF C-TYPE NOT EQUAL TO SV-C-TYPE
               AND C-SERIAL NOT EQUAL TO SV-SERIAL
               GO TO SYS-ERR.
           IF C-NUMB EQUAL TO 2
               MOVE 1 TO NDX1
               GO TO TC2A.
           IF C-NUMB NOT EQUAL TO 3 GO TO SYS-ERR.
           MOVE C-TYPE TO DCODEN.
           MOVE C-QP TO SV-EQP.
           MOVE TDATE TO SV-DATE.
           PERFORM CHK-EQPT  THRU CHKTQX
       TC6.
           PERFORM FIX-SHIFTS THRU FIXSEX.
           MOVE 1 TO NDX1.
                NOTE THAT THIS ROUTINE SETS UP THE CREW CHANGE FILE
                   BEFORE READING THE WORK UNIT CARDS.
       TC6A.
           IF NDX1 GREATER THAN 8 OR MANNO (NDX1) EQUAL TO ZEROS
               GO TO TC7.
           IF  TME-IN (NDX1) EQUAL TO ZEROS   AND TME-OUT (NDX1)
               EQUAL TO ZEROS GO TO TC6BUMP.
           IF TME-IN (NDX1) NOT EQUAL TO ZEROS MOVE TME-IN (NDX1)
               TO DCC-TIME, MOVE ZERO TO DCC-REC-TYPE,
               PERFORM WRITE-CREW-CHANGE.
           IF TME-OUT (NDX1) NOT EQUAL TO ZEROS MOVE TME-OUT (NDX1)
               TO DCC-TIME, MOVE 1 TO DCC-REC-TYPE,
               PERFORM WRITE-CREW-CHANGE.
       TC6BUMP.
           ADD 1 TO NDX1.
           GO TO TC6A.
       WRITE-CREW-CHANGE.
           MOVE SV-SERIAL TO DCC-SEQ-NO.
           MOVE MANNO (NDX1) TO DCC-MAN-NO.
           MOVE "M" TO  DCC-DIVISION.
           MOVE DISTRICT TO DCC-DISTRICT.
           MOVE SHIFT TO DCC-SHIFT.
           WRITE DCREW-CHANGE.
             NOTE THAT THIS ROUTINE SETS UP RECORD THAT WILL BE
                  WRITTEN IN THE READ ROUTINE, THIS RECORD IS MAN HOURS.
       TC7.
           IF QDIV IS NOT EQUAL TO ZERO
               GO TO TC7A.  NOTE THAT IF THERE HAD BEEN A KEY BREAK
                                 THIS FIELD WOULD HAVE BEEN ZEROED.
           MOVE "M" TO QDIV.
           MOVE SV-SHIFT TO QSHIFT.
           MOVE SV-DISTRICT TO QDIST.
           MOVE 9 TO QREC-TYPE.
       TC7A.
           MOVE 1 TO TCNDX.
              NOTE THAT THIS ROUTINE WRITES A DAILY EQUIP RECORD
                   FOR EACH EQUIPMENT CHANGE AND ON EACH NEW SEQUENCE.
       TC7B.
           IF DEQP-DIV IS NOT EQUAL TO ZERO
               WRITE EQUP-DAILY
               MOVE ZEROS TO EQUP-DAILY.
       TC7BX.
           IF TRUCKNUMB (TCNDX) IS NOT EQUAL TO SPACES
               OR TRUCKNUMB (TCNDX) IS NOT EQUAL TO ZEROS
               MOVE SV-DATE TO DEQUIP-DATE
               MOVE TRUCKNUMB (TCNDX) TO DEQUIP-NO.
               MOVE "M" TO DEQP-DIV.
               MOVE SV-C-TYPE TO DEQUIP-EDIT-ERROR.
               MOVE ZERO TO DEQP-REC-TYPE
               MOVE SV-SHIFT TO DEQP-SHIFTY
               MOVE SV-DISTRICT TO DEQP-DIST,
               MOVE ZERO TO DEQP-REC-TYPE.
               NOTE THAT THIS ROUTINE COUNTS THE CREW.
       TC8A.
           IF MANNO (NDX2) EQUAL TO ZEROS
               OR NDX2 GREATER THAN 8
               GO TO TC8B.
           ADD 1 TO NDX2.
           GO TO  TC8A.
       TC8B.
           SUBTRACT 1 FROM NDX1.
           MOVE NDX1 TO DEQP-NUM-MEN.
           PERFORM READ-DETAIL. NOTE THIS SHOULD BE THE NUMBER 4
               CARD IN THE SEQUENCE.
           IF C-NUMB IS NOT EQUAL TO 4
               GO TO SYS-ERR.
       TC8X.
           IF C-TYPE IS NOT EQUAL TO SV-C-TYPE
               OR C-SERIAL IS NOT EQUAL TO SV-SERIAL
               GO TO SYS-ERR.
               MOVE ZERO TO NDX1.
       TC9.
               ADD 1 TO NDX1.
               IF D2-LEAK-NUMB (NDX1) IS NOT EQUAL TO "VOID   "
                   PERFORM ADJUST-TIME THRU ADJTEX
                   MOVE 1 TO ADJT-SW
                   MOVE D2-FINISH-TIME (NDX1) TO SV-FINISH
                   GO TO TC9C.
           IF NDX1 IS EQUAL TO 3
               PERFORM READ-DETAIL
               GO TO TC8X.
                    NOTE
           THIS ROUTINE ACCOUNTS FOR TIME AS FOLLOWS
               1  BY MAN FOR EACH STREET DEPARTMENT LEAK
               2  BY VEHICLE HOURS IN VARIOUS CATAGORIES
               2  BY MAN HOURS IN  THE SAME CATAGORIES AS
                        VEHICLE HOURS PLUS MAN HOURS LOST
                        DUE TO BROKEN EQUIPMENT.
       TC9B.
           IF D2-START-TIME (NDX1) IS EQUAL TO "OVER"
               MOVE 1 TO OT-SW
               ADD 1 TO NDX1
               GO TO TC10.
           PERFORM READ-DETAIL.
           IF C-TYPE IS NOT EQUAL TO SV-C-TYPE
               AND C-SERIAL IS NOT EQUAL TO SV-SERIAL
               GO TO TCOUT.
       TC9C.
           MOVE 1 TO NDX1.
           MOVE C-TYPE TO DCODEN.
       TC10.
           IF  NDX1 IS GREATER THAN 3
               OR D2-WORK-CODE (NDX1) IS EQUAL TO SPACES
               GO TO TC9B.
           IF D2-LEAK-NUMB (NDX1) IS EQUAL TO "VOID   "
               ADD 1 TO NDX1
               GO TO TC10.
           PERFORM ADJUST-TIME THRU ADJTEX.
           PERFORM FIX-TRAVEL THRU FTEX.
           IF SV-DISTRICT EQUAL TO 5
               GO TO TC11.
           IF D2-LEAK-NUMB (NDX1) IS EQUAL TO SPACES
               AND SV-LNUM IS EQUAL TO ZEROS
               GO TO TC11. NOTE THAT IF THIS BRANCH IS TAKEN THERE IS
                           NO LEAK WORKING.
           IF D2-LEAK-NUMB(NDX1) IS NOT EQUAL TO SPACES
              AND SV-LNUM IS NOT EQUAL TO ZEROS,
               AND D2-LEAK-NUMB (NDX1) IS NOT EQUAL TO SV-LNUM,
               PERFORM WRITE-5-REC THRU W5EX,
               MOVE D2-LEAK-NUMB (NDX1) TO SV-LEAK-NUM,
               ADD 1 TO DEQP-LOCTNS.
                   NOTE THAT THE SITUATION ABOVE IS A CHANGE IN LEAK NUMBER.
           IF D2-LEAK-NUMB (NDX1) IS NOT EQUAL TO SPACES
               AND SV-LNUM IS EQUAL TO ZEROS,
               ADD 1 TO DEQP-LOCTNS,
               MOVE D2-LEAK-NUMB (NDX1) TO SV-LEAK-NUMB.
       TC10A.
           MOVE 1 TO NDX2, NDX3, NDX4.
       TC10B.
           IF LK-CDE (NDX2) EQUAL TO "ZZ"
               PERFORM WRITE-5-REC THRU W5EX
               MOVE  ZEROS TO SV-LEAK-NUM,
                   NOTE THAT NO VALID CODE FOR A STREET DEPARTMENT
                        LEAK WAS FOUND.
           IF LK-CDE (NDX2) EQUAL TO D2-WORK-CODE (NDX1)
               MOVE LKNDX (NDX2) TO NDX3.
           MOVE 1 TO NDX5, NDX4.
              NOTE THAT THIS ROUTINE ADD STREET DEPARTMENT LEAK TIME
                   BY EACH MAN ON THE JOB.
       TC10C.
           IF NDX4 GREATER THAN 8
               OR MANNO (NDX4) EQUAL TO ZEROS
               GO TO TC11.
           PERFORM CHECK-TIMES THRU CKTMEX.
           MOVE LKHRS (NDX4,NDX3)  TO START-HOURS.
           MOVE  LKMIN (NDX4,NDX3)  TO START-MINUTES.
           PERFORM ADD-RTN.
           MOVE NET-HOURS TO LKHRS (NDX4,NDX3).
           MOVE NET-MINUTES TO LKMIN (NDX4,NDX3).
           ADD 1 TO NDX4
           GO TO TC10.
             NOTE THAT THIS ROUTINE ACCUMULATES TOTAL MAN HOURS
                  AND DAILY TRUCK HOURS FOR EACH CATAGORY REPORTED
                  ON THE VEHICLE REPORT.
       TC11.
           MOVE 1 TO NDX2, NDX3, NDX4, NDX5.
       TC11A.
           IF EQ-CDE (NDX2) EQUAL TO "ZZ" GO TO TC12.
           IF EQ-CDE (NDX2) EQUAL TO "**" MOVE EQNDX (NDX2) TO NDX3,
                 MOVE MHNDX (NDX2) TO NDX5
                     PERFORM DLY-EQUIPMENT THRU DEQEX,  GO TO TC11B.
                    NOTE THAT NDX3 WILL BE USED TO ACCUMULATE
                       EQUIPMENT TIME  AND  NDX5 TO ACCUMULATE
                       MAN HOURS.
           IF EQ-CDE (NDX2) EQUAL TO D2-WORK-CODE (NDX1)
               PERFORM DLY-EQUIPMENT  THRU DEQEX, GO TO TC11B.
           ADD 1 TO NDX2.
           GO TO TC11A.
       TC11B.
           IF MANNO (NDX4) EQUAL TO ZEROS OR
                  NDX4 GREATER THAN 8
               GO TO TC12.
           PERFORM CHECK-TIMES THRU CKTMEX.   NOTE THAT FINISH-TIMES ARE
                                                SET UP IN THIS ROUTINE.
           MOVE QHRS    (NDX5) TO START-HOURS.
           MOVE QMIN    (NDX5) TO START-MINUTES.
           PERFORM ADD-RTN.
           MOVE NET-HOURS TO QHRS    (NDX5).
           MOVE NET-MINUTES TO QMIN    (NDX5).
           ADD 1 TO NDX4.
           GO TO TC11B.
       TC12.
           MOVE D2-FINISH-TIME (NDX1) TO SV-FINISH.
       TC13.
           ADD 1 TO NDX1.
           GO TO TC10.
       TCOUT.
           IF DMAN-NO (1) IS NOT EQUAL ZEROS
               WRITE LK-DETL
               MOVE ZEROS TO LK-DETL.
           PERFORM TC7B.
           MOVE  ZEROS TO SVMNDXX
               ADDERS, SV-LEAK-NUM, SV-DATE,
               EQUP-DAILY, OT-SW.
           GO TO STRT2.
       FIX-SHIFTS.
           MOVE 1 TO NDX1.   NOTE THAT WE GOT HERE FROM TC6.
                  NOTE
           CON ED NUMBERS SHIFTS AS FOLLOWS
               SHIFT 1 IS FROM  12 MIDNIGHT TO 8 AM,
                     2 IS FROM  8 AM TO 4 PM,
                     3 IS FROM  4 PM TO 12 MIDNIGHT
           IF A MAN OR TRUCK IS MOVED ONTO OR OFF A CREW AFTER
               THE NORMAL SHIFT END THE SPACE ON THE DAILY ROUTE
               SHEET FOLLOWING TIME IN OR TIME OUT WILL CONTAIN
               A NON SPACES
           THE FOLLOWING ROUTINE IS AN ATTEMPT TO SET UP THE
               CORRECT TIME IN OR OUT ON A 24 HOUR BASIS
               SO THAT WHEN THE CHECK-TIMES SUBROUTINE IS
               EXECUTED A PROPER TIME COMPARISON MAY BE MADE.
       FXSF1.
           IF NDX1 IS GREATER THAN 8
               OR MANNO (NDX1) IS EQUAL TO ZEROS
               GO TO FXEQPS.
           IF SV-SHIFT IS NOT EQUAL TO 1
               GO TO FIXSF2.
           IF HRIN (NDX1) IS LESS THAN 12
               AND IOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO HRIN (NDX1)
           IF HROUT (NDX1) IS LESS  THAN 12
               AND OOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO HROUT (NDX1).
           GO TO FIXNEX.
       FIXSF2.
               IF SV-SHIFT IS NOT EQUAL TO 2
                   GO TO FXSF3.
               IF  HRIN (NDX1) IS EQUAL TO ZEROS
                   GO TO FIXSF2A.
               IF HRIN (NDX1) IS LESS THAN 8
                   AND IOTF (NDX1) IS EQUAL TO
                   SPACES ADD 12 TO HRIN (NDX1).
           IF IOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO HRIN (NDX1).
       FIXSF2A.
           IF HROUT (NDX1) IS EQUAL TO ZEROS
               GO TO FIXNEX.
           IF HROUT (NDX1) IS LESS THAN 8
               AND OOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO HROUT (NDX1).
           IF OOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO HROUT (NDX1).
       FXSF3.
           IF HRIN (NDX1) IS EQUAL TO ZERO
               GO TO FIXSF3A.
           IF HRIN (NDX1) IS LESS THAN 4
               AND IOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO HRIN (NDX1).
           IF IOTF (NDX1) IS NOT EQUAL TO SPACES
               AND HRIN (NDX1) IS NOT EQUAL TO 12
               ADD 12 TO HRIN (NDX1).
       FIXSF3A.
           IF HROUT (NDX1) IS EQUAL TO ZEROS
               GO TO FIXNEX.
           IF HROUT (NDX1) IS LESS THAN 4
               AND OOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO HROUT (NDX1).
           IF OOTF (NDX1) IS NOT EQUAL TO SPACES
               AND HROUT (NDX1) IS NOT EQUAL TO 12
               ADD 12 TO HROUT (NDX1).
       FIXNEX.
           ADD 1 TO NDX1
           GO TO FXSF1.
       FXEQPS.
           MOVE 1 TO NDX1.
       FXEQPS1.
           IF NDX1 IS GREATER THAN 3
               OR TRUCKNUMB (NDX1) IS EQUAL TO ZEROS
               GO TO FIXSEX.
           IF SV-SHIFT IS NOT EQUAL TO 1
               GO TO FXTX2.
           IF TRK-HRSIN (NDX1) IS LESS THAN 12
               AND TRK-IOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO TRK-HRSIN (NDX1).
           IF TRK-HRSOUT (NDX1) IS LESS THAN 12
               AND TRK-IOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO TRK-HRSOUT (NDX1).
           GO TO FXTXBMP.
       FXTX2.
           IF SV-SHIFT IS NOT EQUAL TO 2
               GO TO FXTX3.
           IF TRK-HRSIN (NDX1) IS EQUAL TO ZEROS
               GO TO FXTX2A.
           IF TRK-HRSIN (NDX1) IS LESS THAN 8
               AND TRK-IOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO TRK-HRSIN (NDX1).
           IF TRK-IOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO TRK-HRSIN (NDX1).
       FXTX2A.
           IF TRK-HRSOUT (NDX1) IS EQUAL TO ZEROS
               GO TO FXTXBMP.
           IF TRK-HRSOUT (NDX1) IS LESS THAN 8
               AND TRK-OOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO TRK-HRSOUT (NDX1).
           IF  TRK-OOTF (NDX1) IS NOT EQUAL TO SPACES
               ADD 12 TO TRK-HRSOUT (NDX1).
           GO TO FXTXBMP.
       FXTX3.
           IF TRK-HRSIN (NDX1) IS EQUAL TO ZEROS
               GO TO FXTX3A.
           IF TRK-HRSIN (NDX1) IS LESS THAN 4
               AND TRK-IOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO TRK-HRSIN (NDX1).
           IF TRK-IOTF (NDX1) IS NOT EQUAL TO SPACES
               AND TRK-HRSIN (NDX1) IS NOT EQUAL TO 12
               ADD 12 TO TRK-HRSIN (NDX1).
       FXTX3A.
           IF TRK-HRSOUT (NDX1) IS EQUAL TO ZEROS
               GO TO FXTXBMP.
           IF TRK-HRSOUT (NDX1) IS LESS THAN 4
               AND TRK-OOTF (NDX1) IS EQUAL TO SPACES
               ADD 12 TO TRK-HRSOUT (NDX1).
           IF TRK-OOTF (NDX1) IS NOT EQUAL TO SPACES
               AND TRK-HRSOUT (NDX1) IS NOT EQUAL TO 12
               ADD 12 TO TRK-HRSOUT (NDX1).
       FXTXBMP.
           ADD 1 TO NDX1
           GO TO FXEQPS1.
       FIXSEX.
           EXIT.
       ADJUST-TIME.
           IF ADJT-SW IS EQUAL TO 1
               MOVE ZERO TO ADJT-SW
               GO TO ADJTEX.
           IF SV-SHIFT IS NOT EQUAL TO 1
               GO TO ADJT2.
           IF OT-SW IS NOT EQUAL TO 1
               GO TO ADJTEX.
           IF D2-HRS-START   (NDX1) IS LESS THAN 12
               ADD 12 TO D2-HRS-START (NDX1).
           IF D2-HRS-FINISH   (NDX1) IS LESS THAN 12
               ADD 12 TO D2-HRS-FINISH (NDX1).
           GO TO ADJTEX.
       ADJT2.
           IF SV-SHIFT IS NOT EQUAL TO 2
               GO TO ADJT3.
           IF OT-SW IS EQUAL TO 1
               ADD 12 TO D2-HRS-START (NDX1)
               ADD 12 TO D2-HRS-FINISH (NDX1)
               GO TO ADJTEX.
           IF OT-SW IS EQUAL TO ZERO
               AND D2-HRS-START (NDX1) IS LESS THAN 8
               ADD 12 TO D2-HRS-START (NDX1).
           IF OT-SW IS EQUAL TO ZERO
               AND D2-HRS-FINISH (NDX1) IS LESS THAN 8
               ADD 12 TO D2-HRS-FINISH (NDX1).
           GO TO ADJTEX.
       ADJT3.
           IF OT-SW IS EQUAL TO 1
               AND D2-HRS-START (NDX1) IS NOT EQUAL TO 12
               ADD 12 TO D2-HRS-START (NDX1).
           IF OT-SW IS EQUAL TO 1
               AND D2-HRS-FINISH (NDX1) IS NOT EQUAL TO 12
               ADD 12 TO D2-HRS-FINISH (NDX1).
           IF OT-SW IS NOT EQUAL TO ZERO
               GO TO ADJTEX.
           IF D2-HRS-START (NDX1) IS LESS THAN 4
               ADD 12 TO D2-HES-START (NDX1).
           IF D2-HRS-FINISH (NDX1) IS LESS THAN 4
               ADD 12 TO D2-HRS-FINISH (NDX1).
       ADJTEX.
           EXIT.
       CHK-EQPT.
           MOVE 1 TO NDX1.
       CHKTQ1.
           IF NDX3 IS GREATER THAN 3
               GO TO CHKTQX.
           IF TRK-TMIN (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO TRK-HRSIN (NDX1)
               MOVE ZEROS TO TRK-MININ (NDX1).
           IF TRK-TMOUT (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO TRK-HRSOUT (NDX1)
           MOVE ZEROS TO TRK-MINOUT (NDX1).
           IF TRUCKNUMB (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO TRUCKNUMB (NDX1)
           ADD 1 TO NDX1.
           GO TO CHKTQ1.
       CHKTQX.
           EXIT.
       FIX-TRAVEL.
           IF SV-FINISH IS    EQUAL TO D2-HRS-START (NDX1)
               GO TO  FTEX.
           MOVE SV-FHRS TO START-HOURS.
           MOVE SV-FMIN TO START-MINUTES.
           MOVE D2-HRS-START (NDX1) TO FINISH-HOURS.
           MOVE D2-MIN-START (NDX1) TO FINISH-MINUTES.
           PERFORM SUBTRACT-RTNE.
           MOVE NET-HOURS TO TXHR.
           MOVE  NET-MINUTES TO TXMIN.
           IF DEQUIP-NO EQUAL TO ZEROS GO TO FT1.
           PERFORM ADD-RTNE-EX.
           MOVE DEQP-TRVL-HRS TO START-HOURS.
           MOVE DEQP-TRVL-MIN TO START-MINUTES.
           PERFORM ADD-RTN.
           MOVE NET-HOURS TO DEQP-TRVL-HRS.
           MOVE NET-MINUTES TO DEQP-TRVL-MIN.
       FT1.
           MOVE 1 TO NDX4, NDX2.
           MOVE ZERO TO OVER-24-ERR.
                   NOTE THAT THIS COUNTER IS IN THE SUBTRACT-RTNE
                       AND SERVES NO USEFUL PURPOSE IN THIS PROGRAM.
       FT2.
           IF NDX4 IS GREATER THAN 8
               OR MANNO (NDX4) IS EQUAL TO ZEROS
               GO TO FTEX.
           IF  TME-IN (NDX4) IS EQUAL TO ZEROS
               AND TME-OUT (NDX4) IS EQUAL TO ZEROS
               PERFORM FTADD.
           IF  TME-IN (NDX4) IS NOT EQUAL TO ZEROS
               GO TO FT3.
           IF  TME-OUT (NDX4) IS GREATER THAN SV-FINISH
               PERFORM FTADD.
           GO TO BUMP.
       FT3.
           IF TME-IN (NDX4) IS LESS THAN SV-FINISH
               PERFORM FTADD.
               GO TO BUMP.
       FTADD.
           MOVE TXHR TO START-HOURS.
           MOVE TXMIN TO START-MINUTES.
           MOVE DMHRS-TRVL-HRS TO FINISH-HOURS.
           MOVE DMHRS-TRVL-MIN TO FINISH-MINUTES.
           PERFORM ADD-RTN.
           MOVE NET-HOURS TO DMHRS-TRVL-HRS.
           MOVE NET-MINUTES TO DMHRS-TRVL-MIN.
       BUMP.
           ADD 1 TO NDX4.
           GO TO FT2.
       FTEX.
           EXIT.
       WRITE-5-REC.
           MOVE 1 TO NDX5.
           MOVE 5 TO DREC-TYPE.
           MOVE SV-LEAK-NUM TO DLEAK-NO.
           MOVE SV-MO TO DMREC-MO.
           MOVE SV-DAY TO DMREC-DA.
           MOVE SV-YR TO DMREC-YR.
       W5R1.
           IF NDX5 IS GREATER THAN 8
               OR MANNO (NDX5) IS EQUAL TO ZEROS
               GO TO W5R2.
           IF NDX6 IS GREATER THAN 3
               WRITE LK-DETL
               MOVE ZEROS TO LK-DETL
               MOVE 1 TO NDX6.
           MOVE MANNO (NDX5) TO DMAN-NO (NDX6).
           MOVE LEAK-TIMES (NDX5) TO DMTM (NDX6).
           ADD 1 TO NDX5.
           ADD 1 TO NDX6.
           GO TO W5R1.
       W5R2.
           MOVE  ZEROS TO NDX5.
       W5R3.
           ADD 1 TO NDX5.
           IF NDX5 IS GREATER THAN 8
               GO TO W5EX.
           MOVE ZEROS TO NDX4.
       W5R4.
           ADD 1 TO NDX4.
           IF NDX4 IS GREATER THAN 4 
               GO TO W5R3.
           MOVE ZEROS TO LEAK-TIMES (NDX4,NDX5).
           GO TO W5R4.
       W5EX.
           EXIT.
                       NOTE  TCNDX SET UP BY TC7A CONTROLS TRUCK DATA.
       DLY-EQUIPMENT.
           IF DEQUIP-NO IS EQUAL TO ZEROS
               OR NDX3 IS EQUAL TO ZEROS
               GO TO DEQEX.  NOTE NO TRUCK FOR THIS CREW.
           IF TRK-TMIN (TCNDX) IS EQUAL TO ZEROS
               GO TO DEQEXP.  NOTE NO EQUIPMENT CHANGE FOR THIS CREW.
       DEQ1.
           PERFORM CHECK-TIMES.  NOTE THIS CLEARS THE ADDERS.
           IF TRK-TMIN (TCNDX) IS EQUAL TO ZEROS
               OR TRK-TMIN (TCNDX) IS NOT GREATER THAN
                           D2-START-TIME (NDX1)
               GO TO DEQ2. NOTE TRUCK IN TIME IS BEFORE START TIME.
           IF TRK-TMOUT (TCNDX) IS EQUAL TO ZEROS
               OR TRK-TMOUT (TCNDX) IS NOT LESS THAN
                           D2-FINISH-TIME (NDX1)
               GO TO DEQ3. NOTE TRUCK OUT TIME IS AFTER FINISH TIME.
           IF TRK-TMOUT (TCNDX) IS NOT GREATER THAN D2-START-TIME (NDX1)
               OR TRK-TMIN (TCNDX) IS NOT LESS THAN 
                   D2-FINISH-TIME (NDX1)
               PERFORM TC7B
               GO TO DEQEX.
           NOTE  AT THIS POINT TRUCK OUT IS BEFORE THE FINISH BUT AFTER
                 THE START AND  TRUCK IN IS AFTER THE START BUT BEFORE
                 THE FINISH.
       DEQC.
           MOVE  TRK-HRSIN  (TCNDX) TO START-HOURS.
           MOVE  TRK-MININ  (TCNDX) TO START-MINUTES.
       DEQD.
           MOVE  TRK-HRSOUT  (TCNDX) TO FINISH-HOURS.
           MOVE  TRK-MINOUT  (TCNDX) TO FINISH-MINUTES.
       DEQE.
           PERFORM SUBTRACT-RTNE.
           PERFORM ADD-RTNE-EX.
       DEQG.
           GO TO DEQF.
       DEQ2.
           IF TRK-TMOUT (TCNDX)     IS NOT LESS THAN
                               D2-FINISH-TIME (NDX1)
               GO TO DEQEXP.
               MOVE D2-HRS-START (NDX1) TO START-HOURS.
               MOVE D2-MIN-START (NDX1) TO START-MINUTES.
               PERFORM DEQD THRU DEQE.
       DEQF.
           PERFORM DEQADD.
       DEQPUT.
           PERFORM TC7B.
           ADD 1 TO TCNDX
           IF TCNDX IS GREATER THAN 3
               OR TRUCKNUMB (TCNDX) IS EQUAL TO ZEROS
               OR TRUCKNUMB (TCNDX) IS EQUAL TO SPACES
               GO TO DEQEX.
           PERFORM TC7BX THRU TC8B.
           GO TO DLY-EQUIPMENT.
       DEQ3.
           IF TRK-TMIN (TCNDX) IS NOT GREATER
                               D2-START-TIME (NDX1)
               GO TO DEQEXP.
           PERFORM TC7B.
           PERFORM DEQC.
           MOVE D2-HRS-FINISH (NDX1) TO FINISH-HOURS
           MOVE D2-MIN-FINISH (NDX1) TO FINISH-MINUTES.
           PERFORM DEQE.
           PERFORM DEQADD.
       DEQEXP.
           MOVE D2-EXT-HRS (NDX1) TO FINISH-HOURS.
           MOVE D2-EXT-MIN (NDX1) TO FINISH-MINUTES.
       DEQADD.
           MOVE DLYHRS  (NDX3) TO START-HOURS.
           MOVE DLYMIN  (NDX3) TO FINISH-MINUTES
           PERFORM ADD-RTN.
           MOVE NET-HOURS TO DLYHRS (NDX3)
           MOVE NET-MINUTES TO DLYMIN (NDX3)
       DEQEX.
           EXIT.
       CHECK-TIMES.
           MOVE ZEROS TO START-HOURS, FINISH-HOURS,
               START-MINUTES, FINISH-MINUTES.
       CKTMAC.
           IF TME-IN (NDX4) EQUAL TO ZEROS
               AND TME-OUT (NDX4) EQUAL TO ZEROS
               GO TO CKTMEXP.
           IF TME-IN (NDX4) NOT EQUAL TO ZEROS, GO TO CKT1.
           IF TME-OUT (NDX4) GREATER THAN D2-FINISH-TIME (NDX1)
               OR TME-OUT (NDX4) EQUAL TO D2-FINISH-TIME (NDX1)
               GO TO CKTMEXP.
           IF TME-OUT (NDX4) LESS THAN D2-START-TIME (NDX1)
               OR TME-OUT (NDX4) EQUAL TO D2-START-TIME (NDX1)
               GO TO CKTMEX.
           GO TO CTSUB2.
       CKT1.
           IF TME-OUT (NDX4) NOT EQUAL TO ZEROS GO TO CKT2.
           IF TME-IN (NDX4) IS LESS THAN D2-START-TIME (NDX1)
               OR TME-IN (NDX4) EQUAL TO D2-START-TIME (NDX1)
               GO TO CKTMEXP.
           IF TME-IN (NDX4) IS GREATER THAN D2-FINISH-TIME (NDX1)
               OR TME-IN (NDX4) IS EQUAL TO D2-FINISH-TIME(NDX1)
               GO TO CKTMEX.
               GO TO  CTSUB1.
                NOTE THAT BOTH TIME IN AND TIME OUT ARE NOT ZERO.
       CKT2.
           IF TME-IN (NDX4) IS EQUAL TO D2-START-TIME (NDX1)
               OR TME-IN (NDX4) IS LESS THAN D2-START-TIME (NDX1)
               GO TO CKT3.  NOTE GO CHECK TME-OUT.
           IF TME-OUT (NDX4) IS EQUAL TO D2-FINISH-TIME (NDX1)
            OR TME-OUT (NDX4) IS GREATER THAN D2-FINISH-TIME (NDX1)
               GO TO CKT4.  NOTE GO CHECK TME-IN.
           IF TME-OUT (NDX4) IS LESS THAN       D2-FINISH-TIME (NDX1)
               AND TME-OUT (NDX4) IS GREATER  THAN
                   D2-START-TIME (NDX1)  GO TO CKT5.
           IF TME-IN (NDX4) IS LESS  THAN D2-FINISH-TIME (NDX1)
               AND  TME-IN (NDX4) IS GREATER THAN
                   D2-START-TIME (NDX1) GO TO CKT5.
       CKT3.
           IF TME-OUT (NDX4) IS NOT LESS THAN      D2-FINISH-TIME (NDX1)
               GO TO CKTMEXP
               ELSE GO TO CTSUB2.
       CKT4.
           IF TME-IN (NDX4) IS NOT GREATER THAN D2-START-TIME (NDX1)
               GO TO CKTMEXP
               ELSE PERFORM CT3B THRU CT3E.
       CKT5.
           IF TME-IN (NDX4) IS GREATER THAN TME-OUT (NDX4)
               GO TO CTSUB3
               ELSE GO TO CTSUB4.
       CTSUB1.
           PERFORM CT3B.
           PERFORM CT3C.
           PERFORM SUBTRACT-RTNE.
           GO TO CKTMEX.
       CTSUB2.
           PERFORM CTSUB3 THRU CT3D.
           GO TO CKTMEX.
       CTSUB3.
           MOVE D2-HRS-START (NDX1) TO START-HOURS.
           MOVE D2-MIN-START (NDX1) TO START-MINUTES.
       CT3A.
           MOVE  HROUT (NDX4) TO FINISH-HOURS.
           MOVE  MINOUT (NDX4) TO FINISH-MINUTES.
           PERFORM SUBTRACT-RTNE.
       CT3D.
           MOVE NET-HOURS TO SVENET-HRS.
           MOVE NET-MINUTES TO SVENET-MIN.
           PERFORM CHECK-TIMES.
       CT3B.
           MOVE HRIN (NDX4) TO START-HOURS.
           MOVE MININ (NDX4) TO START-MINUTES.
       CT3C.
           MOVE D2-HRS-FINISH (NDX1) TO FINISH-HOURS.
           MOVE D2-MIN-FINISH (NDX1) TO FINISH-MINUTES.
           PERFORM SUBTRACT-RTNE.
       CT3E.
           PERFORM ADD-RTNE-EX.
           MOVE SVENET-HRS  TO START-HOURS.
           MOVE SVENET-MIN  TO START-MINUTES.
           PERFORM ADD-RTN.
       CTXXX.
           GO TO CKTMEX.
       CTSUB4.
           PERFORM CT3A.
           PERFORM CT3B.
           PERFORM SUBTRACT-RTNE.
           GO TO CKTMEX.
       CKTMEXP.
           MOVE  D2-EXT-HRS (NDX1) TO FINISH-HOURS.
           MOVE  D2-EXT-MIN (NDX1) TO FINISH-MINUTES.
       CKTMEX.
           EXIT.
       EQUIP-MSTR SECTION.
       EQMST1.
           IF EQKEY IS NOT EQUAL TO C-KEY-TYPE-DATE
               GO TO EQMST2.
           IF HL-CTYPE IS NOT EQUAL TO 3
               MOVE 3 TO HL-CTYPE
               MOVE ER-K TO PCRD-ID
               MOVE 2 TO LN-ADV
               PERFORM WRITE-PRINT.
           MOVE LK-REC TO PCRD.
           PERFORM ERR-PRINT  THRU  ERPEX.
           GO TO START.
       EQMST2.
           IF DEQP-DIV IS NOT EQUAL TO ZERO
               WRITE EQUP-DAILY.
           MOVE ZEROS TO EQUP-DAILY.
           MOVE C-TYPE TO DEQUIP-EDIT-ERROR.
           MOVE EM-DIST TO  DEQP-DIST.
           MOVE ZERO TO DEQP-REC-TYPE.
           MOVE E-SHIFT TO DEQP-SHIFTY.
           MOVE EDATE TO DEQUIP-DATE.
           MOVE "M" TO DEQP-DIV.
           MOVE 1 TO NDX1.
       EQMST3.
           IF NDX1 IS GREATER THAN 5
               OR EQUIP-OUT (NDX1) IS EQUAL TO SPACES
               GO TO EQMST4.
           MOVE EQUIP-NUMB (NDX1) TO DEQUIP-NO.
           IF EQUIP-OUT-REASON (NDX1) IS EQUAL TO "BK"
               MOVE EQUIP-OUT-HRS (NDX1) TO DEQP-OUT-BRK-HRS
             ELSE MOVE EQUIP-OUT-HRS (NDX1) TO DEQP-NO-MEN-HRS.
           WRITE EQUP-DAILY.
           MOVE ZEROS TO DEQPTIMES.
           ADD 1 TO NDX1.
           GO TO EQMST3.
       EQMST4.
           IF DEQUIP-OUT-BROKE IS NOT EQUAL TO ZEROS
               OR DEQUIP-NO-MEN IS NOT EQUAL TO ZEROS
               WRITE EQUP-DAILY.
           MOVE ZEROS TO EQUP-DAILY.
           GO TO STRT2.
       LK-JE SECTION.
       LKJE.
           IF EQKEY IS NOT EQUAL TO C-KEY-TYPE-DATE
               GO TO LKJE1.
           IF HL-CTYPE IS EQUAL TO 4
               GO TO LKJE4.
           MOVE 4 TO HL-CTYPE.
           MOVE ER-P TO PCRD-ID.
           MOVE 2 TO LN-ADV.
           PERFORM WRITE-PRINT.
       LKJE4.
           MOVE LK-REC TO PCRD.
           PERFORM ERR-PRINT THRU ERPEX.
           GO TO START.
       LKJE1.
           MOVE 1 TO NDX2.
       LKJE2.
           IF JEPTM (NDX2) IS EQUAL TO SPACES
               MOVE ALL ZEROS TO LK-JECOR (NDX2)
               GO TO LKJE3.
           IF JESIGN IS EQUAL TO "S"
               SUBTRACT JEPHRS (NDX2) FROM LK-JE-HRS (NDX2)
               SUBTRACT JEPMIN (NDX2) FROM LK-JE-MIN (NDX2)
                   ELSE ADD JEPHRS (NDX2) TO LK-JE-HRS (NDX2)
                   ADD JEPMIN (NDX2) TO LK-JE-MIN (NDX2).
       LKJE3.
           ADD 1 TO NDX2.
           IF NDX2 IS LESS THAN 5
               GO TO LKJE2.
           MOVE JELEAK-NO TO XLEAK-NO.
           MOVE 6 TO XREC-TYPE.
           MOVE JEPMO TO DMREC-MO.
           MOVE JEPDAY TO DMREC-DA.
           MOVE JEPYR TO DMREC-YR.
           MOVE C-TYPE TO DCODEN.
           WRITE LK-DETL.
           MOVE 95 TO DCARD-NO.
           MOVE 9 TO XREC-TYPE.
           MOVE JEDESCRP TO DNARRATIVE-UF.
           WRITE LK-DETL.
           MOVE ZEROS TO LK-DETL.
           GO TO STRT1.
       EQUIP-JE SECTION.
       QJE.
           PERFORM RD1.  NOTE THAT THIS WRITES ANY RECORD IN THE HOLE.
           IF EQKEY IS NOT EQUAL TO C-KEY-TYPE-DATE
               GO TO QJE1.
           IF HL-CTYPE IS NOT EQUAL TO 5
               MOVE 5 TO HL-CTYPE
               MOVE ER-ST TO PCRD-ID
               MOVE 2 TO LN-ADV
               PERFORM WRITE-PRINT
               MOVE LK-REC TO PCRD
               PERFORM ERR-PRINT THRU ERPEX.
           GO TO START.
       QJE1.
           MOVE 1 TO NDX1.
           MOVE EQUIP-DIV TO DEQP-DIV.
           MOVE EQUIP-SHIFT TO DEQP-SHIFTY.
           MOVE EQUIP-DIST TO DEQP-DIST.
           MOVE 2 TO DEQP-REC-TYPE
       QJE2.
           IF NDX1 IS GREATER THAN 8
               GO TO QJE3.
           IF  HRS-EQUIP-JE (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO HRS-EQUIP-JE (NDX1).
           IF  MIN-EQUIP-JE (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO MIN-EQUIP-JE (NDX1)
           IF C-TYPE EQUAL TO "T"
               SUBTRACT HRS-EQUIP-JE (NDX1) FROM QYTDHRS (NDX1)
               SUBTRACT MIN-EQUIP-JE (NDX1) FROM QYTDMIN (NDX1)
               SUBTRACT TRUCK-NUMB-LOC-CORR FROM DEQP-YLOCTNS,
            ELSE
               ADD HRS-EQUIP-JE (NDX1)  TO  QYTDHRS (NDX1)
               ADD MIN-EQUIP-JE (NDX1)  TO  QYTDMIN (NDX1)
               ADD TRUCK-NUMB-LOC-CORR TO  DEQP-YLOCTNS.
           ADD 1 TO NDX1.
           GO TO QJE2.
       QJE3.
           PERFORM RD1.
           PERFORM READ-DETAIL
           GO TO STRT2.
       MAN-HRS-JE SECTION.
       MHJE.
           PERFORM RD1.   NOTE THIS WRITES ANY RECORDS IN THE HOLE.
           IF EQKEY IS NOT EQUAL TO C-KEY-TYPE-DATE
               GO TO MHJE1.
           IF HL-CTYPE IS NOT EQUAL TO 6
               MOVE 6 TO HL-CTYPE
               MOVE ER-WX TO PCRD-ID
               MOVE 2 TO LN-ADV
               PERFORM WRITE-PRINT.
               MOVE LK-REC TO PCRD
               PERFORM ERR-PRINT THRU ERPEX.
           GO TO START.
       MHJE1.
           MOVE 1 TO NDX1.
           MOVE 8 TO  QREC-TYPE.
           MOVE EQUIP-DIV TO QDIV.
           MOVE EQUIP-DIST TO QDIST.
           MOVE EQUIP-SHIFT TO QSHIFT.
       MHJE2.
           IF NDX1 IS GREATER THAN 8
               GO TO MHJE3.
           IF HRS-EQUIP-JE-02 (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO HRS-EQUIP-JE-02 (NDX1).
           IF MIN-EQUIP-JE-02 (NDX1) IS EQUAL TO SPACES
               MOVE ZEROS TO  MIN-EQUIP-JE-02 (NDX1).
           IF C-TYPE IS EQUAL TO "X"
               SUBTRACT HRS-EQUIP-JE-02 (NDX1) FROM QHRS (NDX1)
               SUBTRACT MIN-EQUIP-JE-02 (NDX1) FROM QMIN (NDX1)
           ELSE
               ADD HRS-EQUIP-JE-02 (NDX1) TO QHRS (NDX1)
               ADD MIN-EQUIP-JE-02 (NDX1) TO QMIN (NDX1).
           ADD 1 TO NDX1.
           GO TO MHJE2.
       MHJE3.
           PERFORM RD1.
           PERFORM READ-DETAIL.
           GO TO STRT2.
3Ik%Œ