IDENTIFICATION  DIVISION.
PROGRAM-ID.  'T3090'.
AUTHOR.  CAGNEY FRANCE, STANFORD OPTNER AND ASSOCIATES INC.
INSTALLATION.  LOS ANGELES TRAFFIC DEPARTMENT, DATA SERVICE BUREAU
     CITY OF LOS ANGELES, JUNE 1969.
REMARKS. THIS PROGRAM PRINTS ALL TRAFFIC II WEEKLY
     REPORTS FOR WHICH REQUESTS HAVE BEEN SUBMITTED.
ENVIRONMENT  DIVISION.
CONFIGURATION  SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
SPECIAL-NAMES.
        CHANNEL (1) IS TOP-OF-PAGE.
INPUT-OUTPUT  SECTION.
FILE-CONTROL.
     SELECT REPORT-FILE ASSIGN DSK,
      RECORDING MODE IS ASCII.

     SELECT OPTIONAL  VOLFILE ASSIGN DSK,
      RECORDING MODE IS ASCII
             RESERVE NO ALTERNATE AREA
             ACCESS MODE IS INDEXED
             RECORD KEY IS VOL-REC-KEY
             SYMBOLIC KEY IS VOL-SYM-KEY.
     SELECT PRINT-FILE ASSIGN DSK,
      RECORDING MODE IS ASCII.
     SELECT ISFILE ASSIGN DSK,
      RECORDING MODE IS ASCII
             RESERVE NO ALTERNATE AREA
             ACCESS MODE IS INDEXED
             RECORD KEY IS IS-REC-KEY
             SYMBOLIC KEY IS SYM-KEY.
     SELECT OPTIONAL  STRAT-FILE ASSIGN DSK,
      RECORDING MODE IS ASCII
         RESERVE NO ALTERNATE AREA
         ACCESS MODE IS INDEXED
         RECORD KEY IS ST-REC-KEY
         SYMBOLIC KEY IS ST-SYM-KEY.
     SELECT OPTIONAL  JURIS-FILE ASSIGN DSK,
      RECORDING MODE IS ASCII.
DATA  DIVISION.
FILE  SECTION.
FD   PRINT-FILE
         VALUE OF IDENTIFICATION IS "PRINT2DAT"
     LABEL RECORDS ARE STANDARD
     RECORD CONTAINS 133 CHARACTERS

     DATA RECORD IS PRINT-REC.
01   PRINT-REC.
     02 FILLER       PICTURE X.
     02 PRINT-LINE           PICTURE  X(132).
     02 PRINTX  REDEFINES PRINT-LINE.
        03 FILLER            PICTURE XX.
        03 PRINT-LINE-SUB OCCURS 3 TIMES.
          05 PRINT-LINE-SUB0.
           07 PRINT-LINE-SUB1 PICTURE X(27).
           07 PRINT-LINE-SUB2 PICTURE XXXX.
        03 PRINT-LINE-SUB-L   PICTURE X(37).
     02  T23-PRINT REDEFINES PRINTX.
         05 T23-P-C          PICTURE X(7).
         05 T23-P-C3 OCCURS 125 TIMES.
            07 T23-P-INC     PICTURE X.
     02  T23-PRINT-1 REDEFINES T23-PRINT.
         05 T23-P-NAM OCCURS 3 TIMES.
            07 T23-P-NAME1   PICTURE X(40).
            07 FILLER        PICTURE X(4).
     02  T23-P-SUMMARY REDEFINES T23-PRINT-1.
         05 FILLER           PICTURE X(18).
         05 T23-SUM-SEC OCCURS 2 TIMES.
           06 T23-S-SEC.
            07 T23-S-CONST   PICTURE X(12).
            07 T23-S1        PICTURE ZZZ.
            07 FILLER        PICTURE X.
            07 T23-S2        PICTURE ZZZ.
            07 FILLER        PICTURE X.
            07 T23-S3        PICTURE ZZZ.
            07 FILLER        PICTURE XXX.
            07 T23-S4        PICTURE Z,ZZZ.
            07 FILLER        PICTURE XX.
            07 T23-S5        PICTURE ZZZ.
            07 FILLER        PICTURE X.
            07 T23-S6        PICTURE ZZZ.
            07 FILLER        PICTURE X(17).
     02  T23-FINAL-S REDEFINES T23-P-SUMMARY.
         05 FILLER           PICTURE X(9).
         05 T23-FINAL-1      PICTURE X(40).
         05 T23-FINAL-2      PICTURE ZZZ,ZZZ.
         05 FILLER REDEFINES T23-FINAL-2.
            07 T23-X1        PICTURE XX.
            07 T23-X2        PICTURE ZZ.ZZ.
         05 FILLER           PICTURE X(75).
     02  P-10-LINEA REDEFINES T23-FINAL-S.
        05 T10-ITEM-1-2-3.
           07 FILLER         PICTURE X(7).
           07 ITEM-1-2 OCCURS 2 TIMES.
              09 ITEM-1-CON  PICTURE X(12).
              09 FILLER      PICTURE XX.
              09 ITEM-1-TOT  PICTURE ZZZZ.
              09 FILLER      PICTURE X(17).
           07 ITEM-3.
              09 FILLER      PICTURE XX.
              09 ITEM-3-CON  PICTURE X(9).
              09 FILLER      PICTURE XXX.
              09 ITEM-3-A-1  PICTURE ZZZ.
              09 FILLER      PICTURE X.
              09 ITEM-3-A-2  PICTURE ZZZ.
              09 FILLER      PICTURE X.
              09 ITEM-3-A-O  PICTURE ZZZ.
              09 FILLER      PICTURE XX.
              09 ITEM-3-A-T  PICTURE ZZZZ.
              09 FILLER      PICTURE X(6).
              09 ITEM-3-D-1  PICTURE ZZZ.
              09 FILLER      PICTURE X.
              09 ITEM-3-D-2  PICTURE ZZZ.
              09 FILLER      PICTURE X.
              09 ITEM-3-D-O  PICTURE ZZZ.
              09 FILLER      PICTURE XX.
              09 ITEM-3-D-T  PICTURE ZZZZ.
              09 FILLER      PICTURE X.
           07 ITEM-3-C REDEFINES ITEM-3.
              09 FILLER      PICTURE X(14).
              09 ITEM-3-C1   PICTURE X(41).
     02 P-10-LINEB REDEFINES P-10-LINEA.
        05 T10-ITEM-4-5-6.
           07 FILLER         PICTURE X(7).
           07 ITEM-4-CON     PICTURE X(12).
           07 ITEM-4-ACC     PICTURE ZZZ.
           07 FILLER         PICTURE X(6).
           07 ITEM-4-PER     PICTURE ZZZ.
           07 FILLER         PICTURE X(10).
           07 ITEM-5-CON     PICTURE X(15).
           07 ITEM-5-ACC     PICTURE ZZZ.
           07 FILLER         PICTURE X(20).
           07 ITEM-6-CON     PICTURE X(6).
           07 ITEM-6-LOC     PICTURE XXX.
           07 FILLER         PICTURE XX.
           07 ITEM-6-DIR     PICTURE XX.
           07 FILLER         PICTURE XX.
           07 ITEM-6-VOL     PICTURE ZZ,ZZZ.
        07 ITEM-6-C          PICTURE XX.
        07 FILLER            PICTURE X.
           07 ITEM-6-MO      PICTURE XX.
           07 ITEM-6-C1      PICTURE X.
           07 ITEM-6-DA      PICTURE XX.
           07 ITEM-6-C2      PICTURE X.
           07 ITEM-6-YR      PICTURE XX.
           07 FILLER         PICTURE X(21).
     02 P-10-LINEC REDEFINES P-10-LINEB.
        05 T10-ITEM-7-8.
           07 ITEM-7-H       PICTURE X(80).
           07 ITEM-FIELDS REDEFINES ITEM-7-H.
              09 ITEM-7-CON  PICTURE X(26).
              09 ITEM-7-MEAN PICTURE ZZ.ZZZ.
              09 FILLER      PICTURE XX.
              09 ITEM-7-DEV  PICTURE ZZ.ZZZ.
              09 FILLER      PICTURE XXX.
              09 ITEM-7-C-VA PICTURE ZZ.ZZZ.
              09 FILLER      PICTURE XX.
         09 ITEM-7-HERE.
           11 ITEM-7-A-VA      PICTURE ZZZZ.ZZ.
           11 FILLER           PICTURE XXX.
         09 ITEM-7-HERE-A REDEFINES ITEM-7-HERE.
            11 ITEM-7-A-VAM  PICTURE ZZZ.ZZ.
            11 FILLER        PICTURE X(4).
              09 ITEM-7-DIFF PICTURE +ZZZ.ZZ.
              09 FILLER      PICTURE X(12).
           07 ITEM-8-CON     PICTURE X(29).
           07 ITEM-8-CON-1   PICTURE X(23).
           07 ITEM-8-FIELD REDEFINES ITEM-8-CON-1.
              09 ITEM-8-N    PICTURE X.
              09 FILLER      PICTURE XXXX.
              09 ITEM-8-S    PICTURE X.
              09 FILLER      PICTURE XXXX.
              09 ITEM-8-E    PICTURE X.
              09 FILLER      PICTURE XXXX.
              09 ITEM-8-W    PICTURE X.
              09 FILLER      PICTURE X(7).
     02 T10-PRI-DET REDEFINES P-10-LINEC.
        05 T10-DR-NO1        PICTURE X(6).
        05 FILLER            PICTURE XX.
        05 T10-MO1           PICTURE XX.
        05 P-T10-1           PICTURE X.
        05 T10-DA1           PICTURE XX.
        05 P-T10-2           PICTURE X.
        05 T10-YR1           PICTURE XX.
        05 FILLER            PICTURE X.
        05 P-T10-DAY         PICTURE XX.
        05 FILLER            PICTURE X.
        05 T10-HR1           PICTURE XX.
        05 P-T10-3           PICTURE X.
        05 T10-M1            PICTURE XX.
        05 FILLER            PICTURE X.
        05 T10-INJ-A1        PICTURE ZZ.
        05 FILLER            PICTURE X.
        05 T10-INJ-B1        PICTURE ZZ.
        05 FILLER            PICTURE X.
        05 T10-INJ-C1        PICTURE ZZ.
        05 FILLER            PICTURE X.
        05 T10-INJ-K1        PICTURE ZZ.
        05 FILLER            PICTURE XX.
        05 P-VEH-TYPE        PICTURE XXXX.
        05 FILLER            PICTURE X.
        05 T10-CLASS1        PICTURE Z.
        05 FILLER            PICTURE X.
        05 T10-COND1         PICTURE ZZ.
        05 FILLER            PICTURE X.
        05 P-ACC-TYPE        PICTURE X(7).
        05 FILLER            PICTURE X.
        05 P-10-ACTION       PICTURE X(8).
        05 FILLER            PICTURE X.
        05 P-10-IS           PICTURE XXX.
        05 FILLER            PICTURE X.
        05 P-PT-IMPACT       PICTURE X(8).
        05 OLD-PT-IMPACT REDEFINES P-PT-IMPACT.
           07 IMPACT-1       PICTURE X(4).
           07 IMPACT-2       PICTURE X(4) JUSTIFIED RIGHT.
        05 OLD-IMPACT-1 REDEFINES OLD-PT-IMPACT.
           07 INTER-PLUS-1   PICTURE ZZ99.
           07 NEW-IMP        PICTURE XXXX.
           07 NEW-IMP-D REDEFINES NEW-IMP.
              09 NEW-IMP-1   PICTURE X.
              09 NEW-IMP-2   PICTURE X.
              09 NEW-IMP-3   PICTURE X.
              09 NEW-IMP-4   PICTURE X.
        05 FILLER            PICTURE XX.
        05 P-10-DIR          PICTURE XXXX.
        05 FILLER   REDEFINES P-10-DIR.
           07 FILLER         PICTURE XX.
           07 P-10-SP        PICTURE X.
           07 P-10-NO            PICTURE 9.
        05 FILLER            PICTURE XX.
        05 DIR-ANL1          PICTURE 999.
        05 FILLER            PICTURE X.
        05 P-CONTRI-CIR      PICTURE X(10).
        05 FILLER            PICTURE XX.
        05 PRI-CODE1         PICTURE X(6).
        05 FILLER            PICTURE X.
        05 P-GP-CODE         PICTURE X(7).
        05 FILLER            PICTURE X.
        05 P-WEATHER         PICTURE X.
        05 FILLER            PICTURE X.
        05 P-LIGHT           PICTURE X(5).
        05 FILLER            PICTURE X.
        05 T10-AGE1          PICTURE ZZ.
        05 FILLER            PICTURE X.
        05 P-RD-CND          PICTURE X.
        05 FILLER            PICTURE X.
        05 SPEC-CIR1         PICTURE XX.
FD   REPORT-FILE
         VALUE OF IDENTIFICATION IS "REPORTDAT"
     LABEL RECORDS ARE STANDARD
     RECORD CONTAINS 290 CHARACTERS
     BLOCK CONTAINS 25 RECORDS
     DATA RECORD IS REPORT-REC.
01   REPORT-REC.
     02  T10-DETAIL.
         05 T10-REQUESTER    PICTURE X(15).
         05 T10-SEARCH-DATES PICTURE X(12).
         05 FILLER           PICTURE XX.
         05 T10-IS-CODE.
            07 T10-IS-1      PICTURE X(5).
            07 T10-IS-2      PICTURE X(5).
         05 DOW              PICTURE X.
         05 D-O-W REDEFINES DOW PICTURE 9.
         05 T10-DATE.
            07 T10-YR        PICTURE XX.
            07 T10-MO        PICTURE XX.
            07 T10-DA        PICTURE XX.
         05 T10-TIME.
            07 T10-HR        PICTURE XX.
            07 T10-M         PICTURE XX.
         05 T10-DR-NO        PICTURE X(6).
         05 PRI-CODE         PICTURE X(6).
         05 GP-CODE          PICTURE XX.
         05 GROUP-CODE REDEFINES  GP-CODE PICTURE 99.
         05 LOCAT-CODE       PICTURE 999.
         05 LOCATION-CODE REDEFINES LOCAT-CODE.
            07 INTERSECTION  PICTURE 9.
            07 DIRECTION-1   PICTURE 9.
            07 DIRECTION-2   PICTURE 9.
         05 LOC-A REDEFINES LOCATION-CODE.
            07 FILLER        PICTURE X.
            07 DISTA         PICTURE 99.
         05 PRI-CODE-1       PICTURE 9.
         05 PRI-CODE-2       PICTURE 9.
         05 SEC-FEET         PICTURE 9999.
         05 SEC-CODE-1       PICTURE 9.
         05 SEC-CODE-2       PICTURE 9.
         05 DIR-ANL          PICTURE 999.
         05 LIGHT            PICTURE X.
         05 RD-CONDITION     PICTURE X.
         05 SPEC-CIR         PICTURE XX.
         05 WEATHER          PICTURE 9.
         05 T10-AT-IS        PICTURE 9.
         05 T10-INJ-A        PICTURE S99.
         05 T10-INJ-B        PICTURE S99.
         05 T10-INJ-C        PICTURE S99.
         05 T10-INJ-K        PICTURE S99.
         05 T10-ACC-TYPE     PICTURE 99.
         05 T10-A-SEV        PICTURE 9.
         05 T10-REVERSE-ST-C PICTURE 9.
         05 FILLER           PICTURE X(4).
         05 T10-PARTY OCCURS 9 TIMES.
           06 T10-PARTY-ENTRY.
            07 PARTY-NO      PICTURE 9.
            07 VEH-ACTION    PICTURE 99.
            07 T10-DIR       PICTURE 9.
            07 VEH-TYPE      PICTURE 99.
            07 T10-CLASS     PICTURE 9.
            07 T10-COND      PICTURE 99.
            07 T10-AGE       PICTURE 99.
            07 PARTY-INJ     PICTURE 9.
            07 CONTRIB-CIR   PICTURE 99.
            07 FILLER        PICTURE XX.
            07 PED-ACTION    PICTURE 99.
         05 FILLER           PICTURE X(8).
     02 T12-19-DETAILS REDEFINES T10-DETAIL.
        03 T12-REQUESTER     PICTURE X(11).
        03 FILLER            PICTURE XXXX.
        03 T12-SEARCH-DATES.
     05 S-FR-MO              PICTURE XX.
     05 S-FR-DA              PICTURE XX.
     05 S-FR-YR              PICTURE XX.
     05 S-TO-MO              PICTURE XX.
     05 S-TO-DA              PICTURE XX.
     05 S-TO-YR              PICTURE XX.
        03 T12-SEARCH-TIMES.
     05 S-FR-HR              PICTURE 99.
     05 S-FR-M               PICTURE 99.
     05 S-TO-HR              PICTURE 99.
     05 S-TO-M               PICTURE 99.
        03 T12-CAT-LIMIT     PICTURE XX.
        03 T12-CAT-CODES OCCURS 14 TIMES.
           05 CAT            PICTURE 999.
        03 T12-AT-CODE       PICTURE X.
        03 T12-I-S           PICTURE S999.
        03 T12-NON-I-S       PICTURE S999.
        03 T12-FROM          PICTURE X(5).
        03 T12-TO            PICTURE X(5).
        03 T12-FROM-NAME     PICTURE X(30).
        03 T12-TO-NAME       PICTURE X(30).
        03 T19-FLD.
           05 T19-SEG-NO     PICTURE X(5).
           05 T19-SEG-CODE   PICTURE X(5).
           05 T19-SEG-NAME   PICTURE X(30).
        03 FILLER            PICTURE X(74).
     02 SORT-KEY.
        05 REPORT-CODE       PICTURE 99.
     05 REQ-NO.
        07 FILLER            PICTURE X(02).
        07 REQ-NO-3          PICTURE X(01).
        05 TOT-CAT-ACC       PICTURE S999.
        05 REC-CODE          PICTURE 9.
        05 DTANCE.
           07 DISTANCE       PICTURE 999.
           07 FILLER         PICTURE XX.
        05 T20-SEG-NO REDEFINES DTANCE   PICTURE X(5).
        05 FILLER            PICTURE X(6).
FD   ISFILE
         VALUE OF IDENTIFICATION IS "INTSRTIDX"
     LABEL RECORDS ARE STANDARD
     RECORD CONTAINS 300 CHARACTERS
     BLOCK CONTAINS 10 RECORDS
     DATA RECORD IS ISFILE-REC.
01   ISFILE-REC.
     03 IS-DELETE-CODE       PICTURE X.
     03 IS-REC-KEY.
        05 IS-KEY-ST-1       PICTURE X(5).
        05 IS-KEY-ST-2       PICTURE X(5).
     03 IS-NAME.
        05 IS-NAME-1         PICTURE X(30).
        05 IS-NAME-2         PICTURE X(30).
     03 IS-CLASSIFICATION.
        05 IS-CODES.
           07 IS-CLASS-CODE  PICTURE 9.
           07 IS-NO-LEGS     PICTURE 9.
           07 IS-CONTROL     PICTURE 9.
           07 IS-SIGNAL-CTRL PICTURE 9.
           07 IS-SIGNAL-CHAR PICTURE 99.
        05 IS-DATE.
           07 IS-MO          PICTURE XX.
           07 IS-DA          PICTURE XX.
           07 IS-YR          PICTURE XX.
     03 IS-APPROACH-CHARS.
        05 IS-LEG-CHAR OCCURS 4 TIMES.
           07 IS-NO-LANES    PICTURE X.
           07 IS-TURN        PICTURE X.
           07 IS-CTRL-SIGN   PICTURE X.
           07 IS-MAST-ARM    PICTURE X.
           07 IS-T-SIG-CTRL  PICTURE X.
           07 IS-LT-SIGN-REG PICTURE X.
           07 IS-LT-PED-REG  PICTURE X.
           07 IS-BUS-ZONE    PICTURE X.
           07 IS-SIG-APPR    PICTURE X.
           07 IS-MED-TYPE    PICTURE X.
           07 IS-MED-WDTH    PICTURE X.
           07 IS-PK-STP-RSTR PICTURE X.
           07 IS-DIR-FLOW    PICTURE X.
           07 IS-SEC-S-LOC   PICTURE X.
     03 IS-CORDINATES.
        05 IS-X-CORD         PICTURE X(6).
        05 IS-X-CORD-N REDEFINES IS-X-CORD PICTURE 9(6).
        05 IS-Y-CORD         PICTURE X(6).
        05 IS-Y-CORD-N REDEFINES IS-Y-CORD PICTURE 9(6).
     03 IS-SEGMENT-NO.
        05 IS-N-LEG          PICTURE X(5).
        05 IS-E-LEG          PICTURE X(5).
        05 IS-S-LEG          PICTURE X(5).
        05 IS-W-LEG          PICTURE X(5).
     03 IS-NEXT-IS.
        05 IS-NEXT-LEG OCCURS 4 TIMES.
           07 IS-NEXT-IS-CODE.
              09 IS-NEXT-ST-1    PICTURE X(5).
              09 IS-NEXT-ST-2    PICTURE X(5).
           07 IS-NEXT-IS-DIST    PICTURE 9999.
           07 IS-NEXT-IS-SLOPE   PICTURE X.
     03 IS-HAZARDOUS-ID.
        05 IS-YEAR OCCURS 4 TIMES.
           07 IS-YEAR-1      PICTURE 9.
           07 IS-YEAR-2      PICTURE 9.
           07 IS-YEAR-3      PICTURE 9.
           07 IS-YEAR-4      PICTURE 9.
     03 IS-DEAD-END          PICTURE X.
     03 IS-VACATED           PICTURE X.
     03 IS-OTHER-IS.
        05 IS-JOG            PICTURE X.
        05 IS-OTHER-ST.
           07 IS-OTHER-ST-1  PICTURE X(5).
           07 IS-OTHER-ST-2  PICTURE X(5).
     03 IS-ASSUME-EW         PICTURE X.
     03 IS-MAINT             PICTURE XXXX.
     03 IS-POL-DIST          PICTURE XXXX.
     03 IS-TR-DIST           PICTURE X.
     03 FILLER               PICTURE X(10).
     03 IS-STRAF             PICTURE 999.
     03 FILLER               PICTURE X(17).
FD   VOLFILE
         VALUE OF IDENTIFICATION IS "VOL   IDX"
     LABEL RECORDS ARE STANDARD
     RECORD CONTAINS 110 CHARACTERS

     DATA RECORD IS VOLFILE-REC.
01   VOLFILE-REC.
     03 VOL-DELETE-CODE      PICTURE X.
     03 VOL-REC-KEY.
        05 VOL-REC-ST.
           07 VOL-REC-ST-1   PICTURE X(5).
           07 VOL-REC-ST-2   PICTURE X(5).
        05 VOL-REC-INDEX     PICTURE XXX.
        05 VOL-REC-INDEX-N REDEFINES VOL-REC-INDEX   PICTURE S999.
     03 VOL-LATEST           PICTURE XXX.
     03 VOL-LATEST-N REDEFINES VOL-LATEST    PICTURE S999.
     03 VOL-OLDEST           PICTURE XXX.
     03 VOL-OLDEST-N REDEFINES VOL-OLDEST    PICTURE S999.
     03 VOL-REC-CODE         PICTURE 9.
     03 VOL-DATE.
        05 VOL-YR        PICTURE 99.
        05 VOL-MO        PICTURE 99.
        05 VOL-DA    ICTURE 99.
     03 VOL-D-O-W            PICTURE X.
     03 VOL-LOC.
        05 VOL-LOC-1         PICTURE X.
        05 VOL-LOC-2         PICTURE X.
        05 VOL-LOC-3         PICTURE X.
     03 VOL-DIST             PICTURE XX.
     03 VOL-REV-CODE         PICTURE X.
     03 VOL-SPEC-CODE        PICTURE X.
     03 VOL-AM-PM-PEAK.
        05 VOL-PEAK OCCURS 2 TIMES.
           07 VOL-N-E.
              09 V-NE-HR.
                 11 V-NE-H   PICTURE XX.
                 11 V-NE-M   PICTURE X.
              09 V-NE-VOL    PICTURE 9999.
           07 VOL-S-W.
              09 V-SW-HR.
                 11 V-SW-H   PICTURE XX.
                 11 V-SW-M   PICTURE X.
              09 V-SW-VOL    PICTURE 9999.
           07 VOL-TOT.
              09 V-TOT-HR.
                 11 V-TOT-H  PICTURE XX.
                 11 V-TOT-M  PICTURE X.
              09 V-TOT-VOL   PICTURE 9999.
     03 VOL-TOTALS-HR.
        05 VOL-TOTAL-NE      PICTURE X(5).
        05 VOL-TOTAL-NE-N REDEFINES VOL-TOTAL-NE PICTURE S9(5).
        05 VOL-TOTAL-SW      PICTURE X(5).
        05 VOL-TOTAL-SW-N REDEFINES VOL-TOTAL-SW PICTURE S9(5).
        05 VOL-TOTAL-TOT     PICTURE X(5).
        05 VOL-TOTAL-TOT-N REDEFINES VOL-TOTAL-TOT PICTURE S9(5).
     03 VOL-PHY-DESC.
        05 VOL-MED           PICTURE XXX.
        05 VOL-ADJ-FAC REDEFINES VOL-MED     PICTURE  9V99.
        05 VOL-RD-WIDTH      PICTURE XXXX.
        05 VOL-NO-LA         PICTURE XX.
     03 FILLER               PICTURE X(9).
FD   STRAT-FILE
         VALUE OF IDENTIFICATION IS "STRAT IDX"
     RECORD CONTAINS 200 CHARACTERS

     LABEL RECORDS ARE STANDARD
     DATA RECORD IS STRAT-REC.
01   STRAT-REC.
     03 ST-DELETE-CODE       PICTURE X.
     03 ST-REC-KEY           PICTURE 999.
     03 ST-TYPE              PICTURE 9.
     03 ST-NO-IN.
        05 ST-TOT-NO         PICTURE 9(5).
        05 ST-NO-WITH-VOL    PICTURE 9(5).
     03 ST-NO-ACC.
        05 ST-TOT-ACC        PICTURE 9(5).
        05 ST-INJ-FAT        PICTURE 9(5).
     03 ST-DATA.
        05 PER-IS-MILE.
           07 ST-M-1         PICTURE 99V999.
           07 ST-SD-1        PICTURE 99V999.
           07 ST-CV-1        PICTURE 99V999.
           07 ST-M-2         PICTURE 99V999.
           07 ST-SD-2        PICTURE 99V999.
           07 ST-CV-2        PICTURE 99V999.
        05 PER-MV-MVM.
           07 ST-M-3         PICTURE 99V999.
           07 ST-SD-3        PICTURE 99V999.
           07 ST-CV-3        PICTURE 99V999.
           07 ST-M-4         PICTURE 99V999.
           07 ST-SD-4        PICTURE 99V999.
           07 ST-CV-4        PICTURE 99V999.
     03 FILLER               PICTURE X(25).
     03 IS-STRATIFICATION    PICTURE X(61).
     03 SEG-STRATIFICATION REDEFINES IS-STRATIFICATION
                             PICTURE X(61).
     03 FILLER               PICTURE X(29).
FD   JURIS-FILE
         VALUE OF IDENTIFICATION IS "JURIS2DAT"
     LABEL RECORDS ARE STANDARD
     DATA RECORD IS J.
01   J.
     03 J-CODE                   PICTURE X(01).
     03 J-NAME                   PICTURE X(20).
     03 FILLER                   PICTURE X(59).
WORKING-STORAGE  SECTION.
77   ANT         COMPUTATIONAL   PICTURE S99     VALUE ZERO.
77   TX-1            PICTURE 9 VALUE 1.
77   TX-2            PICTURE 9 VALUE 1.
77   T81-REQ-HOLD    PICTURE XXX.
77   HDR-CTR         PICTURE 99.
77   PAGE-NO         PICTURE 999   VALUE ZEROES.
77   LINE-NO         PICTURE 99    VALUE ZEROES.
77   WORK-ACC        PICTURE S9(6).
77   EXT-SW          PICTURE 9     VALUE ZERO.
77   SW-81           PICTURE 9     VALUE 1.
77   SW-10           PICTURE 9     VALUE 1.
77   SW-12           PICTURE 9     VALUE 1.
77   SW-19           PICTURE 9     VALUE 1.
77   SW-20           PICTURE 9     VALUE 1.
77   SW-23           PICTURE 9     VALUE 1.
77   T12-REQ-HOLD    PICTURE XXX.
77   T19-REQ-HOLD    PICTURE XXX.
77   T12-T-LOC       PICTURE S9999 VALUE ZEROES.
77   T12-T-CAT-ACC   PICTURE S9(5) VALUE ZEROES.
77   T19-T-SEG       PICTURE S9999 VALUE ZEROES.
77   T19-T-CAT-ACC   PICTURE S9(5) VALUE ZEROES.
77   F               PICTURE 9.
77   L               PICTURE 99.
77   K               PICTURE 99.
77   CAT-SW          PICTURE 9     VALUE 1.
77   CTR             PICTURE 99.
77   C               PICTURE 99.
77   E               PICTURE 999.
77   M               PICTURE 99.
77   N               PICTURE 99.
77   O               PICTURE 999.
77   P               PICTURE 99.
77   Q               PICTURE 99.
77   R               PICTURE 99.
77   FT              PICTURE 999.
77   SW10A           PICTURE     9.
77   T10-IS-HOLD     PICTURE     X(10).
77   PRTY-CTR        PICTURE     9     VALUE ZERO.
77   INTER-SWITCH    PICTURE     9     VALUE ZERO.
77   DIRECTION-HOLD  PICTURE     XXXX JUSTIFIED RIGHT.
77   AT-S            PICTURE     9     VALUE ZERO.
77   G               PICTURE     99.
77   H               PICTURE     99.
77   VOL-LATEST-HOLD PICTURE     S999.
77   VOL-OLDEST-HOLD PICTURE     S999.
77   REV-SWITCH      PICTURE     9     VALUE ZERO.

77   L-CTR                   PICTURE 9.
77   T20-SEG-HOLD            PICTURE X(5).
77   ADT-CTR                 PICTURE S99     VALUE ZEROES.
77   D-CTR                   PICTURE 99.
77   DESC-SW                 PICTURE 9       VALUE 1.
77   SUBSCRIP-H-2            PICTURE 99.
77   10-SW                   PICTURE 9 VALUE ZERO.
77   23-SW                   PICTURE 9.
77   ITEM-7-HOLD             PICTURE S9999.
77   ITEM-7-VOL              PICTURE 9(6).
77   ITEM-7-HO               PICTURE S9999.
77   ITEM-7-HOO              PICTURE S9999V9.
77   SW-0-INDEX          PICTURE 9.
01   T10-REQ-HOLD.
     03 FILLER               PICTURE X(02).
     03 T10-REQ-HOLD-3       PICTURE X(01).
01   NAME-TABLE.
     03 NT-ELEMENTS  OCCURS 20.
        05 NT-CITY               PICTURE X(01).
        05 NT-NAME               PICTURE X(20).
01   ST-SYM-KEY                  PICTURE 9(03).
01   SYM-KEY.
     03 SYM-KEY-ST-1         PICTURE X(5).
     03 SYM-KEY-ST-2         PICTURE X(5).
01   VOL-SYM-KEY.
     03 VOL-SYM-ST-CODE.
        05 VOL-SYM-ST-1      PICTURE X(5).
        05 VOL-SYM-ST-2      PICTURE X(5).
     03 VOL-SYM-INDEX        PICTURE XXX.
     03 VOL-SYM-INDEX-N REDEFINES VOL-SYM-INDEX  PICTURE  999.
01   COMPUTERS-DATE.

     03 DATE.
        05 MO                PICTURE 99 VALUE 05.
        05 FILLER            PICTURE X.
        05 DA                PICTURE 99 VALUE 18.
        05 FILLER            PICTURE X.
        05 YR                PICTURE 99 VALUE 73.

01   CAT-LIMIT-HOLD.
     03 A                    PICTURE 999.
     03 B                    PICTURE 999.
01   S                       PICTURE XX.
01   T REDEFINES S           PICTURE 99.
01   HDR.
     03 FILLER               PICTURE X.
     03 FILLER               PICTURE XXX   VALUE 'T-'.
     03 H-NO                 PICTURE ZZ.
     03 FILLER               PICTURE X(49)   VALUE SPACES.
     03 H-SYSTEM                 PICTURE X(13).

     03 ANT-HDR                  PICTURE X(20).
     03 FILLER                   PICTURE X(37)   VALUE SPACES.
     03 FILLER               PICTURE X(5)  VALUE 'PAGE '.
     03 HP                   PICTURE ZZZ.
01   HDR2-3.
     03 FILLER               PICTURE X(45) VALUE SPACES.
     03 HDR-2-3-TITLE        PICTURE X(40).
     03 FILLER               PICTURE X(48) VALUE SPACES.
01   HDR4.
     03 FILLER               PICTURE X(10) VALUE SPACES.
     03 HDR4-TITLE           PICTURE X(20)
                 VALUE 'STREET CODES/NAMES: '.
     03 HDR4-ST1             PICTURE X(5).
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR4-ST2             PICTURE X(5).
     03 FILLER               PICTURE XX    VALUE SPACES.
     03 HDR4-N1              PICTURE X(30).
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR4-N2              PICTURE X(30).
     03 FILLER               PICTURE X(14) VALUE '   REQUESTER: '.
     03 HDR4-REQ             PICTURE X(15).
01   HDR5.
     03 FILLER               PICTURE X(10) VALUE SPACES.
     03 FILLER               PICTURE X(14) VALUE 'SEARCH DATES: '.
     03 HDR5-MO-FR           PICTURE XX.
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR5-DA-FR           PICTURE XX.
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR5-YR-FR           PICTURE XX.
     03 FILLER               PICTURE X(6)  VALUE '  TO  '.
     03 HDR5-MO-TO           PICTURE XX.
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR5-DA-TO           PICTURE XX.
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR5-YR-TO           PICTURE XX.
     03 FILLER               PICTURE X(61) VALUE SPACES.
     03 FILLER               PICTURE X(10) VALUE 'RUN DATE: '.
     03 HDR5-R-MO            PICTURE XX.
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR5-R-DA            PICTURE XX.
     03 FILLER               PICTURE X     VALUE '-'.
     03 HDR5-R-YR            PICTURE XX.
     03 FILLER               PICTURE X(8)  VALUE SPACES.
01   HDR4-A.
     03 FILLER               PICTURE X(10) VALUE SPACES.
     03 FILLER               PICTURE X(15)
                 VALUE 'STREET ROUTE:  '.
     03 HDR4-A-ST-C          PICTURE X(5).
     03 FILLER               PICTURE XXX   VALUE ' - '.
     03 HDR4-A-ST-N          PICTURE X(30).
     03 FILLER               PICTURE X(44) VALUE SPACES.
     03 FILLER               PICTURE X(11) VALUE 'REQUESTER: '.
     03 HDR4-A-REQ           PICTURE X(15).
01   T10-DET-P-HOLD.
     03 IMPACT-LINE2-H.
        05 IMPCT-1           PICTURE X(4).
        05 IMPCT-2           PICTURE X(4).
     03 NEW-IMP-HOLD REDEFINES IMPACT-LINE2-H.
        05 IMPCT-H           PICTURE ZZ99.
        05 IMPCT-H-1         PICTURE X.
        05 IMPCT-H-2         PICTURE X.
        05 IMPCT-H-3         PICTURE X.
        05 IMPCT-H-4         PICTURE X.
     03 T10LINE2-DIR-H       PICTURE X(4).
     03 T10LINE2-PR-GP-H     PICTURE X(7).
     03 T10LINE2-LIGHT-H     PICTURE X(5).
01   ST-VOL-HOLD.
     03 ST-V-HOLD OCCURS 2 TIMES.
       04 ST-V-H.
        05 ST-V-LOC          PICTURE XXX.
        05 ST-V-DIR          PICTURE XX.
        05 ST-V-V                    PICTURE  9(5).
        05 ST-V-SIGN         PICTURE XX.
01   T10-DAY-TABLE.
     03 T10-DAY-TAB  PICTURE X(14) VALUE 'MOTUWETHFRSASU'.
     03 T10-DAY-ENTRY REDEFINES T10-DAY-TAB OCCURS 7 TIMES
                             PICTURE XX.
01   T10-V-TYPE-TABLE.
     03 T10-V-TAB.
        05 FILLER    PICTURE X(4) VALUE 'UNK.'.
        05 FILLER    PICTURE X(4) VALUE 'AUTO'.
        05 FILLER    PICTURE X(4) VALUE 'AUTO'.
        05 FILLER    PICTURE X(4) VALUE 'AUTO'.
        05 FILLER    PICTURE X(4) VALUE 'AUTO'.
        05 FILLER    PICTURE X(4) VALUE 'TRCK'.
        05 FILLER    PICTURE X(4) VALUE 'AUTO'.
        05 FILLER    PICTURE X(4) VALUE 'AMBU'.
        05 FILLER    PICTURE X(4) VALUE 'BUS '.
        05 FILLER    PICTURE X(4) VALUE 'TRCK'.
        05 FILLER    PICTURE X(4) VALUE 'TRCK'.
        05 FILLER    PICTURE X(4) VALUE 'CYCL'.
        05 FILLER    PICTURE X(4) VALUE 'CYCL'.
        05 FILLER    PICTURE X(4) VALUE 'CYCL'.
        05 FILLER    PICTURE X(4) VALUE 'CYCL'.
        05 FILLER    PICTURE X(4)  VALUE 'TRCK'.
        05 FILLER    PICTURE X(4) VALUE 'TRAN'.
        05 FILLER    PICTURE X(4) VALUE 'BIKE'.
        05 FILLER    PICTURE X(4) VALUE 'AUTO'.
        05 FILLER    PICTURE X(4) VALUE 'ANML'.
        05 FILLER    PICTURE X(4) VALUE 'OTHR'.
     03 T10-V-ENTRY REDEFINES T10-V-TAB OCCURS 21 TIMES
                     PICTURE XXXX.
01   T10-CIR-TABLE.
     03 T10-CIR-TAB.
        05 FILLER    PICTURE X(10) VALUE '   NONE   '.
        05 FILLER    PICTURE X(10) VALUE '  SPEED   '.
        05 FILLER    PICTURE X(10) VALUE 'YIELD R/W '.
        05 FILLER    PICTURE X(10) VALUE 'WRONG SIDE'.
        05 FILLER    PICTURE X(10) VALUE ' RAN STOP '.
        05 FILLER    PICTURE X(10) VALUE 'RAN SIGNAL'.
        05 FILLER    PICTURE X(10) VALUE 'IMPR PASS '.
        05 FILLER    PICTURE X(10) VALUE 'TOO CLOSE '.
        05 FILLER    PICTURE X(10) VALUE 'IMPR TURN '.
        05 FILLER    PICTURE X(10) VALUE 'IMPR DRIVE'.
        05 FILLER    PICTURE X(10) VALUE '  OTHER   '.
        05 FILLER    PICTURE X(10) VALUE 'LANE CHG  '.
     03 T10-CIR-ENTRY REDEFINES T10-CIR-TAB OCCURS 12 TIMES
                             PICTURE X(10).
01   T10-A-TYPE-TABLE.
     03 T10-A-TYPE-TAB.
        05 FILLER    PICTURE X(7)  VALUE '  ROR  '.
       05 FILLER     PICTURE X(7)  VALUE 'OVER TN'.
        05 FILLER    PICTURE X(7)  VALUE '  PED  '.
        05 FILLER    PICTURE X(7)  VALUE '       '.
        05 FILLER    PICTURE X(7)  VALUE 'PRK-VEH'.
        05 FILLER    PICTURE X(7)  VALUE 'NON VEH'.
        05 FILLER    PICTURE X(7)  VALUE 'FIX OBJ'.
        05 FILLER    PICTURE X(7)  VALUE 'OTH OBJ'.
        05 FILLER    PICTURE X(7)  VALUE 'NONCOLL'.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
       05 FILLER     PICTURE X(7)  VALUE '       '.
        05 FILLER    PICTURE X(7)  VALUE '  RA   '.
        05 FILLER    PICTURE X(7)  VALUE ' RA-LT '.
        05 FILLER    PICTURE X(7)  VALUE ' RA-RT '.
        05 FILLER    PICTURE X(7)  VALUE '  LT   '.
        05 FILLER    PICTURE X(7)  VALUE ' LT-LT '.
        05 FILLER    PICTURE X(7)  VALUE ' LT-RT '.
        05 FILLER    PICTURE X(7)  VALUE ' RE/SS '.
        05 FILLER    PICTURE X(7)  VALUE ' RE-LT '.
        05 FILLER    PICTURE X(7)  VALUE ' RE-RT '.
        05 FILLER    PICTURE X(7)  VALUE 'HEAD ON'.
        05 FILLER    PICTURE X(7)  VALUE ' RE/SS '.
       05 FILLER     PICTURE X(7)  VALUE ' SS-LT '.
       05 FILLER     PICTURE X(7)  VALUE ' SS-RT '.
       05 FILLER     PICTURE X(7)  VALUE 'U-TURN '.
       05 FILLER     PICTURE X(7)  VALUE 'EN DRWY'.
       05 FILLER     PICTURE X(7)  VALUE 'LV DRWY'.
       05 FILLER     PICTURE X(7)  VALUE ' OTHER '.
     03 T10-A-TYPE-ENTRY REDEFINES T10-A-TYPE-TAB OCCURS 36 TIMES
                             PICTURE X(7).
01   PRI-GP-TABLE.
     03 PRI-GP-TAB.
        05 FILLER    PICTURE X(14) VALUE ' DRUNK DRIVING'.
        05 FILLER    PICTURE X(14) VALUE ' SPEED        '.
        05 FILLER    PICTURE X(14) VALUE 'SIGNAL  LIGHT '.
        05 FILLER    PICTURE X(14) VALUE ' STOP   SIGN  '.
        05 FILLER    PICTURE X(14) VALUE ' WRONG  SIDE  '.
        05 FILLER    PICTURE X(14) VALUE '  R/W    LT   '.
        05 FILLER    PICTURE X(14) VALUE '  R/W  STP/YLD'.
        05 FILLER    PICTURE X(14) VALUE '  R/W   OTHER '.
        05 FILLER    PICTURE X(14) VALUE '  R/W    PED. '.
        05 FILLER    PICTURE X(14) VALUE '  PED.  VIOL. '.
        05 FILLER    PICTURE X(14) VALUE 'IMPROP.  TURN '.
        05 FILLER    PICTURE X(14) VALUE 'FOL TOO CLOSE '.
        05 FILLER    PICTURE X(14) VALUE 'UNSAFE CHANGE '.
        05 FILLER    PICTURE X(14) VALUE ' START/BACKING'.
        05 FILLER    PICTURE X(14) VALUE ' OTHER   VIOL '.
        05 FILLER    PICTURE X(14) VALUE 'NON-APPIMPR DR'.
        05 FILLER    PICTURE X(14) VALUE 'UNKNOWN       '.
     03 FILLER REDEFINES PRI-GP-TAB OCCURS 17 TIMES.
        05 GP-LINE1          PICTURE X(7).
        05 GP-LINE2          PICTURE X(7).
01   DIR-TABLE.
     03 DIR-TAB PICTURE XXXX VALUE 'NESW'.
     03 DIR-ENTRY REDEFINES DIR-TAB OCCURS 4 TIMES PICTURE X.
01   T10-V-ACT-TABLE.
     03 T10-V-ACT-TAB.
        05 FILLER    PICTURE X(8)  VALUE 'STRAIGHT'.
        05 FILLER    PICTURE X(8)  VALUE 'OVERTAKE'.
        05 FILLER    PICTURE X(8)  VALUE '   RT   '.
        05 FILLER    PICTURE X(8)  VALUE '   LT   '.
        05 FILLER    PICTURE X(8)  VALUE ' U TURN '.
        05 FILLER    PICTURE X(8)  VALUE 'SLW/STOP'.
        05 FILLER    PICTURE X(8)  VALUE 'START LN'.
        05 FILLER    PICTURE X(8)  VALUE 'STRT PRK'.
        05 FILLER    PICTURE X(8)  VALUE 'BACKING '.
        05 FILLER    PICTURE X(8)  VALUE 'STOPPED '.
        05 FILLER    PICTURE X(8)  VALUE ' PARKED '.
        05 FILLER    PICTURE X(8)  VALUE ' OTHER  '.
        05 FILLER    PICTURE X(8)  VALUE ' UNKNOWN'.
     03 T10-V-ACT-ENTRY REDEFINES T10-V-ACT-TAB
                     OCCURS 13 TIMES PICTURE X(8).
01   T10-PED-ACT-TABLE.
     03 T10-PED-ACT-TAB.
        05 FILLER    PICTURE X(8)  VALUE '   01   '.
        05 FILLER    PICTURE X(8)  VALUE '   02   '.
        05 FILLER    PICTURE X(8)  VALUE '   03   '.
        05 FILLER    PICTURE X(8)  VALUE '   04   '.
        05 FILLER    PICTURE X(8)  VALUE '   05   '.
        05 FILLER    PICTURE X(8)  VALUE '   06   '.
        05 FILLER    PICTURE X(8)  VALUE '   07   '.
        05 FILLER    PICTURE X(8)  VALUE '   08   '.
        05 FILLER    PICTURE X(8)  VALUE '   09   '.
        05 FILLER    PICTURE X(8)  VALUE '   10   '.
        05 FILLER    PICTURE X(8)  VALUE '   11   '.
        05 FILLER    PICTURE X(8)  VALUE '   12   '.
        05 FILLER    PICTURE X(8)  VALUE '   13   '.
     03 T10-PED-ACT-ENTRY REDEFINES T10-PED-ACT-TAB
            OCCURS 13 TIMES  PICTURE X(8).
01   ITEM-8-LEG-TABLE.
     03 ITEM-8-L-TAB.
        05 FILLER    PICTURE X(21) VALUE '2 LEGS (L)           '.
        05 FILLER    PICTURE X(21) VALUE '3 LEGS (T)           '.
        05 FILLER    PICTURE X(21) VALUE '3 LEGS (Y)           '.
        05 FILLER    PICTURE X(21) VALUE '4 LEGS (+)           '.
        05 FILLER    PICTURE X(21) VALUE '4 LEG JOGGED         '.
        05 FILLER    PICTURE X(21) VALUE '4 LEG NON-STANDARD   '.
        05 FILLER    PICTURE X(21) VALUE 'MORE THAN 4 LEGS     '.
        05 FILLER    PICTURE X(21) VALUE 'DEAD END OR CITY LINE'.
        05 FILLER    PICTURE X(21) VALUE 'GRADE SEPARATION     '.
     03 ITEM-8-L-ENTRY REDEFINES ITEM-8-L-TAB
                     OCCURS  9 TIMES PICTURE X(21).
01   ITEM-8-IS-TABLE.
     03 ITEM-8-I-TAB.
        05 FILLER    PICTURE X(21) VALUE 'NONE                 '.
         05 FILLER   PICTURE X(21) VALUE 'SIGNAL               '.
        05 FILLER    PICTURE X(21) VALUE 'STOP SIGNS - PARTIAL '.
        05 FILLER    PICTURE X(21) VALUE 'STOP SIGNS - ALL LEGS'.
        05 FILLER    PICTURE X(21) VALUE 'YIELD SIGN           '.
     03 ITEM-8-I-ENTRY REDEFINES ITEM-8-I-TAB
                     OCCURS 5 TIMES PICTURE X(21).
01   ITEM-8-SIG-TABLE.
     03 ITEM-8-S-TAB.
        05 FILLER    PICTURE X(21) VALUE 'NON-SIGNALIZED       '.
        05 FILLER    PICTURE X(21) VALUE 'FIXED TIME           '.
        05 FILLER    PICTURE X(21) VALUE 'SEMI ACTUATED        '.
        05 FILLER    PICTURE X(21) VALUE 'FULLY ACTUATED       '.
         05 FILLER   PICTURE X(21) VALUE 'OTHER                '.
     03 ITEM-8-S-ENTRY REDEFINES ITEM-8-S-TAB
                     OCCURS 5  TIMES PICTURE X(21).
01   ITEM-8-APP-TABLE.
     03 ITEM-8-A-TAB.
        05 FILLER    PICTURE X(21) VALUE 'NO. THRU APPR. LANES:'.
        05 FILLER    PICTURE X(21) VALUE 'TYPE OF TURN LANE:   '.
        05 FILLER    PICTURE X(21) VALUE 'TYPE OF CONTROL SIGN:'.
        05 FILLER    PICTURE X(21) VALUE 'SIZE OF MAST ARM IND:'.
        05 FILLER    PICTURE X(21) VALUE 'TURN SIGNAL CONTROL: '.
        05 FILLER    PICTURE X(21) VALUE 'LT PERM. SIGN REG.:  '.
        05 FILLER    PICTURE X(21) VALUE 'LT PEDESTAL SIGN REG:'.
        05 FILLER    PICTURE X(21) VALUE 'BUS ZONE:            '.
        05 FILLER    PICTURE X(21) VALUE 'PED SIG. ACROSS APPR:'.
        05 FILLER    PICTURE X(21) VALUE 'MEDIAN TYPE:         '.
        05 FILLER    PICTURE X(21) VALUE 'MEDIAN WIDTH:        '.
        05 FILLER    PICTURE X(21) VALUE 'PARK./STOP. RESTRICT:'.
        05 FILLER    PICTURE X(21) VALUE 'DIRECTION FLOW:      '.
        05 FILLER    PICTURE X(21) VALUE 'SEC. SIGNAL LOCATION:'.
     03 ITEM-8-A-ENTRY REDEFINES ITEM-8-A-TAB
                     OCCURS 14 TIMES PICTURE X(21).
01   T10-SUM-WORK.
       02 T10-SUM-LINEA-WORK.
     03 T10-SUM-LINEA OCCURS 9 TIMES.
         05 T10-LINEA-FLD OCCURS 10 TIMES PICTURE 9999.
       02 T10-SUM-LINEB-WORK.
     03 T10-SUM-LINEB OCCURS 7 TIMES.
     05  T10-LINEB-FLD OCCURS 3 TIMES PICTURE 999.
        05 T10-V1.
           07 T10-V1A        PICTURE X.
           07 T10-V1B        PICTURE X.
           07 T10-V1C        PICTURE X.
        05 T10-V2            PICTURE XX.
         05 T10-V3           PICTURE 9(5).
        05 T10-V3A           PICTURE XX.
        05 T10-V4-M          PICTURE XX.
        05 T10-V4-D          PICTURE XX.
        05 T10-V4-Y          PICTURE XX.
       02 T10-SUM-LINEC-WORK.
     03 T10-SUM-LINEC OCCURS 4 TIMES.
         05 T10-LINEC-C.
           07 LINEC-MEAN     PICTURE 99V999.
           07 LINEC-DEV      PICTURE 99V999.
           07 LINEC-CVA      PICTURE 99V999.
     03 T10-SUM-LNEC1-C.
     05 T10-SUM-LINEC1 OCCURS 5 TIMES
                             PICTURE 99.
     03 T10-SUM-LINEC2.
        04 FILLER OCCURS 4 TIMES.
        05 T10-S-LINEC3 OCCURS 14 TIMES PICTURE X.
01   ITEM-8-CLASS-TABLE.
     02 ITEM-8-CLASS-TAB.
     03 FILLER   PICTURE X(21) VALUE '    LOCAL - LOCAL    '.
     03 FILLER   PICTURE X(21) VALUE 'COLLECTOR - LOCAL    '.
     03 FILLER   PICTURE X(21) VALUE 'COLLECTOR - COLLECTOR'.
     03 FILLER   PICTURE X(21) VALUE 'MINOR ART - LOCAL    '.
     03 FILLER   PICTURE X(21) VALUE 'MINOR ART - COLLECTOR'.
     03 FILLER   PICTURE X(21) VALUE 'MINOR ART - MINOR ART'.
     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - LOCAL    '.
     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - COLLECTOR'.
     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - MINOR ART'.
     03 FILLER   PICTURE X(21) VALUE 'PRIM. ART - PRIM. ART'.
     02 ITEM-8-C-ENTRY REDEFINES ITEM-8-CLASS-TAB
                      OCCURS 10 TIMES PICTURE X(21).
01   MONTH-TABLE.
     03 FILLER       PICTURE 999 VALUE 031.
     03 FILLER       PICTURE 999 VALUE 059.
     03 FILLER       PICTURE 999 VALUE 090.
     03 FILLER       PICTURE 999 VALUE 120.
     03 FILLER       PICTURE 999 VALUE 151.
     03 FILLER       PICTURE 999 VALUE 181.
     03 FILLER       PICTURE 999 VALUE 212.
     03 FILLER       PICTURE 999 VALUE 243.
     03 FILLER       PICTURE 999 VALUE 273.
     03 FILLER       PICTURE 999 VALUE 304.
     03 FILLER       PICTURE 999 VALUE 334.
01   FILLER REDEFINES MONTH-TABLE.
     03 MO-ENTRY OCCURS 11 TIMES PICTURE 999.
01   T-D-HOLD        PICTURE X(12).
01   FILLER REDEFINES T-D-HOLD.
     03 C-M          PICTURE 99.
     03 C-D          PICTURE 99.
     03 C-Y          PICTURE 99.
     03 C-M1         PICTURE 99.
     03 C-D1         PICTURE 99.
     03 C-Y1         PICTURE 99.
01   D-TOTAL.
     03 TO-D-TOT     PICTURE 9999.
     03 FR-D-TOT     PICTURE 9999.
     03 YR-D-TOT     PICTURE 9999.
     03 TND          PICTURE 9999.
     03 MO-CT        PICTURE 99.
01   STR-SUM.
     03 IF-AC-SM             PICTURE 999 VALUE ZERO.
     03 T-AC-SM              PICTURE 999 VALUE ZERO.
01    DIR-VEH-C          PICTURE 9 VALUE ZERO.
01   DIR-VEH-HOLD    PICTURE 9 VALUE ZERO.
01   SUPL-RD-ON      PICTURE 9 VALUE ZERO.
01   DIR-ST-SW       PICTURE 9 VALUE ZERO.
01   DATE-2YRS       PICTURE 9(6).
01   FILLER REDEFINES DATE-2YRS.
     03 YRS-2        PICTURE 99.
     03 MOS-2        PICTURE 99.
     03 DAS-2        PICTURE 99.
PROCEDURE  DIVISION.
FIRST-PAR.
     MOVE MO TO HDR5-R-MO.
     MOVE DA TO HDR5-R-DA.
     MOVE YR TO HDR5-R-YR.
     OPEN INPUT  REPORT-FILE
                 VOLFILE
                 ISFILE
                 STRAT-FILE
                 JURIS-FILE
          OUTPUT PRINT-FILE.
     PERFORM LOAD-NAME-TABLE THRU LNT-EXIT.
START-PROCESSING.
     READ REPORT-FILE AT END GO TO EOJ.
PROCESS-REPORT.
        MOVE 10 TO REPORT-CODE.
     IF REPORT-CODE > 10 GO TO EOJ.
     IF REPORT-CODE = 10 PERFORM PRINT-10 THRU PRINT-10-X.
READ-NEXT-REC.
     GO TO START-PROCESSING.
EOJ.
     IF 10-SW = 1 MOVE ZERO TO 10-SW MOVE 2 TO SW10A
         ADD 1 TO PAGE-NO PERFORM SUM-10 THRU SUM-10-X.
     MOVE SPACES TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING   TOP-OF-PAGE.
     DISPLAY 'T3090 COMPLETED NORMALLY'.
     CLOSE REPORT-FILE
           PRINT-FILE
           VOLFILE
           ISFILE
           STRAT-FILE
           JURIS-FILE.
     STOP RUN.
PRINT-10.
     IF SW-10 NOT = 1 GO TO T10B. MOVE ZERO TO SW-10.
         MOVE 1 TO PAGE-NO MOVE 10 TO HDR-CTR.
         MOVE REQ-NO TO T10-REQ-HOLD. PERFORM INIT THRU INIT-X.
T10A.    MOVE 1 TO SW10A MOVE 0 TO LINE-NO.
         MOVE T10-IS-CODE TO T10-IS-HOLD.
     PERFORM HEADER THRU HEADER-X.
     MOVE T10-SEARCH-DATES TO T-D-HOLD.
     PERFORM NO-D-RTN THRU NO-D-RTN-X.
     MOVE 1 TO 10-SW.
T10B.
     IF T10-IS-CODE = T10-IS-HOLD GO TO TB.
TA.  MOVE 2 TO SW10A ADD 1 TO  PAGE-NO.
     PERFORM SUM-10 THRU SUM-10-X.
     IF REQ-NO = T10-REQ-HOLD ADD 1 TO PAGE-NO GO TO T10A.
       MOVE REQ-NO TO T10-REQ-HOLD MOVE 1 TO PAGE-NO GO TO T10A.
TB.  IF REQ-NO = T10-REQ-HOLD GO TO T10C ELSE GO TO TA.
T10C.
     IF LINE-NO > 40 ADD 1 TO PAGE-NO
         MOVE 1 TO SW10A PERFORM HEADER THRU HEADER-X.
     PERFORM T10-DET THRU T10-DET-X.
PRINT-10-X.  EXIT.
T10-DET.
     MOVE 0 TO PRTY-CTR. MOVE SPACES TO PRINT-LINE, T10-DET-P-HOLD
     MOVE T10-CLASS (1) TO T10-CLASS1.
     MOVE '-' TO P-T10-1, P-T10-2  MOVE ':' TO P-T10-3.
     IF DOW > '0' AND DOW < '8' MOVE T10-DAY-ENTRY (D-O-W)
         TO P-T10-DAY ELSE MOVE SPACES TO P-T10-DAY.
     MOVE DIR-ANL   TO DIR-ANL1   MOVE PRI-CODE TO PRI-CODE1.
     MOVE SPEC-CIR  TO SPEC-CIR1.
     MOVE T10-DR-NO TO T10-DR-NO1 MOVE T10-MO TO T10-MO1.
     MOVE T10-DA    TO T10-DA1    MOVE T10-YR TO T10-YR1.
     MOVE T10-HR    TO T10-HR1    MOVE T10-M  TO T10-M1.
     IF WEATHER = 1 MOVE 'C' TO P-WEATHER.
     IF WEATHER = 2 MOVE 'R' TO P-WEATHER.
     IF WEATHER = 3 MOVE 'F' TO P-WEATHER.
     IF WEATHER = 4 MOVE 'O' TO P-WEATHER.
     IF LIGHT   = 1 MOVE ' DAY ' TO P-LIGHT MOVE SPACES TO
         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (1, 3).
     IF LIGHT   = 2 MOVE 'DUSK/' TO P-LIGHT MOVE 'DAWN ' TO
         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (2, 3).
     IF LIGHT = 3, MOVE 'DARK ' TO P-LIGHT, MOVE '     ' TO
         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (3, 3).
     IF LIGHT = 4, MOVE 'DARK/' TO P-LIGHT, MOVE 'ST LT' TO
         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (3, 3).
     IF LIGHT = 5, MOVE ' DAY ' TO P-LIGHT, MOVE 'CLOUD' TO
         T10LINE2-LIGHT-H ADD 1 TO T10-LINEB-FLD (4, 3).
         ADD 1 TO T10-LINEB-FLD (5, 3).
T0.  IF RD-CONDITION = 1 MOVE 'D' TO P-RD-CND.
     IF RD-CONDITION = 2 MOVE 'W' TO P-RD-CND.
     IF RD-CONDITION = 3 MOVE 'I' TO P-RD-CND.
     IF RD-CONDITION = 4 MOVE 'O' TO P-RD-CND.
T1.  MOVE T10-AGE (1) TO T10-AGE1.
     IF PED-ACTION (1) NOT = ZERO MOVE 'PED ' TO P-VEH-TYPE ELSE
     IF VEH-TYPE (1) < 21 ADD 1 TO VEH-TYPE (1) MOVE VEH-TYPE (1)
     TO SUBSCRIP-H-2 MOVE T10-V-ENTRY (SUBSCRIP-H-2) TO
     P-VEH-TYPE.
     MOVE T10-COND (1) TO T10-COND1.
     IF CONTRIB-CIR (1) > 00 AND CONTRIB-CIR (1) < 13 MOVE
     CONTRIB-CIR (1) TO SUBSCRIP-H-2 MOVE T10-CIR-ENTRY
     (SUBSCRIP-H-2) TO P-CONTRI-CIR.
T2.  IF GP-CODE > '00' AND GP-CODE < '18' MOVE GP-LINE1
         (GROUP-CODE) TO P-GP-CODE MOVE GP-LINE2 (GROUP-CODE)
         TO T10LINE2-PR-GP-H.
     MOVE T10-INJ-A TO T10-INJ-A1 MOVE T10-INJ-B TO T10-INJ-B1.
     MOVE T10-INJ-C TO T10-INJ-C1 MOVE T10-INJ-K TO T10-INJ-K1.
     IF T10-A-SEV = 1 ADD 1 TO T10-LINEB-FLD (3, 1).
     IF T10-A-SEV = 2 ADD 1 TO T10-LINEB-FLD (4, 1).
     IF T10-A-SEV = 3 ADD 1 TO T10-LINEB-FLD (5, 1).
     IF T10-A-SEV = 4 ADD 1 TO T10-LINEB-FLD (1, 1).
     IF T10-A-SEV = 5 ADD 1 TO T10-LINEB-FLD (6, 1).
                      ADD T10-INJ-A TO T10-LINEB-FLD (3, 2).
                      ADD T10-INJ-B TO T10-LINEB-FLD (4, 2).
                      ADD T10-INJ-C TO T10-LINEB-FLD (5, 2).
                      ADD T10-INJ-K TO T10-LINEB-FLD (1, 2).
      ADD 1 TO T10-LINEB-FLD (7, 1)  ADD T10-INJ-A T10-INJ-B
      T10-INJ-C T10-INJ-K TO T10-LINEB-FLD (7, 2).
T3.  IF PED-ACTION (1) NOT = 00 AND PED-ACTION (1) < 14
         MOVE PED-ACTION (1) TO SUBSCRIP-H-2
        MOVE T10-PED-ACT-ENTRY (SUBSCRIP-H-2) TO P-10-ACTION
        ELSE IF PED-ACTION (1) = 00 AND VEH-ACTION (1) > 00 AND
        VEH-ACTION (1) < 14 MOVE VEH-ACTION (1) TO SUBSCRIP-H-2
       MOVE T10-V-ACT-ENTRY (SUBSCRIP-H-2) TO P-10-ACTION.
T4.  IF T10-ACC-TYPE > 00 AND T10-ACC-TYPE < 37
         MOVE T10-A-TYPE-ENTRY (T10-ACC-TYPE) TO P-ACC-TYPE.
     IF PRI-CODE-1 NOT = 9 GO TO NEW-IMPACT-RTN.
     IF INTERSECTION NOT NUMERIC MOVE   ZERO TO INTERSECTION.
     IF DIRECTION-1  NOT NUMERIC MOVE   ZERO TO DIRECTION-1.
     IF DIRECTION-2  NOT NUMERIC MOVE   ZERO TO DIRECTION-2.
     IF INTERSECTION = 0 MOVE 'I/S ' TO IMPACT-1 MOVE 1 TO
         INTER-SWITCH ELSE MOVE 0 TO INTER-SWITCH GO TO NON-INTER.
     MOVE SPACES TO DIRECTION-HOLD.
     IF DIRECTION-2 = 5 MOVE 'PED ' TO DIRECTION-HOLD.
     IF T10-REVERSE-ST-C = 1 GO TO INTER-A.
     IF DIRECTION-1 = 1  MOVE 'NB-2'  TO IMPACT-2.
     IF DIRECTION-1 = 2  MOVE 'EB-1'  TO IMPACT-2.
     IF DIRECTION-1 = 3  MOVE 'SB-2'  TO IMPACT-2.
     IF DIRECTION-1 = 4  MOVE 'WB-1'  TO IMPACT-2.
     IF DIRECTION-2 = 1  MOVE 'NB-2'  TO DIRECTION-HOLD.
     IF DIRECTION-2 = 2  MOVE 'EB-1'  TO DIRECTION-HOLD.
     IF DIRECTION-2 = 3  MOVE 'SB-2'  TO DIRECTION-HOLD.
     IF DIRECTION-2 = 4  MOVE 'WB-1'  TO DIRECTION-HOLD.
     GO TO INTER-B.
INTER-A.
     IF DIRECTION-1 = 1 MOVE 'NB-1' TO IMPACT-2.
     IF DIRECTION-1 = 2 MOVE 'EB-2' TO IMPACT-2.
     IF DIRECTION-1 = 3 MOVE 'SB-1' TO IMPACT-2.
     IF DIRECTION-1 = 4 MOVE 'WB-2' TO IMPACT-2.
     IF DIRECTION-2 = 1 MOVE 'NB-1' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 2 MOVE 'EB-2' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 3 MOVE 'SB-1' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 4 MOVE 'WB-2' TO DIRECTION-HOLD.
INTER-B.
     IF DIRECTION-HOLD = SPACE GO TO ACCIDENT-TYPE ELSE IF
     DIRECTION-1 = DIRECTION-2 NEXT SENTENCE ELSE GO TO
     ACCIDENT-TYPE.
     IF T10-REVERSE-ST-C = 1 GO TO INTER-C.
     IF DIRECTION-2 = 4 MOVE 'WB-1' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 1 MOVE 'NB-2' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 2 MOVE 'EB-1' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 3 MOVE 'SB-2' TO DIRECTION-HOLD.
     GO TO ACCIDENT-TYPE.
INTER-C.
     IF DIRECTION-2 = 1 MOVE 'NB-1' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 2 MOVE 'EB-2' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 3 MOVE 'SB-1' TO DIRECTION-HOLD.
     IF DIRECTION-2 = 4 MOVE 'WB-2' TO DIRECTION-HOLD.
     GO TO ACCIDENT-TYPE.
NON-INTER.
     IF INTERSECTION = 1 MOVE ' PED' TO IMPACT-1.
     IF INTERSECTION = 2 MOVE ' DEP' TO IMPACT-1
         MOVE 'N/E' TO IMPACT-2.
     IF INTERSECTION = 3 MOVE ' APP' TO IMPACT-1
         MOVE 'S/W' TO IMPACT-2.
     IF INTERSECTION = 4 MOVE ' DEP' TO IMPACT-1
         MOVE 'S/W' TO IMPACT-2.
     IF INTERSECTION = 5 MOVE ' APP' TO IMPACT-1
         MOVE 'N/E' TO IMPACT-2.
     IF INTERSECTION = 6 MOVE '  HO' TO IMPACT-1
         MOVE 'N/E' TO IMPACT-2.
     IF INTERSECTION = 7 MOVE '  HO' TO IMPACT-1
         MOVE 'S/W' TO IMPACT-2.
     IF INTERSECTION = 8 OR INTERSECTION = 9 MOVE ' OTHER' TO
         P-PT-IMPACT.
     IF T10-REVERSE-ST-C = 0 MOVE ' 1ST' TO DIRECTION-HOLD ELSE
     MOVE ' 2ND' TO DIRECTION-HOLD.
     IF DIRECTION-2 = ZERO AND DIRECTION-1 = ZERO NEXT SENTENCE
     ELSE GO TO NON-INTER-B.
     IF T10-REVERSE-ST-C = 0 GO TO NON-INTER-A.
     IF INTERSECTION = 2  ADD 1 TO T10-LINEA-FLD (4, 7).
     IF INTERSECTION = 3  ADD 1 TO T10-LINEA-FLD (5, 3).
     IF INTERSECTION = 4  ADD 1 TO T10-LINEA-FLD (5, 7).
     IF INTERSECTION = 5  ADD 1  TO T10-LINEA-FLD (4, 3).
     IF INTERSECTION = 6  ADD 1  TO T10-LINEA-FLD (4, 7).
     IF INTERSECTION = 7  ADD 1  TO T10-LINEA-FLD (5, 3).
     GO TO ACCIDENT-TYPE.
NON-INTER-A.
     IF INTERSECTION = 2  ADD 1  TO T10-LINEA-FLD (2, 7).
     IF INTERSECTION = 3  ADD 1  TO T10-LINEA-FLD (3, 3).
     IF INTERSECTION = 4  ADD 1  TO T10-LINEA-FLD (3, 7).
     IF INTERSECTION = 5  ADD 1  TO T10-LINEA-FLD (2, 3).
     IF INTERSECTION = 6  ADD 1  TO T10-LINEA-FLD (2, 7).
     IF INTERSECTION = 7  ADD 1  TO T10-LINEA-FLD (3, 3).
     GO TO ACCIDENT-TYPE.
NON-INTER-B.
     IF DIRECTION-1 = ZERO AND DIRECTION-2 = 1 NEXT SENTENCE ELSE
     GO TO NON-INTER-D.
     IF T10-REVERSE-ST-C = 0 GO TO NON-INTER-C.
     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (4, 8).
     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (5, 4).
     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (5, 8).
     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (4, 4).
     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (4, 8).
     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (5, 8).
     GO TO ACCIDENT-TYPE.
NON-INTER-C.
     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (2, 8).
     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (3, 4).
     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (3, 8).
     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (2, 4).
     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (2, 8).
     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (3, 4).
     GO TO ACCIDENT-TYPE.
NON-INTER-D.
     IF T10-REVERSE-ST-C = 0 GO TO NON-INTER-E.
     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (4, 9).
     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (5, 5).
     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (5, 9).
     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (4, 5).
     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (4, 9).
     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (5, 5).
     GO TO ACCIDENT-TYPE.
NON-INTER-E.
     IF INTERSECTION = 2 ADD 1 TO T10-LINEA-FLD (2, 9).
     IF INTERSECTION = 3 ADD 1 TO T10-LINEA-FLD (3, 5).
     IF INTERSECTION = 4 ADD 1 TO T10-LINEA-FLD (2, 5).
     IF INTERSECTION = 5 ADD 1 TO T10-LINEA-FLD (2, 9).
     IF INTERSECTION = 6 ADD 1 TO T10-LINEA-FLD (3, 9).
     IF INTERSECTION = 7 ADD 1 TO T10-LINEA-FLD (3, 5).
     GO TO ACCIDENT-TYPE.
NEW-IMPACT-RTN.
     IF T10-AT-IS = 1 MOVE 'YES' TO P-10-IS ELSE
     MOVE ' NO' TO P-10-IS.
     IF T10-REVERSE-ST-C NOT = 0 GO TO NEW-IMP-RTN-R.
         MOVE LOCAT-CODE TO INTER-PLUS-1.
         IF PRI-CODE-1 > 0 AND PRI-CODE-1 < 5 MOVE
             DIR-ENTRY (PRI-CODE-1) TO NEW-IMP-2 ELSE MOVE SPACE
             TO NEW-IMP-2. MOVE SPACE TO NEW-IMP-1.
     IF PRI-CODE-2 > 0 AND PRI-CODE-2 < 5 MOVE
             DIR-ENTRY (PRI-CODE-2) TO NEW-IMP-4 ELSE MOVE SPACE
             TO NEW-IMP-4. MOVE '/' TO NEW-IMP-3.
         MOVE SEC-FEET TO IMPCT-H.
         IF SEC-CODE-1 > 0 AND SEC-CODE-1 < 5 MOVE
            DIR-ENTRY (SEC-CODE-1) TO IMPCT-H-2 ELSE MOVE SPACE
             TO IMPCT-H-2. MOVE SPACE TO IMPCT-H-1.
         IF SEC-CODE-2 > 0 AND SEC-CODE-2 < 5 MOVE
             DIR-ENTRY (SEC-CODE-2) TO IMPCT-H-4 ELSE MOVE SPACE
             TO IMPCT-H-4. MOVE    '/' TO IMPCT-H-3.
     IF T10-AT-IS = 1 GO TO A-PIS.
     GO TO A-PX.
NEW-IMP-RTN-R.
     MOVE SEC-FEET TO INTER-PLUS-1.
         MOVE LOCAT-CODE TO IMPCT-H.
     MOVE SPACE TO NEW-IMP-1, IMPCT-H-1.
     MOVE '/' TO NEW-IMP-3, IMPCT-H-3.
     IF SEC-CODE-1 > 0 AND SEC-CODE-1 < 5
         MOVE DIR-ENTRY (SEC-CODE-1) TO NEW-IMP-2 ELSE MOVE SPACE
             TO NEW-IMP-2.
     IF SEC-CODE-2 > 0 AND SEC-CODE-2 < 5
         MOVE DIR-ENTRY (SEC-CODE-2) TO NEW-IMP-4 ELSE MOVE SPACE
             TO NEW-IMP-4.
     IF PRI-CODE-1 > 0 AND PRI-CODE-1 < 5
         MOVE DIR-ENTRY (PRI-CODE-1) TO IMPCT-H-2 ELSE MOVE SPACE
             TO IMPCT-H-2.
     IF PRI-CODE-2 > 0 AND PRI-CODE-2 < 5
         MOVE DIR-ENTRY (PRI-CODE-2) TO IMPCT-H-4 ELSE MOVE SPACE
         TO IMPCT-H-4. IF T10-AT-IS = 1 GO TO A-PIS.
A-PX.
     PERFORM ST-ACC-ON THRU ST-ACC-ON-X.
     IF T10-REVERSE-ST-C = 1 GO TO REV-A-D.
     IF DIR-ANL NOT = 300 GO TO X9.
     IF SEC-FEET < 101 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 7) GO TO A-P.
     IF SEC-FEET < 101 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 7) GO TO A-P.
     IF SEC-FEET < 201 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 8) GO TO A-P.
     IF SEC-FEET < 201 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 8) GO TO A-P.
     IF SEC-FEET > 200 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 9) GO TO A-P.
     IF SEC-FEET > 200 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 9) GO TO A-P.
X9.  IF T10-DIR (1) = ZERO GO TO A-P.
     IF SEC-CODE-1 < 1 OR SEC-CODE-1 > 4 GO TO A-P.
     IF SEC-FEET > 100 GO TO X9-A.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 7)    GO TO A-P.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 7)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 3)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 3)    GO TO A-P.
X9-A.
     IF SEC-FEET > 200 GO TO S2.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 8)    GO TO A-P.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 8)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 4)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 4)    GO TO A-P.
S2.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 9)    GO TO A-P.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 9)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (2 5)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (3 5)    GO TO A-P.
REV-A-D.
     IF DIR-ANL NOT = 300 GO TO REV-CONT.
     IF SEC-FEET < 101 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 7) GO TO A-P.
     IF SEC-FEET < 101 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 7) GO TO A-P.
     IF SEC-FEET < 201 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 8) GO TO A-P.
     IF SEC-FEET < 201 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 8) GO TO A-P.
     IF SEC-FEET > 200 AND (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 9) GO TO A-P.
     IF SEC-FEET > 200 AND (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 9) GO TO A-P.
REV-CONT.
     IF T10-DIR (1) = ZERO GO TO A-P.
     IF SEC-CODE-1 < 1 OR SEC-CODE-1 > 4 GO TO A-P.
     IF SEC-FEET > 100 GO TO REV-CONT-1.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 7)    GO TO A-P.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 7)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 3)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 3)    GO TO A-P.
REV-CONT-1.
     IF SEC-FEET > 200 GO TO REV-CONT-2.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 8)    GO TO A-P.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 8)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 4)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 4)    GO TO A-P.
REV-CONT-2.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 9)    GO TO A-P.
     IF T10-DIR (1) = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 9)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 1 OR SEC-CODE-1 = 2)
         ADD 1 TO T10-LINEA-FLD (4 5)    GO TO A-P.
     IF T10-DIR (1) NOT = SEC-CODE-1 AND
         (SEC-CODE-1 = 3 OR SEC-CODE-1 = 4)
         ADD 1 TO T10-LINEA-FLD (5 5)    GO TO A-P.
A-P.
     MOVE DIR-VEH-HOLD TO T10-DIR (1).
A-PIS.
     PERFORM RIGHT-DIR THRU RIGHT-DIR-X.
ACCIDENT-TYPE.
     IF T10-AT-IS = 1 GO TO SEV-CK.
     IF PRI-CODE-1 NOT = 9 GO TO SEV-CK-NEW.
     IF DIRECTION-1 > ZERO OR DIRECTION-2 > 1 GO TO CK-SE.
     IF INTERSECTION NOT = 3 AND INTERSECTION NOT = 5 GO TO CK-SE.
     GO TO IC.
SEV-CK-NEW.
     IF SEC-FEET > 0200 GO TO CK-SE.
     IF DIR-ANL = 300 GO TO CK-SE.
     IF DIR-VEH-C = ZERO GO TO CK-SE.
     IF SEC-CODE-1 < 1 OR SEC-CODE-1 > 4 GO TO CK-SE.
     IF DIR-VEH-C = SEC-CODE-1 GO TO CK-SE.
IC.
     IF T10-ACC-TYPE = 26 OR T10-ACC-TYPE = 27 OR T10-ACC-TYPE =
         28 OR T10-ACC-TYPE = 30 OR T10-ACC-TYPE = 31 OR
         T10-ACC-TYPE = 32 GO TO SEV-CK ELSE GO TO CK-SE.
SEV-CK.
     IF T10-A-SEV = 1 OR T10-A-SEV = 2 OR T10-A-SEV = 3
         OR T10-A-SEV = 4 ADD 1 TO IF-AC-SM.
     ADD 1 TO T-AC-SM.
CK-SE.
     IF T10-AT-IS = 1 GO TO ACCIDENT-TYPE-A.
     IF T10-ACC-TYPE > 25 AND T10-ACC-TYPE < 29 ADD 1 TO
         T10-LINEA-FLD (1 2) GO TO W-F-R.
     IF T10-ACC-TYPE > 29 AND T10-ACC-TYPE < 33 ADD 1 TO
         T10-LINEA-FLD (2 2) GO TO W-F-R.
     IF T10-ACC-TYPE = 29 ADD 1 TO T10-LINEA-FLD (3 2)
         GO TO W-F-R.
     IF T10-ACC-TYPE = 01 ADD 1 TO T10-LINEA-FLD (4 2)
         GO TO W-F-R.
     IF T10-ACC-TYPE = 07 ADD 1 TO T10-LINEA-FLD (5 2)
         GO TO W-F-R.
     IF T10-ACC-TYPE = 05 ADD 1 TO T10-LINEA-FLD (6 2)
         GO TO W-F-R.
     IF T10-ACC-TYPE = 03 ADD 1 TO T10-LINEA-FLD (7 2)
         GO TO W-F-R.
     ADD 1 TO T10-LINEA-FLD (8 2) GO TO W-F-R.
ACCIDENT-TYPE-A.
     IF T10-ACC-TYPE > 19 AND T10-ACC-TYPE < 23 ADD 1 TO
         T10-LINEA-FLD (1 1) GO TO W-F-R.
     IF T10-ACC-TYPE > 22 AND T10-ACC-TYPE < 26 ADD 1 TO
         T10-LINEA-FLD (2 1) GO TO W-F-R.
     IF T10-ACC-TYPE > 25 AND T10-ACC-TYPE < 29 ADD 1  TO
         T10-LINEA-FLD (3 1) GO TO W-F-R.
     IF T10-ACC-TYPE > 29 AND T10-ACC-TYPE < 33 ADD 1 TO
         T10-LINEA-FLD (4 1) GO TO W-F-R.
     IF T10-ACC-TYPE = 29 ADD 1 TO T10-LINEA-FLD (5 1)
         GO TO W-F-R.
     IF T10-ACC-TYPE = 03 ADD 1 TO T10-LINEA-FLD (6 1)
         GO TO W-F-R.
     ADD 1 TO T10-LINEA-FLD (7 1) GO TO W-F-R.
W-F-R.
     WRITE PRINT-REC AFTER ADVANCING 2 LINES.
     MOVE SPACES TO PRINT-LINE ADD 2 TO PRTY-CTR.
     IF PRI-CODE-1 NOT = 9 GO TO W-S-R.
     MOVE DIRECTION-HOLD TO IMPACT-2.
     IF INTER-SWITCH = ZERO COMPUTE INTER-PLUS-1 =
         ((DISTA + 1) * 100).
     GO TO F-2-R.
W-S-R.
     MOVE IMPACT-LINE2-H TO P-PT-IMPACT.
F-2-R.
     MOVE T10LINE2-PR-GP-H TO P-GP-CODE.
     MOVE T10LINE2-LIGHT-H TO P-LIGHT.
     IF PARTY-NO (2) = 2 NEXT SENTENCE ELSE
     WRITE PRINT-REC AFTER ADVANCING 1 LINES MOVE SPACES TO
     PRINT-LINE  ADD 1  TO PRTY-CTR GO TO T10-DET-1A.
     MOVE  T10-PARTY-ENTRY (2) TO T10-PARTY-ENTRY (1).
S6.
     PERFORM RIGHT-DIR THRU RIGHT-DIR-X.
S4.  MOVE T10-CLASS (1) TO T10-CLASS1.
     PERFORM T1.
     PERFORM T3.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES MOVE SPACES TO
         PRINT-LINE  ADD 1 TO PRTY-CTR.
S7.  IF PARTY-NO (3) NOT = 3 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (3) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
     IF PARTY-NO (4) NOT = 4 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (4) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
     IF PARTY-NO (5) NOT = 5 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (5) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
     IF PARTY-NO (6) NOT = 6 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (6) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
     IF PARTY-NO (7) NOT = 7 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (7) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
     IF PARTY-NO (8) NOT = 8 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (8) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
     IF PARTY-NO (9) NOT = 9 GO TO T10-DET-1A.
     MOVE T10-PARTY-ENTRY (9) TO T10-PARTY-ENTRY (1).
     PERFORM S6 THRU S4.
T10-DET-1A.
     MOVE ZERO TO INTER-SWITCH, AT-S.
     MOVE SPACES TO  T10-DET-P-HOLD.
     ADD PRTY-CTR TO LINE-NO.
     MOVE ZERO TO PRTY-CTR.
T10-DET-X.  EXIT.
RIGHT-DIR.
     IF T10-DIR (1) = ZERO AND T10-DATE < '690701'
         MOVE SPACE TO P-10-DIR GO TO RIGHT-DIR-X.
     IF T10-DIR (1) = ZERO OR T10-DIR (1) > 4
     MOVE 'UNK.' TO P-10-DIR GO TO RIGHT-DIR-X.
     IF T10-DIR (1) = 1 MOVE 'NB- ' TO P-10-DIR.
     IF T10-DIR (1) = 2 MOVE 'EB- ' TO P-10-DIR.
     IF T10-DIR (1) = 3 MOVE 'SB- ' TO P-10-DIR.
     IF T10-DIR (1) = 4 MOVE 'WB- ' TO P-10-DIR.






     IF T10-AT-IS = 1 GO TO RIGHT-DIR-IS.
     IF PRI-CODE-1 = PRI-CODE-2 AND SEC-CODE-1 = SEC-CODE-2
         NEXT SENTENCE ELSE GO TO RIGHT-DIR-ST.
     IF LOCAT-CODE LESS THAN 011 NEXT SENTENCE ELSE
     MOVE SPACE TO P-10-SP GO TO RIGHT-DIR-X.
RIGHT-DIR-ST.
     IF T10-REVERSE-ST-C = 1 MOVE 2 TO P-10-NO ELSE
     MOVE 1 TO P-10-NO.
     GO TO RIGHT-DIR-X.
RIGHT-DIR-IS.
     IF T10-REVERSE-ST-C = 1 GO TO RIGHT-DIR-IS-REV.
     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND
     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 1 TO P-10-NO.
     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND
     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 2 TO P-10-NO.
     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND
     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 2 TO P-10-NO.
     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND
     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 1 TO P-10-NO.
     GO TO RIGHT-DIR-X.
RIGHT-DIR-IS-REV.
     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND
     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 2 TO P-10-NO.
     IF (T10-DIR (1) = 1 OR T10-DIR (1) = 3) AND
     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 1 TO P-10-NO.
     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND
     (PRI-CODE-1 = 2 OR PRI-CODE-1 = 4) MOVE 1 TO P-10-NO.
     IF (T10-DIR (1) = 2 OR T10-DIR (1) = 4) AND
     (PRI-CODE-1 = 1 OR PRI-CODE-1 = 3) MOVE 2 TO P-10-NO.
RIGHT-DIR-X.   EXIT.
HEADER.
     MOVE ZERO TO LINE-NO.
     IF HDR-CTR = 19 MOVE 21 TO H-NO.          IF HDR-CTR = 23
     MOVE 22 TO H-NO ELSE MOVE HDR-CTR TO H-NO. MOVE PAGE-NO TO HP
     PERFORM ANT-LOOKUP VARYING ANT FROM 1 BY 1 UNTIL ANT > 20 OR
                     NT-CITY (ANT) = T10-REQ-HOLD-3.
     IF ANT > 20 MOVE SPACES TO ANT-HDR       ELSE
        MOVE NT-NAME (ANT) TO ANT-HDR.
            WRITE PRINT-REC FROM HDR AFTER ADVANCING   TOP-OF-PAGE.
     IF HDR-CTR = 10 PERFORM H-10 THRU H-10-X.
HEADER-X.  EXIT.
ANT-LOOKUP.  EXIT.
SUM-10.
     MOVE 0 TO 10-SW.
     PERFORM HEADER THRU HEADER-X.
     COMPUTE T10-LINEA-FLD (8 1) = T10-LINEA-FLD (1 1) +
       T10-LINEA-FLD (2 1) + T10-LINEA-FLD (3 1) +
       T10-LINEA-FLD (4 1) + T10-LINEA-FLD (5 1) +
       T10-LINEA-FLD (6 1) + T10-LINEA-FLD (7 1).
     COMPUTE T10-LINEA-FLD (9 2) = T10-LINEA-FLD (1 2) +
       T10-LINEA-FLD (2 2) + T10-LINEA-FLD (3 2) +
       T10-LINEA-FLD (4 2) + T10-LINEA-FLD (5 2) +
       T10-LINEA-FLD (6 2) + T10-LINEA-FLD (7 2) +
       T10-LINEA-FLD (8 2).
     COMPUTE T10-LINEA-FLD (6 3) = T10-LINEA-FLD (2 3) +
       T10-LINEA-FLD (3 3) + T10-LINEA-FLD (4 3) +
       T10-LINEA-FLD (5 3).
     COMPUTE T10-LINEA-FLD (6 4) = T10-LINEA-FLD (2 4) +
       T10-LINEA-FLD (3 4) + T10-LINEA-FLD (4 4) +
       T10-LINEA-FLD (5 4).
     COMPUTE T10-LINEA-FLD (6 5) = T10-LINEA-FLD (2 5) +
       T10-LINEA-FLD (3 5) + T10-LINEA-FLD (4 5) +
       T10-LINEA-FLD (5 5).
     COMPUTE T10-LINEA-FLD (2 6) = T10-LINEA-FLD (2 3) +
       T10-LINEA-FLD (2 4) + T10-LINEA-FLD (2 5).
     COMPUTE T10-LINEA-FLD (3 6) = T10-LINEA-FLD (3 3) +
       T10-LINEA-FLD (3 4) + T10-LINEA-FLD (3 5).
     COMPUTE T10-LINEA-FLD (4 6) = T10-LINEA-FLD (4 3) +
       T10-LINEA-FLD (4 4) + T10-LINEA-FLD (4 5).
     COMPUTE T10-LINEA-FLD (5 6) = T10-LINEA-FLD (5 3) +
       T10-LINEA-FLD (5 4) + T10-LINEA-FLD (5 5).
     COMPUTE T10-LINEA-FLD (6 6) = T10-LINEA-FLD (6 3) +
       T10-LINEA-FLD (6 4) + T10-LINEA-FLD (6 5).
     COMPUTE T10-LINEA-FLD (6 7) = T10-LINEA-FLD (2 7) +
       T10-LINEA-FLD (3 7) + T10-LINEA-FLD (4 7) +
       T10-LINEA-FLD (5 7).
     COMPUTE T10-LINEA-FLD (6 8) = T10-LINEA-FLD (2 8) +
       T10-LINEA-FLD (3 8) + T10-LINEA-FLD (4 8) +
       T10-LINEA-FLD (5 8).
     COMPUTE T10-LINEA-FLD (6 9) = T10-LINEA-FLD (2 9) +
       T10-LINEA-FLD (3 9) + T10-LINEA-FLD (4 9) +
       T10-LINEA-FLD (5 9).
     COMPUTE T10-LINEA-FLD (2 10) = T10-LINEA-FLD (2 7) +
       T10-LINEA-FLD (2 8) + T10-LINEA-FLD (2 9).
     COMPUTE T10-LINEA-FLD (3 10) = T10-LINEA-FLD (3 7) +
       T10-LINEA-FLD (3 8) + T10-LINEA-FLD (3 9).
     COMPUTE T10-LINEA-FLD (4 10) = T10-LINEA-FLD (4 7) +
       T10-LINEA-FLD (4 8) + T10-LINEA-FLD (4 9).
     COMPUTE T10-LINEA-FLD (5 10) = T10-LINEA-FLD (5 7) +
       T10-LINEA-FLD (5 8) + T10-LINEA-FLD (5 9).
     COMPUTE T10-LINEA-FLD (6 10) = T10-LINEA-FLD (6 7) +
       T10-LINEA-FLD (6 8) + T10-LINEA-FLD (6 9).
     PERFORM SUM-10-LINEA THRU SUM-10-LINEA-X VARYING
         G FROM 1 BY 1 UNTIL G GREATER THAN 9.

     MOVE '4. ACCIDENT SEVERITY:             5. LIGHTING CONDITION
-'S:                        6. TRAFFIC VOLUME:' TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 2 LINES.
     MOVE '     DESCRIPTION ACCIDENTS PERSONS     DESCRIPTION    A
-'CCIDENTS                       STREET  LOC  DIR VOLUME    DATE'
          TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     PERFORM TRA-VOL THRU TRA-VOL-X.
     PERFORM SUM-10-LINEB THRU SUM-10-LINEB-X VARYING G FROM 1
         BY 1 UNTIL G GREATER THAN 7.
     MOVE '
-    '                 NOTE: * = (6 HR TOTAL X EXPANSION FACTOR)'
         TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     MOVE '
-    '                       **= (P.M. PEAK HR TOTAL X 10)'
         TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     MOVE '7. ACCIDENT SUMMARY:
-         '                         8. INTERSECTION DESCRIPTION:'
          TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 2 LINES.
     MOVE '                           STRATIFICATION DATA
-         '                         GENERAL' TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     PERFORM STRAF-RD THRU STRAF-RD-X.
     PERFORM SUM-10-LINEC THRU SUM-10-LINEC-X.
     MOVE '   B. ACCIDENT RATE PER
-         '                         APPROACH CHARACTERISTICS:'
          TO PRINT-LINE.
     MOVE '   ** L E G **   ' TO ITEM-8-CON-1.
     WRITE PRINT-REC AFTER ADVANCING 2 LINES.
     MOVE '      MILLION VEHICLES: ' TO PRINT-LINE.
     MOVE 'N    S    E    W' TO ITEM-8-CON-1.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     PERFORM SUM-10-LINEC-1 THRU SUM-10-LINEC-1-X VARYING
         H FROM 1 BY 1 UNTIL H GREATER THAN 14.
     PERFORM INIT THRU INIT-X.
SUM-10-X.  EXIT.
INIT.
     MOVE ZEROES TO T10-SUM-LINEA-WORK T10-SUM-LINEB-WORK
         T10-SUM-LINEC-WORK.
     PERFORM INIT-1X THRU INIT-1X-X VARYING G FROM 1 BY 1
         UNTIL G GREATER THAN 7.
     MOVE ZEROES TO STR-SUM.
INIT-X.  EXIT.
INIT-1X.
     MOVE SPACES TO T10-V1 (G), T10-V2 (G), T10-V4-M (G),
         T10-V4-D (G), T10-V4-Y (G), T10-V3A (G).
     MOVE ZEROES TO T10-V3 (G).
INIT-1X-X.  EXIT.
SUM-10-LINEA.  MOVE SPACES TO PRINT-LINE.
     MOVE T10-LINEA-FLD (G 1) TO ITEM-1-TOT (1).
     MOVE T10-LINEA-FLD (G 2) TO ITEM-1-TOT (2).
     MOVE T10-LINEA-FLD (G 3) TO ITEM-3-A-1.
     MOVE T10-LINEA-FLD (G 4) TO ITEM-3-A-2.
     MOVE T10-LINEA-FLD (G 5) TO ITEM-3-A-O.
     MOVE T10-LINEA-FLD (G 6) TO ITEM-3-A-T.
     MOVE T10-LINEA-FLD (G 7) TO ITEM-3-D-1.
     MOVE T10-LINEA-FLD (G 8) TO ITEM-3-D-2.
     MOVE T10-LINEA-FLD (G 9) TO ITEM-3-D-O.
     MOVE T10-LINEA-FLD (G 10) TO ITEM-3-D-T.
     IF G = 1 MOVE 'RIGHT ANGLE ' TO ITEM-1-CON (1)
              MOVE 'REAR END    ' TO ITEM-1-CON (2)
              MOVE '100 200 OVER TOTAL     100 200 OVER TOTAL'
                 TO ITEM-3-C1.
     IF G = 2 MOVE 'LEFT TURN   ' TO ITEM-1-CON (1)
              MOVE 'SIDE SWIPE  ' TO ITEM-1-CON (2)
              MOVE '1ST - N/E'    TO ITEM-3-CON.
     IF G = 3 MOVE 'REAR END    ' TO ITEM-1-CON (1)
              MOVE 'HEAD ON     ' TO ITEM-1-CON (2)
              MOVE '1ST - S/W'    TO ITEM-3-CON.
     IF G = 4 MOVE 'SIDE SWIPE  ' TO ITEM-1-CON (1)
              MOVE 'RAN OFF ROAD' TO ITEM-1-CON (2)
              MOVE '2ND - N/E'    TO ITEM-3-CON.
     IF G = 5 MOVE 'HEAD ON     ' TO ITEM-1-CON (1)
              MOVE 'FIXED OBJECT' TO ITEM-1-CON (2)
              MOVE '2ND - S/W'    TO ITEM-3-CON.
     IF G = 6 MOVE 'PEDESTRIAN  ' TO ITEM-1-CON (1)
              MOVE 'PARKED VEH. ' TO ITEM-1-CON (2)
              MOVE '  TOTAL  '    TO ITEM-3-CON.
     IF G = 7 MOVE 'MISC.       ' TO ITEM-1-CON (1)
              MOVE 'PEDESTRIAN  ' TO ITEM-1-CON (2).
     IF G = 8 MOVE '   TOTAL    ' TO ITEM-1-CON (1)
              MOVE 'MISC.       ' TO ITEM-1-CON (2).
     IF G = 9 MOVE '   TOTAL    ' TO ITEM-1-CON (2).
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
SUM-10-LINEA-X.  EXIT.
SUM-10-LINEB.
     MOVE SPACES TO PRINT-LINE.
     MOVE T10-LINEB-FLD (G 1) TO ITEM-4-ACC.
     MOVE T10-LINEB-FLD (G 2) TO ITEM-4-PER.
     MOVE T10-LINEB-FLD (G 3) TO ITEM-5-ACC.
     MOVE T10-V1 (G) TO ITEM-6-LOC.
     MOVE T10-V2 (G) TO ITEM-6-DIR.
     MOVE T10-V3 (G) TO ITEM-6-VOL.
     MOVE T10-V3A (G) TO ITEM-6-C.
     MOVE T10-V4-M (G) TO ITEM-6-MO.
     MOVE T10-V4-D (G) TO ITEM-6-DA.
     MOVE T10-V4-Y (G) TO ITEM-6-YR.
     IF G = 1 MOVE 'FATALITY    ' TO ITEM-4-CON
              MOVE 'DAY            ' TO ITEM-5-CON
              MOVE '1ST   ' TO ITEM-6-CON.
     IF G = 2 MOVE 'INJURY      ' TO ITEM-4-CON
              MOVE 'DUSK-DAWN      ' TO ITEM-5-CON
              MOVE '1ST   ' TO ITEM-6-CON.
     IF G = 3 MOVE '  A         ' TO ITEM-4-CON
              MOVE 'DARK           ' TO ITEM-5-CON.
     IF G = 4 MOVE '  B         ' TO ITEM-4-CON
              MOVE 'DAY-DARK-CLOUDY' TO ITEM-5-CON
              MOVE '2ND   ' TO ITEM-6-CON.
     IF G = 5 MOVE '  C         ' TO ITEM-4-CON
              MOVE '   TOTAL       ' TO ITEM-5-CON
              MOVE '2ND   ' TO ITEM-6-CON.
     IF G = 6 MOVE 'PROPERTY    ' TO ITEM-4-CON.
     IF G = 7 MOVE '   TOTAL    ' TO ITEM-4-CON.
     IF G = 1 AND T10-V4-M (1) NOT = SPACE
         MOVE '-' TO ITEM-6-C1, ITEM-6-C2.
     IF G = 4 AND T10-V4-M (4) NOT = SPACE
         MOVE '-' TO ITEM-6-C1, ITEM-6-C2.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
SUM-10-LINEB-X.  EXIT.
W-SPACE-1.
     MOVE ZERO TO ITEM-7-VOL.
     ADD 1 TO G.
     IF G = 1 PERFORM 7A GO TO W-SPACE-2.
     IF G = 2 PERFORM 7B GO TO W-SPACE-2.
     IF G = 3 PERFORM 7C GO TO W-SPACE-2.
     IF G = 4 PERFORM 7D PERFORM 7D-A GO TO W-SPACE-2.
     IF G = 5 PERFORM 7E
     MOVE T-AC-SM TO ITEM-7-HO
     SUBTRACT LINEC-MEAN (2) FROM ITEM-7-HO  GIVING ITEM-7-DIFF
     MOVE LINEC-MEAN (2) TO ITEM-7-MEAN
     MOVE LINEC-DEV (2) TO ITEM-7-DEV
     MOVE LINEC-CVA (2) TO ITEM-7-C-VA
         MOVE T-AC-SM  TO ITEM-7-A-VA GO TO W-SPACE-2.
W-SPACE-2.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     MOVE SPACES TO PRINT-LINE.
W-SPACE-1-X.  EXIT.
7A.  MOVE '     DESCRIPTION                STANDARD  CRITICAL ACTU
-'AL DIFFERENCE ' TO ITEM-7-H.
7B.  MOVE '                           MEAN DEVIATION VALUE    VALU
-'E     (ACTUAL VS MEAN)' TO ITEM-7-H.
7C.  MOVE '   A. NO. OF ACCIDENTS: ' TO ITEM-7-CON.
7D.  MOVE '      1. INJ + FAT      ' TO ITEM-7-CON.
7E.  MOVE '      2. ALL ACCIDENTS  ' TO ITEM-7-CON.
7D-A.
     MOVE IF-AC-SM TO ITEM-7-HOLD.
             MOVE ITEM-7-HOLD TO ITEM-7-A-VA.
     SUBTRACT LINEC-MEAN (1) FROM ITEM-7-HOLD GIVING ITEM-7-DIFF.
     MOVE LINEC-MEAN (1) TO ITEM-7-MEAN.
     MOVE LINEC-DEV (1) TO ITEM-7-DEV.
     MOVE LINEC-CVA (1) TO ITEM-7-C-VA.
7D-B.
     IF IF-AC-SM = ZERO GO TO 7D-B-X.
     IF T10-V3 (1) = ZERO AND T10-V3 (2) = ZERO GO TO 7D-B-X.
     IF T10-V3 (4) = ZERO AND T10-V3 (5) = ZERO GO TO 7D-B-X.
     ADD T10-V3 (1) T10-V3 (2) T10-V3 (4) T10-V3 (5) GIVING
         ITEM-7-VOL.
     IF ITEM-7-VOL = ZERO GO TO 7D-B-X.
     IF TND = ZERO GO TO 7D-B-X.
     COMPUTE ITEM-7-HOO ROUNDED =
         (IF-AC-SM / ((TND * ITEM-7-VOL) / 1000000)).
     MOVE ITEM-7-HOO TO ITEM-7-A-VAM.
     SUBTRACT LINEC-MEAN (3) FROM ITEM-7-HOO GIVING ITEM-7-DIFF.
     MOVE LINEC-MEAN (3) TO ITEM-7-MEAN.
     MOVE LINEC-DEV (3) TO ITEM-7-DEV.
     MOVE LINEC-CVA (3) TO ITEM-7-C-VA.
7D-B-X.  EXIT.
7D-C.
     IF T-AC-SM = ZERO GO TO 7D-C-X.
     IF ITEM-7-VOL = ZERO GO TO 7D-C-X.
     IF TND = ZERO GO TO 7D-C-X.
     COMPUTE ITEM-7-HOO ROUNDED =
         (T-AC-SM / ((TND * ITEM-7-VOL) / 1000000)).
     MOVE ITEM-7-HOO TO ITEM-7-A-VAM.
     SUBTRACT LINEC-MEAN (4) FROM ITEM-7-HOO GIVING ITEM-7-DIFF.
     MOVE LINEC-MEAN (4) TO ITEM-7-MEAN.
     MOVE LINEC-DEV (4) TO ITEM-7-DEV.
     MOVE LINEC-CVA (4) TO ITEM-7-C-VA.
7D-C-X.  EXIT.
SUM-10-LINEC-1.
     MOVE SPACES TO PRINT-LINE.
     IF H = 1 PERFORM 7D MOVE ZERO TO ITEM-7-VOL
         PERFORM 7D-B THRU 7D-B-X.
     IF H = 2 PERFORM 7E PERFORM 7D-C THRU 7D-C-X.
     MOVE ITEM-8-A-ENTRY (H) TO ITEM-8-CON.
     MOVE T10-S-LINEC3 (1 H) TO ITEM-8-N.
     MOVE T10-S-LINEC3 (2 H) TO ITEM-8-E.
     MOVE T10-S-LINEC3 (3 H) TO ITEM-8-S.
     MOVE T10-S-LINEC3 (4 H) TO ITEM-8-W.
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
SUM-10-LINEC-1-X.  EXIT.
SUM-10-LINEC.
     MOVE SPACES TO PRINT-LINE. MOVE ZERO TO G.
     IF T10-SUM-LINEC1 (1) NOT EQUAL TO ZERO
         MOVE T10-SUM-LINEC1 (1) TO S ADD 1 TO T
         MOVE ITEM-8-C-ENTRY (T) TO ITEM-8-CON-1.
         MOVE 'CLASS CODE:' TO ITEM-8-CON.
         PERFORM W-SPACE-1 THRU W-SPACE-1-X.
S1.  IF T10-SUM-LINEC1 (2) > ZERO
     MOVE T10-SUM-LINEC1 (2) TO S
         MOVE ITEM-8-L-ENTRY (T) TO ITEM-8-CON-1.
         MOVE 'NUMBER OF LEGS:'      TO ITEM-8-CON.
         PERFORM W-SPACE-1 THRU W-SPACE-1-X.
     IF T10-SUM-LINEC1 (3) EQUAL TO ZERO OR T10-SUM-LINEC1 (3) >
         '04' GO TO LINEC-A.
         MOVE T10-SUM-LINEC1 (3) TO S ADD 1 TO T
         MOVE ITEM-8-I-ENTRY (T) TO ITEM-8-CON-1.
LINEC-A.
         MOVE 'TYPE OF I/S CONTROL:' TO ITEM-8-CON.
         PERFORM W-SPACE-1 THRU W-SPACE-1-X.
     IF T10-SUM-LINEC1 (4) EQUAL TO ZERO OR T10-SUM-LINEC1 (4) >
         '04' GO TO LINEC-B.
         MOVE T10-SUM-LINEC1 (4) TO S ADD 1 TO T
         MOVE ITEM-8-S-ENTRY (T) TO ITEM-8-CON-1.
LINEC-B.
     MOVE 'TYPE OF SIGNAL CONTROL:   ' TO ITEM-8-CON.
         PERFORM W-SPACE-1 THRU W-SPACE-1-X.
     IF T10-SUM-LINEC1 (5) NOT EQUAL TO ZERO
         MOVE T10-SUM-LINEC1 (5) TO ITEM-8-CON-1.
     MOVE 'SIGNAL OP CHARACTERISTICS:' TO ITEM-8-CON.
         PERFORM W-SPACE-1 THRU W-SPACE-1-X.
SUM-10-LINEC-X.  EXIT.
TRA-VOL.
     MOVE 0 TO SW-0-INDEX.
     MOVE 9 TO REV-SWITCH.
     MOVE SPACES TO ST-V-H (1) ST-V-H (2).
     MOVE ZEROES TO ST-V-V (1)  ST-V-V (2).
     MOVE T10-IS-HOLD TO VOL-SYM-ST-CODE.
     MOVE ZEROES TO VOL-SYM-INDEX.
     IF VOL-DELETE-CODE = HIGH-VALUE GO TO TRA-VOL-X.
         MOVE VOL-LATEST-N TO VOL-LATEST-HOLD.
     MOVE VOL-OLDEST-N TO VOL-OLDEST-HOLD.
     IF  VOL-OLDEST-HOLD = 0
     AND VOL-LATEST-HOLD = 0
         MOVE 1 TO SW-0-INDEX.
     IF VOL-D-O-W = '6' OR VOL-D-O-W = '7' GO TO TRA-VOL-D.
     IF DATE-2YRS = ZERO NEXT SENTENCE ELSE
         IF VOL-DATE < DATE-2YRS GO TO TRA-VOL-D.
T5.  IF VOL-LOC-3 = '5' MOVE 'ND'  TO ST-V-DIR (1)
     MOVE VOL-TOTAL-TOT-N TO ST-V-V (1).
     IF VOL-LOC-3 = '1' MOVE  'NB' TO ST-V-DIR (1)
               MOVE VOL-TOTAL-NE-N TO ST-V-V (1).
     IF VOL-LOC-3 = '2' MOVE  'EB' TO ST-V-DIR (1)
               MOVE VOL-TOTAL-NE-N TO ST-V-V (1).
     IF VOL-LOC-3 = '3' MOVE  'SB' TO ST-V-DIR (1)
               MOVE VOL-TOTAL-SW-N TO ST-V-V (1).
     IF VOL-LOC-3 = '4' MOVE  'WB' TO ST-V-DIR (1)
               MOVE VOL-TOTAL-SW-N TO ST-V-V (1).
     IF VOL-LOC-1 = '1' MOVE 'N/O' TO ST-V-LOC (1).
     IF VOL-LOC-1 = '2' MOVE 'E/O' TO ST-V-LOC (1).
     IF VOL-LOC-1 = '3' MOVE 'S/O' TO ST-V-LOC (1).
     IF VOL-LOC-1 = '4' MOVE 'W/O' TO ST-V-LOC (1).
     IF VOL-LOC-1 = '5' MOVE 'AT ' TO ST-V-LOC (1).
     IF VOL-LOC-1 = '6' MOVE 'AT ' TO ST-V-LOC (1).
     IF VOL-LOC-3 NOT = ZERO GO TO TRA-VOL-A.
     IF VOL-LOC-1 = '1' MOVE 'NB' TO ST-V-DIR (1)
         MOVE 'SB' TO ST-V-DIR (2)
         MOVE 'N/O' TO ST-V-LOC (1), ST-V-LOC (2).
     IF VOL-LOC-1 = '2' MOVE 'EB' TO ST-V-DIR (1)
         MOVE 'WB' TO ST-V-DIR (2)
         MOVE 'E/O' TO ST-V-LOC (1), ST-V-LOC (2).
     IF VOL-LOC-1 = '3' MOVE 'NB' TO ST-V-DIR (1)
         MOVE 'SB' TO ST-V-DIR (2)
         MOVE 'S/O' TO ST-V-LOC (1), ST-V-LOC (2).
     IF VOL-LOC-1 = '4' MOVE 'EB' TO ST-V-DIR (1)
         MOVE 'WB' TO ST-V-DIR (2)
         MOVE 'W/O' TO ST-V-LOC (1), ST-V-LOC (2).
     IF VOL-LOC-1 = '5' MOVE 'NB' TO ST-V-DIR (1)
         MOVE 'SB' TO ST-V-DIR (2)
         MOVE 'AT ' TO ST-V-LOC (1), ST-V-LOC (2).
     IF VOL-LOC-1 = '6' MOVE 'EB' TO ST-V-DIR (1)
         MOVE 'WB' TO ST-V-DIR (2)
         MOVE 'AT ' TO ST-V-LOC (1), ST-V-LOC (2).
               MOVE VOL-TOTAL-NE-N TO ST-V-V (1).
               MOVE VOL-TOTAL-SW-N TO ST-V-V (2).
TRA-VOL-A.
     IF VOL-REC-CODE = 1 GO TO TRA-VOL-B.
     IF ST-V-V (1) = ZERO GO TO TVA
                       ELSE MULTIPLY ST-V-V (1) BY VOL-ADJ-FAC
     GIVING ST-V-V (1) ROUNDED MOVE '*' TO ST-V-SIGN (1).
     IF ST-V-V (2) = ZERO GO TO TVA ELSE
         MULTIPLY ST-V-V (2) BY VOL-ADJ-FAC
     GIVING ST-V-V (2) ROUNDED MOVE '*' TO ST-V-SIGN (2).
TRA-VOL-B.
     IF VOL-REV-CODE = '1' MOVE 1 TO REV-SWITCH GO TO TRA-VOL-C.
     MOVE ZERO TO REV-SWITCH.
     MOVE ST-V-LOC (1) TO T10-V1 (1).
     MOVE ST-V-LOC (2) TO T10-V1 (2).
     MOVE ST-V-DIR (1) TO T10-V2  (1).
     MOVE ST-V-DIR (2) TO T10-V2  (2).
     MOVE ST-V-V   (1) TO T10-V3  (1).
     MOVE ST-V-V   (2) TO T10-V3  (2).
     MOVE ST-V-SIGN (1) TO T10-V3A (1).
     MOVE ST-V-SIGN (2) TO T10-V3A (2).
     MOVE VOL-YR  TO  T10-V4-Y (1).
     MOVE VOL-MO  TO  T10-V4-M (1).
     MOVE VOL-DA  TO  T10-V4-D (1).
     GO TO TRA-VOL-D.
TVA.   COMPUTE ST-V-V (1) ROUNDED =
         ((V-NE-VOL (2) + V-SW-VOL (2)) * 10) / 2.
       MOVE ST-V-V (1) TO ST-V-V (2).
       MOVE '**' TO ST-V-SIGN (1) ST-V-SIGN (2).
     GO TO TRA-VOL-B.
TRA-VOL-C.
     MOVE ST-V-LOC (1) TO T10-V1 (4).
     MOVE ST-V-LOC (2) TO T10-V1 (5).
     MOVE ST-V-DIR (1) TO   T10-V2  (4).
     MOVE ST-V-DIR (2) TO   T10-V2  (5).
     MOVE ST-V-V   (1) TO   T10-V3  (4).
     MOVE ST-V-V   (2) TO   T10-V3  (5).
     MOVE ST-V-SIGN (1) TO  T10-V3A (4).
     MOVE ST-V-SIGN (2) TO  T10-V3A (5).
     MOVE VOL-YR  TO  T10-V4-Y (4).
     MOVE VOL-MO  TO  T10-V4-M (4).
     MOVE VOL-DA  TO  T10-V4-D (4).
TRA-VOL-D.
     IF SW-0-INDEX = 1 GO TO TRA-VOL-E.
     IF VOL-LATEST-HOLD = VOL-SYM-INDEX-N MOVE ZERO
         TO VOL-LATEST-HOLD, VOL-OLDEST-HOLD, REV-SWITCH,
     MOVE SPACES TO ST-VOL-HOLD
     MOVE ZEROES TO ST-V-V (1)  ST-V-V (2) GO TO TRA-VOL-X.
TRA-VOL-E.
     MOVE 0 TO SW-0-INDEX.
     MOVE SPACES TO ST-VOL-HOLD
     MOVE ZEROES TO ST-V-V (1) ST-V-V (2) MOVE VOL-LATEST-HOLD TO
         VOL-SYM-INDEX-N.
TC.  IF VOL-DELETE-CODE = HIGH-VALUE GO TO TCA.
     IF VOL-D-O-W = '6' OR VOL-D-O-W = '7' GO TO TCA.
     IF DATE-2YRS = ZERO NEXT SENTENCE ELSE
         IF VOL-DATE < DATE-2YRS GO TO TCA.
     IF VOL-REV-CODE = REV-SWITCH GO TO TCA.
     IF REV-SWITCH = 9 GO TO T5.
     MOVE ZERO TO VOL-LATEST-HOLD, VOL-SYM-INDEX-N GO TO T5.
TCA.
     IF VOL-LATEST-HOLD = VOL-OLDEST-HOLD MOVE VOL-LATEST-HOLD
         TO VOL-SYM-INDEX-N GO TO TRA-VOL-D.
     SUBTRACT 1 FROM VOL-LATEST-HOLD GIVING VOL-SYM-INDEX-N.
     MOVE VOL-SYM-INDEX-N TO VOL-LATEST-HOLD.
     IF VOL-SYM-INDEX-N = ZERO MOVE ZERO TO VOL-LATEST-HOLD
         GO TO TRA-VOL-D  ELSE GO TO TC.
TRA-VOL-ERROR.
     DISPLAY  VOLFILE-REC.
     DISPLAY  VOL-SYM-KEY.
     GO TO TCA.
TRA-VOL-X.  EXIT.
H1.  MOVE S-FR-MO TO HDR5-MO-FR MOVE S-FR-DA TO HDR5-DA-FR.
     MOVE S-FR-YR TO HDR5-YR-FR MOVE S-TO-MO TO HDR5-MO-TO.
     MOVE S-TO-DA TO HDR5-DA-TO MOVE S-TO-YR TO HDR5-YR-TO.
     MOVE MO TO HDR5-R-MO MOVE DA TO HDR5-R-DA.
     MOVE YR TO HDR5-R-YR.
     WRITE PRINT-REC FROM HDR5 AFTER ADVANCING 1 LINES.
H-10.
     IF SW10A = 1 MOVE '        TRAFFIC  ACCIDENT  REPORT       '
         TO HDR-2-3-TITLE ELSE MOVE '     ACCIDENT LOCATION ANALYS
-'IS REPORT  ' TO HDR-2-3-TITLE.
     WRITE PRINT-REC FROM HDR2-3 AFTER ADVANCING 2 LINES.
     IF SW10A = 2 GO TO H-10-A.
     MOVE T10-IS-CODE TO SYM-KEY.
     READ ISFILE INVALID KEY MOVE 'INVALID INTERSECTION CODE' TO
         HDR4-N1 MOVE SPACES TO HDR4-N2 GO TO V1.
     MOVE IS-NAME-1 TO HDR4-N1  MOVE IS-NAME-2 TO HDR4-N2.
     MOVE IS-APPROACH-CHARS TO T10-SUM-LINEC2.
     MOVE IS-CLASS-CODE TO T10-SUM-LINEC1 (1).
     MOVE IS-NO-LEGS TO T10-SUM-LINEC1 (2).
     MOVE IS-CONTROL TO T10-SUM-LINEC1 (3).
     MOVE IS-SIGNAL-CTRL TO T10-SUM-LINEC1 (4).
     MOVE IS-SIGNAL-CHAR TO T10-SUM-LINEC1 (5).
     MOVE IS-STRAF TO ST-SYM-KEY.
V1.  MOVE T10-REQUESTER TO HDR4-REQ.
     MOVE T10-IS-1 TO HDR4-ST1 MOVE T10-IS-2 TO HDR4-ST2.
     WRITE PRINT-REC FROM HDR4 AFTER ADVANCING 2 LINES.
     MOVE T10-SEARCH-DATES TO T12-SEARCH-DATES.
     PERFORM H1.
     MOVE SPACES TO PRINT-LINE.
     MOVE ' DIV.                       SEVERITY             ******
-'*** ACCIDENT DIAGRAM ********  DIR  ' TO PRINT-LINE.
     MOVE '                                  A      ' TO ITEM-3-C1
     WRITE PRINT-REC AFTER ADVANCING 3 LINES.
     MOVE 'RECORD       ACCIDENT     INJURY  FAT   VEHICLE    ACC
-         '  VEH/PED      POINT OF        ANL C' TO PRINT-LINE.
     MOVE 'ONTRIB.       DMV-CAUSE           G      ' TO ITEM-3-C1
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     MOVE 'NUMBER     DATE  DAY TIME  A  B  C     TYPE C CND  TYPE
-' ACTIONS  I/S  IMPACT   DIR  CODE  ' TO PRINT-LINE.
     MOVE 'CIRCUM      CODE   GROUP  W LIGHT E RC SC' TO ITEM-3-C1
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
     MOVE SPACES TO PRINT-LINE  GO TO H-10-X.
H-10-A.
     WRITE PRINT-REC FROM HDR4 AFTER ADVANCING 2 LINES.
     WRITE PRINT-REC FROM HDR5 AFTER ADVANCING 1 LINES.
     MOVE '                              *   *   *   *   I N T E R
-'S E C T I O N   S U M M A R Y   *   *   *   *'
         TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 3 LINES.
     MOVE '1. INTERSECTION ACCIDENT TYPES:   2. NON-INTERSECTION A
-'CCIDENT TYPES:   3. NON-INTERSECTION ACCIDENT LOCATIONS:'
              TO PRINT-LINE.
     WRITE PRINT-REC AFTER ADVANCING 2 LINES.
     MOVE '     DESCRIPTION   ACCIDENTS           DESCRIPTION
-         '                      STREET  LOC   ' TO PRINT-LINE.
     MOVE '  A P P R O A C H       D E P A R T U R E' TO ITEM-3-C1
     WRITE PRINT-REC AFTER ADVANCING 1 LINES.
H-10-X.  EXIT.
STRAF-RD.
     IF ST-DELETE-CODE = HIGH-VALUE GO TO STRAF-RD-X.
     MOVE ST-M-1 TO LINEC-MEAN (2).
     MOVE ST-SD-1 TO LINEC-DEV (2).
     MOVE ST-CV-1 TO LINEC-CVA (2).
     MOVE ST-M-2 TO LINEC-MEAN (1).
     MOVE ST-SD-2 TO LINEC-DEV (1).
     MOVE ST-CV-2 TO LINEC-CVA (1).
     MOVE ST-M-3 TO LINEC-MEAN (4).
     MOVE ST-SD-3 TO LINEC-DEV (4).
     MOVE ST-CV-3 TO LINEC-CVA (4).
     MOVE ST-M-4 TO LINEC-MEAN (3).
     MOVE ST-SD-4 TO LINEC-DEV (3).
     MOVE ST-CV-4 TO LINEC-CVA (3).
STRAF-RD-X.  EXIT.
NO-D-RTN.
     MOVE C-M1 TO MOS-2. MOVE C-D1 TO DAS-2.
     MOVE C-Y1 TO YRS-2.
     IF T-D-HOLD = ZERO  MOVE MO TO MOS-2
         MOVE DA TO DAS-2  MOVE YR TO YRS-2.
     SUBTRACT 4 FROM YRS-2.
     IF T-D-HOLD = ZEROES MOVE 2190 TO TND GO TO NO-D-RTN-X.
     IF C-M1 = 01 MOVE C-D1 TO TO-D-TOT GO TO NO-D-1.
     SUBTRACT 1 FROM C-M1 GIVING MO-CT.
     MOVE MO-ENTRY (MO-CT) TO TO-D-TOT.
     ADD C-D1 TO TO-D-TOT.
NO-D-1.
     IF C-M = 01 MOVE C-D TO FR-D-TOT GO TO NO-D-2.
     SUBTRACT 1 FROM C-M GIVING MO-CT.
     MOVE MO-ENTRY (MO-CT) TO FR-D-TOT.
     ADD C-D TO FR-D-TOT.
NO-D-2.
     COMPUTE YR-D-TOT =  (365 * (C-Y1 - C-Y)).
     IF FR-D-TOT > TO-D-TOT SUBTRACT 365 FROM YR-D-TOT,
     COMPUTE TND = (((365 + TO-D-TOT) - FR-D-TOT) + YR-D-TOT)
     GO TO NO-D-RTN-X.
     COMPUTE TND = ((TO-D-TOT - FR-D-TOT) + YR-D-TOT).
NO-D-RTN-X.  EXIT.
ST-ACC-ON.
     MOVE T10-DIR (1) TO DIR-VEH-HOLD.
     MOVE ZERO TO DIR-ST-SW.
     MOVE T10-DIR (1) TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (2) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (3) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (4) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (5) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (6) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (7) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (8) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE T10-DIR (9) TO DIR-VEH-C, T10-DIR (1).
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO  ST-ACC-ON-X.
     MOVE ZERO TO T10-DIR (1), DIR-VEH-C.
ST-ACC-ON-X.    EXIT.
CK-DIR-V.
     IF (SEC-CODE-1 = 1 OR SEC-CODE-1 = 3) AND
        (DIR-VEH-C = 1 OR DIR-VEH-C = 3)
     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.
     IF (SEC-CODE-1 = 2 OR SEC-CODE-1 = 4) AND
        (DIR-VEH-C = 2 OR DIR-VEH-C = 4)
     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.
CK-DIR-V-X.    EXIT.
LOAD-NAME-TABLE.
     MOVE ZERO TO ANT.
     MOVE SPACES TO NAME-TABLE.
LNT-LOOP.
     IF J-CODE = '*'
        MOVE J-NAME TO H-SYSTEM
        GO TO LNT-LOOP.
     ADD 1 TO ANT.
     IF ANT > 20
        GO TO LNT-EXIT.
     MOVE J-CODE TO NT-CITY (ANT).
     MOVE J-NAME TO NT-NAME (ANT).
     GO TO LNT-LOOP.
LNT-EXIT.
     EXIT.
   9PS