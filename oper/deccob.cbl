IDENTIFICATION  DIVISION.
PROGRAM-ID.  'T3060'.
AUTHOR.  CAGNEY FRANCE, STANFORD OPTNER AND ASSOCIATES INC.
INSTALLATION.  LOS ANGELES TRAFFIC DEPARTMENT, DATA SERVICE BUREAU
     CITY OF LOS ANGELES, JULY 1969.
ENVIRONMENT  DIVISION.
CONFIGURATION  SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
SPECIAL-NAMES.
     CHANNEL (1) IS TOP-OF-PAGE.
INPUT-OUTPUT  SECTION.
FILE-CONTROL.
     SELECT REPORT-FILE  ASSIGN DSK,
      RECORDING MODE IS ASCII.
     SELECT VOLFILE ASSIGN DSK,
      RECORDING MODE IS ASCII
             RESERVE NO ALTERNATE AREA
             ACCESS MODE IS INDEXED
             RECORD KEY IS VOL-REC-KEY
             SYMBOLIC KEY IS VOL-SYM-KEY.
     SELECT ACCIDENT-FILE ASSIGN DSK,
      RECORDING MODE IS ASCII

             RESERVE NO ALTERNATE AREA
         ACCESS MODE IS INDEXED
         RECORD KEY IS RECORD-KEY
         SYMBOLIC KEY IS A-SYM-KEY.
     SELECT REQUEST-TAPE ASSIGN DSK,
      RECORDING MODE IS ASCII.
     SELECT ISFILE ASSIGN DSK,
      RECORDING MODE IS ASCII

             RESERVE NO ALTERNATE AREA
             ACCESS MODE IS INDEXED
             RECORD KEY IS IS-REC-KEY
             SYMBOLIC KEY IS SYM-KEY.
     SELECT PRINT-FILE ASSIGN DSK,
      RECORDING MODE IS ASCII.
DATA  DIVISION.
FILE  SECTION.


FD   REQUEST-TAPE
         VALUE OF IDENTIFICATION IS "REQESTDAT"
     RECORD CONTAINS 100 CHARACTERS
     BLOCK CONTAINS 35 RECORDS
     LABEL RECORDS ARE STANDARD
     DATA RECORD IS REQUEST-REC.
01   REQUEST-REC.
     03 T10-S-LOC.
        05 REQ-NO-10         PICTURE 999.
        05 IS-10.
           07 IS-10-ST1      PICTURE X(5).
           07 IS-10-ST2      PICTURE X(5).
        05 IS-10-REQUESTER   PICTURE X(15).
        05 DATE-LIMIT-10.
           07 FR-MO          PICTURE 99.
           07 FR-DA          PICTURE 99.
           07 FR-YR          PICTURE 99.
           07 T-MO           PICTURE 99.
           07 T-DA           PICTURE 99.
           07 T-YR           PICTURE 99.
           05 FILLER         PICTURE X(40).
         03 T10-R-T20-T22-REQ REDEFINES T10-S-LOC.
            05 R-10-REQ-NO   PICTURE 999.
            05 R-ST-CODE     PICTURE X(5).
            05 FIRST-ST-CODE PICTURE X(5).
            05 LAST-ST-CODE  PICTURE X(5).
            05 R-SEARCH      PICTURE X.
            05 R-REQUESTER   PICTURE X(15).
            05 R-DATE-LIMIT.
               07 R-FR-MO    PICTURE 99.
               07 R-FR-DA    PICTURE 99.
               07 R-FR-YR    PICTURE 99.
               07 R-T-MO     PICTURE 99.
               07 R-T-DA     PICTURE 99.
               07 R-T-YR     PICTURE 99.
           05 R-DIRECTION    PICTURE X(01).
           05 FILLER         PICTURE X(33).
        03 REQ-SORT-KEY.
           05 FILLER         PICTURE XX.
           05 KEY-CODE       PICTURE 99.
           05 FILLER         PICTURE X(16).
FD   PRINT-FILE
         VALUE OF IDENTIFICATION IS "PRINT DAT"
     RECORD CONTAINS 133 CHARACTERS

     LABEL RECORDS ARE STANDARD
     DATA RECORD IS PRINT-REC.
01   PRINT-REC.
     03 FILLER       PICTURE X.
     03 PRINT-LINE   PICTURE X(132).
FD   REPORT-FILE
         VALUE OF IDENTIFICATION IS "REPORTDAT"
     LABEL RECORDS ARE STANDARD
     RECORD CONTAINS 290 CHARACTERS
     BLOCK CONTAINS 25 RECORDS
     DATA RECORD IS REPORT-REC.
01   REPORT-REC.
     02 T81-DETAIL.
        03 T81-REQUESTER     PICTURE X(15).
        03 T81-SEARCH-DATES  PICTURE X(12).
        03 T81-REC-CODE      PICTURE 9.
        03 T81-DATE.
           05 T81-YR         PICTURE XX.
           05 T81-MO         PICTURE XX.
           05 T81-DA         PICTURE XX.
        03 T81-DAY-OF-WEEK   PICTURE X.
        03 T81-DOW REDEFINES T81-DAY-OF-WEEK PICTURE 9.
        03 T81-LOC.
           05 T81-LOC1       PICTURE X.
           05 T81-LOC2       PICTURE X.
           05 T81-LOC3       PICTURE X.
        03 FILLER            PICTURE XX.
        03 T81-REVERSE-CODE  PICTURE X.
        03 FILLER            PICTURE X.
       03 T81-PEAK.
        04 T81-AM-PEAK.
           05 T81-AM-N-E-HR  PICTURE   99.
           05 T81-AM-N-E-MIN PICTURE    9.
           05 T81-AM-N-E-VOL PICTURE 9999.
           05 T81-AM-S-W-HR  PICTURE   99.
           05 T81-AM-S-W-MIN PICTURE    9.
           05 T81-AM-S-W-VOL PICTURE 9999.
           05 T81-AM-T-HR    PICTURE   99.
           05 T81-AM-T-MIN   PICTURE    9.
           05 T81-AM-T-VOL   PICTURE 9999.
        04 T81-PM-PEAK.
           05 T81-PM-N-E-HR  PICTURE   99.
           05 T81-PM-N-E-MIN PICTURE    9.
           05 T81-PM-N-E-VOL PICTURE 9999.
           05 T81-PM-S-W-HR  PICTURE   99.
           05 T81-PM-S-W-MIN PICTURE    9.
           05 T81-PM-S-W-VOL PICTURE 9999.
           05 T81-PM-T-HR    PICTURE   99.
           05 T81-PM-T-MIN   PICTURE    9.
           05 T81-PM-T-VOL   PICTURE 9999.
        03 T81-TOT-24-HR.
           05 T81-TOT-N-E    PICTURE S9(5).
           05 T81-TOT-S-W    PICTURE S9(5).
           05 T81-TOT-TOT    PICTURE S9(5).
        03 T81-MED           PICTURE   XXX.
        03 T81-ADJ-FAC REDEFINES T81-MED
                             PICTURE  S9V99.
        03 T81-RDWY          PICTURE  XXXX.
        03 T81-LA            PICTURE    XX.
        03 T81-IS-CODE.
           05 T81-ST-1       PICTURE  X(5).
           05 T81-ST-2       PICTURE  X(5).
        03 T81-ST-NAME-1     PICTURE X(30).
        03 T81-ST-NAME-2     PICTURE X(30).
        03 FILLER            PICTURE X(92).
     02 T23-DETAIL REDEFINES T81-DETAIL.
        05 T23-REC-1         PICTURE X(270).
        05 T23-REC-2 REDEFINES T23-REC-1.
           07 T23-REC-2-AT-C PICTURE X.
           07 T23-REC-2-X-SC PICTURE X(5).
           07 T23-REC-2-X-ST PICTURE X(30).
           07 T23-40         PICTURE X(40).
           07 T23-REC-2-INCR REDEFINES T23-40 OCCURS 40 TIMES.
              09 T23-ACC     PICTURE X.
           07 T23-REC-2-OV40 PICTURE X.
           07 FILLER         PICTURE X(193).
     02  T10-DETAIL REDEFINES T23-DETAIL.
       03 T10-DET-A.
         05 T10-REQUESTER    PICTURE X(15).
         05 T10-SEARCH-DATES PICTURE X(12).
         05 FILLER           PICTURE XX.
         05 T10-IS-CODE.
            07 T10-IS-1      PICTURE X(5).
            07 T10-IS-2      PICTURE X(5).
       03 DETAIL-REC-VAR.
         05 DOW              PICTURE X.
         05 D-O-W REDEFINES DOW PICTURE 9.
         05 T10-DATE.
            07 T10-MO        PICTURE XX.
            07 T10-DA        PICTURE XX.
            07 T10-YR        PICTURE XX.
         05 T10-TIME.
            07 T10-HR        PICTURE XX.
            07 T10-M         PICTURE XX.
         05 T10-DR-NO        PICTURE X(6).
         05 PRI-CODE         PICTURE X(6).
         05 GP-CODE          PICTURE XX.
         05 GROUP-CODE REDEFINES  GP-CODE PICTURE 99.
       04 T10-PT-IMPACT.
         05 LOCATION-CODE.
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
           04 T10-FL.
         05 DIR-ANL          PICTURE 999.
         05 LIGHT            PICTURE X.
         05 RD-CONDITION     PICTURE X.
         05 SPEC-CIR         PICTURE XX.
         05 WEATHER          PICTURE 9.
         05 T10-AT-IS        PICTURE 9.
       04 T10-NO-PER-INJ.
         05 T10-INJ-A        PICTURE S99.
         05 T10-INJ-B        PICTURE S99.
         05 T10-INJ-C        PICTURE S99.
         05 T10-INJ-K        PICTURE S99.
       04 T10-DET-B.
         05 T10-ACC-TYPE     PICTURE 99.
         05 T10-A-SEV        PICTURE 9.
         05 T10-REVERSE-ST-C PICTURE 9.
         05 T10-NO-PARTIES   PICTURE 9.
         05 FILLER             PICTURE XXX.
         05 T10-PARTY OCCURS 9 TIMES.
            07 PARTY-NO      PICTURE 9.
           07 T10-PARTY-ENTRY.
            11 VEH-ACTION    PICTURE 99.
            11 T10-DIR       PICTURE 9.
            11 VEH-TYPE      PICTURE 99.
            11 T10-CLASS     PICTURE 9.
            11 T10-COND      PICTURE 99.
            11 T10-AGE       PICTURE 99.
            11 PARTY-INJ     PICTURE 9.
            11 CONTRIB-CIR   PICTURE 99.
            11 FILLER        PICTURE XX.
            11 PED-ACTION    PICTURE 99.
         05 FILLER           PICTURE X(8).
     02 T20-DETAIL REDEFINES T10-DETAIL.
         03 T20-REQUESTER    PICTURE X(15).
         03 T20-SEARCH-DATE  PICTURE X(12).
         03 FILLER           PICTURE X.
         03 T20-ROUTE        PICTURE X(5).
         03 T20-ROUTE-NAME   PICTURE X(30).
         03 T20-X-ST.
            05 T20-X-ST1     PICTURE X(5).
            05 T20-X-ST2     PICTURE X(5).
         03 T20-X-ST-NAME1   PICTURE X(30).
         03 T20-X-ST-NAME2   PICTURE X(30).
         03 FILLER           PICTURE X(137).
     02 T20-DETAIL-A REDEFINES T20-DETAIL.
        03 T20-SUB-SEG       PICTURE X(5).
        03 T20-SUB-SEG-NAME  PICTURE X(30).
        03 T20-AT-IS         PICTURE X.
         03 T20-VOL-ADT      PICTURE 9(5).
        03 T20-VOL-X-ST      PICTURE 9(5).
        03 T20-DISTANCE      PICTURE S9999.
        03 FILLER            PICTURE X(24).
        03 T20-IS-ACC         PICTURE 999.
        03 T20-NON-IS-ACC    PICTURE 999.
        03 T20-STRAF          PICTURE 999.
        03 FILLER            PICTURE X(187).
     02 T20-DETAIL-B REDEFINES T20-DETAIL-A.
        03 T20-CLASS         PICTURE X.
        03 T20-CLASS1  REDEFINES T20-CLASS  PICTURE 9.
        03 T20-NO-LANES      PICTURE X.
        03 T20-NO-LANES-N REDEFINES T20-NO-LANES PICTURE 9.
        03 T20-M-WDTH        PICTURE X.
        03 T20-M-WDTH-N REDEFINES T20-M-WDTH     PICTURE 9.
        03 T20-M-TYPE        PICTURE X.
        03 T20-M-TYPE-N REDEFINES T20-M-TYPE     PICTURE 9.
        03 T20-PARK          PICTURE X.
        03 T20-PARK-N  REDEFINES T20-PARK        PICTURE 9.
        03 T20-PK            PICTURE X.
        03 T20-PK-N REDEFINES T20-PK PICTURE 9.
        03 T20-DIR-FLOW      PICTURE X.
        03 T20-DIR-FLOW-N REDEFINES T20-DIR-FLOW PICTURE 9.
         03 FILLER           PICTURE X(263).
       02 DETAIL-REC-V1.
     04 SORT-KEY.
        05 REPORT-CODE       PICTURE 99.
        05 REQ-NO            PICTURE 999.
        05 TOT-CAT-ACC       PICTURE S999.
        05 REC-CODE          PICTURE 9.
        05 CODE-IS-R.
           07 IS-SEQUENCE    PICTURE 9(4).
           07 CODE-IS-DATE   PICTURE X(6).
        05 FILLER REDEFINES CODE-IS-R.
           07 DISTANCE       PICTURE 9(5).
           07 T20-SEG-NO REDEFINES DISTANCE  PICTURE 9(5).
           07 FILLER-SEG     PICTURE 9(5).
        05 FILLER            PICTURE X.
FD   VOLFILE
         VALUE OF IDENTIFICATION IS "VOL   DAT"
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
        05 VOL-DA        PICTURE 99.
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
FD   ISFILE
         VALUE OF IDENTIFICATION IS "ISFILEDAT"
     LABEL RECORDS ARE STANDARD
     RECORD CONTAINS 300 CHARACTERS
     BLOCK CONTAINS  10 RECORDS
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
           07 IS-CLASS-CODE  PICTURE X.
           07 IS-NO-LEGS     PICTURE X.
           07 IS-CONTROL     PICTURE X.
           07 IS-SIGNAL-CTRL PICTURE X.
           07 IS-SIGNAL-CHAR PICTURE XX.
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
        05 IS-N-LEG          PICTURE 9(5).
        05 IS-E-LEG          PICTURE 9(5).
        05 IS-S-LEG          PICTURE 9(5).
        05 IS-W-LEG          PICTURE 9(5).
     03 IS-NEXT-IS.
        05 IS-NEXT-LEG OCCURS 4 TIMES.
           07 IS-NEXT-IS-CODE.
              09 IS-NEXT-ST-1        PICTURE 9(5).
              09 IS-NEXT-ST-2        PICTURE 9(5).
           07 IS-NEXT-IS-DIST   PICTURE 9999.
           07 IS-NEXT-IS-SLOPE   PICTURE X.
     03 IS-HAZARDOUS-ID.
        05 IS-YEAR OCCURS 4 TIMES.
           07 IS-YEAR-1      PICTURE X.
           07 IS-YEAR-2      PICTURE X.
           07 IS-YEAR-3      PICTURE X.
           07 IS-YEAR-4      PICTURE X.
     03 IS-DEAD-END          PICTURE X.
     03 IS-VACATED           PICTURE X.
     03 IS-OTHER-IS.
        05 IS-JOG            PICTURE X.
        05 IS-OTHER-ST.
           07 IS-OTHER-ST-1  PICTURE 9(5).
           07 IS-OTHER-ST-2  PICTURE 9(5).
     03 IS-ASSUME-EW         PICTURE X.
     03 IS-MAINT             PICTURE XXXX.
     03 IS-POL-DIST          PICTURE XXXX.
     03 IS-TR-DIST           PICTURE X.
     03 FILLER               PICTURE X(30).
FD   ACCIDENT-FILE
         VALUE OF IDENTIFICATION IS "ACCIDTDAT"
         LABEL RECORDS ARE STANDARD
         RECORD CONTAINS 112 CHARACTERS
     BLOCK CONTAINS 64 RECORDS
         DATA RECORD IS ACC-IN-REC-1.
01   ACC-IN-REC-1.
     02  FIELD-1.
         03  DELETE-CODE             PICTURE X.
         03  RECORD-KEY.
             04  IS-CODE-A           PICTURE X(10).
             04  IND-NO-A            PICTURE 999.
             04  REC-CODE-A          PICTURE 9.
         03  FILLER                  PICTURE 9(6).
         03  SUPL-REC-CODE           PICTURE 9.
         03  DAY-OF-WEEK             PICTURE 9.
         03  DATE-OCCURRED.
             04  YR-OCCURRED         PICTURE 99.
             04  MO-OCCURRED         PICTURE 99.
             04  DY-OCCURRED         PICTURE 99.
        03 TIME-OCCURRED     PICTURE 9(4).
        03 FILLER            PICTURE X(7).
     02  FIELD-2                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-3                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-4                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-5                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-6                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-7                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-8                 PICTURE 9(17)   COMPUTATIONAL.
     02  FIELD-9                 PICTURE 9(17)   COMPUTATIONAL.
WORKING-STORAGE  SECTION.
77   FILLER-2        PICTURE X(21) VALUE 'BEGIN WORKING STORAGE'.
77   UNPROC-SW           PICTURE 9 VALUE ZERO.
77   J                       PICTURE 99.
77   H                       PICTURE 9     VALUE 1.
77   PASS-S                  PICTURE 9     VALUE ZERO.
77   SEARCH-SW               PICTURE 9 VALUE ZERO.
77   TYPE-SEARCH             PICTURE X.
77   ISR-SW          PICTURE 9 VALUE ZERO.
77   IS-ER           PICTURE 9 VALUE ZERO.
77   CTR                     PICTURE 99.
77   C                       PICTURE 9.
COUNTER                 PICTURE 999.
77   ISW                     PICTURE 9 VALUE ZERO.
77   COUNT-125               PICTURE 999 VALUE ZERO.
77   R-SEQ                   PICTURE 9(4) VALUE ZERO.
01   REPORT-DATE.
     03 FROM-DATE.
        05 FROM-YR           PICTURE 99.
        05 FROM-MO           PICTURE 99.
        05 FROM-DA           PICTURE 99.
     03 TO-DATE.
        05 TO-YR             PICTURE 99.
        05 TO-MO             PICTURE 99.
        05 TO-DA             PICTURE 99.
01   A-SYM-KEY.
     03 A-SYM-KEY-IS.
        05 A-SYM-ST-1        PICTURE X(5).
        05 A-SYM-ST-2        PICTURE X(5).
     03 A-SYM-INDEX          PICTURE 999.
     03 A-SYM-REC-CODE       PICTURE 9.
01   SEG-SYM-KEY             PICTURE 9(5).
01   SYM-KEY.
     03 SYM-KEY-ST-1         PICTURE X(5).
     03 SYM-KEY-ST-2         PICTURE X(5).
01   VOL-SYM-KEY.
     03 VOL-SYM-ST-CODE.
        05 VOL-SYM-ST-1      PICTURE X(5).
        05 VOL-SYM-ST-2      PICTURE X(5).
     03 VOL-SYM-INDEX                PICTURE XXX.
     03 VOL-SYM-INDEX-N REDEFINES VOL-SYM-INDEX PICTURE 999.
01   A-HOLD.
     05 A-OLDEST             PICTURE 999.
     05 A-LATEST             PICTURE 999.
     05 SU-H                 PICTURE 9.
01   IS-ST-H.
     03 IS-ST1-H             PICTURE X(5).
     03 IS-ST2-H             PICTURE X(5).
01   IS-OTHER-HOLD.
     03 IS-OTHER-1           PICTURE X(5).
     03 IS-OTHER-2           PICTURE X(5).
01   OLD-LOC.
     03 POS-1                PICTURE 9.
     03 POS-2                PICTURE 9.
     03 POS-3                PICTURE 9.
01   ROUTE-ST-HOLD           PICTURE X(5).
01   FIRST-X-ST-HOLD.
     03 FIRST-ST-1           PICTURE X(5).
     03 FIRST-ST-2           PICTURE X(5).
01   LAST-X-ST-HOLD.
     03 LAST-ST-1            PICTURE X(5).
     03 LAST-ST-2            PICTURE X(5).
01   DIR-CODE-HOLD.
     03 EW-DIR               PICTURE 9.
     03 NS-DIR               PICTURE 9.
     03 E-DIR                PICTURE 9.
     03 W-DIR                PICTURE 9.
     03 N-DIR                PICTURE 9.
     03 S-DIR                PICTURE 9.
     03 R-N-DIR              PICTURE 9.
     03 R-E-DIR              PICTURE 9.
     03 R-S-DIR              PICTURE 9.
     03 R-W-DIR              PICTURE 9.
     03 JUST-LEFT-IS.
        05 LEFT-IS-1         PICTURE X(5).
        05 LEFT-IS-2         PICTURE X(5).
01   X-Y-HOLD.
     03 FIRST-X-Y.
        05 FIRST-X           PICTURE X(6).
        05 FIRST-Y           PICTURE X(6).
     03 LAST-X-Y.
        05 LAST-X            PICTURE X(6).
        05 LAST-Y            PICTURE X(6).
01   IS-WORK.
     03 IS-COUNT             PICTURE 999.
01   SW-INTER.
     03 SW-IS                PICTURE 9 VALUE ZERO.
     03 SW-N-IS              PICTURE 9 VALUE ZERO.
01   NO-ACCIDENTS.
     03 P-AC                 PICTURE 99.
     03 I-AC                 PICTURE 99.
     03 F-AC                 PICTURE 99.
     03 T-AC                 PICTURE 999.
01   SEV-SW.
     03 P-SW                 PICTURE 9.
     03 I-SW                 PICTURE 9.
     03 F-SW                 PICTURE 9.
01   33-VOL.
     03 33-V-W               PICTURE 9(5)  VALUE ZERO.
     03 33-V-HOLD            PICTURE 9(6)  VALUE ZERO.
     03 V-COUNT              PICTURE 999   VALUE ZERO.
     03 TOT-ACC              PICTURE 9999  VALUE ZERO.
01   T23-WORK-AREA.
     03 T23-REQUESTER        PICTURE X(15).
        03 T23-SEARCH-DATES  PICTURE X(12).
     03 T23-ROUTE            PICTURE X(5).
     03 T23-R-NAME           PICTURE X(30).
     03 T23-IS-ACC           PICTURE X(70).
     03 FILLER REDEFINES T23-IS-ACC OCCURS 7 TIMES.
        05 FILLER    OCCURS 5 TIMES.
           07 FLD-IS         PICTURE 99.
     03 T23-NON-IS-ACC       PICTURE X(80).
     03 FILLER     REDEFINES T23-NON-IS-ACC OCCURS 8 TIMES.
        05 FILLER OCCURS 5 TIMES.
           07 FLD-NON-IS     PICTURE 99.
     03 T23-ACC-PER-M        PICTURE 999V9.
     03 T23-ACC-PER-MVM      PICTURE 999V9.
     03 T23-VOL-ADT          PICTURE 9(6).
     03 FILLER               PICTURE X(44).
     03 RPT-CODE             PICTURE 99.
     03 RQ-NO                PICTURE 999.
     03 FILLER               PICTURE XXX.
     03 RCD-CODE             PICTURE 9.
     03 FILLER               PICTURE X(11).
01   CHART-HOLD.
     03 CHART-23 OCCURS 125 TIMES.
        05 CHART-ACC-TYPE    PICTURE 9.
        05 CHART-P           PICTURE 99.
        05 CHART-I           PICTURE 99.
        05 CHART-F           PICTURE 99.
        05 CHART-IS          PICTURE X(5).
        05 CHART-NAME        PICTURE X(30).
01   CHART-LOC.
     03 CHART-DIST           PICTURE 9(5).
     03 FILLER REDEFINES  CHART-DIST.
        05 CHART-D           PICTURE 999.
        05 FILLER-2-DIG   PICTURE 99.
01   CHART-OLD               PICTURE 9(5).
01   FILLER REDEFINES CHART-OLD.
     03 CH-OLD-0             PICTURE 9.
     03 CH-OLD               PICTURE 99.
     03 CH-OLD-A REDEFINES CH-OLD.
        05 CH-OLD-1          PICTURE 9.
        05 CH-OLD-2          PICTURE 9.
     03 CH-OLD-3             PICTURE 99.
01   ROUT-SW                 PICTURE 9 VALUE ZERO.
01   IS-DISTANCE             PICTURE 9(5).
01   IS-DIS REDEFINES IS-DISTANCE.
     03 IS-DIS-A             PICTURE 999.
     03 FIL-D-A          PICTURE 99.
01   DUMMY-1.
     02  ACC-IN-REC-2.
         03  DATA-GROUP-1        PICTURE X(40).
         03  DATA-GROUP-2        PICTURE 9(17).
         03  DATA-GROUP-3        PICTURE 9(17).
         03  DATA-GROUP-4        PICTURE 9(17).
         03  DATA-GROUP-5        PICTURE 9(17).
         03  DATA-GROUP-6        PICTURE 9(17).
         03  DATA-GROUP-7        PICTURE 9(17).
         03  DATA-GROUP-8        PICTURE 9(17).
         03  DATA-GROUP-9        PICTURE 9(17).
     02  ACC-HIST-REC    REDEFINES ACC-IN-REC-2.
     03  A-DELETE-CODE               PICTURE  X.
     03  A-RECORD-KEY.
         04  A-IS-CODE               PICTURE  X(10).
         04  A-IND-CODE              PICTURE  9(3).
         04  A-REC-CODE              PICTURE  9.
     03  A-IND-CODE-OLDEST           PICTURE  S9(3).
     03  A-IND-CODE-LATEST           PICTURE  S9(3).
     03  A-SUPL-REC-CODE             PICTURE  9.
     03  A-DAY-OF-WEEK               PICTURE   9.
     03  A-DATE-OCCURRED.
         04  A-YR-OCCURRED           PICTURE   99.
         04  A-MO-OCCURRED           PICTURE   99.
         04  A-DA-OCCURRED           PICTURE   99.
     03  A-TIME-OCCURRED.
         04  A-HOUR-OCCURRED         PICTURE   99.
         04  A-MIN-OCCURRED          PICTURE   99.
     03  A-REP-DIST.
         04  A-DIV                   PICTURE  99.
         04  A-DIST                  PICTURE  99.
     03  A-PROC-CYCLE                PICTURE  99.
     03  A-ACCID-SEVERITY            PICTURE  9.
     03  A-DR-NO                     PICTURE  9(6).
     03  A-HIT-RUN-CODE              PICTURE  9.
     03  A-GOV-PROP-CODE             PICTURE  9.
     03  A-INVES-UNIT                PICTURE  9.
     03  A-PRIMARY-CAUSE.
         04  A-PRIMARY-CODE          PICTURE  9(6).
         04  A-PRIMARY-GROUP         PICTURE  99.
     03  A-TEE-IS-CODE               PICTURE   9.
     03  A-POINT-OF-IMPACT.
         04  A-PI-PRIM-FT            PICTURE  9(3).
         04  A-PI-PRIM-CODE-1        PICTURE  9.
         04  A-PI-PRIM-CODE-2        PICTURE  9.
         04  A-PI-SEC-FT             PICTURE  9(4).
         04  A-PI-SEC-CODE-1         PICTURE  9.
         04  A-PI-SEC-CODE-2         PICTURE  9.
     03  A-INVOL-WITH-CODE           PICTURE  99.
     03  A-DIR-ANAL-CODE             PICTURE  9(3).
     03  A-SURVEY-1                  PICTURE  9.
     03  A-SURVEY-2                  PICTURE  9.
     03  A-LIGHTING                  PICTURE  9.
     03  A-LOCALITY                  PICTURE  9.
     03  A-ROAD-CHAR                 PICTURE  9.
     03  A-ROAD-COND                 PICTURE  9.
     03  A-SPEC-CIRC                 PICTURE  99.
     03  A-WEATHER                   PICTURE  9.
     03  A-AT-IS-CODE                PICTURE  9.
     03  A-ST-REV-CODE               PICTURE 9.
     03  A-NO-PER-INJ.
         04  A-TYPE-A                PICTURE 99.
         04  A-TYPE-B                PICTURE 99.
         04  A-TYPE-C                PICTURE 99.
         04  A-TYPE-K                PICTURE 99.
     03  A-TR-CONTROLS               PICTURE 9.
     03  A-ACCID-TYPE                PICTURE 99.
     03  A-ZEROS-1                   PICTURE 9(6).
     03  A-P1-RES                    PICTURE  9.
     03  A-P1-SEX                    PICTURE  9.
     03 A-PAR-1.
     04  A-P1-VEH-ACTION             PICTURE  99.
     04  A-P1-VEH-DIR                PICTURE  9.
     04  A-P1-VEH-TYPE               PICTURE  99.
     04  A-P1-VEH-CLASS              PICTURE  9.
     04  A-P1-VEH-COND               PICTURE  99.
     04  A-P1-AGE                    PICTURE  99.
     04  A-P1-INJ                    PICTURE  9.
     04  A-P1-CONTR-CIR              PICTURE  99.
     04  A-P1-SOB                    PICTURE  99.
     04  A-P1-PED-ACTION             PICTURE  99.
     03  A-P1-PAS-1-AGE              PICTURE  99.
     03  A-P1-PAS-1-SEX              PICTURE  9.
     03  A-P1-PAS-1-INJ              PICTURE  9.
     03  A-P1-PAS-2-AGE              PICTURE  99.
     03  A-P1-PAS-2-SEX              PICTURE  9.
     03  A-P1-PAS-2-INJ              PICTURE  9.
     03  A-P1-PAS-3-AGE              PICTURE  99.
     03  A-P1-PAS-3-SEX              PICTURE  9.
     03  A-P1-PAS-3-INJ              PICTURE  9.
     03  A-P1-PHYS-COND              PICTURE  9.
     03  A-P1-SURVEY-A               PICTURE  9.
     03  A-P1-SURVEY-B               PICTURE  9.
     03  A-ZEROS-2                   PICTURE 9(3).
     03  A-P2-RES                    PICTURE  9.
     03  A-P2-SEX                    PICTURE  9.
     03 A-PAR-2.
     04  A-P2-VEH-ACTION             PICTURE  99.
     04  A-P2-VEH-DIR                PICTURE  9.
     04  A-P2-VEH-TYPE               PICTURE  99.
     04  A-P2-VEH-CLASS              PICTURE  9.
     04  A-P2-VEH-COND               PICTURE  99.
     04  A-P2-AGE                    PICTURE  99.
     04  A-P2-INJ                    PICTURE  9.
     04  A-P2-CONTR-CIR              PICTURE  99.
     04  A-P2-SOB                    PICTURE  99.
     04  A-P2-PED-ACTION             PICTURE  99.
     03  A-P2-PAS-1-AGE              PICTURE  99.
     03  A-P2-PAS-1-SEX              PICTURE  9.
     03  A-P2-PAS-1-INJ              PICTURE  9.
     03  A-P2-PAS-2-AGE              PICTURE  99.
     03  A-P2-PAS-2-SEX              PICTURE  9.
     03  A-P2-PAS-2-INJ              PICTURE  9.
     03  A-P2-PAS-3-AGE              PICTURE  99.
     03  A-P2-PAS-3-SEX              PICTURE  9.
     03  A-P2-PAS-3-INJ              PICTURE  9.
     03  A-P2-PHYS-COND              PICTURE  9.
     03  A-P2-SURVEY-A               PICTURE  9.
     03  A-P2-SURVEY-B               PICTURE  9.
01   DUMMY-2.
     02  ACC-IN-REC-3.
         03  DATA-GRP-1          PICTURE X(40).
         03  DATA-GRP-2          PICTURE 9(17).
         03  DATA-GRP-3          PICTURE 9(17).
         03  DATA-GRP-4          PICTURE 9(17).
         03  DATA-GRP-5          PICTURE 9(17).
         03  DATA-GRP-6          PICTURE 9(17).
         03  DATA-GRP-7          PICTURE 9(17).
         03  DATA-GRP-8          PICTURE 9(17).
         03  DATA-GRP-9          PICTURE 9(17).
     02  ACC-SUPL-REC    REDEFINES ACC-IN-REC-3.
     03  S-DELETE-CODE               PICTURE X.
     03  S-RECORD-KEY.
         04  S-IS-CODE               PICTURE  X(10).
         04  S-IND-CODE              PICTURE  9(3).
         04  S-REC-CODE              PICTURE  9.
     03  S-BLANKS-1                  PICTURE X(13).
     03  S-PARTY-SECTION     OCCURS   4 TIMES.
         04 S-RES                    PICTURE  9.
         04 S-SEX                    PICTURE  9.
          04 S-PAR.
         05 S-VEH-ACTION             PICTURE  99.
         05 S-VEH-DIR                PICTURE  9.
         05 S-VEH-TYPE               PICTURE  99.
         05 S-VEH-CLASS              PICTURE  9.
         05 S-VEH-COND               PICTURE  99.
         05 S-AGE                    PICTURE  99.
         05 S-INJ                    PICTURE  9.
         05 S-CONTR-CIR              PICTURE  99.
         05 S-SOB                    PICTURE  99.
         05 S-PED-ACTION             PICTURE  99.
         04 S-PAS-1-AGE              PICTURE  99.
         04 S-PAS-1-SEX              PICTURE  9.
         04 S-PAS-1-INJ              PICTURE  9.
         04  S-PAS-2-AGE             PICTURE  99.
         04  S-PAS-2-SEX             PICTURE 9.
         04  S-PAS-2-INJ             PICTURE 9.
         04  S-PAS-3-AGE             PICTURE  99.
         04  S-PAS-3-SEX             PICTURE  9.
         04  S-PAS-3-INJ             PICTURE  9.
         04  S-PHYS-COND             PICTURE  9.
         04  S-SURVEY-A              PICTURE  9.
         04  S-SURVEY-B              PICTURE  9.
         04  S-ZEROS-1               PICTURE  9(3).


01   MONTH-TABLE.
        03 MONTH-TAB.
           05 FILLER         PICTURE 999 VALUE 031.
           05 FILLER         PICTURE 999 VALUE 059.
           05 FILLER         PICTURE 999 VALUE 090.
           05 FILLER         PICTURE 999 VALUE 120.
           05 FILLER         PICTURE 999 VALUE 151.
           05 FILLER         PICTURE 999 VALUE 181.
           05 FILLER         PICTURE 999 VALUE 212.
           05 FILLER         PICTURE 999 VALUE 243.
           05 FILLER         PICTURE 999 VALUE 273.
           05 FILLER         PICTURE 999 VALUE 304.
           05 FILLER         PICTURE 999 VALUE 334.
        03 FILLER REDEFINES MONTH-TAB.
           05 MO-ENTRY OCCURS 11 TIMES PICTURE 999.
01   T-D-HOLD                PICTURE X(12).
01   FILLER REDEFINES T-D-HOLD.
     03 C-M                  PICTURE 99.
     03 C-D                  PICTURE 99.
     03 C-Y                  PICTURE 99.
     03 C-M1                 PICTURE 99.
     03 C-D1                 PICTURE 99.
     03 C-Y1                 PICTURE 99.
01   D-TOTAL.
     03 TO-D-TOT             PICTURE 9999.
     03 FR-D-TOT             PICTURE 9999.
     03 YR-D-TOT             PICTURE 9999.
     03 TND                  PICTURE 9999.
     03 MO-CT                PICTURE 99.
01   RECORD-COUNTER          PICTURE 9(5) VALUE ZERO.
01   PRT-HR.
     03 FILLER       PICTURE X.
     03 PRT-IS-1     PICTURE X(30).
     03 FILLER       PICTURE XXX VALUE SPACES.
     03 PRT-IS-2     PICTURE X(30).
     03 FILLER       PICTURE X(69) VALUE SPACES.
01   SUPL-RD-ON      PICTURE 9 VALUE ZERO.
01   DIR-ST-SW       PICTURE 9 VALUE ZERO.
01   DIR-VEH-C       PICTURE 9 VALUE ZERO.
01   SUPL-INVALID        PICTURE 9 VALUE ZERO.
01   JOGGED-SWITCH       PICTURE 9 VALUE ZERO.
01   DATE-2YRS       PICTURE 9(6).
01   FILLER REDEFINES DATE-2YRS.
     03 YRS-2        PICTURE 99.
     03 MOS-2        PICTURE 99.
     03 DAS-2        PICTURE 99.
01   END-SW          PICTURE 9 VALUE ZERO.
PROCEDURE  DIVISION.
FIRST-PAR.
     OPEN INPUT  REQUEST-TAPE
                 ACCIDENT-FILE
                 ISFILE
                 VOLFILE
          OUTPUT REPORT-FILE
                 PRINT-FILE.
START-PROCESSING.
     READ REQUEST-TAPE AT END, GO TO EOJ.
     IF KEY-CODE < 30 GO TO START-PROCESSING.
     MOVE ZERO TO R-SEQ.
     IF KEY-CODE = 30 PERFORM RTN-30 THRU RTN-30-X
     PERFORM OTHER-IS-RTN THRU OTHER-IS-RTN-X
     PERFORM REQ-PROCESS THRU REQ-PROCESS-X.
     MOVE ZERO TO RECORD-COUNTER.
     IF KEY-CODE = 31 PERFORM RTN-31 THRU RTN-31-X.
     IF KEY-CODE = 33 PERFORM RTN-33 THRU RTN-33-X.
     MOVE ZERO TO RECORD-COUNTER.
     MOVE ZERO TO R-SEQ.
     IF KEY-CODE = 34 PERFORM RTN-34 THRU RTN-34-X
     PERFORM RTN-34A THRU RTN-34A-X
     PERFORM REQ-PROCESS THRU REQ-PROCESS-X.
     IF KEY-CODE > 34 GO TO EOJ.
     MOVE ZERO TO JOGGED-SWITCH.
     GO TO START-PROCESSING.
EOJ.
     CLOSE REQUEST-TAPE
           ACCIDENT-FILE
           ISFILE
           REPORT-FILE
           VOLFILE
           PRINT-FILE.
     DISPLAY 'T-3060 COMPLETED NORMALLY'.
     STOP RUN.
RTN-30.  MOVE ZEROES TO  REPORT-REC.
     MOVE SPACE TO TYPE-SEARCH.
     MOVE IS-10-REQUESTER TO T10-REQUESTER.
     MOVE DATE-LIMIT-10 TO T10-SEARCH-DATES.
     MOVE REQ-NO-10 TO REQ-NO.
     MOVE 10 TO REPORT-CODE.
     MOVE R-SEQ TO IS-SEQUENCE.
     IF IS-10-ST1 GREATER THAN IS-10-ST2 MOVE IS-10-ST1 TO
     A-SYM-ST-2 T10-IS-2 MOVE IS-10-ST2 TO A-SYM-ST-1 T10-IS-1
     ELSE MOVE IS-10 TO A-SYM-KEY-IS T10-IS-CODE.
RTN-30-CONT.
     MOVE FR-MO TO  FROM-MO  MOVE FR-DA TO FROM-DA.
     MOVE FR-YR TO  FROM-YR  MOVE  T-MO TO TO-MO.
     MOVE  T-DA TO    TO-DA  MOVE  T-YR TO TO-YR.
A5.  MOVE ZEROES TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE.
RD-AHFILE.
     READ ACCIDENT-FILE INVALID KEY
         MOVE ZERO TO PASS-S GO TO A-ERROR-RTN.
     IF DELETE-CODE = HIGH-VALUE AND PASS-S = ZERO GO TO RTN-30-X.
     IF DELETE-CODE = HIGH-VALUE AND PASS-S = 1
         GO TO INCREASE-INDEX.
     PERFORM UNPACK THRU UNPACK-X.
     IF PASS-S = 1 GO TO A1.
     MOVE A-IND-CODE-OLDEST TO A-OLDEST.
     MOVE A-IND-CODE-LATEST TO A-LATEST.
A1.  IF TYPE-SEARCH = SPACE GO TO A2.
     PERFORM TYPE-SEARCH-RTN THRU TYPE-SEARCH-RTN-X.
     IF SEARCH-SW = 1 MOVE ZERO TO SEARCH-SW GO TO A2 ELSE
          GO TO INCREASE-INDEX.
A2.
     IF A-DIR-ANAL-CODE = 390 GO TO INCREASE-INDEX.
     IF FROM-DATE = ZERO GO TO A3.
     IF A-DATE-OCCURRED LESS THAN FROM-DATE GO TO INCREASE-INDEX.
     IF A-DATE-OCCURRED GREATER THAN TO-DATE GO TO INCREASE-INDEX.
A3.
     ADD 1 TO RECORD-COUNTER.
     MOVE A-DAY-OF-WEEK TO DOW.
     MOVE A-DATE-OCCURRED TO T10-DATE MOVE A-SUPL-REC-CODE TO SU-H
     MOVE A-DATE-OCCURRED TO CODE-IS-DATE.
     MOVE A-TIME-OCCURRED TO T10-TIME
     MOVE A-DR-NO TO T10-DR-NO MOVE A-PRIMARY-GROUP TO GP-CODE
     MOVE A-PRIMARY-CODE TO PRI-CODE
     MOVE A-POINT-OF-IMPACT TO T10-PT-IMPACT
     MOVE A-DIR-ANAL-CODE TO     DIR-ANL
     MOVE A-LIGHTING TO LIGHT
     MOVE A-ROAD-COND TO RD-CONDITION
     MOVE A-SPEC-CIRC TO SPEC-CIR
     MOVE A-WEATHER TO WEATHER
     MOVE A-AT-IS-CODE TO T10-AT-IS
     MOVE A-NO-PER-INJ TO T10-NO-PER-INJ
     MOVE A-ACCID-SEVERITY TO T10-A-SEV
     MOVE A-ST-REV-CODE TO T10-REVERSE-ST-C
     MOVE A-SUPL-REC-CODE TO T10-NO-PARTIES
     MOVE A-ACCID-TYPE TO T10-ACC-TYPE
     MOVE A-PAR-1 TO T10-PARTY-ENTRY (1).
     MOVE A-PAR-2 TO T10-PARTY-ENTRY (2).
     PERFORM  PARTY-COUNT THRU PARTY-COUNT-X VARYING J FROM 1 BY 1
         UNTIL J GREATER THAN A-SUPL-REC-CODE. MOVE 1 TO H.
     IF SUPL-REC-CODE LESS THAN 3    WRITE REPORT-REC
         GO TO INCREASE-INDEX.
     IF SUPL-INVALID = 1 MOVE ZERO TO SUPL-INVALID
         WRITE REPORT-REC GO TO INCREASE-INDEX.
     IF SUPL-RD-ON = ZERO NEXT SENTENCE ELSE
     MOVE ZERO TO SUPL-RD-ON GO TO A4A.
     ADD 1 TO A-SYM-REC-CODE.
     READ ACCIDENT-FILE INVALID KEY WRITE REPORT-REC
     GO TO INCREASE-INDEX.
     PERFORM UNPACK-S THRU UNPACK-SX.
A4A.
     MOVE S-PAR (1) TO T10-PARTY-ENTRY (3).
     MOVE S-PAR (2) TO T10-PARTY-ENTRY (4).
     MOVE S-PAR (3) TO T10-PARTY-ENTRY (5).
     MOVE S-PAR (4) TO T10-PARTY-ENTRY (6).
     IF SU-H LESS THAN 7  WRITE REPORT-REC GO TO INCREASE-INDEX.
     ADD 1 TO A-SYM-REC-CODE.
     READ ACCIDENT-FILE INVALID KEY WRITE REPORT-REC GO TO
         INCREASE-INDEX.
     PERFORM UNPACK-S THRU UNPACK-SX.
     MOVE S-PAR (1) TO T10-PARTY-ENTRY (7).
     MOVE S-PAR (2) TO T10-PARTY-ENTRY (8).
     MOVE S-PAR (3) TO T10-PARTY-ENTRY (9).
     WRITE REPORT-REC.
INCREASE-INDEX.
     IF KEY-CODE = 30 PERFORM RTN-30 THRU RTN-30-CONT
         GO TO R-30-U.
     MOVE ZERO TO REPORT-REC PERFORM M0 THRU M1
     MOVE R-SEQ TO IS-SEQUENCE.
       MOVE A-SYM-KEY-IS TO T10-IS-CODE.
R-30-U.
     MOVE 1 TO PASS-S.
     IF A-OLDEST GREATER THAN A-LATEST OR A-OLDEST EQUAL TO ZERO
     MOVE ZERO TO PASS-S
     GO TO RTN-30-X.
     MOVE A-OLDEST TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE
     ADD 1 TO A-OLDEST GO TO RD-AHFILE.
RTN-30-X.  EXIT.
A-ERROR-RTN.
     MOVE 1 TO UNPROC-SW.
     PERFORM UNP-A THRU UNP-AX
     MOVE 'THERE ARE NO ACCIDENTS RECORDED FOR REQUESTED LOCATION'
         TO PRINT-LINE
     WRITE PRINT-REC AFTER ADVANCING 2 LINES
     GO TO RTN-30-X.
REQ-PROCESS.
     IF UNPROC-SW = 1 MOVE ZERO TO UNPROC-SW GO TO REQ-PROCESS-A.
     IF RECORD-COUNTER = ZERO
     PERFORM UNP-A THRU UNP-AX
     MOVE 'MASTER FILE DOES NOT CONTAIN RECORDS FOR REQUESTED DATE
-'LIMITS' TO PRINT-LINE
     WRITE PRINT-REC AFTER ADVANCING 2 LINES.
REQ-PROCESS-A.
     MOVE ZERO TO RECORD-COUNTER.
REQ-PROCESS-X.  EXIT.
UNPACK.
     MOVE FIELD-1 TO DATA-GROUP-1.
     MOVE FIELD-2 TO DATA-GROUP-2.
     MOVE FIELD-3 TO DATA-GROUP-3.
     MOVE FIELD-4 TO DATA-GROUP-4.
     MOVE FIELD-5 TO DATA-GROUP-5.
     MOVE FIELD-6 TO DATA-GROUP-6.
     MOVE FIELD-7 TO DATA-GROUP-7.
     MOVE FIELD-8 TO DATA-GROUP-8.
     MOVE FIELD-9 TO DATA-GROUP-9.
UNPACK-X.  EXIT.
UNPACK-S.
     MOVE FIELD-1 TO DATA-GRP-1. MOVE FIELD-2 TO DATA-GRP-2.
     MOVE FIELD-3 TO DATA-GRP-3. MOVE FIELD-4 TO DATA-GRP-4.
     MOVE FIELD-5 TO DATA-GRP-5. MOVE FIELD-6 TO DATA-GRP-6.
     MOVE FIELD-7 TO DATA-GRP-7. MOVE FIELD-8 TO DATA-GRP-8.
     MOVE FIELD-9 TO DATA-GRP-9.
UNPACK-SX.  EXIT.
PARTY-COUNT.
     MOVE H TO PARTY-NO (J)
     ADD 1 TO H.
PARTY-COUNT-X.  EXIT.
RTN-31.
     MOVE ZERO TO JUST-LEFT-IS, R-SEQ.
     MOVE ZEROES TO REPORT-REC, DIR-CODE-HOLD, IS-WORK, X-Y-HOLD.
     ADD 1 TO R-SEQ.
M0.  MOVE R-REQUESTER TO T10-REQUESTER.
     MOVE R-DATE-LIMIT TO T10-SEARCH-DATES.
     MOVE R-10-REQ-NO TO REQ-NO.
     MOVE R-SEQ TO IS-SEQUENCE.
     MOVE 10 TO REPORT-CODE.
M1.  MOVE R-FR-MO TO FROM-MO     MOVE R-FR-DA TO FROM-DA.
     MOVE R-FR-YR TO FROM-YR     MOVE R-T-MO  TO   TO-MO.
     MOVE R-T-DA  TO   TO-DA     MOVE R-T-YR  TO   TO-YR.
M3.  MOVE R-ST-CODE TO ROUTE-ST-HOLD.
     IF R-ST-CODE < FIRST-ST-CODE MOVE R-ST-CODE TO FIRST-ST-1
         MOVE FIRST-ST-CODE TO FIRST-ST-2 ELSE
     MOVE R-ST-CODE TO FIRST-ST-2
     MOVE FIRST-ST-CODE TO FIRST-ST-1.
     IF R-ST-CODE < LAST-ST-CODE MOVE R-ST-CODE TO LAST-ST-1
         MOVE LAST-ST-CODE TO LAST-ST-2   ELSE
     MOVE R-ST-CODE TO LAST-ST-2 MOVE LAST-ST-CODE TO LAST-ST-1.
M2.  MOVE LAST-X-ST-HOLD TO SYM-KEY.
     READ ISFILE INVALID KEY
         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY
         GO TO RTN-31-X.
     MOVE IS-CORDINATES TO LAST-X-Y.
     MOVE FIRST-X-ST-HOLD TO SYM-KEY, T10-IS-CODE.
     READ ISFILE INVALID KEY
         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY
         GO TO RTN-31-X.
     MOVE IS-CORDINATES TO FIRST-X-Y.
     IF R-ST-CODE = IS-NEXT-ST-1 (1) OR R-ST-CODE =
         IS-NEXT-ST-2 (1) OR R-ST-CODE = IS-NEXT-ST-1 (3) OR
         R-ST-CODE = IS-NEXT-ST-2 (3) MOVE 1 TO NS-DIR
     GO TO RTN-31-A.
     IF R-ST-CODE = IS-NEXT-ST-1 (2) OR R-ST-CODE =
         IS-NEXT-ST-2 (2) OR R-ST-CODE = IS-NEXT-ST-1 (4) OR
         R-ST-CODE = IS-NEXT-ST-2 (4) MOVE 1 TO EW-DIR
         GO TO RTN-31-A.
     DISPLAY 'UNABLE TO DETERMINE ROUTE DIRECTION FROM I/S FILE'.
     DISPLAY SYM-KEY       ' I/S RECORD CODE'.
     DISPLAY 'T-10 (ROUTE) REQUEST NO. ' REQ-NO GO TO RTN-31-X.
RTN-31-A.
     IF EW-DIR NOT = 1 GO TO RTN-31-B.
     IF FIRST-X < LAST-X MOVE 1 TO E-DIR R-E-DIR GO TO RTN-31-C.
     IF FIRST-X > LAST-X MOVE 1 TO W-DIR R-W-DIR GO TO RTN-31-C.
     IF R-DIRECTION = 3 MOVE 1 TO E-DIR, R-E-DIR GO TO RTN-31-C.
     IF R-DIRECTION = 4 MOVE 1 TO W-DIR, R-W-DIR GO TO RTN-31-C.
1A.  DISPLAY 'DIRECTION CAN NOT BE DETERMINED FROM X-Y'.
         GO TO RTN-31-X.
RTN-31-B.
     IF FIRST-Y < LAST-Y MOVE 1 TO N-DIR R-N-DIR GO TO RTN-31-C.
     IF FIRST-Y > LAST-Y MOVE 1 TO S-DIR R-S-DIR GO TO RTN-31-C.
     IF R-DIRECTION = 1 MOVE 1 TO N-DIR, R-N-DIR GO TO RTN-31-C.
     IF R-DIRECTION = 2 MOVE 1 TO S-DIR, R-S-DIR GO TO RTN-31-C.
     GO TO 1A.
RTN-31-C.  MOVE ZEROES TO IS-COUNT, IS-DISTANCE.
     MOVE R-SEARCH TO TYPE-SEARCH. MOVE ZERO TO SEARCH-SW.
     MOVE FIRST-X-ST-HOLD TO A-SYM-KEY-IS. ADD 1 TO IS-COUNT.
     MOVE FIRST-X-ST-HOLD TO T10-IS-CODE IS-10.
     MOVE ZEROES TO A-SYM-INDEX  MOVE 1 TO A-SYM-REC-CODE.
     DISPLAY SYM-KEY ' FIRST I/S ON ROUTE'.
CONTINUE-ROUTE.
     PERFORM RD-AHFILE THRU RTN-30-X.
     PERFORM 31-OTHER THRU 31-OTHER-X.
     PERFORM REQ-PROCESS THRU REQ-PROCESS-X.
     IF IS-ER = 1 MOVE ZERO TO IS-ER GO TO RTN-31-X.
     MOVE ZERO TO RECORD-COUNTER.
     IF LAST-X-ST-HOLD = SYM-KEY GO TO RTN-31-X.
     IF IS-DISTANCE > 60000 GO TO RTN-31-X.
     IF IS-COUNT > 100 GO TO RTN-31-X.
     PERFORM NEXT-IS-RTN THRU NEXT-IS-RTN-X.
     IF IS-ER = 1 MOVE 0 TO IS-ER GO TO RTN-31-X.
     MOVE ZEROES TO DETAIL-REC-VAR.
     ADD 1 TO R-SEQ.
     MOVE R-SEQ TO IS-SEQUENCE.
     MOVE SYM-KEY         TO T10-IS-CODE IS-10.
     GO TO CONTINUE-ROUTE.
RTN-31-X.  EXIT.
31-OTHER.
     MOVE ZERO TO IS-ER.
     MOVE ZERO TO JOGGED-SWITCH.
     MOVE SYM-KEY TO IS-OTHER-HOLD.
     IF IS-JOG = ZERO GO TO 31-OTHER-X.
     IF IS-JOG = '1' GO TO 31-OTHER-B.
     IF IS-JOG NOT = '2' GO TO 31-OTHER-X.
     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO IS-ER GO TO 31-OTHER-X.
     IF IS-OTHER-ST = ZERO GO TO 31-OTHER-X.
     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 31-OTHER-X.
     MOVE IS-OTHER-ST TO T10-IS-CODE, A-SYM-KEY-IS, IS-10.
     MOVE ZERO TO A-SYM-INDEX  MOVE 1 TO A-SYM-REC-CODE.
     ADD 1 TO R-SEQ.
     MOVE R-SEQ TO IS-SEQUENCE.
     PERFORM RD-AHFILE THRU RTN-30-X.
     IF (N-DIR = 1) AND ((IS-NEXT-ST-1 (1) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (1) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.
     IF (E-DIR = 1) AND ((IS-NEXT-ST-1 (2) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (2) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.
     IF (S-DIR = 1) AND ((IS-NEXT-ST-1 (3) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (3) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.
     IF (W-DIR = 1) AND ((IS-NEXT-ST-1 (4) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (4) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.
     MOVE IS-OTHER-ST TO SYM-KEY.
     READ ISFILE INVALID KEY GO TO 31-OTHER-D.
     GO TO 31-OTHER-X.
31-OTHER-B.
     IF IS-OTHER-ST = ZERO GO TO 31-OTHER-X.
     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 31-OTHER-D.
     MOVE IS-OTHER-ST TO SYM-KEY.
     MOVE IS-OTHER-ST TO A-SYM-KEY-IS, IS-10.
     MOVE IS-OTHER-ST TO T10-IS-CODE.
     MOVE ZERO TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE.
     ADD 1 TO R-SEQ.
     MOVE R-SEQ TO IS-SEQUENCE.
     PERFORM RD-AHFILE THRU RTN-30-X.
     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO IS-ER.
     READ ISFILE INVALID KEY GO TO 31-OTHER-D.
     GO TO 31-OTHER-B.
31-OTHER-D.
     MOVE IS-OTHER-HOLD TO SYM-KEY.
     READ ISFILE INVALID KEY GO TO 31-OTHER-X.
31-OTHER-X.   EXIT.
NEXT-IS-RTN.
     PERFORM ROUTE-DIR THRU ROUTE-DIR-X.
     IF IS-ER = 1 GO TO NEXT-IS-RTN-X.
     MOVE SYM-KEY TO JUST-LEFT-IS.
     IF N-DIR = 1 MOVE IS-NEXT-IS-CODE (1) TO SYM-KEY
         ADD IS-NEXT-IS-DIST (1) TO IS-DISTANCE  GO TO N1.
     IF E-DIR = 1 MOVE IS-NEXT-IS-CODE (2) TO SYM-KEY
         ADD IS-NEXT-IS-DIST (2) TO IS-DISTANCE  GO TO N1.
     IF S-DIR = 1 MOVE IS-NEXT-IS-CODE (3) TO SYM-KEY
         ADD IS-NEXT-IS-DIST (3) TO IS-DISTANCE  GO TO N1.
     IF W-DIR = 1 MOVE IS-NEXT-IS-CODE (4) TO SYM-KEY
         ADD IS-NEXT-IS-DIST (4) TO IS-DISTANCE  GO TO N1.
N1.  MOVE SYM-KEY TO A-SYM-KEY-IS MOVE 1 TO A-SYM-REC-CODE.
     MOVE ZEROES TO A-SYM-INDEX.
     ADD 1 TO IS-COUNT.
     ADD 100 TO IS-DISTANCE.
     IF FIL-D-A > 50 ADD 100 TO IS-DISTANCE.
     MOVE ZERO TO FIL-D-A.
     READ ISFILE INVALID KEY
         DISPLAY 'I/S FILE DOES NOT CONTAIN NEXT I/S RECORD'
     DISPLAY IS-OTHER-HOLD ' I/S RECORD CODE'
         DISPLAY SYM-KEY MOVE 1 TO IS-ER GO TO NEXT-IS-RTN-X.
     DISPLAY  SYM-KEY ' NEXT I/S ON ROUTE'.
NEXT-IS-RTN-X.  EXIT.
RTN-33.
     MOVE ZERO TO JUST-LEFT-IS.
     MOVE ZEROES TO REPORT-REC, DIR-CODE-HOLD, IS-WORK, X-Y-HOLD,
         T23-WORK-AREA, IS-COUNT, 33-VOL.
     MOVE ZERO TO CHART-HOLD, CHART-LOC, CHART-OLD.
     MOVE ZERO TO ROUT-SW.
     MOVE 00100 TO IS-DISTANCE.
     MOVE R-REQUESTER TO T23-REQUESTER.
     MOVE R-DATE-LIMIT TO T23-SEARCH-DATES.
     MOVE R-DATE-LIMIT TO T-D-HOLD.
     PERFORM NO-D-RTN THRU NO-D-RTN-X.
     MOVE R-10-REQ-NO TO REQ-NO, RQ-NO.
     MOVE 23 TO REPORT-CODE, RPT-CODE.
     MOVE 1 TO RCD-CODE, MOVE 2 TO REC-CODE.
     PERFORM M1 THRU M3.
     MOVE R-ST-CODE TO T23-ROUTE.
     MOVE LAST-X-ST-HOLD TO SYM-KEY.
     READ ISFILE INVALID KEY
         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY
         GO TO RTN-33-X.
     MOVE IS-CORDINATES TO LAST-X-Y.
     IF R-ST-CODE = SYM-KEY-ST-1 MOVE IS-NAME-1 TO T23-R-NAME ELSE
     MOVE IS-NAME-2 TO T23-R-NAME.
     MOVE FIRST-X-ST-HOLD TO SYM-KEY.
     READ ISFILE INVALID KEY
         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY
         GO TO RTN-33-X.
     MOVE IS-CORDINATES TO FIRST-X-Y.
     PERFORM IS-LOC THRU IS-LOC-X.
     ADD 1 TO IS-COUNT.
     IF R-ST-CODE = IS-NEXT-ST-1 (1) OR R-ST-CODE =
         IS-NEXT-ST-2 (1) OR R-ST-CODE = IS-NEXT-ST-1 (3) OR
         R-ST-CODE = IS-NEXT-ST-2 (3) MOVE 1 TO NS-DIR
         GO TO RTN-33-A.
     IF R-ST-CODE = IS-NEXT-ST-1 (2) OR R-ST-CODE =
         IS-NEXT-ST-2 (2) OR R-ST-CODE = IS-NEXT-ST-1 (4) OR
         R-ST-CODE = IS-NEXT-ST-2 (4) MOVE 1 TO EW-DIR
         GO TO RTN-33-A.
     DISPLAY 'UNABLE TO DETERMINE ROUTE DIRECTION FROM I/S FILE'.
     DISPLAY SYM-KEY       ' I/S RECORD CODE'.
     DISPLAY 'T-22 REQUEST NO. ' REQ-NO GO TO RTN-33-X.
RTN-33-A.
     IF EW-DIR NOT = 1 GO TO RTN-33-B.
     IF FIRST-X < LAST-X MOVE 1 TO E-DIR R-E-DIR GO TO RTN-33-C.
     IF FIRST-X > LAST-X MOVE 1 TO W-DIR R-W-DIR GO TO RTN-33-C.
2A.  DISPLAY 'DIRECTION CAN NOT BE DETERMIND FROM X-Y'.
         GO TO RTN-33-X.
RTN-33-B.
     IF FIRST-Y < LAST-Y MOVE 1 TO N-DIR R-N-DIR GO TO RTN-33-C.
     IF FIRST-Y > LAST-Y MOVE 1 TO S-DIR R-S-DIR GO TO RTN-33-C.
     GO TO 2A.
RTN-33-C.
     MOVE R-SEARCH TO TYPE-SEARCH, MOVE ZERO TO SEARCH-SW,
     MOVE FIRST-X-ST-HOLD TO A-SYM-KEY-IS.
3A.  MOVE ZERO TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE.
     PERFORM RTN-33-AHFILE THRU RTN-33-AHFILE-X.
     IF SYM-KEY = LAST-X-ST-HOLD GO TO RTN-33-E.
     IF END-SW = 1  MOVE ZERO TO END-SW GO TO RTN-33-E.
     IF IS-COUNT = 24 GO TO RTN-33-E.
     PERFORM NEXT-IS-RTN THRU NEXT-IS-RTN-X.
     IF IS-ER = 1 MOVE ZERO TO IS-ER GO TO RTN-33-E.
     PERFORM IS-LOC THRU IS-LOC-X.
     GO TO 3A.
RTN-33-E.
     PERFORM UP-IND-A THRU UP-IND-A-X.
     COMPUTE IS-DISTANCE ROUNDED =
         (IS-DISTANCE - (IS-COUNT * 100)).
     IF V-COUNT NOT = ZERO
         COMPUTE T23-VOL-ADT ROUNDED = (33-V-HOLD / V-COUNT).
     IF IS-DISTANCE NOT = ZERO
         COMPUTE T23-ACC-PER-M ROUNDED = (TOT-ACC / (IS-DISTANCE
             / 5280)).
     IF T23-VOL-ADT NOT = ZERO AND IS-DISTANCE NOT = ZERO
         COMPUTE T23-ACC-PER-MVM ROUNDED =
      TOT-ACC / ((TND * T23-VOL-ADT * (IS-DISTANCE / 5280))
             / 1000000).
     WRITE REPORT-REC FROM T23-WORK-AREA.
RTN-33-X.  EXIT.
33-OTHER.
     MOVE ZERO TO END-SW.
     MOVE SYM-KEY TO IS-OTHER-HOLD.
     IF IS-JOG = ZERO GO TO 33-OTHER-X.
     IF IS-JOG = '1' GO TO 33-OTHER-B.
     IF IS-JOG NOT = '2' GO TO 33-OTHER-X.
     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO END-SW
         GO TO 33-OTHER-X.
     IF IS-OTHER-ST = ZERO GO TO 33-OTHER-X.
     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 33-OTHER-X.
     MOVE IS-OTHER-ST TO A-SYM-KEY-IS.
     MOVE ZERO TO A-SYM-INDEX.
     MOVE 1 TO A-SYM-REC-CODE.
     PERFORM RTN-33-AHFILE THRU 33-UP-IND-X.
     IF (N-DIR = 1) AND ((IS-NEXT-ST-1 (1) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (1) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.
     IF (E-DIR = 1) AND ((IS-NEXT-ST-1 (2) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (2) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.
     IF (S-DIR = 1) AND ((IS-NEXT-ST-1 (3) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (3) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.
     IF (W-DIR = 1) AND ((IS-NEXT-ST-1 (4) = ROUTE-ST-HOLD) OR
     (IS-NEXT-ST-2 (4) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.
     MOVE IS-OTHER-ST TO SYM-KEY.
     READ ISFILE INVALID KEY GO TO 33-OTHER-D.
     GO TO 33-OTHER-X.
33-OTHER-B.
     IF IS-OTHER-ST = ZERO GO TO 33-OTHER-X.
     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 33-OTHER-D.
     MOVE IS-OTHER-ST TO SYM-KEY.
     MOVE IS-OTHER-ST TO A-SYM-KEY-IS.
     MOVE ZERO TO A-SYM-INDEX.
     MOVE 1 TO A-SYM-REC-CODE.
     PERFORM RTN-33-AHFILE THRU 33-UP-IND-X.
     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO END-SW.
     READ ISFILE INVALID KEY GO TO 33-OTHER-D.
     GO TO 33-OTHER-B.
33-OTHER-D.
     MOVE IS-OTHER-HOLD TO SYM-KEY.
     READ ISFILE INVALID KEY GO TO 33-OTHER-X.
33-OTHER-X.   EXIT.
RTN-33-AHFILE.
     READ ACCIDENT-FILE INVALID KEY
         DISPLAY A-SYM-KEY ' ACCIDENT RECORD NOT FOUND'
         MOVE ZERO TO PASS-S GO TO 33-UP-IND-X.
     IF DELETE-CODE = HIGH-VALUE AND PASS-S = ZERO
          GO TO 33-UP-IND-X.
     IF DELETE-CODE = HIGH-VALUE AND PASS-S = 1 GO TO 33-UP-IND.
     PERFORM UNPACK THRU UNPACK-X.
     IF PASS-S = 1 GO TO 33-A.
     MOVE A-IND-CODE-OLDEST TO A-OLDEST.
     MOVE A-IND-CODE-LATEST TO A-LATEST.
33-A.
     MOVE ZERO TO ROUT-SW.
     IF TYPE-SEARCH = SPACE GO TO RTN-33-AHFILE-X.
     PERFORM TYPE-SEARCH-RTN THRU TYPE-SEARCH-RTN-X.
     IF SEARCH-SW = 1 MOVE ZERO TO SEARCH-SW GO TO 33-B
            ELSE GO TO 33-UP-IND.
33-B.
     IF A-DIR-ANAL-CODE = 390 GO TO 33-UP-IND.
     IF A-DATE-OCCURRED < FROM-DATE GO TO 33-UP-IND.
     IF A-DATE-OCCURRED > TO-DATE GO TO 33-UP-IND.
     IF A-AT-IS-CODE = 1 MOVE 1 TO SW-IS GO TO 33-CONT.
     IF A-AT-IS-CODE NOT = 2 GO TO 33-UP-IND.
     MOVE 1 TO SW-IS.
33-CONT.  MOVE ZEROES TO SEV-SW.
     IF A-ACCID-SEVERITY = 5 MOVE 1 TO P-SW GO TO IU.
     IF A-ACCID-SEVERITY = 4 MOVE 1 TO F-SW GO TO IU.
     IF A-ACCID-SEVERITY = 1  OR   A-ACCID-SEVERITY = 2 OR
        A-ACCID-SEVERITY = 3 MOVE 1 TO I-SW ELSE
         DISPLAY A-ACCID-SEVERITY ' INVALID SEVERITY CODE'
         GO TO 33-UP-IND.
IU.
     IF ROUT-SW = ZERO MOVE IS-DIS-A TO CHART-D.
     IF ROUT-SW = ZERO GO TO IU-1.
     IF A-PI-PRIM-CODE-1 = 9 GO TO OLD-RT.
     IF R-N-DIR = 1 AND A-PI-SEC-CODE-1 = 1 GO TO ADD-DIS.
     IF R-E-DIR = 1 AND A-PI-SEC-CODE-1 = 2 GO TO ADD-DIS.
     IF R-S-DIR = 1 AND A-PI-SEC-CODE-1 = 3 GO TO ADD-DIS.
     IF R-W-DIR = 1 AND A-PI-SEC-CODE-1 = 4 GO TO ADD-DIS.
     IF R-N-DIR = 1 AND A-PI-SEC-CODE-1 = 3 GO TO SUB-DIS.
     IF R-E-DIR = 1 AND A-PI-SEC-CODE-1 = 4 GO TO SUB-DIS.
     IF R-S-DIR = 1 AND A-PI-SEC-CODE-1 = 1 GO TO SUB-DIS.
     IF R-W-DIR = 1 AND A-PI-SEC-CODE-1 = 2 GO TO SUB-DIS.
U5.  PERFORM CONT-P, MOVE ZERO TO SW-INTER GO TO 33-UP-IND.
OLD-RT.
     IF (R-N-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)
         GO TO ADD-DIS.
     IF (R-E-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)
         GO TO ADD-DIS.
     IF (R-N-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)
         GO TO SUB-DIS.
     IF (R-E-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)
         GO TO SUB-DIS.
     IF (R-S-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)
         GO TO SUB-DIS.
     IF (R-W-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)
         GO TO SUB-DIS.
     IF (R-S-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)
         GO TO ADD-DIS.
     IF (R-W-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)
         GO TO ADD-DIS.
     IF POS-1 = 1 OR POS-1 = 8 OR POS-1 = 9 GO TO CONT-P.
     GO TO U5.
ADD-DIS.
     IF A-SYM-KEY-IS = LAST-X-ST-HOLD GO TO U5.
     IF END-SW = 1 GO TO U5.
     IF FILLER-2-DIG > 50 ADD 1 TO CHART-D.
         MOVE ZERO TO FILLER-2-DIG.
     ADD IS-DISTANCE CHART-DIST GIVING CHART-DIST GO TO IU-1.
SUB-DIS.
     IF IS-DISTANCE = 00100, GO TO U5.
     IF FILLER-2-DIG < 51 NEXT SENTENCE ELSE ADD 1 TO CHART-D.
     MOVE ZERO TO FILLER-2-DIG.
     IF IS-DISTANCE LESS THAN CHART-DIST GO TO U5.
     SUBTRACT CHART-DIST FROM IS-DISTANCE GIVING CHART-DIST
         GO TO IU-1.
IU-1.
     IF CHART-D < 1 OR CHART-D > 125 GO TO U5.
     IF ROUT-SW = ZERO GO TO 33-CONT-BX.
     IF SYM-KEY = LAST-X-ST-HOLD GO TO BOUNDARY-RTN.
     IF END-SW = 1 GO TO BOUNDARY-RTN.
33-CONT-BX.
     IF P-SW = 1 ADD 1 TO CHART-P (CHART-D) GO TO CONT-P.
     IF I-SW = 1 ADD 1 TO CHART-I (CHART-D) GO TO CONT-P.
     IF F-SW = 1 ADD 1 TO CHART-F (CHART-D) GO TO CONT-P.
CONT-P.
     MOVE ZERO TO CHART-LOC, CHART-OLD, ROUT-SW.
CONT-P1.
     IF SW-N-IS = 1 MOVE ZERO TO SW-N-IS GO TO 33-CONT-B.
     MOVE ZERO TO SW-IS.
     IF A-ACCID-TYPE = 03 MOVE 6 TO C GO TO 33-CONT-A.
     IF A-ACCID-TYPE < 20 OR A-ACCID-TYPE > 32 MOVE 7 TO C
         GO TO 33-CONT-A.
     IF A-ACCID-TYPE > 22 NEXT SENTENCE ELSE MOVE 1 TO C
         GO TO 33-CONT-A.
     IF A-ACCID-TYPE > 25 NEXT SENTENCE ELSE MOVE 2 TO C
         GO TO 33-CONT-A.
     IF A-ACCID-TYPE > 28 NEXT SENTENCE ELSE MOVE 3 TO C
         GO TO 33-CONT-A.
     IF A-ACCID-TYPE > 29 NEXT SENTENCE ELSE MOVE 5 TO C
         GO TO 33-CONT-A.
     MOVE 4 TO C.
33-CONT-A.
     IF P-SW = 1
         ADD 1 TO FLD-IS (C, 1) GO TO 33-UP-IND.
     IF I-SW = 1
         ADD 1 TO FLD-IS (C, 2)
         ADD A-TYPE-A A-TYPE-B A-TYPE-C TO FLD-IS (C, 4)
         GO TO 33-UP-IND.
     IF F-SW = 1
         ADD 1 TO FLD-IS (C, 3)
         ADD A-TYPE-K TO FLD-IS (C, 5)
         ADD A-TYPE-A A-TYPE-B A-TYPE-C TO FLD-IS (C, 4).
     GO TO 33-UP-IND.
33-CONT-B.
     IF A-ACCID-TYPE = 01 MOVE 4 TO C GO TO 33-CONT-C.
     IF A-ACCID-TYPE = 03 MOVE 7 TO C GO TO 33-CONT-C.
     IF A-ACCID-TYPE = 05 MOVE 6 TO C GO TO 33-CONT-C.
     IF A-ACCID-TYPE = 07 MOVE 5 TO C GO TO 33-CONT-C.
     IF A-ACCID-TYPE = 29 MOVE 3 TO C GO TO 33-CONT-C.
     IF A-ACCID-TYPE > 25 AND A-ACCID-TYPE < 29
         MOVE 1 TO C GO TO 33-CONT-C.
     IF A-ACCID-TYPE > 29 AND A-ACCID-TYPE < 33
         MOVE 2 TO C GO TO 33-CONT-C.
     MOVE 8 TO C.
33-CONT-C.
     IF P-SW = 1
         ADD 1 TO FLD-NON-IS (C, 1)  GO TO 33-UP-IND.
     IF I-SW = 1
         ADD 1 TO FLD-NON-IS (C, 2)
         ADD A-TYPE-A A-TYPE-B A-TYPE-C  TO FLD-NON-IS (C, 4)
         GO TO 33-UP-IND.
     IF F-SW = 1
         ADD 1 TO FLD-NON-IS (C, 3)
         ADD A-TYPE-A A-TYPE-B A-TYPE-C  TO FLD-NON-IS (C, 4)
         ADD A-TYPE-K TO FLD-NON-IS (C, 5).
     GO TO 33-UP-IND.
33-UP-IND.
     MOVE 1 TO PASS-S.
     IF A-OLDEST > A-LATEST OR A-OLDEST = ZERO MOVE ZERO TO PASS-S
          GO TO 33-UP-IND-X.
     MOVE A-OLDEST TO A-SYM-INDEX ADD 1 TO A-OLDEST
         GO TO RTN-33-AHFILE.
33-UP-IND-X.  EXIT.
RTN-33-AHFILE-A.
     PERFORM 33-OTHER THRU 33-OTHER-X.
     MOVE IS-OTHER-HOLD TO VOL-SYM-KEY.
     MOVE ZEROES TO VOL-SYM-INDEX.
VOL-RTN-CONT.
     READ VOLFILE INVALID KEY GO TO RTN-33-AHFILE-X.
     IF VOL-DELETE-CODE = HIGH-VALUE GO TO VOL-RTN-INCREASE.
     IF VOL-D-O-W = '6' OR VOL-D-O-W = '7' GO TO VOL-RTN-INCREASE.
     IF DATE-2YRS = ZERO NEXT SENTENCE ELSE
         IF VOL-DATE < DATE-2YRS GO TO VOL-RTN-INCREASE.
     IF EW-DIR = 1 AND (VOL-LOC-1 = 1 OR VOL-LOC-1 = 3)
         GO TO VOL-RTN-INCREASE.
     IF NS-DIR = 1 AND (VOL-LOC-1 = 2 OR VOL-LOC-1 = 4)
         GO TO VOL-RTN-INCREASE.
     IF VOL-REC-CODE = 1 GO TO P1.
     COMPUTE 33-V-W ROUNDED = ((VOL-TOTAL-NE-N + VOL-TOTAL-SW-N)
         * VOL-ADJ-FAC).
     IF 33-V-W = ZERO GO TO RTN-33-AHFILE-X  ELSE
          ADD 33-V-W TO 33-V-HOLD ADD 1 TO V-COUNT.
          GO TO RTN-33-AHFILE-X.
P1.  IF VOL-TOTAL-TOT-N = ZERO GO TO RTN-33-AHFILE-X ELSE
         ADD VOL-TOTAL-TOT-N TO 33-V-HOLD ADD 1 TO V-COUNT.
RTN-33-AHFILE-X.  EXIT.
VOL-RTN-INCREASE.
     ADD 1 TO VOL-SYM-INDEX-N, GO TO VOL-RTN-CONT.
IS-LOC.
     IF IS-DIS-A < 1 OR IS-DIS-A > 125 GO TO IS-LOC-X.
     MOVE 1 TO CHART-ACC-TYPE (IS-DIS-A).
     IF R-ST-CODE = SYM-KEY-ST-1
         MOVE SYM-KEY-ST-2 TO CHART-IS (IS-DIS-A)
         MOVE IS-NAME-2 TO CHART-NAME (IS-DIS-A)  ELSE
     MOVE SYM-KEY-ST-1 TO CHART-IS (IS-DIS-A)
     MOVE IS-NAME-1 TO CHART-NAME (IS-DIS-A).
IS-LOC-X.  EXIT.
UP-IND-A.
     PERFORM F-125 THRU F-125-X VARYING COUNT-125 FROM 1 BY 1
         UNTIL COUNT-125 GREATER THAN 125.
UP-IND-A-X.  EXIT.
F-125.
     IF CHART-ACC-TYPE (COUNT-125) NOT = 1 GO TO F-125A.
       MOVE '1' TO T23-REC-2-AT-C.
       MOVE CHART-IS (COUNT-125) TO T23-REC-2-X-SC.
       MOVE CHART-NAME (COUNT-125) TO T23-REC-2-X-ST.
       MOVE ZERO TO C  MOVE  1 TO COUNTER.
       PERFORM BUILD-ACC-TAB THRU BUILD-ACC-TAB-X UNTIL C = 1.
       PERFORM F-125D THRU F-125D-X.
     IF CHART-IS (COUNT-125) = LAST-ST-CODE MOVE 125 TO COUNT-125.
       GO TO F-125-X.
F-125A.
     IF CHART-P (COUNT-125) = ZERO AND CHART-I (COUNT-125) = ZERO
         AND CHART-F (COUNT-125) = ZERO GO TO F-125-X.
     MOVE '2' TO T23-REC-2-AT-C.
     MOVE ZERO TO C.
     MOVE 1 TO COUNTER.
     PERFORM BUILD-ACC-TAB THRU BUILD-ACC-TAB-X UNTIL C = 1.
     PERFORM F-125D THRU F-125D-X.
F-125-X.  EXIT.
F-125D.
     MOVE ZERO TO CHART-DIST. MOVE COUNT-125 TO CHART-D.
     MOVE CHART-DIST TO DISTANCE. MOVE ZERO TO CHART-D.
     WRITE REPORT-REC. ADD T-AC TO TOT-ACC.
     MOVE ZEROES TO REPORT-REC. MOVE 23 TO REPORT-CODE.
     MOVE 2 TO REC-CODE. MOVE R-10-REQ-NO TO REQ-NO.
F-125D-X.  EXIT.
BUILD-ACC-TAB.
     MOVE SPACE TO T23-REC-2-OV40.
     MOVE SPACES TO T23-40.  MOVE ZERO TO T-AC.
     MOVE CHART-P (COUNT-125) TO P-AC.
     MOVE CHART-I (COUNT-125) TO I-AC.
     MOVE CHART-F (COUNT-125) TO F-AC.
     IF F-AC = ZERO GO TO BUILD-ACC-1.
BI.  MOVE 'F' TO T23-ACC (COUNTER).
     IF COUNTER = 40 MOVE '*' TO T23-REC-2-OV40 MOVE 1 TO C
         GO TO BUILD-ACC-TAB-X.
     IF COUNTER = F-AC ADD 1 TO COUNTER GO TO BUILD-ACC-1  ELSE
     ADD 1 TO COUNTER GO TO BI.
BUILD-ACC-1.
     IF I-AC = ZERO GO TO BUILD-ACC-2.
     MOVE F-AC TO T-AC ADD I-AC TO T-AC.
BS.  MOVE 'I' TO T23-ACC (COUNTER).
     IF COUNTER = 40 MOVE '*' TO T23-REC-2-OV40  MOVE 1 TO C
         GO TO BUILD-ACC-TAB-X.
     IF COUNTER = T-AC ADD 1 TO COUNTER GO TO BUILD-ACC-2  ELSE
     ADD 1 TO COUNTER GO TO BS.
BUILD-ACC-2.
     IF P-AC = ZERO MOVE 1 TO C GO TO BUILD-ACC-TAB-X.
     ADD P-AC TO T-AC.
B3.  MOVE 'P' TO T23-ACC (COUNTER).
     IF COUNTER = 40 MOVE '*' TO T23-REC-2-OV40 MOVE 1 TO C
         GO TO BUILD-ACC-TAB-X.
     IF COUNTER = T-AC MOVE 1 TO C GO TO BUILD-ACC-TAB-X.
     ADD 1 TO COUNTER GO TO B3.
BUILD-ACC-TAB-X.  EXIT.
ROUTE-DIR.
     MOVE ZERO TO N-DIR E-DIR S-DIR W-DIR.
     IF R-N-DIR = 1 GO TO NORTH-ROUTE.
     IF R-E-DIR = 1 GO TO  EAST-ROUTE.
     IF R-S-DIR = 1 GO TO SOUTH-ROUTE.
     IF R-W-DIR = 1 GO TO  WEST-ROUTE.
     GO TO ROUTE-DIR-X.
NORTH-ROUTE.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (2) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (4) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.
     IF JUST-LEFT-IS = ZERO MOVE 1 TO N-DIR
     GO TO ROUTE-DIR-X.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.
     MOVE 1 TO IS-ER
     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY
     GO TO ROUTE-DIR-X.
EAST-ROUTE.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (1) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (3) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.
     IF JUST-LEFT-IS = ZERO MOVE 1 TO E-DIR
     GO TO ROUTE-DIR-X.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.
     MOVE 1 TO IS-ER
     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY
     GO TO ROUTE-DIR-X.
SOUTH-ROUTE.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (2) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (4) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.
     IF JUST-LEFT-IS = ZERO MOVE 1 TO S-DIR
     GO TO ROUTE-DIR-X.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.
     MOVE 1 TO IS-ER
     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY
     GO TO ROUTE-DIR-X.
WEST-ROUTE.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (1) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.
     IF IS-NEXT-IS-CODE (3) = JUST-LEFT-IS NEXT SENTENCE ELSE
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.
     IF JUST-LEFT-IS = ZERO MOVE 1 TO W-DIR
     GO TO ROUTE-DIR-X.
     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =
         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.
     MOVE 1 TO IS-ER
     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY
     GO TO ROUTE-DIR-X.
ROUTE-DIR-A.
     DISPLAY N-DIR E-DIR S-DIR W-DIR ' DIR OF NEXT I/S NESW'.
     IF N-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (1)
     MOVE ZERO TO N-DIR  MOVE 1 TO S-DIR
     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -SOUTH'
        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'
         GO TO ROUTE-DIR-X.
     IF E-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (2)
     MOVE ZERO TO E-DIR  MOVE 1 TO W-DIR
     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -WEST'
        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'
         GO TO ROUTE-DIR-X.
     IF S-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (3)
     MOVE ZERO TO S-DIR  MOVE 1 TO N-DIR
     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -NORTH'
        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'
         GO TO ROUTE-DIR-X.
     IF W-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (4)
     MOVE ZERO TO W-DIR  MOVE 1 TO E-DIR
     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -EAST'
        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'
         GO TO ROUTE-DIR-X.
ROUTE-DIR-X.  EXIT.
RTN-34.
     MOVE ZEROES TO REPORT-REC.
     MOVE REQ-NO-10 TO REQ-NO. MOVE 81 TO REPORT-CODE.
     MOVE IS-10-REQUESTER TO T81-REQUESTER.
     MOVE DATE-LIMIT-10 TO T81-SEARCH-DATES.
     MOVE FR-MO TO FROM-MO   MOVE FR-DA TO FROM-DA.
     MOVE FR-YR TO FROM-YR   MOVE  T-MO TO   TO-MO.
     MOVE  T-DA TO   TO-DA   MOVE  T-YR TO   TO-YR.
     IF IS-10-ST1  GREATER THAN IS-10-ST2  MOVE IS-10-ST1  TO
         VOL-SYM-ST-2 SYM-KEY-ST-2 IS-ST2-H  MOVE IS-10-ST2  TO
         VOL-SYM-ST-1 SYM-KEY-ST-1 IS-ST1-H  ELSE
     MOVE IS-10   TO VOL-SYM-ST-CODE SYM-KEY IS-ST-H.
     MOVE SYM-KEY TO T81-IS-CODE.
B2.  MOVE ZEROES TO VOL-SYM-INDEX.
RD-VOLFILE.
     READ VOLFILE INVALID KEY
     GO TO VOL-ERR-RTN.
     IF VOL-DELETE-CODE = HIGH-VALUE GO TO RTN-34-X.
     IF PASS-S EQUAL TO 1  GO TO B1.
     MOVE VOL-LATEST-N TO A-LATEST.
     MOVE VOL-OLDEST-N TO A-OLDEST. PERFORM ISR THRU ISR-X.
B1.
     IF FROM-DATE = ZERO GO TO B1A.
     IF VOL-DATE LESS THAN FROM-DATE GO TO INCREASE-VOL-INDEX.
     IF VOL-DATE GREATER THAN TO-DATE GO TO INCREASE-VOL-INDEX.
B1A.
     ADD 1 TO RECORD-COUNTER.
     MOVE VOL-REC-CODE TO T81-REC-CODE.
     MOVE VOL-DATE TO T81-DATE.
     MOVE VOL-DATE TO CODE-IS-DATE.
     MOVE VOL-D-O-W TO T81-DAY-OF-WEEK.
     MOVE VOL-LOC TO T81-LOC.
     MOVE VOL-REV-CODE TO T81-REVERSE-CODE.
     MOVE VOL-MED TO T81-MED.
     MOVE VOL-RD-WIDTH TO T81-RDWY.
     MOVE VOL-NO-LA TO T81-LA.
     IF V-NE-VOL (1) NOT NUMERIC MOVE ZEROES TO V-NE-VOL (1).
     IF V-NE-VOL (2) NOT NUMERIC MOVE ZEROES TO V-NE-VOL (2).
     IF V-SW-VOL (1) NOT NUMERIC MOVE ZEROES TO V-SW-VOL (1).
     IF V-SW-VOL (2) NOT NUMERIC MOVE ZEROES TO V-SW-VOL (2).
     IF V-TOT-VOL (1) NOT NUMERIC MOVE ZEROES TO V-TOT-VOL (1).
     IF V-TOT-VOL (2) NOT NUMERIC MOVE ZEROES TO V-TOT-VOL (2).
     IF VOL-TOTAL-NE NOT NUMERIC MOVE ZEROES TO VOL-TOTAL-NE.
     IF VOL-TOTAL-SW NOT NUMERIC MOVE ZEROES TO VOL-TOTAL-SW.
     IF VOL-TOTAL-TOT NOT NUMERIC MOVE ZEROES TO VOL-TOTAL-TOT.
     MOVE VOL-AM-PM-PEAK TO T81-PEAK.
     MOVE VOL-TOTALS-HR TO T81-TOT-24-HR.
     PERFORM ISR THRU ISR-X.
     WRITE REPORT-REC.
INCREASE-VOL-INDEX.  MOVE 1 TO PASS-S.
     PERFORM RTN-34.
     IF A-OLDEST GREATER THAN A-LATEST OR A-OLDEST EQUAL TO ZERO
         MOVE ZERO TO PASS-S GO TO RTN-34-X.
     MOVE A-OLDEST TO VOL-SYM-INDEX ADD 1 TO A-OLDEST
     READ VOLFILE INVALID KEY
         GO TO INCREASE-VOL-INDEX.
     IF VOL-DELETE-CODE = HIGH-VALUE GO TO INCREASE-VOL-INDEX.
     GO TO B1.
RTN-34-X.  EXIT.
ISR.
     READ ISFILE INVALID KEY MOVE 'STREET NAME NOT ON FILE'
         TO T81-ST-NAME-1 MOVE SPACES TO T81-ST-NAME-2
     MOVE 1 TO ISR-SW
         GO TO ISR-X.
     MOVE IS-NAME-1 TO T81-ST-NAME-1
     MOVE IS-NAME-2 TO T81-ST-NAME-2.
ISR-X.  EXIT.
RTN-34A.
     IF ISR-SW = 1 MOVE ZERO TO ISR-SW GO TO RTN-34A-X.
     MOVE SYM-KEY TO IS-OTHER-HOLD.
OTHER-A.
     READ ISFILE INVALID KEY GO TO RTN-34A-X.
     IF IS-JOG = ZERO GO TO RTN-34A-X.
     IF IS-OTHER-ST = ZERO GO TO RTN-34A-X.
     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO RTN-34A-X.
     MOVE IS-OTHER-ST TO IS-10.
     ADD 1 TO R-SEQ.
     PERFORM RTN-34 THRU RTN-34-X.
     IF IS-JOG = '2' GO TO RTN-34A-X.
     GO TO OTHER-A.
RTN-34A-X.  EXIT.
OTHER-IS-RTN.
     MOVE ZERO TO JOGGED-SWITCH.
     MOVE A-SYM-KEY-IS TO IS-OTHER-HOLD.
     MOVE A-SYM-KEY-IS TO SYM-KEY.
OTHER-B.
     READ ISFILE INVALID KEY GO TO OTHER-IS-RTN-X.
     IF IS-JOG = ZERO GO TO OTHER-IS-RTN-X.
     IF IS-OTHER-ST = ZERO GO TO OTHER-IS-RTN-X.
     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO OTHER-IS-RTN-X.
     MOVE IS-OTHER-ST TO IS-10 SYM-KEY.
     ADD 1 TO R-SEQ.
     PERFORM RTN-30 THRU RTN-30-X.
     IF IS-JOG = '2' GO TO OTHER-IS-RTN-X.
     GO TO OTHER-B.
OTHER-IS-RTN-X.  EXIT.
TYPE-SEARCH-RTN.
     IF A-AT-IS-CODE = 1 MOVE 1 TO SEARCH-SW
     GO TO TYPE-SEARCH-RTN-A.
     IF A-AT-IS-CODE NOT = 2 GO TO TYPE-SEARCH-RTN-X.
     IF A-ST-REV-CODE = ZERO AND ROUTE-ST-HOLD = A-SYM-ST-1
         MOVE 1 TO ROUT-SW
         MOVE 1 TO SEARCH-SW GO TO TYPE-SEARCH-RTN-A.
     IF A-ST-REV-CODE = 1 AND ROUTE-ST-HOLD = A-SYM-ST-2
         MOVE 1 TO ROUT-SW
         MOVE 1 TO SEARCH-SW GO TO TYPE-SEARCH-RTN-A.
     IF TYPE-SEARCH = '1' GO TO TYPE-SEARCH-RTN-X.
     IF A-PI-PRIM-CODE-1 = 9 GO TO SEARCH-OLD-WAY.
     IF A-DIR-ANAL-CODE = 300 GO TO TYPE-SEARCH-RTN-X.
     IF A-PI-SEC-CODE-1 < 1 OR A-PI-SEC-CODE-1 > 4
         GO TO TYPE-SEARCH-RTN-X.
     IF A-PI-SEC-FT > 200 GO TO TYPE-SEARCH-RTN-X.
     PERFORM ST-ACC-ON THRU ST-ACC-ON-X.
     IF DIR-ST-SW = ZERO GO TO TYPE-SEARCH-RTN-X.
     IF DIR-VEH-C = A-PI-SEC-CODE-1 GO TO TYPE-SEARCH-RTN-X.
     GO TO X-ST-APP.
SEARCH-OLD-WAY.
     MOVE A-PI-PRIM-FT TO OLD-LOC.
     IF POS-1 = 3 OR POS-1 = 5 NEXT SENTENCE  ELSE
         GO TO TYPE-SEARCH-RTN-X.
     IF POS-2 = ZERO AND POS-3 < 2 NEXT SENTENCE   ELSE
         GO TO TYPE-SEARCH-RTN-X.
X-ST-APP.
     IF A-ACCID-TYPE = 26 OR A-ACCID-TYPE = 27 OR A-ACCID-TYPE =
         28 OR A-ACCID-TYPE = 30 OR A-ACCID-TYPE = 31 OR
         A-ACCID-TYPE = 32  MOVE 1 TO SEARCH-SW GO TO
         TYPE-SEARCH-RTN-A ELSE GO TO TYPE-SEARCH-RTN-X.
TYPE-SEARCH-RTN-A.
     IF A-PI-PRIM-CODE-1 NOT = 9 MOVE A-PI-SEC-FT TO CHART-DIST
     GO TO TYPE-SEARCH-RTN-X.
     MOVE A-PI-PRIM-FT TO OLD-LOC.
     MOVE POS-2 TO CH-OLD-1.
     MOVE POS-3 TO CH-OLD-2.
     ADD 1 TO CH-OLD.
     MOVE CHART-OLD TO CHART-DIST.
TYPE-SEARCH-RTN-X.  EXIT.
NO-D-RTN.
     IF T-D-HOLD = SPACES MOVE ZERO TO DATE-2YRS ELSE
         MOVE C-M1 TO MOS-2 MOVE C-D1 TO DAS-2
         MOVE C-Y1 TO YRS-2 SUBTRACT 2 FROM YRS-2.
     IF T-D-HOLD = SPACES MOVE 2190 TO TND GO TO NO-D-RTN-X.
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
     COMPUTE YR-D-TOT = (365 * (C-Y1 - C-Y)).
     IF FR-D-TOT > TO-D-TOT SUBTRACT 365 FROM YR-D-TOT
     COMPUTE TND = (((365 + TO-D-TOT) - FR-D-TOT) + YR-D-TOT)
     GO TO NO-D-RTN-X.
     COMPUTE TND = ((TO-D-TOT - FR-D-TOT) + YR-D-TOT).
NO-D-RTN-X.   EXIT.
UNP-A.
     MOVE 'T - 4                           DEPARTMENT OF TRAFFIC'
         TO PRINT-LINE
     WRITE PRINT-REC AFTER ADVANCING   TOP-OF-PAGE
     MOVE '                                 UNPROCESSED REQUESTS'
         TO PRINT-LINE
     WRITE PRINT-REC AFTER ADVANCING 1 LINES
     MOVE T10-S-LOC TO PRINT-LINE
     WRITE PRINT-REC AFTER ADVANCING 3 LINES.
     IF IS-10-ST1 < IS-10-ST2 MOVE IS-10 TO SYM-KEY GO TO IS-RD-A.
     MOVE IS-10-ST1 TO SYM-KEY-ST-2.
     MOVE IS-10-ST2 TO SYM-KEY-ST-1.
IS-RD-A.
     READ ISFILE INVALID KEY
         MOVE 'NO STREET NAMES AVAILABLE' TO PRT-IS-1
         MOVE SPACES TO PRT-IS-2 GO TO P-MSG-A.
     IF IS-10-ST1 < IS-10-ST2 MOVE IS-NAME-1 TO PRT-IS-1
     MOVE IS-NAME-2 TO PRT-IS-2  ELSE
     MOVE IS-NAME-2 TO PRT-IS-1  MOVE IS-NAME-1 TO PRT-IS-2.
P-MSG-A.
     WRITE PRINT-REC FROM PRT-HR AFTER ADVANCING 1 LINES.
UNP-AX.   EXIT.
VOL-ERR-RTN.
     MOVE 1 TO UNPROC-SW.
     PERFORM UNP-A THRU UNP-AX
     MOVE 'THERE ARE NO VOLUME COUNTS FOR REQUESTED LOCATION'
         TO PRINT-LINE
     WRITE PRINT-REC AFTER ADVANCING 2 LINES
     GO TO RTN-34-X.
ST-ACC-ON.
     MOVE ZERO TO DIR-VEH-C, DIR-ST-SW, SUPL-RD-ON, SUPL-INVALID.
     MOVE A-P1-VEH-DIR TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1, GO TO ST-ACC-ON-X.
     MOVE A-P2-VEH-DIR TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.
     IF SUPL-REC-CODE < 3 GO TO ST-ACC-ON-X.
     ADD 1 TO A-SYM-REC-CODE.
     READ ACCIDENT-FILE INVALID KEY MOVE 1 TO SUPL-INVALID
         A-SYM-REC-CODE  GO TO ST-ACC-ON-X.
     MOVE 1 TO SUPL-RD-ON A-SYM-REC-CODE.
     PERFORM UNPACK-S THRU UNPACK-SX.
     MOVE S-VEH-DIR (1) TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.
     MOVE S-VEH-DIR (2) TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.
     MOVE S-VEH-DIR (3) TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.
     MOVE S-VEH-DIR (4) TO DIR-VEH-C.
     PERFORM CK-DIR-V THRU CK-DIR-V-X.
     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.
      MOVE ZERO TO DIR-VEH-C.
ST-ACC-ON-X.  EXIT.
CK-DIR-V.
     IF (A-PI-SEC-CODE-1 = 1 OR A-PI-SEC-CODE-1 = 3) AND
        (DIR-VEH-C = 1 OR DIR-VEH-C = 3)
     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.
     IF (A-PI-SEC-CODE-1 = 2 OR A-PI-SEC-CODE-1 = 4) AND
        (DIR-VEH-C = 2 OR DIR-VEH-C = 4)
     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.
CK-DIR-V-X.     EXIT.
BOUNDARY-RTN.
     IF A-PI-PRIM-CODE-1 = 9 GO TO BOUN-OLD.
     IF A-PI-SEC-FT < 51 GO TO 33-CONT-BX.
     IF N-DIR = 1 AND A-PI-SEC-CODE-1 = 1 GO TO U5.
     IF E-DIR = 1 AND A-PI-SEC-CODE-1 = 2 GO TO U5.
     IF S-DIR = 1 AND A-PI-SEC-CODE-1 = 3 GO TO U5.
     IF W-DIR = 1 AND A-PI-SEC-CODE-1 = 4 GO TO U5.
     GO TO 33-CONT-BX.
BOUN-OLD.
     IF (N-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)
        GO TO U5.
     IF (E-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)
        GO TO U5.
     IF (S-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)
        GO TO U5.
     IF (W-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)
        GO TO U5.
     GO TO 33-CONT-BX.
    3QFb