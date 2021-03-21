IDENTIFICATION DIVISION.

PROGRAM-ID. SYCOR.

AUTHOR. TYMSHARE TASC.  KENT E. LAWSON.

INSTALLATION. SYCOR, INC.

DATE-WRITTEN. MAY, 1973.
DATE-COMPILED. MAY, 1973.

REMARKS. PROGRAM TO UPDATE THE ORDER BASE.

ENVIRONMENT DIVISION.

CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT WORK-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT SORTED-TRANS ASSIGN TO DSK, DSK, DSK.

        SELECT EXPORT-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT PRICE-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT ORDERS-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT ORDERS-NEW ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT INSTAL-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT INSTAL-NEW ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT CHGVAL-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT CHGVAL-NEW ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT SHIPED-DAT ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT SHIPED-NEW ASSIGN TO DSK
        RECORDING MODE IS ASCII.

        SELECT AUDIT-DAT  ASSIGN TO DSK
        RECORDING MODE IS ASCII.

DATA DIVISION.
FILE SECTION.

FD WORK-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "WORK  DAT"
        DATA RECORDS ARE IN-REC.
01 IN-REC.
       03 IN-TRANS-CODE                PIC XX.
       03 IN-AGREE-NUMB                PIC 9(5).
       03 IN-REC-TYPE                  PIC X.
           88 IN-AGREE-REC             VALUE IS 'A'.
           88 IN-MODEL-REC             VALUE IS 'P'.


       03 IN-ITEM-NUMB                 PIC 99.
       03 IN-MODEL-NUMB                PIC X(9).
       03 FILLER                       PIC X(34).

SD SORTED-TRANS
        DATA RECORDS ARE T-REC.
01 T-REC.
    02 T-SORT-KEY.
       03 T-AGREE-NUMB                 PIC 9(5).
       03 T-ITEM-NUMB                  PIC 99.
       03 T-MODEL-NUMB                 PIC X(9).
       03 T-REC-TYPE                   PIC X.
           88 T-AGREE-REC              VALUE IS 'A'.
           88 T-ITEM-REC               VALUE IS 'I'.
           88 T-NOTE-REC               VALUE IS 'N'.
           88 T-MODEL-REC              VALUE IS 'P'.

       03 T-TRANS-CODE                 PIC XX.
           88 T-NEW-ENTRY              VALUE IS 'S1'.
           88 T-DELETION               VALUE IS 'S2'.
           88 T-ADDITION               VALUE IS 'S3'.
           88 T-CHANGE-REC             VALUE IS 'S4'.
           88 T-SHIP-REC               VALUE IS 'S5'.

    02 T-SORT-RECORD.
       03 W-TRANS-CODE                 PIC XX.
       03 W-AGREE-NUMB                 PIC 9(5).
       03 W-REC-TYPE                   PIC 9.
       03 W-ITEM-NUMB                  PIC 99.
       03 W-MODEL-NUMB                PIC X(9).
       03 FILLER                       PIC X(34).

    02 S1-1-RECORD   REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC X(8).
       03 W-S1-1-BLOCK.
           05 W-AGREE-DATE.
               07 W-AGREE-MONTH        PIC XX.
               07 W-AGREE-DAY          PIC XX.
               07 W-AGREE-YEAR         PIC XX.
           05 FILLER                   PIC X(39).

    02 S1-2-RECORD   REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC X(10).
       03 W-SHIP-DATE.
           05 W-SHIP-MONTH             PIC XX.
           05 W-SHIP-DAY               PIC XX.
           05 W-SHIP-YEAR              PIC XX.
       03 W-SITE-CODE                  PIC X(5).
       03 W-SUB-PO                     PIC X(18).
       03 FILLER                       PIC X(14).

    02 W-NOTE-RECORD   REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC XX.
       03 W-NOTE-KEYS                  PIC X(8).
       03 W-NOTE-ALPHA                 PIC X(40).
       03 FILLER                       PIC X(3).

    02 W-MODEL-RECORD  REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC XX.
       03 W-MODEL-KEYS                 PIC X(8).
       03 FILLER                       PIC X(9).
       03 W-MODEL-QTY                  PIC 9(4).
       03 W-MODEL-LEASE-PURCH          PIC X.
           88 W-LEASE                  VALUE IS 'L'.
           88 W-PURCH                  VALUE IS 'P'.
       03 W-MAINT-TOO                  PIC X.
       03 W-PRICE                      PIC 9(5)V99.
       03 W-MAINT-PRICE                PIC 9(4)V99.
       03 FILLER                       PIC X(29).

    02 FILLER  REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC X(8).
       03 T-CHG-AGREE.
           05 FILLER                   PIC XX.
           05 T-CHG-ITEM.
               07 FILLER               PIC X(9).
               07 T-CHG-MODEL          PIC X(15).
       03 FILLER                       PIC X(19).

    02 W-SHIP-NOTE  REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC X(19).
       03 W-SERIAL-NUMB                PIC X(8).
       03 W-DATE-SHIPPED.
           05 W-DATE-MONTH             PIC XX.
           05 W-DATE-DAY               PIC XX.
           05 W-DATE-YEAR              PIC XX.
        03 FILLER               PIC X(20).

    02 FILLER   REDEFINES T-SORT-RECORD.
       03 FILLER                       PIC XX.
       03 T-ADDR-REC                   PIC X(53).

       66 T-FORK-KEY    RENAMES   T-AGREE-NUMB  THRU  T-ITEM-NUMB.


FD EXPORT-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "EXPORTDAT"
        DATA RECORDS ARE E-REC.
01 E-REC.
       03 E-REC-COUNTRY                PIC X(5).
       03 FILLER                       PIC X(6).

FD PRICE-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "PRICE DAT"
        DATA RECORDS ARE P-REC.
01 P-REC.
        03 P-REC-MODEL                              PIC X(9).
        03 FILLER                                   PIC X(61).

FD ORDERS-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "ORDERSDAT"
        DATA RECORDS ARE ORD-REC.
01 ORD-REC.
        03 ORD-AGREE-NUMB                  PIC 9(5).
        03 ORD-REC-TYPE                    PIC X.
        03 ORD-ITEM-NUMB                   PIC 99.
        03 ORD-MODEL-NUMB                  PIC X(9).
        03 FILLER                          PIC X(35).

FD ORDERS-NEW
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "ORDERSNEW"
        DATA RECORDS ARE NEW-REC.
01 NEW-REC                       PIC X(53).


FD INSTAL-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "INSTALDAT"
        DATA RECORDS ARE INSTAL-REC-IN.
01 INSTAL-REC-IN                       PIC X(53).

FD INSTAL-NEW
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "INSTALNEW"
        DATA RECORDS ARE INSTAL-REC-OUT.
01 INSTAL-REC-OUT                      PIC X(53).

FD CHGVAL-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "CHGVALDAT"
        DATA RECORDS ARE CHGVAL-REC-IN.
01 CHGVAL-REC-IN                          PIC X(88).

FD CHGVAL-NEW
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "CHGVALNEW"
        DATA RECORDS ARE CHGVAL-REC-OUT.
01 CHGVAL-REC-OUT.
       03 CV-SALESMAN                  PIC X(16).
       03        FILLER               PIC XX.
       03 CV-AGREE-NUMB                PIC 9(5).
       03      DASH                    PIC X.
       03 CV-ITEM-NUMB                 PIC 99.
       03        FILLER               PIC XX.
       03 CV-DATE                      PIC X(6).
       03        FILLER               PIC XX.
       03 CV-BILL-TO                   PIC X(30).
       03        FILLER               PIC X.
       03 CV-TRANS                     PIC X(6).
       03        FILLER               PIC XX.
       03 CV-EQUIP-VALUE               PIC 9(6)V99.
       03 CV-ORDER-VALUE               PIC 9(6)V99.
       03 CV-TRIAL-DAYS                PIC 999.


FD SHIPED-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "SHIPEDDAT"
        DATA RECORDS ARE SHIPED-REC-IN.
01 SHIPED-REC-IN                         PIC X(201).

FD SHIPED-NEW
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "SHIPEDNEW"
        DATA RECORDS ARE SHIPED-REC-OUT.
01 SHIPED-REC-OUT.
       03 S-DATE                       PIC X(6).
       03 S-KEY                        PIC X(7).
       03 S-MODEL-NUMB                 PIC X(9).
       03 S-MODEL-DESC                 PIC X(40).
       03 S-SERIAL-NUMB                PIC X(9).
       03 S-CUST-CODE                  PIC X(5).
       03 S-SITE-CODE                  PIC X(5).
       03 S-NAME                       PIC X(30).
       03 S-ADDR-1                     PIC X(30).
       03 S-ADDR-2                     PIC X(25).
       03 S-CITY                       PIC X(20).
       03 S-STATE                      PIC X(5).
       03 S-ZIP                        PIC X(15).
       03 S-ATTN                       PIC X(15).

FD AUDIT-DAT
        LABEL RECORDS ARE STANDARD
        VALUE OF IDENTIFICATION IS "AUDIT  DAT"
        DATA RECORDS ARE AUDIT-REC.
01 AUDIT-REC                    PIC X.

WORKING-STORAGE SECTION.

*
*    THE FOLLOWING RECORDS & TABLES HOLD ALL
*       RELEVENT INFO. FOR ONE ITEM ONAN ORDER
*

01 A-REC.
       03 A-NUMB                       PIC 9(5).
       03 A-REC-TYPE                   PIC X.
       03 A-REVISION-NUMBER            PIC 99.
           03 A-DELETE-FLAG  REDEFINES A-REVISION-NUMBER  PIC 99.
       03 A-DATE                       PIC X(6).
       03 A-CUST-CODE                  PIC X(5).
       03 A-TRIAL-DAYS                 PIC 999.
       03 A-LEASE-MONTHS               PIC 999.
       03 A-PO-NUMB                    PIC X(18).
       03 A-SALESMAN                   PIC X(10).
           66 A-BLOCK RENAMES  A-DATE  THRU A-SALESMAN.

01 BILL-TO-ADDRESS.
       03 BILL-TO-1.
           05 FILLER                   PIC X(8).
           05 BILL-NAME                PIC X(30).
           05 BILL-STATE               PIC X(5).
           05 BILL-ZIP                 PIC X(5).
           05 FILLER                   PIC X(5).

       03 BILL-TO-2.
           05 FILLER                   PIC X(8).
           05 BILL-ATTN                PIC X(15).
           05 BILL-ADDR-1              PIC X(30).

       03 BILL-TO-3.
           05 FILLER                   PIC X(8).
           05 BILL-ADDR-2              PIC X(25).
           05 BILL-CITY                PIC X(20).

01 IT-REC.
       03 IT-AGREE-NUMB                PIC 9(5).
       03 IT-REC-TYPE                  PIC X.
       03 IT-NUMB                      PIC 99.
       03 IT-SHIP-DATE                 PIC X(6).
       03 IT-SITE-CODE                 PIC X(5).
       03 IT-D-OR-E                    PIC X.
       03 IT-EXPORT-DOC.
           05 IT-PRO-FORM              PIC X.
           05 IT-CREDIT-LETTER         PIC X.
           05 IT-IMPORT-DOC            PIC X.
           05 IT-EXPORT-LIC            PIC X.
           05 IT-COFC-CERT             PIC X.
           05 IT-CONSUL-INV            PIC X.
       03 IT-SUB-PO                    PIC X(18).
       03 IT-WO-REVISION               PIC 99.
           03 IT-DELETE-FLAG  REDEFINES IT-WO-REVISION  PIC 99.
       03 FILLER                       PIC X(7).

01 SHIP-TO-ADDRESS.
       03 SHIP-TO-1.
           05 FILLER                   PIC X(8).
           05 SHIP-NAME                PIC X(30).
           05 SHIP-STATE               PIC X(5).
           05 SHIP-ZIP                 PIC X(5).
           05 FILLER                   PIC X(5).

       03 SHIP-TO-2.
           05 FILLER                   PIC X(8).
           05 SHIP-ATTN                PIC X(15).
           05 SHIP-ADDR-1              PIC X(30).

       03 SHIP-TO-3.
           05 FILLER                   PIC X(8).
           05 SHIP-ADDR-2              PIC X(25).
           05 SHIP-CITY                PIC X(20).

01 NOTE-TABLE.
       03 NOTE-LINE  OCCURS 10 TIMES  INDEXED BY NOTE-COUNT.
           05 NOTE-KEYS                PIC X(8).
           05 NOTE-ALPHA               PIC X(45).


01 M-TABLE.
       03 M-REC  OCCURS 1 TO 25 TIMES   DEPENDING ON MODEL-COUNT
           INDEXED BY MODEL-INDX.

           05 M-KEY                    PIC X(8).
           05 M-NUMB                   PIC X(9).
           05 M-TYPE                   PIC X.
               88 MODEL                VALUE IS 'M'.
               88 OPTION               VALUE IS 'O'.
               88 PART                 VALUE IS 'P'.
           05 M-LINE                   PIC X.
           05 M-QTY                    PIC 9(4).
           05 M-LEASE-PURCH            PIC X.
               88 M-LEASED             VALUE IS 'L'.
               88 M-PURCH              VALUE IS 'P'.
           05 M-PRICE                  PIC 9(5)V99.
           05 M-MAINT-TOO              PIC X.
           05 M-MAINT-PRICE            PIC 9(4)V99.
           05 M-SERIAL-NUMB            PIC X(8).
           05 M-SHIP-DATE              PIC X(6).
           05 FILLER                   PIC X.

*
*    SEQUENCE KEYS
*

01 TS-CHECK.
       03 TS-AGREE-NUMB                PIC 9(5).
       03 TS-ITEM-NUMB                 PIC 99.
       03 TS-MODEL-NUMB                PIC X(9).
       03 TS-REC-TYPE                  PIC X.
       03 TS-TRANS-CODE                PIC XX.
           66 TS-FORK-KEY RENAMES TS-AGREE-NUMB  THRU TS-ITEM-NUMB.

01 MAST-SEQ-CHECK.
       03 MAST-AGREE-NUMB              PIC 9(5).
       03 MAST-ITEM-NUMB               PIC 99.
       03 MAST-MODEL-NUMB              PIC X(9).
       03 MAST-REC-TYPE                PIC X.

       66 MAST-FORK-KEY  RENAMES  MAST-AGREE-NUMB  THRU MAST-ITEM-NUMB.

01 LAST-SEQ-CHECK.
       03 LAST-AGREE-NUMB              PIC 9(5).
       03 LAST-ITEM-NUMB               PIC 99.
       03 LAST-MODEL-NUMB             PIC X(9).
       03 LAST-REC-TYPE                PIC 9.
           66 LAST-FORK-KEY RENAMES LAST-AGREE-NUMB THRU LAST-ITEM-NUMB.

01 CURRENT-FORK-KEY.
       03 CURR-AGREE-NUMB              PIC 9(5).
       03 CURR-ITEM-NUMB               PIC 99.

*    THE FOLLOWING TABLES HOLD PRICE LIST AND EXPORT INFORMATION
*

01 PRICE-TABLE   OCCURS 1 TO 50 TIMES   DEPENDING ON PRICE-COUNT
       INDEXED BY PRICE-INDX.
       03 P-MODEL-NUMB                 PIC X(9).
       03 P-TYPE                       PIC X.
       03 P-DESC                       PIC X(40).
       03 P-LINE                       PIC X.
       03 P-PURCH-PRICE                PIC 9(5)V99.
       03 P-LEASE-PRICE                PIC 9(4)V99.
       03 P-MAINT-PRICE                PIC 9(4)V99.

01 EXPORT-TABLE  OCCURS 1 TO 25 TIMES   DEPENDING ON EXPORT-COUNT
       INDEXED BY EXPORT-INDX.

       03 E-COUNTRY                    PIC X(5).
       03 E-PRO-FORM                   PIC X.
       03 E-CREDIT-LETTER              PIC X.
       03 E-IMPORT-DOC                 PIC X.
       03 E-EXPORT-LIC                 PIC X.
       03 E-COFC-CERT                  PIC X.
       03 E-CONSUL-INV                 PIC X.

*
*    WORKING GOODIES
*

01 HOLD-DATE.
       03 HOLD-YEAR                    PIC XX.
       03 HOLD-MONTH                   PIC XX.
       03 HOLD-DAY                     PIC XX.


01 CHG-FIELDS.
       03 CHG-FIELD-NAME               PIC X(6).
       03 CHG-FIELD-VALUE.
         04 CHG-VALUE-10.
           05 CHG-VALUE-7.
             06 CHG-VALUE-6.
               07 CHG-VALUE-4.
                 08 CHG-VALUE-3.
                   09 CHG-VALUE-1      PIC X.
                   09 FILLER           PIC XX.
                 08 FILLER             PIC X.
               07 FILLER               PIC XX.
             06 FILLER                 PIC X.
           05 FILLER                   PIC XXX.
         04 FILLER                     PIC X(8).


       03 FILLER     REDEFINES CHG-FIELD-VALUE.
           05 CHG-MONTH-FIELD          PIC XX.
           05 CHG-DAY-FIELD            PIC XX.
           05 CHG-YEAR-FIELD          PIC XX.
           05 FILLER                   PIC X(12).

       03 FILLER      REDEFINES CHG-FIELD-VALUE.
           05 CHG-CUST-NUMB            PIC X(5).
           05 FILLER                   PIC X.
           05 CHG-SITE-NUMB            PIC X(5).
           05 FILLER                   PIC X(7).



77 NOTE-COUNT                          USAGE IS INDEX.
77 MODEL-COUNT                         USAGE IS INDEX.
77 PRICE-COUNT                         USAGE IS INDEX.
77 EXPORT-COUNT                        USAGE IS INDEX.


**********************************************************************************

PROCEDURE DIVISION.
*
*    SORT ROUTINE -- DRIVES PROGRAM
*
SORTING SECTION.

ONLY-PARAGRAPH.
       SORT SORTED-TRANS  ON ASCENDING KEY T-SORT-KEY
            INPUT PROCEDURE IS MAIN-INPUT
           OUTPUT PROCEDURE IS MAIN-UPDATE.

       DISPLAY "END OF ORDER BASE UPDATE."
       STOP RUN.

*
*    MAIN INPUT SECTION -- IS PRE-PROCESSOR TO SORT
*      FORM SORT KEY
*      RELEASE RECORD TO SORT
*

MAIN-INPUT SECTION.

OPEN-FILES.
       OPEN INPUT WORK-DAT.

READ-INPUT-RECORD.
       READ WORK-DAT  AT END GO TO INPUT-EXIT.

FORM-SORT-KEYS.
        MOVE IN-TRANS-CODE      TO T-TRANS-CODE.
       MOVE IN-AGREE-NUMB        TO T-AGREE-NUMB.
       MOVE IN-REC-TYPE          TO T-REC-TYPE.

       IF NOT IN-AGREE-REC
                MOVE IN-ITEM-NUMB TO T-ITEM-NUMB
           ELSE MOVE ZEROES       TO T-ITEM-NUMB.

       IF IN-MODEL-REC
                MOVE IN-MODEL-NUMB TO T-MODEL-NUMB
           ELSE MOVE ZEROES        TO T-MODEL-NUMB.

RELEASE-TO-SORT.
       MOVE IN-REC TO T-SORT-RECORD.
       RELEASE T-REC.
       GO TO READ-INPUT-RECORD.

*
*    END OF MAIN INPUT
*

INPUT-EXIT.
       CLOSE WORK-DAT.

*******************************************************************************
*
*    MAIN UPDATE SECTION
*
*      LOAD PRICE & T TABLES
*      UPDATE ORDERS FILE WITH SORTED TRANSACTIONS
*      WRITE:
*          CHANGED ORDERS VALUES
*          WEEKLY SHIPPING FILE
*          INSTALLED BASE
*          WORK ORDER FILE
*          AUDIT TRAIL
*

MAIN-UPDATE SECTION.
*
*    LOAD PRICE-FILE.
*
LOAD-PRICE-FILE.
       OPEN INPUT PRICE-DAT.
       MOVE ZEROES TO PRICE-COUNT.

GET-PRICE-RECORD.
       READ PRICE-DAT   AT END GO TO CLOSE-PRICE-FILE.
       IF PRICE-COUNT = ZERO  GO TO STORE-PRICE-RECORD.

       SEARCH PRICE-TABLE   AT END GO TO STORE-PRICE-RECORD
           WHEN P-REC-MODEL = P-MODEL-NUMB (PRICE-INDX) NEXT SENTENCE.
       DISPLAY "DUPLICATE MODEL NUMBERS IN PRICE-DAT.".
       DISPLAY "CORRECT AND RE-RUN.".
       STOP RUN.

STORE-PRICE-RECORD.
       SET PRICE-COUNT UP BY 1.
       IF PRICE-COUNT > 50
           DISPLAY "NO MORE ROOM IN PRICE TABLE."
           DISPLAY "CALL TYMSHARE."
           STOP RUN.
       MOVE P-REC TO PRICE-TABLE  (PRICE-COUNT).
       GO TO GET-PRICE-RECORD.

CLOSE-PRICE-FILE.
       CLOSE PRICE-DAT.

*
*   LOAD EXPORT TABLE
*


LOAD-EXPORT-FILE.
       OPEN INPUT EXPORT-DAT.
       MOVE ZEROES TO EXPORT-COUNT.

GET-EXPORT-RECORD.
       READ EXPORT-DAT   AT END GO TO CLOSE-EXPORT-FILE.
       IF EXPORT-COUNT = ZERO  GO TO STORE-EXPORT-RECORD.

       SEARCH EXPORT-TABLE   AT END GO TO STORE-EXPORT-RECORD
           WHEN E-REC-COUNTRY = E-COUNTRY (EXPORT-INDX)  NEXT SENTENCE.
       DISPLAY "DUPLICATE MODEL NUMBERS IN EXPORT-DAT.".
       DISPLAY "CORRECT AND RE-RUN.".
       STOP RUN.

STORE-EXPORT-RECORD.
       SET EXPORT-COUNT UP BY 1.
       IF EXPORT-COUNT > 50
           DISPLAY "NO MORE ROOM IN EXPORT TABLE."
           DISPLAY "CALL TYMSHARE."
           STOP RUN.
       MOVE E-REC TO EXPORT-TABLE  (EXPORT-COUNT).
       GO TO GET-EXPORT-RECORD.

CLOSE-EXPORT-FILE.
       CLOSE EXPORT-DAT.


*
*    COPY ACCUMULATIVE FILES
*

OPEN-INSTAL-FILES.
        OPEN  INPUT INSTAL-DAT
             OUTPUT INSTAL-NEW.
COPY-INSTAL-FILES.
        READ INSTAL-DAT  AT END GO TO CLOSE-INSTAL-DAT.
        WRITE INSTAL-REC-OUT FROM INSTAL-REC-IN.
        GO TO COPY-INSTAL-FILES.
CLOSE-INSTAL-DAT.
        CLOSE INSTAL-DAT.

OPEN-CHGVAL-FILES.
        OPEN  INPUT CHGVAL-DAT
             OUTPUT CHGVAL-NEW.
COPY-CHGVAL-FILES.
        READ CHGVAL-DAT  AT END GO TO CLOSE-CHGVAL-DAT.
        WRITE CHGVAL-REC-OUT FROM CHGVAL-REC-IN.
        GO TO COPY-CHGVAL-FILES.
CLOSE-CHGVAL-DAT.
        CLOSE CHGVAL-DAT.

OPEN-SHIPED-FILES.
        OPEN  INPUT SHIPED-DAT
             OUTPUT SHIPED-NEW.
COPY-SHIPED-FILES.
        READ SHIPED-DAT  AT END GO TO CLOSE-SHIPED-DAT.
        WRITE SHIPED-REC-OUT FROM SHIPED-REC-IN.
        GO TO COPY-SHIPED-FILES.
CLOSE-SHIPED-DAT.
        CLOSE SHIPED-DAT.


*
*   OPEN FILES FOR MAIN PROCESSING
*

OPEN-OTHER-FILES.
        OPEN  INPUT ORDERS-DAT
             OUTPUT ORDERS-NEW, AUDIT-DAT, WORD-DAT.

*
*    LOAD INPUT BUFFERS.
*

        MOVE LOW-VALUES TO MAST-SEQ-CHECK.
        PERFORM READ-MASTER-REC  THRU CHECK-MAST-SEQ.

        MOVE LOW-VALUES TO TS-CHECK.
        PERFORM RETURN-A-TRANS  THRU CHECK-TRANS-SEQ.
        IF T-SORT-KEY EQUAL HIGH-VALUES  GO TO FINISH-MASTER.

*************************************************************************************
*
*    DECIDE TO LOAD FROM MASTER OR TRANSACTION
*

FORK.
       IF T-SORT-KEY = HIGH-VALUES  GO TO FINISH-MASTER.
        IF T-FORK-KEY  LESS THAN  MAST-FORK-KEY  GO TO LOAD-FROM-TRANS.
        GO TO LOAD-FROM-MASTER.


*
*    ROUTINE READ MASTER & CHECCS SEQUENCE.
*

READ-MASTER-REC.
        MOVE MAST-SEQ-CHECK TO LAST-SEQ-CHECK.
        READ ORDERS-DAT  AT END MOVE HIGH-VALUES TO ORD-REC.

        MOVE ORD-AGREE-NUMB    TO MAST-AGREE-NUMB.
        MOVE ORD-ITEM-NUMB     TO MAST-ITEM-NUMB.
        MOVE ORD-MODEL-NUMB    TO MAST-MODEL-NUMB.
        MOVE ORD-REC-TYPE      TO MAST-REC-TYPE.

CHECK-MAST-SEQ.
        IF MAST-SEQ-CHECK  LESS THAN LAST-SEQ-CHECK
            DISPLAY "CANNOT PROCEED."
            STOP RUN.
        IF MAST-SEQ-CHECK  EQUAL TO LAST-SEQ-CHECK AND MAST-REC-TYPE NOT = 'N'
            DISPLAY "DUPLICATE ENTRIES IN ORDER BASE."
            DISPLAY "CANNOT PROCEED."
            STOP RUN.


*
*    ROUTINE TO LOAD AGREEMENT/ITEM DATA FROM MASTER FILE.
*

LOAD-FROM-MASTER.
       IF MAST-AGREE-NUMB EQUAL TO LAST-AGREE-NUMB   
           GO TO LOAD-MASTER-ITEM.
       IF MAST-REC-TYPE NOT EQUAL TO 'A'  GO TO BAD-MASTER.
       MOVE ORD-REC TO A-REC.

LOAD-MAST-BILL-TO.

       PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
       IF MAST-AGREE-NUMB NOT EQUAL LAST-AGREE-NUMB
           OR MAST-REC-TYPE NOT EQUAL 'B'
           GO TO BAD-MASTER.
       MOVE ORD-REC TO BILL-TO-1.

       PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
       IF MAST-AGREE-NUMB NOT EQUAL LAST-AGREE-NUMB
           OR MAST-REC-TYPE NOT EQUAL 'C'
           GO TO BAD-MASTER.
       MOVE ORD-REC TO BILL-TO-2.

       PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
       IF MAST-AGREE-NUMB NOT EQUAL LAST-AGREE-NUMB
           OR MAST-REC-TYPE NOT EQUAL 'D'
           GO TO BAD-MASTER.
       MOVE ORD-REC TO BILL-TO-3.


PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
LOAD-MASTER-ITEM.
       IF MAST-AGREE-NUMB NOT EQUAL TO A-NUMB
           OR MAST-REC-TYPE NOT EQUAL 'I'  GO TO BAD-MASTER.
       MOVE ZEROES TO NOTE-COUNT, MODEL-COUNT.
       MOVE ORD-REC TO IT-REC.

LOAD-MAST-SHIP-TO.

       PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
       IF MAST-FORK-KEY NOT EQUAL LAST-FORK-KEY
           OR MAST-REC-TYPE NOT EQUAL 'J'
           GO TO BAD-MASTER.
       MOVE ORD-REC TO SHIP-TO-1.

       PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
       IF MAST-FORK-KEY NOT EQUAL LAST-FORK-KEY
           OR MAST-REC-TYPE NOT EQUAL 'K'
           GO TO BAD-MASTER.
       MOVE ORD-REC TO SHIP-TO-2.

       PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
       IF MAST-FORK-KEY NOT EQUAL LAST-FORK-KEY
           OR MAST-REC-TYPE NOT EQUAL 'L'
           GO TO BAD-MASTER.
       MOVE ORD-REC TO SHIP-TO-3.


PERFORM READ-MASTER-REC  THRU  CHECK-MAST-SEQ.
LOAD-NOTE-OR-MODEL.
       PERFORM READ-MASTER-REC  THRU CHECK-MAST-SEQ.
       IF MAST-FORK-KEY NOT EQUAL LAST-FORK-KEY
           GO TO MASTER-BREAK-POINT.
       IF MAST-REC-TYPE = 'N'  GO TO STORE-MASTER-NOTE.
       IF MAST-REC-TYPE = 'P'  GO TO STORE-MASTER-MODEL.
       GO TO BAD-MASTER.

STORE-MASTER-NOTE.
       ADD 1 TO NOTE-COUNT.
       MOVE ORD-REC  TO NOTE-LINE (NOTE-COUNT).
       GO TO LOAD-NOTE-OR-MODEL.

STORE-MASTER-MODEL.
       SET MODEL-COUNT UP BY 1.
       MOVE ORD-REC TO M-REC (MODEL-COUNT).
       GO TO LOAD-NOTE-OR-MODEL.

BAD-MASTER.
       DISPLAY "ORDER MASTER FILE NOT IN LOGICAL ORDER.".
       DISPLAY "CANNOT PROCEED.  CALL TYMSHARE.".
       STOP RUN.

MASTER-BREAK-POINT.
       IF MODEL-COUNT = 0  GO TO BAD-MASTER.
       GO TO CHECK-FOR-UPDATES.

*
*    ROUTINE READS TRANSACTIONS FILE & CHECKS SEQUENCE.
*

READ-TRANS-REC.
       MOVE T-SORT-KEY TO TS-CHECK.

RETURN-A-TRANS.
       RETURN SORTED-TRANS  AT END MOVE HIGH-VALUES TO T-SORT-KEY.
       IF T-SORT-KEY  LESS THAN TS-CHECK
           DISPLAY "FATAL ERROR -- SORT FAILED."
           DISPLAY "RE-TRY ONCE, THEN CALL TYMSHARE."
           STOP RUN.

CHECK-TRANS-SEQ.
       IF (T-SORT-KEY EQUAL TO TS-CHECK) AND 
           NOT (T-NOTE-REC OR T-CHANGE-REC)
           DISPLAY "REDUNDANT ENTRIES: ", TS-CHECK, " AND ", T-SORT-KEY
           PERFORM REJECT-TRANS
           GO TO RETURN-A-TRANS.

*
*    ROUTINE TO LOAD AGREEMENT/ITEM DATA FROM TRANSACTION FILE.
*
LOAD-FROM-TRANS.
       IF T-AGREE-NUMB LESS THAN A-NUMB  GO TO LOGIC-ERROR.
       IF NOT (T-NEW-ENTRY OR T-ADDITION) GO TO BAD-AGREE-OR-ITEM.
       IF T-ITEM-REC
           IF T-AGREE-NUMB EQUAL A-NUMB  GO TO STORE-TRANS-ITEM.
           ELSE                          GO TO BAD-AGREE-OR-ITEM.

       IF T-AGREE-NUMB NOT GREATER THAN A-NUMB
           DISPLAY "DUPLICATED AGREEMENT NUMBER: ",T-AGREE-NUMB
           GO TO REJECT-TRANS.
       IF NOT T-AGREE-REC
           DISPLAY "TRANACTION INCOMPLETE: ",T-SORT-KEY
           GO TO REJECT-TRANS.


NEW-AGREE-TRANS.
       MOVE 'A'                        TO A-REC-TYPE.
       MOVE W-AGREE-NUMB               TO A-NUMB
       MOVE ZEROES                     TO A-REVISION-NUMBER.
       MOVE W-AGREE-MONTH              TO HOLD-MONTH.
       MOVE W-AGREE-DAY                TO HOLD-DAY.
       MOVE W-AGREE-YEAR               TO HOLD-YEAR.
       MOVE HOLD-DATE                  TO W-AGREE-DATE.
       MOVE W-S1-1-BLOCK               TO A-BLOCK.
       MOVE ZERO                       TO IT-NUMB.

STORE-BILL-TO.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-AGREE-NUMB NOT EQUAL TS-AGREE-NUMB
           OR T-REC-TYPE NOT EQUAL 'B'
           GO TO INCOMPLETE-ENTRY.
       MOVE T-ADDR-REC TO BILL-TO-1.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-AGREE-NUMB NOT EQUAL TS-AGREE-NUMB
           OR T-REC-TYPE NOT EQUAL 'C'
           GO TO INCOMPLETE-ENTRY.
       MOVE T-ADDR-REC TO BILL-TO-2.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-AGREE-NUMB NOT EQUAL TS-AGREE-NUMB
           OR T-REC-TYPE NOT EQUAL 'D'
           GO TO INCOMPLETE-ENTRY.
       MOVE T-ADDR-REC TO BILL-TO-3.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-AGREE-NUMB NOT EQUAL A-NUMB  GO TO INCOMPLETE-ENTRY.
       IF NOT T-ITEM-REC  GO TO INCOMPLETE-ENTRY.

STORE-TRANS-ITEM.
       IF T-AGREE-NUMB NOT EQUAL A-NUMB OR NOT T-ITEM-REC
           GO TO LOGIC-ERROR.
       IF T-ITEM-NUMB NOT GREATER THAN IT-NUMB  GO TO BAD-AGREE-OR-ITEM.
       IF NOT (T-NEW-ENTRY OR T-ADDITION)
           DISPLAY "ACTIVITY NOT PERMITTED ON NEW ORDERS."
           GO TO REJECT-TRANS.
       MOVE 'I'                        TO IT-REC-TYPE.
       MOVE T-AGREE-NUMB               TO IT-AGREE-NUMB.
       MOVE W-SHIP-MONTH               TO HOLD-MONTH.
       MOVE W-SHIP-DAY                 TO HOLD-DAY.
       MOVE W-SHIP-YEAR                TO HOLD-YEAR.
       MOVE HOLD-DATE                  TO IT-SHIP-DATE.
       MOVE W-SITE-CODE                TO IT-SITE-CODE.
       MOVE W-SUB-PO                   TO IT-SUB-PO.

***** DON'T HAVE EXPORT COUNTRY HERE !

       MOVE ZEROES TO NOTE-COUNT, MODEL-COUNT.

STORE-SHIP-TO.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-FORK-KEY NOT EQUAL TS-FORK-KEY
           OR T-REC-TYPE NOT EQUAL 'J'
           GO TO INCOMPLETE-ENTRY.
       MOVE T-ADDR-REC TO SHIP-TO-1.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-FORK-KEY NOT EQUAL TS-FORK-KEY
           OR T-REC-TYPE NOT EQUAL 'K'
           GO TO INCOMPLETE-ENTRY.
       MOVE T-ADDR-REC TO SHIP-TO-2.

       PERFORM READ-TRANS-REC  THRU  CHECK-TRANS-SEQ.
       IF T-FORK-KEY NOT EQUAL TS-FORK-KEY
           OR T-REC-TYPE NOT EQUAL 'L'
           GO TO INCOMPLETE-ENTRY.
       MOVE T-ADDR-REC TO SHIP-TO-3.

GET-NEXT-TRANS.
       PERFORM READ-TRANS-REC  THRU CHECK-TRANS-SEQ.
       IF T-FORK-KEY NOT EQUAL TS-AGREE-NUMB  GO TO TRANS-BREAK-POINT.
       IF NOT (T-NEW-ENTRY OR T-ADDITION)
           DISPLAY "ACTIVITY NOT PERMITTED ON NEW ORDERS."
           GO TO REJECT-TRANS.
       IF T-NOTE-REC  GO TO STORE-TRANS-NOTE.
       IF T-MODEL-REC GO TO STORE-TRANS-MODEL.
       GO TO LOGIC-ERROR.

STORE-TRANS-NOTE.
       PERFORM ADD-NOTE-TRANS  THRU ADD-NOTE-EXIT.
       GO TO GET-NEXT-TRANS.

STORE-TRANS-MODEL.
       PERFORM ADD-MODEL-TRANS  THRU  ADD-MODEL-EXIT.
       GO TO GET-NEXT-TRANS.

LOGIC-ERROR.
        DISPLAY "POSSIBLE LOGIC ERROR OR UNANTICIPATED TRANSACTION ERROR.".
        DISPLAY "CALL TYMSHRE."
        STOP RUN.

BAD-AGREE-OR-ITEM.
       IF T-AGREE-NUMB = A-NUMB
           DISPLAY "BAD ITEM NUMBER: ", T-ITEM-NUMB
           ELSE  DISPLAY "BAD AGREEMENT NUMBER: ", T-AGREE-NUMB.
       GO TO REJECT-TRANS.
INCOMPLETE-ENTRY.
       DISPLAY "INCOMPLETE TRANSACTION FOR AGREEMENT # ",A-NUMB.

REJECT-TRANS.
       DISPLAY "TRANSACTION REJECTED: ", T-SORT-RECORD.
       DISPLAY "******"


SKIP-TO-NEXT-AGREEMENT.
       IF T-SORT-KEY = HIGH-VALUES  GO TO FINISH-MASTER.
       PERFORM READ-TRANS-REC  THRU CHECK-TRANS-SEQ.
       IF T-AGREE-NUMB EQUAL TO TS-AGREE-NUMB  GO TO REJECT-TRANS.
       GO TO FORK.

TRANS-BREAK-POINT.
       IF MODEL-COUNT EQUAL ZERO  GO TO INCOMPLETE-ENTRY.
       GO TO CHECK-FOR-UPDATES.

*
*    CHECK FOR UPDATES;  GO TO OUTPUT
*

CHECK-FOR-UPDATES.
       IF T-FORK-KEY GREATER THAN CURRENT-FORK-KEY  GO TO OUTPUT-SECTION.
       IF T-AGREE-NUMB LESS THAN CURR-AGREE-NUMB  GO TO LOGIC-ERROR.
       IF T-ITEM-NUMB LESS THAN CURR-ITEM-NUMB  AND NOT T-AGREE-REC
           GO TO LOGIC-ERROR.

*   NOTE THAT AGREEMENT NUMBERS MUST ALWAYS MATCH, WHILE
*      ITEM NUMBERS MUST MATCH IF NOT AGREEMENT RECORD.

       IF T-DELETION     GO TO HANDLE-CANCELLATION.
       IF T-ADDITION     GO TO HANDLE-ADDITION.
       IF T-CHANGE-REC   GO TO HANDLE-CHANGE.
       IF T-SHIP-REC     GO TO HANDLE-SHIPMENT.
       GO TO LOGIC-ERROR.

HANDLE-CANCELLATION.
       IF T-AGREE-REC
           MOVE 99 TO A-DELETE-FLAG
           GO TO NEXT-TRANS.
       IF T-ITEM-REC
           MOVE 99 TO IT-DELETE-FLAG
           GO TO NEXT-TRANS.
       IF T-NOTE-REC
           MOVE ZERO TO NOTE-COUNT
           GO TO NEXT-TRANS.
       IF NOT T-MODEL-REC  GO TO LOGIC-ERROR.

       SEARCH M-REC  AT END GO TO BAD-MODEL
           WHEN M-NUMB (MODEL-INDX) = T-MODEL-NUMB  NEXT SENTENCE.

MOVE-UP-MODELS.
       IF MODEL-INDX EQUAL MODEL-COUNT
           SET MODEL-COUNT DOWN BY 1
           GO TO NEXT-TRANS.
       MOVE M-REC (MODEL-INDX + 1) TO M-REC (MODEL-INDX).
       SET MODEL-INDX UP BY 1.
       GO TO MOVE-UP-MODELS.


HANDLE-ADDITION.
       IF T-NOTE-REC   PERFORM ADD-NOTE-TRANS   THRU ADD-NOTE-EXIT
           GO TO NEXT-TRANS.
       IF T-MODEL-REC  PERFORM ADD-MODEL-TRANS  THRU ADD-MODEL-EXIT
           GO TO NEXT-TRANS.
       GO TO LOGIC-ERROR.

*
*    ROUTINE TO ADD ADDITIONAL SHIPPING NOTES
*

ADD-NOTE-TRANS.
       IF NOTE-COUNT = 10
           DISPLAY "OVERFLOW ON NOTES.  CALL TYMSHARE."
           PERFORM BAD-TRANS
           GO TO ADD-NOTE-EXIT.
       SET NOTE-COUNT UP BY 1.
       MOVE W-NOTE-KEYS                TO NOTE-KEYS (NOTE-COUNT).
       MOVE W-NOTE-ALPHA               TO NOTE-ALPHA(NOTE-COUNT).

ADD-NOTE-EXIT.
       EXIT.


*
*    ROUTINE TO ADD ADDITIONAL MODELS TO ITEM
*

ADD-MODEL-TRANS.
       IF MODEL-COUNT = 25
           DISPLAY "OVERFLOW IN MODEL TABLE.  CALL TYMSHARE."
           PERFORM BAD-TRANS
           GO TO ADD-MODEL-EXIT.
       SEARCH M-REC    AT END GO TO LOOK-UP-MODEL
           WHEN W-MODEL-NUMB = M-NUMB (MODEL-INDX)   NEXT SENTENCE.
       DISPLAY "DUPLICATE ENTRIES FOR THIS MODEL."
       PERFORM BAD-TRANS.
       GO TO ADD-MODEL-EXIT.

LOOK-UP-MODEL.
*       SEARCH PRICE-TABLE   AT END NEXT SENTENCE
*           WHEN T-MODEL-NUMB = P-MODEL-NUMB (PRICE-INDX)   GO TO MODEL-OK.
       DISPLAY "BAD MODEL NUMBER: ", T-MODEL-NUMB.
       PERFORM BAD-TRANS.
       GO TO ADD-MODEL-EXIT.

MODEL-OK.
       SET MODEL-COUNT UP BY 1.
       MOVE W-MODEL-KEYS               TO M-KEY         (MODEL-INDX).
       MOVE W-MODEL-NUMB               TO M-NUMB        (MODEL-INDX).
       MOVE W-MODEL-QTY                TO M-QTY         (MODEL-INDX).
       MOVE W-MODEL-LEASE-PURCH        TO M-LEASE-PURCH (MODEL-INDX).
       MOVE W-MAINT-TOO          TO M-MAINT-TOO   (MODEL-INDX).
       MOVE P-TYPE (PRICE-INDX)        TO M-TYPE        (MODEL-INDX).
       MOVE P-LINE (PRICE-INDX)        TO M-LINE        (MODEL-INDX).


       IF W-PRICE EQUAL ZERO
           IF   W-PURCH MOVE P-PURCH-PRICE(PRICE-INDX) TO M-PRICE (MODEL-INDX)
           ELSE         MOVE P-LEASE-PRICE(PRICE-INDX) TO M-PRICE (MODEL-INDX)
       ELSE
                        MOVE W-PRICE                     TO M-PRICE (MODEL-INDX).

       IF W-MAINT-PRICE EQUAL ZERO
                MOVE P-MAINT-PRICE (PRICE-INDX) TO M-MAINT-PRICE (MODEL-INDX)
           ELSE MOVE W-MAINT-PRICE              TO M-MAINT-PRICE (MODEL-INDX).
ADD-MODEL-EXIT.
       EXIT.


*
*    HANDLE CHANGES TO EXISTING ORDERS.
*

HANDLE-CHANGE.
       IF T-AGREE-REC     GO TO CHANGE-AGREEMENT.
       IF T-ITEM-REC      GO TO CHANGE-ITEM.
       IF T-MODEL-REC     GO TO CHANGE-MODEL.
       GO TO LOGIC-ERROR.

CHANGE-AGREEMENT.
       MOVE T-CHG-AGREE TO CHG-FIELDS.
       IF CHG-FIELD-NAME = "TRIAL " MOVE CHG-VALUE-3     TO A-TRIAL-DAYS   ELSE
       IF CHG-FIELD-NAME = "LTERM " MOVE CHG-VALUE-3     TO A-LEASE-MONTHS ELSE
       IF CHG-FIELD-NAME = "CUSTPO" MOVE CHG-FIELD-VALUE TO A-PO-NUMB      ELSE
       IF CHG-FIELD-NAME = "SMAN  " MOVE CHG-VALUE-10    TO A-SALESMAN     ELSE
       IF CHG-FIELD-NAME = "ADATE " 
           MOVE CHG-MONTH-FIELD TO HOLD-MONTH
           MOVE CHG-DAY-FIELD   TO HOLD-DAY
           MOVE CHG-YEAR-FIELD  TO HOLD-YEAR
           MOVE HOLD-DATE TO A-DATE

       ELSE
           DISPLAY "BAD FIELD NAME: ", CHG-FIELD-NAME
           PERFORM BAD-TRANS.
       GO TO NEXT-TRANS.

CHANGE-ITEM.
       IF T-ITEM-NUMB NOT EQUAL IT-NUMB   GO TO LOGIC-ERROR.
       MOVE T-CHG-ITEM TO CHG-FIELDS.
       IF CHG-FIELD-NAME = "SUBPO "
           MOVE CHG-FIELD-VALUE TO IT-SUB-PO
           GO TO NEXT-TRANS.
       IF CHG-FIELD-NAME = "SDATE "
           MOVE CHG-MONTH-FIELD  TO  HOLD-MONTH
           MOVE CHG-DAY-FIELD    TO  HOLD-DAY
           MOVE CHG-YEAR-FIELD   TO  HOLD-YEAR
           MOVE HOLD-DATE TO IT-SHIP-DATE
           GO TO NEXT-TRANS.
       IF CHG-FIELD-NAME = "SITE  "   GO TO CHANGE-SITE.

CHANGE-EXPORT-INFO.
       IF CHG-FIELD-NAME NOT EQUAL TO
           ("PROFOR" OR "CREDIT" OR "IMPORT" OR "EXPORT" OR "COFC  " OR "CONSUL")
           DISPLAY "BAD FIELD NAME FOR CHANGE: ", CHG-FIELD-NAME
           PERFORM BAD-TRANS
           GO TO NEXT-TRANS.
       IF CHG-VALUE-3 NOT EQUAL TO ("YES" OR "NO " OR "REC")
           DISPLAY "BAD FIELD VALUE: ", CHG-VALUE-3
           PERFORM BAD-TRANS
           GO TO NEXT-TRANS.

       IF CHG-FIELD-NAME = "PROFOR" MOVE CHG-VALUE-1 TO IT-PRO-FORM.
       IF CHG-FIELD-NAME = "CREDIT" MOVE CHG-VALUE-1 TO IT-CREDIT-LETTER.
       IF CHG-FIELD-NAME = "IMPORT" MOVE CHG-VALUE-1 TO IT-IMPORT-DOC.
       IF CHG-FIELD-NAME = "EXPORT" MOVE CHG-VALUE-1 TO IT-EXPORT-LIC.
       IF CHG-FIELD-NAME = "COFC  " MOVE CHG-VALUE-1 TO IT-COFC-CERT.
       IF CHG-FIELD-NAME = "CONSUL" MOVE CHG-VALUE-1 TO IT-CONSUL-INV.
       GO TO NEXT-TRANS.

CHANGE-SITE.
       IF CHG-CUST-NUMB NOT = A-CUST-CODE
           DISPLAY "CUSTOMER NUMBER INCORRECT: ", CHG-CUST-NUMB
           DISPLAY "SHOULD BE: ", A-CUST-CODE
           PERFORM BAD-TRANS
           GO TO NEXT-TRANS.
       MOVE CHG-SITE-NUMB TO IT-SITE-CODE.
       GO TO NEXT-TRANS.

CHANGE-MODEL.
       IF T-ITEM-NUMB NOT EQUAL IT-NUMB  GO TO LOGIC-ERROR.
       SEARCH M-REC    AT END GO TO BAD-MODEL
           WHEN M-NUMB (MODEL-INDX) = T-MODEL-NUMB   NEXT SENTENCE.
       MOVE T-CHG-MODEL TO CHG-FIELDS.
       IF CHG-FIELD-NAME = "QUAN  " MOVE CHG-VALUE-4     TO M-QTY         (MODEL-INDX)  ELSE
       IF CHG-FIELD-NAME = "PRICE " MOVE CHG-VALUE-7     TO M-PRICE       (MODEL-INDX)  ELSE
       IF CHG-FIELD-NAME = "MCOST " MOVE CHG-VALUE-6     TO M-MAINT-PRICE (MODEL-INDX)  ELSE
       IF CHG-FIELD-NAME = "MAINT " MOVE CHG-VALUE-1     TO M-MAINT-TOO   (MODEL-INDX)  ELSE
           DISPLAY "BAD FIELD NAME: ",CHG-FIELD-NAME
           PERFORM BAD-TRANS.
       GO TO NEXT-TRANS.

*
*    RECEIVED SHIPMENT NOTIFICATION.
*

HANDLE-SHIPMENT.
       IF T-ITEM-NUMB NOT EQUAL IT-NUMB  GO TO LOGIC-ERROR.
       SEARCH M-REC    AT END GO TO BAD-MODEL
           WHEN W-MODEL-NUMB = M-NUMB (MODEL-INDX)  NEXT SENTENCE.
       MOVE W-SERIAL-NUMB              TO M-SERIAL-NUMB (MODEL-INDX).
       MOVE W-DATE-MONTH               TO HOLD-MONTH.
       MOVE W-DATE-DAY                 TO HOLD-DAY.
       MOVE W-DATE-YEAR                TO HOLD-YEAR.
       MOVE HOLD-DATE                  TO M-SHIP-DATE (MODEL-INDX).

*
*    GET NEXT TRANS
*

BAD-MODEL.
       DISPLAY "BAD MODEL NUMBER: ", W-MODEL-NUMB.

BAD-TRANS.
       DISPLAY "TRANSACTION REJECTED: ", T-SORT-RECORD.

NEXT-TRANS.
       PERFORM READ-TRANS-REC  THRU CHECK-TRANS-SEQ.
       IF T-SORT-KEY EQUAL HIGH-VALUES  GO TO OUTPUT-SECTION.
       GO TO CHECK-FOR-UPDATES.


***********************************************************************
*
*   OUTPUT SECTION 
*

OUTPUT-SECTION.

*** INSERT AUDTI & CHGVAL SECTIONS
       DISPLAY "HI".

CHECK-FOR-DELETED.
       IF A-DELETE-FLAG = 99 OR IT-DELETE-FLAG = 99 GO TO FORK.

CHECK-FOR-SHIPMENT.
       SEARCH M-REC  AT END GO TO CHECK-FOR-WORK-ORDER
           WHEN M-SERIAL-NUMB (MODEL-INDX)   NOT EQUAL SPACES  NEXT SENTENCE.
       WRITE INITIAL-REC-OUT FROM  A-REC.
       WRITE INITIAL-REC-OUT FROM IT-REC.
       SET MODEL-INDX TO ZERO.

LOOP-THRU-MODELS.
       SET MODEL-INDX UP BY 1.
       IF MODEL-INDX GREATER THAN MODEL-COUNT  GO TO CHECK-FOR-ALL.
       IF M-TYPE (MODEL-INDX) = 'P'
           MOVE TODAY TO M-SHIP-DATE (MODEL-INDX)
           GO TO LOOP-THRU-MODELS.
       IF M-SERIAL-NUMB (MODEL-INDX) EQUAL SPACES
           AND M-SHIP-DATE (MODEL-INDX) EQUAL SPACES
           DISPLAY "WARNING -- PARTIAL SHIPMENT FOR: ", M-KEY (MODEL-INDX)
           DISPLAY "           EXCLUDES MODEL NUMBER: ", M-NUMB (MODEL-INDX)
           GO TO LOOP-THRU-MODELS.

WRITE-SHIPED-NEW.
       IF M-SHIP-DATE (MODEL-INDX) = SPACES  MOVE TODAY TO M-SHIP-DATE (MODEL-INDX).
       SEARCH PRICE-TABLE  AT END GO TO LOGIC-ERROR
           WHEN M-NUMB (MODEL-INDX) = P-MODEL-NUMB (PRICE-INDX) NEXT SENTENCE.
       MOVE M-SHIP-DATE   (MODEL-INDX)   TO S-DATE.
       MOVE M-KEY         (MODEL-INDX)   TO S-KEY.
       MOVE M-NUMB        (MODEL-INDX)   TO S-MODEL-NUMB.
       MOVE M-SERIAL-NUMB (MODEL-INDX)   TO S-SERIAL-NUMB.
       MOVE P-DESC        (PRICE-INDX)   TO S-MODEL-DESC.
       MOVE A-CUST-CODE                  TO S-CUST-CODE.
       MOVE IT-SITE-CODE                 TO S-SITE-CODE.
       MOVE BILL-NAME                 TO S-NAME.
       MOVE BILL-ADDR-1                  TO S-ADDR-1.
       MOVE BILL-ADDR-2                  TO S-ADDR-2.
       MOVE BILL-CITY                    TO S-CITY.
       MOVE BILL-STATE                   TO S-STATE.
       MOVE BILL-ZIP                     TO S-ZIP.
       MOVE BILL-ATTN                    TO S-ATTN.

       WRITE SHIPED-REC-OUT.

WRITE-INSTAL-NEW.
       WRITE INSTAL-REC-OUT  FROM  A-REC.
       WRITE INSTAL-REC-OUT  FROM IT-REC.
       WRITE INSTAL-REC-OUT  FROM  M-REC (MODEL-INDX).
       GO TO LOOP-THRU-MODELS.

CHECK-FOR-ALL.
       SEARCH M-REC   AT END GO TO FORK
           WHEN M-SHIP-DTE (MODEL-INDX) EQUAL SPACES  NEXT SENTENCE.

CHECK-FOR-WORK-ORDER.
       DISPLAY "HI".

WRITE-ORDER-FILE.
       IF A-DELETE-FLAG NOT = HIGH-VALUES
           WRITE NEW-REC FROM A-REC
           MOVE HIGH-VALUES TO A-DELETE-FLAG.
       WRITE NEW-REC FROM IT-REC.
       PERFORM WRITE-MODEL   VARYING MODEL-INDX FROM 1 BY 1 
           UNTIL MODEL-INDX EQUAL MODEL-COUNT.
       GO TO FORK.

WRITE-MODEL.
       IF M-SHIP-DATE (MODEL-INDX) EQUAL SPACES
           WRITE NEW-REC FROM M-REC (MODEL-INDX).

_!`·