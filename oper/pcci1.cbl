       IDENTIFICATION DIVISION.
       PROGRAM-ID. WSSA40.
       AUTHOR. BILL MORRISSEY.
       INSTALLATION. PCCI.
       REMARKS. THIS PROGRAM UPDATES THE DAILY SALES MASTER FILE
                WITH THE CURRENT MONTHS TRANSACTIONS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. CENTURY-200.
       OBJECT-COMPUTER. CENTURY-200, MEMORY SIZE 32768 CHARACTERS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      FPLANFILE  0001P1108002                     S
      FMASTERIN  1452S20080FN0400NNSCR      MASTERIN            M01UN
      FDSMMASTER 1452S23250FN3250  SCR      DSMMASTER           M02UN
      FSAMASTER  1452D23250FN3250  SCR      SAMASTER            M03UN
      FDISCTEMP  1322D20018FN0504  WM    002DISCTEMP
      F          23
      F          3D01101      0200
      FPRINTFILE 623 P01136            060
           SELECT PLANFILE ASSIGN TO NCR682-100.
           SELECT PRINTFILE ASSIGN TO NCR640-200.
           SELECT MASTERIN ASSIGN TO NCR633-119.
           SELECT DSMMASTER ASSIGN TO NCR633-119.
           SELECT SAMASTER  ASSIGN TO NCR633-119.
           SELECT DISCTEMP ASSIGN TO NCR655-201.
       DATA DIVISION.
       FILE SECTION.
       FD  DISCTEMP
           LABEL RECORD IS STANDARD.
       01  DISC-REC.
           03  D-STORE         PIC 99.
           03  D-MG            PIC 99.
           03  D-DEPT          PIC 999.
           03  D-TRANS         PIC S9999999V99 COMP-3.
           03  D-AMOUNT        PIC S999999999V99 COMP-3.
       FD  PLANFILE
           LABEL RECORD IS OMITTED.
       01  PLAN-REC.
           03  P-STORE         PIC 99.
           03  P-INDEX OCCURS 7 TIMES INDEXED  BY PIN.
               05  P-FIELD     PIC 999.
               05  P-AMOUNT    PIC S99999999.
       01  DATE-REC.
           03  MO-RUN     PIC 99.
           03  FILLER              PIC X.
           03  LY-JUL              PIC S9(3).
           03  FILLER              PIC 9(2).
           03  DAYS-IN-MONTH       PIC S9(2).
           03  FILLER              PIC X(37).
           03  AST                 PIC XX.
           03  FILLER              PIC X(31).
       01  CHANGE-REC.
           03  C-TRANS    PIC 99.
           03  C-COMP.
               05  C-STORE    PIC 99.
               05  C-MG       PIC 99.
               05  C-DEPT      PIC 999.
           03  C-MO     PIC 99.
           03  C-NO       PIC S9(7).
           03  C-AMOUNT       PIC S9(9)V99.
       FD  SAMASTER
           LABEL RECORD IS STANDARD.
       01  OUT-REC.
           03  X-COMP.
               05  X-STORE         PIC 99.
               05  X-MER           PIC 99.
               05  X-DEPT          PIC 999.
           03  X-LY OCCURS 366 TIMES INDEXED BY INX.
               05  XT-TOT      PIC S99999  COMP-3.
               05  XT-AMOUNT   PIC S9(7)V99    COMP-3.
           03  X-YTD           PIC S9(9)V99    COMP-3.
           03  XT-YTD          PIC S9(7)       COMP-3.
           03  PX-YTD          PIC S9(9)V99    COMP-3.
           03  PXT-YTD         PIC S9(7)       COMP-3.
           03  X-PER OCCURS 12 TIMES INDEXED BY PX.
               05  XP-ID       PIC S99V9.
           03  X-RET OCCURS 12 TIMES INDEXED BY RETX.
               05  XR-TRANS      PIC S99999   COMP-3.
               05  XR-AMOUNT       PIC S9(7)V99     COMP-3.
           03  LY-RET OCCURS 12 TIMES INDEXED BY LYRX.
               05  LYX-TRANS     PIC S99999   COMP-3.
               05  LYX-AMOUNT     PIC S9(7)V99    COMP-3.
           03  FILLER       PIC X(67).
       01  P-OUT.
           03  PL-COMP.
               05  PL-STORE    PIC 99.
               05  PL-MER      PIC 99.
               05  PL-DEPT     PIC 999.
           03  PL-IN OCCURS 366 TIMES INDEXED BY PLIN.
               05  PL-ID       PIC S9(8).
           03  FILLER          PIC X(315).
       FD  DSMMASTER
           LABEL RECORD IS STANDARD.
       01  TRANS-REC.
           03  T-COMP.
               05  STORE           PIC 99.
               05  MERCH-GRP       PIC 99.
               05  DEPT            PIC 999.
           03  T-LY OCCURS 366 TIMES INDEXED BY TX.
               05  TY-ID           PIC S9(5)       COMP-3.
               05  TY-AMT          PIC S9(7)V99    COMP-3.
           03  T-YTD               PIC S9(9)V99    COMP-3.
           03  XTYTD              PIC S9(7)       COMP-3.
           03  XYTD               PIC S9(9)V99    COMP-3.
           03  TX-YTD              PIC S9(7)       COMP-3.
           03  T-PER OCCURS 12 TIMES INDEXED BY TPX.
               05  PER-ID          PIC S99V9.
           03  XO-RET OCCURS 12 TIMES INDEXED BY ROX.
               05  XO-TRANS     PIC S99999   COMP-3.
               05  XO-AMOUNT       PIC S9(7)V99   COMP-3.
           03  LYXO-RET OCCURS 12 TIMES INDEXED BY LYOX.
               05  LYXO-TRANS     PIC S99999  COMP-3.
               05  LYXO-AMOUNT     PIC S9(7)V99   COMP-3.
           03  FILLER     PIC X(67).
       01  TPLAN-REC.
           03  TP-COMP.
               05  STORE-A         PIC 99.
               05  MER-A           PIC 99.
               05  DEPT-A          PIC 999.
           03  TP-LY OCCURS 366 TIMES INDEXED BY TPLX.
               04  TP-X.
               05  TP-ID           PIC 9(8).
           03  FILLER              PIC X(315).
       FD  MASTERIN
           LABEL RECORD IS STANDARD.
       01  NEW-REC.
           03  MAST-REC.
               05  TRAN-CD-N       PIC 99.
               05  JUL-DATE-N      PIC 999.
               05  STORE-N         PIC 99.
               05  DEPT-N          PIC 999.
               05  MER-GRP-N       PIC 99.
               05  TRANS-C-N       PIC S9(5).
               05  TRANS-AMT-N     PIC S9(5)V99.
               05  FILLER          PIC X(56).
       FD  PRINTFILE
           LABEL RECORD IS OMITTED.
       01  PRINT-REC.
           03  P-TRANS-AREA        PIC X(80).
           03  ERROR-AREA          PIC X(52).
       WORKING-STORAGE SECTION.
       01  E-COMP.
           03  E-STORE             PIC 99.
           03  E-MERCH-GRP         PIC 99.
           03  E-DEPT              PIC 999.
       01  SWITCH-P                PIC 9       VALUE ZERO.
       01  STORED-DATE             PIC  999.
       01  STORED-DA-MO            PIC  99.
       01  SWITCHX                 PIC 9       VALUE ZERO.
       01  E-COMP-STORE.
           03  EC-STORE            PIC 99.
           03  EC-MER              PIC 99.
           03  EC-DEPT             PIC 999.
       01  Z-COMP              PIC 9(7).
       01  Y-COMP              PIC 9(7).
       01  DAY-MO              PIC 99.
       01 F-STORE              PIC 99       VALUE ZERO.
       01  IN-COUNT            PIC 99.
       PROCEDURE DIVISION.
       OPEN-SECTION.
           OPEN INPUT PLANFILE, MASTERIN, DSMMASTER.
           OPEN OUTPUT SAMASTER, DISCTEMP, PRINTFILE.
           READ PLANFILE AT END GO TO EOF-CARDS.
           IF AST NOT EQUAL :**: DISPLAY :NO DATE CARD - RESTART RUN:
               STOP RUN.
           MOVE DATE-REC TO OUT-REC  WRITE OUT-REC
           MOVE LY-JUL TO STORED-DATE
           MOVE DAYS-IN-MONTH TO DAY-MO.
           MOVE DAY-MO TO STORED-DA-MO
           READ DSMMASTER AT END GO TO END-MAST.
           READ DSMMASTER AT END GO TO END-MAST.
       READ-TRANS.
           READ MASTERIN AT END GO TO END-TRANS.
           IF MER-GRP-N EQ 90 OR 35 OR 30
               GO TO READ-TRANS.
           IF TRAN-CD-N EQ 99
               GO TO READ-TRANS.
           READ PLANFILE AT END GO TO FIRST-EOF.
           GO TO CHECK-UP.
       READ-DSM.
           READ DSMMASTER AT END GO TO END-MAST.
       CHECK-UP.
           IF F-STORE EQ 99 GO TO CHECK-COMP.
           IF DEPT-A EQ ZEROS
               PERFORM PUT-PLAN THRU PUT-PLAN-EXIT.
       CHECK-COMP.
           MOVE STORE-N TO E-STORE.
           MOVE DEPT-N TO E-DEPT.
           MOVE MER-GRP-N TO E-MERCH-GRP.
           IF T-COMP GT E-COMP
           MOVE LOW-VALUES TO OUT-REC
            MOVE E-COMP TO E-COMP-STORE
               PERFORM PSEUDO-REC
               PERFORM PUT-TRANSACTION THRU PUT-TRANSACTION-EXIT
               GO TO CHECK-UP.
           IF T-COMP LT E-COMP
               PERFORM PUT-LY-MONTH THRU SET-LY-EXIT
               PERFORM PUT-MASTER THRU PUT-MASTER-EXIT
           PERFORM READ-MAS
               GO TO CHECK-UP.
               PERFORM PUT-LY-MONTH THRU SET-LY-EXIT.
           MOVE LOW-VALUES TO OUT-REC.
           PERFORM PUT-TRANS THRU PUT-TRANS-EXIT.
           GO TO CHECK-UP.
       READ-MAS.
           READ DSMMASTER AT END GO TO END-MAST.
       PUT-PLAN.
            SET TPLX TO 1.
           PERFORM TP-ID-FIX 366 TIMES.
       PUT-PLAN-1.
           IF P-STORE LT STORE-A
               PERFORM PLAN-ERROR THRU PLAN-ERROR-EXIT
               GO TO PUT-PLAN-2.
           IF P-STORE GT STORE-A AND SWITCH-P EQ 1
               GO TO RESET-P.
           IF P-STORE GT STORE-A
               GO TO PUT-PLAN-EXIT.
           SET PIN TO 1.
       RE-CYCLE.
           IF P-FIELD (PIN) EQ SPACES
               GO TO PUT-PLAN-2.
           SET TPLX TO P-FIELD (PIN).
           MOVE P-AMOUNT (PIN) TO TP-ID (TPLX).
           MOVE 1 TO SWITCH-P.
           SET PIN UP BY 1.
           GO TO RE-CYCLE.
       PUT-PLAN-2.
           READ PLANFILE AT END GO TO EOF-CARDS.
           GO TO PUT-PLAN-1.
       RESET-P.
           WRITE P-OUT FROM TPLAN-REC.
           READ DSMMASTER AT END GO TO END-MAST.
           MOVE ZERO TO SWITCH-P.
       PUT-PLAN-EXIT. EXIT.
       TP-ID-FIX.
           IF TP-X (TPLX) EQ LOW-VALUES
               MOVE ZEROS TO TP-ID (TPLX).
           SET TPLX UP BY 1.
       PUT-MASTER.
           PERFORM CHECK-UPDATE.
           WRITE OUT-REC FROM TRANS-REC.
           MOVE ZEROS TO SWITCHX.
       PUT-MASTER-EXIT.
       PUT-LY-MONTH.
           SET LYOX, ROX TO MO-RUN.
           MOVE XO-TRANS (ROX) TO LYXO-TRANS (LYOX).
           MOVE XO-AMOUNT (ROX) TO LYXO-AMOUNT (LYOX).
           MOVE ZEROS TO XO-TRANS (ROX).
           MOVE ZEROS TO XO-AMOUNT (ROX).
           MOVE ZEROS TO D-TRANS, D-AMOUNT.
           MOVE DAY-MO TO STORED-DA-MO.
           SUBTRACT 1 FROM STORED-DA-MO.
           SET TX TO STORED-DATE.
       RE-DO.
           IF STORED-DA-MO EQ ZERO
               GO TO SET-LY.
           ADD TY-ID (TX) TO D-TRANS.
           ADD TY-AMT (TX) TO D-AMOUNT.
           MOVE ZEROS TO TY-ID (TX).
           MOVE ZEROS TO TY-AMT (TX).
           SUBTRACT 1 FROM STORED-DA-MO.
           SET TX UP BY 1.
           GO TO RE-DO.
       SET-LY.
           MOVE STORE TO D-STORE.
           MOVE MERCH-GRP TO D-MG.
           MOVE DEPT TO D-DEPT.
           WRITE DISC-REC.
           ADD D-TRANS TO XT-YTD.
           ADD D-AMOUNT TO X-YTD.
           MOVE 1 TO SWITCHX.
           MOVE DAY-MO TO STORED-DA-MO.
           MOVE E-COMP TO E-COMP-STORE.
       SET-LY-EXIT. EXIT.
       PUT-TRANS.
           PERFORM CHECK-UPDATE.
           SET TX TO STORED-DATE.
       PUT-TRANS-A.
           ADD 1 TO IN-COUNT.
           IF IN-COUNT GT STORED-DA-MO
               GO TO PUT-TRANS-1.
           MOVE ZEROS TO TY-ID (TX), TY-AMT (TX).
           SET TX UP BY 1.
           GO TO PUT-TRANS-A.
       PUT-TRANS-1.
           IF E-COMP NOT EQUAL E-COMP-STORE GO TO PUT-REC.
           IF JUL-DATE-N EQ 61 OR JUL-DATE-N GT 61
               SET TX TO JUL-DATE-N
               SET TX DOWN BY 1
           GO TO SET-IT-UP.
           SET TX TO JUL-DATE-N.
       SET-IT-UP.
           IF TRAN-CD-N EQ 02
           SET ROX TO MO-RUN
           ADD TRANS-C-N TO XO-TRANS (ROX)
           ADD TRANS-AMT-N TO XO-AMOUNT (ROX)
           GO TO READ-TRANS-2.
           IF TRAN-CD-N EQ 01
           ADD TRANS-C-N   TO TY-ID (TX)
           ADD TRANS-AMT-N TO TY-AMT (TX).
       READ-TRANS-2.
           READ MASTERIN AT END GO TO END-PUT.
           IF TRAN-CD-N EQ 99
               GO TO READ-TRANS-2.
           IF MER-GRP-N EQ 90 OR 35 OR 30
               GO TO READ-TRANS-2.
           MOVE STORE-N TO E-STORE.
           MOVE DEPT-N TO E-DEPT.
           MOVE MER-GRP-N TO E-MERCH-GRP.
           GO TO PUT-TRANS-1.
       PUT-REC.
           WRITE OUT-REC FROM TRANS-REC.
           READ DSMMASTER AT END GO TO END-MAST.
           MOVE E-COMP TO E-COMP-STORE.
           MOVE ZEROS TO SWITCHX.
           MOVE ZEROS TO IN-COUNT.
       PUT-TRANS-EXIT. EXIT.
       PSEUDO-REC.
           MOVE E-STORE TO D-STORE.
           MOVE E-MERCH-GRP TO D-MG.
           MOVE E-DEPT TO D-DEPT.
           MOVE ZEROS TO D-TRANS, D-AMOUNT.
           WRITE DISC-REC.
       TRANS-ERROR.
           MOVE NEW-REC TO P-TRANS-AREA.
           MOVE :THIS TRANSACTION HAS NO MATCHING RECORDS ON MASTER:
               TO ERROR-AREA.
           WRITE PRINT-REC AFTER 1.
           MOVE SPACES TO PRINT-REC.
       TRANS-ERROR-EXIT. EXIT.
       CHECK-UPDATE.
           IF C-COMP NOT EQ E-COMP
               GO TO CHECK-UPDATE-EXIT.
           IF C-TRANS EQ 55
               MOVE C-NO TO TX-YTD
           MOVE C-AMOUNT TO XYTD.
           IF C-TRANS EQ 56
               MOVE C-NO TO XTYTD
               MOVE C-AMOUNT TO T-YTD.
           IF C-TRANS EQ 57
               SET ROX TO C-MO
               MOVE C-NO TO XO-TRANS (ROX)
               MOVE C-AMOUNT TO XO-AMOUNT (ROX).
           IF C-TRANS EQ 58
               SET LYOX TO C-MO
               MOVE C-NO TO LYXO-TRANS (LYOX)
               MOVE C-AMOUNT TO LYXO-AMOUNT (LYOX).
           READ PLANFILE AT END
               MOVE :99: TO F-STORE
               GO TO CHECK-UPDATE-EXIT.
       CHECK-UPDATE-EXIT. EXIT.
       PUT-TRANSACTION.
           IF C-COMP NOT EQ E-COMP
               GO TO PUT-TRANSACTION1.
           IF C-TRANS EQ 55
               MOVE C-NO TO PXT-YTD
               MOVE C-AMOUNT TO PX-YTD.
           IF C-TRANS EQ 56
               MOVE C-NO TO XT-YTD
               MOVE C-AMOUNT TO X-YTD.
           IF C-TRANS EQ 57
               SET RETX TO C-MO
               MOVE C-NO TO XR-TRANS (RETX)
               MOVE C-AMOUNT TO XR-AMOUNT (RETX).
            IF C-TRANS EQ 58
               SET LYRX TO C-MO
               MOVE C-NO TO LYX-TRANS (LYRX)
               MOVE C-AMOUNT TO LYX-AMOUNT (LYRX).
           READ PLANFILE AT END
               MOVE :99: TO F-STORE
               GO TO PUT-TRANSACTION1.
       PUT-TRANSACTION1.
           IF E-COMP NOT EQ E-COMP-STORE
               GO TO PUT-EREC.
           IF JUL-DATE-N EQ 61 OR JUL-DATE-N GT 61
               SET INX TO JUL-DATE-N
               SET INX DOWN BY 1
               GO TO SET-COUNT.
            SET INX TO JUL-DATE-N.
       SET-COUNT.
           IF TRAN-CD-N EQ TO 02
           SET RETX TO MO-RUN
           ADD TRANS-C-N TO XR-TRANS (RETX)
            ADD TRANS-AMT-N TO XR-AMOUNT (RETX)
           GO TO READ-T.
           IF TRAN-CD-N EQ 01
           ADD TRANS-AMT-N TO XT-AMOUNT (INX)
           ADD TRANS-C-N TO XT-TOT (INX).
       READ-T.
           READ MASTERIN AT END GO TO PUT-TRAN-END.
            IF TRAN-CD-N EQ 99
               GO TO READ-T.
            IF MER-GRP-N EQ 90 OR 35 OR 30
               GO TO READ-T.
           MOVE MER-GRP-N TO E-MERCH-GRP.
           MOVE DEPT-N TO E-DEPT.
           MOVE STORE-N TO E-STORE.
           GO TO PUT-TRANSACTION1.
       PUT-EREC.
           MOVE EC-STORE TO X-STORE.
           MOVE EC-DEPT TO X-DEPT.
           MOVE EC-MER TO X-MER.
           WRITE OUT-REC.
       PUT-TRANSACTION-EXIT. EXIT.
       PLAN-ERROR.
           MOVE PLAN-REC TO P-TRANS-AREA.
           MOVE :PLAN TRANSACTION HAS NO MATCHING RECORDS ON MASTER:
               TO ERROR-AREA.
           WRITE PRINT-REC AFTER 1.
           MOVE SPACES TO PRINT-REC.
       PLAN-ERROR-EXIT.
       EOF-CARDS.
           WRITE P-OUT FROM TPLAN-REC.
       FIRST-EOF.
           MOVE 99 TO F-STORE.
            READ DSMMASTER AT END GO TO END-MAST.
            IF T-COMP EQ ZEROS
               GO TO FIRST-EOF.
               GO TO CHECK-COMP.
       END-TRANS.
           IF Z-COMP EQ 9999999
               CLOSE PLANFILE, MASTERIN, DSMMASTER,
                     SAMASTER, DISCTEMP, PRINTFILE
           STOP RUN.
           MOVE 9999999 TO Y-COMP.
       READ-END-MAST.
           READ DSMMASTER AT END GO TO END-MAST.
           PERFORM PUT-MASTER THRU PUT-MASTER-EXIT.
           PERFORM PUT-LY-MONTH THRU SET-LY-EXIT.
           GO TO READ-END-MAST.
       END-MAST.
           IF Y-COMP EQ 9999999
               CLOSE PLANFILE, MASTERIN, DSMMASTER
                     SAMASTER, DISCTEMP, PRINTFILE
           STOP RUN.
           MOVE 9999999 TO Z-COMP.
       READ-END-TRANS.
           READ MASTERIN AT END GO TO END-TRANS.
           IF TRAN-CD-N EQ 99
               GO TO READ-END-TRANS.
           MOVE E-COMP TO E-COMP-STORE.
           MOVE LOW-VALUES TO OUT-REC.
           PERFORM PUT-TRANSACTION THRU PUT-TRANSACTION-EXIT.
           GO TO READ-END-TRANS.
       PUT-TRAN-END.
           MOVE EC-STORE TO X-STORE.
           MOVE EC-DEPT TO X-DEPT.
           MOVE EC-MER TO X-MER.
           WRITE OUT-REC.
           GO TO END-TRANS.
       END-PUT.
           WRITE OUT-REC FROM TRANS-REC.
           GO TO END-TRANS.
 