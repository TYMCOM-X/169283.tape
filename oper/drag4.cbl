001010 IDENTIFICATION DIVISION.                                         SALESUMB
001030 AUTHOR.  DAVID R CHRISTY.                                        SALESUMB
001020 PROGRAM-ID.  SALESU                                              SALESUMB
001040 DATE-WRITTEN.  11/25/70.                                         SALESUMB
001050 ENVIRONMENT DIVISION.                                            SALESUMB
001060 INPUT-OUTPUT SECTION.                                            SALESUMB
001070 FILE-CONTROL.                                                    SALESUMB
001080     SELECT INVOIC, ASSIGN TO DSK RECORDING MODE IS ASCII.        SALESUMB
001090     SELECT CURR, ASSIGN TO  DSK RECORDING MODE IS ASCII.         SALESUMB
001100     SELECT REPT,    ASSIGN TO  DSK RECORDING MODE IS ASCII.      SALESUMB
001110     SELECT DIST ASSIGN TO  DSK RECORDING MODE IS ASCII.          SALESUMB
002010 DATA DIVISION.                                                   SALESUMB
002020 FILE SECTION.                                                    SALESUMB
002030 FD  INVOIC   VALUE OF IDENTIFICATION IS "INVOIC   ".             SALESUMB
002060 01  CURRENT-INVOICE.                                             SALESUMB
002070       05 DATE-OF-REPORT     PICTURE X(8).                        SALESUMB
002090       05 FILLER             PICTURE X(22).                       SALESUMB
002100       05 DATA-CHK           PICTURE X(26).                       SALESUMB
002110       05 FILLER             PICTURE X(24).                       SALESUMB
004010 FD  CURR   VALUE OF IDENTIFICATION IS "CURRRRDAT".               SALESUMB
004040 01  C-T-REC.                                                     SALESUMB
004050     05 GEN-INFO.                                                 SALESUMB
004060       10 SMANS-CODE         PICTURE X.                           SALESUMB
004070       10 DIST-NO            PICTURE X(4).                        SALESUMB
004080       10 PART-NO            PICTURE X(14).                       SALESUMB
004090       10 INV-NO             PICTURE 9(5).                        SALESUMB
004100     05  DATE.                                                    SALESUMB
004110       10 MO                 PICTURE XX.                          SALESUMB
004120       10 DAY                PICTURE XX.                          SALESUMB
004130       10 YR                 PICTURE XX.                          SALESUMB
004135     05 FILLER               PICTURE XX.                          SALESUMB
004140     05 UNIT-COST            PICTURE S9(4)V99 USAGE COMPUTATIONAL.SALESUMB
004150     05 EXT-GROSS            PICTURE S9(6)V99 USAGE COMPUTATIONAL.SALESUMB
004160     05 UNIT-PRICE           PICTURE S9(5)V99 USAGE COMPUTATIONAL.SALESUMB
004170     05 DISCOUNTS  USAGE COMPUTATIONAL.                           SALESUMB
004180       10 DISC-1             PICTURE S99V9.                       SALESUMB
004190       10 DISC-2             PICTURE S99V9.                       SALESUMB
004200       10 DISC-3             PICTURE S99V9.                       SALESUMB
004210     05 QTY                  PICTURE S9(4)    USAGE COMPUTATIONAL.SALESUMB
004220     05 TOT-NET-SALE-1       PICTURE S9(6)V99 USAGE COMPUTATIONAL.SALESUMB
004230     05 DIR-DRP-CODE         PICTURE X.                           SALESUMB
004235     05 FILLER               PICTURE XXX.                         SALESUMB
005010 FD  REPT   VALUE OF IDENTIFICATION IS "REPTTTDAT".               SALESUMB
005040 01  BAL-TRANS-OUT.                                               SALESUMB
005050     05 GENRL-INFO.                                               SALESUMB
005060       10 FILLER             PICTURE X(5).                        SALESUMB
005070       10 SMANS-CODE         PICTURE X.                           SALESUMB
005080       10 DIST-NO            PICTURE X(4).                        SALESUMB
005090       10 FILLER             PICTURE X(5).                        SALESUMB
005100       10 PART-NO            PICTURE X(14).                       SALESUMB
005110       10 FILLER             PICTURE X(3).                        SALESUMB
005120       10 INV-NO             PICTURE Z9(4).                       SALESUMB
005130       10 FILLER             PICTURE X(5).                        SALESUMB
005140     05 DATE.                                                     SALESUMB
005150       10 MO                 PICTURE XX.                          SALESUMB
005160       10 SEP-1              PICTURE X.                           SALESUMB
005170       10 DAY                PICTURE XX.                          SALESUMB
005180       10 SEP-2              PICTURE X.                           SALESUMB
005190       10 YR                 PICTURE XX.                          SALESUMB
005200       10 FILLER             PICTURE X(3).                        SALESUMB
005210     05 UNIT-COST            PICTURE Z,ZZ9.99.                    SALESUMB
005220     05 FILLER               PICTURE X(3).                        SALESUMB
006010     05 EXT-GROSS            PICTURE ZZZ,ZZ9.99-.                 SALESUMB
006020     05 FILLER               PICTURE X(4).                        SALESUMB
006030     05 UNIT-PRICE           PICTURE ZZ,ZZ9.99.                   SALESUMB
006040     05 FILLER               PICTURE X(5).                        SALESUMB
006050     05 DISCOUNTS.                                                SALESUMB
006060       10 DISC-1             PICTURE 990.                         SALESUMB
006070       10 SEP-3              PICTURE X.                           SALESUMB
006080       10 DISC-2             PICTURE 990.                         SALESUMB
006090       10 SEP-4              PICTURE X.                           SALESUMB
006100       10 DISC-3             PICTURE 990.                         SALESUMB
006110       10 FILLER             PICTURE X(2).                        SALESUMB
006120     05 QTY                  PICTURE Z,ZZ9-.                      SALESUMB
006130     05 FILLER               PICTURE X(5).                        SALESUMB
006140     05 TOT-NET-SALE-2       PICTURE ZZZ,ZZ9.99-.                 SALESUMB
006160 01  INV-TOTALS-OUT.                                              SALESUMB
006170     05 FILLER               PICTURE X(25).                       SALESUMB
006180     05 LINE-LABEL-1         PICTURE X(36).                       SALESUMB
006190     05 COST-TOTAL           PICTURE ZZ,ZZZ,ZZ9.99-.              SALESUMB
006200     05 AST-1                PICTURE X(2).                        SALESUMB
006210     05 FILLER               PICTURE X(27).                       SALESUMB
006220     05 QTY-TOTAL            PICTURE ZZZ,ZZ9-.                    SALESUMB
006230     05 FILLER               PICTURE X(2).                        SALESUMB
006240     05 NET-SALE-TOTAL       PICTURE ZZ,ZZZ,ZZ9.99-.              SALESUMB
006250     05 AST-2                PICTURE X(2).                        SALESUMB
006270 FD  DIST   VALUE OF IDENTIFICATION IS "DISTTD   ".               SALESUMB
006310 01  DIST-ID-ENTRY.                                               SALESUMB
006320       05 DIST-NO-ID         PICTURE X(4).                        SALESUMB
006330       05 DIST-NAME          PICTURE X(24).                       SALESUMB
006340       05 FILLER             PICTURE X(28).                       SALESUMB
006350       05 ITEM-3             PICTURE S9(5) COMPUTATIONAL.         SALESUMB
007010 WORKING-STORAGE SECTION.                                         SALESUMB
007015   77 I                     PICTURE S99 COMPUTATIONAL.            SALESUMB
007040   77 PAGE-CTR          PICTURE S99 VALUE 1 USAGE COMPUTATIONAL.  SALESUMB
007050   77 SKIP-TO-NEXT-PAGE      PICTURE 9      VALUE   1.            SALESUMB
007060   77 LINE-CTR          PICTURE S99         USAGE COMPUTATIONAL.  SALESUMB
007080   77 OLD-INV-NO             PICTURE 9(5).                        SALESUMB
007090   77 GROSS-CST-INV-TOT   PICTURE S9(8)V99 VALUE 0 COMPUTATIONAL. SALESUMB
007100   77 GROSS-CST-GRAND-TOT PICTURE S9(8)V99 VALUE 0 COMPUTATIONAL. SALESUMB
007110   77 NET-SALE-INV-TOT    PICTURE S9(8)V99 VALUE 0 COMPUTATIONAL. SALESUMB
007120   77 NET-SALE-GRAND-TOT  PICTURE S9(8)V99 VALUE 0 COMPUTATIONAL. SALESUMB
007130   77 QTY-INV-TOT         PICTURE S9(6)    VALUE 0 COMPUTATIONAL. SALESUMB
007140   77 QTY-GRAND-TOT       PICTURE S9(6)    VALUE 0 COMPUTATIONAL. SALESUMB
007150   77 ASTER                  PICTURE XX          VALUE '**'.      SALESUMB
008010 01  MAST-REPORT-HD.                                              SALESUMB
008020       05 FILLER             PICTURE X(8)   VALUE SPACES.         SALESUMB
008030       05 FILLER             PICTURE X(24)  VALUE                 SALESUMB
008040                        'DRAGON VALVES, INC.     '.               SALESUMB
008050       05 FILLER             PICTURE X(11)  VALUE SPACES.         SALESUMB
008060       05 FILLER             PICTURE X(20)  VALUE                 SALESUMB
008070                        '** SALES ANALYSIS **'.                   SALESUMB
008080       05 FILLER             PICTURE X(8)   VALUE SPACES.         SALESUMB
008090       05 FILLER             PICTURE X(12)  VALUE 'REPORT BAL  '. SALESUMB
008100       05 FILLER             PICTURE X(6)   VALUE SPACES.         SALESUMB
008110       05 FILLER             PICTURE X(7)   VALUE 'AS OF  '.      SALESUMB
008120       05 REPT-DATE          PICTURE X(8).                        SALESUMB
008130       05 FILLER             PICTURE X(12)  VALUE SPACES.         SALESUMB
008140       05 FILLER             PICTURE X(10)  VALUE 'PAGE NO. '.    SALESUMB
008150       05 PAGE-NO            PICTURE Z9.                          SALESUMB
009010 01  REPORT-SUB-HD-1.                                             SALESUMB
009020       05 FILLER             PICTURE X(6)   VALUE SPACES.         SALESUMB
009030       05 FILLER             PICTURE X(5)   VALUE 'DIST.'.        SALESUMB
009040       05 FILLER             PICTURE X(7)   VALUE SPACES.         SALESUMB
009050       05 FILLER             PICTURE X(4)   VALUE 'PART'.         SALESUMB
009060       05 FILLER             PICTURE X(10)  VALUE SPACES.         SALESUMB
009070       05 FILLER             PICTURE X(7)   VALUE 'INVOICE'.      SALESUMB
009080       05 FILLER             PICTURE X(6)   VALUE SPACES.         SALESUMB
009090       05 FILLER             PICTURE X(4)   VALUE 'DATE'.         SALESUMB
009100       05 FILLER             PICTURE X(5)   VALUE SPACES.         SALESUMB
009110       05 FILLER             PICTURE X(24)  VALUE                 SALESUMB
009120                      '- - - - -COST- - - - -  '.                 SALESUMB
009125       05 FILLER             PICTURE X(5) VALUE SPACES.           SALESUMB
009130       05 FILLER             PICTURE X(47)  VALUE                 SALESUMB
009140               '- - - - - - - - - -SALES- - - - - - - - - - - -'. SALESUMB
010010 01  REPORT-SUB-HD-2.                                             SALESUMB
010020       05 FILLER             PICTURE X(7)   VALUE SPACES.         SALESUMB
010030       05 FILLER             PICTURE X(3)   VALUE 'NO.'.          SALESUMB
010040       05 FILLER             PICTURE X(7)   VALUE SPACES.         SALESUMB
010050       05 FILLER             PICTURE X(6)   VALUE 'NUMBER'.       SALESUMB
010060       05 FILLER             PICTURE X(9)   VALUE SPACES.         SALESUMB
010070       05 FILLER             PICTURE X(6)   VALUE 'NUMBER'.       SALESUMB
010080       05 FILLER             PICTURE X(17)  VALUE SPACES.         SALESUMB
010100       05 FILLER             PICTURE X(9)   VALUE 'U/PRICE'.      SALESUMB
010110       05 FILLER             PICTURE X(2)   VALUE SPACES.         SALESUMB
010120       05 FILLER             PICTURE X(9)   VALUE 'EXT GROSS'.    SALESUMB
010130       05 FILLER             PICTURE X(7)   VALUE SPACES.         SALESUMB
010140       05 FILLER             PICTURE X(7)   VALUE 'U/PRICE'.      SALESUMB
010150       05 FILLER             PICTURE X(5)   VALUE SPACES.         SALESUMB
010160       05 FILLER             PICTURE X(8)   VALUE 'DISCOUNT'.     SALESUMB
010170       05 FILLER             PICTURE X(6)   VALUE SPACES.         SALESUMB
010180       05 FILLER             PICTURE X(3)   VALUE 'QTY'.          SALESUMB
010190       05 FILLER             PICTURE X(4)   VALUE SPACES.         SALESUMB
010200       05 FILLER             PICTURE X(13)  VALUE 'TOT NET SALES'.SALESUMB
010210 01  BILLER.                                                      SALESUMB
010220       05 D-NO       OCCURS 75 TIMES PICTURE X(4).                SALESUMB
010230 01  CILLER.                                                      SALESUMB
010240       05 D-NAM      OCCURS 75 TIMES PICTURE X(24).               SALESUMB
010300 01  CURR-INVOICE.                                                SALESUMB
010310     05 GEN-INFO.                                                 SALESUMB
010320       10 SMANS-CODE         PICTURE X.                           SALESUMB
010330       10 DIST-NO            PICTURE X(4).                        SALESUMB
010340       10 PART-NO            PICTURE X(14).                       SALESUMB
010350       10 INV-NO             PICTURE 9(5).                        SALESUMB
010360     05 DATE.                                                     SALESUMB
010370       10 MO                 PICTURE XX.                          SALESUMB
010380       10 DAY                PICTURE XX.                          SALESUMB
010390       10 YR                 PICTURE XX.                          SALESUMB
010400     05 UNIT-COST            PICTURE 9(4)V99.                     SALESUMB
010410     05 UNIT-PRICE           PICTURE 9(5)V99.                     SALESUMB
010420     05 DISCOUNTS.                                                SALESUMB
010430       10 DISC-1-IN          PICTURE 99V9.                        SALESUMB
010440       10 DISC-2-IN          PICTURE 99V9.                        SALESUMB
010450       10 DISC-3-IN          PICTURE 99V9.                        SALESUMB
010460     05 QTY                  PICTURE 9(4).                        SALESUMB
010470     05 FILLER               PICTURE X(3).                        SALESUMB
010480     05 DIR-DROP-CODE        PICTURE X.                           SALESUMB
010490         88 DIRCT                           VALUE SPACE.          SALESUMB
010500         88 DROP                            VALUE '1'.            SALESUMB
010510     05 DEBIT-CREDIT-CODE    PICTURE X.                           SALESUMB
010520         88 DEBIT                           VALUE SPACE.          SALESUMB
010530         88 CREDIT                          VALUE '1'.            SALESUMB
010540     05 FILLER               PICTURE X(19).                       SALESUMB
010560 PROCEDURE DIVISION.                                              SALESUMB
010570   GET-DIST-ID-ARRAY.                                             SALESUMB
010580     OPEN INPUT DIST.                                             SALESUMB
010590       MOVE ZERO TO I.                                            SALESUMB
010600   LOAD-ID-ARRAY.                                                 SALESUMB
010610     READ DIST, AT END GO TO CLOSE-DIST.                          SALESUMB
010620         ADD 1 TO I.                                              SALESUMB
010630         MOVE DIST-NO-ID TO D-NO (I).                             SALESUMB
010640         MOVE DIST-NAME TO D-NAM (I).                             SALESUMB
010650     IF  ITEM-3 GREATER THAN 99990, GO TO CLOSE-DIST.             SALESUMB
010660       IF I LESS THAN 75, GO TO LOAD-ID-ARRAY, ELSE MOVE SPACES   SALESUMB
010670         TO BAL-TRANS-OUT, WRITE BAL-TRANS-OUT AFTER              SALESUMB
010690              SKIP-TO-NEXT-PAGE, DISPLAY                          SALESUMB
010700              'ERROR    DIST GT 75 ENTRIES',                      SALESUMB
010710              PERFORM CLOSE-DIST, STOP RUN.                       SALESUMB
010720   CLOSE-DIST.                                                    SALESUMB
010730     CLOSE DIST.                                                  SALESUMB
011020   PREP-FOR-REPORT-BAL.                                           SALESUMB
011030     OPEN INPUT INVOIC, OUTPUT CURR,                              SALESUMB
011040          REPT.                                                   SALESUMB
011050     READ INVOIC, AT END GO TO QUIT.                              SALESUMB
011060     MOVE DATE-OF-REPORT TO REPT-DATE.                            SALESUMB
011080   WRITE-HEADINGS.                                                SALESUMB
011090     MOVE PAGE-CTR TO PAGE-NO.                                    SALESUMB
011100     WRITE BAL-TRANS-OUT FROM MAST-REPORT-HD AFTER                SALESUMB
011110         SKIP-TO-NEXT-PAGE.                                       SALESUMB
011120     WRITE BAL-TRANS-OUT FROM REPORT-SUB-HD-1 AFTER 2 LINES.      SALESUMB
011130     WRITE BAL-TRANS-OUT FROM REPORT-SUB-HD-2 AFTER 1 LINES.      SALESUMB
011140     MOVE SPACES TO BAL-TRANS-OUT.                                SALESUMB
011150     WRITE BAL-TRANS-OUT AFTER 1 LINES.                           SALESUMB
011160     MOVE ZEROS TO LINE-CTR.                                      SALESUMB
011170     ADD 1 TO PAGE-CTR.                                           SALESUMB
011180   PREPARE-CURRENT-TRANSACTIONS.                                  SALESUMB
011190     READ INVOIC, AT END GO TO QUIT.                              SALESUMB
011195       PERFORM DATA-TEST THRU DUM-EXIT.                           SALESUMB
011200     MOVE INV-NO IN CURR-INVOICE TO OLD-INV-NO.                   SALESUMB
011210   TEST-FOR-VALID-DIST-NO.                                        SALESUMB
011220       MOVE 1 TO I.                                               SALESUMB
011230   NEXT-ID-NO.                                                    SALESUMB
011240     IF D-NO (I) EQUAL TO DIST-NO IN CURR-INVOICE GO TO           SALESUMB
011250         SET-UP-C-T-REC.                                          SALESUMB
01    IF D-NO (I) NOT GREATER THAN DIST-NO IN CURR-INVOICE,        SALESUMB
011270         ADD 1 TO I, GO TO NEXT-ID-NO.                            SALESUMB
011280       MOVE '****' TO DIST-NO IN CURR-INVOICE.                    SALESUMB
012010   SET-UP-C-T-REC.                                                SALESUMB
012020     IF CREDIT, COMPUTE QTY IN C-T-REC = - QTY IN CURR-INVOICE,   SALESUMB
012030         ELSE MOVE QTY IN CURR-INVOICE TO QTY IN C-T-REC.         SALESUMB
012040     MOVE GEN-INFO IN CURR-INVOICE TO GEN-INFO IN C-T-REC.        SALESUMB
012050     MOVE DATE IN CURR-INVOICE TO DATE IN C-T-REC.                SALESUMB
012060     MOVE UNIT-COST IN CURR-INVOICE TO UNIT-COST IN C-T-REC.      SALESUMB
012070     COMPUTE EXT-GROSS IN C-T-REC         = QTY IN C-T-REC *      SALESUMB
012080         UNIT-COST IN C-T-REC.                                    SALESUMB
012090     MOVE UNIT-PRICE IN CURR-INVOICE TO UNIT-PRICE IN C-T-REC.    SALESUMB
012100       MOVE DISC-1-IN TO DISC-1 IN C-T-REC.                       SALESUMB
012102       MOVE DISC-2-IN TO DISC-2 IN C-T-REC.                       SALESUMB
012104       MOVE DISC-3-IN TO DISC-3 IN C-T-REC.                       SALESUMB
012110     COMPUTE TOT-NET-SALE-1 ROUNDED = (1.0 - DISC-1-IN / 100.0) * SALESUMB
012120       (1.0 - DISC-2-IN / 100.0) * (1.0 - DISC-3-IN / 100.0) *    SALESUMB
012130         UNIT-PRICE IN C-T-REC * QTY IN C-T-REC.                  SALESUMB
012140     MOVE DIR-DROP-CODE TO DIR-DRP-CODE.                          SALESUMB
012150   TEST-FOR-INVOICE-CHANGE.                                       SALESUMB
012160     IF INV-NO IN CURR-INVOICE IS EQUAL TO OLD-INV-NO,            SALESUMB
012170         GO TO DO-SUB-TOTALS.                                     SALESUMB
012180   SUB-TOTALS-OUT.                                                SALESUMB
012190     MOVE SPACES TO INV-TOTALS-OUT.                               SALESUMB
012200     MOVE 'INVOICE TOTALS' TO LINE-LABEL-1.                       SALESUMB
012210     MOVE GROSS-CST-INV-TOT TO COST-TOTAL.                        SALESUMB
013010     MOVE ASTER TO AST-1, AST-2.                                  SALESUMB
013020     MOVE QTY-INV-TOT TO QTY-TOTAL.                               SALESUMB
013030     MOVE NET-SALE-INV-TOT TO NET-SALE-TOTAL.                     SALESUMB
013040   ADD-TO-GRAND-TOTALS.                                           SALESUMB
013050     ADD GROSS-CST-INV-TOT TO GROSS-CST-GRAND-TOT.                SALESUMB
013060     ADD NET-SALE-INV-TOT TO NET-SALE-GRAND-TOT.                  SALESUMB
013070     ADD QTY-INV-TOT TO QTY-GRAND-TOT.                            SALESUMB
013080   CLEAR-INV-TOTALS.                                              SALESUMB
013090     MOVE ZEROS TO GROSS-CST-INV-TOT, NET-SALE-INV-TOT,           SALESUMB
013100         QTY-INV-TOT.                                             SALESUMB
013110     WRITE INV-TOTALS-OUT AFTER ADVANCING 1 LINES.                SALESUMB
013120     MOVE SPACES TO INV-TOTALS-OUT.                               SALESUMB
013130     WRITE INV-TOTALS-OUT AFTER ADVANCING 1 LINES.                SALESUMB
013140     MOVE INV-NO IN CURR-INVOICE TO OLD-INV-NO.                   SALESUMB
013150     ADD 2 TO LINE-CTR.                                           SALESUMB
013160   DO-SUB-TOTALS.                                                 SALESUMB
013170     ADD EXT-GROSS IN C-T-REC TO GROSS-CST-INV-TOT.               SALESUMB
013180     ADD QTY IN C-T-REC TO QTY-INV-TOT.                           SALESUMB
013190     ADD TOT-NET-SALE-1 TO NET-SALE-INV-TOT.                      SALESUMB
013200     IF LINE-CTR GREATER THAN 52 PERFORM WRITE-HEADINGS.          SALESUMB
013210   SET-UP-PRINT-REC.                                              SALESUMB
013215       MOVE SPACES TO BAL-TRANS-OUT.                              SALESUMB
013220     MOVE SMANS-CODE IN C-T-REC TO SMANS-CODE IN BAL-TRANS-OUT.   SALESUMB
014010     MOVE DIST-NO IN C-T-REC TO DIST-NO IN BAL-TRANS-OUT.         SALESUMB
014020     MOVE PART-NO IN C-T-REC TO PART-NO IN BAL-TRANS-OUT.         SALESUMB
014030     MOVE INV-NO IN C-T-REC TO INV-NO IN BAL-TRANS-OUT.           SALESUMB
014040     MOVE MO IN C-T-REC TO MO IN BAL-TRANS-OUT.                   SALESUMB
014050     MOVE '/' TO SEP-1, SEP-2.                                    SALESUMB
014060     MOVE DAY IN C-T-REC TO DAY IN BAL-TRANS-OUT.                 SALESUMB
014070     MOVE YR IN C-T-REC TO YR IN BAL-TRANS-OUT.                   SALESUMB
014080     MOVE UNIT-COST IN C-T-REC TO UNIT-COST IN BAL-TRANS-OUT.     SALESUMB
014090     MOVE EXT-GROSS IN C-T-REC TO EXT-GROSS IN BAL-TRANS-OUT.     SALESUMB
014100     MOVE UNIT-PRICE IN C-T-REC TO UNIT-PRICE IN BAL-TRANS-OUT.   SALESUMB
014110     MOVE DISC-1 IN C-T-REC TO DISC-1 IN BAL-TRANS-OUT.           SALESUMB
014120     MOVE '-' TO SEP-3, SEP-4.                                    SALESUMB
014130     MOVE DISC-2 IN C-T-REC TO DISC-2 IN BAL-TRANS-OUT.           SALESUMB
014140     MOVE DISC-3 IN C-T-REC TO DISC-3 IN BAL-TRANS-OUT.           SALESUMB
014150     MOVE QTY IN C-T-REC TO QTY IN BAL-TRANS-OUT.                 SALESUMB
014160     MOVE TOT-NET-SALE-1 TO TOT-NET-SALE-2.                       SALESUMB
014170   OUTPUT-BAL-TRANS-AND-C-T-REC.                                  SALESUMB
014180     WRITE BAL-TRANS-OUT AFTER ADVANCING 1 LINES.                 SALESUMB
014190     MOVE SPACES TO BAL-TRANS-OUT.                                SALESUMB
014200     ADD 1 TO LINE-CTR.                                           SALESUMB
014210     WRITE C-T-REC.                                               SALESUMB
014220     MOVE SPACES TO C-T-REC.                                      SALESUMB
015010   GET-NEXT-RECORD.                                               SALESUMB
015020     READ INVOIC, AT END PERFORM SUB-TOTALS-OUT THRU              SALESUMB
015030         CLEAR-INV-TOTALS, GO TO OUTPUT-GRAND-TOTALS.             SALESUMB
015035       PERFORM DATA-TEST THRU DUM-EXIT.                           SALESUMB
015040     GO TO TEST-FOR-VALID-DIST-NO.                                SALESUMB
015050   OUTPUT-GRAND-TOTALS.                                           SALESUMB
015060     MOVE 'FINAL TOTALS - CURRENT' TO LINE-LABEL-1.               SALESUMB
015070     MOVE GROSS-CST-GRAND-TOT TO COST-TOTAL.                      SALESUMB
015080     MOVE ASTER TO AST-1, AST-2.                                  SALESUMB
015090     MOVE NET-SALE-GRAND-TOT TO NET-SALE-TOTAL.                   SALESUMB
015100     MOVE QTY-GRAND-TOT TO QTY-TOTAL.                             SALESUMB
015110     WRITE INV-TOTALS-OUT AFTER ADVANCING 3 LINES.                SALESUMB
015120   QUIT.                                                          SALESUMB
015130     CLOSE REPT, CURR, INVOIC.                                    SALESUMB
015140     STOP RUN.                                                    SALESUMB
015150   DATA-TEST.                                                     SALESUMB
015160     EXAMINE DATA-CHK REPLACING ALL ' ' BY '0'.                   SALESUMB
016015     MOVE CURRENT-INVOICE TO CURR-INVOICE.                        SALESUMB
016020     IF UNIT-COST IN CURR-INVOICE NOT NUMERIC MOVE ZEROS TO       SALESUMB
016030         UNIT-COST IN CURR-INVOICE.                               SALESUMB
016040     IF UNIT-PRICE IN CURR-INVOICE NOT NUMERIC MOVE ZEROS TO      SALESUMB
016050         UNIT-PRICE IN CURR-INVOICE.                              SALESUMB
016060     IF DISC-1-IN NOT NUMERIC MOVE ZEROS TO DISC-1-IN.            SALESUMB
016070     IF DISC-2-IN NOT NUMERIC MOVE ZEROS TO DISC-2-IN.            SALESUMB
016080     IF DISC-3-IN NOT NUMERIC MOVE ZEROS TO DISC-3-IN.            SALESUMB
016090     IF QTY IN CURR-INVOICE NOT NUMERIC MOVE ZEROS TO             SALESUMB
016100         QTY IN CURR-INVOICE.                                     SALESUMB
016140   DUM-EXIT.  EXIT.                                               SALESUMB

  / h™