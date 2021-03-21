000100 IDENTIFICATION DIVISION.                                         PSE23105
000200 PROGRAM-ID.                                                      PSE23105
000300     ^PSC23105^.
000400 DATE-WRITTEN.                                                    PSE23105
000500     OCTOBER 1970.                                                PSE23105
000600 DATE-COMPILED.                                                   PSE23105
000700     OCTOBER 1970.                                                PSE23105
000800 REMARKS.                                                         PSE23105
000900     THIS PROGRAM IS TO COMPUTE THE PERCENTAGE OF DISTRIBUTION    PSE23105
001000     FOR GRADES 1 THRU 12 AND THE PERCENTAGE OF GROWTH FOR        PSE23105
001100     EACH GRADE FROM A PRIOR PERIOD  MAY BE 1 THRU 99 YEARS       PSE23105
001200     INDICATES THE NUMBER IN THE CURRENT RECORD AND THE AMOUNT    PSE23105
001300     OF INCREASE FROM THE PRIOR PERIOD OF TIME.                   PSE23105
001400                                                                  PSE23105
001500 ENVIRONMENT DIVISION.                                            PSE23105
001600 CONFIGURATION SECTION.                                           PSE23105
001700 SOURCE-COMPUTER.                                                 PSE23105
001800     IBM-360.                                                     PSE23105
001900 OBJECT-COMPUTER.                                                 PSE23105
002000     IBM-360.                                                     PSE23105
002100                                                                  PSE23105
002200 INPUT-OUTPUT SECTION.                                            PSE23105
002300 FILE-CONTROL.                                                    PSE23105
002400                                                                  PSE23105
002500     SELECT CARD-IN ASSIGN TO ^READER^ UTILITY.                   PSE23105
002600                                                                  PSE23105
002700     SELECT PRINT-LINE ASSIGN TO ^PRINTER^ UTILITY.               PSE23105
002800                                                                  PSE23105
002900 DATA DIVISION.                                                   PSE23105
003000 FILE SECTION.                                                    PSE23105
003100                                                                  PSE23105
003200 FD  CARD-IN                                                      PSE23105
003300     RECORDING MODE IS F                                          PSE23105
003400     RECORD CONTAINS 80 CHARACTERS                                PSE23105
003500     BLOCK CONTAINS 0 RECORDS                                     PSE23105
003600     LABEL RECORDS ARE STANDARD                                   PSE23105
003700     DATA RECORDS ARE CARDS-IN, CARD-2.                           PSE23105
003800                                                                  PSE23105
003900 01  CARDS-IN.                                                    PSE23105
004000     05  ALPHA-CARD          PICTURE X]80[.                       PSE23105
004100     05  NUM-CARD REDEFINES ALPHA-CARD.                           PSE23105
004200         10  CARD-AMT       PICTURE S9]6[ OCCURS 13 TIMES.        PSE23105
004300         10  CARD-YR         PICTURE X]2[.                        PSE23105
004400         10  CARD-YR-RED REDEFINES CARD-YR  PICTURE 9]2[.         PSE23105
004500                                                                  PSE23105
004600 01  CARD-2.                                                      PSE23105
004700     10  CARD-IND            PICTURE X.                           PSE23105
004800     10  FILLER              PICTURE X.                           PSE23105
004900     10  COUNTY-NAME         PICTURE X]25[.                       PSE23105
005000     10  FILLER              PICTURE X]53[.                       PSE23105
005100                                                                  PSE23105
005200 FD  PRINT-LINE                                                   PSE23105
005300     RECORDING MODE IS F                                          PSE23105
005400     RECORD CONTAINS 133 CHARACTERS                               PSE23105
005500     BLOCK CONTAINS 0 RECORDS                                     PSE23105
005600     LABEL RECORDS ARE STANDARD                                   PSE23105
005700     DATA RECORD IS PRINT.                                        PSE23105
005800                                                                  PSE23105
005900 01  PRINT.                                                       PSE23105
006000     05  FILLER              PICTURE X.                           PSE23105
006100     05  PRINT-1             PICTURE X]132[.                      PSE23105
006200                                                                  PSE23105
006300 WORKING-STORAGE SECTION.                                         PSE23105
006400                                                                  PSE23105
006500 77  SAVE-YEAR               PICTURE X]2[    VALUE SPACES.        PSE23105
006600 77  TIME                    PICTURE 9]8[.                        PSE23105
006700 77  JDATE                   PICTURE 9]5[.                        PSE23105
006800 77  STDATE                  PICTURE 9]6[.                        PSE23105
006900 77  CARD-SUBSCRIPT          PICTURE 9]2[    VALUE ZEROS.         PSE23105
007000 77  TOTAL-STUDENTS         PICTURE S9]7[    VALUE ZEROS.         PSE23105
007100 77  FIRST-CARD-SW           PICTURE X       VALUE ^0^.           PSE23105
007200 77  GROWTH                 PICTURE S9]6[.                        PSE23105
005120 77  GROWTH-PERCENTAGE       PICTURE S999V99999.
007400 77  CLASS-NUMBER            PICTURE 99.                          PSE23105
007500 77  ZERO-GROWTH             PICTURE 9]6[V99 VALUE 000000.00.     PSE23105
007600 77  TOTAL-GROWTH           PICTURE S9]7[    VALUE ZEROES.        PSE23105
007700 77  CARD-CTR                PICTURE 9]3[    VALUE ZEROES.        PSE23105
007800 77  SAVE-TOTAL             PICTURE S9]7[    VALUE ZEROES.        PSE23105
007900 77  TOT-GROWTH-TOTAL       PICTURE S9]7[    VALUE ZEROES.        PSE23105
008000 77  TOTAL-GR-PCT           PICTURE S999V99999.                   PSE23105
008100 77  SAVE-BIRTHS            PICTURE S9]6[    VALUE ZEROES.        PSE23105
005210 77  GROWTH-DIFF             PICTURE S9]6[   VALUE ZEROES.
008300 77  SAVE-BIRTH-YR           PICTURE 99.                          PSE23105
008400 77  COHORT-CTR             PICTURE S99      VALUE ZEROES.        PSE23105
005220 77  COHORT-DIFF             PICTURE S9]6[   VALUE ZEROES.
008600 77  TOT-COHORT-NO           PICTURE S9]7[   VALUE ZEROES.        PSE23105
008700 77  TOT-COHORT-PCT         PICTURE S999V9999.                    PSE23105
008800 77  CO-SUB-OLD              PICTURE 99      VALUE ZEROES.        PSE23105
008900 77  CO-SUB-NEW              PICTURE 99      VALUE ZEROES.        PSE23105
009000 77  TOTAL-COHORT            PICTURE S9]7[   VALUE ZEROES.        PSE23105
009100 77  TOTAL-NEW-COHORT       PICTURE S9]7[    VALUE ZEROES.        PSE23105
009200 77  TOTAL-COHORT-GROWTH    PICTURE S9]7[    VALUE ZEROES.        PSE23105
009300 77  TOTAL-COHORT-PCT       PICTURE S999V9999.                    PSE23105
009400 77  TOTAL-OLD-COHORT        PICTURE S9]7[   VALUE ZEROES.        PSE23105
005270 77  TOT-STUDENT-9-12        PICTURE S9]7[   VALUE ZEROES.
005280 77  TOT-STUDENT-1-8         PICTURE S9]7[   VALUE ZEROES.
005290 77  TOT-DIST-PCT            PICTURE S999V99 VALUE ZEROES.
005300 77  TOT-GR-NO               PICTURE S9]7[   VALUE ZEROES.
005310 77  TOT-GR-PCT              PICTURE S999V9999 VALUE ZEROES.
005320 77  TOT-CO-NO               PICTURE S9]7[   VALUE ZEROES.
005330 77  TOT-CO-PCT              PICTURE S999V9999 VALUE ZEROES.
005340 77  GR-PERCENT              PICTURE S999V99 VALUE ZEROES.
005350 77  GR-DIFF                 PICTURE S9]6[   VALUE ZEROES.
005360 77  SUB-COHORT-TOTAL        PICTURE S9]7[   VALUE ZEROES.
009500                                                                  PSE23105
009600 01  HEADER-1.                                                    PSE23105
009700     05  FILLER              PICTURE X       VALUE SPACES.        PSE23105
009800     05  FILLER              PICTURE X]10[   VALUE SPACES.        PSE23105
009900     05  FILLER              PICTURE X]2[    VALUE ^19^.          PSE23105
010000     05  PR-YEAR             PICTURE X]2[    VALUE SPACES.        PSE23105
010100     05  FILLER              PICTURE X]12[   VALUE                PSE23105
010200     ^  ENROLLMENT^.                                              PSE23105
010300     05  FILLER              PICTURE X]10[   VALUE SPACES.        PSE23105
010400     05  CTY-NAME            PICTURE X]50[.                       PSE23105
010500     05  FILLER              PICTURE X]46[   VALUE SPACES.        PSE23105
010600                                                                  PSE23105
010700 01  HEADER-2.                                                    PSE23105
010800     05  FILLER              PICTURE X       VALUE SPACES.        PSE23105
010900     05  FILLER              PICTURE X]31[   VALUE SPACES.        PSE23105
011000     05  FILLER              PICTURE X]23[   VALUE                PSE23105
011100     ^PERCENT          GROWTH^.                                   PSE23105
011200     05  FILLER              PICTURE X]18[   VALUE SPACES.        PSE23105
011300     05  FILLER              PICTURE X]13[ VALUE ^COHORT CHANGE^. PSE23105
011400     05  FILLER              PICTURE X]47[   VALUE SPACES.        PSE23105
011500                                                                  PSE23105
011600 01  HEADER-3.                                                    PSE23105
011700     05  FILLER              PICTURE X       VALUE SPACES.        PSE23105
011800     05  FILLER              PICTURE X]17[   VALUE SPACES.        PSE23105
011900     05  FILLER              PICTURE X]42[   VALUE                PSE23105
012000     ^NUMBER     DISTRIBUTION    NUMBER  PERCENT^.                PSE23105
012100     05  FILLER              PICTURE X]12[   VALUE SPACES.        PSE23105
012200     05  FILLER              PICTURE X]16[   VALUE                PSE23105
012300     ^NUMBER   PERCENT^.                                          PSE23105
012400     05  FILLER              PICTURE X]45[   VALUE SPACES.        PSE23105
012500                                                                  PSE23105
012600 01  DETAIL-LINE-1.                                               PSE23105
012700     05  FILLER              PICTURE X       VALUE SPACES.        PSE23105
012800     05  FILLER              PICTURE X]10[   VALUE ^BIRTHS ]19^.  PSE23105
012900     05  BIRTH-YR            PICTURE 99.                          PSE23105
013000     05  FILLER              PICTURE X       VALUE ^[^.           PSE23105
013100     05  FILLER              PICTURE X]3[    VALUE SPACES.        PSE23105
013200     05  NO-OF-BIRTHS        PICTURE ZZZ,ZZ9.                     PSE23105
013300     05  FILLER              PICTURE X]20[   VALUE SPACES.        PSE23105
013400     05  GR-PLUS-OR-MINUS    PICTURE X       VALUE SPACES.        PSE23105
013500     05  GR-NO               PICTURE ZZZ,ZZ9.                     PSE23105
013600     05  FILLER              PICTURE X]2[    VALUE SPACES.        PSE23105
013700     05  PLUS-OR-MINUS       PICTURE X       VALUE SPACES.        PSE23105
013800     05  GR-PCT              PICTURE ZZZ.99.                      PSE23105
013900     05  FILLER              PICTURE X]73[   VALUE SPACES.        PSE23105
014000                                                                  PSE23105
014100 01  DETAIL-LINE-2.                                               PSE23105
014200     05  FILLER              PICTURE X       VALUE SPACES.        PSE23105
014300     05  FILLER              PICTURE X]6[    VALUE ^GRADE ^.      PSE23105
014400     05  GRADE-NO            PICTURE ZZ.                          PSE23105
014500     05  FILLER              PICTURE X]8[    VALUE SPACES.        PSE23105
014600     05  GR-NUMBER           PICTURE ZZZ,ZZ9.                     PSE23105
014610     05  FILLER              PICTURE X       VALUE SPACES.
014620     05  ASTERISK-2          PICTURE X]2[    VALUE SPACES.
014630     05  FILLER              PICTURE X]5[    VALUE SPACES.
014800     05  DIST-PERCENT        PICTURE ZZZ.99.                      PSE23105
014900     05  FILLER              PICTURE X]6[    VALUE SPACES.        PSE23105
015000     05  GROWTH-NO-MINUS     PICTURE X       VALUE SPACES.        PSE23105
015100     05  GROWTH-NO           PICTURE ZZZ,ZZ9.                     PSE23105
015200     05  FILLER              PICTURE X]2[    VALUE SPACES.        PSE23105
015300     05  MINUS-GROWTH        PICTURE X       VALUE SPACES.        PSE23105
015400     05  GROWTH-PERCENT      PICTURE ZZZ.99.                      PSE23105
015500     05  FILLER              PICTURE X]10[   VALUE SPACES.        PSE23105
015600     05  COHORT-NO-MINUS     PICTURE X       VALUE SPACES.        PSE23105
015700     05  COHORT-NO           PICTURE ZZZ,ZZZ.                     PSE23105
015800     05  FILLER              PICTURE X]2[    VALUE SPACES.        PSE23105
015900     05  COHORT-PCT-MINUS    PICTURE X       VALUE SPACES.        PSE23105
016000     05  COHORT-PERCENT      PICTURE ZZZ.ZZ.                      PSE23105
016100     05  FILLER              PICTURE X]46[   VALUE SPACES.        PSE23105
016200                                                                  PSE23105
016300 01  TOTAL-LINE.                                                  PSE23105
016400     05  FILLER              PICTURE X       VALUE SPACES.        PSE23105
009040     05  TOTAL-CLASS-NOS     PICTURE X]14[   VALUE
016600     ^TOTAL 1-12    ^.                                            PSE23105
016700     05  TOTAL-NO            PICTURE Z,ZZZ,ZZ9.                   PSE23105
016800     05  FILLER              PICTURE X]8[    VALUE SPACES.        PSE23105
009070     05  DIST-PCT-PT         PICTURE ZZZ.ZZ.
017000     05  FILLER              PICTURE X]4[    VALUE SPACES.        PSE23105
017100     05  TOTAL-MINUS         PICTURE X       VALUE SPACES.        PSE23105
017200     05  TOTAL-NO-GROWTH     PICTURE Z,ZZZ,ZZ9.                   PSE23105
017300     05  FILLER              PICTURE X]2[    VALUE SPACES.        PSE23105
017400     05  TOT-MINUS           PICTURE X       VALUE SPACES.        PSE23105
017500     05  TOTAL-PERCENT       PICTURE ZZZ.99.                      PSE23105
017600     05  FILLER              PICTURE X]8[    VALUE SPACES.        PSE23105
017700     05  CO-TOT-MINUS        PICTURE X       VALUE SPACES.        PSE23105
017800     05  CO-TOT-NO           PICTURE Z,ZZZ,ZZZ.                   PSE23105
017900     05  FILLER              PICTURE X]2[    VALUE SPACES.        PSE23105
018000     05  CO-PCT-MINUS        PICTURE X       VALUE SPACES.        PSE23105
018100     05  CO-TOT-PCT          PICTURE ZZZ.ZZ.                      PSE23105
018200     05  FILLER              PICTURE X]46[   VALUE SPACES.        PSE23105
009190
009200 01  TOTAL-LINE-RED REDEFINES TOTAL-LINE.
009210     10  FILLER              PICTURE X]133[.
018300                                                                  PSE23105
018400 01  STANDARD-DATE.                                               PSE23105
018500     05  YEAR-IN             PICTURE X]2[    VALUE SPACES.        PSE23105
018600     05  MONTH-IN            PICTURE X]2[    VALUE SPACES.        PSE23105
018700     05  DAY-IN              PICTURE X]2[    VALUE SPACES.        PSE23105
018800                                                                  PSE23105
018900 01  SAVE-CARD.                                                   PSE23105
019000     05  CARD-AMOUNT        PICTURE S9]6[  OCCURS 13 TIMES.       PSE23105
019100     05  CARD-YEAR           PICTURE X]2[.                        PSE23105
019200                                                                  PSE23105
019300 PROCEDURE DIVISION.                                              PSE23105
019400                                                                  PSE23105
019500 100-START.                                                       PSE23105
019600     OPEN INPUT CARD-IN.                                          PSE23105
019700     OPEN OUTPUT PRINT-LINE.                                      PSE23105
019800                                                                  PSE23105
019900 110-CALL-TIMEDATE.                                               PSE23105
020000     ENTER LINKAGE.                                               PSE23105
020100     CALL ^TIMEDATE^ USING TIME JDATE STDATE.                     PSE23105
020200     ENTER COBOL.                                                 PSE23105
020300     MOVE STDATE TO STANDARD-DATE.                                PSE23105
020400     MOVE SPACES TO CTY-NAME.                                     PSE23105
020500                                                                  PSE23105
020600 120-READ-FIRST-CARD.                                             PSE23105
013021     MOVE ZEROES TO TOT-STUDENT-9-12.
013022     MOVE ZEROES TO TOT-STUDENT-1-8.
013023     MOVE ZEROES TO TOT-DIST-PCT.
013024     MOVE ZEROES TO TOT-GR-NO.
013025     MOVE ZEROES TO TOT-GR-PCT.
013026     MOVE ZEROES TO TOT-CO-NO.
013027     MOVE ZEROES TO TOT-CO-PCT.
013028     MOVE SPACES TO TOTAL-LINE-RED.
020700     READ CARD-IN AT END GO TO 900-END-OF-JOB.                    PSE23105
020800     IF CARD-IND ' ^D^                                            PSE23105
020900         MOVE COUNTY-NAME TO CTY-NAME                             PSE23105
021000         MOVE ZEROES TO COHORT-CTR                                PSE23105
021100         GO TO 120-READ-FIRST-CARD.                               PSE23105
021200     TRANSFORM ACARD FROM SPACES TO ZEROES.                  PSE23105
021300     MOVE CARD-YR TO SAVE-YEAR                                    PSE23105
021400     PERFORM 500-HEADING-ROUTINE THRU 510-HEADER-EXIT.            PSE23105
021500                                                                  PSE23105
021600     NOTE *** TO ARRIVE AT TOTAL 1-12 SUBSCRIPT THRU CARD AMOUNT  PSE23105
021700          OF INPUT CARD 12 TIMES THRU A PERFORM STATEMENT ***.    PSE23105
021800                                                                  PSE23105
021900     PERFORM 520-TOTAL-AMT THRU 530-TOTAL-EXIT.                   PSE23105
022000                                                                  PSE23105
022100     NOTE *** PRINT FIRST DETAIL LINE ***.                        PSE23105
022200                                                                  PSE23105
022300     MOVE 01 TO CARD-SUBSCRIPT.                                   PSE23105
022400     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO NO-OF-BIRTHS.              PSE23105
022500     MOVE ZERO-GROWTH TO GR-NO.                                   PSE23105
022600     MOVE ZERO-GROWTH TO GR-PCT.                                  PSE23105
022700     SUBTRACT 06 FROM CARD-YR-RED GIVING SAVE-BIRTH-YR.           PSE23105
022800     MOVE SAVE-BIRTH-YR TO BIRTH-YR.                              PSE23105
022900     WRITE PRINT FROM DETAIL-LINE-1 AFTER 2.                      PSE23105
023000     MOVE ^1^ TO FIRST-CARD-SW.                                   PSE23105
023100     ADD 1 TO CARD-CTR.                                           PSE23105
023200     MOVE CARDS-IN TO SAVE-CARD.                                  PSE23105
023300                                                                  PSE23105
023400 130-CONTINUE-1ST-CARD.                                           PSE23105
023500     MOVE 02 TO CARD-SUBSCRIPT.                                   PSE23105
023600     MOVE 00 TO CLASS-NUMBER.                                     PSE23105
023700                                                                  PSE23105
023800 140-MOVE.                                                        PSE23105
023900     ADD 01 TO CLASS-NUMBER.                                      PSE23105
024000     MOVE CLASS-NUMBER TO GRADE-NO.                               PSE23105
024100     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER.                 PSE23105
024110     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES
024120     MOVE ^**^ TO ASTERISK-2
024130         ELSE MOVE SPACES TO ASTERISK-2.
024200                                                                  PSE23105
024300 150-COMPUTE-DISTRIBUTION.                                        PSE23105
024400     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES                        PSE23105
024500         MOVE ZERO-GROWTH TO DIST-PERCENT                         PSE23105
024600         GO TO 160-PRINT-FIRST-CARD.                              PSE23105
024700                                                                  PSE23105
024800     IF TOTAL-STUDENTS ' ZEROES                                   PSE23105
024900         AND CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES                   PSE23105
025000         MOVE ZERO-GROWTH TO DIST-PERCENT                         PSE23105
025200                                                                  PSE23105
025100         GO TO 160-PRINT-FIRST-CARD.                              PSE23105
025300     DIVIDE TOTAL-STUDENTS INTO CARD-AMT ]CARD-SUBSCRIPT[         PSE23105
025400         GIVING GROWTH-PERCENTAGE ROUNDED.                        PSE23105
025500         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
025600             GIVING GROWTH-PERCENTAGE                             PSE23105
025700                                                                  PSE23105
025800     MOVE GROWTH-PERCENTAGE TO DIST-PERCENT.                      PSE23105
015055     ADD GROWTH-PERCENTAGE TO TOT-DIST-PCT.
025900                                                                  PSE23105
026000 160-PRINT-FIRST-CARD.                                            PSE23105
026100     MOVE ZERO-GROWTH TO GROWTH-NO.                               PSE23105
026200     MOVE ZERO-GROWTH TO GROWTH-PERCENT.                          PSE23105
026300     MOVE SPACES TO GROWTH-NO-MINUS                               PSE23105
026400     MOVE SPACES TO MINUS-GROWTH                                  PSE23105
026500     MOVE ZERO-GROWTH TO COHORT-NO.                               PSE23105
026600     MOVE ZERO-GROWTH TO COHORT-PERCENT.                          PSE23105
026700     MOVE SPACES TO COHORT-NO-MINUS.                              PSE23105
026800     MOVE SPACES TO COHORT-PCT-MINUS.                             PSE23105
026900     WRITE PRINT FROM DETAIL-LINE-2 AFTER 2                       PSE23105
015102     IF CARD-SUBSCRIPT ' 9
015106     MOVE ^TOTAL 1-8   ^ TO TOTAL-CLASS-NOS
015108         PERFORM SUB-TOT-1ST-CARD THRU IST-EXIT.
027000                                                                  PSE23105
027100     ADD 01 TO CARD-SUBSCRIPT.                                    PSE23105
027200     IF CARD-SUBSCRIPT ) 14                                       PSE23105
027300         GO TO 140-MOVE.                                          PSE23105
027400                                                                  PSE23105
027500 170-TOTAL-FIRST-CARD.                                            PSE23105
160025     MOVE ^TOTAL 9-12     ^ TO TOTAL-CLASS-NOS.
160027     PERFORM SUB-TOT-1ST-CARD THRU IST-EXIT.
027600     MOVE ZERO-GROWTH TO TOTAL-NO-GROWTH.                         PSE23105
027700     MOVE SPACES TO TOTAL-MINUS                                   PSE23105
027800     MOVE SPACES TO TOT-MINUS                                     PSE23105
027900     MOVE ZERO-GROWTH TO TOTAL-PERCENT.                           PSE23105
160050     MOVE 100.00 TO DIST-PCT-PT.
028000     MOVE TOTAL-STUDENTS TO TOTAL-NO.                             PSE23105
028100     MOVE ZERO-GROWTH TO CO-TOT-NO.                               PSE23105
028200     MOVE ZERO-GROWTH TO CO-TOT-PCT.                              PSE23105
028300     MOVE SPACES TO CO-TOT-MINUS.                                 PSE23105
028400     MOVE SPACES TO CO-PCT-MINUS.                                 PSE23105
160055     MOVE ^TOTAL 1-12     ^ TO TOTAL-CLASS-NOS.
028500                                                                  PSE23105
028600     WRITE PRINT FROM TOTAL-LINE AFTER 3.                         PSE23105
028700                                                                  PSE23105
028800     MOVE CARD-AMT ]1[ TO SAVE-BIRTHS.                            PSE23105
028900     MOVE TOTAL-STUDENTS TO SAVE-TOTAL.                           PSE23105
029000     MOVE CARDS-IN TO SAVE-CARD.                                  PSE23105
029100                                                                  PSE23105
029200 300-READ-2ND-CARD.                                               PSE23105
029300     READ CARD-IN AT END GO TO 900-END-OF-JOB.                    PSE23105
029400     IF CARD-IND ' ^D^                                            PSE23105
029500         MOVE ZEROES TO COHORT-CTR                                PSE23105
029600         MOVE COUNTY-NAME TO CTY-NAME                             PSE23105
029700         GO TO 120-READ-FIRST-CARD.                               PSE23105
029800     ADD 1 TO COHORT-CTR.                                         PSE23105
029900     ADD 1 TO CARD-CTR.                                           PSE23105
030000     TRANSFORM ALPHA-CARD FROM SPACES TO ZEROES.                  PSE23105
030100     IF CARD-YR # SAVE-YEAR                                       PSE23105
030200         GO TO 350-CONTINUE-2-CARD.                               PSE23105
030300     DISPLAY ^   ^.                                               PSE23105
030400     DISPLAY ^CARDS OUT OF SEQUENCE^.                             PSE23105
030500     DISPLAY CARDS-IN.                                            PSE23105
030600     DISPLAY ^   ^.                                               PSE23105
030700     GO TO 900-END-OF-JOB.                                        PSE23105
030800                                                                  PSE23105
030900 350-CONTINUE-2-CARD.                                             PSE23105
031000                                                                  PSE23105
031100     PERFORM 500-HEADING-ROUTINE THRU 510-HEADER-EXIT.            PSE23105
031200                                                                  PSE23105
031300     PERFORM 520-TOTAL-AMT THRU 530-TOTAL-EXIT.                   PSE23105
031400                                                                  PSE23105
031500 400-COMPUTE-BIRTH-RATE.                                          PSE23105
031600     MOVE 01 TO CARD-SUBSCRIPT                                    PSE23105
031700     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO NO-OF-BIRTHS.              PSE23105
031800     SUBTRACT 06 FROM CARD-YR-RED GIVING SAVE-BIRTH-YR.           PSE23105
031900     MOVE SAVE-BIRTH-YR TO BIRTH-YR.                              PSE23105
032000                                                                  PSE23105
032100     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES AND                    PSE23105
032200         CARD-AMOUNT ]CARD-SUBSCRIPT[ ' ZEROES                    PSE23105
032300         MOVE ZERO-GROWTH TO GR-NO                                PSE23105
032400         MOVE ZERO-GROWTH TO GR-PCT  MOVE SPACES TO PLUS-OR-MINUS PSE23105
032500     MOVE SPACES TO GR-PLUS-OR-MINUS                              PSE23105
032600         GO TO 420-PRINT-BIRTH-LINE.                              PSE23105
032700                                                                  PSE23105
032800     IF CARD-AMT ]CARD-SUBSCRIPT[ # ZEROES AND                    PSE23105
032900         CARD-AMOUNT ]CARD-SUBSCRIPT[ ' ZEROES                    PSE23105
033000         MOVE 100.00 TO GR-PCT   MOVE SPACES TO PLUS-OR-MINUS     PSE23105
033100     MOVE SPACES TO GR-PLUS-OR-MINUS                              PSE23105
033200         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NO                  PSE23105
033300         GO TO 420-PRINT-BIRTH-LINE.                              PSE23105
033400     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES AND                    PSE23105
033500         CARD-AMOUNT ]CARD-SUBSCRIPT[ # ZEROES                    PSE23105
033600         MOVE 100.00 TO GR-PCT                                    PSE23105
033700         MOVE ^-^ TO PLUS-OR-MINUS                                PSE23105
033800     MOVE ^-^ TO GR-PLUS-OR-MINUS                                 PSE23105
033900         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NO                  PSE23105
034000         GO TO 420-PRINT-BIRTH-LINE.                              PSE23105
034100                                                                  PSE23105
034200 410-COMPUTE-BIRTH-PCT.                                           PSE23105
034300     IF CARD-AMT ]CARD-SUBSCRIPT[ ' CARD-AMOUNT ]CARD-SUBSCRIPT[  PSE23105
034400         MOVE ZERO-GROWTH TO GR-PCT  MOVE SPACE TO PLUS-OR-MINUS  PSE23105
034500         MOVE ZERO-GROWTH TO GR-NO                                PSE23105
034600     MOVE SPACES TO GR-PLUS-OR-MINUS                              PSE23105
034700         GO TO 420-PRINT-BIRTH-LINE.                              PSE23105
034800                                                                  PSE23105
034900     IF CARD-AMT ]CARD-SUBSCRIPT[ ) CARD-AMOUNT ]CARD-SUBSCRIPT[  PSE23105
035000         SUBTRACT CARD-AMT ]CARD-SUBSCRIPT[                       PSE23105
035100         FROM CARD-AMOUNT ]CARD-SUBSCRIPT[                        PSE23105
035200         GIVING GROWTH-DIFF                                       PSE23105
035300         DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO                 PSE23105
035400             GROWTH-DIFF GIVING GROWTH-PERCENTAGE ROUNDED         PSE23105
035500         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
035600             GIVING GROWTH-PERCENTAGE                             PSE23105
035700         MOVE GROWTH-DIFF TO GR-NO  MOVE ^-^ TO PLUS-OR-MINUS     PSE23105
035800     MOVE ^-^ TO GR-PLUS-OR-MINUS                                 PSE23105
035900         MOVE GROWTH-PERCENTAGE TO GR-PCT                         PSE23105
036000         GO TO 420-PRINT-BIRTH-LINE.                              PSE23105
036100                                                                  PSE23105
036200     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM                   PSE23105
036300         CARD-AMT ]CARD-SUBSCRIPT[                                PSE23105
036400         GIVING GROWTH-DIFF                                       PSE23105
036500     DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO GROWTH-DIFF         PSE23105
036600         GIVING GROWTH-PERCENTAGE ROUNDED                         PSE23105
036700         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
036800             GIVING GROWTH-PERCENTAGE                             PSE23105
036900     MOVE GROWTH-DIFF TO GR-NO                                    PSE23105
037000     MOVE GROWTH-PERCENTAGE TO GR-PCT                             PSE23105
037100     MOVE SPACES TO GR-PLUS-OR-MINUS                              PSE23105
037200     MOVE SPACE TO PLUS-OR-MINUS.                                 PSE23105
037300 420-PRINT-BIRTH-LINE.                                            PSE23105
037400     WRITE PRINT FROM DETAIL-LINE-1 AFTER 2.                      PSE23105
037500         GO TO 550-DIST-PERCENT.                                  PSE23105
037600                                                                  PSE23105
037700 500-HEADING-ROUTINE.                                             PSE23105
037800     MOVE CARD-YR TO PR-YEAR.                                     PSE23105
037900     WRITE PRINT FROM HEADER-1 AFTER 0.                           PSE23105
038000     WRITE PRINT FROM HEADER-2 AFTER 2.                           PSE23105
038100     WRITE PRINT FROM HEADER-3 AFTER 1.                           PSE23105
038200                                                                  PSE23105
038300 510-HEADER-EXIT.                                                 PSE23105
038400     EXIT.                                                        PSE23105
038500                                                                  PSE23105
038600 520-TOTAL-AMT.                                                   PSE23105
038700     MOVE 02 TO CARD-SUBSCRIPT.                                   PSE23105
038800     MOVE ZEROES TO TOTAL-STUDENTS.                               PSE23105
038900 525-CONTINUE-TOTAL.                                              PSE23105
039000     ADD CARD-AMT ]CARD-SUBSCRIPT[ TO TOTAL-STUDENTS.             PSE23105
121162     IF CARD-SUBSCRIPT ) 10
021164         ADD CARD-AMT ]CARD-SUBSCRIPT[ TO TOT-STUDENT-1-8.
021166     IF CARD-SUBSCRIPT # 9
021168         ADD CARD-AMT ]CARD-SUBSCRIPT[ TO TOT-STUDENT-9-12.
039100     ADD 1 TO CARD-SUBSCRIPT.                                     PSE23105
039200     IF CARD-SUBSCRIPT ) 14                                       PSE23105
039300         GO TO 525-CONTINUE-TOTAL.                                PSE23105
039400                                                                  PSE23105
039500 530-TOTAL-EXIT.                                                  PSE23105
039600     EXIT.                                                        PSE23105
039700                                                                  PSE23105
039800 550-DIST-PERCENT.                                                PSE23105
039900     MOVE 02 TO CARD-SUBSCRIPT.                                   PSE23105
040000     MOVE 00 TO CLASS-NUMBER.                                     PSE23105
040100     MOVE 01 TO CO-SUB-OLD.                                       PSE23105
040200     MOVE 02 TO CO-SUB-NEW.                                       PSE23105
040300     NOTE *** EDIT AMOUNTS FOR ZEROES BEFORE DIVIDING ***.        PSE23105
040400                                                                  PSE23105
040500 560-MOVE.                                                        PSE23105
040600                                                                  PSE23105
040700     ADD 01 TO CLASS-NUMBER.                                      PSE23105
040800     MOVE CLASS-NUMBER TO GRADE-NO.                               PSE23105
040900     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER.                 PSE23105
041000                                                                  PSE23105
041100     NOTE *** EDIT CARD AMTS FOR ZEROES.                          PSE23105
041200                                                                  PSE23105
041300                                                                  PSE23105
041400         MOVE ^1^ TO FIRST-CARD-SW.                               PSE23105
041500                                                                  PSE23105
041600 570-COMPUTE-DIST.                                                PSE23105
041700     IF TOTAL-STUDENTS ' ZEROES AND CARD-AMT ]CARD-SUBSCRIPT[     PSE23105
041800         ' ZEROES                                                 PSE23105
041900         MOVE ZERO-GROWTH TO DIST-PERCENT                         PSE23105
042000         MOVE ZERO-GROWTH TO GR-NUMBER                            PSE23105
042100         GO TO 595-COMPUTE-GROWTH.                                PSE23105
042200     IF TOTAL-STUDENTS ' ZEROES AND CARD-AMT ]CARD-SUBSCRIPT[     PSE23105
042300         # ZEROES                                                 PSE23105
042400     MOVE 100.00 TO GROWTH-PERCENT                                PSE23105
042500         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER              PSE23105
042600         GO TO 595-COMPUTE-GROWTH.                                PSE23105
042700     IF TOTAL-STUDENTS # ZEROES AND CARD-AMT ]CARD-SUBSCRIPT[     PSE23105
042800         ' ZEROES                                                 PSE23105
042900         MOVE ZERO-GROWTH TO DIST-PERCENT                         PSE23105
043000         MOVE ZERO-GROWTH TO GROWTH-NO                            PSE23105
043100         GO TO 595-COMPUTE-GROWTH.                                PSE23105
043200     DIVIDE TOTAL-STUDENTS INTO CARD-AMT ]CARD-SUBSCRIPT[         PSE23105
043300         GIVING GROWTH-PERCENTAGE ROUNDED                         PSE23105
043400         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
043500             GIVING GROWTH-PERCENTAGE                             PSE23105
043600         MOVE GROWTH-PERCENTAGE TO DIST-PERCENT                   PSE23105
043610     ADD GROWTH-PERCENTAGE TO TOT-DIST-PCT
043700         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER.             PSE23105
043800                                                                  PSE23105
043900 595-COMPUTE-GROWTH.                                              PSE23105
044000                                                                  PSE23105
044100     IF CARD-AMT ]CARD-SUBSCRIPT[ ' CARD-AMOUNT ]CARD-SUBSCRIPT[  PSE23105
044200         MOVE ZERO-GROWTH TO GROWTH-PERCENT                       PSE23105
044300         MOVE ZEROES TO GROWTH-DIFF                               PSE23105
044400         MOVE SPACES TO MINUS-GROWTH                              PSE23105
044510     IF CARD-AMT ]CARD-SUBSCRIPT[ # ZEROES
044520     AND CARD-AMOUNT ]CARD-SUBSCRIPT[ ' ZEROES
044530     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM
044540     CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF
044550     MOVE 100.00000 TO GROWTH-PERCENTAGE
044560         GO TO 596-CONT.
044570     IF CARD-AMT ]CARD-SUBSCRIPT[ ) CARD-AMOUNT ]CARD-SUBSCRIPT[
044580         GO TO 598-CONT.
044500     MOVE SPACES TO GROWTH-NO-MINUS                               PSE23105
044600         GO TO 600-PRINT-DETAIL.                                  PSE23105
044700     IF CARD-AMT ]CARD-SUBSCRIPT[ # CARD-AMOUNT ]CARD-SUBSCRIPT[  PSE23105
044800         SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM               PSE23105
044900         CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF             PSE23105
045000         DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO GROWTH-DIFF     PSE23105
045100         GIVING GROWTH-PERCENTAGE ROUNDED                         PSE23105
045200         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
045300             GIVING GROWTH-PERCENTAGE.                            PSE23105
045310 596-CONT.
045400         MOVE GROWTH-PERCENTAGE TO GROWTH-PERCENT                 PSE23105
045410     MOVE GROWTH-PERCENTAGE TO GR-PERCENT
045420     MULTIPLY GR-PERCENT BY <1 GIVING GR-PERCENT
045430     ADD GR-PERCENT TO TOT-GR-PCT
024134     MOVE GROWTH-DIFF TO GR-DIFF
024134     MULTIPLY GR-DIFF BY <1 GIVING GR-DIFF
024134     ADD GR-DIFF TO TOT-GR-NO
045500         MOVE SPACES TO GROWTH-NO-MINUS                           PSE23105
045600         MOVE SPACES TO MINUS-GROWTH                              PSE23105
045700         GO TO 600-PRINT-DETAIL.                                  PSE23105
045710 598-CONT.
045720     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES
045730     AND CARD-AMOUNT ]CARD-SUBSCRIPT[ # ZEROES
045740     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM
045750     CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF
045760     MOVE 100.00000 TO GROWTH-PERCENTAGE
045770         GO TO 599-CONT.
045800     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM                   PSE23105
045900         CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF             PSE23105
046000         DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO GROWTH-DIFF     PSE23105
046100         GIVING GROWTH-PERCENTAGE ROUNDED                         PSE23105
046200         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
046300             GIVING GROWTH-PERCENTAGE.                            PSE23105
046310 599-CONT.
046400         MOVE GROWTH-PERCENTAGE TO GROWTH-PERCENT                 PSE23105
046410     MOVE GROWTH-PERCENTAGE TO GR-PERCENT
046420     MULTIPLY GR-PERCENT BY -1 GIVING GR-PERCENT
046430     SUBTRACT GR-PERCENT FROM TOT-GR-PCT
024204     MOVE GROWTH-DIFF TO GR-DIFF
024204     MULTIPLY GR-DIFF BY -1 GIVING GR-DIFF
024024     SUBTRACT GR-DIFF FROM TOT-GR-NO
046500         MOVE ^-^ TO MINUS-GROWTH                                 PSE23105
046600     MOVE ^-^ TO GROWTH-NO-MINUS                                  PSE23105
046700         GO TO 600-PRINT-DETAIL.                                  PSE23105
046800                                                                  PSE23105
046900 600-PRINT-DETAIL.                                                PSE23105
047000     MOVE ZERO-GROWTH TO COHORT-NO.                               PSE23105
047100     MOVE ZERO-GROWTH TO COHORT-PERCENT.                          PSE23105
047200     MOVE SPACES TO COHORT-NO-MINUS.                              PSE23105
047300     MOVE SPACES TO COHORT-PCT-MINUS.                             PSE23105
047400     IF CLASS-NUMBER # 01                                         PSE23105
047500         PERFORM COHORT-CHANGE                                    PSE23105
047600         THRU COHORT-CHANGE-EXIT.                                 PSE23105
047700     MOVE GROWTH-DIFF TO GROWTH-NO.                               PSE23105
047800     ADD GROWTH-DIFF TO TOTAL-GROWTH.                             PSE23105
047900     WRITE PRINT FROM DETAIL-LINE-2 AFTER 2.                      PSE23105
048000     ADD 01 TO CO-SUB-OLD.                                        PSE23105
048100     ADD 01 TO CO-SUB-NEW.                                        PSE23105
022100     IF CARD-SUBSCRIPT ' 9
022110     MOVE ^TOTAL 1-8     ^ TO TOTAL-CLASS-NOS
022120         PERFORM SUB-TOT-2ND-CARD THRU IID-EXIT.
048200     ADD 1 TO CARD-SUBSCRIPT.                                     PSE23105
048300     IF CARD-SUBSCRIPT ) 14                                       PSE23105
048400         GO TO 560-MOVE.                                          PSE23105
048500                                                                  PSE23105
048600 610-PRINT-TOTAL-LINE.                                            PSE23105
025102     MOVE ^TOTAL 9-12    ^ TO TOTAL-CLASS-NOS.
025104     PERFORM SUB-TOT-2ND-CARD THRU IID-EXIT.
048700     MOVE TOTAL-STUDENTS TO TOTAL-NO.                             PSE23105
025102     MOVE ^TOTAL 1-12    ^ TO TOTAL-CLASS-NOS.
025104     MOVE 100.00 TO DIST-PCT-PT.
048800     IF FIRST-CARD-SW ' ^1^                                       PSE23105
048900         GO TO 630-CONTINUE-TOTAL.                                PSE23105
049000     MOVE TOTAL-STUDENTS TO TOTAL-NO.                             PSE23105
049100     MOVE ZERO-GROWTH TO TOTAL-NO-GROWTH.                         PSE23105
049200     MOVE ZERO-GROWTH TO TOTAL-PERCENT.                           PSE23105
049300     MOVE SPACES TO TOTAL-MINUS                                   PSE23105
049400     MOVE SPACES TO TOT-MINUS                                     PSE23105
049500         GO TO 650-PRINT-TOTAL-LINE.                              PSE23105
049600                                                                  PSE23105
049700 630-CONTINUE-TOTAL.                                              PSE23105
049800     IF TOTAL-STUDENTS ' SAVE-TOTAL                               PSE23105
049900         MOVE ZERO-GROWTH TO TOTAL-NO-GROWTH                      PSE23105
050000         MOVE ZERO-GROWTH TO TOTAL-PERCENT                        PSE23105
050100         MOVE SPACES TO TOT-MINUS                                 PSE23105
050200     MOVE SPACES TO TOTAL-MINUS                                   PSE23105
050300         GO TO 650-PRINT-TOTAL-LINE.                              PSE23105
050400                                                                  PSE23105
050500     IF TOTAL-STUDENTS # SAVE-TOTAL                               PSE23105
050600         SUBTRACT SAVE-TOTAL FROM TOTAL-STUDENTS                  PSE23105
050700         GIVING TOT-GROWTH-TOTAL                                  PSE23105
050800         DIVIDE SAVE-TOTAL INTO TOT-GROWTH-TOTAL                  PSE23105
050900             GIVING TOTAL-GR-PCT ROUNDED                          PSE23105
051000         MULTIPLY TOTAL-GR-PCT BY 100                             PSE23105
051100             GIVING TOTAL-GR-PCT                                  PSE23105
051200         MOVE TOTAL-GR-PCT TO TOTAL-PERCENT                       PSE23105
051300         MOVE SPACES TO TOT-MINUS                                 PSE23105
051400     MOVE SPACES TO TOTAL-MINUS                                   PSE23105
051500         MOVE TOT-GROWTH-TOTAL TO TOTAL-NO-GROWTH                 PSE23105
051600                                                                  PSE23105
051700         MOVE TOTAL-STUDENTS TO SAVE-TOTAL                        PSE23105
051800         GO TO 650-PRINT-TOTAL-LINE.                              PSE23105
051900     SUBTRACT TOTAL-STUDENTS FROM SAVE-TOTAL                      PSE23105
052000         GIVING TOT-GROWTH-TOTAL                                  PSE23105
052100         DIVIDE SAVE-TOTAL INTO TOT-GROWTH-TOTAL                  PSE23105
052200             GIVING TOTAL-GR-PCT ROUNDED                          PSE23105
052300         MULTIPLY TOTAL-GR-PCT BY 100                             PSE23105
052400             GIVING TOTAL-GR-PCT                                  PSE23105
052500         MOVE TOTAL-GR-PCT TO TOTAL-PERCENT                       PSE23105
052600         MOVE ^-^ TO TOT-MINUS                                    PSE23105
052700     MOVE ^-^ TO TOTAL-MINUS                                      PSE23105
052800         MOVE TOT-GROWTH-TOTAL TO TOTAL-NO-GROWTH                 PSE23105
052900         MOVE TOTAL-STUDENTS TO SAVE-TOTAL.                       PSE23105
053000 650-PRINT-TOTAL-LINE.                                            PSE23105
026187     PERFORM COHORT-TOTAL THRU COHORT-TOTAL-EXIT.
053200     WRITE PRINT FROM TOTAL-LINE AFTER 3                          PSE23105
053300     MOVE TOTAL-STUDENTS TO SAVE-TOTAL.                           PSE23105
053400     MOVE CARDS-IN TO SAVE-CARD                                   PSE23105
053410     MOVE SPACES TO TOTAL-LINE-RED.
053500         GO TO 300-READ-2ND-CARD.                                 PSE23105
053600                                                                  PSE23105
053700  900-END-OF-JOB.                                                 PSE23105
053800     CLOSE CARD-IN.                                               PSE23105
053900     CLOSE PRINT-LINE.                                            PSE23105
054000     DISPLAY ^CARDS READ  ^, CARD-CTR.                            PSE23105
054100     STOP RUN.                                                    PSE23105
054200                                                                  PSE23105
054300 COHORT-CHANGE.                                                   PSE23105
054400     IF CARD-AMT ]CO-SUB-NEW[ ' CARD-AMOUNT ]CO-SUB-OLD[          PSE23105
054500         MOVE ZEROES TO COHORT-PERCENT                            PSE23105
054600         MOVE ZEROES TO COHORT-NO                                 PSE23105
054700         MOVE SPACES TO COHORT-NO-MINUS                           PSE23105
054800         MOVE SPACES TO COHORT-PCT-MINUS                          PSE23105
054900         GO TO COHORT-CHANGE-EXIT.                                PSE23105
055000                                                                  PSE23105
055010     IF CARD-AMT ]CO-SUB-NEW[ # ZEROES
055020     AND CARD-AMOUNT ]CO-SUB-OLD[ ' ZEROES
055030     SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[ FROM
055045     CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF
055050     MOVE 100.00000 TO GROWTH-PERCENTAGE
055060         GO TO 910-CONT.
055070     IF CARD-AMT ]CO-SUB-NEW[ ) CARD-AMOUNT ]CO-SUB-OLD[
055080         GO TO 920-CONT.
055100     IF CARD-AMT ]CO-SUB-NEW[ # CARD-AMOUNT ]CO-SUB-OLD[          PSE23105
055200         SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[ FROM                   PSE23105
055300     CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF                     PSE23105
055400         DIVIDE CARD-AMOUNT ]CO-SUB-OLD[ INTO COHORT-DIFF         PSE23105
055500         GIVING GROWTH-PERCENTAGE ROUNDED.                        PSE23105
055510 910-CONT.
055600     MULTIPLY GROWTH-PERCENTAGE BY 100                            PSE23105
055700             GIVING GROWTH-PERCENTAGE                             PSE23105
055800         MOVE GROWTH-PERCENTAGE TO COHORT-PERCENT                 PSE23105
055900         MOVE COHORT-DIFF TO COHORT-NO                            PSE23105
056000         MOVE SPACES TO COHORT-NO-MINUS                           PSE23105
056100         MOVE SPACES TO COHORT-PCT-MINUS                          PSE23105
056200         ADD COHORT-DIFF TO TOTAL-COHORT                          PSE23105
056210     MULTIPLY COHORT-DIFF BY <1 GIVING COHORT-DIFF
056220     ADD COHORT-DIFF TO SUB-COHORT-TOTAL
056300             GO TO COHORT-CHANGE-EXIT.                            PSE23105
056400                                                                  PSE23105
056410 920-CONT.
056420     IF CARD-AMT ]CO-SUB-NEW[ ' ZEROES
056430     AND CARD-AMOUNT ]CO-SUB-OLD[ # ZEROES
056440     SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[
056450     FROM CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF
056460     MOVE 100.00000 TO GROWTH-PERCENTAGE
056470         GO TO 950-CONT.
056500     SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[ FROM                       PSE23105
056600         CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF                 PSE23105
056700         DIVIDE CARD-AMOUNT ]CO-SUB-OLD[ INTO COHORT-DIFF         PSE23105
056800         GIVING GROWTH-PERCENTAGE ROUNDED                         PSE23105
056900         MULTIPLY GROWTH-PERCENTAGE BY 100                        PSE23105
057000             GIVING GROWTH-PERCENTAGE.                            PSE23105
057010 950-CONT.
057100         MOVE GROWTH-PERCENTAGE TO COHORT-PERCENT                 PSE23105
057200         MOVE ^-^ TO COHORT-NO-MINUS                              PSE23105
057300         MOVE ^-^ TO COHORT-PCT-MINUS                             PSE23105
057400         MOVE COHORT-DIFF TO COHORT-NO                            PSE23105
057510     MULTIPLY COHORT-DIFF BY -1 GIVING COHORT-DIFF
057520     SUBTRACT COHORT-DIFF FROM SUB-COHORT-TOTAL.
057500     ADD COHORT-DIFF TO TOTAL-COHORT.                             PSE23105
057600 COHORT-CHANGE-EXIT.                                              PSE23105
057700     EXIT.                                                        PSE23105
057800                                                                  PSE23105
057900 COHORT-TOTAL.                                                    PSE23105
058000     MOVE 02 TO CO-SUB-OLD.                                       PSE23105
058100     MOVE ZEROES TO TOTAL-OLD-COHORT.                             PSE23105
058200 COMPUTE-GDS-1-11.                                                PSE23105
058300     ADD CARD-AMOUNT ]CO-SUB-OLD[ TO TOTAL-OLD-COHORT.            PSE23105
058400     ADD 01 TO CO-SUB-OLD.                                        PSE23105
058500     IF CO-SUB-OLD ) 13                                           PSE23105
058600         GO TO COMPUTE-GDS-1-11.                                  PSE23105
058700 RESET-TOTALS.                                                    PSE23105
058800     MOVE 03 TO CO-SUB-NEW.                                       PSE23105
058900     MOVE ZEROES TO TOTAL-NEW-COHORT.                             PSE23105
059000 COMPUTE-GDS-2-12.                                                PSE23105
059100     ADD CARD-AMT ]CO-SUB-NEW[ TO TOTAL-NEW-COHORT.               PSE23105
059200     ADD 01 TO CO-SUB-NEW.                                        PSE23105
059300     IF CO-SUB-NEW ) 14                                           PSE23105
059400         GO TO COMPUTE-GDS-2-12.                                  PSE23105
059500                                                                  PSE23105
059600                                                                  PSE23105
059700 COMPUTE-COHORT-PCT.                                              PSE23105
059800     IF TOTAL-OLD-COHORT ' TOTAL-NEW-COHORT                       PSE23105
059900     MOVE ZEROES TO CO-TOT-PCT                                    PSE23105
060000     MOVE ZEROES TO CO-TOT-NO                                     PSE23105
060100     MOVE SPACES TO CO-TOT-MINUS                                  PSE23105
060200     MOVE SPACES TO CO-PCT-MINUS                                  PSE23105
060300         GO TO COHORT-TOTAL-EXIT.                                 PSE23105
060400                                                                  PSE23105
060500     IF TOTAL-NEW-COHORT # TOTAL-OLD-COHORT                       PSE23105
060600     SUBTRACT TOTAL-OLD-COHORT FROM TOTAL-NEW-COHORT              PSE23105
060700         GIVING TOTAL-COHORT-GROWTH                               PSE23105
060800     DIVIDE TOTAL-OLD-COHORT INTO TOTAL-COHORT-GROWTH             PSE23105
060900         GIVING TOTAL-COHORT-PCT ROUNDED                          PSE23105
061000     MULTIPLY TOTAL-COHORT-PCT BY 100                             PSE23105
061100         GIVING TOTAL-COHORT-PCT                                  PSE23105
061200     MOVE TOTAL-COHORT-PCT TO CO-TOT-PCT                          PSE23105
061300     MOVE SPACES TO CO-PCT-MINUS                                  PSE23105
061400     MOVE TOTAL-COHORT-GROWTH TO CO-TOT-NO                        PSE23105
061500     MOVE SPACES TO CO-TOT-MINUS                                  PSE23105
061600         GO TO COHORT-TOTAL-EXIT.                                 PSE23105
061700                                                                  PSE23105
061800     SUBTRACT TOTAL-NEW-COHORT FROM TOTAL-OLD-COHORT              PSE23105
061900         GIVING TOTAL-COHORT-GROWTH                               PSE23105
062000     DIVIDE TOTAL-OLD-COHORT INTO TOTAL-COHORT-GROWTH             PSE23105
062100         GIVING TOTAL-COHORT-PCT ROUNDED                          PSE23105
062200     MULTIPLY TOTAL-COHORT-PCT BY 100                             PSE23105
062300         GIVING TOTAL-COHORT-PCT                                  PSE23105
062400     MOVE TOTAL-COHORT-PCT TO CO-TOT-PCT                          PSE23105
062500     MOVE TOTAL-COHORT-GROWTH TO CO-TOT-NO                        PSE23105
062600     MOVE ^-^ TO CO-TOT-MINUS                                     PSE23105
062700     MOVE ^-^ TO CO-PCT-MINUS.                                    PSE23105
062800                                                                  PSE23105
062900 COHORT-TOTAL-EXIT.                                               PSE23105
063000     EXIT.                                                        PSE23105
033010
033010
033020 SUB-TOT-1ST-CARD.
033030     IF CARD-SUBSCRIPT ' 9
033040     MOVE TOT-STUDENT-1-8 TO TOTAL-NO
033050     ELSE MOVE TOT-STUDENT-9-12 TO TOTAL-NO.
033060     MOVE ZERO-GROWTH TO CO-TOT-NO.
033070     MOVE ZERO-GROWTH TO CO-TOT-PCT.
033080     MOVE SPACES TO CO-TOT-MINUS.
033090     MOVE SPACES TO CO-PCT-MINUS.
033110     MOVE TOT-DIST-PCT TO DIST-PCT-PT.
033130     WRITE PRINT FROM TOTAL-LINE AFTER 3.
033140     MOVE SPACES TO PRINT.
033150     WRITE PRINT AFTER 1.
033160     MOVE ZEROES TO TOT-DIST-PCT.
033170     IF CARD-SUBSCRIPT # 9
033180     MOVE ZEROES TO TOT-STUDENT-1-8
033190     MOVE ZEROES TO TOT-STUDENT-9-12.
033210 IST-EXIT.
033220     EXIT.
034010
034020 SUB-TOT-2ND-CARD.
034060     MOVE TOT-DIST-PCT TO DIST-PCT-PT.
034070     MOVE TOT-GR-NO TO TOTAL-NO-GROWTH.
034072     IF CARD-SUBSCRIPT ' 9
034073     DIVIDE TOT-STUDENT-1-8 INTO TOT-GR-NO
034073     GIVING TOT-GR-PCT
034074     ELSE DIVIDE TOT-STUDENT-9-12 INTO TOT-GR-NO
034075         GIVING TOT-GR-PCT.
034075     MULTIPLY TOT-GR-PCT BY 100 GIVING TOT-GR-PCT.
034080     MOVE TOT-GR-PCT TO TOTAL-PERCENT.
034110     MOVE SPACES TO TOTAL-MINUS, TOT-MINUS.
034130     IF TOT-GR-NO IS NEGATIVE
034140     MOVE ^-^ TO TOTAL-MINUS, TOT-MINUS.
           IF CARD-SUBSCRIPT ' 9
           MOVE TOT-STUDENT-1-8 TO TOTAL-NO
           ELSE MOVE TOT-STUDENT-9-12 TO TOTAL-NO.
           MOVE SUB-COHORT-TOTAL TO CO-TOT-NO.
           IF SUB-COHORT-TOTAL IS NEGATIVE
           MOVE ^-^ TO CO-TOT-MINUS ELSE MOVE SPACES TO CO-TOT-MINUS.
           IF SUB-COHORT-TOTAL IS NEGATIVE
           MOVE ^-^ TO CO-PCT-MINUS ELSE MOVE SPACES TO CO-PCT-MINUS.
           IF CARD-SUBSCRIPT ' 9
               GO TO COHORT-TOTALS-1-8.
               GO TO COHORT-TOTALS-9-12.
       COHORT-TOTALS-1-8.
           IF SUB-COHORT-TOTAL IS NEGATIVE
           MULTIPLY -1 BY SUB-COHORT-TOTAL GIVING SUB-COHORT-TOTAL
           ADD SUB-COHORT-TOTAL TO TOT-STUDENT-1-8
           ELSE SUBTRACT SUB-COHORT-TOTAL FROM TOT-STUDENT-1-8.
           SUBTRACT CARD-AMT ]1[ FROM TOT-STUDENT-1-8.
           DIVIDE TOT-STUDENT-1-8 INTO SUB-COHORT-TOTAL
               GIVING TOT-CO-PCT.
               GO TO CONT-PRINT.
       COHORT-TOTALS-9-12.
           IF SUB-COHORT-TOTAL IS NEGATIVE
           MULTIPLY -1 BY SUB-COHORT-TOTAL GIVING SUB-COHORT-TOTAL
           ADD SUB-COHORT-TOTAL TO TOT-STUDENT-9-12
           ELSE SUBTRACT SUB-COHORT-TOTAL FROM TOT-STUDENT-9-12.
           DIVIDE TOT-STUDENT-9-12 INTO SUB-COHORT-TOTAL
               GIVING TOT-CO-PCT.
       CONT-PRINT.
           MULTIPLY TOT-CO-PCT BY 100 GIVING TOT-CO-PCT.
           MOVE TOT-CO-PCT TO CO-TOT-PCT.
035010
035020     WRITE PRINT FROM TOTAL-LINE AFTER 3.
035030     MOVE SPACES TO PRINT.
035040     WRITE PRINT AFTER 1.
035050     MOVE ZEROES TO TOT-DIST-PCT.
035060     MOVE ZEROES TO TOT-GR-NO.
035070     MOVE ZEROES TO TOT-GR-PCT.
035080     MOVE ZEROES TO TOT-CO-NO.
035090     MOVE ZEROES TO TOT-CO-PCT.
035090     MOVE ZEROES TO GR-PERCENT.
035090     MOVE ZEROES TO GR-DIFF.
035092     MOVE ZEROES TO SUB-COHORT-TOTAL.
035094     MOVE SPACES TO CO-TOT-MINUS.
035096     MOVE SPACES TO CO-PCT-MINUS.
035097     MOVE SPACES TO ASTERISK-2.
035100     IF CARD-SUBSCRIPT # 9
035110     MOVE ZEROES TO TOT-STUDENT-1-8
035120     MOVE ZEROES TO TOT-STUDENT-9-12.
035140 IID-EXIT.
035150     EXIT.
   x@`®