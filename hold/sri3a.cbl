000100 IDENTIFICATION DIVISION.                                         ACT3005 
000200 PROGRAM-ID. 'ACT3005'.                                           ACT3005 
000300 AUTHOR. K R MARCUM.                                              ACT3005 
000400 INSTALLATION. STANFORD RESEARCH INSTITUTE.                       ACT3005 
000500 DATE-WRITTEN. JUNE 1970.                                         ACT3005 
000600 REMARKS. PROGRAM CONVERTS VARIOUS ACTRS VOLUME COUNT DATA INTO A ACT3005 
000700          FORMAT ACCEPTABLE TO THE TAIS SYSTEM (PROGRAM T3010)    ACT3005 
000710          CARD CODE OF 21 = HEADER                                ACT3005 
000720          CARD CODE OF 22 = 15 MIN COUNT                          ACT3005 
000730          CARD CODE OF 23 = 30 MIN COUNT                          ACT3005 
000740          CARD CODE OF 24 = 60 MIN COUNT  .                       ACT3005 
000800 ENVIRONMENT DIVISION.                                            ACT3005 
000900 CONFIGURATION SECTION.                                           ACT3005 
001000 SOURCE-COMPUTER. IBM-360 G40.                                    ACT3005 
001100 OBJECT-COMPUTER. IBM-360 G40.                                    ACT3005 
001200 INPUT-OUTPUT SECTION.                                            ACT3005 
001300 FILE-CONTROL.                                                    ACT3005 
001400     SELECT  CARD-INPUT  ASSIGN TO 'SYS010' UNIT-RECORD 2540R.    ACT3005 
001500     SELECT  TAPE-OUTPUT ASSIGN TO 'SYS015' UTILITY     2400.     ACT3005 
001600 DATA DIVISION.                                                   ACT3005 
001700 FILE SECTION.                                                    ACT3005 
001800 FD  CARD-INPUT                                                   ACT3005 
001900     LABEL RECORDS ARE OMITTED                                    ACT3005 
002000     RECORDING MODE IS F                                          ACT3005 
002100     DATA RECORDS ARE CARD-IN                                     ACT3005 
002200                      CARD-22-X                                   ACT3005 
002300                      CARD-22-N                                   ACT3005 
002400                      CARD-23-X                                   ACT3005 
002500                      CARD-23-N                                   ACT3005 
002600                      CARD-24-X                                   ACT3005 
002700                      CARD-24-N.                                  ACT3005 
002800 01  CARD-IN.                                                     ACT3005 
002900     03 CI-REQ-NO                PICTURE X(04).                   ACT3005 
003000     03 CI-DIR                   PICTURE X(01).                   ACT3005 
003100     03 FILLER                   PICTURE X(73).                   ACT3005 
003200     03 CI-CODE                  PICTURE X(02).                   ACT3005 
003300 01  CARD-22-X.                                                   ACT3005 
003400     03 FILLER                   PICTURE X(05).                   ACT3005 
003500     03 C22-HOUR-X               PICTURE X(02).                   ACT3005 
003600     03 C22-AP-X                 PICTURE X(01).                   ACT3005 
003700     03 C22-COUNT-X  OCCURS 16   PICTURE X(04).                   ACT3005 
003800     03 FILLER                   PICTURE X(08).                   ACT3005 
003900 01  CARD-22-N.                                                   ACT3005 
004000     03 FILLER                   PICTURE X(05).                   ACT3005 
004100     03 C22-HOUR-N               PICTURE 9(02).                   ACT3005 
004200     03 C22-AP-N                 PICTURE 9(01).                   ACT3005 
004300     03 C22-COUNT-N  OCCURS 16   PICTURE 9(04).                   ACT3005 
004400     03 FILLER                   PICTURE X(08).                   ACT3005 
004500 01  CARD-23-X.                                                   ACT3005 
004600     03 FILLER                   PICTURE X(05).                   ACT3005 
004700     03 C23-HOUR-X               PICTURE X(02).                   ACT3005 
004800     03 C23-AP-X                 PICTURE X(01).                   ACT3005 
004900     03 C23-COUNT-X  OCCURS 12   PICTURE X(05).                   ACT3005 
005000     03 FILLER                   PICTURE X(12).                   ACT3005 
005100 01  CARD-23-N.                                                   ACT3005 
005200     03 FILLER                   PICTURE X(05).                   ACT3005 
005300     03 C23-HOUR-N               PICTURE 9(02).                   ACT3005 
005400     03 C23-AP-N                 PICTURE 9(01).                   ACT3005 
005500     03 C23-COUNT-N  OCCURS 6.                                    ACT3005 
005600        05 C23-HH-1              PICTURE 9(05).                   ACT3005 
005700        05 C23-HH-2              PICTURE 9(05).                   ACT3005 
005800     03 FILLER                   PICTURE X(12).                   ACT3005 
005900 01  CARD-24-X.                                                   ACT3005 
006000     03 FILLER                   PICTURE X(05).                   ACT3005 
006100     03 C24-HOUR-X               PICTURE X(02).                   ACT3005 
006200     03 C24-AP-X                 PICTURE X(01).                   ACT3005 
006300     03 C24-COUNT-X  OCCURS 12   PICTURE X(05).                   ACT3005 
006400     03 FILLER                   PICTURE X(12).                   ACT3005 
006500 01  CARD-24-N.                                                   ACT3005 
006600     03 FILLER                   PICTURE X(05).                   ACT3005 
006700     03 C24-HOUR-N               PICTURE 9(02).                   ACT3005 
006800     03 C24-AP-N                 PICTURE 9(01).                   ACT3005 
006900     03 C24-COUNT-N  OCCURS 12   PICTURE 9(05).                   ACT3005 
007000     03 FILLER                   PICTURE X(12).                   ACT3005 
007100 FD  TAPE-OUTPUT                                                  ACT3005 
007200     LABEL RECORDS ARE OMITTED                                    ACT3005 
007300     RECORDING MODE IS F                                          ACT3005 
007400     BLOCK  CONTAINS 40 RECORDS                                   ACT3005 
007500     RECORD CONTAINS 80 CHARACTERS                                ACT3005 
007600     DATA RECORD IS TAPE-RECORD.                                  ACT3005 
007700 01  TAPE-RECORD.                                                 ACT3005 
007800     03 T-REQ-NO                 PICTURE X(04).                   ACT3005 
007900     03 T-DIR                    PICTURE X(01).                   ACT3005 
008000     03 T-HOUR                   PICTURE 9(02).                   ACT3005 
008100     03 T-AP                     PICTURE 9(01).                   ACT3005 
008200     03 T-COUNT OCCURS 4.                                         ACT3005 
008300        05 T-Q00                 PICTURE 9(04).                   ACT3005 
008400        05 T-Q15                 PICTURE 9(04).                   ACT3005 
008500        05 T-Q30                 PICTURE 9(04).                   ACT3005 
008600        05 T-Q45                 PICTURE 9(04).                   ACT3005 
008700     03 FILLER                   PICTURE X(06).                   ACT3005 
008800     03 T-CODE                   PICTURE X(02).                   ACT3005 
008900 WORKING-STORAGE SECTION.                                         ACT3005 
009000 77  I       COMPUTATIONAL       PICTURE S99     VALUE ZERO.      ACT3005 
009100 77  J       COMPUTATIONAL       PICTURE S99     VALUE ZERO.      ACT3005 
009200 77  K       COMPUTATIONAL       PICTURE S99     VALUE ZERO.      ACT3005 
009210 77  C22-SWITCH                  PICTURE X(01)   VALUE ZERO.      KM072270
009300 77  C23-SWITCH                  PICTURE X(01)   VALUE ZERO.      ACT3005 
009400 77  C24-SWITCH                  PICTURE X(01)   VALUE ZERO.      ACT3005 
009500 77  WS-REQ-NO                   PICTURE X(04).                   ACT3005 
009600 77  CARD-21-SAVE                PICTURE X(80)   VALUE SPACES.    ACT3005 
009700 77  WS-DIR                      PICTURE X(01).                   ACT3005 
009800 01  HALF-HOUR-TABLE.                                             ACT3005 
009900     03 HH           OCCURS 24.                                   ACT3005 
010000        05 HH-1                  PICTURE 9(05).                   ACT3005 
010100        05 HH-2                  PICTURE 9(05).                   ACT3005 
010200 01  FULL-HOUR-TABLE.                                             ACT3005 
010300     03 FH           OCCURS 24.                                   ACT3005 
010400        05 FH-1                  PICTURE 9(05).                   ACT3005 
010500 PROCEDURE DIVISION.                                              ACT3005 
010600 START.                                                           ACT3005 
010700     OPEN INPUT  CARD-INPUT                                       ACT3005 
010800          OUTPUT TAPE-OUTPUT.                                     ACT3005 
010900     PERFORM ZERO-TABLE  VARYING I FROM 1 BY 1 UNTIL I > 24.      ACT3005 
011000     READ CARD-INPUT AT END GO TO EOJ.                            ACT3005 
011100     MOVE CI-REQ-NO TO WS-REQ-NO                                  ACT3005 
011200     GO TO SKIP-READ.                                             ACT3005 
011300 READ-CARDS.                                                      ACT3005 
011400     READ CARD-INPUT AT END GO TO EOJ.                            ACT3005 
011500 SKIP-READ.                                                       ACT3005 
011600     IF CI-CODE = 21 GO TO C21-ROUTINE.                           ACT3005 
011700     IF CI-CODE = 22 GO TO C22-ROUTINE.                           ACT3005 
011800     IF CI-CODE = 23 GO TO C23-ROUTINE.                           ACT3005 
011900     IF CI-CODE = 24 GO TO C24-ROUTINE.                           ACT3005 
012000     IF C22-SWITCH = 2 OR C23-SWITCH = 2 OR C24-SWITCH = 2        KM032571
012120     DISPLAY CARD-21-SAVE, ' SET REJECTED - INVALID START HR'.    KM032571
012100     IF C23-SWITCH = 1                                            KM033171
012200        PERFORM CONVERT-HALF-HOUR THRU CONVERT-HALF-EXIT.         KM033171
012300     IF C24-SWITCH = 1                                            KM033171
012400        PERFORM CONVERT-FULL-HOUR THRU CONVERT-FULL-EXIT.         KM033171
012410     MOVE 0 TO C22-SWITCH, C23-SWITCH, C24-SWITCH.                KM033171
012500     WRITE TAPE-RECORD FROM CARD-IN.                              ACT3005 
012600     GO TO READ-CARDS.                                            DC070670
012700 C21-ROUTINE.                                                     ACT3005 
012710     IF C22-SWITCH = 2 OR C23-SWITCH = 2 OR C24-SWITCH = 2        KM032571
012720     DISPLAY CARD-21-SAVE, ' SET REJECTED - INVALID START HR'.    KM032571
012800     IF C23-SWITCH = 1                                            ACT3005 
012900        PERFORM CONVERT-HALF-HOUR THRU CONVERT-HALF-EXIT.         ACT3005 
013000     IF C24-SWITCH = 1                                            ACT3005 
013100        PERFORM CONVERT-FULL-HOUR THRU CONVERT-FULL-EXIT.         ACT3005 
013110     MOVE 0 TO C22-SWITCH, C23-SWITCH, C24-SWITCH.                KM032571
013200     MOVE CARD-IN TO CARD-21-SAVE.                                ACT3005 
013300     MOVE CI-REQ-NO TO WS-REQ-NO.                                 ACT3005 
013310                                                                  KM032571
013400     GO TO READ-CARDS.                                            ACT3005 
013500 C22-ROUTINE.                                                     ACT3005 
013510     IF C22-SWITCH = 2 GO TO READ-CARDS.                          KM032571
013520     IF C22-SWITCH = 0                                            KM032571
013530        IF C22-HOUR-X = 12 AND                                    KM032571
013540           C22-AP-X   =  1                                        KM032571
013550           NEXT SENTENCE                                          KM032571
013560        ELSE                                                      KM032571
013570           MOVE 2 TO C22-SWITCH                                   KM032571
013580           GO TO READ-CARDS.                                      KM032571
013582     IF C22-SWITCH = 0                                            KM032571
013584        WRITE TAPE-RECORD FROM CARD-21-SAVE                       KM072270
013586        MOVE 1 TO C22-SWITCH.                                     KM032571
013600     PERFORM C22-EXAMINE VARYING I FROM 1 BY 1 UNTIL I > 16.      KM071570
013700     WRITE TAPE-RECORD FROM CARD-IN.                              ACT3005 
013800     GO TO READ-CARDS.                                            ACT3005 
013900 C22-EXAMINE.                                                     KM071570
014000     EXAMINE C22-COUNT-X (I) REPLACING ALL SPACES BY ZERO.        KM071570
014100 C23-ROUTINE.                                                     ACT3005 
014110     IF C23-SWITCH = 2 GO TO READ-CARDS.                          KM032571
014120     IF C23-SWITCH = 0                                            KM032571
014130        IF C23-HOUR-X = 12 AND                                    KM032571
014140           C23-AP-X   =  1                                        KM032571
014150           NEXT SENTENCE                                          KM032571
014160        ELSE                                                      KM032571
014170           MOVE 2 TO C23-SWITCH                                   KM032571
014180           GO TO READ-CARDS.                                      KM032571
014200     PERFORM C23-EXAMINE VARYING I FROM 1 BY 1 UNTIL I > 12.      KM072070
014300     IF C23-HOUR-X = 12 OR                                        ACT3005 
014400        C23-HOUR-X = 06                                           ACT3005 
014500        NEXT SENTENCE                                             ACT3005 
014600     ELSE                                                         ACT3005 
014700        MOVE 2 TO C23-SWITCH                                      ACT3005 
014800        DISPLAY CARD-23-X, '*REJECT INVALID HOUR'                 ACT3005 
014900        GO TO CARD-23-EXIT.                                       ACT3005 
015000     IF C23-HOUR-X = 12                                           ACT3005 
015100        MOVE ZERO TO C23-HOUR-X.                                  ACT3005 
015200     IF C23-AP-X = 1 OR                                           ACT3005 
015300        C23-AP-X = 2                                              ACT3005 
015400        NEXT SENTENCE                                             ACT3005 
015500     ELSE                                                         ACT3005 
015600        MOVE 2 TO C23-SWITCH                                      ACT3005 
015700        DISPLAY CARD-23-X, '*REJECTED INVALID A/P'                ACT3005 
015800        GO TO CARD-23-EXIT.                                       ACT3005 
015900     IF C23-SWITCH = 2                                            ACT3005 
016000        GO TO CARD-23-EXIT.                                       ACT3005 
016100     MOVE 1 TO C23-SWITCH.                                        ACT3005 
016200     COMPUTE J = C23-HOUR-N + (C23-AP-N * 12) - 12.               ACT3005 
016300     COMPUTE I = 0.                                               ACT3005 
016400 C23-LOOP.                                                        ACT3005 
016500     ADD 1 TO I.                                                  ACT3005 
016600     ADD 1 TO J.                                                  ACT3005 
016700     IF I > 6 GO TO CARD-23-EXIT.                                 ACT3005 
016800     ADD C23-HH-1 (I) TO HH-1 (J).                                ACT3005 
016900     ADD C23-HH-2 (I) TO HH-2 (J).                                ACT3005 
017000     GO TO C23-LOOP.                                              ACT3005 
017100 CARD-23-EXIT.                                                    ACT3005 
017200     MOVE CI-REQ-NO TO WS-REQ-NO.                                 ACT3005 
017300     MOVE CI-DIR    TO WS-DIR.                                    ACT3005 
017400     GO TO READ-CARDS.                                            ACT3005 
017500 C23-EXAMINE.                                                     KM072070
017600     EXAMINE C23-COUNT-X (I) REPLACING ALL SPACES BY ZERO.        KM072070
017700 CONVERT-HALF-HOUR.                                               ACT3005 
017800     WRITE TAPE-RECORD FROM CARD-21-SAVE.                         ACT3005 
017900     MOVE ZERO TO TAPE-RECORD.                                    ACT3005 
018000     MOVE 0 TO I.                                                 ACT3005 
018100     MOVE 0 TO J.                                                 ACT3005 
018200 CONVERT-HH-LOOP.                                                 ACT3005 
018300     ADD 1 TO I                                                   ACT3005 
018400     ADD 1 TO J.                                                  ACT3005 
018500     COMPUTE T-Q00 (I) = HH-1 (J) / 2.                            ACT3005 
018600     COMPUTE T-Q15 (I) = HH-1 (J).                                ACT3005 
018700     COMPUTE T-Q30 (I) = (HH-2 (J) + HH-1 (J)) / 2.               ACT3005 
018800     COMPUTE T-Q45 (I) = HH-2 (J).                                ACT3005 
018900     IF I = 4                                                     ACT3005 
019000        GO TO WRITE-HH.                                           ACT3005 
019100     GO TO CONVERT-HH-LOOP.                                       ACT3005 
019200 WRITE-HH.                                                        ACT3005 
019300     IF J = 4                                                     ACT3005 
019400        MOVE 12 TO T-HOUR                                         ACT3005 
01       MOVE 1  TO T-AP.                                          ACT3005 
019600     IF J = 8                                                     ACT3005 
019700        MOVE 04 TO T-HOUR                                         ACT3005 
019800        MOVE 1  TO T-AP.                                          ACT3005 
019900     IF J = 12                                                    ACT3005 
020000        MOVE 08 TO T-HOUR                                         ACT3005 
020100        MOVE 1  TO T-AP.                                          ACT3005 
020200     IF J = 16                                                    ACT3005 
020300        MOVE 12 TO T-HOUR                                         ACT3005 
020400        MOVE 2  TO T-AP.                                          ACT3005 
020500     IF J = 20                                                    ACT3005 
020600        MOVE 04 TO T-HOUR                                         ACT3005 
020700        MOVE 2  TO T-AP.                                          ACT3005 
020800     IF J = 24                                                    ACT3005 
020900        MOVE 08 TO T-HOUR                                         ACT3005 
021000        MOVE 2  TO T-AP.                                          ACT3005 
021100     MOVE WS-REQ-NO TO T-REQ-NO.                                          
021200     MOVE WS-DIR TO T-DIR.                                        ACT3005 
021300     MOVE 22 TO T-CODE.                                           ACT3005 
021400     WRITE TAPE-RECORD.                                           ACT3005 
021410 WRITE-HH-STOP.                                                           
021500     IF J = 24                                                    ACT3005 
021600        PERFORM ZERO-TABLE VARYING I FROM 1 BY 1 UNTIL I > 24     ACT3005 
021700        MOVE ZERO TO C23-SWITCH                                   ACT3005 
021800        GO TO CONVERT-HALF-EXIT.                                  ACT3005 
021900     MOVE ZERO TO I.                                              ACT3005 
022000     GO TO CONVERT-HH-LOOP.                                       ACT3005 
022100 CONVERT-HALF-EXIT.                                               ACT3005 
022200     EXIT.                                                        ACT3005 
022300 C24-ROUTINE.                                                     ACT3005 
022310     IF C24-SWITCH = 2 GO TO READ-CARDS.                          KM032571
022320     IF C24-SWITCH = 0                                            KM032571
022330        IF C24-HOUR-X = 12 AND                                    KM032571
022340           C24-AP-X   =  1                                        KM032571
022350           NEXT SENTENCE                                          KM032571
022360        ELSE                                                      KM032571
022370           MOVE 2 TO C24-SWITCH                                   KM032571
022380           GO TO READ-CARDS.                                      KM032571
022400     PERFORM C24-EXAMINE VARYING I FROM 1 BY 1 UNTIL I > 12.      KM072070
022410     COMPUTE J = (C24-HOUR-N * C24-AP-N) - 12.                            
022420     COMPUTE I = 0.                                                       
022430     PERFORM MOVE-FH 12 TIMES.                                            
022440     MOVE CI-REQ-NO TO WS-REQ-NO.                                         
022450     MOVE CI-DIR    TO WS-DIR.                                            
022460     MOVE 1 TO C24-SWITCH.                                                
022470     GO TO READ-CARDS.                                                    
022480 MOVE-FH.                                                                 
022490     ADD 1 TO I.                                                          
022492     ADD 1 TO J.                                                          
022494     MOVE C24-COUNT-N (I) TO FH-1 (J).                                    
022500 C24-EXAMINE.                                                     KM072070
022600     EXAMINE C24-COUNT-X (I) REPLACING ALL SPACES BY ZERO.        KM072070
022700 CONVERT-FULL-HOUR.                                               ACT3005 
022710     WRITE TAPE-RECORD FROM CARD-21-SAVE.                                 
022720     MOVE ZERO TO TAPE-RECORD.                                            
022730     MOVE 0    TO I.                                                      
022740     MOVE 0    TO J.                                                      
022750 CONVERT-FH-LOOP.                                                         
022755     ADD 1 TO I.                                                          
022756     ADD 1 TO J.                                                          
022760     MOVE 0          TO T-Q00 (I).                                        
022770     MOVE 0          TO T-Q15 (I).                                        
022780     MOVE 0          TO T-Q30 (I).                                        
022790     MOVE FH-1 (J)   TO T-Q45 (I).                                        
022800     IF I = 4                                                             
022810        GO TO WRITE-FH.                                                   
022820     GO TO CONVERT-FH-LOOP.                                               
022830 WRITE-FH.                                                                
022840     PERFORM WRITE-HH.                                                    
022850     IF J = 24                                                            
022860        PERFORM ZERO-TABLE VARYING I FROM 1 BY 1 UNTIL I > 24             
022870        GO TO CONVERT-FULL-EXIT.                                          
022880     MOVE 0 TO I.                                                         
022890     GO TO CONVERT-FH-LOOP.                                               
022900 CONVERT-FULL-EXIT.                                                       
022910     EXIT.                                                                
023000 ZERO-TABLE.                                                      ACT3005 
023100     MOVE ZERO TO HH-1 (I).                                       ACT3005 
023200     MOVE ZERO TO HH-2 (I).                                       ACT3005 
023300     MOVE ZERO TO FH-1 (I).                                       ACT3005 
023400 EOJ.                                                             ACT3005 
023410     IF C22-SWITCH = 2 OR C23-SWITCH = 2 OR C24-SWITCH = 2        KM032571
023420     DISPLAY CARD-21-SAVE, ' SET REJECTED - INVALID START HR'.    KM032571
023500     IF C23-SWITCH = 1                                            ACT3005 
023600        PERFORM CONVERT-HALF-HOUR THRU CONVERT-HALF-EXIT.         ACT3005 
023700     IF C24-SWITCH = 1                                            ACT3005 
023800        PERFORM CONVERT-FULL-HOUR THRU CONVERT-FULL-EXIT.         ACT3005 
023900     CLOSE CARD-INPUT                                             ACT3005 
024000           TAPE-OUTPUT.                                           ACT3005 
024100     STOP RUN.                                                    ACT3005 
  4 
"