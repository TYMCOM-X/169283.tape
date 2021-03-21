000000 IDENTIFICATION DIVISION.                    
000010 PROGRAM-ID. 'T3010'.                                                     
000020 AUTHOR. CAGNEY FRANCE, S L OPTNER AND ASSOCIATES INC.                    
000030 INSTALLATION. DATA SERVICE BUREAU, DEPT. OF TRAFFIC, CITY OF L.A.        
000040 DATE-WRITTEN. AUGUST 1969.                                               
000050 REMARKS.  PROGRAM ACCEPTS ALL WEEKLY PHASE 2 TRAFFIC CARD INPUT,         
000060     BASIC EDITING IS DONE, THE FOLLOWING FILES ARE CREATED -             
000070     WEEKLY ERROR (DISK) AND VALID WEEKLY INPUT (DISK).                   
000080 ENVIRONMENT DIVISION.                                                    
000090 CONFIGURATION SECTION.                                                   
000100 SOURCE-COMPUTER. IBM-360.                                                
000110 OBJECT-COMPUTER. IBM-360.                                                
000120 INPUT-OUTPUT SECTION.                                                    
000130 FILE-CONTROL.                                                            
000140     SELECT CARDIN ASSIGN TO 'SYS015' UTILITY 2400.               KM120370
000150     SELECT DISKOP ASSIGN TO 'SYS021' UTILITY 2314.               KM120370
000160     SELECT ERROP  ASSIGN TO 'SYS022' UTILITY 2314.               KM120370
000170     SELECT ISFILE ASSIGN TO 'SYS020' DIRECT-ACCESS 2314          KM120370
000172     RESERVE NO ALTERNATE AREA                                    KM120370
000180     ACCESS IS RANDOM ORGANIZATION IS INDEXED                             
000190     SYMBOLIC KEY IS IS-FILE-KEY RECORD KEY IS IS-KEY.                    
000200 I-O-CONTROL.                                                             
000210 DATA DIVISION.                                                           
000220 FILE SECTION.                                                            
000230 FD  CARDIN                                                               
000240     BLOCK CONTAINS 40 RECORDS                                    KM120370
000250     RECORD CONTAINS  80 CHARACTERS                                       
000260     RECORDING MODE IS F                                                  
000270     LABEL RECORDS ARE OMITTED                                            
000280     DATA RECORD IS CARD-REC.                                             
000290 01  CARD-REC.                                                            
000300     03  FILLER                      PICTURE  X(78).                      
000310     03  CARD-CODE                   PICTURE  XX.                         
000320 01  CARD-21 REDEFINES CARD-REC.                                          
000330     03  21-REQ                      PICTURE  X(4).                       
000340     03  21-IS-CODE                  PICTURE  X(10).                      
000350     03  21-IS-C REDEFINES 21-IS-CODE.                                    
000360         05  IS-C-1                  PICTURE X(05).               KM120370
000370         05  IS-C-2                  PICTURE X(05).               KM120370
000380     03  21-LOC1                     PICTURE  X.                          
000390     03  21-LOC2                     PICTURE  X.                          
000400     03  21-LOC3                     PICTURE  X.                          
000410     03  21-DATE                     PICTURE  X(6).                       
000420     03  21-DOW                      PICTURE  X.                          
000430     03  21-PHYS                     PICTURE  X(9).                       
000440     03  21-P-D REDEFINES 21-PHYS.                                        
000450         05  PD-TYPE                 PICTURE  X.                          
000460         05  PD-BAL                  PICTURE  X(8).                       
000470     03  21-DIST                     PICTURE  XX.                         
000480     03  FILLER                      PICTURE  X(45).                      
000490 01  CARD-22 REDEFINES CARD-21.                                           
000500     03  22-REQ                      PICTURE  X(4).                       
000510     03  22-DIR                      PICTURE  X.                          
000520     03  22-TIME-HR                  PICTURE  XX.                         
000530     03  22-TIM-HR REDEFINES 22-TIME-HR  PICTURE  99.                     
000540     03  22-TIME-AP                  PICTURE  X.                          
000550      03  22-COUNTS                   PICTURE  X(64).                     
000560     03  FILLER REDEFINES 22-COUNTS.                                      
000570         05  22-COUNT    OCCURS 16 TIMES   PICTURE X(4).                  
000580     03  FILLER                      PICTURE       X(8).                  
000590 01  CARD-30-34 REDEFINES CARD-22.                                        
000600     03 30-34-REQ                    PICTURE XXX.                         
000610     03 30-34-IS-CODE                PICTURE X(10).                       
000620     03 30-34-REQUESTER              PICTURE X(15).                       
000630     03 30-34-DATE                   PICTURE X(12).                       
000640     03 FILLER                       PICTURE X(40).                       
000650 01  CARD-35-36 REDEFINES CARD-30-34.                                     
000660     03 35-36-REQ                    PICTURE XXX.                         
000670     03 CAT-PARA             PICTURE X(42).                               
000680     03 CATEGORY REDEFINES CAT-PARA OCCURS 14 TIMES                       
000690                             PICTURE XXX.                                 
000700     03 CAT-LIMIT                    PICTURE XX.                          
000710     03 35-36-REQUESTER              PICTURE X(11).                       
000720     03 35-36-DATE                   PICTURE X(12).                       
000730     03 35-36-TIME                   PICTURE X(8).                        
000740     03 FILLER                       PICTURE XX.                          
000750 01  CARD-14 REDEFINES CARD-35-36.                                        
000760     03 14-IS-CODE-OLD               PICTURE X(10).                       
000770     03 14-IS-CODE-NEW               PICTURE X(10).                       
000780     03 14-NAME-1                    PICTURE X(29).                       
000790     03 14-NAME-2                    PICTURE X(29).                       
000800     03 FILLER                       PICTURE XX.                          
000810 01  CARD-15 REDEFINES CARD-14.                                           
000820     03 15-IS-CODE                   PICTURE X(10).                       
000830     03 15-NAME-1                    PICTURE X(30).                       
000840     03 15-NAME-2                    PICTURE X(30).                       
000850     03 15-CLASS-CODE                PICTURE X.                           
000860     03 15-LEGS                      PICTURE X.                           
000870     03 15-CODE                      PICTURE X.                           
000880     03 15-MAINT-AREA                PICTURE XXXX.                        
000890     03 15-TR-DIST                   PICTURE X.                           
000900     03 FILLER                       PICTURE XX.                          
000910 01  CARD-25 REDEFINES CARD-15.                                           
000920     03 25-IS-CODE                   PICTURE X(10).                       
000930     03 25-DATE                      PICTURE X(6).                        
000940     03 25-DOW                       PICTURE X.                           
000950     03 25-LOC                       PICTURE XXX.                         
000960     03 25-DIST                      PICTURE XX.                          
000970     03 25-AM-PEAK                   PICTURE X(14).                       
000980     03 25-PM-PEAK                   PICTURE X(14).                       
000990     03 25-T-6-HR                    PICTURE X(10).                       
001000     03 25-ADJ-FACTOR                PICTURE XXX.                         
001010     03 FILLER                       PICTURE X(17).                       
001020 01  CARD-27 REDEFINES CARD-25.                                           
001030     03 27-IS-CODE                   PICTURE X(10).                       
001040     03 27-DATE                      PICTURE X(6).                        
001050     03 27-LOC                       PICTURE XXX.                         
001060     03 27-TYPE                      PICTURE X.                           
001070     03 27-ACTION                    PICTURE X.                           
001080     03 FILLER                       PICTURE X(59).                       
001090 01  CARD-31-32-33 REDEFINES CARD-27.                                     
001100     03 31-REQ                       PICTURE XXX.                         
001110     03 31-IS-CODE                   PICTURE X(10).                       
001120     03 31-LAST-IS-CODE              PICTURE X(5).                        
001130     03 31-CODE                      PICTURE X.                           
001140     03 31-REQUESTER                 PICTURE X(15).                       
001150     03 31-DATE                      PICTURE X(12).                       
001160     03 FILLER                       PICTURE X(34).                       
001170 01  CARD-28 REDEFINES CARD-31-32-33.                                     
001180     03 28-NO                PICTURE X(5).                                
001182     03 28-29-N1 REDEFINES 28-NO.                                 KM021971
001184        05 28-29-JURIS       PICTURE X(01).                       KM021271
001186        05 28-29-NO          PICTURE X(04).                       KM021271
001187     03 28-29-N2 REDEFINES 28-29-N1.                              KM021971
001188        05 FILLER            PICTURE X(04).                       KM021271
001189        05 28-29-NO-5        PICTURE X(01).                       KM021271
001190     03 28-AST               PICTURE X.                                   
001200     03 28-C                 PICTURE X.                                   
001210     03 28-A                 PICTURE X.                                   
001220     03 28-J                 PICTURE X.                                   
001230     03 28-K                 PICTURE X.                                   
001240     03 28-L1                PICTURE X.                                   
001250     03 28-L2                PICTURE X.                                   
001260     03 28-M                 PICTURE X.                                   
001270     03 FILLER               PICTURE XXX.                                 
001280     03 28-DATE              PICTURE X(6).                                
001290     03 28-S-DIST            PICTURE X(5).                                
001300     03 28-SEG-VOL           PICTURE X(6).                                
001310     03 FILLER               PICTURE X(47).                               
001320 01  CARD-29 REDEFINES CARD-28.                                           
001330     03 29-NO                PICTURE X(5).                                
001340     03 29-S-DIST            PICTURE X(5).                                
001350     03 29-S-VOL             PICTURE X(6).                                
001360     03 29-SEG-CLASS.                                                     
001370     05 29-C                 PICTURE X.                                   
001380     05 29-A                 PICTURE X.                                   
001390     05 29-J                 PICTURE X.                                   
001400     05 29-K                 PICTURE X.                                   
001410     05 29-L1                PICTURE X.                                   
001420     05 29-L2                PICTURE X.                                   
001430     05 29-M                 PICTURE X.                                   
001440     05 FILLER               PICTURE XXX.                                 
001450     03 29-DATE              PICTURE X(6).                                
001460     03 29-ROUTE             PICTURE X(5).                                
001470     03 29-X-1               PICTURE X(5).                                
001480     03 29-X-2               PICTURE X(5).                                
001490     03 29-STRAF             PICTURE XXX.                                 
001500     03 FILLER               PICTURE X(30).                               
001510 FD  DISKOP                                                               
001520     RECORD CONTAINS 100 CHARACTERS                                       
001530     BLOCK CONTAINS 72 RECORDS                                            
001540     RECORDING MODE IS F                                                  
001550     LABEL RECORDS ARE STANDARD                                           
001560     DATA RECORD IS DISK-REC.                                             
001570 01  DISK-REC.                                                            
001580     03  DISK-1-80                   PICTURE  X(80).                      
001590     03  DISK-81-100                 PICTURE  X(20).                      
001600     03  END-21 REDEFINES DISK-81-100.                                    
001610         05  21-KEY                  PICTURE  X(4).                       
001620         05  21-NO                   PICTURE  X(4).                       
001630         05  21-SEQ                  PICTURE  X.                          
001640         05  21-BAL                  PICTURE  X(11).                      
001650     03  END-22 REDEFINES END-21.                                         
001660         05  22-KEY                  PICTURE  X(4).                       
001670         05  22-NO                   PICTURE  X(4).                       
001680         05  22-SEQ                  PICTURE  X.                          
001690         05  22-DIRTN                PICTURE  X.                          
001700         05  22-TIME                 PICTURE  99.                         
001710         05  22-BAL                  PICTURE  X(8).                       
001720 FD  ERROP                                                                
001730     RECORD CONTAINS 100 CHARACTERS                                       
001740     BLOCK CONTAINS 72 RECORDS                                            
001750     RECORDING MODE IS F                                                  
001760     LABEL RECORDS ARE STANDARD                                           
001770     DATA RECORD IS ERROR-REC.                                            
001780 01  ERROR-REC.                                                           
001790     03 ER-1-80                      PICTURE  X(80).                      
001800     03 ER-CODE1                     PICTURE  XX.                         
001810     03 ER-COL1                      PICTURE  XX.                         
001820     03 ER-CODE2                     PICTURE XX.                          
001830     03 ER-COL2                      PICTURE  XX.                         
001840     03 ER-CODE3                     PICTURE XX.                          
001850     03 ER-COL3                      PICTURE  XX.                         
001860     03 FILLER                       PICTURE X(6).                        
001870     03 ERROR-CODE                   PICTURE XX.                          
001880 FD  ISFILE                                                               
001890     RECORD CONTAINS 300 CHARACTERS                                       
001900     BLOCK CONTAINS  10 RECORDS                                   KM120171
001910     RECORDING MODE IS F                                                  
001920     LABEL RECORDS ARE STANDARD                                           
001930                                                                          
001940                                                                          
001950     DATA RECORD IS IS-REC.                                               
001960 01  IS-REC.                                                              
001970     03  FILLER                      PICTURE  X.                          
001980     03  IS-KEY                      PICTURE  X(10).                      
001990     03 FILLER                       PICTURE X(289).                      
002000 WORKING-STORAGE SECTION.                                                 
002010 77  FILLER-1        PICTURE X(2000) VALUE SPACES.                        
002020 77  FILLER-2        PICTURE X(2000) VALUE SPACES.                        
002030 77  FILLER-3        PICTURE X(2000) VALUE SPACES.                        
002040 77  FILLER-4        PICTURE X(2000) VALUE SPACES.                        
002050 77  ERR-CODE                        PICTURE  99.                         
002060 77  DATE-ERR-SW                     PICTURE  9      VALUE 0.             
002070 77  ERR-COL                         PICTURE  99.                         
002080 77  INDEX-1     COMPUTATIONAL       PICTURE  S99    VALUE 01.            
002090 77  INDEX-2     COMPUTATIONAL       PICTURE  S99    VALUE 01.            
002100 77  COUNT-HOLD                      PICTURE  9(4).                       
002110 77  IS-H                            PICTURE X(5).                        
002120 77  ER-S                            PICTURE 9 VALUE ZERO.                
002130 77  S                               PICTURE 9 VALUE ZERO.                
002140 77  E                               PICTURE 9 VALUE ZERO.                
002150 77  C                               PICTURE 99.                          
002160 77  NE-SW                           PICTURE 9 VALUE ZERO.                
002170 77  SW-SW                      PICTURE 9 VALUE ZERO.                
002175 77  VOL-FLUSH-SW                    PICTURE X(01).               KM020271
002180 01  IS-FILE-KEY.                                                         
002190     03  IS-ST-1                     PICTURE  X(5).                       
002200     03  IS-ST-2                     PICTURE  X(5).                       
002210 01  DATE-WORK.                                                           
002220     03  DW-MO                       PICTURE  XX.                         
002230     03  DW-DA                       PICTURE  XX.                         
002240     03  DW-YR                       PICTURE  XX.                         
       01  CH-2            PICTURE XX.                                          
       01  FILLER REDEFINES CH-2.                                               
           03 FILLER       PICTURE X.                                           
           03 CH-2A        PICTURE X.                                           
002250 01  COMPUTERS-DATE.                                                      
002260                                                                  KM120370
002270     03 DATE.                                                             
002280        05 MO                        PICTURE 99.                          
002282        05 FILLER                PICTURE X(01).                   KM120370
002290        05 DA                        PICTURE 99.                          
002292        05 FILLER                PICTURE X(01).                   KM120370
002300        05 YR                        PICTURE 99.                          
002310     03 FILLER                       PICTURE X(7).                        
002320 01  WORK-TIME.                                                           
002330     03 FROM-TIME                    PICTURE XXXX.                        
002340     03 TO-TIME                      PICTURE XXXX.                        
002350 01  T-WORK.                                                              
002360     03 TIME-H                       PICTURE XX.                          
002370     03 TIME-M                       PICTURE XX.                          
002380 01    IS-TEST                       PICTURE X(10).                       
002390 01    IS-TESTX REDEFINES IS-TEST.                                        
002400        03 FILLER                    PICTURE X(9).                        
002410        03 IS-TEST-1                 PICTURE X.                           
002420 01  LOC-WORK.                                                            
002430     03 LOC-W1                       PICTURE X.                           
002440     03 LOC-W2                       PICTURE X.                           
002450     03 LOC-W3                       PICTURE X.                           
002460 01  CARD-CODE-HOLD.                                                      
002470     03 FILLER                       PICTURE XX VALUE ZEROES.             
002480     03 CODE-HOLD                    PICTURE XX.                          
002490 01  REQ-TEST                        PICTURE XXX.                         
002500 01  REQ-TESTX REDEFINES REQ-TEST.                                        
002510     03 FILLER                       PICTURE XX.                          
002520     03 REQ-TEST-1                   PICTURE X.                           
002530 01  WORK-DAT.                                                            
002540     03 FROM-DATE                    PICTURE X(6).                        
002550     03 TO-DATE                      PICTURE X(6).                        
002560 01  CAT-HOLD                        PICTURE XXX.                         
002570 01  CAT-HOLDX REDEFINES CAT-HOLD.                                        
002580     03 FILLER                       PICTURE XX.                          
002590     03 CAT-1                        PICTURE X.                           
002600 01  PEAK-WORK.                                                           
002610     03 AM-NE.                                                            
002620        05 NE-HR.                                                         
002630           07 NE-HR-1                PICTURE XX.                          
002640           07 NE-HR-2                PICTURE X.                           
002650        05 NE-VOL                    PICTURE XXXX.                        
002660        05 NE-VOLX REDEFINES NE-VOL.                                      
002670           07 FILLER                 PICTURE XXX.                         
002680           07 NE-VOL-1               PICTURE X.                           
002690     03 AM-SW.                                                            
002700        05 SW-HR.                                                         
002710           07 SW-HR-1                PICTURE XX.                          
002720           07 SW-HR-2                PICTURE X.                           
002730        05 SW-VOL                    PICTURE XXXX.                        
002740        05 SW-VOLX REDEFINES SW-VOL.                                      
002750           07 FILLER                 PICTURE XXX.                         
002760           07 SW-VOL-1               PICTURE X.                           
002770 01  6-HR-TOT.                                                            
002780     03 6-HR-NE                      PICTURE X(5).                        
002790     03 6-HR-NEX REDEFINES 6-HR-NE.                                       
002800        05 FILLER                    PICTURE XXXX.                        
002810        05 6-HR-NE-1                 PICTURE X.                           
002820     03 6-HR-SW                      PICTURE X(5).                        
002830 01  INPUT-COUNT.                                                         
002840        03 ACC-14    PICTURE 9999 VALUE ZEROES.                           
002850        03 ACC-15    PICTURE 9999 VALUE ZEROES.                           
002860        03 ACC-21    PICTURE 9999 VALUE ZEROES.                           
002870        03 ACC-22    PICTURE 9999 VALUE ZEROES.                           
002880        03 ACC-25    PICTURE 9999 VALUE ZEROES.                           
002890        03 ACC-27    PICTURE 9999 VALUE ZEROES.                           
002900        03 ACC-28    PICTURE 9999 VALUE ZEROES.                           
002910        03 ACC-29    PICTURE 9999 VALUE ZEROES.                           
002920        03 ACC-30    PICTURE 9999 VALUE ZEROES.                           
002930        03 ACC-31    PICTURE 9999 VALUE ZEROES.                           
002940        03 ACC-32    PICTURE 9999 VALUE ZEROES.                           
002950        03 ACC-33    PICTURE 9999 VALUE ZEROES.                           
002960        03 ACC-34    PICTURE 9999 VALUE ZEROES.                           
002970        03 ACC-35    PICTURE 9999 VALUE ZEROES.                           
002980        03 ACC-36    PICTURE 9999 VALUE ZEROES.                           
002990        03 ACC-XX    PICTURE 9999 VALUE ZEROES.                           
003000 01  SE-TEST                 PICTURE X(5).                                
003010 01  SE-TESTX REDEFINES SE-TEST.                                          
003020     03 FILLER               PICTURE XXXX.                                
003030     03 SE-TEST-1            PICTURE X.                                   
003040 01  SE-T                    PICTURE X(6).                                
003050 01  SE-TX REDEFINES SE-T.                                                
003060     03 FILLER               PICTURE X(5).                                
003070     03 SE-T-1               PICTURE X.                                   
003080 PROCEDURE DIVISION.                                                      
003090     ENTER LINKAGE.                                                       
003100     CALL 'TODAY' USING COMPUTERS-DATE.                           KM120370
003110     ENTER COBOL.                                                         
003120 START.                                                                   
003130     OPEN INPUT  CARDIN                                           KM120370
003132                 ISFILE                                           KM120370
003140          OUTPUT DISKOP                                           KM120370
003142                 ERROP.                                           KM120370
003150 READ-CARD.                                                               
003160     MOVE SPACES TO ERROR-REC MOVE ZERO TO S E.                           
003170     READ CARDIN RECORD AT END GO TO END-OF-JOB.                          
003180     IF CARD-CODE IS EQUAL TO 21 GO TO ROUT-21.                           
003190     IF CARD-CODE IS EQUAL TO 22 GO TO ROUT-22.                           
003200     IF CARD-CODE = '14' MOVE '14' TO CODE-HOLD                           
003210     ADD 1 TO ACC-14                                                      
003220         PERFORM EDIT-RTN-14 THRU EDIT-RTN-14-X, GO TO READ-CARD.         
003230     IF CARD-CODE = '15' MOVE '15' TO CODE-HOLD                           
003240     ADD 1 TO ACC-15                                                      
003250         PERFORM EDIT-RTN-15 THRU EDIT-RTN-15-X, GO TO READ-CARD.         
003260     IF CARD-CODE = '25' MOVE '25' TO CODE-HOLD                           
003270         PERFORM EDIT-RTN-25 THRU EDIT-RTN-25-X, GO TO READ-CARD.         
003280     IF CARD-CODE = '27' MOVE '27' TO CODE-HOLD                           
003290         PERFORM EDIT-RTN-27 THRU EDIT-RTN-27-X, GO TO READ-CARD.         
003300     IF CARD-CODE = '28' MOVE '28' TO CODE-HOLD                           
003310         ADD 1 TO ACC-28                                                  
003320         PERFORM EDIT-RTN-28 THRU EDIT-RTN-28-X, GO TO READ-CARD.         
003330     IF CARD-CODE = '29' MOVE '29' TO CODE-HOLD                           
003340         ADD 1 TO ACC-29                                                  
003350         PERFORM EDIT-RTN-29 THRU EDIT-RTN-29-X, GO TO READ-CARD.         
003360     IF CARD-CODE = '30' MOVE '30' TO CODE-HOLD                           
003370     ADD 1 TO ACC-30                                                      
003380         PERFORM EDIT-RTN-30 THRU EDIT-RTN-30-X, GO TO READ-CARD.         
003390     IF CARD-CODE = '31' MOVE '31' TO CODE-HOLD                           
003400     ADD 1 TO ACC-31                                                      
003410         PERFORM EDIT-RTN-31 THRU EDIT-RTN-31-X, GO TO READ-CARD.         
003420     IF CARD-CODE = '32' MOVE '32' TO CODE-HOLD                           
003430     ADD 1 TO ACC-32                                                      
003440         PERFORM EDIT-RTN-32 THRU EDIT-RTN-32-X, GO TO READ-CARD.         
003450     IF CARD-CODE = '33' MOVE '33' TO CODE-HOLD                           
003460     ADD 1 TO ACC-33                                                      
003470         PERFORM EDIT-RTN-33 THRU EDIT-RTN-33-X, GO TO READ-CARD.         
003480     IF CARD-CODE = '34' MOVE '34' TO CODE-HOLD                           
003490     ADD 1 TO ACC-34                                                      
003500         PERFORM EDIT-RTN-34 THRU EDIT-RTN-34-X, GO TO READ-CARD.         
003510     IF CARD-CODE = '35' MOVE '35' TO CODE-HOLD                           
003520     ADD 1 TO ACC-35                                                      
003530         PERFORM EDIT-RTN-35 THRU EDIT-RTN-35-X, GO TO READ-CARD.         
003540     IF CARD-CODE = '36' MOVE '36' TO CODE-HOLD                           
003550     ADD 1 TO ACC-36                                                      
003560         PERFORM EDIT-RTN-36 THRU EDIT-RTN-36-X, GO TO READ-CARD.         
003570     PERFORM CARD-CODE-ERROR THRU CARD-CODE-ERROR-X                       
003580     ADD 1 TO ACC-XX                                                      
003590         GO TO READ-CARD.                                                 
003600 ROUT-21.                                                                 
003605     MOVE ZERO TO VOL-FLUSH-SW.                                   KM020271
003610     IF 21-REQ IS NOT NUMERIC, MOVE 01 TO ERR-CODE, MOVE 01 TO            
003620             ERR-COL, PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.          
003630                                                                  KM120370
003640                                                                  KM120370
003650     IF IS-C-1 IS GREATER THAN IS-C-2, MOVE IS-C-2 TO IS-ST-1,            
003660             MOVE IS-C-1 TO IS-ST-2, ELSE MOVE 21-IS-CODE                 
003670             TO IS-FILE-KEY.                                              
003680             READ ISFILE RECORD INVALID KEY PERFORM ERR-02.               
003690 R-21-A.                                                                  
003700     IF 21-LOC1  IS LESS THAN 1 OR 21-LOC1  IS GREATER THAN 6,            
003710             MOVE 03 TO ERR-CODE, MOVE 15 TO ERR-COL,                     
003720             PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                   
003730     IF 21-LOC2  IS NOT EQUAL TO 2 AND 21-LOC2 IS NOT EQUAL TO 8          
003740            AND 21-LOC2  IS NOT EQUAL TO  0, MOVE 03 TO ERR-CODE,         
003750             MOVE 16 TO ERR-COL,                                          
003760             PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                   
003770     IF 21-LOC3  IS LESS THAN 0 OR 21-LOC3 IS GREATER THAN 5,             
003780             MOVE 03 TO ERR-CODE, MOVE 17 TO ERR-COL,                     
003790             PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                   
003800     MOVE 21-DATE TO DATE-WORK, PERFORM COMMON-DATE-EDIT, IF              
003810             DATE-ERR-SW IS EQUAL TO 1, MOVE 0 TO DATE-ERR-SW,            
003820             MOVE 04 TO ERR-CODE, MOVE 18 TO ERR-COL,                     
003830             PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                   
003840     IF 21-DOW IS LESS THAN 1 OR 21-DOW IS GREATER THAN  7, MOVE          
003850             07 TO ERR-CODE,  MOVE 24 TO ERR-COL,                         
003860             PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                   
003870     IF PD-BAL IS NOT NUMERIC MOVE 08 TO ERR-CODE,                        
003880             MOVE 26 TO ERR-COL, PERFORM OUTPUT-ERROR-ROUTINE             
003890             THRU OP-EXIT.                                                
003900     IF 21-DIST IS NOT ALPHABETIC MOVE 14 TO ERR-CODE,                    
003910             MOVE 34 TO ERR-COL, PERFORM OUTPUT-ERROR-ROUTINE             
003920             THRU OP-EXIT.                                                
003930     IF ER-1-80 IS EQUAL TO CARD-REC AND ER-CODE1 IS NOT EQUAL TO         
003940             SPACES, PERFORM I-C-2 THRU OP-EXIT, GO TO READ-CARD.         
003950     IF ER-1-80 IS EQUAL TO CARD-REC GO TO READ-CARD.                     
003960             GO TO GEN-21.                                                
003970 ERR-02.                                                                  
003980     MOVE 02 TO ERR-CODE, MOVE 05 TO ERR-COL, PERFORM                     
003990             OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
004000 GEN-21.                                                                  
004005     IF VOL-FLUSH-SW = 1 GO TO READ-CARD.                         KM020271
004010     MOVE CARD-REC TO DISK-1-80.                                          
004020     MOVE ZEROS TO 21-KEY.                                                
004030     MOVE 21-REQ TO 21-NO.                                                
004040     MOVE 1 TO 21-SEQ.                                                    
004050     MOVE ZEROS TO 21-BAL.                                                
004060     PERFORM WRITE-OP-ROUTINE.                                            
004070     GO TO READ-CARD.                                                     
004080 ROUT-22.                                                                 
004082     IF VOL-FLUSH-SW = 1 GO TO READ-CARD.                         KM020271
004090     IF 22-REQ IS NOT NUMERIC MOVE 01 TO ERR-CODE, MOVE 01 TO             
004100             ERR-COL, PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.          
004110     IF 22-DIR IS NOT EQUAL TO '1', AND   22-DIR IS NOT EQUAL TO          
004120             '2' AND 22-DIR IS NOT EQUAL TO '3', AND 22-DIR IS NOT        
004130             EQUAL TO '4', AND 22-DIR IS NOT EQUAL TO '5',                
004140             MOVE 05 TO ERR-CODE, MOVE 05 TO                              
004150             ERR-COL, PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.          
004160     IF 22-TIME-HR IS NOT EQUAL TO 12, AND 22-TIME-HR IS NOT EQUAL        
004170             TO 04 AND   22-TIME-HR IS NOT EQUAL TO 08, MOVE 06           
004180             TO ERR-CODE, MOVE 06 TO ERR-COL, PERFORM                     
004190             OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
004200     IF 22-TIME-AP IS NOT EQUAL TO '1', AND   22-TIME-AP IS NOT           
004210             EQUAL TO '2', MOVE 06 TO ERR-CODE, MOVE 08 TO                
004220             ERR-COL, PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.          
004230 EDIT-COUNTS.                                                             
004240     MOVE 01 TO INDEX-1.                                                  
004250     MOVE 02 TO INDEX-2.                                                  
004260 LOOP-1.                                                                  
004270     IF 22-COUNT (INDEX-1) IS NOT NUMERIC, PERFORM COUNT-ERR-1.           
004280     IF 22-COUNT (INDEX-2) IS NOT NUMERIC, PERFORM COUNT-ERR-2.           
004290     IF 22-COUNT (INDEX-2) IS LESS THAN 22-COUNT (INDEX-1),               
004300             PERFORM COUNT-ERR-2.                                         
004310     ADD 01 TO INDEX-1.                                                   
004320     ADD 01 TO INDEX-2.                                                   
004330     IF INDEX-1 IS EQUAL TO 4, ADD 1 TO INDEX-1, ADD 1 TO INDEX-2.        
004340     IF INDEX-1 IS EQUAL TO 8, ADD 1 TO INDEX-1, ADD 1 TO INDEX-2.        
004350     IF INDEX-1 IS EQUAL TO 12 ADD 1 TO INDEX-1, ADD 1 TO INDEX-2.        
004360     IF INDEX-1 IS EQUAL TO 16, GO TO GEN-22.                             
004370     GO TO LOOP-1.                                                        
004380 COUNT-ERR-1.                                                             
004390     MOVE 17 TO ERR-CODE.  COMPUTE ERR-COL =                              
004400             (5 + (INDEX-1 * 4)).                                         
004410     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
004420 COUNT-ERR-2.                                                             
004430     MOVE 17 TO ERR-CODE.                                                 
004440     COMPUTE ERR-COL = (09 + (INDEX-1 * 4)).                              
004450     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
004460 GEN-22.                                                                  
004462     IF VOL-FLUSH-SW = 1 GO TO READ-CARD.                         KM020271
004470     IF ER-1-80 IS EQUAL TO CARD-REC AND ER-CODE1 IS NOT EQUAL TO         
004480             SPACES, PERFORM I-C-2 THRU OP-EXIT, GO TO READ-CARD.         
004490     IF ER-1-80 IS EQUAL TO CARD-REC, GO TO READ-CARD.                    
004500     MOVE CARD-REC TO DISK-1-80.                                          
004510     MOVE ZEROS TO 22-KEY.                                                
004520     MOVE 22-REQ TO 22-NO.                                                
004530     MOVE 2 TO 22-SEQ.                                                    
004540     IF 22-DIR = 1, MOVE 'N' TO 22-DIR.                                   
004550     IF 22-DIR = 2, MOVE 'E' TO 22-DIR.                                   
004560     IF 22-DIR = 3, MOVE 'S' TO 22-DIR.                                   
004570     IF 22-DIR = 4, MOVE 'W' TO 22-DIR.                                   
004580     IF 22-DIR = 5, MOVE 'B' TO 22-DIR.                                   
004590     MOVE 22-DIR TO 22-DIRTN.                                             
004600     MOVE 22-TIM-HR  TO 22-TIME.                                          
004610     IF 22-TIME-HR IS EQUAL TO 12, MOVE 00 TO 22-TIME.                    
004620     IF 22-TIME-AP IS EQUAL TO '2' ADD 12 TO 22-TIME.                     
004630     MOVE ZEROS TO 22-BAL.                                                
004640     PERFORM WRITE-OP-ROUTINE.                                            
004650     GO TO READ-CARD.                                                     
004660 COMMON-DATE-EDIT.                                                        
004670     IF DW-MO IS LESS THAN 01 OR    DW-MO IS GREATER THAN 12              
004680             MOVE 1 TO DATE-ERR-SW.                                       
004690     IF DW-DA IS LESS THAN 01 OR    DW-DA IS GREATER THAN 31,             
004700             MOVE 1 TO DATE-ERR-SW.                                       
           IF DW-YR LESS THAN ZERO OR DW-YR GREATER THAN YR                     
           MOVE 1 TO DATE-ERR-SW.                                               
004720 OUTPUT-ERROR-ROUTINE.                                                    
004722     IF CARD-CODE = 21 OR                                         KM020271
004724        CARD-CODE = 22                                            KM020271
004726        MOVE 1 TO VOL-FLUSH-SW.                                   KM020271
004730     IF ERROR-REC IS EQUAL TO SPACES GO TO INSERT-CODE.                   
004740     IF ER-1-80 IS EQUAL TO CARD-REC GO TO I-C-1.                         
004750     MOVE CARD-CODE TO ERROR-CODE.                                        
004760     WRITE ERROR-REC.                                                     
004770 INSERT-CODE.                                                             
004780     MOVE CARD-REC TO ER-1-80.                                            
004790 I-C-1.                                                                   
004800     IF ER-CODE1 = SPACES MOVE 1 TO E MOVE ERR-CODE TO ER-CODE1           
004810             MOVE ERR-COL TO ER-COL1, GO TO OP-EXIT.                      
004820     IF ER-CODE2 IS EQUAL TO SPACES, MOVE ERR-CODE TO ER-CODE2,           
004830             MOVE ERR-COL TO ER-COL2, GO TO OP-EXIT.                      
004840     MOVE ERR-CODE TO ER-CODE3, MOVE ERR-COL TO ER-COL3.                  
004850 I-C-2.                                                                   
004860     MOVE CARD-CODE TO ERROR-CODE.                                        
004870     WRITE ERROR-REC.                                                     
004880     MOVE 1 TO S.                                                         
004890     MOVE CARD-REC TO ER-1-80.                                            
004900     MOVE SPACES TO ER-CODE1.                                             
004910     MOVE SPACES TO ER-CODE2.                                             
004920     MOVE SPACES TO ER-CODE3.                                             
004930     MOVE SPACES TO ER-COL1.                                              
004940     MOVE SPACES TO ER-COL2.                                              
004950     MOVE SPACES TO ER-COL3.                                              
004960 OP-EXIT.                                                                 
004970     EXIT.                                                                
004980 WRITE-OP-ROUTINE.                                                        
004990     WRITE DISK-REC.                                                      
005000 END-OF-JOB.                                                              
005010     DISPLAY 'COUNT  TYPE INPUT'.                                         
005020     DISPLAY '                 '.                                         
005030     DISPLAY ACC-14 '    I/S FILE CODE CHANGE (14)            '.          
005040     DISPLAY ACC-15 '    I/S FILE REVISIONS (15)              '.          
005050     DISPLAY ACC-28 '    NETWORK FILE REVISIONS (28)          '.          
005060     DISPLAY ACC-29 '    NETWORK FILE ADDITIONS (29)          '.          
005070     DISPLAY ACC-30 '    TRAFFIC ACCIDENT/LOC ANALYSIS (30)   '.          
005080     DISPLAY ACC-31 '    TRAFFIC ACCIDENT/LOC ANALYSIS (31)   '.          
005090     DISPLAY ACC-32 '    ACCIDENT ROUTE ANALYSIS (32)         '.          
005100     DISPLAY ACC-33 '    ACCIDENT DISTRIBUTION CHART (33)     '.          
005110     DISPLAY ACC-34 '    HISTORICAL TRAFFIC VOLUME BY LOC (34)'.          
005120     DISPLAY ACC-35 '    ACCIDENT CATEGORY-LOC (35)           '.          
005130     DISPLAY ACC-36 '    ACCIDENT CATEGORY-SEGMENT (36)       '.          
005140     DISPLAY ACC-XX '    UNIDENTIFIED INPUT                   '.          
005150     CLOSE CARDIN, DISKOP.                                                
005160     CLOSE ISFILE ERROP.                                                  
005170     STOP RUN.                                                            
005180 EDIT-RTN-14.                                                             
005190     MOVE 14-IS-CODE-OLD TO IS-FILE-KEY.                                  
005200     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
005210     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
005220         MOVE 01 TO ERR-COL  PERFORM OUTPUT-ERROR-ROUTINE                 
005230         THRU OP-EXIT.                                                    
005240     MOVE 14-IS-CODE-NEW TO IS-TEST.                                      
005250     IF  IS-TEST  NOT NUMERIC GO TO RTN-14-A.                             
005260     IF  IS-TEST-1  < ZERO NEXT SENTENCE ELSE GO TO RTN-14-B.             
005270                                                                          
005280 RTN-14-A.                                                                
005290     MOVE 02 TO ERR-CODE MOVE 11 TO ERR-COL                               
005300     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
005310 RTN-14-B.                                                                
005320     IF 14-NAME-1 = SPACES  MOVE 21 TO ERR-COL PERFORM RTN-14-C.          
005330     IF ER-1-80 = CARD-REC AND ER-CODE1 = SPACES                          
005340         GO TO EDIT-RTN-14-X.                                             
005350     IF 14-NAME-2 NOT = SPACES  GO TO RTN-14-D.                           
005360     MOVE 50 TO ERR-COL.                                                  
005370 RTN-14-C.                                                                
005380     MOVE 09 TO ERR-CODE PERFORM  OUTPUT-ERROR-ROUTINE                    
005390         THRU OP-EXIT.                                                    
005400 RTN-14-D.                                                                
005410     IF ER-1-80 = CARD-REC AND ER-CODE1 NOT = SPACES                      
005420         PERFORM I-C-2 THRU OP-EXIT GO TO EDIT-RTN-14-X.                  
005430     IF  ER-1-80 = CARD-REC GO TO EDIT-RTN-14-X.                          
005440     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
005450 EDIT-RTN-14-X. EXIT.                                                     
005460 1F.                                                                      
005470     MOVE 15-IS-CODE TO IS-TEST.                                          
005480     IF IS-TEST NOT NUMERIC GO TO 1F-A.                                   
005490     IF IS-TEST-1 < ZERO NEXT SENTENCE ELSE GO TO 1F-X.                   
005500 1F-A.                                                                    
005510     MOVE 02 TO ERR-CODE MOVE 01 TO ERR-COL                               
005520      PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                          
005530 1F-X.  EXIT.                                                             
005540 EDIT-RTN-15.                                                             
005550     IF 15-CODE = '1' GO TO X5.                                   KM010571
005560     MOVE 15-IS-CODE TO IS-FILE-KEY.                                      
005570     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
005580     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
005590         MOVE 01 TO ERR-COL  PERFORM OUTPUT-ERROR-ROUTINE                 
005600         THRU OP-EXIT.                                                    
005610     IF 15-CODE = ZERO OR 15-CODE > 3 MOVE 12 TO ERR-CODE                 
005620         MOVE 73 TO ERR-COL PERFORM OUTPUT-ERROR-ROUTINE                  
005630         THRU OP-EXIT GO TO RTN-15-C.                                     
005640 X5. IF 15-CODE = '1' AND 15-NAME-1 = SPACES                              
005650         MOVE 11 TO ERR-COL PERFORM RTN-15-A.                             
005660     IF 15-CODE = '1' AND 15-NAME-2 = SPACES NEXT SENTENCE ELSE           
005670         GO TO RTN-15-B.                                                  
005680         MOVE 41 TO ERR-COL.                                              
005690 RTN-15-A.                                                                
005700     MOVE 09 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                     
005710         THRU OP-EXIT.                                                    
005720 RTN-15-B.                                                                
005730     IF 15-CODE < ZERO NEXT SENTENCE ELSE GO TO RTN-15-C.                 
005740     IF S = 1 GO TO EDIT-RTN-15-X.                                        
005750     IF 15-CLASS-CODE NOT = SPACES NEXT SENTENCE ELSE GO TO Y1.           
005760     IF 15-CLASS-CODE < '0' MOVE 10 TO ERR-CODE MOVE 71 TO ERR-COL        
005770         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
005780 Y1. IF S = 1 GO TO EDIT-RTN-15-X.                                        
005790     IF 15-LEGS NOT = SPACES NEXT SENTENCE ELSE GO TO  Y2.                
005800     IF 15-LEGS < '1' MOVE 72 TO ERR-COL                                  
005810     MOVE 11 TO ERR-CODE                                                  
005820         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
005830 Y2. IF S = 1 GO TO EDIT-RTN-15-X.                                        
005840     IF 15-MAINT-AREA NOT = SPACES NEXT SENTENCE ELSE GO TO Y3.           
005850     IF 15-MAINT-AREA < '1000' OR 15-MAINT-AREA > '7250'                  
005860         MOVE 74 TO ERR-COL MOVE 13 TO ERR-CODE PERFORM                   
005870         OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                               
005880 Y3. IF S = 1 GO TO EDIT-RTN-15-X.                                        
005890     IF 15-TR-DIST    NOT = SPACES NEXT SENTENCE ELSE                     
005900         GO TO RTN-15-C.                                                  
005910     IF 15-TR-DIST < '1' OR 15-TR-DIST > '5' MOVE 78 TO ERR-COL           
005920         MOVE 15 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
005930         THRU OP-EXIT.                                                    
005940 RTN-15-C.                                                                
005950     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
005960         GO TO EDIT-RTN-15-X.                                             
005970     IF S = 1 GO TO EDIT-RTN-15-X.                                        
005980     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
005990 EDIT-RTN-15-X. EXIT.                                                     
006000 EDIT-RTN-25.                                                             
006010     EXAMINE 25-AM-PEAK REPLACING ALL SPACES BY ZEROES.                   
006020     EXAMINE 25-PM-PEAK REPLACING ALL SPACES BY ZEROES.                   
006030     EXAMINE 25-T-6-HR REPLACING ALL SPACES BY ZEROES.                    
006040     PERFORM EDIT-RTN-27.                                                 
006050     IF 25-DOW > ZERO AND 25-DOW < '8' GO TO RTN-25-A.                    
006060         MOVE 07 TO ERR-CODE MOVE 17 TO ERR-COL                           
006070         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
006080     IF S = 1 GO TO EDIT-RTN-25-X.                                        
006090 RTN-25-A.                                                                
006100     MOVE 25-LOC TO LOC-WORK. PERFORM LOC-CK THRU LOC-CK-X.               
006110     IF ER-S = 1 MOVE ZERO TO ER-S   MOVE 03 TO ERR-CODE                  
006120         MOVE 18 TO ERR-COL  PERFORM OUTPUT-ERROR-ROUTINE                 
006130         THRU OP-EXIT.                                                    
006140     IF S = 1 GO TO EDIT-RTN-25-X.                                        
006150     IF 25-DIST IS ALPHABETIC GO TO RTN-25-B.                             
006160         MOVE 14 TO ERR-CODE MOVE 21 TO ERR-COL                           
006170         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
006180     IF S = 1 GO TO EDIT-RTN-25-X.                                        
006190 RTN-25-B.                                                                
006200     MOVE 25-AM-PEAK TO PEAK-WORK                                         
006210     PERFORM  PEAK-CK THRU PEAK-CK-X.                                     
006220     IF NE-SW = 1 MOVE ZERO TO NE-SW MOVE 23 TO ERR-COL                   
006230         MOVE 23 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
006240         THRU OP-EXIT.                                                    
006250     IF S = 1  MOVE ZERO TO SW-SW GO TO EDIT-RTN-25-X.                    
006260     IF SW-SW = 1 MOVE ZERO TO SW-SW MOVE 30 TO ERR-COL                   
006270     PERFORM V1 THRU V1-X.                                                
006280     IF S = 1 GO TO EDIT-RTN-25-X.                                        
006290     MOVE 25-PM-PEAK TO PEAK-WORK.                                        
006300     PERFORM PEAK-CK THRU PEAK-CK-X.                                      
006310     IF NE-SW = 1 MOVE ZERO TO NE-SW MOVE 37 TO ERR-COL                   
006320         PERFORM V1 THRU V1-X.                                            
006330     IF S = 1  MOVE ZERO TO SW-SW GO TO EDIT-RTN-25-X.                    
006340     IF SW-SW = 1 MOVE ZERO TO SW-SW MOVE 44 TO ERR-COL                   
006350         PERFORM V1 THRU V1-X.                                            
006360     IF S = 1  GO TO EDIT-RTN-25-X.                                       
006370     MOVE 25-T-6-HR TO 6-HR-TOT.                                          
006380     IF 6-HR-NE NOT NUMERIC MOVE 51 TO ERR-COL GO TO RTN-25-C.            
006390     IF 6-HR-NE-1 < ZERO    MOVE 51 TO ERR-COL GO TO RTN-25-C.            
006400     IF 6-HR-NE > '15000' MOVE 51 TO ERR-COL GO TO RTN-25-C.              
006410     GO TO RTN-25-D.                                                      
006420 RTN-25-C.                                                                
006430     MOVE 17 TO ERR-CODE                                                  
006440     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
006450 RTN-25-D.                                                                
006460     IF S = 1 GO TO EDIT-RTN-25-X.                                        
006470     MOVE 6-HR-SW TO 6-HR-NE.                                             
006480     IF 6-HR-NE NOT NUMERIC MOVE 56 TO ERR-COL GO TO RTN-25-E.            
006490     IF 6-HR-NE-1 < ZERO  MOVE 56 TO ERR-COL GO TO RTN-25-E.              
006500     IF 6-HR-NE > '15000' MOVE 56 TO ERR-COL GO TO RTN-25-C.              
006510     GO TO RTN-25-F.                                                      
006520 RTN-25-E.                                                                
006530     PERFORM RTN-25-C.                                                    
006540 RTN-25-F.                                                                
006550     IF S = 1 GO TO EDIT-RTN-25-X.                                        
006560     MOVE 25-ADJ-FACTOR TO REQ-TEST.                                      
006570     IF REQ-TEST NOT NUMERIC GO TO RTN-25-G.                              
006580     IF REQ-TEST-1 < ZERO GO TO RTN-25-G.                                 
006590     GO TO RTN-25-H.                                                      
006600 RTN-25-G.                                                                
006610     MOVE '235' TO 25-ADJ-FACTOR.                                         
006620 RTN-25-H.                                                                
006630     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
006640         GO TO EDIT-RTN-25-X.                                             
006650     IF S = 1  GO TO EDIT-RTN-25-X.                                       
006660     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
006670 EDIT-RTN-25-X. EXIT.                                                     
006680 PEAK-CK.                                                                 
006690     IF NE-HR-1 < ZERO OR NE-HR-1 > '24' MOVE 1 TO NE-SW                  
006700         GO TO PEAK-CK-A.                                                 
006710     IF NE-HR-2 NOT = ZERO AND NE-HR-2 NOT = '1' AND NE-HR-2              
006720         NOT = '3' AND NE-HR-2 NOT = '4' MOVE 1 TO NE-SW                  
006730         GO TO PEAK-CK-A.                                                 
006740     IF NE-VOL NOT NUMERIC MOVE 1 TO NE-SW GO TO PEAK-CK-A.               
006750     IF NE-VOL-1 < ZERO MOVE 1 TO NE-SW GO TO PEAK-CK-A.                  
006760     IF NE-VOL > '5000' MOVE 1 TO NE-SW.                                  
006770 PEAK-CK-A.                                                               
006780     IF SW-HR-1 < ZERO OR SW-HR-1 > '24' MOVE 1 TO SW-SW                  
006790         GO TO PEAK-CK-X.                                                 
006800     IF SW-HR-2 NOT = ZERO AND  SW-HR-2 NOT = '1' AND SW-HR-2             
006810         NOT = '3' AND SW-HR-2 NOT = '4' MOVE 1 TO SW-SW                  
006820         GO TO PEAK-CK-X.                                                 
006830     IF SW-VOL NOT NUMERIC MOVE 1 TO SW-SW GO TO PEAK-CK-X.               
006840     IF SW-VOL-1 < ZERO MOVE 1 TO SW-SW GO TO PEAK-CK-X.                  
006850     IF SW-VOL > '5000' MOVE 1  TO SW-SW.                                 
006860 PEAK-CK-X. EXIT.                                                         
006870 V1. MOVE 23 TO ERR-CODE                                                  
006880     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
006890 V1-X. EXIT.                                                              
006900 EDIT-RTN-27.                                                             
006910     MOVE 27-IS-CODE TO IS-FILE-KEY.                                      
006920     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
006930     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 01 TO ERR-COL                     
006940         MOVE 02 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
006950         THRU OP-EXIT.                                                    
006960     MOVE 27-DATE TO DATE-WORK.                                           
006970     PERFORM COMMON-DATE-EDIT.                                            
006980     IF DATE-ERR-SW = 1 MOVE ZERO TO DATE-ERR-SW                          
006990     MOVE 04 TO ERR-CODE MOVE 11 TO ERR-COL                               
007000     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
007010 R7. MOVE 27-LOC TO  LOC-WORK.                                            
007020     PERFORM LOC-CK THRU LOC-CK-X.                                        
007030     IF ER-S = 1, MOVE ZERO TO ER-S, MOVE 03 TO ERR-CODE                  
007040         MOVE 17 TO ERR-COL PERFORM OUTPUT-ERROR-ROUTINE                  
007050         THRU OP-EXIT.                                                    
007060     IF S = 1 GO TO EDIT-RTN-27-X.                                        
007070     IF 27-TYPE NOT = '1' AND 27-TYPE NOT = '2'                           
007080         MOVE 16 TO ERR-CODE, MOVE 20 TO ERR-COL                          
007090         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
007100     IF S = 1 GO TO EDIT-RTN-27-X.                                        
007110     IF 27-ACTION NOT = '1' AND 27-ACTION NOT = '2'                       
007120         MOVE 18 TO ERR-CODE, MOVE 21 TO ERR-COL                          
007130         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
007140     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
007150         GO TO EDIT-RTN-27-X.                                             
007160     IF S = 1 GO TO EDIT-RTN-27-X.                                        
007170     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
007180 EDIT-RTN-27-X. EXIT.                                                     
007190 EDIT-RTN-30.                                                             
007200     MOVE 30-34-REQ TO REQ-TEST.                                          
007210                                                                  KM120370
007220     GO TO RTN-30-B.                                              KM120370
007230 RTN-30-A.                                                                
007240     MOVE 01 TO ERR-CODE, MOVE 01 TO ERR-COL                              
007250     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
007260 RTN-30-B.                                                                
007270     MOVE 30-34-IS-CODE TO IS-FILE-KEY.                                   
007280     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
007290     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
007300         MOVE 04 TO ERR-COL  PERFORM OUTPUT-ERROR-ROUTINE                 
007310         THRU OP-EXIT.                                                    
007320     IF 30-34-REQUESTER = SPACES, MOVE 14 TO ERR-COL                      
007330         MOVE 19 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
007340         THRU OP-EXIT.                                                    
007350     IF S = 1 GO TO EDIT-RTN-30-X.                                        
007360     IF 30-34-DATE = SPACES MOVE ZEROES TO 30-34-DATE                     
007370         GO TO RTN-30-D.                                                  
007380     MOVE 30-34-DATE TO WORK-DAT.                                         
007390     MOVE FROM-DATE TO DATE-WORK.                                         
007400     PERFORM COMMON-DATE-EDIT.                                            
007410     IF DATE-ERR-SW = 1  MOVE ZERO TO DATE-ERR-SW                         
007420         MOVE 29 TO ERR-COL PERFORM RTN-30-C.                             
007430     IF S = 1 GO TO EDIT-RTN-30-X.                                        
007440     MOVE TO-DATE TO DATE-WORK.                                           
007450     PERFORM COMMON-DATE-EDIT.                                            
007460     IF DATE-ERR-SW NOT =    1 GO TO RTN-30-D.                            
007470         MOVE ZERO TO DATE-ERR-SW.                                        
007480         MOVE 35 TO ERR-COL.                                              
007490 RTN-30-C.                                                                
007500     MOVE 04 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                     
007510         THRU OP-EXIT.                                                    
007520 RTN-30-D.                                                                
007530     IF E = 1 AND S = ZERO, PERFORM I-C-2 THRU OP-EXIT                    
007540         GO TO EDIT-RTN-30-X.                                             
007550     IF S = 1 GO TO EDIT-RTN-30-X.                                        
007560     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
007570 EDIT-RTN-30-X. EXIT.                                                     
007580 EDIT-RTN-31.                                                             
007590     MOVE 31-REQ TO REQ-TEST.                                             
007600                                                                  KM042071
007610                                                                  KM042071
007620 RTN-31-A.                                                                
007630     MOVE 31-IS-CODE TO IS-FILE-KEY.                                      
007640     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
007650     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
007660     MOVE 04 TO ERR-COL PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.        
007670     GO TO RTN-31-B.                                                      
007680     MOVE 31-IS-CODE TO IS-FILE-KEY.                                      
007690     MOVE 31-LAST-IS-CODE TO IS-ST-2.                                     
007700     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
007710     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
007720     MOVE 04 TO ERR-COL PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.        
007730 RTN-31-B.                                                                
007740     IF 31-CODE NOT = SPACE AND 31-CODE NOT = '1' AND 31-CODE             
007750         NOT = '2' MOVE 19 TO ERR-COL MOVE 12 TO ERR-CODE                 
007760         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
007770     IF S = 1 GO TO EDIT-RTN-31-X.                                        
007780     IF 31-REQUESTER = SPACES MOVE 20 TO ERR-COL                          
007790         MOVE 19 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
007800         THRU OP-EXIT.                                                    
007810     IF S = 1 GO TO EDIT-RTN-31-X.                                        
007820     MOVE 31-DATE TO WORK-DAT.                                            
007830     MOVE FROM-DATE TO DATE-WORK.                                         
007840     PERFORM COMMON-DATE-EDIT.                                            
007850     IF DATE-ERR-SW = 1 MOVE ZERO TO DATE-ERR-SW                          
007860         MOVE 35 TO ERR-COL PERFORM RTN-30-C.                             
007870     IF S = 1 GO TO EDIT-RTN-31-X.                                        
007880     MOVE TO-DATE TO DATE-WORK.                                           
007890     PERFORM COMMON-DATE-EDIT.                                            
007900     IF DATE-ERR-SW = 1  MOVE ZERO TO DATE-ERR-SW                         
007910         MOVE 41 TO ERR-COL  PERFORM RTN-30-C.                            
007920     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
007930         GO TO EDIT-RTN-31-X.                                             
007940     IF S = 1 GO TO EDIT-RTN-31-X.                                        
007950     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
007960 EDIT-RTN-31-X. EXIT.                                                     
007970 EDIT-RTN-34.                                                             
007980     PERFORM EDIT-RTN-30 THRU EDIT-RTN-30-X.                              
007990 EDIT-RTN-34-X. EXIT.                                                     
008000 EDIT-RTN-36.                                                             
008010     PERFORM EDIT-RTN-35 THRU EDIT-RTN-35-X.                              
008020 EDIT-RTN-36-X. EXIT.                                                     
008030 EDIT-RTN-32.                                                             
008040     PERFORM EDIT-RTN-31 THRU EDIT-RTN-31-X.                              
008050 EDIT-RTN-32-X. EXIT.                                                     
008060 EDIT-RTN-33.                                                             
008070     PERFORM EDIT-RTN-31 THRU EDIT-RTN-31-X.                              
008080 EDIT-RTN-33-X. EXIT.                                                     
008090 EDIT-RTN-35.                                                             
008100     EXAMINE CAT-PARA REPLACING ALL SPACES BY ZEROES.                     
008110     MOVE 35-36-REQ TO REQ-TEST.                                          
008120                                                                  KM120370
008130     GO TO RTN-35-B.                                              KM120370
008140 RTN-35-A.                                                                
008150     PERFORM RTN-30-A.                                                    
008160 RTN-35-B.                                                                
008170     MOVE 1 TO C.                                                         
008180 RTN-35-C.                                                                
008190     MOVE CATEGORY (C) TO CAT-HOLD.                                       
008200     PERFORM CAT-CK THRU CAT-CK-X.                                        
008210     IF ER-S = 1 MOVE ZERO TO ER-S                                        
008220         COMPUTE ERR-COL = ((C * 3) + 1)                                  
008230         MOVE 20 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
008240         THRU OP-EXIT.                                                    
008250     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008260     IF C = 14 OR C > 14 NEXT SENTENCE ELSE ADD 1 TO C                    
008270         GO TO RTN-35-C.                                                  
           MOVE CAT-LIMIT TO CH-2.                                              
           IF CH-2 NOT NUMERIC GO TO RT-35-A.                                   
           IF CH-2A < ZERO NEXT SENTENCE ELSE GO TO RT-35-B.                    
       RT-35-A.                                                                 
           MOVE 21 TO ERR-CODE.                                                 
008290         MOVE 46 TO ERR-COL  PERFORM OUTPUT-ERROR-ROUTINE                 
008300         THRU OP-EXIT.                                                    
       RT-35-B.                                                                 
008310     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008320     IF 35-36-REQUESTER = SPACES  MOVE 19 TO ERR-CODE                     
008330         MOVE 48 TO ERR-COL PERFORM OUTPUT-ERROR-ROUTINE                  
008340         THRU OP-EXIT.                                                    
008350     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008360     MOVE 35-36-DATE TO WORK-DAT.                                         
008370     MOVE FROM-DATE TO DATE-WORK.                                         
008380     PERFORM COMMON-DATE-EDIT.                                            
008390     IF DATE-ERR-SW = 1 MOVE ZERO TO DATE-ERR-SW                          
008400         MOVE 59 TO ERR-COL  PERFORM RTN-30-C.                            
008410     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008420     MOVE TO-DATE TO DATE-WORK.                                           
008430     PERFORM COMMON-DATE-EDIT.                                            
008440     IF DATE-ERR-SW = 1 MOVE ZERO TO DATE-ERR-SW                          
008450         MOVE 65 TO ERR-COL  PERFORM RTN-30-C.                            
008460     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008470     IF 35-36-TIME = SPACES MOVE ZEROES TO 35-36-TIME                     
008480         GO TO RTN-35-D.                                                  
008490     MOVE 35-36-TIME TO WORK-TIME. MOVE FROM-TIME TO T-WORK.              
008500     PERFORM COMMON-TIME-EDIT THRU COMMON-TIME-EDIT-X.                    
008510     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 71 TO ERR-COL                     
008520         MOVE 06 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
008530         THRU OP-EXIT.                                                    
008540     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008550     MOVE TO-TIME TO T-WORK.                                              
008560     PERFORM COMMON-TIME-EDIT THRU COMMON-TIME-EDIT-X.                    
008570     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 75 TO ERR-COL                     
008580         MOVE 06 TO ERR-CODE PERFORM OUTPUT-ERROR-ROUTINE                 
008590         THRU OP-EXIT.                                                    
008600 RTN-35-D.                                                                
008610     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
008620         GO TO EDIT-RTN-35-X.                                             
008630     IF S = 1 GO TO EDIT-RTN-35-X.                                        
008640     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
008650 EDIT-RTN-35-X. EXIT.                                                     
008660 CAT-CK.                                                                  
008670     IF C NOT = 1, GO TO CAT-CK-A.                                        
008680     IF CAT-HOLD NOT NUMERIC GO TO CAT-A.                                 
008690     IF CAT-1 < ZERO NEXT SENTENCE ELSE GO TO CAT-CK-X.                   
008700 CAT-A.                                                                   
008710     MOVE 20 TO ERR-CODE  MOVE 04 TO ERR-COL                              
008720         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
008730         GO TO CAT-CK-X.                                                  
008740 CAT-CK-A.                                                                
008750     IF CAT-HOLD = ZERO MOVE 14 TO C                                      
008760     GO TO CAT-CK-X.                                                      
008770     IF CAT-HOLD NOT NUMERIC MOVE 1 TO ER-S GO TO CAT-CK-X.               
008780     IF CAT-1 < ZERO MOVE 1 TO ER-S.                                      
008790 CAT-CK-X. EXIT.                                                          
008800 CARD-CODE-ERROR.                                                         
008810     MOVE '99' TO ERROR-CODE.                                             
008820     MOVE '79' TO ER-COL1.                                                
008830     MOVE '30' TO ER-CODE1.                                               
008840     MOVE CARD-REC TO ER-1-80.                                            
008850     WRITE ERROR-REC.                                                     
008860 CARD-CODE-ERROR-X. EXIT.                                                 
008870 COMMON-TIME-EDIT.                                                        
008880     IF TIME-H < ZERO OR TIME-H > '24' MOVE 1 TO ER-S.                    
008890     IF TIME-M < ZERO OR TIME-M > '59' MOVE 1 TO ER-S.                    
008900 COMMON-TIME-EDIT-X. EXIT.                                                
008910 IS-CODE-EDIT.                                                            
008920     IF IS-ST-1 > IS-ST-2 MOVE IS-ST-1 TO IS-H                            
008930         MOVE IS-ST-2 TO IS-ST-1, MOVE IS-H TO IS-ST-2.                   
008940     READ ISFILE INVALID KEY MOVE 1 TO ER-S.                              
008950 IS-CODE-EDIT-X.                                                          
008960                                                                          
008970 LOC-CK.                                                                  
008980     IF LOC-W1 < '1' OR  LOC-W1 > '6' GO TO LOC-CK-A.                     
008990                                                                          
009000     IF LOC-W2 NOT = '2' AND LOC-W2 NOT = '8' AND LOC-W2 NOT = '0'        
009010         GO TO LOC-CK-A.                                                  
009020     IF LOC-W3 < ZERO OR LOC-W3 > '5' GO TO LOC-CK-A.                     
009030     GO TO LOC-CK-X.                                                      
009040 LOC-CK-A.                                                                
009050     MOVE 1 TO ER-S.                                                      
009060 LOC-CK-X. EXIT.                                                          
009070 WRITE-DISK.                                                              
009080     MOVE CARD-REC TO DISK-1-80.                                          
009090     MOVE ZEROES TO DISK-81-100.                                          
009100     MOVE CARD-CODE-HOLD TO 21-KEY.                                       
009110     WRITE DISK-REC.                                                      
009120 WRITE-DISK-X. EXIT.                                                      
009130 EDIT-RTN-28.                                                             
009140     IF 28-29-NO NOT NUMERIC OR                                   KM021271
009145        28-29-NO-5 < ZERO                                         KM021271
009150        GO TO RTN-28-A.                                           KM021271
009160     GO TO RTN-28-B.                                              KM021271
009170 RTN-28-A.                                                                
009180     MOVE 25 TO ERR-CODE MOVE 01 TO ERR-COL                               
009190     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT                            
009200         GO TO RTN-28-K.                                                  
009210 RTN-28-B.                                                                
009220     IF 28-AST NOT = 'R' AND 28-AST NOT = 'D'                             
009230         MOVE 18 TO ERR-CODE MOVE 06 TO ERR-COL                           
009240         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT                        
009250         GO TO RTN-28-K.                                                  
009260     IF 28-C = SPACE GO TO RTN-28-C.                                      
009270     IF 28-C < '1'  OR 28-C > '3' MOVE 07 TO ERR-COL                      
009280         PERFORM RTN-28-Y.                                                
009290 RTN-28-C.                                                                
009300     IF 28-A = SPACE GO TO RTN-28-D.                                      
009310     IF 28-A < ZERO OR 28-A > '5' MOVE 08 TO ERR-COL                      
009320         PERFORM RTN-28-Y.                                                
009330 RTN-28-D.                                                                
009340     IF 28-J = SPACE GO TO RTN-28-E.                                      
009350     IF 28-J < ZERO OR 28-J > '7' MOVE 09 TO ERR-COL                      
009360         PERFORM RTN-28-Y.                                                
009370 RTN-28-E.                                                                
009380     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009390     IF 28-K = SPACE GO TO RTN-28-F.                                      
009400     IF 28-K < ZERO OR 28-K > '5' MOVE 10 TO ERR-COL                      
009410         PERFORM RTN-28-Y.                                                
009420 RTN-28-F.                                                                
009430     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009440     IF 28-L1 = SPACE GO TO RTN-28-G.                                     
009450     IF 28-L1 < ZERO OR 28-L1 > '5' MOVE 11 TO ERR-COL                    
009460         PERFORM RTN-28-Y.                                                
009470 RTN-28-G.                                                                
009480     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009490     IF 28-L2 = SPACE GO TO RTN-28-H.                                     
009500     IF 28-L2 < ZERO OR 28-L2 > '5' MOVE 12 TO ERR-COL                    
009510         PERFORM RTN-28-Y.                                                
009520 RTN-28-H.                                                                
009530     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009540     IF 28-M = SPACE GO TO RTN-28-I.                                      
009550     IF 28-M < '1' OR 28-M > '2' MOVE 13 TO ERR-COL                       
009560         PERFORM RTN-28-Y.                                                
009570 RTN-28-I.                                                                
009580     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009590     IF 28-DATE = SPACE GO TO RTN-28-I1.                                  
009600     MOVE 28-DATE TO DATE-WORK.                                           
009610     PERFORM COMMON-DATE-EDIT.                                            
009620     IF DATE-ERR-SW = 1 MOVE ZERO TO DATE-ERR-SW                          
009630         MOVE 04 TO ERR-CODE MOVE 17 TO ERR-COL                           
009640         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
009650 RTN-28-I1.                                                               
009660     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009670     IF 28-S-DIST = SPACE GO TO RTN-28-J.                                 
009680     MOVE 28-S-DIST TO SE-TEST.                                           
009690     IF SE-TEST NOT NUMERIC GO TO RTN-28-I2.                              
009700     IF SE-TEST-1 < ZERO NEXT SENTENCE ELSE GO TO RTN-28-J.               
009710 RTN-28-I2.                                                               
009720     MOVE 26 TO ERR-CODE MOVE 23 TO ERR-COL                               
009730     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
009740 RTN-28-J.                                                                
009750     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009760     IF 28-SEG-VOL = SPACE GO TO RTN-28-K.                                
009770     MOVE 28-SEG-VOL TO SE-T.                                             
009780     IF SE-T NOT NUMERIC GO TO RTN-28-J2.                                 
009790     IF SE-T-1 < ZERO NEXT SENTENCE ELSE GO TO RTN-28-K.                  
009800 RTN-28-J2.                                                               
009810     MOVE 17 TO ERR-CODE MOVE 28 TO ERR-COL                               
009820     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
009830 RTN-28-K.                                                                
009840     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
009850         GO TO EDIT-RTN-28-X.                                             
009860     IF S = 1 GO TO EDIT-RTN-28-X.                                        
009870     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
009880 EDIT-RTN-28-X. EXIT.                                                     
009890 RTN-28-Y.                                                                
009900     MOVE 10 TO ERR-CODE                                                  
009910     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
009920 EDIT-RTN-29.                                                             
009930     EXAMINE 29-SEG-CLASS REPLACING ALL SPACES BY ZEROES.                 
009940     EXAMINE 29-S-DIST    REPLACING ALL SPACES BY ZEROES.                 
009950     EXAMINE 29-S-VOL     REPLACING ALL SPACES BY ZEROES.                 
009960     EXAMINE 29-STRAF     REPLACING ALL SPACES BY ZEROES.                 
009970     IF 28-29-NO NOT NUMERIC OR                                   KM021271
009975        28-29-NO-5 < ZERO                                         KM021271
009980        GO TO RTN-29-A.                                           KM021271
009990     GO TO RTN-29-B.                                              KM021271
010000 RTN-29-A.                                                                
010010     MOVE 25 TO ERR-CODE MOVE 01 TO ERR-COL                               
010020     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT                            
010030     GO TO RTN-29-K.                                                      
010040 RTN-29-B.                                                                
010050     IF 29-S-DIST = ZERO GO TO RTN-29-C.                                  
010060     MOVE 29-S-DIST TO SE-TEST.                                           
010070     IF SE-TEST NOT NUMERIC GO TO RTN-29-B1.                              
010080     IF SE-TEST-1 < ZERO NEXT SENTENCE ELSE GO TO RTN-29-C.               
010090 RTN-29-B1.                                                               
010100     MOVE 26 TO ERR-CODE  MOVE 06 TO ERR-COL                              
010110     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
010120 RTN-29-C.                                                                
010130     IF 29-S-VOL = ZERO GO TO RTN-29-D.                                   
010140     MOVE 29-S-VOL TO SE-T.                                               
010150     IF SE-T NOT NUMERIC  GO TO RTN-29-C1.                                
010160     IF SE-T-1 < ZERO NEXT SENTENCE ELSE GO TO RTN-29-D.                  
010170 RTN-29-C1.                                                               
010180     MOVE 17 TO ERR-CODE  MOVE 11 TO ERR-COL                              
010190     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
010200 RTN-29-D.                                                                
010210     IF 29-C < ZERO OR 29-C > '3' MOVE 17 TO ERR-COL                      
010220         PERFORM RTN-28-Y.                                                
010230     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010240     IF 29-A < ZERO OR 29-A > '5' MOVE 18 TO ERR-COL                      
010250         PERFORM RTN-28-Y.                                                
010260     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010270     IF 29-J < ZERO OR 29-J > '7' MOVE 19 TO ERR-COL                      
010280         PERFORM RTN-28-Y.                                                
010290     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010300     IF 29-K < ZERO OR 29-K > '5' MOVE 20 TO ERR-COL                      
010310         PERFORM RTN-28-Y.                                                
010320     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010330     IF 29-L1 < ZERO OR 29-L1 > '5' MOVE 21 TO ERR-COL                    
010340         PERFORM RTN-28-Y.                                                
010350     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010360     IF 29-L2 < ZERO OR 29-L2 > '5' MOVE 22 TO ERR-COL                    
010370         PERFORM RTN-28-Y.                                                
010380     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010390     IF 29-M  < ZERO OR 29-M >  '2' MOVE 23 TO ERR-COL                    
010400         PERFORM RTN-28-Y.                                                
010410     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010420     MOVE 29-DATE TO DATE-WORK.                                           
010430     PERFORM COMMON-DATE-EDIT.                                            
010440     IF DATE-ERR-SW = 1 MOVE ZERO TO DATE-ERR-SW                          
010450         MOVE 04 TO ERR-CODE MOVE 27 TO ERR-COL                           
010460         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                       
010470     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010480     MOVE 29-ROUTE TO IS-ST-1 MOVE 29-X-1 TO IS-ST-2.                     
010490     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
010500     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
010510         MOVE 33 TO ERR-COL                                               
010520         PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT GO TO RTN-29-E.        
010530     MOVE 29-ROUTE TO IS-ST-1 MOVE 29-X-2 TO IS-ST-2.                     
010540     PERFORM IS-CODE-EDIT THRU IS-CODE-EDIT-X.                            
010550     IF ER-S = 1 MOVE ZERO TO ER-S MOVE 02 TO ERR-CODE                    
010560     MOVE 43 TO ERR-COL PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.        
010570 RTN-29-E.                                                                
010580     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010590     IF 29-STRAF = ZERO GO TO RTN-29-K.                                   
010600     MOVE 29-STRAF TO REQ-TEST.                                           
010610     IF REQ-TEST NOT NUMERIC GO TO RTN-29-F.                              
010620     IF REQ-TEST-1 < ZERO NEXT SENTENCE ELSE GO TO RTN-29-K.              
010630 RTN-29-F.                                                                
010640     MOVE 29 TO ERR-CODE MOVE 48 TO ERR-COL                               
010650     PERFORM OUTPUT-ERROR-ROUTINE THRU OP-EXIT.                           
010660 RTN-29-K.                                                                
010670     IF E = 1 AND S = ZERO PERFORM I-C-2 THRU OP-EXIT                     
010680         GO TO EDIT-RTN-29-X.                                             
010690     IF S = 1 GO TO EDIT-RTN-29-X.                                        
010700     PERFORM WRITE-DISK THRU WRITE-DISK-X.                                
010710 EDIT-RTN-29-X. EXIT.                                                     
/*                                                                              
// LBLTYP NSD(05)                                                               
// EXEC LNKEDT                                                                  
/*                                                                              
/&                                                                              
   xF \