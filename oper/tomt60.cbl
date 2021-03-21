 IDENTIFICATION DIVISION.                                       
 PROGRAM-ID. S0CMR060.                                          
 AUTHOR. S GOGELA.                                              
 ENVIRONMENT DIVISION.                                          
 CONFIGURATION SECTION.                                         
 INPUT-OUTPUT SECTION.                                          
 FILE-CONTROL.                                                  
     SELECT AMFTG ASSIGN TO DSK RECORDING MODE IS ASCII . 
     SELECT MODTG ASSIGN TO DSK RECORDING MODE IS ASCII . 
     SELECT FIL60 ASSIGN TO DSK RECORDING MODE IS ASCII . 
     SELECT FIL65 ASSIGN TO DSK RECORDING MODE IS ASCII . 
 DATA DIVISION.                                                 
 FILE SECTION.                                                  
 FD  AMFTG                                                    
     VALUE OF IDENTIFICATION IS 'AMFTG DAT'. 
 01  REQ-REC . 
     02 R-NO     PICTURE 9(2).                                  
     02 INFO     PICTURE 9(3).                                  
     02 REQPR OCCURS 5 TIMES.                                   
       04 PR-NO  PICTURE 9(2).                                  
       04 REC-MOV.                                              
         06 VAL-R    PICTURE 9(6)V9(4).                         
         06 DCD      PICTURE 9(2).                              
         06 RCD      PICTURE X.                                 
 FD  MODTG                                                    
     VALUE OF IDENTIFICATION IS 'MODTG DAT'. 
 01  MF-REC . 
     02 F-R.                                                    
       04 H-C    PICTURE X.                                     
       04 UDC    PICTURE X.                                     
       04 MAT-NO PICTURE X(6).                                  
       04 FILLER PICTURE X(72).                                 
     02 S-R REDEFINES F-R.                                      
       04 FILLER PICTURE X(8).                                  
       04 PRPR OCCURS 3 TIMES.                                  
         06 PNOR     PICTURE 9(2).                              
         06 MF-MOV.                                             
           08 VLF    PICTURE 9(6)V9(4).                         
           08 VLT    PICTURE 9(6)V9(4).                         
           08 DCDE   PICTURE 9(2).                              
 FD  FIL60                                                    
     VALUE OF IDENTIFICATION IS 'FIL60 DAT'. 
 01  OUT-REC . 
     02 RQNO     PICTURE 9(2).                                  
     02 HDR-REC  PICTURE X(80).                                 
     02 INFO-OUT PICTURE 9(3).                                  
     02 ZP-INF     PICTURE 9(2).                                
     02 LPXZ.                                                   
      03 LPX1 OCCURS 2 TIMES PICTURE 9(2).                      
     02 ZERO-PROPERTIES.                                        
       03 ZO-PS  OCCURS 6 TIMES  PIC 9(2).                      
 FD  FIL65                                                    
     VALUE OF IDENTIFICATION IS 'FIL65 DAT'. 
 01  PRINT . 
     02 CC       PICTURE X.                                     
     02 FILLER   PICTURE X(132).                                
 WORKING-STORAGE SECTION.                                       
 77  SUBZP   COMPUTATIONAL  PIC S9(6) VALUE 0 SYNC.             
 77  Z-SW   PIC 9 VALUE 0.                                      
 77  TS-ZPMF    PICTURE 99 VALUE 0.                             
 77  COUNT-RQ   PICTURE 99 VALUE 0.                             
 77  R-CH      PICTURE X.                                       
 77  DCH1         PICTURE 9(2).                                 
 77  DCH2         PICTURE 9(2).                                 
 77  DCH3         PICTURE 9(2).                                 
 77  D-CH1        PICTURE 9(2).                                 
 77  D-CH2        PICTURE 9(2).                                 
 77  D-CH3        PICTURE 9(2).                                 
 77  VALH   COMPUTATIONAL   PICTURE S9(6)V9(4) SYNC.            
 77  VALFH   COMPUTATIONAL   PICTURE S9(6)V9(4) SYNC.           
 77  VALTH   COMPUTATIONAL   PICTURE S9(6)V9(4) SYNC.           
 77  I   COMPUTATIONAL   PICTURE S9 VALUE 1 SYNC.               
 77  J   COMPUTATIONAL   PICTURE S99 VALUE 1 SYNC.              
 77  K   COMPUTATIONAL   PICTURE S9 VALUE 1 SYNC.               
 77  L   COMPUTATIONAL   PICTURE S9 VALUE 1 SYNC.               
 77  M   COMPUTATIONAL   PICTURE S99 VALUE 1 SYNC.              
 77  N   COMPUTATIONAL   PICTURE S99 VALUE 1 SYNC.              
 77  X           PICTURE X VALUE ZERO.                          
 77  OPT         PICTURE 9(3) VALUE ZERO.                       
 77  Y           PICTURE X VALUE ZERO.                          
 77  Z           PICTURE X VALUE ZERO.                          
 77  S           PICTURE X VALUE ZERO.                          
 77  R-OLD       PICTURE 9(2).                                  
 77  COUNT   COMPUTATIONAL  PICTURE S9 VALUE ZERO SYNC.         
 77  ARGO PICTURE IS 9(12)V9(4) COMPUTATIONAL. 
 77  HP1     PICTURE IS 9(12)V9(4) USAGE IS COMPUTATIONAL. 
 77  HP2     PICTURE IS 9(12)V9(4) USAGE IS COMPUTATIONAL. 
 77  RAN1    PICTURE IS 9(12)V9(4) USAGE IS COMPUTATIONAL. 
 77  RAN2    PICTURE IS 9(12)V9(4) USAGE IS COMPUTATIONAL. 
 77  SWX1      PICTURE 99 VALUE 0.                              
 01  TA1 . 
     02 TB1.                                                    
     03 TT1 PICTURE X(13) VALUE '1144447888474'.                
     02 TC1 REDEFINES TB1.                                      
     03 T1 OCCURS 13 TIMES PICTURE 9.                           
 01  TA36 . 
     02 TB36.                                                   
     03 TT36 PICTURE X(24) VALUE '112244145663333333131445'.    
     02 TC36 REDEFINES TB36.                                    
     03 T36 OCCURS 24 TIMES PICTURE 9.                          
 01  TA37 . 
     02 TB37.                                                   
     03 TT37 PICTURE X(39) VALUE '112224232221622422255552225526
-    '3461232'.                                                 
     02 TC37 REDEFINES TB37.                                    
     03 T37 OCCURS 39 TIMES PICTURE 9.                          
 01  TA38 . 
     02 TB38.                                                   
     03 TT38 PICTURE X(17) VALUE '11585558911185195'.           
     02 TC38 REDEFINES TB38.                                    
     03 T38 OCCURS 17 TIMES PICTURE 9.                          
 01  TA39 . 
     02 TB39.                                                   
     03 TT39 PICTURE X(27) VALUE '123333233333344243214431223'. 
     02 TC39 REDEFINES TB39.                                    
     03 T39 OCCURS 27 TIMES PICTURE 9.                          
 01  TA40 . 
     02 TB40.                                                   
     03 TT40 PICTURE X(32) VALUE '113313334334511151433444413455
-    ''.                                                        
     02 TC40 REDEFINES TB40.                                    
     03 T40 OCCURS 32 TIMES PICTURE 9.                          
 01  TA42 . 
     02 TB42.                                                   
     03 TT42 PICTURE X(80) VALUE '010103040506070809010112131415
-    '171819202122233025260128013001323334353637013940'.        
     03 TTC2 PICTURE X(44) VALUE '414243444546474849505152015455
-    '575859606162'.                                            
     02 TC42 REDEFINES TB42.                                    
     03 T42 OCCURS 62 TIMES PICTURE 99.                         
 01  TA44 . 
     02 TB44.                                                   
     03 TT44 PICTURE X(30) VALUE '524454555555555514155142511511'. 
     02 TC44 REDEFINES TB44.                                    
     03 T44 OCCURS 30 TIMES PICTURE 9.                          
 01  TAB-NOR . 
     02 RCHX OCCURS 5 TIMES PICTURE 9(2).                       
 01  TABLE-REQ . 
     02 REQ OCCURS 5 TIMES.                                     
       04 PRR OCCURS 44 TIMES.                                  
         06 PROPR OCCURS 3 TIMES.                               
           08 VAL   COMPUTATIONAL PICTURE S9(6)V9(4).           
           08 D-C    PICTURE 9(2).                              
           08 R-C    PICTURE X.                                 
 01  TABLE-MF . 
     02 HEADER   PICTURE X(80).                                 
     02 PR OCCURS 44 TIMES.                                     
         06 PROP OCCURS 3 TIMES.                                
           08 VALF  COMPUTATIONAL PICTURE S9(6)V9(4).           
           08 VALT  COMPUTATIONAL PICTURE S9(6)V9(4).           
           08 DC      PICTURE 9(2).                             
 01  H1 . 
     02 C1       PICTURE X VALUE '1'.                           
     02 REC-PRINT  PICTURE X(80).                               
     02 FILLER PICTURE X(6) VALUE SPACES.                       
     02 DESC-PRINT PICTURE X(40).                               
     02 FILLER     PICTURE X(6)  VALUE SPACES.                  
 PROCEDURE DIVISION.                                            
 ST-PR.                                                         
     OPEN  INPUT  AMFTG                                       
     OPEN  INPUT  MODTG                                       
     OPEN  OUTPUT FIL60                                       
     OPEN  OUTPUT FIL65.                                      
     MOVE ZEROS TO HEADER.                                      
 LOOPZM.                                                        
     MOVE 0 TO VALF (J, K).                                     
     MOVE 0 TO VALT (J, K).                                     
     MOVE 0 TO DC (J, K).                                       
     ADD 1 TO K.                                                
     IF K NOT > 3 GO TO LOOPZM.                                 
     MOVE 1 TO K.                                               
     ADD 1 TO J.                                                
     IF J NOT > 44 GO TO LOOPZM.                                
     MOVE 1 TO J.                                               
 ZE-RCHX.                                                       
     MOVE 0 TO RCHX (I).                                        
     ADD 1 TO I.                                                
     IF I NOT > 5 GO TO ZE-RCHX.                                
     MOVE 1 TO I.                                               
 LOOPZ.                                                         
     MOVE 0 TO VAL (I, J, K).                                   
     MOVE 0 TO D-C (I, J, K).                                   
     MOVE 0 TO R-C (I, J, K).                                   
     ADD 1 TO K.                                                
     IF K NOT > 3 GO TO LOOPZ.                                  
     MOVE 1 TO K.                                               
     ADD 1 TO J.                                                
     IF J NOT > 44 GO TO LOOPZ.                                 
     MOVE 1 TO J.                                               
     ADD 1 TO I.                                                
     IF I NOT > 5 GO TO LOOPZ.                                  
     MOVE 1 TO I.                                               
 READ-TAPEREQ.                                                  
     READ AMFTG AT END GO TO NEXT-MF.                         
     IF X = 0 MOVE 1 TO X MOVE R-NO TO R-OLD.                   
     IF R-NO = R-OLD GO TO TEST-I.                              
     IF I NOT > 5 MOVE R-OLD TO RCHX (I).                       
     MOVE R-NO TO R-OLD ADD 1 TO I MOVE 1 TO K.                 
     IF I > 5 GO TO ER-1 ELSE MOVE ZERO TO Y GO TO ST-REC.      
 TEST-I.                                                        
     IF I > 5 GO TO ER-1.                                       
     IF Y = 0 GO TO ST-REC.                                     
     MOVE ZERO TO Y.                                            
     GO TO TEST-J.                                              
 ST-REC.                                                        
     MOVE PR-NO (L) TO J.                                       
 STORE-INP.                                                     
     MOVE VAL-R (L) TO VAL (I, J, K).                           
     MOVE DCD (L) TO D-C (I, J, K).                             
     MOVE RCD (L) TO R-C (I, J, K).                             
 TEST-L.                                                        
     ADD 1 TO L.                                                
     IF L > 5 MOVE 1 TO L MOVE 1 TO Y GO TO READ-TAPEREQ.       
 TEST-J.                                                        
     IF PR-NO (L) NOT = J GO TO VRT.                            
     ADD 1 TO K.                                                
     IF K NOT > 3 GO TO STORE-INP.                              
     MOVE REQ-REC TO REC-PRINT.                                 
     MOVE 'ERROR - MORE THAN 3 ENTRIES PER PROPERTY'            
     TO DESC-PRINT.                                             
     IF Z = 0 MOVE 1 TO Z ELSE MOVE ' ' TO C1.                  
     WRITE PRINT FROM H1 . 
     GO TO TEST-L.                                              
 ER-1.                                                          
     MOVE REQ-REC TO REC-PRINT.                                 
     MOVE 'ERROR - MORE THAN 5 REQUESTS /SKIPPED/  '            
     TO DESC-PRINT.                                             
     IF Z = 0 MOVE 1 TO Z ELSE MOVE ' ' TO C1.                  
     WRITE PRINT FROM H1 . 
     GO TO READ-TAPEREQ.                                        
 VRT.                                                           
     IF PR-NO (L) = 0 MOVE 1 TO L MOVE 1 TO Y GO TO READ-TAPEREQ. 
     MOVE 1 TO K.                                               
     GO TO ST-REC.                                              
 NEXT-MF.                                                       
     IF I NOT > 5 MOVE R-OLD TO RCHX (I).                       
     MOVE 1 TO I.                                               
     MOVE 1 TO J.                                               
     MOVE 1 TO K.                                               
     MOVE 1 TO L.                                               
     MOVE ZERO TO X.                                            
     MOVE ZERO TO Y.                                            
 READ-MF.                                                       
     READ MODTG AT END GO TO EMF.                             
     IF X = 0 MOVE 1 TO X MOVE F-R TO HEADER GO TO READ-MF.     
     IF H-C = 1 GO TO COMPARE.                                  
     IF Y NOT = 0 MOVE ZERO TO Y GO TO TST-J.                   
 LOAD-J.                                                        
     MOVE PNOR (L) TO J.                                        
     IF J = 0 MOVE 1 TO L MOVE '0' TO Y MOVE 1 TO K GO TO READ-MF. 
 LOAD-TAB.                                                      
     MOVE VLF (L) TO VALF (J, K).                               
     MOVE VLT (L) TO VALT (J, K).                               
     MOVE DCDE (L) TO DC (J, K).                                
 TST-L.                                                         
     ADD 1 TO L.                                                
     IF L > 3 MOVE 1 TO L MOVE 1 TO Y MOVE ZERO TO S            
     GO TO READ-MF.                                             
 TST-J.                                                         
     IF PNOR (L) NOT = J MOVE 1 TO K GO TO LOAD-J.              
     ADD 1 TO K.                                                
     IF K > 3 GO TO TST-L ELSE GO TO LOAD-TAB.                  
 EMF.                                                           
     MOVE 2 TO X.                                               
     IF S NOT = 0 GO TO EOR.                                    
 COMPARE.                                                       
     MOVE ALL '0' TO ZERO-PROPERTIES.                           
     MOVE 1 TO J.                                               
     MOVE 1 TO K.                                               
     MOVE 1 TO L.                                               
 TEST-PR.                                                       
     MOVE DC (J, 1) TO DCH1.                                    
     MOVE DC (J, 2) TO DCH2.                                    
     MOVE DC (J, 3) TO DCH3.                                    
     MOVE D-C (I, J, 1) TO D-CH1.                               
     MOVE D-C (I, J, 2) TO D-CH2.                               
     MOVE D-C (I, J, 3) TO D-CH3.                               
     MOVE VAL (I, J, 1) TO VALH.                                
     MOVE VALF (J, 1) TO VALFH.                                 
     MOVE VALT (J, 1) TO VALTH.                                 
     MOVE R-C (I, J, 1) TO R-CH.                                
     IF VALH = 0 AND D-CH1 = 0 GO TO SAT.                       
     IF J = 36 PERFORM CONVERSION-36 THRU E-CON36.              
     ADD 1 TO COUNT-RQ.                                         
     IF VALH NOT = 0 GO TO ST1.                                 
     IF VALFH = 0 AND VALTH = 0 GO TO XCH-DCXC.                 
 BACK-XYZ.                                                      
     IF J =  1 GO TO CTC1.                                      
     IF J = 36 GO TO CTC36.                                     
     IF J = 37 GO TO CTC37.                                     
     IF J = 38 GO TO CTC38.                                     
     IF J = 39 GO TO CTC39.                                     
     IF J = 40 OR J = 41 GO TO CTC40.                           
     IF J = 42 GO TO CTC42.                                     
   J = 44 GO TO CTC44.                                     
 N-CONT.                                                        
     IF VALFH = 0 AND VALTH = 0 GO TO XCH-DCXC1.                
 BACK-XYZ1.                                                     
     IF D-CH1 = DCH1 GO TO ST2.                                 
     IF D-CH1 = DCH2 GO TO ST2.                                 
     IF D-CH1 NOT = DCH3 GO TO FAIL.                            
 ST2.                                                           
     IF D-CH2 = 0 GO TO SAT.                                    
     IF D-CH2 = DCH1 GO TO ST3.                                 
     IF D-CH2 = DCH2 GO TO ST3.                                 
     IF D-CH2 NOT = DCH3 GO TO FAIL.                            
 ST3.                                                           
     IF D-CH3 = 0 GO TO SAT.                                    
     IF D-CH3 = DCH1 GO TO SAT.                                 
     IF D-CH3 = DCH2 GO TO SAT.                                 
     IF D-CH3 NOT = DCH3 GO TO FAIL.                            
 SAT.                                                           
     IF Z-SW NOT = 0 PERFORM ST-ZPRP.                           
     ADD 1 TO J.                                                
     IF J NOT > 44 GO TO TEST-PR.                               
     IF OPT = 0 MOVE 0 TO LPX1 (1) LPX1 (2)                     
     GO TO MT-XK.                                               
     IF COUNT-RQ = 3 AND OPT = 1 MOVE 0 TO LPX1 (2)             
     GO TO MT-XK.                                               
     IF COUNT-RQ = 4 AND OPT = 1 MOVE 0 TO LPX1 (2)             
     GO TO MT-XK.                                               
     IF OPT = 1 MOVE 0 TO LPX1 (2).                             
     IF COUNT-RQ > 4 GO TO MT-XK.                               
     GO TO OBXK1.                                               
 MT-XK.                                                         
     MOVE RCHX (I) TO RQNO.                                     
     MOVE HEADER TO HDR-REC.                                    
     MOVE OPT TO INFO-OUT.                                      
     MOVE TS-ZPMF TO ZP-INF.                                    
     WRITE OUT-REC.                                             
     GO TO OBXK1.                                               
 FAIL.                                                          
     ADD 1 TO OPT.                                              
     IF OPT NOT > 2 MOVE J TO LPX1 (OPT) GO TO SAT.             
 OBXK1.                                                         
     MOVE 0 TO OPT COUNT-RQ TS-ZPMF.                            
     MOVE 0 TO Z-SW SUBZP.                                      
     MOVE ALL '0' TO ZERO-PROPERTIES.                           
     MOVE 1 TO J.                                               
     ADD 1 TO I.                                                
     IF I > 5 GO TO FXA.                                        
     IF RCHX (I) = 0 GO TO FXA.                                 
     MOVE 1 TO K GO TO TEST-PR.                                 
 FXA.                                                           
     IF X = 2 GO TO EOR.                                        
     MOVE 1 TO X.                                               
     MOVE 1 TO J.                                               
     MOVE 1 TO K.                                               
     PERFORM LOOPZM.                                            
     MOVE F-R TO HEADER.                                        
     MOVE 1 TO I.                                               
     MOVE 1 TO L.                                               
     MOVE ZERO TO Y.                                            
     MOVE 1 TO S.                                               
     GO TO READ-MF.                                             
 CTC1.                                                          
     IF DCH1 NOT = 0 MOVE T1 (DCH1) TO DCH1.                    
     IF DCH2 NOT = 0 MOVE T1 (DCH2) TO DCH2.                    
     IF DCH3 NOT = 0 MOVE T1 (DCH3) TO DCH3.                    
     MOVE T1 (D-CH1) TO D-CH1.                                  
     IF D-CH2 NOT = 0 MOVE T1 (D-CH2) TO D-CH2.                 
     IF D-CH3 NOT = 0 MOVE T1 (D-CH3) TO D-CH3.                 
     GO TO COMP-TC.                                             
 CTC36.                                                         
     IF DCH1 NOT = 0 MOVE T36 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T36 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T36 (DCH3) TO DCH3.                   
     MOVE T36 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T36 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T36 (D-CH3) TO D-CH3.                
     GO TO COMP-TC.                                             
 CTC37.                                                         
     IF DCH1 NOT = 0 MOVE T37 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T37 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T37 (DCH3) TO DCH3.                   
     MOVE T37 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T37 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T37 (D-CH3) TO D-CH3.                
     GO TO COMP-TC.                                             
 CTC38.                                                         
     IF DCH1 NOT = 0 MOVE T38 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T38 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T38 (DCH3) TO DCH3.                   
     MOVE T38 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T38 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T38 (D-CH3) TO D-CH3.                
     GO TO COMP-TC.                                             
 CTC39.                                                         
     IF DCH1 NOT = 0 MOVE T39 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T39 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T39 (DCH3) TO DCH3.                   
     MOVE T39 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T39 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T39 (D-CH3) TO D-CH3.                
     GO TO COMP-TC.                                             
 CTC40.                                                         
     IF DCH1 NOT = 0 MOVE T40 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T40 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T40 (DCH3) TO DCH3.                   
     MOVE T40 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T40 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T40 (D-CH3) TO D-CH3.                
     GO TO COMP-TC.                                             
 CTC42.                                                         
     IF DCH1 NOT = 0 MOVE T42 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T42 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T42 (DCH3) TO DCH3.                   
     MOVE T42 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T42 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T42 (D-CH3) TO D-CH3.                
     GO TO N-CONT.                                              
 CTC44.                                                         
     IF DCH1 NOT = 0 MOVE T44 (DCH1) TO DCH1.                   
     IF DCH2 NOT = 0 MOVE T44 (DCH2) TO DCH2.                   
     IF DCH3 NOT = 0 MOVE T44 (DCH3) TO DCH3.                   
     MOVE T44 (D-CH1) TO D-CH1.                                 
     IF D-CH2 NOT = 0 MOVE T44 (D-CH2) TO D-CH2.                
     IF D-CH3 NOT = 0 MOVE T44 (D-CH3) TO D-CH3.                
 COMP-TC.                                                       
     IF DCH1 > D-CH1 GO TO FAIL.                                
     IF DCH2 > D-CH1 GO TO FAIL.                                
     IF DCH3 > D-CH1 GO TO FAIL.                                
     IF D-CH2 = 0 GO TO SAT.                                    
     IF DCH1 > D-CH2 GO TO FAIL.                                
     IF DCH2 > D-CH2 GO TO FAIL.                                
     IF DCH3 > D-CH2 GO TO FAIL.                                
     IF D-CH3 = 0 GO TO SAT.                                    
     IF DCH1 > D-CH3 GO TO FAIL.                                
     IF DCH2 > D-CH3 GO TO FAIL.                                
     IF DCH3 > D-CH3 GO TO FAIL.                                
     GO TO SAT.                                                 
 ST1.                                                           
     IF VALFH = 0 AND VALTH = 0 GO TO ST4.                      
     GO TO ST5.                                                 
 ST4.                                                           
     ADD 1 TO TS-ZPMF.                                          
     MOVE 1 TO Z-SW.                                            
     IF DCH1 = 0 GO TO SAT.                                     
 ST5.                                                           
     IF J NOT = 25 GO TO ST6.                                   
     IF D-CH1 = 0 COMPUTE ARGO = VALH * 1 GO TO ST7.            
     COMPUTE ARGO = VALH * 10 **  (D-CH1 + 3).                  
 ST7.                                                           
     IF DCH1 NOT = 0 GO TO ST8.                                 
     COMPUTE RAN2 = VALTH * 1.                                  
     IF VALFH NOT = 0 GO TO ST9.                                
     COMPUTE RAN1 = 1 * 0.                                      
     GO TO ST11.                                                
 ST9.                                                           
     COMPUTE RAN1 = VALFH * 1.                                  
     IF VALTH = 0 GO TO ST12 ELSE GO TO ST11.                   
 ST8.                                                           
     COMPUTE RAN1 = VALFH * 10 **  (DCH1 + 3).                  
     IF VALTH = 0 GO TO ST12.                                   
     COMPUTE RAN2 = VALTH * 10 **  (DCH1 + 3).                  
 ST11.                                                          
     IF ARGO < RAN1 GO TO ST13.                                 
     IF ARGO NOT > RAN2 GO TO SAT.                              
     IF R-CH = '2' GO TO SAT ELSE GO TO FAIL.                   
 ST13.                                                          
     IF R-CH = '1' GO TO SAT ELSE GO TO FAIL.                   
 ST12.                                                          
     IF RAN1 = ARGO GO TO SAT.                                  
     IF R-CH = ' ' GO TO FAIL.                                  
     IF RAN1 > ARGO AND R-CH = '1' GO TO SAT.                   
     IF RAN1 NOT > ARGO AND R-CH = '2' GO TO SAT.               
     GO TO FAIL.                                                
 ST6.                                                           
     IF J NOT = 18 GO TO ST14.                                  
     IF D-CH1 NOT = 5 GO TO ST15.                               
     COMPUTE ARGO = VALH * 10 ** 6.                             
     IF DCH1 NOT = 5 GO TO ST16.                                
 ST17.                                                          
     COMPUTE RAN1 = VALFH * 10 ** 6.                            
     IF VALTH = 0 GO TO ST12.                                   
     COMPUTE RAN2 = VALTH * 10 ** 6.                            
     GO TO ST11.                                                
 ST16.                                                          
     COMPUTE RAN1 = VALFH * 1.                                  
     IF VALTH = 0 GO TO ST12 ELSE COMPUTE RAN2 = VALTH * 1      
     GO TO ST11.                                                
 ST15.                                                          
     IF DCH1 NOT = 5 GO TO ST14.                                
     IF D-CH1 = 0 COMPUTE ARGO = VALH * 1 GO TO ST17.           
     IF D-CH1 = DCH2 COMPUTE ARGO = VALH * 1 GO TO ST17.        
     GO TO FAIL.                                                
 ST14.                                                          
     IF J NOT = 22 GO TO ST18.                                  
     IF D-CH1 = 0 GO TO ST19.                                   
     COMPUTE ARGO = VALH / 10 **  (D-CH1 + 4).                  
     IF DCH1 = 0 GO TO ST16.                                    
 ST20.                                                          
     COMPUTE RAN1 = VALFH / 10 **  (DCH1 + 4).                  
     IF VALTH = 0 GO TO ST12.                                   
     COMPUTE RAN2 = VALTH / 10 **  (DCH1 + 4).                  
     GO TO ST11.                                                
 ST19.                                                          
     IF DCH1 = 0 GO TO ST18.                                    
     COMPUTE ARGO = VALH * 1.                                   
     GO TO ST20.                                                
 ST18.                                                          
     MOVE ZERO TO COUNT.                                        
     MOVE 1 TO N.                                               
     IF D-CH1 NOT = 0 GO TO ST21.                               
 ST22.                                                          
     ADD K COUNT GIVING M.                                      
     IF M > 3 GO TO FAIL.                                       
     IF M = 1 GO TO CHX1.                                       
     IF VALF (J, M) NOT = 0 GO TO CHX1.                         
     IF VALT (J, M) NOT = 0 GO TO CHX1.                         
     GO TO FAIL.                                                
 CHX1.                                                          
     IF VALT (J, M) = 0 GO TO ST23.                             
     IF VAL (I, J, N) < VALF (J, M) GO TO ST24.                 
     IF VAL (I, J, N) > VALT (J, M) GO TO ST25.                 
 ST26.                                                          
     MOVE ZERO TO COUNT.                                        
     ADD 1 TO N.                                                
     IF N > 3 GO TO SAT.                                        
     IF VAL (I, J, N) = 0 GO TO SAT ELSE GO TO ST22.            
 ST23.                                                          
     IF VAL (I, J, N) = VALF (J, M) GO TO ST26.                 
     IF R-C (I, J, N) = ' ' ADD 1 TO COUNT GO TO ST22.          
     IF VAL (I, J, N) > VALF (J, M) GO TO ST25.                 
 ST24.                                                          
     IF R-C (I, J, N) = '1' GO TO ST26.                         
     ADD 1 TO COUNT GO TO ST22.                                 
 ST25.                                                          
     IF R-C (I, J, N) = '2' GO TO ST26.                         
     ADD 1 TO COUNT GO TO ST22.                                 
 ST21.                                                          
     ADD K COUNT GIVING M.                                      
     IF M > 3 GO TO FAIL.                                       
     IF M = 1 GO TO CHX2.                                       
     IF VALF (J, M) NOT = 0 GO TO CHX2.                         
     IF VALT (J, M) NOT = 0 GO TO CHX2.                         
     IF DC (J, M) NOT = 0 GO TO CHX2.                           
     GO TO FAIL.                                                
 CHX2.                                                          
     IF D-C (I, J, N) NOT = DC (J, M) ADD 1 TO COUNT GO TO ST21.
     IF VALT (J, M) = 0 GO TO ST27.                             
     IF VAL (I, J, N) < VALF (J, M) GO TO ST28.                 
     IF VAL (I, J, N) > VALT (J, M) GO TO ST29.                 
 ST30.                                                          
     MOVE ZERO TO COUNT.                                        
     ADD 1 TO N.                                                
     IF N > 3 GO TO SAT.                                        
     IF VAL (I, J, N) NOT = 0 GO TO ST21.                       
     IF D-C (I, J, N) NOT = 0 GO TO ST21.                       
     IF R-C (I, J, N) NOT = 0 GO TO ST21.                       
     GO TO SAT.                                                 
 ST27.                                                          
     IF VAL (I, J, N) = VALF (J, M) GO TO ST30.                 
     IF R-C (I, J, N) = ' ' ADD 1 TO COUNT GO TO ST21.          
     IF VAL (I, J, N) > VALF (J, M) GO TO ST29.                 
 ST28.                                                          
     IF R-C (I, J, N) = '1' GO TO ST30.                         
     ADD 1 TO COUNT.                                            
     GO TO ST21.                                                
 ST29.                                                          
     IF R-C (I, J, N) = '2' GO TO ST30.                         
     ADD 1 TO COUNT.                                            
     GO TO ST21.                                                
 EOR.                                                           
     MOVE SPACES TO OUT-REC.                                    
     MOVE 1 TO I.                                               
 RST.                                                           
     IF RCHX (I) NOT = 0 MOVE RCHX (I) TO RQNO WRITE OUT-REC.   
     MOVE SPACES TO OUT-REC.                                    
     ADD 1 TO I.                                                
     IF I NOT > 5 GO TO RST.                                    
     CLOSE        AMFTG                                       
     CLOSE        MODTG                                       
     CLOSE        FIL60                                       
     CLOSE        FIL65.                                      
        STOP RUN. 
 XCH-DCXC.                                                      
     IF DCH1 = 0 ADD 1 TO TS-ZPMF MOVE 1 TO Z-SW.               
     GO TO BACK-XYZ.                                            
 XCH-DCXC1.                                                     
     IF DCH1 = 0 GO TO SAT.                                     
     GO TO BACK-XYZ1.                                           
 CONVERSION-36.                                                 
     IF VALTH = 0 AND VALFH = 0 GO TO CHECK-VALH.               
     IF VALH NOT = 0 GO TO E-CON36.                             
     IF VALTH = 0 PERFORM CONV-FIRST THRU ECNF                  
     MOVE 0 TO VALFH GO TO E-CON36.                             
     IF VALTH NOT > 0.2 MOVE 1 TO DCH1 MOVE 0 TO VALTH VALFH    
     GO TO E-CON36.                                             
     IF VALTH NOT > 0.5 MOVE 12 TO DCH1 MOVE 0 TO VALTH VALFH   
     GO TO E-CON36.                                             
     IF VALTH NOT > 2   MOVE 23 TO DCH1 MOVE 0 TO VALTH VALFH   
     GO TO E-CON36.                                             
                        MOVE 24 TO DCH1 MOVE 0 TO VALTH VALFH   
     GO TO E-CON36.                                             
 CHECK-VALH.                                                    
     IF VALH = 0 GO TO E-CON36.                                 
     IF VALH NOT > 0.2 MOVE 3 TO D-CH1 MOVE 0 TO VALH           
     GO TO E-CON36.                                             
     IF VALH NOT > 0.5 MOVE 12 TO D-CH1 MOVE 0 TO VALH          
     GO TO E-CON36.                                             
     IF VALH NOT > 2   MOVE 23 TO D-CH1 MOVE 0 TO VALH          
     GO TO E-CON36.                                             
                       MOVE 24 TO D-CH1 MOVE 0 TO VALH.         
 E-CON36.                                                       
     EXIT.                                                      
 CONV-FIRST.                                                    
     IF VALFH NOT > 0.2 MOVE 1 TO DCH1 GO TO ECNF.              
     IF VALFH NOT > 0.5 MOVE 12 TO DCH1 GO TO ECNF.             
     IF VALFH NOT > 2   MOVE 23 TO DCH1 GO TO ECNF.             
     MOVE 24 TO DCH1.                                           
 ECNF.                                                          
     EXIT.                                                      
 ST-ZPRP.                                                       
     MOVE 0 TO Z-SW.                                            
     ADD 1 TO SUBZP.                                            
     IF SUBZP NOT > 6 MOVE J TO ZO-PS (SUBZP).                  
    '@X