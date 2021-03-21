IDENTIFICATION DIVISION.
 PROGRAM-ID. S0CMR040.                                           
 AUTHOR. S GOGELA.                                               
 ENVIRONMENT DIVISION.                                           
 CONFIGURATION SECTION.                                          
 INPUT-OUTPUT SECTION.                                           
 FILE-CONTROL.                                                   
     SELECT DATA1 ASSIGN TO DSK RECORDING MODE IS ASCII. 
     SELECT AMFTG ASSIGN TO DSK RECORDING MODE IS ASCII. 

 DATA DIVISION.                                                  
 FILE SECTION.                                                   
 FD  DATA1                                                      
     VALUE OF IDENTIFICATION IS 'DATA1 DAT'. 
 01  IN-REC . 
     02 F-R.                                                     
       04 OPT    PICTURE X(7).                                   
       04 OP     PICTURE X(3).                                   
       04 FILLER PICTURE X(70).                                  
     02 S-R REDEFINES F-R.                                       
       04 REQ-NO.                                                
         06 RQNMR   PICTURE 9(2).                                
       04 FILLER PICTURE X(78).                                  
 FD  AMFTG                                                      
     VALUE OF IDENTIFICATION IS 'AMFTG DAT'. 
 01  OUT . 
     02 FILLER   PICTURE X(80).                                  
 WORKING-STORAGE SECTION.                                        
 77  RSN    COMPUTATIONAL   PICTURE S99 VALUE 1 SYNC.            
 77  SW      PICTURE X VALUE ZERO.                               
 77  X       PICTURE X VALUE ZERO.                               
 77  Y       PICTURE X VALUE ZERO.                               
 77  Z       PICTURE X VALUE ZERO.                               
 77  A       PICTURE X VALUE ZERO.                               
 77  ERR     PICTURE X VALUE ZERO.                               
 77  REQ-P   PICTURE X(2) VALUE ZEROS.                           
 77  I       COMPUTATIONAL PICTURE S99 VALUE 1 SYNC.             
 77  K       COMPUTATIONAL PICTURE S99 VALUE 1 SYNC.             
 77  L       COMPUTATIONAL PICTURE S9  VALUE 1 SYNC.             
 77  ABC     PICTURE 9 VALUE ZERO.                               
 77  PN-CH   PICTURE 9(2) VALUE ZERO.                            
01  PRINT . 
    02 CC        PICTURE X. 
    02 FILLER   PICTURE X(132). 
 01  CH-R . 
     02 RECORD--X OCCURS 31 TIMES.                               
       04 CCC PICTURE X.                                         
       04 REC.                                                   
         06 RQNOR.                                               
         07 R-NO   PICTURE 9(2).                                 
         06 FORM  PICTURE 9(3).                                  
         06 PROP-REC OCCURS 5 TIMES.                             
          07 PNX.                                                
           08 P-N    PICTURE 9(2).                               
          07 VLX.                                                
           08 VAL    PICTURE 9(6)V9(4).                          
          07 DCX.                                                
           08 D-C    PICTURE 9(2).                               
          07 R-C     PICTURE X.                                  
       04 FILLER     PICTURE X(10).                              
       04 DESC       PICTURE X(42).                              
 01  H1 . 
     02 C1       PICTURE X VALUE '1'.                            
     02 FILLER   PICTURE X(8) VALUE '  ***   '.                  
     02 DATO     PICTURE X(12).                                  
     02 FILLER   PICTURE X(80) VALUE '   *   COMPUTERIZED MATERIA
-    ' SELECTION SYSTEM   *  INVALID REQUEST CARDS  ***   '.     
     02 FILLER   PICTURE X(32) VALUE SPACES.                     
 01  H2 . 
     02 C2       PICTURE X VALUE '-'.                            
     02 FILLER   PICTURE X(26) VALUE SPACES.                     
     02 FILLER   PICTURE X(25) VALUE ' = MISSING OPTION CARD = '.
     02 FILLER   PICTURE X(81) VALUE SPACES.                     
 01  R-NO--Q000.                                                 
 02  R-NO--R000 PICTURE S9(2).                                   
 01  P-N--Q001.                                                  
 02  P-N--R001 PICTURE S9(2).                                    
 01  VAL--Q002 . 
 02  VAL--R002 PICTURE S9(6)V9(4).                               
 01  D-C--Q003.                                                  
 02  D-C--R003 PICTURE S9(2).                                    
 PROCEDURE DIVISION.                                             
 ST-PR.                                                          
     OPEN  INPUT  DATA1                                         
     OPEN  OUTPUT AMFTG                                         . 
     MOVE TODAY TO DATO.                                  
     MOVE SPACES TO CH-R.                                        
 READ-REC.                                                       
     READ DATA1 AT END GO TO EOR.                               
     IF X = '0' GO TO ST1.                                       
 ST2.                                                            
     IF Z = '0' MOVE 1 TO Z MOVE REQ-NO TO REQ-P.                
     IF REQ-NO = REQ-P GO TO ST3.                                
     MOVE REQ-NO TO REQ-P.                                       
     IF A = '0' GO TO ST4.                                       
     MOVE ZERO TO A.                                             
     MOVE 1 TO I.                                                
     GO TO ST5.                                                  
 ST1.                                                            
     MOVE 1 TO X.                                                
     IF OPT = 'OPTION ' GO TO ST6.                               
     MOVE 3 TO ABC.                                              
     GO TO ST2.                                                  
 ST6.                                                            
     IF OP = 'ALL' MOVE 1 TO ABC GO TO READ-REC.                 
     IF OP = 'ONL' MOVE 2 TO ABC GO TO READ-REC.                 
     MOVE 3 TO ABC.                                              
     GO TO READ-REC.                                             
 ST3.                                                            
     IF I NOT > 30 GO TO ST5.                                    
     IF A = '0' GO TO ST7.                                       
 OLE.                                                            
     MOVE 'WRONG NUMBER OF REQ.CARDS FOR 1 REQUEST>30' TO        
     DESC (1).                                                   
     MOVE IN-REC TO REC (1).                                     
     DISPLAY RECORD--X (1). 
     MOVE SPACES TO CH-R.                                        
     GO TO READ-REC.                                             
 ST7.                                                            
     MOVE 1 TO K.                                                
     IF Y = 0 MOVE 1 TO Y DISPLAY H1. 
 ST8.                                                            
     MOVE 'WRONG NUMBER OF REQ.CARDS FOR 1 REQUEST>30' TO        
     DESC (K).                                                   
     DISPLAY RECORD--X (K). 
     ADD 1 TO K.                                                 
     IF K NOT > I GO TO ST8.                                     
     MOVE 1 TO A.                                                
     GO TO OLE.                                                  
 ST5.                                                            
     MOVE IN-REC TO REC (I).                                     
     ADD 1 TO I.                                                 
     GO TO READ-REC.                                             
 ST4.                                                            
     MOVE 1 TO K.                                                
     MOVE 1 TO L.                                                
 ST9.                                                            
     EXAMINE RQNOR (K) REPLACING LEADING ' ' BY ZERO.            
     MOVE R-NO (K) TO R-NO--Q000 IF R-NO--R000 NOT NUMERIC MOVE 1
     TO ERR                                                      
     MOVE 'REQ-NO IS NOT NUMERIC' TO DESC (K).                   
     EXAMINE PNX (K, L) REPLACING LEADING ' ' BY ZERO.           
     EXAMINE VLX (K, L) REPLACING LEADING ' ' BY ZERO.           
     EXAMINE DCX (K, L) REPLACING LEADING ' ' BY ZERO.           
     MOVE P-N (K L) TO P-N--Q001 IF P-N--R001 NOT NUMERIC MOVE 1 
     TO ERR                                                      
     MOVE 'PROPERTY NUMBER IS NOT NUMERIC' TO DESC (K)           
     GO TO KRX7.                                                 
     IF P-N (K, L) > 44 MOVE 1 TO ERR                            
     MOVE 'PROPERTY NUMBER IS > 44       ' TO DESC (K).          
 KRX7.                                                           
     MOVE VAL (K L) TO VAL--R002 IF VAL--R002 NOT NUMERIC MOVE 1 
     TO ERR                                                      
     MOVE 'VALUE IS NOT NUMERIC' TO DESC (K).                    
     MOVE D-C (K L) TO D-C--Q003 IF D-C--R003 NOT NUMERIC MOVE 1 
     TO ERR                                                      
     MOVE 'DESCRIPTION CODE IS NOT NUMERIC' TO DESC (K).         
     IF R-C (K, L) = ' ' GO TO ST10.                             
     IF R-C (K, L) = '1' GO TO ST10.                             
     IF R-C (K, L) = '2' GO TO ST10.                             
     MOVE 'WRONG R-CODE' TO DESC (K).                            
     MOVE 1 TO ERR.                                              
 ST10.                                                           
     IF K = 1 AND L = 1 MOVE P-N (K, L) TO PN-CH.                
     IF P-N (K, L) = PN-CH GO TO ST11.                           
     IF P-N (K, L) > PN-CH GO TO ST11.                           
     IF P-N (K, L) NOT = 0 GO TO MERY.                           
     IF L = 5  ADD K 1 GIVING RSN ELSE GO TO MARA.               
     IF RSN = I GO TO ST11.                                      
     GO TO MERY.                                                 
 MARA.                                                           
     ADD L 1 GIVING RSN.                                         
     IF PNX (K, RSN) = '  ' GO TO ST11.                          
 MERY.                                                           
     MOVE 'WRONG SEQUENCE OF PROP.NO' TO DESC (K).               
     MOVE 1 TO ERR.                                              
 ST11.                                                           
     MOVE P-N (K, L) TO PN-CH.                                   
     ADD 1 TO L.                                                 
     IF L NOT = 6 GO TO ST9.                                     
     MOVE 1 TO L.                                                
     ADD 1 TO K.                                                 
     IF K NOT = I GO TO ST9.                                     
     IF ERR NOT = '0' GO TO ST12.                                
     MOVE 1 TO K.                                                
 ST13.                                                           
     MOVE ABC TO FORM (K).                                       
     WRITE OUT FROM REC (K).                                     
     ADD 1 TO K.                                                 
     IF K NOT = I GO TO ST13.                                    
 ST14.                                                           
     MOVE SPACES TO CH-R.                                        
     MOVE ZERO TO ERR.                                           
     MOVE 1 TO I.                                                
     MOVE IN-REC TO REC (I).                                     
     ADD 1 TO I.                                                 
     IF SW NOT = '0' GO TO EOJ.                                  
     GO TO READ-REC.                                             
 ST12.                                                           
     MOVE 1 TO K.                                                
     IF Y = '0' MOVE 1 TO Y DISPLAY H1. 
 ST15.                                                           
     DISPLAY RECORD--X (K). 
     ADD 1 TO K.                                                 
     IF K NOT = I GO TO ST15.                                    
     GO TO ST14.                                                 
 EOR.                                                            
     IF A = '0' MOVE 1 TO SW GO TO ST4.                          
 EOJ.                                                            
     CLOSE        DATA1                                         
     CLOSE        AMFTG                                         
     STOP RUN. 
  
   