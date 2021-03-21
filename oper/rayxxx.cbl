IDENTIFICATION  DIVISION.                                         
PROGRAM-ID.  XEZ45CF8
AUTHOR.  DENNIS L. WARN.                                          
INSTALLATION.  OXNARD DIGITAL.                                    
DATE-WRITTEN.  03/19/73   REVISED   /  / .                        
DATE-COMPILED.    /  / .                                          
SECURITY.                                                         
REMARKS.                                                          
     DGF8 CONVERTS A FROM/TO TAPE INTO                            
     A TRUE STRING TAPE.                                          
                                                                  
                                                                  
ENVIRONMENT  DIVISION.                                            
CONFIGURATION  SECTION.                                           
SOURCE-COMPUTER. PDP-10.                                          
OBJECT-COMPUTER. PDP-10.                                          
INPUT-OUTPUT  SECTION.                                            
FILE-CONTROL.                                                     
     SELECT CARD-IN                                               
         ASSIGN DSK,                                              
      RECORDING MODE IS ASCII.                                    
                                                                  
     SELECT TAPE-IN                                               
         ASSIGN DSK,                                              
      RECORDING MODE IS ASCII.                                    
                                                                  
     SELECT TAPE-OUT                                              
         ASSIGN DSK,                                              
      RECORDING MODE IS ASCII.                                    
                                                                  
                                                                  
                                                                  
DATA  DIVISION.                                                   
FILE  SECTION.                                                    
FD   CARD-IN                                                      
         VALUE OF IDENTIFICATION IS "CARDINDAT"                   
     RECORD CONTAINS 80 CHARACTERS                                
     LABEL RECORD IS STANDARD                                     
     DATA RECORD IS CARDIN.                                       
01   CARDIN.                                                      
     02  CARD-TAB                PICTURE X(11).                   
     02  FILLER                  PICTURE X(69).                   
                                                                  
FD   TAPE-IN                                                      
         VALUE OF IDENTIFICATION IS "TAPEINDAT"                   
     RECORD CONTAINS 60 CHARACTERS                                
     BLOCK CONTAINS 50 RECORDS                                    
     LABEL RECORD IS STANDARD                                     
     DATA RECORD IS TAPEIN.                                       
01   TAPEIN.                                                      
     02  TI-SIGNAL               PICTURE X(5).                    
     02  FILLER                  PICTURE X.                       
     02  TI-FROM                 PICTURE X(9).                    
     02  FILLER                  PICTURE X.                       
     02  TI-TO                   PICTURE X(9).                    
     02  FILLER                  PICTURE X(35).                   
                                                                  
FD   TAPE-OUT                                                     
         VALUE OF IDENTIFICATION IS "TAPEOTDAT"                   
     RECORD CONTAINS 60 CHARACTERS                                
     BLOCK CONTAINS 50 RECORDS                                    
     LABEL RECORD IS STANDARD                                     
     DATA RECORD IS TAPEOUT.                                      
01   TAPEOUT                     PICTURE X(60).                   
                                                                  
WORKING-STORAGE  SECTION.                                         
77   A           COMPUTATIONAL   PICTURE 9(5).                    
77   B           COMPUTATIONAL   PICTURE 9(5).                    
77   C           COMPUTATIONAL   PICTURE 9(5).                    
77   D           COMPUTATIONAL   PICTURE 9(5).                    
77   E           COMPUTATIONAL   PICTURE 9(5).                    
77   F1          COMPUTATIONAL   PICTURE 9(5).                    
77   F2          COMPUTATIONAL   PICTURE 9(5).                    
77   G1          COMPUTATIONAL   PICTURE 9(5).                    
77   G2          COMPUTATIONAL   PICTURE 9(5).                    
77   H           COMPUTATIONAL   PICTURE 9(5).                    
77   I           COMPUTATIONAL   PICTURE 9(5).                    
77   J           COMPUTATIONAL   PICTURE 9(5).                    
77   K           COMPUTATIONAL   PICTURE 9(5).                    
77   L           COMPUTATIONAL   PICTURE 9(5).                    
77   M           COMPUTATIONAL   PICTURE 9(5).                    
77   N           COMPUTATIONAL   PICTURE 9(5).
01   MATRIX.
     02  MAT1        OCCURS 2.
         03  MAT2    OCCURS 7500.
             04  MATF            PICTURE 9.
             04  MATP   COMPUTATIONAL PICTURE 9(5).
01   DIMENSIONS.
     02  DIM         OCCURS 500.                                  
         03  KDIM.                                                
             04  KDIM13.                                          
                 05  KDIM12      PICTURE XX.                      
                 05  KDIM3       PICTURE X.                       
             04  KDIM46.                                          
                 05  KDIM45.                                      
                     06  KDIM4   PICTURE X.                       
                     06  FILLER  PICTURE X.                       
                 05  FILLER      PICTURE X.                       
         03  SDIM                        PICTURE 9(5).            
01   SPLITTER.                                                    
     02  CONN-ID                 PICTURE XX.                      
     02  BLOCK-NO                PICTURE X.                       
     02  CONN-NO                 PICTURE XX.                      
     02  DASH-CON                PICTURE X.                       
     02  PIN-NO                  PICTURE XXX.                     
01   MAKE-KEY.                                                    
     02  KCONN-ID                PICTURE XX.                      
     02  ABC                     PICTURE X.                       
     02  KBCP-NO                 PICTURE XXX.                     
01   STRING-RECORD.                                               
     02  SR-NAME                 PICTURE X(7).                    
     02  SR-CODE                 PICTURE X.                       
     02  SR-OUTPUT               PICTURE X(7).                    
     02  SR-CONN                 PICTURE X(9).                    
     02  FILLER                  PICTURE X(36).                   
01   INDEX-HOLD.                                                  
     02  INDEXH PICTURE 9(3) OCCURS 3 TIMES.
01   RET-KEY.                                                     
     02  CONN-OD                 PICTURE XX.                      
     02  BLOCK-NU                PICTURE X.                       
01   SIGNAL-NAME.                                                 
     02  SIG-ST                  PICTURE XX      VALUE 'ST'.      
     02  SIG-NO                  PICTURE 9(4).                    
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
PROCEDURE  DIVISION.                                              
INITIALIZE.                                                       
     OPEN INPUT CARD-IN TAPE-IN OUTPUT TAPE-OUT.                  
     MOVE 1 TO A B C D E F1 F2 G1 G2 H I J K L M N SIG-NO.        
     MOVE ZERO TO        DIMENSIONS MOVE SPACE TO STRING-RECORD.  
ZERO-MAT.                                                         
     MOVE ZERO TO MATF (A, B) MATP (A, B).                        
     ADD 1 TO B.                                                  
     IF B IS NOT EQUAL TO  7501  GO TO ZERO-MAT.                  
     MOVE 1 TO B ADD 1 TO A.                                      
     IF A IS NOT EQUAL TO  3  GO TO ZERO-MAT.                     
READ-CARD.                                                        
     READ CARD-IN AT END GO TO READ-TAPE.                         
     MOVE CARD-TAB TO DIM (A).                                    
     ADD 1 TO A GO TO READ-CARD.                                  
READ-TAPE.                                                        
     READ TAPE-IN AT END MOVE 1 TO J GO TO OUTPUT-STRING.         
     MOVE TI-FROM TO SPLITTER MOVE CONN-ID TO KCONN-ID.           
     MOVE 1 TO E.                                                 
     DISPLAY       TAPEIN.                                        
GET-DIM-LOOP.                                                     
     MOVE 'A' TO ABC MOVE PIN-NO TO KBCP-NO MOVE 1 TO B.          
     PERFORM GET-DIM THRU GET-DIM-EXIT.                           
     MOVE SDIM (B) TO C.                                          
     MOVE 'B' TO ABC MOVE BLOCK-NO TO KBCP-NO.                    
     PERFORM GET-DIM THRU GET-DIM-EXIT.                           
     ADD  SDIM (B) TO C.                                          
     MOVE 'C' TO ABC MOVE CONN-NO TO KBCP-NO.                     
     PERFORM GET-DIM THRU GET-DIM-EXIT.                           
     ADD SDIM (B) TO C.                                           
     IF E IS EQUAL TO 1 ADD 1 TO E MOVE C TO D                    
         MOVE TI-TO TO SPLITTER MOVE CONN-ID TO KCONN-ID          
         GO TO GET-DIM-LOOP.                                      
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MULTIPLY MATF (F1, F2) BY 4 GIVING E.                        
     ADD MATF (G1, G2) TO E.                                      
     DISPLAY       E D MAT2 (F1, F2) C MAT2 (G1, G2).             
     IF E =  0 GO TO SIT00.                                       
     IF E = 12 GO TO SIT30.                                       
     IF E = 13 GO TO SIT31.                                       
     IF E =  1 GO TO SIT01.                                       
     IF E =  2 GO TO SIT02.                                       
     IF E =  3 GO TO SIT03.                                       
     IF E =  4 GO TO SIT10.                                       
     IF E =  5 GO TO SIT11.                                       
     IF E =  6 GO TO SIT12.                                       
     IF E =  7 GO TO SIT13.                                       
     IF E =  8 GO TO SIT20.                                       
     IF E =  9 GO TO SIT21.                                       
     IF E = 10 GO TO SIT22.                                       
     IF E = 11 GO TO SIT23.                                       
     IF E = 14 GO TO SIT32.                                       
     IF E = 15 GO TO SIT33.                                       
     DISPLAY 'BAD SITUATION POINTER = ' E ', ' D ', ' C.          
     DISPLAY TAPEIN.                                              
     GO TO READ-TAPE.                                             
SIT00.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 1 TO MATF (F1, F2) MOVE 3 TO MATF (G1, G2).             
     MOVE C TO MATP (F1, F2) GO TO READ-TAPE.                     
SIT01.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 1 TO MATF (F1, F2) MOVE 2 TO MATF (G1, G2).             
     MOVE C TO MATP (F1, F2) GO TO READ-TAPE.                     
SIT02.                                                            
     MOVE C TO H PERFORM FIX-SUB PERFORM FIND-END.                
     IF F1 = 0 GO TO READ-TAPE.                                   
     PERFORM MOVE-SUB MOVE D TO H PERFORM FIX-SUB.                
     MOVE 2 TO MATF (G1, G2) MOVE 3 TO MATF (F1, F2).             
     MOVE D TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT03.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (G1, G2) MOVE 3 TO MATF (F1, F2).             
     MOVE D TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT10.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 1 TO MATF (G1, G2) MOVE 2 TO MATF (F1, F2).             
     MOVE D TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT11.                                                            
     MOVE D TO H I PERFORM FIX-SUB THRU MOVE-SUB.                 
     MOVE MATP (G1, G2) TO H J PERFORM FIX-SUB.                   
     MOVE MATP (F1, F2) TO K.                                     
     PERFORM REVERSE-STRING.                                      
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE C TO MATF (F1, F2) GO TO READ-TAPE.                     
SIT12.                                                            
     MOVE C TO H PERFORM FIX-SUB PERFORM FIND-END.                
     IF F1 = 0 GO TO READ-TAPE.                                   
     PERFORM MOVE-SUB MOVE D TO H PERFORM FIX-SUB.                
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE D TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT13.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE D TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT20.                                                            
     MOVE D TO H PERFORM FIX-SUB PERFORM FIND-END.                
     IF F1 = 0 GO TO READ-TAPE.                                   
     PERFORM MOVE-SUB MOVE C TO H PERFORM FIX-SUB.                
     MOVE 2 TO MATF (G1, G2) MOVE 3 TO MATF (F1, F2).             
     MOVE C TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT21.                                                            
     MOVE D TO H PERFORM FIX-SUB PERFORM FIND-END.                
     IF F1 = 0 GO TO READ-TAPE.                                   
     PERFORM MOVE-SUB MOVE C TO H PERFORM FIX-SUB.                
     MOVE 2 TO MATF (G1, G2) MATF (F1, F2).                       
     MOVE C TO MATP (G1, G2) GO TO READ-TAPE.                     
SIT22.                                                            
     MOVE C TO I MOVE 1 TO J PERFORM FIND-HEAD THRU HEAD-EXIT.    
     IF F1 = 0 GO TO READ-TAPE.                                   
     PERFORM MOVE-SUB.                                            
     MOVE D TO H PERFORM FIX-SUB PERFORM FIND-END.                
     IF F1 = 0 GO TO READ-TAPE.                                   
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE J TO MATP (F1, F2) GO TO READ-TAPE.                     
SIT23.                                                            
     GO TO SIT22.                                                 
SIT30.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (F1, F2) MOVE 3 TO MATF (G1, G2).             
     MOVE C TO MATP (F1, F2) GO TO READ-TAPE.                     
SIT31.                                                            
     MOVE C TO H PERFORM FIX-SUB THRU MOVE-SUB.                   
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE C TO MATP (F1, F2) GO TO READ-TAPE.                     
SIT32.                                                            
     MOVE C TO I MOVE 1 TO J PERFORM FIND-HEAD THRU HEAD-EXIT.    
     IF F1 = 0 GO TO READ-TAPE.                                   
     PERFORM MOVE-SUB.                                            
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE J TO MATP (F1, F2) GO TO READ-TAPE.                     
SIT33.                                                            
     MOVE C TO I MOVE 1 TO J PERFORM FIND-HEAD THRU HEAD-EXIT.    
     IF F1 = 0 GO TO READ-TAPE.                                   
     MOVE J TO C H I PERFORM FIX-SUB THRU MOVE-SUB.               
     MOVE MATP (G1, G2) TO H J PERFOX-SUB.                   
     MOVE MATP (F1, F2) TO K.                                     
     PERFORM REVERSE-STRING PERFORM MOVE-SUB.                     
     MOVE D TO H PERFORM FIX-SUB.                                 
     MOVE 2 TO MATF (F1, F2) MATF (G1, G2).                       
     MOVE C TO MATP (F1, F2) GO TO READ-TAPE.                     
OUTPUT-STRING.                                                    
     MOVE J TO H PERFORM FIX-SUB.                                 
     IF F2 = 7501 GO TO EOJ.                                      
     IF MATF (F1, F2) NOT EQUAL TO 1                              
         ADD 1 TO J GO TO OUTPUT-STRING.                          
     MOVE 1 TO I K.                                               
     PERFORM RETRIEVE-CONN THRU RET-EXIT.                         
     MOVE SIGNAL-NAME TO SR-NAME MOVE '1' TO SR-CODE.             
     WRITE TAPEOUT FROM STRING-RECORD.                            
     ADD 1 TO J ADD 1 TO SIG-NO.                                  
OUT-INPUT.                                                        
     MOVE MATP (F1, F2) TO H PERFORM FIX-SUB.                     
     MOVE 1 TO I K.                                               
     PERFORM RETRIEVE-CONN THRU RET-EXIT.                         
     MOVE '2' TO SR-CODE.                                         
     WRITE TAPEOUT FROM STRING-RECORD.                            
     IF MATF (F1, F2) IS EQUAL TO 3                               
         MOVE SPACE TO STRING-RECORD GO TO OUTPUT-STRING.         
     GO TO OUT-INPUT.                                             
                                                                  
GET-DIM.                                                          
     IF KDIM (B) = 0 DISPLAY 'BAD KEY  '                          
         MAKE-KEY '  ' TI-SIGNAL '  ' SPLITTER                    
         MOVE 1 TO B GO TO GET-DIM-EXIT.                          
     IF KDIM (B) IS NOT EQUAL TO MAKE-KEY                         
         ADD 1 TO B GO TO GET-DIM.                                
GET-DIM-EXIT.                                                     
     EXIT.                                                        
FIND-END.                                                         
     IF MATF (F1, F2) = 2 MOVE MATP (F1, F2) TO H                 
         PERFORM FIX-SUB GO TO FIND-END.                          
     IF MATF (F1, F2) IS NOT EQUAL TO 3                           
         DISPLAY 'END NOT A 3 = ' MATF (F1, F2) ', ' F1 ', ' F2   
         MOVE ZERO TO F1.                                         
FIX-SUB.                                                          
     IF H IS LESS THAN 7501                                       
         MOVE 1 TO F1 MOVE H TO F2.                               
     IF H IS GREATER THAN 7500 MOVE 2 TO F1                       
         SUBTRACT 7500 FROM H GIVING F2.                          
MOVE-SUB.                                                         
     MOVE F1 TO G1 MOVE F2 TO G2.                                 
REVERSE-STRING.                                                   
     IF MATF (G1, G2) = 1                                         
         MOVE 3 TO MATF (G1, G2) MOVE 0 TO MATP (G1, G2).         
     IF MATF (F1, F2) = 1 OR MATF (F1, F2) = 2                    
         MOVE I TO MATP (F1, F2) PERFORM MOVE-SUB                 
         MOVE J TO I MOVE K TO J H PERFORM FIX-SUB                
         IF MATF (F1, F2) = 3 NEXT SENTENCE                       
             ELSE MOVE MATP (F1, F2) TO K GO TO REVERSE-STRING.   
     MOVE I TO MATP (F1, F2) MOVE 1 TO MATF (F1, F2).             
FIND-HEAD.                                                        
     MOVE J TO H PERFORM FIX-SUB.                                 
     IF F2 = 7501 DISPLAY 'COULD NOT FIND HEAD-OF-STRING'         
         DISPLAY TAPEIN MOVE 0 TO F1 GO TO HEAD-EXIT.             
     IF I = MATP (F1, F2) NEXT SENTENCE                           
         ELSE ADD 1 TO J GO TO FIND-HEAD.                         
     IF 1 = MATF (F1, F2) NEXT SENTENCE                           
         ELSE MOVE 1 TO J MOVE MATP (F1, F2) TO I                 
         GO TO FIND-HEAD.                                         
HEAD-EXIT.                                                        
     EXIT.                                                        
                                                                  
RETRIEVE-CONN.                                                    
     MOVE KDIM12 (I) TO CONN-OD MOVE 1 TO K.                      
     MOVE KDIM3 (I) TO BLOCK-NU ADD 1 TO I.                       
RET-CONN.                                                         
     IF KDIM3 (I) = BLOCK-NU ADD 1 TO I GO TO RET-CONN.           
     SUBTRACT 1 FROM I GIVING INDEXH (K).                         
     ADD 1 TO K MOVE KDIM3 (I) TO BLOCK-NU.                       
     IF KDIM12 (I) = CONN-OD ADD 1 TO I GO TO RET-CONN.           
     MOVE INDEXH (1) TO K MOVE SDIM (K) TO L.                     
     MOVE INDEXH (2) TO K ADD SDIM (K) TO L.                      
     MOVE INDEXH (3) TO K ADD SDIM (K) TO L.                      
     IF H IS GREATER THAN L                                       
         IF A = I DISPLAY 'CAN NOT CONVERT ' H                    
             MOVE 0 TO I GO TO RET-EXIT                           
             ELSE GO TO RETRIEVE-CONN.                            
     MOVE INDEXH (2) TO K.                                        
BLOCK-LOOP.                                                       
     IF H IS GREATER THAN SDIM (K) MOVE KDIM12 (K) TO CONN-ID     
         MOVE KDIM4 (K) TO BLOCK-NO SUBTRACT SDIM (K) FROM H      
         MOVE INDEXH (3) TO K GO TO CONN-LOOP                     
         ELSE SUBTRACT 1 FROM K GO TO BLOCK-LOOP.                 
CONN-LOOP.                                                        
     IF H IS GREATER THAN SDIM (K) MOVE KDIM45 (K) TO CONN-NO     
         SUBTRACT SDIM (K) FROM H MOVE INDEXH (1) TO K            
         GO TO PIN-LOOP                                           
         ELSE SUBTRACT 1 FROM K GO TO CONN-LOOP.                  
PIN-LOOP.                                                         
     IF H IS NOT EQUAL TO SDIM (K)                                
         SUBTRACT 1 FROM K GO TO PIN-LOOP.                        
     MOVE KDIM46 (K) TO PIN-NO MOVE '-' TO DASH-CON.              
     MOVE SPLITTER TO SR-CONN.                                    
RET-EXIT.                                                         
     EXIT.                                                        
EOJ.                                                              
     CLOSE CARD-IN TAPE-IN TAPE-OUT.                              
     STOP RUN.                                                    
    @?W