 IDENTIFICATION DIVISION.                                        
 PROGRAM-ID. S0CMR050.                                           
 AUTHOR. S GOGELA.                                               
 ENVIRONMENT DIVISION.                                           
 CONFIGURATION SECTION.                                          
 INPUT-OUTPUT SECTION.                                           
 FILE-CONTROL.                                                   
     SELECT AMFTG ASSIGN TO DSK
         RECORDING MODE IS ASCII. 
     SELECT PDAMF ASSIGN TO DSK
          RECORDING MODE IS ASCII
          FILE LIMIT IS 534. 
     SELECT FILEO ASSIGN TO DSK
         RECORDING MODE IS ASCII. 
 DATA DIVISION.                                                  
 FILE SECTION.                                                   
 FD  AMFTG                                                         
     VALUE OF IDENTIFICATION IS 'AMFTG DAT'. 
 01  I1R .                                                   
     02 R-NO   PICTURE 9(2).                                     
     02 FILLER    PICTURE X(3).                                  
     02 RQPR OCCURS 5 TIMES.                                     
       04 PR-NO    PICTURE 9(2).                                 
       04 REC-MOV.                                               
         06 VAL    PICTURE 9(6)V9(4).                            
         06 D      PICTURE 9(2).                                 
         06 R      PICTURE X.                                    
 FD  PDAMF                                                         
     VALUE OF IDENTIFICATION IS 'PDAMF DAT'. 
 01  I2R . 
     02 IND     PICTURE X(2).                                    
     02 RC1.                                                     
      03 FILLER    PICTURE X(43).                                
      03 PNO-TXT.                                                
       04 PN    PICTURE 9(2).                                    
       04 TXT   PICTURE X(22).                                   
     02 RC2 REDEFINES RC1.                                       
      03 FILLER     PICTURE X(42).                               
      03 DESC       PICTURE X(25).                               
     02 FILLER    PICTURE X(11).                                 
 FD  FILEO                                                       
     VALUE OF IDENTIFICATION IS 'FILEO DAT'. 
 01  PRINT .                                                 
     02 C PICTURE X.                                             
     02 PR-AREA   PICTURE X(132).                                
 WORKING-STORAGE SECTION.                                        
 77  L-C        PICTURE 99 VALUE 60.                             
 77  P-C        PICTURE 9(5) VALUE 0.                            
 77  SW1        PICTURE 9 VALUE 0.                               
 77  SW2        PICTURE 9 VALUE 0.                               
 77  SW3        PICTURE 9 VALUE 0.                               
 77  CN1   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  CN2   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  CN3   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  CN4   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  CN5   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  CN6   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  CN7   PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  X1    PICTURE S9(3) COMPUTATIONAL VALUE 0 SYNC.             
 77  R-SX    PICTURE 9(2) VALUE 0.                               
 01  H1 .                                                    
     02 C1    PICTURE X VALUE '1'.                               
     02 FILLER PICTURE X(80) VALUE 'C O M P U T E R I Z E D   M A
-    'T E R I A L   S E L E C T I O N   S Y S T E M    -'.       
     02 FILLER PICTURE X(52) VALUE '--    REQUESTED PROPERTIES   
-    '---    LISTING    ----'.                                   
 01  H2 .                                                    
     02 C2   PICTURE X VALUE '0'.                                
     02 FILLER PICTURE X(7) VALUE 'DATE : '.                     
     02 DATO   PICTURE X(12).                                    
     02 FILLER   PICTURE X(101) VALUE SPACES.                    
     02 FILLER  PICTURE X(7) VALUE 'PAGE   '.                    
     02 PAGE--X  PICTURE Z(5).                                   
 01  H3 .                                                    
     02 C3   PICTURE X VALUE '-'.                                
     02 FILLER  PICTURE X(12) VALUE 'REQUEST NO. '.              
     02 REQ-O   PICTURE ZZ.                                      
     02 FILLER  PICTURE X(118) VALUE SPACES.                     
 01  H4 .                                                    
     02 C4  PICTURE X VALUE ' '.                                 
     02 FILLER  PICTURE X(14) VALUE '--------------'.            
     02 FILLER  PICTURE X(118) VALUE SPACES.                     
 01  HL .                                                    
     02 HC    OCCURS 2 TIMES.                                    
      03 CL   PICTURE X.                                         
      03 TL   PICTURE X(24).                                     
      03 FILLER   PICTURE X.                                     
      03 DASH   PICTURE X(3).                                    
      03 FILLER   PICTURE X.                                     
      03 FORM1  OCCURS 3 TIMES.                                  
       04 A1.                                                    
        05 TEXT-OUT.                                             
         06 FILLER    PICTURE X(5).                              
         06 VALUEL   PICTURE Z(6).Z(4).                          
         06 FILLER   PICTURE X(2).                               
         06 LG    PICTURE X(4).                                  
         06 FILLER   PICTURE X(3).                               
        05 FILLER   PICTURE X(3).                                
      03 FILLER    PICTURE X(19).                                
 01  TXLE .                                                  
     02 PROPX OCCURS 44 TIMES.                                   
      03 VAL-3 OCCURS 3 TIMES.                                   
       04 RECX.                                                  
        05 VALTX     PICTURE 9(6)V9(4).                          
        05 DTX       PICTURE 9(2).                               
        05 RTX       PICTURE X.                                  
 01  XDT .                                                   
     02 XD OCCURS 800 TIMES   PICTURE X(25).                     
 01  XTT .                                                   
     02 XTX  OCCURS 44 TIMES.                                    
        03 XT   PICTURE X(24).                                   
        03 XC   PICTURE 9(3) .                                    
 PROCEDURE DIVISION.                                             
 BEGIN.                                                          
     OPEN  INPUT  AMFTG                                             
     OPEN  INPUT  PDAMF                                             
     OPEN  OUTPUT FILEO.                                    
     MOVE SPACES TO XDT.                                         
 LOOP.                                                           
     ADD 1 TO CN1.                                               
     MOVE SPACES TO XT (CN1).                                    
     MOVE 0 TO XC (CN1).                                         
     IF CN1 NOT = 44 GO TO LOOP.                                 
     MOVE 1 TO CN1 CN4 CN5.                                      
 READ-I2.                                                        
     READ PDAMF AT END MOVE 0 TO CN1 CN2 CN3 PERFORM CL-TXLE        
        THRU E-CL-TXLE                                           
     GO TO READ-I1.                                              
     IF IND NOT = 'XX' MOVE 2 TO CN4 ADD 1 TO CN2                
     MOVE DESC TO XD (CN5) ADD 1 TO CN5 GO TO READ-I2.           
     IF CN4 = 1 GO TO LOAD-CN1.                                  
     IF CN4 NOT = 2 MOVE 0 TO XC (CN3) GO TO LOAD-CN1.           
     ADD CN2 TO CN1.                                             
 LOAD-CN1.                                                       
     MOVE CN1 TO XC (PN).                                        
     MOVE PNO-TXT TO XT (PN).                                    
     MOVE 0 TO CN2 CN4.                                          
     MOVE PN TO CN3.                                             
     GO TO READ-I2.                                              
 READ-I1.                                                        
     READ AMFTG AT END PERFORM PR-PRINT THRU E-PR-PRINT  GO TO      
       EOP--X. 
     IF SW1 = 0 MOVE 1 TO SW1 MOVE R-NO TO R-SX.                 
     IF R-SX NOT = R-NO PERFORM PR-PRINT THRU E-PR-PRINT         
     MOVE R-NO TO R-SX PERFORM CL-TXLE THRU E-CL-TXLE.           
     PERFORM LOAD-TXLE THRU E-LOAD-TXLE.                         
     GO TO READ-I1.                                              
 PR-PRINT.                                                       
     PERFORM HEADERS.                                            
 LOOP1.                                                          
     ADD 1 TO CN1.                                               
     IF CN1 > 44 GO TO E-PR-PRINT.                               
     IF VALTX (CN1, 1) = 0 AND DTX (CN1, 1) = 0 GO TO LOOP1.     
     MOVE SPACES TO HL.                                          
 LOOP2.                                                          
     ADD 1 TO CN2.                                               
     IF CN2 = 1                                                  
     MOVE XT (CN1) TO TL (1)                                     
     MOVE '---' TO DASH (1).                                     
     IF CN2 > 3 MOVE 0 TO CN2 PERFORM WRXOUT GO TO LOOP1.        
     IF VALTX (CN1, CN2) = 0 AND                                 
          DTX (CN1, CN2) = 0 MOVE 0 TO CN2 PERFORM WRXOUT        
     GO TO LOOP1.                                                
     IF VALTX (CN1, CN2) = 0 PERFORM LOAD-DESC  ELSE             
     PERFORM LOAD-VALUE.                                         
     GO TO LOOP2.                                                
 E-PR-PRINT.                                                     
     MOVE 0 TO CN1 CN2 CN3.                                      
 LOAD-DESC.                                                      
     COMPUTE CN3 = XC (CN1) + DTX (CN1, CN2) - 1.                
     MOVE XD (CN3) TO TEXT-OUT (1, CN2).                         
 LOAD-VALUE.                                                     
     MOVE VALTX (CN1, CN2) TO VALUEL (1, CN2).                   
     IF RTX (CN1, CN2) = '1'                                     
     MOVE 'OR >' TO LG (1, CN2).                                 
     IF RTX (CN1, CN2) = '2'                                     
     MOVE 'OR <' TO LG (1, CN2).                                 
     IF DTX (CN1, CN2) NOT = 0                                   
     COMPUTE CN3 = XC (CN1) + DTX (CN1, CN2) - 1                 
     MOVE XD (CN3) TO TEXT-OUT (2, CN2).                         
 HEADERS.                                                        
     ADD 1 TO P-C.                                               
     MOVE P-C TO PAGE--X.                                        
     MOVE TODAY TO DATO.                                  
     MOVE R-SX TO REQ-O.                                         
     WRITE PRINT FROM H1. 
     WRITE PRINT FROM H2. 
     WRITE PRINT FROM H3. 
     WRITE PRINT FROM H4. 
     MOVE SPACES TO PRINT.                                       
     WRITE PRINT. 
     MOVE 0 TO L-C.                                              
 LOAD-TXLE.                                                      
     ADD 1 TO CN1.                                               
     IF CN1 > 5 GO TO E-LOAD-TXLE.                               
     IF VAL (CN1) = 0 AND D (CN1) = 0 GO TO LOAD-TXLE.           
     MOVE PR-NO (CN1) TO X1.                                     
     IF VALTX (X1, 1) = 0 AND DTX (X1, 1) = 0                    
     MOVE REC-MOV (CN1) TO RECX (X1, 1) GO TO LOAD-TXLE.         
     IF VALTX (X1, 2) = 0 AND DTX (X1, 2) = 0                    
     MOVE REC-MOV (CN1) TO RECX (X1, 2) GO TO LOAD-TXLE.         
     MOVE REC-MOV (CN1) TO RECX (X1, 3) GO TO LOAD-TXLE.         
 E-LOAD-TXLE.                                                    
     MOVE 0 TO CN1.                                              
 CL-TXLE.                                                        
     ADD 1 TO CN2.                                               
     IF CN2 > 44 GO TO E-CL-TXLE.                                
 IN-LOP.                                                         
     ADD 1 TO CN3.                                               
     IF CN3 > 3 MOVE 0 TO CN3 GO TO CL-TXLE.                     
     MOVE 0 TO VALTX (CN2, CN3).                                 
     MOVE 0 TO DTX   (CN2, CN3).                                 
     MOVE '0' TO RTX (CN2, CN3).                                 
     GO TO IN-LOP.                                               
 E-CL-TXLE.                                                      
     MOVE 0 TO CN2 CN3.                                          
 WRXOUT.                                                         
     MOVE HC (1) TO PRINT.                                       
     WRITE PRINT. 
     IF HC (2) NOT = SPACES MOVE HC (2) TO PRINT                 
     WRITE PRINT. 
 EOP--X.                                                         
     CLOSE        AMFTG                                             
     CLOSE        PDAMF                                             
     CLOSE        FILEO.                                    
     STOP RUN. 
