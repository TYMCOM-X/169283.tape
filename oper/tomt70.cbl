 IDENTIFICATION DIVISION.                                       
 PROGRAM-ID. S0CMR070.                                          
 AUTHOR. S GOGELA.                                              
 ENVIRONMENT DIVISION.                                          
 CONFIGURATION SECTION.                                         
 INPUT-OUTPUT SECTION.                                          
 FILE-CONTROL.                                                  
     SELECT FIL60 ASSIGN TO DSK RECORDING MODE IS ASCII. 
     SELECT FIL70 ASSIGN TO DSK RECORDING MODE IS ASCII. 
     SELECT DATA2 ASSIGN TO DSK RECORDING MODE IS ASCII. 
 DATA DIVISION.                                                 
 FILE SECTION.                                                  
 FD  FIL60                                                         
     VALUE OF IDENTIFICATION IS 'FIL60 DAT'. 
 01  IN-REC . 
     02 RQ-NO   PICTURE 9(2).                                   
     02 FILLER  PICTURE X(2).                                   
     02 MAT-NO.                                                 
       04 H4.                                                   
        06 H2   PICTURE X(2).                                   
        06 FILLER  PICTURE X(2).                                
       04 FILLER  PICTURE X(2).                                 
     02 FILLER  PICTURE X(93).                                  
 FD  FIL70                                                         
     VALUE OF IDENTIFICATION IS 'FIL70 DAT'. 
01   STO . 
     02 FILLER   PICTURE X(103).                                
 FD  DATA2                                                  
     VALUE OF IDENTIFICATION IS 'DATA2 DAT'. 
 01  CARD . 
     02 RXQ   PICTURE 9(2).                                     
     02 FILLER  PICTURE X(7).                                   
     02 INF OCCURS 3 TIMES.                                     
       03 A    PICTURE X(4).                                    
       03 FILLER   PICTURE X(2).                                
       03 A1    PICTURE X.                                      
       03 FILLER   PICTURE X.                                   
       03 SPCD     PICTURE X.                                   
       03 FILLER   PICTURE X.                                   
     02 FILLER   PICTURE X(41).                                 
 WORKING-STORAGE SECTION.                                       
 77  R          PICTURE 9(2) VALUE 0.                           
 77  J          PICTURE 9 VALUE 1.                              
 77  OKX        PICTURE 9 VALUE 0.                              
 77  OKY        PICTURE 9 VALUE 0.                              
 01  TABLE . 
     02 TRQ  OCCURS 100 TIMES.                                  
       03 TB  OCCURS 3 TIMES.                                   
         04 H.                                                  
            05 H1   PICTURE X(2).                               
            05 FILLER  PICTURE X(2).                            
         04 N   PICTURE X.                                      
         04 SPX PICTURE X.                                      
 PROCEDURE DIVISION.                                            
 BEGIN.                                                         
     OPEN  INPUT  FIL60                                            
     OPEN  INPUT  DATA2                                     
     OPEN  OUTPUT FIL70.                                           
 ZERO-TAB.                                                      
     MOVE ALL '0' TO TABLE.                                     
 READ-C.                                                        
     READ DATA2 AT END GO TO EOC.                           
     IF A1 (1) NOT = SPACES AND                                 
        A (1) NOT = SPACES MOVE A (1) TO H (RXQ, 1)             
     MOVE SPCD (1) TO SPX (RXQ, 1)                              
     MOVE A1 (1) TO  N (RXQ, 1).                                
     IF A1 (2) NOT = SPACES AND                                 
        A (2) NOT = SPACES MOVE A (2) TO H (RXQ, 2)             
     MOVE SPCD (2) TO SPX (RXQ, 2)                              
     MOVE A1 (2) TO  N (RXQ, 2).                                
     IF A1 (3) NOT = SPACES AND                                 
        A (3) NOT = SPACES MOVE A (3) TO H (RXQ, 3)             
     MOVE SPCD (3) TO SPX (RXQ, 3)                              
     MOVE A1 (3) TO  N (RXQ, 3).                                
     GO TO READ-C.                                              
 EOC.                                                           
     MOVE 0 TO R.                                               
     MOVE 0 TO OKX OKY.                                         
     MOVE 1 TO J.                                               
 READ-I.                                                        
     READ FIL60 AT END GO TO EOP--X.                               
     IF RQ-NO NOT = R MOVE RQ-NO TO R GO TO WR-REC.             
 TST-1.                                                         
     IF N (R, J) = '0' GO TO TOTST.                             
 TST-2.                                                         
     IF N (R, J) NOT = '2' GO TO ON-4.                          
     IF H1 (R, J) = H2 GO TO TX-1.                              
     IF H2 > H1 (R, J) GO TO TX-5.                              
 TX-6.                                                          
     IF SPX (R, J) = '<' GO TO TX-7.                            
 TX-8.                                                          
     IF SPX (R, J) = 'N' GO TO TX-7.                            
     IF SPX (R, J) NOT = ' ' GO TO TX-4.                        
     IF OKX = 0 GO TO TX-9.                                     
     IF OKY NOT = 0 GO TO TX-9.                                 
     ADD 1 TO J.                                                
     IF J > 3 GO TO TO-WR.                                      
     GO TO TST-1.                                               
 TX-9.                                                          
     ADD 1 TO J.                                                
     IF J > 3 GO TO TX-3.                                       
     GO TO TX-2.                                                
 TX-5.                                                          
     IF SPX (R, J) = '>' GO TO TX-7.                            
     GO TO TX-8.                                                
 TX-7.                                                          
      ADD 1 TO J.                                               
     IF J > 3 GO TO TX-10.                                      
     ADD 1 TO OKX.                                              
     GO TO TST-1.                                               
 TX-10.                                                         
     IF OKY = 0 GO TO TO-WR.                                    
     GO TO TX-3.                                                
 ON-4.                                                          
     IF H (R, J) = H4 GO TO TX-1.                               
     IF H4 > H (R, J) GO TO TX-5.                               
     GO TO TX-6.                                                
 TO-WR.                                                         
     MOVE 1 TO J.                                               
     MOVE 0 TO OKX OKY.                                         
 WR-REC.                                                        
     WRITE STO FROM IN-REC.                                     
     GO TO READ-I.                                              
 TX-1.                                                          
     IF SPX (R, J) = 'N' GO TO TX-3.                            
     IF SPX (R, J) = ' ' GO TO TO-WR.                           
     GO TO TX-4.                                                
 TX-3.                                                          
     MOVE 1 TO J.                                               
     MOVE 0 TO OKX OKY.                                         
     GO TO READ-I.                                              
 TX-4.                                                          
      ADD 1 TO OKY.                                             
      ADD 1 TO J.                                               
     IF J > 3 GO TO  TX-3.                                      
 TX-2.                                                          
     IF N (R, J) = '0' GO TO TX-3.                              
     GO TO TST-2.                                               
 TOTST.                                                         
      IF OKY = 0 GO TO TO-WR.                                   
     GO TO TX-3.                                                
 EOP--X.                                                        
     CLOSE        FIL60                                            
     CLOSE        FIL70                                            
     CLOSE        DATA2.                                    
     STOP RUN. 

