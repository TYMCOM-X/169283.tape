 IDENTIFICATION DIVISION.                                        
 PROGRAM-ID. S0CMR080.                                           
 AUTHOR. S GOGELA.                                               
 ENVIRONMENT DIVISION.                                           
 CONFIGURATION SECTION.                                          
 INPUT-OUTPUT SECTION.                                           
 FILE-CONTROL.                                                   
     SELECT FIL70 ASSIGN TO DSK RECORDING MODE IS ASCII. 
     SELECT FIL80 ASSIGN TO DSK RECORDING MODE IS ASCII. 
 DATA DIVISION.                                                  
 FILE SECTION.                                                   
 FD  FIL70                                                     
     VALUE OF IDENTIFICATION IS 'FIL70 DAT'. 
 01  IN-REC . 
     02 RQ-NO    PICTURE 9(2).                                   
     02 FILLER   PICTURE X(2).                                   
     02 MAT-NO   PICTURE X(6).                                   
     02 N-E-P    PICTURE X(7).                                   
     02 MAT-NAME PICTURE X(44).                                  
     02 VENDOR   PICTURE X(6).                                   
     02 VEN-NO   PICTURE X(6).                                   
     02 N-E-S    PICTURE X(5).                                   
     02 PRICE    PICTURE X(4).                                   
     02 INFO     PICTURE 9(3).                                   
     02 ZP-INFO    PICTURE 9(2).                                 
     02 LPX1   PICTURE 9(2).                                     
     02 LPX2   PICTURE 9(2).                                     
     02 ZX1   PIC 9(2).                                          
     02 ZX2   PIC 9(2).                                          
     02 ZX3   PIC 9(2).                                          
     02 ZX4   PIC 9(2).                                          
     02 ZX5   PIC 9(2).                                          
     02 ZX6   PIC 9(2).                                          
 FD  FIL80                                                     
     VALUE OF IDENTIFICATION IS 'FIL80 DAT'. 
 01  PRINT . 
     02 C    PICTURE X.                                          
     02 FILLER PICTURE X(2).                                     
     02 MTN-PR PICTURE X(6).                                     
     02 FILLER PICTURE X(3).                                     
     02 NEPN-PR PICTURE X(7).                                    
     02 FILLER  PICTURE X(4).                                    
     02 TEXT-PR PICTURE X(44).                                   
     02 FILLER  PICTURE X(3).                                    
     02 VEN-PR  PICTURE X(6).                                    
     02 FILLER   PICTURE X(3).                                   
     02 VEN-NO-PR  PICTURE X(6).                                 
     02 FILLER     PICTURE X(3).                                 
     02 NESN-PR    PICTURE X(5).                                 
     02 FILLER     PICTURE X(3).                                 
     02 PRICE-PR   PICTURE X(4).                                 
     02 FILLER     PICTURE X(5).                                 
     02 LPXZ1    PICTURE ZZ.                                     
     02 ASTER    PICTURE X.                                      
     02 LPXZ2    PICTURE ZZ.                                     
     02 INF-Z-O    PICTURE X(5).                                 
     02 ZVO1   PICTURE ZZ.                                       
     02 CAR1       PIC X.                                        
     02 ZVO2   PICTURE ZZ.                                       
     02 CAR2       PIC X.                                        
     02 ZVO3   PICTURE ZZ.                                       
     02 CAR3       PIC X.                                        
     02 ZVO4   PICTURE ZZ.                                       
     02 CAR4       PIC X.                                        
     02 ZVO5   PICTURE ZZ.                                       
     02 CAR5       PIC X.                                        
     02 ZVO6   PICTURE ZZ.                                       
     02 CAR6       PIC X.                                        
 WORKING-STORAGE SECTION.                                        
 77  L-C         PICTURE 99 VALUE 1.                             
 77  P-C         PICTURE 999 VALUE 1.                            
 77  X           PICTURE X VALUE ZERO.                           
 77  Y           PICTURE X VALUE ZERO.                           
 77  COUNT-XYZ    PICTURE 99 VALUE 0.                            
 01  H1 . 
     02 C1     PICTURE X VALUE '1'.                              
     02 FILLER PICTURE X(8) VALUE '   ***  '.                    
     02 DATO   PICTURE X(12).                                    
     02 FILLER PICTURE X(80) VALUE '  *  C O M P U T E R I Z E D 
-    ' M A T E R I A L   S E L E C T I O N   S Y S T E M'.       
     02 FILLER PICTURE X(25) VALUE '  *  PLASTICS  ***  PAGE '.  
     02 PAGE--X   PICTURE ZZZZ.                                  
     02 FILLER PICTURE X(3) VALUE SPACES.                        
 01  H2 . 
     02 C2       PICTURE X VALUE ' '.                            
     02 FILLER   PICTURE X(25) VALUE SPACES.                     
     02 FILLER   PICTURE X(75) VALUE '- - - - - - - - - - - -   -
-    '- - - - - - -   - - - - - - - - -   - - - - - -'.          
     02 FILLER   PICTURE X(32) VALUE SPACES.                     
 01  H3 . 
     02 C3       PICTURE X VALUE '-'.                            
     02 FILLER   PICTURE X(12) VALUE 'REQUEST NO. '.             
     02 RQ-N     PICTURE ZZ.                                     
     02 FILLER   PICTURE X(118) VALUE SPACES.                    
 01  H4 . 
     02 C4       PICTURE X VALUE ' '.                            
     02 FILLER   PICTURE X(14) VALUE '--------------'.           
     02 FILLER   PICTURE X(118) VALUE SPACES.                    
 01  H5 . 
     02 C5       PICTURE X VALUE ' '.                            
     02 FILLER PICTURE X(99) VALUE '  MAT.NO  PART NO.    MATERIAL
-    ' NAME                                  VENDOR   VEN.NO  SPEC
-    '.NO PRICE'.                                                
     02 FILLER    PICTURE X(33) VALUE '   NOT SAT.PR.   PROP.ON M
-    ',VAL=0'.                                                   
 01  H6 . 
     02 C6     PICTURE X VALUE ' '.                              
     02 FILLER PICTURE X(99) VALUE '------------------------------
-    '------------------------------------------------------------
-    '---------'.                                                
     02 FILLER PICTURE X(13) VALUE '-------------'.              
     02 FILLER PICTURE X(20) VALUE '--------------------'.       
 01  H7 . 
     02 C7     PICTURE X VALUE '-'.                              
     02 FILLER PICTURE X(64) VALUE '         ***  NO SATISFYING M
-    'TERIAL FOUND FOR THIS REQUEST  ***'.                       
     02 FILLER PICTURE X(68) VALUE SPACES.                       
 01  H8 . 
     02 C8     PICTURE X VALUE ' '.                              
     02 FILLER     PICTURE X(22) VALUE 'SATISFYING MATERIALS :'. 
     02 FILLER   PICTURE X(110) VALUE SPACES.                    
01   SF-SAVE . 
     02 RQ-NO-S    PICTURE 9(2).                                 
     02 FILLER     PICTURE X(101) VALUE SPACES.                  
 PROCEDURE DIVISION.                                             
 ST-PR.                                                          
     OPEN  INPUT  FIL70                                        
     OPEN  OUTPUT FIL80.                                       
     MOVE TODAY TO DATO.                                  
 READ-TPSF.                                                      
     READ FIL70 AT END GO TO ETSF.                             
     IF X = 0 MOVE 1 TO X MOVE IN-REC TO SF-SAVE                 
     MOVE RQ-NO TO RQ-N PERFORM HEADERS MOVE ZERO TO Y           
     GO TO READ-TPSF.                                            
     IF RQ-NO NOT = RQ-NO-S GO TO ST1.                           
     IF Y = 0 PERFORM SEC-HEAD MOVE 1 TO Y.                      
     IF L-C > 25 PERFORM HEADERS PERFORM SEC-HEAD.               
     MOVE SPACES TO PRINT.                                       
     MOVE '0' TO C.                                              
     MOVE MAT-NO TO MTN-PR MOVE N-E-P TO NEPN-PR                 
     MOVE MAT-NAME TO TEXT-PR MOVE VENDOR TO VEN-PR              
     MOVE VEN-NO TO VEN-NO-PR MOVE N-E-S TO NESN-PR              
     MOVE PRICE TO PRICE-PR.                                     
     MOVE LPX1 TO LPXZ1.                                         
     MOVE LPX2 TO LPXZ2.                                         
     IF LPX2 NOT = 0 MOVE ','  TO ASTER.                         
     IF ZX1 = 0 GO TO PXG1.                                      
     MOVE ZX1 TO ZVO1.                                           
     IF ZX2 = 0 GO TO PXG1.                                      
     MOVE ZX2 TO ZVO2 MOVE ',' TO CAR1.                          
     IF ZX3 = 0 GO TO PXG1.                                      
     MOVE ZX3 TO ZVO3 MOVE ',' TO CAR2.                          
     IF ZX4 = 0 GO TO PXG1.                                      
     MOVE ZX4 TO ZVO4 MOVE ',' TO CAR3.                          
     IF ZX5 = 0 GO TO PXG1.                                      
     MOVE ZX5 TO ZVO5 MOVE ',' TO CAR4.                          
     IF ZX6 = 0 GO TO PXG1.                                      
     MOVE ZX6 TO ZVO6 MOVE ',' TO CAR5.                          
     IF ZP-INFO > 6 MOVE '*' TO CAR6.                            
 PXG1.                                                           
     WRITE PRINT. 
     ADD 1 TO L-C.                                               
     GO TO READ-TPSF.                                            
 HEADERS.                                                        
     MOVE P-C TO PAGE--X.                                        
     WRITE PRINT FROM H1. 
     WRITE PRINT FROM H2. 
     WRITE PRINT FROM H3. 
     WRITE PRINT FROM H4. 
     MOVE 1 TO L-C, ADD 1 TO P-C.                                
 SEC-HEAD.                                                       
     WRITE PRINT FROM H8. 
     WRITE PRINT FROM H6. 
     WRITE PRINT FROM H5. 
     WRITE PRINT FROM H6. 
 ST1.                                                            
     IF Y = 0 WRITE PRINT FROM H7. 
     MOVE IN-REC TO SF-SAVE.                                     
     MOVE RQ-NO TO RQ-N.                                         
     PERFORM HEADERS.                                            
     MOVE ZERO TO Y.                                             
     GO TO READ-TPSF.                                            
 ETSF.                                                           
     IF Y = 0 WRITE PRINT FROM H7. 
     CLOSE        FIL70                                        
     CLOSE        FIL80.                                       
     STOP RUN.                                                     
  
