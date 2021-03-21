IDENTIFICATION DIVISION.                                                        
PROGRAM-ID. SBUILD.                                                             
AUTHOR. JOHN SMITH.                                                             
INSTALLATION. NAVY.                                                             
DATE-WRITTEN. 2/28/73.                                                          
DATE-COMPILED. 2/28/73.                                                         
SECURITY. NONE.                                                                 
REMARKS. TEST PROGRAM.                                                          
ENVIRONMENT DIVISION.                                                           
CONFIGURATION SECTION.                                                          
SOURCE-COMPUTER. PDP-10.                                                        
OBJECT-COMPUTER. PDP-10.                                                        
INPUT-OUTPUT SECTION.                                                           
FILE-CONTROL.                                                                   
        SELECT POP-FILE ASSIGN TO DSK                                           
        RECORDING MODE IS ASCII.                                                
        SELECT CARDS-IN-FILE ASSIGN TO DSK                                      
        RECORDING MODE IS ASCII.                                                
        SELECT CARDS-OUT-FILE ASSIGN TO DSK                                     
        RECORDING MODE IS ASCII.                                                
DATA DIVISION.                                                                  
FILE SECTION.                                                                   
FD POP-FILE                                                                     
        LABEL RECORDS ARE STANDARD                                              
        VALUE OF IDENTIFICATION IS "POPFILDAT".                                 
01 POP-RECORD.                                                                  
        02 POP-REC-COLUMN-1 PIC X.                                              
        02 FILLER           PIC X.                                              
        02 POP-REC-COLUMN-3-7  PIC XXXXX.                                       
        02 FILLER           PIC X(73).                                          
FD CARDS-IN-FILE                                                                
        LABEL RECORDS ARE STANDARD                                              
        VALUE OF IDENTIFICATION IS "CARDINDAT".                                 
01 CARDS-IN-RECORD.                                                             
        02 CARDS-IN-REC-TEST-NO  PIC X(5).                                      
        02 FILLER                PIC X(75).                                     
FD CARDS-OUT-FILE                                                               
        LABEL RECORDS ARE STANDARD                                              
        VALUE OF IDENTIFICATION IS "CARDOTDAT".                                 
01 CARDS-OUT-RECORD  PIC X(80).                                                 
WORKING-STORAGE SECTION.                                                        
77 WS-SEARCH-OR-PUNCH             PIC X   VALUE "S".                            
77 WS-TEST-NO-SAVE                PIC X(5)   VALUE "     ".                     
77 END-OF-CARDS-IN-FILE           PIC X    VALUE "N".                           
01 WS-COUNTERS.                                                                 
        02 CARDS-OUT-COUNT   PIC S9(7)  VALUE +0000000.                         
        02 CARDS-IN-COUNT    PIC S9(7)  VALUE +0000000.                         
PROCEDURE DIVISION.                                                             
 000-INITIALIZE-FILES.                                                          
        OPEN INPUT POP-FILE CARDS-IN-FILE.                                      
        OPEN OUTPUT CARDS-OUT-FILE.                                             
        PERFORM 030-READ-A-CARD THRU 030-END-READ-CARD                          
        GO TO 040-READ-A-POP-RECORD.                                            
 010-DECIDE-DT00. NOTE                   DETAP/IMI V4-1 08/25/72                
     DETAP     010-DECIDE                        00001 03 05 05                 
     RL1                                 0   0   0   0   0                      
     RL2                                 1   2   3   4   5   $                  
     CONDITION SECTION                                                          
     C POP-REC-COLUMN-1 EQUAL TO "T"     Y   Y   Y   N   N                      
     C POP-REC-COLUMN-3-7 :                                                     
       CARDS-IN-REC-TEST-NO              >   =   <   -   -                      
     * RULE 1 GREATER                                                           
     * RULE 3 LESS                                                              
     C WS-SEARCH-OR-PUNCH EQUAL TO       -   -   -   "S" "P"                    
     ACTION SECTION                                                             
     A MOVE "S" TO WS-SEARCH-OR-PUNCH            X                              
     A MOVE "P" TO WS-SEARCH-OR-PUNCH        X                                  
     A PERFORM 015-NOT-FOUND             X                                      
     A PERFORM 020-PUNCH-A-CARD              X           X                      
     A PERFORM 030-READ-A-CARD THRU                                             
               030-END-READ-CARD         X   X                                  
     A GO TO 040-READ-A-POP-RECORD           X   X   X   X                      
     A GO TO 010-DECIDE                  X                                      
     TEND.                                                                      
 010-DECIDE SECTION.                                                            
 DT00001000.                                                                    
     IF POP-REC-COLUMN-1 EQUAL TO "T" GO TO DT00001001.                         
     IF WS-SEARCH-OR-PUNCH EQUAL TO "S" GO TO AT00001004.                       
     IF WS-SEARCH-OR-PUNCH EQUAL TO "P" GO TO AT00001005                        
         ELSE GO TO EL00001001.                                                 
 DT00001001.                                                                    
     IF POP-REC-COLUMN-3-7 > CARDS-IN-REC-TEST-NO                               
         GO TO AT00001001.                                                      
     IF POP-REC-COLUMN-3-7 < CARDS-IN-REC-TEST-NO                               
         GO TO AT00001003 ELSE GO TO AT00001002.                                
 AT00001001.                                                                    
     PERFORM 015-NOT-FOUND.                                                     
     PERFORM 030-READ-A-CARD THRU 030-END-READ-CARD.                            
     GO TO 010-DECIDE.                                                          
 AT00001002.                                                                    
     MOVE "P" TO WS-SEARCH-OR-PUNCH.                                            
     PERFORM 020-PUNCH-A-CARD.                                                  
     PERFORM 030-READ-A-CARD THRU 030-END-READ-CARD.                            
     GO TO 040-READ-A-POP-RECORD.                                               
 AT00001003.                                                                    
     MOVE "S" TO WS-SEARCH-OR-PUNCH.                                            
     GO TO 040-READ-A-POP-RECORD.                                               
 AT00001004.                                                                    
     GO TO 040-READ-A-POP-RECORD.                                               
 AT00001005.                                                                    
     PERFORM 020-PUNCH-A-CARD.                                                  
     GO TO 040-READ-A-POP-RECORD.                                               
 EL00001001.                                                                    
     DISPLAY "ELSE RULE NONE SPECIFIED-TBL=010-DECIDE".                         
     STOP RUN.                                                                  
 015-NOT-FOUND.                                                                 
     DISPLAY CARDS-IN-REC-TEST-NO  " NOT FOUND, BYPASSED".                      
 020-PUNCH-A-CARD.                                                              
        MOVE POP-RECORD TO CARDS-OUT-RECORD                                     
        WRITE CARDS-OUT-RECORD                                                  
        ADD +1 TO CARDS-OUT-COUNT.                                              
 030-READ-A-CARD.                                                               
        IF  END-OF-CARDS-IN-FILE = "Y"                                          
        GO TO 050-END-OF-JOB.                                                   
        READ CARDS-IN-FILE                                                      
          AT END                                                                
        MOVE "Y" TO END-OF-CARDS-IN-FILE                                        
        GO TO 030-END-READ-CARD.                                                
        IF CARDS-IN-REC-TEST-NO NOT GREATER THAN WS-TEST-NO-SAVE                
           DISPLAY  CARDS-IN-REC-TEST-NO                                        
        " OUT OF SEQUENCE, BYPASSED"                                            
        GO TO 030-READ-A-CARD.                                                  
        ADD +1 TO CARDS-IN-COUNT                                                
        MOVE CARDS-IN-REC-TEST-NO TO WS-TEST-NO-SAVE.                           
 030-END-READ-CARD. EXIT.                                                       
 040-READ-A-POP-RECORD.                                                         
        READ POP-FILE                                                           
          AT END                                                                
        DISPLAY  "POP FILE AT END BEFORE CARDS IN"                              
        GO TO 050-END-OF-JOB.                                                   
        GO TO 010-DECIDE.                                                       
 050-END-OF-JOB.                                                                
        DISPLAY "CARDS-IN-COUNT = " CARDS-IN-COUNT.                             
        DISPLAY "CARDS-OUT-COUNT = " CARDS-OUT-COUNT.                           
        CLOSE POP-FILE CARDS-IN-FILE CARDS-OUT-FILE                             
        STOP RUN.                                                               
   