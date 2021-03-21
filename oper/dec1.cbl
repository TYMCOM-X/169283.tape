IDENTIFICATION  DIVISION.                                         
PROGRAM-ID.                                                       
     A050.                                                        
AUTHOR.                                                           
DATE-WRITTEN.                                                     
     SEPTEMBER 15,1970.                                           
DATE-COMPILED.   11/16/72.                                        
REMARKS.                                                          
     CREATES MAG TEST FILE AS I/P TO ANY PROGRAM                  
     USING MAG MASTER FILE. FILE CONTAINS                         
     25 FOREIGN ACCTS                                             
     ENTIRE STATE OF CALIFORNIA                                   
     ENTIRE STATE OF MASS                                         
     CONVERTED TO ANS BY M. ORBAN 11/10/72.                       
ENVIRONMENT  DIVISION.                                            
CONFIGURATION  SECTION.                                           
SOURCE-COMPUTER. PDP-10.                                          
OBJECT-COMPUTER. PDP-10.                                          
INPUT-OUTPUT  SECTION.                                            
FILE-CONTROL.                                                     
     SELECT  MASTER-FILE                                          
         ASSIGN DSK,                                              
      RECORDING MODE IS ASCII.                                    
                                                                  
     SELECT NEW-MASTER-FILE ASSIGN DSK,                           
      RECORDING MODE IS ASCII.                                    
                                                                  
DATA  DIVISION.                                                   
FILE  SECTION.                                                    
FD   MASTER-FILE                                                  
         VALUE OF IDENTIFICATION IS A050                          
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                   "C"                            
                                   "*"                            
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                   "C"                            
                                   "*"                            
                                                                  
                                                                  
                                                                  
                                   "     "                        
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
                                                                  
     LABEL RECORDS ARE STANDARD                                   
     BLOCK CONTAINS 1364 CHARACTERS                               
     DATA RECORDS ARE N-MAST-SCH, N-MAST-TCH.                     
01   N-MAST-TCH                                                   
     02  FILLER                  PICTURE X(164).                  
01   N-MAST-SCH                                                   
     02  FILLER                  PICTURE X(109).                  
WORKING-STORAGE  SECTION.                                         
77   FOR-CTR     PICTURE S99     COMPUTATIONAL   VALUE ZEROS SYNC.
77   SAVE-ZIP                PICTURE X(5)  VALUE SPACES.          
77   SAVE-SCH                PICTURE XXX VALUE SPACES.            
77   FIRST-REC                   PICTURE X   VALUE "Y".           
     88  FIRST-RECORD        VALUE "Y".                           
77   MSG1    PICTURE X(46)   VALUE                                
         "01.1ST RECORD NOT A SCHOOL ON MAG MASTER FILE.".        
01   COUNTERS    COMPUTATIONAL                                    
     02  RECORDS-IN              PICTURE S9(12)  VALUE ZEROS.     
     02  SCHLS-IN                PICTURE S9(12)  VALUE ZEROS.     
     02  SELECTED-SCHLS          PICTURE S9(12)  VALUE ZEROS.     
     02  NOT-SELECTED-SCHLS      PICTURE S9(12)  VALUE ZEROS.     
     02  TCHRS-IN                PICTURE S9(12)  VALUE ZEROS.     
     02  SELECTED-TCHRS          PICTURE S9(12)  VALUE ZEROS.     
     02  NOT-SELECTED-TCHRS      PICTURE S9(12)  VALUE ZEROS.     
01   CONSOLE-OUTPUT-AREA                                          
     02  FILLER      PICTURE X(10)           VALUE ".A050    .".  
     02  CONSOLE-MSG PICTURE X(60)           VALUE SPACES.        
01   TOTAL-LINE                                                   
     02  CC                      PICTURE X   VALUE SPACES.        
     02  FILLER                  PICTURE X(9)    VALUE SPACES.    
     02  TOT-NAME                PICTURE X(30)   VALUE SPACES.    
     02  FILLER                  PICTURE XX      VALUE SPACES.    
     02  TOT-EDIT                PICTURE ZZZ,ZZZ,ZZZ,ZZ9.         
     02  FILLER                  PICTURE X(76)   VALUE SPACES.    
PROCEDURE  DIVISION.                                              
BEGIN-JOB.                                                        
     OPEN INPUT  MASTER-FILE                                      
          OUTPUT NEW-MASTER-FILE.                                 
     MOVE SPACES TO TOTAL-LINE.                                   
READ-MASTER.                                                      
     READ MASTER-FILE AT END GO TO END-OF-JOB.                    
     ADD 1 TO RECORDS-IN.                                         
START-WITH-SCHOOL.                                                
     IF FIRST-RECORD AND NOT SCH-RECORD                           
     MOVE MSG1 TO CONSOLE-MSG                                     
     PERFORM CALL-CONSPOOL                                        
         PERFORM READ-MASTER UNTIL SCH-RECORD.                    
         MOVE "N" TO FIRST-REC.                                   
TEST-FOR-SCHOOL-RECORD.                                           
     IF SCH-RECORD NEXT SENTENCE ELSE                             
         GO TO TEST-TCHR.                                         
     ADD 1 TO SCHLS-IN.                                           
TEST-FORG-SCHLS.                                                  
     IF ZIP-FOR-S NEXT SENTENCE ELSE GO TO TEST-FOR-STATES.       
     ADD 1 TO FOR-CTR.                                            
     IF FOR-CTR IS < 25                                           
         ADD 1 TO NOT-SELECTED-SCHLS                              
         GO TO READ-MASTER.                                       
     GO TO SAVE-ID.                                               
TEST-FOR-STATES.                                                  
     IF STATE-ALPHA IS EQUAL TO "CA" OR                           
        STATE-ALPHA IS EQUAL TO "MA"                              
         NEXT SENTENCE  ELSE                                      
             ADD 1 TO NOT-SELECTED-SCHLS                          
             GO TO READ-MASTER.                                   
SAVE-ID.                                                          
     MOVE ZIP-CODE-S TO SAVE-ZIP.                                 
     MOVE SCH-NUMBER-S TO SAVE-SCH.                               
WRITE-SCHOOL.                                                     
     WRITE N-MAST-SCH FROM SCHOOL-RECORD.                         
     MOVE SPACES TO N-MAST-SCH.                                   
     ADD 1 TO SELECTED-SCHLS.                                     
     GO TO READ-MASTER.                                           
TEST-TCHR.                                                        
     ADD 1 TO TCHRS-IN.                                           
     IF ZIP-CODE-T IS EQUAL TO SAVE-ZIP AND                       
         SCH-NUMBER IS EQUAL TO SAVE-SCH NEXT SENTENCE ELSE       
             ADD 1 TO NOT-SELECTED-TCHRS                          
         GO TO READ-MASTER.                                       
WRITE-TEACHER.                                                    
     WRITE N-MAST-TCH FROM TEACHER-RECORD.                        
     MOVE SPACES TO N-MAST-TCH.                                   
     ADD 1 TO SELECTED-TCHRS.                                     
     GO TO READ-MASTER.                                           
W-REC.                                                            
     CALL "PRTSPOOL" USING TOTAL-LINE.                            
     MOVE SPACES TO TOTAL-LINE.                                   
CALL-CONSPOOL.                                                    
     CALL "CONSPOOL" USING CONSOLE-OUTPUT-AREA.                   
END-OF-JOB.                                                       
     MOVE "MAG TEST FILE    CREATE TOTALS" TO TOT-NAME.           
     MOVE "1" TO CC.                                              
     PERFORM W-REC.                                               
     MOVE "-" TO CC.                                              
     MOVE "TOTAL RECORDS IN " TO TOT-NAME.                        
     MOVE RECORDS-IN TO TOT-EDIT.                                 
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     MOVE "TOTAL SCHOOLS IN " TO TOT-NAME.                        
     MOVE SCHLS-IN TO TOT-EDIT.                                   
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     MOVE "TOTAL SCHOOLS NOT SELECTED" TO TOT-NAME.               
     MOVE NOT-SELECTED-SCHLS TO TOT-EDIT.                         
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     MOVE "TOTAL SCHOOLS SELECTED" TO TOT-NAME.                   
     MOVE SELECTED-SCHLS TO TOT-EDIT.                             
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     MOVE "TOTAL TCHRS IN" TO TOT-NAME.                           
     MOVE TCHRS-IN TO TOT-EDIT.                                   
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     MOVE "TOTAL TCHRS   SELECTED" TO TOT-NAME.                   
     MOVE SELECTED-TCHRS TO TOT-EDIT.                             
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     MOVE "TOTAL TCHRS  NOT SELECTED" TO TOT-NAME.                
     MOVE NOT-SELECTED-TCHRS TO TOT-EDIT.                         
     PERFORM W-REC.                                               
ABNORM-EOJ.                                                       
     CLOSE MASTER-FILE  NEW-MASTER-FILE.                          
     CALL "UNITCLOS".                                             
     STOP RUN.                                                    
