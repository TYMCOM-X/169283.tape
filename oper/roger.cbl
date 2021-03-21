IDENTIFICATION DIVISION.                                                        
PROGRAM-ID. ROGER.                                                              
AUTHOR. JOLLY ROGER.                                                            
DATE-WRITTEN. 31DEC70.                                                          
                                                                                
ENVIRONMENT DIVISION.                                                           
CONFIGURATION SECTION.                                                          
SPECIAL-NAMES.                                                                  
    CHANNEL (1) IS TOP-OF-FORM.                                                 
                                                                                
INPUT-OUTPUT SECTION.                                                           
FILE-CONTROL.                                                                   
    SELECT IN-FILE              ASSIGN TO DSK
        RECORDING MODE IS ASCII.
    SELECT L-PRINT              ASSIGN TO DSK.                                  
                                                                                
DATA DIVISION.                                                                  
FILE SECTION.                                                                   
                                                                                
FD  IN-FILE                                                                     
    LABEL RECORDS ARE STANDARD                                                  
    VALUE OF IDENTIFICATION IS "CARDS    ".                                     
                                                                                
 01 INPUT-RECORD.
       02 CUSTOMER-NAME PIC X(30).
       02 CITY PIC X(20).
       02 STATE PIC XX.
       02 TOTAL-SALES PIC S9(9)V99.
                                                                                
FD  L-PRINT                                                                     
    LABEL RECORDS ARE STANDARD                                                  
    VALUE OF IDENTIFICATION IS "REPOR    ".                                     
                                                                                
01  PRINT-LINE; DISPLAY-7.                                                      
    02 FILLER                   PICTURE X(65).                                  
    02 HEADER-PAGE              PICTURE ZZ9.                                    
    02 FILLER                   PICTURE X(64).                                  
                                                                                
WORKING-STORAGE SECTION.                                                        
                                                                                
77  PAGE-COUNT                  PICTURE S9(3);  COMPUTATIONAL.                  
77  LINE-COUNT                  PICTURE S99;    COMPUTATIONAL.                  
                                                                                
77  SAVE-01                     PICTURE XX.                                     
77  SAVE-02                     PICTURE X(20).                                  
                                                                                
01  OUTPUT-LINE; DISPLAY-7.                                                     
       02 PRINT-CUSTOMER.
         03 FILLER PIC X(19).
         03 TITLE-1 PIC X(11).
       02 FILLER PIC XX.
       02 PRINT-CITY PIC X(20).
       02 FILLER PIC XX.
       02 PRINT-STATE PIC XX.
       02 FILLER PIC XX.
       02 PRINT-SALES PIC ZZZ,ZZZ,ZZZ,ZZZ,99-.
       02 FILLER PIC XX.
       02 PRINT-NUM PIC Z,ZZ9.
                                                                                
                                                                                
01  LEVEL-0-ACS; COMPUTATIONAL.                                                 
    02 ACCUMULATOR-0             PICTURE S9(12)V9(2).                           
    02 ACCUMULATOR-1             PICTURE S9(5).                                 
                                                                                
01  LEVEL-1-ACS; COMPUTATIONAL.                                                 
    02 ACCUMULATOR-0             PICTURE S9(12)V9(2).                           
    02 ACCUMULATOR-1             PICTURE S9(5).                                 
                                                                                
01  LEVEL-F-ACS; COMPUTATIONAL.                                                 
    02 ACCUMULATOR-0             PICTURE S9(12)V9(2).                           
    02 ACCUMULATOR-1             PICTURE S9(5).                                 
                                                                                
01  HEADER.                                                                     
    02 FILL PIC X(30); VALUE "CUSTOMER   CITY    STATE    SA".                  
    02 FILL PIC X(30); VALUE "LES                           ".                  
    02 FILL PIC X(33); VALUE "PAGE                             ".               
    02 FILL PIC X(33); VALUE "                                 ".               
                                                                                
PROCEDURE DIVISION.                                                             
                                                                                
ONLY SECTION.                                                                   
START.                                                                          
    OPEN INPUT IN-FILE.                                                         
    OPEN OUTPUT L-PRINT.                                                        
    MOVE SPACES TO OUTPUT-LINE.                                                 
                                                                                
    MOVE ZERO TO PAGE-COUNT.                                                    
    PERFORM PL-HDR THRU PL-EXIT.                                                
                                                                                
READ-IN.                                                                        
    READ IN-FILE; AT END GO TO LEVEL-0-BREAK.                                   
                                                                                
INIT-SW. GO TO RESET-INIT.                                                      
        NOTE AFTER THE FIRST READ, INIT-SW WILL GO TO COMPARE.                  
RESET-INIT. ALTER INIT-SW TO PROCEED TO COMPARE.                                
    GO TO RESET-LEVEL-F.                                                        
                                                                                
COMPARE.                                                                        
    IF SAVE-01 IS NOT EQUAL TO STATE                                            
        ALTER LEVEL-1-SW TO PROCEED TO RESET-LEVEL-1;                           
        GO TO LEVEL-0-BREAK.                                                    
                                                                                
    IF SAVE-02 IS NOT EQUAL TO CITY                                             
        ALTER LEVEL-0-SW TO PROCEED TO RESET-LEVEL-0;                           
        GO TO LEVEL-0-BREAK.                                                    
                                                                                
PRINT-DETAIL.                                                                   
    MOVE CUSTOMER-NAME            TO PRINT-CUSTOMER.                            
    MOVE TOTAL-SALES              TO PRINT-SALES.                               
    MOVE CITY                     TO PRINT-CITY.                                
    MOVE STATE                    TO PRINT-STATE.                               
    MOVE TOTAL-SALES              TO PRINT-SALES.                               
                                                                                
    MOVE OUTPUT-LINE TO PRINT-LINE.                                             
    PERFORM PRINT-1 THRU PL-EXIT.                                               
    MOVE SPACES TO OUTPUT-LINE.                                                 
                                                                                
    ADD TOTAL-SALES              TO ACCUMULATOR-0 OF LEVEL-0-ACS.               
    ADD 1                        TO ACCUMULATOR-1 OF LEVEL-0-ACS.               
                                                                                
    GO TO READ-IN.                                                              
                                                                                
LEVEL-0-BREAK.                                                                  
    MOVE SPACES TO PRINT-LINE.                                                  
    PERFORM PRINT-1 THRU PL-EXIT.                                               
                                                                                
    MOVE ACCUMULATOR-0 OF LEVEL-0-ACS TO PRINT-SALES.                           
    MOVE ACCUMULATOR-1 OF LEVEL-0-ACS TO PRINT-NUM.                             
    MOVE "CITY TOTAL"             TO TITLE-1.                                   
    MOVE OUTPUT-LINE TO PRINT-LINE.                                             
    PERFORM PRINT-2 THRU PL-EXIT.                                               
    MOVE SPACES TO OUTPUT-LINE.                                                 
                                                                                
    ADD CORRESPONDING LEVEL-0-ACS TO LEVEL-1-ACS.                               
                                                                                
LEVEL-0-SW. GO TO. NOTE GO TO RESET-LEVEL-0 OR LEVEL-1-BREAK.                   
                                                                                
LEVEL-1-BREAK.                                                                  
    MOVE ACCUMULATOR-0 OF LEVEL-1-ACS TO PRINT-SALES.                           
    MOVE ACCUMULATOR-1 OF LEVEL-1-ACS TO PRINT-NUM.                             
    MOVE "STATE TOTAL"            TO TITLE-1.                                   
    MOVE OUTPUT-LINE TO PRINT-LINE.                                             
    PERFORM PRINT-CH-1 THRU PL-EXIT.                                            
    MOVE SPACES TO OUTPUT-LINE.                                                 
                                                                                
    ADD CORRESPONDING LEVEL-1-ACS TO LEVEL-F-ACS.                               
                                                                                
LEVEL-1-SW. GO TO. NOTE GO TO RESET-LEVEL-1 OR LEVEL-F-BREAK.                   
                                                                                
LEVEL-F-BREAK.                                                                  
    MOVE SPACES TO PRINT-LINE.                                                  
    PERFORM PRINT-1 THRU PL-EXIT.                                               
                                                                                
    MOVE ACCUMULATOR-0 OF LEVEL-F-ACS TO PRINT-SALES.                           
    MOVE ACCUMULATOR-1 OF LEVEL-F-ACS TO PRINT-NUM.                             
    MOVE "FINAL TOTAL"            TO TITLE-1.                                   
    MOVE OUTPUT-LINE TO PRINT-LINE.                                             
    PERFORM PRINT-CH-1 THRU PL-EXIT.                                            
    MOVE SPACES TO OUTPUT-LINE.                                                 
                                                                                
    GO TO FINISH.                                                               
                                                                                
                                                                                
RESET-LEVEL-F.                                                                  
    MOVE LOW-VALUES TO LEVEL-F-ACS.                                             
                                                                                
RESET-LEVEL-1.                                                                  
    MOVE LOW-VALUES TO LEVEL-1-ACS.                                             
    MOVE STATE                    TO SAVE-01.                                   
    ALTER LEVEL-1-SW TO PROCEED TO LEVEL-F-BREAK.                               
                                                                                
RESET-LEVEL-0.                                                                  
    MOVE LOW-VALUES TO LEVEL-0-ACS.                                             
    MOVE CITY                     TO SAVE-02.                                   
    ALTER LEVEL-0-SW TO PROCEED TO LEVEL-1-BREAK.                               
    GO TO PRINT-DETAIL.                                                         
                                                                                
                                                                                
PRINT-1. WRITE PRINT-LINE.                                                      
    SUBTRACT 1 FROM LINE-COUNT.                                                 
    IF LINE-COUNT IS POSITIVE GO TO PL-EXIT; ELSE GO TO PL-HDR.                 
                                                                                
PRINT-2. WRITE PRINT-LINE BEFORE 2 LINES;                                       
    SUBTRACT 2 FROM LINE-COUNT.                                                 
    IF LINE-COUNT IS POSITIVE GO TO PL-EXIT; ELSE GO TO PL-HDR.                 
                                                                                
PL-HDR. MOVE SPACES TO PRINT-LINE.                                              
                                                                                
PRINT-CH-1. WRITE PRINT-LINE BEFORE TOP-OF-FORM.                                
    MOVE HEADER TO PRINT-LINE.                                                  
    ADD 1 TO PAGE-COUNT; MOVE PAGE-COUNT TO HEADER-PAGE.                        
    WRITE PRINT-LINE BEFORE ADVANCING 2 LINES.                                  
    MOVE 54 TO LINE-COUNT.                                                      
                                                                                
PL-EXIT.  EXIT.                                                                 
                                                                                
                                                                                
FINISH.                                                                         
    CLOSE IN-FILE.                                                              
    CLOSE L-PRINT.                                                              
                                                                                
    STOP RUN.                                                                   
    