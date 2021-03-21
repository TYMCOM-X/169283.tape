IDENTIFICATION  DIVISION.                                         
PROGRAM-ID.                                                       
     ^PSC23105^.                                                  
DATE-WRITTEN.                                                     
     OCTOBER 1970.                                                
DATE-COMPILED.                                                    
     OCTOBER 1970.                                                
REMARKS.                                                          
     THIS PROGRAM IS TO COMPUTE THE PERCENTAGE OF DISTRIBUTION    
     FOR GRADES 1 THRU 12 AND THE PERCENTAGE OF GROWTH FOR        
     EACH GRADE FROM A PRIOR PERIOD MAY BE 1 THRU 99 YEARS        
     INDICATES THE NUMBER IN THE CURRENT RECORD AND THE AMOUNT    
     OF INCREASE FROM THE PRIOR PERIOD OF TIME.                   
                                                                  
ENVIRONMENT  DIVISION.                                            
CONFIGURATION  SECTION.                                           
SOURCE-COMPUTER. PDP-10.                                          
OBJECT-COMPUTER. PDP-10.                                          
INPUT-OUTPUT  SECTION.                                            
FILE-CONTROL.                                                     
                                                                  
     SELECT CARD-IN ASSIGN DSK,                                   
      RECORDING MODE IS ASCII.                                    
                                       UTILITY.                   
                                                                  
     SELECT PRINT-LINE ASSIGN DSK,                                
      RECORDING MODE IS ASCII.                                    
                                           UTILITY.               
                                                                  
DATA  DIVISION.                                                   
FILE  SECTION.                                                    
                                                                  
FD   CARD-IN                                                      
         VALUE OF IDENTIFICATION IS CARDINDAT                     
     RECORD CONTAINS 80 CHARACTERS                                
     BLOCK CONTAINS 0 RECORDS                                     
     LABEL RECORDS ARE STANDARD                                   
     DATA RECORDS ARE CARDS-IN, CARD-2.                           
                                                                  
01   CARDS-IN.                                                    
     05  ALPHA-CARD          PICTURE X]80[.                       
     05  NUM-CARD REDEFINES ALPHA-CARD.                           
         10  CARD-AMT       PICTURE S9]6[ OCCURS 13 TIMES.        
         10  CARD-YR         PICTURE X]2[.                        
         10  CARD-YR-RED REDEFINES CARD-YR  PICTURE 9]2[.         
                                                                  
01   CARD-2.                                                      
     10  CARD-IND            PICTURE X.                           
     10  FILLER              PICTURE X.                           
     10  COUNTY-NAME         PICTURE X]25[.                       
     10  FILLER              PICTURE X]53[.                       
                                                                  
FD   PRINT-LINE                                                   
         VALUE OF IDENTIFICATION IS "PRINTLDAT"                   
     RECORD CONTAINS 133 CHARACTERS                               
     BLOCK CONTAINS 0 RECORDS                                     
     LABEL RECORDS ARE STANDARD                                   
     DATA RECORD IS PRINT.                                        
                                                                  
01   PRINT.                                                       
     05  FILLER              PICTURE X.                           
     05  PRINT-1             PICTURE X]132[.                      
                                                                  
WORKING-STORAGE  SECTION.                                         
                                                                  
77   SAVE-YEAR               PICTURE X]2[    VALUE SPACES.        
77   TIME                    PICTURE 9]8[.                        
77   JDATE                   PICTURE 9]5[.                        
77   STDATE                  PICTURE 9]6[.                        
77   CARD-SUBSCRIPT          PICTURE 9]2[    VALUE ZEROS.         
77   TOTAL-STUDENTS         PICTURE S9]7[    VALUE ZEROS.         
77   FIRST-CARD-SW           PICTURE X       VALUE ^0^.           
77   GROWTH                 PICTURE S9]6[.                        
77   GROWTH-PERCENTAGE       PICTURE S999V99999.                  
77   CLASS-NUMBER            PICTURE 99.                          
77   ZERO-GROWTH             PICTURE 9]6[V99 VALUE 000000.00.     
77   TOTAL-GROWTH           PICTURE S9]7[    VALUE ZEROES.        
77   CARD-CTR                PICTURE 9]3[    VALUE ZEROES.        
77   SAVE-TOTAL             PICTURE S9]7[    VALUE ZEROES.        
77   TOT-GROWTH-TOTAL       PICTURE S9]7[    VALUE ZEROES.        
77   TOTAL-GR-PCT           PICTURE S999V99999.                   
77   SAVE-BIRTHS            PICTURE S9]6[    VALUE ZEROES.        
77   GROWTH-DIFF             PICTURE S9]6[   VALUE ZEROES.        
77   SAVE-BIRTH-YR           PICTURE 99.                          
77   COHORT-CTR             PICTURE S99      VALUE ZEROES.        
77   COHORT-DIFF             PICTURE S9]6[   VALUE ZEROES.        
77   TOT-COHORT-NO           PICTURE S9]7[   VALUE ZEROES.        
77   TOT-COHORT-PCT         PICTURE S999V9999.                    
77   CO-SUB-OLD              PICTURE 99      VALUE ZEROES.        
77   CO-SUB-NEW              PICTURE 99      VALUE ZEROES.        
77   TOTAL-COHORT            PICTURE S9]7[   VALUE ZEROES.        
77   TOTAL-NEW-COHORT       PICTURE S9]7[    VALUE ZEROES.        
77   TOTAL-COHORT-GROWTH    PICTURE S9]7[    VALUE ZEROES.        
77   TOTAL-COHORT-PCT       PICTURE S999V9999.                    
77   TOTAL-OLD-COHORT        PICTURE S9]7[   VALUE ZEROES.        
77   TOT-STUDENT-9-12        PICTURE S9]7[   VALUE ZEROES.        
77   TOT-STUDENT-1-8         PICTURE S9]7[   VALUE ZEROES.        
77   TOT-DIST-PCT            PICTURE S999V99 VALUE ZEROES.        
77   TOT-GR-NO               PICTURE S9]7[   VALUE ZEROES.        
77   TOT-GR-PCT              PICTURE S999V9999 VALUE ZEROES.      
77   TOT-CO-NO               PICTURE S9]7[   VALUE ZEROES.        
77   TOT-CO-PCT              PICTURE S999V9999 VALUE ZEROES.      
77   GR-PERCENT              PICTURE S999V99 VALUE ZEROES.        
77   GR-DIFF                 PICTURE S9]6[   VALUE ZEROES.        
77   SUB-COHORT-TOTAL        PICTURE S9]7[   VALUE ZEROES.        
                                                                  
01   HEADER-1.                                                    
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  FILLER              PICTURE X]10[   VALUE SPACES.        
     05  FILLER              PICTURE X]2[    VALUE ^19^.          
     05  PR-YEAR             PICTURE X]2[    VALUE SPACES.        
     05  FILLER              PICTURE X]12[   VALUE                
     ^  ENROLLMENT^.                                              
     05  FILLER              PICTURE X]10[   VALUE SPACES.        
     05  CTY-NAME            PICTURE X]50[.                       
     05  FILLER              PICTURE X]46[   VALUE SPACES.        
                                                                  
01   HEADER-2.                                                    
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  FILLER              PICTURE X]31[   VALUE SPACES.        
     05  FILLER              PICTURE X]23[   VALUE                
     ^PERCENT          GROWTH^.                                   
     05  FILLER              PICTURE X]18[   VALUE SPACES.        
     05  FILLER              PICTURE X]13[ VALUE ^COHORT CHANGE^. 
     05  FILLER              PICTURE X]47[   VALUE SPACES.        
                                                                  
01   HEADER-3.                                                    
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  FILLER              PICTURE X]17[   VALUE SPACES.        
     05  FILLER              PICTURE X]42[   VALUE                
     ^NUMBER     DISTRIBUTION    NUMBER  PERCENT^.                
     05  FILLER              PICTURE X]12[   VALUE SPACES.        
     05  FILLER              PICTURE X]16[   VALUE                
     ^NUMBER   PERCENT^.                                          
     05  FILLER              PICTURE X]45[   VALUE SPACES.        
                                                                  
01   DETAIL-LINE-1.                                               
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  FILLER              PICTURE X]10[   VALUE ^BIRTHS ]19^.  
     05  BIRTH-YR            PICTURE 99.                          
     05  FILLER              PICTURE X       VALUE ^[^.           
     05  FILLER              PICTURE X]3[    VALUE SPACES.        
     05  NO-OF-BIRTHS        PICTURE ZZZ,ZZ9.                     
     05  FILLER              PICTURE X]20[   VALUE SPACES.        
     05  GR-PLUS-OR-MINUS    PICTURE X       VALUE SPACES.        
     05  GR-NO               PICTURE ZZZ,ZZ9.                     
     05  FILLER              PICTURE X]2[    VALUE SPACES.        
     05  PLUS-OR-MINUS       PICTURE X       VALUE SPACES.        
     05  GR-PCT              PICTURE ZZZ.99.                      
     05  FILLER              PICTURE X]73[   VALUE SPACES.        
                                                                  
01   DETAIL-LINE-2.                                               
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  FILLER              PICTURE X]6[    VALUE ^GRADE ^.      
     05  GRADE-NO            PICTURE ZZ.                          
     05  FILLER              PICTURE X]8[    VALUE SPACES.        
     05  GR-NUMBER           PICTURE ZZZ,ZZ9.                     
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  ASTERISK-2          PICTURE X]2[    VALUE SPACES.        
     05  FILLER              PICTURE X]5[    VALUE SPACES.        
     05  DIST-PERCENT        PICTURE ZZZ.99.                      
     05  FILLER              PICTURE X]6[    VALUE SPACES.        
     05  GROWTH-NO-MINUS     PICTURE X       VALUE SPACES.        
     05  GROWTH-NO           PICTURE ZZZ,ZZ9.                     
     05  FILLER              PICTURE X]2[    VALUE SPACES.        
     05  MINUS-GROWTH        PICTURE X       VALUE SPACES.        
     05  GROWTH-PERCENT      PICTURE ZZZ.99.                      
     05  FILLER              PICTURE X]10[   VALUE SPACES.        
     05  COHORT-NO-MINUS     PICTURE X       VALUE SPACES.        
     05  COHORT-NO           PICTURE ZZZ,ZZZ.                     
     05  FILLER              PICTURE X]2[    VALUE SPACES.        
     05  COHORT-PCT-MINUS    PICTURE X       VALUE SPACES.        
     05  COHORT-PERCENT      PICTURE ZZZ.ZZ.                      
     05  FILLER              PICTURE X]46[   VALUE SPACES.        
                                                                  
01   TOTAL-LINE.                                                  
     05  FILLER              PICTURE X       VALUE SPACES.        
     05  TOTAL-CLASS-NOS     PICTURE X]14[   VALUE                
     ^TOTAL 1-12    ^.                                            
     05  TOTAL-NO            PICTURE Z,ZZZ,ZZ9.                   
     05  FILLER              PICTURE X]8[    VALUE SPACES.        
     05  DIST-PCT-PT         PICTURE ZZZ.ZZ.                      
     05  FILLER              PICTURE X]4[    VALUE SPACES.        
     05  TOTAL-MINUS         PICTURE X       VALUE SPACES.        
     05  TOTAL-NO-GROWTH     PICTURE Z,ZZZ,ZZ9.                   
     05  FILLER              PICTURE X]2[    VALUE SPACES.        
     05  TOT-MINUS           PICTURE X       VALUE SPACES.        
     05  TOTAL-PERCENT       PICTURE ZZZ.99.                      
     05  FILLER              PICTURE X]8[    VALUE SPACES.        
     05  CO-TOT-MINUS        PICTURE X       VALUE SPACES.        
     05  CO-TOT-NO           PICTURE Z,ZZZ,ZZZ.                   
     05  FILLER              PICTURE X]2[    VALUE SPACES.        
     05  CO-PCT-MINUS        PICTURE X       VALUE SPACES.        
     05  CO-TOT-PCT          PICTURE ZZZ.ZZ.                      
     05  FILLER              PICTURE X]46[   VALUE SPACES.        
                                                                  
01   TOTAL-LINE-RED REDEFINES TOTAL-LINE.                         
     10  FILLER              PICTURE X]133[.                      
                                                                  
01   STANDARD-DATE.                                               
     05  YEAR-IN             PICTURE X]2[    VALUE SPACES.        
     05  MONTH-IN            PICTURE X]2[    VALUE SPACES.        
     05  DAY-IN              PICTURE X]2[    VALUE SPACES.        
                                                                  
01   SAVE-CARD.                                                   
     05  CARD-AMOUNT        PICTURE S9]6[  OCCURS 13 TIMES.       
     05  CARD-YEAR           PICTURE X]2[.                        
                                                                  
PROCEDURE  DIVISION.                                              
                                                                  
100-START.                                                        
     OPEN INPUT CARD-IN.                                          
     OPEN OUTPUT PRINT-LINE.                                      
                                                                  
110-CALL-TIMEDATE.                                                
     ENTER LINKAGE.                                               
     CALL ^TIMEDATE^ USING TIME JDATE STDATE.                     
     ENTER COBOL.                                                 
     MOVE STDATE TO STANDARD-DATE.                                
     MOVE SPACES TO CTY-NAME.                                     
                                                                  
120-READ-FIRST-CARD.                                              
     MOVE ZEROES TO TOT-STUDENT-9-12.                             
     MOVE ZEROES TO TOT-STUDENT-1-8.                              
     MOVE ZEROES TO TOT-DIST-PCT.                                 
     MOVE ZEROES TO TOT-GR-NO.                                    
     MOVE ZEROES TO TOT-GR-PCT.                                   
     MOVE ZEROES TO TOT-CO-NO.                                    
     MOVE ZEROES TO TOT-CO-PCT.                                   
     MOVE SPACES TO TOTAL-LINE-RED.                               
     READ CARD-IN AT END GO TO 900-END-OF-JOB.                    
     IF CARD-IND ' ^D^                                            
         MOVE COUNTY-NAME TO CTY-NAME                             
         MOVE ZEROES TO COHORT-CTR                                
         GO TO 120-READ-FIRST-CARD.                               
     TRANSFORM ALPHA-CARD FROM SPACES TO ZEROES.                  
     MOVE CARD-YR TO SAVE-YEAR                                    
     PERFORM 500-HEADING-ROUTINE THRU 510-HEADER-EXIT.            
                                                                  
     NOTE *** TO ARRIVE AT TOTAL 1-12 SUBSCRIPT THRU CARD AMOUNT  
          OF INPUT CARD 12 TIMES THRU A PERFORM STATEMENT ***.    
                                                                  
     PERFORM 520-TOTAL-AMT THRU 530-TOTAL-EXIT.                   
                                                                  
     NOTE *** PRINT FIRST DETAIL LINE ***.                        
                                                                  
     MOVE 01 TO CARD-SUBSCRIPT.                                   
     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO NO-OF-BIRTHS.              
     MOVE ZERO-GROWTH TO GR-NO.                                   
     MOVE ZERO-GROWTH TO GR-PCT.                                  
     SUBTRACT 06 FROM CARD-YR-RED GIVING SAVE-BIRTH-YR.           
     MOVE SAVE-BIRTH-YR TO BIRTH-YR.                              
     WRITE PRINT FROM DETAIL-LINE-1 AFTER 2.                      
     MOVE ^1^ TO FIRST-CARD-SW.                                   
     ADD 1 TO CARD-CTR.                                           
     MOVE CARDS-IN TO SAVE-CARD.                                  
                                                                  
130-CONTINUE-1ST-CARD.                                            
     MOVE 02 TO CARD-SUBSCRIPT.                                   
     MOVE 00 TO CLASS-NUMBER.                                     
                                                                  
140-MOVE.                                                         
     ADD 01 TO CLASS-NUMBER.                                      
     MOVE CLASS-NUMBER TO GRADE-NO.                               
     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER.                 
     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES                        
     MOVE ^**^ TO ASTERISK-2                                      
         ELSE MOVE SPACES TO ASTERISK-2.                          
                                                                  
150-COMPUTE-DISTRIBUTION.                                         
     IF CAT ]CARD-SUBSCRIPT[ ' ZEROES                        
         MOVE ZERO-GROWTH TO DIST-PERCENT                         
         GO TO 160-PRINT-FIRST-CARD.                              
                                                                  
     IF TOTAL-STUDENTS ' ZEROES                                   
         AND CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES                   
         MOVE ZERO-GROWTH TO DIST-PERCENT                         
                                                                  
         GO TO 160-PRINT-FIRST-CARD.                              
     DIVIDE TOTAL-STUDENTS INTO CARD-AMT ]CARD-SUBSCRIPT[         
         GIVING GROWTH-PERCENTAGE ROUNDED.                        
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE                             
                                                                  
     MOVE GROWTH-PERCENTAGE TO DIST-PERCENT.                      
     ADD GROWTH-PERCENTAGE TO TOT-DIST-PCT.                       
                                                                  
160-PRINT-FIRST-CARD.                                             
     MOVE ZERO-GROWTH TO GROWTH-NO.                               
     MOVE ZERO-GROWTH TO GROWTH-PERCENT.                          
     MOVE SPACES TO GROWTH-NO-MINUS                               
     MOVE SPACES TO MINUS-GROWTH                                  
     MOVE ZERO-GROWTH TO COHORT-NO.                               
     MOVE ZERO-GROWTH TO COHORT-PERCENT.                          
     MOVE SPACES TO COHORT-NO-MINUS.                              
     MOVE SPACES TO COHORT-PCT-MINUS.                             
     WRITE PRINT FROM DETAIL-LINE-2 AFTER 2                       
     IF CARD-SUBSCRIPT ' 9                                        
     MOVE ^TOTAL 1-8   ^ TO TOTAL-CLASS-NOS                       
         PERFORM SUB-TOT-1ST-CARD THRU IST-EXIT.                  
                                                                  
     ADD 01 TO CARD-SUBSCRIPT.                                    
     IF CARD-SUBSCRIPT ) 14                                       
         GO TO 140-MOVE.                                          
                                                                  
170-TOTAL-FIRST-CARD.                                             
     MOVE ^TOTAL 9-12     ^ TO TOTAL-CLASS-NOS.                   
     PERFORM SUB-TOT-1ST-CARD THRU IST-EXIT.                      
     MOVE ZERO-GROWTH TO TOTAL-NO-GROWTH.                         
     MOVE SPACES TO TOTAL-MINUS                                   
     MOVE SPACES TO TOT-MINUS                                     
     MOVE ZERO-GROWTH TO TOTAL-PERCENT.                           
     MOVE 100.00 TO DIST-PCT-PT.                                  
     MOVE TOTAL-STUDENTS TO TOTAL-NO.                             
     MOVE ZERO-GROWTH TO CO-TOT-NO.                               
     MOVE ZERO-GROWTH TO CO-TOT-PCT.                              
     MOVE SPACES TO CO-TOT-MINUS.                                 
     MOVE SPACES TO CO-PCT-MINUS.                                 
     MOVE ^TOTAL 1-12     ^ TO TOTAL-CLASS-NOS.                   
                                                                  
     WRITE PRINT FROM TOTAL-LINE AFTER 3.                         
                                                                  
     MOVE CARD-AMT ]1[ TO SAVE-BIRTHS.                            
     MOVE TOTAL-STUDENTS TO SAVE-TOTAL.                           
     MOVE CARDS-IN TO SAVE-CARD.                                  
                                                                  
300-READ-2ND-CARD.                                                
     READ CARD-IN AT END GO TO 900-END-OF-JOB.                    
     IF CARD-IND ' ^D^                                            
         MOVE ZEROES TO COHORT-CTR                                
         MOVE COUNTY-NAME TO CTY-NAME                             
         GO TO 120-READ-FIRST-CARD.                               
     ADD 1 TO COHORT-CTR.                                         
     ADD 1 TO CARD-CTR.                                           
     TRANSFORM ALPHA-CARD FROM SPACES TO ZEROES.                  
     IF CARD-YR # SAVE-YEAR                                       
         GO TO 350-CONTINUE-2-CARD.                               
     DISPLAY ^   ^.                                               
     DISPLAY ^CARDS OUT OF SEQUENCE^.                             
     DISPLAY CARDS-IN.                                            
     DISPLAY ^   ^.                                               
     GO TO 900-END-OF-JOB.                                        
                                                                  
350-CONTINUE-2-CARD.                                              
                                                                  
     PERFORM 500-HEADING-ROUTINE THRU 510-HEADER-EXIT.            
                                                                  
     PERFORM 520-TOTAL-AMT THRU 530-TOTAL-EXIT.                   
                                                                  
400-COMPUTE-BIRTH-RATE.                                           
     MOVE 01 TO CARD-SUBSCRIPT                                    
     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO NO-OF-BIRTHS.              
     SUBTRACT 06 FROM CARD-YR-RED GIVING SAVE-BIRTH-YR.           
     MOVE SAVE-BIRTH-YR TO BIRTH-YR.                              
                                                                  
     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES AND                    
         CARD-AMOUNT ]CARD-SUBSCRIPT[ ' ZEROES                    
         MOVE ZERO-GROWTH TO GR-NO                                
         MOVE ZERO-GROWTH TO GR-PCT  MOVE SPACES TO PLUS-OR-MINUS 
     MOVE SPACES TO GR-PLUS-OR-MINUS                              
         GO TO 420-PRINT-BIRTH-LINE.                              
                                                                  
     IF CARD-AMT ]CARD-SUBSCRIPT[ # ZEROES AND                    
         CARD-AMOUNT ]CARD-SUBSCRIPT[ ' ZEROES                    
         MOVE 100.00 TO GR-PCT   MOVE SPACES TO PLUS-OR-MINUS     
     MOVE SPACES TO GR-PLUS-OR-MINUS                              
         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NO                  
         GO TO 420-PRINT-BIRTH-LINE.                              
     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES AND                    
         CARD-AMOUNT ]CARD-SUBSCRIPT[ # ZEROES                    
         MOVE 100.00 TO GR-PCT                                    
         MOVE ^-^ TO PLUS-OR-MINUS                                
     MOVE ^-^ TO GR-PLUS-OR-MINUS                                 
         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NO                  
         GO TO 420-PRINT-BIRTH-LINE.                              
                                                                  
410-COMPUTE-BIRTH-PCT.                                            
     IF CARD-AMT ]CARD-SUBSCRIPT[ ' CARD-AMOUNT ]CARD-SUBSCRIPT[  
         MOVE ZERO-GROWTH TO GR-PCT  MOVE SPACE TO PLUS-OR-MINUS  
         MOVE ZERO-GROWTH TO GR-NO                                
     MOVE SPACES TO GR-PLUS-OR-MINUS                              
         GO TO 420-PRINT-BIRTH-LINE.                              
                                                                  
     IF CARD-AMT ]CARD-SUBSCRIPT[ ) CARD-AMOUNT ]CARD-SUBSCRIPT[  
         SUBTRACT CARD-AMT ]CARD-SUBSCRIPT[                       
         FROM CARD-AMOUNT ]CARD-SUBSCRIPT[                        
         GIVING GROWTH-DIFF                                       
         DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO                 
             GROWTH-DIFF GIVING GROWTH-PERCENTAGE ROUNDED         
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE                             
         MOVE GROWTH-DIFF TO GR-NO  MOVE ^-^ TO PLUS-OR-MINUS     
     MOVE ^-^ TO GR-PLUS-OR-MINUS                                 
         MOVE GROWTH-PERCENTAGE TO GR-PCT                         
         GO TO 420-PRINT-BIRTH-LINE.                              
                                                                  
     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM                   
         CARD-AMT ]CARD-SUBSCRIPT[                                
         GIVING GROWTH-DIFF                                       
     DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO GROWTH-DIFF         
         GIVING GROWTH-PERCENTAGE ROUNDED                         
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE                             
     MOVE GROWTH-DIFF TO GR-NO                                    
     MOVE GROWTH-PERCENTAGE TO GR-PCT                             
     MOVE SPACES TO GR-PLUS-OR-MINUS                              
     MOVE SPACE TO PLUS-OR-MINUS.                                 
420-PRINT-BIRTH-LINE.                                             
     WRITE PRINT FROM DETAIL-LINE-1 AFTER 2.                      
         GO TO 550-DIST-PERCENT.                                  
                                                                  
500-HEADING-ROUTINE.                                              
     MOVE CARD-YR TO PR-YEAR.                                     
     WRITE PRINT FROM HEADER-1 AFTER 0.                           
     WRITE PRINT FROM HEADER-2 AFTER 2.                           
     WRITE PRINT FROM HEADER-3 AFTER 1.                           
                                                                  
510-HEADER-EXIT.                                                  
     EXIT.                                                        
                                                                  
520-TOTAL-AMT.                                                    
     MOVE 02 TO CARD-SUBSCRIPT.                                   
     MOVE ZEROES TO TOTAL-STUDENTS.                               
525-CONTINUE-TOTAL.                                               
     ADD CARD-AMT ]CARD-SUBSCRIPT[ TO TOTAL-STUDENTS.             
     IF CARD-SUBSCRIPT ) 10                                       
         ADD CARD-AMT ]CARD-SUBSCRIPT[ TO TOT-STUDENT-1-8.        
     IF CARD-SUBSCRIPT # 9                                        
         ADD CARD-AMT ]CARD-SUBSCRIPT[ TO TOT-STUDENT-9-12.       
     ADD 1 TO CARD-SUBSCRIPT.                                     
     IF CARD-SUBSCRIPT ) 14                                       
         GO TO 525-CONTINUE-TOTAL.                                
                                                                  
530-TOTAL-EXIT.                                                   
     EXIT.                                                        
                                                                  
550-DIST-PERCENT.                                                 
     MOVE 02 TO CARD-SUBSCRIPT.                                   
     MOVE 00 TO CLASS-NUMBER.                                     
     MOVE 01 TO CO-SUB-OLD.                                       
     MOVE 02 TO CO-SUB-NEW.                                       
     NOTE *** EDIT AMOUNTS FOR ZEROES BEFORE DIVIDING ***.        
                                                                  
560-MOVE.                                                         
                                                                  
     ADD 01 TO CLASS-NUMBER.                                      
     MOVE CLASS-NUMBER TO GRADE-NO.                               
     MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER.                 
                                                                  
     NOTE *** EDIT CARD AMTS FOR ZEROES.                          
                                                                  
                                                                  
         MOVE ^1^ TO FIRST-CARD-SW.                               
                                                                  
570-COMPUTE-DIST.                                                 
     IF TOTAL-STUDENTS ' ZEROES AND CARD-AMT ]CARD-SUBSCRIPT[     
         ' ZEROES                                                 
         MOVE ZERO-GROWTH TO DIST-PERCENT                         
         MOVE ZERO-GROWTH TO GR-NUMBER                            
         GO TO 595-COMPUTE-GROWTH.                                
     IF TOTAL-STUDENTS ' ZEROES AND CARD-AMT ]CARD-SUBSCRIPT[     
         # ZEROES                                                 
     MOVE 100.00 TO GROWTH-PERCENT                                
         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER              
         GO TO 595-COMPUTE-GROWTH.                                
     IF TOTAL-STUDENTS # ZEROES AND CARD-AMT ]CARD-SUBSCRIPT[     
         ' ZEROES                                                 
         MOVE ZERO-GROWTH TO DIST-PERCENT                         
         MOVE ZERO-GROWTH TO GROWTH-NO                            
         GO TO 595-COMPUTE-GROWTH.                                
     DIVIDE TOTAL-STUDENTS INTO CARD-AMT ]CARD-SUBSCRIPT[         
         GIVING GROWTH-PERCENTAGE ROUNDED                         
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE                             
         MOVE GROWTH-PERCENTAGE TO DIST-PERCENT                   
     ADD GROWTH-PERCENTAGE TO TOT-DIST-PCT                        
         MOVE CARD-AMT ]CARD-SUBSCRIPT[ TO GR-NUMBER.             
                                                                  
595-COMPUTE-GROWTH.                                               
                                                                  
     IF CARD-AMT ]CARD-SUBSCRIPT[ ' CARD-AMOUNT ]CARD-SUBSCRIPT[  
         MOVE ZERO-GROWTH TO GROWTH-PERCENT                       
         MOVE ZEROES TO GROWTH-DIFF                               
         MOVE SPACES TO MINUS-GROWTH                              
     IF CARD-AMT ]CARD-SUBSCRIPT[ # ZEROES                        
     AND CARD-AMOUNT ]CARD-SUBSCRIPT[ ' ZEROES                    
     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM                   
     CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF                 
     MOVE 100.00000 TO GROWTH-PERCENTAGE                          
         GO TO 596-CONT.                                          
     IF CARD-AMT ]CARD-SUBSCRIPT[ ) CARD-AMOUNT ]CARD-SUBSCRIPT[  
         GO TO 598-CONT.                                          
     MOVE SPACES TO GROWTH-NO-MINUS                               
         GO TO 600-PRINT-DETAIL.                                  
     IF CARD-AMT ]CARD-SUBSCRIPT[ # CARD-AMOUNT ]CARD-SUBSCRIPT[  
         SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM               
         CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF             
         DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO GROWTH-DIFF     
         GIVING GROWTH-PERCENTAGE ROUNDED                         
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE.                            
596-CONT.                                                         
         MOVE GROWTH-PERCENTAGE TO GROWTH-PERCENT                 
     MOVE GROWTH-PERCENTAGE TO GR-PERCENT                         
     MULTIPLY GR-PERCENT BY <1 GIVING GR-PERCENT                  
     ADD GR-PERCENT TO TOT-GR-PCT                                 
     MOVE GROWTH-DIFF TO GR-DIFF                                  
     MULTIPLY GR-DIFF BY <1 GIVING GR-DIFF                        
     ADD GR-DIFF TO TOT-GR-NO                                     
         MOVE SPACES TO GROWTH-NO-MINUS                           
         MOVE SPACES TO MINUS-GROWTH                              
         GO TO 600-PRINT-DETAIL.                                  
598-CONT.                                                         
     IF CARD-AMT ]CARD-SUBSCRIPT[ ' ZEROES                        
     AND CARD-AMOUNT ]CARD-SUBSCRIPT[ # ZEROES                    
     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM                   
     CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF                 
     MOVE 100.00000 TO GROWTH-PERCENTAGE                          
         GO TO 599-CONT.                                          
     SUBTRACT CARD-AMOUNT ]CARD-SUBSCRIPT[ FROM                   
         CARD-AMT ]CARD-SUBSCRIPT[ GIVING GROWTH-DIFF             
         DIVIDE CARD-AMOUNT ]CARD-SUBSCRIPT[ INTO GROWTH-DIFF     
         GIVING GROWTH-PERCENTAGE ROUNDED                         
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE.                            
599-CONT.                                                         
         MOVE GROWTH-PERCENTAGE TO GROWTH-PERCENT                 
     MOVE GROWTH-PERCENTAGE TO GR-PERCENT                         
     MULTIPLY GR-PERCENT BY -1 GIVING GR-PERCENT                  
     SUBTRACT GR-PERCENT FROM TOT-GR-PCT                          
     MOVE GROWTH-DIFF TO GR-DIFF                                  
     MULTIPLY GR-DIFF BY -1 GIVING GR-DIFF                        
     SUBTRACT GR-DIFF FROM TOT-GR-NO                              
         MOVE ^-^ TO MINUS-GROWTH                                 
     MOVE ^-^ TO GROWTH-NO-MINUS                                  
         GO TO 600-PRINT-DETAIL.                                  
                                                                  
600-PRINT-DETAIL.                                                 
     MOVE ZERO-GROWTH TO COHORT-NO.                               
     MOVE ZERO-GROWTH TO COHORT-PERCENT.                          
     MOVE SPACES TO COHORT-NO-MINUS.                              
     MOVE SPACES TO COHORT-PCT-MINUS.                             
     IF CLASS-NUMBER # 01                                         
         PERFORM COHORT-CHANGE                                    
         THRU COHORT-CHANGE-EXIT.                                 
     MOVE GROWTH-DIFF TO GROWTH-NO.                               
     ADD GROWTH-DIFF TO TOTAL-GROWTH.                             
     WRITE PRINT FROM DETAIL-LINE-2 AFTER 2.                      
     ADD 01 TO CO-SUB-OLD.                                        
     ADD 01 TO CO-SUB-NEW.                                        
     IF CARD-SUBSCRIPT ' 9                                        
     MOVE ^TOTAL 1-8     ^ TO TOTAL-CLASS-NOS                     
         PERFORM SUB-TOT-2ND-CARD THRU IID-EXIT.                  
     ADD 1 TO CARD-SUBSCRIPT.                                     
     IF CARD-SUBSCRIPT ) 14                                       
         GO TO 560-MOVE.                                          
                                                                  
610-PRINT-TOTAL-LINE.                                             
     MOVE ^TOTAL 9-12    ^ TO TOTAL-CLASS-NOS.                    
     PERFORM SUB-TOT-2ND-CARD THRU IID-EXIT.                      
     MOVE TOTAL-STUDENTS TO TOTAL-NO.                             
     MOVE ^TOTAL 1-12    ^ TO TOTAL-CLASS-NOS.                    
     MOVE 100.00 TO DIST-PCT-PT.                                  
     IF FIRST-CARD-SW ' ^1^                                       
         GO TO 630-CONTINUE-TOTAL.                                
     MOVE TOTAL-STUDENTS TO TOTAL-NO.                             
     MOVE ZERO-GROWTH TO TOTAL-NO-GROWTH.                         
     MOVE ZERO-GROWTH TO TOTAL-PERCENT.                           
     MOVE SPACES TO TOTAL-MINUS                                   
     MOVE SPACES TO TOT-MINUS                                     
         GO TO 650-PRINT-TOTAL-LINE.                              
                                                                  
630-CONTINUE-TOTAL.                                               
     IF TOTAL-STUDENTS ' SAVE-TOTAL                               
         MOVE ZERO-GROWTH TO TOTAL-NO-GROWTH                      
         MOVE ZERO-GROWTH TO TOTAL-PERCENT                        
         MOVE SPACES TO TOT-MINUS                                 
     MOVE SPACES TO TOTAL-MINUS                                   
         GO TO 650-PRINT-TOTAL-LINE.                              
                                                                  
     IF TOTAL-STUDENTS # SAVE-TOTAL                               
         SUBTRACT SAVE-TOTAL FROM TOTAL-STUDENTS                  
         GIVING TOT-GROWTH-TOTAL                                  
         DIVIDE SAVE-TOTAL INTO TOT-GROWTH-TOTAL                  
             GIVING TOTAL-GR-PCT ROUNDED                          
         MULTIPLY TOTAL-GR-PCT BY 100                             
             GIVING TOTAL-GR-PCT                                  
         MOVE TOTAL-GR-PCT TO TOTAL-PERCENT                       
         MOVE SPACES TO TOT-MINUS                                 
     MOVE SPACES TO TOTAL-MINUS                                   
         MOVE TOT-GROWTH-TOTAL TO TOTAL-NO-GROWTH                 
                                                                  
         MOVE TOTAL-STUDENTS TO SAVE-TOTAL                        
         GO TO 650-PRINT-TOTAL-LINE.                              
     SUBTRACT TOTAL-STUDENTS FROM SAVE-TOTAL                      
         GIVING TOT-GROWTH-TOTAL                                  
         DIVIDE SAVE-TOTAL INTO TOT-GROWTH-TOTAL                  
             GIVING TOTAL-GR-PCT ROUNDED                          
         MULTIPLY TOTAL-GR-PCT BY 100                             
             GIVING TOTAL-GR-PCT                                  
         MOVE TOTAL-GR-PCT TO TOTAL-PERCENT                       
         MOVE ^-^ TO TOT-MINUS                                    
     MOVE ^-^ TO TOTAL-MINUS                                      
         MOVE TOT-GROWTH-TOTAL TO TOTAL-NO-GROWTH                 
         MOVE TOTAL-STUDENTS TO SAVE-TOTAL.                       
650-PRINT-TOTAL-LINE.                                             
     PERFORM COHORT-TOTAL THRU COHORT-TOTAL-EXIT.                 
     WRITE PRINT FROM TOTAL-LINE AFTER 3                          
     MOVE TOTAL-STUDENTS TO SAVE-TOTAL.                           
     MOVE CARDS-IN TO SAVE-CARD                                   
     MOVE SPACES TO TOTAL-LINE-RED.                               
         GO TO 300-READ-2ND-CARD.                                 
                                                                  
  900-END-OF-JOB.                                                 
     CLOSE CARD-IN.                                               
     CLOSE PRINT-LINE.                                            
     DISPLAY ^CARDS READ  ^, CARD-CTR.                            
     STOP RUN.                                                    
                                                                  
COHORT-CHANGE.                                                    
     IF CARD-AMT ]CO-SUB-NEW[ ' CARD-AMOUNT ]CO-SUB-OLD[          
         MOVE ZEROES TO COHORT-PERCENT                            
         MOVE ZEROES TO COHORT-NO                                 
         MOVE SPACES TO COHORT-NO-MINUS                           
         MOVE SPACES TO COHORT-PCT-MINUS                          
         GO TO COHORT-CHANGE-EXIT.                                
                                                                  
     IF CARD-AMT ]CO-SUB-NEW[ # ZEROES                            
     AND CARD-AMOUNT ]CO-SUB-OLD[ ' ZEROES                        
     SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[ FROM                       
     CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF                     
     MOVE 100.00000 TO GROWTH-PERCENTAGE                          
         GO TO 910-CONT.                                          
     IF CARD-AMT ]CO-SUB-NEW[ ) CARD-AMOUNT ]CO-SUB-OLD[          
         GO TO 920-CONT.                                          
     IF CARD-AMT ]CO-SUB-NEW[ # CARD-AMOUNT ]CO-SUB-OLD[          
         SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[ FROM                   
     CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF                     
         DIVIDE CARD-AMOUNT ]CO-SUB-OLD[ INTO COHORT-DIFF         
         GIVING GROWTH-PERCENTAGE ROUNDED.                        
910-CONT.                                                         
     MULTIPLY GROWTH-PERCENTAGE BY 100                            
             GIVING GROWTH-PERCENTAGE                             
         MOVE GROWTH-PERCENTAGE TO COHORT-PERCENT                 
         MOVE COHORT-DIFF TO COHORT-NO                            
         MOVE SPACES TO COHORT-NO-MINUS                           
         MOVE SPACES TO COHORT-PCT-MINUS                          
         ADD COHORT-DIFF TO TOTAL-COHORT                          
     MULTIPLY COHORT-DIFF BY <1 GIVING COHORT-DIFF                
     ADD COHORT-DIFF TO SUB-COHORT-TOTAL                          
             GO TO COHORT-CHANGE-EXIT.                            
                                                                  
920-CONT.                                                         
     IF CARD-AMT ]CO-SUB-NEW[ ' ZEROES                            
     AND CARD-AMOUNT ]CO-SUB-OLD[ # ZEROES                        
     SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[                            
     FROM CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF                
     MOVE 100.00000 TO GROWTH-PERCENTAGE                          
         GO TO 950-CONT.                                          
     SUBTRACT CARD-AMOUNT ]CO-SUB-OLD[ FROM                       
         CARD-AMT ]CO-SUB-NEW[ GIVING COHORT-DIFF                 
         DIVIDE CARD-AMOUNT ]CO-SUB-OLD[ INTO COHORT-DIFF         
         GIVING GROWTH-PERCENTAGE ROUNDED                         
         MULTIPLY GROWTH-PERCENTAGE BY 100                        
             GIVING GROWTH-PERCENTAGE.                            
950-CONT.                                                         
         MOVE GROWTH-PERCENTAGE TO COHORT-PERCENT                 
         MOVE ^-^ TO COHORT-NO-MINUS                              
         MOVE ^-^ TO COHORT-PCT-MINUS                             
         MOVE COHORT-DIFF TO COHORT-NO                            
     MULTIPLY COHORT-DIFF BY -1 GIVING COHORT-DIFF                
     SUBTRACT COHORT-DIFF FROM SUB-COHORT-TOTAL.                  
     ADD COHORT-DIFF TO TOTAL-COHORT.                             
COHORT-CHANGE-EXIT.                                               
     EXIT.                                                        
                                                                  
COHORT-TOTAL.                                                     
     MOVE 02 TO CO-SUB-OLD.                                       
     MOVE ZEROES TO TOTAL-OLD-COHORT.                             
COMPUTE-GDS-1-11.                                                 
     ADD CARD-AMOUNT ]CO-SUB-OLD[ TO TOTAL-OLD-COHORT.            
     ADD 01 TO CO-SUB-OLD.                                        
     IF CO-SUB-OLD ) 13                                           
         GO TO COMPUTE-GDS-1-11.                                  
RESET-TOTALS.                                                     
     MOVE 03 TO CO-SUB-NEW.                                       
     MOVE ZEROES TO TOTAL-NEW-COHORT.                             
COMPUTE-GDS-2-12.                                                 
     ADD CARD-AMT ]CO-SUB-NEW[ TO TOTAL-NEW-COHORT.               
     ADD 01 TO CO-SUB-NEW.                                        
     IF CO-SUB-NEW ) 14                                           
         GO TO COMPUTE-GDS-2-12.                                  
                                                                  
                                                                  
COMPUTE-COHORT-PCT.                                               
     IF TOTAL-OLD-COHORT ' TOTAL-NEW-COHORT                       
     MOVE ZEROES TO CO-TOT-PCT                                    
     MOVE ZEROES TO CO-TOT-NO                                     
     MOVE SPACES TO CO-TOT-MINUS                                  
     MOVE SPACES TO CO-PCT-MINUS                                  
         GO TO COHORT-TOTAL-EXIT.                                 
                                                                  
     IF TOTAL-NEW-COHORT # TOTAL-OLD-COHORT                       
     SUBTRACT TOTAL-OLD-COHORT FROM TOTAL-NEW-COHORT              
         GIVING TOTAL-COHORT-GROWTH                               
     DIVIDE TOTAL-OLD-COHORT INTO TOTAL-COHORT-GROWTH             
         GIVING TOTAL-COHORT-PCT ROUNDED                          
     MULTIPLY TOTAL-COHORT-PCT BY 100                             
         GIVING TOTAL-COHORT-PCT                                  
     MOVE TOTAL-COHORT-PCT TO CO-TOT-PCT                          
     MOVE SPACES TO CO-PCT-MINUS                                  
     MOVE TOTAL-COHORT-GROWTH TO CO-TOT-NO                        
     MOVE SPACES TO CO-TOT-MINUS                                  
         GO TO COHORT-TOTAL-EXIT.                                 
                                                                  
     SUBTRACT TOTAL-NEW-COHORT FROM TOTAL-OLD-COHORT              
         GIVING TOTAL-COHORT-GROWTH                               
     DIVIDE TOTAL-OLD-COHORT INTO TOTAL-COHORT-GROWTH             
         GIVING TOTAL-COHORT-PCT ROUNDED                          
     MULTIPLY TOTAL-COHORT-PCT BY 100                             
         GIVING TOTAL-COHORT-PCT                                  
     MOVE TOTAL-COHORT-PCT TO CO-TOT-PCT                          
     MOVE TOTAL-COHORT-GROWTH TO CO-TOT-NO                        
     MOVE ^-^ TO CO-TOT-MINUS                                     
     MOVE ^-^ TO CO-PCT-MINUS.                                    
                                                                  
COHORT-TOTAL-EXIT.                                                
     EXIT.                                                        
                                                                  
                                                                  
SUB-TOT-1ST-CARD.                                                 
     IF CARD-SUBSCRIPT ' 9                                        
     MOVE TOT-STUDENT-1-8 TO TOTAL-NO                             
     ELSE MOVE TOT-STUDENT-9-12 TO TOTAL-NO.                      
     MOVE ZERO-GROWTH TO CO-TOT-NO.                               
     MOVE ZERO-GROWTH TO CO-TOT-PCT.                              
     MOVE SPACES TO CO-TOT-MINUS.                                 
     MOVE SPACES TO CO-PCT-MINUS.                                 
     MOVE TOT-DIST-PCT TO DIST-PCT-PT.                            
     WRITE PRINT FROM TOTAL-LINE AFTER 3.                         
     MOVE SPACES TO PRINT.                                        
     WRITE PRINT AFTER 1.                                         
     MOVE ZEROES TO TOT-DIST-PCT.                                 
     IF CARD-SUBSCRIPT # 9                                        
     MOVE ZEROES TO TOT-STUDENT-1-8                               
     MOVE ZEROES TO TOT-STUDENT-9-12.                             
IST-EXIT.                                                         
     EXIT.                                                        
                                                                  
SUB-TOT-2ND-CARD.                                                 
     MOVE TOT-DIST-PCT TO DIST-PCT-PT.                            
     MOVE TOT-GR-NO TO TOTAL-NO-GROWTH.                           
     IF CARD-SUBSCRIPT ' 9                                        
     DIVIDE TOT-STUDENT-1-8 INTO TOT-GR-NO                        
     GIVING TOT-GR-PCT                                            
     ELSE DIVIDE TOT-STUDENT-9-12 INTO TOT-GR-NO                  
         GIVING TOT-GR-PCT.                                       
     MULTIPLY TOT-GR-PCT BY 100 GIVING TOT-GR-PCT.                
     MOVE TOT-GR-PCT TO TOTAL-PERCENT.                            
     MOVE SPACES TO TOTAL-MINUS, TOT-MINUS.                       
     IF TOT-GR-NO IS NEGATIVE                                     
     MOVE ^-^ TO TOTAL-MINUS, TOT-MINUS.                          
     IF CARD-SUBSCRIPT ' 9                                        
     MOVE TOT-STUDENT-1-8 TO TOTAL-NO                             
     ELSE MOVE TOT-STUDENT-9-12 TO TOTAL-NO.                      
     MOVE SUB-COHORT-TOTAL TO CO-TOT-NO.                          
     IF SUB-COHORT-TOTAL IS NEGATIVE                              
     MOVE ^-^ TO CO-TOT-MINUS ELSE MOVE SPACES TO CO-TOT-MINUS.   
     IF SUB-COHORT-TOTAL IS NEGATIVE                              
     MOVE ^-^ TO CO-PCT-MINUS ELSE MOVE SPACES TO CO-PCT-MINUS.   
     IF CARD-SUBSCRIPT ' 9                                        
         GO TO COHORT-TOTALS-1-8.                                 
         GO TO COHORT-TOTALS-9-12.                                
COHORT-TOTALS-1-8.                                                
     IF SUB-COHORT-TOTAL IS NEGATIVE                              
     MULTIPLY -1 BY SUB-COHORT-TOTAL GIVING SUB-COHORT-TOTAL      
     ADD SUB-COHORT-TOTAL TO TOT-STUDENT-1-8                      
     ELSE SUBTRACT SUB-COHORT-TOTAL FROM TOT-STUDENT-1-8.         
     SUBTRACT CARD-AMT ]1[ FROM TOT-STUDENT-1-8.                  
     DIVIDE TOT-STUDENT-1-8 INTO SUB-COHORT-TOTAL                 
         GIVING TOT-CO-PCT.                                       
         GO TO CONT-PRINT.                                        
COHORT-TOTALS-9-12.                                               
     IF SUB-COHORT-TOTAL IS NEGATIVE                              
     MULTIPLY -1 BY SUB-COHORT-TOTAL GIVING SUB-COHORT-TOTAL      
     ADD SUB-COHORT-TOTAL TO TOT-STUDENT-9-12                     
     ELSE SUBTRACT SUB-COHORT-TOTAL FROM TOT-STUDENT-9-12.        
     DIVIDE TOT-STUDENT-9-12 INTO SUB-COHORT-TOTAL                
         GIVING TOT-CO-PCT.                                       
CONT-PRINT.                                                       
     MULTIPLY TOT-CO-PCT BY 100 GIVING TOT-CO-PCT.                
     MOVE TOT-CO-PCT TO CO-TOT-PCT.                               
                                                                  
     WRITE PRINT FROM TOTAL-LINE AFTER 3.                         
     MOVE SPACES TO PRINT.                                        
     WRITE PRINT AFTER 1.                                         
     MOVE ZEROES TO TOT-DIST-PCT.                                 
     MOVE ZEROES TO TOT-GR-NO.                                    
     MOVE ZEROES TO TOT-GR-PCT.                                   
     MOVE ZEROES TO TOT-CO-NO.                                    
     MOVE ZEROES TO TOT-CO-PCT.                                   
     MOVE ZEROES TO GR-PERCENT.                                   
     MOVE ZEROES TO GR-DIFF.                                      
     MOVE ZEROES TO SUB-COHORT-TOTAL.                             
     MOVE SPACES TO CO-TOT-MINUS.                                 
     MOVE SPACES TO CO-PCT-MINUS.                                 
     MOVE SPACES TO ASTERISK-2.                                   
     IF CARD-SUBSCRIPT # 9                                        
     MOVE ZEROES TO TOT-STUDENT-1-8                               
     MOVE ZEROES TO TOT-STUDENT-9-12.                             
IID-EXIT.                                                         
     EXIT.                                                        
    bRƒ