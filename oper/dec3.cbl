IDENTIFICATION  DIVISION.                                         
PROGRAM-ID.                                                       
     A230.                                                        
AUTHOR.                                                           
     MIKE ORBAN.                                                  
DATE-WRITTEN.                                                     
     MARCH 15, 1972.                                              
DATE-COMPILED.   11/30/72.                                        
REMARKS.                                                          
     THIS PROGRAM WILL QUALIFY ALL ACTIVE COACH AND SINGLE        
     FORECAST ACCOUNTS.                                           
ENVIRONMENT  DIVISION.                                            
CONFIGURATION  SECTION.                                           
SOURCE-COMPUTER. PDP-10.                                          
OBJECT-COMPUTER. PDP-10.                                          
INPUT-OUTPUT  SECTION.                                            
FILE-CONTROL.                                                     
     SELECT  INPUT-MASTER-FILE ASSIGN DSK,                        
      RECORDING MODE IS ASCII.                                    
                                                                  
     SELECT  OUTPUT-MASTER-FILE ASSIGN DSK,                       
      RECORDING MODE IS ASCII.                                    
                                                                  
I-O-CONTROL.                                                      
     APPLY WRITE-ONLY ON OUTPUT-MASTER-FILE.                      
DATA  DIVISION.                                                   
FILE  SECTION.                                                    
FD   INPUT-MASTER-FILE                                            
         VALUE OF IDENTIFICATION IS "INMAS DAT"                   
     BLOCK CONTAINS 1364 CHARACTERS                               
     LABEL RECORDS ARE STANDARD                                   
     DATA RECORDS ARE  INPUT-TEACHER-MASTER, INPUT-SCHOOL-MASTER  .
01   INPUT-TEACHER-MASTER .                                   
     02  ID-NUMBER.                                               
         03  ZIP-CODE            PICTURE  9(5).                   
         03  CODE-ZIP REDEFINES ZIP-CODE.                         
             04  ZIP-HIGH        PICTURE X.                       
             04  FILLER          PICTURE X(4).                    
         03  SCH-NUMBER          PICTURE  999.                    
         03  EDITION             PICTURE  99.                     
         03  TCH-NUMBER          PICTURE  999.                    
         03  CHECK-DIGIT         PICTURE     9.                   
     02  REST-OF-MASTERI.                                         
         03  CURRENT-COPYI.                                       
             04  STUD-COPYI      PICTURE    9(4).                 
             04  XSTUDCOPYI REDEFINES STUD-COPYI  PICTURE X(4).   
             04  PAID-TCHI       PICTURE    99.                   
             04  XPAIDTCHI REDEFINES PAID-TCHI  PICTURE XX.       
             04  RECSI           PICTURE    99.                   
             04  FILMI           PICTURE    99.                   
             04  XTCI            PICTURE    99.                   
         03  AMOUNT-PAIDI  PICTURE S9(4)V99 COMPUTATIONAL.        
          03 TERM-IN             PICTURE    9.                    
             88 ITERM1   VALUE 1.                                 
             88 ITERM2   VALUE 2.                                 
             88 ITERM3   VALUE 3.                                 
             88 ITERM4   VALUE 4.                                 
             88 ITERM5   VALUE 5.                                 
             88 ITERM6   VALUE 6.                                 
             88 ITERM7   VALUE 7.                                 
             88 ITERM8   VALUE 8.                                 
         03  TERMX REDEFINES TERM-IN    PICTURE X.                
         03  GRADE-API           PICTURE  XX.                     
         03  TEACHER-NAMEI       PICTURE  X(25).                  
         03  PROMO-KEYI          PICTURE  X(5).                   
         03  BILL-TOI            PICTURE    9.                    
             88 IBILL1   VALUE 1.                                 
             88 IBILL2   VALUE 2.                                 
             88 IBILL3   VALUE 3.                                 
             88 IBILL4   VALUE 4.                                 
             88 IBILL5   VALUE 5.                                 
             88 IBILL6   VALUE 6.                                 
         03  BOARD-NUMBERI       PICTURE  X(5).                   
         03  ISSUE-DATESI.                                        
             04  BEGINI.                                          
                 05  ISSI        PICTURE    99.                   
                 05  YEARI       PICTURE    9.                    
             04  EXPIREI.                                         
                 05  ISSI        PICTURE    99.                   
                 05  YEARI       PICTURE   9.                     
     03  SPEC-ISS-CHECK REDEFINES  ISSUE-DATESI.                  
         04  BGNI.                                                
                 05  II        PICTURE  XX.                       
                 05  YYI            PICTURE  X.                   
             04  EXPI.                                            
                 05  II            PICTURE  XX.                   
                 05  YYI       PICTURE  X.                        
         03  CHARGESI        COMPUTATIONAL.                       
             04  FIRST-YR-TERMI  PICTURE  S9(4)V99.               
             04  SECOND-SEMI     PICTURE  S9(4)V99.               
             04  POSTALI         PICTURE  S9(3)V99.               
             04  OTHERI          PICTURE  S9(3)V99.               
             04  BALANCE-DUEI    PICTURE  S9(4)V99.               
         03  REVISION-ONEI.                                       
             04  REV1-STUDI      PICTURE     9(4).                
             04  REV1-TCHI       PICTURE       99.                
             04  REV1-RECI       PICTURE       99.                
             04  REV1-FILMI      PICTURE       99.                
             04  REV1-ISSUEI.                                     
                 05  REV1-ISSI   PICTURE    99.                   
                 05  REV1-YEARI  PICTURE        9.                
         03  REVISION-TWOI.                                       
             04  REV2-STUDI      PICTURE     9(4).                
             04  REV2-TCHI       PICTURE       99.                
             04  REV2-RECI       PICTURE       99.                
             04  REV2-FILMI      PICTURE       99.                
             04  REV2-ISSUEI.                                     
                 05  REV2-ISSI   PICTURE    99.                   
                 05  REV2-YEARI  PICTURE        9.                
         03  REVISION-THREEI.                                     
             04  REV3-STUDI      PICTURE     9(4).                
             04  REV3-TCHI       PICTURE       99.                
             04  REV3-RECI       PICTURE       99.                
             04  REV3-FILMI      PICTURE       99.                
             04  REV3-ISSUEI.                                     
                 05  REV3-ISSI   PICTURE    99.                   
                 05  REV3-YEARI  PICTURE        9.                
         03  TYPE-MASTERI        PICTURE A.                       
         03  MASTER-HOLD-CODEI   PICTURE X.                       
         03  LAST-SEMESTERI.                                      
             04  LAST-STUDI      PICTURE   9(4).                  
             04  LAST-TCHI       PICTURE    99.                   
             04  LAST-XTCI       PICTURE    99.                   
             04  LAST-RECI       PICTURE    99.                   
         03  TRANS-INFOI.                                         
             04  MTRANS-CODEI    PICTURE    X.                    
             04  MBATCH-NUMBERI  PICTURE   X(6).                  
             04  MMAIL-NUMBERI   PICTURE    XX.                   
         03  FILLER              PICTURE   X(11).                 
01                       INPUT-SCHOOL-MASTER .                
     02  ID-NUMBER.                                               
         03  ZIP-CODE            PICTURE  9(5).                   
         03  SCH-NUMBER          PICTURE  999.                    
         03  BLANKS              PICTURE  X(5).                   
             88  SCH-RECORD      VALUE SPACES.                    
         03  CHECK-DIGIT         PICTURE        9.                
     02  REST-OF-MASTERIS.                                        
         03  SCHOOL-NAMEI        PICTURE      X(20).              
         03  STREET-ADDRESSI     PICTURE      X(20).              
         03  CITY-NAMEI          PICTURE      X(20).              
         03  STATE-ALPHAI        PICTURE       XX.                
         03  STATE-CODEI         PICTURE       XX.                
         03  SR-CODEI            PICTURE       XX.                
         03  COUNTYI             PICTURE      XXX.                
         03  PP-ZONEI            PICTURE        X.                
         03  NEW-IDI             PICTURE      X(8).               
         03  HOLDI                         PICTURE  X.            
         03  SEXI                     PICTURE  X.                 
         03  TYPESCHI                     PICTURE  XX.            
         03  SCHSIZEI                        PICTURE  X(4).       
         03  STRANS-INFOI.                                        
             04  STRANS-CODEI    PICTURE       X.                 
             04  SBATCH-NUMBERI  PICTURE      X(6).               
             04  SMAIL-NUMBERI   PICTURE       XX.                
FD   OUTPUT-MASTER-FILE                                           
         VALUE OF IDENTIFICATION IS "OUTMASDAT"                   
         BLOCK CONTAINS 1364 CHARACTERS                           
     LABEL RECORDS ARE STANDARD                                   
     DATA RECORDS ARE OUTPUT-TEACHER-MASTER, OUTPUT-SCHOOL-MASTER  .
01   OUTPUT-TEACHER-MASTER       PICTURE X(164) .             
01   OUTPUT-SCHOOL-MASTER        PICTURE X(109) .             
WORKING-STORAGE  SECTION.                                         
77   MODULE-ID  PIC X(24)  VALUE "A230SANS 11/23/72 01****".      
77   SINGLES-CTR  PIC S9(9)  COMPUTATIONAL SYNC VALUE 0.         
77   ERR-INC    COMPUTATIONAL PICTURE S99  VALUE ZEROS SYNC.      
77   LINE-COUNT COMP  PIC S99  SYNC  VALUE 0.                    
77   RECORDS-IN COMPUTATIONAL    PICTURE S9(11)  VALUE ZEROS.     
77   RECORDS-OUT COMPUTATIONAL   PICTURE S9(11)  VALUE ZEROS.     
77   PAGE-CTR    COMPUTATIONAL   PICTURE S9(3)   VALUE ZEROS.     
77   SCHOOL-CTR  COMPUTATIONAL   PICTURE S9(11)  VALUE ZEROS.     
77   TEACHER-CTR COMPUTATIONAL   PICTURE S9(11)  VALUE ZEROS.     
77   FIRST-RECORD                PICTURE X       VALUE "Y".       
77   TOTAL-ADJUSTED-RECORDS      COMPUTATIONAL                    
         PICTURE S9(11) VALUE ZEROS.                              
01   PRINTER .                                                
     02  CC  PICTURE X VALUE SPACES.                              
     02  DATAREA  PICTURE X(132)  VALUE SPACES.                   
01   HDR1 .                                                   
     02  FILLER              PICTURE X(3)    VALUE SPACES.        
     02  DATE-CONSTANT       PICTURE X(5)    VALUE "DATE ".       
     02  RPT-DATE            PICTURE X(8)    VALUE SPACES.        
     02  FILLER              PICTURE X(12)   VALUE SPACES.        
     02  RPT-NAME            PICTURE X(80)   VALUE                
     "    M A G A Z I N E    G R A D E   A P   C O R R E C T I O  
-        "N   L I S T I N G   ".                                  
     02  FILLER              PICTURE X(10)   VALUE SPACES.        
     02  PAGE-CONSTANT       PICTURE X(5)    VALUE "PAGE ".       
     02  PAGE-EDIT           PICTURE ZZZ.                         
     02  FILLER              PICTURE X(7)    VALUE SPACES.        
01   HDR2 .                                                   
     02  FILLER                  PICTURE X(5)    VALUE SPACES.    
     02  FILLER                  PICTURE X(30)   VALUE            
         "ZIP   SCH ED TCH    1ST  SEM  ".                        
     02  FILLER                  PICTURE X(45)   VALUE            
         "        2ND  SEM         AMT PAID            ".         
     02  FILLER                  PICTURE X(46)   VALUE            
         "BAL-DUE     TERM     STUD   PDTCH  TYPE  GAP  ".        
     02  FILLER                  PICTURE X(7)    VALUE SPACES.    
01   DETAIL-LINE .                                            
     02  FILLER                  PICTURE X(5)    VALUE SPACES.    
     02  ZIPP                    PICTURE X(5)    VALUE SPACES.    
     02  FILLER                  PICTURE X       VALUE SPACES.    
     02  SCHP                    PICTURE XXX     VALUE SPACES.    
     02  FILLER                  PICTURE X       VALUE SPACES.    
     02  EDP                     PICTURE XX      VALUE SPACES.    
     02  FILLER                  PICTURE X       VALUE SPACES.    
     02  TCHP                    PICTURE XXX     VALUE SPACES.    
     02  FILLER                  PICTURE X(5)    VALUE SPACES.    
     02  1SEMP         PICTURE Z,ZZZ.99CR.                        
     02  FILLER                  PICTURE X(7)     VALUE SPACES.   
     02  2SEMP         PICTURE Z,ZZZ.99CR.                        
     02  FILLER                  PICTURE X(7)     VALUE SPACES.   
     02  AMT-PDP                 PICTURE Z,ZZZ.99CR.              
     02  FILLER                  PICTURE X(9)    VALUE SPACES.    
     02  BAL-DUEP                PICTURE Z,ZZZ.99CR.              
     02  FILLER                  PICTURE X(5)    VALUE SPACES.    
     02  TERMP                   PICTURE X       VALUE SPACES.    
     02  FILLER                  PICTURE X(7)    VALUE SPACES.    
     02  S-COPYP             PICTURE ZZZ9.                        
     02  FILLER              PICTURE X(5)    VALUE SPACES.        
     02  P-TCHP              PICTURE Z9.                          
     02  FILLER              PICTURE X(5)    VALUE SPACES.        
     02  TYPEP               PICTURE X       VALUE SPACES.        
     02  FILLER              PICTURE XX      VALUE SPACES.        
     02  FILLERCOM           PICTURE X(12)   VALUE SPACES.        
01   TOTALS-LINE REDEFINES DETAIL-LINE .                      
     02  FILLER                  PICTURE X.                       
     02  TOTAL-LINE-ID           PICTURE X(30).                   
     02  FILLER                  PICTURE XX.                      
     02  TOTAL-LINE-COUNT        PICTURE ZZZ,ZZZ,ZZZ,ZZ9.         
     02  FILLER                  PICTURE X(85).                   
01   TEACHER-MASTER-WORK .                                    
     02  ID-NUMBER.                                               
         03  ZIP-CODE            PICTURE   9(5).                  
             03  SCH-NUMBER      PICTURE 999.                     
         03  EDITION             PICTURE    99.                   
         03  TCH-NUMBER          PICTURE   999.                   
         03  CHECK-DIGIT         PICTURE  9.                      
     02  REST-OF-MASTER-T.                                        
         03  CURRENT-COPY.                                        
             04  STUD-COPY       PICTURE    9(4).                 
             04  XSTUDCOPY REDEFINES STUD-COPY  PICTURE X(4).     
             04  PAID-TCH        PICTURE    99.                   
             04  XPAIDTCH REDEFINES PAID-TCH  PICTURE XX.         
             04  RECS            PICTURE    99.                   
             04  FILM            PICTURE    99.                   
             04  XTC             PICTURE    99.                   
         03  CURRENT-BLANK  REDEFINES CURRENT-COPY.               
             04  STUD-BLANK              PICTURE  X(4).           
             04  TCH-BLANK               PICTURE   XX.            
             04  REC-BLANK               PICTURE   XX.            
             04  FILM-BLANK              PICTURE   XX.            
                 04  SPECIALE REDEFINES FILM-BLANK.               
                     05  1ST-SPEC  PICTURE X.                     
                     05  2ND-SPEC  PICTURE X.                     
             04  XTC-BLANK               PICTURE   XX.            
         03  AMOUNT-PAID    PICTURE S9(4)V99 COMPUTATIONAL.       
         03  TERM                PICTURE X.                       
             88  TERM1   VALUE "1".                               
             88  TERM2   VALUE "2".                               
             88  TERM3   VALUE "3".                               
             88  TERM4   VALUE "4".                               
             88  TERM5   VALUE "5".                               
             88  TERM6   VALUE "6".                               
             88  TERM7   VALUE "7".                               
             88  TERM8   VALUE "8".                               
         03  GRADE-AP            PICTURE  XX.                     
         03  TEACHER-NAME        PICTURE  X(25).                  
         03  PROMO-KEY           PICTURE  X(5).                   
         03  BILL-TO             PICTURE  X.                      
             88  BILL1   VALUE "1".                          
             88  BILL2   VALUE "2".                               
             88  BILL3   VALUE "3".                               
             88  BILL4   VALUE "4".                               
             88  BILL5   VALUE "5".                               
             88  BILL6   VALUE "6".                               
         03  BOARD-NUMBER        PICTURE  X(5).                   
         03  ISSUE-DATES.                                         
             04  BEGINN.                                          
                 05  ISS         PICTURE   99.                    
                 05  YEAR        PICTURE    9.                    
         04  BEGINNN REDEFINES  BEGINN.                           
             05  BEGIN           PICTURE 999.                     
             04  EXPIREE.                                         
                 05  ISS         PICTURE   99.                    
                 05  YEAR        PICTURE   9.                     
         04  EXPIREEE  REDEFINES  EXPIREE.                        
             05  EXPIRE          PICTURE 999.                     
         03  CHARGES         COMPUTATIONAL.                       
             04  FIRST-YR-TERM   PICTURE  S9(4)V99.               
             04  SECOND-SEM      PICTURE  S9(4)V99.               
             04  POSTAL          PICTURE  S9(3)V99.               
             04  OTHER           PICTURE  S9(3)V99.               
             04  BALANCE-DUE     PICTURE  S9(4)V99.               
         03  REVISION-ONE.                                        
             04  REV1-STUD       PICTURE     9(4).                
             04  REV1-TCH        PICTURE       99.                
             04  REV1-REC        PICTURE       99.                
             04  REV1-FILM       PICTURE       99.                
             04  REV1-ISSUE.                                      
                 05  REV1-ISS    PICTURE       99.                
                 05  REV1-YEAR   PICTURE        9.                
         03  REVISION-ONE-BLANK  REDEFINES REVISION-ONE           
                         PICTURE X(13).                           
         03  REVISION-TWO.                                        
             04  REV2-STUD       PICTURE     9(4).                
             04  REV2-TCH        PICTURE       99.                
             04  REV2-REC        PICTURE       99.                
             04  REV2-FILM       PICTURE       99.                
             04  REV2-ISSUE.                                      
                 05  REV2-ISS    PICTURE       99.                
                 05  REV2-YEAR   PICTURE        9.                
         03  REVISION-TWO-BLANK  REDEFINES  REVISION-TWO          
                          PICTURE X(13).                          
         03  REVISION-THREE.                                      
             04  REV3-STUD       PICTURE     9(4).                
             04  REV3-TCH        PICTURE       99.                
             04  REV3-REC        PICTURE       99.                
             04  REV3-FILM       PICTURE       99.                
             04  REV3-ISSUE.                                      
                 05  REV3-ISS    PICTURE       99.                
                 05  REV3-YEAR   PICTURE        9.                
         03  REVISION-THREE-BLANK REDEFINES  REVISION-THREE       
                         PICTURE  X(13).                          
         03  TYPE-MASTER         PICTURE X.                       
         03  MASTER-HOLD-CODE    PICTURE X.                       
         03  LAST-SEMESTER.                                       
             04  LAST-STUD       PICTURE   X(4).                  
       04  LAST-STUN REDEFINES LAST-STUD PICTURE 9(4).            
             04  LAST-TCH        PICTURE    XX.                   
       04  LAST-TCN  REDEFINES LAST-TCH  PICTURE 99.              
             04  LAST-XTC        PICTURE    XX.                   
             04  LAST-REC        PICTURE    XX.                   
         03  LASN REDEFINES LAST-SEMESTER.                        
         04  LASN-STUD  PICTURE 9(4).                             
         04  LASN-TCH   PICTURE 99.                               
         04  FILLER     PICTURE X(4).                             
         03  TRANS-INFO.                                          
             04  MTRANS-CODE     PICTURE    X.                    
             04  MBATCH-NUMBER   PICTURE   X(6).                  
             04  MMAIL-NUMBER    PICTURE  XX.                     
         03  FILLER              PICTURE   X(11).                 
01   SCHOOL-MASTER-WORK .                                     
     02  ID-NUMBER.                                               
         03  ZIP-CODE            PICTURE      9(5).               
         03  SCH-NUMBER          PICTURE      999.                
         03  ED-TCH-BLANK        PICTURE   X(5).                  
         03  CHECK-DIGIT         PICTURE        9.                
     02  REST-OF-MASTER-S.                                        
         03  SCHOOL-NAME         PICTURE      X(20).              
         03  STREET-ADDRESS      PICTURE      X(20).              
         03  CITY-NAME           PICTURE      X(20).              
         03  STATE-ALPHA         PICTURE       XX.                
         03  STATE-CODE          PICTURE       XX.                
         03  SR-CODE             PICTURE       XX.                
         03  COUNTY              PICTURE      XXX.                
         03  PP-ZONE             PICTURE        X.                
         03  NEW-ZIP             PICTURE X(8).                    
         03  HOLD-SCH            PICTURE          X.              
         03  SEX                   PICTURE  X.                    
         03  TYPESCH                 PICTURE  XX.                 
         03  SCHSIZE                         PICTURE  X(4).       
         03  STRANS-INFO.                                         
             04  STRANS-CODE     PICTURE       X.                 
             04  SBATCH-NUMBER   PICTURE      X(6).               
             04  SMAIL-NUMBER    PICTURE       XX.                
01   ERROR-TABLE .                                            
     02  ERROR-MSGS.                                              
     03  EM01 PICTURE X(27)  VALUE "01.1ST REC IPMAST NOT SCHL.". 
     03  EM02 PICTURE X(27)  VALUE "02.SAVE IP ENTER PROG DEPT.". 
     03  EM03 PICTURE X(27)  VALUE "03.END OF COACH AP CORRECT.". 
     02  ERR-MSGS REDEFINES ERROR-MSGS.                           
         03  MSG     PICTURE X(27) OCCURS 03 TIMES.               
01   CONSOLE-OUTPUT-AREA .                                    
     02  FILLER              PICTURE X(10)   VALUE ".A230S   .".  
     02  CONSOLE-MSG         PICTURE X(60)   VALUE SPACES.        
01   COMRG-INFO .                                             
     02  COM-VARDATE         PICTURE X(8)    VALUE SPACES.        
     02  COM-DATE            PICTURE X(6)    VALUE SPACES.        
     02  COM-JULDAY          PICTURE X(3)    VALUE SPACES.        
     02  COM-PROGNAME        PICTURE X(8)    VALUE SPACES.        
     02  COM-PREPASS         PICTURE X(11)   VALUE SPACES.        
     02  COM-UPSI            PICTURE X(8)    VALUE SPACES.        
     02  COM-TIME            PICTURE X(8)    VALUE SPACES.        
     02  FILLER              PICTURE X(4)    VALUE SPACES.        
PROCEDURE  DIVISION.                                              
HSK.                                                              
     OPEN INPUT  INPUT-MASTER-FILE                                
         OUTPUT OUTPUT-MASTER-FILE.                               
     MOVE SPACES TO PRINTER,                                      
         SCHOOL-MASTER-WORK  TEACHER-MASTER-WORK.                 
     MOVE SPACES TO DETAIL-LINE.                                  
GET-DATE.                                                         
     ENTER MACRO "GETCOMRG" USING COMRG-INFO.                            
     MOVE COM-VARDATE TO RPT-DATE.                                
HEADING-ROUTINE.                                                  
     ADD 1 TO PAGE-CTR.                                           
     MOVE PAGE-CTR TO PAGE-EDIT.                                  
     MOVE HDR1 TO PRINTER.                                        
     MOVE "1" TO CC.                                              
     PERFORM W-REC.                                               
     MOVE HDR2 TO PRINTER.                                        
     MOVE "0" TO CC.                                              
     PERFORM W-REC.                                               
     MOVE "0" TO CC.                                              
     PERFORM W-REC.                                               
     MOVE ZEROS TO LINE-COUNT.                                    
READ-MASTER.                                                      
     READ INPUT-MASTER-FILE AT END GO TO EOJ.                     
     ADD 1 TO RECORDS-IN.                                         
START-WITH-SCHOOL-RECORD.                                         
     IF FIRST-RECORD IS EQUAL TO "Y" AND NOT SCH-RECORD           
         MOVE 01 TO ERR-INC  PERFORM ERR-LOOKUP                   
         MOVE 02 TO ERR-INC GO TO ERR-LOOKUP.                     
     MOVE "N" TO FIRST-RECORD.                                    
TST-IF-SCHOOL.                                                    
     IF SCH-RECORD                                                
         MOVE INPUT-SCHOOL-MASTER TO SCHOOL-MASTER-WORK           
         GO TO WRITE-SCHOOL-RECORD.                               
MOVE-TCH-TO-WORKAREA.                                             
     MOVE INPUT-TEACHER-MASTER TO TEACHER-MASTER-WORK.            
     IF TYPE-MASTER IS EQUAL TO "A" NEXT SENTENCE ELSE            
         GO TO WRITE-TEACHER-RECORD.                              
SELECT-COACH.                                                     
     IF EDITION OF TEACHER-MASTER-WORK IS = "34" NEXT SENTENCE    
         ELSE GO TO SELECT-FORECAST.                              
     IF TYPESCH IS = "6E" OR                                      
        TYPESCH IS = "8E" OR                                      
        TYPESCH IS = "CE" NEXT SENTENCE ELSE                      
         GO TO CONT-CHK-SCHL-AP.                                  
     IF GRADE-AP IS = "8Q"                                        
         GO TO WRITE-TEACHER-RECORD ELSE                          
         MOVE "8Q" TO GRADE-AP GO TO SET-UP-PRINTER.              
CONT-CHK-SCHL-AP.                                                 
     IF TYPESCH IS = " S" NEXT SENTENCE ELSE                      
         GO TO TST-J3Q.                                           
     IF GRADE-AP IS = "1Q"   GO TO WRITE-TEACHER-RECORD.          
     MOVE "1Q" TO GRADE-AP GO TO SET-UP-PRINTER.                  
TST-J3Q.                                                          
     IF TYPESCH IS = " J" NEXT SENTENCE ELSE                      
         GO TO TST-JS2Q.                                          
     IF GRADE-AP IS = "3Q" GO TO WRITE-TEACHER-RECORD.            
     MOVE "3Q" TO GRADE-AP GO TO SET-UP-PRINTER.                  
TST-JS2Q.                                                         
     IF TYPESCH IS = "JS" NEXT SENTENCE ELSE                      
         GO TO TST-E8Q.                                           
     IF GRADE-AP IS = "2Q" GO TO WRITE-TEACHER-RECORD.            
     MOVE "2Q" TO GRADE-AP GO TO SET-UP-PRINTER.                  
TST-E8Q.                                                          
     IF TYPESCH IS = " E" NEXT SENTENCE ELSE                      
         GO TO TST-C4Q.                                           
     IF GRADE-AP IS = "8Q" GO TO WRITE-TEACHER-RECORD.            
     MOVE "8Q" TO GRADE-AP GO TO SET-UP-PRINTER.                  
TST-C4Q.                                                          
     IF TYPESCH IS = " C" NEXT SENTENCE ELSE                      
         GO TO TST-JCBQ.                                          
     IF GRADE-AP IS = "4Q" GO TO WRITE-TEACHER-RECORD.            
     MOVE "4Q" TO GRADE-AP GO TO SET-UP-PRINTER.                  
TST-JCBQ.                                                         
     IF TYPESCH IS = "JC" NEXT SENTENCE ELSE                      
         GO TO TST-CLAQ.                                          
     IF GRADE-AP IS = "BQ" GO TO WRITE-TEACHER-RECORD.            
     MOVE "BQ" TO GRADE-AP GO TO SET-UP-PRINTER.                  
TST-CLAQ.                                                         
     IF TYPESCH IS = "CL" NEXT SENTENCE ELSE                      
         GO TO WRITE-TEACHER-RECORD.                              
     IF  GRADE-AP IS = "AQ" GO TO WRITE-TEACHER-RECORD.           
     MOVE "AQ" TO GRADE-AP.                                       
SELECT-FORECAST.                                                  
     IF EDITION OF TEACHER-MASTER-WORK IS EQUAL TO "33"           
         NEXT SENTENCE ELSE GO TO WRITE-TEACHER-RECORD.           
     EXAMINE STUD-BLANK REPLACING ALL " " BY "0".                 
     EXAMINE TCH-BLANK  REPLACING ALL " " BY "0".                 
     EXAMINE REC-BLANK  REPLACING ALL " " BY "0".                 
     COMPUTE SINGLES-CTR = STUD-COPY - PAID-TCH - RECS.           
     IF SINGLES-CTR IS EQUAL TO 1                                 
             MOVE "1A" TO GRADE-AP                                
             GO TO SET-UP-PRINTER.                                
     GO TO WRITE-TEACHER-RECORD.                                  
SET-UP-PRINTER.                                                   
        IF LINE-COUNT IS = 56 PERFORM HEADING-ROUTINE.
     MOVE ZIP-CODE OF TEACHER-MASTER-WORK TO ZIPP.                
     MOVE SCH-NUMBER OF TEACHER-MASTER-WORK TO SCHP.              
     MOVE EDITION OF TEACHER-MASTER-WORK TO EDP.                  
     MOVE TCH-NUMBER OF TEACHER-MASTER-WORK TO TCHP.              
     MOVE TERM TO TERMP.                                          
     MOVE FIRST-YR-TERM TO 1SEMP.                                 
     MOVE SECOND-SEM TO 2SEMP.                                    
     MOVE AMOUNT-PAID TO AMT-PDP.                                 
     MOVE BALANCE-DUE TO BAL-DUEP.                                
     MOVE STUD-COPY TO S-COPYP.                                   
     MOVE PAID-TCH TO P-TCHP.                                     
     MOVE TYPE-MASTER TO TYPEP.                                   
     MOVE GRADE-AP OF TEACHER-MASTER-WORK TO FILLERCOM.           
     ADD 1 TO TOTAL-ADJUSTED-RECORDS.                             
     MOVE DETAIL-LINE TO PRINTER.                                 
     MOVE SPACES TO DETAIL-LINE.                                  
     MOVE " " TO CC.                                              
W-REC.                                                            
     ENTER MACRO "PRTSPOOL" USING PRINTER.                               
     ADD 1 TO LINE-COUNT.                                         
     MOVE SPACES TO DATAREA.                                      
WRITE-TEACHER-RECORD.                                             
     WRITE OUTPUT-TEACHER-MASTER FROM TEACHER-MASTER-WORK.        
     MOVE SPACES TO TEACHER-MASTER-WORK.                          
     MOVE ZEROS TO SINGLES-CTR.                                   
     ADD 1 TO RECORDS-OUT.                                        
     ADD 1 TO TEACHER-CTR.                                        
     GO TO READ-MASTER.                                           
WRITE-SCHOOL-RECORD.                                              
     WRITE OUTPUT-SCHOOL-MASTER FROM SCHOOL-MASTER-WORK.          
     MOVE SPACES TO SCHOOL-MASTER-WORK.                           
     ADD 1 TO RECORDS-OUT.                                        
     ADD 1 TO SCHOOL-CTR.                                         
     GO TO READ-MASTER.                                           
EOJ.                                                              
     PERFORM HEADING-ROUTINE.                                     
     MOVE SPACES TO TOTALS-LINE.                                  
     MOVE "RECORDS IN" TO TOTAL-LINE-ID.                          
     MOVE RECORDS-IN TO TOTAL-LINE-COUNT.                         
     MOVE TOTALS-LINE TO PRINTER.                                 
     MOVE "0" TO CC.                                              
     PERFORM W-REC.                                               
     MOVE "RECORDS OUT" TO TOTAL-LINE-ID.                         
     MOVE RECORDS-OUT TO TOTAL-LINE-COUNT.                        
     MOVE TOTALS-LINE TO PRINTER.                                 
     PERFORM W-REC.                                               
     MOVE "SCHOOLS OUT"  TO TOTAL-LINE-ID                         
     MOVE SCHOOL-CTR TO TOTAL-LINE-COUNT.                         
     MOVE TOTALS-LINE TO PRINTER.                                 
     PERFORM W-REC.                                               
     MOVE "TEACHERS OUT" TO TOTAL-LINE-ID.                        
     MOVE TEACHER-CTR TO TOTAL-LINE-COUNT.                        
     MOVE TOTALS-LINE TO PRINTER.                                 
     PERFORM W-REC.                                               
     MOVE "TOTAL ADJUSTED RECORDS" TO TOTAL-LINE-ID.              
     MOVE TOTAL-ADJUSTED-RECORDS TO TOTAL-LINE-COUNT.             
     MOVE TOTALS-LINE TO PRINTER.                                 
     PERFORM W-REC.                                               
CLOSE-FILES.                                                      
     CLOSE  INPUT-MASTER-FILE, OUTPUT-MASTER-FILE.                
     MOVE 03 TO ERR-INC PERFORM ERR-LOOKUP.                       
     ENTER MACRO "UNITCLOS".                                             
     STOP RUN.                                                    
ERR-LOOKUP.                                                       
     MOVE MSG (ERR-INC) TO CONSOLE-MSG.                           
     ENTER MACRO "CONSPOOL" USING CONSOLE-OUTPUT-AREA.                   
ABEND.                                                            
     ENTER MACRO "DUMPSTOP".                                             
THATS-ALL-FOLKS.                                                  
  )+,†