    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1
    FAA1.CBL    04-JUN-73  14:36

    0001    IDENTIFICATION  DIVISION.                                         
    0002    PROGRAM-ID.     "BB01A".                                          
    0003    AUTHOR.          WESTERN REGION PROGRAMMING STAFF.                
    0004    INSTALLATION.    FAA, WESTERN REGION.                             
    0005    DATE-WRITTEN.    16 MAR 1972.                                     
    0006    DATE-COMPILED. 04-JUN-73. 
    0007    REMARKS.                                                          
    0008        PURPOSE.                                                     
    0009        VALIDATE SIP-6 CARD INPUT AND PREPARE BATCH LISTING          
    0010        SHOWING CODING ERRORS AND TOTALS.                            
    0011        REVISIONS.                                                   
    0012        01-20-73                                                     
    0013        PROGRAMMER KEN ELSING, FAA WESTERN REGION.                   
    0014        MODIFY EDIT AND CHANGE HEADINGS.                             
    0015    ENVIRONMENT  DIVISION.                                            
    0016    CONFIGURATION  SECTION.                                           
    0017    SOURCE-COMPUTER. DECsystem-10. 
    0018    OBJECT-COMPUTER. PDP-10.                                          
    0019    INPUT-OUTPUT  SECTION.                                            
    0020    FILE-CONTROL.                                                     
    0021        SELECT  TRANS   ASSIGN DSK,                                  
    0022          RECORDING MODE IS ASCII.                                   
    0023                                                                     
    0024        SELECT  PRINT   ASSIGN DSK,                                  
    0025          RECORDING MODE IS ASCII.                                   
    0026                                                                     
    0027    DATA  DIVISION.                                                   
    0028    FILE  SECTION.                                                    
    0029    FD   TRANS                                                        
    0030        RECORD  CONTAINS   80  CHARACTERS                            
    0031        LABEL   RECORDS   ARE  STANDARD
    0032           VALUE OF IDENTIFICATION IS "TRANS DAT"
    0033        DATA    RECORD    IS   REC-IN.
    0034    01   REC-IN          PICTURE X(80).                               
    0035    FD   PRINT                                                        
    0036        RECORD  CONTAINS  133  CHARACTERS                            
    0037        LABEL RECORDS ARE STANDARD
    0038           VALUE OF IDENTIFICATION IS "PRINT DAT"
    0039           DATA RECORD IS PRINT-LINE.
    0040    01   PRINT-LINE.                                                  
    0041        03  FILLER          PICTURE X.                               
    0042        03  PRINT-IO        PICTURE X(132).                          
    0043        03  DETAIL-LINE REDEFINES PRINT-IO.                          
    0044            05  FILLER          PICTURE X(3).                        
    0045            05  BATCH-RPT       PICTURE X(3).                        
    0046            05  FILLER          PICTURE X(3).                        
    0047            05  REG-RPT         PICTURE X.                           
    0048            05  CST-CEN-RPT     PICTURE X(4).                        
    0049            05  FILLER          PICTURE X(4).                        
    0050            05  LOCATION-RPT    PICTURE X(4).                        
    0051            05  FILLER          PICTURE XX.                          
    0052            05  FAC-TYPE-RPT    PICTURE X(5).                        
    0053            05  FILLER          PICTURE XX.                          
    0054            05  FSNA-RPT        PICTURE X(4).                        
    0055            05  DASH1-RPT       PICTURE X.                           
    0056            05  FSNB-RPT        PICTURE X(3).                            P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-1
    FAA1.CBL    04-JUN-73  14:36

    0057            05  DASH2-RPT       PICTURE X.                           
    0058            05  FSNC-RPT        PICTURE X(3).                        
    0059            05  FSND-RPT        PICTURE X.                           
    0060            05  FILLER          PICTURE X.                           
    0061            05  EXCESS-FLAG-RPT PICTURE X.                           
    0062            05  FILLER          PICTURE XX.                          
    0063            05  EQUIP-RPT       PICTURE X(14).                       
    0064            05  FILLER          PICTURE XX.                          
    0065            05  OWN-RPT         PICTURE X.                           
    0066            05  FILLER          PICTURE X(3).                        
    0067            05  ASS-CODE-RPT    PICTURE XX.                          
    0068            05  FILLER          PICTURE X(3).                        
    0069            05  QUANTITY-RPT    PICTURE XX.                          
    0070            05  FILLER          PICTURE X.                           
    0071            05  UNIT-VALUE-RPT  PICTURE Z,ZZZ,ZZZ.99.                
    0072            05  TOTAL-VALUE-RPT PICTURE ZZZ,ZZZ,ZZZ.99.              
    0073            05  FILLER          PICTURE X(3).                        
    0074            05  TRANS-CODE-RPT  PICTURE XX.                          
    0075            05  FILLER          PICTURE X(3).                        
    0076            05  MONTH-RPT       PICTURE X.                           
    0077            05  DASH3-RPT       PICTURE X.                           
    0078            05  YEAR-RPT        PICTURE XX.                          
    0079            05  FILLER          PICTURE X(3).                        
    0080            05  DOC-NUM-RPT     PICTURE X(5).                        
    0081            05  FILLER          PICTURE X(10).                       
    0082        03  TOTAL-LINE-RPT REDEFINES PRINT-IO.                       
    0083            05  FILLER          PICTURE X(19).                       
    0084            05  T-1             PICTURE X(8).                        
    0085            05  T-2             PICTURE X(6).                        
    0086            05  T-3             PICTURE X(8).                        
    0087            05  FILLER          PICTURE X(10).                       
    0088            05  T-4             PICTURE X(8).                        
    0089            05  FILLER          PICTURE X(6).                        
    0090            05  DECR-CNT-RPT    PICTURE ZZ,ZZZ.                      
    0091            05  FILLER          PICTURE X(3).                        
    0092            05  DECR-AMT-RPT    PICTURE ZZZ,ZZZ,ZZZ.ZZCR.            
    0093            05  FILLER          PICTURE X(3).                        
    0094            05  T-5             PICTURE X(8).                        
    0095            05  FILLER          PICTURE X(3).                        
    0096            05  INCR-CNT-RPT    PICTURE ZZ,ZZZ.                      
    0097            05  FILLER          PICTURE XX.                          
    0098            05  INCR-AMT-RPT    PICTURE ZZZ,ZZZ,ZZZ.ZZCR.            
    0099            05  FILLER          PICTURE X(4).                        
    0100    WORKING-STORAGE  SECTION.                                         
    0101    77   PRT-CTRL   PICTURE X.                               
    0102    77   ERROR-IND           PICTURE X   VALUE "0".                   
    0103    77   FIRST-RCD-IND       PICTURE X   VALUE "0".                   
    0104    77   END-JOB-IND         PICTURE X   VALUE "0".                   
    0105    77   WS-REGION-HOLD      PICTURE X   VALUE SPACE.                 
    0106    77   WS-BATCH-HOLD       PICTURE X(3)  VALUE SPACE.               
    0107    77   WS-DOC-NUM          PICTURE X(5)  VALUE SPACE.               
    0108    77   AC-HOLD             PICTURE XX  VALUE SPACE.                 
    0109    77   TR-HOLD             PICTURE XX  VALUE SPACE.                 
    0110    77   SUB         PICTURE S99 USAGE IS COMPUTATIONAL.              
    0111    77   LINE-CTR        PICTURE S99         COMPUTATIONAL   VALUE 0. 
    0112    77   PAGE-CTR        PICTURE S9(3)       COMPUTATIONAL   VALUE 0.     P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-2
    FAA1.CBL    04-JUN-73  14:36

    0113    77   DOC-DECR-CTR    PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0114    77   DOC-INCR-CTR    PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0115    77   BATCH-DECR-CTR  PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0116    77   BATCH-INCR-CTR  PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0117    77   BATCH-CTR       PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0118    77   REGION-CTR      PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0119    77   OV-CTR          PICTURE S9(5)       COMPUTATIONAL   VALUE 0. 
    0120    77   DOC-DECR-AMT    PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0121    77   DOC-INCR-AMT    PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0122    77   BATCH-DECR-AMT  PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0123    77   BATCH-INCR-AMT  PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0124    77   BATCH-AMT       PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0125    77   REGION-AMT      PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0126    77   OV-AMT          PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0127    77   TEMP-TOTAL      PICTURE S9(9)V99    COMPUTATIONAL   VALUE 0. 
    0128    77   UNIT-VALUE-WS   PICTURE S9(9)V99  COMPUTATIONAL   VALUE 0.   
    0129    77   QUAN-WS         PICTURE S9(3)     COMPUTATIONAL   VALUE 0.   
    0130    01   WORK-CONSTANTS.                                              
    0131        03  HYPHEN      PICTURE X   VALUE  "-".                      
    0132        03  DOCMNT      PICTURE X(8)   VALUE "DOCUMENT".             
    0133        03  TOTL        PICTURE X(8)   VALUE "TOTAL   ".             
    0134        03  INCR        PICTURE X(8)   VALUE "INCREASE".             
    0135        03  DECR        PICTURE X(8)   VALUE "DECREASE".             
    0136        03  ITEM        PICTURE X(6)   VALUE "ITEM  ".               
    0137        03  BATCH       PICTURE X(8)   VALUE "  BATCH ".             
    0138        03  TVAL        PICTURE X(8)   VALUE "  VALUE ".             
    0139        03  TCNT        PICTURE X(8)   VALUE "  COUNT ".             
    0140        03  TNET        PICTURE X(8)   VALUE "   NET  ".             
    0141    01   TRANS-INPUT-WS              PICTURE X(80) VALUE SPACES.      
    0142    01   TR-IN-WS-X REDEFINES TRANS-INPUT-WS.                         
    0143        03  FILL-BB                 PICTURE XX.                      
    0144        03  REGION-IN               PICTURE X.                       
    0145        03  CST-CEN-CODE-IN         PICTURE X(4).                    
    0146        03  LOCATION-IN             PICTURE X(4).                    
    0147        03  FAC-ID-IN.                                               
    0148            05  SYSTEM-IN           PICTURE X.                       
    0149            05  CATEGORY-IN         PICTURE X.                       
    0150            05  FAC-TYPE-IN.                                         
    0151                07  FAC-TYPE-1ST    PICTURE X.                       
    0152                07  FAC-TYPE-2ND    PICTURE X.                       
    0153            05  M-CODE              PICTURE X.                       
    0154        03  FSN-IN.                                                  
    0155            05  FSNA-IN             PICTURE X(4).                    
    0156            05  FSNB-IN             PICTURE X(3).                    
    0157            05  FSNLAST-4-IN.                                        
    0158                07  FSNC-IN         PICTURE X(3).                    
    0159                07  FSND-IN         PICTURE X.                       
    0160        03  EQUIPMENT-IN            PICTURE X(14).                   
    0161        03  OWN-IN                  PICTURE X.                       
    0162        03  ASSET-CODE-IN           PICTURE XX.                      
    0163        03  QUANTITY-IN             PICTURE XX.                      
    0164        03  QUANTITY-IN-9 REDEFINES QUANTITY-IN PICTURE 99.          
    0165        03  UNIT-VALUE-IN           PICTURE 9(7)V99.                 
    0166        03  UNIT-VALUE-X REDEFINES UNIT-VALUE-IN PICTURE X(9).       
    0167        03  TOTAL-VALUE-IN          PICTURE 9(7)V99.                 
    0168        03  TOTAL-VALUE-X REDEFINES TOTAL-VALUE-IN PICTURE X(9).         P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-3
    FAA1.CBL    04-JUN-73  14:36

    0169        03  TRANS-CODE-IN           PICTURE XX.                      
    0170        03  ACTION-DATE-IN.                                          
    0171            05  MONTH-IN            PICTURE X.                       
    0172            05  YEAR-IN             PICTURE XX.                      
    0173        03  DOC-NUM-IN              PICTURE X(5).                    
    0174        03  BATCH-NUM-IN            PICTURE X(3).                    
    0175        03  FILLER                  PICTURE X(3).                    
    0176    01   HEAD-1.                                                      
    0177        03  FILLER      PICTURE X(16)  VALUE "RIN_  BB01AR1   ".     
    0178        03  REGION-HDR  PICTURE X(21)  VALUE SPACES.                 
    0179        03  FILLER      PICTURE X(60)  VALUE "     PERSONAL PROPERTY 
    0180  -     "INVENTORY BATCH LISTING              ".                     
    0181        03  DATE-HDR    PICTURE X(11)  VALUE SPACES.                 
    0182        03  FILLER      PICTURE X(20)  VALUE "               PAGE ". 
    0183        03  PAGE-HDR    PICTURE ZZ.                                  
    0184        03  FILLER      PICTURE X     VALUE SPACE.                   
    0185    01   HEAD-2.                                                      
    0186        03  FILLER      PICTURE X(20) VALUE "  BATCH  COST     LO".  
    0187        03  FILLER      PICTURE X(20) VALUE "CA  FAC    FED STK  ".  
    0188        03  FILLER      PICTURE X(20) VALUE "NO     EQUIP  TYPE  ".  
    0189        03  FILLER      PICTURE X(20) VALUE "    O ASSET QUAN    ".  
    0190        03  FILLER      PICTURE X(3) VALUE SPACE.                    
    0191        03  FILLER      PICTURE X(20) VALUE "UNIT       SUMMARY  ".  
    0192        03  FILLER      PICTURE X(20) VALUE " TRS  ACTION  DOCMT ".  
    0193        03  FILLER      PICTURE X(9) VALUE SPACE.                    
    0194    01   HEAD-3.                                                      
    0195        03  FILLER      PICTURE X(20) VALUE "    NO    CENTER  TI".  
    0196        03  FILLER      PICTURE X(20) VALUE "ON   TYPE  FCLT OR I".  
    0197        03  FILLER      PICTURE X(20) VALUE "NST      SERIAL  NO ".  
    0198        03  FILLER      PICTURE X(20) VALUE "    W  CODE TITY    ".  
    0199        03  FILLER      PICTURE X(3) VALUE SPACE.                    
    0200        03  FILLER      PICTURE X(20) VALUE "PRICE      VALUE    ".  
    0201        03  FILLER      PICTURE X(20) VALUE "  CD   M YR      NO ".  
    0202        03  FILLER      PICTURE X(9) VALUE SPACE.                    
    0203    01   ERROR-LN        PICTURE X(132)  VALUE SPACES.                
    0204    01   ERROR-LINE REDEFINES ERROR-LN.                               
    0205        03  FILLER      PICTURE X(3).                                
    0206        03  BTCH-ERR    PICTURE XXX.                                 
    0207        03  FILLER      PICTURE XXX.                                 
    0208        03  CC-ERR.                                                  
    0209            05  REG-ERR PICTURE X.                                   
    0210            05  CST-ERR PICTURE X(4).                                
    0211        03  FILLER      PICTURE X(4).                                
    0212        03  LOC-ERR     PICTURE X(4).                                
    0213        03  FILLER      PICTURE XX.                                  
    0214        03  FACTPE-ERR  PICTURE X(5).                                
    0215        03  FILLER      PICTURE XX.                                  
    0216        03  FSN.                                                     
    0217            05  FSNA-ERR PICTURE X(4).                               
    0218            05  FILLER   PICTURE X.                                  
    0219            05  FSNB-ERR PICTURE XXX.                                
    0220            05  FILLER   PICTURE X.                                  
    0221            05  FSNC-ERR PICTURE XXX.                                
    0222            05  FSND-ERR PICTURE X.                                  
    0223        03  FILLER       PICTURE X(4).                               
    0224        03  EQUIP-ERR    PICTURE X(14).                                  P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-4
    FAA1.CBL    04-JUN-73  14:36

    0225        03  FILLER     PICTURE   XX.                                 
    0226        03  OWN-ERR      PICTURE X.                                  
    0227        03  FILLER       PICTURE XXX.                                
    0228        03  AC-ERR      PICTURE XX                              
    0229        03  FILLER      PICTURE XXX.                                 
    0230        03  QUAN-ERR    PICTURE XX.                                  
    0231        03  FILLER      PICTURE XXX.                                 
    0232        03  PRICE-ERR   PICTURE X(10).                               
    0233        03  FILLER      PICTURE X(3).                                
    0234        03  VALUE-ERR   PICTURE X(11).                               
    0235        03  FILLER      PICTURE X(3).                                
    0236        03  TR-CD-ERR   PICTURE XX.                                  
    0237        03  FILLER      PICTURE XXX.                                 
    0238        03  ACT-DTE-ERR PICTURE XXXX.                                
    0239        03  FILLER      PICTURE X(6).
    0240        03  DOC-NO-ERR  PICTURE X(5).                                
    0241        03  ERR-FLG     PICTURE X(10).                               
    0242    01   ADD-TBLS.                                                    
    0243        03  FILLER      PICTURE X(40)   VALUE                        
    0244            "0A0B0C0D0E0F0G00010203040506070851535557".              
    0245    01   ADDS        REDEFINES  ADD-TBLS.                             
    0246        03  INCR-TBL    OCCURS  37  TIMES  PICTURE XX.               
    0247    01   DELETES.                                                     
    0248        03  FILLER      PICTURE X(44)   VALUE                        
    0249            "1A1B1C1D1F1G1H1I1J1K10111214151718212325271M".          
    0250    01   SUB-DR REDEFINES DELETES.                                    
    0251        03  DECR-TBL    OCCURS  22  TIMES  PICTURE XX.               
    0252    01   ASSET-CODE-TABLE.                                            
    0253        03  FILLER  VALUE  "11" PICTURE  XX.                         
    0254        03  FILLER  VALUE  "12" PICTURE  XX.                         
    0255        03  FILLER  VALUE  "13" PICTURE  XX.                         
    0256        03  FILLER  VALUE  "14" PICTURE  XX.                         
    0257        03  FILLER  VALUE  "15" PICTURE  XX.                         
    0258        03  FILLER  VALUE  "16" PICTURE  XX.                         
    0259        03  FILLER  VALUE  "20" PICTURE  XX.                         
    0260        03  FILLER  VALUE  "41" PICTURE  XX.                         
    0261        03  FILLER  VALUE  "42" PICTURE  XX.                         
    0262        03  FILLER  VALUE  "43" PICTURE  XX.                         
    0263        03  FILLER  VALUE  "44" PICTURE  XX.                         
    0264        03  FILLER  VALUE  "45" PICTURE  XX.                         
    0265        03  FILLER  VALUE  "46" PICTURE  XX.                         
    0266        03  FILLER  VALUE  "61" PICTURE  XX.                         
    0267        03  FILLER  VALUE  "62" PICTURE  XX.                         
    0268        03  FILLER  VALUE  "63" PICTURE  XX.                         
    0269        03  FILLER  VALUE  "64" PICTURE  XX.                         
    0270        03  FILLER  VALUE  "81" PICTURE  XX.                         
    0271        03  FILLER  VALUE  "82" PICTURE  XX.                         
    0272        03  FILLER  VALUE  "83" PICTURE  XX.                         
    0273    01   G1  REDEFINES ASSET-CODE-TABLE.                              
    0274        03  AC OCCURS 20 TIMES    PICTURE XX.                        
    0275    01   TABLE-B.                                                     
    0276        03  REGION-TABLE.                                            
    0277            05  FILLER  PICTURE X(22) VALUE "4WESTERN REGION       ".
    0278            05  FILLER  PICTURE X(22) VALUE "DROCKY MOUNTAIN REGION".
    0279            05  FILLER  PICTURE X(22) VALUE "SNORTHWEST REGION     ".
    0280            05  FILLER  PICTURE X(22) VALUE "ENEW ENGLAND REGION   ".    P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-5
    FAA1.CBL    04-JUN-73  14:36

    0281            05  FILLER  PICTURE X(22) VALUE "CGREAT LAKES REGION   ".
    0282            05  FILLER  PICTURE X(22) VALUE "1EASTERN REGION       ".
    0283            05  FILLER  PICTURE X(22) VALUE "2SOUTHWEST REGION     ".
    0284            05  FILLER  PICTURE X(22) VALUE "3CENTRAL REGION       ".
    0285            05  FILLER  PICTURE X(22) VALUE "5ALASKAN REGION       ".
    0286            05  FILLER  PICTURE X(22) VALUE "6PACIFIC REGION       ".
    0287            05  FILLER  PICTURE X(22) VALUE "7SOUTHERN REGION      ".
    0288        03  REG-TBL REDEFINES REGION-TABLE                           
    0289                    OCCURS 11 TIMES PICTURE X(22).                   
    0290    01   REG-BRKDWN  PICTURE X(22)   VALUE SPACES.                    
    0291    01   R-BRK REDEFINES REG-BRKDWN.                                  
    0292        03  REG-CODE-TBL        PICTURE X.                           
    0293        03  REG-NAME-TBL        PICTURE X(21).                       
    0294    01   NUMERIC-CHECK-AREA.                                          
    0295        03  NUMB-TEST-9     PICTURE X(10) VALUE "0000000000".        
    0296        03  N-T-9 REDEFINES NUMB-TEST-9.                             
    0297            05  NINE-TEST   PICTURE X(9).                            
    0298            05  FILLER      PICTURE X.                               
    0299        03  NUMB-TEST-5     PICTURE X(6)  VALUE "000000".            
    0300        03  N-T-5 REDEFINES NUMB-TEST-5.                             
    0301            05  FIVE-TEST   PICTURE X(5).                            
    0302            05  FILLER      PICTURE X.                               
    0303        03  NUMB-TEST-4     PICTURE X(5)  VALUE "00000".             
    0304        03  N-T-4 REDEFINES NUMB-TEST-4.                             
    0305            05  FOUR-TEST   PICTURE X(4).                            
    0306            05  FILLER      PICTURE X.                               
    0307        03  NUMB-TEST-3     PICTURE X(4)  VALUE "0000".              
    0308        03  N-T-3 REDEFINES NUMB-TEST-3.                             
    0309            05  THREE-TEST  PICTURE X(3).                            
    0310            05  FILLER      PICTURE X.                               
    0311    PROCEDURE  DIVISION.                                              
    0312        OPEN INPUT  TRANS,                                           
    0313             OUTPUT PRINT.                                           
    0314        DISPLAY "KEY IN TODAYS DATE - IE_ JAN 10 1973" UPON CONSOLE.     
    0315        ACCEPT DATE-HDR.
    0316    010-READ-INPUT.                                                   
    0317        READ TRANS INTO TRANS-INPUT-WS AT END                        
    0318            MOVE "1" TO END-JOB-IND                                  
    0319            GO TO 350-INPUT-AT-END.                                  
    0320        IF FIRST-RCD-IND = "0"                                       
    0321            PERFORM 280-FIND-REGION-HDR THRU 290-FIND-REG-EXIT       
    0322            PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXIT         
    0323            MOVE "1" TO FIRST-RCD-IND                                
    0324            MOVE REGION-IN TO WS-REGION-HOLD                         
    0325            MOVE BATCH-NUM-IN TO WS-BATCH-HOLD                       
    0326            MOVE DOC-NUM-IN TO WS-DOC-NUM.                           
    0327    020-COMPARE-INPUT-TO-HOLDS.                                       
    0328        IF WS-REGION-HOLD NOT = REGION-IN                            
    0329            PERFORM 260-END-REGION-RTN THRU 270-END-REGION-EXIT      
    0330            GO TO 030-SET-UP-PRINT-LINE.                             
    0331        IF WS-BATCH-HOLD NOT = BATCH-NUM-IN                          
    0332            PERFORM 240-END-BATCH-RTN THRU 250-BATCH-RTN-EXIT        
    0333            MOVE ZEROS TO PAGE-CTR LINE-CTR                          
    0334            PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXIT         
    0335            GO TO 030-SET-UP-PRINT-LINE.                             
    0336        IF WS-DOC-NUM NOT = DOC-NUM-IN                                   P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-6
    FAA1.CBL    04-JUN-73  14:36

    0337            PERFORM 220-END-DOC-RTN THRU 230-END-DOC-EXIT.           
    0338    030-SET-UP-PRINT-LINE.                                            
    0339        MOVE REGION-IN       TO REG-RPT.                             
    0340        MOVE CST-CEN-CODE-IN TO CST-CEN-RPT.                         
    0341        MOVE LOCATION-IN     TO LOCATION-RPT.                        
    0342        MOVE FAC-ID-IN       TO FAC-TYPE-RPT.                        
    0343        MOVE FSNA-IN         TO FSNA-RPT.                            
    0344        MOVE FSNB-IN         TO FSNB-RPT.                            
    0345        MOVE FSNC-IN         TO FSNC-RPT.                            
    0346        MOVE FSND-IN         TO FSND-RPT.                            
    0347        MOVE HYPHEN          TO DASH1-RPT, DASH2-RPT, DASH3-RPT.     
    0348        MOVE EQUIPMENT-IN    TO EQUIP-RPT.                           
    0349        MOVE OWN-IN          TO OWN-RPT.                             
    0350        MOVE ASSET-CODE-IN TO ASS-CODE-RPT.                          
    0351        MOVE QUANTITY-IN     TO QUANTITY-RPT.                        
    0352        MOVE UNIT-VALUE-IN   TO UNIT-VALUE-RPT.                      
    0353        MOVE TRANS-CODE-IN   TO TRANS-CODE-RPT.                      
    0354        MOVE MONTH-IN        TO MONTH-RPT.                           
    0355        MOVE YEAR-IN         TO YEAR-RPT.                            
    0356        MOVE DOC-NUM-IN      TO DOC-NUM-RPT.                         
    0357        MOVE BATCH-NUM-IN    TO BATCH-RPT.                           
    0358        MOVE CST-CEN-CODE-IN TO FOUR-TEST.                           
    0359        IF NUMB-TEST-4 IS NOT NUMERIC                                
    0360            MOVE "****" TO CST-ERR                                   
    0361             MOVE "1" TO ERROR-IND.                                  
    0362        IF LOCATION-IN IS NOT ALPHABETIC                             
    0363            MOVE "****" TO LOC-ERR                                   
    0364             MOVE "1" TO ERROR-IND.                                  
    0365        IF SYSTEM-IN = SPACE OR                                      
    0366            CATEGORY-IN = SPACE OR                                   
    0367            FAC-TYPE-2ND = SPACE OR                                  
    0368            M-CODE = SPACE                                           
    0369            GO TO 040-FACILITY-CODE-ERROR.                           
    0370        IF FAC-TYPE-1ST < "0" OR FAC-TYPE-1ST > "9"                  
    0371            GO TO 040-FACILITY-CODE-ERROR   ELSE                     
    0372            GO TO 050-FSN-CHECK.                                     
    0373    040-FACILITY-CODE-ERROR.                                          
    0374        MOVE "*****" TO FACTPE-ERR.                                  
    0375        MOVE "1" TO ERROR-IND.                                       
    0376    050-FSN-CHECK.                                                    
    0377        IF FSNA-IN = "FCLT"                                          
    0378            GO TO 060-EQP-CK.                                        
    0379        IF FSNA-IN = "INST"                                          
    0380            GO TO 070-CHG-CK.                                        
    0381        MOVE FSNA-IN TO FOUR-TEST.                                   
    0382        IF NUMB-TEST-4 NOT NUMERIC                                   
    0383            MOVE "****" TO FSNA-ERR                                  
    0384            MOVE "1" TO ERROR-IND.                                   
    0385        GO TO 080-CHECK-MID-3.                                       
    0386    060-EQP-CK.                                                       
    0387        IF FSNB-IN NOT = "EQP"                                       
    0388            PERFORM 090-FSN-MID-3-ERROR.                             
    0389        GO TO 120-LAST-FOUR-SPACES.                                  
    0390    070-CHG-CK.                                                       
    0391        IF FSNB-IN NOT = "CHG"                                       
    0392            PERFORM 090-FSN-MID-3-ERROR.                                 P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-7
    FAA1.CBL    04-JUN-73  14:36

    0393        GO TO 120-LAST-FOUR-SPACES.                                  
    0394    080-CHECK-MID-3.                                                  
    0395        IF FSNB-IN = "WE0" OR "RM0" OR "NW0"                         
    0396            GO TO 100-CHECK-LAST-4FSN.                               
    0397        MOVE FSNB-IN TO THREE-TEST.                                  
    0398        IF NUMB-TEST-3 IS NOT NUMERIC                                
    0399            GO TO 090-FSN-MID-3-ERROR.                               
    0400        GO TO 100-CHECK-LAST-4FSN.                                   
    0401    090-FSN-MID-3-ERROR.                                              
    0402        MOVE "***" TO FSNB-ERR.                                      
    0403        MOVE "1" TO ERROR-IND.                                       
    0404    100-CHECK-LAST-4FSN.                                              
    0405        IF FSND-IN < "?" OR FSND-IN > "I"                            
    0406            GO TO 110-CHECK-ALL4-NUMERIC.                            
    0407        MOVE "*" TO EXCESS-FLAG-RPT.                                 
    0408        NOTE ---> THIS NEXT COMPARE IS FOR A 12/0 PUNCH NOT A SPACE. 
    0409        IF FSND-IN = "?"                                             
    0410            MOVE "0" TO FSND-RPT.                                    
    0411        IF FSND-IN = "A"                                             
    0412            MOVE "1" TO FSND-RPT.                                    
    0413        IF FSND-IN = "C"                                             
    0414            MOVE "3" TO FSND-RPT.                                    
    0415            MOVE "2" TO FSND-RPT.                                    
    0416        IF FSND-IN = "B"                                             
    0417        IF FSND-IN = "D"                                             
    0418            MOVE "4" TO FSND-RPT.                                    
    0419        IF FSND-IN = "E"                                             
    0420            MOVE "5" TO FSND-RPT.                                    
    0421        IF FSND-IN = "F"                                             
    0422            MOVE "6" TO FSND-RPT.                                    
    0423        IF FSND-IN = "G"                                             
    0424            MOVE "7" TO FSND-RPT.                                    
    0425        IF FSND-IN = "H"                                             
    0426            MOVE "8" TO FSND-RPT.                                    
    0427        IF FSND-IN = "I"                                             
    0428            MOVE "9" TO FSND-RPT.                                    
    0429        MOVE FSNC-IN TO THREE-TEST.                                  
    0430        IF NUMB-TEST-3 IS NOT NUMERIC                                
    0431            MOVE "***" TO FSNC-ERR                                   
    0432            MOVE "1" TO ERROR-IND.                                   
    0433        GO TO 125-CK-OWN-FIELD.                                      
    0434    110-CHECK-ALL4-NUMERIC.                                           
    0435        MOVE FSNLAST-4-IN TO FOUR-TEST.                              
    0436        IF NUMB-TEST-4 IS NOT NUMERIC                                
    0437            MOVE "***" TO FSNC-ERR                                   
    0438            MOVE "*" TO FSND-ERR                                     
    0439            GO TO 110-CHECK-ALL4-NUMERIC.                            
    0440            MOVE "1" TO ERROR-IND.                                   
    0441    120-LAST-FOUR-SPACES.                                             
    0442        IF FSNA-IN = "FCLT" OR "INST"                                
    0443            NEXT SENTENCE  ELSE                                      
    0444            GO TO 125-CK-OWN-FIELD.                                  
    0445        IF FSNLAST-4-IN = SPACES                                     
    0446            GO TO 125-CK-OWN-FIELD.                                  
    0447        MOVE "***" TO FSNC-ERR.                                      
    0448        MOVE "*" TO FSND-ERR.                                            P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-8
    FAA1.CBL    04-JUN-73  14:36

    0449        MOVE "1" TO ERROR-IND.                                       
    0450    125-CK-OWN-FIELD.                                                 
    0451        IF OWN-IN < "1" OR                                           
    0452            OWN-IN > "9"                                             
    0453            MOVE "*" TO OWN-ERR                                      
    0454            MOVE "1" TO ERROR-IND.                                   
    0455        MOVE 1 TO SUB.                                               
    0456    130-ASSET-CODE-LOOKUP.                                            
    0457        MOVE AC (SUB) TO AC-HOLD.                                    
    0458        IF ASSET-CODE-IN = AC-HOLD                                   
    0459            GO TO 140-QTY-CHECK.                                     
    0460        ADD 1 TO SUB.                                                
    0461        IF SUB > 20                                                  
    0462            MOVE "**" TO AC-ERR                                      
    0463            MOVE "1" TO ERROR-IND                                    
    0464            GO TO 140-QTY-CHECK   ELSE                               
    0465            GO TO 130-ASSET-CODE-LOOKUP.                             
    0466    140-QTY-CHECK.                                                    
    0467        IF FSNA-IN = "FCLT" OR "INST"                                
    0468            MOVE SPACE TO DASH2-RPT                                  
    0469            MOVE " 0" TO QUANTITY-RPT                                
    0470            MOVE "01" TO QUANTITY-IN                                 
    0471            MOVE ZEROS TO UNIT-VALUE-IN                              
    0472            GO TO 145-TOTAL-VALUE-CK.                                
    0473        IF QUANTITY-IN < "01" OR                                     
    0474            QUANTITY-IN > "99"                                       
    0475            MOVE "**" TO QUAN-ERR                                    
    0476            MOVE ZEROS TO QUANTITY-IN                                
    0477            MOVE "1" TO ERROR-IND.                                   
    0478        IF UNIT-VALUE-X = SPACES                                     
    0479            MOVE "000000000" TO UNIT-VALUE-X.                        
    0480        EXAMINE UNIT-VALUE-X REPLACING LEADING SPACES BY ZEROS.      
    0481        MOVE UNIT-VALUE-X TO NINE-TEST.                              
    0482        IF NUMB-TEST-9 IS NOT NUMERIC                                
    0483            MOVE ZEROS TO UNIT-VALUE-IN                              
    0484            MOVE "**********" TO PRICE-ERR                           
    0485            MOVE "1" TO ERROR-IND.                                   
    0486    145-TOTAL-VALUE-CK.                                               
    0487        IF TOTAL-VALUE-X = SPACES                                    
    0488            MOVE "000000000" TO TOTAL-VALUE-X.                       
    0489        EXAMINE TOTAL-VALUE-X REPLACING LEADING SPACES BY ZEROS.     
    0490        MOVE TOTAL-VALUE-X TO NINE-TEST.                             
    0491        IF NUMB-TEST-9 IS NOT NUMERIC                                
    0492            MOVE ZEROS TO TOTAL-VALUE-IN                             
    0493            MOVE "***********" TO VALUE-ERR                          
    0494            MOVE "1" TO ERROR-IND.                                   
    0495    150-ACTION-DATE-CK.                                               
    0496        IF MONTH-IN = "O" OR "N" OR "D"                              
    0497            GO TO 155-YEAR-CK.                                       
    0498        IF MONTH-IN < "0" OR                                         
    0499            MONTH-IN > "9"                                           
    0500            MOVE "****" TO ACT-DTE-ERR                               
    0501            MOVE "1" TO ERROR-IND.                                   
    0502    155-YEAR-CK.                                                      
    0503        IF YEAR-IN < "00" OR                                         
    0504            YEAR-IN > "99"                                               P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-9
    FAA1.CBL    04-JUN-73  14:36

    0505            MOVE "****" TO ACT-DTE-ERR                               
    0506            MOVE "1" TO ERROR-IND.                                   
    0507    160-DOC-NUM-CK.                                                   
    0508        MOVE DOC-NUM-IN TO FIVE-TEST.                                
    0509        IF NUMB-TEST-5 IS NOT NUMERIC                                
    0510            MOVE "*****" TO DOC-NO-ERR                               
    0511            MOVE "1" TO ERROR-IND.                                   
    0512        IF BATCH-NUM-IN = SPACES                                     
    0513            MOVE "***" TO BTCH-ERR                                   
    0514            MOVE "1" TO ERROR-IND.                                   
    0515        MOVE 1 TO SUB.                                               
    0516    165-MOVE-FIELDS-TO-COMP-WS.                                       
    0517        IF FSNA-IN = "FCLT" OR "INST"                                
    0518            MOVE TOTAL-VALUE-IN TO TEMP-TOTAL                        
    0519            MOVE TOTAL-VALUE-IN TO TOTAL-VALUE-RPT                   
    0520            MOVE 01 TO QUAN-WS                                       
    0521            GO TO 170-CHECK-IF-INCR.                                 
    0522        MOVE UNIT-VALUE-IN TO UNIT-VALUE-WS.                         
    0523        MOVE QUANTITY-IN-9 TO QUAN-WS.                               
    0524        MULTIPLY UNIT-VALUE-WS BY QUAN-WS GIVING TEMP-TOTAL.         
    0525        MOVE TEMP-TOTAL TO TOTAL-VALUE-RPT.                          
    0526    170-CHECK-IF-INCR.                                                
    0527        MOVE INCR-TBL (SUB) TO TR-HOLD.                              
    0528        IF TRANS-CODE-IN = TR-HOLD                                   
    0529            GO TO 190-INCR-ROUTINE.                                  
    0530        IF SUB = 20                                                  
    0531        MOVE 1 TO SUB                                                
    0532            GO TO 180-CHECK-IF-DECR   ELSE                           
    0533            ADD 1 TO SUB                                             
    0534            GO TO 170-CHECK-IF-INCR.                                 
    0535    180-CHECK-IF-DECR.                                                
    0536        MOVE DECR-TBL (SUB) TO TR-HOLD.                              
    0537        IF TRANS-CODE-IN = TR-HOLD                                   
    0538            GO TO 200-DECR-ROUTINE.                                  
    0539        IF SUB = 22                                                  
    0540            MOVE "**" TO TR-CD-ERR                                   
    0541            MOVE "1" TO ERROR-IND                                    
    0542            GO TO 210-PRINT-DOC  ELSE                                
    0543            ADD 1 TO SUB                                             
    0544            GO TO 180-CHECK-IF-DECR.                                 
    0545    190-INCR-ROUTINE.                                                 
    0546        ADD QUAN-WS TO DOC-INCR-CTR.                                 
    0547        ADD QUAN-WS TO BATCH-INCR-CTR.                               
    0548        ADD QUAN-WS TO BATCH-CTR.                                    
    0549        ADD QUAN-WS TO REGION-CTR.                                   
    0550        ADD QUAN-WS TO OV-CTR.                                       
    0551        ADD TEMP-TOTAL TO BATCH-INCR-AMT.                            
    0552        ADD TEMP-TOTAL TO DOC-INCR-AMT.                              
    0553        ADD TEMP-TOTAL TO BATCH-AMT.                                 
    0554        ADD TEMP-TOTAL TO REGION-AMT.                                
    0555        ADD TEMP-TOTAL TO OV-AMT.                                    
    0556        GO TO 210-PRINT-DOC.                                         
    0557    200-DECR-ROUTINE.                                                 
    0558        SUBTRACT QUAN-WS FROM DOC-DECR-CTR.                          
    0559        SUBTRACT QUAN-WS FROM BATCH-DECR-CTR.                        
    0560        ADD QUAN-WS TO BATCH-CTR.                                        P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-10
    FAA1.CBL    04-JUN-73  14:36

    0561        ADD QUAN-WS TO REGION-CTR.                                   
    0562        ADD QUAN-WS TO OV-CTR.                                       
    0563        SUBTRACT TEMP-TOTAL FROM DOC-DECR-AMT.                       
    0564        SUBTRACT TEMP-TOTAL FROM BATCH-DECR-AMT.                     
    0565        SUBTRACT TEMP-TOTAL FROM REGION-AMT.                         
    0566        SUBTRACT TEMP-TOTAL FROM BATCH-AMT.                          
    0567        SUBTRACT TEMP-TOTAL FROM OV-AMT.                             
    0568    210-PRINT-DOC.                                                    
    0569        MOVE "0" TO PRT-CTRL.                                      
    0570        ADD 2 TO LINE-CTR.                                           
    0571        PERFORM 320-WRITE-A-LINE.                                    
    0572        IF ERROR-IND = "1"                                           
    0573            MOVE "**********" TO ERR-FLG                             
    0574            ADD 1 TO LINE-CTR                                        
    0575            MOVE ERROR-LINE TO PRINT-IO                              
    0576            MOVE SPACES TO ERROR-LINE                                
    0577            MOVE " " TO ERROR-IND                                    
    0578            MOVE " " TO PRT-CTRL                                   
    0579            PERFORM 320-WRITE-A-LINE.                                
    0580        PERFORM 330-CHECK-OVERFLOW THRU 340-WRITE-EXIT.              
    0581        GO TO 010-READ-INPUT.                                        
    0582    220-END-DOC-RTN.                                                  
    0583        MOVE DOCMNT TO T-1.                                          
    0584        MOVE TOTL TO T-3.                                            
    0585        MOVE DECR TO T-4.                                            
    0586        MOVE INCR TO T-5.                                            
    0587        MOVE DOC-DECR-CTR TO DECR-CNT-RPT.                           
    0588        MOVE DOC-DECR-AMT TO DECR-AMT-RPT.                           
    0589        MOVE DOC-INCR-CTR TO INCR-CNT-RPT.                           
    0590        MOVE DOC-INCR-AMT TO INCR-AMT-RPT.                           
    0591        MOVE ZEROS TO DOC-DECR-CTR DOC-DECR-AMT                      
    0592                      DOC-INCR-CTR DOC-INCR-AMT.                     
    0593        MOVE "-" TO PRT-CTRL.                                      
    0594        ADD 3 TO LINE-CTR.                                           
    0595        PERFORM 320-WRITE-A-LINE THRU 340-WRITE-EXIT.                
    0596        IF END-JOB-IND = "1"                                         
    0597            MOVE " " TO PRT-CTRL                                   
    0598            PERFORM 320-WRITE-A-LINE.                                
    0599        MOVE DOC-NUM-IN TO WS-DOC-NUM.                               
    0600    230-END-DOC-EXIT.                                                 
    0601        EXIT.                                                        
    0602    240-END-BATCH-RTN.                                                
    0603        PERFORM 220-END-DOC-RTN THRU 230-END-DOC-EXIT.               
    0604        MOVE BATCH TO T-1.                                           
    0605        MOVE TOTL TO T-3.                                            
    0606        MOVE DECR TO T-4.                                            
    0607        MOVE INCR TO T-5.                                            
    0608        MOVE BATCH-DECR-CTR TO DECR-CNT-RPT.                         
    0609        MOVE BATCH-DECR-AMT TO DECR-AMT-RPT.                         
    0610        MOVE BATCH-INCR-CTR TO INCR-CNT-RPT.                         
    0611        MOVE BATCH-INCR-AMT TO INCR-AMT-RPT.                         
    0612        MOVE "0" TO PRT-CTRL.                                      
    0613        ADD 2 TO LINE-CTR.                                           
    0614        PERFORM 320-WRITE-A-LINE.                                    
    0615        MOVE BATCH TO T-1.                                           
    0616        MOVE TVAL TO T-3.                                                P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-11
    FAA1.CBL    04-JUN-73  14:36

    0617        MOVE BATCH-AMT TO INCR-AMT-RPT.                              
    0618        ADD 2 TO LINE-CTR.                                           
    0619        PERFORM 320-WRITE-A-LINE.                                    
    0620        MOVE BATCH TO T-1.                                           
    0621        MOVE ITEM TO T-2.                                            
    0622        MOVE TCNT TO T-3.                                            
    0623        MOVE BATCH-CTR TO INCR-CNT-RPT.                              
    0624        ADD 2 TO LINE-CTR.                                           
    0625        PERFORM 320-WRITE-A-LINE THRU 340-WRITE-EXIT.                
    0626        MOVE ZEROS TO BATCH-DECR-CTR, BATCH-DECR-AMT, BATCH-INCR-AMT 
    0627               BATCH-INCR-CTR, BATCH-AMT, BATCH-CTR.                 
    0628        MOVE BATCH-NUM-IN TO WS-BATCH-HOLD.                          
    0629    250-BATCH-RTN-EXIT.                                               
    0630        EXIT.                                                        
    0631    260-END-REGION-RTN.                                               
    0632        PERFORM 240-END-BATCH-RTN THRU 250-BATCH-RTN-EXIT.           
    0633        MOVE TNET TO T-1.                                            
    0634        MOVE TVAL TO T-3.                                            
    0635        MOVE REGION-AMT TO INCR-AMT-RPT.                             
    0636        MOVE "0" TO PRT-CTRL.                                      
    0637        ADD 2 TO LINE-CTR.                                           
    0638        PERFORM 320-WRITE-A-LINE.                                    
    0639        MOVE TOTL TO T-1.                                            
    0640        MOVE ITEM TO T-2.                                            
    0641        MOVE TCNT TO T-3.                                            
    0642        MOVE REGION-CTR TO INCR-CNT-RPT.                             
    0643        ADD 2 TO LINE-CTR.                                           
    0644        PERFORM 320-WRITE-A-LINE.                                    
    0645        MOVE ZEROS TO REGION-AMT, REGION-CTR, LINE-CTR, PAGE-CTR.    
    0646        IF END-JOB-IND = "1"                                         
    0647            NEXT SENTENCE  ELSE                                      
    0648            MOVE ZEROS TO PAGE-CTR                                   
    0649            PERFORM 280-FIND-REGION-HDR THRU 290-FIND-REG-EXIT.      
    0650        PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXIT.            
    0651        MOVE REGION-IN TO WS-REGION-HOLD.                            
    0652    270-END-REGION-EXIT.                                              
    0653        EXIT.                                                        
    0654    280-FIND-REGION-HDR.                                              
    0655        MOVE 1 TO SUB.                                               
    0656    285-START-HDR-LOOKUP.                                             
    0657        MOVE REG-TBL (SUB) TO REG-BRKDWN.                            
    0658        IF REGION-IN = REG-CODE-TBL                                  
    0659            MOVE REGION-IN TO WS-REGION-HOLD                         
    0660            MOVE REG-NAME-TBL TO REGION-HDR                          
    0661            GO TO 290-FIND-REG-EXIT.                                 
    0662        IF SUB > 10                                                  
    0663            MOVE "*" TO REG-ERR                                      
    0664            MOVE "1" TO ERROR-IND                                    
    0665            GO TO 290-FIND-REG-EXIT   ELSE                           
    0666            ADD 1 TO SUB                                             
    0667            GO TO 285-START-HDR-LOOKUP.                              
    0668    290-FIND-REG-EXIT.                                                
    0669        EXIT.                                                        
    0670    300-HEADER-ROUTINE.                                               
    0671        ADD 1 TO PAGE-CTR.                                           
    0672        MOVE PAGE-CTR TO PAGE-HDR.                                       P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE 1-12
    FAA1.CBL    04-JUN-73  14:36

    0673        MOVE "1" TO PRT-CTRL.                                      
    0674        MOVE HEAD-1 TO PRINT-IO.                                     
    0675        PERFORM 320-WRITE-A-LINE.                                    
    0676        MOVE "0" TO PRT-CTRL.                                      
    0677        MOVE HEAD-2 TO PRINT-IO.                                     
    0678        PERFORM 320-WRITE-A-LINE.                                    
    0679        MOVE " " TO PRT-CTRL.                                      
    0680        MOVE HEAD-3 TO PRINT-IO.                                     
    0681        PERFORM 320-WRITE-A-LINE.                                    
    0682    310-HDR-RTN-EXIT.                                                 
    0683        EXIT.                                                        
    0684    320-WRITE-A-LINE.                                                 
    0685        WRITE PRINT-LINE AFTER ADVANCING 2 LINES.           
    0686        MOVE SPACES TO PRINT-IO.                                     
    0687    330-CHECK-OVERFLOW.                                               
    0688        IF LINE-CTR > 46                                             
    0689            MOVE ZEROS TO LINE-CTR                                   
    0690            PERFORM 300-HEADER-ROUTINE THRU 310-HDR-RTN-EXIT.        
    0691    340-WRITE-EXIT.                                                   
    0692        EXIT.                                                        
    0693    350-INPUT-AT-END.                                                 
    0694        PERFORM 260-END-REGION-RTN THRU 270-END-REGION-EXIT.         
    0695        MOVE "OVERALL " TO T-1.                                      
    0696        MOVE TOTL TO T-3.                                            
    0697        PERFORM 320-WRITE-A-LINE.                                    
    0698        MOVE TNET TO T-1.                                            
    0699        MOVE TVAL TO T-3.                                            
    0700        MOVE OV-AMT TO INCR-AMT-RPT.                                 
    0701        PERFORM 320-WRITE-A-LINE.                                    
    0702        MOVE TOTL TO T-1.                                            
    0703        MOVE ITEM TO T-2.                                            
    0704        MOVE TCNT TO T-3.                                            
    0705        MOVE OV-CTR TO INCR-CNT-RPT.                                 
    0706        PERFORM 320-WRITE-A-LINE.                                    
    0707        CLOSE TRANS, PRINT.                                          
    0708        DISPLAY "EXCELLENT PROGRAMMING JOB" UPON CONSOLE.                
    0709        DISPLAY "END BB01A" UPON CONSOLE.                                
    0710        STOP RUN.                                                    
    0711                                                                     
    0712                                                                         P R O G R A M   B B 0 1 A 		COBOL (005001,107001)	 4-JUN-73  17:10		PAGE W-1
    FAA1.CBL    04-JUN-73  14:36

WARNINGS:

    0204  REDEFINITION IS NOT THE SAME SIZE AS THE REDEFINED ITEM
    0245  REDEFINITION IS NOT THE SAME SIZE AS THE REDEFINED ITEM
    0524  MOST SIGNIFICANT DIGITS TRUNCATED ON TEMP-TOTAL
    0575  RIGHT-MOST TRUNCATION ON PRINT-IO
    0672  MOST SIGNIFICANT DIGITS TRUNCATED ON PAGE-HDR


NO FATAL ERRORS, 5 WARNINGS
 @	&Ñ