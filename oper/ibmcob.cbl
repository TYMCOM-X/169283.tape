000000 IDENTIFICATION DIVISION.                                                 
000010 PROGRAM-ID. 'T3060'.                                                     
000020 AUTHOR. CAGNEY FRANCE, STANFORD OPTNER AND ASSOCIATES INC.               
000030 INSTALLATION. LOS ANGELES TRAFFIC DEPARTMENT, DATA SERVICE BUREAU        
000040     CITY OF LOS ANGELES, JULY 1969.                                      
000080 ENVIRONMENT DIVISION.                                                    
000090 CONFIGURATION SECTION.                                                   
000100 SOURCE-COMPUTER. IBM-360.                                                
000110 OBJECT-COMPUTER. IBM-360.                                                
000120 INPUT-OUTPUT SECTION.                                                    
000130 FILE-CONTROL.                                                            
000140     SELECT REPORT-FILE  ASSIGN TO 'SYS020' DIRECT-ACCESS 2314.   KM120470
000150     SELECT VOLFILE ASSIGN TO 'SYS021' DIRECT-ACCESS 2314         KM120470
000152             RESERVE NO ALTERNATE AREA                            KM120470
000160             ACCESS IS RANDOM                                             
000170             ORGANIZATION IS INDEXED                                      
000180             RECORD KEY IS VOL-REC-KEY                                    
000190             SYMBOLIC KEY IS VOL-SYM-KEY.                                 
000200     SELECT ACCIDENT-FILE ASSIGN TO 'SYS022' DIRECT-ACCESS 2314   KM120470
000202             RESERVE NO ALTERNATE AREA                            KM120470
000210         ACCESS IS RANDOM                                                 
000220         ORGANIZATION IS INDEXED                                          
000230         RECORD KEY IS RECORD-KEY                                         
000240         SYMBOLIC KEY IS A-SYM-KEY.                                       
000250     SELECT REQUEST-TAPE ASSIGN TO 'SYS015' UTILITY 2400.         KM120470
000260     SELECT ISFILE ASSIGN TO 'SYS023' DIRECT-ACCESS 2314          KM120470
000262             RESERVE NO ALTERNATE AREA                            KM120470
000270             ACCESS IS RANDOM                                             
000280             ORGANIZATION IS INDEXED                                      
000290             RECORD KEY IS IS-REC-KEY                                     
000300             SYMBOLIC KEY IS SYM-KEY.                                     
000310     SELECT PRINT-FILE ASSIGN TO  'SYS012' UNIT-RECORD 1403.      KM120470
000320 DATA DIVISION.                                                           
000330 FILE SECTION.                                                            
000315                                                                  KM120470
000316                                                                  KM120470
000340 FD  REQUEST-TAPE                                                         
000350     RECORD CONTAINS 100 CHARACTERS                                       
000360     BLOCK CONTAINS 35 RECORDS                                            
000370     RECORDING MODE IS F                                                  
000380     LABEL RECORDS ARE OMITTED                                    KM120470
000390     DATA RECORD IS REQUEST-REC.                                          
000400 01  REQUEST-REC.                                                         
000410     03 T10-S-LOC.                                                        
000420        05 REQ-NO-10         PICTURE 999.                                 
000430        05 IS-10.                                                         
000440           07 IS-10-ST1      PICTURE X(5).                        KM120470
000450           07 IS-10-ST2      PICTURE X(5).                        KM120470
000460        05 IS-10-REQUESTER   PICTURE X(15).                               
000470        05 DATE-LIMIT-10.                                                 
000480           07 FR-MO          PICTURE 99.                                  
000490           07 FR-DA          PICTURE 99.                                  
000500           07 FR-YR          PICTURE 99.                                  
000510           07 T-MO           PICTURE 99.                                  
000520           07 T-DA           PICTURE 99.                                  
000530           07 T-YR           PICTURE 99.                                  
000540           05 FILLER         PICTURE X(40).                               
000550         03 T10-R-T20-T22-REQ REDEFINES T10-S-LOC.                        
000560            05 R-10-REQ-NO   PICTURE 999.                                 
000570            05 R-ST-CODE     PICTURE X(5).                        KM120470
000580            05 FIRST-ST-CODE PICTURE X(5).                        KM120470
000590            05 LAST-ST-CODE  PICTURE X(5).                        KM120470
000600            05 R-SEARCH      PICTURE X.                                   
000610            05 R-REQUESTER   PICTURE X(15).                               
000620            05 R-DATE-LIMIT.                                              
000630               07 R-FR-MO    PICTURE 99.                                  
000640               07 R-FR-DA    PICTURE 99.                                  
000650               07 R-FR-YR    PICTURE 99.                                  
000660               07 R-T-MO     PICTURE 99.                                  
000670               07 R-T-DA     PICTURE 99.                                  
000680               07 R-T-YR     PICTURE 99.                                  
000682           05 R-DIRECTION    PICTURE X(01).                       KM010671
000690           05 FILLER         PICTURE X(33).                       KM010671
000700        03 REQ-SORT-KEY.                                                  
000710           05 FILLER         PICTURE XX.                                  
000720           05 KEY-CODE       PICTURE 99.                                  
000730           05 FILLER         PICTURE X(16).                               
000740 FD  PRINT-FILE                                                           
000750     RECORD CONTAINS 133 CHARACTERS                                       
000760                                                                  KM120470
000770     RECORDING MODE IS F                                                  
000780     LABEL RECORDS ARE OMITTED                                            
000790     DATA RECORD IS PRINT-REC.                                            
000800 01  PRINT-REC.                                                           
000810     03 FILLER       PICTURE X.                                           
000820     03 PRINT-LINE   PICTURE X(132).                                      
000830 FD  REPORT-FILE                                                  T-3090  
000840     LABEL RECORDS ARE STANDARD                                   T-3090  
000850     RECORDING MODE IS F                                          T-3090  
000860     RECORD CONTAINS 290 CHARACTERS                               T-3090  
000870     BLOCK CONTAINS 25 RECORDS                                    T-3090  
000880     DATA RECORD IS REPORT-REC.                                   T-3090  
000890 01  REPORT-REC.                                                  T-3090  
000900     02 T81-DETAIL.                                               T-3090  
000910        03 T81-REQUESTER     PICTURE X(15).                       T-3090  
000920        03 T81-SEARCH-DATES  PICTURE X(12).                       T-3090  
000930        03 T81-REC-CODE      PICTURE 9.                           T-3090  
000940        03 T81-DATE.                                              T-3090  
000950           05 T81-YR         PICTURE XX.                          T-3090  
000960           05 T81-MO         PICTURE XX.                          T-3090  
000970           05 T81-DA         PICTURE XX.                          T-3090  
000980        03 T81-DAY-OF-WEEK   PICTURE X.                           T-3090  
000990        03 T81-DOW REDEFINES T81-DAY-OF-WEEK PICTURE 9.           T-3090  
001000        03 T81-LOC.                                               T-3090  
001010           05 T81-LOC1       PICTURE X.                           T-3090  
001020           05 T81-LOC2       PICTURE X.                           T-3090  
001030           05 T81-LOC3       PICTURE X.                           T-3090  
001040        03 FILLER            PICTURE XX.                          T-3090  
001050        03 T81-REVERSE-CODE  PICTURE X.                           T-3090  
001060        03 FILLER            PICTURE X.                           T-3090  
001070       03 T81-PEAK.                                                       
001080        04 T81-AM-PEAK.                                                   
001090           05 T81-AM-N-E-HR  PICTURE   99.                        T-3090  
001100           05 T81-AM-N-E-MIN PICTURE    9.                        T-3090  
001110           05 T81-AM-N-E-VOL PICTURE 9999.                        T-3090  
001120           05 T81-AM-S-W-HR  PICTURE   99.                        T-3090  
001130           05 T81-AM-S-W-MIN PICTURE    9.                        T-3090  
001140           05 T81-AM-S-W-VOL PICTURE 9999.                        T-3090  
001150           05 T81-AM-T-HR    PICTURE   99.                        T-3090  
001160           05 T81-AM-T-MIN   PICTURE    9.                        T-3090  
001170           05 T81-AM-T-VOL   PICTURE 9999.                        T-3090  
001180        04 T81-PM-PEAK.                                                   
001190           05 T81-PM-N-E-HR  PICTURE   99.                        T-3090  
001200           05 T81-PM-N-E-MIN PICTURE    9.                        T-3090  
001210           05 T81-PM-N-E-VOL PICTURE 9999.                        T-3090  
001220           05 T81-PM-S-W-HR  PICTURE   99.                        T-3090  
001230           05 T81-PM-S-W-MIN PICTURE    9.                        T-3090  
001240           05 T81-PM-S-W-VOL PICTURE 9999.                        T-3090  
001250           05 T81-PM-T-HR    PICTURE   99.                        T-3090  
001260           05 T81-PM-T-MIN   PICTURE    9.                        T-3090  
001270           05 T81-PM-T-VOL   PICTURE 9999.                        T-3090  
001280        03 T81-TOT-24-HR.                                         T-3090  
001290           05 T81-TOT-N-E    PICTURE S9(5).                       T-3090  
001300           05 T81-TOT-S-W    PICTURE S9(5).                       T-3090  
001310           05 T81-TOT-TOT    PICTURE S9(5).                       T-3090  
001320        03 T81-MED           PICTURE   XXX.                       T-3090  
001330        03 T81-ADJ-FAC REDEFINES T81-MED                          T-3090  
001340                             PICTURE  S9V99.                      T-3090  
001350        03 T81-RDWY          PICTURE  XXXX.                       T-3090  
001360        03 T81-LA            PICTURE    XX.                       T-3090  
001370        03 T81-IS-CODE.                                           T-3090  
001380           05 T81-ST-1       PICTURE  X(5).                       T-3090  
001390           05 T81-ST-2       PICTURE  X(5).                       T-3090  
001400        03 T81-ST-NAME-1     PICTURE X(30).                       T-3090  
001410        03 T81-ST-NAME-2     PICTURE X(30).                       T-3090  
001420        03 FILLER            PICTURE X(92).                       T-3090  
001430     02 T23-DETAIL REDEFINES T81-DETAIL.                                  
001440        05 T23-REC-1         PICTURE X(270).                              
001450        05 T23-REC-2 REDEFINES T23-REC-1.                         T-3090  
001460           07 T23-REC-2-AT-C PICTURE X.                           T-3090  
001470           07 T23-REC-2-X-SC PICTURE X(5).                        T-3090  
001480           07 T23-REC-2-X-ST PICTURE X(30).                       T-3090  
001490           07 T23-40         PICTURE X(40).                               
001500           07 T23-REC-2-INCR REDEFINES T23-40 OCCURS 40 TIMES.            
001510              09 T23-ACC     PICTURE X.                           T-3090  
001520           07 T23-REC-2-OV40 PICTURE X.                           T-3090  
001530           07 FILLER         PICTURE X(193).                      T-3090  
001540     02  T10-DETAIL REDEFINES T23-DETAIL.                         T-3090  
001550       03 T10-DET-A.                                                      
001560         05 T10-REQUESTER    PICTURE X(15).                       T-3090  
001570         05 T10-SEARCH-DATES PICTURE X(12).                       T-3090  
001580         05 FILLER           PICTURE XX.                          T-3090  
001590         05 T10-IS-CODE.                                          T-3090  
001600            07 T10-IS-1      PICTURE X(5).                        KM120470
001610            07 T10-IS-2      PICTURE X(5).                        KM120470
001620       03 DETAIL-REC-VAR.                                                 
001630         05 DOW              PICTURE X.                           T-3090  
001640         05 D-O-W REDEFINES DOW PICTURE 9.                        T-3090  
001650         05 T10-DATE.                                             T-3090  
001660            07 T10-MO        PICTURE XX.                          T-3090  
001670            07 T10-DA        PICTURE XX.                          T-3090  
001680            07 T10-YR        PICTURE XX.                          T-3090  
001690         05 T10-TIME.                                             T-3090  
001700            07 T10-HR        PICTURE XX.                          T-3090  
001710            07 T10-M         PICTURE XX.                          T-3090  
001720         05 T10-DR-NO        PICTURE X(6).                        T-3090  
001730         05 PRI-CODE         PICTURE X(6).                        T-3090  
001740         05 GP-CODE          PICTURE XX.                          T-3090  
001750         05 GROUP-CODE REDEFINES  GP-CODE PICTURE 99.             T-3090  
001760       04 T10-PT-IMPACT.                                                  
001770         05 LOCATION-CODE.                                        T-3090  
001780            07 INTERSECTION  PICTURE 9.                           T-3090  
001790            07 DIRECTION-1   PICTURE 9.                           T-3090  
001800            07 DIRECTION-2   PICTURE 9.                           T-3090  
001810         05 LOC-A REDEFINES LOCATION-CODE.                        T-3090  
001820            07 FILLER        PICTURE X.                           T-3090  
001830            07 DISTA         PICTURE 99.                          T-3090  
001840         05 PRI-CODE-1       PICTURE 9.                           T-3090  
001850         05 PRI-CODE-2       PICTURE 9.                           T-3090  
001860         05 SEC-FEET         PICTURE 9999.                        T-3090  
001870         05 SEC-CODE-1       PICTURE 9.                           T-3090  
001880         05 SEC-CODE-2       PICTURE 9.                           T-3090  
001890           04 T10-FL.                                                     
001900         05 DIR-ANL          PICTURE 999.                         T-3090  
001910         05 LIGHT            PICTURE X.                           T-3090  
001920         05 RD-CONDITION     PICTURE X.                           T-3090  
001930         05 SPEC-CIR         PICTURE XX.                          T-3090  
001940         05 WEATHER          PICTURE 9.                           T-3090  
001950         05 T10-AT-IS        PICTURE 9.                           T-3090  
001960       04 T10-NO-PER-INJ.                                                 
001970         05 T10-INJ-A        PICTURE S99.                         T-3090  
001980         05 T10-INJ-B        PICTURE S99.                         T-3090  
001990         05 T10-INJ-C        PICTURE S99.                         T-3090  
002000         05 T10-INJ-K        PICTURE S99.                         T-3090  
002010       04 T10-DET-B.                                                      
002020         05 T10-ACC-TYPE     PICTURE 99.                          T-3090  
002030         05 T10-A-SEV        PICTURE 9.                           T-3090  
002040         05 T10-REVERSE-ST-C PICTURE 9.                           T-3090  
002050         05 T10-NO-PARTIES   PICTURE 9.                                   
002060         05 FILLER             PICTURE XXX.                               
002070         05 T10-PARTY OCCURS 9 TIMES.                             T-3090  
002080            07 PARTY-NO      PICTURE 9.                                   
002090           07 T10-PARTY-ENTRY.                                            
002100            11 VEH-ACTION    PICTURE 99.                                  
002110            11 T10-DIR       PICTURE 9.                                   
002120            11 VEH-TYPE      PICTURE 99.                                  
002130            11 T10-CLASS     PICTURE 9.                                   
002140            11 T10-COND      PICTURE 99.                                  
002150            11 T10-AGE       PICTURE 99.                                  
002160            11 PARTY-INJ     PICTURE 9.                                   
002170            11 CONTRIB-CIR   PICTURE 99.                                  
002180            11 FILLER        PICTURE XX.                                  
002190            11 PED-ACTION    PICTURE 99.                                  
002200         05 FILLER           PICTURE X(8).                        T-3090  
00    02 T20-DETAIL REDEFINES T10-DETAIL.                          T-3090  
002220         03 T20-REQUESTER    PICTURE X(15).                       T-3090  
002230         03 T20-SEARCH-DATE  PICTURE X(12).                       T-3090  
002240         03 FILLER           PICTURE X.                           T-3090  
002250         03 T20-ROUTE        PICTURE X(5).                        T-3090  
002260         03 T20-ROUTE-NAME   PICTURE X(30).                       T-3090  
002270         03 T20-X-ST.                                             T-3090  
002280            05 T20-X-ST1     PICTURE X(5).                        T-3090  
002290            05 T20-X-ST2     PICTURE X(5).                        T-3090  
002300         03 T20-X-ST-NAME1   PICTURE X(30).                       T-3090  
002310         03 T20-X-ST-NAME2   PICTURE X(30).                       T-3090  
002320         03 FILLER           PICTURE X(137).                      T-3090  
002330     02 T20-DETAIL-A REDEFINES T20-DETAIL.                        T-3090  
002340        03 T20-SUB-SEG       PICTURE X(5).                        T-3090  
002350        03 T20-SUB-SEG-NAME  PICTURE X(30).                       T-3090  
002360        03 T20-AT-IS         PICTURE X.                           T-3090  
002370         03 T20-VOL-ADT      PICTURE 9(5).                                
002380        03 T20-VOL-X-ST      PICTURE 9(5).                        T-3090  
002390        03 T20-DISTANCE      PICTURE S9999.                       T-3090  
002400        03 FILLER            PICTURE X(24).                               
002410        03 T20-IS-ACC         PICTURE 999.                                
002420        03 T20-NON-IS-ACC    PICTURE 999.                                 
002430        03 T20-STRAF          PICTURE 999.                                
002440        03 FILLER            PICTURE X(187).                              
002450     02 T20-DETAIL-B REDEFINES T20-DETAIL-A.                      T-3090  
002460        03 T20-CLASS         PICTURE X.                           T-3090  
002470        03 T20-CLASS1  REDEFINES T20-CLASS  PICTURE 9.                    
002480        03 T20-NO-LANES      PICTURE X.                           T-3090  
002490        03 T20-NO-LANES-N REDEFINES T20-NO-LANES PICTURE 9.       T-3090  
002500        03 T20-M-WDTH        PICTURE X.                           T-3090  
002510        03 T20-M-WDTH-N REDEFINES T20-M-WDTH     PICTURE 9.       T-3090  
002520        03 T20-M-TYPE        PICTURE X.                           T-3090  
002530        03 T20-M-TYPE-N REDEFINES T20-M-TYPE     PICTURE 9.       T-3090  
002540        03 T20-PARK          PICTURE X.                           T-3090  
002550        03 T20-PARK-N  REDEFINES T20-PARK        PICTURE 9.       T-3090  
002560        03 T20-PK            PICTURE X.                                   
002570        03 T20-PK-N REDEFINES T20-PK PICTURE 9.                           
002580        03 T20-DIR-FLOW      PICTURE X.                           T-3090  
002590        03 T20-DIR-FLOW-N REDEFINES T20-DIR-FLOW PICTURE 9.       T-3090  
002600         03 FILLER           PICTURE X(263).                              
002610       02 DETAIL-REC-V1.                                                  
002620     04 SORT-KEY.                                                         
002630        05 REPORT-CODE       PICTURE 99.                          T-3090  
002640        05 REQ-NO            PICTURE 999.                         T-3090  
002650        05 TOT-CAT-ACC       PICTURE S999.                        T-3090  
002660        05 REC-CODE          PICTURE 9.                           T-3090  
002670        05 CODE-IS-R.                                                     
002680           07 IS-SEQUENCE    PICTURE 9(4).                                
002690           07 CODE-IS-DATE   PICTURE X(6).                                
002700        05 FILLER REDEFINES CODE-IS-R.                                    
002710           07 DISTANCE       PICTURE 9(5).                                
002720           07 T20-SEG-NO REDEFINES DISTANCE  PICTURE 9(5).                
002730           07 FILLER-SEG     PICTURE 9(5).                                
002740        05 FILLER            PICTURE X.                                   
002750 FD  VOLFILE                                                      T-3090  
002760     LABEL RECORDS ARE STANDARD                                   T-3090  
002770     RECORDING MODE IS F                                          T-3090  
002780     RECORD CONTAINS 110 CHARACTERS                               T-3090  
002790                                                                  KM120470
002800     DATA RECORD IS VOLFILE-REC.                                  T-3090  
002810 01  VOLFILE-REC.                                                 T-3090  
002820     03 VOL-DELETE-CODE      PICTURE X.                           T-3090  
002830     03 VOL-REC-KEY.                                              T-3090  
002840        05 VOL-REC-ST.                                            T-3090  
002850           07 VOL-REC-ST-1   PICTURE X(5).                        T-3090  
002860           07 VOL-REC-ST-2   PICTURE X(5).                        T-3090  
002870        05 VOL-REC-INDEX     PICTURE XXX.                         T-3090  
002880        05 VOL-REC-INDEX-N REDEFINES VOL-REC-INDEX   PICTURE S999.T-3090  
002890     03 VOL-LATEST           PICTURE XXX.                         T-3090  
002900     03 VOL-LATEST-N REDEFINES VOL-LATEST    PICTURE S999.        T-3090  
002910     03 VOL-OLDEST           PICTURE XXX.                         T-3090  
002920     03 VOL-OLDEST-N REDEFINES VOL-OLDEST    PICTURE S999.        T-3090  
002930     03 VOL-REC-CODE         PICTURE 9.                           T-3090  
002940     03 VOL-DATE.                                                 T-3090  
002950        05 VOL-YR        PICTURE 99.                                      
002960        05 VOL-MO        PICTURE 99.                                      
002970        05 VOL-DA        PICTURE 99.                                      
002980     03 VOL-D-O-W            PICTURE X.                           T-3090  
002990     03 VOL-LOC.                                                  T-3090  
003000        05 VOL-LOC-1         PICTURE X.                           T-3090  
003010        05 VOL-LOC-2         PICTURE X.                           T-3090  
003020        05 VOL-LOC-3         PICTURE X.                           T-3090  
003030     03 VOL-DIST             PICTURE XX.                          T-3090  
003040     03 VOL-REV-CODE         PICTURE X.                           T-3090  
003050     03 VOL-SPEC-CODE        PICTURE X.                           T-3090  
003060     03 VOL-AM-PM-PEAK.                                           T-3090  
003070        05 VOL-PEAK OCCURS 2 TIMES.                               T-3090  
003080           07 VOL-N-E.                                            T-3090  
003090              09 V-NE-HR.                                         T-3090  
003100                 11 V-NE-H   PICTURE XX.                          T-3090  
003110                 11 V-NE-M   PICTURE X.                           T-3090  
003120              09 V-NE-VOL    PICTURE 9999.                                
003130           07 VOL-S-W.                                            T-3090  
003140              09 V-SW-HR.                                         T-3090  
003150                 11 V-SW-H   PICTURE XX.                          T-3090  
003160                 11 V-SW-M   PICTURE X.                           T-3090  
003170              09 V-SW-VOL    PICTURE 9999.                                
003180           07 VOL-TOT.                                            T-3090  
003190              09 V-TOT-HR.                                        T-3090  
003200                 11 V-TOT-H  PICTURE XX.                          T-3090  
003210                 11 V-TOT-M  PICTURE X.                           T-3090  
003220              09 V-TOT-VOL   PICTURE 9999.                                
003230     03 VOL-TOTALS-HR.                                            T-3090  
003240        05 VOL-TOTAL-NE      PICTURE X(5).                        T-3090  
003250        05 VOL-TOTAL-NE-N REDEFINES VOL-TOTAL-NE PICTURE S9(5).   T-3090  
003260        05 VOL-TOTAL-SW      PICTURE X(5).                        T-3090  
003270        05 VOL-TOTAL-SW-N REDEFINES VOL-TOTAL-SW PICTURE S9(5).   T-3090  
003280        05 VOL-TOTAL-TOT     PICTURE X(5).                        T-3090  
003290        05 VOL-TOTAL-TOT-N REDEFINES VOL-TOTAL-TOT PICTURE S9(5). T-3090  
003300     03 VOL-PHY-DESC.                                             T-3090  
003310        05 VOL-MED           PICTURE XXX.                         T-3090  
003320        05 VOL-ADJ-FAC REDEFINES VOL-MED     PICTURE  9V99.       T-3090  
003330        05 VOL-RD-WIDTH      PICTURE XXXX.                        T-3090  
003340        05 VOL-NO-LA         PICTURE XX.                          T-3090  
003350     03 FILLER               PICTURE X(9).                        T-3090  
003360 FD  ISFILE                                                       T-3090  
003370     LABEL RECORDS ARE STANDARD                                   T-3090  
003380     RECORDING MODE IS F                                          T-3090  
003390     RECORD CONTAINS 300 CHARACTERS                               T-3090  
003400     BLOCK CONTAINS  10 RECORDS                                   KM120171
003410     DATA RECORD IS ISFILE-REC.                                   T-3090  
003420 01  ISFILE-REC.                                                  T-3090  
003430     03 IS-DELETE-CODE       PICTURE X.                           T-3090  
003440     03 IS-REC-KEY.                                               T-3090  
003450        05 IS-KEY-ST-1       PICTURE X(5).                        T-3090  
003460        05 IS-KEY-ST-2       PICTURE X(5).                        T-3090  
003470     03 IS-NAME.                                                  T-3090  
003480        05 IS-NAME-1         PICTURE X(30).                       T-3090  
003490        05 IS-NAME-2         PICTURE X(30).                       T-3090  
003500     03 IS-CLASSIFICATION.                                                
003510        05 IS-CODES.                                                      
003520           07 IS-CLASS-CODE  PICTURE X.                                   
003530           07 IS-NO-LEGS     PICTURE X.                                   
003540           07 IS-CONTROL     PICTURE X.                                   
003550           07 IS-SIGNAL-CTRL PICTURE X.                                   
003560           07 IS-SIGNAL-CHAR PICTURE XX.                                  
003570        05 IS-DATE.                                                       
003580           07 IS-MO          PICTURE XX.                                  
003590           07 IS-DA          PICTURE XX.                                  
003600           07 IS-YR          PICTURE XX.                                  
003610     03 IS-APPROACH-CHARS.                                        T-3090  
003620        05 IS-LEG-CHAR OCCURS 4 TIMES.                            T-3090  
003630           07 IS-NO-LANES    PICTURE X.                           T-3090  
003640           07 IS-TURN        PICTURE X.                           T-3090  
003650           07 IS-CTRL-SIGN   PICTURE X.                           T-3090  
003660           07 IS-MAST-ARM    PICTURE X.                           T-3090  
003670           07 IS-T-SIG-CTRL  PICTURE X.                           T-3090  
003680           07 IS-LT-SIGN-REG PICTURE X.                           T-3090  
003690           07 IS-LT-PED-REG  PICTURE X.                           T-3090  
003700           07 IS-BUS-ZONE    PICTURE X.                           T-3090  
003710           07 IS-SIG-APPR    PICTURE X.                           T-3090  
003720           07 IS-MED-TYPE    PICTURE X.                           T-3090  
003730           07 IS-MED-WDTH    PICTURE X.                           T-3090  
003740           07 IS-PK-STP-RSTR PICTURE X.                           T-3090  
003750           07 IS-DIR-FLOW    PICTURE X.                           T-3090  
003760           07 IS-SEC-S-LOC   PICTURE X.                           T-3090  
003770     03 IS-CORDINATES.                                            T-3090  
003780        05 IS-X-CORD         PICTURE X(6).                        T-3090  
003790        05 IS-X-CORD-N REDEFINES IS-X-CORD PICTURE 9(6).          T-3090  
003800        05 IS-Y-CORD         PICTURE X(6).                        T-3090  
003810        05 IS-Y-CORD-N REDEFINES IS-Y-CORD PICTURE 9(6).          T-3090  
003820     03 IS-SEGMENT-NO.                                            T-3090  
003830        05 IS-N-LEG          PICTURE 9(5).                                
003840        05 IS-E-LEG          PICTURE 9(5).                                
003850        05 IS-S-LEG          PICTURE 9(5).                                
003860        05 IS-W-LEG          PICTURE 9(5).                                
003870     03 IS-NEXT-IS.                                               T-3090  
003880        05 IS-NEXT-LEG OCCURS 4 TIMES.                            T-3090  
003890           07 IS-NEXT-IS-CODE.                                    T-3090  
003900              09 IS-NEXT-ST-1        PICTURE 9(5).                        
003910              09 IS-NEXT-ST-2        PICTURE 9(5).                        
003920           07 IS-NEXT-IS-DIST   PICTURE 9999.                             
003930           07 IS-NEXT-IS-SLOPE   PICTURE X.                       T-3090  
003940     03 IS-HAZARDOUS-ID.                                          T-3090  
003950        05 IS-YEAR OCCURS 4 TIMES.                                T-3090  
003960           07 IS-YEAR-1      PICTURE X.                           T-3090  
003970           07 IS-YEAR-2      PICTURE X.                           T-3090  
003980           07 IS-YEAR-3      PICTURE X.                           T-3090  
003990           07 IS-YEAR-4      PICTURE X.                           T-3090  
004000     03 IS-DEAD-END          PICTURE X.                           T-3090  
004010     03 IS-VACATED           PICTURE X.                           T-3090  
004020     03 IS-OTHER-IS.                                              T-3090  
004030        05 IS-JOG            PICTURE X.                           T-3090  
004040        05 IS-OTHER-ST.                                           T-3090  
004050           07 IS-OTHER-ST-1  PICTURE 9(5).                                
004060           07 IS-OTHER-ST-2  PICTURE 9(5).                                
004070     03 IS-ASSUME-EW         PICTURE X.                           T-3090  
004080     03 IS-MAINT             PICTURE XXXX.                        T-3090  
004090     03 IS-POL-DIST          PICTURE XXXX.                        T-3090  
004100     03 IS-TR-DIST           PICTURE X.                           T-3090  
004110     03 FILLER               PICTURE X(30).                               
004120 FD  ACCIDENT-FILE                                                        
004130         LABEL RECORDS ARE STANDARD                                       
004140         RECORDING MODE IS F                                              
004150         RECORD CONTAINS 112 CHARACTERS                                   
004160     BLOCK CONTAINS 64 RECORDS                                    KM081871
004170         DATA RECORD IS ACC-IN-REC-1.                                     
004180 01  ACC-IN-REC-1.                                                        
004190     02  FIELD-1.                                                         
004200         03  DELETE-CODE             PICTURE X.                           
004210         03  RECORD-KEY.                                                  
004220             04  IS-CODE-A           PICTURE X(10).               KM120470
004230             04  IND-NO-A            PICTURE 999.                         
004240             04  REC-CODE-A          PICTURE 9.                           
004250         03  FILLER                  PICTURE 9(6).                        
004260         03  SUPL-REC-CODE           PICTURE 9.                           
004270         03  DAY-OF-WEEK             PICTURE 9.                           
004280         03  DATE-OCCURRED.                                               
004290             04  YR-OCCURRED         PICTURE 99.                          
004300             04  MO-OCCURRED         PICTURE 99.                          
004310             04  DY-OCCURRED         PICTURE 99.                          
004320        03 TIME-OCCURRED     PICTURE 9(4).                                
004330        03 FILLER            PICTURE X(7).                                
004340     02  FIELD-2                 PICTURE 9(17)   COMPUTATIONAL-3.         
004350     02  FIELD-3                 PICTURE 9(17)   COMPUTATIONAL-3.         
004360     02  FIELD-4                 PICTURE 9(17)   COMPUTATIONAL-3.         
004370     02  FIELD-5                 PICTURE 9(17)   COMPUTATIONAL-3.         
004380     02  FIELD-6                 PICTURE 9(17)   COMPUTATIONAL-3.         
004390     02  FIELD-7                 PICTURE 9(17)   COMPUTATIONAL-3.         
004400     02  FIELD-8                 PICTURE 9(17)   COMPUTATIONAL-3.         
004410     02  FIELD-9                 PICTURE 9(17)   COMPUTATIONAL-3.         
004420 WORKING-STORAGE SECTION.                                                 
004430 77  FILLER-2        PICTURE X(21) VALUE 'BEGIN WORKING STORAGE'.         
004440 77  UNPROC-SW           PICTURE 9 VALUE ZERO.                            
004450 77  J                       PICTURE 99.                                  
004460 77  H                       PICTURE 9     VALUE 1.                       
004470 77  PASS-S                  PICTURE 9     VALUE ZERO.                    
004480 77  SEARCH-SW               PICTURE 9 VALUE ZERO.                        
004490 77  TYPE-SEARCH             PICTURE X.                                   
004500 77  ISR-SW          PICTURE 9 VALUE ZERO.                                
004510 77  IS-ER           PICTURE 9 VALUE ZERO.                                
004520 77  CTR                     PICTURE 99.                                  
004530 77  C                       PICTURE 9.                                   
004540 77  COUNTER                 PICTURE 999.                                 
004550 77  ISW                     PICTURE 9 VALUE ZERO.                        
004560 77  COUNT-125               PICTURE 999 VALUE ZERO.                      
004570 77  R-SEQ                   PICTURE 9(4) VALUE ZERO.                     
004580 01  REPORT-DATE.                                                         
004590     03 FROM-DATE.                                                        
004600        05 FROM-YR           PICTURE 99.                                  
004610        05 FROM-MO           PICTURE 99.                                  
004620        05 FROM-DA           PICTURE 99.                                  
004630     03 TO-DATE.                                                          
004640        05 TO-YR             PICTURE 99.                                  
004650        05 TO-MO             PICTURE 99.                                  
004660        05 TO-DA             PICTURE 99.                                  
004670 01  A-SYM-KEY.                                                           
004680     03 A-SYM-KEY-IS.                                                     
004690        05 A-SYM-ST-1        PICTURE X(5).                        KM120470
004700        05 A-SYM-ST-2        PICTURE X(5).                        KM120470
004710     03 A-SYM-INDEX          PICTURE 999.                                 
004720     03 A-SYM-REC-CODE       PICTURE 9.                                   
004730 01  SEG-SYM-KEY             PICTURE 9(5).                                
004740 01  SYM-KEY.                                                             
004750     03 SYM-KEY-ST-1         PICTURE X(5).                        KM120470
004760     03 SYM-KEY-ST-2         PICTURE X(5).                        KM120470
004770 01  VOL-SYM-KEY.                                                         
004780     03 VOL-SYM-ST-CODE.                                                  
004790        05 VOL-SYM-ST-1      PICTURE X(5).                        KM120470
004800        05 VOL-SYM-ST-2      PICTURE X(5).                        KM120470
004810     03 VOL-SYM-INDEX                PICTURE XXX.                         
004820     03 VOL-SYM-INDEX-N REDEFINES VOL-SYM-INDEX PICTURE 999.              
004830 01  A-HOLD.                                                              
004840     05 A-OLDEST             PICTURE 999.                                 
004850     05 A-LATEST             PICTURE 999.                                 
004860     05 SU-H                 PICTURE 9.                                   
004870 01  IS-ST-H.                                                             
004880     03 IS-ST1-H             PICTURE X(5).                                
004890     03 IS-ST2-H             PICTURE X(5).                                
004900 01  IS-OTHER-HOLD.                                                       
004910     03 IS-OTHER-1           PICTURE X(5).                        KM120470
004920     03 IS-OTHER-2           PICTURE X(5).                        KM120470
004930 01  OLD-LOC.                                                             
004940     03 POS-1                PICTURE 9.                                   
004950     03 POS-2                PICTURE 9.                                   
004960     03 POS-3                PICTURE 9.                                   
004970 01  ROUTE-ST-HOLD           PICTURE X(5).                        KM120970
004980 01  FIRST-X-ST-HOLD.                                                     
004990     03 FIRST-ST-1           PICTURE X(5).                        KM120470
005000     03 FIRST-ST-2           PICTURE X(5).                        KM120470
005010 01  LAST-X-ST-HOLD.                                                      
005020     03 LAST-ST-1            PICTURE X(5).                        KM120470
005030     03 LAST-ST-2            PICTURE X(5).                        KM120470
005040 01  DIR-CODE-HOLD.                                                       
005050     03 EW-DIR               PICTURE 9.                                   
005060     03 NS-DIR               PICTURE 9.                                   
005070     03 E-DIR                PICTURE 9.                                   
005080     03 W-DIR                PICTURE 9.                                   
005090     03 N-DIR                PICTURE 9.                                   
005100     03 S-DIR                PICTURE 9.                                   
005110     03 R-N-DIR              PICTURE 9.                                   
005120     03 R-E-DIR              PICTURE 9.                                   
005130     03 R-S-DIR              PICTURE 9.                                   
005140     03 R-W-DIR              PICTURE 9.                                   
005150     03 JUST-LEFT-IS.                                                     
005160        05 LEFT-IS-1         PICTURE X(5).                        KM120470
005170        05 LEFT-IS-2         PICTURE X(5).                        KM120470
005180 01  X-Y-HOLD.                                                            
005190     03 FIRST-X-Y.                                                        
005200        05 FIRST-X           PICTURE X(6).                                
005210        05 FIRST-Y           PICTURE X(6).                                
005220     03 LAST-X-Y.                                                         
005230        05 LAST-X            PICTURE X(6).                                
005240        05 LAST-Y            PICTURE X(6).                                
005250 01  IS-WORK.                                                             
005260     03 IS-COUNT             PICTURE 999.                                 
005270 01  SW-INTER.                                                            
005280     03 SW-IS                PICTURE 9 VALUE ZERO.                        
005290     03 SW-N-IS              PICTURE 9 VALUE ZERO.                        
005300 01  NO-ACCIDENTS.                                                        
005310     03 P-AC                 PICTURE 99.                                  
005320     03 I-AC                 PICTURE 99.                                  
005330     03 F-AC                 PICTURE 99.                                  
005340     03 T-AC                 PICTURE 999.                                 
005350 01  SEV-SW.                                                              
005360     03 P-SW                 PICTURE 9.                                   
005370     03 I-SW                 PICTURE 9.                                   
005380     03 F-SW                 PICTURE 9.                                   
005390 01  33-VOL.                                                              
005400     03 33-V-W               PICTURE 9(5)  VALUE ZERO.                    
005410     03 33-V-HOLD            PICTURE 9(6)  VALUE ZERO.                    
005420     03 V-COUNT              PICTURE 999   VALUE ZERO.                    
005430     03 TOT-ACC              PICTURE 9999  VALUE ZERO.                    
005440 01  T23-WORK-AREA.                                                       
005450     03 T23-REQUESTER        PICTURE X(15).                               
005460        03 T23-SEARCH-DATES  PICTURE X(12).                               
005470     03 T23-ROUTE            PICTURE X(5).                        KM120470
005480     03 T23-R-NAME           PICTURE X(30).                               
005490     03 T23-IS-ACC           PICTURE X(70).                               
005500     03 FILLER REDEFINES T23-IS-ACC OCCURS 7 TIMES.                       
005510        05 FILLER    OCCURS 5 TIMES.                                      
005520           07 FLD-IS         PICTURE 99.                                  
005530     03 T23-NON-IS-ACC       PICTURE X(80).                               
005540     03 FILLER     REDEFINES T23-NON-IS-ACC OCCURS 8 TIMES.               
005550        05 FILLER OCCURS 5 TIMES.                                         
005560           07 FLD-NON-IS     PICTURE 99.                                  
005570     03 T23-ACC-PER-M        PICTURE 999V9.                               
005580     03 T23-ACC-PER-MVM      PICTURE 999V9.                               
005590     03 T23-VOL-ADT          PICTURE 9(6).                                
005600     03 FILLER               PICTURE X(44).                               
005610     03 RPT-CODE             PICTURE 99.                                  
005620     03 RQ-NO                PICTURE 999.                                 
005630     03 FILLER               PICTURE XXX.                                 
005640     03 RCD-CODE             PICTURE 9.                                   
005650     03 FILLER               PICTURE X(11).                               
005660 01  CHART-HOLD.                                                          
005670     03 CHART-23 OCCURS 125 TIMES.                                        
005680        05 CHART-ACC-TYPE    PICTURE 9.                                   
005690        05 CHART-P           PICTURE 99.                                  
005700        05 CHART-I           PICTURE 99.                                  
005710        05 CHART-F           PICTURE 99.                                  
005720        05 CHART-IS          PICTURE X(5).                        KM120970
005730        05 CHART-NAME        PICTURE X(30).                               
005740 01  CHART-LOC.                                                           
005750     03 CHART-DIST           PICTURE 9(5).                                
005760     03 FILLER REDEFINES  CHART-DIST.                                     
005770        05 CHART-D           PICTURE 999.                                 
005780        05 FILLER-2-DIG   PICTURE 99.                                     
005790 01  CHART-OLD               PICTURE 9(5).                                
005800 01  FILLER REDEFINES CHART-OLD.                                          
005810     03 CH-OLD-0             PICTURE 9.                                   
005820     03 CH-OLD               PICTURE 99.                                  
005830     03 CH-OLD-A REDEFINES CH-OLD.                                        
005840        05 CH-OLD-1          PICTURE 9.                                   
005850        05 CH-OLD-2          PICTURE 9.                                   
005860     03 CH-OLD-3             PICTURE 99.                                  
005870 01  ROUT-SW                 PICTURE 9 VALUE ZERO.                        
005880 01  IS-DISTANCE             PICTURE 9(5).                                
005890 01  IS-DIS REDEFINES IS-DISTANCE.                                        
005900     03 IS-DIS-A             PICTURE 999.                                 
005910     03 FIL-D-A          PICTURE 99.                                      
005920 01  DUMMY-1.                                                             
005930     02  ACC-IN-REC-2.                                                    
005940         03  DATA-GROUP-1        PICTURE X(40).                           
005950         03  DATA-GROUP-2        PICTURE 9(17).                           
005960         03  DATA-GROUP-3        PICTURE 9(17).                           
005970         03  DATA-GROUP-4        PICTURE 9(17).                           
005980         03  DATA-GROUP-5        PICTURE 9(17).                           
005990         03  DATA-GROUP-6        PICTURE 9(17).                           
006000         03  DATA-GROUP-7        PICTURE 9(17).                           
006010         03  DATA-GROUP-8        PICTURE 9(17).                           
006020         03  DATA-GROUP-9        PICTURE 9(17).                           
006030     02  ACC-HIST-REC    REDEFINES ACC-IN-REC-2.                          
006040     03  A-DELETE-CODE               PICTURE  X.                  ACC-HIST
006050     03  A-RECORD-KEY.                                            ACC-HIST
006060         04  A-IS-CODE               PICTURE  X(10).              KM120470
006070         04  A-IND-CODE              PICTURE  9(3).               ACC-HIST
006080         04  A-REC-CODE              PICTURE  9.                  ACC-HIST
006090     03  A-IND-CODE-OLDEST           PICTURE  S9(3).              ACC-HIST
006100     03  A-IND-CODE-LATEST           PICTURE  S9(3).              ACC-HIST
006110     03  A-SUPL-REC-CODE             PICTURE  9.                  ACC-HIST
006120     03  A-DAY-OF-WEEK               PICTURE   9.                         
006130     03  A-DATE-OCCURRED.                                         ACC-HIST
006140         04  A-YR-OCCURRED           PICTURE   99.                        
006150         04  A-MO-OCCURRED           PICTURE   99.                        
006160         04  A-DA-OCCURRED           PICTURE   99.                        
006170     03  A-TIME-OCCURRED.                                         ACC-HIST
006180         04  A-HOUR-OCCURRED         PICTURE   99.                        
006190         04  A-MIN-OCCURRED          PICTURE   99.                        
006200     03  A-REP-DIST.                                              ACC-HIST
006210         04  A-DIV                   PICTURE  99.                 ACC-HIST
006220         04  A-DIST                  PICTURE  99.                 ACC-HIST
006230     03  A-PROC-CYCLE                PICTURE  99.                         
006240     03  A-ACCID-SEVERITY            PICTURE  9.                          
006250     03  A-DR-NO                     PICTURE  9(6).               ACC-HIST
006260     03  A-HIT-RUN-CODE              PICTURE  9.                  ACC-HIST
006270     03  A-GOV-PROP-CODE             PICTURE  9.                  ACC-HIST
006280     03  A-INVES-UNIT                PICTURE  9.                  ACC-HIST
006290     03  A-PRIMARY-CAUSE.                                         ACC-HIST
006300         04  A-PRIMARY-CODE          PICTURE  9(6).               ACC-HIST
006310         04  A-PRIMARY-GROUP         PICTURE  99.                 ACC-HIST
006320     03  A-TEE-IS-CODE               PICTURE   9.                 ACC-HIST
006330     03  A-POINT-OF-IMPACT.                                       ACC-HIST
006340         04  A-PI-PRIM-FT            PICTURE  9(3).               ACC-HIST
006350         04  A-PI-PRIM-CODE-1        PICTURE  9.                  ACC-HIST
006360         04  A-PI-PRIM-CODE-2        PICTURE  9.                  ACC-HIST
006370         04  A-PI-SEC-FT             PICTURE  9(4).               ACC-HIST
006380         04  A-PI-SEC-CODE-1         PICTURE  9.                  ACC-HIST
006390         04  A-PI-SEC-CODE-2         PICTURE  9.                  ACC-HIST
006400     03  A-INVOL-WITH-CODE           PICTURE  99.                 ACC-HIST
006410     03  A-DIR-ANAL-CODE             PICTURE  9(3).               ACC-HIST
006420     03  A-SURVEY-1                  PICTURE  9.                  ACC-HIST
006430     03  A-SURVEY-2                  PICTURE  9.                  ACC-HIST
006440     03  A-LIGHTING                  PICTURE  9.                  ACC-HIST
006450     03  A-LOCALITY                  PICTURE  9.                  ACC-HIST
006460     03  A-ROAD-CHAR                 PICTURE  9.                  ACC-HIST
006470     03  A-ROAD-COND                 PICTURE  9.                  ACC-HIST
006480     03  A-SPEC-CIRC                 PICTURE  99.                 ACC-HIST
006490     03  A-WEATHER                   PICTURE  9.                  ACC-HIST
006500     03  A-AT-IS-CODE                PICTURE  9.                  ACC-HIST
006510     03  A-ST-REV-CODE               PICTURE 9.                   ACC-HIST
006520     03  A-NO-PER-INJ.                                            ACC-HIST
006530         04  A-TYPE-A                PICTURE 99.                  ACC-HIST
006540         04  A-TYPE-B                PICTURE 99.                  ACC-HIST
006550         04  A-TYPE-C                PICTURE 99.                  ACC-HIST
006560         04  A-TYPE-K                PICTURE 99.                  ACC-HIST
006570     03  A-TR-CONTROLS               PICTURE 9.                   ACC-HIST
006580     03  A-ACCID-TYPE                PICTURE 99.                  ACC-HIST
006590     03  A-ZEROS-1                   PICTURE 9(6).                ACC-HIST
006600     03  A-P1-RES                    PICTURE  9.                  ACC-HIST
006610     03  A-P1-SEX                    PICTURE  9.                  ACC-HIST
006620     03 A-PAR-1.                                                          
006630     04  A-P1-VEH-ACTION             PICTURE  99.                         
006640     04  A-P1-VEH-DIR                PICTURE  9.                  ACC-HIST
006650     04  A-P1-VEH-TYPE               PICTURE  99.                 ACC-HIST
006660     04  A-P1-VEH-CLASS              PICTURE  9.                  ACC-HIST
006670     04  A-P1-VEH-COND               PICTURE  99.                         
006680     04  A-P1-AGE                    PICTURE  99.                 ACC-HIST
006690     04  A-P1-INJ                    PICTURE  9.                  ACC-HIST
006700     04  A-P1-CONTR-CIR              PICTURE  99.                 ACC-HIST
006710     04  A-P1-SOB                    PICTURE  99.                         
006720     04  A-P1-PED-ACTION             PICTURE  99.                 ACC-HIST
006730     03  A-P1-PAS-1-AGE              PICTURE  99.                 ACC-HIST
006740     03  A-P1-PAS-1-SEX              PICTURE  9.                  ACC-HIST
006750     03  A-P1-PAS-1-INJ              PICTURE  9.                  ACC-HIST
006760     03  A-P1-PAS-2-AGE              PICTURE  99.                 ACC-HIST
006770     03  A-P1-PAS-2-SEX              PICTURE  9.                  ACC-HIST
006780     03  A-P1-PAS-2-INJ              PICTURE  9.                  ACC-HIST
006790     03  A-P1-PAS-3-AGE              PICTURE  99.                 ACC-HIST
006800     03  A-P1-PAS-3-SEX              PICTURE  9.                  ACC-HIST
006810     03  A-P1-PAS-3-INJ              PICTURE  9.                  ACC-HIST
006820     03  A-P1-PHYS-COND              PICTURE  9.                  ACC-HIST
006830     03  A-P1-SURVEY-A               PICTURE  9.                  ACC-HIST
006840     03  A-P1-SURVEY-B               PICTURE  9.                  ACC-HIST
006850     03  A-ZEROS-2                   PICTURE 9(3).                ACC-HIST
006860     03  A-P2-RES                    PICTURE  9.                  ACC-HIST
006870     03  A-P2-SEX                    PICTURE  9.                  ACC-HIST
006880     03 A-PAR-2.                                                          
006890     04  A-P2-VEH-ACTION             PICTURE  99.                         
006900     04  A-P2-VEH-DIR                PICTURE  9.                  ACC-HIST
006910     04  A-P2-VEH-TYPE               PICTURE  99.                 ACC-HIST
006920     04  A-P2-VEH-CLASS              PICTURE  9.                  ACC-HIST
006930     04  A-P2-VEH-COND               PICTURE  99.                 ACC-HIST
006940     04  A-P2-AGE                    PICTURE  99.                 ACC-HIST
006950     04  A-P2-INJ                    PICTURE  9.                  ACC-HIST
006960     04  A-P2-CONTR-CIR              PICTURE  99.                 ACC-HIST
006970     04  A-P2-SOB                    PICTURE  99.                 ACC-HIST
006980     04  A-P2-PED-ACTION             PICTURE  99.                 ACC-HIST
006990     03  A-P2-PAS-1-AGE              PICTURE  99.                 ACC-HIST
007000     03  A-P2-PAS-1-SEX              PICTURE  9.                  ACC-HIST
007010     03  A-P2-PAS-1-INJ              PICTURE  9.                  ACC-HIST
007020     03  A-P2-PAS-2-AGE              PICTURE  99.                 ACC-HIST
007030     03  A-P2-PAS-2-SEX              PICTURE  9.                  ACC-HIST
007040     03  A-P2-PAS-2-INJ              PICTURE  9.                  ACC-HIST
007050     03  A-P2-PAS-3-AGE              PICTURE  99.                 ACC-HIST
007060     03  A-P2-PAS-3-SEX              PICTURE  9.                  ACC-HIST
007070     03  A-P2-PAS-3-INJ              PICTURE  9.                  ACC-HIST
007080     03  A-P2-PHYS-COND              PICTURE  9.                  ACC-HIST
007090     03  A-P2-SURVEY-A               PICTURE  9.                  ACC-HIST
007100     03  A-P2-SURVEY-B               PICTURE  9.                  ACC-HIST
007110 01  DUMMY-2.                                                             
007120     02  ACC-IN-REC-3.                                                    
007130         03  DATA-GRP-1          PICTURE X(40).                           
007140         03  DATA-GRP-2          PICTURE 9(17).                           
007150         03  DATA-GRP-3          PICTURE 9(17).                           
007160         03  DATA-GRP-4          PICTURE 9(17).                           
007170         03  DATA-GRP-5          PICTURE 9(17).                           
007180         03  DATA-GRP-6          PICTURE 9(17).                           
007190         03  DATA-GRP-7          PICTURE 9(17).                           
007200         03  DATA-GRP-8          PICTURE 9(17).                           
007210         03  DATA-GRP-9          PICTURE 9(17).                           
007220     02  ACC-SUPL-REC    REDEFINES ACC-IN-REC-3.                          
007230     03  S-DELETE-CODE               PICTURE X.                           
007240     03  S-RECORD-KEY.                                            ACC-HIST
007250         04  S-IS-CODE               PICTURE  X(10).              KM120470
007260         04  S-IND-CODE              PICTURE  9(3).               ACC-HIST
007270         04  S-REC-CODE              PICTURE  9.                  ACC-HIST
007280     03  S-BLANKS-1                  PICTURE X(13).               ACC-HIST
007290     03  S-PARTY-SECTION     OCCURS   4 TIMES.                    ACC-HIST
007300         04 S-RES                    PICTURE  9.                  ACC-HIST
007310         04 S-SEX                    PICTURE  9.                  ACC-HIST
007320          04 S-PAR.                                                       
007330         05 S-VEH-ACTION             PICTURE  99.                         
007340         05 S-VEH-DIR                PICTURE  9.                  ACC-HIST
007350         05 S-VEH-TYPE               PICTURE  99.                 ACC-HIST
007360         05 S-VEH-CLASS              PICTURE  9.                  ACC-HIST
007370         05 S-VEH-COND               PICTURE  99.                 ACC-HIST
007380         05 S-AGE                    PICTURE  99.                 ACC-HIST
007390         05 S-INJ                    PICTURE  9.                  ACC-HIST
007400         05 S-CONTR-CIR              PICTURE  99.                 ACC-HIST
007410         05 S-SOB                    PICTURE  99.                 ACC-HIST
007420         05 S-PED-ACTION             PICTURE  99.                 ACC-HIST
007430         04 S-PAS-1-AGE              PICTURE  99.                 ACC-HIST
007440         04 S-PAS-1-SEX              PICTURE  9.                  ACC-HIST
007450         04 S-PAS-1-INJ              PICTURE  9.                  ACC-HIST
007460         04  S-PAS-2-AGE             PICTURE  99.                 ACC-HIST
007470         04  S-PAS-2-SEX             PICTURE 9.                           
007480         04  S-PAS-2-INJ             PICTURE 9.                           
007490         04  S-PAS-3-AGE             PICTURE  99.                 ACC-HIST
007500         04  S-PAS-3-SEX             PICTURE  9.                  ACC-HIST
007510         04  S-PAS-3-INJ             PICTURE  9.                  ACC-HIST
007520         04  S-PHYS-COND             PICTURE  9.                  ACC-HIST
007530         04  S-SURVEY-A              PICTURE  9.                  ACC-HIST
007540         04  S-SURVEY-B              PICTURE  9.                  ACC-HIST
007550         04  S-ZEROS-1               PICTURE  9(3).                       
007560                                                                          
007570                                                                          
007580 01  MONTH-TABLE.                                                         
007590        03 MONTH-TAB.                                                     
007600           05 FILLER         PICTURE 999 VALUE 031.                       
007610           05 FILLER         PICTURE 999 VALUE 059.                       
007620           05 FILLER         PICTURE 999 VALUE 090.                       
007630           05 FILLER         PICTURE 999 VALUE 120.                       
007640           05 FILLER         PICTURE 999 VALUE 151.                       
007650           05 FILLER         PICTURE 999 VALUE 181.                       
007660           05 FILLER         PICTURE 999 VALUE 212.                       
007670           05 FILLER         PICTURE 999 VALUE 243.                       
007680           05 FILLER         PICTURE 999 VALUE 273.                       
007690           05 FILLER         PICTURE 999 VALUE 304.                       
007700           05 FILLER         PICTURE 999 VALUE 334.                       
007710        03 FILLER REDEFINES MONTH-TAB.                                    
007720           05 MO-ENTRY OCCURS 11 TIMES PICTURE 999.                       
007730 01  T-D-HOLD                PICTURE X(12).                               
007740 01  FILLER REDEFINES T-D-HOLD.                                           
007750     03 C-M                  PICTURE 99.                                  
007760     03 C-D                  PICTURE 99.                                  
007770     03 C-Y                  PICTURE 99.                                  
007780     03 C-M1                 PICTURE 99.                                  
007790     03 C-D1                 PICTURE 99.                                  
007800     03 C-Y1                 PICTURE 99.                                  
007810 01  D-TOTAL.                                                             
007820     03 TO-D-TOT             PICTURE 9999.                                
007830     03 FR-D-TOT             PICTURE 9999.                                
007840     03 YR-D-TOT             PICTURE 9999.                                
007850     03 TND                  PICTURE 9999.                                
007860     03 MO-CT                PICTURE 99.                                  
007870 01  RECORD-COUNTER          PICTURE 9(5) VALUE ZERO.                     
007880 01  PRT-HR.                                                              
007890     03 FILLER       PICTURE X.                                           
007900     03 PRT-IS-1     PICTURE X(30).                                       
007910     03 FILLER       PICTURE XXX VALUE SPACES.                            
007920     03 PRT-IS-2     PICTURE X(30).                                       
007930     03 FILLER       PICTURE X(69) VALUE SPACES.                          
007940 01  SUPL-RD-ON      PICTURE 9 VALUE ZERO.                                
007950 01  DIR-ST-SW       PICTURE 9 VALUE ZERO.                                
007960 01  DIR-VEH-C       PICTURE 9 VALUE ZERO.                                
007970 01  SUPL-INVALID        PICTURE 9 VALUE ZERO.                            
007980 01  JOGGED-SWITCH       PICTURE 9 VALUE ZERO.                            
007984 01  DATE-2YRS       PICTURE 9(6).                                        
007985 01  FILLER REDEFINES DATE-2YRS.                                          
007985     03 YRS-2        PICTURE 99.                                          
007986     03 MOS-2        PICTURE 99.                                          
007988     03 DAS-2        PICTURE 99.                                          
007989 01  END-SW          PICTURE 9 VALUE ZERO.                                
007990 PROCEDURE DIVISION.                                                      
007992     ENTER LINKAGE.                                               KM120470
007994     CALL 'CHGDTF' USING ACCIDENT-FILE.                           KM120470
007996     ENTER COBOL.                                                 KM120470
008000     OPEN INPUT  REQUEST-TAPE                                     KM120470
008002                 ACCIDENT-FILE                                    KM120470
008004                 ISFILE                                           KM120470
008010                 VOLFILE                                          KM120470
008020          OUTPUT REPORT-FILE                                      KM120470
008030                 PRINT-FILE.                                      KM120470
008040 START-PROCESSING.                                                        
008050     READ REQUEST-TAPE AT END, GO TO EOJ.                                 
008060     IF KEY-CODE < 30 GO TO START-PROCESSING.                             
008070     MOVE ZERO TO R-SEQ.                                                  
008080     IF KEY-CODE = 30 PERFORM RTN-30 THRU RTN-30-X                        
008090     PERFORM OTHER-IS-RTN THRU OTHER-IS-RTN-X                             
008100     PERFORM REQ-PROCESS THRU REQ-PROCESS-X.                              
008110     MOVE ZERO TO RECORD-COUNTER.                                         
008120     IF KEY-CODE = 31 PERFORM RTN-31 THRU RTN-31-X.                       
008130     IF KEY-CODE = 33 PERFORM RTN-33 THRU RTN-33-X.                       
008140     MOVE ZERO TO RECORD-COUNTER.                                         
008150     MOVE ZERO TO R-SEQ.                                                  
008160     IF KEY-CODE = 34 PERFORM RTN-34 THRU RTN-34-X                        
008170     PERFORM RTN-34A THRU RTN-34A-X                                       
008180     PERFORM REQ-PROCESS THRU REQ-PROCESS-X.                              
008190     IF KEY-CODE > 34 GO TO EOJ.                                          
008200     MOVE ZERO TO JOGGED-SWITCH.                                          
008210     GO TO START-PROCESSING.                                              
008220 EOJ.                                                                     
008230     CLOSE REQUEST-TAPE                                           KM120470
008240           ACCIDENT-FILE                                          KM120470
008242           ISFILE                                                 KM120470
008250           REPORT-FILE                                            KM120470
008260           VOLFILE                                                KM120470
008270           PRINT-FILE.                                            KM120470
008280     DISPLAY 'T-3060 COMPLETED NORMALLY'.                                 
008290     STOP RUN.                                                            
008300 RTN-30. MOVE ZEROES TO  REPORT-REC.                                      
008310     MOVE SPACE TO TYPE-SEARCH.                                           
008320     MOVE IS-10-REQUESTER TO T10-REQUESTER.                               
008330     MOVE DATE-LIMIT-10 TO T10-SEARCH-DATES.                              
008340     MOVE REQ-NO-10 TO REQ-NO.                                            
008350     MOVE 10 TO REPORT-CODE.                                              
008360     MOVE R-SEQ TO IS-SEQUENCE.                                           
008390     IF IS-10-ST1 GREATER THAN IS-10-ST2 MOVE IS-10-ST1 TO                
008400     A-SYM-ST-2 T10-IS-2 MOVE IS-10-ST2 TO A-SYM-ST-1 T10-IS-1            
008410     ELSE MOVE IS-10 TO A-SYM-KEY-IS T10-IS-CODE.                         
008420 RTN-30-CONT.                                                             
008430     MOVE FR-MO TO  FROM-MO  MOVE FR-DA TO FROM-DA.                       
008440     MOVE FR-YR TO  FROM-YR  MOVE  T-MO TO TO-MO.                         
008450     MOVE  T-DA TO    TO-DA  MOVE  T-YR TO TO-YR.                         
008460 A5. MOVE ZEROES TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE.                
008470 RD-AHFILE.                                                               
008480     READ ACCIDENT-FILE INVALID KEY                                       
008490         MOVE ZERO TO PASS-S GO TO A-ERROR-RTN.                           
008500     IF DELETE-CODE = HIGH-VALUE AND PASS-S = ZERO GO TO RTN-30-X.        
008510     IF DELETE-CODE = HIGH-VALUE AND PASS-S = 1                           
008520         GO TO INCREASE-INDEX.                                            
008530     PERFORM UNPACK THRU UNPACK-X.                                        
008540     IF PASS-S = 1 GO TO A1.                                              
008550     MOVE A-IND-CODE-OLDEST TO A-OLDEST.                                  
008560     MOVE A-IND-CODE-LATEST TO A-LATEST.                                  
008570 A1. IF TYPE-SEARCH = SPACE GO TO A2.                                     
008580     PERFORM TYPE-SEARCH-RTN THRU TYPE-SEARCH-RTN-X.                      
008590     IF SEARCH-SW = 1 MOVE ZERO TO SEARCH-SW GO TO A2 ELSE                
008600          GO TO INCREASE-INDEX.                                           
008610 A2.                                                                      
008615     IF A-DIR-ANAL-CODE = 390 GO TO INCREASE-INDEX.                       
008620     IF FROM-DATE = ZERO GO TO A3.                                        
008630     IF A-DATE-OCCURRED LESS THAN FROM-DATE GO TO INCREASE-INDEX.         
008640     IF A-DATE-OCCURRED GREATER THAN TO-DATE GO TO INCREASE-INDEX.        
008650 A3.                                                                      
008660     ADD 1 TO RECORD-COUNTER.                                             
008670     MOVE A-DAY-OF-WEEK TO DOW.                                           
008680     MOVE A-DATE-OCCURRED TO T10-DATE MOVE A-SUPL-REC-CODE TO SU-H        
008690     MOVE A-DATE-OCCURRED TO CODE-IS-DATE.                                
008700     MOVE A-TIME-OCCURRED TO T10-TIME                                     
008710     MOVE A-DR-NO TO T10-DR-NO MOVE A-PRIMARY-GROUP TO GP-CODE            
008720     MOVE A-PRIMARY-CODE TO PRI-CODE                                      
008730     MOVE A-POINT-OF-IMPACT TO T10-PT-IMPACT                              
008740     MOVE A-DIR-ANAL-CODE TO     DIR-ANL                                  
008750     MOVE A-LIGHTING TO LIGHT                                             
008760     MOVE A-ROAD-COND TO RD-CONDITION                                     
008770     MOVE A-SPEC-CIRC TO SPEC-CIR                                         
008780     MOVE A-WEATHER TO WEATHER                                            
008790     MOVE A-AT-IS-CODE TO T10-AT-IS                                       
008800     MOVE A-NO-PER-INJ TO T10-NO-PER-INJ                                  
008810     MOVE A-ACCID-SEVERITY TO T10-A-SEV                                   
008820     MOVE A-ST-REV-CODE TO T10-REVERSE-ST-C                               
008830     MOVE A-SUPL-REC-CODE TO T10-NO-PARTIES                               
008840     MOVE A-ACCID-TYPE TO T10-ACC-TYPE                                    
008850     MOVE A-PAR-1 TO T10-PARTY-ENTRY (1).                                 
008860     MOVE A-PAR-2 TO T10-PARTY-ENTRY (2).                                 
008870     PERFORM  PARTY-COUNT THRU PARTY-COUNT-X VARYING J FROM 1 BY 1        
008880         UNTIL J GREATER THAN A-SUPL-REC-CODE. MOVE 1 TO H.               
008890     IF SUPL-REC-CODE LESS THAN 3    WRITE REPORT-REC                     
008900         GO TO INCREASE-INDEX.                                            
008910     IF SUPL-INVALID = 1 MOVE ZERO TO SUPL-INVALID                        
008920         WRITE REPORT-REC GO TO INCREASE-INDEX.                           
008930     IF SUPL-RD-ON = ZERO NEXT SENTENCE ELSE                              
008940     MOVE ZERO TO SUPL-RD-ON GO TO A4A.                                   
008950     ADD 1 TO A-SYM-REC-CODE.                                             
008960     READ ACCIDENT-FILE INVALID KEY WRITE REPORT-REC                      
008970     GO TO INCREASE-INDEX.                                                
008980     PERFORM UNPACK-S THRU UNPACK-SX.                                     
008990 A4A.                                                                     
009000     MOVE S-PAR (1) TO T10-PARTY-ENTRY (3).                               
009010     MOVE S-PAR (2) TO T10-PARTY-ENTRY (4).                               
009020     MOVE S-PAR (3) TO T10-PARTY-ENTRY (5).                               
009030     MOVE S-PAR (4) TO T10-PARTY-ENTRY (6).                               
009040     IF SU-H LESS THAN 7  WRITE REPORT-REC GO TO INCREASE-INDEX.          
009050     ADD 1 TO A-SYM-REC-CODE.                                             
009060     READ ACCIDENT-FILE INVALID KEY WRITE REPORT-REC GO TO                
009070         INCREASE-INDEX.                                                  
009080     PERFORM UNPACK-S THRU UNPACK-SX.                                     
009090     MOVE S-PAR (1) TO T10-PARTY-ENTRY (7).                               
009100     MOVE S-PAR (2) TO T10-PARTY-ENTRY (8).                               
009110     MOVE S-PAR (3) TO T10-PARTY-ENTRY (9).                               
009120     WRITE REPORT-REC.                                                    
009130 INCREASE-INDEX.                                                          
009140     IF KEY-CODE = 30 PERFORM RTN-30 THRU RTN-30-CONT                     
009150         GO TO R-30-U.                                                    
009160     MOVE ZERO TO REPORT-REC PERFORM M0 THRU M1                           
009170     MOVE R-SEQ TO IS-SEQUENCE.                                           
009190       MOVE A-SYM-KEY-IS TO T10-IS-CODE.                                  
009200 R-30-U.                                                                  
009210     MOVE 1 TO PASS-S.                                                    
009220     IF A-OLDEST GREATER THAN A-LATEST OR A-OLDEST EQUAL TO ZERO          
009230     MOVE ZERO TO PASS-S                                                  
009240     GO TO RTN-30-X.                                                      
009250     MOVE A-OLDEST TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE               
009260     ADD 1 TO A-OLDEST GO TO RD-AHFILE.                                   
009270 RTN-30-X. EXIT.                                                          
009280 A-ERROR-RTN.                                                             
009290     MOVE 1 TO UNPROC-SW.                                                 
009310     PERFORM UNP-A THRU UNP-AX                                            
009320     MOVE 'THERE ARE NO ACCIDENTS RECORDED FOR REQUESTED LOCATION'        
009330         TO PRINT-LINE                                                    
009340     WRITE PRINT-REC AFTER ADVANCING 2 LINES                              
009350     GO TO RTN-30-X.                                                      
009360 REQ-PROCESS.                                                             
009370     IF UNPROC-SW = 1 MOVE ZERO TO UNPROC-SW GO TO REQ-PROCESS-A.         
009380     IF RECORD-COUNTER = ZERO                                             
009390     PERFORM UNP-A THRU UNP-AX                                            
009400     MOVE 'MASTER FILE DOES NOT CONTAIN RECORDS FOR REQUESTED DATE        
009410-        ' LIMITS'  TO PRINT-LINE                                         
009420     WRITE PRINT-REC AFTER ADVANCING 2 LINES.                             
009430 REQ-PROCESS-A.                                                           
009440     MOVE ZERO TO RECORD-COUNTER.                                         
009450 REQ-PROCESS-X. EXIT.                                                     
009460 UNPACK.                                                                  
009470     MOVE FIELD-1 TO DATA-GROUP-1.                                        
009480     MOVE FIELD-2 TO DATA-GROUP-2.                                        
009490     MOVE FIELD-3 TO DATA-GROUP-3.                                        
009500     MOVE FIELD-4 TO DATA-GROUP-4.                                        
009510     MOVE FIELD-5 TO DATA-GROUP-5.                                        
009520     MOVE FIELD-6 TO DATA-GROUP-6.                                        
009530     MOVE FIELD-7 TO DATA-GROUP-7.                                        
009540     MOVE FIELD-8 TO DATA-GROUP-8.                                        
009550     MOVE FIELD-9 TO DATA-GROUP-9.                                        
009560 UNPACK-X. EXIT.                                                          
009570 UNPACK-S.                                                                
009580     MOVE FIELD-1 TO DATA-GRP-1. MOVE FIELD-2 TO DATA-GRP-2.              
009590     MOVE FIELD-3 TO DATA-GRP-3. MOVE FIELD-4 TO DATA-GRP-4.              
009600     MOVE FIELD-5 TO DATA-GRP-5. MOVE FIELD-6 TO DATA-GRP-6.              
009610     MOVE FIELD-7 TO DATA-GRP-7. MOVE FIELD-8 TO DATA-GRP-8.              
009620     MOVE FIELD-9 TO DATA-GRP-9.                                          
009630 UNPACK-SX. EXIT.                                                         
009640 PARTY-COUNT.                                                             
009650     MOVE H TO PARTY-NO (J)                                               
009660     ADD 1 TO H.                                                          
009670 PARTY-COUNT-X. EXIT.                                                     
009680 RTN-31.                                                                  
009690     MOVE ZERO TO JUST-LEFT-IS, R-SEQ.                                    
009700     MOVE ZEROES TO REPORT-REC, DIR-CODE-HOLD, IS-WORK, X-Y-HOLD.         
009710     ADD 1 TO R-SEQ.                                                      
009720 M0. MOVE R-REQUESTER TO T10-REQUESTER.                                   
009730     MOVE R-DATE-LIMIT TO T10-SEARCH-DATES.                               
009740     MOVE R-10-REQ-NO TO REQ-NO.                                          
009750     MOVE R-SEQ TO IS-SEQUENCE.                                           
009760     MOVE 10 TO REPORT-CODE.                                              
009770 M1. MOVE R-FR-MO TO FROM-MO     MOVE R-FR-DA TO FROM-DA.                 
009780     MOVE R-FR-YR TO FROM-YR     MOVE R-T-MO  TO   TO-MO.                 
009790     MOVE R-T-DA  TO   TO-DA     MOVE R-T-YR  TO   TO-YR.                 
009800 M3. MOVE R-ST-CODE TO ROUTE-ST-HOLD.                                     
009810     IF R-ST-CODE < FIRST-ST-CODE MOVE R-ST-CODE TO FIRST-ST-1            
009820         MOVE FIRST-ST-CODE TO FIRST-ST-2 ELSE                            
009830     MOVE R-ST-CODE TO FIRST-ST-2                                         
009840     MOVE FIRST-ST-CODE TO FIRST-ST-1.                                    
009850     IF R-ST-CODE < LAST-ST-CODE MOVE R-ST-CODE TO LAST-ST-1              
009860         MOVE LAST-ST-CODE TO LAST-ST-2   ELSE                            
009870     MOVE R-ST-CODE TO LAST-ST-2 MOVE LAST-ST-CODE TO LAST-ST-1.          
009880 M2. MOVE LAST-X-ST-HOLD TO SYM-KEY.                                      
009890     READ ISFILE INVALID KEY                                              
009900         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY          
009910         GO TO RTN-31-X.                                                  
009920     MOVE IS-CORDINATES TO LAST-X-Y.                                      
009930     MOVE FIRST-X-ST-HOLD TO SYM-KEY, T10-IS-CODE.                        
009940     READ ISFILE INVALID KEY                                              
009950         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY          
009960         GO TO RTN-31-X.                                                  
009970     MOVE IS-CORDINATES TO FIRST-X-Y.                                     
009980     IF R-ST-CODE = IS-NEXT-ST-1 (1) OR R-ST-CODE =                       
009990         IS-NEXT-ST-2 (1) OR R-ST-CODE = IS-NEXT-ST-1 (3) OR              
010000         R-ST-CODE = IS-NEXT-ST-2 (3) MOVE 1 TO NS-DIR                    
010010     GO TO RTN-31-A.                                                      
010020     IF R-ST-CODE = IS-NEXT-ST-1 (2) OR R-ST-CODE =                       
010030         IS-NEXT-ST-2 (2) OR R-ST-CODE = IS-NEXT-ST-1 (4) OR              
010040         R-ST-CODE = IS-NEXT-ST-2 (4) MOVE 1 TO EW-DIR                    
010050         GO TO RTN-31-A.                                                  
010060     DISPLAY 'UNABLE TO DETERMINE ROUTE DIRECTION FROM I/S FILE'.         
010070     DISPLAY SYM-KEY       ' I/S RECORD CODE'.                            
010080     DISPLAY 'T-10 (ROUTE) REQUEST NO. ' REQ-NO GO TO RTN-31-X.           
010090 RTN-31-A.                                                                
010100     IF EW-DIR NOT = 1 GO TO RTN-31-B.                                    
010110     IF FIRST-X < LAST-X MOVE 1 TO E-DIR R-E-DIR GO TO RTN-31-C.          
010120     IF FIRST-X > LAST-X MOVE 1 TO W-DIR R-W-DIR GO TO RTN-31-C.          
010122     IF R-DIRECTION = 3 MOVE 1 TO E-DIR, R-E-DIR GO TO RTN-31-C.  KM010671
010124     IF R-DIRECTION = 4 MOVE 1 TO W-DIR, R-W-DIR GO TO RTN-31-C.  KM010671
010130 1A. DISPLAY 'DIRECTION CAN NOT BE DETERMINED FROM X-Y'.                  
010140         GO TO RTN-31-X.                                                  
010150 RTN-31-B.                                                                
010160     IF FIRST-Y < LAST-Y MOVE 1 TO N-DIR R-N-DIR GO TO RTN-31-C.          
010170     IF FIRST-Y > LAST-Y MOVE 1 TO S-DIR R-S-DIR GO TO RTN-31-C.          
010172     IF R-DIRECTION = 1 MOVE 1 TO N-DIR, R-N-DIR GO TO RTN-31-C.  KM010671
010174     IF R-DIRECTION = 2 MOVE 1 TO S-DIR, R-S-DIR GO TO RTN-31-C.  KM010671
010180     GO TO 1A.                                                            
010190 RTN-31-C. MOVE ZEROES TO IS-COUNT, IS-DISTANCE.                          
010200     MOVE R-SEARCH TO TYPE-SEARCH. MOVE ZERO TO SEARCH-SW.                
010210     MOVE FIRST-X-ST-HOLD TO A-SYM-KEY-IS. ADD 1 TO IS-COUNT.             
010220     MOVE FIRST-X-ST-HOLD TO T10-IS-CODE IS-10.                           
010230     MOVE ZEROES TO A-SYM-INDEX  MOVE 1 TO A-SYM-REC-CODE.                
010240     DISPLAY SYM-KEY ' FIRST I/S ON ROUTE'.                               
010250 CONTINUE-ROUTE.                                                          
010260     PERFORM RD-AHFILE THRU RTN-30-X.                                     
010270     PERFORM 31-OTHER THRU 31-OTHER-X.                                    
010280     PERFORM REQ-PROCESS THRU REQ-PROCESS-X.                              
010282     IF IS-ER = 1 MOVE ZERO TO IS-ER GO TO RTN-31-X.                      
010290     MOVE ZERO TO RECORD-COUNTER.                                         
010300     IF LAST-X-ST-HOLD = SYM-KEY GO TO RTN-31-X.                          
010310     IF IS-DISTANCE > 60000 GO TO RTN-31-X.                               
010320     IF IS-COUNT > 100 GO TO RTN-31-X.                                    
010330     PERFORM NEXT-IS-RTN THRU NEXT-IS-RTN-X.                              
010340     IF IS-ER = 1 MOVE 0 TO IS-ER GO TO RTN-31-X.                         
010350     MOVE ZEROES TO DETAIL-REC-VAR.                                       
010360     ADD 1 TO R-SEQ.                                                      
010370     MOVE R-SEQ TO IS-SEQUENCE.                                           
010380     MOVE SYM-KEY         TO T10-IS-CODE IS-10.                           
010390     GO TO CONTINUE-ROUTE.                                                
010400 RTN-31-X. EXIT.                                                          
010410 31-OTHER.                                                                
010412     MOVE ZERO TO IS-ER.                                                  
010420     MOVE ZERO TO JOGGED-SWITCH.                                          
010430     MOVE SYM-KEY TO IS-OTHER-HOLD.                                       
010440     IF IS-JOG = ZERO GO TO 31-OTHER-X.                                   
010450     IF IS-JOG = '1' GO TO 31-OTHER-B.                                    
010460     IF IS-JOG NOT = '2' GO TO 31-OTHER-X.                                
010462     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO IS-ER GO TO 31-OTHER-X.        
010470     IF IS-OTHER-ST = ZERO GO TO 31-OTHER-X.                              
010480     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 31-OTHER-X.                     
010490     MOVE IS-OTHER-ST TO T10-IS-CODE, A-SYM-KEY-IS, IS-10.                
010500     MOVE ZERO TO A-SYM-INDEX  MOVE 1 TO A-SYM-REC-CODE.                  
010510     ADD 1 TO R-SEQ.                                                      
010520     MOVE R-SEQ TO IS-SEQUENCE.                                           
010530     PERFORM RD-AHFILE THRU RTN-30-X.                                     
010540     IF (N-DIR = 1) AND ((IS-NEXT-ST-1 (1) = ROUTE-ST-HOLD) OR            
010550     (IS-NEXT-ST-2 (1) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.                
010560     IF (E-DIR = 1) AND ((IS-NEXT-ST-1 (2) = ROUTE-ST-HOLD) OR            
010570     (IS-NEXT-ST-2 (2) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.                
010580     IF (S-DIR = 1) AND ((IS-NEXT-ST-1 (3) = ROUTE-ST-HOLD) OR            
010590     (IS-NEXT-ST-2 (3) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.                
010600     IF (W-DIR = 1) AND ((IS-NEXT-ST-1 (4) = ROUTE-ST-HOLD) OR            
010610     (IS-NEXT-ST-2 (4) = ROUTE-ST-HOLD)) GO TO 31-OTHER-X.                
010620     MOVE IS-OTHER-ST TO SYM-KEY.                                         
010630     READ ISFILE INVALID KEY GO TO 31-OTHER-D.                            
010640     GO TO 31-OTHER-X.                                                    
010650 31-OTHER-B.                                                              
010660     IF IS-OTHER-ST = ZERO GO TO 31-OTHER-X.                              
010670     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 31-OTHER-D.                     
010680     MOVE IS-OTHER-ST TO SYM-KEY.                                         
010690     MOVE IS-OTHER-ST TO A-SYM-KEY-IS, IS-10.                             
010700     MOVE IS-OTHER-ST TO T10-IS-CODE.                                     
010720     MOVE ZERO TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE.                  
010725     ADD 1 TO R-SEQ.                                                      
010730     MOVE R-SEQ TO IS-SEQUENCE.                                           
010740     PERFORM RD-AHFILE THRU RTN-30-X.                                     
010742     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO IS-ER.                         
010750     READ ISFILE INVALID KEY GO TO 31-OTHER-D.                            
010760     GO TO 31-OTHER-B.                                                    
010770 31-OTHER-D.                                                              
010780     MOVE IS-OTHER-HOLD TO SYM-KEY.                                       
010790     READ ISFILE INVALID KEY GO TO 31-OTHER-X.                            
010800 31-OTHER-X.  EXIT.                                                       
010810 NEXT-IS-RTN.                                                             
010820     PERFORM ROUTE-DIR THRU ROUTE-DIR-X.                                  
010830     IF IS-ER = 1 GO TO NEXT-IS-RTN-X.                                    
010840     MOVE SYM-KEY TO JUST-LEFT-IS.                                        
010850     IF N-DIR = 1 MOVE IS-NEXT-IS-CODE (1) TO SYM-KEY                     
010860         ADD IS-NEXT-IS-DIST (1) TO IS-DISTANCE  GO TO N1.                
010870     IF E-DIR = 1 MOVE IS-NEXT-IS-CODE (2) TO SYM-KEY                     
010880         ADD IS-NEXT-IS-DIST (2) TO IS-DISTANCE  GO TO N1.                
010890     IF S-DIR = 1 MOVE IS-NEXT-IS-CODE (3) TO SYM-KEY                     
010900         ADD IS-NEXT-IS-DIST (3) TO IS-DISTANCE  GO TO N1.                
010910     IF W-DIR = 1 MOVE IS-NEXT-IS-CODE (4) TO SYM-KEY                     
010920         ADD IS-NEXT-IS-DIST (4) TO IS-DISTANCE  GO TO N1.                
010930 N1. MOVE SYM-KEY TO A-SYM-KEY-IS MOVE 1 TO A-SYM-REC-CODE.               
010940     MOVE ZEROES TO A-SYM-INDEX.                                          
010950     ADD 1 TO IS-COUNT.                                                   
010955     ADD 100 TO IS-DISTANCE.                                              
010956     IF FIL-D-A > 50 ADD 100 TO IS-DISTANCE.                              
010957     MOVE ZERO TO FIL-D-A.                                                
010960     READ ISFILE INVALID KEY                                              
010970         DISPLAY 'I/S FILE DOES NOT CONTAIN NEXT I/S RECORD'              
010980     DISPLAY IS-OTHER-HOLD ' I/S RECORD CODE'                             
010990         DISPLAY SYM-KEY MOVE 1 TO IS-ER GO TO NEXT-IS-RTN-X.             
011000     DISPLAY  SYM-KEY ' NEXT I/S ON ROUTE'.                               
011010 NEXT-IS-RTN-X. EXIT.                                                     
011020 RTN-33.                                                                  
011030     MOVE ZERO TO JUST-LEFT-IS.                                           
011040     MOVE ZEROES TO REPORT-REC, DIR-CODE-HOLD, IS-WORK, X-Y-HOLD,         
011050         T23-WORK-AREA, IS-COUNT, 33-VOL.                                 
011060     MOVE ZERO TO CHART-HOLD, CHART-LOC, CHART-OLD.                       
011065     MOVE ZERO TO ROUT-SW.                                                
011070     MOVE 00100 TO IS-DISTANCE.                                           
011080     MOVE R-REQUESTER TO T23-REQUESTER.                                   
011090     MOVE R-DATE-LIMIT TO T23-SEARCH-DATES.                               
011100     MOVE R-DATE-LIMIT TO T-D-HOLD.                                       
011110     PERFORM NO-D-RTN THRU NO-D-RTN-X.                                    
011120     MOVE R-10-REQ-NO TO REQ-NO, RQ-NO.                                   
011130     MOVE 23 TO REPORT-CODE, RPT-CODE.                                    
011140     MOVE 1 TO RCD-CODE, MOVE 2 TO REC-CODE.                              
011150     PERFORM M1 THRU M3.                                                  
011160     MOVE R-ST-CODE TO T23-ROUTE.                                         
011170     MOVE LAST-X-ST-HOLD TO SYM-KEY.                                      
011180     READ ISFILE INVALID KEY                                              
011190         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY          
011200         GO TO RTN-33-X.                                                  
011210     MOVE IS-CORDINATES TO LAST-X-Y.                                      
011220     IF R-ST-CODE = SYM-KEY-ST-1 MOVE IS-NAME-1 TO T23-R-NAME ELSE        
011230     MOVE IS-NAME-2 TO T23-R-NAME.                                        
011240     MOVE FIRST-X-ST-HOLD TO SYM-KEY.                                     
011250     READ ISFILE INVALID KEY                                              
011260         DISPLAY 'INVALID I/S CODE, ROUTE UNDETERMINED ' SYM-KEY          
011270         GO TO RTN-33-X.                                                  
011280     MOVE IS-CORDINATES TO FIRST-X-Y.                                     
011290     PERFORM IS-LOC THRU IS-LOC-X.                                        
011300     ADD 1 TO IS-COUNT.                                                   
011310     IF R-ST-CODE = IS-NEXT-ST-1 (1) OR R-ST-CODE =                       
011320         IS-NEXT-ST-2 (1) OR R-ST-CODE = IS-NEXT-ST-1 (3) OR              
011330         R-ST-CODE = IS-NEXT-ST-2 (3) MOVE 1 TO NS-DIR                    
011340         GO TO RTN-33-A.                                                  
011350     IF R-ST-CODE = IS-NEXT-ST-1 (2) OR R-ST-CODE =                       
011360         IS-NEXT-ST-2 (2) OR R-ST-CODE = IS-NEXT-ST-1 (4) OR              
011370         R-ST-CODE = IS-NEXT-ST-2 (4) MOVE 1 TO EW-DIR                    
011380         GO TO RTN-33-A.                                                  
011390     DISPLAY 'UNABLE TO DETERMINE ROUTE DIRECTION FROM I/S FILE'.         
011400     DISPLAY SYM-KEY       ' I/S RECORD CODE'.                            
011410     DISPLAY 'T-22 REQUEST NO. ' REQ-NO GO TO RTN-33-X.                   
011420 RTN-33-A.                                                                
011430     IF EW-DIR NOT = 1 GO TO RTN-33-B.                                    
011440     IF FIRST-X < LAST-X MOVE 1 TO E-DIR R-E-DIR GO TO RTN-33-C.          
011450     IF FIRST-X > LAST-X MOVE 1 TO W-DIR R-W-DIR GO TO RTN-33-C.          
011460 2A. DISPLAY 'DIRECTION CAN NOT BE DETERMIND FROM X-Y'.                   
011470         GO TO RTN-33-X.                                                  
011480 RTN-33-B.                                                                
011490     IF FIRST-Y < LAST-Y MOVE 1 TO N-DIR R-N-DIR GO TO RTN-33-C.          
011500     IF FIRST-Y > LAST-Y MOVE 1 TO S-DIR R-S-DIR GO TO RTN-33-C.          
011510     GO TO 2A.                                                            
011520 RTN-33-C.                                                                
011530     MOVE R-SEARCH TO TYPE-SEARCH, MOVE ZERO TO SEARCH-SW,                
011540     MOVE FIRST-X-ST-HOLD TO A-SYM-KEY-IS.                                
011550 3A. MOVE ZERO TO A-SYM-INDEX, MOVE 1 TO A-SYM-REC-CODE.                  
011560     PERFORM RTN-33-AHFILE THRU RTN-33-AHFILE-X.                          
011570     IF SYM-KEY = LAST-X-ST-HOLD GO TO RTN-33-E.                          
011572     IF END-SW = 1  MOVE ZERO TO END-SW GO TO RTN-33-E.                   
011580     IF IS-COUNT = 24 GO TO RTN-33-E.                                     
011590     PERFORM NEXT-IS-RTN THRU NEXT-IS-RTN-X.                              
011600     IF IS-ER = 1 MOVE ZERO TO IS-ER GO TO RTN-33-E.                      
011610     PERFORM IS-LOC THRU IS-LOC-X.                                        
011620     GO TO 3A.                                                            
011630 RTN-33-E.                                                                
011640     PERFORM UP-IND-A THRU UP-IND-A-X.                                    
011645     COMPUTE IS-DISTANCE ROUNDED =                                        
011646         (IS-DISTANCE - (IS-COUNT * 100)).                                
011650     IF V-COUNT NOT = ZERO                                                
011660         COMPUTE T23-VOL-ADT ROUNDED = (33-V-HOLD / V-COUNT).             
011670     IF IS-DISTANCE NOT = ZERO                                            
011680         COMPUTE T23-ACC-PER-M ROUNDED = (TOT-ACC / (IS-DISTANCE          
011690             / 5280)).                                                    
011700     IF T23-VOL-ADT NOT = ZERO AND IS-DISTANCE NOT = ZERO                 
011710         COMPUTE T23-ACC-PER-MVM ROUNDED =                                
011720      TOT-ACC / ((TND * T23-VOL-ADT * (IS-DISTANCE / 5280))               
011730             / 1000000).                                          DC062871
011740     WRITE REPORT-REC FROM T23-WORK-AREA.                                 
011750 RTN-33-X. EXIT.                                                          
011760 33-OTHER.                                                                
011765     MOVE ZERO TO END-SW.                                                 
011770     MOVE SYM-KEY TO IS-OTHER-HOLD.                                       
011780     IF IS-JOG = ZERO GO TO 33-OTHER-X.                                   
011790     IF IS-JOG = '1' GO TO 33-OTHER-B.                                    
011800     IF IS-JOG NOT = '2' GO TO 33-OTHER-X.                                
011805     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO END-SW                         
011806         GO TO 33-OTHER-X.                                                
011810     IF IS-OTHER-ST = ZERO GO TO 33-OTHER-X.                              
011820     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 33-OTHER-X.                     
011830     MOVE IS-OTHER-ST TO A-SYM-KEY-IS.                                    
011840     MOVE ZERO TO A-SYM-INDEX.                                            
011850     MOVE 1 TO A-SYM-REC-CODE.                                            
011860     PERFORM RTN-33-AHFILE THRU 33-UP-IND-X.                              
011870     IF (N-DIR = 1) AND ((IS-NEXT-ST-1 (1) = ROUTE-ST-HOLD) OR            
011880     (IS-NEXT-ST-2 (1) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.                
011890     IF (E-DIR = 1) AND ((IS-NEXT-ST-1 (2) = ROUTE-ST-HOLD) OR            
011900     (IS-NEXT-ST-2 (2) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.                
011910     IF (S-DIR = 1) AND ((IS-NEXT-ST-1 (3) = ROUTE-ST-HOLD) OR            
011920     (IS-NEXT-ST-2 (3) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.                
011930     IF (W-DIR = 1) AND ((IS-NEXT-ST-1 (4) = ROUTE-ST-HOLD) OR            
011940     (IS-NEXT-ST-2 (4) = ROUTE-ST-HOLD)) GO TO 33-OTHER-X.                
011950     MOVE IS-OTHER-ST TO SYM-KEY.                                         
011960     READ ISFILE INVALID KEY GO TO 33-OTHER-D.                            
011970     GO TO 33-OTHER-X.                                                    
011980 33-OTHER-B.                                                              
011990     IF IS-OTHER-ST = ZERO GO TO 33-OTHER-X.                              
012000     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO 33-OTHER-D.                     
012010     MOVE IS-OTHER-ST TO SYM-KEY.                                         
012020     MOVE IS-OTHER-ST TO A-SYM-KEY-IS.                                    
012030     MOVE ZERO TO A-SYM-INDEX.                                            
012040     MOVE 1 TO A-SYM-REC-CODE.                                            
012050     PERFORM RTN-33-AHFILE THRU 33-UP-IND-X.                              
012055     IF SYM-KEY = LAST-X-ST-HOLD MOVE 1 TO END-SW.                        
012060     READ ISFILE INVALID KEY GO TO 33-OTHER-D.                            
012070     GO TO 33-OTHER-B.                                                    
012080 33-OTHER-D.                                                              
012090     MOVE IS-OTHER-HOLD TO SYM-KEY.                                       
012100     READ ISFILE INVALID KEY GO TO 33-OTHER-X.                            
012110 33-OTHER-X.  EXIT.                                                       
012120 RTN-33-AHFILE.                                                           
012130     READ ACCIDENT-FILE INVALID KEY                                       
012140         DISPLAY A-SYM-KEY ' ACCIDENT RECORD NOT FOUND'                   
012150         MOVE ZERO TO PASS-S GO TO 33-UP-IND-X.                           
012160     IF DELETE-CODE = HIGH-VALUE AND PASS-S = ZERO                        
012170          GO TO 33-UP-IND-X.                                              
012180     IF DELETE-CODE = HIGH-VALUE AND PASS-S = 1 GO TO 33-UP-IND.          
012190     PERFORM UNPACK THRU UNPACK-X.                                        
012200     IF PASS-S = 1 GO TO 33-A.                                            
012210     MOVE A-IND-CODE-OLDEST TO A-OLDEST.                                  
012220     MOVE A-IND-CODE-LATEST TO A-LATEST.                                  
012230 33-A.                                                                    
012235     MOVE ZERO TO ROUT-SW.                                                
012240     IF TYPE-SEARCH = SPACE GO TO RTN-33-AHFILE-X.                        
012250     PERFORM TYPE-SEARCH-RTN THRU TYPE-SEARCH-RTN-X.                      
012260     IF SEARCH-SW = 1 MOVE ZERO TO SEARCH-SW GO TO 33-B                   
012270            ELSE GO TO 33-UP-IND.                                         
012280 33-B.                                                                    
012285     IF A-DIR-ANAL-CODE = 390 GO TO 33-UP-IND.                            
012290     IF A-DATE-OCCURRED < FROM-DATE GO TO 33-UP-IND.                      
012300     IF A-DATE-OCCURRED > TO-DATE GO TO 33-UP-IND.                        
012310     IF A-AT-IS-CODE = 1 MOVE 1 TO SW-IS GO TO 33-CONT.                   
012320     IF A-AT-IS-CODE NOT = 2 GO TO 33-UP-IND.                             
012340     MOVE 1 TO SW-IS.                                                     
012350 33-CONT. MOVE ZEROES TO SEV-SW.                                          
012360     IF A-ACCID-SEVERITY = 5 MOVE 1 TO P-SW GO TO IU.                     
012370     IF A-ACCID-SEVERITY = 4 MOVE 1 TO F-SW GO TO IU.                     
012380     IF A-ACCID-SEVERITY = 1  OR   A-ACCID-SEVERITY = 2 OR                
012390        A-ACCID-SEVERITY = 3 MOVE 1 TO I-SW ELSE                          
012400         DISPLAY A-ACCID-SEVERITY ' INVALID SEVERITY CODE'                
012410         GO TO 33-UP-IND.                                                 
012420 IU.                                                                      
012430     IF ROUT-SW = ZERO MOVE IS-DIS-A TO CHART-D.                          
012470     IF ROUT-SW = ZERO GO TO IU-1.                                        
012480     IF A-PI-PRIM-CODE-1 = 9 GO TO OLD-RT.                                
012490     IF R-N-DIR = 1 AND A-PI-SEC-CODE-1 = 1 GO TO ADD-DIS.                
012500     IF R-E-DIR = 1 AND A-PI-SEC-CODE-1 = 2 GO TO ADD-DIS.                
012510     IF R-S-DIR = 1 AND A-PI-SEC-CODE-1 = 3 GO TO ADD-DIS.                
012520     IF R-W-DIR = 1 AND A-PI-SEC-CODE-1 = 4 GO TO ADD-DIS.                
012530     IF R-N-DIR = 1 AND A-PI-SEC-CODE-1 = 3 GO TO SUB-DIS.                
012540     IF R-E-DIR = 1 AND A-PI-SEC-CODE-1 = 4 GO TO SUB-DIS.                
012550     IF R-S-DIR = 1 AND A-PI-SEC-CODE-1 = 1 GO TO SUB-DIS.                
012560     IF R-W-DIR = 1 AND A-PI-SEC-CODE-1 = 2 GO TO SUB-DIS.                
012570 U5. PERFORM CONT-P, MOVE ZERO TO SW-INTER GO TO 33-UP-IND.               
012580 OLD-RT.                                                                  
012590     IF (R-N-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)           
012600         GO TO ADD-DIS.                                                   
012610     IF (R-E-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)           
012620         GO TO ADD-DIS.                                                   
012630     IF (R-N-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)           
012640         GO TO SUB-DIS.                                                   
012650     IF (R-E-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)           
012660         GO TO SUB-DIS.                                                   
012670     IF (R-S-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)           
012680         GO TO SUB-DIS.                                                   
012690     IF (R-W-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)           
012700         GO TO SUB-DIS.                                                   
012710     IF (R-S-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)           
012720         GO TO ADD-DIS.                                                   
012730     IF (R-W-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)           
012740         GO TO ADD-DIS.                                                   
012750     IF POS-1 = 1 OR POS-1 = 8 OR POS-1 = 9 GO TO CONT-P.                 
012760     GO TO U5.                                                            
012770 ADD-DIS.                                                                 
012772     IF A-SYM-KEY-IS = LAST-X-ST-HOLD GO TO U5.                           
012773     IF END-SW = 1 GO TO U5.                                              
012775     IF FILLER-2-DIG > 50 ADD 1 TO CHART-D.                               
012776         MOVE ZERO TO FILLER-2-DIG.                                       
012780     ADD IS-DISTANCE CHART-DIST GIVING CHART-DIST GO TO IU-1.             
012790 SUB-DIS.                                                                 
012793     IF IS-DISTANCE = 00100, GO TO U5.                                    
012795     IF FILLER-2-DIG < 51 NEXT SENTENCE ELSE ADD 1 TO CHART-D.            
012796     MOVE ZERO TO FILLER-2-DIG.                                           
012797     IF IS-DISTANCE LESS THAN CHART-DIST GO TO U5.                        
012800     SUBTRACT CHART-DIST FROM IS-DISTANCE GIVING CHART-DIST               
012810         GO TO IU-1.                                                      
012820 IU-1.                                                                    
012840     IF CHART-D < 1 OR CHART-D > 125 GO TO U5.                            
012842     IF ROUT-SW = ZERO GO TO 33-CONT-BX.                                  
012843     IF SYM-KEY = LAST-X-ST-HOLD GO TO BOUNDARY-RTN.                      
012844     IF END-SW = 1 GO TO BOUNDARY-RTN.                                    
012845 33-CONT-BX.                                                              
012850     IF P-SW = 1 ADD 1 TO CHART-P (CHART-D) GO TO CONT-P.                 
012860     IF I-SW = 1 ADD 1 TO CHART-I (CHART-D) GO TO CONT-P.                 
012870     IF F-SW = 1 ADD 1 TO CHART-F (CHART-D) GO TO CONT-P.                 
012880 CONT-P.                                                                  
012890     MOVE ZERO TO CHART-LOC, CHART-OLD, ROUT-SW.                          
012900 CONT-P1.                                                                 
012910     IF SW-N-IS = 1 MOVE ZERO TO SW-N-IS GO TO 33-CONT-B.                 
012920     MOVE ZERO TO SW-IS.                                                  
012930     IF A-ACCID-TYPE = 03 MOVE 6 TO C GO TO 33-CONT-A.                    
012940     IF A-ACCID-TYPE < 20 OR A-ACCID-TYPE > 32 MOVE 7 TO C                
012950         GO TO 33-CONT-A.                                                 
012960     IF A-ACCID-TYPE > 22 NEXT SENTENCE ELSE MOVE 1 TO C                  
012970         GO TO 33-CONT-A.                                                 
012980     IF A-ACCID-TYPE > 25 NEXT SENTENCE ELSE MOVE 2 TO C                  
012990         GO TO 33-CONT-A.                                                 
013000     IF A-ACCID-TYPE > 28 NEXT SENTENCE ELSE MOVE 3 TO C                  
013010         GO TO 33-CONT-A.                                                 
013020     IF A-ACCID-TYPE > 29 NEXT SENTENCE ELSE MOVE 5 TO C                  
013030         GO TO 33-CONT-A.                                                 
013040     MOVE 4 TO C.                                                         
013050 33-CONT-A.                                                               
013060     IF P-SW = 1                                                          
013070         ADD 1 TO FLD-IS (C, 1) GO TO 33-UP-IND.                          
013080     IF I-SW = 1                                                          
013090         ADD 1 TO FLD-IS (C, 2)                                           
013100         ADD A-TYPE-A A-TYPE-B A-TYPE-C TO FLD-IS (C, 4)                  
013110         GO TO 33-UP-IND.                                                 
013120     IF F-SW = 1                                                          
013130         ADD 1 TO FLD-IS (C, 3)                                           
013140         ADD A-TYPE-K TO FLD-IS (C, 5)                                    
013150         ADD A-TYPE-A A-TYPE-B A-TYPE-C TO FLD-IS (C, 4).                 
013160     GO TO 33-UP-IND.                                                     
013170 33-CONT-B.                                                               
013180     IF A-ACCID-TYPE = 01 MOVE 4 TO C GO TO 33-CONT-C.                    
013190     IF A-ACCID-TYPE = 03 MOVE 7 TO C GO TO 33-CONT-C.                    
013200     IF A-ACCID-TYPE = 05 MOVE 6 TO C GO TO 33-CONT-C.                    
013210     IF A-ACCID-TYPE = 07 MOVE 5 TO C GO TO 33-CONT-C.                    
013220     IF A-ACCID-TYPE = 29 MOVE 3 TO C GO TO 33-CONT-C.                    
013230     IF A-ACCID-TYPE > 25 AND A-ACCID-TYPE < 29                           
013240         MOVE 1 TO C GO TO 33-CONT-C.                                     
013250     IF A-ACCID-TYPE > 29 AND A-ACCID-TYPE < 33                           
013260         MOVE 2 TO C GO TO 33-CONT-C.                                     
013270     MOVE 8 TO C.                                                         
013280 33-CONT-C.                                                               
013290     IF P-SW = 1                                                          
013300         ADD 1 TO FLD-NON-IS (C, 1)  GO TO 33-UP-IND.                     
013310     IF I-SW = 1                                                          
013320         ADD 1 TO FLD-NON-IS (C, 2)                                       
013330         ADD A-TYPE-A A-TYPE-B A-TYPE-C  TO FLD-NON-IS (C, 4)             
013340         GO TO 33-UP-IND.                                                 
013350     IF F-SW = 1                                                          
013360         ADD 1 TO FLD-NON-IS (C, 3)                                       
013370         ADD A-TYPE-A A-TYPE-B A-TYPE-C  TO FLD-NON-IS (C, 4)             
013380         ADD A-TYPE-K TO FLD-NON-IS (C, 5).                               
013390     GO TO 33-UP-IND.                                                     
013400 33-UP-IND.                                                               
013410     MOVE 1 TO PASS-S.                                                    
013420     IF A-OLDEST > A-LATEST OR A-OLDEST = ZERO MOVE ZERO TO PASS-S        
013430          GO TO 33-UP-IND-X.                                              
013440     MOVE A-OLDEST TO A-SYM-INDEX ADD 1 TO A-OLDEST                       
013450         GO TO RTN-33-AHFILE.                                             
013455 33-UP-IND-X. EXIT.                                                       
013460 RTN-33-AHFILE-A.                                                         
013470     PERFORM 33-OTHER THRU 33-OTHER-X.                                    
013480     MOVE IS-OTHER-HOLD TO VOL-SYM-KEY.                                   
013490     MOVE ZEROES TO VOL-SYM-INDEX.                                        
013495 VOL-RTN-CONT.                                                            
013500     READ VOLFILE INVALID KEY GO TO RTN-33-AHFILE-X.                      
013504     IF VOL-DELETE-CODE = HIGH-VALUE GO TO VOL-RTN-INCREASE.              
013505     IF VOL-D-O-W = '6' OR VOL-D-O-W = '7' GO TO VOL-RTN-INCREASE.KM120970
013506     IF DATE-2YRS = ZERO NEXT SENTENCE ELSE                               
013507         IF VOL-DATE < DATE-2YRS GO TO VOL-RTN-INCREASE.                  
013510     IF EW-DIR = 1 AND (VOL-LOC-1 = 1 OR VOL-LOC-1 = 3)                   
013520         GO TO VOL-RTN-INCREASE.                                          
013530     IF NS-DIR = 1 AND (VOL-LOC-1 = 2 OR VOL-LOC-1 = 4)                   
013540         GO TO VOL-RTN-INCREASE.                                          
013550     IF VOL-REC-CODE = 1 GO TO P1.                                        
013560     COMPUTE 33-V-W ROUNDED = ((VOL-TOTAL-NE-N + VOL-TOTAL-SW-N)          
013570         * VOL-ADJ-FAC).                                                  
013580     IF 33-V-W = ZERO GO TO RTN-33-AHFILE-X  ELSE                         
013590          ADD 33-V-W TO 33-V-HOLD ADD 1 TO V-COUNT.                       
013600          GO TO RTN-33-AHFILE-X.                                          
013610 P1. IF VOL-TOTAL-TOT-N = ZERO GO TO RTN-33-AHFILE-X ELSE                 
013620         ADD VOL-TOTAL-TOT-N TO 33-V-HOLD ADD 1 TO V-COUNT.               
013630 RTN-33-AHFILE-X. EXIT.                                                   
013635 VOL-RTN-INCREASE.                                                        
013636     ADD 1 TO VOL-SYM-INDEX-N, GO TO VOL-RTN-CONT.                        
013640 IS-LOC.                                                                  
013650     IF IS-DIS-A < 1 OR IS-DIS-A > 125 GO TO IS-LOC-X.                    
013660     MOVE 1 TO CHART-ACC-TYPE (IS-DIS-A).                                 
013670     IF R-ST-CODE = SYM-KEY-ST-1                                          
013680         MOVE SYM-KEY-ST-2 TO CHART-IS (IS-DIS-A)                         
013690         MOVE IS-NAME-2 TO CHART-NAME (IS-DIS-A)  ELSE                    
013700     MOVE SYM-KEY-ST-1 TO CHART-IS (IS-DIS-A)                             
013710     MOVE IS-NAME-1 TO CHART-NAME (IS-DIS-A).                             
013720 IS-LOC-X. EXIT.                                                          
013730 UP-IND-A.                                                                
013740     PERFORM F-125 THRU F-125-X VARYING COUNT-125 FROM 1 BY 1             
013750         UNTIL COUNT-125 GREATER THAN 125.                                
013760 UP-IND-A-X. EXIT.                                                        
013770 F-125.                                                                   
013780     IF CHART-ACC-TYPE (COUNT-125) NOT = 1 GO TO F-125A.                  
013790       MOVE '1' TO T23-REC-2-AT-C.                                        
013800       MOVE CHART-IS (COUNT-125) TO T23-REC-2-X-SC.                       
013810       MOVE CHART-NAME (COUNT-125) TO T23-REC-2-X-ST.                     
013820       MOVE ZERO TO C  MOVE  1 TO COUNTER.                                
013830       PERFORM BUILD-ACC-TAB THRU BUILD-ACC-TAB-X UNTIL C = 1.            
013840       PERFORM F-125D THRU F-125D-X.                                      
013845     IF CHART-IS (COUNT-125) = LAST-ST-CODE MOVE 125 TO COUNT-125.        
013850       GO TO F-125-X.                                                     
013860 F-125A.                                                                  
013870     IF CHART-P (COUNT-125) = ZERO AND CHART-I (COUNT-125) = ZERO         
013880         AND CHART-F (COUNT-125) = ZERO GO TO F-125-X.                    
013890     MOVE '2' TO T23-REC-2-AT-C.                                          
013900     MOVE ZERO TO C.                                                      
013910     MOVE 1 TO COUNTER.                                                   
013920     PERFORM BUILD-ACC-TAB THRU BUILD-ACC-TAB-X UNTIL C = 1.              
013930     PERFORM F-125D THRU F-125D-X.                                        
013940 F-125-X. EXIT.                                                           
013950 F-125D.                                                                  
013960     MOVE ZERO TO CHART-DIST. MOVE COUNT-125 TO CHART-D.                  
013970     MOVE CHART-DIST TO DISTANCE. MOVE ZERO TO CHART-D.                   
013980     WRITE REPORT-REC. ADD T-AC TO TOT-ACC.                               
013990     MOVE ZEROES TO REPORT-REC. MOVE 23 TO REPORT-CODE.                   
014000     MOVE 2 TO REC-CODE. MOVE R-10-REQ-NO TO REQ-NO.                      
014010 F-125D-X. EXIT.                                                          
014020 BUILD-ACC-TAB.                                                           
014030     MOVE SPACE TO T23-REC-2-OV40.                                        
014040     MOVE SPACES TO T23-40.  MOVE ZERO TO T-AC.                           
014050     MOVE CHART-P (COUNT-125) TO P-AC.                                    
014060     MOVE CHART-I (COUNT-125) TO I-AC.                                    
014070     MOVE CHART-F (COUNT-125) TO F-AC.                                    
014080     IF F-AC = ZERO GO TO BUILD-ACC-1.                                    
014090 BI. MOVE 'F' TO T23-ACC (COUNTER).                                       
014100     IF COUNTER = 40 MOVE '*' TO T23-REC-2-OV40 MOVE 1 TO C               
014110         GO TO BUILD-ACC-TAB-X.                                           
014120     IF COUNTER = F-AC ADD 1 TO COUNTER GO TO BUILD-ACC-1  ELSE           
014130     ADD 1 TO COUNTER GO TO BI.                                           
014140 BUILD-ACC-1.                                                             
014150     IF I-AC = ZERO GO TO BUILD-ACC-2.                                    
014160     MOVE F-AC TO T-AC ADD I-AC TO T-AC.                                  
014170 BS. MOVE 'I' TO T23-ACC (COUNTER).                                       
014180     IF COUNTER = 40 MOVE '*' TO T23-REC-2-OV40  MOVE 1 TO C              
014190         GO TO BUILD-ACC-TAB-X.                                           
014200     IF COUNTER = T-AC ADD 1 TO COUNTER GO TO BUILD-ACC-2  ELSE           
014210     ADD 1 TO COUNTER GO TO BS.                                           
014220 BUILD-ACC-2.                                                             
014230     IF P-AC = ZERO MOVE 1 TO C GO TO BUILD-ACC-TAB-X.                    
014240     ADD P-AC TO T-AC.                                                    
014250 B3. MOVE 'P' TO T23-ACC (COUNTER).                                       
014260     IF COUNTER = 40 MOVE '*' TO T23-REC-2-OV40 MOVE 1 TO C               
014270         GO TO BUILD-ACC-TAB-X.                                           
014280     IF COUNTER = T-AC MOVE 1 TO C GO TO BUILD-ACC-TAB-X.                 
014290     ADD 1 TO COUNTER GO TO B3.                                           
014300 BUILD-ACC-TAB-X. EXIT.                                                   
014310 ROUTE-DIR.                                                               
014320     MOVE ZERO TO N-DIR E-DIR S-DIR W-DIR.                                
014330     IF R-N-DIR = 1 GO TO NORTH-ROUTE.                                    
014340     IF R-E-DIR = 1 GO TO  EAST-ROUTE.                                    
014350     IF R-S-DIR = 1 GO TO SOUTH-ROUTE.                                    
014360     IF R-W-DIR = 1 GO TO  WEST-ROUTE.                                    
014370     GO TO ROUTE-DIR-X.                                                   
014380 NORTH-ROUTE.                                                             
014390     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =               
014400         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.              
014410     IF IS-NEXT-IS-CODE (2) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014420     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =               
014430         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.              
014440     IF IS-NEXT-IS-CODE (4) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014450     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =               
014460         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.              
014470     IF JUST-LEFT-IS = ZERO MOVE 1 TO N-DIR                               
014490     GO TO ROUTE-DIR-X.                                                   
014500     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =               
014510         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.              
014520     MOVE 1 TO IS-ER                                                      
014530     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY          
014540     GO TO ROUTE-DIR-X.                                                   
014550 EAST-ROUTE.                                                              
014560     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =               
014570         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.              
014580     IF IS-NEXT-IS-CODE (1) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014590     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =               
014600         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.              
014610     IF IS-NEXT-IS-CODE (3) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014620     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =               
014630         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.              
014640     IF JUST-LEFT-IS = ZERO MOVE 1 TO E-DIR                               
014660     GO TO ROUTE-DIR-X.                                                   
014670     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =               
014680         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.              
014690     MOVE 1 TO IS-ER                                                      
014700     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY          
014710     GO TO ROUTE-DIR-X.                                                   
014720 SOUTH-ROUTE.                                                             
014730     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =               
014740         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.              
014750     IF IS-NEXT-IS-CODE (2) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014760     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =               
014770         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.              
014780     IF IS-NEXT-IS-CODE (4) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014790     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =               
014800         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.              
014810     IF JUST-LEFT-IS = ZERO MOVE 1 TO S-DIR                               
014830     GO TO ROUTE-DIR-X.                                                   
014840     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =               
014850         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.              
014860     MOVE 1 TO IS-ER                                                      
014870     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY          
014880     GO TO ROUTE-DIR-X.                                                   
014890 WEST-ROUTE.                                                              
014900     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (4) OR ROUTE-ST-HOLD =               
014910         IS-NEXT-ST-2 (4) MOVE 1 TO W-DIR GO TO ROUTE-DIR-A.              
014920     IF IS-NEXT-IS-CODE (1) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014930     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (1) OR ROUTE-ST-HOLD =               
014940         IS-NEXT-ST-2 (1) MOVE 1 TO N-DIR GO TO ROUTE-DIR-A.              
014950     IF IS-NEXT-IS-CODE (3) = JUST-LEFT-IS NEXT SENTENCE ELSE             
014960     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (3) OR ROUTE-ST-HOLD =               
014970         IS-NEXT-ST-2 (3) MOVE 1 TO S-DIR GO TO ROUTE-DIR-A.              
014980     IF JUST-LEFT-IS = ZERO MOVE 1 TO W-DIR                               
015000     GO TO ROUTE-DIR-X.                                                   
015010     IF ROUTE-ST-HOLD = IS-NEXT-ST-1 (2) OR ROUTE-ST-HOLD =               
015020         IS-NEXT-ST-2 (2) MOVE 1 TO E-DIR GO TO ROUTE-DIR-A.              
015030     MOVE 1 TO IS-ER                                                      
015040     DISPLAY 'CANNOT DETERMINE DIRECTION FROM I/S FILE ' SYM-KEY          
015050     GO TO ROUTE-DIR-X.                                                   
015060 ROUTE-DIR-A.                                                             
015070     DISPLAY N-DIR E-DIR S-DIR W-DIR ' DIR OF NEXT I/S NESW'.             
015080     IF N-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (1)                  
015090     MOVE ZERO TO N-DIR  MOVE 1 TO S-DIR                                  
015100     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -SOUTH'        
015110        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'            
015120         GO TO ROUTE-DIR-X.                                               
015130     IF E-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (2)                  
015140     MOVE ZERO TO E-DIR  MOVE 1 TO W-DIR                                  
015150     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -WEST'         
015160        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'            
015170         GO TO ROUTE-DIR-X.                                               
015180     IF S-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (3)                  
015190     MOVE ZERO TO S-DIR  MOVE 1 TO N-DIR                                  
015200     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -NORTH'        
015210        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'            
015220         GO TO ROUTE-DIR-X.                                               
015230     IF W-DIR = 1 AND JUST-LEFT-IS = IS-NEXT-IS-CODE (4)                  
015240     MOVE ZERO TO W-DIR  MOVE 1 TO E-DIR                                  
015250     DISPLAY 'NEXT I/S MUST BE JOGGED, WILL CONTINUE ROUTE -EAST'         
015260        DISPLAY SYM-KEY ' I/S CODE ' JUST-LEFT-IS ' JUST LEFT'            
015270         GO TO ROUTE-DIR-X.                                               
015280 ROUTE-DIR-X. EXIT.                                                       
015290 RTN-34.                                                                  
015300     MOVE ZEROES TO REPORT-REC.                                           
015310     MOVE REQ-NO-10 TO REQ-NO. MOVE 81 TO REPORT-CODE.                    
015320     MOVE IS-10-REQUESTER TO T81-REQUESTER.                               
015330     MOVE DATE-LIMIT-10 TO T81-SEARCH-DATES.                              
015340     MOVE FR-MO TO FROM-MO   MOVE FR-DA TO FROM-DA.                       
015350     MOVE FR-YR TO FROM-YR   MOVE  T-MO TO   TO-MO.                       
015360     MOVE  T-DA TO   TO-DA   MOVE  T-YR TO   TO-YR.                       
015370     IF IS-10-ST1  GREATER THAN IS-10-ST2  MOVE IS-10-ST1  TO             
015380         VOL-SYM-ST-2 SYM-KEY-ST-2 IS-ST2-H  MOVE IS-10-ST2  TO           
015390         VOL-SYM-ST-1 SYM-KEY-ST-1 IS-ST1-H  ELSE                         
015400     MOVE IS-10   TO VOL-SYM-ST-CODE SYM-KEY IS-ST-H.                     
015410     MOVE SYM-KEY TO T81-IS-CODE.                                         
015420 B2. MOVE ZEROES TO VOL-SYM-INDEX.                                        
015430 RD-VOLFILE.                                                              
015440     READ VOLFILE INVALID KEY                                             
015450     GO TO VOL-ERR-RTN.                                                   
015460     IF VOL-DELETE-CODE = HIGH-VALUE GO TO RTN-34-X.                      
015470     IF PASS-S EQUAL TO 1  GO TO B1.                                      
015480     MOVE VOL-LATEST-N TO A-LATEST.                                       
015490     MOVE VOL-OLDEST-N TO A-OLDEST. PERFORM ISR THRU ISR-X.               
015500 B1.                                                                      
015510     IF FROM-DATE = ZERO GO TO B1A.                                       
015520     IF VOL-DATE LESS THAN FROM-DATE GO TO INCREASE-VOL-INDEX.            
015530     IF VOL-DATE GREATER THAN TO-DATE GO TO INCREASE-VOL-INDEX.           
015540 B1A.                                                                     
015550     ADD 1 TO RECORD-COUNTER.                                             
015560     MOVE VOL-REC-CODE TO T81-REC-CODE.                                   
015570     MOVE VOL-DATE TO T81-DATE.                                           
015580     MOVE VOL-DATE TO CODE-IS-DATE.                                       
015590     MOVE VOL-D-O-W TO T81-DAY-OF-WEEK.                                   
015600     MOVE VOL-LOC TO T81-LOC.                                             
015610     MOVE VOL-REV-CODE TO T81-REVERSE-CODE.                               
015620     MOVE VOL-MED TO T81-MED.                                             
015630     MOVE VOL-RD-WIDTH TO T81-RDWY.                                       
015640     MOVE VOL-NO-LA TO T81-LA.                                            
015650     IF V-NE-VOL (1) NOT NUMERIC MOVE ZEROES TO V-NE-VOL (1).             
015660     IF V-NE-VOL (2) NOT NUMERIC MOVE ZEROES TO V-NE-VOL (2).             
015670     IF V-SW-VOL (1) NOT NUMERIC MOVE ZEROES TO V-SW-VOL (1).             
015680     IF V-SW-VOL (2) NOT NUMERIC MOVE ZEROES TO V-SW-VOL (2).             
015690     IF V-TOT-VOL (1) NOT NUMERIC MOVE ZEROES TO V-TOT-VOL (1).           
015700     IF V-TOT-VOL (2) NOT NUMERIC MOVE ZEROES TO V-TOT-VOL (2).           
015710     IF VOL-TOTAL-NE NOT NUMERIC MOVE ZEROES TO VOL-TOTAL-NE.             
015720     IF VOL-TOTAL-SW NOT NUMERIC MOVE ZEROES TO VOL-TOTAL-SW.             
015730     IF VOL-TOTAL-TOT NOT NUMERIC MOVE ZEROES TO VOL-TOTAL-TOT.           
015740     MOVE VOL-AM-PM-PEAK TO T81-PEAK.                                     
015750     MOVE VOL-TOTALS-HR TO T81-TOT-24-HR.                                 
015760     PERFORM ISR THRU ISR-X.                                              
015770     WRITE REPORT-REC.                                                    
015780 INCREASE-VOL-INDEX. MOVE 1 TO PASS-S.                                    
015790     PERFORM RTN-34.                                                      
015800     IF A-OLDEST GREATER THAN A-LATEST OR A-OLDEST EQUAL TO ZERO          
015810         MOVE ZERO TO PASS-S GO TO RTN-34-X.                              
015820     MOVE A-OLDEST TO VOL-SYM-INDEX ADD 1 TO A-OLDEST                     
015830     READ VOLFILE INVALID KEY                                             
015840         GO TO INCREASE-VOL-INDEX.                                        
015850     IF VOL-DELETE-CODE = HIGH-VALUE GO TO INCREASE-VOL-INDEX.            
015860     GO TO B1.                                                            
015870 RTN-34-X. EXIT.                                                          
015880 ISR.                                                                     
015890     READ ISFILE INVALID KEY MOVE 'STREET NAME NOT ON FILE'               
015900         TO T81-ST-NAME-1 MOVE SPACES TO T81-ST-NAME-2                    
015910     MOVE 1 TO ISR-SW                                                     
015920         GO TO ISR-X.                                                     
015930     MOVE IS-NAME-1 TO T81-ST-NAME-1                                      
015940     MOVE IS-NAME-2 TO T81-ST-NAME-2.                                     
015950 ISR-X. EXIT.                                                             
015960 RTN-34A.                                                                 
015970     IF ISR-SW = 1 MOVE ZERO TO ISR-SW GO TO RTN-34A-X.                   
015980     MOVE SYM-KEY TO IS-OTHER-HOLD.                                       
015990 OTHER-A.                                                                 
016000     READ ISFILE INVALID KEY GO TO RTN-34A-X.                             
016010     IF IS-JOG = ZERO GO TO RTN-34A-X.                                    
016020     IF IS-OTHER-ST = ZERO GO TO RTN-34A-X.                               
016030     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO RTN-34A-X.                      
016040     MOVE IS-OTHER-ST TO IS-10.                                           
016050     ADD 1 TO R-SEQ.                                                      
016060     PERFORM RTN-34 THRU RTN-34-X.                                        
016070     IF IS-JOG = '2' GO TO RTN-34A-X.                                     
016080     GO TO OTHER-A.                                                       
016090 RTN-34A-X. EXIT.                                                         
016100 OTHER-IS-RTN.                                                            
016110     MOVE ZERO TO JOGGED-SWITCH.                                          
016120     MOVE A-SYM-KEY-IS TO IS-OTHER-HOLD.                                  
016130     MOVE A-SYM-KEY-IS TO SYM-KEY.                                        
016140 OTHER-B.                                                                 
016150     READ ISFILE INVALID KEY GO TO OTHER-IS-RTN-X.                        
016160     IF IS-JOG = ZERO GO TO OTHER-IS-RTN-X.                               
016170     IF IS-OTHER-ST = ZERO GO TO OTHER-IS-RTN-X.                          
016180     IF IS-OTHER-ST = IS-OTHER-HOLD GO TO OTHER-IS-RTN-X.                 
016190     MOVE IS-OTHER-ST TO IS-10 SYM-KEY.                                   
016200     ADD 1 TO R-SEQ.                                                      
016210     PERFORM RTN-30 THRU RTN-30-X.                                        
016220     IF IS-JOG = '2' GO TO OTHER-IS-RTN-X.                                
016230     GO TO OTHER-B.                                                       
016240 OTHER-IS-RTN-X. EXIT.                                                    
016250 TYPE-SEARCH-RTN.                                                         
016260     IF A-AT-IS-CODE = 1 MOVE 1 TO SEARCH-SW                              
016270     GO TO TYPE-SEARCH-RTN-A.                                             
016280     IF A-AT-IS-CODE NOT = 2 GO TO TYPE-SEARCH-RTN-X.                     
016290     IF A-ST-REV-CODE = ZERO AND ROUTE-ST-HOLD = A-SYM-ST-1               
016300         MOVE 1 TO ROUT-SW                                                
016310         MOVE 1 TO SEARCH-SW GO TO TYPE-SEARCH-RTN-A.                     
016320     IF A-ST-REV-CODE = 1 AND ROUTE-ST-HOLD = A-SYM-ST-2                  
016330         MOVE 1 TO ROUT-SW                                                
016340         MOVE 1 TO SEARCH-SW GO TO TYPE-SEARCH-RTN-A.                     
016350     IF TYPE-SEARCH = '1' GO TO TYPE-SEARCH-RTN-X.                        
016360     IF A-PI-PRIM-CODE-1 = 9 GO TO SEARCH-OLD-WAY.                        
016370     IF A-DIR-ANAL-CODE = 300 GO TO TYPE-SEARCH-RTN-X.                    
016380     IF A-PI-SEC-CODE-1 < 1 OR A-PI-SEC-CODE-1 > 4                        
016390         GO TO TYPE-SEARCH-RTN-X.                                         
016400     IF A-PI-SEC-FT > 200 GO TO TYPE-SEARCH-RTN-X.                        
016410     PERFORM ST-ACC-ON THRU ST-ACC-ON-X.                                  
016420     IF DIR-ST-SW = ZERO GO TO TYPE-SEARCH-RTN-X.                         
016430     IF DIR-VEH-C = A-PI-SEC-CODE-1 GO TO TYPE-SEARCH-RTN-X.              
016440     GO TO X-ST-APP.                                                      
016450 SEARCH-OLD-WAY.                                                          
016460     MOVE A-PI-PRIM-FT TO OLD-LOC.                                        
016470     IF POS-1 = 3 OR POS-1 = 5 NEXT SENTENCE  ELSE                        
016480         GO TO TYPE-SEARCH-RTN-X.                                         
016490     IF POS-2 = ZERO AND POS-3 < 2 NEXT SENTENCE   ELSE                   
016500         GO TO TYPE-SEARCH-RTN-X.                                         
016510 X-ST-APP.                                                                
016520     IF A-ACCID-TYPE = 26 OR A-ACCID-TYPE = 27 OR A-ACCID-TYPE =          
016530         28 OR A-ACCID-TYPE = 30 OR A-ACCID-TYPE = 31 OR                  
016540         A-ACCID-TYPE = 32  MOVE 1 TO SEARCH-SW GO TO                     
016550         TYPE-SEARCH-RTN-A ELSE GO TO TYPE-SEARCH-RTN-X.                  
016560 TYPE-SEARCH-RTN-A.                                                       
016570     IF A-PI-PRIM-CODE-1 NOT = 9 MOVE A-PI-SEC-FT TO CHART-DIST           
016580     GO TO TYPE-SEARCH-RTN-X.                                             
016590     MOVE A-PI-PRIM-FT TO OLD-LOC.                                        
016600     MOVE POS-2 TO CH-OLD-1.                                              
016610     MOVE POS-3 TO CH-OLD-2.                                              
016620     ADD 1 TO CH-OLD.                                                     
016630     MOVE CHART-OLD TO CHART-DIST.                                        
016640 TYPE-SEARCH-RTN-X. EXIT.                                                 
016650 NO-D-RTN.                                                                
016654     IF T-D-HOLD = SPACES MOVE ZERO TO DATE-2YRS ELSE                     
016656         MOVE C-M1 TO MOS-2 MOVE C-D1 TO DAS-2                            
016657         MOVE C-Y1 TO YRS-2 SUBTRACT 2 FROM YRS-2.                        
016660     IF T-D-HOLD = SPACES MOVE 2190 TO TND GO TO NO-D-RTN-X.              
016670     IF C-M1 = 01 MOVE C-D1 TO TO-D-TOT GO TO NO-D-1.                     
016680     SUBTRACT 1 FROM C-M1 GIVING MO-CT.                                   
016690     MOVE MO-ENTRY (MO-CT) TO TO-D-TOT.                                   
016700     ADD C-D1 TO TO-D-TOT.                                                
016710 NO-D-1.                                                                  
016720     IF C-M = 01 MOVE C-D TO FR-D-TOT GO TO NO-D-2.                       
016730     SUBTRACT 1 FROM C-M GIVING MO-CT.                                    
016740     MOVE MO-ENTRY (MO-CT) TO FR-D-TOT.                                   
016750     ADD C-D TO FR-D-TOT.                                                 
016760 NO-D-2.                                                                  
016770     COMPUTE YR-D-TOT = (365 * (C-Y1 - C-Y)).                             
016780     IF FR-D-TOT > TO-D-TOT SUBTRACT 365 FROM YR-D-TOT                    
016790     COMPUTE TND = (((365 + TO-D-TOT) - FR-D-TOT) + YR-D-TOT)             
016800     GO TO NO-D-RTN-X.                                                    
016810     COMPUTE TND = ((TO-D-TOT - FR-D-TOT) + YR-D-TOT).                    
016820 NO-D-RTN-X.  EXIT.                                                       
016830 UNP-A.                                                                   
016840     MOVE 'T - 4                           DEPARTMENT OF TRAFFIC'         
016850         TO PRINT-LINE                                                    
016860     WRITE PRINT-REC AFTER ADVANCING 0 LINES                              
016870     MOVE '                                 UNPROCESSED REQUESTS'         
016880         TO PRINT-LINE                                                    
016890     WRITE PRINT-REC AFTER ADVANCING 1 LINES                              
016900     MOVE T10-S-LOC TO PRINT-LINE                                         
016910     WRITE PRINT-REC AFTER ADVANCING 3 LINES.                             
016920     IF IS-10-ST1 < IS-10-ST2 MOVE IS-10 TO SYM-KEY GO TO IS-RD-A.        
016930     MOVE IS-10-ST1 TO SYM-KEY-ST-2.                                      
016940     MOVE IS-10-ST2 TO SYM-KEY-ST-1.                                      
016950 IS-RD-A.                                                                 
016960     READ ISFILE INVALID KEY                                              
016970         MOVE 'NO STREET NAMES AVAILABLE' TO PRT-IS-1                     
016980         MOVE SPACES TO PRT-IS-2 GO TO P-MSG-A.                           
016990     IF IS-10-ST1 < IS-10-ST2 MOVE IS-NAME-1 TO PRT-IS-1                  
017000     MOVE IS-NAME-2 TO PRT-IS-2  ELSE                                     
017010     MOVE IS-NAME-2 TO PRT-IS-1  MOVE IS-NAME-1 TO PRT-IS-2.              
017020 P-MSG-A.                                                                 
017030     WRITE PRINT-REC FROM PRT-HR AFTER ADVANCING 1 LINES.                 
017040 UNP-AX.  EXIT.                                                           
017050 VOL-ERR-RTN.                                                             
017060     MOVE 1 TO UNPROC-SW.                                                 
017080     PERFORM UNP-A THRU UNP-AX                                            
017090     MOVE 'THERE ARE NO VOLUME COUNTS FOR REQUESTED LOCATION'             
017100         TO PRINT-LINE                                                    
017110     WRITE PRINT-REC AFTER ADVANCING 2 LINES                              
017120     GO TO RTN-34-X.                                                      
017130 ST-ACC-ON.                                                               
017140     MOVE ZERO TO DIR-VEH-C, DIR-ST-SW, SUPL-RD-ON, SUPL-INVALID.         
017150     MOVE A-P1-VEH-DIR TO DIR-VEH-C.                                      
017160     PERFORM CK-DIR-V THRU CK-DIR-V-X.                                    
017170     IF DIR-ST-SW = 1, GO TO ST-ACC-ON-X.                                 
017180     MOVE A-P2-VEH-DIR TO DIR-VEH-C.                                      
017190     PERFORM CK-DIR-V THRU CK-DIR-V-X.                                    
017200     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.                                  
017210     IF SUPL-REC-CODE < 3 GO TO ST-ACC-ON-X.                              
017220     ADD 1 TO A-SYM-REC-CODE.                                             
017230     READ ACCIDENT-FILE INVALID KEY MOVE 1 TO SUPL-INVALID                
017240         A-SYM-REC-CODE  GO TO ST-ACC-ON-X.                               
017250     MOVE 1 TO SUPL-RD-ON A-SYM-REC-CODE.                                 
017260     PERFORM UNPACK-S THRU UNPACK-SX.                                     
017270     MOVE S-VEH-DIR (1) TO DIR-VEH-C.                                     
017280     PERFORM CK-DIR-V THRU CK-DIR-V-X.                                    
017290     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.                                  
017300     MOVE S-VEH-DIR (2) TO DIR-VEH-C.                                     
017310     PERFORM CK-DIR-V THRU CK-DIR-V-X.                                    
017320     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.                                  
017330     MOVE S-VEH-DIR (3) TO DIR-VEH-C.                                     
017340     PERFORM CK-DIR-V THRU CK-DIR-V-X.                                    
017350     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.                                  
017360     MOVE S-VEH-DIR (4) TO DIR-VEH-C.                                     
017370     PERFORM CK-DIR-V THRU CK-DIR-V-X.                                    
017380     IF DIR-ST-SW = 1 GO TO ST-ACC-ON-X.                                  
017390      MOVE ZERO TO DIR-VEH-C.                                             
017400 ST-ACC-ON-X. EXIT.                                                       
017410 CK-DIR-V.                                                                
017420     IF (A-PI-SEC-CODE-1 = 1 OR A-PI-SEC-CODE-1 = 3) AND          KM120970
017422        (DIR-VEH-C = 1 OR DIR-VEH-C = 3)                          KM120970
017430     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.                                
017440     IF (A-PI-SEC-CODE-1 = 2 OR A-PI-SEC-CODE-1 = 4) AND          KM120970
017442        (DIR-VEH-C = 2 OR DIR-VEH-C = 4)                          KM120970
017450     MOVE 1 TO DIR-ST-SW GO TO CK-DIR-V-X.                                
017460 CK-DIR-V-X.    EXIT.                                                     
019485 BOUNDARY-RTN.                                                            
019486     IF A-PI-PRIM-CODE-1 = 9 GO TO BOUN-OLD.                              
019487     IF A-PI-SEC-FT < 51 GO TO 33-CONT-BX.                                
019488     IF N-DIR = 1 AND A-PI-SEC-CODE-1 = 1 GO TO U5.                       
019489     IF E-DIR = 1 AND A-PI-SEC-CODE-1 = 2 GO TO U5.                       
019850     IF S-DIR = 1 AND A-PI-SEC-CODE-1 = 3 GO TO U5.                       
019851     IF W-DIR = 1 AND A-PI-SEC-CODE-1 = 4 GO TO U5.                       
019852     GO TO 33-CONT-BX.                                                    
019853 BOUN-OLD.                                                                
019854     IF (N-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)     KM120970
019856        GO TO U5.                                                 KM120970
019858     IF (E-DIR = 1) AND (POS-1 = 2 OR POS-1 = 5 OR POS-1 = 6)     KM120970
019860        GO TO U5.                                                 KM120970
019862     IF (S-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)     KM120970
019864        GO TO U5.                                                 KM120970
019866     IF (W-DIR = 1) AND (POS-1 = 3 OR POS-1 = 4 OR POS-1 = 7)     KM120970
019868        GO TO U5.                                                 KM120970
019870     GO TO 33-CONT-BX.                                            KM120970
/*                                                                              
// LBLTYP NSD(05)                                                               
// EXEC LNKEDT                                                                  
/*                                                                              
/&                                                                              
  g 3R