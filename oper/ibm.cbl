000010 IDENTIFICATION DIVISION.                                         IBMTEST1
000020 PROGRAM-ID. HOUSEPROG4.                                          IBMTEST1
000030 AUTHOR. JOHN T. DOE.                                             IBMTEST1
000040 DATE-WRITTEN. MAY 12, 1972.                                      IBMTEST1
000050 DATE-COMPILED. OCTOBER 31, 1972.                                 IBMTEST1
000060 REMARKS. THIS PROGRAM PROVIDES A DETAIL LIST                     IBMTEST1
000070      AND ANALYSIS OF HOUSES AVAILABLE IN THE AREA.               IBMTEST1
000080 ENVIRONMENT DIVISION.                                            IBMTEST1
000090 CONFIGURATION SECTION.                                           IBMTEST1
000100 SOURCE-COMPUTER. IBM-360.                                        IBMTEST1
000110 OBJECT-COMPUTER. IBM-360.                                        IBMTEST1
000120 INPUT-OUTPUT SECTION.                                            IBMTEST1
000130 FILE-CONTROL.                                                    IBMTEST1
000140     SELECT HSEFILE ASSIGN TO UT-S-HSEIN.                         IBMTEST1
000150     SELECT SRTFILE ASSIGN TO UT-S-HSESRT.                        IBMTEST1
000160     SELECT HSEREPORT ASSIGN TO UT-S-HSEOUT.                      IBMTEST1
000170 DATA DIVISION.                                                   IBMTEST1
000180 FILE SECTION.                                                    IBMTEST1
000190 FD      HSEFILE                                                  IBMTEST1
000200     RECORD CONTAINS 24 CHARACTERS                                IBMTEST1
000210     BLOCK CONTAINS 83 RECORDS                                    IBMTEST1
000220     LABEL RECORDS ARE STANDARD                                   IBMTEST1
000230     RECORDING MODE IS F                                          IBMTEST1
000240     DATA RECORD IS HSE-REC.                                      IBMTEST1
000250 01      HSE-REC PIC X(24)    SYNC.                               IBMTEST1
000260 SD      SRTFILE                                                  IBMTEST1
000270     RECORD CONTAINS 24 CHARACTERS                                IBMTEST1
000280     RECORDING MODE IS F                                          IBMTEST1
000290     DATA RECORD IS SRT-REC.                                      IBMTEST1
000300 01      SRT-REC    SYNC.                                         IBMTEST1
000310     05      FILLER  PIC X(15).                                   IBMTEST1
000320     05      SRT-LOC PIC X(3).                                    IBMTEST1
000330     05      SRT-PRICE       PIC 9(6).                            IBMTEST1
000340 FD      HSEREPORT                                                IBMTEST1
000350     RECORD CONTAINS 52 CHARACTERS                                IBMTEST1
000360     BLOCK CONTAINS 38 RECORDS                                    IBMTEST1
000370     LABEL RECORDS ARE STANDARD                                   IBMTEST1
000380     RECORDING MODE IS F                                          IBMTEST1
000390     DATA RECORD IS RPT-REC.                                      IBMTEST1
000400 01      RPT-REC PIC X(52)   SYNC.                                IBMTEST1
000410 WORKING-STORAGE SECTION.                                         IBMTEST1
000420 77      I1      PIC XX.                                          IBMTEST1
000430 77      CNTR    PIC S999  VALUE 1 COMP-3.                        IBMTEST1
000440 77      HLDR    PIC S9(9) COMP-3.                                IBMTEST1
000450 77      SUM-PRICE       PIC S9(9) VALUE ZEROES COMP-3.           IBMTEST1
000460 01      IP-AREA   SYNC.                                          IBMTEST1
000470     05 IP-BDRMS     PIC XX.                                      IBMTEST1
000480     05 IP-BTHS      PIC XX.                                      IBMTEST1
000490     05 IP-GAR       PIC X.                                       IBMTEST1
000500     05 IP-TYPE      PIC X(3).                                    IBMTEST1
000510     05 IP-EXT       PIC X(3).                                    IBMTEST1
000520     05 IP-SQFT      PIC 9(4).                                    IBMTEST1
000530     05 IP-SQFT-RE REDEFINES IP-SQFT PIC X(4).                    IBMTEST1
000540     05 IP-LOC       PIC X(3).                                    IBMTEST1
000550     05 IP-PRICE     PIC 9(6).                                    IBMTEST1
000560     05 IP-PRICE-RE REDEFINES IP-PRICE PIC X(6).                  IBMTEST1
000570 01      RPT-DATA   SYNC.                                         IBMTEST1
000580     05 FILLER       PIC XX  VALUE SPACES.                        IBMTEST1
000590     05 RD-BDRMS     PIC XX.                                      IBMTEST1
000600     05 FILLER       PIC XXX VALUE SPACES.                        IBMTEST1
000610     05 RD-BTHS      PIC XX.                                      IBMTEST1
000620     05 FILLER       PIC XXX VALUE SPACES.                        IBMTEST1
000630     05 RD-GAR       PIC X.                                       IBMTEST1
000640     05 FILLER       PIC XXX VALUE SPACES.                        IBMTEST1
000650     05 RD-TYPE      PIC XXX.                                     IBMTEST1
000660     05 FILLER       PIC X(7) VALUE SPACES.                       IBMTEST1
000670     05 RD-EXT       PIC XXX.                                     IBMTEST1
000680     05 FILLER       PIC X(4) VALUE SPACES.                       IBMTEST1
000690     05 RD-SQFT      PIC 9(4).                                    IBMTEST1
000700     05 FILLER       PIC XX  VALUE SPACES.                        IBMTEST1
000710     05 RD-LOC       PIC XXX.                                     IBMTEST1
000720     05 FILLER       PIC XX  VALUE SPACES.                        IBMTEST1
000730     05 RD-PRICE     PIC $$$$,999.                                IBMTEST1
000740 01      RPT-HLD-AREA   SYNC.                                     IBMTEST1
000750     05 RPT-HLD OCCURS 100 TIMES INDEXED BY                       IBMTEST1
000760             IDX     PIC X(52).                                   IBMTEST1
000770 01      RPT-HDNG   SYNC.                                         IBMTEST1
000780     05 FILLER       PIC X(17) VALUE SPACES.                      IBMTEST1
000790     05 FILLER       PIC X(16) VALUE 'HOUSING ANALYSIS'.          IBMTEST1
000800     05 FILLER       PIC X(17) VALUE SPACES.                      IBMTEST1
000810 01      RPT-HDR1   SYNC.                                         IBMTEST1
000820     05 FILLER       PIC X(52)   VALUE                            IBMTEST1
000830     '   HOUSE DESCRIPTION   EXTERIOR SQ.FT. LOC    PRICE '.      IBMTEST1
000840 01      RPT-HDR2   SYNC.                                         IBMTEST1
000850     05 FILLER       PIC X(50) VALUE                              IBMTEST1
000860     'BDRS BTHS GAR INC/RES'.                                     IBMTEST1
000870 01      RPT-END    SYNC.                                         IBMTEST1
000880     05 FILLER       PIC X(18)  VALUE SPACES.                     IBMTEST1
000890     05 FILLER       PIC X(13)  VALUE 'END OF REPORT'.            IBMTEST1
000900     05 FILLER       PIC X(19)  VALUE SPACES.                     IBMTEST1
000910 PROCEDURE DIVISION.                                              IBMTEST1
000920     OPEN OUTPUT HSEREPORT.                                       IBMTEST1
000930     MOVE 0 TO IDX.                                               IBMTEST1
000940 START.                                                           IBMTEST1
000950     DISPLAY '--- INDICATE SEQUENCE OF HOUSING REPORT DESIRED'    IBMTEST1
000960        UPON CONSOLE.                                             IBMTEST1
000970     DISPLAY '     - LOCATION (LC) OR PRICE (PR) AS MAJOR '       IBMTEST1
000980-    'SEQUENCE OF REPORT' UPON CONSOLE.                           IBMTEST1
000990     ACCEPT I1.                                                   IBMTEST1
001000     IF I1 = 'LC' NEXT SENTENCE                                   IBMTEST1
001010       OTHERWISE      GO TO S1.                                   IBMTEST1
001020     SORT SRTFILE ON ASCENDING KEY SRT-LOC, SRT-PRICE             IBMTEST1
001030       USING HSEFILE                                              IBMTEST1
001040       OUTPUT PROCEDURE MAIN-PGM THRU MP-EXIT.                    IBMTEST1
001050     GO TO PUTOUT-RPT.                                            IBMTEST1
001060 S1.     IF I1 NOT EQUAL 'PR' GO TO ERROR-HALT.                   IBMTEST1
001070     SORT SRTFILE ON ASCENDING KEY SRT-PRICE, SRT-LOC             IBMTEST1
001080       USING HSEFILE                                              IBMTEST1
001090       OUTPUT PROCEDURE MAIN-PGM THRU MP-EXIT.                    IBMTEST1
001100     GO TO PUTOUT-RPT.                                            IBMTEST1
001110* THIS IS THE OUTPUT PROCEDURE SECTION.                           IBMTEST1
001120 MAIN-PGM.                                                        IBMTEST1
001130     RETURN SRTFILE INTO IP-AREA AT END GO TO MP-EXIT.            IBMTEST1
001140     EXAMINE IP-SQFT-RE REPLACING ALL SPACES BY ZEROES.           IBMTEST1
001150     EXAMINE IP-PRICE-RE REPLACING ALL SPACES BY ZEROS.           IBMTEST1
001160     PERFORM MVE-RTN THRU M-R-EXIT.                               IBMTEST1
001170     GO TO MAIN-PGM.                                              IBMTEST1
001180 MP-EXIT.        EXIT.                                            IBMTEST1
001190 MVE-RTN.                                                         IBMTEST1
001200     MOVE IP-BDRMS   TO RD-BDRMS.                                 IBMTEST1
001210     MOVE IP-BTHS    TO RD-BTHS.                                  IBMTEST1
001220     MOVE IP-GAR     TO RD-GAR.                                   IBMTEST1
001230     MOVE IP-TYPE    TO RD-TYPE.                                  IBMTEST1
001240     MOVE IP-EXT     TO RD-EXT.                                   IBMTEST1
001250     MOVE IP-SQFT    TO RD-SQFT.                                  IBMTEST1
001260     MOVE IP-LOC     TO RD-LOC.                                   IBMTEST1
001270     MOVE IP-PRICE   TO RD-PRICE.                                 IBMTEST1
001280     ADD 1 TO IDX.                                                IBMTEST1
001290     MOVE RPT-DATA TO RPT-HLD (IDX).                              IBMTEST1
001300     ADD IP-PRICE TO SUM-PRICE.                                   IBMTEST1
001310 M-R-EXIT.       EXIT.                                            IBMTEST1
001320 PUTOUT-RPT.                                                      IBMTEST1
001330     WRITE RPT-REC FROM RPT-HDNG BEFORE POSITIONING 2 LINES.      IBMTEST1
001340     WRITE RPT-REC FROM RPT-HDR1.                                 IBMTEST1
001350     WRITE RPT-REC FROM RPT-HDR2 BEFORE                           IBMTEST1
001360       POSITIONING 2 LINES.                                       IBMTEST1
001370 PR1.                                                             IBMTEST1
001380     IF CNTR GREATER THAN IDX GO TO END-PAR.                      IBMTEST1
001390     WRITE RPT-REC FROM RPT-HLD (CNTR).                           IBMTEST1
001400     ADD 1 TO CNTR.                                               IBMTEST1
001410     GO TO PR1.                                                   IBMTEST1
001420 ERROR-HALT.                                                      IBMTEST1
001430     DISPLAY 'HALT 300. WRONG INPUT FROM CONSOLE.'                IBMTEST1
001440       UPON CONSOLE.                                              IBMTEST1
001450     GO TO START.                                                 IBMTEST1
001460 END-PAR.                                                         IBMTEST1
001470     WRITE RPT-REC FROM RPT-END AFTER POSITIONING 3 LINES.        IBMTEST1
001480     COMPUTE HLDR = SUM-PRICE / IDX.                              IBMTEST1
001490     DISPLAY '*** NOTE :AVG PRICE IN THIS ANALYSIS IS $ ',        IBMTEST1
001500       HLDR UPON CONSOLE.                                         IBMTEST1
001510     CLOSE HSEREPORT.                                             IBMTEST1
001520     GOBACK.                                                      IBMTEST1
 