000001 IDENTIFICATION DIVISION.                                         WDECON  
000002 PROGRAM-ID. WDETST.                                              WDECON  
000003 AUTHOR. ERIC FORSTER.                                            WDECON  
000004 INSTALLATION. TYMSHARE.                                          WDECON  
000005 DATE-WRITTEN. 4-27-73E.                                          WDECON  
000006 SECURITY. NONE.                                                  WDECON  
000007 REMARKS.    TEST.                                                WDECON  
000008 ENVIRONMENT DIVISION.                                            WDECON  
000009 CONFIGURATION SECTION.                                           WDECON  
000010 SOURCE-COMPUTER. PDP-10.                                         WDECON  
000011 OBJECT-COMPUTER. PDP-10.                                         WDECON  
000012 INPUT-OUTPUT SECTION.                                            WDECON  
000013 FILE-CONTROL.                                                    WDECON  
000014        SELECT CARDIN ASSIGN TO DSK                               WDECON  
000015        RECORDING MODE IS ASCII.                                  WDECON  
000016        SELECT CRDOUT ASSIGN TO DSK                               WDECON  
000017        RECORDING MODE IS ASCII.                                  WDECON  
000018 DATA DIVISION.                                                   WDECON  
000019 FILE SECTION.                                                    WDECON  
000020 WORKING-STORAGE SECTION.                                         WDECON  
000021 77      COUNTER-IN PIC 999 VALUE ZERO.                           WDECON  
000022 77      CTR-OUT PIC 999 VALUE ZERO.                              WDECON  
000023 PROCEDURE DIVISION.                                              WDECON  
000024 USER-FIRST-PARAGRAPH.                                            WDECON  
000025        OPEN INPUT CARD-IN, OUTPUT CRD-OUT.                       WDECON  
000026 READ-CARDS.                                                      WDECON  
000027        READ CARDIN AT END GO TO EOJ.                             WDECON  
000028        ADD 1 TO COUNTER-IN.                                      WDECON  
000029 WRITE-OUT.                                                       WDECON  
000030        MOVE CARD-IN TO CRD-OUT.                                  WDECON  
000031        WRITE CRD-OUT.                                            WDECON  
000032        ADD 1 TO CTR-OUT.                                         WDECON  
000033        GO TO READ-CARDS.                                         WDECON  
000034 EOJ.                                                             WDECON  
000035        CLOSE CARDIN, CRDOUT.                                     WDECON  
000036        STOP RUN.                                                 WDECON  
   