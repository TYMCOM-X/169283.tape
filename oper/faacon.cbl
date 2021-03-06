000001 IDENTIFICATION DIVISION.                                         FAACON  
000002 PROGRAM-ID. FAADEM.                                              FAACON  
000003 AUTHOR. ERIC FORSTER.                                            FAACON  
000004 INSTALLATION. TYMSHARE, INC.                                     FAACON  
000005 DATE-WRITTEN. 4-25-73.                                           FAACON  
000006 SECURITY. NONE.                                                  FAACON  
000007 REMARKS. THIS PROGRAM DEMONSTRATES COBAID'S CAPABILITIES AS A
   FAACON  
000008 ROGRAM BUILDER.                                                  FAACON  
000009 ENVIRONMENT DIVISION.                                            FAACON  
000010 CONFIGURATION SECTION.                                           FAACON  
000011 SOURCE-COMPUTER. PDP-10.                                         FAACON  
000012 OBJECT-COMPUTER. PDP-10.                                         FAACON  
000013 INPUT-OUTPUT SECTION.                                            FAACON  
000014 FILE-CONTROL.                                                    FAACON  
000015        SELECT CARDIN ASSIGN TO DSK                               FAACON  
000016        RECORDING MODE IS ASCII.                                  FAACON  
000017        SELECT PRINTFILE ASSIGN TO DSK                            FAACON  
000018        RECORDING MODE IS ASCII.                                  FAACON  
000019 DATA DIVISION.                                                   FAACON  
000020 FILE SECTION.                                                    FAACON  
000021 FD CARDIN                                                        FAACON  
000022        BLOCK CONTAINS 1 RECORDS                                  FAACON  
000023        RECORD CONTAINS 80 CHARACTERS                             FAACON  
000024        LABEL RECORDS ARE STANDARD                                FAACON  
000025        VALUE OF IDENTIFICATION IS "CARDINDAT".                   FAACON  
000026        DATA RECORDS ARE CARD-IN.                                 FAACON  
000027 01      CARD-IN PIC X(80).                                       FAACON  
000028 FD PRINTFILE                                                     FAACON  
000029        BLOCK CONTAINS 10 RECORDS                                 FAACON  
000030        RECORD CONTAINS 72 CHARACTERS                             FAACON  
000031        LABEL RECORDS ARE STANDARD                                FAACON  
000032        VALUE OF IDENTIFICATION IS "PRINTRDAT".                   FAACON  
000033        DATA RECORDS ARE PRINT-LINE.                              FAACON  
000034 01      PRINT-LINE PIC X(72).                                    FAACON  
000035 WORKING-STORAGE SECTION.                                         FAACON  
000036 77      COUNT-IN PIC 9(3) VALUE 0.                               FAACON  
000037 77      COUNT-OUT PIC 9(3) VALUE 0.                              FAACON  
000038 01      WS-IN.                                                   FAACON  
000039        02 WS-CODE PIC 99.                                        FAACON  
000040        02 CUST-NAME PIC X(20).                                   FAACON  
000041        02 ADDRESS5.                                              FAACON  
000042                03 STREET PIC X(20).                              FAACON  
000043                03 CITY   PIC X(10).                              FAACON  
000044                03 STATE  PIC X(03).                              FAACON  
000045                03 ZIP    PIC X(5).                               FAACON  
000046        02 CREDIT-INFO PIC X(20).                                 FAACON  
000047 01      WS-OUT.                                                  FAACON  
000048        02 WS-CODE     PIC 99.                                    FAACON  
000049        02 FILLER      PIC X(5) VALUE SPACES.                     FAACON  
000050        02 CUST-NAME   PIC X(20).                                 FAACON  
000051        02 FILLER      PIC X(5) VALUE SPACES.                     FAACON  
000052        02 CREDIT-INFO PIC X(20).                                 FAACON  
000053        02 FILLER      PIC X(20) VALUE SPACES.                    FAACON  
000054 PROCEDURE DIVISION.                                              FAACON  
000055 USER-FIRST-PARAGRAPH.                                            FAACON  
000056        OPEN INPUT CARDIN, OUTPUT PRINTFILE.                      FAACON  
000057 READ-CARDS.                                                      FAACON  
000058        READ CARDIN INTO WS-IN AT AND GO TO EOJ.                  FAACON  
000059        ADD 1 TO COUNT-IN.                                        FAACON  
000060        IF WS-CODE IN WS-IN = 99 GO TO READ-CARDS.                FAACON  
000061 WRITE-OUT.                                                       FAACON  
000062        MOVE CORRESPONDING WS-IN TO WS-OUT.                       FAACON  
000063        IF CREDIT-INFO = SPACES MOVE 44 TO                        FAACON  
000064          WS-CODE IN WS-OUT                                       FAACON  
000065          MOVE "TO BE VERIFIED" TO CREDIT-INFO IN WS-OUT          FAACON  
000066        ELSE MOVE 66 TO WS-CODE IN WS-OUT.                        FAACON  
000067        WRITE PRINT-LINE FROM WS-OUT.                             FAACON  
000068        ADD 1 TO COUNT-OUT.                                       FAACON  
000069        GO TO READ-CARDS.                                         FAACON  
000070 EOJ.                                                             FAACON  
000071        IF COUNT-OUT NOT EQUAL COUNT-IN                           FAACON  
000072          DISPLAY "INPUT COUNTER: " COUNT-IN                      FAACON  
000073          DISPLAY "OUTPUT COUNTER: " COUNT-OUT.                   FAACON  
000074        DISPLAY "E N D   O F   J O B"                             FAACON  
000075        CLOSE CARDIN, PRINTFILE.                                  FAACON  
000076        STOP RUN.                                                 FAACON  
  