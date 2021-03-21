000001 IDENTIFICATION DIVISION.                                         CREAT3.C
000002 PROGRAM-ID. CREATE.                                              CREAT3.C
000003 AUTHOR. ERIC FORSTER.                                            CREAT3.C
000004 INSTALLATION. TYMSHARE,INC.                                      CREAT3.C
000005 DATE-WRITTEN. 5-1-73.                                            CREAT3.C
000006 SECURITY. NONE.                                                  CREAT3.C
000007 REMARKS. PROGRAM CREATES A DATA SET FROM THE TERMINAL.           CREAT3.C
000008 ENVIRONMENT DIVISION.                                            CREAT3.C
000009 CONFIGURATION SECTION.                                           CREAT3.C
000010 SOURCE-COMPUTER. PDP-10.                                         CREAT3.C
000011 OBJECT-COMPUTER. PDP-10.                                         CREAT3.C
000012 INPUT-OUTPUT SECTION.                                            CREAT3.C
000013 FILE-CONTROL.                                                    CREAT3.C
000014        SELECT CARD-IMAGES ASSIGN TO DSK                          CREAT3.C
000015        RECORDING MODE IS ASCII.                                  CREAT3.C
000016 DATA DIVISION.                                                   CREAT3.C
000017 FILE SECTION.                                                    CREAT3.C
000018 FD CARD-IMAGES                                                   CREAT3.C
000019        BLOCK CONTAINS 1 RECORDS                                  CREAT3.C
000020        RECORD CONTAINS 72 CHARACTERS                             CREAT3.C
000021        LABEL RECORDS ARE STANDARD                                CREAT3.C
000022        VALUE OF IDENTIFICATION IS "CARDINDAT"                    CREAT3.C
000023        DATA RECORDS ARE OUTPUT-RECORD.                           CREAT3.C
000024 01      OUTPUT-RECORD PIC X(72).                                 CREAT3.C
000025 WORKING-STORAGE SECTION.                                         CREAT3.C
000026 01      REC-COUNT PICTURE 9999 VALUE ZERO.                       CREAT3.C
000027 01      CUR-REC PICTURE 999  VALUE ZERO.                         CREAT3.C
000028 01      WORK-AREA.                                               CREAT3.C
000029        02 CUST-NAME PICTURE X(17).                               CREAT3.C
000030        02 CITY PICTURE X(11).                                    CREAT3.C
000031        02 STATE PICTURE XX.                                      CREAT3.C
000032        02 TOTAL-SALES PICTURE 9(4)V99.                           CREAT3.C
000033 PROCEDURE DIVISION.                                              CREAT3.C
000034 USER-FIRST-PARAGRAPH.                                            CREAT3.C
000035        OPEN OUTPUT CARD-IMAGES.                                  CREAT3.C
000036 READ-TELETYPE.                                                   CREAT3.C
000037        DISPLAY " ".                                              CREAT3.C
000038        DISPLAY CUR-REC,":".                                      CREAT3.C
000039        DISPLAY "CUSTOMER NAME: " WITH NO ADVANCING.              CREAT3.C
000040        ACCEPT CUST-NAME IN WORK-AREA.                            CREAT3.C
000041        IF CUST-NAME  = "Q" OR "QUIT" GO TO DONE.                 CREAT3.C
000042        DISPLAY "CITY: " WITH NO ADVANCING.                       CREAT3.C
000043        ACCEPT CITY IN WORK-AREA.                                 CREAT3.C
000044        DISPLAY "STATE: " WITH NO ADVANCING.                      CREAT3.C
000045        ACCEPT STATE IN WORK-AREA.                                CREAT3.C
000046        DISPLAY "TOTAL SALES: " WITH NO ADVANCING.                CREAT3.C
000047        ACCEPT TOTAL-SALES IN WORK-AREA.                          CREAT3.C
000048        WRITE OUTPUT-RECORD FROM WORK-AREA.                       CREAT3.C
000049        ADD 1 TO REC-COUNT CUR-REC.                               CREAT3.C
000050        GO TO READ-TELETYPE.                                      CREAT3.C
000051 DONE.                                                            CREAT3.C
000052        DISPLAY " ".                                              CREAT3.C
000053        DISPLAY REC-COUNT, " RECORDS CREATED. GOOD LUCK!".        CREAT3.C
000054        CLOSE CARD-IMAGES.                                        CREAT3.C
000055        STOP RUN.                                                 CREAT3.C
