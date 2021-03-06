000001 IDENTIFICATION DIVISION.                                         NEWRANCB
000002 ENVIRONMENT DIVISION.                                            NEWRANCB
000003 INPUT-OUTPUT SECTION.                                            NEWRANCB
000004 FILE-CONTROL.                                                    NEWRANCB
000005        SELECT DATAX ASSIGN TO DSK                                NEWRANCB
000006        FILE LIMIT IS 1 THRU 5                                    NEWRANCB
000007        ACCESS MODE IS RANDOM                                     NEWRANCB
000008        RECORDING MODE IS ASCII                                   NEWRANCB
000009        ACTUAL KEY IS PARTX.                                      NEWRANCB
000010 DATA DIVISION.                                                   NEWRANCB
000011 FILE SECTION.                                                    NEWRANCB
000012 FD      DATAX VALUE OF ID IS 'RANDOMDAT'                         NEWRANCB
000013        BLOCK CONTAINS 1 RECORD.                                  NEWRANCB
000014 01      RECORDX.                                                 NEWRANCB
000015        05 FILLER PIC X.                                          NEWRANCB
000016        05 BODE PIC X(2).                                         NEWRANCB
000017        05 PART PIC X(4).                                         NEWRANCB
000018 WORKING-STORAGE SECTION.                                         NEWRANCB
000019 77      PARTX PIC S99 COMP.                                      NEWRANCB
000020 77      MODE-ANSWER PIC X.                                       NEWRANCB
000021 77      EMPTY-FIELD PIC X VALUE SPACES.                          NEWRANCB
000022 PROCEDURE DIVISION.                                              NEWRANCB
000023 SETUP.                                                           NEWRANCB
000024        OPEN I-O DATAX.                                           NEWRANCB
000025 MAIN-1.                                                          NEWRANCB
000026        DISPLAY EMPTY-FIELD.                                      NEWRANCB
000027        DISPLAY 'R,W,OR Q : ' WITH NO ADVANCING.                  NEWRANCB
000028        ACCEPT MODE-ANSWER.                                       NEWRANCB
000029        IF MODE-ANSWER='Q' GO TO EOJ.                             NEWRANCB
000030        IF MODE-ANSWER ='R' PERFORM READ-RECORD THRU READ-FIN     NEWRANCB
000031        ELSE PERFORM WRITE-RECORD.                                NEWRANCB
000032        GO TO MAIN-1.                                             NEWRANCB
000033 WRITE-RECORD.                                                    NEWRANCB
000034        DISPLAY 'AT: ' WITH NO ADVANCING,                         NEWRANCB
000035        ACCEPT PARTX.                                             NEWRANCB
000036        DISPLAY 'F1 (XX): 'WITH NO ADVANCING.                     NEWRANCB
000037        ACCEPT BODE.                                              NEWRANCB
000038        DISPLAY 'F2 (XXXX) : ' WITH NO ADVANCING.                 NEWRANCB
000039        ACCEPT PART.                                              NEWRANCB
000040        WRITE RECORDX INVALID KEY PERFORM WRITE-ERR.              NEWRANCB
000041 WRITE-ERR.                                                       NEWRANCB
000042        DISPLAY 'CANNOT WRITE AT 'PARTX.                          NEWRANCB
000043 READ-RECORD.                                                     NEWRANCB
000044        DISPLAY 'AT: 'WITH NO ADVANCING.                          NEWRANCB
000045        ACCEPT PARTX.                                             NEWRANCB
000046        READ DATAX INVALID KEY GO TO READ-ERR.                    NEWRANCB
000047        DISPLAY RECORDX.                                          NEWRANCB
000048        GO TO READ-FIN.                                           NEWRANCB
000049 READ-ERR.                                                        NEWRANCB
000050        DISPLAY 'CANNOT READ AT 'PARTX.                           NEWRANCB
000051 READ-FIN.                                                        NEWRANCB
000052        EXIT.                                                     NEWRANCB
000053 EOJ.                                                             NEWRANCB
000054        DISPLAY 'FINISHED'.                                       NEWRANCB
000055        CLOSE DATAX.                                              NEWRANCB
000056        STOP RUN.                                                 NEWRANCB
   