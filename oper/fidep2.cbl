000001 IDENTIFICATION DIVISION.                                         FIDEP   
000002 PROGRAM-ID.    FIDEP.                                            FIDEP   
000003 AUTHOR.    J.M. ZIKRATCH.                                        FIDEP   
000004 INSTALLATION.    PRESS-ENTERPRISE CO.                            FIDEP   
000005 DATE-WRITTEN.    JANUARY 1973.                                   FIDEP   
000006 REMARKS.          FIXTURE INVENTORY SYSTEM                       FIDEP   
000007                                                                  FIDEP   
000008     FIXTURE INVENTORY DEPRECIATION CALCULATION                   FIDEP   
000009                                                                  FIDEP   
000010     THIS PROGRAM CALCULATES THE CURRENT DEPRECIATION             FIDEP   
000011     AMOUNT FOR EACH ITEM THAT HAS NOT YET OUTLIVED ITS           FIDEP   
000012     YEARS OF USEFUL LIFE.  ANY ITEM ON FILE WHICH DOES           FIDEP   
000013     NOT SHOW ANY DEPRECIATION RESERVE (E.G., AN ITEM             FIDEP   
000014     WHICH HAS BEEN ADDED TO THE FILE SINCE THE                   FIDEP   
000015     PROGRAM WAS LAST RUN) WILL HAVE ITS ENTIRE RESERVE           FIDEP   
000016     TO-DATE CALCULATED.                                          FIDEP   
000017                                                                  FIDEP   
000018 ENVIRONMENT DIVISION.                                            FIDEP   
000019 CONFIGURATION SECTION.                                           FIDEP   
000020 SOURCE-COMPUTER.    IBM-1130.                                    FIDEP   
000021 OBJECT-COMPUTER.    PDP-10.                                      FIDEP   
000022 SPECIAL-NAMES.    CONSOLE IS TYPEWRITER                          FIDEP   
000023                  CONSOLE IS KEYBOARD.                            FIDEP   
000024                                                                  FIDEP   
000025 INPUT-OUTPUT SECTION.                                            FIDEP   
000026 FILE-CONTROL.                                                    FIDEP   
000027     SELECT FIXED-ASSET-MASTER-FILE                               FIDEP   
000028*        ASSIGN TO DF-1-5000-X                                    FIDEP   
000029         ASSIGN TO DSK                                            FIDEP   
000030         ACCESS IS SEQUENTIAL.                                    FIDEP   
000031     SELECT NEW-FIXED-ASSET-MASTER                                FIDEP   
000032*        ASSIGN TO DF-2-5000-X                                    FIDEP   
000033         ASSIGN TO DSK                                            FIDEP   
000034         ACCESS IS SEQUENTIAL.                                    FIDEP   
000035 DATA DIVISION.                                                   FIDEP   
000036 FILE SECTION.                                                    FIDEP   
000037 FD  FIXED-ASSET-MASTER-FILE                                      FIDEP   
000038         VALUE OF ID IS 'FILTSTDAT'                               FIDEP   
000039         LABEL RECORDS ARE STANDARD                               FIDEP   
000040         RECORD CONTAINS 80 CHARACTERS                            FIDEP   
000041         DATA RECORD IS ASSET-MASTER-FILE-RECORD.                 FIDEP   
000042 01  ASSET-MASTER-FILE-RECORD.                                    FIDEP   
000043     05  ASSET-ITEM-NO           PICTURE 9(4) COMPUTATIONAL.      FIDEP   
000044     05  DESCRIPTION             PICTURE X(30).                   FIDEP   
000045     05  SERIAL-NO-OR-DIMENSIONS PICTURE X(9).                    FIDEP   
000046     05  LOCATION-NO             PICTURE 9(4) COMPUTATIONAL.      FIDEP   
000047     05  FILLER                  PICTURE X(8).                    FIDEP   
000048     05  GENERAL-LEDGER-ACCT-NO  PICTURE XXX.                     FIDEP   
000049     05  METHOD-OF-DEPRECIATION  PICTURE X.                       FIDEP   
000050     05  YEARS-OF-LIFE           PICTURE 9(4) COMPUTATIONAL.      FIDEP   
000051     05  DATE-ACQUIRED.                                           FIDEP   
000052         10  YEAR-ACQUIRED       PICTURE 99.                      FIDEP   
000053         10  MONTH-ACQUIRED      PICTURE 99.                      FIDEP   
000054         10  DAY-ACQUIRED        PICTURE 99.                      FIDEP   
000055     05  DATE-SOLD               PICTURE 9(6) COMPUTATIONAL.      FIDEP   
000056     05  USED-ITEM-INDICATOR     PICTURE X.                       FIDEP   
000057         88  ITEM-IS-USED        VALUE "U".                       FIDEP   
000058     05  PURCHASE-COST           PICTURE 9(9) COMPUTATIONAL.      FIDEP   
000059     05  REPLACEMENT-COST        PICTURE 9(9) COMPUTATIONAL.      FIDEP   
000060     05  SALVAGE-VALUE           PICTURE 9(9) COMPUTATIONAL.      FIDEP   
000061     05  AMOUNT-SOLD-FOR         PICTURE 9(9) COMPUTATIONAL.      FIDEP   
000062     05  DEPRECIATION-RESERVE    PICTURE 9(9) COMPUTATIONAL.      FIDEP   
000063     05  CURRENT-DEPRECIATION    PICTURE 9(9) COMPUTATIONAL.      FIDEP   
000064                                                                  FIDEP   
000065 FD  NEW-FIXED-ASSET-MASTER                                       FIDEP   
000066         VALUE OF ID IS 'NFIXAS   '                               FIDEP   
000067         RECORD CONTAINS 80 CHARACTERS                            FIDEP   
000068         LABEL RECORDS ARE STANDARD                               FIDEP   
000069         DATA RECORD IS NEW-ASSET-MASTER-RECORD.                  FIDEP   
000070 01  NEW-ASSET-MASTER-RECORD      PICTURE X(80).                  FIDEP   
000071                                                                  FIDEP   
000072 WORKING-STORAGE SECTION.                                         FIDEP   
000073 77  MONTHS-OF-LIFE PIC 999 COMP.                                 FIDEP   
000074 77  MONTHS-OF-LIFE-LEFT PIC 999 COMP.                            FIDEP   
000075 77  DEPRECIATION-RATE PIC 9V9999 COMP.                           FIDEP   
000076 77  DECLINING-FACTOR PIC 99V99 COMP.                             FIDEP   
000077 77  MONTHLY-DEPRECIATION PIC 9(9)V999 COMP.                      FIDEP   
000078 77   RUN-DATE PICTURE X(8).                                      FIDEP   
000079 77  EFFECTIVE-COST PIC 9(9) COMP.                                FIDEP   
000080 77  SUM-OF-LIFE-DIGITS PIC 9(6) COMP.                            FIDEP   
000081 77  NO-MONTHS-DEPRECIATED PIC 9(4) COMP.                         FIDEP   
000082 77  NO-OF-MONTHS PIC 9(4) COMP.                                  FIDEP   
000083 77  TEMP-CURRENT-DEPRECIATION PIC 9(9)V999 COMP.                 FIDEP   
000084 77   WORK-YEAR-ACQUIRED PICTURE 99 COMP.                         FIDEP   
000085 77   WORK-MONTH-ACQUIRED PICTURE 99 COMP.                        FIDEP   
000086 01   EFFECTIVE-DATE.                                             FIDEP   
000087     05  EFFECTIVE-MONTH PICTURE 99.                              FIDEP   
000088     05  FILLER PICTURE X.                                        FIDEP   
000089     05  EFFECTIVE-DAY PICTURE 99.                                FIDEP   
000090     05  FILLER PICTURE X.                                        FIDEP   
000091     05  EFFECTIVE-YEAR PICTURE 99.                               FIDEP   
000092                                                                  FIDEP   
000093 01   WORK-DATE.                                                  FIDEP   
000094     05  WORK-YEAR PIC 99 COMP.                                   FIDEP   
000095     05  WORK-MONTH PIC 99 COMP.                                  FIDEP   
000096     05  WORK-DAY PIC 99 COMP.                                    FIDEP   
000097 01  PREVIOUS-WORK-DATE.                                          FIDEP   
000098     05  PREVIOUS-WORK-MONTH PICTURE 99.                          FIDEP   
000099     05  PREVIOUS-WORK-DAY PICTURE 99.                            FIDEP   
000100     05  PREVIOUS-WORK-YEAR PICTURE 99.                           FIDEP   
000101 01  FIXED-ASSET-MASTER-HEADER.                                   FIDEP   
000102     05  F-A-PARAMETERS OCCURS 12 TIMES                           FIDEP   
000103         INDEXED BY F-A-P-INDEX.                                  FIDEP   
000104         10  F-A-RUN-DATE PICTURE X(6).                           FIDEP   
000105         10  F-A-EFFECTIVE-DATE.                                  FIDEP   
000106             15  F-A-EFFECTIVE-MONTH PIC 99.                      FIDEP   
000107             15  F-A-EFFECTIVE-DAY PIC 99.                        FIDEP   
000108             15  F-A-EFFECTIVE-YEAR PIC 99.                       FIDEP   
000109     05  FILLER PICTURE X(176).                                   FIDEP   
000110                                                                  FIDEP   
000111 PROCEDURE DIVISION.                                              FIDEP   
000112 OPEN-FILES.                                                      FIDEP   
000113     OPEN INPUT FIXED-ASSET-MASTER-FILE                           FIDEP   
000114         OUTPUT NEW-FIXED-ASSET-MASTER.                           FIDEP   
000115 GET-RUN-DATE.                                                    FIDEP   
000116     DISPLAY "ENTER TODAY" QUOTE "S DATE AS MM-DD-YY"             FIDEP   
000117         UPON TYPEWRITER.                                         FIDEP   
000118     ACCEPT RUN-DATE FROM KEYBOARD.                               FIDEP   
000119 GET-EFFECTIVE-DATE.                                              FIDEP   
000120     DISPLAY "ENTER EFFECTIVE DATE AS MM-DD-YY"                   FIDEP   
000121         UPON TYPEWRITER.                                         FIDEP   
000122     ACCEPT EFFECTIVE-DATE FROM KEYBOARD.                         FIDEP   
000123     IF EFFECTIVE-YEAR NOT NUMERIC                                FIDEP   
000124         OR EFFECTIVE-YEAR NOT POSITIVE                           FIDEP   
000125         OR EFFECTIVE-YEAR LESS THAN 60                           FIDEP   
000126         GO TO BAD-DATE.                                          FIDEP   
000127     IF EFFECTIVE-MONTH NOT NUMERIC                               FIDEP   
000128         OR EFFECTIVE-MONTH NOT POSITIVE                          FIDEP   
000129         OR EFFECTIVE-MONTH GREATER THAN 13                       FIDEP   
000130         GO TO BAD-DATE.                                          FIDEP   
000131     MOVE EFFECTIVE-YEAR TO WORK-YEAR.                            FIDEP   
000132     MOVE EFFECTIVE-MONTH TO WORK-MONTH.                          FIDEP   
000133     MOVE EFFECTIVE-DAY TO WORK-DAY.                              FIDEP   
000134     GO TO OPEN-FILES.                                            FIDEP   
000135 BAD-DATE.                                                        FIDEP   
000136     DISPLAY "INVALID DATE ENTERED"                               FIDEP   
000137         UPON TYPEWRITER.                                         FIDEP   
000138     GO TO GET-EFFECTIVE-DATE.                                    FIDEP   
000139 READ-PARM-INFO.                                                  FIDEP   
000140     READ FIXED-ASSET-MASTER-FILE INTO FIXED-ASSET-MASTER-HEADER  FIDEP   
000141                AT END DISPLAY "OLD MASTER EMPTY"                 FIDEP   
000142                        UPON TYPEWRITER                           FIDEP   
000143                GO TO E-O-J.                                      FIDEP   
000144     MOVE F-A-EFFECTIVE-DATE (1) TO PREVIOUS-WORK-DATE.           FIDEP   
000145     PERFORM MOVE-PARM-FIELDS THRU MOVE-P-F-X VARYING F-A-P-INDEX FIDEP   
000146         FROM 12 BY -1 UNTIL F-A-P-INDEX LESS THAN 2.             FIDEP   
000147     MOVE EFFECTIVE-YEAR TO F-A-EFFECTIVE-YEAR (1).               FIDEP   
000148     MOVE EFFECTIVE-MONTH TO F-A-EFFECTIVE-MONTH (1).             FIDEP   
000149     MOVE EFFECTIVE-DAY TO F-A-EFFECTIVE-DAY (1).                 FIDEP   
000150     WRITE NEW-ASSET-MASTER-RECORD FROM FIXED-ASSET-MASTER-HEADER FIDEP   
000151         INVALID KEY DISPLAY "BAD WRITE TO NEW MASTER"            FIDEP   
000152         UPON TYPEWRITER                                          FIDEP   
000153         GO TO E-O-J.                                             FIDEP   
000154 READ-A-RECORD.                                                   FIDEP   
000155     READ FIXED-ASSET-MASTER-FILE                                 FIDEP   
000156         AT END GO TO E-O-J.                                      FIDEP   
000157     IF DATE-SOLD NOT = ZERO                                      FIDEP   
000158         GO TO WRITE-BACK-RECORD.                                 FIDEP   
000159     IF YEAR-ACQUIRED IS GREATER THAN WORK-YEAR                   FIDEP   
000160         GO TO WRITE-BACK-RECORD.                                 FIDEP   
000161     MOVE YEAR-ACQUIRED TO WORK-YEAR-ACQUIRED.                    FIDEP   
000162     MOVE MONTH-ACQUIRED TO WORK-MONTH-ACQUIRED.                  FIDEP   
000163     COMPUTE MONTHS-OF-LIFE = YEARS-OF-LIFE * 12.                 FIDEP   
000164     IF DEPRECIATION-RESERVE = ZERO                               FIDEP   
000165         MOVE YEAR-ACQUIRED TO WORK-YEAR-ACQUIRED                 FIDEP   
000166         MOVE MONTH-ACQUIRED TO WORK-MONTH-ACQUIRED               FIDEP   
000167         COMPUTE MONTHS-OF-LIFE-LEFT = MONTHS-OF-LIFE             FIDEP   
000168         GO TO COMPUTE-MONTHS-OF-DEPRECIATION.                    FIDEP   
000169     MOVE PREVIOUS-WORK-YEAR TO WORK-YEAR-ACQUIRED.               FIDEP   
000170     COMPUTE WORK-MONTH-ACQUIRED = PREVIOUS-WORK-MONTH + 1.       FIDEP   
000171     IF YEAR-ACQUIRED = PREVIOUS-WORK-YEAR                        FIDEP   
000172         COMPUTE NO-OF-MONTHS-DEPRECIATED =                       FIDEP   
000173             PREVIOUS-WORK-MONTH - MONTH-ACQUIRED + 1             FIDEP   
000174     ELSE                                                         FIDEP   
000175         COMPUTE NO-OF-MONTHS-DEPRECIATED =                       FIDEP   
000176             (13 - MONTH-ACQUIRED)                                FIDEP   
000177             + (PREVIOUS-WORK-MONTH)                              FIDEP   
000178             + (PREVIOUS-WORK-YEAR - YEAR-ACQUIRED - 1) * 12.     FIDEP   
000179     COMPUTE MONTHS-OF-LIFE-LEFT = (YEARS-OF-LIFE * 12)           FIDEP   
000180         - NO-MONTHS-DEPRECIATED.                                 FIDEP   
000181     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE                          FIDEP   
000182         GO TO WRITE-BACK-RECORD.                                 FIDEP   
000183 COMPUTE-MONTHS-OF-DEPRECIATION.                                  FIDEP   
000184     IF WORK-YEAR-ACQUIRED = WORK-YEAR                            FIDEP   
000185         COMPUTE NO-OF-MONTHS = WORK-MONTH                        FIDEP   
000186         - WORK-MONTH-ACQUIRED + 1                                FIDEP   
000187         ELSE                                                     FIDEP   
000188        COMPUTE NO-OF-MONTHS = (13 - WORK-MONTH-ACQUIRED)         FIDEP   
000189             + (WORK-MONTH) + ( (WORK-YEAR -                      FIDEP   
000190             WORK-YEAR-ACQUIRED - 1) * 12).                       FIDEP   
000191                                                                  FIDEP   
000192     MOVE ZERO TO CURRENT-DEPRECIATION TEMP-CURRENT-DEPRECIATION. FIDEP   
000193     IF METHOD-OF-DEPRECIATION = "S"                              FIDEP   
000194         GO TO STRAIGHT-LINE.                                     FIDEP   
000195     IF METHOD-OF-DEPRECIATION = "D"                              FIDEP   
000196         MOVE 2 TO DECLINING-FACTOR                               FIDEP   
000197         GO TO DECLINING-BALANCE.                                 FIDEP   
000198     IF METHOD-OF-DEPRECIATION = "H"                              FIDEP   
000199         MOVE 1.5 TO DECLINING-FACTOR                             FIDEP   
000200         GO TO DECLINING-BALANCE.                                 FIDEP   
000201     IF METHOD-OF-DEPRECIATION = "Y"                              FIDEP   
000202         GO TO SUM-OF-YEARS-DIGITS.                               FIDEP   
000203     DISPLAY "INVALID METH. OF DEPR. = "                          FIDEP   
000204         METHOD-OF-DEPRECIATION                                   FIDEP   
000205         " FOR ASSET " ASSET-ITEM-NO                              FIDEP   
000206         UPON TYPEWRITER.                                         FIDEP   
000207     GO TO WRITE-BACK-RECORD.                                     FIDEP   
000208                                                                  FIDEP   
000209 STRAIGHT-LINE.                                                   FIDEP   
000210     COMPUTE DEPRECIATION-RATE ROUNDED = 1 / MONTHS-OF-LIFE.      FIDEP   
000211     COMPUTE MONTHLY-DEPRECIATION ROUNDED =                       FIDEP   
000212         (PURCHASE-COST - SALVAGE-VALUE)                          FIDEP   
000213         * DEPRECIATION-RATE.                                     FIDEP   
000214     PERFORM ACCUMULATE-ST-LINE-DEPR THRU                         FIDEP   
000215         ACCUMULATE-S-L-D-X NO-OF-MONTHS TIMES.                   FIDEP   
000216     GO TO ADD-DEPR-TO-RESERVE.                                   FIDEP   
000217                                                                  FIDEP   
000218 DECLINING-BALANCE.                                               FIDEP   
000219     COMPUTE DEPRECIATION-RATE ROUNDED = DECLINING-FACTOR         FIDEP   
000220         / MONTHS-OF-LIFE.                                        FIDEP   
000221     PERFORM ACCUMULATE-DECL-DEPR THRU ACCUMULATE-D-D-X           FIDEP   
000222         NO-OF-MONTHS TIMES.                                      FIDEP   
000223     GO TO WRITE-BACK-RECORD.                                     FIDEP   
000224                                                                  FIDEP   
00SUM-OF-YEARS-DIGITS.                                             FIDEP   
000226     COMPUTE SUM-OF-LIFE-DIGITS = MONTHS-OF-LIFE                  FIDEP   
000227         * (MONTHS-OF-LIFE + 1) / 2.                              FIDEP   
000228     COMPUTE EFFECTIVE-COST = PURCHASE-COST - SALVAGE-VALUE.      FIDEP   
000229     PERFORM ACCUMULATE-S-Y-D-DEPR THRU ACCUMULATE-S-Y-D-D-X      FIDEP   
000230         NO-OF-MONTHS TIMES.                                      FIDEP   
000231     GO TO ADD-DEPR-TO-RESERVE.                                   FIDEP   
000232                                                                  FIDEP   
000233 ADD-DEPR-TO-RESERVE.                                             FIDEP   
000234     ADD TEMP-CURRENT-DEPRECIATION TO CURRENT-DEPRECIATION        FIDEP   
000235         ROUNDED.                                                 FIDEP   
000236     ADD CURRENT-DEPRECIATION TO DEPRECIATION-RESERVE.            FIDEP   
000237                                                                  FIDEP   
000238 WRITE-BACK-RECORD.                                               FIDEP   
000239     WRITE NEW-ASSET-MASTER-RECORD FROM ASSET-MASTER-FILE-RECORD  FIDEP   
000240         INVALID KEY DISPLAY "NEW MASTER OVERFLOW"                FIDEP   
000241         UPON TYPEWRITER.                                         FIDEP   
000242     GO TO READ-A-RECORD.                                         FIDEP   
000243                                                                  FIDEP   
000244 E-O-J.                                                           FIDEP   
000245     CLOSE FIXED-ASSET-MASTER-FILE                                FIDEP   
000246         NEW-FIXED-ASSET-MASTER.                                  FIDEP   
000247     DISPLAY "EOJ--FIDEP" UPON TYPEWRITER.                        FIDEP   
000248     STOP RUN.                                                    FIDEP   
000249                                                                  FIDEP   
000250 PERFORMED-ROUTINES SECTION.                                      FIDEP   
000251 ACCUMULATE-ST-LINE-DEPR.                                         FIDEP   
000252     ADD MONTHLY-DEPRECIATION TO TEMP-CURRENT-DEPRECIATION.       FIDEP   
000253     SUBTRACT 1 FROM MONTHS-OF-LIFE-LEFT.                         FIDEP   
000254     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE                          FIDEP   
000255         GO TO ADD-DEPR-TO-RESERVE.                               FIDEP   
000256 ACCUMULATE-S-L-D-X.    EXIT.                                     FIDEP   
000257                                                                  FIDEP   
000258 ACCUMULATE-DECL-DEPR.                                            FIDEP   
000259     COMPUTE MONTHLY-DEPRECIATION ROUNDED = DEPRECIATION-RATE     FIDEP   
000260         * (PURCHASE-COST - SALVAGE-VALUE                         FIDEP   
000261         - DEPRECIATION-RESERVE).                                 FIDEP   
000262     ADD MONTHLY-DEPRECIATION TO CURRENT-DEPRECIATION ROUNDED.    FIDEP   
000263     ADD MONTHLY-DEPRECIATION TO DEPRECIATION-RESERVE ROUNDED.    FIDEP   
000264     SUBTRACT 1 FROM MONTHS-OF-LIFE-LEFT.                         FIDEP   
000265     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE                          FIDEP   
000266         GO TO WRITE-BACK-RECORD.                                 FIDEP   
000267 ACCUMULATE-D-D-X.    EXIT.                                       FIDEP   
000268                                                                  FIDEP   
000269 MOVE-PARM-FIELDS.                                                FIDEP   
000270     MOVE F-A-PARAMETERS (F-A-P-INDEX - 1) TO                     FIDEP   
000271         F-A-PARAMETERS (F-A-P-INDEX).                            FIDEP   
000272 MOVE-P-F-X.    EXIT.                                             FIDEP   
000273                                                                  FIDEP   
000274 ACCUMULATE-S-Y-D-DEPR.                                           FIDEP   
000275     COMPUTE TEMP-CURRENT-DEPRECIATION ROUNDED =                  FIDEP   
000276         TEMP-CURRENT-DEPRECIATION + (EFFECTIVE-COST * MONTHS-OF-LFIDEP   
000277-    IFE-LEFT)                                                    FIDEP   
000278         / SUM-OF-LIFE-DIGITS.                                    FIDEP   
000279     SUBTRACT 1 FROM MONTHS-OF-LIFE-LEFT.                         FIDEP   
000280     IF MONTHS-OF-LIFE-LEFT NOT POSITIVE                          FIDEP   
000281         GO TO ADD-DEPR-TO-RESERVE.                               FIDEP   
000282 ACCUMULATE-S-Y-D-D-X.    EXIT.                                   FIDEP   
 ~)$ j