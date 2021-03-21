000010 IDENTIFICATION  DIVISION.                                        IH342     
000020 PROGRAM-ID.   IH340  REVISION A.                                 IH342     
000030 AUTHOR.       MSDO.                                              IH342     
000040 DATE-WRITTEN.  APRIL 1971.                                       IH342     
000050 DATE-COMPILED.  09/15/72.                                        IH342     
000060 REMARKS. PAYROLL COMPUTATION PROGRAM                             IH342     
000070     THE PAYROLL IS PREPARED IN BADGE NUMBER SEQUENCE WITHIN      IH342     
000080     ACTIVITY AND GRADED AND UNGRADED, USING THE PAYROLL MASTER   IH342     
000090     RECORD AS THE BASIS. PAYROLL IS COMPUTED ON AN EXCEPTION     IH342     
000100     BASIS. THE EXCEPTIONS ARE PROCESSED FROM THE TIME CARDS. IN  IH342     
000110     ADDITION TO PAYROLL EXCEPTION ADJUSTMENTS, LEAVE WITHOUT     IH342     
000120     PAY IS PROCESSED FROM THE LEAVE PROGRAM.                     IH342     
000130     THE SEQUENCE FOR PAYROLL PROCESSING IS                       IH342     
000140     ACTIVITY MAJOR POS 001                                       IH342     
000150     GRADED OR UNGRADED INT POS 002                               IH342     
000160     CONTROL GROUP INT POS 003-004                                IH342     
000170     BADGE MINOR POS 005-010                                      IH342     
000180     THE MASTER PERSONNEL RECORD IS UPDATED BI-WEEKLY TO          IH342     
000190     BRING EARNING AND DEDUCTION TOTALS UP TO DATE.               IH342     
000200     THE SMOOTH ROLL IS PREPARED FOR PERSONEL SERVICES            IH342     
000210     SHOWING EACH INDIVIDUAL PAY RECORD FOR THE PAY PERIOD. IT    IH342     
000220     INCLUDES PAGE CONTROL-GROUP ACTIVITY AND GRAND TOTALS        IH342     
000230     THE PAYROLL WORK TAPE IS CREATED FOR USE IN PREPARING        IH342     
000240     INDIVIDUAL PAYROLL CHECKS, EARNING STATEMENTS, AND           IH342     
000250     WITH-HOLDING STATEMENTS.                                     IH342     
000260     CONTROL GROUP TOTALS AND ERROR NOTIFICATION                  IH342     
000270     MESSAGE ARE PREPARED                                         IH342     
000280     THE PAYROLL EXCEPTION LISTING BY ACTIVITY IS PREPARED        IH342     
000290     FOR ALL EXCEPTIONS THAT CANNOT BE AUDITED FROM THE SMOOTH    IH342     
000300     ROLL. THIS INCLUDES INDIVIDUAL EXCEPTION CODES WITH EXCEP-   IH342     
000310     TION DESCRIPTIONS.                                           IH342     
000320     PAYROLL PROGRAM ROUTINE AND PAGE NUMBER CROSS REFERENCE.     IH342     
000330     ROUTINE PAGE NO.                                             IH342     
000340     HOUSEKEEPING 100                                             IH342     
000350     READ PERSONNEL MASTER FILE 108                               IH342     
000360     READ PAYROLL DETAIL FILE 116                                 IH342     
000370     MATCH PERSONNEL MASTER VS PAYROLL DETAIL 120                 IH342     
000380     DETAIL PAYROLL CODES 122                                     IH342     
000390     WRITE ERROR, NOTIFICATION MESSAGES 196                       IH342     
000400     COMPUTE OTHER THAN NORMAL HOURS AND PAY 202                  IH342     
000410     COMPUTE PAYROLL REGULAR PAY 236                              IH342     
000420     COMPUTE OVERTIME 252                                         IH342     
000430     TEST FOR ANNUM MAXIMUM PAY 260                               IH342     
000440     TEST FOR MONEY ADJUSTMENTS 264                               IH342     
000450     COMPUTE GROSS MONEY 276                                      IH342     
000460     COMPUTE FICA DEDUCTION 278                                   IH342     
000470     COMPUTE CSRA DEDUCTION 286                                   IH342     
000480     COMPUTE FEDERAL TAX DEDUCTION 290                            IH342     
000490     HEALTH INSURANCE DEDUCTION 298                               IH342     
000500     LIFE INSURANCE DEDUCTION 306                                 IH342     
000510     BOND OR FREEDOM SHARE DEDUCTION 316                          IH342     
000520     GOVERNMENT INDEBTED DEDUCTION 324                            IH342     
000530     COMPUTE STATE TAX DEDUCTION 328                              IH342     
000540     UNION DUES DEDUCTION 332                                     IH342     
000550     CHARITABLE CONTRIBUTION DEDUCTION 336                        IH342     
000560     SAVINGS ALLOTMENT DEDUCTION 338                              IH342     
000570     PREPARE SMOOTH ROLL HEADINGS 340                             IH342     
000580     PREPARE SMOOTH ROLL DETAIL LINES 346                         IH342     
000590     WRITE SMOOTH ROLL FILE 364                                   IH342     
000600     PREPARE SMOOTH ROLL PAGE AND ACTIVITY TOTAL LINES 368        IH342     
000610     PREPARE PAYROLL YTD ACTIVITY BALANCE TOTALS 374              IH342     
000620     PREPARE PAYROLL CONTROL GROUP TOTALS 378                     IH342     
000630     PREPARE PAYROLL EARNINGS WORK TAPE 382                       IH342     
000640     CROSSFOOT CURRENT MONEY 388                                  IH342     
000650     WRITE LABOR RECONCILIATION FILE 392                          IH342     
000660     PREPARE PAYROLL EXCEPTION LISTING 394                        IH342     
000670     POST PAYROLL YTD TOTALS FOR BALANCE 402                      IH342     
000680     WRITE UPDATED PERSONNEL MASTER FILE 406                      IH342     
000690     END OF FILES 406                                             IH342     
000700     END OF JOB 410                                               IH342     
000710     PARAM CARD.                                                  IH342     
000720     CC 1 - 4 ZERO                                                IH342     
000730     CC 5 COMPOSIT CHECK SWITCH                                   IH342     
000740     0 = COMPOSIT CHECKS , 1 = BYPASS CHECKS                      IH342     
000750     CC 6 - 10 ZEROS                                              IH342     
000760     CC 11 - 16 BEG PP DATE MMDDYY                                IH342     
000770     CC 17 - 22 END PP DATE MMDDYY                                IH342     
000780     CC 23 - 24 ROLL NR 01 THRU 27 EXCEPTED                       IH342     
000790     CC 25 - 48 BIPASS ACTIVITY CODES                             IH342     
000800     10 PLACES. RUN EXCEPTION CODES.                              IH342     
000810     CC 49 - 56 BEGINING CHECK NR                                 IH342     
000820     CC 57 - 64 ENDING CHECK NR BLOCK 1 *                         IH342     
000830     CC 65 - 72 BEGINING CHECK NR BLOCK 2 *                       IH342     
000840     CC 73 - 80 ENDING CHECK NR BLOCK 2 *                         IH342     
000850     * NOTE IF ONLY ONE BLOCK OF CHECKS, ZEROS IN THESE COLUMNS.  IH342     
000860     TAPE ID NAME REC SIZE BLK FAC DEVICE ASSGN                   IH342     
000870     IH110U01 PERS-MSTR-IN-FILE 0610 001 01D1                     IH342     
000880     IH335UT1 PAYROLL-DETAIL-FILE 0080 001 01D2                   IH342     
000890     IH110U01 PERS-MSTR-OUT-FILE 0610 001 01D3                    IH342     
000900     IH340U01 PAYROLL-WORK-FILE 0400 001 01D4                     IH342     
000910     IH340UP1 SMOOTH-ROLL-FILE 0160 001 01D5                      IH342     
000920     IH340UP2 CONTROL-ERROR-FILE 0120 001 01D6                    IH342     
000930     IH340UP3 PAY-EXCEPT-LIST-FILE 0120 001 01D7                  IH342     
000940     INTERNAL PROGRAM SWITCHES.                                   IH342     
000950     SW1 ON = PAYROLL DETAIL ERROR.                               IH342     
000960     SW2 ON = PERSONNEL MASTER INPUT END OF FILE.                 IH342     
000970     SW3 ON = PAYROLL DETAIL INPUT END OF FILE.                   IH342     
000980     SW4 ON = THIRD SHIFT BONUS NIGHT PAY.                        IH342     
000990     SW5 ON = PAYROLL EXCEPTION PAY.                              IH342     
001000     SW6 ON = COMPUTE PAYROLL EXCEPTION.                          IH342     
001010     SW7 ON = CODE B BONUS.                                       IH342     
001020     SW8 ON = WHARF BUILDER BONUS.                                IH342     
001030     SW9 ON = DIVER PAY.                                          IH342     
001040     SW10 ON = OVERTIME PAY TO BE COMPUTED.                       IH342     
001050     SW11 ON = HOLIDAY HOURS WORKED AND PAID.                     IH342     
001060     SW12 ON = HOLIDAY INCLUDED IN ANNUAL LEAVE PAYOFF.           IH342     
001070     SW13 ON = LIFE INSURANCE AND OPTION DEDUCTIONS CANCELLED.    IH342     
001080     SW14 ON = HEALTH INSURANCE DEDUCTION CANCELLED.              IH342     
001090     SW15 ON = TAX LEVY DEDUCTION.                                IH342     
001100     SW16 ON = RETROACTIVE REGULAR HOURS.                         IH342     
001110     SW17 ON = 80TH HOUR WITH ADDED PAY.                          IH342     
001120     SW18 ON = PAID BY VOUCHER, NO CHECK NUMBER.                  IH342     
001130     SW19 ON = CSRA DEDUCTION ADJUSTMENT.                         IH342     
001140     SW20 ON = FICA DEDUCTION ADJUSTMENT.                         IH342     
001150     SW21 ON = FEDERAL TAX DEDUCTION ADJUSTMENT.                  IH342     
001160     SW22 ON = HEALTH INSURANCE DEDUCTION ADJUSTMENT.             IH342     
001170     SW23 ON = LIFE INSURANCE DEDUCTION ADJUSTMENT.               IH342     
001180     SW24 ON = LIFE INSURANCE OPTION DEDUCTION ADJUSTMENT.        IH342     
001190     SW25 ON = STATE TAX DEDUCTION ADJUSTMENT.                    IH342     
001200     SW26 ON = UNION DUES DEDUCTION ADJUSTMENT.                   IH342     
001210     SW27 ON = BOND DEDUCTION CANCELLED.                          IH342     
001220     SW28 ON = DECEASED ANNUAL LEAVE PAYOFF.                      IH342     
001230     SW29 ON = TERMINAL ANNUAL LEAVE PAYOFF.                      IH342     
001240     SW30 ON = CASH AWARD.                                        IH342     
001250     SW31 ON = PAYROLL ERROR MESSAGE DO NOT WRITE DETAIL.         IH342     
001260     SW32 ON = 25( SUNDAY PREMIUM REGULAR SCHEDULE.               IH342     
001270     SW33 ON = 80TH HOUR WITH ADDED PAY HOURLY RATE SAVED.        IH342     
001280     SW34 ON = HOLIDAY WORKED ADD TO OTHER MONEY.                 IH342     
001290     SW35 ON = FICA MAXIMUM REACHED CURRENT PERIOD.               IH342     
001300     SW36 ON = COMPUTE TAX ON TOTAL CASH AWARD.                   IH342     
001310     SW37 ON = WRITE SEPARATE TAX LEVY CHECK.                     IH342     
001320     SW38 ON = END OF JOB.                                        IH342     
001330     SW40 ON = CHARITY ADJUSTMENT                                 IH342     
001340     SW41 ON = BOND ADJUSTMENT                                    IH342     
001350     SW42 ON = RETRO OVERTIME                                     IH342     
001360     SW43 ON = ALLOT1 ADJUSTMENT                                  IH342     
001370     SW44 ON = ALLOT2 ADJUSTMENT                                  IH342     
001380     SW45 ON = OTHER DED ADJUSTMENT                               IH342     
001390     SW46 ON = NET PAY ADJUSTMENT                                 IH342     
001400     SW47 ON = NOT SUB TO FED TAX ADJUSTMENT                      IH342     
001410     SW48 ON = RETRO OTHER MONEY                                  IH342     
001420     SW49 ON = PAYOFF LWOP INVOLVED                               IH342     
001430     SW50 ON = PAYOFF REGULAR                                     IH342     
001440     SW51 ON = THIRD SHIFT OVERTIME (M)                           IH342     
001450     SW52 ON = SECOND SHIFT OVERTIME (L)                          IH342     
001460     SW53 ON = CODE K BONUS                                       IH342     
001470     SW54 ON = CODE N BONUS                                       IH342     
001480     SW55 ON = CODE R BONUS                                       IH342     
001490 ENVIRONMENT  DIVISION.                                           IH342     
001500 CONFIGURATION  SECTION.                                          IH342     
001510 SOURCE-COMPUTER. B-3500.                                         IH342     
001520 OBJECT-COMPUTER. B-3500 SEGMENT-LIMIT IS 01.                     IH342     
001530 SPECIAL-NAMES.                                                   IH342     
001550 INPUT-OUTPUT  SECTION.                                           IH342     
001560 FILE-CONTROL.                                                    IH342     
001570     SELECT PERS-MSTR-IN-FILE ASSIGN TO DISK.                     IH342     
001580                                                                  IH342     
001590     SELECT PAYROLL-DETAIL-FILE ASSIGN TO DISK.                   IH342     
001600                                                                  IH342     
001610     SELECT PERS-MSTR-OUT-FILE ASSIGN TO DISK.                    IH342     
001620                                                                  IH342     
001630     SELECT PAYROLL-WORK-FILE ASSIGN TO DISK.                     IH342     
001640                                                                  IH342     
001650     SELECT SMOOTH-ROLL-FILE ASSIGN TO DISK.                      IH342     
001660                                                                  IH342     
001670     SELECT CONTROL-ERROR-FILE,                                   IH342     
001680        ASSIGN TO DISK.                                           IH342     
001690                                                                  IH342     
001700     SELECT PAY-EXCEPT-LIST-FILE ASSIGN TO DISK.                  IH342     
001710                                                                  IH342     
001720 DATA  DIVISION.                                                  IH342     
001730 FILE  SECTION.                                                   IH342     
001740 FD   PERS-MSTR-IN-FILE                                           IH342     
001750     RECORD CONTAINS 610 CHARACTERS                               IH342     
001760     LABEL RECORDS ARE STANDARD                                   IH342     
001770     VALUE OF IDENTIFICATION IS "FILE11"                          IH342     
001780     DATA RECORDS ARE PERS-MSTR-IN-REC, CONSTANT-REC-1,           IH342     
001790                      CONSTANT-REC-2                              IH342     
001800                                                                  IH342     
001810          .                                                       IH342     
001820 01   PERS-MSTR-IN-REC.                                           IH342     
001830     02  MSTR-IN-SEQ.                                             IH342     
001840         04  IN-ACTY-MAJOR   PICTURE X.                           IH342     
001850         04  IN-GORU-INTER   PICTURE X.                           IH342     
001860         04  IN-CNTL-GRP     PICTURE XX.                          IH342     
001870         04  IN-BADGE-MINOR  PICTURE X(6).                        IH342     
001880     02  FILLER              PICTURE X(600).                      IH342     
001890 01  CONSTANT-REC-1.                                              IH342     
001900     02 CON-ID               PICTURE X(30).                       IH342     
001910     02 CON-KEY              PICTURE X.                           IH342     
001920     02 ACT-DATA-1           PICTURE X(253).                      IH342     
001930     02 FILLER               PICTURE X.                           IH342     
001940     02 ACT-NAME             PICTURE X(20).                       IH342     
001950     02 FILLER               PICTURE XXX.                         IH342     
001960     02 ACT-DATA-2           PICTURE X(273).                      IH342     
001970     02 FILLER               PICTURE X(29).                       IH342     
001980 01  CONSTANT-REC-2.                                              IH342     
001990     02 FILLER               PICTURE X(31).                       IH342     
002000     02 UNION-DATA-IN        PICTURE X(175).                      IH342     
002010     02 LOCAL-CON            PICTURE X(404).                      IH342     
002020 FD   PAYROLL-DETAIL-FILE                                         IH342     
002030     RECORD CONTAINS 80 CHARACTERS                                IH342     
002040     LABEL RECORDS ARE STANDARD                                   IH342     
002050     VALUE OF IDENTIFICATION IS "FILE22"                          IH342     
002060     DATA RECORDS ARE PAYROLL-DETAIL-REC, PARAM-CARD              IH342     
002070                                                                  IH342     
002080          .                                                       IH342     
002090 01   PAYROLL-DETAIL-REC.                                         IH342     
002100     02  DTL-SEQ.                                                 IH342     
002110         04  DTL-ACTY        PICTURE X.                           IH342     
002120         04  DTL-GORU        PICTURE X.                           IH342     
002130         04  DTL-CNTL-GRP    PICTURE XX.                          IH342     
002140         04  DTL-BADGE       PICTURE X(6).                        IH342     
002150     02  DTL-PAYROLL-CODES.                                       IH342     
002160         04  DTL-PAY-CODE                    OCCURS 5 TIMES.      IH342     
002170             06  PAY-CODE    PICTURE X.                           IH342     
002180             06  PAY-CODE-WEEK PICTURE X.                         IH342     
002190     02  REDEF-PAY-CODE REDEFINES DTL-PAYROLL-CODES.              IH342     
002200         04  DTL-PAY-NUM     PICTURE 99      OCCURS ES.      IH342     
002210     02  DTL-HOURS           PICTURE S999V99.                     IH342     
002220     02 DTL-X-HOURS REDEFINES DTL-HOURS                           IH342     
002230                             PICTURE X(5).                        IH342     
002240     02  DTL-AMOUNT          PICTURE S9999V99.                    IH342     
002250     02  DTL-X-AMOUNT REDEFINES DTL-AMOUNT                        IH342     
002260                             PICTURE XXXXXX.                      IH342     
002270     02  FILLER              PICTURE X(49).                       IH342     
002280 01   PARAM-CARD.                                                 IH342     
002290     02  PARAM-CNTL          PICTURE XXXX.                        IH342     
002300     02  PARAM-DATA-IN       PICTURE X(76).                       IH342     
002310 FD   PERS-MSTR-OUT-FILE                                          IH342     
002320     RECORD CONTAINS 610 CHARACTERS                               IH342     
002330     LABEL RECORDS ARE STANDARD                                   IH342     
002340     VALUE OF IDENTIFICATION IS "FILE33"                          IH342     
002350     DATA RECORDS ARE PERS-MSTR-OUT-REC                           IH342     
002360                                                                  IH342     
002370          .                                                       IH342     
002380 01   PERS-MSTR-OUT-REC.                                          IH342     
002390     02  MSTR-OUT-SEQ.                                            IH342     
002400         04  MSTR-MAJ-CNTRL.                                      IH342     
002410         06  MSTR-ACTY       PICTURE X.                           IH342     
002420         06  MSTR-GORU       PICTURE X.                           IH342     
002430             88  ANNUM-MSTR                  VALUE "G".           IH342     
002440             88  DIEM-MSTR                   VALUE "U".           IH342     
002450         06  MSTR-CONT-GRP   PICTURE XX.                          IH342     
002460         04  MSTR-BADGE      PICTURE X(6).                        IH342     
002470     02  MSTR-NAME           PICTURE X(20).                       IH342     
002480     02  MSTR-ACTIVE-CODE    PICTURE X.                           IH342     
002490         88  ACTIVE-MSTR                     VALUE " ".           IH342     
002500     02  MSTR-COST-WORK-CNTR.                                     IH342     
002510       04  CC-WC-ACTY        PICTURE X.                           IH342     
002520         04  MSTR-COST-CNTR  PICTURE XX.                          IH342     
002530         04  MSTR-WORK-CNTR  PICTURE XX.                          IH342     
002540     02  FILLER              PICTURE XXXX.                        IH342     
002550     02  MSTR-GS-ANNUM       PICTURE X.                           IH342     
002560         88  GS-GRADED-MSTR                  VALUE "1".           IH342     
002570     02  MSTR-GS-GRADE       PICTURE XX.                          IH342     
002580     02  MSTR-STEP-NO        PICTURE XX.                          IH342     
002590     02  FILLER              PICTURE XXXX.                        IH342     
002600     02  PART-TIME-CODE      PICTURE X.                           IH342     
002610     02  MSTR-MARRIED-CODE   PICTURE X.                           IH342     
002620         88  MARRIED-MSTR                    VALUE "M".           IH342     
002630         88  SINGLE-MSTR                     VALUE "S".           IH342     
002640     02  MSTR-SS-NO          PICTURE X(12).                       IH342     
002650     02  FILLER              PICTURE X(61).                       IH342     
002660     02  MSTR-OCC-CD         PICTURE X(10).                       IH342     
002670     02  MSTR-RET-SER        PICTURE X(5).                        IH342     
002680     02  MSTR-ANNUAL-RATE    PICTURE S9(5)V99.                    IH342     
002690     02  FILLER          PICTURE X(29).                           IH342     
002700     02  MSTR-CLOCK-NO   PICTURE XX.                              IH342     
002710     02  FILLER          PICTURE X(24).                           IH342     
002720     02  MSTR-EOD-DATE.                                           IH342     
002730         04  EOD-MODAY.                                           IH342     
002740             06  EOD-MONTH   PICTURE 99.                          IH342     
002750             06  EOD-DAY     PICTURE 99.                          IH342     
002760         04  EOD-YEAR        PICTURE S99.                         IH342     
002770     02  FILLER              PICTURE X(67).                       IH342     
002780     02  MSTR-RETIRE-CODE    PICTURE X.                           IH342     
002790         88  CSRA-MSTR                       VALUE "1".           IH342     
002800         88  FICA-MSTR                       VALUE "2".           IH342     
002810     02  MSTR-FF-CODE        PICTURE X.                           IH342     
002820         88  FIRE-CHIEF-MSTR                 VALUE "1".           IH342     
002830         88  FIRE-FIGHTER-MSTR               VALUE "2".           IH342     
002840         88  FIRE-FIGHTER1-MSTR              VALUE "6".           IH342     
002850         88  NOT-FF-MSTR                     VALUE " ".           IH342     
002860     02  MSTR-FF-SUN-PREM-CD PICTURE X.                           IH342     
002870         88  FF-QRTR-PREM-MSTR               VALUE "1".           IH342     
002880         88  FF-FULL-PREM-MSTR               VALUE "2".           IH342     
002890         88  FF-HALF-PREM-MSTR               VALUE "6".           IH342     
002900     02  MSTR-NITE-CODE      PICTURE X.                           IH342     
002910         88  2ND-SHIFT-MSTR                  VALUE "2".           IH342     
002920         88  3RD-SHIFT-MSTR                  VALUE "3".           IH342     
002930     02  MSTR-TAX-CODE       PICTURE S99.                         IH342     
002940     02  MSTR-ADDED-TAX      PICTURE S999.                        IH342     
002950     02  MSTR-STATE-TAX-CODE PICTURE X.                           IH342     
002960     02  MSTR-UNION-CODE     PICTURE X.                           IH342     
002970     02  MSTR-UNION REDEFINES MSTR-UNION-CODE                     IH342     
002980                             PICTURE S9.                          IH342     
002990     02  MSTR-HOURLY-RATE    PICTURE S99V99.                      IH342     
003000     02  MSTR-LIFE-INS-CODE  PICTURE X.                           IH342     
003010         88  LIFE-INSUR-MSTR                 VALUE "1".           IH342     
003020     02  MSTR-LIFE-OPT-CODE  PICTURE X.                           IH342     
003030     02  MSTR-OPT-CODE REDEFINES MSTR-LIFE-OPT-CODE               IH342     
003040                             PICTURE S9.                          IH342     
003050     02  MSTR-LIFE-INS-DED   PICTURE S99V99.                      IH342     
003060     02  MSTR-LIFE-OPT-DED   PICTURE S99V99.                      IH342     
003070     02  MSTR-CHARITY-CODE   PICTURE S99.                         IH342     
003080     02  MSTR-CHARITABLE     PICTURE S99V99.                      IH342     
003090     02  MSTR-HLTH-CODE      PICTURE S999.                        IH342     
003100     02 MSTR-HLTH-CODE-RE REDEFINES MSTR-HLTH-CODE PIC X(4).      IH342     
003110     02  MSTR-HLTH-CARRIER   PICTURE S9(8).                       IH342     
003120     02  MSTR-HLTH-DATE      PICTURE S9(6).                       IH342     
003130     02  MSTR-HLTH-DED       PICTURE S99V99.                      IH342     
003140     02 MSTR-HLTH-DED-RE REDEFINES MSTR-HLTH-DED PIC X(5).        IH342     
003150     02  MSTR-HLTH-CONTRIB   PICTURE S99V99.                      IH342     
003160     02 MSTR-HLTH-CONTRIB-RE REDEFINES MSTR-HLTH-CONTRIB PIC X(5).IH342     
003170     02  MSTR-BANK-DEP-CODE.                                      IH342     
003180         04  BANK-CODE1      PICTURE XX.                          IH342     
003190         04  BRANCH-CODE1    PICTURE XX.                          IH342     
003200     02  MSTR-BANK-DEP-NO    PICTURE X(11).                       IH342     
003210     02  MSTR-ALLOT1-CODE    PICTURE XXXX.                        IH342     
003220     02  MSTR-ALLOT1-DEP-NO  PICTURE X(11).                       IH342     
003230     02  MSTR-ALLOT1-DED     PICTURE S999.                        IH342     
003240     02  MSTR-ALLOT2-CODE    PICTURE XXXX.                        IH342     
003250     02  MSTR-ALLOT2-DEP-NO  PICTURE X(11).                       IH342     
003260     02  MSTR-ALLOT2-DED     PICTURE S999.                        IH342     
003270     02  MSTR-YTD-NET-PAY    PICTURE S9(5)V99.                    IH342     
003280     02  MSTR-YTD-FICA       PICTURE S999V99.                     IH342     
003290     02  MSTR-YTD-RETIRE     PICTURE S9999V99.                    IH342     
003300     02  MSTR-TOT-RETIRE     PICTURE S9(5)V99.                    IH342     
003310     02  MSTR-YTD-FED-TAX    PICTURE S9999V99.                    IH342     
003320     02  MSTR-YTD-HLTH-DED   PICTURE S999V99.                     IH342     
003330     02  MSTR-YTD-HLTH-CON   PICTURE S999V99.                     IH342     
003340     02  MSTR-YTD-LIFE-INS   PICTURE S999V99.                     IH342     
003350     02  MSTR-YTD-LIFE-OPT   PICTURE S999V99.                     IH342     
003360     02  MSTR-YTD-BONDS      PICTURE S9999V99.                    IH342     
003370     02  MSTR-ADDL-ST-TAX    PICTURE S999V99.                     IH342     
003380     02  MSTR-YTD-OTH-DED    PICTURE S9999V99.                    IH342     
003390     02  MSTR-YTD-STATE-TAX  PICTURE S9999V99.                    IH342     
003400     02  MSTR-YTD-UNION      PICTURE S999V99.                     IH342     
003410     02  MSTR-YTD-GOV-INDEBT PICTURE S9999V99.                    IH342     
003420     02  MSTR-YTD-GOV-PAY    PICTURE S999V99.                     IH342     
003430     02  MSTR-YTD-CHARITY    PICTURE S999V99.                     IH342     
003440     02  MSTR-YTD-ALLOT1-DED PICTURE S9(5).                       IH342     
003450     02  MSTR-YTD-ALLOT2-DED PICTURE S9(5).                       IH342     
003460     02  MSTR-YTD-REG-MONEY  PICTURE S9(5)V99.                    IH342     
003470     02  MSTR-YTD-OT-MONEY   PICTURE S9999V99.                    IH342     
003480     02  MSTR-YTD-OTH-MONEY  PICTURE S9999V99.                    IH342     
003490     02  MSTR-YTD-TOT-EARN   PICTURE S9(5)V99.                    IH342     
003500     02  MSTR-QTR-SUB-FICA   PICTURE S9999V99.                    IH342     
003510     02  MSTR-YTD-SUB-FICA   PICTURE S9999V99.                    IH342     
003520     02 MSTR-YTD-SUB-FICA-RE REDEFINES MSTR-YTD-SUB-FICA PIC X(7).IH342     
003530     02  MSTR-YTD-NOT-FICA   PICTURE S9(5)V99.                    IH342     
003540     02  MSTR-YTD-SUB-RET    PICTURE S9(5)V99.                    IH342     
003550     02  MSTR-BOND-PLEDGE    PICTURE X.                           IH342     
003560     02  MSTR-NUMERIC-BOND REDEFINES                              IH342     
003570     MSTR-BOND-PLEDGE    PICTURE 9.                               IH342     
003580     02  FILLER              PICTURE X(11).                       IH342     
003590     02  MSTR-BOND-DED       PICTURE S9999V99.                    IH342     
003600     02  FILLER              PICTURE X(5).                        IH342     
003610     02  MSTR-YTD-NSFT       PICTURE S9999V99.                    IH342     
003620     02  FILLER              PICTURE X(24).                       IH342     
003630     02  MSTR-ST-TAX-EXEM-CD PICTURE S99.                         IH342     
003640     02 MSTR-ST-TAX-EXEM-CD-RE REDEFINES MSTR-ST-TAX-EXEM-CD      IH342     
003650             PIC X(3).                                            IH342     
003660     02  MSTR-ST-QTRLY-TAX   PICTURE S9999V99.                    IH342     
003670     02  FILLER              PICTURE  XXX.                        IH342     
003680     02  MSTR-OTHER-PAY-CODE PICTURE X.                           IH342     
003690     02  FILLER              PICTURE X(7).                        IH342     
003700 FD   SMOOTH-ROLL-FILE                                            IH342     
003710     RECORD CONTAINS 160 CHARACTERS                               IH342     
003720     LABEL RECORDS ARE STANDARD                                   IH342     
003730     VALUE OF IDENTIFICATION IS "FILE44"                          IH342     
003740     DATA RECORDS ARE  SMOOTH-ROLL-REC,  SMOOTH-ROLL-REC-LINE2,   IH342     
003750                      SMOOTH-ROLL-TOTAL-LINE1,                    IH342     
003760                      SMOOTH-ROLL-TOTAL-LINE2,                    IH342     
003770                      SMOOTH-ROLL-TOTAL-LINE3,                    IH342     
003780                      SMOOTH-ROLL-HEADING1,                       IH342     
003790                      SMOOTH-ROLL-HEADING2,                       IH342     
003800                      SMOOTH-ROLL-HEADING3-4                      IH342     
003810                                                                  IH342     
003820          .                                                       IH342     
003830 01   SMOOTH-ROLL-REC.                                            IH342     
003840     02  SR-NAME             PICTURE X(20).                       IH342     
003850     02  FILLER          PICTURE X.                               IH342     
003860     02  SR-BANK-CODE    PICTURE XXXX.                            IH342     
003870     02  FILLER          PICTURE X.                               IH342     
003880     02  SR-CLOCK-NO     PICTURE XX.                              IH342     
003890     02  FILLER          PICTURE XX.                              IH342     
003900     02  SR-HOURLY-RATE      PICTURE ZZZVZZ-.                     IH342     
003910     02  SR-REG-HOURS        PICTURE ZZZVZZ-.                     IH342     
003920     02  SR-REG-PAY          PICTURE ZZZZVZZ-.                    IH342     
003930     02  SR-OT-HOURS         PICTURE ZZZVZZ-.                     IH342     
003940     02  SR-OT-MONEY         PICTURE ZZZZVZZ-.                    IH342     
003950     02  SR-EXC-CODE         PICTURE X.                           IH342     
003960     02  FILLER              PICTURE XX.                          IH342     
003970     02  SR-OTHER-HOURS      PICTURE ZZZVZZ-.                     IH342     
003980     02  SR-OTHER-MONEY      PICTURE ZZZZVZZ-.                    IH342     
003990     02  FILLER              PICTURE X.                           IH342     
004000     02  SR-CSRA             PICTURE ZZZVZZ-.                     IH342     
004010     02  SR-FICA             PICTURE ZZZVZZ-.                     IH342     
004020     02  SR-FED-MAR-CODE     PICTURE X.                           IH342     
004030     02  FILLER              PICTURE X.                           IH342     
004040     02  SR-TAX-CODE         PICTURE ZZ.                          IH342     
004050     02  SR-FED-TAX          PICTURE ZZZZVZZ-.                    IH342     
004060     02  SR-STATE-TAX-CODE   PICTURE X.                           IH342     
004070     02  SR-STATE-MAR-CODE   PICTURE XX.                          IH342     
004080     02  SR-STATE-EXC        PICTURE XX.                          IH342     
004090     02  SR-STATE-TAX        PICTURE ZZZZVZZ-.                    IH342     
004100     02  SR-ADDED-TAX        PICTURE ZZZ-.                        IH342     
004110     02  FILLER              PICTURE XXXX.                        IH342     
004120     02  SR-FILLER           PICTURE X(10).                       IH342     
004130 01   SMOOTH-ROLL-REC-LINE2.                                      IH342     
004140     02  SR-BADGE            PICTURE Z9(5).                       IH342     
004150     02  FILLER              PICTURE X.                           IH342     
004160     02  FILLER              PICTURE X.                           IH342     
004170     02  SR-PREFIX.                                               IH342     
004180         04  SR-PFX-ACTY     PICTURE X.                           IH342     
004190         04  SR-PFX-CTL-GP   PICTURE XX.                          IH342     
004200     02  FILLER              PICTURE X.                           IH342     
004210     02  SR-CHECK-NO         PICTURE Z(8).                        IH342     
004220     02  SR-DEPOSIT REDEFINES SR-CHECK-NO  PICTURE X(8).          IH342     
004230     02  SR-NET-PAY          PICTURE ZZZZVZZ-.                    IH342     
004240     02  SR-HLTH-CODE        PICTURE ZZZ.                         IH342     
004250     02  SR-HLTH-DED         PICTURE ZZZVZZ-.                     IH342     
004260     02  SR-LIFE-INS-DED     PICTURE ZZZVZZ-.                     IH342     
004270     02  SR-LIFE-OPT-DED     PICTURE ZZZZVZZ-.                    IH342     
004280     02  SR-UNION-CODE       PICTURE X.                           IH342     
004290     02  FILLER              PICTURE X(5).                        IH342     
004300     02  SR-UNION-DED        PICTURE ZZZZVZZ-.                    IH342     
004310     02  SR-CHARITY-CODE     PICTURE XX.                          IH342     
004320     02  FILLER              PICTURE X.                           IH342     
004330     02  SR-CHARITY-DED      PICTURE ZZZVZZ-.                     IH342     
004340     02  SR-OTHER-DED        PICTURE ZZZZVZZ-.                    IH342     
004350     02  FILLER              PICTURE X.                           IH342     
004360     02  SR-BOND-DED         PICTURE ZZZVZZ-.                     IH342     
004370     02  FILLER              PICTURE X(6).                        IH342     
004380     02  SR-BANK-CODE1       PICTURE XXXX.                        IH342     
004390     02  SR-BANK-ALLOT-DED1  PICTURE ZZZZVZZ-.                    IH342     
004400     02  SR-BANK-CODE2       PICTURE XXXX.                        IH342     
004410     02  FILLER              PICTURE X.                           IH342     
004420     02  SR-BANK-ALLOT-DED2  PICTURE ZZZZVZZ-.                    IH342     
004430     02  SR-ADDED-ST-TAX     PICTURE ZZZVZZ-.                     IH342     
004440     02  FILLER              PICTURE X(2).                        IH342     
004450     02  SR2-FILLER          PICTURE X(10).                       IH342     
004460 01   SMOOTH-ROLL-TOTAL-LINE1.                                    IH342     
004470     02  SR-EMP-COUNT        PICTURE Z(7).                        IH342     
004480     02  SR-TOT-RATE         PICTURE Z(7)VZZ-.                    IH342     
004490     02  SR-TOT-NET          PICTURE Z(7)VZZ-.                    IH342     
004500     02  SR-TOT-REG-HR       PICTURE Z(6)VZZ-.                    IH342     
004510     02  SR-TOT-REG          PICTURE Z(8)VZZ-.                    IH342     
004520     02  SR-TOT-OT-HR        PICTURE Z(5)VZZ-.                    IH342     
004530     02  SR-TOT-OT           PICTURE Z(8)VZZ-.                    IH342     
004540     02  SR-TOT-OTHER-HR     PICTURE Z(7)VZZ-.                    IH342     
004550     02  SR-TOT-OTHER        PICTURE Z(8)VZZ-.                    IH342     
004560     02  SR-TOT-CSRA         PICTURE Z(7)VZZ-.                    IH342     
004570     02  SR-TOT-FICA         PICTURE Z(6)VZZ-.                    IH342     
004580     02  SR-TOT-FED-TAX      PICTURE Z(8)VZZ-.                    IH342     
004590     02  FILLER              PICTURE X(15).                       IH342     
004600 01   SMOOTH-ROLL-TOTAL-LINE2.                                    IH342     
004610     02  SR-TOT-STATE-TAX    PICTURE Z(8)VZZ-.                    IH342     
004620     02  SR-TOT-HLTH-DED     PICTURE Z(5)VZZ-.                    IH342     
004630     02  SR-TOT-LIFE-INS     PICTURE Z(6)VZZ-.                    IH342     
004640     02  SR-TOT-OPTA         PICTURE Z(6)VZZ-.                    IH342     
004650     02  SR-TOT-OPTC         PICTURE Z(6)VZZ-.                    IH342     
004660     02  SR-TOT-OPTE         PICTURE Z(6)VZZ-.                    IH342     
004670     02  FILLER              PICTURE X.                           IH342     
004680     02  SR-TOT-OPTG         PICTURE Z(6)VZZ-.                    IH342     
004690     02  SR-TOT-UNION-DED    PICTURE Z(5)VZZ-.                    IH342     
004700     02  SR-TOT-CHARITY      PICTURE Z(5)VZZ-.                    IH342     
004710     02  SR-TOT-OTHER-DED    PICTURE Z(6)VZZ-.                    IH342     
004720     02  SR-TOT-BOND-DED     PICTURE Z(7)VZZ-.                    IH342     
004730     02  FILLER              PICTURE X(10).                       IH342     
004740     02  SR-TOT-ALLOT        PICTURE Z(7)VZZ-.                    IH342     
004750     02  FILLER              PICTURE X(12).                       IH342     
004760 01   SMOOTH-ROLL-TOTAL-LINE3.                                    IH342     
004770     02 FILLER               PICTURE X(28).                       IH342     
004780     02 SR-TOT-OPTB          PICTURE Z(6)VZZ-.                    IH342     
004790     02 SR-TOT-OPTD          PICTURE Z(6)VZZ-.                    IH342     
004800     02 SR-TOT-OPTF          PICTURE Z(6)VZZ-.                    IH342     
004810     02 SR-TOT-OPT-INS       PICTURE Z(7)VZZ.                     IH342     
004820     02 SR-AST               PICTURE X.                           IH342     
004830     02  FILLER              PICTURE X(67).                       IH342     
004840 01   SMOOTH-ROLL-HEADING1.                                       IH342     
004850     02  SR-DEPT             PICTURE X(6).                        IH342     
004860     02  SR-FROM-MO          PICTURE XX.                          IH342     
004870     02  SR-DASH1            PICTURE X.                           IH342     
004880     02  SR-FROM-DAY         PICTURE XX.                          IH342     
004890     02  FILLER              PICTURE XX.                          IH342     
004900     02  SR-TO-MO            PICTURE XX.                          IH342     
004910     02  SR-DASH2            PICTURE X.                           IH342     
004920     02  SR-TO-DAY           PICTURE XX.                          IH342     
004930     02  FILLER              PICTURE XXX.                         IH342     
004940     02  SR-DO-SYMBOL        PICTURE XXXX.                        IH342     
004950     02  FILLER              PICTURE X(107).                      IH342     
004960 01   SMOOTH-ROLL-HEADING2.                                       IH342     
004970     02  SR-LOCATION         PICTURE X(27).                       IH342     
004980     02  FILLER              PICTURE X(9).                        IH342     
004990     02  SR-CPR-KON          PICTURE XXX.                         IH342     
005000     02  FILLER              PICTURE XX.                          IH342     
005010     02  SR-ROLL-NO          PICTURE 99.                          IH342     
005020     02  FILLER              PICTURE X(4).                        IH342     
005030     02  SR-YEAR-KON         PICTURE X(4).                        IH342     
005040     02  FILLER              PICTURE X.                           IH342     
005050     02  SR-PERIOD-YEAR      PICTURE XX.                          IH342     
005060     02  FILLER              PICTURE X(12).                       IH342     
005070     02  SR-ACTY-CODE        PICTURE X.                           IH342     
005080     02  SR-CNTL-GRP         PICTURE XX.                          IH342     
005090     02  FILLER              PICTURE X.                           IH342     
005100     02  SR-ACTIVITY         PICTURE X(20).                       IH342     
005110     02 FILLER               PICTURE XX.                          IH342     
005120     02  SR-UG-LITERAL       PICTURE X(10).                       IH342     
005130     02  SR-PAGE-KON         PICTURE X(9).                        IH342     
005140     02  SR-PAGE-NO          PICTURE ZZZZZ.                       IH342     
005150     02 FILLER               PICTURE X(16).                       IH342     
005160 01   SMOOTH-ROLL-HEADING3-4.                                     IH342     
005170     02  FILLER              PICTURE X(115).                      IH342     
005180     02 SR-ADDL-KON          PICTURE X(17).                       IH342     
005190 FD   PAYROLL-WORK-FILE                                           IH342     
005200     RECORD CONTAINS 400 CHARACTERS                               IH342     
005210     LABEL RECORDS ARE STANDARD                                   IH342     
005220     VALUE OF IDENTIFICATION IS "FILE55"                          IH342     
005230     DATA RECORDS ARE PAYROLL-WORK-REC                            IH342     
005240                                                                  IH342     
005250          .                                                       IH342     
005260 01   PAYROLL-WORK-REC.                                           IH342     
005270     02  WT-SEQUENCE.                                             IH342     
005280         04  WT-ACTIVITY     PICTURE X.                           IH342     
005290         04  WT-GORU         PICTURE X.                           IH342     
005300         04  WT-CNTL-GRP     PICTURE XX.                          IH342     
005310         04  WT-BADGE        PICTURE X(6).                        IH342     
005320     02  WT-COST-WORK-CNTR   PICTURE X(5).                        IH342     
005330     02  WT-NAME             PICTURE X(20).                       IH342     
005340     02  WT-RETIRE-CODE      PICTURE X.                           IH342     
005350     02  WT-MARRIED-CODE     PICTURE X.                           IH342     
005360     02  WT-UNION-CODE       PICTURE X.                           IH342     
005370     02  WT-ROLL             PICTURE 99.                          IH342     
005380     02  WT-BANK-DEP-CODE    PICTURE XXXX.                        IH342     
005390     02  WT-BANK-DEP-NO      PICTURE X(11).                       IH342     
005400     02  WT-ALLOT1-CODE      PICTURE XXXX.                        IH342     
005410     02  WT-ALLOT1-DEP-NO    PICTURE X(11).                       IH342     
005420     02  WT-ALLOT2-CODE      PICTURE XXXX.                        IH342     
005430     02  WT-ALLOT2-DEP-NO    PICTURE X(11).                       IH342     
005440     02  WT-CHECK-NO         PICTURE S9(8).                       IH342     
005450     02  WT-CHECK-CNTR       PICTURE S9.                          IH342     
005460     02  WT-NET-PAY          PICTURE S9999V99.                    IH342     
005470     02  WT-HOURLY-RATE      PICTURE S99V99.                      IH342     
005480     02  WT-TAX-LEVY-ZEROS.                                       IH342     
005490         04  WT-REG-HOURS    PICTURE S999V99.                     IH342     
005500         04  WT-REG-PAY      PICTURE S9999V99.                    IH342     
005510         04  WT-OT-HOURS     PICTURE S999V99.                     IH342     
005520         04  WT-OT-PAY       PICTURE S9999V99.                    IH342     
005530         04  WT-OTHER-HOURS  PICTURE S999V99.                     IH342     
005540         04  WT-OTHER-PAY    PICTURE S9999V99.                    IH342     
005550         04  WT-SUB-FICA     PICTURE S9999V99.                    IH342     
005560         04  WT-FICA-CSRA    PICTURE S999V99.                     IH342     
005570         04  WT-FED-TAX-CODE PICTURE S99.                         IH342     
005580         04  FILLER          PICTURE X(5).                        IH342     
005590         04  WT-STATE-TAX    PICTURE S9999V99.                    IH342     
005600         04  WT-HLTH-CODE    PICTURE S999.                        IH342     
005610         04  WT-HLTH-DED     PICTURE S999V99.                     IH342     
005620         04  WT-HLTH-CONTRIB PICTURE S99V99.                      IH342     
005630         04  WT-LIFE-INS-DED PICTURE S99V99.                      IH342     
005640         04  WT-LIFE-OPT-DED PICTURE S99V99.                      IH342     
005650         04  WT-UNION-DED    PICTURE S99V99.                      IH342     
005660         04  WT-CHARITY-CODE PICTURE XX.                          IH342     
005670         04  WT-CHARITY-DED  PICTURE S99V99.                      IH342     
005680         04  WT-OTHER-DED    PICTURE S9999V99.                    IH342     
005690         04  WT-BOND-DED     PICTURE S999V99.                     IH342     
005700         04  WT-ALLOT1-DED   PICTURE S999.                        IH342     
005710         04  WT-ALLOT2-DED   PICTURE S999.                        IH342     
005720     02  WT-WE-DATE          PICTURE X(6).                        IH342     
005730     02  WT-OTHER-PAY-CODE   PICTURE X.                           IH342     
005740     02  WT-STATE-TAX-CODE   PICTURE X.                           IH342     
005750     02  WT-FF-CODE          PICTURE X.                           IH342     
005760     02  WT-WORK-SCHED       PICTURE X.                           IH342     
005770     02  WT-PAY-EXCEP-CODE   PICTURE XX.                          IH342     
005780     02  WT-MONEY-HOURS-ZEROS.                                    IH342     
005790         04  WT-CATA-MONEY   PICTURE S9(6)V99.                    IH342     
005800         04  WT-CATA-HOURS   PICTURE S9(6)V99.                    IH342     
005810         04  WT-CATB-MONEY   PICTURE S9(5)V99.                    IH342     
005820         04  WT-CATB-HOURS   PICTURE S9(5)V99.                    IH342     
005830         04  WT-CATC-MONEY   PICTURE S9(4)V99.                    IH342     
005840         04  WT-CATC-HOURS   PICTURE S9(5)V99.                    IH342     
005850         04  WT-CATH-MONEY   PICTURE S9(5)V99.                    IH342     
005860         04  WT-CATK-MONEY   PICTURE S9(5)V99.                    IH342     
005870         04  WT-CATL-MONEY   PICTURE S9(5)V99.                    IH342     
005880         04  WT-CATJ-MONEY   PICTURE S9(5)V99.                    IH342     
005890         04  WT-CATJ-HOURS   PICTURE S9(5)V99.                    IH342     
005900         04  WT-NEG-OT       PICTURE S9(5)V99.                    IH342     
005910         04 WT-NEG-OT-RE REDEFINES WT-NEG-OT PIC X(8).            IH342     
005920         04  WT-RETRO-OTPAY  PICTURE S9(5)V99.                    IH342     
005930         04 WT-RETRO-OTPAY-RE REDEFINES WT-RETRO-OTPAY PIC X(8).  IH342     
005940         04  WT-RETRO-OTHRS  PICTURE S9(5)V99.                    IH342     
005950         04  WT-RETRO-OTHRS-RE REDEFINES WT-RETRO-OTHRS PIC X(8). IH342     
005960     02  WT-CLOCK-NO         PICTURE XX.                          IH342     
005970     02  WT-LIFE-OPT-CD      PICTURE X.                           IH342     
005980     02  WT-FED-TAX          PICTURE S9(4)V99.                    IH342     
005990     02  WT-SS-NO            PICTURE X(12).                       IH342     
006000     02  WT-OCC-CD           PICTURE X(10).                       IH342     
006010     02  WT-MONEY-HRS-ZEROS.                                      IH342     
006020         04  WT-CATM-MONEY   PICTURE S9(4)V99.                    IH342     
006030         04  WT-CATM-HOURS   PICTURE S999V99.                     IH342     
006040         04  WT-CATN-MONEY   PICTURE S9(4)V99.                    IH342     
006050         04  WT-CATN-HOURS   PICTURE S99V99.                      IH342     
006060     02  FILLER              PICTURE X(29).                       IH342     
006070 FD   CONTROL-ERROR-FILE                                          IH342     
006080     RECORD CONTAINS 120 CHARACTERS                               IH342     
006090     LABEL RECORDS ARE STANDARD                                   IH342     
006100     VALUE OF IDENTIFICATION IS "FILE66"                          IH342     
006110     DATA RECORDS ARE CONTROL-REC,                                IH342     
006120                      ERROR-MESSAGE-REC                           IH342     
006130                                                                  IH342     
006140          .                                                       IH342     
006150 01   CONTROL-REC.                                                IH342     
006160     02  FILLER              PICTURE X.                           IH342     
006170     02  CNTL-AORD           PICTURE X(5).                        IH342     
006180     02  FILLER              PICTURE X.                           IH342     
006190     02  CNTL-MESSAGE        PICTURE X(20).                       IH342     
006200     02  FILLER              PICTURE X.                           IH342     
006210     02  CNTL-MONEY          PICTURE Z,ZZZ,ZZZ.99-.               IH342     
006220     02  CNTL-COUNT REDEFINES CNTL-MONEY                          IH342     
006230                             PICTURE ZZZZ,ZZZ,ZZ9-.               IH342     
006240     02  FILLER              PICTURE X.                           IH342     
006250     02  CNTL-GRP-KON        PICTURE XXX.                         IH342     
006260     02  FILLER              PICTURE X.                           IH342     
006270     02  CNTL-GROUP          PICTURE XXXX.                        IH342     
006280     02  FILLER              PICTURE X(30).                       IH342     
006290     02  CON-MSG-PRINT       PICTURE X(14).                       IH342     
006300     02  FILLER              PICTURE X(26).                       IH342     
006310 01   ERROR-MESSAGE-REC.                                          IH342     
006320     02  ERR-DETAIL.                                              IH342     
006330         04  ERR-ACTIVITY    PICTURE X.                           IH342     
006340         04  ERR-CONTROL-GRP PICTURE XX.                          IH342     
006350         04  FILLER          PICTURE X.                           IH342     
006360         04  ERR-BADGE       PICTURE X(6).                        IH342     
006370         04  FILLER          PICTURE X.                           IH342     
006380         04  ERR-NAME        PICTURE X(20).                       IH342     
006390       04  FILLER            PICTURE XXX.                         IH342     
006400         04  ERR-EXPLAIN.                                         IH342     
006410             06  ERR-HOURS   PICTURE ZZZ.ZZ.                      IH342     
006420             06  ERR-SPACE REDEFINES ERR-HOURS PICTURE X(6).      IH342     
006430             06  FILLER      PICTURE XX.                          IH342     
006440             06  ERR-MESS    PICTURE X(37).                       IH342     
006450         04  FILLER          PICTURE X(41).                       IH342     
006460 FD   PAY-EXCEPT-LIST-FILE                                        IH342     
006470     RECORD CONTAINS 120 CHARACTERS                               IH342     
006480     LABEL RECORDS ARE STANDARD                                   IH342     
006490     VALUE OF IDENTIFICATION IS "FILE77"                          IH342     
006500     DATA RECORDS ARE PAY-EXCEPT-LIST-REC,  EXCEPT-LIST-DTL,      IH342     
006510                      EXCEPT-LIST-HEADING                         IH342     
006520                                                                  IH342     
006530          .                                                       IH342     
006540 01   PAY-EXCEPT-LIST-REC.                                        IH342     
006550     02  EXC-ACTY-PRINT      PICTURE X.                           IH342     
006560     02  EXC-CTL-GRP-PRINT   PICTURE XX.                          IH342     
006570     02  FILLER              PICTURE XX.                          IH342     
006580     02  EXC-BADGE-KON       PICTURE X(8).                        IH342     
006590     02  FILLER              PICTURE X.                           IH342     
006600     02  EXC-BADGE           PICTURE X(6).                        IH342     
006610     02  EXC-COMMA-KON       PICTURE X.                           IH342     
006620     02  FILLER              PICTURE X.                           IH342     
006630     02  EXC-NAME            PICTURE X(20).                       IH342     
006640     02  FILLER              PICTURE X(78).                       IH342     
006650 01   EXCEPT-LIST-DTL.                                            IH342     
006660     02  EXC-CODE            PICTURE XX.                          IH342     
006670     02  FILLER              PICTURE X.                           IH342     
006680     02  EXC-HRS-AMT         PICTURE ZZ,ZZZ.99-.                  IH342     
006690     02  FINAL-EXPLAIN       PICTURE X(38).                       IH342     
006700     02  EXC-EXPLAIN         PICTURE X(32).                       IH342     
006710     02  FILLER              PICTURE XX.                          IH342     
006720     02  EXC-END-TOTALS      PICTURE Z(6).                        IH342     
006730     02  FILLER              PICTURE X(29).                       IH342     
006740 01   EXCEPT-LIST-HEADING.                                        IH342     
006750     02  EXC-ACTIVITY        PICTURE X(20).                       IH342     
006760     02  FILLER              PICTURE XX.                          IH342     
006770     02  EXC-AORD            PICTURE X(8).                        IH342     
006780     02  FILLER              PICTURE X.                           IH342     
006790     02  EXC-LIST-TITLE      PICTURE X(25).                       IH342     
006800     02  FILLER              PICTURE XXX.                         IH342     
006810     02  EXC-WE-MO           PICTURE XX.                          IH342     
006820     02  EXC-SLASH1          PICTURE X.                           IH342     
006830     02  EXC-WE-DAY          PICTURE XX.                          IH342     
006840     02  EXC-SLASH2          PICTURE X.                           IH342     
006850     02  EXC-WE-YR           PICTURE XX.                          IH342     
006860     02  FILLER              PICTURE XX.                          IH342     
006870     02  EXC-ROLL-KON        PICTURE XXXX.                        IH342     
006880     02  FILLER              PICTURE X.                           IH342     
006890     02  EXC-ROLL            PICTURE 99.                          IH342     
006900     02  FILLER              PICTURE XX.                          IH342     
006910     02  EXPGCON             PICTURE X(5).                        IH342     
006920     02  EXC-PG-NO           PICTURE ZZZ9.                        IH342     
006930     02  FILLER              PICTURE X(33).                       IH342     
006940 WORKING-STORAGE  SECTION.                                        IH342     
006950 01   SEQ-PERS-MSTR           PICTURE X(10)   VALUE ZEROS.        IH342     
006960 01   SEQ-PAY-DTL             PICTURE X(10)   VALUE ZEROS.        IH342     
006970 01   PARM-DATA.                                                  IH342     
006980     02  COMPOSIT-SW         PICTURE X       VALUE SPACE.         IH342     
006990        88 COMPOSIT-CHECKS                   VALUE ZERO.          IH342     
007000     02  FILLER              PICTURE X(5)    VALUE SPACES.        IH342     
007010     02  WEEK-BEGIN-DATE.                                         IH342     
007020         04  WB-MONTH        PICTURE XX      VALUE SPACES.        IH342     
007030         04  WB-DAY          PICTURE XX      VALUE SPACES.        IH342     
007040         04  WB-YEAR         PICTURE XX      VALUE SPACES.        IH342     
007050     02  WEEK-END-DATE.                                           IH342     
007060         04  WE-MODAY.                                            IH342     
007070             06  WE-MONTH    PICTURE 99      VALUE 00.            IH342     
007080             06  WE-DAY      PICTURE 99      VALUE 00.            IH342     
007090         04  WE-YEAR         PICTURE 99      VALUE 00.            IH342     
007100     02  ROLL-NO             PICTURE 99      VALUE 00.            IH342     
007110     02  EXCEPT-CODES.                                            IH342     
007120         04  EXCEPT-1        PICTURE X       VALUE SPACES.        IH342     
007130         04  EXCEPT-2        PICTURE X       VALUE SPACES.        IH342     
007140         04  EXCEPT-3        PICTURE X       VALUE SPACES.        IH342     
007150         04  EXCEPT-4        PICTURE X       VALUE SPACES.        IH342     
007160         04  EXCEPT-5        PICTURE X       VALUE SPACES.        IH342     
007170         04  EXCEPT-6        PICTURE X       VALUE SPACES.        IH342     
007180         04  EXCEPT-7        PICTURE X       VALUE SPACES.        IH342     
007190         04  EXCEPT-8        PICTURE X       VALUE SPACES.        IH342     
007200         04  EXCEPT-9        PICTURE X       VALUE SPACES.        IH342     
007210         04  EXCEPT-10       PICTURE X       VALUE SPACES.        IH342     
007220         04  EXCEPT-11       PICTURE X       VALUE SPACES.        IH342     
007230         04  EXCEPT-12       PICTURE X       VALUE SPACES.        IH342     
007240         04  EXCEPT-13       PICTURE X       VALUE SPACES.        IH342     
007250         04  EXCEPT-14       PICTURE X       VALUE SPACES.        IH342     
007260         04  EXCEPT-15       PICTURE X       VALUE SPACES.        IH342     
007270         04  EXCEPT-16       PICTURE X       VALUE SPACES.        IH342     
007280         04  EXCEPT-17       PICTURE X       VALUE SPACES.        IH342     
007290         04  EXCEPT-18       PICTURE X       VALUE SPACES.        IH342     
007300         04  EXCEPT-19       PICTURE X       VALUE SPACES.        IH342     
007310         04  EXCEPT-20       PICTURE X       VALUE SPACES.        IH342     
007320         04  EXCEPT-21       PICTURE X       VALUE SPACES.        IH342     
007330         04  EXCEPT-22       PICTURE X       VALUE SPACES.        IH342     
007340         04  EXCEPT-23       PICTURE X       VALUE SPACES.        IH342     
007350         04  EXCEPT-24       PICTURE X       VALUE SPACES.        IH342     
007360     02  EXCEPT-CODE-TABLE REDEFINES EXCEPT-CODES.                IH342     
007370         04  EXCEPT-TABLE    PICTURE X       OCCURS 24 TIMES.     IH342     
007380     02  BEGIN-CHECK-NO      PICTURE 9(8)    VALUE 00000000.      IH342     
007390     02  BEGIN-CHECK-NO2     PICTURE 9(8)    VALUE 00000000.      IH342     
007400     02  BEGIN-CHECK-NO3     PICTURE 9(8)    VALUE 00000000.      IH342     
007410     02  BEGIN-CHECK-NO4     PICTURE 9(8)    VALUE 00000000.      IH342     
007420 01   TEST-EOD.                                                   IH342     
007430     02  TEST-EOD-YR         PICTURE 99      VALUE 00.            IH342     
007440     02  TEST-EOD-MODAY      PICTURE XXXX    VALUE "0000".        IH342     
007450 01   TEST-WE.                                                    IH342     
007460     02  TEST-WE-YR          PICTURE 99      VALUE 00.            IH342     
007470     02  TEST-WE-MODAY       PICTURE XXXX    VALUE "0000".        IH342     
007480 01   HOLD-CONTROLS.                                              IH342     
007490     02  MAJOR-CONTROL.                                           IH342     
007500         04  MINOR-ACTIVITY  PICTURE X       VALUE SPACE.         IH342     
007510         04  MINOR-AORD      PICTURE X       VALUE SPACE.         IH342     
007520     02  MINOR-CONTROL       PICTURE XX      VALUE SPACES.        IH342     
007530 01   SAVE-HOLD-CONTROLS.                                         IH342     
007540     02  SAVE-MINOR-ACT      PICTURE X       VALUE "0".           IH342     
007550     02  SAVE-GORU           PICTURE X       VALUE "0".           IH342     
007560     02  SAVE-MINOR-CONTROL  PICTURE XX      VALUE "00".          IH342     
007570 01   PAY-CODE-TABLE.                                             IH342     
007580     02  FILLER              PICTURE X(19)    VALUE               IH342     
007590         "STABWEPCGFHLMJKNRXY".                                   IH342     
007600 01   REDEF-PAY-TABLE REDEFINES PAY-CODE-TABLE.                   IH342     
007610     02  PAY-CODE-TBL        PICTURE X        OCCURS 19 TIMES.    IH342     
007620 01   SAVE-NITE-WEEK          PICTURE X       VALUE SPACE.        IH342     
007630 01   SAVE-BONUS-WEEK         PICTURE X       VALUE SPACE.        IH342     
007640 01   SAVE-SUNDAY-WEEK        PICTURE X       VALUE SPACE.        IH342     
007650 01   SAVE-DIVER-WEEK         PICTURE X       VALUE SPACE.        IH342     
007660 01   SAVE-OT-WEEK            PICTURE X       VALUE SPACE.        IH342     
007670 01   NUMBER-OF               PICTURE 99      VALUE 00.           IH342     
007680 01   ACTY-CHG                PICTURE X.                          IH342     
007690 01   WORK-OTH-PAY-CODE       PICTURE X       VALUE SPACE.        IH342     
007700 01   REC-OT-RATE2            PICTURE S9V99   VALUE ZEROS.        IH342     
007710 01   REC-OT-RATE1            PICTURE S9V99   VALUE ZEROS.        IH342     
007720 01   DTL-IN-GRP              PICTURE 9(5)    VALUE ZEROS.        IH342     
007730 01   WT-PAY-EXC-HOLD         PICTURE XX.                         IH342     
007740 01   MESSAGE2.                                                   IH342     
007750     02  FILLER              PICTURE X(11)   VALUE "GOVERNMENT ". IH342     
007760     02  MSG2-KON            PICTURE X(8).                        IH342     
007770     02  FILLER              PICTURE X(8)    VALUE " ADJ TO ".    IH342     
007780     02  MSG2-AMT            PICTURE Z,ZZZ.99-.                   IH342     
007790 01   MESSAGE3.                                                   IH342     
007800     02  FILLER              PICTURE X(24)   VALUE                IH342     
007810         "LWOP EXCEEDS REG HRS BY ".                              IH342     
007820     02  MSG3-HRS            PICTURE ZZZ.9-.                      IH342     
007830 01   MESSAGE4.                                                   IH342     
007840     02  FILLER              PICTURE X(20)   VALUE                IH342     
007850         "CURR EARNED EXCEEDS ".                                  IH342     
007860     02  MSG4-MAX            PICTURE Z,ZZZ.99.                    IH342     
007870     02  FILLER              PICTURE XXXX    VALUE " BY ".        IH342     
007880     02  MSG4-AMT            PICTURE Z,ZZZ.99-.                   IH342     
007890 01   MESSAGE5.                                                   IH342     
007900     02  FILLER              PICTURE X(19)   VALUE                IH342     
007910         "CURR EARNED ADJ TO ".                                   IH342     
007920     02  MSG5-AMT            PICTURE Z,ZZZ.99-.                   IH342     
007930 01   MESSAGE6.                                                   IH342     
007940     02  FILLER              PICTURE X(21)   VALUE                IH342     
007950         "SUBJECT MONEY ADJ TO ".                                 IH342     
007960     02  MSG6-AMT            PICTURE Z,ZZZ.99-.                   IH342     
007970 01   MESSAGE7.                                                   IH342     
007980     02  FILLER              PICTURE X(17)   VALUE                IH342     
007990         "DEDUCTION ADJ TO ".                                     IH342     
008000     02  MSG7-AMT            PICTURE Z,ZZZ.99-.                   IH342     
008010 01   MESSAGE8.                                                   IH342     
008020     02  FILLER              PICTURE X(5)    VALUE "CODE ".       IH342     
008030     02  MSG8-CODE           PICTURE 999.                         IH342     
008040     02  FILLER              PICTURE X(5)    VALUE " DED ".       IH342     
008050     02  MSG8-AMT1           PICTURE ZZ.99-.                      IH342     
008060     02  FILLER              PICTURE X(9)    VALUE " EMP CON ".   IH342     
008070     02  MSG8-AMT2           PICTURE ZZ.99-.                      IH342     
008080     02  FILLER              PICTURE X(9)    VALUE " CARRIER ".   IH342     
008090     02  MSG8-CARRIER        PICTURE X(7).                        IH342     
008100 01   MESSAGE9.                                                   IH342     
008110     02  FILLER              PICTURE X(25)   VALUE                IH342     
008120         "CURR PAY DOES NOT C/F BY ".                             IH342     
008130     02  MSG9-AMT            PICTURE Z,ZZZ.99-.                   IH342     
008140 01   MESSAGE10.                                                  IH342     
008150     02  FILLER              PICTURE X(35)   VALUE                IH342     
008160         "AMT PAID BY VOUCHER EXCEEDS NET BY ".                   IH342     
008170     02  MSG10-AMT           PICTURE Z,ZZZ.ZZ-.                   IH342     
008180 01   CODE10-MSG              PICTURE X(32)   VALUE               IH342     
008190           "CODE 10  SPECIAL TAX ADJUSTMENT".                     IH342     
008200 01   DUP-MSG.                                                    IH342     
008210     02  FILLER              PICTURE X(35)   VALUE                IH342     
008220         "DUPLICATE RECORDS, 1 RECORD DROPPED".                   IH342     
008230 01   EXC-MAJOR-CONTROL       PICTURE XX      VALUE SPACES.       IH342     
008240 01   EOJ-MSG.                                                    IH342     
008250     02  EOJ-MSG-DESC        PICTURE X(19)   VALUE SPACES.        IH342     
008260     02  FILLER              PICTURE X(10)   VALUE SPACES.        IH342     
008270     02  EOJ-MSG-TOT         PICTURE ZZ,ZZZ-.                     IH342     
008280 01   DET-CNTL-MSG.                                               IH342     
008290     02  FILLER              PICTURE X(40)   VALUE SPACES.        IH342     
008300     02  FILLER              PICTURE X(40)   VALUE                IH342     
008310          " TOTAL EXCEPTIONS PROCESSED THIS PREFIX ".             IH342     
008320     02  CNTL-MSG-DTL        PICTURE Z(5)    VALUE ZEROS.         IH342     
008330     02  FILLER              PICTURE X(35)   VALUE SPACES.        IH342     
008340 01   INTERNAL-PROGRAM-SWITCHES.                                  IH342     
008350     02  SW1                 PICTURE X       VALUE SPACE.         IH342     
008360         88  DTL-ERROR                       VALUE "1".           IH342     
008370     02  SW2                 PICTURE X       VALUE SPACE.         IH342     
008380         88  EOF-MASTER                      VALUE "1".           IH342     
008390     02  SW3                 PICTURE X       VALUE SPACE.         IH342     
008400         88  EOF-DETAIL                      VALUE "1".           IH342     
008410     02  SW4                 PICTURE X       VALUE SPACE.         IH342     
008420         88  THIRD-SHIFT                     VALUE "1".           IH342     
008430     02  SW5                 PICTURE X       VALUE SPACE.         IH342     
008440         88  PAY-EXCEPT                      VALUE "1".           IH342     
008450     02  SW6                 PICTURE X       VALUE SPACE.         IH342     
008460         88  EXCEPT-TO-COMPUTE               VALUE "1".           IH342     
008470     02  SW7                 PICTURE X       VALUE SPACE.         IH342     
008480         88  CODE-B-BONUS                    VALUE "1".           IH342     
008490     02  SW8                 PICTURE X       VALUE SPACE.         IH342     
008500         88  WHARF-BUILDER                   VALUE "1".           IH342     
008510     02  SW9                 PICTURE X       VALUE SPACE.         IH342     
008520         88  DIVER-PAY                       VALUE "1".           IH342     
008530     02  SW10                PICTURE X       VALUE SPACE.         IH342     
008540         88  OVERTIME                        VALUE "1".           IH342     
008550     02  SW11                PICTURE X       VALUE SPACE.         IH342     
008560         88  HOLIDAY-WORKED                  VALUE "1".           IH342     
008570     02  SW12                PICTURE X       VALUE SPACE.         IH342     
008580         88  HOLIDAY-PAYOFF                  VALUE "1".           IH342     
008590     02  SW13                PICTURE X       VALUE SPACE.         IH342     
008600         88  CANCEL-LIFE                     VALUE "1".           IH342     
008610     02  SW14                PICTURE X       VALUE SPACE.         IH342     
008620         88  CANCEL-HEALTH                   VALUE "1".           IH342     
008630     02  SW15                PICTURE X       VALUE SPACE.         IH342     
008640         88  TAX-LEVY                        VALUE "1".           IH342     
008650     02  SW16                PICTURE X       VALUE SPACE.         IH342     
008660         88  RETRO-HOURS                     VALUE "1".           IH342     
008670     02  SW17                PICTURE X       VALUE SPACE.         IH342     
008680         88  80TH-ADDED-PAY                  VALUE "1".           IH342     
008690     02  SW18                PICTURE X       VALUE SPACE.         IH342     
008700         88  PAID-BY-VOUCHER                 VALUE "1".           IH342     
008710     02  SW19                PICTURE X       VALUE SPACE.         IH342     
008720         88  CSRA-ADJUSTMENT                 VALUE "1".           IH342     
008730     02  SW20                PICTURE X       VALUE SPACE.         IH342     
008740         88  FICA-ADJUSTMENT                 VALUE "1".           IH342     
008750     02  SW21                PICTURE X       VALUE SPACE.         IH342     
008760         88  TAX-ADJUSTMENT                  VALUE "1".           IH342     
008770     02  SW22                PICTURE X       VALUE SPACE.         IH342     
008780         88  HLTH-DED-ADJUSTMENT             VALUE "1".           IH342     
008790     02  SW23                PICTURE X       VALUE SPACE.         IH342     
008800         88  LIFE-INS-ADJUSTMENT             VALUE "1".           IH342     
008810     02  SW24                PICTURE X       VALUE SPACE.         IH342     
008820         88  LIFE-OPT-ADJUSTMENT             VALUE "1".           IH342     
008830     02  SW25                PICTURE X       VALUE SPACE.         IH342     
008840         88  STATE-ADJUSTMENT                VALUE "1".           IH342     
008850     02  SW26                PICTURE X       VALUE SPACE.         IH342     
008860         88  UNION-ADJUSTMENT                VALUE "1".           IH342     
008870     02  SW27                PICTURE X       VALUE SPACE.         IH342     
008880         88  CANCEL-BOND                     VALUE "1".           IH342     
008890     02  SW28                PICTURE X       VALUE SPACE.         IH342     
008900         88  DECEASED-PAYOFF                 VALUE "1".           IH342     
008910     02  SW29                PICTURE X       VALUE SPACE.         IH342     
008920         88  ANNUAL-PAYOFF                   VALUE "1".           IH342     
008930     02  SW30                PICTURE X       VALUE SPACE.         IH342     
008940         88  CASH-AWARD                      VALUE "1".           IH342     
008950     02  SW31                PICTURE X       VALUE SPACE.         IH342     
008960         88  NOT-DTL-ERROR                   VALUE "1".           IH342     
008970     02  SW32                PICTURE X       VALUE SPACE.         IH342     
008980         88  SUNDAY-PREMIUM                  VALUE "1".           IH342     
008990     02  SW33                PICTURE X       VALUE SPACE.         IH342     
009000         88  80TH-HRLY-SAVED                 VALUE "1".           IH342     
009010     02  SW34                PICTURE X       VALUE SPACE.         IH342     
009020         88  HOLIDAY-OTH-MONEY               VALUE "1".           IH342     
009030     02  SW35                PICTURE X       VALUE SPACE.         IH342     
009040         88  FICA-MAXIMUM                    VALUE "1".           IH342     
009050     02  SW36                PICTURE X       VALUE SPACE.         IH342     
009060         88  WRITE-CASH-AWARD                VALUE "1".           IH342     
009070     02  SW37                PICTURE X       VALUE SPACE.         IH342     
009080         88  WRITE-TAX-LEVY                  VALUE "1".           IH342     
009090     02  SW38                PICTURE X       VALUE SPACE.         IH342     
009100         88  END-OF-JOB                      VALUE "1".           IH342     
009110     02  SW40                PICTURE X       VALUE SPACE.         IH342     
009120         88  CHARITY-ADJ                     VALUE "1".           IH342     
009130     02  SW41                PICTURE X       VALUE SPACE.         IH342     
009140         88  BOND-ADJUST                     VALUE "1".           IH342     
009150     02  SW42                PICTURE X       VALUE SPACE.         IH342     
009160         88  RETRO-OVERTIME                  VALUE "1".           IH342     
009170     02  SW43                PICTURE X       VALUE SPACE.         IH342     
009180         88  ALLOT1-ADJ                      VALUE "1".           IH342     
009190     02  SW44                PICTURE X       VALUE SPACE.         IH342     
009200         88  ALLOT2-ADJ                      VALUE "1".           IH342     
009210     02  SW45                PICTURE X       VALUE SPACE.         IH342     
009220         88  ADJ-OTH-DED                     VALUE "1".           IH342     
009230     02  SW46                PICTURE X       VALUE SPACE.         IH342     
009240         88  ADJ-TO-NET-PAY                  VALUE "1".           IH342     
009250     02  SW47                PICTURE X       VALUE SPACE.         IH342     
009260         88  ADJ-NSFT                        VALUE "1".           IH342     
009270     02  SW48                PICTURE X       VALUE SPACE.         IH342     
009280         88  RETRO-OTHER                     VALUE "1".           IH342     
009290     02  SW49                PICTURE X       VALUE SPACE.         IH342     
009300         88  PAYOFF-LWOP                     VALUE "1".           IH342     
009310     02  SW50                PICTURE X       VALUE SPACE.         IH342     
009320         88  PAYOFF-SW                       VALUE "1".           IH342     
009330     02  SW51                PICTURE X       VALUE SPACE.         IH342     
009340         88  SHIFT-2-OT                      VALUE "1".           IH342     
009350     02  SW52                PICTURE X       VALUE SPACE.         IH342     
009360         88  SHIFT-3-OT                      VALUE "1".           IH342     
009370     02  SW53                PICTURE X       VALUE SPACE.         IH342     
009380         88  CODE-K-BONUS                    VALUE "1".           IH342     
009390     02  SW54                PICTURE X       VALUE SPACE.         IH342     
009400         88  CODE-N-BONUS                    VALUE "1".           IH342     
009410     02  SW55                PICTURE X       VALUE SPACE.         IH342     
009420         88  CODE-R-BONUS                    VALUE "1".           IH342     
009430     02  SW56                PICTURE X       VALUE SPACE.         IH342     
009440         88  CODE-B-OT                       VALUE "1".           IH342     
009450     02  SW57                PICTURE X       VALUE SPACE.         IH342     
009460         88  CD10-TAX-ADJ                    VALUE "1".           IH342     
009470 01   CONTROL-DESCRIPTION.                                        IH342     
009480     02  FILLER              PICTURE X(20)                        IH342     
009490         VALUE "CHECK COUNT IS".                                  IH342     
009500     02  FILLER              PICTURE X(20)                        IH342     
009510         VALUE "SUB TO FICA IS".                                  IH342     
009520     02  FILLER              PICTURE X(20)                        IH342     
009530         VALUE "SUB TO CSRA IS".                                  IH342     
009540     02  FILLER              PICTURE X(20)                        IH342     
009550         VALUE "HEALTH COUNT IS".                                 IH342     
009560     02  FILLER              PICTURE X(20)                        IH342     
009570         VALUE "EMPLOYEE DED IS".                                 IH342     
009580     02  FILLER              PICTURE X(20)                        IH342     
009590         VALUE "EMPLOYER CON IS".                                 IH342     
009600     02  FILLER              PICTURE X(20)                        IH342     
009610         VALUE "BASIC HOURS ARE".                                 IH342     
009620     02  FILLER              PICTURE X(20)                        IH342     
009630         VALUE "OVERTIME HOURS ARE".                              IH342     
009640     02  FILLER  PICTURE X(20) VALUE "BOND DED COUNT IS ".        IH342     
009650     02  FILLER  PICTURE X(20) VALUE "BOND DED MONEY IS".         IH342     
009660 01   REDEF-CNTRL-DESC REDEFINES CONTROL-DESCRIPTION.             IH342     
009670     02  CNTRL-DESC          PICTURE X(20) OCCURS 10 TIMES.       IH342     
009680 01   PAYROLL-EXC-CODE-DESC.                                      IH342     
009690     02  FILLER              PICTURE XX      VALUE "01".          IH342     
009700     02  FILLER              PICTURE X(32)   VALUE                IH342     
009710             "NIGHT DIFFERENTIAL SECOND SHIFT".                   IH342     
009720     02  FILLER              PICTURE XX      VALUE "02".          IH342     
009730     02  FILLER              PICTURE X(32)   VALUE                IH342     
009740             "NIGHT DIFFERENTIAL THIRD SHIFT".                    IH342     
009750     02  FILLER              PICTURE XX      VALUE "03".          IH342     
009760     02  FILLER              PICTURE X(32)   VALUE                IH342     
009770         " CODE A DIFFERENTIAL".                                  IH342     
009780     02  FILLER              PICTURE XX      VALUE "04".          IH342     
009790     02  FILLER              PICTURE X(32)   VALUE                IH342     
009800         " CODE B DIFFERENTIAL".                                  IH342     
009810     02  FILLER              PICTURE XX      VALUE "05".          IH342     
009820     02  FILLER              PICTURE X(32)   VALUE                IH342     
009830             "DIVER RATE HOURS".                                  IH342     
009840     02  FILLER              PICTURE XX      VALUE "06".          IH342     
009850     02  FILLER              PICTURE X(32)   VALUE                IH342     
009860             "HOLIDAY WORKED HOURS".                              IH342     
009870     02  FILLER              PICTURE XX      VALUE "07".          IH342     
009880     02  FILLER              PICTURE X(32)   VALUE                IH342     
009890             "ADJUSTMENT TO SUBJECT MONEY".                       IH342     
009900     02  FILLER              PICTURE XX      VALUE "08".          IH342     
009910     02  FILLER              PICTURE X(32)   VALUE                IH342     
009920             "ADJUSTMENT TO REGULAR MONEY".                       IH342     
009930     02  FILLER              PICTURE XX      VALUE "09".          IH342     
009940     02  FILLER              PICTURE X(32)   VALUE                IH342     
009950             "ADJUSTMENT TO OTHER MONEY".                         IH342     
009960     02  FILLER              PICTURE XX      VALUE "10".          IH342     
009970     02  FILLER              PICTURE X(32)   VALUE                IH342     
009980             "ADJUSTMENT TO OVERTIME MONEY".                      IH342     
009990     02  FILLER              PICTURE XX      VALUE "11".          IH342     
010000     02  FILLER              PICTURE X(32)   VALUE                IH342     
010010             "ADJUSTMENT TO CSRA".                                IH342     
010020     02  FILLER              PICTURE XX      VALUE "12".          IH342     
010030     02  FILLER              PICTURE X(32)   VALUE                IH342     
010040             "ADJUSTMENT TO FICA".                                IH342     
010050     02  FILLER              PICTURE XX      VALUE "13".          IH342     
010060     02  FILLER              PICTURE X(32)   VALUE                IH342     
010070             "ADJUSTMENT TO HEALTH INSURANCE".                    IH342     
010080     02  FILLER              PICTURE XX      VALUE "14".          IH342     
010090     02  FILLER              PICTURE X(32)   VALUE                IH342     
010100             "ADJUSTMENT TO FEDERAL TAXES".                       IH342     
010110     02  FILLER              PICTURE XX      VALUE "15".          IH342     
010120     02  FILLER              PICTURE X(32)   VALUE                IH342     
010130             "ADJUSTMENT TO LIFE INSURANCE".                      IH342     
010140     02  FILLER              PICTURE XX      VALUE "16".          IH342     
010150     02  FILLER              PICTURE X(32)   VALUE                IH342     
010160             "ADJUSTMENT TO STATE TAXES".                         IH342     
010170     02  FILLER              PICTURE XX      VALUE "17".          IH342     
010180     02  FILLER              PICTURE X(32)   VALUE                IH342     
010190             "ADJUSTMENT TO UNION DUES".                          IH342     
010200     02  FILLER              PICTURE XX      VALUE "18".          IH342     
010210     02  FILLER              PICTURE X(32)   VALUE                IH342     
010220             "ANNUAL LEAVE PAYOFF HOURS".                         IH342     
010230     02  FILLER              PICTURE XX      VALUE "19".          IH342     
010240     02  FILLER              PICTURE X(32)   VALUE                IH342     
010250             "DECEASED PAYOFF HOURS".                             IH342     
010260     02  FILLER              PICTURE XX      VALUE "20".          IH342     
010270     02  FILLER              PICTURE X(32)   VALUE                IH342     
010280             "RETROACTIVE REGULAR HOURS".                         IH342     
010290     02  FILLER              PICTURE XX      VALUE "21".          IH342     
010300     02  FILLER              PICTURE X(32)   VALUE                IH342     
010310             "HOLIDAY INCLUDED IN PAYOFF HOURS".                  IH342     
010320     02  FILLER              PICTURE XX      VALUE "22".          IH342     
010330     02  FILLER              PICTURE X(32)   VALUE                IH342     
010340             "TAX LEVY AMOUNT".                                   IH342     
010350     02  FILLER              PICTURE XX      VALUE "23".          IH342     
010360     02  FILLER              PICTURE X(32)   VALUE                IH342     
010370             "CASH AWARD".                                        IH342     
010380     02  FILLER              PICTURE XX      VALUE "24".          IH342     
010390     02  FILLER              PICTURE X(32)   VALUE                IH342     
010400             "CASH AWARD".                                        IH342     
010410     02  FILLER              PICTURE XX      VALUE "25".          IH342     
010420     02  FILLER              PICTURE X(32)   VALUE                IH342     
010430             "CASH AWARD".                                        IH342     
010440     02  FILLER              PICTURE XX      VALUE "26".          IH342     
010450     02  FILLER              PICTURE X(32)   VALUE                IH342     
010460             "CASH AWARD".                                        IH342     
010470     02  FILLER              PICTURE XX      VALUE "27".          IH342     
010480     02  FILLER              PICTURE X(32)   VALUE                IH342     
010490             "SUNDAY REGULAR SCHEDULED HOURS".                    IH342     
010500     02  FILLER              PICTURE XX      VALUE "28".          IH342     
010510     02  FILLER              PICTURE X(32)   VALUE                IH342     
010520             "WHARF BUILDER HOURS".                               IH342     
010530     02  FILLER              PICTURE XX      VALUE "29".          IH342     
010540     02  FILLER              PICTURE X(32)   VALUE                IH342     
010550             "CHARITABLE CONTRIBUTIONS".                          IH342     
010560     02  FILLER              PICTURE XX      VALUE "30".          IH342     
010570     02  FILLER              PICTURE X(32)   VALUE                IH342     
010580           "BONDS".                                               IH342     
010590     02  FILLER              PICTURE XX      VALUE "31".          IH342     
010600     02  FILLER              PICTURE X(32)   VALUE                IH342     
010610           "RETROACTIVE OVERTIME MONEY".                          IH342     
010620     02  FILLER              PICTURE XX      VALUE "32".          IH342     
010630     02  FILLER              PICTURE X(32)   VALUE                IH342     
010640           "SAVINGS ALLOTMENT #1".                                IH342     
010650     02  FILLER              PICTURE XX      VALUE "33".          IH342     
010660     02  FILLER              PICTURE X(32)   VALUE                IH342     
010670           "SAVINGS ALLOTMENT #2".                                IH342     
010680     02  FILLER              PICTURE XX      VALUE "34".          IH342     
010690     02  FILLER              PICTURE X(32)   VALUE                IH342     
010700             "OTHER DEDUCTIONS".                                  IH342     
010710     02  FILLER              PICTURE XX      VALUE "35".          IH342     
010720     02  FILLER              PICTURE X(32)   VALUE                IH342     
010730             "NET-PAY".                                           IH342     
010740     02  FILLER              PICTURE XX      VALUE "36".          IH342     
010750     02  FILLER              PICTURE X(32)   VALUE                IH342     
010760             "AMT NOT SUB TO FED TAX".                            IH342     
010770     02  FILLER              PICTURE XX      VALUE "37".          IH342     
010780     02  FILLER              PICTURE X(32)   VALUE                IH342     
010790           "RETROACTIVE OTHER MONEY".                             IH342     
010800     02  FILLER              PICTURE XX      VALUE "38".          IH342     
010810     02  FILLER              PICTURE X(32)   VALUE                IH342     
010820         " SHIFT-2 OVERTIME".                                     IH342     
010830     02  FILLER              PICTURE XX      VALUE "39".          IH342     
010840     02  FILLER              PICTURE X(32)   VALUE                IH342     
010850         " SHIFT-3 OVERTIME".                                     IH342     
010860     02  FILLER              PICTURE XX      VALUE "40".          IH342     
010870     02  FILLER              PICTURE X(32)   VALUE                IH342     
010880           "ANNUAL LV PAYOFF WITH EARNINGS".                      IH342     
010890     02  FILLER              PICTURE XX      VALUE "41".          IH342     
010900     02  FILLER              PICTURE X(32)   VALUE                IH342     
010910           "ANNUAL LV PAYOFF NO EARNINGS".                        IH342     
010920     02  FILLER              PICTURE XX      VALUE "42".          IH342     
010930     02  FILLER              PICTURE X(32)   VALUE                IH342     
010940           "CODE J ADDED HOURS".                                  IH342     
010950     02  FILLER              PICTURE XX      VALUE "43".          IH342     
010960     02  FILLER              PICTURE X(32)   VALUE                IH342     
010970           "CODE K ADDED HOURS".                                  IH342     
010980     02  FILLER              PICTURE XX      VALUE "44".          IH342     
010990     02  FILLER              PICTURE X(32)   VALUE                IH342     
011000           "CODE N ADDED HOURS".                                  IH342     
011010     02  FILLER              PICTURE XX      VALUE "45".          IH342     
011020     02  FILLER              PICTURE X(32)   VALUE                IH342     
011030           "CODE R ADDED HOURS".                                  IH342     
011040     02  FILLER              PICTURE XX      VALUE "46".          IH342     
011050     02  FILLER              PICTURE X(32)   VALUE                IH342     
011060           "CODE A OVERTIME".                                     IH342     
011070     02  FILLER              PICTURE XX      VALUE "47".          IH342     
011080     02  FILLER              PICTURE X(32)   VALUE                IH342     
011090           "CODE B OVERTIME".                                     IH342     
011100 01   REDEF-PAY-EXC-DESC REDEFINES PAYROLL-EXC-CODE-DESC.         IH342     
011110     02  CODE-AND-DESC                       OCCURS 47 TIMES.     IH342     
011120         04  PAY-EXC-CODE    PICTURE XX.                          IH342     
011130         04  PAY-EXC-DESC    PICTURE X(32).                       IH342     
011140 01   PAYROLL-EOJ-DESC.                                           IH342     
011150     02  FILLER              PICTURE X(19)   VALUE                IH342     
011160             "MASTERS IN".                                        IH342     
011170     02  FILLER              PICTURE X(19)   VALUE                IH342     
011180             "DETAIL IN".                                         IH342     
011190     02  FILLER              PICTURE X(19)   VALUE                IH342     
011200             "MASTERS OUT".                                       IH342     
011210     02  FILLER              PICTURE X(19)   VALUE                IH342     
011220             "ACTIVE MASTERS OUT".                                IH342     
011230     02  FILLER              PICTURE X(19)   VALUE                IH342     
011240             "SMOOTH ROLL OUT".                                   IH342     
011250     02  FILLER              PICTURE X(19)   VALUE                IH342     
011260             "WORKTAPE OUT".                                      IH342     
011270     02  FILLER              PICTURE X(19)   VALUE                IH342     
011280             "EXCEPTION LIST OUT".                                IH342     
011290     02  FILLER              PICTURE X(19)       VALUE            IH342     
011300             "ERROR DETAILS OUT  ".                               IH342     
011310     02  FILLER              PICTURE X(19)   VALUE                IH342     
011320             "CONTROL RECORDS OUT".                               IH342     
011330 01   REDEF-EOJ-DESC REDEFINES PAYROLL-EOJ-DESC.                  IH342     
011340     02  EOJ-DESC            PICTURE X(19)   OCCURS 10 TIMES.     IH342     
011350 01  PAYROLL-WORK-AREAS.                                          IH342     
011360     02  OT-HRS-WK1          PICTURE S999V99  VALUE +000.00.      IH342     
011370     02  OT-HRS-WK2          PICTURE S999V99  VALUE +000.00.      IH342     
011380     02  LWOP-WK1            PICTURE S999V99  VALUE +000.00.      IH342     
011390     02  LWOP-WK2            PICTURE S999V99  VALUE +000.00.      IH342     
011400     02  REG-HRS-WK1         PICTURE S999V99  VALUE +000.00.      IH342     
011410     02  REG-HRS-WK2         PICTURE S999V99  VALUE +000.00.      IH342     
011420     02  WK1-SUBJECT         PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011430     02  WK2-SUBJECT         PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011440     02  SUNDAY-PREM-WK1     PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011450     02  SUNDAY-PREM-WK2     PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011460     02  CURR-EARNED         PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011470     02  ADJ-RETRO-HRS       PICTURE S999V99  VALUE +000.00.      IH342     
011480     02  NET-PAY             PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011490     02  REG-MONEY           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011500     02  OT-MONEY            PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011510     02  OTHER-HOURS         PICTURE S999V99  VALUE +000.00.      IH342     
011520     02  OTHER-MONEY         PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011530     02  RETIRE-FICA         PICTURE S999V99  VALUE +000.00.      IH342     
011540     02  FED-TAXES           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011550     02  STATE-TAXES         PICTURE S9999V99 VALUE +0000.00.     IH342     
011560     02  HLTH-INS-DED        PICTURE S999V99  VALUE +000.00.      IH342     
011570     02  HLTH-INS-CON        PICTURE S999V99  VALUE +000.00.      IH342     
011580     02  LIFE-INS-DED        PICTURE S999V99  VALUE +000.00.      IH342     
011590     02  LIFE-OPT-DED        PICTURE S999V99  VALUE +000.00.      IH342     
011600     02  OTHER-DED           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011610     02  UNION-DUES          PICTURE S999V99  VALUE +000.00.      IH342     
011620     02  CHARITY-CON         PICTURE S999V99  VALUE +000.00.      IH342     
011630     02  BOND-DED            PICTURE S999V99  VALUE +000.00.      IH342     
011640     02  ALLOT-DED           PICTURE S9(5)    VALUE +00000.       IH342     
011650     02  DIVER-OT-HR1        PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011660     02  DIVER-OT-HR2        PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011670     02  ADJ-RETIRE          PICTURE S999V99  VALUE +000.00.      IH342     
011680     02  ADJ-TAXES           PICTURE S999V99  VALUE +000.00.      IH342     
011690     02  ADJ-HEALTH-DED      PICTURE S999V99  VALUE +000.00.      IH342     
011700     02  ADJ-HEALTH-CON      PICTURE S999V99  VALUE +000.00.      IH342     
011710     02  ADJ-LIFE-INS        PICTURE S999V99  VALUE +000.00.      IH342     
011720     02  ADJ-LIFE-OPT        PICTURE S999V99  VALUE +000.00.      IH342     
011730     02  ADJ-STATE           PICTURE S999V99  VALUE +000.00.      IH342     
011740     02  ADJ-UNION           PICTURE S999V99  VALUE +000.00.      IH342     
011750     02  ADJ-CHARITY         PICTURE S999V99  VALUE 000.00.       IH342     
011760     02  ADJ-TO-BONDS        PICTURE S9999V99 VALUE 0000.00.      IH342     
011770     02  ADJ-TO-ALLOT1       PICTURE S999     VALUE 000.          IH342     
011780     02  ADJ-TO-ALLOT2       PICTURE S999     VALUE 000.          IH342     
011790     02  ADJ-TO-OTH-DED      PICTURE S9999V99 VALUE 0000.00.      IH342     
011800     02  NET-PAY-ADJ         PICTURE S9999V99 VALUE 0000.00.      IH342     
011810     02  NOT-SUBJ-FED-TAX    PICTURE S9999V99 VALUE +0000.00.     IH342     
011820     02  ALLOT1-WORK         PICTURE S999     VALUE 000.          IH342     
011830     02  ALLOT2-WORK         PICTURE S999     VALUE 000.          IH342     
011840     02  CODE-A-PAY          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011850     02  CODE-B-PAY          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011860     02  CAT-A-MONEY         PICTURE S9(6)V99 VALUE 000000.00.    IH342     
011870     02  CAT-A-HOURS         PICTURE S9(6)V99 VALUE 000000.00.    IH342     
011880     02  CAT-B-MONEY         PICTURE S9(5)V99 VALUE 00000.00.     IH342     
011890     02  CAT-B-HOURS         PICTURE S9(5)V99 VALUE 00000.00.     IH342     
011900     02  CAT-C-MONEY         PICTURE S9(4)V99 VALUE 0000.00.      IH342     
011910     02  CAT-C-HOURS         PICTURE S9(5)V99 VALUE 00000.00.     IH342     
011920     02  CAT-H-MONEY         PICTURE S9(5)V99 VALUE 00000.00.     IH342     
011930     02  CAT-J-MONEY         PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011940     02  CAT-J-HOURS         PICTURE S9(5)V99 VALUE +00000.00.    IH342     
011950     02  CAT-K-MONEY         PICTURE S9(5)V99 VALUE 00000.00.     IH342     
011960     02  CAT-L-MONEY         PICTURE S9(5)V99 VALUE 00000.00.     IH342     
011970     02  CAT-M-MONEY         PICTURE S9(4)V99 VALUE   0000.00.    IH342     
011980     02  CAT-M-HOURS         PICTURE S999V999 VALUE +000.00.      IH342     
011990     02  CAT-N-MONEY         PICTURE S9(4)V99 VALUE +0000.00.     IH342     
012000     02  CAT-N-HOURS         PICTURE S99V99   VALUE +00.00.       IH342     
012010     02  CAT-WORK            PICTURE S9(6)V99 VALUE 000000.00.    IH342     
012020     02  CAT-MONEY           PICTURE S9(6)V99 VALUE 000000.00.    IH342     
012030     02  CAT-C-WORK          PICTURE S9(6)V99 VALUE 000000.00.    IH342     
012040     02  HOLD-RETRO-OTPAY    PICTURE S9(5)V99 VALUE 00000.00.     IH342     
012050     02  HOLD-RETRO-OTHRS    PICTURE S9(3)V99 VALUE 000.00.       IH342     
012060     02  VOUCHER-PAID        PICTURE S9(5)V99 VALUE 00000.00.     IH342     
012070 01  ARITHMETIC-WORK-AREAS.                                       IH342     
012080     02 MINUS-DIVER-RATE      PICTURE S9V99    VALUE ZEROS.       IH342     
012090     02  ANNUM-OT-MAX        PICTURE S9V99    VALUE +8.76.        IH342     
012100     02  FF-MAX-RATE         PICTURE S9(9)V99 VALUE +12151.00.    IH342     
012110     02  ANNUM-MAX-PAY       PICTURE S9(5)V99 VALUE +01279.20.    IH342     
012120     02  HRLY-RATE-WORK      PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012130     02  SAVE-NITE-PAY       PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012140     02  DIEM-NITE-PAY       PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012150     02  DIEM-BONUS-PAY      PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012160     02  TAX-LEVY-DED        PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012170     02  ADJ-WK-SUB          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012180     02  ADJ-REG-HRS         PICTURE S999V99  VALUE +000.00.      IH342     
012190     02  ADJ-REG-MONEY       PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012200     02  ADJ-OT-HOURS        PICTURE S999V99  VALUE +000.00.      IH342     
012210     02  ADJ-OT-MONEY        PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012220     02  ADJ-OTHER-MONEY     PICTURE S9(5)V99 VALUE +00000.00.    IH342     
012230     02  80TH-INSUR          PICTURE S999V99  VALUE +000.00.      IH342     
012240     02  FF-PREM-WORK        PICTURE S999V99  VALUE +000.00.      IH342     
012250     02  FF-BONUS-PERCENT    PICTURE SV999    VALUE +.000.        IH342     
012260     02  FF-BONUS-HOURS      PICTURE S99V9    VALUE +00.0.        IH342     
012270     02  TOT-SUB-RETIRE      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
012280     02  SR-LINE-COUNT       PICTURE S999     VALUE +000.         IH342     
012290     02  EX-LINE-COUNT       PICTURE S999     VALUE +054.         IH342     
012300     02  SR-PAGE-COUNT       PICTURE S99999      VALUE 00000.     IH342     
012310     02  CHECK-COUNT         PICTURE S9       VALUE +1.           IH342     
012320     02  ROUND-WORK-AREA     PICTURE S9(6)V999 VALUE +000000.000. IH342     
012330     02  SAVE-PAYOFF         PICTURE S9(9)V99 VALUE +000000000.00.IH342     
012340     02  ST-CURR-EARNED      PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012350     02  YRLY-GROSS          PICTURE S9(6)V99 VALUE ZEROS.        IH342     
012360     02  YRLY-ADJ-GROSS      PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012370     02 YRLY-FED-TAXES       PICTURE S9(6)V99 VALUE ZEROS.        IH342     
012380     02 YRLY-NET             PICTURE S9(6)V99 VALUE ZEROS.        IH342     
012390     02 HALF-FED             PICTURE S9(5)V99 VALUE ZEROS.        IH342     
012400     02 ALLOWANCE            PICTURE S9(4)V99 VALUE ZEROS.        IH342     
012410     02 ALLI REDEFINES ALLOWANCE.                                 IH342     
012420        04 DEP-ALLOW         PICTURE S99.                         IH342     
012430        04 BAL-ALLOW         PICTURE 99V99.                       IH342     
012440     02  TAXABLE         PICTURE S9(7)V99 VALUE ZERO.             IH342     
012450     02  STATAX-1            PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012460     02  STATAX-2            PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012470     02  STATAX-3            PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012480     02  STATAX-4            PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012490     02  STATAX-5            PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012500     02  STATAX-6            PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012510     02  YEARLY-STATAX       PICTURE S9(7)V99 VALUE ZEROS.        IH342     
012520     02 NY-CTR               PICTURE S99      VALUE ZEROS.        IH342     
012530     02 TAX-PC               PICTURE SV9999.                      IH342     
012540 01   NC-STATE-TAX-TABLE.                                         IH342     
012550     02 FILLER               PICTURE X(6)     VALUE "003359".     IH342     
012560     02 FILLER               PICTURE X(6)     VALUE "103090".     IH342     
012570     02 FILLER               PICTURE X(6)     VALUE "162928".     IH342     
012580     02 FILLER               PICTURE X(6)     VALUE "202821".     IH342     
012590     02 FILLER               PICTURE X(6)     VALUE "222767".     IH342     
012600     02 FILLER               PICTURE X(6)     VALUE "262659".     IH342     
012610     02 FILLER               PICTURE X(6)     VALUE "282605".     IH342     
012620     02 FILLER               PICTURE X(6)     VALUE "302552".     IH342     
012630     02 FILLER               PICTURE X(6)     VALUE "322498".     IH342     
012640     02 FILLER               PICTURE X(6)     VALUE "342444".     IH342     
012650     02 FILLER               PICTURE X(6)     VALUE "362390".     IH342     
012660     02 FILLER               PICTURE X(6)     VALUE "382336".     IH342     
012670     02 FILLER               PICTURE X(6)     VALUE "402282".     IH342     
012680     02 FILLER               PICTURE X(6)     VALUE "422228".     IH342     
012690     02 FILLER               PICTURE X(6)     VALUE "442175".     IH342     
012700     02 FILLER               PICTURE X(6)     VALUE "462121".     IH342     
012710     02 FILLER               PICTURE X(6)     VALUE "482021".     IH342     
012720     02 FILLER               PICTURE X(6)     VALUE "502013".     IH342     
012730     02 FILLER               PICTURE X(6)     VALUE "521959".     IH342     
012740     02 FILLER               PICTURE X(6)     VALUE "541905".     IH342     
012750     02 FILLER               PICTURE X(6)     VALUE "561852".     IH342     
012760     02 FILLER               PICTURE X(6)     VALUE "581805".     IH342     
012770     02 FILLER               PICTURE X(6)     VALUE "601758".     IH342     
012780     02 FILLER               PICTURE X(6)     VALUE "621712".     IH342     
012790     02 FILLER               PICTURE X(6)     VALUE "641666".     IH342     
012800     02 FILLER               PICTURE X(6)     VALUE "661620".     IH342     
012810     02 FILLER               PICTURE X(6)     VALUE "681574".     IH342     
012820     02 FILLER               PICTURE X(6)     VALUE "701528".     IH342     
012830     02 FILLER               PICTURE X(6)     VALUE "721482".     IH342     
012840     02 FILLER               PICTURE X(6)     VALUE "741435".     IH342     
012850     02 FILLER               PICTURE X(6)     VALUE "761389".     IH342     
012860     02 FILLER               PICTURE X(6)     VALUE "781343".     IH342     
012870     02 FILLER               PICTURE X(6)     VALUE "801297".     IH342     
012880     02 FILLER               PICTURE X(6)     VALUE "821251".     IH342     
012890 01   NC-STABLE REDEFINES NC-STATE-TAX-TABLE.                     IH342     
012900     02 EXEMP-RATES                           OCCURS 34 TIMES.    IH342     
012910        03 EX-EM             PICTURE 99.                          IH342     
012920        03 EX-AMT            PICTURE 99V99.                       IH342     
012930 01   INCR-TAX.                                                   IH342     
012940     02 TAXER                PICTURE 99.                          IH342     
012950 01   NY-TAX-TABLE.                                               IH342     
012960     02  FILLER              PICTURE X(13)  VALUE "0000000002050".IH342     
012970     02  FILLER              PICTURE X(13)  VALUE "0038007803075".IH342     
012980     02  FILLER              PICTURE X(13)  VALUE "0115031404100".IH342     
012990     02  FILLER              PICTURE X(13)  VALUE "0192063005125".IH342     
013000     02  FILLER              PICTURE X(13)  VALUE "0269102406150".IH342     
013010     02  FILLER              PICTURE X(13)  VALUE "0346149707175".IH342     
013020     02  FILLER              PICTURE X(13)  VALUE "0423204908200".IH342     
013030     02  FILLER              PICTURE X(13)  VALUE "0500268109225".IH342     
013040     02  FILLER              PICTURE X(13)  VALUE "0577339110250".IH342     
013050     02  FILLER              PICTURE X(13)  VALUE "0654418111275".IH342     
013060     02  FILLER              PICTURE X(13)  VALUE "0731504912300".IH342     
013070     02  FILLER              PICTURE X(13)  VALUE "0808599613325".IH342     
013080     02  FILLER              PICTURE X(13)  VALUE "0885702314350".IH342     
013090     02  FILLER              PICTURE X(13)  VALUE "0962812815375".IH342     
013100 01   TABLE-NY REDEFINES NY-TAX-TABLE.                            IH342     
013110     02  NY-TAXES                           OCCURS 14 TIMES.      IH342     
013120        03 NY-WAGE           PICTURE S9(4).                       IH342     
013130        03 NY-ST-TAX         PICTURE S99V99.                      IH342     
013140        03 NY-PCAMT          PICTURE SV99999.                     IH342     
013150 01   CALIF-TAX-TABLES.                                           IH342     
013160     02  CALIF-SINGLE-MAR-WAGES.                                  IH342     
013170         04  FILLER          PICTURE S999V99 VALUE +077.00.       IH342     
013180         04  FILLER          PICTURE S999V99 VALUE +058.00.       IH342     
013190         04  FILLER          PICTURE S999V99 VALUE +057.00.       IH342     
013200         04  FILLER          PICTURE S999V99 VALUE +058.00.       IH342     
013210         04  FILLER          PICTURE S999V99 VALUE +058.00.       IH342     
013220         04  FILLER          PICTURE S999V99 VALUE +057.00.       IH342     
013230         04  FILLER          PICTURE S999V99 VALUE +058.00.       IH342     
013240         04  FILLER          PICTURE S999V99 VALUE +058.00.       IH342     
013250         04  FILLER          PICTURE S999V99 VALUE +057.00.       IH342     
013260         04  FILLER          PICTURE S999V99 VALUE +154.00.       IH342     
013270         04  FILLER          PICTURE S999V99 VALUE +115.00.       IH342     
013280         04  FILLER          PICTURE S999V99 VALUE +116.00.       IH342     
013290         04  FILLER          PICTURE S999V99 VALUE +115.00.       IH342     
013300         04  FILLER          PICTURE S999V99 VALUE +115.00.       IH342     
013310         04  FILLER          PICTURE S999V99 VALUE +116.00.       IH342     
013320         04  FILLER          PICTURE S999V99 VALUE +115.00.       IH342     
013330         04  FILLER          PICTURE S999V99 VALUE +116.00.       IH342     
013340         04  FILLER          PICTURE S999V99 VALUE +115.00.       IH342     
013350     02  REDEF-CALIF-WAGES REDEFINES CALIF-SINGLE-MAR-WAGES.      IH342     
013360         04  CALIF-WAGES     PICTURE S999V99 OCCURS 18 TIMES.     IH342     
013370     02  CALIF-SINGLE-MAR-TAX.                                    IH342     
013380         04  FILLER          PICTURE S999V99 VALUE +000.00.       IH342     
013390         04  FILLER          PICTURE S999V99 VALUE +000.77.       IH342     
013400         04  FILLER          PICTURE S999V99 VALUE +001.92.       IH342     
013410         04  FILLER          PICTURE S999V99 VALUE +003.65.       IH342     
013420         04  FILLER          PICTURE S999V99 VALUE +005.96.       IH342     
013430         04  FILLER          PICTURE S999V99 VALUE +008.85.       IH342     
013440         04  FILLER          PICTURE S999V99 VALUE +012.31.       IH342     
013450         04  FILLER          PICTURE S999V99 VALUE +016.35.       IH342     
013460         04  FILLER          PICTURE S999V99 VALUE +020.96.       IH342     
013470         04  FILLER          PICTURE S999V99 VALUE +026.15.       IH342     
013480         04  FILLER          PICTURE S999V99 VALUE +000.00.       IH342     
013490         04  FILLER          PICTURE S999V99 VALUE +001.54.       IH342     
013500         04  FILLER          PICTURE S999V99 VALUE +003.85.       IH342     
013510         04  FILLER          PICTURE S999V99 VALUE +007.31.       IH342     
013520         04  FILLER          PICTURE S999V99 VALUE +011.92.       IH342     
013530         04  FILLER          PICTURE S999V99 VALUE +017.69.       IH342     
013540         04  FILLER          PICTURE S999V99 VALUE +024.62.       IH342     
013550         04  FILLER          PICTURE S999V99 VALUE +032.69.       IH342     
013560         04  FILLER          PICTURE S999V99 VALUE +041.92.       IH342     
013570         04  FILLER          PICTURE S999V99 VALUE +052.31.       IH342     
013580     02  REDEF-CALIF-TAX REDEFINES CALIF-SINGLE-MAR-TAX.          IH342     
013590         04  CALIF-TAX       PICTURE S999V99 OCCURS 20 TIMES.     IH342     
013600     02  CALIF-TAX-PLUS-PERCENT.                                  IH342     
013610         04  FILLER          PICTURE SV999   VALUE +.010.         IH342     
013620         04  FILLER          PICTURE SV999   VALUE +.020.         IH342     
013630         04  FILLER          PICTURE SV999   VALUE +.030.         IH342     
013640         04  FILLER          PICTURE SV999   VALUE +.040.         IH342     
013650         04  FILLER          PICTURE SV999   VALUE +.050.         IH342     
013660         04  FILLER          PICTURE SV999   VALUE +.060.         IH342     
013670         04  FILLER          PICTURE SV999   VALUE +.070.         IH342     
013680         04  FILLER          PICTURE SV999   VALUE +.080.         IH342     
013690         04  FILLER          PICTURE SV999   VALUE +.090.         IH342     
013700         04  FILLER          PICTURE SV999   VALUE +.100.         IH342     
013710         04  FILLER          PICTURE SV999   VALUE +.010.         IH342     
013720         04  FILLER          PICTURE SV999   VALUE +.020.         IH342     
013730         04  FILLER          PICTURE SV999   VALUE +.030.         IH342     
013740         04  FILLER          PICTURE SV999   VALUE +.040.         IH342     
013750         04  FILLER          PICTURE SV999   VALUE +.050.         IH342     
013760         04  FILLER          PICTURE SV999   VALUE +.060.         IH342     
013770         04  FILLER          PICTURE SV999   VALUE +.070.         IH342     
013780         04  FILLER          PICTURE SV999   VALUE +.080.         IH342     
013790         04  FILLER          PICTURE SV999   VALUE +.090.         IH342     
013800         04  FILLER          PICTURE SV999   VALUE +.100.         IH342     
013810     02  REDEF-CALIF-PERCENT REDEFINES CALIF-TAX-PLUS-PERCENT.    IH342     
013820         04  CALIF-EXCESS-PERCENT PICTURE SV999 OCCURS 20 TIMES.  IH342     
013830     02  CALIF-TAX-CREDIT.                                        IH342     
013840         04  FILLER          PICTURE S99V99  VALUE +00.96.        IH342     
013850         04  FILLER          PICTURE S99V99  VALUE +01.27.        IH342     
013860         04  FILLER          PICTURE S99V99  VALUE +01.58.        IH342     
013870         04  FILLER          PICTURE S99V99  VALUE +01.88.        IH342     
013880         04  FILLER          PICTURE S99V99  VALUE +02.19.        IH342     
013890         04  FILLER          PICTURE S99V99  VALUE +02.50.        IH342     
013900         04  FILLER          PICTURE S99V99  VALUE +02.81.        IH342     
013910         04  FILLER          PICTURE S99V99  VALUE +03.12.        IH342     
013920         04  FILLER          PICTURE S99V99  VALUE +03.42.        IH342     
013930         04  FILLER          PICTURE S99V99  VALUE +03.73.        IH342     
013940         04  FILLER          PICTURE S99V99  VALUE +00.96.        IH342     
013950         04  FILLER          PICTURE S99V99  VALUE +01.92.        IH342     
013960         04  FILLER          PICTURE S99V99  VALUE +02.23.        IH342     
013970         04  FILLER          PICTURE S99V99  VALUE +02.54.        IH342     
013980         04  FILLER          PICTURE S99V99  VALUE +02.85.        IH342     
013990         04  FILLER          PICTURE S99V99  VALUE +03.15.        IH342     
014000         04  FILLER          PICTURE S99V99  VALUE +03.46.        IH342     
014010         04  FILLER          PICTURE S99V99  VALUE +03.77.        IH342     
014020         04  FILLER          PICTURE S99V99  VALUE +04.08.        IH342     
014030         04  FILLER          PICTURE S99V99  VALUE +04.38.        IH342     
014040     02  REDEF-CALIF-TAX-CREDIT REDEFINES CALIF-TAX-CREDIT.       IH342     
014050         04  CALIF-CREDIT    PICTURE S99V99  OCCURS 20 TIMES.     IH342     
014060 01  SMOOTH-ROLL-ACTY-HDR                       VALUE SPACES.     IH342     
014070     02 ACT-CON-1            PICTURE X(253).                      IH342     
014080     02 ACT-CON-2            PICTURE X(20).                       IH342     
014090     02 ACT-CON-3            PICTURE X(273).                      IH342     
014100 01  REDEF-SMOOTH-ROLL-ACTY-HDR REDEFINES SMOOTH-ROLL-ACTY-HDR.   IH342     
014110     02 ACTY-AND-LOCATION            OCCURS 26 TIMES.             IH342     
014120        04 SR-ACT-CODE       PICTURE X.                           IH342     
014130        04 SR-LOCATION-HDR   PICTURE X(20).                       IH342     
014140 01  ACT-MOVE REDEFINES REDEF-SMOOTH-ROLL-ACTY-HDR.               IH342     
014150     02 ACT-GP-1              PICTURE X(105).                     IH342     
014160     02 ACT-GP-2              PICTURE X(60).                      IH342     
014170     02 ACT-GP-3              PICTURE X(105).                     IH342     
014180     02 ACT-GP-4              PICTURE X(60).                      IH342     
014190     02 ACT-GP-5              PICTURE X(105).                     IH342     
014200     02 ACT-GP-6              PICTURE X(60).                      IH342     
014210     02 ACT-GP-7              PICTURE X(51).                      IH342     
014220 01  ACTY-STORE                   PICTURE X   VALUE HIGH-VALUE.   IH342     
014230 01  UNION-DATA               PICTURE X(175)    VALUE SPACES.     IH342     
014240 01  UNION-TABLE REDEFINES UNION-DATA.                            IH342     
014250     02 UNION-CON            OCCURS 35 TIMES INDEXED BY AKEY.     IH342     
014260       04 UNION-CODE         PICTURE X.                           IH342     
014270       04 UNION-DUE          PICTURE 99V99.                       IH342     
014280 01  UNION-MOVE REDEFINES UNION-TABLE.                            IH342     
014290     02 UNION-GP-1            PICTURE X(105).                     IH342     
014300     02 UNION-GP-2            PICTURE X(60).                      IH342     
014310     02 UNION-GP-3            PICTURE X(10).                      IH342     
014320 01  OTHER-CON                                  VALUE SPACES.     IH342     
014330     02 ACTY-NAME-LOC         PICTURE X(27).                      IH342     
014340     02 DISBURSING-CODE       PICTURE XXXX.                       IH342     
014350     02 NARF-ACTY-CODES.                                          IH342     
014360       04 CODE-1             PICTURE X.                           IH342     
014370       04 CODE-2             PICTURE X.                           IH342     
014380       04 CODE-3             PICTURE X.                           IH342     
014390       04 CODE-4             PICTURE X.                           IH342     
014400     02 DIFF-GRP-1                PICTURE X(19).                  IH342     
014410     02 FILLER                    PICTURE X(48).                  IH342     
014420     02 DIFF-GRP-2                PICTURE X(12).                  IH342     
014430     02 DEPT-NAME                 PICTURE X(6).                   IH342     
014440     02 SR-PAGE-SW                PICTURE X.                      IH342     
014450     02 OUT-ACT-A.                                                IH342     
014460      03 ACT-A1                  PICTURE X.                       IH342     
014470      03 ACT-A2                  PICTURE X.                       IH342     
014480      03 ACT-A3                  PICTURE X.                       IH342     
014490      03 ACT-A4                  PICTURE X.                       IH342     
014500     02 OUT-GRP-A                 PICTURE X(31).                  IH342     
014510     02 OUT-ACT-B.                                                IH342     
014520      03 ACT-B1                  PICTURE X.                       IH342     
014530      03 ACT-B2                  PICTURE X.                       IH342     
014540      03 ACT-B3                  PICTURE X.                       IH342     
014550      03 ACT-B4                  PICTURE X.                       IH342     
014560     02 OUT-GRP-B                 PICTURE X(31).                  IH342     
014570     02 OUT-ACT-C.                                                IH342     
014580      03 ACT-C1                  PICTURE X.                       IH342     
014590      03 ACT-C2                  PICTURE X.                       IH342     
014600      03 ACT-C3                  PICTURE X.                       IH342     
014610      03 ACT-C4                  PICTURE X.                       IH342     
014620     02 OUT-GRP-C                 PICTURE X(31).                  IH342     
014630     02 OUT-ACT-D.                                                IH342     
014640      03 ACT-D1                  PICTURE X.                       IH342     
014650      03 ACT-D2                  PICTURE X.                       IH342     
014660      03 ACT-D3                  PICTURE X.                       IH342     
014670      03 ACT-D4                  PICTURE X.                       IH342     
014680     02 OUT-GRP-D                 PICTURE X(31).                  IH342     
014690     02 OUT-ACT-E.                                                IH342     
014700      03 ACT-E1                  PICTURE X.                       IH342     
014710      03 ACT-E2                  PICTURE X.                       IH342     
014720      03 ACT-E3                  PICTURE X.                       IH342     
014730      03 ACT-E4                  PICTURE X.                       IH342     
014740     02 OUT-GRP-E                 PICTURE X(31).                  IH342     
014750     02 OUT-ACT-F.                                                IH342     
014760      03 ACT-F1                  PICTURE X.                       IH342     
014770      03 ACT-F2                  PICTURE X.                       IH342     
014780      03 ACT-F3                  PICTURE X.                       IH342     
014790      03 ACT-F4                  PICTURE X.                       IH342     
014800     02 OUT-GRP-F                 PICTURE X(31).                  IH342     
014810     02 OUT-ACT-G.                                                IH342     
014820      03 ACT-G1                  PICTURE X.                       IH342     
014830      03 ACT-G2                  PICTURE X.                       IH342     
014840      03 ACT-G3                  PICTURE X.                       IH342     
014850      03 ACT-G4                  PICTURE X.                       IH342     
014860     02 OUT-GRP-G                 PICTURE X(31).                  IH342     
014870     02 OUT-ACT-H.                                                IH342     
014880      03 ACT-H1                  PICTURE X.                       IH342     
014890      03 ACT-H2                  PICTURE X.                       IH342     
014900      03 ACT-H3                  PICTURE X.                       IH342     
014910      03 ACT-H4                  PICTURE X.                       IH342     
014920     02 OUT-GRP-H                 PICTURE X(31).                  IH342     
014930     02 FILLER                    PICTURE XXX.                    IH342     
014940 01  CON-MOVE REDEFINES OTHER-CON.                                IH342     
014950     02 CON-GP-1              PICTURE X(85).                      IH342     
014960     02 FILLER                PICTURE X(276).                     IH342     
014970 01  CON-WORK-REC.                                                IH342     
014980     02 FILLER                PICTURE X(9)    VALUE ZEROS.        IH342     
014990     02 CONS-ID               PICTURE X       VALUE SPACE.        IH342     
015000     02 FILLER                PICTURE X(28)   VALUE ZEROS.        IH342     
015010     02 CONS-DATA-1.                                              IH342     
015020       04 DATA-1A            PICTURE X(20)   VALUE SPACE.         IH342     
015030       04 DATA-1B            PICTURE X(85)   VALUE SPACE.         IH342     
015040     02 FILLER                PICTURE X(17)   VALUE ZEROS.        IH342     
015050     02 CONS-DATA-2           PICTURE X(60)   VALUE SPACE.        IH342     
015060     02 FILLER                    PICTURE X(180)  VALUE SPACE.    IH342     
015070 01  PAY-DIFF-AREA                                VALUE ZEROS.    IH342     
015080     02 DIFF-AREA-1.                                              IH342     
015090       04 DIFF-A                 PICTURE 9V99.                    IH342     
015100       04 DIFF-B                 PICTURE 9V99.                    IH342     
015110       04 WHARF-DIFF             PICTURE 9V99.                    IH342     
015120       04 SHIFT-2                PICTURE 9V99.                    IH342     
015130       04 SHIFT-3                PICTURE 9V99.                    IH342     
015140       04 DIVER-RATE-KON         PICTURE 99V99.                   IH342     
015150     02 DIFF-AREA-2.                                              IH342     
015160       04 DIFF-J                 PICTURE 9V99.                    IH342     
015170       04 DIFF-K                 PICTURE 9V99.                    IH342     
015180       04 DIFF-N                 PICTURE 9V99.                    IH342     
015190       04 DIFF-R                 PICTURE 9V99.                    IH342     
015200 01   SR-STRIKE-TEST.                                             IH342     
015210     02  FILLER              PICTURE X(40)   VALUE                IH342     
015220         "XXXXXXX--ALIGN-TO-6TH-LINE-FROM-TOPXXXXX".              IH342     
015230     02  FILLER              PICTURE X(40)   VALUE                IH342     
015240         "XXXXXXXXXX--USE------SPECIAL---CARRIAGE-".              IH342     
015250     02  FILLER              PICTURE X(40)   VALUE                IH342     
015260         "TAPE---SMOOTH-ROLL-PRINTING---XXXXXXXXXX".              IH342     
015270 01   SMOOTH-ROLL-TOTALS.                                         IH342     
015280     02  SR-PAGE-ACTIVITY-TOTALS.                                 IH342     
015290         04  SR-PAGE-TOTALS.                                      IH342     
015300             06  PT-EMP-CTR  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015310             06  PT-RATE     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015320             06  PT-NET-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015330             06  PT-REG-HRS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015340             06  PT-REG-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015350             06  PT-OT-HRS   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015360             06  PT-OT-PAY   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015370             06  PT-OTH-HRS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015380             06  PT-OTH-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015390             06  PT-CSRA     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015400             06  PT-FICA     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015410             06  PT-FED-TAX  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015420             06  PT-ST-TAX   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015430             06  PT-HEALTH   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015440             06  PT-REG-LIFE PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015450             06  PT-AADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015460             06  PT-CADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015470             06  PT-EADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015480             06  PT-GADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015490             06  PT-UNION    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015500             06  PT-CONTRIB  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015510             06  PT-OTHER    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015520             06  PT-BOND     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015530             06  PT-ALLOT    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015540             06  PT-BADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015550             06  PT-DADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015560             06  PT-FADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015570             06  PT-OPT-INS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015580         04  SR-CNTL-GP-TOTALS.                                   IH342     
015590             06  CG-EMP-CTR  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015600             06  CG-RATE     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015610             06  CG-NET-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015620             06  CG-REG-HRS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015630             06  CG-REG-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015640             06  CG-OT-HRS   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015650             06  CG-OT-PAY   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015660             06  CG-OTH-HRS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015670             06  CG-OTH-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015680             06  CG-CSRA     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015690             06  CG-FICA     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015700             06  CG-FED-TAX  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015710             06  CG-ST-TAX   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015720             06  CG-HEALTH   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015730             06  CG-REG-LIFE PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015740             06 CG-AADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015750             06 CG-CADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015760             06 CG-EADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015770             06 CG-GADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015780             06  CG-UNION    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015790             06  CG-CONTRIB  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015800             06  CG-OTHER    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015810             06  CG-BOND     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015820             06  CG-ALLOT    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015830             06 CG-BADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015840             06 CG-DADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015850             06 CG-FADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015860             06 CG-OPT-INS   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015870         04  SR-ACTIVITY-TOTALS.                                  IH342     
015880             06  AT-EMP-CTR  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015890             06  AT-RATE     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015900             06  AT-NET-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015910             06  AT-REG-HRS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015920             06  AT-REG-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015930             06  AT-OT-HRS   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015940             06  AT-OT-PAY   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015950             06  AT-OTH-HRS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015960             06  AT-OTH-PAY  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015970             06  AT-CSRA     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015980             06  AT-FICA     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
015990             06  AT-FED-TAX  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016000             06  AT-ST-TAX   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016010             06  AT-HEALTH   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016020             06  AT-REG-LIFE PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016030             06 AT-AADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016040             06 AT-CADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016050             06 AT-EADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016060             06 AT-GADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016070             06  AT-UNION    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016080             06  AT-CONTRIB  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016090             06  AT-OTHER    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016100             06  AT-BOND     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016110             06  AT-ALLOT    PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016120             06 AT-BADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016130             06 AT-DADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016140             06 AT-FADD      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016150             06 AT-OPT-INS   PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016160         04  PAYROLL-GRAND-TOTALS.                                IH342     
016170             06  PGT-EMP-CTR  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016180             06  PGT-RATE     PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016190             06  PGT-NET-PAY  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016200             06  PGT-REG-HRS  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016210             06  PGT-REG-PAY  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016220             06  PGT-OT-HRS   PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016230             06  PGT-OT-PAY   PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016240             06  PGT-OTH-HRS  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016250             06  PGT-OTH-PAY  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016260             06  PGT-CSRA     PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016270             06  PGT-FICA     PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016280             06  PGT-FED-TAX  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016290             06  PGT-ST-TAX   PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016300             06  PGT-HEALTH   PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016310             06  PGT-REG-LIFE PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016320             06 PGT-AADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016330             06 PGT-CADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016340             06 PGT-EADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016350             06 PGT-GADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016360             06  PGT-UNION    PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016370             06  PGT-CONTRIB  PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016380             06  PGT-OTHER    PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016390             06  PGT-BOND     PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016400             06  PGT-ALLOT    PICTURE S9(7)V99 VALUE 0000000.00.  IH342     
016410             06 PGT-BADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016420             06 PGT-DADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016430             06 PGT-FADD     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016440             06 PGT-OPT-INS  PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016450     02  REDEF-SR-TOTALS REDEFINES SR-PAGE-ACTIVITY-TOTALS.       IH342     
016460         04  REDEF-PAGE-ACTIVITY-TOTS        OCCURS 4 TIMES.      IH342     
016470             06  RT-EMP-CTR  PICTURE S9(7)V99.                    IH342     
016480             06  RT-RATE     PICTURE S9(7)V99.                    IH342     
016490             06  RT-NET-PAY  PICTURE S9(7)V99.                    IH342     
016500             06  RT-REG-HRS  PICTURE S9(7)V99.                    IH342     
016510             06  RT-REG-PAY  PICTURE S9(7)V99.                    IH342     
016520             06  RT-OT-HRS   PICTURE S9(7)V99.                    IH342     
016530             06  RT-OT-PAY   PICTURE S9(7)V99.                    IH342     
016540             06  RT-OTH-HRS  PICTURE S9(7)V99.                    IH342     
016550             06  RT-OTH-PAY  PICTURE S9(7)V99.                    IH342     
016560             06  RT-CSRA     PICTURE S9(7)V99.                    IH342     
016570             06  RT-FICA     PICTURE S9(7)V99.                    IH342     
016580             06  RT-FED-TAX  PICTURE S9(7)V99.                    IH342     
016590             06  RT-ST-TAX   PICTURE S9(7)V99.                    IH342     
016600             06  RT-HEALTH   PICTURE S9(7)V99.                    IH342     
016610             06  RT-REG-LIFE PICTURE S9(7)V99.                    IH342     
016620             06  RT-AADD     PICTURE S9(7)V99.                    IH342     
016630             06 RT-CADD      PICTURE S9(7)V99.                    IH342     
016640             06 RT-EADD      PICTURE S9(7)V99.                    IH342     
016650             06 RT-GADD      PICTURE S9(7)V99.                    IH342     
016660             06  RT-UNION    PICTURE S9(7)V99.                    IH342     
016670             06  RT-CONTRIB  PICTURE S9(7)V99.                    IH342     
016680             06  RT-OTHER    PICTURE S9(7)V99.                    IH342     
016690             06  RT-BOND     PICTURE S9(7)V99.                    IH342     
016700             06  RT-ALLOT    PICTURE S9(7)V99.                    IH342     
016710             06 RT-BADD      PICTURE S9(7)V99.                    IH342     
016720             06 RT-DADD      PICTURE S9(7)V99.                    IH342     
016730             06 RT-FADD      PICTURE S9(7)V99.                    IH342     
016740             06 RT-OPT-INS   PICTURE S9(7)V99.                    IH342     
016750 01  CONTROL-TOTALS.                                              IH342     
016760     02  CHECK-COUNTER       PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016770     02  SUBJECT-FICA        PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016780     02  SUBJECT-RETIRE      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016790     02  HEALTH-COUNT        PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016800     02  EMPLOYEE-HEALTH     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016810     02  EMPLOYER-HEALTH     PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016820     02  BASIC-HOURS         PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016830     02  OVERTIME-HOURS      PICTURE S9(7)V99 VALUE +0000000.00.  IH342     
016840     02  BOND-COUNT      PICTURE   S9(7)V99 VALUE +0000000.00.    IH342     
016850     02  BOND-DEDUCTION  PICTURE   S9(7)V99 VALUE +0000000.00.    IH342     
016860 01  REDEF-CONTROL-TOTALS REDEFINES CONTROL-TOTALS.               IH342     
016870     02  CNTRL-TOTS      PICTURE S9(7)V99 OCCURS 10 TIMES.        IH342     
016880 01   FED-TAX-COMPUTATION-TABLES.                                 IH342     
016890     02  SINGLE-MARRIED-WAGES.                                    IH342     
016900         04  FILLER          PICTURE S999V99 VALUE +069.00.       IH342     
016910         04  FILLER          PICTURE S999V99 VALUE +077.00.       IH342     
016920         04  FILLER          PICTURE S999V99 VALUE +258.00.       IH342     
016930         04  FILLER          PICTURE S999V99 VALUE +058.00.       IH342     
016940         04  FILLER          PICTURE S999V99 VALUE +076.00.       IH342     
016950         04  FILLER          PICTURE S999V99 VALUE +127.00.       IH342     
016960         04  FILLER          PICTURE S999V99 VALUE +079.00.       IH342     
016970         04  FILLER          PICTURE S999V99 VALUE +256.00.       IH342     
016980         04  FILLER          PICTURE S999V99 VALUE +078.00.       IH342     
016990         04  FILLER          PICTURE S999V99 VALUE +235.00.       IH342     
017000         04  FILLER          PICTURE S999V99 VALUE +169.00.       IH342     
017010         04  FILLER          PICTURE S999V99 VALUE +154.00.       IH342     
017020     02  REDEF-WAGES REDEFINES SINGLE-MARRIED-WAGES.              IH342     
017030         04  TAXABLE-WAGES   PICTURE S999V99 OCCURS 12 TIMES.     IH342     
017040     02  SINGLE-MARRIED-DEDUCTIONS.                               IH342     
017050         04  FILLER          PICTURE S999V99  VALUE +000.00.      IH342     
017060         04  FILLER          PICTURE S999V99 VALUE +006.72.       IH342     
017070         04  FILLER          PICTURE S999V99 VALUE +020.58.       IH342     
017080         04  FILLER          PICTURE S999V99 VALUE +074.76.       IH342     
017090         04  FILLER          PICTURE S999V99 VALUE +088.10.       IH342     
017100         04  FILLER          PICTURE S999V99 VALUE +108.62.       IH342     
017110         04  FILLER          PICTURE S999V99 VALUE +147.99.       IH342     
017120         04  FILLER          PICTURE S999V99  VALUE +000.00.      IH342     
017130         04  FILLER          PICTURE S999V99 VALUE +008.12.       IH342     
017140         04  FILLER          PICTURE S999V99 VALUE +049.08.       IH342     
017150         04  FILLER          PICTURE S999V99 VALUE +064.68.       IH342     
017160         04  FILLER          PICTURE S999V99 VALUE +121.08.       IH342     
017170         04  FILLER          PICTURE S999V99 VALUE +168.40.       IH342     
017180         04  FILLER          PICTURE S999V99 VALUE +217.68.       IH342     
017190     02  REDEF-DEDUCTIONS REDEFINES SINGLE-MARRIED-DEDUCTIONS.    IH342     
017200         04  TAX-DEDUCTIONS  PICTURE S999V99 OCCURS 14 TIMES.     IH342     
017210     02  TAX-PLUS-PERCENT.                                        IH342     
017220         04  FILLER          PICTURE SV999    VALUE +.140.        IH342     
017230         04  FILLER          PICTURE SV999   VALUE +.180.         IH342     
017240         04  FILLER          PICTURE SV999   VALUE +.210.         IH342     
017250         04  FILLER          PICTURE SV999   VALUE +.230.         IH342     
017260         04  FILLER          PICTURE SV999   VALUE +.270.         IH342     
017270         04  FILLER          PICTURE SV999   VALUE +.310.         IH342     
017280         04  FILLER          PICTURE SV999   VALUE +.350.         IH342     
017290         04  FILLER          PICTURE SV999    VALUE +.140.        IH342     
017300         04  FILLER          PICTURE SV999   VALUE +.160.         IH342     
017310         04  FILLER          PICTURE SV999   VALUE +.200.         IH342     
017320         04  FILLER          PICTURE SV999   VALUE +.240.         IH342     
017330         04  FILLER          PICTURE SV999   VALUE +.280.         IH342     
017340         04  FILLER          PICTURE SV999   VALUE +.320.         IH342     
017350         04  FILLER          PICTURE SV999   VALUE +.360.         IH342     
017360     02  REDEF-PERCENT REDEFINES TAX-PLUS-PERCENT.                IH342     
017370         04  EXCESS-PERCENT  PICTURE SV999   OCCURS 14 TIMES.     IH342     
017380 01   PAYROLL-EXC-LIST-TOTALS.                                    IH342     
017390     02  EXC-CODE1           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017400     02  EXC-CODE2           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017410     02  EXC-CODE3           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017420     02  EXC-CODE4           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017430     02  EXC-CODE5           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017440     02  EXC-CODE6           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017450     02  EXC-CODE7           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017460     02  EXC-CODE8           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017470     02  EXC-CODE9           PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017480     02  EXC-CODE10          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017490     02  EXC-CODE11          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017500     02  EXC-CODE12          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017510     02  EXC-CODE13          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017520     02  EXC-CODE14          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017530     02  EXC-CODE15          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017540     02  EXC-CODE16          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017550     02  EXC-CODE17          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017560     02  EXC-CODE18          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017570     02  EXC-CODE19          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017580     02  EXC-CODE20          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017590     02  EXC-CODE21          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017600     02  EXC-CODE22          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017610     02  EXC-CODE23          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017620     02  EXC-CODE24          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017630     02  EXC-CODE25          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017640     02  EXC-CODE26          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017650     02  EXC-CODE27          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017660     02  EXC-CODE28          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017670     02  EXC-CODE29          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017680     02  EXC-CODE30          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017690     02  EXC-CODE31          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017700     02  EXC-CODE32          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017710     02  EXC-CODE33          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017720     02  EXC-CODE34          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017730     02  EXC-CODE35          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017740     02  EXC-CODE36          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017750     02  EXC-CODE37          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017760     02  EXC-CODE38          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017770     02  EXC-CODE39          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017780     02  EXC-CODE40          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017790     02  EXC-CODE41          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017800     02  EXC-CODE42          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017810     02  EXC-CODE43          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017820     02  EXC-CODE44          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017830     02  EXC-CODE45          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017840     02  EXC-CODE46          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017850     02  EXC-CODE47          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017860     02  EXC-CODE48          PICTURE S9(5)V99 VALUE +00000.00.    IH342     
017870 01   REDEF-LIST-TOTS REDEFINES PAYROLL-EXC-LIST-TOTALS.          IH342     
017880     02  EXC-LIST-TOT        PICTURE S9(5)V99 OCCURS 48 TIMES.    IH342     
017890 01   PAYROLL-EOJ-REC-TOTS.                                       IH342     
017900     02  MSTR-IN             PICTURE S9(5)   VALUE +00000.        IH342     
017910     02  DTL-IN              PICTURE S9(5)   VALUE +00000.        IH342     
017920     02  MSTR-OUT            PICTURE S9(5)   VALUE +00000.        IH342     
017930     02  ACTIVE-OUT          PICTURE S9(5)   VALUE +00000.        IH342     
017940     02  SR-OUT              PICTURE S9(5)   VALUE +00000.        IH342     
017950     02  WT-OUT              PICTURE S9(5)   VALUE +00000.        IH342     
017960     02  EXC-OUT             PICTURE S9(5)   VALUE +00000.        IH342     
017970     02  ERR-DET-OUT         PICTURE S9(5)   VALUE +00000.        IH342     
017980     02  CONTROL-REC-OUT     PICTURE S9(5)   VALUE +00000.        IH342     
017990     02  YTD-OUT             PICTURE S9(5) VALUE ZERO.            IH342     
018000 01   REDEF-EOJ-TOTS REDEFINES PAYROLL-EOJ-REC-TOTS.              IH342     
018010     02  EOJ-TOTALS          PICTURE S9(5) OCCURS 10 TIMES.       IH342     
018020 01   BINARY-WORK-AREA.                                           IH342     
018030     02  SUBS-1              PICTURE S99     VALUE +00.           IH342     
018040     02  SUBS-2              PICTURE S99     VALUE +00.           IH342     
018050     02  SUBS-3              PICTURE S99     VALUE +00.           IH342     
018060     02  SUBS-4              PICTURE S99     VALUE 00.            IH342     
018070     02  WORK-CODE           PICTURE S99     VALUE +00.           IH342     
018080     02  CASH-AWARD-SUBS     PICTURE S99     VALUE +00.           IH342     
018090 01   CONTR-HEADER.                                               IH342     
018100     02  FILLER        PICTURE X(20) VALUE "CONTROL LISTING".     IH342     
018110     02  CONTR-GORU    PICTURE X(20).                             IH342     
018120     02  FILLER        PICTURE X(12) VALUE "IH340  PPE".          IH342     
018130     02  CONTR-DATE    PICTURE X(18).                             IH342     
018140     02  FILLER        PICTURE X(8) VALUE "PAGE NO".              IH342     
018150     02  CONTR-PAGE    PICTURE ZZZZ9.                             IH342     
018160     02  FILLER        PICTURE X(37) VALUE SPACES.                IH342     
018170 01   CLPAGE    PICTURE 9(5) VALUE ZERO.                          IH342     
018180 01   EXCPAGE                 PICTURE 9999  VALUE ZERO.           IH342     
018190 PROCEDURE  DIVISION.                                             IH342     
018200 1STPAR.                                                          IH342     
018210     NOTE ******************************************************* IH342     
018220          *                                                     * IH342     
018230          *  PAYROLL HOUSEKEEPING ROUTINE                       * IH342     
018240          *      OPEN ALL INPUT AND OUTPUT FILES                * IH342     
018250          *      GETPARM  WEEK/BEGINNING AND WEEK/ENDING DATES  * IH342     
018260          *          PER ANNUM ROLL NUMBER                      * IH342     
018270          *              EXCEPTION RUN CONTROL SWITCH SETTINGS  * IH342     
018280          *                                                     * IH342     
018290          *******************************************************.IH342     
018300 100140.                                                          IH342     
018310     DISPLAY "BEGIN IH340 REVISION A AS OF  04/71".               IH342     
018320     OPEN INPUT PERS-MSTR-IN-FILE.                                IH342     
018330     OPEN INPUT PAYROLL-DETAIL-FILE.                              IH342     
018340         OPEN OUTPUT PERS-MSTR-OUT-FILE.                          IH342     
018350         OPEN OUTPUT SMOOTH-ROLL-FILE.                            IH342     
018360         OPEN OUTPUT PAYROLL-WORK-FILE.                           IH342     
018370         OPEN OUTPUT CONTROL-ERROR-FILE,                          IH342     
018380         OPEN OUTPUT PAY-EXCEPT-LIST-FILE.                        IH342     
018390             MOVE SPACES TO SMOOTH-ROLL-REC,                      IH342     
018400                            PAYROLL-WORK-REC,                     IH342     
018410                            CONTROL-REC,                          IH342     
018420                        PAY-EXCEPT-LIST-REC.                      IH342     
018430     WRITE SMOOTH-ROLL-REC FROM SR-STRIKE-TEST BEFORE ADVANCING   IH342     
018440         CHANNEL 1.                                               IH342     
018450     MOVE ALL SPACES TO SMOOTH-ROLL-REC.                          IH342     
018460     NOTE ******************************************************* IH342     
018470          *                                                     * IH342     
018480          *  EXTRACT CONSTANT DATA FROM CONSTANT RECORDS        * IH342     
018490          *  GET PARM DATA FROM EXEC CARD                       * IH342     
018500          *      AUDIT FIELDS FOR CORRECT INFORMATION           * IH342     
018510          *                                                     * IH342     
018520          *******************************************************.IH342     
018530 103000.                                                          IH342     
018540     READ PAYROLL-DETAIL-FILE, AT END GO TO 408010.               IH342     
018550     IF PARAM-CNTL NOT EQUAL TO ZEROS, GO TO 490000.              IH342     
018560     MOVE PARAM-DATA-IN TO PARM-DATA.                             IH342     
018570     PERFORM 376000.                                              IH342     
018580         IF BEGIN-CHECK-NO NOT NUMERIC,                           IH342     
018590             GO TO 104100.                                        IH342     
018600       IF ROLL-NO NOT NUMERIC GO TO 104120.                       IH342     
018610         IF ROLL-NO GREATER THAN 00,                              IH342     
018620             AND LESS THAN 28,                                    IH342     
018630            GO TO PROCESS-CONSTANTS.                              IH342     
018640     GO TO 104120.                                                IH342     
018650 104100.                                                          IH342     
018660     DISPLAY "XXXXXXXXXX BEGIN CHECK INVALID" BEGIN-CHECK-NO.     IH342     
018670     STOP "XXXXXXXXXX HALT 0004".                                 IH342     
018680     GO TO 104100.                                                IH342     
018690 104120.                                                          IH342     
018700     IF ROLL-NO EQUALS 99 GO TO PROCESS-CONSTANTS.                IH342     
018710     DISPLAY "XXXXXXXXXX ROLL NR. INVALID" ROLL-NO.               IH342     
018720     STOP "XXXXXXXXXX HALT 0005".                                 IH342     
018730     GO TO 104120.                                                IH342     
018740 PROCESS-CONSTANTS.                                               IH342     
018750     READ PERS-MSTR-IN-FILE  AT END GO TO 408140.                 IH342     
018760     IF CON-ID NOT EQUAL TO ZEROS GO TO CONSTANT-MISS.            IH342     
018770     IF CON-KEY NOT EQUAL TO "1" GO TO CK-FOR-2.                  IH342     
018780     MOVE ACT-DATA-1 TO ACT-CON-1.                                IH342     
018790     MOVE ACT-NAME TO ACT-CON-2.                                  IH342     
018800     MOVE ACT-DATA-2 TO ACT-CON-3.                                IH342     
018810     MOVE CONSTANT-REC-1 TO PERS-MSTR-OUT-REC.                    IH342     
018820     WRITE PERS-MSTR-OUT-REC.                                     IH342     
018830     IF OTHER-CON EQUAL TO SPACES GO TO PROCESS-CONSTANTS.        IH342     
018840     GO TO WRITE-CON-WORK.                                        IH342     
018850 CK-FOR-2.                                                        IH342     
018860     IF CON-KEY NOT EQUAL TO "2" GO TO CONSTANT-MISS.             IH342     
018870     MOVE UNION-DATA-IN TO UNION-DATA.                            IH342     
018880     MOVE LOCAL-CON TO OTHER-CON.                                 IH342     
018890     MOVE CONSTANT-REC-2 TO PERS-MSTR-OUT-REC.                    IH342     
018900     WRITE PERS-MSTR-OUT-REC.                                     IH342     
018910     IF ACT-CON-1 EQUAL TO SPACES GO TO PROCESS-CONSTANTS.        IH342     
018920     GO TO WRITE-CON-WORK.                                        IH342     
018930 CONSTANT-MISS.                                                   IH342     
018940     DISPLAY "XXXXXXXXXX CONSTANT REC ERROR".                     IH342     
018950     GO TO CONSTANT-MISS.                                         IH342     
018960 WRITE-CON-WORK.                                                  IH342     
018970     MOVE "1" TO CONS-ID.                                         IH342     
018980     MOVE ACT-GP-1 TO CONS-DATA-1.                                IH342     
018990     MOVE ACT-GP-2 TO CONS-DATA-2.                                IH342     
019000     MOVE CON-WORK-REC TO PAYROLL-WORK-REC.                       IH342     
019010     WRITE PAYROLL-WORK-REC.                                      IH342     
019020     MOVE "2" TO CONS-ID.                                         IH342     
019030     MOVE ACT-GP-3 TO CONS-DATA-1.                                IH342     
019040     MOVE ACT-GP-4 TO CONS-DATA-2.                                IH342     
019050     MOVE CON-WORK-REC TO PAYROLL-WORK-REC.                       IH342     
019060     WRITE PAYROLL-WORK-REC.                                      IH342     
019070     MOVE "3" TO CONS-ID.                                         IH342     
019080     MOVE ACT-GP-5 TO CONS-DATA-1.                                IH342     
019090     MOVE ACT-GP-6 TO CONS-DATA-2.                                IH342     
019100     MOVE CON-WORK-REC TO PAYROLL-WORK-REC.                       IH342     
019110     WRITE PAYROLL-WORK-REC.                                      IH342     
019120     MOVE "4" TO CONS-ID.                                         IH342     
019130     MOVE ACT-GP-7 TO CONS-DATA-1.                                IH342     
019140     MOVE SPACES TO CONS-DATA-2.                                  IH342     
019150     MOVE CON-WORK-REC TO PAYROLL-WORK-REC.                       IH342     
019160     WRITE PAYROLL-WORK-REC.                                      IH342     
019170     MOVE "5" TO CONS-ID.                                         IH342     
019180     MOVE UNION-GP-1 TO CONS-DATA-1.                              IH342     
019190     MOVE UNION-GP-2 TO CONS-DATA-2.                              IH342     
019200     MOVE CON-WORK-REC TO PAYROLL-WORK-REC.                       IH342     
019210     WRITE PAYROLL-WORK-REC.                                      IH342     
019220     MOVE "6" TO CONS-ID.                                         IH342     
019230     MOVE UNION-GP-3 TO DATA-1A.                                  IH342     
019240     MOVE CON-GP-1 TO DATA-1B.                                    IH342     
019250     MOVE SPACES TO CONS-DATA-2.                                  IH342     
019260     MOVE CON-WORK-REC TO PAYROLL-WORK-REC.                       IH342     
019270     WRITE PAYROLL-WORK-REC.                                      IH342     
019280     MOVE SPACES TO PAYROLL-WORK-REC.                             IH342     
019290     MOVE ZEROES TO WT-TAX-LEVY-ZEROS.                            IH342     
019300     MOVE ZEROES TO WT-MONEY-HOURS-ZEROS.                         IH342     
019310     NOTE ******************************************************* IH342     
019320          *                                                     * IH342     
019330          *  BEGIN PAYROLL MAIN PROGRAM ROUTINE                 * IH342     
019340          *      READ PERSONNEL MASTER FILE AND                 * IH342     
019350          *          SEQUENCE CHECK EACH MASTER                 * IH342     
019360          *                                                     * IH342     
019370          *******************************************************.IH342     
019380 108090.                                                          IH342     
019390     READ PERS-MSTR-IN-FILE,                                      IH342     
019400         AT END GO TO 408140.                                     IH342     
019410     ADD  +1 TO MSTR-IN.                                          IH342     
019420     MOVE +1 TO CHECK-COUNT.                                      IH342     
019430     IF MSTR-IN-SEQ GREATER THAN SEQ-PERS-MSTR,                   IH342     
019440         MOVE MSTR-IN-SEQ TO SEQ-PERS-MSTR,                       IH342     
019450         MOVE PERS-MSTR-IN-REC TO PERS-MSTR-OUT-REC,              IH342     
019460             GO TO 110070.                                        IH342     
019470     IF MSTR-IN-SEQ IS EQUAL TO SEQ-PERS-MSTR, GO TO 109000.      IH342     
019480             GO TO 110000.                                        IH342     
019490 109000.                                                          IH342     
019500     PERFORM 198160.                                              IH342     
019510     MOVE DUP-MSG TO ERR-EXPLAIN.                                 IH342     
019520     ALTER 198140 TO PROCEED TO 108090.                           IH342     
019530     GO TO 198050.                                                IH342     
019540     NOTE ******************************************************* IH342     
019550          *                                                     * IH342     
019560          *  TEST FOR PAYROLL RUN EXCEPTIONS,                   * IH342     
019570          *      IF RUN EXCEPTION CODE EQUAL TO PERSONNEL       * IH342     
019580          *          MASTER ACTIVITY CODE BYPASS MASTER         * IH342     
019590          *                                                     * IH342     
019600          *******************************************************.IH342     
019610 110000.                                                          IH342     
019620     DISPLAY "XXXXXXXXXX MASTER IN  " MSTR-IN-SEQ.                IH342     
019630     DISPLAY "XXXXXXXXXX MASTER HOLD" SEQ-PERS-MSTR.              IH342     
019640     STOP "XXXXXXXXXX HALT 0001".                                 IH342     
019650     GO TO 110000.                                                IH342     
019660 110070.                                                          IH342     
019670     IF EXCEPT-CODES EQUAL TO SPACES,                             IH342     
019680             GO TO 112070.                                        IH342     
019690         MOVE 01 TO SUBS-1.                                       IH342     
019700 110110.                                                          IH342     
019710         IF MSTR-ACTY EQUAL TO EXCEPT-TABLE (SUBS-1),             IH342     
019720             GO TO 406100.                                        IH342     
019730     IF SUBS-1 IS LESS THAN 24,                                   IH342     
019740             ADD 01 TO SUBS-1,                                    IH342     
019750                 GO TO 110110.                                    IH342     
019760     NOTE ******************************************************* IH342     
019770          *                                                     * IH342     
019780          *  SELECT ACTIVE PERSONNEL                            * IH342     
019790          *      TEST WEEK ENDING DATE AGAINST ENTRANCE         * IH342     
019800          *          ON DUTY DATE, IF EOD IS GREATER THAN       * IH342     
019810          *          W/E DATE BYPASS MASTER                     * IH342     
019820          *  LOAD PAY DIFF  VALUES INTO COMPUTE AREA            * IH342     
019830          *                                                     * IH342     
019840          *******************************************************.IH342     
019850 112070.                                                          IH342     
019860     IF NOT ACTIVE-MSTR,                                          IH342     
019870         GO TO 406100.                                            IH342     
019880         MOVE EOD-YEAR TO TEST-EOD-YR.                            IH342     
019890         MOVE EOD-MODAY TO TEST-EOD-MODAY.                        IH342     
019900         MOVE WE-YEAR TO TEST-WE-YR.                              IH342     
019910         MOVE WE-MODAY TO TEST-WE-MODAY.                          IH342     
019920         IF TEST-EOD GREATER THAN TEST-WE,                        IH342     
019930             GO TO 406100.                                        IH342     
019940     IF MSTR-ACTY EQUAL TO ACTY-STORE GO TO 112120.               IH342     
019950     MOVE MSTR-ACTY TO ACTY-STORE.                                IH342     
019960     IF OUT-ACT-A EQUAL TO SPACES GO TO 112074.                   IH342     
019970     IF ACT-A1 EQUAL TO MSTR-ACTY GO TO 112072.                   IH342     
019980     IF ACT-A2 EQUAL TO MSTR-ACTY GO TO 112072.                   IH342     
019990     IF ACT-A3 EQUAL TO MSTR-ACTY GO TO 112072.                   IH342     
020000     IF ACT-A4 EQUAL TO MSTR-ACTY GO TO 112072.                   IH342     
020010     GO TO 112074.                                                IH342     
020020 112072.                                                          IH342     
020030     MOVE OUT-GRP-A TO PAY-DIFF-AREA.                             IH342     
020040     GO TO 112100.                                                IH342     
020050 112074.                                                          IH342     
020060     IF OUT-ACT-B EQUAL TO SPACES GO TO 112078.                   IH342     
020070     IF ACT-B1 EQUAL TO MSTR-ACTY GO TO 112076.                   IH342     
020080     IF ACT-B2 EQUAL TO MSTR-ACTY GO TO 112076.                   IH342     
020090     IF ACT-B3 EQUAL TO MSTR-ACTY GO TO 112076.                   IH342     
020100     IF ACT-B4 EQUAL TO MSTR-ACTY GO TO 112076.                   IH342     
020110     GO TO 112078.                                                IH342     
020120 112076.                                                          IH342     
020130     MOVE OUT-GRP-B TO PAY-DIFF-AREA.                             IH342     
020140     GO TO 112100.                                                IH342     
020150 112078.                                                          IH342     
020160     IF OUT-ACT-C EQUAL TO SPACES GO TO 112082.                   IH342     
020170     IF ACT-C1 EQUAL TO MSTR-ACTY GO TO 112080.                   IH342     
020180     IF ACT-C2 EQUAL TO MSTR-ACTY GO TO 112080.                   IH342     
020190     IF ACT-C3 EQUAL TO MSTR-ACTY GO TO 112080.                   IH342     
020200     IF ACT-C4 EQUAL TO MSTR-ACTY GO TO 112080.                   IH342     
020210     GO TO 112082.                                                IH342     
020220 112080.                                                          IH342     
020230     MOVE OUT-GRP-C TO PAY-DIFF-AREA.                             IH342     
020240     GO TO 112100.                                                IH342     
020250 112082.                                                          IH342     
020260     IF OUT-ACT-D EQUAL TO SPACES GO TO 112086.                   IH342     
020270     IF ACT-D1 EQUAL TO MSTR-ACTY GO TO 112084.                   IH342     
020280     IF ACT-D2 EQUAL TO MSTR-ACTY GO TO 112084.                   IH342     
020290     IF ACT-D3 EQUAL TO MSTR-ACTY GO TO 112084.                   IH342     
020300     IF ACT-D4 EQUAL TO MSTR-ACTY GO TO 112084.                   IH342     
020310     GO TO 112086.                                                IH342     
020320 112084.                                                          IH342     
020330     MOVE OUT-GRP-D TO PAY-DIFF-AREA.                             IH342     
020340     GO TO 112100.                                                IH342     
020350 112086.                                                          IH342     
020360     IF OUT-ACT-E EQUAL TO SPACES GO TO 112090.                   IH342     
020370     IF ACT-E1 EQUAL TO MSTR-ACTY GO TO 112088.                   IH342     
020380     IF ACT-E2 EQUAL TO MSTR-ACTY GO TO 112088.                   IH342     
020390     IF ACT-E3 EQUAL TO MSTR-ACTY GO TO 112088.                   IH342     
020400     IF ACT-E4 EQUAL TO MSTR-ACTY GO TO 112088.                   IH342     
020410     GO TO 112090.                                                IH342     
020420 112088.                                                          IH342     
020430     MOVE OUT-GRP-E TO PAY-DIFF-AREA.                             IH342     
020440     GO TO 112100.                                                IH342     
020450 112090.                                                          IH342     
020460     IF OUT-ACT-F EQUAL TO SPACES GO TO 112092.                   IH342     
020470     IF ACT-F1 EQUAL TO MSTR-ACTY GO TO 112091.                   IH342     
020480     IF ACT-F2 EQUAL TO MSTR-ACTY GO TO 112091.                   IH342     
020490     IF ACT-F3 EQUAL TO MSTR-ACTY GO TO 112091.                   IH342     
020500     IF ACT-F4 EQUAL TO MSTR-ACTY GO TO 112091.                   IH342     
020510     GO TO 112092.                                                IH342     
020520 112091.                                                          IH342     
020530     MOVE OUT-GRP-F TO PAY-DIFF-AREA.                             IH342     
020540     GO TO 112100.                                                IH342     
020550 112092.                                                          IH342     
020560     IF OUT-ACT-G EQUAL TO SPACES GO TO 112094.                   IH342     
020570     IF ACT-G1 EQUAL TO MSTR-ACTY GO TO 112093.                   IH342     
020580     IF ACT-G2 EQUAL TO MSTR-ACTY GO TO 112093.                   IH342     
020590     IF ACT-G3 EQUAL TO MSTR-ACTY GO TO 112093.                   IH342     
020600     IF ACT-G4 EQUAL TO MSTR-ACTY GO TO 112093.                   IH342     
020610     GO TO 112094.                                                IH342     
020620 112093.                                                          IH342     
020630     MOVE OUT-GRP-G TO PAY-DIFF-AREA.                             IH342     
020640     GO TO 112100.                                                IH342     
020650 112094.                                                          IH342     
020660     IF OUT-ACT-H EQUAL TO SPACES GO TO 112098.                   IH342     
020670     IF ACT-H1 EQUAL TO MSTR-ACTY GO TO 112096.                   IH342     
020680     IF ACT-H2 EQUAL TO MSTR-ACTY GO TO 112096.                   IH342     
020690     IF ACT-H3 EQUAL TO MSTR-ACTY GO TO 112096.                   IH342     
020700     IF ACT-H4 EQUAL TO MSTR-ACTY GO TO 112096.                   IH342     
020710     GO TO 112098.                                                IH342     
020720 112096.                                                          IH342     
020730     MOVE OUT-GRP-H TO PAY-DIFF-AREA.                             IH342     
020740     GO TO 112100.                                                IH342     
020750 112098.                                                          IH342     
020760     MOVE DIFF-GRP-1 TO DIFF-AREA-1.                              IH342     
020770     MOVE DIFF-GRP-2 TO DIFF-AREA-2.                              IH342     
020780 112100.                                                          IH342     
020790     MULTIPLY DIVER-RATE-KON BY -1.00 GIVING MINUS-DIVER-RATE.    IH342     
020800 112120.                                                          IH342     
020810     IF MSTR-HOURLY-RATE EQUAL TO +00.00,                         IH342     
020820         MOVE ZEROS TO NET-PAY,                                   IH342     
020830             GO TO 338170.                                        IH342     
020840 FFNOTE1.                                                         IH342     
020850     NOTE ******************************************************* IH342     
020860          *                                                     * IH342     
020870          *  TEST FOR FIREFIGHTER OR FIRECHIEF MASTER           * IH342     
020880          *      SAVE PREMIUM PERCENT, PREMIUM HOURS,           * IH342     
020890          *      AND PREMIUM BONUS HOURS                        * IH342     
020900          *                                                     * IH342     
020910          *******************************************************.IH342     
020920 112170.                                                          IH342     
020930     IF NOT-FF-MSTR,                                              IH342     
020940         GO TO 114070.                                            IH342     
020950     IF FIRE-CHIEF-MSTR,                                          IH342     
020960         GO TO 112400.                                            IH342     
020970     MOVE +144 TO FF-PREM-WORK.                                   IH342     
020980     MOVE +64.0 TO FF-BONUS-HOURS.                                IH342     
020990     IF FF-QRTR-PREM-MSTR,                                        IH342     
021000         MOVE +.150 TO FF-BONUS-PERCENT,                          IH342     
021010             GO TO 114070.                                        IH342     
021020     IF FF-HALF-PREM-MSTR,                                        IH342     
021030         MOVE +.225 TO FF-BONUS-PERCENT,                          IH342     
021040             GO TO 114070.                                        IH342     
021050     IF FF-FULL-PREM-MSTR,                                        IH342     
021060         MOVE +.250 TO FF-BONUS-PERCENT,                          IH342     
021070             GO TO 114070.                                        IH342     
021080 112370.                                                          IH342     
021090         MOVE +.200 TO FF-BONUS-PERCENT.                          IH342     
021100             GO TO 114070.                                        IH342     
021110 112400.                                                          IH342     
021120     MOVE +112 TO FF-PREM-WORK.                                   IH342     
021130     MOVE +32.0 TO FF-BONUS-HOURS.                                IH342     
021140     IF FF-HALF-PREM-MSTR,                                        IH342     
021150         MOVE +.175 TO FF-BONUS-PERCENT,                          IH342     
021160             GO TO 114070.                                        IH342     
021170     IF FF-FULL-PREM-MSTR,                                        IH342     
021180             GO TO 112370.                                        IH342     
021190         MOVE +.150 TO FF-BONUS-PERCENT.                          IH342     
021200     NOTE ******************************************************* IH342     
021210          *                                                     * IH342     
021220          *  TEST FOR MAJOR CONTROL BREAK BY ACTIVITY, OR MINOR * IH342     
021230          *      CONTROL BREAK BY CONTROL GROUP CODE            * IH342     
021240          *          WRITE SMOOTH ROLL PAGE AND ACTIVITY        * IH342     
021250          *          TOTALS                                     * IH342     
021260          *          WRITE PAYROLL CONTROLS                     * IH342     
021270          *                                                     * IH342     
021280          *******************************************************.IH342     
021290 114070.                                                          IH342     
021300     GO TO 114170.                                                IH342     
021310 114090.                                                          IH342     
021320     IF MSTR-MAJ-CNTRL NOT EQUAL TO HOLD-CONTROLS,                IH342     
021330     MOVE HOLD-CONTROLS TO SAVE-HOLD-CONTROLS,                    IH342     
021340         ALTER 372090 TO PROCEED TO 372110,                       IH342     
021350                 GO TO 368080.                                    IH342     
021360 114170.                                                          IH342     
021370     ALTER 114070 TO PROCEED TO 114090.                           IH342     
021380 114190.                                                          IH342     
021390     MOVE MSTR-MAJ-CNTRL TO HOLD-CONTROLS.                        IH342     
021400     NOTE ******************************************************* IH342     
021410          *                                                     * IH342     
021420          *  READ PAYROLL DETAIL FILE                           * IH342     
021430          *      EDIT HOURS AND AMOUNT FIELDS FOR TRUE          * IH342     
021440          *      NUMERIC VALUES                                 * IH342     
021450          *          SEQUENCE CHECK EACH DETAIL                 * IH342     
021460          *                                                     * IH342     
021470          *******************************************************.IH342     
021480 116100.                                                          IH342     
021490     GO TO 116120.                                                IH342     
021500 116120.                                                          IH342     
021510     ALTER 116100 TO PROCEED TO 120170.                           IH342     
021520 116140.                                                          IH342     
021530     READ PAYROLL-DETAIL-FILE,                                    IH342     
021540         AT END GO TO 408010.                                     IH342     
021550     ADD +1 TO DTL-IN.                                            IH342     
021560     ADD +1 TO DTL-IN-GRP.                                        IH342     
021570     EXAMINE DTL-X-HOURS REPLACING ALL " " BY "0".                IH342     
021580     EXAMINE DTL-X-AMOUNT REPLACING ALL " " BY "0".               IH342     
021590     IF DTL-SEQ EQUAL TO SEQ-PAY-DTL,                             IH342     
021600         GO TO 194010.                                            IH342     
021610     IF DTL-SEQ LESS THAN SEQ-PAY-DTL,                            IH342     
021620         GO TO 480000.                                            IH342     
021630         MOVE DTL-SEQ TO SEQ-PAY-DTL.                             IH342     
021640     NOTE ******************************************************* IH342     
021650          *                                                     * IH342     
021660          *  TEST FOR PAYROLL RUN EXCEPTIONS                    * IH342     
021670          *      IF RUN EXCEPTION CODE EQUAL TO PAYROLL DETAIL  * IH342     
021680          *          ACTIVITY CODE BYPASS DETAIL                * IH342     
021690          *                                                     * IH342     
021700          *******************************************************.IH342     
021710 118140.                                                          IH342     
021720     IF EXCEPT-CODES EQUAL TO SPACES,                             IH342     
021730             GO TO 120110.                                        IH342     
021740         MOVE 01 TO SUBS-1.                                       IH342     
021750 118180.                                                          IH342     
021760         IF DTL-ACTY EQUAL TO EXCEPT-TABLE (SUBS-1),              IH342     
021770             MOVE "DETAIL WITH RUN EXCEPTION CODE" TO ERR-EXPLAIN,IH342     
021780                 GO TO 196160.                                    IH342     
021790     IF SUBS-1 NOT EQUAL TO 24,                                   IH342     
021800             ADD 01 TO SUBS-1,                                    IH342     
021810                 GO TO 118180.                                    IH342     
021820     NOTE ******************************************************* IH342     
021830          *                                                     * IH342     
021840          *  MATCH PERSONNEL MASTER VS PAYROLL DETAIL           * IH342     
021850          *                                                     * IH342     
021860          *******************************************************.IH342     
021870 120110.                                                          IH342     
021880     GO TO 120130.                                                IH342     
021890 120130.                                                          IH342     
021900     ALTER 120110 TO PROCEED TO 196010.                           IH342     
021910 120150.                                                          IH342     
021920     MOVE SPACE TO SW1.                                           IH342     
021930 120170.                                                          IH342     
021940     IF EOF-MASTER, GO TO 196140.                                 IH342     
021950     IF EOF-DETAIL, GO TO 236010.                                 IH342     
021960         IF DTL-SEQ LESS THAN MSTR-OUT-SEQ,                       IH342     
021970             GO TO 196140.                                        IH342     
021980         IF MSTR-OUT-SEQ LESS THAN DTL-SEQ,                       IH342     
021990             GO TO 236010.                                        IH342     
022000     MOVE 01 TO SUBS-1.                                           IH342     
022010     MOVE DTL-PAY-CODE (SUBS-1) TO WT-PAY-EXC-HOLD.               IH342     
022020     NOTE ******************************************************* IH342     
022030          *                                                     * IH342     
022040          *  PAYROLL DETAIL MATCHED TO PERSONNEL MASTER         * IH342     
022050          *      AUDIT PAYROLL CODES                            * IH342     
022060          *      EXECUTE SELECTED ROUTINES DEPENDING ON PAYROLL * IH342     
022070          *          DETAIL CODES                               * IH342     
022080          *                                                     * IH342     
022090          *******************************************************.IH342     
022100 122130.                                                          IH342     
022110     MOVE 00 TO SUBS-1.                                           IH342     
022120 122150.                                                          IH342     
022130     ADD 01 TO SUBS-1.                                            IH342     
022140     IF SUBS-1 GREATER THAN 05,                                   IH342     
022150         OR DTL-PAY-CODE (SUBS-1) EQUAL TO SPACES,                IH342     
022160             GO TO 200100.                                        IH342     
022170     IF DTL-PAY-CODE (SUBS-1) IS EQUAL TO "ZZ" GO TO 192090.      IH342     
022180     IF DTL-PAY-CODE (SUBS-1) GREATER THAN 09                     IH342     
022190         AND LESS THAN 66, GO TO 126010.                          IH342     
022200         MOVE 01 TO SUBS-2.                                       IH342     
022210 124020.                                                          IH342     
022220         IF     PAY-CODE (SUBS-1) EQUAL TO PAY-CODE-TBL (SUBS-2), IH342     
022230             GO TO 128010, 130160, 132080, 134140, 136050, 136170,IH342     
022240                   138110, 140060, 142060, 144010, 144140,        IH342     
022250                   144160 144180 145020 145040 145060 145080      IH342     
022260                   146020  146040                                 IH342     
022270                 DEPENDING ON SUBS-2.                             IH342     
022280         IF SUBS-2 NOT EQUAL TO 19                                IH342     
022290                 ADD 01 TO SUBS-2,                                IH342     
022300                     GO TO 124020.                                IH342     
022310 124112.                                                          IH342     
022320         MOVE "INCORRECT PAY CODE" TO ERR-EXPLAIN.                IH342     
022330 124122.                                                          IH342     
022340         ALTER 198140 TO PROCEED TO 122150.                       IH342     
022350             GO TO 198160.                                        IH342     
022360     NOTE ******************************************************* IH342     
022370          *                                                     * IH342     
022380          *  PAYROLL NUMERIC CODES 10 THROUGH 50                * IH342     
022390          *                                                     * IH342     
022400          *******************************************************.IH342     
022410 126010.                                                          IH342     
022420         SUBTRACT 09 FROM DTL-PAY-NUM (SUBS-1), GIVING WORK-CODE. IH342     
022430         GO TO 146070, 148010, 148120, 150010, 150150, 152090,    IH342     
022440               154030, 154130, 156062, 156200, 158130, 160030,    IH342     
022450               160160, 162060, 162180, 164080, 164200, 166100,    IH342     
022460               168010, 168110, 170030, 170130, 172050, 172150,    IH342     
022470               174050, 174150, 176070, 176170, 178070, 178170,    IH342     
022480               180090, 180190, 182110, 184010, 184110, 186090,    IH342     
022490               186190, 188130, 190010, 190170, 192090,            IH342     
022500           192351, 192352, 192353, 192354, 192355, 192356,        IH342     
022510           192357, 192358, 192359, 192360, 192361, 192362,        IH342     
022520           192363, 192364, 192365,                                IH342     
022530             DEPENDING ON WORK-CODE.                              IH342     
022540     NOTE ******************************************************* IH342     
022550          *                                                     * IH342     
022560          *              PAYROLL CODE S                         * IH342     
022570          *  NIGHT PAY FOR SECOND SHIFT                         * IH342     
022580          *          PAYROLL CODE WEEK INDICATOR MUST BE 1 OR 2 * IH342     
022590          *                                                     * IH342     
022600          *******************************************************.IH342     
022610 128010.                                                          IH342     
022620     MOVE 01 TO SUBS-2.                                           IH342     
022630 128014.                                                          IH342     
022640     IF ANNUM-MSTR,                                               IH342     
022650         MULTIPLY MSTR-HOURLY-RATE BY +.10,                       IH342     
022660             GIVING HRLY-RATE-WORK, ROUNDED,                      IH342     
022670         MULTIPLY DTL-HOURS BY HRLY-RATE-WORK, ROUNDED,           IH342     
022680         ADD HRLY-RATE-WORK TO SAVE-NITE-PAY,                     IH342     
022690             GO TO 128100.                                        IH342     
022700 128072.                                                          IH342     
022710     IF PAY-CODE-WEEK (SUBS-1) NOT EQUAL TO "1",                  IH342     
022720         AND PAY-CODE-WEEK (SUBS-1) NOT EQUAL TO "2",             IH342     
022730             MOVE SPACE TO SW4,                                   IH342     
022740                 GO TO 124112.                                    IH342     
022750         MOVE PAY-CODE-WEEK (SUBS-1) TO SAVE-NITE-WEEK.           IH342     
022760         IF NOT OVERTIME  ADD DTL-HOURS TO CAT-C-HOURS.           IH342     
022770         IF THIRD-SHIFT,                                          IH342     
022780             MOVE SPACE TO SW4,                                   IH342     
022790             ADD SHIFT-3 TO DIEM-NITE-PAY,                        IH342     
022800             GO TO 128140.                                        IH342     
022810     ADD SHIFT-2 TO DIEM-NITE-PAY.                                IH342     
022820     GO TO 128180.                                                IH342     
022830 128100.                                                          IH342     
022840     IF NOT OVERTIME                                              IH342     
022850         ADD DTL-HOURS TO CAT-C-HOURS,                            IH342     
022860         ADD HRLY-RATE-WORK TO CAT-C-MONEY.                       IH342     
022870     GO TO 128200.                                                IH342     
022880 128140.                                                          IH342     
022890     IF NOT OVERTIME                                              IH342     
022900         MOVE ZEROS TO CAT-WORK                                   IH342     
022910         MULTIPLY DTL-HOURS BY SHIFT-3 GIVING CAT-WORK, ROUNDED,  IH342     
022920         ADD CAT-WORK TO CAT-C-MONEY.                             IH342     
022930     GO TO 128200.                                                IH342     
022940 128180.                                                          IH342     
022950     IF NOT OVERTIME                                              IH342     
022960         MOVE ZEROS TO CAT-WORK                                   IH342     
022970         MULTIPLY DTL-HOURS BY SHIFT-2 GIVING CAT-WORK, ROUNDED,  IH342     
022980         ADD CAT-WORK TO CAT-C-MONEY.                             IH342     
022990     GO TO 128200.                                                IH342     
023000 128200.                                                          IH342     
023010         ADD DTL-HOURS TO EXC-LIST-TOT (SUBS-2).                  IH342     
023020 130020.                                                          IH342     
023030         MOVE "1" TO SW6.                                         IH342     
023040 130040.                                                          IH342     
023050     MOVE "1" TO SW5.                                             IH342     
023060         GO TO 122150.                                            IH342     
023070     NOTE ******************************************************* IH342     
023080          *                                                     * IH342     
023090          *              PAYROLL CODE T                         * IH342     
023100          *  NIGHT PAY FOR THIRD SHIFT                          * IH342     
023110          *                                                     * IH342     
023120          *******************************************************.IH342     
023130 130160.                                                          IH342     
023140     MOVE 02 TO SUBS-2.                                           IH342     
023150     IF ANNUM-MSTR, GO TO 128014.                                 IH342     
023160         MOVE "1" TO SW4.                                         IH342     
023170             GO TO 128072.                                        IH342     
023180     NOTE ******************************************************* IH342     
023190          *                                                     * IH342     
023200          *              PAYROLL CODE A                         * IH342     
023210          *                                                     * IH342     
023220          *******************************************************.IH342     
023230 132080.                                                          IH342     
023240     MOVE 03 TO SUBS-2.                                           IH342     
023250 132100.                                                          IH342     
023260     IF ANNUM-MSTR, GO TO 124112.                                 IH342     
023270     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO 1, GO TO 133000.          IH342     
023280     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO 2, GO TO 133000.          IH342     
023290     MOVE SPACE TO SW7.                                           IH342     
023300     MOVE SPACE TO SW8.                                           IH342     
023310     GO TO 124112.                                                IH342     
023320 133000.                                                          IH342     
023330     MOVE PAY-CODE-WEEK (SUBS-1) TO SAVE-BONUS-WEEK.              IH342     
023340     IF CODE-B-BONUS,                                             IH342     
023350         MOVE SPACE TO SW7,                                       IH342     
023360         ADD DTL-HOURS TO CAT-C-HOURS                             IH342     
023370         MOVE ZEROS TO CAT-WORK,                                  IH342     
023380         MULTIPLY DTL-HOURS BY DIFF-B GIVING CAT-WORK, ROUNDED,   IH342     
023390         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
023400         ADD DIFF-B TO DIEM-BONUS-PAY, CODE-B-PAY,                IH342     
023410             GO TO 128200.                                        IH342     
023420     IF WHARF-BUILDER,                                            IH342     
023430         MOVE SPACE TO SW8,                                       IH342     
023440         ADD DTL-HOURS TO CAT-C-HOURS                             IH342     
023450         MOVE ZEROS TO CAT-WORK,                                  IH342     
023460         MULTIPLY DTL-HOURS BY WHARF-DIFF GIVING CAT-WORK ROUNDED IH342     
023470         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
023480         ADD WHARF-DIFF TO DIEM-BONUS-PAY,                        IH342     
023490             GO TO 128200.                                        IH342     
023500     ADD DIFF-A TO DIEM-BONUS-PAY, CODE-A-PAY,                    IH342     
023510         ADD DTL-HOURS TO CAT-C-HOURS                             IH342     
023520         MOVE ZEROS TO CAT-WORK,                                  IH342     
023530         MULTIPLY DTL-HOURS BY DIFF-A GIVING CAT-WORK, ROUNDED,   IH342     
023540         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
023550         GO TO 128200.                                            IH342     
023560     NOTE ******************************************************* IH342     
023570          *                                                     * IH342     
023580          *              PAYROLL CODE B                         * IH342     
023590          *                                                     * IH342     
023600          *******************************************************.IH342     
023610 134140.                                                          IH342     
023620     MOVE 04 TO SUBS-2.                                           IH342     
023630     MOVE "1" TO SW7.                                             IH342     
023640         GO TO 132100.                                            IH342     
023650     NOTE ******************************************************* IH342     
023660          *                                                     * IH342     
023670          *              PAYROLL CODE W                         * IH342     
023680          *              WHARF  BUILDER                         * IH342     
023690          *                                                     * IH342     
023700          *******************************************************.IH342     
023710 136050.                                                          IH342     
023720     MOVE 28 TO SUBS-2.                                           IH342     
023730     MOVE "1" TO SW8.                                             IH342     
023740         GO TO 132100.                                            IH342     
023750     NOTE ******************************************************* IH342     
023760          *                                                     * IH342     
023770          *              PAYROLL CODE E                         * IH342     
023780          *  DIVER PAY, REPLACES ANY REGULAR HOURLY RATE        * IH342     
023790          *                                                     * IH342     
023800          *******************************************************.IH342     
023810 136170.                                                          IH342     
023820     IF ANNUM-MSTR,                                               IH342     
023830         OR (PAY-CODE-WEEK (SUBS-1) NOT EQUAL TO "1",             IH342     
023840             AND PAY-CODE-WEEK (SUBS-1) NOT EQUAL TO "2" ),       IH342     
023850                 GO TO 124112.                                    IH342     
023860     MOVE PAY-CODE-WEEK (SUBS-1) TO SAVE-DIVER-WEEK.              IH342     
023870     MOVE 05 TO SUBS-2.                                           IH342     
023880     MOVE "1" TO SW9.                                             IH342     
023890         GO TO 128200.                                            IH342     
023900     NOTE ******************************************************* IH342     
023910          *                                                     * IH342     
023920          *              PAYROLL CODE P                         * IH342     
023930          *                                                     * IH342     
023940          *******************************************************.IH342     
023950 138110.                                                          IH342     
023960     IF PAY-CODE-WEEK (SUBS-1) NOT EQUAL TO "1",                  IH342     
023970         AND PAY-CODE-WEEK (SUBS-1) NOT EQUAL TO "2",             IH342     
023980             GO TO 124112.                                        IH342     
023990     MOVE PAY-CODE-WEEK (SUBS-1) TO SAVE-SUNDAY-WEEK.             IH342     
024000     MOVE 27 TO SUBS-2.                                           IH342     
024010     MOVE "1" TO SW32.                                            IH342     
024020         GO TO 128200.                                            IH342     
024030     NOTE ******************************************************* IH342     
024040          *                                                     * IH342     
024050          *              PAYROLL CODE C                         * IH342     
024060          *  OVERTIME ROUTINE                                   * IH342     
024070          *      SPLIT HOURS BY WEEK INDICATOR FOR WEEK 1 AND 2 * IH342     
024080          *                                                     * IH342     
024090          *******************************************************.IH342     
024100 140060.                                                          IH342     
024110     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO "1",                      IH342     
024120         ADD DTL-HOURS TO OT-HRS-WK1,                             IH342     
024130             GO TO 140140.                                        IH342     
024140     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO "2",                      IH342     
024150         ADD DTL-HOURS TO OT-HRS-WK2,                             IH342     
024160             GO TO 140140.                                        IH342     
024170         GO TO 124112.                                            IH342     
024180 140140.                                                          IH342     
024190     MOVE PAY-CODE-WEEK (SUBS-1) TO SAVE-OT-WEEK.                 IH342     
024200     MOVE "1" TO SW10, SW6.                                       IH342     
024210         GO TO 122150.                                            IH342     
024220     NOTE ******************************************************* IH342     
024230          *                                                     * IH342     
024240          *              PAYROLL CODE G                         * IH342     
024250          *  LWOP ROUTINE                                       * IH342     
024260          *      SAVE LWOP HOURS ACCORDING TO WEEK INDICATOR    * IH342     
024270          *                                                     * IH342     
024280          *******************************************************.IH342     
024290 142060.                                                          IH342     
024300     MOVE "F" TO WORK-OTH-PAY-CODE.                               IH342     
024310     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO "1",                      IH342     
024320         ADD DTL-HOURS TO LWOP-WK1,                               IH342     
024330             GO TO 122150.                                        IH342     
024340     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO "2",                      IH342     
024350         ADD DTL-HOURS TO LWOP-WK2,                               IH342     
024360             GO TO 122150.                                        IH342     
024370         GO TO 124112.                                            IH342     
024380     NOTE ******************************************************* IH342     
024390          *                                                     * IH342     
024400          *              PAYROLL CODE F                         * IH342     
024410          *  HOLIDAY WORKED AND PAID                            * IH342     
024420          *                                                     * IH342     
024430          *******************************************************.IH342     
024440 144010.                                                          IH342     
024450     MOVE 06 TO SUBS-2.                                           IH342     
024460     MOVE "1" TO SW11.                                            IH342     
024470 144032.                                                          IH342     
024480     ADD DTL-HOURS TO OTHER-HOURS.                                IH342     
024490         GO TO 128200.                                            IH342     
024500     NOTE ******************************************************* IH342     
024510          *                                                     * IH342     
024520          *              PAYROLL CODE H                         * IH342     
024530          *  HOLIDAY INCLUDED IN ANNUAL LEAVE PAYOFF            * IH342     
024540          *      INCREASE OTHER HOURS FOR EACH INCLUDED HOLIDAY * IH342     
024550          *                                                     * IH342     
024560          *******************************************************.IH342     
024570 144140.                                                          IH342     
024580     MOVE 21 TO SUBS-2.                                           IH342     
024590     MOVE "1" TO SW12.                                            IH342     
024600     GO TO 144032.                                                IH342     
024610     NOTE ******************************************************* IH342     
024620          *                                                     * IH342     
024630          *              PAYROLL CODE L                         * IH342     
024640          *  NITE PAY FOR SECOND SHIFT WHEN WORKING OVERTIME    * IH342     
024650          *                                                     * IH342     
024660          *******************************************************.IH342     
024670 144160.                                                          IH342     
024680     IF ANNUM-MSTR GO TO 124112.                                  IH342     
024690     IF PAY-CODE-WEEK (SUBS-1) EQUALS 1 OR 2 GO TO 144165.        IH342     
024700     GO TO 124112.                                                IH342     
024710 144165.                                                          IH342     
024720     MULTIPLY DTL-HOURS BY SHIFT-2 GIVING HRLY-RATE-WORK ROUNDED. IH342     
024730     MOVE 38 TO SUBS-2.                                           IH342     
024740     MOVE 1 TO SW51.                                              IH342     
024750 144170.                                                          IH342     
024760     ADD HRLY-RATE-WORK TO OT-MONEY.                              IH342     
024770     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
024780     GO TO 128200.                                                IH342     
024790     NOTE ******************************************************* IH342     
024800          *                                                     * IH342     
024810          *              PAYROLL CODE M                         * IH342     
024820          *   NITE PAY FOR THIRD SHIFT WHEN WORKING OVERTIME    * IH342     
024830          *                                                     * IH342     
024840          *******************************************************.IH342     
024850 144180.                                                          IH342     
024860     IF ANNUM-MSTR GO TO 124112.                                  IH342     
024870     IF PAY-CODE-WEEK (SUBS-1) EQUALS 1 OR 2 GO TO 144185.        IH342     
024880     GO TO 124112.                                                IH342     
024890 144185.                                                          IH342     
024900     MULTIPLY DTL-HOURS BY SHIFT-3 GIVING HRLY-RATE-WORK ROUNDED. IH342     
024910     MOVE 39 TO SUBS-2.                                           IH342     
024920     MOVE 1 TO SW52.                                              IH342     
024930     GO TO 144170.                                                IH342     
024940     NOTE ******************************************************* IH342     
024950          *                                                     * IH342     
024960          *              PAYROLL CODE J                         * IH342     
024970          *     CODE J BONUS PAY                                * IH342     
024980          *                                                     * IH342     
024990          *******************************************************.IH342     
025000 145020.                                                          IH342     
025010     MOVE 42 TO SUBS-2.                                           IH342     
025020 145030.                                                          IH342     
025030     IF ANNUM-MSTR GO TO 124112.                                  IH342     
025040     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO 1 GO TO 145035.           IH342     
025050     IF PAY-CODE-WEEK (SUBS-1) EQUAL TO 2 GO TO 145035.           IH342     
025060     MOVE SPACE TO SW53 SW54 SW55.                                IH342     
025070     GO TO 124112.                                                IH342     
025080 145035.                                                          IH342     
025090     MOVE PAY-CODE-WEEK (SUBS-1) TO SAVE-BONUS-WEEK.              IH342     
025100     IF CODE-K-BONUS                                              IH342     
025110         MOVE SPACE TO SW53                                       IH342     
025120         ADD DTL-HOURS TO CAT-C-HOURS                             IH342     
025130         MULTIPLY DTL-HOURS BY DIFF-K GIVING CAT-WORK ROUNDED     IH342     
025140         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
025150         ADD DIFF-K TO DIEM-BONUS-PAY                             IH342     
025160         GO TO 128200.                                            IH342     
025170     IF CODE-N-BONUS                                              IH342     
025180         MOVE SPACE TO SW54                                       IH342     
025190         ADD DTL-HOURS TO CAT-C-HOURS                             IH342     
025200         MULTIPLY DTL-HOURS BY DIFF-N GIVING CAT-WORK ROUNDED     IH342     
025210         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
025220         ADD DIFF-N TO DIEM-BONUS-PAY                             IH342     
025230         GO TO 128200.                                            IH342     
025240     IF CODE-R-BONUS                                              IH342     
025250         MOVE SPACE TO SW55                                       IH342     
025260         ADD DTL-HOURS TO CAT-C-HOURS                             IH342     
025270         MULTIPLY DTL-HOURS BY DIFF-R GIVING CAT-WORK ROUNDED     IH342     
025280         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
025290         ADD DIFF-R TO DIEM-BONUS-PAY                             IH342     
025300         GO TO 128200.                                            IH342     
025310     ADD DTL-HOURS TO CAT-C-HOURS                                 IH342     
025320         MULTIPLY DTL-HOURS BY DIFF-J GIVING CAT-WORK ROUNDED     IH342     
025330         ADD CAT-WORK TO CAT-C-MONEY                              IH342     
025340         ADD DIFF-J TO DIEM-BONUS-PAY                             IH342     
025350         GO TO 128200.                                            IH342     
025360     NOTE ******************************************************* IH342     
025370          *                                                     * IH342     
025380          *              PAYROLL CODE K                         * IH342     
025390          *        CODE K BONUS PAY                             * IH342     
025400          *                                                     * IH342     
025410          *******************************************************.IH342     
025420 145040.                                                          IH342     
025430     MOVE 43 TO SUBS-2.                                           IH342     
025440     MOVE 1 TO SW53.                                              IH342     
025450     GO TO 145030.                                                IH342     
025460     NOTE ******************************************************* IH342     
025470          *                                                     * IH342     
025480          *              PAYROLL CODE N                         * IH342     
025490          *        CODE N BONUS PAY                             * IH342     
025500          *                                                     * IH342     
025510          *******************************************************.IH342     
025520 145060.                                                          IH342     
025530     MOVE 44 TO SUBS-2.                                           IH342     
025540     MOVE 1 TO SW54.                                              IH342     
025550     GO TO 145030.                                                IH342     
025560     NOTE ******************************************************* IH342     
025570          *                                                     * IH342     
025580          *              PAYROLL CODE R                         * IH342     
025590          *        CODE R BONUS PAY                             * IH342     
025600          *                                                     * IH342     
025610          *******************************************************.IH342     
025620 145080.                                                          IH342     
025630     MOVE 45 TO SUBS-2.                                           IH342     
025640     MOVE 1 TO SW55.                                              IH342     
025650     GO TO 145030.                                                IH342     
025660     NOTE ******************************************************* IH342     
025670          *                                                     * IH342     
025680          *              PAYROLL CODE X                         * IH342     
025690          *   CODE A BONUS WHEN WORKING OVERTIME                * IH342     
025700          *                                                     * IH342     
025710          *******************************************************.IH342     
025720 146020.                                                          IH342     
025730     MOVE 46 TO SUBS-2.                                           IH342     
025740 146025.                                                          IH342     
025750     IF ANNUM-MSTR GO TO 124112.                                  IH342     
025760     IF PAY-CODE-WEEK (SUBS-1) EQUALS 1 OR 2 GO TO 146030.        IH342     
025770     MOVE SPACE TO SW56.                                          IH342     
025780     GO TO 124112.                                                IH342     
025790 146030.                                                          IH342     
025800     IF CODE-B-OT                                                 IH342     
025810         MOVE SPACE TO SW56                                       IH342     
025820         MULTIPLY DTL-HOURS BY DIFF-B GIVING HRLY-RATE-WORK       IH342     
025830             ROUNDED                                              IH342     
025840         GO TO 146035.                                            IH342     
025850     MULTIPLY DTL-HOURS BY DIFF-A GIVING HRLY-RATE-WORK ROUNDED.  IH342     
025860 146035.                                                          IH342     
025870     MULTIPLY +1.5 BY HRLY-RATE-WORK ROUNDED.                     IH342     
025880     ADD HRLY-RATE-WORK TO OT-MONEY.                              IH342     
025890     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
025900     GO TO 128200.                                                IH342     
025910     NOTE ******************************************************* IH342     
025920          *                                                     * IH342     
025930          *              PAYROLL CODE Y                         * IH342     
025940          *   CODE B BONUS WHEN WORKING OVERTIME                * IH342     
025950          *                                                     * IH342     
025960          *******************************************************.IH342     
025970 146040.                                                          IH342     
025980     MOVE 47 TO SUBS-2.                                           IH342     
025990     MOVE 1 TO SW56                                               IH342     
026000     GO TO 146025.                                                IH342     
026010     NOTE ******************************************************* IH342     
026020          *                                                     * IH342     
026030          *              PAYROLL CODE 10                        * IH342     
026040          *           SPECIAL TAX ADJUSTMENT                    * IH342     
026050          *           20% GROSS FEDERAL TAX                     * IH342     
026060          *           3% GROSS CALIF STATE TAX                  * IH342     
026070          *                                                     * IH342     
026080          *******************************************************.IH342     
026090 146070.                                                          IH342     
026100     MOVE 1 TO SW57.                                              IH342     
026110     MOVE CODE10-MSG TO ERR-EXPLAIN.                              IH342     
026120     MOVE 1 TO SW31.                                              IH342     
026130     GO TO 124122.                                                IH342     
026140     NOTE ******************************************************* IH342     
026150          *                                                     * IH342     
026160          *              PAYROLL CODE 11                        * IH342     
026170          *  CANCEL LIFE INSURANCE AND LIFE INSURANCE OPTION    * IH342     
026180          *      DEDUCTIONS FOR CURRENT PAY PERIOD              * IH342     
026190          *                                                     * IH342     
026200          *******************************************************.IH342     
026210 148010.                                                          IH342     
026220     MOVE "1" TO SW13.                                            IH342     
026230         GO TO 122150.                                            IH342     
026240     NOTE ******************************************************* IH342     
026250          *                                                     * IH342     
026260          *              PAYROLL CODE 12                        * IH342     
026270          *  CANCEL HEALTH INSURANCE DEDUCTION FOR CURRENT      * IH342     
026280          *      PAY PERIOD                                     * IH342     
026290          *                                                     * IH342     
026300          *******************************************************.IH342     
026310 148120.                                                          IH342     
026320     MOVE "1" TO SW14.                                            IH342     
026330         GO TO 122150.                                            IH342     
026340     NOTE ******************************************************* IH342     
026350          *                                                     * IH342     
026360          *              PAYROLL CODE 13                        * IH342     
026370          *  TAX LEVY DEDUCTION                                 * IH342     
026380          *                                                     * IH342     
026390          *******************************************************.IH342     
026400 150010.                                                          IH342     
026410     MOVE DTL-AMOUNT TO TAX-LEVY-DED.                             IH342     
026420     MOVE "1" TO SW15.                                            IH342     
026430     MOVE 22 TO SUBS-2.                                           IH342     
026440 150050.                                                          IH342     
026450     ADD DTL-AMOUNT TO EXC-LIST-TOT (SUBS-2).                     IH342     
026460         GO TO 130040.                                            IH342     
026470     NOTE ******************************************************* IH342     
026480          *                                                     * IH342     
026490          *              PAYROLL CODE 14                        * IH342     
026500          *  RETROACTIVE REGULAR HOURS                          * IH342     
026510          *                                                     * IH342     
026520          *******************************************************.IH342     
026530 150150.                                                          IH342     
026540     ADD DTL-HOURS TO ADJ-REG-HRS.                                IH342     
026550     MOVE "1" TO SW16.                                            IH342     
026560     MOVE 20 TO SUBS-2.                                           IH342     
026570         GO TO 128200.                                            IH342     
026580     NOTE ******************************************************* IH342     
026590          *                                                     * IH342     
026600          *              PAYROLL CODE 15                        * IH342     
026610          *  80TH HOUR WITH ADDED PAY                           * IH342     
026620          *      INCREASE LIFE INSURANCE DEDUCTION IF PER DIEM  * IH342     
026630          *      IF NOT INSURED, BYPASS CODE                    * IH342     
026640          *                                                     * IH342     
026650          *******************************************************.IH342     
026660 152090.                                                          IH342     
026670     IF ANNUM-MSTR,                                               IH342     
026680         GO TO 124112.                                            IH342     
026690     IF LIFE-INSUR-MSTR,                                          IH342     
026700         ADD +.28 TO ADJ-LIFE-INS                                 IH342     
026710         MOVE 1 TO SW23.                                          IH342     
026720             GO TO 122150.                                        IH342     
026730     NOTE ******************************************************* IH342     
026740          *                                                     * IH342     
026750          *              PAYROLL CODE 16                        * IH342     
026760          *  PAID BY VOUCHER                                    * IH342     
026770          *      NO CHECK NUMBER                                * IH342     
026780          *                                                     * IH342     
026790          *******************************************************.IH342     
026800 154030.                                                          IH342     
026810     MOVE "1" TO SW18.                                            IH342     
026820     ADD DTL-AMOUNT TO VOUCHER-PAID.                              IH342     
026830         GO TO 122150.                                            IH342     
026840     NOTE ******************************************************* IH342     
026850          *                                                     * IH342     
026860          *              PAYROLL CODE 17                        * IH342     
026870          *  PLUS SUBJECT MONEY ADJUSTMENT ROUTINE              * IH342     
026880          *                                                     * IH342     
026890          *******************************************************.IH342     
026900 154130.                                                          IH342     
026910     MOVE 07 TO SUBS-2.                                           IH342     
026920     ADD DTL-AMOUNT TO ADJ-WK-SUB.                                IH342     
026930         GO TO 150050.                                            IH342     
026940     NOTE ******************************************************* IH342     
026950          *                                                     * IH342     
026960          *              PAYROLL CODE 18                        * IH342     
026970          *  MINUS SUBJECT MONEY ADJUSTMENT ROUTINE             * IH342     
026980          *                                                     * IH342     
026990          *******************************************************.IH342     
027000 156062.                                                          IH342     
027010     ALTER 156110 TO PROCEED TO 154130.                           IH342     
027020 156080.                                                          IH342     
027030     MULTIPLY -1.0 BY DTL-HOURS.                                  IH342     
027040     MULTIPLY -1.00 BY DTL-AMOUNT.                                IH342     
027050 156110.                                                          IH342     
027060     GO TO 154130.                                                IH342     
027070     NOTE ******************************************************* IH342     
027080          *                                                     * IH342     
027090          *              PAYROLL CODE 19                        * IH342     
027100          *  PLUS REGULAR MONEY ADJUSTMENT ROUTINE              * IH342     
027110          *                                                     * IH342     
027120          *******************************************************.IH342     
027130 156200.                                                          IH342     
027140     MOVE 08 TO SUBS-2.                                           IH342     
027150     ADD DTL-AMOUNT TO ADJ-REG-MONEY, CAT-A-MONEY.                IH342     
027160     ADD DTL-HOURS TO ADJ-REG-HRS, CAT-A-HOURS.                   IH342     
027170         GO TO 150050.                                            IH342     
027180     NOTE ******************************************************* IH342     
027190          *                                                     * IH342     
027200          *              PAYROLL CODE 20                        * IH342     
027210          *  MINUS REGUALAR MONEY ADJUSTMENT ROUTINE            * IH342     
027220          *                                                     * IH342     
027230          *******************************************************.IH342     
027240 158130.                                                          IH342     
027250     ALTER 156110 TO PROCEED TO 156200.                           IH342     
027260         GO TO 156080.                                            IH342     
027270     NOTE ******************************************************* IH342     
027280          *                                                     * IH342     
027290          *              PAYROLL CODE 21                        * IH342     
027300          *  PLUS OTHER MONEY ADJUSTMENT ROUTINE                * IH342     
027310          *                                                     * IH342     
027320          *******************************************************.IH342     
027330 160030.                                                          IH342     
027340     MOVE 09 TO SUBS-2.                                           IH342     
027350     ADD DTL-AMOUNT TO ADJ-OTHER-MONEY.                           IH342     
027360     IF MSTR-OTHER-PAY-CODE EQUALS "P" OR "S"                     IH342     
027370         ADD DTL-AMOUNT TO CAT-H-MONEY GO TO 160035.              IH342     
027380     IF MSTR-OTHER-PAY-CODE EQUALS "A" OR "Q"                     IH342     
027390         ADD DTL-AMOUNT TO CAT-L-MONEY GO TO 160035.              IH342     
027400     ADD DTL-AMOUNT TO CAT-C-MONEY.                               IH342     
027410     ADD DTL-HOURS TO CAT-C-HOURS.                                IH342     
027420 160035.                                                          IH342     
027430     ADD DTL-HOURS TO OTHER-HOURS.                                IH342     
027440         GO TO 150050.                                            IH342     
027450     NOTE ******************************************************* IH342     
027460          *                                                     * IH342     
027470          *              PAYROLL CODE 22                        * IH342     
027480          *  MINUS OTHER MONEY ADJUSTMENT ROUTINE               * IH342     
027490          *                                                     * IH342     
027500          *******************************************************.IH342     
027510 160160.                                                          IH342     
027520     ALTER 156110 TO PROCEED TO 160030.                           IH342     
027530         GO TO 156080.                                            IH342     
027540     NOTE ******************************************************* IH342     
027550          *                                                     * IH342     
027560          *              PAYROLL CODE 23                        * IH342     
027570          *  PLUS OVERTIME MONEY ADJUSTMENT ROUTINE             * IH342     
027580          *                                                     * IH342     
027590          *******************************************************.IH342     
027600 162060.                                                          IH342     
027610     MOVE 10 TO SUBS-2.                                           IH342     
027620     ADD DTL-AMOUNT TO ADJ-OT-MONEY.                              IH342     
027630     ADD DTL-HOURS TO ADJ-OT-HOURS.                               IH342     
027640         GO TO 150050.                                            IH342     
027650     NOTE ******************************************************* IH342     
027660          *                                                     * IH342     
027670          *              PAYROLL CODE 24                        * IH342     
027680          *  MINUS OVERTIME MONEY ADJUSTMENT ROUTINE            * IH342     
027690          *                                                     * IH342     
027700          *******************************************************.IH342     
027710 162180.                                                          IH342     
027720     ALTER 156110 TO PROCEED TO 162060.                           IH342     
027730         GO TO 156080.                                            IH342     
027740     NOTE ******************************************************* IH342     
027750          *                                                     * IH342     
027760          *              PAYROLL CODE 25                        * IH342     
027770          *  PLUS CSRA DEDUCTION ADJUSTMENT ROUTINE             * IH342     
027780          *                                                     * IH342     
027790          *******************************************************.IH342     
027800 164080.                                                          IH342     
027810     MOVE 11 TO SUBS-2.                                           IH342     
027820     MOVE "1" TO SW19.                                            IH342     
027830 164102.                                                          IH342     
027840     ADD  DTL-AMOUNT TO ADJ-RETIRE.                               IH342     
027850         GO TO 150050.                                            IH342     
027860     NOTE ******************************************************* IH342     
027870          *                                                     * IH342     
027880          *              PAYROLL CODE 26                        * IH342     
027890          *  MINUS CSRA DEDUCTION ROUTINE                       * IH342     
027900          *                                                     * IH342     
027910          *******************************************************.IH342     
027920 164200.                                                          IH342     
027930     ALTER 156110 TO PROCEED TO 164080.                           IH342     
027940         GO TO 156080.                                            IH342     
027950     NOTE ******************************************************* IH342     
027960          *                                                     * IH342     
027970          *              PAYROLL CODE 27                        * IH342     
027980          *  PLUS FICA DEDUCTION ADJUSTMENT ROUTINE             * IH342     
027990          *                                                     * IH342     
028000          *******************************************************.IH342     
028010 166100.                                                          IH342     
028020     MOVE 12 TO SUBS-2.                                           IH342     
028030     MOVE "1" TO SW20.                                            IH342     
028040         GO TO 164102.                                            IH342     
028050     NOTE ******************************************************* IH342     
028060          *                                                     * IH342     
028070          *              PAYROLL CODE 28                        * IH342     
028080          *  MINUS FICA DEDUCTION ADJUSTMENT ROUTINE            * IH342     
028090          *                                                     * IH342     
028100          *******************************************************.IH342     
028110 168010.                                                          IH342     
028120     ALTER 156110 TO PROCEED TO 166100.                           IH342     
028130         GO TO 156080.                                            IH342     
028140     NOTE ******************************************************* IH342     
028150          *                                                     * IH342     
028160          *              PAYROLL CODE 29                        * IH342     
028170          *  PLUS TAX DEDUCTION ADJUSTMENT ROUTINE              * IH342     
028180          *                                                     * IH342     
028190          *******************************************************.IH342     
028200 168110.                                                          IH342     
028210     MOVE 14 TO SUBS-2.                                           IH342     
028220     MOVE "1" TO SW21.                                            IH342     
028230     ADD  DTL-AMOUNT TO ADJ-TAXES.                                IH342     
028240         GO TO 150050.                                            IH342     
028250     NOTE ******************************************************* IH342     
028260          *                                                     * IH342     
028270          *              PAYROLL CODE 30                        * IH342     
028280          *  MINUS TAX DEDUCTION ADJUSTMENT ROUTINE             * IH342     
028290          *                                                     * IH342     
028300          *******************************************************.IH342     
028310 170030.                                                          IH342     
028320     ALTER 156110 TO PROCEED TO 168110.                           IH342     
028330         GO TO 156080.                                            IH342     
028340     NOTE ******************************************************* IH342     
028350          *                                                     * IH342     
028360          *              PAYROLL CODE 31                        * IH342     
028370          *  PLUS HEALTH INSURANCE DEDUCTION ADJUSTMENT ROUTINE * IH342     
028380          *                                                     * IH342     
028390          *******************************************************.IH342     
028400 170130.                                                          IH342     
028410     MOVE 13 TO SUBS-2.                                           IH342     
028420     ADD  DTL-AMOUNT TO ADJ-HEALTH-DED.                           IH342     
028430     MOVE "1" TO SW22.                                            IH342     
028440         GO TO 150050.                                            IH342     
028450     NOTE ******************************************************* IH342     
028460          *                                                     * IH342     
028470          *              PAYROLL CODE 32                        * IH342     
028480          *  MINUS HEALTH INSURANCE DEDUCTION ADJUST ROUTINE    * IH342     
028490          *                                                     * IH342     
028500          *******************************************************.IH342     
028510 172050.                                                          IH342     
028520     ALTER 156110 TO PROCEED TO 170130.                           IH342     
028530         GO TO 156080.                                            IH342     
028540     NOTE ******************************************************* IH342     
028550          *                                                     * IH342     
028560          *              PAYROLL CODE 33                        * IH342     
028570          *  PLUS HEALTH INSURANCE CONTRIB ADJUSTMENT ROUTINE   * IH342     
028580          *                                                     * IH342     
028590          *******************************************************.IH342     
028600 172150.                                                          IH342     
028610     ADD  DTL-AMOUNT TO ADJ-HEALTH-CON.                           IH342     
028620     MOVE "1" TO SW22.                                            IH342     
028630         GO TO 122150.                                            IH342     
028640     NOTE ******************************************************* IH342     
028650          *                                                     * IH342     
028660          *              PAYROLL CODE 34                        * IH342     
028670          *  MINUS HEALTH INSURANCE CONTRIB ADJUSTMENT ROUTINE  * IH342     
028680          *                                                     * IH342     
028690          *******************************************************.IH342     
028700 174050.                                                          IH342     
028710     ALTER 156110 TO PROCEED TO 172150.                           IH342     
028720         GO TO 156080.                                            IH342     
028730     NOTE ******************************************************* IH342     
028740          *                                                     * IH342     
028750          *              PAYROLL CODE 35                        * IH342     
028760          *  PLUS LIFE INSURANCE DEDUCTION ADJUSTMENT ROUTINE   * IH342     
028770          *                                                     * IH342     
028780          *******************************************************.IH342     
028790 174150.                                                          IH342     
028800     ADD  DTL-AMOUNT TO ADJ-LIFE-INS.                             IH342     
028810     MOVE "1" TO SW23.                                            IH342     
028820 174182.                                                          IH342     
028830     MOVE 15 TO SUBS-2.                                           IH342     
028840         GO TO 150050.                                            IH342     
028850     NOTE ******************************************************* IH342     
028860          *                                                     * IH342     
028870          *              PAYROLL CODE 36                        * IH342     
028880          *  MINUS LIFE INSURANCE DEDUCTION ADJUSTMENT ROUTINE  * IH342     
028890          *                                                     * IH342     
028900          *******************************************************.IH342     
028910 176070.                                                          IH342     
028920     ALTER 156110 TO PROCEED TO 174150.                           IH342     
028930         GO TO 156080.                                            IH342     
028940     NOTE ******************************************************* IH342     
028950          *                                                     * IH342     
028960          *              PAYROLL CODE 37                        * IH342     
028970          *  PLUS LIFE INSURANCE OPTION DEDUCT ADJUST ROUTINE   * IH342     
028980          *                                                     * IH342     
028990          *******************************************************.IH342     
029000 176170.                                                          IH342     
029010     ADD  DTL-AMOUNT TO ADJ-LIFE-OPT.                             IH342     
029020     MOVE "1" TO SW24.                                            IH342     
029030         GO TO 174182.                                            IH342     
029040     NOTE ******************************************************* IH342     
029050          *                                                     * IH342     
029060          *              PAYROLL CODE 38                        * IH342     
029070          *  MINUS LIFE INSURANCE OPTION DEDUCT ADJUST ROUTINE  * IH342     
029080          *                                                     * IH342     
029090          *******************************************************.IH342     
029100 178070.                                                          IH342     
029110     ALTER 156110 TO PROCEED TO 176170.                           IH342     
029120         GO TO 156080.                                            IH342     
029130     NOTE ******************************************************* IH342     
029140          *                                                     * IH342     
029150          *              PAYROLL CODE 39                        * IH342     
029160          *  PLUS STATE TAXES ADJUSTMENT ROUTINE                * IH342     
029170          *                                                     * IH342     
029180          *******************************************************.IH342     
029190 178170.                                                          IH342     
029200     MOVE 16 TO SUBS-2.                                           IH342     
029210     ADD  DTL-AMOUNT TO ADJ-STATE.                                IH342     
029220     MOVE "1" TO SW25.                                            IH342     
029230         GO TO 150050.                                            IH342     
029240     NOTE ******************************************************* IH342     
029250          *                                                     * IH342     
029260          *              PAYROLL CODE 40                        * IH342     
029270          *  MINUS STATE TAXES ADJUSTMENT ROUTINE               * IH342     
029280          *                                                     * IH342     
029290          *******************************************************.IH342     
029300 180090.                                                          IH342     
029310     ALTER 156110 TO PROCEED TO 178170.                           IH342     
029320         GO TO 156080.                                            IH342     
029330     NOTE ******************************************************* IH342     
029340          *                                                     * IH342     
029350          *              PAYROLL CODE 41                        * IH342     
029360          *  PLUS UNION DUES DEDUCTION ADJUSTMENT ROUTINE       * IH342     
029370          *                                                     * IH342     
029380          *******************************************************.IH342     
029390 180190.                                                          IH342     
029400     MOVE 17 TO SUBS-2.                                           IH342     
029410     ADD  DTL-AMOUNT TO ADJ-UNION.                                IH342     
029420     MOVE "1" TO SW26.                                            IH342     
029430         GO TO 150050.                                            IH342     
029440     NOTE ******************************************************* IH342     
029450          *                                                     * IH342     
029460          *              PAYROLL CODE 42                        * IH342     
029470          *  MINUS UNION DUES DEDUCTION ADJUSTMENT ROUTINE      * IH342     
029480          *                                                     * IH342     
029490          *******************************************************.IH342     
029500 182110.                                                          IH342     
029510     ALTER 156110 TO PROCEED TO 180190.                           IH342     
029520         GO TO 156080.                                            IH342     
029530     NOTE ******************************************************* IH342     
029540          *                                                     * IH342     
029550          *              PAYROLL CODE 43                        * IH342     
029560          *  PLUS GOVERNMENT INDEBTED TOTAL ADJUSTMENT ROUTINE  * IH342     
029570          *                                                     * IH342     
029580          *******************************************************.IH342     
029590 184010.                                                          IH342     
029600     ADD DTL-AMOUNT TO MSTR-YTD-GOV-INDEBT.                       IH342     
029610         GO TO 122150.                                            IH342     
029620     NOTE ******************************************************* IH342     
029630          *                                                     * IH342     
029640          *              PAYROLL CODE 44                        * IH342     
029650          *  MINUS GOVERNMENT INDEBTED TOTAL ADJUSTMENT ROUTINE * IH342     
029660          *                                                     * IH342     
029670          *******************************************************.IH342     
029680 184110.                                                          IH342     
029690     SUBTRACT DTL-AMOUNT FROM MSTR-YTD-GOV-INDEBT.                IH342     
029700     IF MSTR-YTD-GOV-INDEBT POSITIVE,                             IH342     
029710         GO TO 122150.                                            IH342     
029720     MOVE MSTR-YTD-GOV-INDEBT TO MSG2-AMT.                        IH342     
029730     MOVE +0000.00 TO MSTR-YTD-GOV-INDEBT.                        IH342     
029740     MOVE "INDEBTED" TO MSG2-KON.                                 IH342     
029750 184180.                                                          IH342     
029760     MOVE +000.00 TO MSTR-YTD-GOV-PAY.                            IH342     
029770     MOVE MESSAGE2 TO ERR-EXPLAIN.                                IH342     
029780         GO TO 124122.                                            IH342     
029790     NOTE ******************************************************* IH342     
029800          *                                                     * IH342     
029810          *              PAYROLL CODE 45                        * IH342     
029820          *  PLUS GOVERNMENT PAYMENT ADJUSTMENT ROUTINE         * IH342     
029830          *                                                     * IH342     
029840          *******************************************************.IH342     
029850 186090.                                                          IH342     
029860     ADD DTL-AMOUNT TO MSTR-YTD-GOV-PAY.                          IH342     
029870         GO TO 122150.                                            IH342     
029880     NOTE ******************************************************* IH342     
029890          *                                                     * IH342     
029900          *              PAYROLL CODE 46                        * IH342     
029910          *  MINUS GOVERNMENT PAYMENT ADJUSTMENT ROUTINE        * IH342     
029920          *                                                     * IH342     
029930          *******************************************************.IH342     
029940 186190.                                                          IH342     
029950     SUBTRACT DTL-AMOUNT FROM MSTR-YTD-GOV-PAY.                   IH342     
029960     IF MSTR-YTD-GOV-PAY POSITIVE,                                IH342     
029970         GO TO 122150.                                            IH342     
029980     MOVE "PAYMENTS" TO MSG2-KON.                                 IH342     
029990     MOVE MSTR-YTD-GOV-PAY TO MSG2-AMT.                           IH342     
030000         GO TO 184180.                                            IH342     
030010     NOTE ******************************************************* IH342     
030020          *                                                     * IH342     
030030          *              PAYROLL CODE 47                        * IH342     
030040          *  CANCEL BOND DEDUCTION FOR CURRENT PAY PERIOD       * IH342     
030050          *                                                     * IH342     
030060          *******************************************************.IH342     
030070 188130.                                                          IH342     
030080     MOVE "1" TO SW27.                                            IH342     
030090         GO TO 122150.                                            IH342     
030100     NOTE ******************************************************* IH342     
030110          *                                                     * IH342     
030120          *              PAYROLL CODE 48                        * IH342     
030130          *  DECEASED ANNUAL LEAVE PAYOFF                       * IH342     
030140          *      INACTIVATE PERSONNEL MASTER RECORD             * IH342     
030150          *                                                     * IH342     
030160          *******************************************************.IH342     
030170 190010.                                                          IH342     
030180     MOVE "1" TO SW28.                                            IH342     
030190     MOVE 19 TO SUBS-2.                                           IH342     
030200 190040.                                                          IH342     
030210     ADD DTL-HOURS TO OTHER-HOURS.                                IH342     
030220     MOVE "1" TO MSTR-ACTIVE-CODE.                                IH342     
030230     MOVE "1" TO SW29.                                            IH342     
030240         GO TO 128200.                                            IH342     
030250     NOTE ******************************************************* IH342     
030260          *                                                     * IH342     
030270          *              PAYROLL CODE 49                        * IH342     
030280          *  TERMINAL ANNUAL LEAVE PAYOFF                       * IH342     
030290          *      INACTIVATE PERSONNEL MASTER RECORD             * IH342     
030300          *                                                     * IH342     
030310          *******************************************************.IH342     
030320 190170.                                                          IH342     
030330     MOVE 18 TO SUBS-2.                                           IH342     
030340         GO TO 190040.                                            IH342     
030350     NOTE ******************************************************* IH342     
030360          *                                                     * IH342     
030370          *              PAYROLL CODE ZZ                        * IH342     
030380          *  CASH AWARD                                         * IH342     
030390          *      MAXIMUM OF 4 CASH AWARDS PER PERSON            * IH342     
030400          *                                                     * IH342     
030410          *******************************************************.IH342     
030420 192090.                                                          IH342     
030430     MOVE 23 TO CASH-AWARD-SUBS.                                  IH342     
030440     ADD DTL-AMOUNT TO EXC-LIST-TOT (CASH-AWARD-SUBS).            IH342     
030450     ADD DTL-AMOUNT TO SAVE-PAYOFF.                               IH342     
030460     MOVE "1" TO SW5, SW30.                                       IH342     
030470         GO TO 226090.                                            IH342     
030480 192351.                                                          IH342     
030490     MOVE 29 TO SUBS-2.                                           IH342     
030500     ADD DTL-AMOUNT TO ADJ-CHARITY.                               IH342     
030510     MOVE 1 TO SW40.                                              IH342     
030520     GO TO 150050.                                                IH342     
030530 192352.                                                          IH342     
030540     ALTER 156110 TO PROCEED TO 192351.                           IH342     
030550     GO TO 156080.                                                IH342     
030560 192353.                                                          IH342     
030570     MOVE 30 TO SUBS-2.                                           IH342     
030580     ADD DTL-AMOUNT TO ADJ-TO-BONDS.                              IH342     
030590     MOVE 1 TO SW41.                                              IH342     
030600     GO TO 150050.                                                IH342     
030610 192354.                                                          IH342     
030620     ALTER 156110 TO PROCEED TO 192353.                           IH342     
030630     GO TO 156080.                                                IH342     
030640     NOTE ******************************************************* IH342     
030650          *                                                     * IH342     
030660          *              PAYROLL CODE 55                        * IH342     
030670          *         RETROACTIVE OVERTIME MONEY                  * IH342     
030680          *                                                     * IH342     
030690          *******************************************************.IH342     
030700 192355.                                                          IH342     
030710     MOVE 31 TO SUBS-2.                                           IH342     
030720     ADD DTL-AMOUNT TO ADJ-OT-MONEY, CAT-K-MONEY.                 IH342     
030730     ADD DTL-HOURS TO ADJ-OT-HOURS.                               IH342     
030740     MOVE DTL-HOURS TO WT-RETRO-OTHRS.                            IH342     
030750     MOVE DTL-AMOUNT TO WT-RETRO-OTPAY.                           IH342     
030760     MOVE 1 TO SW42.                                              IH342     
030770     GO TO 150050.                                                IH342     
030780     NOTE ******************************************************* IH342     
030790          *                                                     * IH342     
030800          *              PAYROLL CODE 56                        * IH342     
030810          *         RETROACTIVE OTHER MONEY                     * IH342     
030820          *                                                     * IH342     
030830          *******************************************************.IH342     
030840 192356.                                                          IH342     
030850     MOVE 37 TO SUBS-2.                                           IH342     
030860     ADD DTL-AMOUNT TO ADJ-OTHER-MONEY, CAT-K-MONEY.              IH342     
030870     ADD DTL-HOURS TO OTHER-HOURS.                                IH342     
030880     MOVE 1 TO SW48.                                              IH342     
030890     GO TO 150050.                                                IH342     
030900 192357.                                                          IH342     
030910     MOVE 32 TO SUBS-2.                                           IH342     
030920     ADD DTL-AMOUNT TO ADJ-TO-ALLOT1.                             IH342     
030930     MOVE 1 TO SW43.                                              IH342     
030940     GO TO 150050.                                                IH342     
030950 192358.                                                          IH342     
030960     ALTER 156110 TO PROCEED TO 192357.                           IH342     
030970     GO TO 156080.                                                IH342     
030980 192359.                                                          IH342     
030990     MOVE 33 TO SUBS-2.                                           IH342     
031000     ADD DTL-AMOUNT TO ADJ-TO-ALLOT2.                             IH342     
031010     MOVE 1 TO SW44.                                              IH342     
031020     GO TO 150050.                                                IH342     
031030 192360.                                                          IH342     
031040     ALTER 156110 TO PROCEED TO 192359.                           IH342     
031050     GO TO 156080.                                                IH342     
031060 192361.                                                          IH342     
031070     MOVE 34 TO SUBS-2.                                           IH342     
031080     ADD DTL-AMOUNT TO ADJ-TO-OTH-DED.                            IH342     
031090     MOVE 1 TO SW45.                                              IH342     
031100     GO TO 150050.                                                IH342     
031110 192362.                                                          IH342     
031120     ALTER 156110 TO PROCEED TO 192361.                           IH342     
031130     GO TO 156080.                                                IH342     
031140 192363.                                                          IH342     
031150     MOVE 35 TO SUBS-2.                                           IH342     
031160     ADD DTL-AMOUNT TO NET-PAY-ADJ.                               IH342     
031170     MOVE 1 TO SW46.                                              IH342     
031180     GO TO 150050.                                                IH342     
031190 192364.                                                          IH342     
031200     ALTER 156110 TO PROCEED TO 192363.                           IH342     
031210     GO TO 156080.                                                IH342     
031220 192365.                                                          IH342     
031230     MOVE 36 TO SUBS-2.                                           IH342     
031240     ADD DTL-AMOUNT TO NOT-SUBJ-FED-TAX, MSTR-YTD-NSFT.           IH342     
031250     MOVE 1 TO SW47.                                              IH342     
031260     GO TO 150050.                                                IH342     
031270     NOTE ******************************************************* IH342     
031280          *                                                     * IH342     
031290          *  EQUAL PAYROLL EXCEPTION DETAIL                     * IH342     
031300          *      CHECK FOR PREVIOUS DETAIL ERROR OR             * IH342     
031310          *      MORE CASH AWARDS                               * IH342     
031320          *                                                     * IH342     
031330          *******************************************************.IH342     
031340 194010.                                                          IH342     
031350     IF DTL-ERROR, GO TO 198082.                                  IH342     
031360     IF NOT WRITE-CASH-AWARD, GO TO 122130.                       IH342     
031370         IF DTL-PAY-CODE (01) NOT EQUAL TO "ZZ",                  IH342     
031380             OR CASH-AWARD-SUBS EQUAL TO 27,                      IH342     
031390                 MOVE "INCORRECT PAY CODE" TO ERR-EXPLAIN,        IH342     
031400                     GO TO 196180.                                IH342     
031410         MOVE "1" TO SW30.                                        IH342     
031420         ADD 01 TO CASH-AWARD-SUBS.                               IH342     
031430         ADD DTL-AMOUNT TO EXC-LIST-TOT (CASH-AWARD-SUBS).        IH342     
031440         ADD DTL-AMOUNT TO SAVE-PAYOFF.                           IH342     
031450             GO TO 276040.                                        IH342     
031460     NOTE ******************************************************* IH342     
031470          *                                                     * IH342     
031480          *  PAYROLL EXCEPTION DETAIL SEQUENCE CONTROL BREAK    * IH342     
031490          *      IF PREVIOUS DETAIL ERROR GO TO MATCH ROUTINE   * IH342     
031500          *      IF PREVIOUS DETAIL CASH AWARD GO TO            * IH342     
031510          *          PROCESS CASH AWARD                         * IH342     
031520          *                                                     * IH342     
031530          *******************************************************.IH342     
031540 196010.                                                          IH342     
031550     IF DTL-ERROR, GO TO 120150.                                  IH342     
031560     IF WRITE-CASH-AWARD,                                         IH342     
031570             GO TO 276040.                                        IH342     
031580     GO TO 226090.                                                IH342     
031590     NOTE ******************************************************* IH342     
031600          *                                                     * IH342     
031610          *  PAYROLL ERROR OR NOTIFICATION MESSAGE ROUTINE      * IH342     
031620          *                                                     * IH342     
031630          *******************************************************.IH342     
031640 196140.                                                          IH342     
031650     MOVE "UNMATCHED DETAIL" TO ERR-EXPLAIN.                      IH342     
031660 196160.                                                          IH342     
031670     MOVE "1" TO SW1.                                             IH342     
031680 196180.                                                          IH342     
031690     ALTER 198140 TO PROCEED TO 116140.                           IH342     
031700     MOVE DTL-ACTY TO ERR-ACTIVITY.                               IH342     
031710     MOVE DTL-CNTL-GRP TO ERR-CONTROL-GRP.                        IH342     
031720     MOVE DTL-BADGE TO ERR-BADGE.                                 IH342     
031730         ADD +1 TO ERR-DET-OUT.                                   IH342     
031740 198050.                                                          IH342     
031750     WRITE CONTROL-REC AFTER ADVANCING 2 LINES.                   IH342     
031760         MOVE SPACES TO CONTROL-REC.                              IH342     
031770     IF NOT-DTL-ERROR, GO TO 198120.                              IH342     
031780 198082.                                                          IH342     
031790         MOVE "1" TO SW31.                                        IH342     
031800         MOVE PAYROLL-DETAIL-REC TO ERR-DETAIL.                   IH342     
031810           ADD +1 TO ERR-DET-OUT.                                 IH342     
031820             GO TO 198050.                                        IH342     
031830 198120.                                                          IH342     
031840     MOVE SPACE TO SW31.                                          IH342     
031850 198140.                                                          IH342     
031860         GO TO 116140.                                            IH342     
031870 198160.                                                          IH342     
031880     MOVE MSTR-BADGE TO ERR-BADGE.                                IH342     
031890     MOVE MSTR-NAME TO ERR-NAME.                                  IH342     
031900     MOVE MSTR-ACTY TO ERR-ACTIVITY.                              IH342     
031910     MOVE MSTR-CONT-GRP TO ERR-CONTROL-GRP.                       IH342     
031920 199900.                                                          IH342     
031930         ADD +1 TO ERR-DET-OUT.                                   IH342     
031940             GO TO 198050.                                        IH342     
031950     NOTE ******************************************************* IH342     
031960          *                                                     * IH342     
031970          *  ALL PAYROLL CODES PROCESSED FROM PAYROLL EXCEPTION * IH342     
031980          *      DETAIL MATCHED TO PERSONNEL MASTER             * IH342     
031990          *          TEST FOR EXCEPTION PAY TO BE COMPUTED      * IH342     
032000          *                                                     * IH342     
032010          *******************************************************.IH342     
032020 200100.                                                          IH342     
032030     IF NOT EXCEPT-TO-COMPUTE,                                    IH342     
032040         ADD SAVE-NITE-PAY TO OTHER-MONEY,                        IH342     
032050         MOVE +00000.00 TO SAVE-NITE-PAY,                         IH342     
032060             GO TO 116140.                                        IH342     
032070     MOVE SPACE TO SW6.                                           IH342     
032080     IF SAVE-NITE-PAY ZERO,                                       IH342     
032090             GO TO 202120.                                        IH342     
032100         IF DIEM-MSTR, GO TO 202010.                              IH342     
032110         IF ANNUAL-PAYOFF, OR HOLIDAY-PAYOFF,                     IH342     
032120             ADD SAVE-NITE-PAY TO SAVE-PAYOFF.                    IH342     
032130         IF OVERTIME,                                             IH342     
032140             ADD SAVE-NITE-PAY TO OT-MONEY,                       IH342     
032150                 GO TO 202030.                                    IH342     
032160 202010.                                                          IH342     
032170         ADD SAVE-NITE-PAY TO OTHER-MONEY.                        IH342     
032180 202030.                                                          IH342     
032190         MOVE +00000.00 TO SAVE-NITE-PAY.                         IH342     
032200     NOTE ******************************************************* IH342     
032210          *                                                     * IH342     
032220          *  COMPUTE OTHER THAN NORMAL HOURS AND EXCEPTION PAY  * IH342     
032230          *                                                     * IH342     
032240          *******************************************************.IH342     
032250 202120.                                                          IH342     
032260     IF OVERTIME,                                                 IH342     
032270         GO TO 212170.                                            IH342     
032280     IF ANNUAL-PAYOFF,                                            IH342     
032290         GO TO 214130.                                            IH342     
032300     IF RETRO-HOURS,                                              IH342     
032310         GO TO 218040.                                            IH342     
032320     IF HOLIDAY-PAYOFF,                                           IH342     
032330             GO TO 214130.                                        IH342     
032340     IF HOLIDAY-WORKED,                                           IH342     
032350         GO TO 220100.                                            IH342     
032360 204052.                                                          IH342     
032370     IF DIVER-PAY,                                                IH342     
032380         GO TO 222060.                                            IH342     
032390     IF SUNDAY-PREMIUM,                                           IH342     
032400         GO TO 224070.                                            IH342     
032410     NOTE ******************************************************* IH342     
032420          *                                                     * IH342     
032430          *  COMPUTE AND POST PER DIEM NIGHT PAY                * IH342     
032440          *                                                     * IH342     
032450          *******************************************************.IH342     
032460 204170.                                                          IH342     
032470     IF DIEM-NITE-PAY ZERO,                                       IH342     
032480         GO TO 208060.                                            IH342     
032490     MULTIPLY DTL-HOURS BY DIEM-NITE-PAY ROUNDED.                 IH342     
032500     IF OVERTIME,                                                 IH342     
032510         ADD DIEM-NITE-PAY TO OT-MONEY,                           IH342     
032520             GO TO 208060.                                        IH342     
032530     IF ANNUAL-PAYOFF,                                            IH342     
032540         ADD DIEM-NITE-PAY TO SAVE-PAYOFF,                        IH342     
032550         GO TO 206100.                                            IH342     
032560     IF HOLIDAY-WORKED,                                           IH342     
032570         GO TO 206140.                                            IH342     
032580     IF HOLIDAY-PAYOFF, ADD DIEM-NITE-PAY TO SAVE-PAYOFF.         IH342     
032590     IF RETRO-HOURS,                                              IH342     
032600         ADD DIEM-NITE-PAY TO ADJ-WK-SUB,                         IH342     
032610             GO TO 206100.                                        IH342     
032620     IF SAVE-NITE-WEEK EQUAL TO "2",                              IH342     
032630         ADD DIEM-NITE-PAY TO WK2-SUBJECT,                        IH342     
032640             GO TO 206100.                                        IH342     
032650         ADD DIEM-NITE-PAY TO WK1-SUBJECT.                        IH342     
032660 206100.                                                          IH342     
032670     IF MSTR-NITE-CODE NOT EQUAL TO SPACE,                        IH342     
032680         ADD DIEM-NITE-PAY TO REG-MONEY,                          IH342     
032690             GO TO 208060.                                        IH342     
032700 206140.                                                          IH342     
032710         ADD DIEM-NITE-PAY TO OTHER-MONEY.                        IH342     
032720     NOTE ******************************************************* IH342     
032730          *                                                     * IH342     
032740          *  COMPUTE AND POST PER DIEM BONUS PAY                * IH342     
032750          *                                                     * IH342     
032760          *******************************************************.IH342     
032770 208060.                                                          IH342     
032780     IF DIEM-BONUS-PAY ZERO,                                      IH342     
032790         GO TO 210010.                                            IH342     
032800     MULTIPLY DTL-HOURS BY DIEM-BONUS-PAY ROUNDED.                IH342     
032810     IF ANNUAL-PAYOFF,                                            IH342     
032820         ADD DIEM-BONUS-PAY TO SAVE-PAYOFF,                       IH342     
032830             GO TO 208190.                                        IH342     
032840     IF HOLIDAY-WORKED,                                           IH342     
032850         GO TO 208190.                                            IH342     
032860     IF HOLIDAY-PAYOFF, ADD DIEM-BONUS-PAY TO SAVE-PAYOFF.        IH342     
032870     IF RETRO-HOURS,                                              IH342     
032880         ADD DIEM-BONUS-PAY TO ADJ-WK-SUB,                        IH342     
032890             GO TO 208190.                                        IH342     
032900     IF SAVE-BONUS-WEEK EQUAL TO "2",                             IH342     
032910         ADD DIEM-BONUS-PAY TO WK2-SUBJECT,                       IH342     
032920             GO TO 208190.                                        IH342     
032930         ADD DIEM-BONUS-PAY TO WK1-SUBJECT.                       IH342     
032940 208190.                                                          IH342     
032950     ADD DIEM-BONUS-PAY TO OTHER-MONEY.                           IH342     
032960 210010.                                                          IH342     
032970     MOVE +00000.00 TO DIEM-NITE-PAY, DIEM-BONUS-PAY.             IH342     
032980 210030.                                                          IH342     
032990     MOVE SPACE TO SW17, SW10, SW29, SW16, SW11, SW9, SW32, SW12. IH342     
033000         GO TO 116140.                                            IH342     
033010     NOTE ******************************************************* IH342     
033020          *                                                     * IH342     
033030          *  PER DIEM OVERTIME TO BE COMPUTED                   * IH342     
033040          *      TEST FOR OVERTIME AT DIVER RATE                * IH342     
033050          *          SAVE DIVER RATE HOURS BY WEEK INDICATOR    * IH342     
033060          *                                                     * IH342     
033070          *******************************************************.IH342     
033080 212170.                                                          IH342     
033090     MOVE SPACE TO SW32.                                          IH342     
033100     IF NOT DIVER-PAY,                                            IH342     
033110         GO TO 204170.                                            IH342     
033120     IF SAVE-OT-WEEK EQUAL TO "1",                                IH342     
033130         ADD DTL-HOURS TO DIVER-OT-HR1,                           IH342     
033140             GO TO 204170.                                        IH342     
033150         ADD DTL-HOURS TO DIVER-OT-HR2.                           IH342     
033160             GO TO 204170.                                        IH342     
033170     NOTE ******************************************************* IH342     
033180          *                                                     * IH342     
033190          *  DECEASED OR TERMINAL ANNUAL LEAVE PAYOFF           * IH342     
033200          *      COMPUTE FIREFIGHTER PREMIUM PAYOFF             * IH342     
033210          *                                                     * IH342     
033220          *******************************************************.IH342     
033230 214120.                                                          IH342     
033240     IF NOT-FF-MSTR MOVE -80.00 TO HRLY-RATE-WORK                 IH342     
033250         GO TO 214125.                                            IH342     
033260     IF FIRE-CHIEF-MSTR MOVE -112.00 TO HRLY-RATE-WORK            IH342     
033270         GO TO 214125.                                            IH342     
033280     MOVE -144.00 TO HRLY-RATE-WORK.                              IH342     
033290 214125.                                                          IH342     
033300     ADD LWOP-WK1 LWOP-WK2 TO HRLY-RATE-WORK.                     IH342     
033310     IF HRLY-RATE-WORK POSITIVE                                   IH342     
033320         OR HRLY-RATE-WORK ZERO                                   IH342     
033330         MOVE "1" TO SW49.                                        IH342     
033340     MOVE SPACE TO SW50.                                          IH342     
033350 214130.                                                          IH342     
033360     IF ANNUAL-PAYOFF OR HOLIDAY-PAYOFF MOVE "1" TO SW50.         IH342     
033370     MULTIPLY DTL-HOURS BY MSTR-HOURLY-RATE,                      IH342     
033380         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
033390     ADD HRLY-RATE-WORK TO SAVE-PAYOFF.                           IH342     
033400     IF ANNUAL-PAYOFF OR HOLIDAY-PAYOFF,                          IH342     
033410         ADD HRLY-RATE-WORK TO CAT-J-MONEY                        IH342     
033420         ADD DTL-HOURS TO CAT-J-HOURS.                            IH342     
033430     IF HOLIDAY-OTH-MONEY,                                        IH342     
033440         MOVE SPACE TO SW34,                                      IH342     
033450         ADD HRLY-RATE-WORK TO OTHER-MONEY,                       IH342     
033460             GO TO 216010.                                        IH342     
033470         ADD HRLY-RATE-WORK TO ADJ-OTHER-MONEY.                   IH342     
033480 216010.                                                          IH342     
033490     IF DIEM-MSTR,                                                IH342     
033500             GO TO 204170.                                        IH342     
033510 216050.                                                          IH342     
033520     IF NOT-FF-MSTR,                                              IH342     
033530         GO TO 210030.                                            IH342     
033540     MULTIPLY FF-BONUS-PERCENT BY HRLY-RATE-WORK ROUNDED.         IH342     
033550     ADD HRLY-RATE-WORK TO ADJ-OTHER-MONEY.                       IH342     
033560     IF ANNUAL-PAYOFF, OR HOLIDAY-PAYOFF,                         IH342     
033570         ADD HRLY-RATE-WORK TO SAVE-PAYOFF CAT-J-MONEY.           IH342     
033580         GO TO 210030.                                            IH342     
033590     NOTE ******************************************************* IH342     
033600          *                                                     * IH342     
033610          *  COMPUTE RETROACTIVE REGULAR HOURS                  * IH342     
033620          *      COMPUTE FIREFIGHTER PREMIUM                    * IH342     
033630          *                                                     * IH342     
033640          *******************************************************.IH342     
033650 218040.                                                          IH342     
033660     IF NOT-FF-MSTR,                                              IH342     
033670         MOVE MSTR-HOURLY-RATE TO ROUND-WORK-AREA,                IH342     
033680             GO TO 218142.                                        IH342     
033690     MULTIPLY +80 BY MSTR-HOURLY-RATE,                            IH342     
033700         GIVING ROUND-WORK-AREA.                                  IH342     
033710     DIVIDE FF-PREM-WORK INTO ROUND-WORK-AREA.                    IH342     
033720 218142.                                                          IH342     
033730     MULTIPLY DTL-HOURS BY ROUND-WORK-AREA,                       IH342     
033740         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
033750     ADD HRLY-RATE-WORK TO ADJ-REG-MONEY.                         IH342     
033760     ADD HRLY-RATE-WORK TO ADJ-WK-SUB, CAT-K-MONEY.               IH342     
033770 218180.                                                          IH342     
033780     IF DIEM-MSTR,                                                IH342     
033790         GO TO 204170.                                            IH342     
033800     GO TO 216050.                                                IH342     
033810     NOTE ******************************************************* IH342     
033820          *                                                     * IH342     
033830          *  COMPUTE HOURS WORKED ON HOLIDAY                    * IH342     
033840          *      TEST FOR DIVER RATE                            * IH342     
033850          *                                                     * IH342     
033860          *******************************************************.IH342     
033870 220100.                                                          IH342     
033880     IF DIVER-PAY,                                                IH342     
033890         MOVE DIVER-RATE-KON TO HRLY-RATE-WORK,                   IH342     
033900             GO TO 220150.                                        IH342     
033910         MOVE MSTR-HOURLY-RATE TO HRLY-RATE-WORK.                 IH342     
033920 220150.                                                          IH342     
033930     MULTIPLY DTL-HOURS BY HRLY-RATE-WORK ROUNDED.                IH342     
033940     ADD HRLY-RATE-WORK TO OTHER-MONEY, CAT-B-MONEY.              IH342     
033950     ADD DTL-HOURS TO CAT-B-HOURS.                                IH342     
033960         GO TO 216010.                                            IH342     
033970     NOTE ******************************************************* IH342     
033980          *                                                     * IH342     
033990          *  COMPUTE DIVER RATE FOR REGULAR HOURS               * IH342     
034000          *                                                     * IH342     
034010          *******************************************************.IH342     
034020 222060.                                                          IH342     
034030     MULTIPLY DIVER-RATE-KON BY DTL-HOURS,                        IH342     
034040         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
034050     ADD HRLY-RATE-WORK TO OTHER-MONEY, CAT-A-MONEY.              IH342     
034060     ADD DTL-HOURS TO OTHER-HOURS, CAT-A-HOURS.                   IH342     
034070     IF SAVE-DIVER-WEEK EQUAL TO "1",                             IH342     
034080         ADD HRLY-RATE-WORK TO WK1-SUBJECT,                       IH342     
034090         ADD DTL-HOURS TO REG-HRS-WK1,                            IH342     
034100             GO TO 218180.                                        IH342     
034110         ADD HRLY-RATE-WORK TO WK2-SUBJECT.                       IH342     
034120         ADD DTL-HOURS TO REG-HRS-WK2.                            IH342     
034130             GO TO 218180.                                        IH342     
034140     NOTE ******************************************************* IH342     
034150          *                                                     * IH342     
034160          *  COMPUTE 25 PERCENT SUNDAY REGULAR SCHEDULE         * IH342     
034170          *      INCLUDE ALL BONUS PAY EARNED                   * IH342     
034180          *      SAVE PREMIUM BY WEEK INDICATOR FOR PER DIEM    * IH342     
034190          *                                                     * IH342     
034200          *******************************************************.IH342     
034210 224070.                                                          IH342     
034220     MOVE MSTR-HOURLY-RATE TO HRLY-RATE-WORK.                     IH342     
034230     IF DIEM-MSTR,                                                IH342     
034240         ADD DIEM-NITE-PAY, DIEM-BONUS-PAY TO HRLY-RATE-WORK.     IH342     
034250     MULTIPLY +.25 BY HRLY-RATE-WORK ROUNDED.                     IH342     
034260     MULTIPLY DTL-HOURS BY HRLY-RATE-WORK ROUNDED.                IH342     
034270     ADD HRLY-RATE-WORK TO OTHER-MONEY CAT-N-MONEY.               IH342     
034280     ADD DTL-HOURS TO CAT-N-HOURS.                                IH342     
034290     IF ANNUM-MSTR,                                               IH342     
034300         GO TO 210010.                                            IH342     
034310     IF SAVE-SUNDAY-WEEK EQUAL TO "1",                            IH342     
034320         ADD HRLY-RATE-WORK TO SUNDAY-PREM-WK1,                   IH342     
034330             GO TO 210010.                                        IH342     
034340         ADD HRLY-RATE-WORK TO SUNDAY-PREM-WK2.                   IH342     
034350             GO TO 210010.                                        IH342     
034360     NOTE ******************************************************* IH342     
034370          *                                                     * IH342     
034380          *  BEGIN PAYROLL COMPUTATION AND DEDUCTION ROUTINES   * IH342     
034390          *  TEST FOR OVERTIME AND LWOP IN FIRST WEEK           * IH342     
034400          *      REDUCE LWOP HOURS BY OVERTIME HOURS            * IH342     
034410          *          WRITE NOTIFICATION MESSAGE                 * IH342     
034420          *                                                     * IH342     
034430          *******************************************************.IH342     
034440 226090.                                                          IH342     
034450     IF LWOP-WK1 ZERO,                                            IH342     
034460         GO TO 232010.                                            IH342     
034470     IF OT-HRS-WK1 ZERO,                                          IH342     
034480         GO TO 232010.                                            IH342     
034490     SUBTRACT OT-HRS-WK1 FROM LWOP-WK1,                           IH342     
034500         GIVING HRLY-RATE-WORK.                                   IH342     
034510     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
034520         MOVE OT-HRS-WK1 TO ERR-HOURS                             IH342     
034530         GO TO 230010.                                            IH342     
034540     MOVE LWOP-WK1 TO ERR-HOURS.                                  IH342     
034550     MOVE +000.00 TO LWOP-WK1.                                    IH342     
034560     MULTIPLY -1.00 BY HRLY-RATE-WORK.                            IH342     
034570     MOVE HRLY-RATE-WORK TO OT-HRS-WK1.                           IH342     
034580     IF DIVER-OT-HR1 ZERO,                                        IH342     
034590         GO TO 228160.                                            IH342     
034600     SUBTRACT DIVER-OT-HR1 FROM HRLY-RATE-WORK.                   IH342     
034610     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
034620         GO TO 228160.                                            IH342     
034630     MOVE SPACES TO ERR-SPACE.                                    IH342     
034640     ADD HRLY-RATE-WORK TO DIVER-OT-HR1.                          IH342     
034650     SUBTRACT HRLY-RATE-WORK FROM REG-HRS-WK1.                    IH342     
034660     MULTIPLY MINUS-DIVER-RATE BY HRLY-RATE-WORK ROUNDED.         IH342     
034670     ADD HRLY-RATE-WORK TO REG-MONEY.                             IH342     
034680     ADD HRLY-RATE-WORK TO WK1-SUBJECT.                           IH342     
034690     NOTE ******************************************************* IH342     
034700          *                                                     * IH342     
034710          *  WRITE NOTIFICATION MESSAGE                         * IH342     
034720          *      OVERTIME HOURS APPLIED TO LWOP                 * IH342     
034730          *                                                     * IH342     
034740          *******************************************************.IH342     
034750 228160.                                                          IH342     
034760     ALTER 198140 TO PROCEED TO 232010.                           IH342     
034770 228180.                                                          IH342     
034780     MOVE "OT HOURS APPLIED TO LWOP" TO ERR-MESS.                 IH342     
034790 228200.                                                          IH342     
034800     MOVE "1" TO SW31.                                            IH342     
034810         GO TO 198160.                                            IH342     
034820 230010.                                                          IH342     
034830     MOVE HRLY-RATE-WORK TO LWOP-WK1.                             IH342     
034840     IF DIVER-OT-HR1 ZERO,                                        IH342     
034850         GO TO 230090.                                            IH342     
034860     ADD DIVER-OT-HR1 TO REG-HRS-WK1.                             IH342     
034870     MULTIPLY DIVER-RATE-KON BY DIVER-OT-HR1 ROUNDED.             IH342     
034880     ADD DIVER-OT-HR1 TO REG-MONEY.                               IH342     
034890     ADD DIVER-OT-HR1 TO WK1-SUBJECT.                             IH342     
034900 230090.                                                          IH342     
034910     MOVE +00000.00 TO OT-HRS-WK1, DIVER-OT-HR1.                  IH342     
034920         GO TO 228160.                                            IH342     
034930     NOTE ******************************************************* IH342     
034940          *                                                     * IH342     
034950          *  TEST FOR OVERTIME AND LWOP IN SECOND WEEK          * IH342     
034960          *      REDUCE LWOP HOURS BY OVERTIME HOURS            * IH342     
034970          *          WRITE NOTIFICATION MESSAGE                 * IH342     
034980          *                                                     * IH342     
034990          *******************************************************.IH342     
035000 232010.                                                          IH342     
035010     IF LWOP-WK2 ZERO,                                            IH342     
035020         GO TO 236010.                                            IH342     
035030     IF OT-HRS-WK2 ZERO,                                          IH342     
035040         GO TO 236010.                                            IH342     
035050     SUBTRACT OT-HRS-WK2 FROM LWOP-WK2,                           IH342     
035060         GIVING HRLY-RATE-WORK.                                   IH342     
035070     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
035080         MOVE OT-HRS-WK2 TO ERR-HOURS                             IH342     
035090         GO TO 234040.                                            IH342     
035100     MOVE LWOP-WK2 TO ERR-HOURS.                                  IH342     
035110     MOVE +000.00 TO LWOP-WK2.                                    IH342     
035120     MULTIPLY -1.00 BY HRLY-RATE-WORK.                            IH342     
035130     MOVE HRLY-RATE-WORK TO OT-HRS-WK2.                           IH342     
035140     IF DIVER-OT-HR2 ZERO,                                        IH342     
035150         GO TO 234010.                                            IH342     
035160     SUBTRACT DIVER-OT-HR2 FROM HRLY-RATE-WORK.                   IH342     
035170     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
035180         GO TO 234010.                                            IH342     
035190     MOVE SPACES TO ERR-SPACE.                                    IH342     
035200     ADD HRLY-RATE-WORK TO DIVER-OT-HR2.                          IH342     
035210     SUBTRACT HRLY-RATE-WORK FROM REG-HRS-WK2.                    IH342     
035220     MULTIPLY MINUS-DIVER-RATE BY HRLY-RATE-WORK ROUNDED.         IH342     
035230     ADD HRLY-RATE-WORK TO REG-MONEY.                             IH342     
035240     ADD HRLY-RATE-WORK TO WK2-SUBJECT.                           IH342     
035250 234010.                                                          IH342     
035260     ALTER 198140 TO PROCEED TO 236010.                           IH342     
035270         GO TO 228180.                                            IH342     
035280 234040.                                                          IH342     
035290     MOVE HRLY-RATE-WORK TO LWOP-WK2.                             IH342     
035300     IF DIVER-OT-HR2 ZERO,                                        IH342     
035310         GO TO 234120.                                            IH342     
035320     ADD DIVER-OT-HR2 TO REG-HRS-WK2.                             IH342     
035330     MULTIPLY DIVER-RATE-KON BY DIVER-OT-HR2 ROUNDED.             IH342     
035340     ADD DIVER-OT-HR2 TO REG-MONEY.                               IH342     
035350     ADD DIVER-OT-HR2 TO WK2-SUBJECT.                             IH342     
035360 234120.                                                          IH342     
035370     MOVE +00000.00 TO OT-HRS-WK2, DIVER-OT-HR2.                  IH342     
035380         GO TO 234010.                                            IH342     
035390     NOTE ******************************************************* IH342     
035400          *                                                     * IH342     
035410          *  COMPUTATION OF PAYROLL REGULAR HOURS               * IH342     
035420          *      INCLUDE FIREFIGHTER REGULAR HOURS              * IH342     
035430          *          TEST FOR LWOP EXCEEDING REGULAR HOURS      * IH342     
035440          *                                                     * IH342     
035450          *******************************************************.IH342     
035460 236010.                                                          IH342     
035470     IF DIEM-MSTR,                                                IH342     
035480         GO TO 236080.                                            IH342     
035490     IF GS-GRADED-MSTR,                                           IH342     
035500         GO TO 238060.                                            IH342     
035510 236080.                                                          IH342     
035520     SUBTRACT LWOP-WK1, REG-HRS-WK1 FROM +040.00                  IH342     
035530         GIVING HRLY-RATE-WORK.                                   IH342     
035540 236102.                                                          IH342     
035550     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
035560         OR HRLY-RATE-WORK ZERO,                                  IH342     
035570             GO TO 240080.                                        IH342     
035580     ALTER 198140 TO PROCEED TO 240060.                           IH342     
035590     NOTE ******************************************************* IH342     
035600          *                                                     * IH342     
035610          *  WRITE NOTIFICATION MESSAGE                         * IH342     
035620          *      LWOP HOURS EXCEED REGULAR HOURS                * IH342     
035630          *                                                     * IH342     
035640          *******************************************************.IH342     
035650 238020.                                                          IH342     
035660     MOVE HRLY-RATE-WORK TO MSG3-HRS.                             IH342     
035670     MOVE MESSAGE3 TO ERR-EXPLAIN.                                IH342     
035680         GO TO 228200.                                            IH342     
035690 238060.                                                          IH342     
035700     IF LWOP-WK1 ZERO, AND LWOP-WK2 ZERO,                         IH342     
035710         MOVE +80.00 TO HRLY-RATE-WORK,                           IH342     
035720             GO TO 236102.                                        IH342     
035730     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
035740     SUBTRACT LWOP-WK1, LWOP-WK2 FROM HRLY-RATE-WORK.             IH342     
035750     IF NOT-FF-MSTR,                                              IH342     
035760         ADD +80.00 TO HRLY-RATE-WORK,                            IH342     
035770             GO TO 236102.                                        IH342     
035780         ADD FF-PREM-WORK TO HRLY-RATE-WORK.                      IH342     
035790             GO TO 244100.                                        IH342     
035800     NOTE ******************************************************* IH342     
035810          *                                                     * IH342     
035820          *  LWOP HOURS LESS THAN REGULAR HOURS                 * IH342     
035830          *      COMPUTE REGULAR MONEY                          * IH342     
035840          *                                                     * IH342     
035850          *******************************************************.IH342     
035860 240060.                                                          IH342     
035870     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
035880 240080.                                                          IH342     
035890     MOVE HRLY-RATE-WORK TO REG-HRS-WK1.                          IH342     
035900     ADD HRLY-RATE-WORK TO CAT-A-HOURS.                           IH342     
035910     MULTIPLY MSTR-HOURLY-RATE BY HRLY-RATE-WORK ROUNDED.         IH342     
035920     IF GS-GRADED-MSTR ADD HRLY-RATE-WORK TO CAT-A-MONEY.         IH342     
035930     IF DIEM-MSTR ADD HRLY-RATE-WORK TO CAT-MONEY.                IH342     
035940     ADD HRLY-RATE-WORK TO WK1-SUBJECT.                           IH342     
035950     ADD HRLY-RATE-WORK TO REG-MONEY.                             IH342     
035960 DIEM1.                                                           IH342     
035970     NOTE ******************************************************* IH342     
035980          *                                                     * IH342     
035990          *   TEST FOR PER DIEM MASTERS                         * IH342     
036000          *      COMPUTE PER DIEM WEEK 2 PAY                    * IH342     
036010          *                                                     * IH342     
036020          *******************************************************.IH342     
036030 241000.                                                          IH342     
036040     IF DIEM-MSTR,                                                IH342     
036050         GO TO 242070.                                            IH342     
036060     IF GS-GRADED-MSTR,                                           IH342     
036070         GO TO 246122.                                            IH342     
036080 242070.                                                          IH342     
036090     SUBTRACT LWOP-WK2, REG-HRS-WK2 FROM +040.00                  IH342     
036100         GIVING HRLY-RATE-WORK.                                   IH342     
036110     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
036120         ALTER 198140 TO PROCEED TO 242140,                       IH342     
036130             GO TO 238020.                                        IH342     
036140         GO TO 242160.                                            IH342     
036150 242140.                                                          IH342     
036160     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
036170 242160.                                                          IH342     
036180     MOVE HRLY-RATE-WORK TO REG-HRS-WK2.                          IH342     
036190     ADD HRLY-RATE-WORK TO CAT-A-HOURS.                           IH342     
036200     ADD REG-HRS-WK2 TO REG-HRS-WK1.                              IH342     
036210     MULTIPLY MSTR-HOURLY-RATE BY HRLY-RATE-WORK ROUNDED.         IH342     
036220     ADD HRLY-RATE-WORK TO WK2-SUBJECT, CAT-MONEY.                IH342     
036230     ADD HRLY-RATE-WORK TO REG-MONEY.                             IH342     
036240     IF 2ND-SHIFT-MSTR                                            IH342     
036250         MULTIPLY REG-HRS-WK1 BY SHIFT-2 GIVING CAT-C-WORK ROUNDEDIH342     
036260         GO TO 242180.                                            IH342     
036270     IF 3RD-SHIFT-MSTR                                            IH342     
036280         MULTIPLY REG-HRS-WK1 BY SHIFT-3 GIVING CAT-C-WORK ROUNDEDIH342     
036290         GO TO 242180.                                            IH342     
036300     ADD CAT-MONEY TO CAT-A-MONEY.                                IH342     
036310     GO TO 252080.                                                IH342     
036320 242180.                                                          IH342     
036330     ADD CAT-C-WORK TO CAT-C-MONEY.                               IH342     
036340     SUBTRACT CAT-C-WORK FROM CAT-MONEY.                          IH342     
036350     ADD CAT-MONEY TO CAT-A-MONEY.                                IH342     
036360     ADD REG-HRS-WK1 TO CAT-C-HOURS.                              IH342     
036370         GO TO 252080.                                            IH342     
036380     NOTE ******************************************************* IH342     
036390          *                                                     * IH342     
036400          *  COMPUTATION OF FIREFIGHTER REGULAR HOURS           * IH342     
036410          *                                                     * IH342     
036420          *******************************************************.IH342     
036430 244100.                                                          IH342     
036440     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
036450         GO TO 244140.                                            IH342     
036460     ADD HRLY-RATE-WORK TO REG-HRS-WK1.                           IH342     
036470 244140.                                                          IH342     
036480     MULTIPLY +80.0 BY MSTR-HOURLY-RATE,                          IH342     
036490         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
036500     MOVE HRLY-RATE-WORK TO ROUND-WORK-AREA.                      IH342     
036510     DIVIDE FF-PREM-WORK INTO ROUND-WORK-AREA,                    IH342     
036520         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
036530     MULTIPLY REG-HRS-WK1 BY HRLY-RATE-WORK ROUNDED.              IH342     
036540     ADD HRLY-RATE-WORK TO WK1-SUBJECT.                           IH342     
036550     ADD HRLY-RATE-WORK TO REG-MONEY.                             IH342     
036560     MULTIPLY FF-BONUS-PERCENT BY HRLY-RATE-WORK ROUNDED.         IH342     
036570     ADD HRLY-RATE-WORK TO WK1-SUBJECT.                           IH342     
036580 246098.                                                          IH342     
036590     ADD HRLY-RATE-WORK TO OTHER-MONEY.                           IH342     
036600         GO TO 252080.                                            IH342     
036610 246122.                                                          IH342     
036620     IF NOT-FF-MSTR,                                              IH342     
036630         GO TO 252080.                                            IH342     
036640     NOTE ******************************************************* IH342     
036650          *                                                     * IH342     
036660          *  COMPUTATION OF 15( OR 20( BONUS FOR PREMIUM        * IH342     
036670          *      FIREFIGHTERS AND FIRECHIEF WITHOUT LWOP        * IH342     
036680          *          TEST ANNUAL RATE FOR MAXIMUM OF GS-10      * IH342     
036690          *          FIRST STEP                                 * IH342     
036700          *          ALSO APPLY ADDITIONAL SUNDAY PREMIUM PAY   * IH342     
036710          *                                                     * IH342     
036720          *******************************************************.IH342     
036730 248030.                                                          IH342     
036740     IF MSTR-ANNUAL-RATE GREATER THAN FF-MAX-RATE,                IH342     
036750         MOVE FF-MAX-RATE TO HRLY-RATE-WORK,                      IH342     
036760             GO TO 248080.                                        IH342     
036770         MOVE MSTR-ANNUAL-RATE TO HRLY-RATE-WORK.                 IH342     
036780 248080.                                                          IH342     
036790     ADD FF-BONUS-HOURS TO REG-HRS-WK1.                           IH342     
036800     MULTIPLY FF-BONUS-PERCENT BY HRLY-RATE-WORK ROUNDED.         IH342     
036810     ADD MSTR-ANNUAL-RATE TO HRLY-RATE-WORK.                      IH342     
036820     MOVE HRLY-RATE-WORK TO ROUND-WORK-AREA.                      IH342     
036830     DIVIDE +2080 INTO ROUND-WORK-AREA,                           IH342     
036840         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
036850     SUBTRACT MSTR-HOURLY-RATE FROM HRLY-RATE-WORK.               IH342     
036860     MULTIPLY +80 BY HRLY-RATE-WORK.                              IH342     
036870     ADD HRLY-RATE-WORK TO WK1-SUBJECT.                           IH342     
036880         GO TO 246098.                                            IH342     
036890     NOTE ******************************************************* IH342     
036900          *                                                     * IH342     
036910          *  COMPUTE PER ANNUM OVERTIME                         * IH342     
036920          *      TEST FOR GS-10 STEP 1 MAXIMUM RATE             * IH342     
036930          *                                                     * IH342     
036940          *******************************************************.IH342     
036950 252080.                                                          IH342     
036960     IF DIEM-MSTR,                                                IH342     
036970         GO TO 254140.                                            IH342     
036980     IF NOT GS-GRADED-MSTR,                                       IH342     
036990         GO TO 254140.                                            IH342     
037000 252150.                                                          IH342     
037010     ADD OT-HRS-WK2 TO OT-HRS-WK1.                                IH342     
037020     IF OT-HRS-WK1 ZERO,                                          IH342     
037030         GO TO 260140.                                            IH342     
037040     MULTIPLY MSTR-HOURLY-RATE BY +1.5,                           IH342     
037050         GIVING HRLY-RATE-WORK ROUNDED.                           IH342     
037060     IF HRLY-RATE-WORK GREATER THAN ANNUM-OT-MAX,                 IH342     
037070         MOVE ANNUM-OT-MAX TO HRLY-RATE-WORK.                     IH342     
037080     MULTIPLY OT-HRS-WK1 BY HRLY-RATE-WORK ROUNDED.               IH342     
037090     ADD HRLY-RATE-WORK TO OT-MONEY.                              IH342     
037100         GO TO 260140.                                            IH342     
037110     NOTE ******************************************************* IH342     
037120          *                                                     * IH342     
037130          *  COMPUTE PER DIEM WEEK 1 OVERTIME                   * IH342     
037140          *                                                     * IH342     
037150          *******************************************************.IH342     
037160 254140.                                                          IH342     
037170     IF OT-HRS-WK1 ZERO,                                          IH342     
037180         GO TO 258030.                                            IH342     
037190     MULTIPLY MSTR-HOURLY-RATE BY +1.5,                           IH342     
037200        GIVING HRLY-RATE-WORK ROUNDED.                            IH342     
037210         MULTIPLY OT-HRS-WK1 BY HRLY-RATE-WORK ROUNDED.           IH342     
037220 256130.                                                          IH342     
037230     ADD HRLY-RATE-WORK TO OT-MONEY.                              IH342     
037240     NOTE ******************************************************* IH342     
037250          *                                                     * IH342     
037260          *  COMPUTE PER DIEM WEEK 2 OVERTIME                   * IH342     
037270          *                                                     * IH342     
037280          *******************************************************.IH342     
037290 258030.                                                          IH342     
037300     IF OT-HRS-WK2 ZERO,                                          IH342     
037310         GO TO 264160.                                            IH342     
037320     MULTIPLY MSTR-HOURLY-RATE BY +1.5,                           IH342     
037330        GIVING HRLY-RATE-WORK ROUNDED.                            IH342     
037340         MULTIPLY OT-HRS-WK2 BY HRLY-RATE-WORK ROUNDED.           IH342     
037350 260020.                                                          IH342     
037360     ADD HRLY-RATE-WORK TO OT-MONEY.                              IH342     
037370         GO TO 264160.                                            IH342     
037380     NOTE ******************************************************* IH342     
037390          *                                                     * IH342     
037400          *  TEST FOR PER ANNUM MAXIMUM ACCRUAL PERIOD PAY      * IH342     
037410          *      WRITE NOTIFICATION MESSAGE FOR CURRENT         * IH342     
037420          *          EARNED EXCEEDING GS-15 STEP 10 MAXIMUM     * IH342     
037430          *                                                     * IH342     
037440          *******************************************************.IH342     
037450 260140.                                                          IH342     
037460     MOVE ANNUM-MAX-PAY TO HRLY-RATE-WORK.                        IH342     
037470     SUBTRACT REG-MONEY, OT-MONEY, OTHER-MONEY,                   IH342     
037480         FROM HRLY-RATE-WORK.                                     IH342     
037490     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
037500         GO TO 264160.                                            IH342     
037510     IF MSTR-GS-GRADE EQUAL TO "PL",                              IH342     
037520         OR MSTR-GS-GRADE GREATER THAN "15",                      IH342     
037530         OR (MSTR-GS-GRADE EQUAL TO "15",                         IH342     
037540         AND MSTR-STEP-NO EQUAL TO "10" ),                        IH342     
037550             MOVE +00000.00 TO OT-MONEY, OTHER-MONEY,             IH342     
037560                 GO TO 264160.                                    IH342     
037570 ERNOTE1.                                                         IH342     
037580     NOTE ******************************************************* IH342     
037590          *                                                     * IH342     
037600          *  WRITE NOTIFICATION MESSAGE                         * IH342     
037610          *      CURRENT EARNED EXCEEDS MAXIMUM                 * IH342     
037620          *                                                     * IH342     
037630          *******************************************************.IH342     
037640 261000.                                                          IH342     
037650     IF OT-MONEY ZERO,                                            IH342     
037660         ADD HRLY-RATE-WORK TO OTHER-MONEY,                       IH342     
037670             GO TO 262160.                                        IH342     
037680         ADD HRLY-RATE-WORK TO OT-MONEY.                          IH342     
037690 262160.                                                          IH342     
037700     MOVE ANNUM-MAX-PAY TO MSG4-MAX.                              IH342     
037710     MOVE HRLY-RATE-WORK TO MSG4-AMT.                             IH342     
037720     MOVE MESSAGE4 TO ERR-EXPLAIN.                                IH342     
037730     ALTER 198140 TO PROCEED TO 264020.                           IH342     
037740         GO TO 228200.                                            IH342     
037750 264020.                                                          IH342     
037760     IF OT-MONEY POSITIVE,                                        IH342     
037770         GO TO 264160.                                            IH342     
037780     ADD OT-MONEY TO OTHER-MONEY.                                 IH342     
037790     MOVE +00000.00 TO OT-MONEY.                                  IH342     
037800     NOTE ******************************************************* IH342     
037810          *                                                     * IH342     
037820          *  TEST FOR ADJUSTMENT TO REGULAR MONEY               * IH342     
037830          *      WRITE NOTIFICATION MESSAGE IF REGULAR          * IH342     
037840          *          MONEY ADJUSTED TO CREDIT                   * IH342     
037850          *                                                     * IH342     
037860          *******************************************************.IH342     
037870 264160.                                                          IH342     
037880     IF ADJ-REG-MONEY ZERO,                                       IH342     
037890         GO TO 270090.                                            IH342     
037900     MOVE ADJ-REG-MONEY TO HRLY-RATE-WORK.                        IH342     
037910     MOVE +00000.00 TO ADJ-REG-MONEY.                             IH342     
037920 264184.                                                          IH342     
037930     ADD REG-MONEY TO HRLY-RATE-WORK.                             IH342     
037940     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
037950         OR HRLY-RATE-WORK ZERO,                                  IH342     
037960             GO TO 268140.                                        IH342     
037970         MOVE +00000.00 TO REG-MONEY.                             IH342     
037980         ADD OTHER-MONEY TO HRLY-RATE-WORK.                       IH342     
037990     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
038000         OR HRLY-RATE-WORK ZERO,                                  IH342     
038010             GO TO 268170.                                        IH342     
038020         MOVE +00000.00 TO OTHER-MONEY.                           IH342     
038030         ADD OT-MONEY TO HRLY-RATE-WORK.                          IH342     
038040     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
038050         OR HRLY-RATE-WORK ZERO,                                  IH342     
038060             GO TO 268200.                                        IH342     
038070         MOVE +00000.00 TO OT-MONEY.                              IH342     
038080         ADD ADJ-OTHER-MONEY TO HRLY-RATE-WORK.                   IH342     
038090         MOVE +00000.00 TO ADJ-OTHER-MONEY.                       IH342     
038100     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
038110         OR HRLY-RATE-WORK ZERO,                                  IH342     
038120             GO TO 268170.                                        IH342     
038130         ADD ADJ-OT-MONEY TO HRLY-RATE-WORK.                      IH342     
038140         MOVE +00000.00 TO ADJ-OT-MONEY.                          IH342     
038150     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
038160         OR HRLY-RATE-WORK ZERO,                                  IH342     
038170             GO TO 268200.                                        IH342     
038180     MOVE +00000.00 TO WK1-SUBJECT, WK2-SUBJECT, ADJ-WK-SUB.      IH342     
038190     ALTER 198140 TO PROCEED TO 278090.                           IH342     
038200     NOTE ******************************************************* IH342     
038210          *                                                     * IH342     
038220          *  WRITE NOTIFICATION MESSAGE                         * IH342     
038230          *      CURRENT EARNED ADJUSTED TO CREDIT              * IH342     
038240          *                                                     * IH342     
038250          *******************************************************.IH342     
038260 268100.                                                          IH342     
038270     MOVE HRLY-RATE-WORK TO MSG5-AMT.                             IH342     
038280     MOVE MESSAGE5 TO ERR-EXPLAIN.                                IH342     
038290         GO TO 228200.                                            IH342     
038300 268140.                                                          IH342     
038310     MOVE HRLY-RATE-WORK TO REG-MONEY.                            IH342     
038320         GO TO 270090.                                            IH342     
038330 268170.                                                          IH342     
038340     MOVE HRLY-RATE-WORK TO OTHER-MONEY.                          IH342     
038350         GO TO 270090.                                            IH342     
038360 268200.                                                          IH342     
038370     MOVE HRLY-RATE-WORK TO OT-MONEY.                             IH342     
038380     NOTE ******************************************************* IH342     
038390          *                                                     * IH342     
038400          *  TEST FOR ADJUSTMENT TO OTHER MONEY                 * IH342     
038410          *                                                     * IH342     
038420          *******************************************************.IH342     
038430 270090.                                                          IH342     
038440     IF ADJ-OTHER-MONEY ZERO,                                     IH342     
038450         GO TO 272060.                                            IH342     
038460     MOVE ADJ-OTHER-MONEY TO HRLY-RATE-WORK.                      IH342     
038470     MOVE +00000.00 TO ADJ-OTHER-MONEY.                           IH342     
038480     ADD OTHER-MONEY TO HRLY-RATE-WORK.                           IH342     
038490     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
038500         MOVE +00000.00 TO OTHER-MONEY,                           IH342     
038510             GO TO 264184.                                        IH342     
038520         GO TO 268170.                                            IH342     
038530     NOTE ******************************************************* IH342     
038540          *                                                     * IH342     
038550          *  TEST FOR ADJUSTMENT TO OVERTIME MONEY              * IH342     
038560          *                                                     * IH342     
038570          *******************************************************.IH342     
038580 272060.                                                          IH342     
038590     IF ADJ-OT-MONEY ZERO,                                        IH342     
038600         GO TO 274050.                                            IH342     
038610     MOVE ADJ-OT-MONEY TO HRLY-RATE-WORK.                         IH342     
038620     MOVE +00000.00 TO ADJ-OT-MONEY.                              IH342     
038630     ADD OT-MONEY TO HRLY-RATE-WORK.                              IH342     
038640     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
038650         MOVE HRLY-RATE-WORK TO WT-NEG-OT                         IH342     
038660         MOVE +00000.00 TO OT-MONEY,                              IH342     
038670             GO TO 264184.                                        IH342     
038680         GO TO 268200.                                            IH342     
038690     NOTE ******************************************************* IH342     
038700          *                                                     * IH342     
038710          *  TEST FOR ADJUSTMENT TO SUBJECT MONEY               * IH342     
038720          *      WRITE NOTIFICATION MESSAGE IF SUBJECT          * IH342     
038730          *          MONEY ADJUSTED TO CREDIT                   * IH342     
038740          *                                                     * IH342     
038750          *******************************************************.IH342     
038760 274050.                                                          IH342     
038770     MOVE ADJ-WK-SUB TO HRLY-RATE-WORK.                           IH342     
038780     MOVE +00000.00 TO ADJ-WK-SUB.                                IH342     
038790     ADD WK1-SUBJECT, WK2-SUBJECT TO HRLY-RATE-WORK.              IH342     
038800     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
038810         MOVE +00000.00 TO WK1-SUBJECT, WK2-SUBJECT,              IH342     
038820         MOVE HRLY-RATE-WORK TO MSG6-AMT,                         IH342     
038830         MOVE MESSAGE6 TO ERR-EXPLAIN,                            IH342     
038840         ALTER 198140 TO PROCEED TO 276040,                       IH342     
038850             GO TO 228200.                                        IH342     
038860         MOVE HRLY-RATE-WORK TO WK1-SUBJECT.                      IH342     
038870     NOTE ******************************************************* IH342     
038880          *                                                     * IH342     
038890          *  COMPUTE PAYROLL GROSS MONEY                        * IH342     
038900          *                                                     * IH342     
038910          *******************************************************.IH342     
038920 276040.                                                          IH342     
038930     IF PAYOFF-SW PERFORM 214120 THRU 214125.                     IH342     
038940     ADD REG-MONEY, OT-MONEY, OTHER-MONEY,                        IH342     
038950         GIVING HRLY-RATE-WORK.                                   IH342     
038960     ADD HRLY-RATE-WORK TO CURR-EARNED.                           IH342     
038970     ADD HRLY-RATE-WORK TO NET-PAY.                               IH342     
038980     IF DECEASED-PAYOFF,                                          IH342     
038990         GO TO 278090.                                            IH342     
039000     ADD HRLY-RATE-WORK TO MSTR-YTD-TOT-EARN.                     IH342     
039010     ADD REG-MONEY TO MSTR-YTD-REG-MONEY.                         IH342     
039020     ADD OTHER-MONEY TO MSTR-YTD-OTH-MONEY.                       IH342     
039030     ADD OT-MONEY TO MSTR-YTD-OT-MONEY.                           IH342     
039040     NOTE ******************************************************* IH342     
039050          *                                                     * IH342     
039060          *  COMPUTATION OF FICA DEDUCTION ROUTINE              * IH342     
039070          *      YTD SUBJECT TO FICA MAXIMUM IS 9,000 DOLLARS   * IH342     
039080          *      FICA DEDUCTION PERCENTAGE IS 5-2 OF GROSS     *  IH342     
039090          *      FICA DEDUCTION MAXIMUM IS 468 DOLLARS          * IH342     
039100          *      WRITE NOTIFICATION MESSAGE FOR FICA            * IH342     
039110          *          DEDUCTION ADJUSTED TO CREDIT               * IH342     
039120          *                                                     * IH342     
039130          *******************************************************.IH342     
039140 278090.                                                          IH342     
039150     IF NOT FICA-MSTR,                                            IH342     
039160         GO TO 286130.                                            IH342     
039170     EXAMINE MSTR-YTD-SUB-FICA-RE REPLACING ALL " " BY "0".       IH342     
039180     SUBTRACT +9000.00 FROM MSTR-YTD-SUB-FICA                     IH342     
039190         GIVING HRLY-RATE-WORK.                                   IH342     
039200     IF HRLY-RATE-WORK ZERO,                                      IH342     
039210         GO TO 282110.                                            IH342     
039220     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
039230         ALTER 198140 TO PROCEED TO 290150,                       IH342     
039240         MOVE "YTD SUBJECT TO FICA OVER MAXIMUM" TO ERR-EXPLAIN,  IH342     
039250             GO TO 228200.                                        IH342     
039260     ADD CURR-EARNED TO HRLY-RATE-WORK.                           IH342     
039270     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
039280         GO TO 282170.                                            IH342     
039290     MOVE CURR-EARNED TO WK1-SUBJECT.                             IH342     
039300 280040.                                                          IH342     
039310     ADD WK1-SUBJECT TO MSTR-QTR-SUB-FICA.                        IH342     
039320     ADD WK1-SUBJECT TO MSTR-YTD-SUB-FICA.                        IH342     
039330     ADD WK1-SUBJECT TO SUBJECT-FICA.                             IH342     
039340     MULTIPLY WK1-SUBJECT BY +.052                                IH342     
039350         GIVING RETIRE-FICA ROUNDED.                              IH342     
039360     IF FICA-ADJUSTMENT,                                          IH342     
039370         GO TO 284040.                                            IH342     
039380 280130.                                                          IH342     
039390     IF WRITE-CASH-AWARD                                          IH342     
039400         MOVE CURR-EARNED TO NET-PAY.                             IH342     
039410     SUBTRACT RETIRE-FICA FROM NET-PAY,                           IH342     
039420         GIVING HRLY-RATE-WORK.                                   IH342     
039430     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
039440         ADD HRLY-RATE-WORK TO RETIRE-FICA,                       IH342     
039450         ALTER 198140 TO PROCEED TO 280130,                       IH342     
039460             GO TO 268100.                                        IH342     
039470     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
039480     IF NET-PAY ZERO,                                             IH342     
039490         GO TO 282072.                                            IH342     
039500 282022.                                                          IH342     
039510     IF FICA-MAXIMUM,                                             IH342     
039520         SUBTRACT MSTR-YTD-FICA, RETIRE-FICA, FROM +00468.00      IH342     
039530             GIVING HRLY-RATE-WORK,                               IH342     
039540         ADD HRLY-RATE-WORK TO RETIRE-FICA,                       IH342     
039550         SUBTRACT HRLY-RATE-WORK FROM NET-PAY.                    IH342     
039560 282072.                                                          IH342     
039570     MOVE SPACE TO SW35.                                          IH342     
039580     ADD RETIRE-FICA TO MSTR-YTD-FICA.                            IH342     
039590         GO TO 290150.                                            IH342     
039600 282110.                                                          IH342     
039610     MOVE +00000.00 TO WK1-SUBJECT.                               IH342     
039620     IF DECEASED-PAYOFF,                                          IH342     
039630         GO TO 290150.                                            IH342     
039640     ADD CURR-EARNED TO MSTR-YTD-NOT-FICA.                        IH342     
039650         GO TO 290150.                                            IH342     
039660 282170.                                                          IH342     
039670     IF NOT DECEASED-PAYOFF,                                      IH342     
039680         ADD HRLY-RATE-WORK TO MSTR-YTD-NOT-FICA.                 IH342     
039690     MULTIPLY -1.00 BY HRLY-RATE-WORK.                            IH342     
039700     ADD CURR-EARNED, HRLY-RATE-WORK,                             IH342     
039710         GIVING WK1-SUBJECT.                                      IH342     
039720     MOVE "1" TO SW35.                                            IH342     
039730             GO TO 280040.                                        IH342     
039740 284040.                                                          IH342     
039750     ADD ADJ-RETIRE TO RETIRE-FICA.                               IH342     
039760     IF RETIRE-FICA ZERO,                                         IH342     
039770         GO TO 282022.                                            IH342     
039780     IF RETIRE-FICA POSITIVE,                                     IH342     
039790         GO TO 280130.                                            IH342     
039800     ALTER 198140 TO PROCEED TO 280130.                           IH342     
039810     NOTE ******************************************************* IH342     
039820          *                                                     * IH342     
039830          *  WRITE NOTIFICATION MESSAGE                         * IH342     
039840          *      FICA OR CSRA DEDUCTION ADJUSTED TO CREDIT      * IH342     
039850          *                                                     * IH342     
039860          *******************************************************.IH342     
039870 284190.                                                          IH342     
039880     MOVE RETIRE-FICA TO MSG7-AMT.                                IH342     
039890 284210.                                                          IH342     
039900     MOVE MESSAGE7 TO ERR-EXPLAIN.                                IH342     
039910         GO TO 228200.                                            IH342     
039920 285000.                                                          IH342     
039930     NOTE ******************************************************* IH342     
039940          *                                                     * IH342     
039950          *  COMPUTATION OF CSRA DEDUCTION ROUTINE              * IH342     
039960          *      CSRA DEDUCTION PERCENTAGE IS  7  OF SUBJECT    * IH342     
039970          *      WRITE NOTIFICATION MESSAGE FOR CSRA            * IH342     
039980          *          DEDUCTION ADJUSTED TO CREDIT               * IH342     
039990          *                                                     * IH342     
040000          *******************************************************.IH342     
040010 286130.                                                          IH342     
040020     IF WRITE-CASH-AWARD,                                         IH342     
040030         GO TO 290150.                                            IH342     
040040     IF NOT CSRA-MSTR,                                            IH342     
040050         GO TO 290150.                                            IH342     
040060     IF NOT DECEASED-PAYOFF,                                      IH342     
040070         ADD WK1-SUBJECT TO MSTR-YTD-SUB-RET.                     IH342     
040080     ADD WK1-SUBJECT TO SUBJECT-RETIRE.                           IH342     
040090     ADD WK1-SUBJECT TO TOT-SUB-RETIRE.                           IH342     
040100     MULTIPLY WK1-SUBJECT BY +.07,                                IH342     
040110         GIVING RETIRE-FICA ROUNDED.                              IH342     
040120     IF CSRA-ADJUSTMENT,                                          IH342     
040130         GO TO 288170.                                            IH342     
040140 288060.                                                          IH342     
040150     SUBTRACT RETIRE-FICA FROM NET-PAY,                           IH342     
040160         GIVING HRLY-RATE-WORK.                                   IH342     
040170     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
040180         ADD HRLY-RATE-WORK TO RETIRE-FICA,                       IH342     
040190         ALTER 198140 TO PROCEED TO 288060,                       IH342     
040200             GO TO 268100.                                        IH342     
040210     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
040220     ADD RETIRE-FICA TO MSTR-YTD-RETIRE.                          IH342     
040230     ADD RETIRE-FICA TO MSTR-TOT-RETIRE.                          IH342     
040240         GO TO 290150.                                            IH342     
040250 288170.                                                          IH342     
040260     ADD ADJ-RETIRE TO RETIRE-FICA.                               IH342     
040270     IF RETIRE-FICA ZERO,                                         IH342     
040280         GO TO 290150.                                            IH342     
040290     IF RETIRE-FICA POSITIVE,                                     IH342     
040300         GO TO 288060.                                            IH342     
040310     ALTER 198140 TO PROCEED TO 288060.                           IH342     
040320         GO TO 284190.                                            IH342     
040330     NOTE ******************************************************* IH342     
040340          *                                                     * IH342     
040350          *  COMPUTATION OF FEDERAL TAX DEDUCTION ROUTINE       * IH342     
040360          *      SEPARATE DEDUCTION TABLE FOR SINGLE OR MARRIED * IH342     
040370          *      ADDED TAX DEDUCTIONS ONLY IF NO EXEMPTIONS     * IH342     
040380          *      COMPUTE TAX ONLY IF CURRENT EARNED OVER $8     * IH342     
040390          *                                                     * IH342     
040400          *******************************************************.IH342     
040410 290150.                                                          IH342     
040420     IF DECEASED-PAYOFF,                                          IH342     
040430         GO TO 298000.                                            IH342     
040440     IF WRITE-CASH-AWARD AND FICA-MSTR GO TO 297000.              IH342     
040450     IF WRITE-CASH-AWARD,                                         IH342     
040460         MOVE CURR-EARNED TO NET-PAY,                             IH342     
040470             GO TO 297000.                                        IH342     
040480     IF ADJ-NSFT,                                                 IH342     
040490     SUBTRACT NOT-SUBJ-FED-TAX FROM CURR-EARNED.                  IH342     
040500     IF CD10-TAX-ADJ                                              IH342     
040510         GO TO 297000.                                            IH342     
040520     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
040530     IF MSTR-TAX-CODE NOT ZERO,                                   IH342     
040540         MULTIPLY MSTR-TAX-CODE BY -28.80                         IH342     
040550             GIVING HRLY-RATE-WORK.                               IH342     
040560     ADD CURR-EARNED TO HRLY-RATE-WORK.                           IH342     
040570     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
040580         GO TO 294140.                                            IH342     
040590 292052.                                                          IH342     
040600     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
040610 292070.                                                          IH342     
040620     MOVE HRLY-RATE-WORK TO FED-TAXES.                            IH342     
040630     IF MSTR-ADDED-TAX EQUAL TO ZEROS OR SPACES GO TO 292122.     IH342     
040640     ADD MSTR-ADDED-TAX TO FED-TAXES.                             IH342     
040650 292122.                                                          IH342     
040660     IF TAX-ADJUSTMENT,                                           IH342     
040670         GO TO 294050.                                            IH342     
040680 292150.                                                          IH342     
040690     SUBTRACT FED-TAXES FROM NET-PAY,                             IH342     
040700         GIVING HRLY-RATE-WORK.                                   IH342     
040710     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
040720         ADD HRLY-RATE-WORK TO FED-TAXES,                         IH342     
040730         ALTER 198140 TO PROCEED TO 292150,                       IH342     
040740             GO TO 268100.                                        IH342     
040750     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
040760     ADD FED-TAXES TO MSTR-YTD-FED-TAX.                           IH342     
040770         GO TO 298000.                                            IH342     
040780 294050.                                                          IH342     
040790     ADD ADJ-TAXES TO FED-TAXES.                                  IH342     
040800     IF FED-TAXES ZERO,                                           IH342     
040810         GO TO 298000.                                            IH342     
040820     IF FED-TAXES POSITIVE,                                       IH342     
040830         GO TO 292150.                                            IH342     
040840     ALTER 198140 TO PROCEED TO 292150.                           IH342     
040850     MOVE FED-TAXES TO MSG7-AMT.                                  IH342     
040860         GO TO 284210.                                            IH342     
040870 294140.                                                          IH342     
040880     IF HRLY-RATE-WORK NOT GREATER THAN +00021.00                 IH342     
040890         GO TO 292052.                                            IH342     
040900     IF MARRIED-MSTR,                                             IH342     
040910         MOVE 07 TO SUBS-1                                        IH342     
040920         MOVE 08 TO SUBS-2                                        IH342     
040930         MOVE 08 TO SUBS-3                                        IH342     
040940             GO TO 296020.                                        IH342     
040950     MOVE 01 TO SUBS-1, SUBS-2.                                   IH342     
040960     MOVE 01 TO SUBS-3.                                           IH342     
040970 296020.                                                          IH342     
040980     IF HRLY-RATE-WORK NOT GREATER THAN TAXABLE-WAGES (SUBS-1),   IH342     
040990         SUBTRACT +21.00 FROM HRLY-RATE-WORK.                     IH342     
041000 296060.                                                          IH342     
041010     SUBTRACT TAXABLE-WAGES (SUBS-1) FROM HRLY-RATE-WORK.         IH342     
041020     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
041030         GO TO 296150.                                            IH342     
041040     ADD TAXABLE-WAGES (SUBS-1) TO HRLY-RATE-WORK.                IH342     
041050 296110.                                                          IH342     
041060     MULTIPLY EXCESS-PERCENT (SUBS-3) BY HRLY-RATE-WORK ROUNDED.  IH342     
041070     ADD TAX-DEDUCTIONS (SUBS-2) TO HRLY-RATE-WORK.               IH342     
041080         GO TO 292070.                                            IH342     
041090 296150.                                                          IH342     
041100     ADD 1 TO SUBS-1.                                             IH342     
041110     ADD 1 TO SUBS-2.                                             IH342     
041120     ADD 1 TO SUBS-3.                                             IH342     
041130     IF SUBS-3 EQUAL TO 07 OR 14                                  IH342     
041140         GO TO 296110.                                            IH342     
041150     GO TO 296060.                                                IH342     
041160 297000.                                                          IH342     
041170     MULTIPLY CURR-EARNED BY +.20 GIVING FED-TAXES ROUNDED.       IH342     
041180     GO TO 292122.                                                IH342     
041190 298000.                                                          IH342     
041200     ADD NOT-SUBJ-FED-TAX TO CURR-EARNED.                         IH342     
041210     NOTE ******************************************************* IH342     
041220          *                                                     * IH342     
041230          *  HEALTH INSURANCE DEDUCTION ROUTINE                 * IH342     
041240          *      WRITE NOTIFICATION MESSAGE IF NORMAL HEALTH    * IH342     
041250          *          INSURANCE DEDUCTION NOT TAKEN              * IH342     
041260          *                                                     * IH342     
041270          *******************************************************.IH342     
041280 298080.                                                          IH342     
041290     EXAMINE MSTR-HLTH-CODE-RE REPLACING ALL " " BY "0".          IH342     
041300     EXAMINE MSTR-HLTH-DED-RE  REPLACING ALL " " BY "0".          IH342     
041310     EXAMINE MSTR-HLTH-CONTRIB-RE REPLACING ALL " " BY "0".       IH342     
041320     IF WRITE-CASH-AWARD,                                         IH342     
041330         GO TO 328100.                                            IH342     
041340     IF CANCEL-HEALTH,                                            IH342     
041350         MOVE SPACE TO SW14,                                      IH342     
041360             GO TO 302200.                                        IH342     
041370     IF MSTR-HLTH-CODE NOT GREATER THAN +003                      IH342     
041380         GO TO 306030.                                            IH342     
041390     IF PAYOFF-LWOP GO TO 302200.                                 IH342     
041400     IF FIRE-FIGHTER-MSTR,                                        IH342     
041410         MOVE -144.00 TO HRLY-RATE-WORK,                          IH342     
041420             GO TO 300030.                                        IH342     
041430     IF FIRE-CHIEF-MSTR,                                          IH342     
041440         MOVE -112.00 TO HRLY-RATE-WORK,                          IH342     
041450             GO TO 300030.                                        IH342     
041460         MOVE -80.00 TO HRLY-RATE-WORK.                           IH342     
041470 300030.                                                          IH342     
041480     ADD LWOP-WK1, LWOP-WK2 TO HRLY-RATE-WORK.                    IH342     
041490     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
041500         OR HRLY-RATE-WORK ZERO,                                  IH342     
041510             GO TO 302200.                                        IH342     
041520     MOVE MSTR-HLTH-DED TO HLTH-INS-DED.                          IH342     
041530     MOVE MSTR-HLTH-CONTRIB TO HLTH-INS-CON.                      IH342     
041540     IF HLTH-DED-ADJUSTMENT,                                      IH342     
041550         GO TO 302020.                                            IH342     
041560 300102.                                                          IH342     
041570     SUBTRACT HLTH-INS-DED FROM NET-PAY,                          IH342     
041580         GIVING HRLY-RATE-WORK.                                   IH342     
041590     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
041600         GO TO 304050.                                            IH342     
041610     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
041620     ADD HLTH-INS-DED TO EMPLOYEE-HEALTH.                         IH342     
041630     ADD HLTH-INS-CON TO EMPLOYER-HEALTH.                         IH342     
041640     ADD 1 TO HEALTH-COUNT.                                       IH342     
041650     ADD HLTH-INS-DED TO MSTR-YTD-HLTH-DED.                       IH342     
041660     ADD HLTH-INS-CON TO MSTR-YTD-HLTH-CON.                       IH342     
041670         GO TO 306030.                                            IH342     
041680 302020.                                                          IH342     
041690     ADD ADJ-HEALTH-DED TO HLTH-INS-DED.                          IH342     
041700     ADD ADJ-HEALTH-CON TO HLTH-INS-CON.                          IH342     
041710     IF HLTH-INS-DED ZERO,                                        IH342     
041720         GO TO 304050.                                            IH342     
041730     IF HLTH-INS-DED POSITIVE,                                    IH342     
041740         GO TO 300102.                                            IH342     
041750     ALTER 198140 TO PROCEED TO 300102.                           IH342     
041760     MOVE HLTH-INS-DED TO MSG7-AMT.                               IH342     
041770         GO TO 284210.                                            IH342     
041780     NOTE ******************************************************* IH342     
041790          *                                                     * IH342     
041800          *  WRITE NOTIFICATION MESSAGE                         * IH342     
041810          *      HEALTH INSURANCE NORMAL DEDUCTION NOT TAKEN    * IH342     
041820          *                                                     * IH342     
041830          *******************************************************.IH342     
041840 302200.                                                          IH342     
041850     IF HLTH-DED-ADJUSTMENT,                                      IH342     
041860         MOVE ADJ-HEALTH-DED TO HLTH-INS-DED,                     IH342     
041870         MOVE ADJ-HEALTH-CON TO HLTH-INS-CON,                     IH342     
041880             GO TO 300102.                                        IH342     
041890 304050.                                                          IH342     
041900     MOVE +000.00 TO HLTH-INS-DED, HLTH-INS-CON.                  IH342     
041910     MOVE MSTR-HLTH-CODE TO MSG8-CODE.                            IH342     
041920     MOVE MSTR-HLTH-DED TO MSG8-AMT1.                             IH342     
041930     MOVE MSTR-HLTH-CONTRIB TO MSG8-AMT2.                         IH342     
041940     MOVE MESSAGE8 TO ERR-EXPLAIN.                                IH342     
041950     ALTER 198140 TO PROCEED TO 306030.                           IH342     
041960         GO TO 228200.                                            IH342     
041970     NOTE ******************************************************* IH342     
041980          *                                                     * IH342     
041990          *  LIFE INSURANCE DEDUCTION ROUTINE                   * IH342     
042000          *      LIFE INSURANCE OPTION DEDUCTION INCLUDED       * IH342     
042010          *      COMPUTE LIFE INSURANCE AT 80TH HOUR            * IH342     
042020          *                                                     * IH342     
042030          *******************************************************.IH342     
042040 306030.                                                          IH342     
042050     IF CANCEL-LIFE,                                              IH342     
042060         MOVE +000.00 TO LIFE-INS-DED, LIFE-OPT-DED,              IH342     
042070             GO TO 308060.                                        IH342     
042080     IF NOT LIFE-INSUR-MSTR,                                      IH342     
042090         GO TO 316100.                                            IH342     
042100     IF PAYOFF-LWOP                                               IH342     
042110         MOVE +000.00 TO LIFE-INS-DED LIFE-OPT-DED                IH342     
042120         GO TO 308060.                                            IH342     
042130     IF FIRE-FIGHTER-MSTR,                                        IH342     
042140         MOVE -144.00 TO HRLY-RATE-WORK,                          IH342     
042150             GO TO 306170.                                        IH342     
042160     IF FIRE-CHIEF-MSTR,                                          IH342     
042170         MOVE -112.00 TO HRLY-RATE-WORK,                          IH342     
042180             GO TO 306170.                                        IH342     
042190     MOVE -80.00 TO HRLY-RATE-WORK.                               IH342     
042200 306170.                                                          IH342     
042210     ADD LWOP-WK1, LWOP-WK2 TO HRLY-RATE-WORK.                    IH342     
042220     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
042230         OR HRLY-RATE-WORK ZERO,                                  IH342     
042240             MOVE "1" TO SW13,                                    IH342     
042250             MOVE +000.00 TO LIFE-INS-DED, LIFE-OPT-DED,          IH342     
042260                 GO TO 308060.                                    IH342     
042270     MOVE MSTR-LIFE-INS-DED TO LIFE-INS-DED.                      IH342     
042280 308055.                                                          IH342     
042290     MOVE MSTR-LIFE-OPT-DED TO LIFE-OPT-DED.                      IH342     
042300 308060.                                                          IH342     
042310     IF LIFE-INS-ADJUSTMENT,                                      IH342     
042320         GO TO 310010.                                            IH342     
042330 308082.                                                          IH342     
042340     IF LIFE-OPT-ADJUSTMENT,                                      IH342     
042350         GO TO 310160.                                            IH342     
042360 308110.                                                          IH342     
042370     SUBTRACT LIFE-INS-DED, LIFE-OPT-DED FROM NET-PAY,            IH342     
042380         GIVING HRLY-RATE-WORK.                                   IH342     
042390     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
042400         ALTER 198140 TO PROCEED TO 312170,                       IH342     
042410             GO TO 268100.                                        IH342     
042420     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
042430     ADD LIFE-INS-DED TO MSTR-YTD-LIFE-INS.                       IH342     
042440     ADD LIFE-OPT-DED TO MSTR-YTD-LIFE-OPT.                       IH342     
042450         GO TO 316100.                                            IH342     
042460 310010.                                                          IH342     
042470     ADD ADJ-LIFE-INS TO LIFE-INS-DED.                            IH342     
042480     IF LIFE-INS-DED NEGATIVE,                                    IH342     
042490         ALTER 198140 TO PROCEED TO 308082,                       IH342     
042500         MOVE LIFE-INS-DED TO MSG7-AMT,                           IH342     
042510             GO TO 284210.                                        IH342     
042520         GO TO 308082.                                            IH342     
042530 310160.                                                          IH342     
042540     IF MSTR-LIFE-OPT-CODE IS LESS THAN "A"                       IH342     
042550         AND GREATER THAN "G",                                    IH342     
042560         MOVE "INCORRECT LIFE OPTION ADJUSTMENT" TO ERR-EXPLAIN,  IH342     
042570         ALTER 198140 TO PROCEED TO 308110,                       IH342     
042580             GO TO 228200.                                        IH342     
042590     ADD ADJ-LIFE-OPT TO LIFE-OPT-DED.                            IH342     
042600     IF LIFE-OPT-DED NEGATIVE,                                    IH342     
042610         ALTER 198140 TO PROCEED TO 308110,                       IH342     
042620         MOVE LIFE-OPT-DED TO MSG7-AMT,                           IH342     
042630             GO TO 284210.                                        IH342     
042640         GO TO 308110.                                            IH342     
042650 312170.                                                          IH342     
042660     ADD LIFE-OPT-DED TO HRLY-RATE-WORK.                          IH342     
042670     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
042680         MOVE +000.00 TO LIFE-OPT-DED,                            IH342     
042690         ADD HRLY-RATE-WORK TO LIFE-INS-DED,                      IH342     
042700             GO TO 308110.                                        IH342     
042710         MOVE HRLY-RATE-WORK TO LIFE-OPT-DED.                     IH342     
042720         GO TO 308110.                                            IH342     
042730     NOTE ******************************************************* IH342     
042740          *                                                     * IH342     
042750          *  BOND DEDUCTION ROUTINE                             * IH342     
042760          *                                                     * IH342     
042770          *******************************************************.IH342     
042780 316100.                                                          IH342     
042790     IF CANCEL-BOND,                                              IH342     
042800         GO TO 324170.                                            IH342     
042810     MOVE MSTR-BOND-DED TO BOND-DED.                              IH342     
042820     IF PAYOFF-LWOP                                               IH342     
042830         MOVE +000.00 TO BOND-DED.                                IH342     
042840     IF BOND-ADJUST,                                              IH342     
042850        GO TO 317000.                                             IH342     
042860 316300.                                                          IH342     
042870     MOVE 01 TO SUBS-1.                                           IH342     
042880 316350.                                                          IH342     
042890     SUBTRACT BOND-DED FROM NET-PAY,                              IH342     
042900         GIVING HRLY-RATE-WORK.                                   IH342     
042910     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
042920         MOVE +000.00 TO BOND-DED,                                IH342     
042930             GO TO 324170.                                        IH342     
042940         MOVE HRLY-RATE-WORK TO NET-PAY.                          IH342     
042950     ADD BOND-DED TO MSTR-YTD-BONDS.                              IH342     
042960     IF BOND-DED POSITIVE                                         IH342     
042970          ADD BOND-DED TO BOND-DEDUCTION                          IH342     
042980        ADD 1 TO BOND-COUNT.                                      IH342     
042990     GO TO 324170.                                                IH342     
043000 317000.                                                          IH342     
043010     ADD ADJ-TO-BONDS TO BOND-DED.                                IH342     
043020     IF BOND-DED ZERO GO TO 324170.                               IH342     
043030     IF BOND-DED POSITIVE, GO TO 316350.                          IH342     
043040     ALTER 198140 TO PROCEED TO 316350.                           IH342     
043050     MOVE BOND-DED TO MSG7-AMT.                                   IH342     
043060        GO TO 284210.                                             IH342     
043070     NOTE ******************************************************* IH342     
043080          *                                                     * IH342     
043090          *  GOVERNMENT INDEBTED DEDUCTION ROUTINE              * IH342     
043100          *      WRITE NOTIFICATION MESSAGE IF PAYMENT          * IH342     
043110          *          EXCEEDS NET PAY                            * IH342     
043120          *      LAST PAYMENT AUTOMATICALLY REDUCED TO          * IH342     
043130          *          BALANCE AMOUNT                             * IH342     
043140          *                                                     * IH342     
043150          *******************************************************.IH342     
043160 324170.                                                          IH342     
043170     IF NOT ADJ-OTH-DED,                                          IH342     
043180         GO TO 325050.                                            IH342     
043190     SUBTRACT ADJ-TO-OTH-DED FROM NET-PAY,                        IH342     
043200         GIVING HRLY-RATE-WORK.                                   IH342     
043210     IF HRLY-RATE-WORK POSITIVE,                                  IH342     
043220         GO TO 325000.                                            IH342     
043230     MOVE 0000.00 TO ADJ-TO-OTH-DED.                              IH342     
043240 325000.                                                          IH342     
043250     ADD ADJ-TO-OTH-DED TO MSTR-YTD-OTH-DED.                      IH342     
043260 325050.                                                          IH342     
043270     IF MSTR-YTD-GOV-INDEBT ZERO,                                 IH342     
043280         GO TO 328100.                                            IH342     
043290     SUBTRACT MSTR-YTD-GOV-PAY FROM NET-PAY,                      IH342     
043300         GIVING HRLY-RATE-WORK.                                   IH342     
043310     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
043320         ALTER 198140 TO PROCEED TO 328100,                       IH342     
043330         MOVE "PAYMENT EXCEEDS NET PAY, NO PAYMENT" TO            IH342     
043340             ERR-EXPLAIN,                                         IH342     
043350                 GO TO 228200.                                    IH342     
043360         MOVE HRLY-RATE-WORK TO NET-PAY.                          IH342     
043370         MOVE MSTR-YTD-GOV-PAY TO HRLY-RATE-WORK.                 IH342     
043380         SUBTRACT MSTR-YTD-GOV-PAY FROM MSTR-YTD-GOV-INDEBT.      IH342     
043390     IF MSTR-YTD-GOV-INDEBT ZERO,                                 IH342     
043400         GO TO 326150.                                            IH342     
043410     IF MSTR-YTD-GOV-INDEBT POSITIVE,                             IH342     
043420         GO TO 326180.                                            IH342     
043430     ADD MSTR-YTD-GOV-INDEBT TO HRLY-RATE-WORK.                   IH342     
043440     SUBTRACT MSTR-YTD-GOV-INDEBT FROM NET-PAY.                   IH342     
043450 326150.                                                          IH342     
043460     MOVE +0000.00 TO MSTR-YTD-GOV-INDEBT,                        IH342     
043470         MSTR-YTD-GOV-PAY.                                        IH342     
043480 326180.                                                          IH342     
043490     ADD HRLY-RATE-WORK TO OTHER-DED.                             IH342     
043500     ADD HRLY-RATE-WORK TO MSTR-YTD-OTH-DED.                      IH342     
043510     NOTE ******************************************************* IH342     
043520          *                                                     * IH342     
043530          *  COMPUTATION OF STATE TAX DEDUCTION ROUTINE         * IH342     
043540          *      ARIZONA STATE TAX IS 1 PERCENT OF GROSS        * IH342     
043550          *      UTAH STATE TAX IS 14 PERCENT OF FED TAX        * IH342     
043560          *      MISSOURI STATE TAX IS 9 PERCENT OF FED TAX     * IH342     
043570          *                                                     * IH342     
043580          *******************************************************.IH342     
043590 328100.                                                          IH342     
043600     IF MSTR-ST-TAX-EXEM-CD-RE EQUALS "ZZ" GO TO 332120.          IH342     
043610     EXAMINE MSTR-ST-TAX-EXEM-CD-RE REPLACING ALL " " BY "0".     IH342     
043620     IF DECEASED-PAYOFF,                                          IH342     
043630         GO TO 332120.                                            IH342     
043640     IF CURR-EARNED EQUALS ZEROS GO TO 332120.                    IH342     
043650     IF MSTR-STATE-TAX-CODE IS EQUAL TO "0" GO TO 332120.         IH342     
043660     IF MSTR-STATE-TAX-CODE IS EQUAL TO " " GO TO 332120.         IH342     
043670     MOVE CURR-EARNED TO ST-CURR-EARNED.                          IH342     
043680     MOVE MSTR-ST-TAX-EXEM-CD TO DEP-ALLOW.                       IH342     
043690     IF MSTR-STATE-TAX-CODE = "B" GO TO ALABAMA.                  IH342     
043700     IF MSTR-STATE-TAX-CODE = "G" GO TO GEORGIA.                  IH342     
043710     IF MSTR-STATE-TAX-CODE = "V" GO TO VIRGINIA.                 IH342     
043720     IF MSTR-STATE-TAX-CODE = "E" GO TO MASSACHUSETTS.            IH342     
043730     IF MSTR-STATE-TAX-CODE = "M" GO TO MARYLAND.                 IH342     
043740     IF MSTR-STATE-TAX-CODE = "C" GO TO NORTH-CAROLINA.           IH342     
043750     IF MSTR-STATE-TAX-CODE = "Y" GO TO NEW-YORK.                 IH342     
043760     IF MSTR-STATE-TAX-CODE EQUAL TO "P" GO TO PENNSYLVANIA.      IH342     
043770     IF MSTR-STATE-TAX-CODE IS EQUAL TO "W" GO TO 328200.         IH342     
043780     IF MSTR-STATE-TAX-CODE EQUAL TO "F" GO TO OHIO.              IH342     
043790     IF MSTR-STATE-TAX-CODE EQUAL TO "A" OR "D" OR "T"            IH342     
043800         MULTIPLY FED-TAXES BY +.10,                              IH342     
043810             GIVING STATE-TAXES ROUNDED,                          IH342     
043820                 GO TO 330010.                                    IH342     
043830     IF MSTR-STATE-TAX-CODE EQUAL TO "R"                          IH342     
043840         MULTIPLY FED-TAXES BY +.15                               IH342     
043850         GIVING STATE-TAXES ROUNDED                               IH342     
043860         GO TO 330010.                                            IH342     
043870     IF MSTR-STATE-TAX-CODE = "J" OR "K"  GO TO SOUTH-CAROLINA.   IH342     
043880     IF MSTR-STATE-TAX-CODE EQUAL TO "S",                         IH342     
043890             MULTIPLY FED-TAXES BY +.09,                          IH342     
043900             GIVING STATE-TAXES ROUNDED,                          IH342     
043910                 GO TO 330010.                                    IH342     
043920     IF MSTR-STATE-TAX-CODE EQUAL TO "U",                         IH342     
043930         MULTIPLY FED-TAXES BY +.14,                              IH342     
043940             GIVING STATE-TAXES ROUNDED GO TO 330010.             IH342     
043950     GO TO 330020.                                                IH342     
043960 PENNSYLVANIA.                                                    IH342     
043970     MULTIPLY ST-CURR-EARNED BY .023 GIVING STATE-TAXES ROUNDED.  IH342     
043980     IF DEP-ALLOW EQUAL TO ZEROS GO TO END-ECOAST-TAXES.          IH342     
043990     ADD DEP-ALLOW TO STATE-TAXES.                                IH342     
044000     GO TO END-ECOAST-TAXES.                                      IH342     
044010 ALABAMA.                                                         IH342     
044020     MULTIPLY ST-CURR-EARNED BY +26 GIVING YRLY-GROSS.            IH342     
044030     MULTIPLY YRLY-GROSS BY +.07 GIVING YRLY-ADJ-GROSS.           IH342     
044040     IF +500.00 IS LESS THAN YRLY-ADJ-GROSS MOVE +500.00 TO       IH342     
044050     YRLY-ADJ-GROSS.                                              IH342     
044060     SUBTRACT YRLY-ADJ-GROSS FROM YRLY-GROSS.                     IH342     
044070     MULTIPLY FED-TAXES BY +26 GIVING YRLY-FED-TAXES.             IH342     
044080     SUBTRACT YRLY-FED-TAXES FROM YRLY-GROSS GIVING YRLY-NET.     IH342     
044090     IF ALLOWANCE GREATER YRLY-NET GO TO END-ECOAST-TAXES.        IH342     
044100     SUBTRACT ALLOWANCE FROM YRLY-NET GIVING TAXABLE.             IH342     
044110     IF TAXABLE IS NOT GREATER THAN +1000.00 GO TO ALA-1.         IH342     
044120     MOVE +15.00 TO STATAX-1.                                     IH342     
044130     SUBTRACT +1000.00 FROM TAXABLE.                              IH342     
044140     IF TAXABLE IS NOT GREATER THAN +2000.00 GO TO ALA-2.         IH342     
044150     MOVE +60.00 TO STATAX-2.                                     IH342     
044160     SUBTRACT +2000.00 FROM TAXABLE.                              IH342     
044170     IF TAXABLE IS NOT GREATER THAN +2000.00 GO TO ALA-3.         IH342     
044180     MOVE +90.00 TO STATAX-3.                                     IH342     
044190     SUBTRACT +2000.00 FROM TAXABLE.                              IH342     
044200     MULTIPLY TAXABLE BY +.05 GIVING STATAX-4.                    IH342     
044210     GO TO FINISH-ALAGAVANC.                                      IH342     
044220 ALA-1.                                                           IH342     
044230     MULTIPLY TAXABLE BY +.015 GIVING STATAX-1.                   IH342     
044240     GO TO FINISH-ALAGAVANC.                                      IH342     
044250 ALA-2.                                                           IH342     
044260     MULTIPLY TAXABLE BY +.03 GIVING STATAX-2.                    IH342     
044270     GO TO FINISH-ALAGAVANC.                                      IH342     
044280 ALA-3.                                                           IH342     
044290     MULTIPLY TAXABLE BY +.045 GIVING STATAX-3.                   IH342     
044300 FINISH-ALAGAVANC.                                                IH342     
044310     ADD STATAX-1, STATAX-2, STATAX-3, STATAX-4, STATAX-5,        IH342     
044320     STATAX-6 GIVING YEARLY-STATAX.                               IH342     
044330     DIVIDE +26 INTO YEARLY-STATAX GIVING STATE-TAXES ROUNDED.    IH342     
044340 END-ECOAST-TAXES.                                                IH342     
044350     MOVE ZEROS TO ST-CURR-EARNED.                                IH342     
044360     MOVE +000000.00 TO YRLY-GROSS.                               IH342     
044370     MOVE ZEROS TO YRLY-ADJ-GROSS.                                IH342     
044380     MOVE +000000.00 TO YRLY-FED-TAXES.                           IH342     
044390     MOVE +00000.00 TO HALF-FED.                                  IH342     
044400     MOVE +000000.00 TO YRLY-NET.                                 IH342     
044410     MOVE +0000.00 TO ALLOWANCE.                                  IH342     
044420     MOVE ZEROS TO TAXABLE.                                       IH342     
044430     MOVE ZEROS TO STATAX-1.                                      IH342     
044440     MOVE ZEROS TO STATAX-2.                                      IH342     
044450     MOVE ZEROS TO STATAX-3.                                      IH342     
044460     MOVE ZEROS TO STATAX-4.                                      IH342     
044470     MOVE ZEROS TO STATAX-5.                                      IH342     
044480     MOVE ZEROS TO STATAX-6.                                      IH342     
044490     MOVE ZEROS TO YEARLY-STATAX.                                 IH342     
044500     MOVE 0 TO NY-CTR.                                            IH342     
044510     GO TO 330010.                                                IH342     
044520 GEORGIA.                                                         IH342     
044530     MULTIPLY ST-CURR-EARNED BY .15 GIVING STATAX-1.              IH342     
044540     IF 80.00 IS LESS THAN STATAX-1 MOVE 80.00 TO STATAX-1.       IH342     
044550     IF 50.00 IS GREATER THAN STATAX-1 MOVE 50.00 TO STATAX-1.    IH342     
044560     SUBTRACT STATAX-1 FROM ST-CURR-EARNED GIVING TAXABLE.        IH342     
044570     IF DEP-ALLOW IS EQUAL TO ZERO GO TO GA-1                     IH342     
044580         ELSE SUBTRACT 1 FROM DEP-ALLOW.                          IH342     
044590     MULTIPLY DEP-ALLOW BY 27.00 GIVING STATAX-2.                 IH342     
044600     IF MARRIED-MSTR ADD 115.00 TO STATAX-2                       IH342     
044610         ELSE ADD 57.50 TO STATAX-2.                              IH342     
044620     SUBTRACT STATAX-2 FROM TAXABLE.                              IH342     
044630 GA-1.                                                            IH342     
044640     IF TAXABLE IS NOT GREATER THAN ZERO                          IH342     
044650         MOVE ZEROS TO STATE-TAXES                                IH342     
044660         GO TO END-ECOAST-TAXES.                                  IH342     
044670     IF NOT MARRIED-MSTR GO TO GA-2.                              IH342     
044680     IF TAXABLE IS NOT GREATER THAN 40.00                         IH342     
044690         MULTIPLY .01 BY TAXABLE GIVING STATE-TAXES ROUNDED       IH342     
044700         GO TO END-ECOAST-TAXES.                                  IH342     
044710     IF TAXABLE IS NOT GREATER THAN 115.00                        IH342     
044720         SUBTRACT 40.00 FROM TAXABLE                              IH342     
044730         MULTIPLY .02 BY TAXABLE ROUNDED                          IH342     
044740         ADD .40    TAXABLE GIVING STATE-TAXES                    IH342     
044750         GO TO END-ECOAST-TAXES.                                  IH342     
044760     IF TAXABLE IS NOT GREATER THAN 192.00                        IH342     
044770         SUBTRACT 115.00 FROM TAXABLE                             IH342     
044780         MULTIPLY .04 BY TAXABLE ROUNDED                          IH342     
044790         ADD 2.00    TAXABLE GIVING STATE-TAXES                   IH342     
044800         GO TO END-ECOAST-TAXES.                                  IH342     
044810     IF TAXABLE IS NOT GREATER THAN 269.00                        IH342     
044820         SUBTRACT 192.00 FROM TAXABLE                             IH342     
044830         MULTIPLY .04 BY TAXABLE ROUNDED                          IH342     
044840         ADD 4.30    TAXABLE GIVING STATE-TAXES                   IH342     
044850         GO TO END-ECOAST-TAXES.                                  IH342     
044860     IF TAXABLE IS NOT GREATER THAN 385.00                        IH342     
044870         SUBTRACT 269.00 FROM TAXABLE                             IH342     
044880         MULTIPLY .05 BY TAXABLE ROUNDED                          IH342     
044890         ADD 7.35    TAXABLE GIVING STATE-TAXES                   IH342     
044900         GO TO END-ECOAST-TAXES.                                  IH342     
044910     SUBTRACT 385.00 FROM TAXABLE.                                IH342     
044920     MULTIPLY .06 BY TAXABLE ROUNDED.                             IH342     
044930     ADD 13.10    TAXABLE GIVING STATE-TAXES.                     IH342     
044940     GO TO END-ECOAST-TAXES.                                      IH342     
044950 GA-2.                                                            IH342     
044960     IF TAXABLE IS NOT GREATER THAN 29.00                         IH342     
044970         MULTIPLY .01 BY TAXABLE GIVING STATE-TAXES ROUNDED       IH342     
044980         GO TO END-ECOAST-TAXES.                                  IH342     
044990     IF TAXABLE IS NOT GREATER THAN 86.50                         IH342     
045000         SUBTRACT 29.00 FROM TAXABLE                              IH342     
045010         MULTIPLY .02 BY TAXABLE ROUNDED                          IH342     
045020         ADD .29    TAXABLE GIVING STATE-TAXES                    IH342     
045030         GO TO END-ECOAST-TAXES.                                  IH342     
045040     IF TAXABLE IS NOT GREATER THAN 144.25                        IH342     
045050         SUBTRACT 86.50  FROM TAXABLE                             IH342     
045060         MULTIPLY .03 BY TAXABLE ROUNDED                          IH342     
045070         ADD 1.44    TAXABLE GIVING STATE-TAXES                   IH342     
045080         GO TO END-ECOAST-TAXES.                                  IH342     
045090     IF TAXABLE IS NOT GREATER THAN 202.00                        IH342     
045100         SUBTRACT 144.25 FROM TAXABLE                             IH342     
045110         MULTIPLY .04 BY TAXABLE ROUNDED                          IH342     
045120         ADD 3.17    TAXABLE GIVING STATE-TAXES                   IH342     
045130         GO TO END-ECOAST-TAXES.                                  IH342     
045140     IF TAXABLE IS NOT GREATER THAN 269.25                        IH342     
045150         SUBTRACT 202.00 FROM TAXABLE                             IH342     
045160         MULTIPLY .05 BY TAXABLE ROUNDED                          IH342     
045170         ADD 5.48    TAXABLE GIVING STATE-TAXES                   IH342     
045180         GO TO END-ECOAST-TAXES.                                  IH342     
045190     SUBTRACT 269.25 FROM TAXABLE.                                IH342     
045200     MULTIPLY .06 BY TAXABLE ROUNDED.                             IH342     
045210     ADD 8.85     TAXABLE GIVING STATE-TAXES.                     IH342     
045220     GO TO END-ECOAST-TAXES.                                      IH342     
045230 VIRGINIA.                                                        IH342     
045240     COMPUTE YRLY-ADJ-GROSS ROUNDED =                             IH342     
045250         (ST-CURR-EARNED * 26) - (+650.00 +                       IH342     
045260         (+600.00 * MSTR-ST-TAX-EXEM-CD)).                        IH342     
045270     IF YRLY-ADJ-GROSS IS NOT GREATER THAN ZERO                   IH342     
045280         MOVE ZEROS TO STATE-TAXES                                IH342     
045290         GO TO END-ECOAST-TAXES.                                  IH342     
045300     IF YRLY-ADJ-GROSS IS GREATER THAN +12000.00                  IH342     
045310         SUBTRACT +12000.00 FROM YRLY-ADJ-GROSS GIVING STATAX-1   IH342     
045320         MULTIPLY STATAX-1 BY +.0575 GIVING STATAX-2 ROUNDED      IH342     
045330         ADD +470.00  STATAX-2 GIVING YEARLY-STATAX               IH342     
045340         GO TO VA-1.                                              IH342     
045350     IF YRLY-ADJ-GROSS IS GREATER THAN +5000.00                   IH342     
045360         SUBTRACT +5000.00  FROM YRLY-ADJ-GROSS GIVING STATAX-1   IH342     
045370         MULTIPLY STATAX-1 BY +.05   GIVING STATAX-2 ROUNDED      IH342     
045380         ADD +120.00  STATAX-2 GIVING YEARLY-STATAX               IH342     
045390         GO TO VA-1.                                              IH342     
045400     IF YRLY-ADJ-GROSS IS GREATER THAN +3000.00                   IH342     
045410         SUBTRACT +3000.00  FROM YRLY-ADJ-GROSS GIVING STATAX-1   IH342     
045420         MULTIPLY STATAX-1 BY +.03   GIVING STATAX-2 ROUNDED      IH342     
045430         ADD +60.00   STATAX-2 GIVING YEARLY-STATAX               IH342     
045440         GO TO VA-1.                                              IH342     
045450     MULTIPLY YRLY-ADJ-GROSS BY +.02                              IH342     
045460         GIVING YEARLY-STATAX ROUNDED.                            IH342     
045470 VA-1.                                                            IH342     
045480     DIVIDE +26 INTO YEARLY-STATAX GIVING STATE-TAXES ROUNDED.    IH342     
045490     GO TO END-ECOAST-TAXES.                                      IH342     
045500 MASSACHUSETTS.                                                   IH342     
045510     IF MSTR-ST-TAX-EXEM-CD EQUALS 00 GO TO MASS-1.               IH342     
045520     MULTIPLY MSTR-ST-TAX-EXEM-CD BY +23.00 GIVING ALLOWANCE.     IH342     
045530     ADD +56.00 TO ALLOWANCE.                                     IH342     
045540     IF ALLOWANCE GREATER ST-CURR-EARNED GO TO END-ECOAST-TAXES.  IH342     
045550     SUBTRACT ALLOWANCE FROM ST-CURR-EARNED.                      IH342     
045560 MASS-1.                                                          IH342     
045570     DIVIDE +2 INTO FED-TAXES GIVING HALF-FED.                    IH342     
045580     SUBTRACT HALF-FED FROM ST-CURR-EARNED.                       IH342     
045590     SUBTRACT RETIRE-FICA FROM ST-CURR-EARNED GIVING TAXABLE.     IH342     
045600     MULTIPLY TAXABLE BY +.04 GIVING STATE-TAXES ROUNDED.         IH342     
045610     GO TO END-ECOAST-TAXES.                                      IH342     
045620 MARYLAND.                                                        IH342     
045630     MULTIPLY MSTR-ST-TAX-EXEM-CD BY +31.00 GIVING ALLOWANCE.     IH342     
045640     MULTIPLY ST-CURR-EARNED BY +.10 GIVING YRLY-ADJ-GROSS.       IH342     
045650     IF +20.00 IS LESS THAN YRLY-ADJ-GROSS MOVE +20.00 TO         IH342     
045660     YRLY-ADJ-GROSS.                                              IH342     
045670     SUBTRACT YRLY-ADJ-GROSS FROM ST-CURR-EARNED GIVING           IH342     
045680     YRLY-GROSS.                                                  IH342     
045690     IF ALLOWANCE GREATER YRLY-GROSS GO TO END-ECOAST-TAXES.      IH342     
045700     SUBTRACT ALLOWANCE FROM YRLY-GROSS GIVING TAXABLE.           IH342     
045710     IF TAXABLE IS NOT GREATER THAN +38.00 GO TO MD-1.            IH342     
045720     IF TAXABLE IS NOT GREATER THAN +76.00 GO TO MD-2.            IH342     
045730     IF TAXABLE IS NOT GREATER THAN +115.00 GO TO MD-3.           IH342     
045740     SUBTRACT 115.00 FROM TAXABLE.                                IH342     
045750     MULTIPLY TAXABLE BY +.06 GIVING STATAX-1 ROUNDED.            IH342     
045760     ADD +4.15, STATAX-1 GIVING STATE-TAXES.                      IH342     
045770     GO TO END-ECOAST-TAXES.                                      IH342     
045780 MD-1.                                                            IH342     
045790     MULTIPLY TAXABLE BY +.024 GIVING STATE-TAXES ROUNDED.        IH342     
045800     GO TO END-ECOAST-TAXES.                                      IH342     
045810 MD-2.                                                            IH342     
045820     SUBTRACT +38.00 FROM TAXABLE.                                IH342     
045830     MULTIPLY TAXABLE BY +.036 GIVING STATAX-1 ROUNDED.           IH342     
045840     ADD +.92, STATAX-1 GIVING STATE-TAXES.                       IH342     
045850     GO TO END-ECOAST-TAXES.                                      IH342     
045860 MD-3.                                                            IH342     
045870     SUBTRACT +76.00 FROM TAXABLE.                                IH342     
045880     MULTIPLY TAXABLE BY +.048 GIVING STATAX-1 ROUNDED.           IH342     
045890     ADD +2.31, STATAX-1 GIVING STATE-TAXES.                      IH342     
045900     GO TO END-ECOAST-TAXES.                                      IH342     
045910 OHIO.                                                            IH342     
045920     IF DEP-ALLOW IS GREATER THAN 06                              IH342     
045930         MOVE 06 TO DEP-ALLOW.                                    IH342     
045940     COMPUTE YRLY-ADJ-GROSS ROUNDED =                             IH342     
045950         (ST-CURR-EARNED * 26) - (+500.00 * DEP-ALLOW)            IH342     
045960     IF YRLY-ADJ-GROSS GREATER THAN +15000.00                     IH342     
045970         GO TO OHIO-2.                                            IH342     
045980     IF YRLY-ADJ-GROSS GREATER THAN +10000.00                     IH342     
045990         COMPUTE STATE-TAXES ROUNDED =                            IH342     
046000             ((YRLY-ADJ-GROSS - +10000.00) * .02 + +75.00)        IH342     
046010                 / +26.00                                         IH342     
046020         GO TO OHIO-1.                                            IH342     
046030     IF YRLY-ADJ-GROSS GREATER THAN +5000.00                      IH342     
046040         COMPUTE STATE-TAXES ROUNDED =                            IH342     
046050             ((YRLY-ADJ-GROSS - +5000.00) * .01 + +25.00) / +26.00IH342     
046060         GO TO OHIO-1.                                            IH342     
046070     COMPUTE STATE-TAXES ROUNDED =                                IH342     
046080         (YRLY-ADJ-GROSS * .005) / +26.00.                        IH342     
046090 OHIO-1.                                                          IH342     
046100     MOVE +00000.00 TO ST-CURR-EARNED.                            IH342     
046110     MOVE +00000.00 TO YRLY-ADJ-GROSS.                            IH342     
046120     GO TO 330010.                                                IH342     
046130 OHIO-2.                                                          IH342     
046140     IF YRLY-ADJ-GROSS GREATER THAN +40000.00                     IH342     
046150         COMPUTE STATE-TAXES ROUNDED =                            IH342     
046160             ((YRLY-ADJ-GROSS - +40000.00) * .035 + +900.00)      IH342     
046170                 / +26.00                                         IH342     
046180         GO TO OHIO-1.                                            IH342     
046190     IF YRLY-ADJ-GROSS GREATER THAN +20000.00                     IH342     
046200         COMPUTE STATE-TAXES ROUNDED =                            IH342     
046210             ((YRLY-ADJ-GROSS - +20000.00) * .03 + +300.00)       IH342     
046220                 / +26.00                                         IH342     
046230         GO TO OHIO-1.                                            IH342     
046240     COMPUTE STATE-TAXES ROUNDED =                                IH342     
046250         ((YRLY-ADJ-GROSS - +15000.00) * .025 + +175.00) / +26.00 IH342     
046260         GO TO OHIO-1.                                            IH342     
046270 NEW-YORK.                                                        IH342     
046280     MOVE 1 TO NY-CTR.                                            IH342     
046290     IF ST-CURR-EARNED IS LESS THAN 275.00 SUBTRACT 13.50         IH342     
046300          FROM ST-CURR-EARNED GO TO NY-A.                         IH342     
046310     IF ST-CURR-EARNED IS GREATER THAN 550.00 SUBTRACT 52.00      IH342     
046320          FROM ST-CURR-EARNED GO TO NY-A.                         IH342     
046330     MULTIPLY ST-CURR-EARNED BY .14 GIVING ALLOWANCE.             IH342     
046340     SUBTRACT 25.00 FROM ALLOWANCE.                               IH342     
046350     SUBTRACT ALLOWANCE FROM ST-CURR-EARNED.                      IH342     
046360 NY-A.                                                            IH342     
046370     MULTIPLY MSTR-ST-TAX-EXEM-CD BY 25.00 GIVING ALLOWANCE.      IH342     
046380     IF ALLOWANCE > ST-CURR-EARNED GO TO NY-ZERO.                 IH342     
046390     SUBTRACT ALLOWANCE FROM ST-CURR-EARNED GIVING STATAX-1.      IH342     
046400     IF STATAX-1 < NY-WAGE (NY-CTR) GO TO NY-ZERO.                IH342     
046410 NY-1.                                                            IH342     
046420     ADD 1 TO NY-CTR.                                             IH342     
046430     IF STATAX-1 < NY-WAGE (NY-CTR) GO TO NY-2.                   IH342     
046440     IF NY-CTR > 13 GO TO NY-3.                                   IH342     
046450     GO TO NY-1.                                                  IH342     
046460 NY-2.                                                            IH342     
046470     SUBTRACT 1 FROM NY-CTR.                                      IH342     
046480 NY-3.                                                            IH342     
046490     MOVE NY-WAGE (NY-CTR) TO STATAX-2.                           IH342     
046500     MOVE NY-ST-TAX (NY-CTR) TO STATAX-3.                         IH342     
046510     MOVE NY-PCAMT (NY-CTR) TO TAX-PC.                            IH342     
046520     COMPUTE STATE-TAXES ROUNDED =                                IH342     
046530       ((STATAX-1 - STATAX-2) * TAX-PC) + STATAX-3.               IH342     
046540     GO TO END-ECOAST-TAXES.                                      IH342     
046550 NY-ZERO.                                                         IH342     
046560     MOVE 000.00 TO STATE-TAXES.                                  IH342     
046570     GO TO END-ECOAST-TAXES.                                      IH342     
046580 SOUTH-CAROLINA.                                                  IH342     
046590     MULTIPLY MSTR-ST-TAX-EXEM-CD BY +3.84 GIVING ALLOWANCE.      IH342     
046600     MULTIPLY ST-CURR-EARNED BY +.10 GIVING YRLY-ADJ-GROSS.       IH342     
046610     IF MSTR-STATE-TAX-CODE = "K" GO TO SC-JOINT.                 IH342     
046620     IF YRLY-ADJ-GROSS > +19.20                                   IH342     
046630     MOVE +19.20 TO YRLY-ADJ-GROSS.                               IH342     
046640     GO TO SC-COMMON.                                             IH342     
046650 SC-JOINT.                                                        IH342     
046660     IF YRLY-ADJ-GROSS > +38.40                                   IH342     
046670       MOVE +38.40 TO YRLY-ADJ-GROSS.                             IH342     
046680 SC-COMMON.                                                       IH342     
046690     COMPUTE TAXABLE = ST-CURR-EARNED -     ALLOWANCE             IH342     
046700       -     YRLY-ADJ-GROSS.                                      IH342     
046710     IF TAXABLE IS GREATER THAN +76.92 GO TO SC-1.                IH342     
046720     MULTIPLY TAXABLE BY +.02 GIVING STATE-TAXES ROUNDED.         IH342     
046730     GO TO END-ECOAST-TAXES.                                      IH342     
046740 SC-1.                                                            IH342     
046750     COMPUTE STATE-TAXES ROUNDED = (TAXABLE * +.000065 + +.0158)  IH342     
046760       * TAXABLE.                                                 IH342     
046770     GO TO END-ECOAST-TAXES.                                      IH342     
046780 NORTH-CAROLINA.                                                  IH342     
046790     IF ST-CURR-EARNED > +620.00 GO TO NC-LUMP.                   IH342     
046800     COMPUTE ALLOWANCE = DEP-ALLOW * +3.84.                       IH342     
046810     MULTIPLY ST-CURR-EARNED BY +.10 GIVING YRLY-ADJ-GROSS.       IH342     
046820     IF YRLY-ADJ-GROSS > +19.20                                   IH342     
046830       MOVE +19.20 TO YRLY-ADJ-GROSS.                             IH342     
046840     COMPUTE TAXABLE = ST-CURR-EARNED -                           IH342     
046850       (ALLOWANCE + YRLY-ADJ-GROSS).                              IH342     
046860     COMPUTE STATE-TAXES ROUNDED =                                IH342     
046870       ((TAXABLE * +.00005194) + +.028117) * TAXABLE.             IH342     
046880     GO TO END-ECOAST-TAXES.                                      IH342     
046890 NC-LUMP.                                                         IH342     
046900     SUBTRACT +619.00 FROM ST-CURR-EARNED.                        IH342     
046910     MULTIPLY ST-CURR-EARNED BY +.07 GIVING STATE-TAXES ROUNDED.  IH342     
046920     MOVE 01 TO TAXER.                                            IH342     
046930 NC-SEARCH.                                                       IH342     
046940     IF MSTR-ST-TAX-EXEM-CD = EX-EM (TAXER) GO TO NC-ADD.         IH342     
046950     IF TAXER LESS THAN 34 ADD 01 TO TAXER GO TO NC-SEARCH.       IH342     
046960     ADD +12.00 TO STATE-TAXES.                                   IH342     
046970     GO TO END-ECOAST-TAXES.                                      IH342     
046980 NC-ADD.                                                          IH342     
046990     ADD EX-AMT (TAXER) TO STATE-TAXES.                           IH342     
047000     GO TO END-ECOAST-TAXES.                                      IH342     
047010 328200.                                                          IH342     
047020     IF WRITE-CASH-AWARD OR CD10-TAX-ADJ                          IH342     
047030         MULTIPLY CURR-EARNED BY .03                              IH342     
047040             GIVING STATE-TAXES ROUNDED                           IH342     
047050         GO TO 330030.                                            IH342     
047060     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
047070     IF MARRIED-MSTR GO TO 328210.                                IH342     
047080     MOVE -00038.46 TO HRLY-RATE-WORK.                            IH342     
047090     GO TO 328220.                                                IH342     
047100 328210.                                                          IH342     
047110     IF MSTR-TAX-CODE GREATER THAN 1                              IH342     
047120         MOVE -00076.92 TO HRLY-RATE-WORK                         IH342     
047130         ELSE      MOVE -00038.46 TO HRLY-RATE-WORK.              IH342     
047140 328220.                                                          IH342     
047150     ADD CURR-EARNED TO HRLY-RATE-WORK.                           IH342     
047160     IF HRLY-RATE-WORK POSITIVE GO TO 328230.                     IH342     
047170 328225.                                                          IH342     
047180     MOVE +00000.00 TO HRLY-RATE-WORK.                            IH342     
047190     GO TO 328290.                                                IH342     
047200 328230.                                                          IH342     
047210     IF HRLY-RATE-WORK NOT GREATER THAN +00001.00                 IH342     
047220         GO TO 328225.                                            IH342     
047230     IF MARRIED-MSTR                                              IH342     
047240         MOVE 10 TO SUBS-1                                        IH342     
047250         MOVE 11 TO SUBS-2                                        IH342     
047260         MOVE 11 TO SUBS-3                                        IH342     
047270         GO TO 328240.                                            IH342     
047280     MOVE 01 TO SUBS-1, SUBS-2, SUBS-3.                           IH342     
047290 328240.                                                          IH342     
047300     IF HRLY-RATE-WORK NOT GREATER THAN CALIF-WAGES (SUBS-1)      IH342     
047310         SUBTRACT +01.00 FROM HRLY-RATE-WORK.                     IH342     
047320 328250.                                                          IH342     
047330     SUBTRACT CALIF-WAGES (SUBS-1) FROM HRLY-RATE-WORK.           IH342     
047340     IF HRLY-RATE-WORK POSITIVE                                   IH342     
047350         GO TO 328270.                                            IH342     
047360     ADD CALIF-WAGES (SUBS-1) TO HRLY-RATE-WORK.                  IH342     
047370 328260.                                                          IH342     
047380     MULTIPLY CALIF-EXCESS-PERCENT (SUBS-3) BY HRLY-RATE-WORK     IH342     
047390         ROUNDED.                                                 IH342     
047400     ADD CALIF-TAX (SUBS-2) TO HRLY-RATE-WORK.                    IH342     
047410     GO TO 328280.                                                IH342     
047420 328270.                                                          IH342     
047430     ADD 1 TO SUBS-1, SUBS-2, SUBS-3.                             IH342     
047440     IF SUBS-3 EQUAL TO 10  OR 20                                 IH342     
047450         GO TO 328260.                                            IH342     
047460     GO TO 328250.                                                IH342     
047470 328280.                                                          IH342     
047480     IF MSTR-TAX-CODE ZERO GO TO 328290.                          IH342     
047490     IF MSTR-TAX-CODE GREATER THAN 10                             IH342     
047500         MOVE 10 TO SUBS-4                                        IH342     
047510         ELSE      MOVE MSTR-TAX-CODE TO SUBS-4.                  IH342     
047520     IF MARRIED-MSTR ADD 10 TO SUBS-4.                            IH342     
047530     SUBTRACT CALIF-CREDIT (SUBS-4) FROM HRLY-RATE-WORK.          IH342     
047540     IF HRLY-RATE-WORK NEGATIVE GO TO 328225.                     IH342     
047550 328290.                                                          IH342     
047560     MOVE HRLY-RATE-WORK TO STATE-TAXES.                          IH342     
047570     GO TO 330010.                                                IH342     
047580 330010.                                                          IH342     
047590     IF STATE-TAXES NEGATIVE MOVE ZEROS TO STATE-TAXES.           IH342     
047600 330020.                                                          IH342     
047610     IF MSTR-ADDL-ST-TAX EQUALS ZEROS OR SPACES GO TO 330030.     IH342     
047620     ADD MSTR-ADDL-ST-TAX TO STATE-TAXES.                         IH342     
047630 330030.                                                          IH342     
047640     IF STATE-ADJUSTMENT                                          IH342     
047650         GO TO 330140.                                            IH342     
047660 330040.                                                          IH342     
047670     SUBTRACT STATE-TAXES FROM NET-PAY,                           IH342     
047680         GIVING HRLY-RATE-WORK.                                   IH342     
047690     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
047700         ADD HRLY-RATE-WORK TO STATE-TAXES,                       IH342     
047710         ALTER 198140 TO PROCEED TO 330040,                       IH342     
047720             GO TO 268100.                                        IH342     
047730         MOVE HRLY-RATE-WORK TO NET-PAY.                          IH342     
047740         ADD STATE-TAXES TO MSTR-ST-QTRLY-TAX.                    IH342     
047750         ADD STATE-TAXES TO MSTR-YTD-STATE-TAX.                   IH342     
047760             GO TO 332120.                                        IH342     
047770 330140.                                                          IH342     
047780     ADD ADJ-STATE TO STATE-TAXES.                                IH342     
047790     IF STATE-TAXES ZERO,                                         IH342     
047800         GO TO 332120.                                            IH342     
047810     IF STATE-TAXES POSITIVE,                                     IH342     
047820         GO TO 330040.                                            IH342     
047830     ALTER 198140 TO PROCEED TO 330040.                           IH342     
047840     MOVE STATE-TAXES TO MSG7-AMT.                                IH342     
047850         GO TO 284210.                                            IH342     
047860     NOTE ******************************************************* IH342     
047870          *                                                     * IH342     
047880          *  UNION DUES DEDUCTION ROUTINE                       * IH342     
047890          *                                                     * IH342     
047900          *******************************************************.IH342     
047910 332120.                                                          IH342     
047920     IF WRITE-CASH-AWARD,                                         IH342     
047930         GO TO 338170.                                            IH342     
047940     IF WK1-SUBJECT ZERO,                                         IH342     
047950         GO TO 336112.                                            IH342     
047960 332170.                                                          IH342     
047970     IF MSTR-UNION EQUAL TO SPACE GO TO 336190.                   IH342     
047980     SET AKEY TO 1.                                               IH342     
047990     SEARCH UNION-CON VARYING AKEY  AT END GO TO 334100           IH342     
048000         WHEN MSTR-UNION EQUAL TO UNION-CODE (AKEY)               IH342     
048010         MOVE UNION-DUE (AKEY) TO UNION-DUES.                     IH342     
048020 334100.                                                          IH342     
048030     IF PAYOFF-LWOP  MOVE +000.00 TO UNION-DUES.                  IH342     
048040     IF UNION-ADJUSTMENT,                                         IH342     
048050         GO TO 336030.                                            IH342     
048060 334130.                                                          IH342     
048070     IF UNION-DUES GREATER THAN NET-PAY                           IH342     
048080         MOVE ZEROS TO UNION-DUES GO TO 336190.                   IH342     
048090     SUBTRACT UNION-DUES FROM NET-PAY,                            IH342     
048100         GIVING HRLY-RATE-WORK.                                   IH342     
048110     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
048120         ADD HRLY-RATE-WORK TO UNION-DUES,                        IH342     
048130         ALTER 198140 TO PROCEED TO 334130,                       IH342     
048140             GO TO 268100.                                        IH342     
048150         MOVE HRLY-RATE-WORK TO NET-PAY.                          IH342     
048160         ADD UNION-DUES TO MSTR-YTD-UNION.                        IH342     
048170             GO TO 336190.                                        IH342     
048180 336030.                                                          IH342     
048190     ADD ADJ-UNION TO UNION-DUES.                                 IH342     
048200     IF UNION-DUES ZERO,                                          IH342     
048210         GO TO 336190.                                            IH342     
048220     IF UNION-DUES POSITIVE,                                      IH342     
048230         GO TO 334130.                                            IH342     
048240     ALTER 198140 TO PROCEED TO 334130.                           IH342     
048250     MOVE UNION-DUES TO MSG7-AMT.                                 IH342     
048260         GO TO 284210.                                            IH342     
048270 336112.                                                          IH342     
048280     IF NOT FICA-MSTR,                                            IH342     
048290         GO TO 336190.                                            IH342     
048300     GO TO 336190.                                                IH342     
048310     NOTE ******************************************************* IH342     
048320          *                                                     * IH342     
048330          *  CHARITABLE CONTRIBUTION DEDUCTION ROUTINE          * IH342     
048340          *                                                     * IH342     
048350          *******************************************************.IH342     
048360 336190.                                                          IH342     
048370     IF DECEASED-PAYOFF,                                          IH342     
048380         GO TO 338170.                                            IH342     
048390     MOVE MSTR-CHARITABLE TO CHARITY-CON.                         IH342     
048400     IF PAYOFF-LWOP  MOVE +000.00 TO CHARITY-CON.                 IH342     
048410     IF CHARITY-ADJ,                                              IH342     
048420         GO TO 337000.                                            IH342     
048430     IF CHARITY-CON ZERO GO TO 338120.                            IH342     
048440 336195.                                                          IH342     
048450     SUBTRACT CHARITY-CON FROM NET-PAY                            IH342     
048460         GIVING HRLY-RATE-WORK.                                   IH342     
048470     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
048480         MOVE ZEROS TO CHARITY-CON                                IH342     
048490         GO TO 338120.                                            IH342     
048500     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
048510     ADD CHARITY-CON TO MSTR-YTD-CHARITY.                         IH342     
048520     GO TO 338120.                                                IH342     
048530 337000.                                                          IH342     
048540     ADD ADJ-CHARITY TO CHARITY-CON.                              IH342     
048550     IF CHARITY-CON ZERO GO TO 338120.                            IH342     
048560     IF CHARITY-CON POSITIVE GO TO 336195.                        IH342     
048570     ALTER 198140 TO PROCEED TO 336195.                           IH342     
048580     MOVE CHARITY-CON TO MSG7-AMT.                                IH342     
048590     GO TO 284210.                                                IH342     
048600     NOTE ******************************************************* IH342     
048610          *                                                     * IH342     
048620          *  SAVINGS ALLOTMENT 1 AND 2 DEDUCTION ROUTINE        * IH342     
048630          *                                                     * IH342     
048640          *******************************************************.IH342     
048650 338120.                                                          IH342     
048660     MOVE MSTR-ALLOT1-DED TO ALLOT1-WORK.                         IH342     
048670     MOVE MSTR-ALLOT2-DED TO ALLOT2-WORK.                         IH342     
048680     IF PAYOFF-LWOP MOVE +000 TO ALLOT1-WORK ALLOT2-WORK.         IH342     
048690     IF ALLOT1-ADJ,                                               IH342     
048700     GO TO 338150.                                                IH342     
048710     IF ALLOT2-ADJ,                                               IH342     
048720        GO TO 338150.                                             IH342     
048730     IF ALLOT1-WORK ZERO AND ALLOT2-WORK ZERO GO TO 338170.       IH342     
048740     ADD ALLOT1-WORK ALLOT2-WORK GIVING ALLOT-DED.                IH342     
048750 338125.                                                          IH342     
048760     SUBTRACT ALLOT-DED FROM NET-PAY,                             IH342     
048770         GIVING HRLY-RATE-WORK.                                   IH342     
048780     IF HRLY-RATE-WORK NEGATIVE,                                  IH342     
048790         MOVE +00000 TO ALLOT-DED,                                IH342     
048800         GO TO 338170.                                            IH342     
048810     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
048820     ADD ALLOT1-WORK TO MSTR-YTD-ALLOT1-DED.                      IH342     
048830     ADD ALLOT2-WORK TO MSTR-YTD-ALLOT2-DED.                      IH342     
048840     GO TO 338170.                                                IH342     
048850 338150.                                                          IH342     
048860     IF ALLOT1-ADJ ADD ADJ-TO-ALLOT1 TO ALLOT1-WORK.              IH342     
048870     IF ALLOT2-ADJ ADD ADJ-TO-ALLOT2 TO ALLOT2-WORK.              IH342     
048880     ADD ALLOT1-WORK ALLOT2-WORK GIVING ALLOT-DED.                IH342     
048890     IF ALLOT-DED ZERO GO TO 338170.                              IH342     
048900     IF ALLOT-DED POSITIVE GO TO 338125.                          IH342     
048910     ALTER 198140 TO PROCEED TO 338125.                           IH342     
048920     MOVE ALLOT-DED TO MSG7-AMT.                                  IH342     
048930     GO TO 284210.                                                IH342     
048940     NOTE ******************************************************* IH342     
048950          *                                                     * IH342     
048960          *  PREPARATION OF PAYROLL SMOOTH ROLL                 * IH342     
048970          *                                                     * IH342     
048980          *******************************************************.IH342     
048990 338170.                                                          IH342     
049000     IF SR-LINE-COUNT NOT ZERO,                                   IH342     
049010         GO TO 346060.                                            IH342     
049020 SRNOTE1.                                                         IH342     
049030     NOTE ******************************************************* IH342     
049040          *                                                     * IH342     
049050          *  PREPARE SMOOTH ROLL HEADING LINES                  * IH342     
049060          *                                                     * IH342     
049070          *******************************************************.IH342     
049080 340170.                                                          IH342     
049090     MOVE ALL SPACES TO SMOOTH-ROLL-REC.                          IH342     
049100     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING CHANNEL 1.            IH342     
049110 342010.                                                          IH342     
049120     MOVE DEPT-NAME TO SR-DEPT.                                   IH342     
049130     MOVE WB-MONTH TO SR-FROM-MO.                                 IH342     
049140     MOVE WB-DAY TO SR-FROM-DAY.                                  IH342     
049150     MOVE WE-MONTH TO SR-TO-MO.                                   IH342     
049160     MOVE WE-DAY TO SR-TO-DAY.                                    IH342     
049170     MOVE "-" TO SR-DASH1, SR-DASH2.                              IH342     
049180     MOVE DISBURSING-CODE TO SR-DO-SYMBOL.                        IH342     
049190     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 2 LINES.              IH342     
049200     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
049210     ADD 002 TO SR-LINE-COUNT.                                    IH342     
049220 342110.                                                          IH342     
049230     MOVE ACTY-NAME-LOC TO SR-LOCATION.                           IH342     
049240     MOVE "CPR" TO SR-CPR-KON.                                    IH342     
049250     MOVE ROLL-NO TO SR-ROLL-NO.                                  IH342     
049260     MOVE "YEAR" TO SR-YEAR-KON.                                  IH342     
049270     MOVE WE-YEAR TO SR-PERIOD-YEAR.                              IH342     
049280     MOVE MINOR-ACTIVITY TO SR-ACTY-CODE.                         IH342     
049290     MOVE MINOR-CONTROL TO SR-CNTL-GRP.                           IH342     
049300     MOVE 01 TO SUBS-4.                                           IH342     
049310 342200.                                                          IH342     
049320     IF MINOR-ACTIVITY EQUAL TO SR-ACT-CODE (SUBS-4),             IH342     
049330         MOVE SR-LOCATION-HDR (SUBS-4) TO SR-ACTIVITY,            IH342     
049340             GO TO 342250.                                        IH342     
049350     IF SUBS-4 GREATER THAN 25 GO TO 342250.                      IH342     
049360     ADD 01 TO SUBS-4, GO TO 342200.                              IH342     
049370 342250.                                                          IH342     
049380     MOVE 01 TO SUBS-4.                                           IH342     
049390     IF ANNUM-MSTR MOVE "GRADED" TO SR-UG-LITERAL                 IH342     
049400         ELSE MOVE "UNGRADED" TO SR-UG-LITERAL.                   IH342     
049410     MOVE "PAGE NO. " TO SR-PAGE-KON.                             IH342     
049420     ADD 00001 TO SR-PAGE-COUNT.                                  IH342     
049430     MOVE SR-PAGE-COUNT TO SR-PAGE-NO.                            IH342     
049440     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 3 LINES.              IH342     
049450     ADD 03 TO SR-LINE-COUNT.                                     IH342     
049460     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
049470 344140.                                                          IH342     
049480     MOVE "ADD'L" TO SR-ADDL-KON.                                 IH342     
049490     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 1 LINES.              IH342     
049500     ADD 01 TO SR-LINE-COUNT.                                     IH342     
049510 344150.                                                          IH342     
049520     MOVE "F TAX" TO SR-ADDL-KON.                                 IH342     
049530     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 3 LINES.              IH342     
049540     MOVE "ADD'L" TO SR-ADDL-KON.                                 IH342     
049550     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 1 LINES.              IH342     
049560     MOVE "S TAX" TO SR-ADDL-KON.                                 IH342     
049570     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 2 LINES.              IH342     
049580     ADD 06 TO SR-LINE-COUNT.                                     IH342     
049590     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
049600 34415E.                                                          IH342     
049610     EXIT.                                                        IH342     
049620 344160.                                                          IH342     
049630         GO TO 346060.                                            IH342     
049640     NOTE ******************************************************* IH342     
049650          *                                                     * IH342     
049660          *  PREPARE SMOOTH ROLL DETAIL PRINT LINE 1            * IH342     
049670          *      PRINT EARNINGS AND VARIABLE DEDUCTIONS         * IH342     
049680          *                                                     * IH342     
049690          *******************************************************.IH342     
049700 346060.                                                          IH342     
049710     ADD +1 TO PT-EMP-CTR.                                        IH342     
049720     ADD +1 TO CG-EMP-CTR.                                        IH342     
049730     ADD +1 TO AT-EMP-CTR.                                        IH342     
049740     ADD +1 TO PGT-EMP-CTR.                                       IH342     
049750     MOVE MSTR-NAME TO SR-NAME.                                   IH342     
049760     MOVE MSTR-BANK-DEP-CODE TO SR-BANK-CODE.                     IH342     
049770     MOVE MSTR-CLOCK-NO TO SR-CLOCK-NO.                           IH342     
049780     IF NOT WRITE-CASH-AWARD,                                     IH342     
049790         ADD MSTR-HOURLY-RATE TO PT-RATE,                         IH342     
049800         ADD MSTR-HOURLY-RATE TO CG-RATE.                         IH342     
049810         ADD MSTR-HOURLY-RATE TO AT-RATE.                         IH342     
049820         ADD MSTR-HOURLY-RATE TO PGT-RATE.                        IH342     
049830     MOVE MSTR-HOURLY-RATE TO SR-HOURLY-RATE.                     IH342     
049840     IF WRITE-TAX-LEVY,                                           IH342     
049850         WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 1 LINES,          IH342     
049860           MOVE SPACES TO SMOOTH-ROLL-REC,                        IH342     
049870             ADD 001 TO SR-LINE-COUNT,                            IH342     
049880                 GO TO 356140.                                    IH342     
049890     IF ADJ-REG-HRS NOT ZERO,                                     IH342     
049900         ADD ADJ-REG-HRS TO REG-HRS-WK1,                          IH342     
049910         MOVE +000.00 TO ADJ-REG-HRS.                             IH342     
049920     MOVE REG-HRS-WK1 TO SR-REG-HOURS.                            IH342     
049930     ADD REG-HRS-WK1 TO PT-REG-HRS.                               IH342     
049940     ADD REG-HRS-WK1 TO CG-REG-HRS.                               IH342     
049950     ADD REG-HRS-WK1 TO AT-REG-HRS.                               IH342     
049960     ADD REG-HRS-WK1 TO PGT-REG-HRS.                              IH342     
049970     ADD REG-HRS-WK1 TO BASIC-HOURS.                              IH342     
049980     IF REG-MONEY NOT ZERO,                                       IH342     
049990         ADD REG-MONEY TO PT-REG-PAY,                             IH342     
050000         ADD REG-MONEY TO CG-REG-PAY,                             IH342     
050010         ADD REG-MONEY TO AT-REG-PAY,                             IH342     
050020         ADD REG-MONEY TO PGT-REG-PAY,                            IH342     
050030         MOVE REG-MONEY TO SR-REG-PAY.                            IH342     
050040     IF ADJ-OT-HOURS NOT ZERO,                                    IH342     
050050         ADD ADJ-OT-HOURS TO OT-HRS-WK1,                          IH342     
050060         MOVE +000.00 TO ADJ-OT-HOURS.                            IH342     
050070     IF DIEM-MSTR,                                                IH342     
050080         GO TO 352072.                                            IH342     
050090     IF GS-GRADED-MSTR,                                           IH342     
050100         GO TO 352090.                                            IH342     
050110 352072.                                                          IH342     
050120     ADD OT-HRS-WK2 TO OT-HRS-WK1.                                IH342     
050130 352090.                                                          IH342     
050140     ADD OT-HRS-WK1 TO PT-OT-HRS.                                 IH342     
050150     ADD OT-HRS-WK1 TO CG-OT-HRS.                                 IH342     
050160     ADD OT-HRS-WK1 TO AT-OT-HRS.                                 IH342     
050170     ADD OT-HRS-WK1 TO PGT-OT-HRS.                                IH342     
050180     ADD OT-HRS-WK1 TO OVERTIME-HOURS.                            IH342     
050190     MOVE OT-HRS-WK1 TO SR-OT-HOURS.                              IH342     
050200     IF OT-MONEY NOT ZERO,                                        IH342     
050210         ADD OT-MONEY TO PT-OT-PAY,                               IH342     
050220         ADD OT-MONEY TO CG-OT-PAY,                               IH342     
050230         ADD OT-MONEY TO AT-OT-PAY,                               IH342     
050240         ADD OT-MONEY TO PGT-OT-PAY,                              IH342     
050250         MOVE OT-MONEY TO SR-OT-MONEY.                            IH342     
050260     IF MSTR-FF-CODE EQUAL TO "1" OR "2" OR "6"                   IH342     
050270         MOVE MSTR-FF-CODE TO SR-EXC-CODE                         IH342     
050280         GO TO 352095.                                            IH342     
050290     MOVE MSTR-OTHER-PAY-CODE TO SR-EXC-CODE.                     IH342     
050300 352095.                                                          IH342     
050310     IF OTHER-HOURS NOT ZERO,                                     IH342     
050320         ADD OTHER-HOURS TO PT-OTH-HRS,                           IH342     
050330         ADD OTHER-HOURS TO CG-OTH-HRS,                           IH342     
050340         ADD OTHER-HOURS TO AT-OTH-HRS,                           IH342     
050350         ADD OTHER-HOURS TO PGT-OTH-HRS,                          IH342     
050360         ADD OTHER-HOURS TO BASIC-HOURS,                          IH342     
050370         MOVE OTHER-HOURS TO SR-OTHER-HOURS.                      IH342     
050380     IF OTHER-MONEY NOT ZERO,                                     IH342     
050390         ADD OTHER-MONEY TO PT-OTH-PAY,                           IH342     
050400         ADD OTHER-MONEY TO CG-OTH-PAY,                           IH342     
050410         ADD OTHER-MONEY TO AT-OTH-PAY,                           IH342     
050420         ADD OTHER-MONEY TO PGT-OTH-PAY,                          IH342     
050430         MOVE OTHER-MONEY TO SR-OTHER-MONEY.                      IH342     
050440     IF RETIRE-FICA ZERO,                                         IH342     
050450             GO TO 356030.                                        IH342     
050460         IF FICA-MSTR,                                            IH342     
050470             ADD RETIRE-FICA TO PT-FICA,                          IH342     
050480             ADD RETIRE-FICA TO CG-FICA,                          IH342     
050490             ADD RETIRE-FICA TO AT-FICA,                          IH342     
050500             ADD RETIRE-FICA TO PGT-FICA,                         IH342     
050510             MOVE RETIRE-FICA TO SR-FICA,                         IH342     
050520                 GO TO 356030.                                    IH342     
050530             ADD RETIRE-FICA TO PT-CSRA.                          IH342     
050540             ADD RETIRE-FICA TO CG-CSRA.                          IH342     
050550             ADD RETIRE-FICA TO AT-CSRA.                          IH342     
050560             ADD RETIRE-FICA TO PGT-CSRA.                         IH342     
050570             MOVE RETIRE-FICA TO SR-CSRA.                         IH342     
050580 356030.                                                          IH342     
050590     IF NOT WRITE-CASH-AWARD,                                     IH342     
050600         MOVE MSTR-MARRIED-CODE TO SR-FED-MAR-CODE,               IH342     
050610         MOVE MSTR-TAX-CODE TO SR-TAX-CODE.                       IH342     
050620     IF MSTR-ADDED-TAX NOT ZERO,                                  IH342     
050630         MOVE MSTR-ADDED-TAX TO SR-ADDED-TAX.                     IH342     
050640     IF FED-TAXES NOT ZERO,                                       IH342     
050650         ADD FED-TAXES TO PT-FED-TAX,                             IH342     
050660         ADD FED-TAXES TO CG-FED-TAX,                             IH342     
050670         ADD FED-TAXES TO AT-FED-TAX,                             IH342     
050680         ADD FED-TAXES TO PGT-FED-TAX,                            IH342     
050690         MOVE FED-TAXES TO SR-FED-TAX.                            IH342     
050700     IF STATE-TAXES NOT ZERO,                                     IH342     
050710         MOVE MSTR-ST-TAX-EXEM-CD TO SR-STATE-EXC                 IH342     
050720         MOVE MSTR-STATE-TAX-CODE TO SR-STATE-TAX-CODE            IH342     
050730         ADD STATE-TAXES TO PT-ST-TAX,                            IH342     
050740     ADD STATE-TAXES TO CG-ST-TAX,                                IH342     
050750         ADD STATE-TAXES TO AT-ST-TAX,                            IH342     
050760     ADD STATE-TAXES TO PGT-ST-TAX,                               IH342     
050770         MOVE STATE-TAXES TO SR-STATE-TAX.                        IH342     
050780     IF WRITE-CASH-AWARD MOVE "CASH AWARD" TO SR-FILLER.          IH342     
050790     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 1 LINES.              IH342     
050800     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
050810     ADD 001 TO SR-LINE-COUNT.                                    IH342     
050820     ADD 00001 TO SR-OUT.                                         IH342     
050830     NOTE ******************************************************* IH342     
050840          *                                                     * IH342     
050850          *  PREPARE SMOOTH ROLL DETAIL PRINT LINE 2            * IH342     
050860          *      PRINT FIXED DEDUCTIONS AND SAVINGS             * IH342     
050870          *                                                     * IH342     
050880          *******************************************************.IH342     
050890 356140.                                                          IH342     
050900     MOVE MSTR-BADGE TO SR-BADGE.                                 IH342     
050910     MOVE MSTR-ACTY TO SR-PFX-ACTY.                               IH342     
050920     MOVE MSTR-CONT-GRP TO SR-PFX-CTL-GP.                         IH342     
050930     IF ADJ-TO-NET-PAY, ADD NET-PAY-ADJ    TO NET-PAY.            IH342     
050940     IF NET-PAY ZERO GO TO 358100.                                IH342     
050950     IF TAX-LEVY,                                                 IH342     
050960         GO TO 364200.                                            IH342     
050970 356190.                                                          IH342     
050980     IF DECEASED-PAYOFF,                                          IH342     
050990         ADD NET-PAY TO OTHER-DED,                                IH342     
051000         MOVE NET-PAY TO HRLY-RATE-WORK,                          IH342     
051010         MOVE +00000.00 TO NET-PAY.                               IH342     
051020     IF NOT PAID-BY-VOUCHER GO TO 358100.                         IH342     
051030     SUBTRACT VOUCHER-PAID FROM NET-PAY GIVING HRLY-RATE-WORK.    IH342     
051040     IF HRLY-RATE-WORK NEGATIVE                                   IH342     
051050         MOVE HRLY-RATE-WORK TO MSG10-AMT                         IH342     
051060         MOVE MESSAGE10 TO ERR-EXPLAIN                            IH342     
051070         ADD NET-PAY TO OTHER-DED, MSTR-YTD-OTH-DED               IH342     
051080         MOVE +00000.00 TO NET-PAY                                IH342     
051090         ALTER 198140 TO PROCEED TO 358190                        IH342     
051100         GO TO 228200.                                            IH342     
051110     MOVE HRLY-RATE-WORK TO NET-PAY.                              IH342     
051120         ADD VOUCHER-PAID TO OTHER-DED, MSTR-YTD-OTH-DED.         IH342     
051130 358100.                                                          IH342     
051140     IF NET-PAY ZERO  MOVE "NO PAY" TO SR-DEPOSIT                 IH342     
051150         GO TO 358190.                                            IH342     
051160     IF WRITE-CASH-AWARD OR WRITE-TAX-LEVY GO TO 358110.          IH342     
051170     IF MSTR-BANK-DEP-CODE EQUAL TO ZEROS OR SPACES GO TO 358110. IH342     
051180     IF NOT COMPOSIT-CHECKS GO TO 358110.                         IH342     
051190     MOVE "DEPOSIT" TO SR-DEPOSIT.                                IH342     
051200     GO TO 358140.                                                IH342     
051210 358110.                                                          IH342     
051220     ADD 1 TO CHECK-COUNTER.                                      IH342     
051230     MOVE BEGIN-CHECK-NO TO SR-CHECK-NO.                          IH342     
051240 358140.                                                          IH342     
051250     ADD NET-PAY TO PT-NET-PAY.                                   IH342     
051260     ADD NET-PAY TO CG-NET-PAY.                                   IH342     
051270     ADD NET-PAY TO AT-NET-PAY.                                   IH342     
051280     ADD NET-PAY TO PGT-NET-PAY,                                  IH342     
051290     ADD NET-PAY TO MSTR-YTD-NET-PAY.                             IH342     
051300     MOVE NET-PAY TO SR-NET-PAY.                                  IH342     
051310 358190.                                                          IH342     
051320     IF WRITE-TAX-LEVY,                                           IH342     
051330         MOVE "TAX LEIN" TO SR2-FILLER                            IH342     
051340         GO TO 362162.                                            IH342     
051350     IF MSTR-HLTH-CODE NOT ZERO,                                  IH342     
051360         MOVE MSTR-HLTH-CODE TO SR-HLTH-CODE.                     IH342     
051370     IF HLTH-INS-DED NOT ZERO,                                    IH342     
051380         ADD HLTH-INS-DED TO PT-HEALTH,                           IH342     
051390         ADD HLTH-INS-DED TO CG-HEALTH,                           IH342     
051400         ADD HLTH-INS-DED TO AT-HEALTH,                           IH342     
051410         ADD HLTH-INS-DED TO PGT-HEALTH,                          IH342     
051420         MOVE HLTH-INS-DED TO SR-HLTH-DED.                        IH342     
051430     IF LIFE-INS-DED NOT ZERO,                                    IH342     
051440         ADD LIFE-INS-DED TO PT-REG-LIFE,                         IH342     
051450         ADD LIFE-INS-DED TO CG-REG-LIFE,                         IH342     
051460         ADD LIFE-INS-DED TO AT-REG-LIFE,                         IH342     
051470         ADD LIFE-INS-DED TO PGT-REG-LIFE,                        IH342     
051480         MOVE LIFE-INS-DED TO SR-LIFE-INS-DED.                    IH342     
051490     IF LIFE-OPT-DED ZERO,                                        IH342     
051500         GO TO 360250.                                            IH342     
051510     ADD LIFE-OPT-DED TO PT-OPT-INS.                              IH342     
051520     ADD LIFE-OPT-DED TO CG-OPT-INS.                              IH342     
051530     ADD LIFE-OPT-DED TO AT-OPT-INS.                              IH342     
051540     ADD LIFE-OPT-DED TO PGT-OPT-INS.                             IH342     
051550     MOVE LIFE-OPT-DED TO SR-LIFE-OPT-DED.                        IH342     
051560     IF MSTR-LIFE-OPT-CODE EQUALS "A",                            IH342     
051570         ADD LIFE-OPT-DED TO PT-AADD,                             IH342     
051580         ADD LIFE-OPT-DED TO CG-AADD,                             IH342     
051590         ADD LIFE-OPT-DED TO PGT-AADD,                            IH342     
051600         ADD LIFE-OPT-DED TO AT-AADD,                             IH342     
051610             GO TO 360250.                                        IH342     
051620     IF MSTR-LIFE-OPT-CODE EQUALS "B",                            IH342     
051630         ADD LIFE-OPT-DED TO PT-BADD,                             IH342     
051640         ADD LIFE-OPT-DED TO CG-BADD,                             IH342     
051650         ADD LIFE-OPT-DED TO AT-BADD,                             IH342     
051660         ADD LIFE-OPT-DED TO PGT-BADD,                            IH342     
051670             GO TO 360250.                                        IH342     
051680     IF MSTR-LIFE-OPT-CODE EQUALS "C",                            IH342     
051690         ADD LIFE-OPT-DED TO PT-CADD,                             IH342     
051700         ADD LIFE-OPT-DED TO CG-CADD,                             IH342     
051710         ADD LIFE-OPT-DED TO AT-CADD,                             IH342     
051720         ADD LIFE-OPT-DED TO PGT-CADD,                            IH342     
051730             GO TO 360250.                                        IH342     
051740     IF MSTR-LIFE-OPT-CODE EQUALS "D",                            IH342     
051750         ADD LIFE-OPT-DED TO PT-DADD,                             IH342     
051760         ADD LIFE-OPT-DED TO CG-DADD,                             IH342     
051770         ADD LIFE-OPT-DED TO AT-DADD,                             IH342     
051780         ADD LIFE-OPT-DED TO PGT-DADD,                            IH342     
051790             GO TO 360250.                                        IH342     
051800     IF MSTR-LIFE-OPT-CODE EQUALS "E",                            IH342     
051810         ADD LIFE-OPT-DED TO PT-EADD,                             IH342     
051820         ADD LIFE-OPT-DED TO CG-EADD,                             IH342     
051830         ADD LIFE-OPT-DED TO AT-EADD,                             IH342     
051840         ADD LIFE-OPT-DED TO PGT-EADD,                            IH342     
051850             GO TO 360250.                                        IH342     
051860     IF MSTR-LIFE-OPT-CODE EQUALS "F",                            IH342     
051870         ADD LIFE-OPT-DED TO PT-FADD,                             IH342     
051880         ADD LIFE-OPT-DED TO CG-FADD,                             IH342     
051890         ADD LIFE-OPT-DED TO AT-FADD,                             IH342     
051900         ADD LIFE-OPT-DED TO PGT-FADD,                            IH342     
051910             GO TO 360250.                                        IH342     
051920     ADD LIFE-OPT-DED TO PT-GADD.                                 IH342     
051930     ADD LIFE-OPT-DED TO CG-GADD.                                 IH342     
051940     ADD LIFE-OPT-DED TO AT-GADD.                                 IH342     
051950     ADD LIFE-OPT-DED TO PGT-GADD.                                IH342     
051960 360250.                                                          IH342     
051970     IF UNION-DUES NOT ZERO,                                      IH342     
051980         ADD UNION-DUES TO PT-UNION,                              IH342     
051990         ADD UNION-DUES TO CG-UNION,                              IH342     
052000         ADD UNION-DUES TO AT-UNION,                              IH342     
052010         ADD UNION-DUES TO PGT-UNION,                             IH342     
052020         MOVE MSTR-UNION TO SR-UNION-CODE,                        IH342     
052030         MOVE UNION-DUES TO SR-UNION-DED.                         IH342     
052040     IF CHARITY-CON IS GREATER THAN +111.00,                      IH342     
052050         SUBTRACT +111.00 FROM CHARITY-CON.                       IH342     
052060     IF CHARITY-CON NOT ZERO,                                     IH342     
052070         ADD CHARITY-CON TO PT-CONTRIB,                           IH342     
052080         ADD CHARITY-CON TO CG-CONTRIB,                           IH342     
052090         ADD CHARITY-CON TO AT-CONTRIB,                           IH342     
052100         ADD CHARITY-CON TO PGT-CONTRIB,                          IH342     
052110         MOVE MSTR-CHARITY-CODE TO SR-CHARITY-CODE,               IH342     
052120         MOVE CHARITY-CON TO SR-CHARITY-DED.                      IH342     
052130     IF OTHER-DED NOT ZERO,                                       IH342     
052140         ADD OTHER-DED TO PT-OTHER,                               IH342     
052150         ADD OTHER-DED TO CG-OTHER,                               IH342     
052160         ADD OTHER-DED TO AT-OTHER,                               IH342     
052170         ADD OTHER-DED TO PGT-OTHER,                              IH342     
052180         MOVE OTHER-DED TO SR-OTHER-DED.                          IH342     
052190     IF BOND-DED NOT ZERO,                                        IH342     
052200         ADD BOND-DED TO PT-BOND,                                 IH342     
052210         ADD BOND-DED TO CG-BOND,                                 IH342     
052220         ADD BOND-DED TO AT-BOND,                                 IH342     
052230         ADD BOND-DED TO PGT-BOND,                                IH342     
052240         MOVE BOND-DED TO SR-BOND-DED.                            IH342     
052250     IF ALLOT-DED NOT ZERO,                                       IH342     
052260         ADD ALLOT-DED TO PT-ALLOT,                               IH342     
052270         ADD ALLOT-DED TO CG-ALLOT,                               IH342     
052280         ADD ALLOT-DED TO AT-ALLOT,                               IH342     
052290         ADD ALLOT-DED TO PGT-ALLOT,                              IH342     
052300         MOVE MSTR-ALLOT1-CODE TO SR-BANK-CODE1,                  IH342     
052310     MOVE ALLOT1-WORK TO SR-BANK-ALLOT-DED1                       IH342     
052320         MOVE MSTR-ALLOT2-CODE TO SR-BANK-CODE2,                  IH342     
052330     MOVE ALLOT2-WORK TO SR-BANK-ALLOT-DED2.                      IH342     
052340     IF MSTR-ADDL-ST-TAX NOT ZERO                                 IH342     
052350         MOVE MSTR-ADDL-ST-TAX TO SR-ADDED-ST-TAX.                IH342     
052360 362162.                                                          IH342     
052370     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING 2 LINES.              IH342     
052380     ADD 002 TO SR-LINE-COUNT.                                    IH342     
052390     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
052400     GO TO 366170.                                                IH342     
052410     NOTE ******************************************************* IH342     
052420          *                                                     * IH342     
052430          *  WRITE PAYROLL SMOOTH ROLL                          * IH342     
052440          *                                                     * IH342     
052450          *******************************************************.IH342     
052460 364040.                                                          IH342     
052470     WRITE SMOOTH-ROLL-REC AFTER ADVANCING NUMBER-OF LINES.       IH342     
052480         MOVE SPACES TO SMOOTH-ROLL-REC.                          IH342     
052490 364070.                                                          IH342     
052500     GO TO 366170.                                                IH342     
052510     NOTE ******************************************************* IH342     
052520          *                                                     * IH342     
052530          *  TAX LEVY DEDUCTION ROUTINE                         * IH342     
052540          *      WRITE SEPARATE CHECK FOR TAX LEVY IF NOT       * IH342     
052550          *          GREATER THAN NET PAY                       * IH342     
052560          *      WRITE NOTIFICATION MESSAGE FOR TAX LEVY        * IH342     
052570          *          EXCEEDING NET PAY                          * IH342     
052580          *                                                     * IH342     
052590          *******************************************************.IH342     
052600 364200.                                                          IH342     
052610     IF DECEASED-PAYOFF, OR PAID-BY-VOUCHER,                      IH342     
052620         GO TO 366045.                                            IH342     
052630     SUBTRACT TAX-LEVY-DED FROM NET-PAY.                          IH342     
052640     IF NET-PAY POSITIVE,                                         IH342     
052650         GO TO 356190.                                            IH342     
052660     ADD TAX-LEVY-DED TO NET-PAY.                                 IH342     
052670 366045.                                                          IH342     
052680     MOVE +00000.00 TO TAX-LEVY-DED.                              IH342     
052690     MOVE "TAX LEVY EXCEEDS NET PAY, ONE CHECK" TO ERR-EXPLAIN.   IH342     
052700     ALTER 198140 TO PROCEED TO 356190.                           IH342     
052710         GO TO 228200.                                            IH342     
052720     NOTE ******************************************************* IH342     
052730          *                                                     * IH342     
052740          *  JUST WROTE SMOOTH ROLL DETAIL LINE                 * IH342     
052750          *      TEST FOR PAGE OVERFLOW                         * IH342     
052760          *                                                     * IH342     
052770          *******************************************************.IH342     
052780 366170.                                                          IH342     
052790     IF SR-LINE-COUNT IS LESS THAN 086,                           IH342     
052800         GO TO 382030.                                            IH342     
052810     ALTER 372090 TO PROCEED TO 382030.                           IH342     
052820     NOTE ******************************************************* IH342     
052830          *                                                     * IH342     
052840          *  WRITE SMOOTH ROLL PAGE AND ACTIVITY TOTAL LINES    * IH342     
052850          *                                                     * IH342     
052860          *******************************************************.IH342     
052870 368080.                                                          IH342     
052880     MOVE 01 TO SUBS-1.                                           IH342     
052890 368100.                                                          IH342     
052900     IF SR-LINE-COUNT IS EQUAL TO 086, MOVE 03 TO NUMBER-OF,      IH342     
052910         GO TO 368170.                                            IH342     
052920     SUBTRACT SR-LINE-COUNT FROM 089 GIVING NUMBER-OF.            IH342     
052930 368170.                                                          IH342     
052940     MOVE RT-EMP-CTR (SUBS-1) TO SR-EMP-COUNT.                    IH342     
052950     MOVE RT-RATE (SUBS-1) TO SR-TOT-RATE.                        IH342     
052960     MOVE RT-NET-PAY (SUBS-1) TO SR-TOT-NET.                      IH342     
052970     MOVE RT-REG-HRS (SUBS-1) TO SR-TOT-REG-HR.                   IH342     
052980     MOVE RT-REG-PAY (SUBS-1) TO SR-TOT-REG.                      IH342     
052990     MOVE RT-OT-HRS (SUBS-1) TO SR-TOT-OT-HR.                     IH342     
053000     MOVE RT-OT-PAY (SUBS-1) TO SR-TOT-OT.                        IH342     
053010     MOVE RT-OTH-HRS (SUBS-1) TO SR-TOT-OTHER-HR.                 IH342     
053020     MOVE RT-OTH-PAY (SUBS-1) TO SR-TOT-OTHER.                    IH342     
053030     MOVE RT-CSRA (SUBS-1) TO SR-TOT-CSRA.                        IH342     
053040     MOVE RT-FICA (SUBS-1) TO SR-TOT-FICA.                        IH342     
053050     MOVE RT-FED-TAX (SUBS-1) TO SR-TOT-FED-TAX.                  IH342     
053060     ALTER 364070 TO PROCEED TO 368240.                           IH342     
053070     GO TO 364040.                                                IH342     
053080 368240.                                                          IH342     
053090     ALTER 364070 TO PROCEED TO 372090.                           IH342     
053100     MOVE RT-ST-TAX (SUBS-1) TO SR-TOT-STATE-TAX.                 IH342     
053110     MOVE RT-HEALTH (SUBS-1) TO SR-TOT-HLTH-DED.                  IH342     
053120     MOVE RT-REG-LIFE (SUBS-1) TO SR-TOT-LIFE-INS.                IH342     
053130     MOVE RT-AADD (SUBS-1) TO SR-TOT-OPTA.                        IH342     
053140     MOVE RT-CADD (SUBS-1) TO SR-TOT-OPTC.                        IH342     
053150     MOVE RT-EADD (SUBS-1) TO SR-TOT-OPTE.                        IH342     
053160     MOVE RT-GADD (SUBS-1) TO SR-TOT-OPTG.                        IH342     
053170     MOVE RT-UNION (SUBS-1) TO SR-TOT-UNION-DED.                  IH342     
053180     MOVE RT-CONTRIB (SUBS-1) TO SR-TOT-CHARITY.                  IH342     
053190     MOVE RT-OTHER (SUBS-1) TO SR-TOT-OTHER-DED.                  IH342     
053200     MOVE RT-BOND (SUBS-1) TO SR-TOT-BOND-DED.                    IH342     
053210     MOVE RT-ALLOT (SUBS-1) TO SR-TOT-ALLOT.                      IH342     
053220     WRITE SMOOTH-ROLL-REC AFTER ADVANCING 2 LINES.               IH342     
053230     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
053240     MOVE RT-BADD (SUBS-1) TO SR-TOT-OPTB.                        IH342     
053250     MOVE RT-DADD (SUBS-1) TO SR-TOT-OPTD.                        IH342     
053260     MOVE RT-FADD (SUBS-1) TO SR-TOT-OPTF.                        IH342     
053270     MOVE RT-OPT-INS (SUBS-1) TO SR-TOT-OPT-INS.                  IH342     
053280     IF RT-OPT-INS (SUBS-1) NOT ZERO MOVE "*" TO SR-AST.          IH342     
053290     MOVE ZEROS TO REDEF-PAGE-ACTIVITY-TOTS (SUBS-1).             IH342     
053300     MOVE 000 TO SR-LINE-COUNT.                                   IH342     
053310     WRITE SMOOTH-ROLL-REC AFTER ADVANCING 1 LINES.               IH342     
053320     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
053330     IF SR-PAGE-SW = 1 AND SUBS-1 = 3                             IH342     
053340       MOVE ZEROS TO SR-PAGE-COUNT.                               IH342     
053350 372090.                                                          IH342     
053360     GO TO 382030.                                                IH342     
053370 372110.                                                          IH342     
053380     PERFORM 340170 THRU 34415E.                                  IH342     
053390     ADD 1 TO SUBS-1.                                             IH342     
053400     ALTER 372090 TO PROCEED TO 372117,                           IH342     
053410              GO TO 368100.                                       IH342     
053420 372117.                                                          IH342     
053430     IF MSTR-ACTY EQUAL TO MINOR-ACTIVITY GO TO 374010.           IH342     
053440 372128.                                                          IH342     
053450     IF MSTR-ACTY EQUAL TO CODE-1 GO TO 374010.                   IH342     
053460     IF MSTR-ACTY EQUAL TO CODE-2 GO TO 374010.                   IH342     
053470     IF MSTR-ACTY EQUAL TO CODE-3 GO TO 374010.                   IH342     
053480     IF MSTR-ACTY EQUAL TO CODE-4 GO TO 374010.                   IH342     
053490     PERFORM 340170 THRU 34415E.                                  IH342     
053500     ADD 01 TO SUBS-1.                                            IH342     
053510     MOVE 1 TO ACTY-CHG.                                          IH342     
053520     ALTER 372090 TO PROCEED TO 374010.                           IH342     
053530         GO TO 368100.                                            IH342     
053540 374010.                                                          IH342     
053550     IF SR-PAGE-SW = 1 NEXT SENTENCE ELSE                         IH342     
053560       MOVE ZEROS TO SR-PAGE-COUNT.                               IH342     
053570     MOVE SPACES TO SMOOTH-ROLL-REC.                              IH342     
053580     MOVE 000 TO SR-LINE-COUNT.                                   IH342     
053590     MOVE 01 TO SUBS-4.                                           IH342     
053600     MOVE 01 TO SUBS-1.                                           IH342     
053610     MOVE 45 TO EX-LINE-COUNT.                                    IH342     
053620     PERFORM 114190.                                              IH342     
053630     ALTER 198140 TO PROCEED TO 380090.                           IH342     
053640     GO TO 378110.                                                IH342     
053650 376000.                                                          IH342     
053660     ADD 1 TO CLPAGE.                                             IH342     
053670     MOVE CLPAGE TO CONTR-PAGE.                                   IH342     
053680     MOVE WEEK-END-DATE TO CONTR-DATE.                            IH342     
053690     WRITE CONTROL-REC FROM CONTR-HEADER AFTER CHANNEL 1.         IH342     
053700     MOVE ALL SPACES TO CONTROL-REC.                              IH342     
053710     WRITE CONTROL-REC AFTER 2 LINES.                             IH342     
053720 378110.                                                          IH342     
053730     MOVE  SAVE-HOLD-CONTROLS TO CNTL-GROUP.                      IH342     
053740 378170.                                                          IH342     
053750     MOVE "GRP" TO CNTL-GRP-KON.                                  IH342     
053760     IF SUBS-1 = 01 OR 04 OR 09                                   IH342     
053770          MOVE CNTRL-TOTS (SUBS-1) TO CNTL-COUNT                  IH342     
053780               ELSE                                               IH342     
053790             MOVE CNTRL-TOTS (SUBS-1) TO CNTL-MONEY.              IH342     
053800 380050.                                                          IH342     
053810     MOVE +0000000.00 TO CNTRL-TOTS (SUBS-1).                     IH342     
053820     MOVE CNTRL-DESC (SUBS-1) TO CNTL-MESSAGE.                    IH342     
053830     MOVE "1" TO SW31.                                            IH342     
053840         ADD +1 TO CONTROL-REC-OUT.                               IH342     
053850             MOVE "CONTROL RECORD" TO CON-MSG-PRINT.              IH342     
053860             GO TO 198050.                                        IH342     
053870 380090.                                                          IH342     
053880     IF SUBS-1 NOT EQUAL TO 10                                    IH342     
053890         ADD 1 TO SUBS-1,                                         IH342     
053900             GO TO 378110.                                        IH342     
053910 380100.                                                          IH342     
053920     MOVE DTL-IN-GRP TO CNTL-MSG-DTL.                             IH342     
053930     MOVE DET-CNTL-MSG TO CONTROL-REC.                            IH342     
053940     WRITE CONTROL-REC AFTER ADVANCING 2 LINES.                   IH342     
053950     MOVE SPACES TO CONTROL-REC.                                  IH342     
053960     MOVE ZEROS TO DTL-IN-GRP.                                    IH342     
053970     IF NOT END-OF-JOB PERFORM 376000 GO TO 114190.               IH342     
053980     MOVE EXC-OUT TO EXC-END-TOTALS.                              IH342     
053990          MOVE "END OF PAYROLL EXCEPTION LISTING",                IH342     
054000             TO FINAL-EXPLAIN.                                    IH342     
054010         MOVE "TOTAL EXCEPTIONS PROCESSED"  TO EXC-EXPLAIN.       IH342     
054020     WRITE PAY-EXCEPT-LIST-REC AFTER ADVANCING 6 LINES.           IH342     
054030     MOVE SPACES TO PAY-EXCEPT-LIST-REC.                          IH342     
054040         GO TO 410100.                                            IH342     
054050     NOTE ******************************************************* IH342     
054060          *                                                     * IH342     
054070          *  PREPARE PAYROLL EARNINGS WORKTAPE ROUTINE          * IH342     
054080          *                                                     * IH342     
054090          *******************************************************.IH342     
054100 382030.                                                          IH342     
054110     MOVE MSTR-OUT-SEQ TO WT-SEQUENCE.                            IH342     
054120     MOVE MSTR-COST-WORK-CNTR TO WT-COST-WORK-CNTR.               IH342     
054130     MOVE MSTR-NAME TO WT-NAME.                                   IH342     
054140     MOVE MSTR-RETIRE-CODE TO WT-RETIRE-CODE.                     IH342     
054150     MOVE MSTR-MARRIED-CODE TO WT-MARRIED-CODE.                   IH342     
054160     IF MSTR-UNION NOT ZERO,                                      IH342     
054170         MOVE MSTR-UNION TO WT-UNION-CODE.                        IH342     
054180         MOVE ROLL-NO TO WT-ROLL.                                 IH342     
054190 384010.                                                          IH342     
054200     IF NET-PAY ZERO GO TO 384015.                                IH342     
054210     IF WRITE-CASH-AWARD,                                         IH342     
054220         OR WRITE-TAX-LEVY,                                       IH342     
054230             GO TO 384110.                                        IH342     
054240 384015.                                                          IH342     
054250     IF MSTR-BANK-DEP-CODE EQUAL TO ZERO OR SPACES                IH342     
054260         GO TO 384110.                                            IH342     
054270     MOVE MSTR-BANK-DEP-CODE TO WT-BANK-DEP-CODE.                 IH342     
054280     MOVE MSTR-BANK-DEP-NO  TO WT-BANK-DEP-NO.                    IH342     
054290     IF NOT COMPOSIT-CHECKS GO TO 384110.                         IH342     
054300 384046.                                                          IH342     
054310     MOVE +00000000 TO WT-CHECK-NO.                               IH342     
054320     GO TO 384114.                                                IH342     
054330 384110.                                                          IH342     
054340     IF NET-PAY ZERO GO TO 384046.                                IH342     
054350     MOVE BEGIN-CHECK-NO TO WT-CHECK-NO.                          IH342     
054360     IF BEGIN-CHECK-NO2 IS EQUAL TO ZEROS  GO TO 384112.          IH342     
054370     IF BEGIN-CHECK-NO2 IS NOT EQUAL TO BEGIN-CHECK-NO,           IH342     
054380         GO TO 384112.                                            IH342     
054390     MOVE BEGIN-CHECK-NO3 TO BEGIN-CHECK-NO.                      IH342     
054400     MOVE BEGIN-CHECK-NO4 TO BEGIN-CHECK-NO2.                     IH342     
054410     GO TO 384114.                                                IH342     
054420 384112.                                                          IH342     
054430     ADD 1 TO BEGIN-CHECK-NO.                                     IH342     
054440 384114.                                                          IH342     
054450     IF ALLOT-DED NOT ZERO,                                       IH342     
054460         MOVE MSTR-ALLOT1-CODE TO WT-ALLOT1-CODE,                 IH342     
054470         MOVE MSTR-ALLOT1-DEP-NO TO WT-ALLOT1-DEP-NO,             IH342     
054480         MOVE MSTR-ALLOT2-CODE TO WT-ALLOT2-CODE,                 IH342     
054490         MOVE MSTR-ALLOT2-DEP-NO TO WT-ALLOT2-DEP-NO.             IH342     
054500 384130.                                                          IH342     
054510     MOVE CHECK-COUNT TO WT-CHECK-CNTR.                           IH342     
054520         ADD 1 TO CHECK-COUNT.                                    IH342     
054530     MOVE NET-PAY TO WT-NET-PAY.                                  IH342     
054540     MOVE MSTR-HOURLY-RATE TO WT-HOURLY-RATE.                     IH342     
054550     IF WRITE-TAX-LEVY,                                           IH342     
054560     MOVE ZEROS TO WT-TAX-LEVY-ZEROS, WT-MONEY-HOURS-ZEROS,       IH342     
054570         WT-MONEY-HRS-ZEROS WT-FED-TAX GO TO 386172.              IH342     
054580     MOVE REG-HRS-WK1 TO WT-REG-HOURS.                            IH342     
054590     MOVE REG-MONEY TO WT-REG-PAY.                                IH342     
054600     IF RETRO-OVERTIME                                            IH342     
054610         SUBTRACT HOLD-RETRO-OTPAY FROM OT-MONEY                  IH342     
054620         SUBTRACT HOLD-RETRO-OTHRS FROM OT-HRS-WK1.               IH342     
054630     MOVE OT-HRS-WK1 TO WT-OT-HOURS.                              IH342     
054640     MOVE OT-MONEY TO WT-OT-PAY.                                  IH342     
054650     IF RETRO-OVERTIME                                            IH342     
054660         ADD HOLD-RETRO-OTPAY TO OT-MONEY                         IH342     
054670         ADD HOLD-RETRO-OTHRS TO OT-HRS-WK1.                      IH342     
054680     IF MSTR-ACTY EQUAL TO CODE-1 OR CODE-2 OR CODE-3 OR CODE-4   IH342     
054690         SUBTRACT CAT-B-HOURS FROM OTHER-HOURS.                   IH342     
054700     MOVE OTHER-HOURS TO WT-OTHER-HOURS.                          IH342     
054710     MOVE OTHER-MONEY TO WT-OTHER-PAY.                            IH342     
054720     MOVE CAT-A-MONEY TO WT-CATA-MONEY.                           IH342     
054730     MOVE CAT-A-HOURS TO WT-CATA-HOURS.                           IH342     
054740     MOVE CAT-B-MONEY TO WT-CATB-MONEY.                           IH342     
054750     MOVE CAT-B-HOURS TO WT-CATB-HOURS.                           IH342     
054760     MOVE CAT-C-MONEY TO WT-CATC-MONEY.                           IH342     
054770     MOVE CAT-C-HOURS TO WT-CATC-HOURS.                           IH342     
054780     MOVE CAT-H-MONEY TO WT-CATH-MONEY.                           IH342     
054790     MOVE CAT-J-MONEY TO WT-CATJ-MONEY.                           IH342     
054800     MOVE CAT-J-HOURS TO WT-CATJ-HOURS.                           IH342     
054810     MOVE CAT-K-MONEY TO WT-CATK-MONEY.                           IH342     
054820     MOVE CAT-L-MONEY TO WT-CATL-MONEY.                           IH342     
054830     MOVE CAT-M-MONEY TO WT-CATM-MONEY.                           IH342     
054840     MOVE CAT-M-HOURS TO WT-CATM-HOURS.                           IH342     
054850     MOVE CAT-N-MONEY TO WT-CATN-MONEY.                           IH342     
054860     MOVE CAT-N-HOURS TO WT-CATN-HOURS.                           IH342     
054870     EXAMINE WT-NEG-OT-RE REPLACING ALL SPACES BY ZEROS.          IH342     
054880     EXAMINE WT-RETRO-OTPAY-RE REPLACING ALL SPACES BY ZEROS.     IH342     
054890     EXAMINE WT-RETRO-OTHRS-RE REPLACING ALL SPACES BY ZEROS.     IH342     
054900     IF FICA-MSTR,                                                IH342     
054910         MOVE WK1-SUBJECT TO WT-SUB-FICA,                         IH342     
054920             GO TO 386052.                                        IH342     
054930         MOVE +0000.00 TO WT-SUB-FICA.                            IH342     
054940 386052.                                                          IH342     
054950     MOVE RETIRE-FICA TO WT-FICA-CSRA.                            IH342     
054960     IF WRITE-CASH-AWARD,                                         IH342     
054970         MOVE +00 TO WT-FED-TAX-CODE,                             IH342     
054980             GO TO 386082.                                        IH342     
054990         MOVE MSTR-TAX-CODE TO WT-FED-TAX-CODE.                   IH342     
055000 386082.                                                          IH342     
055010     MOVE FED-TAXES TO WT-FED-TAX.                                IH342     
055020     MOVE STATE-TAXES TO WT-STATE-TAX.                            IH342     
055030     MOVE MSTR-HLTH-CODE TO WT-HLTH-CODE.                         IH342     
055040     MOVE HLTH-INS-DED TO WT-HLTH-DED.                            IH342     
055050     MOVE HLTH-INS-CON TO WT-HLTH-CONTRIB.                        IH342     
055060     MOVE LIFE-INS-DED TO WT-LIFE-INS-DED.                        IH342     
055070     MOVE LIFE-OPT-DED TO WT-LIFE-OPT-DED.                        IH342     
055080     MOVE UNION-DUES TO WT-UNION-DED.                             IH342     
055090     MOVE MSTR-CHARITY-CODE TO WT-CHARITY-CODE.                   IH342     
055100     MOVE CHARITY-CON TO WT-CHARITY-DED.                          IH342     
055110     MOVE OTHER-DED TO WT-OTHER-DED.                              IH342     
055120     MOVE BOND-DED TO WT-BOND-DED.                                IH342     
055130     MOVE MSTR-CLOCK-NO TO WT-CLOCK-NO.                           IH342     
055140     MOVE MSTR-LIFE-OPT-CODE TO WT-LIFE-OPT-CD.                   IH342     
055150     IF ALLOT-DED ZERO,                                           IH342     
055160         MOVE +000 TO WT-ALLOT1-DED, WT-ALLOT2-DED,               IH342     
055170             GO TO 386172.                                        IH342     
055180     IF ALLOT1-WORK ZERO MOVE +000 TO WT-ALLOT1-DED GO TO 386167. IH342     
055190     MOVE ALLOT1-WORK TO WT-ALLOT1-DED.                           IH342     
055200 386167.                                                          IH342     
055210     IF ALLOT2-WORK ZERO                                          IH342     
055220         MOVE +000 TO WT-ALLOT2-DED,                              IH342     
055230             GO TO 386172.                                        IH342     
055240         MOVE ALLOT2-WORK TO WT-ALLOT2-DED.                       IH342     
055250 386172.                                                          IH342     
055260     MOVE MSTR-OTHER-PAY-CODE TO WT-OTHER-PAY-CODE.               IH342     
055270     MOVE PART-TIME-CODE      TO WT-WORK-SCHED.                   IH342     
055280     MOVE MSTR-FF-CODE        TO WT-FF-CODE.                      IH342     
055290     IF WRITE-CASH-AWARD MOVE "ZZ" TO WT-PAY-EXCEP-CODE.          IH342     
055300     IF WORK-OTH-PAY-CODE EQUAL TO "F",                           IH342     
055310             MOVE "F" TO WT-OTHER-PAY-CODE.                       IH342     
055320     MOVE WEEK-END-DATE TO WT-WE-DATE.                            IH342     
055330     MOVE MSTR-STATE-TAX-CODE TO WT-STATE-TAX-CODE.               IH342     
055340     MOVE MSTR-SS-NO TO WT-SS-NO.                                 IH342     
055350     MOVE MSTR-OCC-CD TO WT-OCC-CD.                               IH342     
055360     WRITE PAYROLL-WORK-REC.                                      IH342     
055370         ADD +1 TO WT-OUT.                                        IH342     
055380         MOVE SPACES TO PAYROLL-WORK-REC.                         IH342     
055390     IF TAX-LEVY-DED NOT ZERO,                                    IH342     
055400         SUBTRACT NET-PAY FROM REG-MONEY,                         IH342     
055410         MOVE TAX-LEVY-DED TO NET-PAY,                            IH342     
055420         MOVE "1" TO SW37,                                        IH342     
055430         MOVE +00000.00 TO TAX-LEVY-DED,                          IH342     
055440         MOVE SPACE TO SW15,                                      IH342     
055450             GO TO 338170.                                        IH342     
055460 CFOOT1.                                                          IH342     
055470     NOTE ******************************************************* IH342     
055480          *                                                     * IH342     
055490          *  CROSSFOOT CURRENT MONEY                            * IH342     
055500          *      WRITE NOTIFICATION MESSAGE IF CURRENT MONEY    * IH342     
055510          *          DOES NOT CROSSFOOT TO ZERO                 * IH342     
055520          *                                                     * IH342     
055530          *******************************************************.IH342     
055540 387000.                                                          IH342     
055550     ADD REG-MONEY, OT-MONEY, OTHER-MONEY,                        IH342     
055560         GIVING HRLY-RATE-WORK.                                   IH342     
055570     SUBTRACT NET-PAY,                                            IH342     
055580              RETIRE-FICA,                                        IH342     
055590              FED-TAXES,                                          IH342     
055600              STATE-TAXES,                                        IH342     
055610              HLTH-INS-DED,                                       IH342     
055620              LIFE-INS-DED,                                       IH342     
055630              LIFE-OPT-DED,                                       IH342     
055640              UNION-DUES,                                         IH342     
055650              CHARITY-CON,                                        IH342     
055660              OTHER-DED,                                          IH342     
055670              BOND-DED,                                           IH342     
055680              ALLOT-DED, FROM HRLY-RATE-WORK.                     IH342     
055690     IF HRLY-RATE-WORK NOT ZERO,                                  IH342     
055700         MOVE HRLY-RATE-WORK TO MSG9-AMT,                         IH342     
055710         MOVE MESSAGE9 TO ERR-EXPLAIN,                            IH342     
055720         ALTER 198140 TO PROCEED TO 392010,                       IH342     
055730             GO TO 228200.                                        IH342     
055740     NOTE ******************************************************* IH342     
055750          *                                                     * IH342     
055760          *  ZERO OUT ALL CURRENT PAYROLL TOTALS                * IH342     
055770          *      TEST FOR ADDITIONAL CASH AWARD CHECKS          * IH342     
055780          *      TURN OFF ALL PROGRAM SWITCHES                  * IH342     
055790          *                                                     * IH342     
055800          *******************************************************.IH342     
055810 392010.                                                          IH342     
055820     MOVE ZEROS TO PAYROLL-WORK-AREAS.                            IH342     
055830     MOVE SPACES TO WT-PAY-EXC-HOLD, WORK-OTH-PAY-CODE.           IH342     
055840     IF CASH-AWARD,                                               IH342     
055850         MOVE SPACE TO SW30, SW18,                                IH342     
055860         MOVE "1" TO SW36,                                        IH342     
055870         ADD DTL-AMOUNT TO OTHER-MONEY,                           IH342     
055880             GO TO 116140.                                        IH342     
055890     MOVE SPACE TO SW4, SW6, SW7, SW8, SW9, SW10, SW11, SW12,     IH342     
055900         SW13, SW14, SW15, SW16, SW17, SW18, SW19, SW20, SW21,    IH342     
055910         SW22, SW23, SW24, SW25, SW26, SW27, SW28, SW29, SW30,    IH342     
055920         SW31, SW32, SW33, SW34, SW35, SW36, SW37.                IH342     
055930     MOVE SPACE TO SW40, SW41, SW42, SW43, SW44, SW45, SW46,      IH342     
055940         SW47 SW48 SW49 SW50 SW51 SW52 SW53 SW54 SW55 SW56.       IH342     
055950     MOVE SPACE TO SW57.                                          IH342     
055960 EXCNOTE1.                                                        IH342     
055970     NOTE ******************************************************* IH342     
055980          *                                                     * IH342     
055990          *  PREPARE PAYROLL EXCEPTION LISTING                  * IH342     
056000          *      LIST ALL EXCEPTION PAY CODES WITH DESCRIPTION  * IH342     
056010          *                                                     * IH342     
056020          *******************************************************.IH342     
056030 393000.                                                          IH342     
056040     IF NOT PAY-EXCEPT,                                           IH342     
056050         GO TO 406100.                                            IH342     
056060     MOVE SPACE TO SW5.                                           IH342     
056070     IF EX-LINE-COUNT LESS THAN 45, GO TO 400040.                 IH342     
056080     NOTE ******************************************************* IH342     
056090          *                                                     * IH342     
056100          *  PREPARE EXCEPTION LIST HEADING LINE                * IH342     
056110          *                                                     * IH342     
056120          *******************************************************.IH342     
056130 396050.                                                          IH342     
056140     MOVE +000 TO EX-LINE-COUNT.                                  IH342     
056150     MOVE 01 TO SUBS-4.                                           IH342     
056160 396100.                                                          IH342     
056170     IF MINOR-ACTIVITY EQUAL TO SR-ACT-CODE (SUBS-4)              IH342     
056180        MOVE SR-LOCATION-HDR (SUBS-4) TO EXC-ACTIVITY,            IH342     
056190             GO TO 398012.                                        IH342     
056200     IF SUBS-4 GREATER THAN 25 GO TO 398012.                      IH342     
056210     ADD 01 TO SUBS-4, GO TO 396100.                              IH342     
056220 398012.                                                          IH342     
056230     MOVE 01 TO SUBS-4.                                           IH342     
056240 398080.                                                          IH342     
056250     ADD 1 TO EXCPAGE.                                            IH342     
056260     MOVE EXCPAGE TO EXC-PG-NO.                                   IH342     
056270     MOVE "PAGE" TO EXPGCON.                                      IH342     
056280     IF SAVE-GORU EQUAL TO "G" OR "0" OR " "                      IH342     
056290         MOVE "GRADED" TO EXC-AORD                                IH342     
056300         ELSE MOVE "UNGRADED" TO EXC-AORD.                        IH342     
056310     MOVE "PAYROLL EXCEPTION LISTING" TO EXC-LIST-TITLE.          IH342     
056320     MOVE "ROLL" TO EXC-ROLL-KON.                                 IH342     
056330     MOVE ROLL-NO TO EXC-ROLL.                                    IH342     
056340     MOVE WE-MONTH TO EXC-WE-MO.                                  IH342     
056350     MOVE WE-DAY TO EXC-WE-DAY.                                   IH342     
056360     MOVE WE-YEAR TO EXC-WE-YR.                                   IH342     
056370     MOVE "/" TO EXC-SLASH1, EXC-SLASH2.                          IH342     
056380     WRITE EXCEPT-LIST-HEADING AFTER ADVANCING CHANNEL 1.         IH342     
056390     MOVE SPACES TO PAY-EXCEPT-LIST-REC.                          IH342     
056400     WRITE PAY-EXCEPT-LIST-REC BEFORE ADVANCING 2 LINES.          IH342     
056410     MOVE 02 TO EX-LINE-COUNT.                                    IH342     
056420     NOTE ******************************************************* IH342     
056430          *                                                     * IH342     
056440          *  PREPARE EXCEPTION LIST DETAIL PRINT LINES          * IH342     
056450          *                                                     * IH342     
056460          *******************************************************.IH342     
056470 400040.                                                          IH342     
056480     ADD 1 TO EX-LINE-COUNT.                                      IH342     
056490     MOVE MSTR-ACTY     TO EXC-ACTY-PRINT.                        IH342     
056500     MOVE MSTR-CONT-GRP TO EXC-CTL-GRP-PRINT.                     IH342     
056510     MOVE "BADGE NO" TO EXC-BADGE-KON.                            IH342     
056520     MOVE MSTR-BADGE TO EXC-BADGE.                                IH342     
056530     MOVE "," TO EXC-COMMA-KON.                                   IH342     
056540     MOVE MSTR-NAME TO EXC-NAME.                                  IH342     
056550     ALTER 400150 TO PROCEED TO 400170.                           IH342     
056560 400100.                                                          IH342     
056570     ADD 1 TO EX-LINE-COUNT.                                      IH342     
056580 400120.                                                          IH342     
056590     WRITE PAY-EXCEPT-LIST-REC AFTER ADVANCING 1 LINES.           IH342     
056600         MOVE SPACES TO PAY-EXCEPT-LIST-REC.                      IH342     
056610 400150.                                                          IH342     
056620     GO TO 400170.                                                IH342     
056630 400170.                                                          IH342     
056640     MOVE 01 TO SUBS-1.                                           IH342     
056650     ALTER 400150 TO PROCEED TO 402060.                           IH342     
056660 400200.                                                          IH342     
056670     IF EXC-LIST-TOT (SUBS-1) ZERO,                               IH342     
056680         GO TO 402060.                                            IH342     
056690     IF EX-LINE-COUNT GREATER THAN 45,                            IH342     
056700         GO TO 396050.                                            IH342     
056710     MOVE PAY-EXC-CODE (SUBS-1) TO EXC-CODE.                      IH342     
056720     MOVE PAY-EXC-DESC (SUBS-1) TO EXC-EXPLAIN.                   IH342     
056730     MOVE EXC-LIST-TOT (SUBS-1) TO EXC-HRS-AMT.                   IH342     
056740     MOVE +00000.00 TO EXC-LIST-TOT (SUBS-1).                     IH342     
056750     ADD +1 TO EXC-OUT.                                           IH342     
056760         GO TO 400100.                                            IH342     
056770 402060.                                                          IH342     
056780     IF SUBS-1 LESS THAN 48                                       IH342     
056790         ADD 1 TO SUBS-1,                                         IH342     
056800             GO TO 400200.                                        IH342     
056810     MOVE SPACES TO PAY-EXCEPT-LIST-REC.                          IH342     
056820     WRITE PAY-EXCEPT-LIST-REC AFTER ADVANCING 1 LINES.           IH342     
056830     ADD 01 TO EX-LINE-COUNT.                                     IH342     
056840     NOTE ******************************************************* IH342     
056850          *                                                     * IH342     
056860          *  WRITE PAYROLL/PERSONNEL MASTER RECORD              * IH342     
056870          *      ALL CURRENT PAYROLL EARNINGS AND DEDUCTIONS    * IH342     
056880          *          HAVE BEEN UPDATED IN MASTER                * IH342     
056890          *                                                     * IH342     
056900          *******************************************************.IH342     
056910 406100.                                                          IH342     
056920     IF ACTIVE-MSTR,                                              IH342     
056930         ADD +1 TO ACTIVE-OUT.                                    IH342     
056940     WRITE PERS-MSTR-OUT-REC.                                     IH342     
056950         ADD +1 TO MSTR-OUT.                                      IH342     
056960         GO TO 108090.                                            IH342     
056970     NOTE ******************************************************* IH342     
056980          *                                                     * IH342     
056990          *  PAYROLL DETAIL END OF FILE                         * IH342     
057000          *      TEST FOR PAYROLL MASTER END OF FILE            * IH342     
057010          *                                                     * IH342     
057020          *******************************************************.IH342     
057030 408010.                                                          IH342     
057040     IF EOF-MASTER,                                               IH342     
057050         GO TO 408180.                                            IH342     
057060     MOVE "1" TO SW3.                                             IH342     
057070         GO TO 120110.                                            IH342     
057080     NOTE ******************************************************* IH342     
057090          *                                                     * IH342     
057100          *  PAYROLL MASTER END OF FILE                         * IH342     
057110          *      TEST FOR PAYROLL DETAIL END OF FILE            * IH342     
057120          *                                                     * IH342     
057130          *******************************************************.IH342     
057140 408140.                                                          IH342     
057150     IF NOT EOF-DETAIL,                                           IH342     
057160         MOVE "1" TO SW2,                                         IH342     
057170             GO TO 196140.                                        IH342     
057180 408180.                                                          IH342     
057190     PERFORM 368080 THRU 368240.                                  IH342     
057200     MOVE 02 TO SUBS-1.                                           IH342     
057210     MOVE "1" TO SW38.                                            IH342     
057220     PERFORM 340170 THRU 34415E.                                  IH342     
057230     ALTER 372090 TO PROCEED TO 372110.                           IH342     
057240     MOVE HIGH-VALUES TO MINOR-ACTIVITY.                          IH342     
057250     GO TO 368100.                                                IH342     
057260     NOTE ******************************************************* IH342     
057270          *                                                     * IH342     
057280          *  END OF JOB                                         * IH342     
057290          *      WRITE EOJ RECORD COUNTS                        * IH342     
057300          *      CLOSE ALL INPUT/OUTPUT FILES                   * IH342     
057310          *                                                     * IH342     
057320          *******************************************************.IH342     
057330 410100.                                                          IH342     
057340     MOVE 01 TO SUBS-1.                                           IH342     
057350 410120.                                                          IH342     
057360     MOVE EOJ-DESC (SUBS-1) TO EOJ-MSG-DESC.                      IH342     
057370     MOVE EOJ-TOTALS (SUBS-1) TO EOJ-MSG-TOT.                     IH342     
057380     DISPLAY EOJ-MSG.                                             IH342     
057390     IF SUBS-1 LESS THAN 10,                                      IH342     
057400         ADD 1 TO SUBS-1,                                         IH342     
057410             GO TO 410120.                                        IH342     
057420 420000.                                                          IH342     
057430     MOVE ALL SPACES TO SMOOTH-ROLL-REC.                          IH342     
057440     WRITE SMOOTH-ROLL-REC BEFORE ADVANCING CHANNEL 1.            IH342     
057450     MOVE 04 TO SUBS-1.                                           IH342     
057460     MOVE SPACES TO MINOR-ACTIVITY, MINOR-CONTROL.                IH342     
057470     PERFORM 340170 THRU 34415E.                                  IH342     
057480     ALTER 372090 TO PROCEED TO 421000.                           IH342     
057490     GO TO 368100.                                                IH342     
057500 421000.                                                          IH342     
057510     EXIT.                                                        IH342     
057520 CLOSE1.                                                          IH342     
057530     NOTE ******************************************************* IH342     
057540          *                                                     * IH342     
057550          *  CLOSE ALL INPUT/OUTPUT FILES                       * IH342     
057560          *                                                     * IH342     
057570          *******************************************************.IH342     
057580 430000.                                                          IH342     
057590     CLOSE PERS-MSTR-IN-FILE WITH LOCK.                           IH342     
057600     CLOSE PAYROLL-DETAIL-FILE WITH LOCK.                         IH342     
057610     CLOSE PERS-MSTR-OUT-FILE WITH LOCK.                          IH342     
057620     CLOSE SMOOTH-ROLL-FILE WITH LOCK.                            IH342     
057630     CLOSE PAYROLL-WORK-FILE WITH LOCK.                           IH342     
057640     CLOSE CONTROL-ERROR-FILE WITH LOCK.                          IH342     
057650     CLOSE PAY-EXCEPT-LIST-FILE WITH LOCK.                        IH342     
057660         STOP RUN.                                                IH342     
057670 480000.                                                          IH342     
057680     DISPLAY "XXXXXXXXXX DETAIL IN  " DTL-SEQ.                    IH342     
057690     DISPLAY "XXXXXXXXXX DETAIL HOLD" SEQ-PAY-DTL.                IH342     
057700     STOP "XXXXXXXXXX HALT 0002".                                 IH342     
057710     GO TO 480000.                                                IH342     
057720 490000.                                                          IH342     
057730     DISPLAY "XXXXXXXXXX PARAM CARD MISSING".                     IH342     
057740     STOP "XXXXXXXXXX HALT 0003".                                 IH342     
057750     GO TO 490000.                                                IH342     
057760 PATCHS.                                                          IH342     
057770     GO TO PATCHS.                                                IH342     
057780 END-OF-JOB.                                                      IH342     
057790                                                                  IH342     
   fMa√