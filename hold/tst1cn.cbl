000001 IDENTIFICATION DIVISION.                                         TST1CN  
000002 PROGRAM-ID. TST1.                                                TST1CN  
000003 AUTHOR. ME.                                                      TST1CN  
000004 INSTALLATION. HERE.                                              TST1CN  
000005 SECURITY. NONE.                                                  TST1CN  
000006 REMARKS. NONE.                                                   TST1CN  
000007 ENVIRONMENT DIVISION.                                            TST1CN  
000008 CONFIGURATION SECTION.                                           TST1CN  
000009 SOURCE-COMPUTER. PDP-10.                                         TST1CN  
000010 OBJECT-COMPUTER. PDP-10.                                         TST1CN  
000011 INPUT-OUTPUT SECTION.                                            TST1CN  
000012 FILE-CONTROL.                                                    TST1CN  
000013        SELECT ABC ASSIGN TO DSK                                  TST1CN  
000014        RECORDING MODE IS ASCII.                                  TST1CN  
000015 DATA DIVISION.                                                   TST1CN  
000016 WORKING-STORAGE SECTION.                                         TST1CN  
000017 01 ONLY RECORD.                                                  TST1CN  
000018                                                                  TST1CN  
000019 WORKING STO                                                      TST1CN  
000020                                                                  TST1CN  
000021 END OF WORK                                                      TST1CN  
000022 A                                                                TST1CN  
000023 PROCEDURE DIVISION.                                              TST1CN  
000024 USER-FIRST-PARAGRAPH.                                            TST1CN  
000025 FIRST-AND-LAST-PARA.                                             TST1CN  
000026        STOP RUN.                                                 TST1CN  
   