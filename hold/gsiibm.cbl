000010 IDENTIFICATION DIVISION.                                         GSIIBM    
000020 PROGRAM-ID. GSI.                                                 GSIIBM    
000030 AUTHOR. BISIAUX.                                                 GSIIBM    
000040 REMARKS. CHANGEMENT DU NO DE CLIENT SUR FICHIER RELEVES.         GSIIBM    
000050 ENVIRONMENT DIVISION.                                            GSIIBM    
000060 CONFIGURATION SECTION.                                           GSIIBM    
000070 SOURCE-COMPUTER. S/360.                                          GSIIBM    
000080 OBJECT-COMPUTER. S/360 SEGMENT-LIMIT IS 01.                      GSIIBM    
000090 INPUT-OUTPUT SECTION.                                            GSIIBM    
000100 FILE-CONTROL.                                                    GSIIBM    
000110        SELECT FICIN ASSIGN    TO "SYS001".                       GSIIBM    
000120        RECORDING MODE IS ASCII.                                  GSIIBM    
000130        SELECT FICOUT ASSIGN    TO "SYS002".                      GSIIBM    
000140        RECORDING MODE IS ASCII.                                  GSIIBM    
000150        SELECT FICMVT ASSIGN    TO "SYS003".                      GSIIBM    
000160        RECORDING MODE IS ASCII.                                  GSIIBM    
000170        SELECT FICANO ASSIGN    TO "SYS004".                      GSIIBM    
000180        RECORDING MODE IS ASCII.                                  GSIIBM    
000190 DATA DIVISION.                                                   GSIIBM    
000200 FILE SECTION.                                                    GSIIBM    
000210 FD FICIN                                                         GSIIBM    
000220        RECORD CONTAINS 40                                        GSIIBM    
000230        LABEL RECORD STANDARD                                     GSIIBM    
000240        VALUE OF IDENTIFICATION IS "FICIN DAT".                   GSIIBM    
000250 01 ENTREE PIC X(40).                                             GSIIBM    
000260 FD FICOUT                                                        GSIIBM    
000270        RECORD CONTAINS 40                                        GSIIBM    
000280        LABEL RECORD STANDARD                                     GSIIBM    
000290        VALUE OF IDENTIFICATION IS "FICOUTDAT".                   GSIIBM    
000300 01 SORTIE PIC X(40).                                             GSIIBM    
000310 FD FICMVT                                                        GSIIBM    
000320        RECORD CONTAINS 80                                        GSIIBM    
000330        LABEL RECORD STANDARD                                     GSIIBM    
000340        VALUE OF IDENTIFICATION IS "FICMVTDAT".                   GSIIBM    
000350 01 DONMVT PIC X(80).                                             GSIIBM    
000360 FD FICANO                                                        GSIIBM    
000370        RECORD CONTAINS 133                                       GSIIBM    
000380        LABEL RECORD STANDARD                                     GSIIBM    
000390        VALUE OF IDENTIFICATION IS "FICANODAT".                   GSIIBM    
000400 01 IMP.                                                          GSIIBM    
000410                02 SAUT PIC X.                                    GSIIBM    
000420                02 LIGNE PIC X(132).                              GSIIBM    
000430 WORKING-STORAGE SECTION.                                         GSIIBM    
000440 77 CTRLIGNE PIC S99 COMP VALUE 70.                               GSIIBM    
000450 77 CTRPAGE PIC S9(6) COMP VALUE 0.                               GSIIBM    
000460 77 FINMVT PIC X VALUE LOW-VALUE.                                 GSIIBM    
000470 77 FINPERM PIC X VALUE LOW-VALUE.                                GSIIBM    
000480 77 CTRMVT PIC S9(9) VALUE 0.                                     GSIIBM    
000490 77 CTRPER PIC S9(9) VALUE 0.                                     GSIIBM    
000500 01 MOUV.                                                         GSIIBM    
000510        02 CARMVT PIC XX.                                         GSIIBM    
000520        02 SOCMVT PIC XXX.                                        GSIIBM    
000530        02 ANCMVT PIC X(12).                                      GSIIBM    
000540        02 NOUMVT PIC X(12).                                      GSIIBM    
000550        02 FILLER PIC X(51).                                      GSIIBM    
000560 01 PERM.                                                         GSIIBM    
000570        02 FILLER PIC XX.                                         GSIIBM    
000580        02 SOCPERM PIC XXX.                                       GSIIBM    
000590        02 CPTPERM PIC X(12).                                     GSIIBM    
000600        02 FILLER PIC X(23).                                      GSIIBM    
000610 01 LIGTIT1.                                                      GSIIBM    
000620        02 FILLER PIC X VALUE "1".                                GSIIBM    
000630        02 FILLER PIC X(18) VALUE "   ETS. S.S. WHITE".           GSIIBM    
000640        02 FILLER PIC X(60) VALUE SPACES.                         GSIIBM    
000650        02 FILLER PIC X(5) VALUE "DATE ".                         GSIIBM    
000660        02 DATPARM PIC X(8).                                      GSIIBM    
000670        02 FILLER PIC X(20) VALUE SPACES.                         GSIIBM    
000680        02 FILLER PIC X(5) VALUE "PAGE".                          GSIIBM    
000690        02 NUMPAGE PIC Z(5)9.                                     GSIIBM    
000700        02 FILLER PIC X(10) VALUE SPACES.                         GSIIBM    
000710 01 LIGTIT2.                                                      GSIIBM    
000720        02 FILLER PIC X VALUE "-".                                GSIIBM    
000730        02 FILLER PIC X(42) VALUE SPACES.                         GSIIBM    
000740        02 FILLER PIC X(46) VALUE "MODIFICATION DU CODE CLIENT    GSIIBM    
000750-       "AU FICHIER COMPTES".                                     GSIIBM    
000760 01 LIGTIT3.                                                      GSIIBM    
000770        02 FILLER PIC X VALUE "-".                                GSIIBM    
000780        02 FILLER PIC X(60) VALUE SPACES.                         GSIIBM    
000790        02 FILLER PIC X(38) VALUE "STE      ANCIEN CODE      NO   GSIIBM    
000800-       "UVEAU CODE".                                             GSIIBM    
000810 01 LIGBAN.                                                       GSIIBM    
000820        02 FILLER PIC X(4) VALUE "0   ".                          GSIIBM    
000830        02 LIBEL PIC X(57).                                       GSIIBM    
000840        02 S-STE PIC XXX.                                         GSIIBM    
000850        02 FILLER PIC X(6) VALUE SPACES.                          GSIIBM    
000860        02 S-ANCOD PIC X(12).                                     GSIIBM    
000870        02 FILLER PIC X(6) VALUE SPACES.                          GSIIBM    
000880        02 S-NOCOD PIC X(12).                                     GSIIBM    
000890 PROCEDURE DIVISION.                                              GSIIBM    
000900 USER-FIRST-PARAGRAPH.                                            GSIIBM    
000910        OPEN INPUT FICIN FICMVT OUTPUT FICOUT FICANO.             GSIIBM    
000920 LECPERM.                                                         GSIIBM    
000930        IF FINPERM = HIGH-VALUE GO TO SLECPERM.                   GSIIBM    
000940        READ FICIN INTO PERM AT END MOVE HIGH-VALUE TO FINPERM    GSIIBM    
000950        SOCPERM CPTPERM.                                          GSIIBM    
000960        ADD 1 TO CTRPER.                                          GSIIBM    
000970 FLECPERM. EXIT.                                                  GSIIBM    
000980 LECMVT.                                                          GSIIBM    
000990        IF FINMVT = HIGH-VALUE GO TO FLECMVT.                     GSIIBM    
001000        READ FICMVT INTO MOUV AT END MOVE HIGH-VALUE TO FINMVT SOCMVT       
001010        ANCMVT.                                                   GSIIBM    
001020        ADD 1 TO CTRMVT.                                          GSIIBM    
001030 FLECMVT. EXIT.                                                   GSIIBM    
001040 SLECPERM.                                                        GSIIBM    
001050        IF FINMVT = HIGH-VALUE GO TO FINTRA.                      GSIIBM    
001060 S1.     IF FINPERM = HIGH-VALUE AND FINMVT = HIGH-VALUE          GSIIBM    
001070        GO TO FINTRA.                                             GSIIBM    
001080        IF FINPERM = HIGH-VALUE GO TO COMPARE.                    GSIIBM    
001090        IF FINMVT = HIGH-VALUE                                    GSIIBM    
001100        PERFORM ECRPERM THRU FECRPERM                             GSIIBM    
001110        PERFORM LECPERM THRU FLECPERM                             GSIIBM    
001120        GO TO S1.                                                 GSIIBM    
001130 COMPARE.                                                         GSIIBM    
001140        IF CARMVT NOT = "40"                                      GSIIBM    
001150        DISPLAY "CODE CARTE INVALIDE" CARMVT "COMPTE " ANCMVT     GSIIBM    
001160        PERFORM LECMVT THRU FLECMVT GO TO S1.                     GSIIBM    
001170        IF SOCMVT < SOCPERM                                       GSIIBM    
001180        MOVE "MODIFICATION IMPOSSIBLE - CODE CLIENT INEXISTANT"   GSIIBM    
001190        TO LIBEL                                                  GSIIBM    
001200        PERFORM ECRLIST THRU FECRLIST                             GSIIBM    
001210        PERFORM LECMVT THRU FLECMVT                               GSIIBM    
001220        GO TO S1.                                                 GSIIBM    
001230        IF SOCMVT > SOCPERM                                       GSIIBM    
001240        PERFORM ECRPERM THRU FECRPERM                             GSIIBM    
001250        PERFORM LECPERM THRU FLECPERM                             GSIIBM    
001260        GO TO S1.                                                 GSIIBM    
001270        IF ANCMVT < CPTPERM                                       GSIIBM    
001280        MOVE "MODIFICATION IMPOSSIBLE - CODE CLIENT INEXISTANT"   GSIIBM    
001290         TO LIBEL                                                 GSIIBM    
001300        PERFORM ECRLIST THRU FECRLIST                             GSIIBM    
001310        PERFORM LECMVT  THRU FLECMVT                              GSIIBM    
001320        GO TO S1.                                                 GSIIBM    
001330        IF ANCMVT > CPTPERM                                       GSIIBM    
001340        PERFORM ECRPERM THRU FECRPERM                             GSIIBM    
001350        PERFORM LECPERM THRU FLECPERM                             GSIIBM    
001360        GO TO S1.                                                 GSIIBM    
001370        MOVE NOUMVT TO CPTPERM.                                   GSIIBM    
001380        MOVE "MODIFICATION EFFECTUEE " TO LIBEL.                  GSIIBM    
001390        PERFORM ECRLIST THRU FECRLIST.                            GSIIBM    
001400        PERFORM ECRPERM THRU FECRPERM.                            GSIIBM    
001410        PERFORM LECPERM THRU FLECPERM.                            GSIIBM    
001420        PERFORM LECMVT  THRU FLECMVT.                             GSIIBM    
001430        GO TO S1.                                                 GSIIBM    
001440 ECRPERM.                                                         GSIIBM    
001450        WRITE SORTIE FROM PERM.                                   GSIIBM    
001460 FECRPERM.                                                        GSIIBM    
001470        EXIT.                                                     GSIIBM    
001480 ECRLIST.                                                         GSIIBM    
001490        IF CTRLIGNE > 60 PERFORM TITRE THRU FTITRE.               GSIIBM    
001500        MOVE SOCMVT TO S-STE.                                     GSIIBM    
001510        MOVE ANCMVT TO S-ANCOD.                                   GSIIBM    
001520        MOVE NOUMVT TO S-NOCOD. MOVE LIGBAN TO IMP.               GSIIBM    
001530        PERFORM ECRLIG THRU FECRLIG.                              GSIIBM    
001540 FECRLIST.                                                        GSIIBM    
001550        EXIT.                                                     GSIIBM    
001560 ECRLIG.                                                          GSIIBM    
001570        IF SAUT = "-" WRITE IMP AFTER 3                           GSIIBM    
001580        ADD 3 TO CTRLIGNE.                                        GSIIBM    
001590        IF SAUT = "0" WRITE IMP AFTER 2                           GSIIBM    
001600        ADD 2 TO CTRLIGNE.                                        GSIIBM    
001610 FECRLIG. EXIT.                                                   GSIIBM    
001620 TITRE.                                                           GSIIBM    
001630        MOVE 0 TO CTRLIGNE.                                       GSIIBM    
001640        ADD 1 TO CTRPAGE.                                         GSIIBM    
001650        MOVE CTRPAGE TO NUMPAGE.                                  GSIIBM    
001660        MOVE LIGTIT1 TO IMP.                                      GSIIBM    
001670        PERFORM ECRLIG THRU FECRLIG.                              GSIIBM    
001680        MOVE LIGTIT2 TO IMP.                                      GSIIBM    
001690        PERFORM ECRLIG THRU FECRLIG.                              GSIIBM    
001700        MOVE LIGTIT3 TO IMP.                                      GSIIBM    
001710        PERFORM ECRLIG THRU FECRLIG.                              GSIIBM    
001720 FTITRE. EXIT.                                                    GSIIBM    
001730 FINTRA.                                                          GSIIBM    
001740        DISPLAY "NOMBRE ENR. FICHIER " CTRPER.                    GSIIBM    
001750        DISPLAY "NOMBRE ENR. MVT. " CTRMVT.                       GSIIBM    
001760        MOVE SPACE TO S-STE S-ANCOD S-NOCOD.                      GSIIBM    
001770        MOVE "FIN DE MODIFICATION " TO LIBEL.                     GSIIBM    
001780        MOVE LIGBAN TO IMP.                                       GSIIBM    
001790        PERFORM ECRLIG THRU FECRLIG.                              GSIIBM    
001800        CLOSE FICIN FICOUT FICMVT FICANO.                         GSIIBM    
001810        STOP RUN.                                                 GSIIBM    
001820 END-OF-JOB.                                                      GSIIBM    
001830                                                                  GSIIBM    
   