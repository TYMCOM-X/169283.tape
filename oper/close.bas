100 '
110 '     C L O S E
120 '     ---------
130 '
140 '     INITIALIZES/SCRATCHES FILES AT BEGINNING OF ACCT. PERIOD
150 '
160 '     COPYRIGHT 1973, LUPFER & LONG COMPUTER SERVICES.
170 '
180 DIM G$(5),C$(10)
190 DIM I(132),O(132)
200 DIM T(50),F$(50)
210 REM =========== INIT =============
250 FILE #3, "COMCOM.TMP"
260 FOR I=1 TO 50
265   IF END #3 GO TO 290
270   READ #3, F$(I)
280 NEXT I
290 FILE :4: F$(5)		'  .COA
300   GOSUB 8500
310 T(0)=10
320 FOR I=1 TO T(0)
330   T(I)=100
340 NEXT I
350 CHANGE T TO H$
360 E9$="EOF"
390 REM ---------- CLOSE .COA ------------
400 IF G6=1 GO TO 430
410   PRINT "CHART OF ACCOUNTS NOT UPDATED - CAN'T CONTINUE"
420   GO TO 1010
430 CHANGE G1$ TO O
435 O(15)=100
440 PRINT "DATE FOR END OF NEXT ACCOUNTING PERIOD";
450 INPUT R$
460 CHANGE R$ TO T
470 GOSUB 3380
480   IF E1>0 GO TO 450
510 V1=29+8*G4
520 GOSUB 2600			'  INSERT DATE
570 SET :4, 1
580 CHANGE O TO O$
590 WRITE :4: O$
600 REM -------- SCRATCH LEDGER ----------
610 FILE :1: F$(4)		'  .MDT
630   SCRATCH :1
680 WRITE :1: H$
690 WRITE :1: E9$
700 REM -------- SCRATCH JOURNALS --------
710 FOR I=21 TO 50
720   IF F$(I)="*" GO TO 900
730     F$(I) = F$(I) + ".DAT$"
740     FILE :1: F$(I)
760       SCRATCH :1
765       WRITE :1: H$
768       WRITE :1: E9$
770 NEXT I
900 REM ========= WRAP-UP ================
910 PRINT "SYSTEM INITIALIZED"
1000 REM ----------- DONE ----------------
1010 CHAIN F$(10)+F$(9)
2000 REM ======= SUBROUTINES =============
2010 '
2600 REM -------- INSERT ----------
2610 K1=1
2620 FOR K2=V1 TO V1+T(0)-1
2630   O(K2)=T(K1)
2640   K1=K1+1
2650 NEXT K2
2660 RETURN
3000 REM --------- EDIT ROUTINES --------
3380 REM     - FILL -
3390 E1$="-"
3400 E1=0
3410 D=8-T(0)
3430 FOR K1=8 TO 1 STEP -1
3440   IF K1<=D GO TO 3470
3450   T(K1)=T(K1-D)
3460     GO TO 3480
3470   T(K1)=ASC(0)
3480 NEXT K1
3540 T(0)=8
3560 REM     - DATE -
3580 IF T(6)<>ASC(-) GO TO 3760
3590 IF T(3)= ASC(-) GO TO 3650
3600 IF T(4)<>ASC(-) GO TO 3760
3610   FOR K1=1 TO 3
3620     T(K1)=T(K1+1)
3630   NEXT K1
3640   T(4)=ASC(0)
3650 IF T(1)> ASC(1) GO TO 4120
3660 IF T(4)> ASC(3) GO TO 4120
3670 IF T(7)<>ASC(7) GO TO 4120
3750   GO TO 4140
3760 E1$="USE FORM 'MM-DD-YY'"
3770   GO TO 4120
3780 '
3790 REM     - NUMERIC -
3800 E1=0
3805 E1$="-"
3810 T1=T(0)
3840 FOR K1=1 TO T1
3850   IF T(K1)>ASC(9) GO TO 3890
3860   IF T(K1)<ASC(0) GO TO 3890
3870 NEXT K1
3880   GO TO 4140
3890 E1$="NON-NUMERIC CHARACTER"
3900   GO TO 4120
4110 '
4120 PRINT " ERROR - ";E1$;" - RETYPE LINE"
4130 E1=1
4140 RETURN
6000 REM --- KLUDGE CHAR SET FOR PDP-10 ---
6010 FOR K1=1 TO O(0)
6020   IF O(K1)>13 GO TO 6040
6030     O(K1)=100+O(K1)
6040 NEXT K1
6050 RETURN
6100 REM ---- UNKLUDGE PDP-10 CHARS -----
6110 FOR K1=1 TO I(0)
6120   IF I(K1)<100 GO TO 6140
6130     I(K1)=I(K1)-100
6140 NEXT K1
6150 RETURN
8500 REM ----- READ .COA HEADER ---------
8510 S1=0
8520 GOSUB 8900
8521 G1$=I$
8530 CHANGE G1$ TO I
8535 GOSUB 6100
8540 G1=I(10)
8550 G2=I(11)
8560 G3=I(12)
8570 G4=I(13)
8580 G5=I(14)
8590 G6=I(15)
8600 G7=I(16)
8601 G8=I(16)
8602 S$=G1$
8604 G2$=MID$(S$,29+8*(G4-G6),8)
8610 SET :4, LOC(4)+1
8690 RETURN
8700 REM ----- READ .COA RECORD -----------
8705 SET :4, LOC(4)+G7
8710 IF LOC(4)<=LOF(4) GO TO 8800
8720   S1=1
8730   GO TO 8890
8800 GOSUB 8900
8830 G$(1)=LEFT$(I$,8)
8840 G$(2)=MID$(I$,10,4)
8850 G$(3)=MID$(I$,14,10)
8860 G$(4)=MID$(I$,24,G2)
8870 GOSUB 8900
8880 G$(5)=I$
8890 RETURN
8900 '   SPECIAL READ WITH RE-TRY
8910 READ :4: I$
8920 SET :4, LOC(4)-1
8930 READ :4: I2$
8940 IF I$=I2$ GO TO 8990
8960   SET :4, LOC(4)-1
8970   GO TO 8910
8990 RETURN
9999 END
    