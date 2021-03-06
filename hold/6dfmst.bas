110 '	D E F M S T
120 '
130 '   COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES
135 '
140 '   VERSION 1.0
150 '
160 DIM E(16,15),M$(60),G(72)
170 DIM F(16,15),N$(60),H(72)
180 DIM I(132),O(132)
190 DIM F$(50),T$(4)
200 '
230 FILE #3, "COMCOM.TMP"
240 FOR I=1 TO 50
250   READ #3, F$(I)
252   IF I<21 GO TO 260
254   IF F$(I)="*" GO TO 280
260 NEXT I
270 '
280 FILE :2: F$(3)		'  .MST
290 '
292 J9=20
294 MAT READ T$
300 P9=1
310 GOSUB 7500			'  READ MERGE BLOCK (.MST)
320 GOSUB 6500			'  DISPLAY STRUCTURE
330 L2=LOC(2)
340 REM ------------ NEXT JOURNAL ------------
350 F9=0
360 J9=J9+1
370 IF J9>50 GO TO 1400
380 IF F$(J9)="*" GO TO 1400
390 F1$ = F$(J9) + ".STR$"
410 FILE :1: F1$		'  .STR
420 SET :2, L2
430 IF END :2 GO TO 500
440 P9=P9+1
460 GOSUB 7500			'  READ OLD JOURNAL BLOCK  (.MST)
470 IF F$(J9)<>LEFT$(N$,5) GO TO 430
480   PRINT N$(0);" ALREADY ON LEDGER - CAN'T CONTINUE"
490   GO TO 1380
500 '   NEW JOURNAL
510 GOSUB 8000			'  READ .STR
520 GOSUB 6500			'  DISPLAY STRUCTURE
522 FOR J2=1 TO O9
524   F(14,J2)=0
526 NEXT J2
530 REM ------- HANDLE IDENTICAL FIELDS ----------
540 PRINT "ALL JOURNAL FIELDS WITH THE SAME NAME AND TYPE AS A"
550 PRINT "LEDGER FIELD MAY BE PLACED ON THE LEDGER AUTOMATICALLY."
560 Q$="HANDLE IDENTICAL FIELD DEFINITION AUTOMATICALLY"
570 GOSUB 2100
580 IF R$="YES" GO TO 640
590   J1=G(10)		'  ACCOUNT REQUIRED
595   J2=H(10)
600   GOSUB 1020
610   J1=G(13)		'  AMOUNT REQUIRED
615   J2=H(13)
620   GOSUB 1020
630     GO TO 730
640 FOR J1=1 TO O9
650 IF E(6,J1)=0 GO TO 720
660   FOR J2=1 TO O9
670   IF F(6,J2)=0 GO TO 710
680     IF E(5,J1)<>F(5,J2) GO TO 710
690     IF M$(J1) <>N$(J2)  GO TO 710
700       GOSUB 1020		' RECONCILE
710   NEXT J2
720 NEXT J1
730 REM ------ HANDLE NON-IDENTICAL FIELDS -------
735 IF F9=G(3) GO TO 1220
740 IF F9=H(3) GO TO 835
745 Q$="SHOULD ANY MORE JOURNAL FIELDS APPEAR ON THE LEDGER"
750   GOSUB 2100
755   IF R$="NO" GO TO 835
760 PRINT
765 GOSUB 4200			'  WHICH JOURNAL FIELD
770   IF E1>0 GO TO 745
775 GOSUB 4400			'  WHICH LEDGER FIELD
780   IF E1>0 GO TO 745
785 IF E(5,J1)=F(5,J2) GO TO 800
790 IF E(5,J1)<>2 GO TO 810
795 IF E(5,J1)=4  GO TO 810
800 GOSUB 1020			'  RECONCILE
805   GO TO 730
810 '  MISMATCH
815 PRINT
820 PRINT T$(F(5,J2));" FIELD CAN'T GO INTO ";T$(E(5,J1));" FIELD"
825   GO TO 730
830 '
835 REM ------- HANDLE "CONSTANT" FIELD --------
838 IF F9=G(3) GO TO 1220
840 Q$="SHOULD ANY 'CONSTANT' FIELDS APPEAR ON LEDGER"
845 GOSUB 2100
850 IF R$="NO" GO TO 1220
855 GOSUB 4400			'  WHICH LEDGER FIELD
860   IF E1>0 GO TO 840
865 H8=H(8)+1
870 FOR T=1 TO 16
875   F(T,H8)=E(T,J1)
880 NEXT T
885 PRINT "VALUE";
890 INPUT R$
895 CHANGE R$ TO T
900 GOSUB 3000
905   IF E1>0 GO TO 885
910 CHANGE T TO N$(H8)
915 H(8)=H8
920 H(3)=H(3)+1
922 F(1,H8)=H(3)
924 F(2,H8)=H(8)
925 F(5,H8)=9
930 F(7,H8)=F(6,H8)
935 F(8,H8)=F(6,H8)
940 FOR J=1 TO 3
945   N$(H8+J*O9)=SPACE$(F(6,H8))
950 NEXT J
955 J2=H8
960 GOSUB 1020			'  RECONCILE
965   GO TO 835
1020 REM ------ RECONCILE ---------
1030 IF F(8,J2)<=E(8,J1) GO TO 1160
1040 PRINT
1050 PRINT "NOT ENOUGH ROOM ON LEDGER"
1060 PRINT E(8,J1);" CHARACTERS IN '";M$(J1);"' ON LEDGER"
1070 PRINT F(8,J2);" CHARACTERS IN '";N$(J2);"' ON JOURNAL"
1080 IF F(5,J2)<>2 GO TO 1120
1090   Q$="USE EVEN THOUGH INFORMATION MAY BE LOST"
1100   GOSUB 2100
1110   IF R$="YES" GO TO 1160
1120 Q$ = "CONTINUE WITHOUT " + N$(J2)
1130 GOSUB 2100
1140 IF R$="NO" GO TO 1380
1150   GO TO 1210
1160 F(14,J2)=J1		'  OK - MARK WITH MERGE FIELD NO.
1170 E(14,J1)=E(14,J1)+1	'  AND MARK MERGE FIELD PRESENT
1180 F9=F9+1
1190 PRINT
1200 PRINT "* '";N$(J2);"' WILL APPEAR";
1202 IF N$(J2)=M$(J1) GO TO 1208
1206   PRINT " AS ";M$(0);" '";M$(J1);"'";
1208 PRINT
1210 RETURN
1220 REM ------- CLEAN-UP AND WRITE .MST ------
1230 '
1240 '	MARK MERGE FIELD 1 IF ALWAYS PRESENT, 0 OTHERWISE
1250 FOR J1=1 TO O9
1260   IF E(14,J1)<2 GO TO 1290
1270     E(14,J1)=1
1280     GO TO 1300
1290   E(14,J1)=0
1300 NEXT J1
1320 P9=99
1325 SET :2, LOF(2)+1
1330 GOSUB 7000			'  WRITE JOURNAL BLOCK
1340 P9=1
1350 SET :2,1
1360 GOSUB 7000			'  WRITE MERGE BLOCK
1370   GO TO 340
1380 REM ------- ABNORMAL TERMINATION ---------
1390 PRINT "ABNORMAL TERMINATION"
1400 REM -------- NORMAL TERMINATION ----------
1410 CHAIN F$(10)+F$(9)
2100 REM ------ YES/NO -------
2110 PRINT
2120 PRINT Q$;
2130 INPUT R$
2132   IF R$<>"Y" GO TO 2136
2134     R$="YES"
2136   IF R$<>"N" GO TO 2140
2138     R$="NO"
2140   IF R$="YES" GO TO 2180
2150   IF R$="NO" GO TO 2180
2160 PRINT "PLEASE ANSWER 'YES' OR 'NO'"
2170   GO TO 2100
2180 RETURN
3000 REM ------ SPECIAL VERSION OF EDIT --------
3005 E1=0
3010 E1$="-"
3020 L6=F(6,H8)
3025 IF F(5,H8)<>3 GO TO 3035
3030   L6=8
3035 '     - $ FIELD -
3040 IF F(5,H8)<>4 GO TO 3150
3045 IF T(T(0))=ASC(-) GO TO 3070
3050 IF T(T(0))=ASC(+) GO TO 3065
3055 IF T(T(0))=32     GO TO 3065
3060   T(0)=T(0)+1
3065   T(T(0))=32
3070 IF T(0)<4 GO TO 3140
3075 IF T(T(0)-3)<>ASC(.) GO TO 3140
3080   T(T(0)-3)=ASC(,)
3085 K2=0
3090 FOR K1=1 TO T(0)
3095   IF T(K1)=ASC(,) GO TO 3110
3100   K2=K2+1
3105   T(K2)=T(K1)
3110 NEXT K1
3115 T(0)=K2
3120   GO TO 3150
3140 E1$="NO DECIMAL"
3145   GO TO 3520
3150 '     - LENGTH -
3155 IF F(5,H8)=3 GO TO 3185
3160 IF T(0)>L6 GO TO 3175
3165 IF T(0)<F(9,H8) GO TO 3175
3170   GO TO 3185
3175 E1$="INCORRECT LENGTH"
3180   GO TO 3520
3185 '     - FILL -
3190 IF T(0)>=L6 GO TO 3270
3195   D=L6-T(0)
3200   IF F(5,H8)=2 GO TO 3245
3205 '    "0"
3210 FOR K1=L6 TO 1 STEP -1
3215   IF K1<=D GO TO 3230
3220   T(K1)=T(K1-D)
3225     GO TO 3235
3230   T(K1)=ASC(0)
3235 NEXT K1
3240 GO TO 3265
3245 '    " "
3250 FOR K1=T(0)+1 TO L6
3255   T(K1)=32
3260 NEXT K1
3265 T(0)=L6
3270 '     - DATE -
3275 IF F(5,H8)<>3 GO TO 3380
3280 IF T(6)<>ASC(-) GO TO 3370
3285 IF T(3)= ASC(-) GO TO 3315
3290 IF T(4)<>ASC(-) GO TO 3370
3295   FOR K1=1 TO 3
3300     T(K1)=T(K1+1)
3305   NEXT K1
3310   T(4)=ASC(0)
3315 IF T(1)> ASC(1) GO TO 3520
3320 IF T(4)> ASC(3) GO TO 3520
3325 IF T(7)<>ASC(7) GO TO 3520
3330 T(6)=T(5)
3335 T(5)=T(4)
3340 T(4)=T(2)
3345 T(3)=T(1)
3350 T(1)=T(7)
3355 T(2)=T(8)
3360 T(0)=6
3365   GO TO 3380
3370 E1$="USE FORM 'MM-DD-YY'"
3375   GO TO 3520
3380 '     - NUMERIC -
3385 IF F(5,H8)=2 GO TO 3440
3390 T1=T(0)
3395 IF F(5,H8)<>4 GO TO 3405
3400   T1=T1-1
3405 FOR K1=1 TO T1
3410   IF T(K1)>ASC(9) GO TO 3430
3415   IF T(K1)<ASC(0) GO TO 3430
3420 NEXT K1
3425   GO TO 3440
3430 E1$="NON-NUMERIC CHARACTER"
3435   GO TO 3520
3440 '     - OK -
3450   GO TO 3590
3515 '
3520 '     - ERROR -
3525 PRINT TAB(2);N$(H8);" IN ERROR - ";E1$;" - RE-TYPE"
3530 E1=1
3590   RETURN
4200 REM -------- WHICH JOURNAL FIELD ----------
4210 E1=0
4220 PRINT "WHICH JOURNAL FIELD";
4230   INPUT R$
4240   FOR J2=1 TO O9
4250     IF R$=N$(J2) GO TO 4290
4260   NEXT J2
4270   PRINT R$;" NOT ON JOURNAL"
4280     GO TO 4310
4290 IF F(14,J2)=0 GO TO 4320
4300   PRINT "ALREADY INCLUDED"
4310   E1=1
4320 RETURN
4400 REM -------- WHICH LEDGER FIELD -----------
4410 E1=0
4420 PRINT "AS WHICH LEDGER FIELD";
4430   INPUT R$
4440   FOR J1=1 TO O9
4450     IF R$=M$(J1) GO TO 4490
4460   NEXT J1
4470   PRINT R$;" NOT ON LEDGER"
4480     GO TO 4510
4490 IF E(14,J1)<>2 GO TO 4520
4500   PRINT "ALREADY DEFINED"
4510   E1=1
4520 RETURN
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
6500 REM --- DISPLAY STRUCTURE -----
6530 PRINT
6540 PRINT N$(0);" FORMAT IS:"
6550 PRINT
6560 PRINT TAB(5);"FIELD NAME";TAB(25);"TYPE";TAB(35);"LENGTH"
6570 PRINT
6580 FOR I=1 TO O9
6590   IF F(6,I)=0 GO TO 6610
6600     PRINT TAB(5);N$(I);TAB(25);T$(F(5,I));TAB(35);F(8,I)
6610 NEXT I
6620 PRINT
6630 RETURN
7000 REM --- WRITE STRUCTURE BLOCK ------
7010 IF P9<>1 GO TO 7110
7015 G(2)=2
7020 FOR Y=0 TO 72
7030   H(Y)=G(Y)
7040 NEXT Y
7050 FOR Y=0 TO O9
7055   FOR X=0 TO 3
7060     N$(Y + X*O9) = M$(Y + X*O9)
7065   NEXT X
7070   FOR X=0 TO 16
7080     F(X,Y)=E(X,Y)
7090   NEXT X
7100 NEXT Y
7110 '	HEADER
7120 O(0)=33
7130 FOR X=1 TO O(0)
7140   O(X)=H(X)
7150 NEXT X
7160 GOSUB 6000
7165 CHANGE O TO O$
7170 O$=O$+N$(0)
7180 WRITE :2: O$
7190 '	FIELD RECORDS
7200 O(0)=16
7210 FOR Y=1 TO O9
7220   IF F(6,Y)=0 GO TO 7310
7230   FOR X=1 TO 16
7240     O(X)=F(X,Y)
7250   NEXT X
7260   GOSUB 6000
7265   CHANGE O TO O$
7270   FOR X=0 TO 3
7280     O$ = O$ + N$(Y + X*O9)
7290   NEXT X
7300   WRITE :2: O$
7310 NEXT Y
7320 RETURN
7500 REM ------ READ LEDGER STRUCTURE - RLST -----
7510 '
7520 '	P9 = "STRUCTURE BLOCK" NUMBER.
7530 '	S2   SWITCH FOR READING .STR (1) OR .MST (2)
7540 '
7550 S2=2
7560 GOSUB 8000			'  USE RSTR
7570 S2=1
7600 IF P9<>1 GO TO 7700
7610 FOR Y=0 TO 72
7620   G(Y)=H(Y)
7630 NEXT Y
7640 FOR Y=0 TO O9
7645   FOR X=0 TO 3
7650     M$(Y + X*O9) = N$(Y + X*O9)
7655   NEXT X
7660   FOR X=0 TO 16
7670     E(X,Y)=F(X,Y)
7680   NEXT X
7690 NEXT Y
7700 RETURN
7800 REM --- SPECIAL INIT ROUTINE FOR STRUCTURE DATA ---
7810 FOR Y=0 TO 72
7820   H(Y)=0
7830 NEXT Y
7840 FOR Y=0 TO O9
7850   N$(Y)=""
7860   FOR X=0 TO 16
7870     F(X,Y)=0
7880   NEXT X
7890 NEXT Y
7895 RETURN
7900 REM --- SPECIAL READ - .STR OR .MST ----
7910 IF S2=2 GO TO 7940
7920 READ :1: I$
7930   GO TO 7950
7940 READ :2: I$		  '  READ .MST
7950 RETURN
8000 REM ---READ STRUCTURE FILE---
8004 GOSUB 7800			'  INIT. STRUCTURE DATA
8005 O9=15
8010 GOSUB 7900			'  SPECIAL READ
8015 CHANGE I$ TO I
8020 GOSUB 6100
8025 FOR Y=0 TO I(0)
8030   H(Y)=I(Y)
8035 NEXT Y
8038 N$=MID$(I$,20,13)
8040 N$(0)=MID$(I$,34,H(33))
8045 FOR T2=1 TO H(3)
8050   GOSUB 7900		'  SPECIAL READ
8055   CHANGE I$ TO I
8060   GOSUB 6100
8065   T3=I(2)
8070   FOR T1=1 TO 16
8075     F(T1,T3)=I(T1)
8080   NEXT T1
8085   T4=17
8090   N$(T3)=MID$(I$,T4,I(7))
8095   T4=T4+I(7)
8100   N$(T3+1*O9)=MID$(I$,T4,I(8))
8105   T4=T4+I(8)
8110   N$(T3+2*O9)=MID$(I$,T4,I(8))
8115   T4=T4+I(8)
8120   N$(T3+3*O9)=MID$(I$,T4,I(8))
8125 NEXT T2
8255 F(13,0)=1
8260 F(6,0)=2
8265 RETURN
9800 DATA NUMERIC,TEXT,DATE,DOLLAR
9999 END
   