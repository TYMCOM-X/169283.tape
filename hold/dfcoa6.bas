100 '     D E F C O A
105 '     -----------
110 '
115 '     COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES
120 '
125 '     VERSION 1.2
130 '
135 DIM F$(10),C$(5),R$(4)
140 DIM I(132),O(132),T(132)
145 '
150 FILE #1, "COMCOM.TMP"
155 MAT READ #1, F$
160 '
175 FOR K1=1 TO 132
180   O(K1)=ASC(0)
185 NEXT K1
190 MAT READ C$(5)
195 '
205 IF F$(2)<>"RRUN" GO TO 295
210 '
215 PRINT "=== CHART OF ACCOUNTS ADDITIONS - ";F$(1);" ==="
220 PRINT
225 PRINT "NOTE - ACCOUNT BALANCES MAY NEED RE-COMPUTING"
230 FILE :4: F$(5)
235 READ :4: I$			'  READ .COA HEADER
240 CHANGE I$ TO I
245 GOSUB 6100
250 FOR T=1 TO 8
255   Q(T)=I(9+T)
260 NEXT T
265 O(0)=I(0)		'  SAME LENGTH AS BEFORE
266 L4=LOF(4)
267 T2=2+Q(7)
268 IF L4=INT(L4/T2)*T2 GO TO 274
270   PRINT "POSSIBLE FILE PROBLEMS - RECOVERY PROCEDURE USED"
271   PRINT "PROCESSING WILL PROCEED AS NORMAL"
272   L4=INT(L4/T2)*T2
274 SET :4, L4+1
276 PRINT
280 PRINT "GIVE DETAIL INFORMATION FOR EACH ACCOUNT"
285   GO TO 645
290 '
295 PRINT "=== CHART OF ACCOUNTS DEFINITION - ";F$(1);" ==="
300 PRINT
305 PRINT "1) WHICH METHOD OF REPORTING DO YOU WISH TO USE,"
310 PRINT "   CASH OR ACCRUAL";
315 INPUT I$
320 Q(5)=2
325 IF I$="ACCRUAL" GO TO 360
330 IF I$<>"CASH"   GO TO 310
335   Q(5)=1
360 PRINT
365 PRINT "2) HOW MANY DIGITS IN YOUR ACCOUNT NUMBER";
370 INPUT Q(1)
375 IF Q(1)<9 THEN 390
380 PRINT "TOO LARGE - LIMIT IS 8 DIGITS - RETYPE"
385 GO TO 370
390 Q(2)=25
395 Q(6)=0
400 Q(7)=1
405 Q(8)=10
410 PRINT
415 PRINT "3) HOW MANY ACCOUNTING PERIODS IN YOUR YEAR";
420 INPUT Q(3)
425 IF Q(3)<=12 GO TO 440
430 PRINT "MAXIMUM IS 12 - RE-ENTER"
435   GO TO 415
440 Q$="   WILL OPENING BALANCES BE FOR THE 1ST PERIOD"
445 GOSUB 2100
450 IF R$="YES" GO TO 480
455 PRINT "   FOR WHAT PERIOD";
460 INPUT Q(4)
465 IF Q(4)<1    GO TO 455
470 IF Q(4)>Q(3) GO TO 455
475 Q(4)=Q(4)-1			'  REALLY LAST PERIOD'S ENDING
480 PRINT
485 PRINT "4) GIVE CLOSING DATE FOR PERIOD";Q(4)+1;"IN THE"
490 PRINT "   FORM 'MM-DD-YY'";
495 INPUT I$
500 CHANGE I$ TO T
505 GOSUB 3380
510   IF E1=1 GO TO 495
515 V1=29
520 GOSUB 2600
525 PRINT
530 PRINT "5) NOW SUPPLY DETAILED INFORMATION FOR EACH ACCOUNT:"
535 REM =========== WRITE .COA HEADER ============
540 O(0)=10*Q(3)+9
542 IF O(0)>48 GO TO 545
544   O(0)=49		'  MINIMUM LENGTH
545 F$(5)=F$(5)+STR$(O(0))
550 FILE :4: F$(5)
555 IF LOF(4)=0 GO TO 570
560   PRINT "CHART OF ACCOUNTS ALREADY EXISTS"
565   GO TO 9000
570 FOR I=1 TO 8
575   O(9+I)=Q(I)
580 NEXT I
600 O(9)=ASC(A)
605 GOSUB 6000
610 GOSUB 2900
615 '
620 GOSUB 2800
625 O(9)=ASC(B)
630 GOSUB 2900
645 REM =========== DEFINE ACCOUNTS ==========
650 Q1=8-Q(1)+1
655 C9=2
660 IF Q(5)=1 GO TO 670
665   C9=5
670 PRINT
675 PRINT "    -ACCOUNT NUMBER"
680 PRINT "    -NAME OF ACCOUNT"
685 PRINT "    -TYPE OF ACCOUNT";
690 IF Q(5)=2 GO TO 705
695 PRINT " (R=REVENUE, E=EXPENSE)"
700   GO TO 710
705 PRINT " (A=ASSET,L=LIABILITY,R=REVENUE,E=EXPENSE,N=NONE)"
710 PRINT "    -OPENING BALANCE"
715 PRINT
720 PRINT "    BEGIN ENTERING ACCOUNTS---TYPE 'DONE' WHEN FINISHED"
725 REM =========== IND. ACCTS ==============
726 GOSUB 2800
727 N9=0
728 FOR T=1 TO 4
729   R$(T)=""
730 NEXT T
732 PRINT
734 PRINT "ACCT";
735 MAT INPUT R$
740 N9=NUM
745 IF R$(1)="DONE" THEN 1205
750 CHANGE R$(1) TO T
755 IF T(0)=Q(1) THEN 770
760 PRINT "ACCOUNT NUMBER MUST BE";Q(1);"DIGITS LONG"
765   GO TO 725
770 GOSUB 3790
775   IF E1>0 GO TO 725
780   V1=Q1
785   GOSUB 2600
790 REM ----------- NAME -------------
795 IF N9>1 GO TO 810
800 PRINT "NAME";
805 INPUT R$(2)
810 IF LEN(R$(2))=0    GO TO 800
815 IF LEN(R$(2))=Q(2) GO TO 845
820 IF LEN(R$(2))<Q(2) GO TO 840
825 PRINT "NAME MUST BE";Q(2);"OR LESS CHARACTERS"
830   N9=0
835   GO TO 800
840 T=Q(2)-LEN(R$(2))
842 S$=SPACE$(T)
844 R$(2)=R$(2)+S$
845 CHANGE R$(2) TO T
850   V1=24
855   GOSUB 2600
860 REM ----------- TYPE -------------
865 IF N9>2 GO TO 880
870 PRINT "TYPE";
875 INPUT R$(3)
880 FOR J=1 TO C9
885   IF R$(3)=C$(J) GO TO 910
890 NEXT J
895 PRINT "INVALID TYPE"
900   N9=0
905   GO TO 870
910 CHANGE R$(3) TO T
915 O(10)=T(1)
925 REM --------- BALANCE ------------
930 IF N9>3 GO TO 945
935 PRINT "BALANCE";
940 INPUT R$(4)
945 IF R$(4)="0" GO TO 960
950 IF R$(4)=""  GO TO 960
955   GO TO 965
960 R$(4)="0.00"
965 CHANGE R$(4) TO I
970 FOR Z=1 TO 12
975   T(Z)=ASC(0)
980 NEXT Z
985 T1=I(0)
990 T(11)=32
995 IF I(T1)=ASC(+) THEN 1015
1000 IF I(T1)=32 THEN 1015
1005 IF I(T1)<>ASC(-) THEN 1020
1010 T(11)=ASC(-)
1015 T1=T1-1
1020 IF T1>1 THEN 1040
1025 IF I(1)=ASC(0) THEN 1060
1030 PRINT "ERROR---NO DECIMAL POINT"
1035   GO TO 935
1040 IF I(T1-2)<>ASC(.) THEN 1030
1045 I(T1-2)=I(T1-1)
1050 I(T1-1)=I(T1)
1055 T1=T1-1
1060 FOR Z=1 TO T1
1065   IF I(Z)<ASC(0) THEN 1090
1070   IF I(Z)>ASC(9) THEN 1090
1075   T(10-T1+Z)=I(Z)
1080 NEXT Z
1085   GO TO 1100
1090 PRINT "ERROR---BALANCE CONTAINS NON-NUMERIC CHARACTER"
1095   GO TO 935
1100 IF T(11)<>ASC(-) GO TO 1110
1105   T(10)=T(10)+10
1110 T(0)=10
1115 IF Q(4)-Q(6)>0 GO TO 1130
1120   V1=14
1125   GOSUB 2600
1130 REM ------- WRITE .COA RECORD --------
1135 O(9)=ASC(A)
1140 GOSUB 2900
1145 GOSUB 2800
1150 O(9)=ASC(B)
1155 IF Q(4)-Q(6)<1 GO TO 1170
1160   V1=10*(Q(4)-Q(6))		'  AMOUNT STILL IN T
1165   GOSUB 2600		'  GOES INTO 2ND DETAIL
1170 GOSUB 2900
1200   GO TO 725
1205 REM =========== WRAP-UP ===========
1240 L4=LOF(4)
1245 T2=2+Q(7)
1250 IF L4=INT(L4/T2)*T2 GO TO 1275
1260   PRINT "FILE PROBLEMS - DATA MAY BE MISSING"
1265   PRINT "PLEASE CHECK ACCOUNTS & RE-ENTER IF NESCESSARY"
1270   PRINT "PROCESSING WILL PROCEED NORMALLY"
1275 REM ----------- DONE --------------
1500 CHAIN F$(10)+F$(9)
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
2600 REM -------- INSERT ----------
2610 K1=1
2620 FOR K2=V1 TO V1+T(0)-1
2630   O(K2)=T(K1)
2640   K1=K1+1
2650 NEXT K2
2660 RETURN
2800 REM -------- INIT RECORD ------
2810 FOR K1=10 TO 132
2820   O(K1)=ASC(0)
2830 NEXT K1
2840 RETURN
2900 REM ------ WRITE NEW .COA RECORD --------
2901 FOR T8=1 TO O(0)
2902   IF O(T8)<20  GO TO 2906
2903   IF O(T8)>125 GO TO 2906
2904 NEXT T8
2905   GO TO 2915
2906 PRINT "DATA TRANSMISSION ERROR - PLEASE RE-ENTER ACCOUNT"
2912   GO TO 725
2915 CHANGE O TO O$
2918 IF O(9)<ASC(A)      GO TO 2980
2920 IF O(9)>ASC(B)      GO TO 2980
2940 WRITE :4: O$
2950   RETURN
2980 PRINT 2980;O(9);O$
2985   GO TO 9000
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
9000 REM ------------- ERROR -------------
9001 PRINT "* PROGRAM ERROR * DEFCOA *"
9002 STOP
9008 '
9010 DATA R,E,A,L,N
9999 END
  