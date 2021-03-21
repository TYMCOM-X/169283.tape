100 '     S O R T
110 '     -------
112 '
114 '     COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES
120 '
122 '     VERSION 1.1
124 '
130 DIM F$(10)
140 DIM K$(100),R$(100),P(100)
145 '
150 FILE #3, "COMCOM.TMP"
160 FOR X=1 TO 10
170   READ #3, F$(X)
180 NEXT X
190 FILE :1: "WORK3.TMP$"		'  INPUT
200 FILE :2: "WORK1.TMP$132"
205   SCRATCH :2
210 FILE :3: "WORK2.TMP$132"
215   SCRATCH :3
225 REM =========== INITIALIZATION ==========
230 IF F$(2)<>"LAST" GO TO 250
240   S2=1
250 READ :1: R1$
270 B9=100
280 M1=0
290 M2=3
300 M3=2
310 K9$="^^^^"
320 REM ======= SET-UP SORT =========
330 X=1
340 C=0
350 T1=0
360 FOR I=0 TO B9
370   P(I)=0
380 NEXT I
390 REM -------- BEGIN SORT ----------
400 IF END :1 THEN 620
410 READ :1: K$(X),R$(X)
450 IF K$(X)>=K$(C) THEN 510
460 C=P(0)
470 IF K$(X)>=K$(C) THEN 510
480 T1=P(0)
490 C=0
500   GO TO 560
510 T1=P(C)
520 IF T1=0 THEN 560
530 IF K$(T1)>K$(X) THEN 560
540 C=T1
550   GO TO 510
560 P(X)=T1
570 P(C)=X
580 C=X
590 X=X+1
600 IF X<=B9 THEN 400
610   GO TO 640
620 REM ========== MERGE ============
630 F$="EOF"
640 IF X=1 THEN 1090
650 X=P(0)
660 T1=M2
670 M2=M3
680 M3=T1
690 SET :2, 1
700 SET :3, 1
780 W$=K9$
790 C$=K$(X)
800 IF M1=0 THEN 820
810 GOSUB 1700			'  READ
820 IF C$>W$ THEN 850
830 L$=C$
840   GO TO 860
850 L$=W$
860 IF L$=K9$ THEN 1060
870 REM ----- TAKE FROM WORKFILE --------
880 IF W$>L$ THEN 950
890 ON M3 GO TO 9000,900,910
900 WRITE :2: W$,I$
905   GO TO 920
910 WRITE :3: W$,I$
920 GOSUB 1700                  '  READ
930 IF W$<K9$ THEN 870
940   GO TO 820
950 REM ----- TAKE FROM CORE ------------
960 IF C$>L$ THEN 820
970 ON M3 GO TO 9000,980,990
980 WRITE :2: C$,R$(X)
985   GO TO 1000
990 WRITE :3: C$,R$(X)
1000 IF P(X)>0 THEN 1030
1010 C$=K9$
1020   GO TO 820
1030 X=P(X)
1040 C$=K$(X)
1050   GO TO 950
1060 REM ---- MERGE PASS DONE ---------
1062 S1$=K9$
1064 S2$="END"
1066 GOSUB 1500
1070 M1=M1+1
1080 IF F$<>"EOF" GO TO 320
1090 REM ========= WRAP-UP ===========
1091 FILE :1: F$(7)		'  OUTPUT
1115   SCRATCH :1
1120 WRITE :1: R1$
1130 SET :2, 1
1140 SET :3, 1
1150 M2=M3
1160 GOSUB 1700			'  READ
1170 X=1
1180 K$(X)=W$
1190 R$(X)=I$
1200 Y=X+1
1210 IF Y=2 GO TO 1230
1220   Y=1
1230 ON M2 GO TO 9000,1240,1270
1240 IF END :2 GO TO 1400
1250 READ :2: K$(Y),R$(Y)
1260   GO TO 1290
1270 IF END :3 GO TO 1400
1280 READ :3: K$(Y),R$(Y)
1290 IF S2=0 GO TO 1310
1300   IF K$(X)=K$(Y) GO TO 1320
1310 WRITE :1: R$(X)
1320 X=Y
1330   GO TO 1200
1400 REM ============ TERMINATE ==============
1450 CHAIN F$(10)+F$(9)
1500 REM ---------- WRITE WORKFILE ------------
1510 ON M3 GO TO 9000,1560,1600
1560 WRITE :2: S1$,S2$
1590   GO TO 1690
1600 WRITE :3: S1$,S2$
1690 RETURN
1700 REM ---------- READ WORKFILE ------------
1710 ON M2 GO TO 9000,1760,1800
1760 IF END :2 GO TO 1870
1770 READ :2: W$,I$
1790   GO TO 1890
1800 IF END :3 GO TO 1870
1810 READ :3: W$,I$
1830   GO TO 1890
1870 PRINT 1870;M2
1872 PRINT W$,I$
1875   GO TO 9000
1890 RETURN
9000 REM ========== SYSTEM ERROR ===========
9010 PRINT "* PROGRAM ERROR * SORT *"
9020   STOP
9999 END
  