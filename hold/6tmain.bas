140 DEF FNN                      '  FNN=1 IF R$ IS NUMERIC
142   FNN=0
144   CHANGE R$ TO T
145   IF T(0)=0 GO TO 156
146   FOR T1=1 TO T(0)
148     IF T(T1) < ASC(0) GO TO 156
150     IF T(T1) > ASC(9) GO TO 156
152   NEXT T1
154   FNN=1
156 FNEND
158 '
170 DIM R$(5),H(72),F$(10)
180 '
190 FILE #1, "COMCOM.TMP"
200 MAT READ #1, F$
210 '
220 FILE :1: F$(5)
230 READ :1: I$
240 CHANGE I$ TO H
250 '
260 D5$="*DEL*"
270 '
300 REM ============= ACTION ==============
305 PRINT "ACTION";
310 INPUT R$
320 IF LEN(R$)<3 GO TO 300
330 R$=LEFT$(R$,3)
340 IF R$="ADD" GO TO 500
350 IF R$="CHA" GO TO 600
360 IF R$="DEL" GO TO 700
370 IF R$="PRI" GO TO 800
380   GO TO 300
500 REM --------------- ADD ---------------
510 SET :1, (H(4)+1)*H(9)+1
520 PRINT
525 PRINT "NO.";H(4)+1
530 PRINT
540 GOSUB 3200
550 IF R$="DONE" GO TO 300
560   H(4)=H(4)+1
570   GO TO 520
600 REM ------------- CHANGE --------------
610 GOSUB 3600
620 IF R$="DONE" GO TO 300
630 GOSUB 3200
640 IF R$="DONE" GO TO 300
650   GO TO 600
700 REM ------------- DELETE --------------
710 GOSUB 3600
720 IF R$="DONE" GO TO 300
730 FOR T=1 TO H(9)
740   WRITE :1: D5$
750 NEXT T
760 PRINT "OK"
770   GO TO 710
800 REM ------------- PRINT ---------------
810 GOSUB 3600
820 IF R$="DONE" GO TO 300
830 IF R$<>"ALL" GO TO 950
840 SET :1, 2
850 IF END :1 GO TO 300
860 PRINT
870 PRINT "NO."; LOC(2)/H(9)-1
880 PRINT
890 FOR L=1 TO H(9)
900   READ I$
910   PRINT I$
920 NEXT L
930   GO TO 850
950 PRINT 	'  SINGLE
960 FOR L=1 TO H(9)
970   READ I$
980   PRINT I$
990 NEXT L
1000   GO TO 800
1400 REM ============= DONE ==============
1410 CHANGE H TO O$
1420 SET :1, 1
1430 WRITE :1: O$
1480 '
1490 CHAIN F$(10)+F$(9)
1800 REM ============= SUBS ===============
1810 '
3200 REM --------- INPUT TEXT -------------
3210 PRINT
3220 FOR L=1 TO H(9)
3230   PRINT L;
3240   MAT INPUT R$
3250   R$=R$(1)
3255   FOR T=2 TO NUM
3260     R$=R$+", "+R$(T)
3280   NEXT T
3290   IF R$="DONE" GO TO 3390
3300   O$(L)=R$
3310 NEXT L
3320 FOR L=1 TO H(9)
3330   WRITE :1: O$(L)
3340 NEXT L
3390   RETURN
3600 REM --- GET REC # AND SET ------------
3610 PRINT
3620 PRINT "NO.";
3630 INPUT R$
3640 IF R$="DONE" GO TO 3690
3650 IF R$="ALL"  GO TO 3690
3660 IF FNN=0 GO TO 3610
3670 P1=VAL(R$)
3680 SET :1, P1*H(9)+1
3690   RETURN
  