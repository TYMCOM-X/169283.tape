100 '     D C O A
110 '
120 '     COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES
130 '
140 '     VERSION 1.1
150 '
160 DIM I(132),T(50)
170 DIM F$(10)
190 '
200 FILE #1, "COMCOM.TMP"
210 MAT READ #1, F$
220 '
300 FILE :4: F$(5)
310   GOSUB 8500
315 T2=I(0)		'  LENGTH - FROM READ COA ROUTINE * *
320 F$=LEFT$(F$(5),5)+".TMP$"+STR$(T2)
330 FILE :1: F$
340 SCRATCH :1
350 SET :4, 1
360 FOR J=1 TO LOF(4)
370   READ  :4: I$
380   WRITE :1: I$
390 NEXT J
400 '
410 D$="DEL"
420 P1=9-G1
450 PRINT "=== CHART OF ACCOUNTS DELETIONS - ";F$(1);" ==="
460 PRINT
470 PRINT "GIVE ACCT. NUMBERS TO BE DELETED, TYPE 'DONE' WHEN THROUGH"
490 '
500 PRINT
510 PRINT "ACCT NO.";
520 INPUT A$
522 IF A$="DONE" GO TO 1200
524 IF A$="STOP" GO TO 1290
530 GOSUB 3100
540 SET :1, 2
550 '
560 SET :1, LOC(1)+1+G7
570 IF END :1 GO TO 1000
580 READ :1: I$
590 IF MID$(I$,P1,G1)<>A$ GO TO 560
690 CHANGE I$ TO I
700 IF I(9)<>ASC(A) GO TO 9000
710 FOR J=G2 TO 5 STEP -1
720   IF I(23+J)<>ASC(SP) GO TO 740
730 NEXT J
740 N$=MID$(I$,24,J)
750 PRINT N$;", OK";
760 INPUT R$
770 IF R$="OK"  GO TO 820
780 IF R$="Y"   GO TO 820
790 IF R$="YES" GO TO 820
800 PRINT "NOT DELETING ";A$;", ";N$
810   GO TO 500
820 SET :1, LOC(1)-1
830 FOR J=1 TO 2+G7
840   WRITE :1: D$
850 NEXT J
860   GO TO 500
990 '
1000 PRINT "CAN'T FIND ";A$
1010   GO TO 500
1200 REM ========== WRAP-UP =========
1210 SET :1, 1
1220 SCRATCH :4
1230 FOR J=1 TO LOF(1)
1240   READ :1: I$
1250   IF I$=D$ GO TO 1270
1260     WRITE :4: I$
1270 NEXT J
1290 CHAIN F$(10)+F$(9)
3100 REM ---------- EDIT ACCT NO. ----------
3110 CHANGE A$ TO T
3120 IF T(0)>=G1 GO TO 3290
3130 K2=T(0)
3140 FOR K1=G1 TO 1 STEP -1
3150   IF K2=0 GO TO 3190
3160   T(K1)=T(K2)
3170   K2=K2-1
3180     GO TO 3200
3190   T(K1)=ASC(0)
3200 NEXT K1
3210 T(0)=G1
3220 CHANGE T TO A$
3290 RETURN
6100 REM ---- UNKLUDGE PDP-10 CHARS -----
6110 FOR K1=1 TO I(0)
6120   IF I(K1)<100 GO TO 6140
6130     I(K1)=I(K1)-100
6140 NEXT K1
6150 RETURN
8500 REM ----- READ .COA HEADER ---------
8520 READ :4: G1$
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
8690 RETURN
9000 REM ========== ERROR ==============
9001 PRINT LOC(1);I$
9010 PRINT "*PROGRAM ERROR * DCOA *"
9020 STOP
9999 END
    