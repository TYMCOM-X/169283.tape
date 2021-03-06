100 '     Q S O R T
105 '     ---------
110 '
115 '     COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES
120 '
125 '     QSORT6 - VERSION 1.0
130 '
135 DIM H(72)
140 DIM K(15),L(15)
145 DIM I(132),P(100),K$(100)	'***BUFFERS***
150 DIM F(2,15),T(132)
155 DIM F$(50)
160 '
165 B9=50		'  * * BUFFER SIZE * *
170 K8=0		'REAL COUNT OF KEYS
175 K$(0)=CHR$(31)
180 K1=1
185 K9=0
190 E9=0
195 R9=0			'COUNT OF RECORDS
200 '
205 FILE #3, "COMCOM.TMP"
210 FOR K=1 TO 50
215   READ #3, F$(K)
220   IF K<21 GO TO 230
225     IF F$(K)="*" THEN 235
230 NEXT K
235 FILE :1: F$(4)		'INPUT FILE
240 READ :1: I$			'READ HEADER
245 L2=LEN(I$)
250 K=K-21
255 '
260 REM ====== KEY IS ACCOUNT NUMBER (COA) ======
265 IF F$(21)<>"COA" THEN 315
270 CHANGE I$ TO I
275 GOSUB 6100
280 K(1)=9-I(10)
285 L(1)=I(10)
290 R1=I(16)+2			'2+VAR.
295 K=1
300 K9=L(1)
305   GO TO 465
310 REM ====== KEYS ARE ACTUAL CHAR POSITIONS ======
315 IF F$(21)<> "POINTERS" THEN 370
320 FOR X=1 TO (K-1)/2		'ONCE FOR EACH PAIR
325   K(X)=VAL(F$(20+2*X))		'STARTING CHAR POSITION
330   L(X)=VAL(F$(21+2*X))		'LENGTH
335   K9=K9+L(X)
340 NEXT X
345 K=(K-1)/2
350 R1=1			'ASSUMPTION
355   GO TO 465
360 '
365 REM ====== KEYS ARE FIELD NAMES ======
370 FILE :3: F$(3)
375 GOSUB 8000			'  READ .STR
380 FOR X=21 TO 20+K
385   FOR Y=1 TO O9
390     IF F(0,Y)=0 THEN 400
395     IF F$(X)=K$(Y) THEN 425
400   NEXT Y
405   IF F$(2)="OK" GO TO 445
410     PRINT "* PROGRAM ERROR * SORT CALL *"
415     PRINT "  NON-EXISTANT FIELD - ";F$(X)
420       STOP
425   K(X-20)=F(1,Y)
430   L(X-20)=F(2,Y)
435   K9=K9+F(2,Y)
440   K8=K8+1
445 NEXT X
450 K=K8
455 R1=1			'ASSUMPTION
460 '
465 REM --------- SET UP FILES ------------
470 L2$=STR$(L2)
475 K9$=STR$(K9+6)
480 '			 NAME THE WORK FILES
485 W1$="W1" + K9$ + ".TMP$" + K9$
490 W4$="W4"+L2$+".TMP$"+L2$
495 FILE :2: W1$		'OUTPUT
500   SCRATCH :2
505 '			SET SWITCHES
510 IF F$(2)<>"LAST" THEN 520
515 S2=1
520 IF F$(4)<>F$(7) THEN 535
525 S1=1
530 FILE :4: W4$
535 SET :1, 1
540 FOR X=1 TO R1
545   READ :1: I$
550   IF S1<>1 THEN 560
555   WRITE :4: I$
560 NEXT X
565 REM =========== COPY FILE ===========
570 P1=LOC(1)
575 '			READ LOOP
580 X=0
585 '
590 SET :1,P1
595 IF END :1 THEN 765
600 READ :1: I$
605 IF I$="EOF" THEN 760
610 X=X+1
615 R9=R9+1
620 T1=1
625 CHANGE I$ TO T
630 FOR T2=1 TO K
635   FOR T3=K(T2) TO K(T2)+L(T2)-1
640     P(T1)=T(T3)
645     T1=T1+1
650   NEXT T3
655 NEXT T2
660 P(0)=K9
665 CHANGE P TO K$(X)
670 I(X)=P1
675 IF S1=0 THEN 710
680 T1=1			'COPY INPUT TO TEMP FILE
685 WRITE :4: I$
690 T1=T1+1
695 IF T1>R1 THEN 710
700 READ :1: I$
705   GO TO 685
710 P1=P1+R1			'NEXT LOGICAL RECORD
715 IF X<B9 THEN 590
720 T1=LOC(1)			'PEEK AT NEXT RECORD
725 SET :1,P1
730 IF END :1 THEN 765
735 READ :1: I$
740 IF I$="EOF" THEN 760
745 SET :1,T1
750 S1$="MORE"			'MORE TO SORT - JUST DO IT
755   GO TO 780
760 E9=LOC(1)-1
765 S1$="LAST"			'LAST PASS
770 IF K1>1 THEN 780
775 S1$="CORE"			'ONLY 1 BUFFER - SORT FROM CORE
780 REM------SORT ONE BUFFER------
785 '
790 '			SET-UP
795 X1=X
800 X=1
805 C=0
810 T1=0
815 FOR I=0 TO B9
820   P(I)=0
825 NEXT I
830 '			SORT
835 IF K$(X)>=K$(C) THEN 865
840 C=P(0)
845 IF K$(X)>=K$(C) THEN 865
850 T1=P(0)
855 C=0
860   GO TO 890
865 T1=P(C)
870 IF T1=0 THEN 890
875 IF K$(T1)>K$(X) THEN 890
880 C=T1
885   GO TO 865
890 P(X)=T1
895 P(C)=X
900 C=X
905 X=X+1
910 IF X<=X1 THEN 835
915 REM------EMPTY THE BUFFER------
920 IF S1$="CORE" THEN 965
925 '			COPY BUFFER TO WORK FILE
930 X=P(0)
935 T$="00000" + STR$(I(X))	'BUILD POINTER
940 P$=RIGHT$(T$,6)
945 O$=K$(X) + P$		'OUTPUT IS KEY AND POINTER
950 WRITE :2: O$
955 X=P(X)
960 IF X>0 THEN 935
965 IF S1$<> "MORE" THEN 990
970 K1=K1+1
975   GO TO 580'GO GET ANOTHER BUFFER
980 REM======SORT DONE - COPY REAL FILE======
985 '
990 IF S1=0 THEN 1005
995 FILE #4, "DUMMY.TMP"
1000 FILE :1: W4$
1005 FILE :3: F$(7)
1010 SCRATCH :3
1015 SET :1,1		'COPY HEADER
1020 FOR I=1 TO R1
1025 READ :1: I$
1030 WRITE :3: I$
1035 NEXT I
1040 IF S1$<> "CORE" THEN 1165
1045 REM		USE KEYS DIRECTLY FROM CORE
1050 IF S2>0 THEN 1100
1055 '			NO IDENTICAL KEY
1060 C=P(0)
1065 IF C=0 THEN 1360'EOF
1070 P1=P(C)
1075 P(C)=I(C)
1080 GOSUB 3000
1085 C=P1
1090   GO TO 1065
1095 '			IDENTICAL KEY CODING
1100 C=0
1105 K$(0)=CHR$(122)		'HIGH VALUE
1110 C=P(C)
1115 N=P(C)
1120 IF K$(C)<>K$(N) THEN 1140
1125 GOSUB 2010			'IDENTICAL KEYS
1130   GO TO 1110
1135 P1=P(C)
1140 P(C)=I(C)
1145 GOSUB 2060
1150 C=P1
1155 IF N>0 THEN 1115
1160   GO TO 1360'EOF
1165 REM -------- USE KEYS FROM WORKFILE ------
1170 WRITE :2: CHR$(122)
1175 FOR X=1 TO K1		'INITIALIZE
1180   I(X)=1
1185   SET :2, ((X-1)*B9)+1
1190   READ :2: W$
1195   GOSUB 3200			'EXTRACT
1200 NEXT X
1205 '			FIND LOW KEY
1210 L$=CHR$(122)
1215 FOR X=1 TO K1
1220   IF I(X)>B9 THEN 1235
1225   IF K$(X)>L$ THEN 1235
1230   L$=K$(X)
1235 NEXT X
1240 IF L$=CHR$(122) THEN 1360'EOF
1245 '
1250 FOR X=1 TO K1
1255   IF I(X)>B9 THEN 1340
1260   IF K$(X)>L$ THEN 1340
1265   C=X
1270 IF S2=0 THEN 1285
1275   GOSUB 2010		'MATCH
1280     GO TO 1290
1285   GOSUB 3000		'COPY
1290   I(X)=I(X)+1
1295   IF I(X)>B9 THEN 1330
1300   B8=(X-1)*B9
1305   IF I(X)+B8>R9 THEN 1330
1310   SET :2, I(X)+B8
1315   READ :2: W$
1320   GOSUB 3200			'EXTRACT
1325     GO TO 1255
1330   K$(X)=CHR$(122)
1335   I(X)=B9+1
1340 NEXT X
1345 IF S2=0 THEN 1355
1350 GOSUB 2060			'BREAK
1355   GO TO 1210
1360 REM ======EOF======
1365 IF E9=0 THEN 1380
1370 SET :3,E9
1375 WRITE :3: "EOF"
1380 CHAIN F$(10) + F$(9)
2000 REM======SUBROUTINES======
2001 REM------IDENTICAL KEY CODING------
2004 '			MATCH
2010 V1=1			'***DO NOTHING FOR NORMAL CASE***
2048   RETURN
2050 '			BREAK
2060 GOSUB 3000
2098   RETURN
2099 '
3000 REM------COPY RECORD(S)------
3010 T1=0
3020 SET :1, P(C)
3030 READ :1: I$
3040 WRITE :3: I$
3050 T1=T1+1
3060 IF T1<R1 THEN 3030
3070   RETURN
3200 REM------EXTRACT KEY AND POINTER------
3210 T$=RIGHT$(W$,6)
3220 P(X)=VAL(T$)
3230 K$(X)=LEFT$(W$,K9)
3240   RETURN
6100 REM ---- UNKLUDGE PDP-10 CHARS -----
6110 FOR T1=1 TO I(0)
6120   IF I(T1)<100 THEN 6140
6130   I(T1)=I(T1)-100
6140 NEXT T1
6150 RETURN
8000 REM ---READ STRUCTURE FILE---
8005 O9=15
8010 READ :3: I$
8015 CHANGE I$ TO I
8020 GOSUB 6100
8025 H3=I(3)
8030 R1=I(9)
8045 FOR T2=1 TO H3
8050   READ :3: I$
8055   CHANGE I$ TO I
8060   GOSUB 6100
8065   T3=I(2)
8070   F(0,T3)=I(2)
8075   F(1,T3)=I(13)
8080   F(2,T3)=I(6)
8085   T$=LEFT$(I$,16+I(7))
8090   K$(T3)=RIGHT$(T$,I(7))
8095 NEXT T2
8265 RETURN
9999 END
    