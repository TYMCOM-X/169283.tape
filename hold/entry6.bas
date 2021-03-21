100 '     E N T R Y
105 '
110 '     COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES
115 '
120 '     ENTRY6 - VERSION 1.1
125 '
130 DIM I(132),O(132),A(15,2),T(50)
135 DIM H(72),F(16,15),N$(15),U$(30),L$(400)
140 DIM R$(15),G$(15),V$(15),Z(15),P(15)
145 DIM F$(10)
150 '
155 DEF FNB(A,P)
160   B=INT(A/(2^(P-1)))
165   FNB=B-INT(B/2)*2
170 FNEND
175 '
180 MAT READ O$(5)
185 D1$="/"
190 D1=ASC(/)
195 '
200 FILE #1, "COMCOM.TMP"
205 MAT READ #1, F$
210 '
215 FILE :1: F$(3)		' STRUCTURE
220   GOSUB 8000
225 FILE :1: F$(4)		'  DATA
230   READ :1: I$
235   CHANGE I$ TO I
240   GOSUB 6100
245   N8=N9=100*I(3)+I(4)+1
250   SET :1, N8+1
255 '
260 FOR I=1 TO H(8)		'  INITIALIZE "STATE" OF FIELD
265   Z(I)=-1
270   IF F(2,I)=0 GO TO 280
275     Z(I)=0
280 NEXT I
285 '
290 FOR I=6 TO 8		'  INPUT OPTIONS FROM ICL
295   IF F$(I)="*" GO TO 325
300   R$=F$(I)
305   S7=1
310   GOSUB 2200
315   GOSUB 2600
320 NEXT I
325 S7=0
330 '
335 O(0)=H(6)
340 FOR J=1 TO O(0)
345   O(J)=ASC(0)
350 NEXT J
355 CHANGE O TO B8$
360 '
365 PRINT
370 PRINT "=== ENTRIES TO ";N$(0);" - ";F$(1);" ==="
375 PRINT
380 '
385 IF F$(2)<>"FILE" GO TO 415
390   PRINT "INPUT FILE (NAME.EXT)";
395   INPUT R$
400   FILE #4, R$
405   S4=1
410 '
415 REM ------------OPTIONS--------------
420 Q$="USE SPECIAL INPUT OPTIONS"
425   GOSUB 2100
430   IF R$="NO" GO TO 455
435 GOSUB 2700                  '  OPTION CONTROL
440 PRINT
445 PRINT "OPTIONS MAY BE CHANGED BY GIVING THE COMMAND 'SET'"
450 PRINT
455 REM -------- FINAL SET-UP --------------
460 GOSUB 2500
465 IF S4=0 GO TO 485
470 PRINT "BEGINNING AT ENTRY ";N9
475 PRINT
480   GO TO 500
485 PRINT "TYPE 'DONE' WHEN YOU HAVE NO MORE ENTRIES"
490 PRINT
495 PRINT
500 REM ======== BEGIN TRANSACTION LOOP ==========
505 S7=0
510 IF Z6=0 GO TO 545
515   A9=1
520   GOSUB 5200		'  READ ACCUM.
525   V3=F(6,Z6)
530   GOSUB 6840
535   T(0)=T(0)-1
540   CHANGE T TO G$(Z6)
545 REM -------------- INPUT ---------------
550 IF S4=0 GO TO 585
555 IF E9>1 GO TO 585
560 '     FILE
565 S5=1
570 IF END #4 GO TO 1030
575 INPUT #4, R$
580   GO TO 615
585 '     TTY
590 S5=0
595 PRINT TAB(1);N9;
600 IF Z6=0 GO TO 610
605   PRINT "(";G$(Z6);")";
610 INPUT R$
615 REM ------------- CONTROL --------------
620 IF R$= "DONE"    GO TO 1030
625 IF R$<>"HELP"    GO TO 640
630   GOSUB 2500
635   GO TO 500
640 IF R$<>"TOTAL"   GO TO 655
645   GOSUB 6500
650   GO TO 500
655 IF R$<>"RESTORE" GO TO 670
660   GOSUB 1500
665   GO TO 500
670 IF R$<>"LATER" GO TO 685
675   E9=0
680   GO TO 500
685 IF R$<>"SET" GO TO 720
690   IF S4=0 GO TO 705
695     PRINT
700     PRINT "SET INPUT OPTIONS FOR ENTRY ";N9
705   GOSUB 2700		'  OPTION CONTROL
710   GOSUB 2500		'  EXPLAIN FORMAT
715   GO TO 500
720 REM ------- BREAK & HANDLE SPECIAL -----
725 GOSUB 2200		'  BREAK
730 IF R$(P(1))<>"SET" GO TO 755
735   S7=1
740   GOSUB 2600
745   GOSUB 2500
750   GO TO 500
755 IF N1<>2 GO TO 825
760 IF R$(P(2))<>"-" GO TO 825
765   J1=H(10)
770   R$(J1)=R$(1)
775   GOSUB 3000
780     IF E1>0 GO TO 500
785   G$(J1)=R$(J1)
790   CHANGE G$(H(13)) TO T
795   GOSUB 4600
800   CHANGE T TO G$(H(13))
805   FOR T=1 TO H(8)
810     R$(T)=G$(T)
815   NEXT T
820     GO TO 980
825 IF N1>=Z8 GO TO 845
830   PRINT "YOU FORGOT A FIELD - RE-TYPE ENTRY (OR TYPE 'HELP')"
835   E9=1
840   GO TO 500
845 REM ---------- PROCESS ENTRY -----------
850 E9=0
855 FOR J1=1 TO H(8)
860   IF Z(J1)<0 GO TO 950
865   IF R$(J1)<>"^" GO TO 905
870     IF S4=0 GO TO 895
875     IF S5=1 GO TO 895
880       PRINT "CAN'T USE '^' FROM TERMINAL - RETYPE LINE"
885       E9=1
890       GO TO 500
895     R$(J1)=G$(J1)
900     GO TO 950
905   ON Z(J1)+1 GO TO 945,910,945,925,935
910   IF R$(J1)<>"?" GO TO 945
915     R$(J1)=V$(J1)
920     GO TO 950
925   R$(J1)=G$(J1)
930     GO TO 945
935   R$(J1)=G$(J1)
940     GO TO 950
945   GOSUB 3000		'  EDIT
950 NEXT J1
955 IF E9=0 GO TO 980
960 IF S5=0 GO TO 975
965   PRINT
970   PRINT TAB(1);N9;R$
975 GO TO 500
980 REM ----------- GOOD ENTRY -------------
985 GOSUB 1760			'  TRANS TO FILE
990 N9=N9+1
995 IF Z6=0 GO TO 1025
1000   T(0)=2
1005   T(1)=ASC(1)
1010   T(2)=ASC(SP)
1015   A9=1
1020   GOSUB 5000              '   ADD
1025 GO TO 500
1030 REM ========== WRAP-UP =================
1035 S1$="EOF"
1040 WRITE :1: S1$
1045 N9=N9-1
1050 GOSUB 2000			'  WRITE HEADER
1055 GOSUB 6500			'  TOTALS
1060 IF N9<N8 GO TO 1075
1065 PRINT
1070 PRINT "ENTRIES ";N8;" - ";N9;" WRITTEN TO ";N$(0)
1075 CHAIN F$(10)+F$(9)
1200 REM ======= SUBROUTINES ========
1210 '
1500 REM --------- RESTORE ------------
1502 PRINT
1505 PRINT "TO ENTRY #";
1510 INPUT N3
1515 IF N3=N9 GO TO 1595
1520 IF N3>N9 GO TO 1545
1525 IF N3>=N8 GO TO 1535
1530   N8=N3
1535 N9=N3
1540   GO TO 1595
1545 IF END :1 GO TO 1585
1550 READ :1: I$
1555 IF I$<>"EOF" GO TO 1570
1560   SET :1, LOC(2)-1
1565   GO TO 1585
1570 IF LOC(2)<N3+1 GO TO 1545
1575   N9=N3
1580   GO TO 1595
1585 PRINT "NOT ENOUGH ENTRIES IN ";N$(0)
1590 N9=LOC(2)-1
1595 SET :1, N9+1
1600 A9=2
1605 GOSUB 5600
1610 PRINT
1615 RETURN
1760 REM ----- TRANS TO FILE ------
1770 CHANGE B8$ TO O
1790 O(1)=INT(N9/100)
1792 O(2)=INT(N9-100*O(1))
1794 O(1)=O(1)+20
1796 O(2)=O(2)+20
1800 FOR J1=1 TO H(8)
1810   IF Z(J1)<0 GO TO 1890
1830   G$(J1)=R$(J1)
1840   CHANGE G$(J1) TO T
1850   FOR K1=1 TO T(0)
1860     K2=F(13,J1)+K1-1
1870     O(K2)=T(K1)
1880   NEXT K1
1882   IF J1<>H(13) GO TO 1890
1884     A9=2
1886     GOSUB 5000
1890 NEXT J1
1900 CHANGE O TO O$
1910 WRITE :1: O$
1920 RETURN
2000 REM ----- WRITE HEADER -------
2005 SET :1, 1
2010 CHANGE B8$ TO O
2030 O(1)=INT(N8/100)
2035 O(2)=INT(N8-100*O(1))
2040 O(3)=INT(N9/100)
2045 O(4)=INT(N9-100*O(3))
2050 GOSUB 6000
2055 CHANGE O TO O$
2060 WRITE :1: O$
2065 RETURN
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
2200 REM ------ BREAK ---------
2210 CHANGE R$ TO I
2220 I(I(0)+1)=D1
2230 K1=1
2240 J1=N1=0
2250 FOR K2=1 TO I(0)+1
2260   IF I(K2)<>D1 GO TO 2400
2270   N1=N1+1
2280   J1=J1+1
2290   IF J1>O9 GO TO 2410
2300   IF Z(J1)<0 GO TO 2280
2302   IF Z(J1)>2 GO TO 2280
2304   P(N1)=J1		'  PTR IF WE NEED 1,2,3,4....
2310   T(0)=K2-K1
2320   IF T(0)>0 GO TO 2350
2330     R$(J1)="?"
2340     GO TO 2390
2350   FOR J=1 TO T(0)
2360     T(J)=I(J+K1-1)
2370   NEXT J
2380   CHANGE T TO R$(J1)
2390   K1=K2+1
2400 NEXT K2
2410 RETURN
2500 REM ---- EXPL. FORM, COUNT FIELDS -----
2505 T1=Z8=0
2510 FOR J1=1 TO H(8)
2515   IF Z(J1)<0 GO TO 2580
2520   IF Z(J1)>2 GO TO 2580
2522      Z8=Z8+1
2525   IF S4+S7>=1 GO TO 2580		'  NO PRINT IF FILE OR QSET
2530   IF T1=1 GO TO 2565
2535     PRINT
2540     PRINT "ENTRY FORMAT IS:"
2545     PRINT
2550     PRINT "   ";N$(J1);
2555     T1=1
2560     GO TO 2580
2565   PRINT D1$;N$(J1);
2580 NEXT J1
2585 PRINT
2586 PRINT
2590 RETURN
2600 REM ----- OPTION CONTROL (QUICK) -------
2605 R$=R$(P(2))
2610 GOSUB 4335'  GET FIELD NO. (J1)
2615 IF J1=0 GO TO 2680
2620 FOR T3=1 TO 5
2625   IF R$(P(3))=O$(T3) GO TO 2640
2630 NEXT T3
2635   GO TO 2680
2640 IF T3=5 GO TO 2655
2645 IF FNB(F(4,J1),T3)<>1 GO TO 2680
2650 R$(J1)=R$(P(4))
2655 IF Z(J1)<>3 GO TO 2665
2660   Z6=0
2665 Z(J1)=0
2670 ON T3 GO TO 2756,2800,2852,2928,2944
2675 '
2680 PRINT "  QUICK 'SET' FORMAT BAD - IGNORED"
2685   RETURN
2700 REM --------- OPTION CONTROL ------------
2704 GOSUB 4300'  WHAT FIELD
2708 IF R$="DONE" GO TO 2948
2712 IF Z(J1)<>3 GO TO 2720
2716   Z6=0
2720 Z(J1)=0
2724 '
2728 '   DEFAULT VALUE
2732 IF FNB(F(4,J1),1)<>1 GO TO 2780
2736 Q$="  USE DEFAULT"
2740   GOSUB 2100
2744   IF R$="NO" GO TO 2780
2748 PRINT "  VALUE";
2752   INPUT R$(J1)
2756   GOSUB 3000
2760   IF E1<>0 GO TO 2748
2764 V$(J1)=R$(J1)
2768 Z(J1)=1
2772 GO TO 2944
2776 '
2780 '   SIGN REVERSAL
2784 IF FNB(F(4,J1),2)<>1 GO TO 2812
2788 Q$="  REVERSE SIGN"
2792   GOSUB 2100
2796   IF R$="NO" GO TO 2812
2800 Z(J1)=2
2804 GO TO 2944
2808 '
2812 '   AUTO SEQUENCE
2816 IF FNB(F(4,J1),3)<>1 GO TO 2900
2820 IF Z6<>J1 GO TO 2828
2824   Z6=0
2828 IF Z6<>0  GO TO 2900
2832 Q$="  CONSECUTIVE NUMBERS AUTOMATICALLY"
2836   GOSUB 2100
2840   IF R$="NO" GO TO 2900
2844 PRINT "  ENTER THE BEGINNING NUMBER";
2848   INPUT R$(J1)
2852   GOSUB 3000' EDIT
2856   IF E1>0 GO TO 2844
2860 Z6=J1
2864 G$(J1)=R$(J1)
2868 S$=G$(J1)+" "
2872 CHANGE S$ TO T
2876 A9=1
2880 GOSUB 5600
2884 GOSUB 5000			'  ACCUMULATE
2888 Z(J1)=3
2892 GO TO 2944
2896 '
2900 '   BATCH
2904 IF FNB(F(4,J1),4)<>1 GO TO 2944
2908 Q$="  IDENTICAL ENTRIES"
2912   GOSUB 2100
2916   IF R$="NO" GO TO 2944
2920 PRINT "  VALUE";
2924     INPUT R$(J1)
2928     GOSUB 3000' EDIT
2932     IF E1<>0 GO TO 2920
2936 G$(J1)=R$(J1)
2940 Z(J1)=4
2944 IF S7=0 GO TO 2700
2948 RETURN
3000 REM ------ EDIT ----------
3005 E1=0
3010 E1$="-"
3015 CHANGE R$(J1) TO T
3020 L6=F(6,J1)
3025 IF F(5,J1)<>3 GO TO 3035
3030   L6=8
3035 '     - $ FIELD -
3040 IF F(5,J1)<>4 GO TO 3150
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
3120 '   SIGN REVERSAL
3125 IF Z(J1)<>2 GO TO 3150
3130   GOSUB 4600
3135   GO TO 3150
3140 E1$="NO DECIMAL"
3145   GO TO 3520
3150 '     - LENGTH -
3155 IF F(5,J1)=3 GO TO 3185
3160 IF T(0)>L6 GO TO 3175
3165 IF T(0)<F(9,J1) GO TO 3175
3170   GO TO 3185
3175 E1$="INCORRECT LENGTH"
3180   GO TO 3520
3185 '     - FILL -
3190 IF T(0)>=L6 GO TO 3270
3195   D=L6-T(0)
3200   IF F(5,J1)=2 GO TO 3245
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
3275 IF F(5,J1)<>3 GO TO 3380
3280 IF T(6)<>ASC(-) GO TO 3370
3285 IF T(3)= ASC(-) GO TO 3315
3290 IF T(4)<>ASC(-) GO TO 3370
3295   FOR K1=1 TO 3
3300     T(K1)=T(K1+1)
3305   NEXT K1
3310   T(4)=ASC(0)
3315 T(6)=T(5)
3335 T(5)=T(4)
3340 T(4)=T(2)
3345 T(3)=T(1)
3350 T(1)=T(7)
3355 T(2)=T(8)
3360 T(0)=6
3362 IF T(1)<>ASC(7) GO TO 3520
3364 IF (T(3)-ASC(0))*10 + T(4)-ASC(0) > 12 GO TO 3520
3366 IF (T(5)-ASC(0))*10 + T(6)-ASC(0) > 31 GO TO 3520
3368   GO TO 3380
3370 E1$="USE FORM 'MM-DD-YY'"
3375   GO TO 3520
3380 '     - NUMERIC -
3385 IF F(5,J1)=2 GO TO 3440
3390 T1=T(0)
3395 IF F(5,J1)<>4 GO TO 3405
3400   T1=T1-1
3405 FOR K1=1 TO T1
3410   IF T(K1)>ASC(9) GO TO 3430
3415   IF T(K1)<ASC(0) GO TO 3430
3420 NEXT K1
3425   GO TO 3440
3430 E1$="NON-NUMERIC CHARACTER"
3435   GO TO 3520
3440 '     - BACK TO STRING
3445 CHANGE T TO R$(J1)
3450 ON F(3,J1)+1 GO TO 3545,3455,3485
3455 '     - LIST -
3460 FOR I=F(11,J1) TO F(11,J1)+F(12,J1)-1
3465   IF R$(J1)=L$(I) GO TO 3545
3470 NEXT I
3475 E1$="NOT IN LIST"
3480   GO TO 3520
3485 '     - RANGE -
3490 IF R$(J1)<U$(J1) GO TO 3505
3495 IF R$(J1)>U$(O9+J1) GO TO 3505
3500   GO TO 3545
3505 E1$="OUT OF RANGE"
3510   GO TO 3520
3515 '
3520 IF E9>1 GO TO 3530
3525 PRINT TAB(2);N$(J1);" IN ERROR - ";E1$;" - RE-TYPE"
3530 E1=1
3535 E9=E9+1
3540 CHANGE T TO R$(J1)
3545 RETURN
4300 REM ------ WHICH FIELD -------
4305 PRINT
4310 PRINT "  FIELD NAME (OR 'DONE')";
4315 INPUT R$
4320 J1=0
4325 IF R$="HELP" GO TO 4370
4330 IF R$="DONE" GO TO 4415
4335 REM      - E.P. 2 -
4340 FOR J1=1 TO H(8)
4345   IF F(2,J1)=0 GO TO 4355
4350   IF N$(J1)=R$ GO TO 4415
4355 NEXT J1
4360 PRINT "  '";R$;"' NOT PRESENT -TYPE 'HELP' IF NEEDED"
4365   GO TO 4300
4370 '  HELP
4375 PRINT
4380 PRINT "FIELDS ARE:"
4385 PRINT
4390 FOR J1=1 TO H(8)
4395   IF F(2,J1)=0 GO TO 4405
4400     PRINT TAB(5);N$(J1)
4405 NEXT J1
4410   GO TO 4300
4415 RETURN
4600 REM -------- REVERSE SIGN ---------
4605 T(T(0))=T(T(0))+13
4610 IF T(T(0))=ASC(-) GO TO 4620
4615   T(T(0))=ASC(SP)
4620 RETURN
5000 REM ---ACCUMULATE---
5010 A1=13-T(0)
5020 A2=1
5030 IF T(13-A1)<>ASC(-) THEN 5050
5040 A2=-1
5050 FOR A8=A1 TO 11
5060   IF T(A8-A1+1)=ASC(0) THEN 5100
5070   A4=T(A8-A1+1)-48
5080   A5=INT((A2*A4)+.5)
5090   A(A8,A9)=A(A8,A9)+A5
5100 NEXT A8
5110   RETURN
5200 REM ---READ ACCUMULATOR---
5210 T(12)=32
5220 FOR A8=11 TO 1 STEP -1
5230   A1=0
5240   IF A(A8,A9)=0 THEN 5260
5250   A1=A(A8,A9)/10
5260   A2=INT(A1)
5270   A(A8-1,A9)=A(A8-1,A9)+A2
5280   A(A8,A9)=INT(.5+10*(A1-A2))
5290 NEXT A8
5300 IF A(0,A9)=0 THEN 5350
5310 A(11,A9)=A(11,A9)-1
5320 A(0,A9)=0
5330 T(12)=ASC(-)
5340   GO TO 5220
5350 FOR A8=1 TO 11
5360   IF T(12)<>ASC(-) THEN 5390
5370   T(A8)=ASC(0)+9-A(A8,A9)
5380     GO TO 5400
5390   T(A8)=ASC(0)+A(A8,A9)
5400 NEXT A8
5410 T(0)=12
5420 IF T(12)<>ASC(-) THEN 5450
5430 A(0,A9)=-1
5440 A(11,A9)=A(11,A9)+1
5450 RETURN
5600 REM ---CLEAR ACCUMULATOR---
5610 FOR A8=0 TO 12
5620   A(A8,A9)=0
5630 NEXT A8
5640 RETURN
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
6500 REM -------- PRINT TOTAL -----------
6505 IF H(13)=0 GO TO 6590
6510 A9=2
6520 GOSUB 5200
6530 GOSUB 6700
6540 CHANGE T TO S$
6550 PRINT
6560 PRINT "   NET BALANCE FOR CURRENT ENTRIES:   ";S$
6570 PRINT
6590 RETURN
6700 REM -------- $ FORMAT --------
6710 FOR K1=T(0) TO T(0)-3 STEP -1
6720   T(K1+1)=T(K1)
6730 NEXT K1
6740 T(0)=T(0)+1
6750 T(T(0)-3)=ASC(.)
6760 V3=4
6770 GOSUB 6840
6790 RETURN
6840 REM ----- LEFT STRIP & TRUNCATE ------
6850 FOR K1=1 TO T(0)-V3
6860   IF T(K1)>ASC(0) GO TO 6890
6880 NEXT K1
6890 K3=0
6895 FOR K2=K1 TO T(0)
6900   K3=K3+1
6910   T(K3)=T(K2)
6920 NEXT K2
6930 T(0)=K3
6990 RETURN
8000 REM ---READ STRUCTURE FILE---
8005 O9=15
8010 READ :1: I$
8015 CHANGE I$ TO I
8020 GOSUB 6100
8025 FOR Y=0 TO I(0)
8030   H(Y)=I(Y)
8035 NEXT Y
8040 N$=MID$(I$,20,13)
8045 N$(0)=MID$(I$,34,H(33))
8050 FOR T2=1 TO H(3)
8055   READ :1: I$
8060   CHANGE I$ TO I
8065   GOSUB 6100
8070   T3=I(2)
8075   FOR T1=1 TO 16
8080     F(T1,T3)=I(T1)
8085   NEXT T1
8088   F(11,T3)=100*F(15,T3)+F(11,T3)
8090   F(12,T3)=100*F(16,T3)+F(12,T3)
8095   N$(T3)=MID$(I$,17,I(7))
8130 NEXT T2
8135 L=1
8140 T5=1
8145 FOR T1=1 TO H(8)
8150   IF F(3,T1)<>1 THEN 8205
8155   FOR T2=1 TO F(10,T1)
8160     READ :1: I$
8165     T4=1
8170     FOR T3=1 TO INT(132/F(6,T1))
8175       L$(L)=MID$(I$,T4,F(6,T1))
8180       T4=T4+F(6,T1)
8185     L=L+1
8190     IF L-T5=F(12,T1) THEN 8205
8195     NEXT T3
8200   NEXT T2
8205   T5=L
8210 NEXT T1
8215 IF H(5)=0 GO TO 8260
8220 FOR T1=1 TO H(5)
8225   READ :1: I$
8230   CHANGE I$ TO I
8235   GOSUB 6100
8240   T2=I(1)
8245   U$(T2)=MID$(I$,2,F(6,T2))
8250   U$(T2+O9)=MID$(I$,2+F(6,T2),F(6,T2))
8255 NEXT T1
8260 F(13,0)=1
8265 F(6,0)=2
8270 RETURN
9800 '
9810 DATA DEF,REV,SEQ,IDE,OFF
9999 END
  