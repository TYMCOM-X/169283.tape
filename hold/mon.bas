100 '     M O N
102 '     -----
104 '
106 '     COMPANY PROPRIETARY, LUPFER & LONG COMPUTER SERVICES.
110 '
120 DIM F$(50),T(10)
125 REM ========= INIT =============
130 F1=0
140 E2$="-"
150 FILE #3, "COMCOM.TMP"
160   QUOTE #3
170 F9=20
180 FOR I=1 TO 50
185   IF END #3 GO TO 196
190   READ #3: F$(I)
192     GO TO 200
196   F$(I)="*"
200 NEXT I
210 FILE #1: F$(11)       ' SUPERFILE
215   QUOTE #1
220 FILE #2: F$(12)       ' ICL
230 REM ------ CUSTOMER ------------
240 IF F$(18)<>"START" GO TO 400
245 F$(18)="*"
250 P1=0
260 IF F$(1)<>"*" GO TO 300
280 PRINT "CUSTOMER";
290 INPUT F$(1)
300 RESTORE #1
310 READ #1, Z,Z1$,Z2$,Z3$
320 IF Z<>0 GO TO 360
330 IF Z3$<>F$(1) GO TO 360
340     F$(15)=Z2$
350     GO TO 520
360 IF END #1 GO TO 370
365   GO TO 310
370 PRINT "CUSTOMER NOT FOUND"
380 PRINT
390 GO TO 280
400 REM -------- RETURN ------------
440 P1=VAL(F$(14))
450 IF F1=0 GO TO 470
460   PRINT "P1=";P1
470 FOR I=1 TO P1
480   READ #2: C1,C2,C$
490 NEXT I
500 GO TO 680
520 REM -------- FUNCTION ----------
530 PRINT
540 PRINT "FUNCTION";
550   P1=0
560   RESTORE #2
570   INPUT R$
582   IF R$="HELP" GO TO 4000
585   FOR I=2 TO 50
586     IF I<11 GO TO 588
587     IF I<21 GO TO 590
588       F$(I)="*"		'  IF I NOT 11 THRU 20
590   NEXT I
592   F$(16)=R$
595 IF END #2 GO TO 650
600 READ #2: C1,C2,C$
610   P1=P1+1
620   IF C1<>0 GO TO 640
630   IF C$=R$ GO TO 680
640 GO TO 595
650   PRINT "INVALID FUNCTION - TYPE 'HELP'"
660   GO TO 520
680 REM ********* ICL **************
690 P1=P1+1
710 READ #2: C1,C2,C$
720 IF C$<>"??" GO TO 770
730   IF F$(17)<>"*" GO TO 760
740     E2$="NO ASK"
750     GO TO 4240
760   C$=F$(17)
770 IF C1=0 GO TO 680
800 IF C1>10 GO TO 820
810 ON C1 GO TO 830,930,970,1010,1130,4240,1510,1550,4240,4240
820 ON(C1-10) GO TO 1840,1872,4240,4240,4240,4240,4240,4240,2100,2140
830 REM -------- 1 - PASS ----------
840 S1$=C$
845 IF RIGHT$(S1$,1)<>"&" GO TO 900
850   S1$=""
860   IF LEFT$(C$,1)<>"." GO TO 890
870     S1$ = LEFT$(C$,4) + "$"
890   C$=F$(19)+S1$
900 F$(C2)=C$
902 IF C2<F9 GO TO 910
904   F9=C2+1
910 GO TO 680
930 REM -------- 2 - SKIP -----------
940 FOR I=1 TO C2
950   READ #2, C1,T2,C$
960   P1=P1+1
970 NEXT I
980 GO TO 680
990 GO TO 680
1010 REM ------- 4 - SEARCH --------
1020 RESTORE #1
1025 IF END #1 GO TO 1100
1030 READ #1: Z,Z1$,Z2$,Z3$
1040 IF Z<>C2      GO TO 1025
1050 IF Z1$<>F$(15) GO TO 1025
1060 IF Z3$<>C$    GO TO 1025
1070   F$(19)="F"+Z1$+Z2$
1080   GO TO 680
1100 PRINT "CAN'T FIND ";C$
1110   GO TO 520
1130 REM ------- 5 - IFNO ----------
1140 PRINT
1150 PRINT C$;
1160 I1=1
1170 INPUT R$
1180 IF R$="YES" GO TO 680
1190 IF R$="Y"   GO TO 680
1200 IF R$="NO"  GO TO 930
1210 IF R$="N"   GO TO 930
1220 PRINT "PLEASE ANSWER 'YES' OR 'NO'"
1230   GO TO 1130
1510 REM ------- 7 - ASK -----------
1520 PRINT C$;
1530 INPUT F$(17)
1540 GO TO 680
1550 REM ------- 8 - RUN -----------
1560 T(0)=3		'  REPLACE 'BUGGED' STR$( )
1562 T(1)=INT(P1/100)
1564 P1=INT(P1-100*T(1))
1566 T(2)=INT(P1/10)
1568 T(3)=ASC(0)+INT(P1-10*T(2))
1570 T(1)=ASC(0)+T(1)
1572 T(2)=ASC(0)+T(2)
1574 CHANGE T TO F$(14)
1576 F$(9)="MON"
1580 F$(10)="(DCLAWSON)"
1585 F$(13)=C$
1590 IF LEFT$(C$,1)="(" GO TO 1600
1595   F$(13)=F$(10)+F$(13)
1600 SCRATCH #3
1610 FOR I=1 TO F9
1620   WRITE #3, F$(I)
1630 NEXT I
1690 CHAIN F$(13)
1700 REM ------- 19 - END -----------
1710 GO TO 520
1840 REM ------ 11 - COMMENT -------
1850 IF C2<>99 GO TO 1870
1860   PRINT C$
1870 GO TO 680
1872 REM --------- 12 - FIND ALL ------------
1873 IF C$<>"PRINT" GO TO 1875
1874   PRINT
1875 RESTORE #1
1876 I=21
1878 IF END #1 GO TO 680
1880 READ #1, Z,Z1$,Z2$,Z3$
1881 IF Z<>C2 GO TO 1878
1882 IF Z1$<>F$(15) GO TO 1878
1884 IF C$<>"PRINT" GO TO 1890
1886   PRINT TAB(5);Z3$
1888   GO TO 1878
1890 F$(I) = "F" + Z1$ + Z2$
1894 I=I+1
1895 F9=I
1896 GO TO 1878
2100 REM ------- 19 - END -----------
2110 GO TO 520
2140 REM ------- 20 - STOP ----------
2150 STOP
2800 '
4000 REM ====== HELP (FUNCTION) ====
4010 PRINT
4020 PRINT "THE FOLLOWING FUNCTIONS ARE AVAILABLE"
4030 T1=0
4040 RESTORE #2
4050 '
4060 IF END #2 GO TO 4220
4070 READ #2: C1,C2,C$
4080 IF C1<>0 GO TO 4150
4090   PRINT
4100   IF T1=0 GO TO 4120
4110   PRINT
4120 PRINT C$;
4130   T1=LEN(C$)
4140   GO TO 4210
4150 IF C1<>11 GO TO 4210
4160 IF C2<>0  GO TO 4210
4170   IF T1<23 GO TO 4190
4180     PRINT
4190   T1=0
4200   PRINT TAB(25);C$
4210 GO TO 4060
4220   PRINT
4230   GO TO 520
4240 REM ====== SYSTEM ERROR =======
4250 PRINT "* PROGRAM ERROR *"
4260 PRINT E2$
9999 END
 