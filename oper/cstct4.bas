1 :'RRR#### 'LLLLLLLL######.# ######.# ####### ####.# #### ####### ######
20:     COST CENTER 'RRRRRRRR 'RRRRRRR  $ VAR.   ACT   BUD     QTY   IPH
30 FILES WTDSC,EMP8,CCRATE,CORPRT,CCYTD,CYTD2
40 SCRATCH #4
50 SCRATCH #6
60 PRINT "ENTER NO. OF DAYS";
70 INPUT D1
72 PRINT "ENTER 1 IF FIRST PERIOD";
73 INPUT X9
75 PRINT "ENTER CO. , MO. , DAY , YR."
76 INPUT C$,M5,M6,M7
80 PRINT
85 PAGE 55
90 PRINT
95 PRINT USING 1657,C$,M5,M6,M7
96 PRINT
100 Y=50
110 DIM E(50),F(50),G(50),H(50),R(50),T(50)
120 FOR Q=1 TO Y
130 IF END #5 THEN 170
140 READ #5,E(Q),F(Q),G(Q),H(Q),R(Q),T(Q)
150 D4=D4+1
160 NEXT Q
170 PAGE ALL 58
180 G1=100
190 F$="TOT HRS"
200 W$="TOTAL BY OPER CODE"
210 Z$="TOTAL BY CST CTR"
220 U$="GRAND TOTAL"
230 G$="CHG HRS"
240 H$="NON-CHG HRS"
250 Q$="HRS     %       HRS     %"
260 R$="TOT $ VARIANCE"
270 P$="COST ABSORB."
275 K$="-"
280 S$="VOLUME VARIANCE"
300 O$="ANALYSIS"
310 T$="EFF. VARIANCE"
320 PRINT USING 20,F$,G$
330 PRINT
340 L$="WK"
350 M$="MTD"
360 V$="ACTUAL          BUDGET"
370 A$="  OPER       ACTUAL    STD         COMP"
380 READ M1,M2
390 DIM D(50),D$(50),M(50),N(50),K(50),P(50),L(50)
400 FOR I=1 TO Y
410 IF END #3 THEN 450
420 READ #3,D(I),D$(I),M(I),N(I),K(I),P(I),L(I)
430 D2=D2+1
440 NEXT I
450 S=1
460 X=50
470 DIM A(50),B$(50)
480 FOR J=1 TO X
490 IF END #2 THEN 590
500 READ #2,A(J),B$(J)
510 D3=D3+1
520 NEXT J
590 IF END #1 THEN 830
600 INPUT #1,C1,C2,E1,C3,E3,C4,B1,C5,C6
610 IF S=1 THEN 1670
620 IF C2<>A2 THEN 830
630 IF C3<>A1 THEN 810
640 S=2
650 IF E1=A(J) THEN 690
660 FOR J=1 TO D3
665 IF E1=A(J) THEN 690
670 NEXT J
680 IF E1<>A(J) THEN 1930
690 IF C3>10 THEN 750
700 T1=T1+C4
710 F1=F1+C4
720 T2=T2+B1
730 T3=T3+C5
740 GO TO 760
750 T1=T1+C4
760 A1=C3
770 A2=C2
780 PRINT #4,USING 790,C1,E1,B$(J),C3,E3,C4,B1,C5,A3
790 :########   #### 'LLLLLLLLLLLL ### ####  ###.#   ###.# ######### ##
800 GO TO 590
810 IF A1<10 THEN 830
820 IF C3>9 THEN 640
830 PRINT #4,USING 860,W$,T1,T2,T3
840 IF A1<>2 THEN 870
850 N1=T1
855 R6=T3
860 :'LLLLLLLLLLLLLLLLLLLLLLL           ########.# #####.# #########
870 IF T1=0 THEN 880
872 R1=T3/T1
873 IF A1<>2 THEN 880
874 R5=R1
880 PRINT #4,USING 890,R1
890 :QTY PER HR                                            #########
910 R1=0
920 PRINT #4
930 T4=T4+T1
940 T5=T5+T2
950 T6=T6+T3
960 T1=0
970 T2=0
980 T3=0
990 IF END #1 THEN1010
1000 IF C2=A2 THEN 640
1008 IF N(I)=0 THEN 1020
1010 B2=(P(I)/N(I))*M(I)*D1
1020 B3=K(I)*B2
1030 B4=B2-B3
1040 F2=T4-F1
1050 PRINT #4,USING 860,Z$,T4,T5,T6
1060 PRINT #4
1070 PRINT #4
1080 F3=F1/T4
1090 PRINT #4,USING 1730,V$
1100 K1=(F1-B3)*L(I)
1110 K2=((T4-B2)*K(I))*L(I)
1120 K3=((F3-K(I))*T4)*L(I)
1128 IF M(I)=0 THEN 1140
1130 E4=T4/(M(I)*D1)
1140 E5=K(I)*G1
1150 E6=G1-E5
1160 F3=F3*100
1170 F4=G1-F3
1180 PRINT #4,USING 1190,E4,P(I)
1190 :EQUIVALENT MPR  ####.#          ####.#
1200 PRINT #4,USING 1250,Q$
1210 PRINT #4,USING 1240,F$,T4,G1,B2,G1
1220 PRINT #4,USING 1240,G$,F1,F3,B3,E5
1230 PRINT #4,USING 1240,H$,F2,F4,B4,E6
1240 :'LLLLLLLLLLL  ######.# ####.# ######.# ####.#
1250 :                   'LLLLLLLLLLLLLLLLLLLLLLLL
1260 PRINT #4
1270 PRINT #4,USING 2080,O$
1280 PRINT #4
1290 PRINT #4,USING 1380,L(I)
1300 PRINT #4
1310 PRINT #4,USING 1370,R$,K1
1320 PRINT #4,USING 1370,S$,K2
1330 PRINT #4,USING 1370,T$,K3
1332 K4=K4+K1
1334 K5=K5+K2
1335 K6=K6+K3
1340 PRINT #4
1350 PRINT #4
1360 PRINT #4
1370 :'LLLLLLLLLLLLLLLLL  #######.
1380 :HOURLY RATE            ####.##
1390 S1=F1*L(I)
1400 S2=S2+S1
1410 PRINT #4,USING 1370,P$,S1
1412 PRINT #4 <PA>
1415 T9=T9+T6
1418 T6=T6/1000
1419 R6=R6/1000
1420 IF A2=M1 THEN 2100
1425 IF A2=M2 THEN 2120
1430 IF S3=2 THEN 2140
1450 S1=0
1460 FOR Q=1 TO Y
1470 IF E(Q)=A2 THEN 1485
1480 NEXT Q
1482 GO TO 2251
1485 E(Q) = E(Q)+1000
1488 IF X9=1 THEN 2252
1490 F(Q)=F(Q)+T4
1492 X3=F(Q)
1500 G(Q)=G(Q)+F1
1502 X4=G(Q)
1510 H(Q)=H(Q)+K1
1512 X5=H(Q)
1520 R(Q)=R(Q)+N1
1522 X6=R(Q)
1527 X7=T(Q)
1530 T(Q)=T(Q)+R6
1540 IF F(Q)=0 THEN 1548
1545 R3=(G(Q)/F(Q))*100
1548 IF G(Q)=0 THEN 1560
1549 IF R(Q)=0 THEN 1560
1550 R4=(T(Q)*1000)/R(Q)
1560 PRINT USING 1,L$,A2,D$(I),T4,F1,K1,F3,E5,T6,R5
1570 PRINT USING 1,M$,A2,D$(I),F(Q),G(Q),H(Q),R3,E5,T(Q),R4
1575 PRINT
1580 WRITE #6,A2;F(Q);G(Q);H(Q);R(Q);T(Q)
1590 T7=T7+T4
1600 T8=T8+K1
1620 V1=V1+F1
1630 V5=V5+B2
1635 R6=0
1640 V6=V6+B3
1641 X3=0
1642 X4=0
1643 X5=0
1644 X6=0
1645 X7=0
1646 R3=0
1647 R4=0
1648 R5=0
1649 B2=0
1657 :PRODUCTION REPORT  'LLLLLLLLLLLLLLLLLLLL  WK ENDING ### ### ###
1658 PRINT #4,K$
1660 IF END #1 THEN 1830
1670 FOR I=1 TO D2
1680 IF D(I)=C2 THEN 1740
1690 X2=X2+1
1700 IF X2>300 THEN 1950
1710 NEXT I
1715 D$(I)="CHECK CC"
1720 X2=0
1730 :                   'LLLLLLLLLLLLLLLLLLLLLLLLL
1740 PRINT #4
1742 PRINT #4,USING 1657,C$,M5,M6,M7
1745 PRINT #4
1750 PRINT #4,USING 1760,C2,D$(I)
1760 :COST CTR #### 'LLLLLLLLLLLLLLLLLLL
1770 PRINT #4,USING 1820,A$
1780 E$="JOB NO   EMPLOYEE      CODE  MDATE  HRS     HRS      QTY  CD"
1790 PRINT #4,USING 1800,E$
1800:      'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
1810 PRINT #4
1820:                    'RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
1830 H1=H1+F(Q)
1840 H2=H2+G(Q)
1850 H3=H3+H(Q)
1860 H4=H4+T(Q)
1870 T4=0
1880 T5=0
1890 T6=0
1900  F1=0
1910 IF END #1 THEN 1970
1920 GO TO 640
1930 B$(J)="NO NAME"
1940 GO TO 690
1950 D$(I)="NO NAME"
1952 X2=0
1960 GO TO 1720
1970 PRINT #4
1980 PRINT #4,USING 860,U$,T7,T8,T9
1990 PRINT #4
2000 PRINT #4,USING 1370,P$,S2
2010 :'LLLLLLLLLLL
2020 V3=(V1/T7)*100
2030 V4=(V6/V5)*100
2040 H5=(H2/H1)*100
2050 H6=V4
2055 T4=0
2057 T9=T9/1000
2060 PRINT USING 1,L$,T4,U$,T7,V1,T8,V3,V4,T9
2070 PRINT USING 1,M$,T4,U$,H1,H2,H3,H5,H6,H4
2080 :'LLLLLLLLLLLLLLL
2081 J1=T7
2082 J2=V1
2083 J3=V5
2084 J4=V6
2085 J7=K4
2086 J8=K5
2087 J9=K6
2090 GO TO 2260
2100 S3=2
2110 GO TO 2140
2120 S3=0
2130 READ M1,M2
2140 J1=J1+T4
2150 J2=J2+F1
2160 J3=J3+B2
2170 J4=J4+B3
2180 J5=J5+F1
2190 J6=J6+S1
2200 J7=J7+K1
2210 J8=J8+K2
2220 J9=J7-J8
2230 IF END #1 THEN 2260
2240 IF S3=O THEN 2260
2250 GO TO 1450
2251 E(Q)=A2
2252 F(Q)=T4
2253 G(Q)=F1
2254 H(Q)=K1
2255 R(Q)=N1
2256 T(Q)=T6
2258 GO TO 1540
2260 PRINT #4
2265 IF J1=0 THEN 2275
2270 F5=(J2/J1)*100
2275 IF J3=0 THEN 2290
2280 F6=(J4/J3)*100
2290 F7=J1-J2
2300 F8=G1-F5
2310 F9=J3-J4
2320 E8=G1-F6
2321 PRINT #4
2325 PRINT #4
2326 PRINT #4,USING 1730,V$
2328 PRINT #4
2329 PRINT #4,USING 1250,Q$
2330 PRINT #4, USING 1240,F$,J1,G1,J3,G1
2340 PRINT #4, USING 1240,G$,J2,F5,J4,F6
2350 PRINT #4,USING 1240,H$,F7,F8,F9,E8
2360 PRINT #4
2370 PRINT #4,USING 2080,O$
2380 PRINT #4
2382 PRINT #4,USING 1370,R$,J7
2384 PRINT #4,USING 1370,S$,J8
2386 PRINT #4,USING 1370,T$,J9
2400 J1=0
2402 J2=0
2404 J3=0
2406 J4=0
2408 F7=0
2410 F9=0
2412 J5=0
2414 J6=0
2416 J7=0
2418 J8=0
2420 J9=0
2425 IF END #1 THEN 3000
2450 GO TO 1450
3000 PRINT
5000 DATA 201,216,999,999
6000 FOR Q=1 TO Y
6005 IF E(Q) =0 THEN 6020
6010 IF E(Q)<999 THEN 6050
6020 NEXT Q
6030 GO TO 9000
6050 WRITE #6,E(Q);F(Q);G(Q);H(Q);R(Q);T(Q)
6060 GO TO 6020
9000 END
 