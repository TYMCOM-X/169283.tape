90 DEF FNA(Y)=Y-(INT((Y-1)/W1))*W1
100 N$=""
101 DIM A(360,3),B(440,20),K(2,25),H(2,55),T(400),M(400,10),N(400,7),E(110),Z(400)
105 PRINT "1=NARROW, 2=WIDE CARRIAGE."
110 INPUT W3
115 PRINT" NO. OF PERIODS ? (MONTHLY=12, QTRLY=4, ....IE. NO. OF PERIODS IN A YEAR"
120 INPUT W1
121 O$="PRD"
122 IF W1>4 GOTO 126
123 O$="QTR"
125 GOTO 128
126 O$="MON"
128 IF W3>1 GOTO 2680
130 O=100
131 G=.5
135 PRINT "DATAFILE";
140 INPUT K$
145 FILE #1,K$
150 REM COMPANY NAME
155 READ #1,L$
160 REM FINAL YR,PRD
165 READ #1,Y2,Q2
170 Q1=W1*(Y2-1)+Q2
210 MAT M=ZER
215 MAT P= ZER
220 MAT T = ZER
225 MAT N= ZER
230 GOTO 1785
235 D$="NO"
240 REM "ITC",YR,PRD
245 READ #1, I5,Y3,Q3
250 C3=W1*(Y3-1)+Q3
255 C4=Y3
260 REM "TAX RATE";
265 READ #1, Z1
270 REM NO. LOANS
275 READ #1,N0,I$
276 IFN0=0 THEN 595
280 FOR N9=1 TO N0
285 REM DEBT TYPE: LEVEL,MORATORIUM,DATA INPUT
290 READ #1, A$
295 N=W1
300 IF LEFT$(A$,1)="L" THEN 450
305 IF LEFT$(A$,1)="M" THEN 325
310 IF LEFT$(A$,1)="D" THEN 405
315 PRINT"ERROR IN DEBT TYPE IN DATA FILE"
320 STOP
325 REM MORATORIUM CALCULATION
330 REM INPUT=PRINCIPLE,INT RATE,STARTING YR,PRD;TOTAL YRS,YRS INT ONLY
335 READ#1,B1,R,Y2,Q2,Y4,Y3
340 Q3=Q4=Q2
345 Y3=Y3+Y2
350 Y4=Y4+Y2
355 B2=W1*(Y2-1)+Q2
360 B3=W1*(Y3-1)+Q3
365 I6=B1*R/(W1*100)
370 B4=W1*(Y4-1)+Q4-1
375 E=B4-B3+1
380 FOR Y=B2 TO B3-1
385 M(Y,3)=I6+M(Y,3)
390 NEXT Y
395 I7=B3
400 GOTO 480
405 PRINT"DATA INPUT FOR INTEREST: INTEREST,STARTING YR,PRD;ENDING YR,PRDFOR EACH AMT (ALL ZEROES TO END)"
410 INPUT I6,Y2,Q2,Y3,Q3
415 IF I6=0 THEN 530
420 B2=W1*(Y2-1)+Q2
425 B3=W1*(Y3-1)+Q3
430 FOR Y=B2 TO B3
435 M(Y,3)=I6+M(Y,3)
440 NEXT Y
445 GOTO 410
450 REM "AMT,RATE,STARTING YR,PRD;NO YRS
455 READ #1, B1,R,Y2,Q2,S
460 B3=W1*(Y2-1)+Q2
465 B4=W1*S+B3-1
470 E=B4-B3+1
475 I7=B3
480 P=(B1*R/N)/(1-(1+(R/N)/100)^(-E))+.5
485 P=INT(P)/100
490 B6=P
495 R=R*100
500 FOR Y=I7 TO E+I7-1
505 I1=INT((B1*R)/(N*100)+.5)/100
510 P1=P-I1
515 B1=B1-P1
520 M(Y,3)=I1+M(Y,3)
525 NEXT Y
530 NEXT N9
535 REM D/S
540 READ #1,I$
545 READ #1,D1
546 READ #1,Y2,Q2,Y3,Q3
550 D2=W1*(Y2-1)+Q2
555 D3=W1*(Y3-1)+Q3
560 FOR Y=D2 TO D3
565 M(Y,4)=D1
570 NEXT Y
571 READ #1,I$
572 IF LEFT$(I$,1)="R" THEN 600
573 D1=VAL(I$)
575 GO TO 546
580 GOTO 545
585 B1=0
590 REM "RENT STARTING YR,PRD;ENDING YR,PRD
595 READ #1,I$
600 READ #1,B1
603 READ #1,Y2,Q2,Y3,Q3
605 B2=W1*(Y2-1)+Q2
610 B3=W1*(Y3-1)+Q3
615 FOR Y=B2 TO B3
620 M(Y,1)=B1
625 NEXT Y
626 READ #1,I$
627 IF LEFT$(I$,1)="E" THEN 650
628 B1=VAL(I$)
630 GOTO603
635 GOTO 600
640 REM "EXP";
645 READ #1,I$
650 READ #1, E,Y2,Q2,Y3,Q3
655 E2=W1*(Y2-1)+Q2
660 E3=W1*(Y3-1)+Q3
665 FOR Y=E2 TO E3
670 M(Y,5)=E
675 NEXT Y
680 IF E3>=Q1 GOTO 690
685 GOTO 650
690 REM "ANNUAL SF RATE (%)
695 READ #1, R3
700 R3=R3/100
705 REM "FEE,FRONT(F) OR AMORTZD(A)";
710 READ #1, H6,F$
715 IF F$="F"  GOTO 730
720 H7=H6/Q1
725 H6=0
730 REM"EQUITY
735 READ #1, N(1,6)
740 REM"NET RESID";
745 READ #1, K1
750 PRINT "PRINT FLOWS";
755 INPUT W$
760 IF W$="NO" GOTO 780
765 PRINT "ANNUAL OR PERIODICAL FLOWS";
770 INPUT M$
775 M$=LEFT$(M$,1)
780 PRINT "CF FILENAME";
785 INPUT F$
790 FILE #3,F$
795 SCRATCH #3
800 FOR I=1TO4
805 PRINT
810 NEXT I
815 PRINT SPACE$(40-LEN(L$)/2);L$
820 PRINT SPACE$(33);"LEASE ANALYSIS"
822 PRINT SPACE$(40);"FIGURES IN HUNDREDS OF DOLLARS"
825 FOR I=1TO3
830 PRINT
835 NEXT I
840 PRINT USING 845,N$
845 :PRD RENTAL  DEP+  INT.   INT. & OTHER INTNL  TAX LS TX SAV AFT TX CUML '
850 PRINT USING 855,N$
855 :    INCOME FT END ON AMR PRINCP EXPEN CASHFL OR GAI OR PMT CASHFL AFT TX '
860 PRINT USING 865,Z1*100
865 :           FEES   DEBT   ON DBT SES   BEF.TX N     @###.#%       CASHFL
870 PRINT
875 PRINT
880 IF W$="NO" THEN 890
885 V1=V2=V3=V4=V5=V6=V7=V8=V9=0
890 L1=L2=L3=L4=L5=L6=L7=L8=L9=0
895 FOR Y=1 TO Q1+1
900 M(Q1+1,1)=K1
905 L9=L9+1
910 M(Y,2)=M(Y,2)+H7
915 M(1,2)=M(1,2)+H6
920 M(Q1+1,2)=X0
925 M(Y,6)=M(Y,1)-M(Y,4)-M(Y,5)
930 T7=M(Y,6)
935 M(Q1+1,6)=K1
940 M(Y,7)=M(Y,1)-M(Y,2)-M(Y,3)-M(Y,5)-(T7-M(Y,6))
945 M(Q1+1,7)=K1-X0
950 M(Y,8)=-M(Y,7)*Z1
955 M(Y,9)=M(Y,8)+M(Y,6)
960 M(C3,9)=M(C3,9)+I5
965 M(Q1+1,9)=K1-(K1-X0)*Z1
970 L1=L1+M(Y,1)
975 L2=L2+M(Y,2)
980 V1=V1+M(Y,1)
985 V2=V2+M(Y,2)
990 V3=V3+M(Y,3)
995 V4=V4+M(Y,4)
1000 L3=L3+M(Y,3)
1005 L4=L4+M(Y,4)
1010 L5=L5+M(Y,5)
1015 V5=V5+M(Y,5)
1020 V6=V6+M(Y,6)
1025 V7=V7+M(Y,7)
1030 V8=V8+M(Y,8)
1035 L6=L6+M(Y,6)
1040 V9=V9+M(Y,9)
1045 L7=L7+M(Y,7)
1050 L8=L8+M(Y,8)
1055 L0=L0+M(Y,9)
1060 T5=T5+M(Y,9)
1065 M(Y,10)=M(Y,2)+M(Y,3)+M(Y,5)
1070 IF W$="NO" THEN 1130
1075 IF M$="P" THEN 1125
1080 A5=W1
1085 IF Y=Q1+1 THEN 1115
1090 IF Y=Q1 THEN 1100
1095 IF L9<>W1 THEN 1130
1100 PRINT USING 1105,(Y+W1-1)/W1,L1/O+G,L2/O+G,L3/O+G,L4/O+G,L5/O+G,L6/O+G,L7/O+G,L8/O+G,L0/O+G,T5/O+G
1105 :#### ###### ###### ##### ###### ##### ###### ###### ###### ###### ######
1110:'LLL ###### ###### ##### ###### ##### ###### ###### ###### ###### ######
1115 L1=L2=L3=L4=L5=L6=L7=L8=L9=L0=0
1120 GOTO 1130
1125 PRINT USING 1105,Y,M(Y,1)/O+G,M(Y,2)/O+G,M(Y,3)/O+G,M(Y,4)/O+G,M(Y,5)/O+G,M(Y,6)/O+G,M(Y,7)/O+G,M(Y,8)/O+G,M(Y,9)/O+G,T5/O+G
1130 T(Y)=M(Y,9)
1135 WRITE #3, T(Y)
1140 NEXT Y
1145 K2=(K1-X0)*Z1
1150 K3=K1-((K1-X0)*Z1)
1155 PRINT
1160 PRINT
1165 PRINT USING 1110, "SALE",K1/O+G,X0/O+G,0,0,0,K1/O+G,(K1-X0)/O+G,(-K2)/O+G,K3/O+G,T5/O+G
1170 PRINT
1175 PRINT USING 1180,V1/O+G,V2/O+G,V3/O+G,V4/O+G,V5/O+G,V6/O+G,V7/O+G,V8/O+G,V9/O+G,T5/O+G
1180:TOT  ###### ###### ##### ###### ##### ###### ###### ###### ###### ###### 
1185 PRINT
1190 B1=Q1+1
1195 FOR Y=2 TO B1+1
1200 N(Y,1)=T(Y-1)
1205 NEXT Y
1210 C=N(1,6)
1215 REM SINKING FUND ROUTINE
1220 MAT A = ZER
1225 FOR Y= 1 TO B1
1230 A(Y,1)=T(Y)
1235 NEXT Y
1240 IF T(Q1+1)>0 THEN 1250
1245 A(Q1,1)=A(Q1,1)+T(Q1+1)
1250   
1255 IF D$="YES" THEN 1265
1260 GOTO 1305
1265 PRINT "SF RATE,INVESTMENT,RESIDUAL";
1270 INPUT R,C,Z2
1275 P=W1
1280 R=R/100
1285 Z3=Z2-((Z2-X0)*Z1)
1290 A(B1,1)=Z3
1295 T(Q1+1)=Z3
1300 GOTO 1315
1305 R=R3
1310 P=W1
1315 N1=Y1=N2=N3=T=N5=0
1320 R5=R
1325 REM CALC OF SINKING FUND
1330 N2=0
1335 FOR Y=Q1 TO 1 STEP-1
1340 IF A(Y,1)+N2<0 THEN  1370
1345 A(Y,3)=A(Y,1)+N2   'RETURNS TO LESSOR
1350 N(Y+1,4)=-N2
1355 A(Y,2)=-N2
1360 N2=0
1365 GOTO 1385
1370 A(Y,2)=-N2
1375 N(Y+1,4)=A(Y,1)
1380 N2=(A(Y,1)+N2)/(1+R/P)   'PREV PERIOD S.F. BAL
1385 NEXT Y
1390 IF T(Q1+1)<0 THEN 1400
1395 A(Q1,3)=T(Q1+1)+A(Q1,3)
1400 C1=P
1405 N(1,6)=-C
1410 R=Z=0
1415 J=0
1420 Z=0
1425 FOR J=1 TO 6
1430 Z=.1^J
1435 R =R+Z
1440 Y=0
1445 FOR I=1 TO B1
1450 Y=Y+A(I,3)/((1+R/C1)^I)
1455 NEXT I
1460 IF Y-C>0 THEN 1435
1465 R=R-Z
1470 NEXT J
1475 PRINT "AT AFTER TAX RESIDUAL OF $";A(B1,1)/100
1480 PRINT "THE IRR IS ";R*100;"% USING A ";100*R5;"% SF RATE"
1485 PRINT "THE EQUIVALENT PRE-TAX RETURN IS ";R*100/(1-Z1);"%"
1490 PRINT "AGAIN ";
1495 INPUT D$
1500 IF D$= "YES" GOTO 1215
1505 :#### #####.## #####.## #####.## #####.## ######.## ######.## ######.##
1510 :    'LLL     ########.##  ########.##  ########.##  ########.##  #######.##  ########.##  ########.##
1515 N(B1+1,1)=A(B1,1)
1520 FOR Y=2 TO B1
1525 N(Y,2)=-N(Y-1,6)*(R/P)
1530 N(Y,3)=N(Y,1)-N(Y,2)-N(Y,4)
1535 N(Y,6)=N(Y-1,6)+N(Y,3)
1540 N(Y,5)=A(Y-2,2)*(R5/P)
1545 N(Y,7)=A(Y-1,2)
1550 NEXT Y
1555 PRINT "PRINT RECONCILIATION";
1560 INPUT R$
1565 IF R$="NO" THEN 1770
1570 PRINT"PRDLY OR ANNUAL RECONCILIATION";
1575 INPUT U$
1580 U$=LEFT$(U$,1)
1585  PRINT
1590 R6=R*100
1595 PRINT USING 1625," "
1600 PRINT
1605 PRINT USING 1630," "
1610 PRINT
1615 PRINT USING 1635," "
1620 R7=R5*100
1625 :                     -------CASH FLOW FROM LEASE------  'L
1630 :                 --RETURN TO INVESTOR--  -CONTRIBUTION TO S.F.-  'L
1635 : PRD   CASH   EARNINGS PRINCIPL CASH NTO EARN ON SF  INVEST. SINK.FD. 'L
1640 PRINT USING 1645,R6,R7
1645 :      AVAILBL @###.##% REPAYMT  SINK.FD  @###.###%  REMAING  BALANCE
1650 PRINT SPACE$(40);"FIGURES IN HUNDREDS OF DOLLARS"
1655 PRINT
1660 FOR Y=1 TO B1
1665 IF Y=1 GOTO 1715
1670 U=U+1
1675 U1=U1+N(Y,1)
1680 U2=U2+N(Y,2)
1685 U3=U3+N(Y,3)
1690 U4=U4+N(Y,4)
1695 U5=U5+N(Y,5)
1700 U6=N(Y,6)
1705 U7=N(Y,7)
1710 IF U$="A" GOTO 1730
1715 PRINT USING 1505,F1,N(Y,1)/O,N(Y,2)/O,N(Y,3)/O,N(Y,4)/O,N(Y,5)/O,-N(Y,6)/O,N(Y,7)/O
1720 F1=F1+1
1725 GOTO 1755
1730 IF Y=B1 GOTO 1740
1735 IF U<>W1 GOTO 1755
1740 F2=F2+1
1745 PRINT USING 1505,F2,U1/O,U2/O,U3/O,U4/O,U5/O,(-U6)/O,U7/O
1750 U=U1=U2=U3=U4=U5=0
1755 NEXT Y
1760 PRINT
1765 PRINT USING 1510,"SALE",N(B1+1,1)/O,N(B1+1,2)/O,N(B1+1,3)/O,N(B1+1,4)/O,N(B1+1,5)/O,-N(B1+1,6)/O,N(B1+1,7)/O
1770 GOTO 2335
1775 REM DEP ROUTINE+-DDB;2 YRS    SYD REMAINDER
1780 REM NO ITEMS
1785 READ #1,N0,I$
1790 FOR I9=1 TO N0
1795 REM "DEPR AMT, YRS,STARTING YR,PRD
1800 READ #1, X1,X2,Y3,Q3
1805 B3=W1*(Y3-1)+Q3
1810 REM "SALVAGE VALUE,$,DEP METHOD(1=ADR,2=GL,3=SL,4=150%,5=SYD,6=DATA)";
1815 READ #1, X3,X9
1820 X0=X0+X3
1825 ON X9, GOTO 1830,1940,2040,2080,2160,2245
1830 E=0
1835 P1=2*(1/X2)
1840 H(1,1)=X1
1845 FOR Y=1 TO 2
1850 H(2,Y)=H(1,Y)*P1
1855 H(1,Y+1)=H(1,Y)-H(2,Y)
1860 NEXT Y
1865 Q=X2-2
1870 FOR Y=1 TO Q
1875 H=H+Y
1880 NEXT Y
1885 FOR I=Q+1 TO 1 STEP -1
1890 E=E+1
1895 I1=I-1
1900 K=(I1/H)*H(1,3)
1905 K(1,1)=H(1,3)
1910 IF K(1,E)-K<= X3 THEN 1930
1915 K(1,E+1)=K(1,E)-K
1920 H(2,E+2)=K
1925 NEXT I
1930 H(2,E+2)=K(1,E)-X3
1935 GOTO 2270
1940 REM GUIDELINE
1945 E=H=0
1950 P1=2*(1/X2)
1955 H(1,1)=X1
1960 FOR Y = 1 TO 3
1965 H(2,Y)=H(1,Y)*P1
1970 H(1,Y+1)=H(1,Y)-H(2,Y)
1975 NEXT Y
1980 Q=X2-3
1985 FOR Y = 1 TO Q
1990 H = H+Y
1995 NEXT Y
2000 H(1,4)=H(1,4)-X3
2005 FOR I = Q+1 TO 1 STEP -1
2010 E=E+1
2015 I1=I-1
2020 K=(I1/H)*H(1,4)
2025 H(2,E+3)=K
2030 NEXT I
2035 GOTO 2270
2040 REM S/L DEP
2045 E=0
2050 Q=X2
2055 FOR Y= 1 TO Q
2060 H(2,Y)=(X1-X3)/X2
2065 NEXT Y
2070 H(2,Q+1)=X3
2075 GOTO 2270
2080 REM 150% DB,SWITCH YEAR S/B X4
2085 P1=1.5*(1/X2)
2090 H(1,1)=X1
2095 REM "SWITCH YEAR";
2100 READ #1,X4
2105 FOR Y=1 TO X4
2110 H(2,Y)=H(1,Y)*P1
2115 H(1,Y+1)=H(1,Y)-H(2,Y)
2120 NEXT Y
2125 X5=K=0
2130 X5=X2-X4
2135 K=H(1,Y+1)-X3
2140 FOR Y=X4+1 TO X2
2145 H(2,Y)=K/X5
2150 NEXT Y
2155 GOTO 2270
2160 REM SYD
2165 H=0
2170 H(1,1)=X1
2175 FOR Y=1 TO X2
2180 H=H+Y
2185 NEXT Y
2190 FOR I=X2+1 TO 1 STEP -1
2195 E=E+1
2200 I1=I-1
2205 K=(I1/H)*H(1,1)
2210 K(1,1)=H(1,1)
2215 IF K(1,E)-K<=X3 THEN 2235
2220 K(1,E+1)=K(1,E)-K
2225 H(2,E)=K
2230 NEXT I
2235 H(2,E)=K(1,E)-X3
2240 GO TO 2270
2245 REM DATA
2246FOR Y=1 TO X2
2247 READ #1,H(2,Y)
2248 NEXT Y
2249 C=Q3-1
2250 FOR Y=Q3 TO W1
2251 C=C+1
2252 M(C,2)=H(2,1)/(W1+1-Q3)
2253 S4=S4+M(C,2)
2254 NEXT Y
2255 FOR Y=2 TOX2-1
2256 FOR Y1=1 TO W1
2257 C=C+1
2258 M(C,2)=H(2,Y)/W1
2259 S4=S4+M(C,2)
2260 NEXT Y1
2262 NEXT Y
2263 FOR Y=1 TO Q3-1
2264 C=C+1
2265 M(C,2)=H(2,X2)/(Q3-1)
2266 S4=S4+M(C,2)
2267 NEXT Y
2268 GO TO 2322
2270 C=B3-1
2272 FOR X=1 TO X2
2274 H(2,X)=H(2,X)/W1
2276 FOR Y=1 TO W1
2278 C=C+1
2280 M(C,2)=H(2,X)+M(C,2)
2282 S4=S4+M(C,2)
2284 NEXT Y
2286 NEXT X
2287 FOR Y=B3 TO C
2288 Z(Y)=M(Y,2)
2289 NEXT Y
2290 C1=B3-1+W1/2
2292 FOR Y=B3+W1+1-Q3 TO W1+(W1*X2)
2294 C1=C1+1
2296 M(Y,2)=Z(C1)
2298 NEXT Y
2304 FOR Y=B3 TO B3+W1-Q3
2320 M(Y,2)=(Z(B3)*W1/2)/(W1-Q3+1)
2322 NEXT Y
2324 MAT H=ZER
2326 NEXT I9
2328 GO TO 235
2330 REM EARNINGS
2335 PRINT "UNEARNED INC";
2340 INPUT E(1)
2345 IF E(1)=0 THEN 2675
2350 PRINT "ANNUAL CF EARNGS RATE";
2355 INPUT E9
2360 E9=E9/W1
2365 IF C3<5 GOTO 2380
2370 E$="1"
2375 GOTO 2400
2380 PRINT "ITC;1=PASS THRU, 2=AMORTIZED";
2385 INPUT E$
2390 IF E$="1" GOTO 2400
2395 E(6) = I5
2400 FOR P= 1 TO 6 STEP 5
2405 H=0
2410 S=Q1/W1
2415 FOR I=1 TO S
2420 H=H+I
2425 NEXT I
2430 C=0
2435 FOR Y=S+1 TO 1 STEP -1
2440 C=C+1
2445 I1=Y-1
2450 B(C,P)=(I1/H)*E(P)
2455 NEXT Y
2460 IF E$="1" GOTO 2470
2465 NEXT P
2470 A1=A2=A3=A4=A5=0
2475 FOR Y=1 TO Q1
2480 A1=A1+1
2485 A2=A2+M(Y,3)
2490 A3=A3+M(Y,5)
2495 A4=A4+M(Y,9)
2500 B(Y,10)=A4
2505 IF A1<>W1 GOTO 2530
2510 B(Y/W1,2)=A2
2515 B(Y/W1,3)=A3
2520 B(Y/W1,9)=A4
2525 A1=A2=A3=0
2530 NEXT Y
2535 C1=C2=0
2540 T6=0
2545 FOR Y=1 TO Q1+1
2550 C1=C1+1
2555 T6=T6+T(Y)
2560 B(Y,10)=T6
2565 B(Y,11)=B(Y,10)+B(Y,12)
2570 B(Y+1,12)=B(Y,11)*E9
2575 C2=C2+B(Y,12)
2580 IF C1<>W1 THEN 2595
2585 B(Y/W1,7)=C2
2590 C1=C2=0
2595 NEXT Y
2600 PRINT
2605 IF E$<>"1" GOTO 2615
2610 B(C4,6)=I5
2615 PRINT USING 2620,N$
2620 :  YR   INCOME INTEREST    INT. LOSS/GN  TAXES      ITC  C.F.EAR NET EARN '
2625 PRINT
2630 FOR Y=1 TO S
2635 B(Y,4)=B(Y,1)+B(Y,14)-B(Y,2)-B(Y,3)
2640 B(Y,5)=B(Y,4)*Z1
2645 B(Y,8)=B(Y,4)*(1-Z1)+B(Y,6)+B(Y,7)
2650 PRINT USING 2655,Y,B(Y,1)/O,B(Y,2)/O,B(Y,3)/O,B(Y,4)/O,B(Y,5)/O,B(Y,6)/O,B(Y,7)/O,B(Y,8)/O
2655 :#### #####.## #####.## ####.## #####.## ####.## ####.## ####.## #####.##
2660 NEXT Y
2665 GOTO 2335
2670 GOTO 2675
2675 GOTO 5185
2680 PRINT "DATAFILE";
2685 INPUT K$
2690 FILE #1,K$
2695 REM COMPANY NAME
2700 READ #1,L$
2705 REM FINAL YR,PRD
2710 READ #1,Y2,Q2
2715 Q1=W1*(Y2-1)+Q2
2720 MAT M=ZER
2725 MAT P= ZER
2730 MAT T = ZER
2735 MAT N= ZER
2740 GOTO 4295
2745 D$="NO"
2750 REM "ITC",YR,PRD
2755 READ #1, I5,Y3,Q3
2760 C3=W1*(Y3-1)+Q3
2765 C4=Y3
2770 REM "TAX RATE";
2775 READ #1, Z1
2780 REM NO. LOANS
2785 READ #1,N0,I$
2787 IF N0=0 THEN 3105
2790 FOR N9=1 TO N0
2795 REM DEBT TYPE: LEVEL,MORATORIUM,DATA INPUT
2800 READ #1, A$
2805 N=W1
2810 IF LEFT$(A$,1)="L" THEN 2960
2815 IF LEFT$(A$,1)="M" THEN 2835
2820 IF LEFT$(A$,1)="D" THEN 2915
2825 PRINT"ERROR IN DEBT TYPE IN DATA FILE"
2830 STOP
2835 REM MORATORIUM CALCULATION
2840 REM INPUT=PRINCIPLE,INT RATE,STARTING YR,PRD;TOTAL YRS,YRS INT ONLY
2845 READ#1,B1,R,Y2,Q2,Y4,Y3
2850 Q3=Q4=Q2
2855 Y3=Y3+Y2
2860 Y4=Y4+Y2
2865 B2=W1*(Y2-1)+Q2
2870 B3=W1*(Y3-1)+Q3
2875 I6=B1*R/(W1*100)
2880 B4=W1*(Y4-1)+Q4-1
2885 E=B4-B3+1
2890 FOR Y=B2 TO B3-1
2895 M(Y,3)=I6+M(Y,3)
2900 NEXT Y
2905 I7=B3
2910 GOTO 2990
2915 PRINT"DATA INPUT FOR INTEREST: INTEREST,STARTING YR,PRD;ENDING YR,PRDFOR EACH AMT (ALL ZEROES TO END)"
2920 INPUT I6,Y2,Q2,Y3,Q3
2925 IF I6=0 THEN 3040
2930 B2=W1*(Y2-1)+Q2
2935 B3=W1*(Y3-1)+Q3
2940 FOR Y=B2 TO B3
2945 M(Y,3)=I6+M(Y,3)
2950 NEXT Y
2955 GOTO 2920
2960 REM "AMT,RATE,STARTING YR,PRD;NO YRS
2965 READ #1, B1,R,Y2,Q2,S
2970 B3=W1*(Y2-1)+Q2
2975 B4=W1*S+B3-1
2980 E=B4-B3+1
2985 I7=B3
2990 P=(B1*R/N)/(1-(1+(R/N)/100)^(-E))+.5
2995 P=INT(P)/100
3000 B6=P
3005 R=R*100
3010 FOR Y=I7 TO E+I7-1
3015 I1=INT((B1*R)/(N*100)+.5)/100
3020 P1=P-I1
3025 B1=B1-P1
3030 M(Y,3)=I1+M(Y,3)
3035 NEXT Y
3040 NEXT N9
3045 REM D/S
3050 READ #1,I$
3055 READ #1,D1
3056 READ #1,Y2,Q2,Y3,Q3
3060 D2=W1*(Y2-1)+Q2
3065 D3=W1*(Y3-1)+Q3
3070 FOR Y=D2 TO D3
3075 M(Y,4)=D1
3080 NEXT Y
3082 READ #1,I$
3084 IF LEFT$(I$,1)="R" THEN 3110
3086 D1=VAL(I$)
3088 GO TO 3056
3095 B1=0
3100 REM "RENT STARTING YR,PRD;ENDING YR,PRD
3105 READ #1,I$
3110 READ #1,B1
3111 READ #1,Y2,Q2,Y3,Q3
3115 B2=W1*(Y2-1)+Q2
3120 B3=W1*(Y3-1)+Q3
3125 FOR Y=B2 TO B3
3130 M(Y,1)=B1
3135 NEXT Y
3137 READ #1,I$
3139 IF LEFT$(I$,1)="E" THEN 3160
3150 REM "EXP";
3151 B1=VAL(I$)
3153 GO TO 3111
3155 READ #1,I$
3160 READ #1, E,Y2,Q2,Y3,Q3
3165 E2=W1*(Y2-1)+Q2
3170 E3=W1*(Y3-1)+Q3
3175 FOR Y=E2 TO E3
3180 M(Y,5)=E
3185 NEXT Y
3190 IF E3>=Q1 GOTO 3200
3195 GOTO 3160
3200 REM "ANNUAL SF RATE (%)
3205 READ #1, R3
3210 R3=R3/100
3215 REM "FEE,FRONT(F) OR AMORTZD(A)";
3220 READ #1, H6,F$
3225 IF F$="F"  GOTO 3240
3230 H7=H6/Q1
3235 H6=0
3240 REM"EQUITY
3245 READ #1, N(1,6)
3250 REM"NET RESID";
3255 READ #1, K1
3260 PRINT "PRINT FLOWS";
3265 INPUT W$
3270 IF W$="NO" GOTO 3290
3275 PRINT "ANNUAL OR PERIODICAL FLOWS";
3280 INPUT M$
3285 M$=LEFT$(M$,1)
3290 PRINT "CF FILENAME";
3295 INPUT F$
3300 FILE #3,F$
3305 SCRATCH #3
3310 FOR I=1TO4
3315 PRINT
3320 NEXT I
3325 PRINT SPACE$(57-LEN(L$)/2);L$
3330 PRINT SPACE$(50);"LEASE ANALYSIS"
3335 FOR I=1TO3
3340 PRINT
3345 NEXT I
3350 PRINT USING 3355,O$,N$
3355 : YR 'LLL   RENTAL  DEPRECIA-   INTEREST  INTEREST &   OTHER    INTERNAL   TAX LOSS   TAX SAVING AFTER TAX  CUMULATIVE '
3360 PRINT USING 3365,N$
3365 :         INCOME  TION PLUS   ON AMORT  PRINCIPAL   EXPENSES  CASH FLOW  OR GAIN    OR PAYMENT CASH FLOW  AFTER TAX '
3370 PRINT USING 3375, Z1*100
3375 :                 FT END FEES  DEBT     ON DEBT               BEFORE TAX            @###.#%               CASH FLOW
3380 PRINT
3385 PRINT
3390 IF W$="NO" THEN 3400
3395 V1=V2=V3=V4=V5=V6=V7=V8=V9=0
3400 L1=L2=L3=L4=L5=L6=L7=L8=L9=0
3405 FOR Y=1 TO Q1+1
3410 M(Q1+1,1)=K1
3415 L9=L9+1
3420 M(Y,2)=M(Y,2)+H7
3425 M(1,2)=M(1,2)+H6
3430 M(Q1+1,2)=X0
3435 M(Y,6)=M(Y,1)-M(Y,4)-M(Y,5)
3440 T7=M(Y,6)
3445 M(Q1+1,6)=K1
3450 M(Y,7)=M(Y,1)-M(Y,2)-M(Y,3)-M(Y,5)-(T7-M(Y,6))
3455 M(Q1+1,7)=K1-X0
3460 M(Y,8)=-M(Y,7)*Z1
3465 M(Y,9)=M(Y,8)+M(Y,6)
3470 M(C3,9)=M(C3,9)+I5
3475 M(Q1+1,9)=K1-(K1-X0)*Z1
3480 L1=L1+M(Y,1)
3485 L2=L2+M(Y,2)
3490 V1=V1+M(Y,1)
3495 V2=V2+M(Y,2)
3500 V3=V3+M(Y,3)
3505 V4=V4+M(Y,4)
3510 L3=L3+M(Y,3)
3515 L4=L4+M(Y,4)
3520 L5=L5+M(Y,5)
3525 V5=V5+M(Y,5)
3530 V6=V6+M(Y,6)
3535 V7=V7+M(Y,7)
3540 V8=V8+M(Y,8)
3545 L6=L6+M(Y,6)
3550 V9=V9+M(Y,9)
3555 L7=L7+M(Y,7)
3560 L8=L8+M(Y,8)
3565 L0=L0+M(Y,9)
3570 T5=T5+M(Y,9)
3575 M(Y,10)=M(Y,2)+M(Y,3)+M(Y,5)
3580 IF W$="NO" THEN 3640
3585 IF M$="P" THEN 3635
3590 A5=W1
3595 IF Y=Q1+1 THEN 3625
3600 IF Y=Q1 THEN 3610
3605 IF L9<>W1 THEN 3640
3610 PRINT USING 3615,(Y+W1-1)/W1,(Y+W1-1)/W1,L1,L2,L3,L4,L5,L6,L7,L8,L0,T5
3615:### ### ######### ######### ######### ########## ########## ########## ########## ########## ########## ########### ###########
3620:'LLL   ######### ######### ######### ########## ########## ########## ########## ########## ########## ########### ###########
3625 L1=L2=L3=L4=L5=L6=L7=L8=L9=L0=0
3630 GOTO 3640
3635 PRINT USING 3615,INT((Y-1)/W1)+1,FNA(Y),M(Y,1),M(Y,2),M(Y,3),M(Y,4),M(Y,5),M(Y,6),M(Y,7),M(Y,8),M(Y,9),T5
3640 T(Y)=M(Y,9)
3645 WRITE #3, T(Y)
3650 NEXT Y
3655 K2=(K1-X0)*Z1
3660 K3=K1-((K1-X0)*Z1)
3665 PRINT
3670 PRINT
3675 PRINT USING 3620, "SALE",K1,X0,0,0,0,K1,K1-X0,-K2,K3,T5
3680 PRINT
3685 PRINT USING 3690,V1,V2,V3,V4,V5,V6,V7,V8,V9,T5
3TOT ######### ######### ######### ########## ########## ########## ########## ########## ########## ########### ###########
3695 PRINT
3700 B1=Q1+1
3705 FOR Y=2 TO B1+1
3710 N(Y,1)=T(Y-1)
3715 NEXT Y
3720 C=N(1,6)
3725 REM SINKING FUND ROUTINE
3730 MAT A = ZER
3735 FOR Y= 1 TO B1
3740 A(Y,1)=T(Y)
3745 NEXT Y
3750 IF T(Q1+1)>0 THEN 3760
3755 A(Q1,1)=A(Q1,1)+T(Q1+1)
3760   
3765 IF D$="YES" THEN 3775
3770 GOTO 3815
3775 PRINT "SF RATE,INVESTMENT,RESIDUAL";
3780 INPUT R,C,Z2
3785 P=W1
3790 R=R/100
3795 Z3=Z2-((Z2-X0)*Z1)
3800 A(B1,1)=Z3
3805 T(Q1+1)=Z3
3810 GOTO 3825
3815 R=R3
3820 P=W1
3825 N1=Y1=N2=N3=T=N5=0
3830 R5=R
3835 REM CALC OF SINKING FUND
3840 N2=0
3845 FOR Y=Q1 TO 1 STEP-1
3850 IF A(Y,1)+N2<0 THEN  3880
3855 A(Y,3)=A(Y,1)+N2   'RETURNS TO LESSOR
3860 N(Y+1,4)=-N2
3865 A(Y,2)=-N2
3870 N2=0
3875 GOTO 3895
3880 A(Y,2)=-N2
3885 N(Y+1,4)=A(Y,1)
3890 N2=(A(Y,1)+N2)/(1+R/P)   'PREV PERIOD S.F. BAL
3895 NEXT Y
3900 IF T(Q1+1)<0 THEN 3910
3905 A(Q1,3)=T(Q1+1)+A(Q1,3)
3910 C1=P
3915 N(1,6)=-C
3920 R=Z=0
3925 J=0
3930 Z=0
3935 FOR J=1 TO 6
3940 Z=.1^J
3945 R =R+Z
3950 Y=0
3955 FOR I=1 TO B1
3960 Y=Y+A(I,3)/((1+R/C1)^I)
3965 NEXT I
3970 IF Y-C>0 THEN 3945
3975 R=R-Z
3980 NEXT J
3985 PRINT "AT AFTER TAX RESIDUAL OF $";A(B1,1)
3990 PRINT "THE IRR IS ";R*100;"% USING A ";100*R5;"% SF RATE"
3995 PRINT "THE EQUIVALENT PRE-TAX RETURN IS ";R*100/(1-Z1);"%"
4000 PRINT "AGAIN ";
4005 INPUT D$
4010 IF D$= "YES" GOTO 3725
4015 :### ###      ########.##  ########.##  ########.##  ########.##  #######.##  ########.##  ########.##
4020 :    'LLL     ########.##  ########.##  ########.##  ########.##  #######.##  ########.##  ########.##
4025 N(B1+1,1)=A(B1,1)
4030 FOR Y=2 TO B1
4035 N(Y,2)=-N(Y-1,6)*(R/P)
4040 N(Y,3)=N(Y,1)-N(Y,2)-N(Y,4)
4045 N(Y,6)=N(Y-1,6)+N(Y,3)
4050 N(Y,5)=A(Y-2,2)*(R5/P)
4055 N(Y,7)=A(Y-1,2)
4060 NEXT Y
4065 PRINT "PRINT RECONCILIATION";
4070 INPUT R$
4075 IF R$="NO" THEN 4280
4080 PRINT"PRDLY OR ANNUAL RECONCILIATION";
4085 INPUT U$
4090 U$=LEFT$(U$,1)
4095  PRINT
4100 R6=R*100
4105 PRINT USING 4135," "
4110 PRINT
4115 PRINT USING 4140," "
4120 PRINT
4125 PRINT USING 4145," "
4130 R7=R5*100
4135 :                             -------CASH FLOW FROM LEASE------  'L
4140 :                             --RETURN TO INVESTOR--  -CONTRIBUTION TO S.F.-  'L
4145 :    PERIOD       CASH        EARNINGS     PRINCIPAL  CASH INTO  EARN. ON SF   INVESTMENT   SINK. FUND  'L
4150 PRINT USING 4155,R6,R7
4155 :              AVAILABLE      AT###.###%   REPAYMENT  SINK. FUND  AT###.###%   REMAINING     BALANCE
4160 PRINT
4165 PRINT
4170 FOR Y=1 TO B1
4175 IF Y=1 GOTO 4225
4180 U=U+1
4185 U1=U1+N(Y,1)
4190 U2=U2+N(Y,2)
4195 U3=U3+N(Y,3)
4200 U4=U4+N(Y,4)
4205 U5=U5+N(Y,5)
4210 U6=N(Y,6)
4215 U7=N(Y,7)
4220 IF U$="A" GOTO 4240
4225 PRINT USING 4015,INT((F1-1)/W1)+1,FNA(F1),N(Y,1),N(Y,2),N(Y,3),N(Y,4),N(Y,5),-N(Y,6),N(Y,7)
4230 F1=F1+1
4235 GOTO 4265
4240 IF Y=B1 GOTO 4250
4245 IF U<>W1 GOTO 4265
4250 F2=F2+1
4255 PRINT USING 4015,F2,F2,U1,U2,U3,U4,U5,-U6,U7
4260 U=U1=U2=U3=U4=U5=0
4265 NEXT Y
4270 PRINT
4275 PRINT USING 4020,"SALE",N(B1+1,1),N(B1+1,2),N(B1+1,3),N(B1+1,4),N(B1+1,5),-N(B1+1,6),N(B1+1,7)
4280 GOTO 4845
4285 REM DEP ROUTINE+-DDB;2 YRS    SYD REMAINDER
4290 REM NO ITEMS
4295 READ #1,N0,I$
4300 FOR I9=1 TO N0
4305 REM "DEPR AMT, YRS,STARTING YR,PRD
4310 READ #1, X1,X2,Y3,Q3
4315 B3=W1*(Y3-1)+Q3
4320 REM "SALVAGE VALUE,$,DEP METHOD(1=ADR,2=GL,3=SL,4=150%,5=SYD,6=DATA)";
4325 READ #1, X3,X9
4330 X0=X0+X3
4335 ON X9, GOTO 4340,4450,4550,4590,4670,4755
4340 E=0
4345 P1=2*(1/X2)
4350 H(1,1)=X1
4355 FOR Y=1 TO 2
4360 H(2,Y)=H(1,Y)*P1
4365 H(1,Y+1)=H(1,Y)-H(2,Y)
4370 NEXT Y
4375 Q=X2-2
4380 FOR Y=1 TO Q
4385 H=H+Y
4390 NEXT Y
4395 FOR I=Q+1 TO 1 STEP -1
4400 E=E+1
4405 I1=I-1
4410 K=(I1/H)*H(1,3)
4415 K(1,1)=H(1,3)
4420 IF K(1,E)-K<= X3 THEN 4440
4425 K(1,E+1)=K(1,E)-K
4430 H(2,E+2)=K
4435 NEXT I
4440 H(2,E+2)=K(1,E)-X3
4445 GOTO 4780
4450 REM GUIDELINE
4455 E=H=0
4460 P1=2*(1/X2)
4465 H(1,1)=X1
4470 FOR Y = 1 TO 3
4475 H(2,Y)=H(1,Y)*P1
4480 H(1,Y+1)=H(1,Y)-H(2,Y)
4485 NEXT Y
4490 Q=X2-3
4495 FOR Y = 1 TO Q
4500 H = H+Y
4505 NEXT Y
4510 H(1,4)=H(1,4)-X3
4515 FOR I = Q+1 TO 1 STEP -1
4520 E=E+1
4525 I1=I-1
4530 K=(I1/H)*H(1,4)
4535 H(2,E+3)=K
4540 NEXT I
4545 GOTO 4780
4550 REM S/L DEP
4555 E=0
4560 Q=X2
4565 FOR Y= 1 TO Q
4570 H(2,Y)=(X1-X3)/X2
4575 NEXT Y
4580 H(2,Q+1)=X3
4585 GOTO 4780
4590 REM 150% DB,SWITCH YEAR S/B X4
4595 P1=1.5*(1/X2)
4600 H(1,1)=X1
4605 REM "SWITCH YEAR";
4610 READ #1,X4
4615 FOR Y=1 TO X4
4620 H(2,Y)=H(1,Y)*P1
4625 H(1,Y+1)=H(1,Y)-H(2,Y)
4630 NEXT Y
4635 X5=K=0
4640 X5=X2-X4
4645 K=H(1,Y+1)-X3
4650 FOR Y=X4+1 TO X2
4655 H(2,Y)=K/X5
4660 NEXT Y
4665 GOTO 4780
4670 REM SYD
4675 H=0
4680 H(1,1)=X1
4685 FOR Y=1 TO X2
4690 H=H+Y
4695 NEXT Y
4700 FOR I=X2+1 TO 1 STEP -1
4705 E=E+1
4710 I1=I-1
4715 K=(I1/H)*H(1,1)
4720 K(1,1)=H(1,1)
4725 IF K(1,E)-K<=X3 THEN 4745
4730 K(1,E+1)=K(1,E)-K
4735 H(2,E)=K
4740 NEXT I
4745 H(2,E)=K(1,E)-X3
4750 GO TO 4780
4755 REM DATA
4756 FOR Y=1 TO X2
4757 READ #1,H(2,Y)
4758 NEXT Y
4760 C=Q3-1
4761 FOR Y=Q3 TO W1
4762 C=C+1
4763 M(C,2)=H(2,1)/(W1+1-Q3)
4764 S4=S4+M(C,2)
4765 NEXT Y
4766 FOR Y =2 TO X2-1
4767 FOR Y1=1 TO W1
4768 C=C+1
4770 M(C,2)=H(2,Y)/W1
4771 S4=S4+M(C,2)
4772 NEXT Y1
4773 NEXT Y
4774 FOR Y=1 TO Q3-1
4775 C=C+1
4776 M(C,2)=H(2,X2)/(Q3-1)
4777 S4=S4+M(C,2)
4778 NEXT Y
4779 GO TO 4836
4780 IF O0=1 THEN 4805
4781 C=B3-1
4782 FOR X=1 TO W1+1-Q3
4783 C=C+1
4785 M(C,2)=(H(2,1)/2)/(W1+1-Q3)
4787 S4=S4+M(C,2)
4789 NEXT X
4791 O0=1
4793 H=0
4795 X1=X1-(H(2,1)/2)
4797 ON X9 GOTO 4340,4450,4550,4590,4670,4755
4805 FOR X=1 TO X2
4807 H(2,X)=H(2,X)/W1
4809 FOR Y1=1 TO W1
4811 C=C+1
4813 M(C,2)=H(2,X)+M(C,2)
4815 S4=S4+M(C,2)
4817 NEXT Y1
4819 NEXT X
4821 FOR X0=1 TO Q3-1
4823 C=C+1
4825 IF Q3=1 THEN 4831
4827 M(C,2)=H(2,E+2)/(Q3-1)
4829 GO TO 4833
4831 M(C,2)=H(2,E+2)
4833 S4=S4+M(C,2)
4834 NEXT X0
4835 GOTO 2745
4836 NEXT I9
4838 GO TO 2745
4840 REM EARNINGS
4845 PRINT "UNEARNED INC";
4850 INPUT E(1)
4855 IF E(1)=0 THEN 5185
4860 PRINT "ANNUAL CF EARNGS RATE";
4865 INPUT E9
4870 E9=E9/W1
4875 IF C3<5 GOTO 4890
4880 E$="1"
4885 GOTO 4910
4890 PRINT "ITC;1=PASS THRU, 2=AMORTIZED";
4895 INPUT E$
4900 IF E$="1" GOTO 4910
4905 E(6) = I5
4910 FOR P= 1 TO 6 STEP 5
4915 H=0
4920 S=Q1/W1
4925 FOR I=1 TO S
4930 H=H+I
4935 NEXT I
4940 C=0
4945 FOR Y=S+1 TO 1 STEP -1
4950 C=C+1
4955 I1=Y-1
4960 B(C,P)=(I1/H)*E(P)
4965 NEXT Y
4970 IF E$="1" GOTO 4980
4975 NEXT P
4980 A1=A2=A3=A4=A5=0
4985 FOR Y=1 TO Q1
4990 A1=A1+1
4995 A2=A2+M(Y,3)
5000 A3=A3+M(Y,5)
5005 A4=A4+M(Y,9)
5010 B(Y,10)=A4
5015 IF A1<>W1 GOTO 5040
5020 B(Y/W1,2)=A2
5025 B(Y/W1,3)=A3
5030 B(Y/W1,9)=A4
5035 A1=A2=A3=0
5040 NEXT Y
5045 C1=C2=0
5050 T6=0
5055 FOR Y=1 TO Q1+1
5060 C1=C1+1
5065 T6=T6+T(Y)
5070 B(Y,10)=T6
5075 B(Y,11)=B(Y,10)+B(Y,12)
5080 B(Y+1,12)=B(Y,11)*E9
5085 C2=C2+B(Y,12)
5090 IF C1<>W1 THEN 5105
5095 B(Y/W1,7)=C2
5100 C1=C2=0
5105 NEXT Y
5110 PRINT
5115 IF E$<>"1" GOTO 5125
5120 B(C4,6)=I5
5125 PRINT USING 5130,N$
5130 :   YR     INCOME    INTEREST    INTEREST   LOSS/GAIN    TAXES       ITC       C.F. EARN   NET EARNINGS '
5135 PRINT
5140 FOR Y=1 TO S
5145 B(Y,4)=B(Y,1)+B(Y,14)-B(Y,2)-B(Y,3)
5150 B(Y,5)=B(Y,4)*Z1
5155 B(Y,8)=B(Y,4)*(1-Z1)+B(Y,6)+B(Y,7)
5160 PRINT USING 5165,Y,B(Y,1),B(Y,2),B(Y,3),B(Y,4),B(Y,5),B(Y,6),B(Y,7),B(Y,8)
5165 : ####  #######.##  #######.##  #######.##  #######.##  #######.## #######.##  #######.## #######.##
5170 NEXT Y
5175 GOTO 4845
5180 GOTO 5185
5185 END
   _#g�