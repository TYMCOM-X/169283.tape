1000 DIM L1$(100),M1(100),Q(100),P(100),E(100),H(100),H1(100)
1010 DIM C(100),M(100),V(100),X(100),N(100),L1(100),B3(100),A2(100)
1020 DIM X9(100),H6(100),A1(100),B9(100),C1(100),C2(100),C3(100),A3(100)
1030 DIM C4(100),C5(100),C6(100),C7(100),C8(100),C9(100),B8(100),A7(100)
1040 DIM A8(100),A9(100),B5(100),B2(100),B1(100)
1050 DIM X8(100),A6(100),F1(100),F2(100),F3(100),F4(100),F5(100)
1051 DIM F6(100)
1060 PRINT "*****DATA FILE NAME IS";
1070 INPUT Z1$
1072 FILE #1,Z1$
1080 PRINT "****** HOW MANY TOTAL ASSEMBLIES ";
1090 INPUT N1
1100 FOR S=1 TO N1
1110 L1(S)=1
1120 NEXT S
1130 B=24
1140 G=24 
1150 Z=45
1160 A4=.5
1170 A5 =.5
1180 B4 =.1
1190 S1=6
1200 S2=6
1210 D=.1126
1220 K1=1600
1230 K7=22
1240 W=.06
1250 J=900000
1260 Y=2350
1270 L=120
1280 PRINT 
1290 PRINT 
1300 X2=0
1310 PRINT 
1320 X3=0
1330 X4=0
1340 FOR I =1 TO N1
1350 INPUT #1:L1$(I),M1(I),Q(I),P(I),E(I),H(I),H1(I),C(I),M(I),V(I),X(I),N(I)
1370 X9(I)=(G*Z*Q(I)*L)/(M1(I)/2)
1380 X8(I)=X9(I)*M(I)
1390 NEXT I
1400 FOR U=1 TO N1
1410 T=T+X8(U)
1420 NEXT U
1430 FOR U1=1 TO N1
1440 B3(U1)=X8(U1)/T
1450 NEXT U1
1460 
1470 X2=X2+1
1480 PRINT
1490 PRINT"********************* ITERATION NUMBER";X2;" **********************************"
1500 PRINT
1510 PRINT"** ASSEMBLY **"
1520 PRINT"              *** DAF ***   *** REPAIR ***  *** AGE % ***"
1530 PRINT
1540 FOR K=1 TO N1
1550 IF B3(K)=0 GO TO 1840
1560 A2(K)=M1(K)/2
1570 A1(K)=(120*G*Z*P(K))/A2(K)
1580 A3(K)=(120*G*Z*Q(K)*(.284*E(K)*A4+.76*E(K)*A5))/A2(K)
1590 A6(K)=A1(K)+A3(K)
1600 A7(K)=.33*P(K)*G*Z*Q(K)/A2(K)
1610 A8(K)=120*G*Z*Q(K)*C(K)/A2(K)
1620 A9(K)=1080*G*Z*Q(K)*M(K)/A2(K)
1630 B9(K)=((G*Z*Q(K)*P(K))/A2(K))*(1.5+A4+2*A5)
1640C1(K)=(.5*G*Z*Q(K)*P(K))/A2(K)
1650 C2(K)=(120*G*Z*Q(K)*C(K))/A2(K)
1660 C3(K)=(1200*G*Z*Q(K)*M(K))/A2(K)
1670 C4(K)=2*A3(K)
1680 C5(K)=(B3(K)*(J+Y+10*B4*J)+((N(K)*K7)/B))/B+((N(K)*K7)/B)
1690 C6(K)=(1.9*S2*D*K1)/B
1700 C7(K)=(160*X(K))/B
1710 C8(K)=(1030*(H(K)-1+H1(K)))/B+((120*V(K))/B)
1720 C9(K)=C1(K)+C2(K)+C3(K)+C4(K)+C5(K)+C6(K)+C7(K)+C8(K)+((G*Z*Q(K)*P(K))/A2(K))*(1.5+A4+2*A5)
1740 X9(K)=(G*Z*Q(K)*L)/A2(K)
1750 IF A6(K)>C9(K) GO TO 1780
1760 L1(K)=0
1770 X3=1
1780 IF X2=1 GO TO 1800
1790 IF L1(K)=0 GO TO 1840
1800 IF L1(K)=2 GO TO 1840
1810  PRINT
1820 PRINT L1$(K)
1821 IF A6(K)>C9(K) GO TO 1824
1822 X6$="DISCARD"
1823 GO TO 1830
1824 X6$="REPAIR"
1830 PRINT USING 1831,X6$,A6(K),C9(K),B3(K)*100
1831 :  'LLLLLL $$$,$$$,$$$.$$   $$$,$$$,$$$.$$    ###.####
1840 NEXT K
1850 FOR Y =1 TO N1
1860 IF X4<>1 GO TO 1890
1870 IF L1(Y)=3 GO TO 1940
1880 GO TO 3260
1890  IF X2<>1GO TO 1910
1900 IF X3=0 GO TO 1940
1910 IF L1(Y)=0 GO TO 1940
1920 GO TO 3260
1930 
1940 PRINT
1960 PRINT "===================================================================="
1970 PRINT"    OPTIMUM REPAIR LEVEL ANALYSIS, DISCARD VS. DEPOT REPAIR"
1980 PRINT TAB(17);L1$(Y)
1990 PRINT
2000 PRINT TAB(11);"***  AGE ITERATION NUMBER ";X2;" ***"
2010 PRINT"                  INPUT DATA VALUES"
2020 PRINT 
2030 PRINT "B=";B,"KZ=";K7,"J=";J
2040 PRINT "C=";C(Y),"SD=";S2,"%=";B3(Y)*100
2050 PRINT "E=";E(Y),"V=";V(Y),"G=";G
2060 PRINT "H=";H(Y),"W=";W,"D=";D
2070 PRINT "H'=";H1(Y),"Z=";Z,"N=";N(Y)
2080 PRINT "KT=";K1,"MTBD=";INT(A2(Y))
2090 PRINT "L=";L,"Q=";Q(Y)
2100 PRINT "M=";M(Y),"% CONUS=";A4
2110 PRINT "P=";P(Y),"% OS=";A5
2120 PRINT "X=";X(Y),"MTBF=";M1(Y)
2130 PRINT "LIFE CYCLE DEMANDS =";X9(Y)
2140 PRINT "                      TOTAL COST ANALYSIS"
2150 PRINT
2160 PRINTUSING2170,A6(Y)
2170 : TOTAL COST TO DISCARD AT FAILURE IS           $ ##,###,###.##
2180 PRINT
2190PRINT USING 2200,C9(Y)
2200 : TOTAL COST TO REPAIR AT DEPOT LEVEL IS        $ ##,###,###.##
2210 PRINT
2230 : SPARES                  $##,###,###.##   $##,###,###.##
2240 : SAFETY STOCK                              ##,###,###.##
2250 : REPAIR PARTS                              ##,###,###.##
2260 : REPAIR MANHOURS                           ##,###,###.##
2270 : TRANSPORTAION            ##,###,###.##    ##,###,###.##
2280 : AGE                                       ##,###,###.##
2290 : TRAINING                                  ##,###,###.##
2300 : SUPPLY ADMIN.                             ##,###,###.##
2310 : TECH DATA                                 ##,###,###.##
2330 : TOTALS                 $##,###,###.##   $##,###,###.##
2340 PRINT 
2350 
2360PRINT "                      SYSTEM ANALYSIS"
2370 PRINT
2380 PRINT
2390 PRINT "   COST ELEMENTS                DAF              DEPOT"
2400 PRINT
2410 PRINT USING 2230,A1(Y),B9(Y)
2420 PRINT USING 2240 ,C1(Y)
2430 PRINT USING 2250,C2(Y)
2440 PRINT USING 2260,C3(Y)
2450 PRINT USING 2270 ,A3(Y),C4(Y)
2460 PRINT USING 2280,C5(Y)
2470 PRINT USING 2290,C6(Y)
2480 PRINT USING 2300,C8(Y)
2490 PRINT USING 2310,C7(Y)
2500 PRINT "                          -------------    --------------"
2510 PRINT USING 2330,A6(Y),C9(Y)
2530 : MTBD             LO     $##,###,###.##   $##,###,###.##
2540 :                  HI      ##,###,###.##    ##,###,###.##
2550 : MANHOURS/REPAIR  LO      ##,###,###.##    ##,###,###.##
2560 : TRAINING COST    LO      ##,###,###.##    ##,###,###.##
2570 : UNIT PRICE       LO      ##,###,###.##    ##,###,###.##
2580 : AGE COST         LO      ##,###,###.##    ##,###,###.##
2590 : OPER. ELEMENTS   LO      ##,###,###.##    ##,###,###.##
2600 : UTILIZATION RATE LO      ##,###,###.##    ##,###,###.##
2610 PRINT
2620 PRINT
2630PRINT "                      SENSITIVITY ANALYSIS"
2640 PRINT
2650 PRINT "   SENSITIVITY TESTS            DAF              DEPOT"
2660 PRINT
2670  F1(Y)=A6(Y)-.33*A6(Y)
2680  F2(Y)=2*A6(Y)
2690 
2700  F3(Y)=B8(Y)-(.33*(A7(Y)+A8(Y)+A9(Y)+B1(Y)))-B2(Y)*((-.33*B3(Y)+.33)/(1-.33*B3(Y)))
2710  F4(Y)=B8(Y)+A7(Y)+A8(Y)+A9(Y)+B1(Y)+B2(Y)*((1-B3(Y))/(1+B3(Y)))
2720  F5(Y)=C9(Y)-.33*(B9(Y)+C1(Y)+C2(Y)+C3(Y)+C4(Y))-C5(Y)*((-.33*B3(Y)+.33)/(1-.33*B3(Y)))
2730  F6(Y)=C9(Y)+B9(Y)+C1(Y)+C2(Y)+C3(Y)+C4(Y)+C5(Y)*((1-B3(Y))/(1+B3(Y)))
2740 PRINT USING2530,F2(Y),F6(Y)
2750 PRINT USING2540,F1(Y),F5(Y)
2760  F1(Y)=B8(Y)+.5*A9(Y)
2770  F2(Y)=B8(Y)-.5*A9(Y)
2780  F3(Y)=C9(Y) +.5*C3(Y)
2790  F4(Y)=C9(Y)-.5*C3(Y)
2800 PRINT USING2550,A6(Y),F4(Y)
2810 PRINT USING2540,A6(Y),F3(Y)
2820  F1(Y)=B8(Y)+.5*B5(Y)
2830  F2(Y)=B8(Y)-.5*B5(Y)
2840  F3(Y)=C9(Y)+.5*C6(Y)
2850  F4(Y)=C9(Y)-.5*C6(Y)
2860 PRINT USING2560,A6(Y),F4(Y)
2870 PRINT USING2540,A6(Y),F3(Y)
2880  F1(Y)=A6(Y)+.5*A1(Y)
2890  F2(Y)=A6(Y)-.5*A1(Y)
2900  F3(Y)=B8(Y)+.5*(A7(Y)+A8(Y))
2910  F4(Y)=B8(Y)-.5*(A7(Y)+A8(Y))
2920  F5(Y)=C9(Y)+.5*(B9(Y)+C1(Y)+C2(Y))
2930  F6(Y)=C9(Y)-.5*(B9(Y)+C1(Y)+C2(Y))
2940 PRINT USING 2570,F2(Y),F6(Y)
2950 PRINT USING 2540,F1(Y),F5(Y)
2960  F1(Y)=B8(Y)+.5*B2(Y)
2970  F2(Y)=B8(Y)-.5*B2(Y)
2980  F3(Y)=C9(Y)+.5*C5(Y)
2990  F4(Y)=C9(Y)-.5*C5(Y)
3000 PRINT USING2580,A6(Y),F4(Y)
3010 PRINT USING2540,A6(Y),F3(Y)
3020  F1(Y)=A6(Y)+.25*A6(Y)
3030  F2(Y)=A6(Y)-.25*A6(Y)
3040  F3(Y)=B8(Y)+.25*(A7(Y)+A8(Y)+A9(Y)+B1(Y))+B2(Y)*((.25-.25*B3(Y))/(1+.25*B3(Y)))
3050  F4(Y)=B8(Y)-.25*(A7(Y)+A8(Y)+A9(Y)+B1(Y))-B2(Y)*((.25-.25*B3(Y))/(1-.25*B3(Y)))
3060  F5(Y)=C9(Y)+.25*(B9(Y)+C1(Y)+C2(Y)+C3(Y)+C4(Y))+C5(Y)*((.25-.25*B3(Y))/(1+.25*B3(Y)))
3070  F6(Y)=C9(Y)-.25*(B9(Y)+C1(Y)+C2(Y)+C3(Y)+C4(Y))-C5(Y)*((.25-.25*B3(Y))/(1-.25*B3(Y)))
3080 PRINT USING2590,F2(Y),F6(Y)
3090 PRINT USING2540,F1(Y),F5(Y)
3100  F1(Y)=A6(Y)+.5*A6(Y)
3110  F2(Y)=A6(Y)-.5*A6(Y)
3120  F3(Y)=B8(Y)+.5*(A7(Y)+A8(Y)+A9(Y)+B1(Y))+B2(Y)*((.5-.5*B3(Y))/(1+.5*B3(Y)))
3130  F4(Y)=B8(Y)-.5*(A7(Y)+A8(Y)+A9(Y)+B1(Y))-B2(Y)*((.5-.5*B3(Y))/(1-.5*B3(Y)))
3140  F5(Y)=C9(Y)+.5*(B9(Y)+C1(Y)+C2(Y)+C3(Y)+C4(Y))+C5(Y)*((.5-.5*B3(Y))/(1+.5*B3(Y)))
3150  F6(Y)=C9(Y)-.5*(B9(Y)+C1(Y)+C2(Y)+C3(Y)+C4(Y))-C5(Y)*((.5-.5*B3(Y))/(1-.5*B3(Y)))
3160 PRINT USING2600,F2(Y),F6(Y)
3170 PRINT USING2540,F1(Y),F5(Y)
3180 PRINT 
3190  IF A6(Y)<C9(Y) GO TO 3230
3200 L9(Y)=C9(Y)/((G*Z*Q(Y)*120)/A2(Y))
3210 PRINT USING 3220,L9(Y)
3220 :    REPAIRED MODULE UNIT PRICE IS  $ 99,999,999.99
3230 PRINT
3240PRINT"======================================================================"
3250 L1(Y)=2
3260 NEXT Y
3270 IF X3=0 GO TO 3410
3280 T=0
3290 FOR U3 =1 TO N1
3300 IF L1(U3)=2 GO TO 3320
3310 T=T+X8(U3)
3320 NEXT U3
3330 FOR U4=1 TO N1
3340 IF L1(U4)=1 GO TO 3370
3350 B3(U4)=0
3360 GO TO 3380
3370 B3(U4)=X8(U4)/T
3380 NEXT U4
3390 X3=0
3400 GO TO  1470
3410 IF X4=1 GO TO 3480 
3420 X4=1
3430 FOR U5=1 TO N1
3440 IF L1(U5)<>1 GO TO 3460
3450 L1(U5)=3
3460 NEXT U5
3470 GO TO 1850
3480 PRINT "END OF ORLA ITERATIONS "
3490 END
    