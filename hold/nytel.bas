90 FILES,WKNG
100PRINT" DOES YOUR FILE CONTAIN NON-BUSY SEASON 10-HIGH"
 110PRINT" DAY DATA (YES OR NO)";
 120INPUTO$
   130IFO$="NO"THEN160
    140PRINT" HOW MANY DAYS";
   150INPUTO6
   160REM VERSION OF 10/2/72
   170PRINT" IN WHAT FILE IS YOUR DATA STORED";
    180INPUTM$
   200 FILE#1,M$
210DIM X(150),Y(150),A(10),B(10),C(50),E(50),F(50),G(50),S(50)
 220DIMU(150),Z(150),V(150),H(200),D(50)
    230DEF FNR(M)=.001*INT(1000*M+.5)
240READ#1,Z7
 250FORI=0TOZ7
260READ#1,X(I)
    280NEXTI
290RESTORE#1
 310FILE#1,M$
 320READ#1,Z7
 330PRINT
340PRINT
350LETN1=Z7
  360LETN=N1-1
 370FORI=0TO3
 380LETS(I)=0
      390NEXTI
400FORI=0TON
 410LETY(0)=X(I)
   420LETY(1)=X(I)^2
 430LETY(2)=Y(0)*Y(1)
   440LETY(3)=Y(1)^2
 450FORJ=0TO3
 460LETS(J)=S(J)+Y(J)
   470NEXTJ
480NEXTI
490LETS=S(0)
 500FORI=0TO3
 510LETY(I)=(1/N1)*S(I)
 520NEXTI
530IFO$="YES"THEN830
   540LETS(1)=Y(1)-Y(0)^2
 550LETS(2)=Y(2)-3*Y(0)*Y(1)+2*Y(0)^3
  560LETS(3)=Y(3)-4*Y(2)*Y(0)+6*Y(1)*Y(0)^2-3*Y(0)^4
   570LETY(1)=SQR(S(1))
   580LETY(2)=S(2)/(S(1)*Y(1))
 590LETY(3)=(S(3)/S(1)^2)-3
  600PRINT" DO YOU WANT SUMMARY STATISTICS(YES OR NO)";
610INPUTR$
   620IFR$="NO"THEN930
    630PRINT
640PRINT
650PRINT"S U M M A R Y   S T A T I S T I C S"
   660PRINT
670PRINT"    VALID DAYS OF DATA =";N1
 680PRINT"  ARITHMETIC MEAN(ABS) =";Y(0)
    690PRINT"    STANDARD DEVIATION =";Y(1)
    700PRINT"              VARIANCE =";S(1)
    710PRINT"    COEFF OF VAR (PCT) =";FNR(100*Y(1)/Y(0))
720PRINT"     STANDARD SKEWNESS = ";FNR(Y(2))
   730PRINT"       STANDARD EXCESS = ";FNR(Y(3))
   740REM
  750REM THE PROGRAM NOW CALCULATES THE CV FACTOR USING THE GAMMA 
    760REM EQUATION FOUND IN THE LATEST SUMMARY OF ENG. GUIDELINES.
770LETL8=((Y(1)/Y(0))*2.55)+1.00
 780PRINT"             CV FACTOR =";L8
 790IFL8>1.300 THEN820
  800IFL8<1.120 THEN820
  810GOTO830
                            820PRINT" ****CHECK CV FACTOR. IT IS >1.38 OR <1.12 ********"
  830LETY(1)=0
 831LETY(0)=0
 832LETO5=Z7-(O6+1)
833FORI=0TOO5
834LETY(1)=Y(1)+X(I)
   835NEXTI
836LETY(0)=Y(1)/(O5+1)
 838PRINT
839FORI=0TON-1
    840FORJ=I+1TON
    850IFX(I)<X(J)THEN 890
 860LETY1=X(I)
870LETX(I)=X(J)
   880LETX(J)=Y1
890NEXTJ
900NEXTI
910PRINT
920PRINT
930PRINT" DO YOU WANT ORDER STATASTICS (YES OR NO )";
940INPUTR$
   950IFR$="NO"THEN1325
   955PRINT
956PRINT
960PRINT"      SMALLEST VARIATE =";X(0)
    970LETP1=10
  980GOSUB 1920
990LETT1=P2
  1000PRINT"          LOWER DECILE =";T1
1010LETP1=25
 1020GOSUB 1920
         1030LETT2=P2
 1040PRINT"        FIRST QUARTILE =";T2
1050LETP1=50
 1060GOSUB 1920
    1070PRINT"                MEDIAN =";P2
1080LETP5=P2
 1090LETP1=75
 1100GOSUB 1920
    1110LETT3=P2
 1120PRINT"        THIRD QUARTILE =";T3
1130LETP1=90
 1140GOSUB 1920
    1150LETT4=P2
 1160PRINT"          UPPER DECILE =";T4
1170PRINT"       LARGEST VARIATE =";X(N)
   1180PRINT
    1190LETU=X(N)-X(0)
1200PRINT"           TOTAL RANGE =";U
 1210PRINT"          DECILE RANGE =";T4-T1
  1220PRINT"   SEMI-QUARTILE RANGE =";(T3-T2)/2
   1230PRINT"     BOWLEY'S SKEWNESS = ";FNR((T3+T2-2*P5)/(T3-T2))
                                    1240PRINT"      PEARSON SKEWNESS = ";FNR(3*(Y(0)-P5)/Y(1))
1250FORI=(N-9)TON
 1260LETK8=X(I)
    1270LETK9=K8+K9
   1280NEXTI
    1290LETK0=K9/10
   1300PRINT"   AVG OF 10 HIGH DAYS =";K0
1310PRINT
    1320PRINT
    1325IFO$="YES"THEN1430
 1330PRINT" AT YOUR REQUEST THE PROGRAM WILL STOP AND ALLOW"
    1340PRINT" YOU TO INCLUDE NON-BUSY SEASON HIGH DAY DATA IN"
    1350PRINT" YOUR FILE. DO YOU WANT THE PROGRAM TO STOP(YES OR NO)";
  1360INPUTR$
  1370IFR$="NO"THEN1430
  1380PRINT
    1390PRINT" THE PROGRAM WILL NOW STOP AND ALLOW YOU TO UPDATE"
  1400PRINT" YOUR DATA FILE."
 1410PRINT
    1420STOP
               1430PRINT"DO YOU WANT A FREQUENCY DISTRABUTION(YES OR NO)";
    1440INPUTS$
  1450IFS$="NO"THEN2070
  1460PRINT
    1470PRINT" FOR A GOOD FREQUENCY DISTRIBUTION, STATE THE RANGE"
 1480PRINT" BETWEEN TWO TYPICAL DATA POINTS BY ENTERING THE LOWEST"
  1490PRINT" FIRST, AND THEN THE HIGHER. FOR EXAMPLE,"
 1495PRINT" 4,6;OR 110,120,OR 2400,2700 ETC.."
   1500INPUTW1,W2
    1510LETD=ABS(W2-W1)
    1520IFD=0THEN 2060
1530 LET Y1=W1-INT((W1-X(0))/D+.99999)*D
   1540 LET L=INT((X(N)-Y1+.00001)/D)+1
  1550 IF L>50 THEN 2010
 1560 FOR I=0 TO L
 1570 LET C(I)=Y1+I*D
   1580 LET F(I)=0
   1590 LET E(I)=0
   1600 NEXT I
       1610REM THE PROGRAM NOW CALCULATES A FREQUENCY DISTRIBUTION
    1620REM USING THE BUSY SEASON, PLUS HIGH-DAY INCLUDED DATA
1630PRINT
    1640PRINT
    1650REM
 1660PRINT
    1670PRINT
    1680PRINT"F R E Q U E N C Y   D I S T R I B U T I O N"
    1690PRINT
    1700PRINT"            UP TO BUT","","PERCENT"
   1710PRINT"FROM       NOT INCLUDING   FREQUENCY","FREQUENCY"
    1720PRINT
    1730FORI=0TON
1740LETH=1+INT((X(I)-C(0))/D+1E-6)
    1750LETF(H)=F(H)+1
1760LETE(H)=E(H)+X(I)
  1770NEXTI
    1780LETJ=0
   1790LETJ=J+1
 1800LETG(J)=100*F(J)/N1
1810PRINTC(J-1),C(J),F(J),FNR(G(J))
   1820 IF J=L THEN 1840
       1830GOTO 1790
1840FORJ=2TOL
1850LETF(J)=F(J-1)+F(J)
1860 LET F(J)=100*F(J)/N1
   1870LETE(J)=E(J-1)+E(J)
1880NEXTJ
    1890PRINT
    1900GOSUB2070
1910RETURN
   1920LETG5=P1*(N1+1)/100
1930LETP2=X(0)
    1940IFG5<1THEN 2000
    1950LETP2=X(N1-1)
 1960IFG5>N1THEN 2000
   1970LETQ5=INT(G5)
 1980 LET H5=G5-Q5
 1990LETP2=H5*X(Q5)+(1-H5)*X(Q5-1)
2000RETURN
   2010PRINT
    2020 PRINT"MINIMUMIS 50 CLASSES."
2030 PRINT"RESPECIFY L,U";
  2040 INPUT W1,W2
  2050GOTO1510
 2060GOTO2070
 2070PRINT
    2080PRINT" DO YOU WANT CRITICAL RATIOS (YES OR NO)";
 2090INPUTR$
  2100IFR$="NO"THEN2320
                           2120PRINT "        T H E    C R I T I C A L   R A T I O E S"
   2125PRINT
    2130PRINT"   DAY","ACTUAL VALUE","RATIO TO ABS"
 2140PRINT
    2150FORI=N-9TON
   2160LETR0=X(I)
    2170LETF9=I
  2180LETB0=N
  2190LETC9=(B0+1)-F9
    2210PRINT"   ";C9;"","";R0;"","";R0/Y(0)
   2220NEXTI
    2230PRINT
    2240PRINT"THE AVG 10 HI DAY TO ABS RATIO=";(K0/Y(0))*1.000
2250PRINT
    2260PRINT
    2270PRINT
    2280PRINT
    2290PRINT
    2300PRINT
    2310PRINT
    2320PRINT" DO YOU WANT A LOAD SERVICE CURVE (YES OR NO)";
 2330INPUTT8$
 2340IFT8$="NO"THEN2860
 2350FORI=1TO6
2360LETS(I)=0
2370NEXTI
                   2380IFT8$="NO"THEN2860
 2390REM THE PROGRAM  NOW FITS THE USAGE AND SERVICE DATA
  2400PRINT"    L O A D  S E R V I C E  C U R V E  ( D A I L Y  )"
    2405PRINT
    2410PRINT"";,;"";,;"EXPECTED";,;"PERCENT"
  2420PRINT"LOAD";,;"SERVICE";,;"SERVICE";,;"DIFFEREFCE"
    2430FORI=1TOZ7
    2440READ#1,X(I)
   2450NEXTI
    2460FORI=1TOZ7
    2470READ#1,Y(I)
   2480NEXTI
    2490LETN=Z7
  2500LETD1=1
  2510FORJ=1TON
2520LETV(J)=LOG(Y(J))
  2530LETS(5)=S(5)+V(J)^2
2540LETS(3)=S(3)+V(J)
  2550NEXTJ
    2560FORJ=1TON
2570LETU(J)=X(J)
  2580LETS(1)=S(1)+U(J)
  2590LETS(2)=S(2)+U(J)^2
2600LETS(4)=S(4)+U(J)*V(J)
       2610NEXTJ
    2620LETB=(N*S(4)-S(1)*S(3))/(N*S(2)-(S(1)^2))
   2630LETA=(S(3)-B*S(1))/N
    2640FORI=1TON-1
   2650LETM=I
   2660FORJ=I+1TON
   2670 IFX(M)<=X(J)THEN 2690
  2680LETM=J
   2690NEXTJ
    2700IFM=ITHEN2770
 2710LETP=X(M)
2720LETQ=Y(M)
2730LETX(M)=X(I)
  2740LETY(M)=Y(I)
  2750LETX(I)=P
2760LETY(I)=Q
2770NEXTI
    2780FORJ=1TONSTEP3
2790LETY=EXP(A)*EXP(B*X(J))
 2800PRINT
    2810PRINTX(J),Y(J),Y,
  2820LETD=Y(J)-Y
   2830LETD=.1*SGN(D)*INT(1000*ABS(D/Y))
 2840PRINTD
   2850NEXTJ
    2860RESTORE#1
2880FORI=1TO10
    2890LETS(I)=0
2900NEXTI
    2910FILE#2,"WKNG"
                               2920PRINT" DO YOU WANT A TREND (YES OR NO)";
    2930INPUTR$
  2940IFR$="NO"THEN5480
  2950PRINT
    2960READ#1,Z7
2970FORI=1TOZ7
    2980READ#1,X(I)
   2990READ#1,Y(I)
   3000NEXTI
    3010READ#1,N
 3020READ#1,M1
3030SCRATCH#2
3040FORI=1TON
3050READ#1,Y(I)
   3060WRITE#2,Y(I)
  3070NEXTI
    3080LETO2=Y(0)
    3090WRITE#2,O2
    3100FORI=1TON
3110READ#1,X(I)
   3120WRITE#2,X(I)
  3130NEXTI
    3140LETO3=X(N)+1
  3150WRITE#2,O3
    3160FORI=1TOM1
    3170LETZ8=I
  3180LETZ(I)=O3+Z8
 3190WRITE#2,Z(I)
  3200NEXTI
    3210RESTORE#2
3220FORI=1TON+1
   3230READ#2,Y(I)
   3240NEXTI
    3250 FOR I=1TON+1
 3260READ#2,X(I)
   3270NEXTI
    3280FORI=1TOM1
    3290READ#2,Z(I)
   3300NEXTI
    3310LETD2=0
  3320LETD1=1
  3330FORK=1TO6
3331LETF(K)=1
3332NEXTK
    3333RESTORE
  3334PRINT
    3340PRINT
    3350PRINT" YOUR BEST FITTED TREND CURVE IS FOUND WHERE THE INDEX"
   3360PRINT" OF DETERMINATION IS CLOSEST TO'1' AND THERE IS NOT A"
    3370PRINT" ZERO IN THE A OR B COLUMNS"
3380PRINT
    3390PRINT
    3400PRINT"    D E T E R M I N A T I O N    M A T R I X"
   3410PRINT
    3420PRINT"CURVE TYPE","  INDEX  OF","   A","   B"
    3430PRINT" ","DETERMINATION"
3440PRINT
    3450PRINT
    3460FORI=1TO6
               3470FORI1=1TO6
    3480LETS(I1)=0
    3481REM
 3490NEXTI1
   3500GOSUB4570
3510IF(I-5)*(I-6)=0THEN3660
 3520IF(I-2)*(I-3)=0THEN3590
 3530FORJ=1TON+1
   3540LETV(J)=Y(J)
  3550GOSUB4350
3560NEXTJ
    3570IFI=1THEN3760
 3580GOTO3870
 3590FORJ=1TON+1
   3600IFY(J)<=0THEN3730
  3610LETV(J)=LOG(Y(J))
  3620GOSUB4350
3630NEXTJ
    3640IFI=3THEN3820
 3650GOTO3760
 3660FORJ=1TON+1
   3670IFY(J)=0THEN3730
   3680LETV(J)=1/Y(J)
3690GOSUB4350
3700NEXTJ
    3710IFI=6THEN3870
 3720GOTO3760
 3730PRINT"CAN'T FIT"
   3740LETF(I)=0
3750GOTO3940
 3760FORJ=1TON+1
   3770LETU(J)=X(J)
  3780GOSUB4380
3790NEXTJ
         3800GOTO3920
 3810FORJ=1TON+1
   3820IFX(J)<=0THEN3730
  3830LETU(J)=LOG(U(J))
  3840GOSUB4380
3850NEXTJ
    3860GOTO3920
 3870FORJ=1TON+1
   3880IFX(J)=0THEN3730
   3890LETU(J)=1/X(J)
3900GOSUB4380
3910NEXTJ
    3920GOSUB4990
3930PRINTC(I),A(I),B(I)
3940NEXTI
    3950IFD<>1THEN3990
3970PRINT
    3980PRINT
    3990PRINT"DETAILS FOR";
4000INPUTI
   4005 IF I=0 THEN 5480
  4010PRINT
    4020PRINT
    4030LETK=I
   4040LETD1=1
  4090 GOSUB4750
    4100PRINT"      OF A LEAST-SQUARES FIT OF ITS LINEAR TRANSFORM"
4110FORJ=1TON+1STEP1
   4120LETY=A(I)+B(I)*X(J)
4130IFI=1THEN4170
 4140LETY=1/Y
                4150IFI=5THEN4170
 4160LETY=X(J)*Y
   4170GOSUB5140
4180NEXTJ
    4190LETD=D1
  4200GOTO5380
 4210GOTO3990
 4220FORJ=1TON+1STEP1
   4230IFI=2THEN4290
 4240IFI=3THEN4270
 4250LETY=A(4)+B(4)/X(J)
4260GOTO4300
 4270LETY=A(3)*(X(J)^B(3))
   4280GOTO4300
 4290LETY=A(2)*EXP(B(2)*X(J))
4300GOSUB5140
4310NEXTJ
    4320LETD=D1
  4330GOTO5260
 4340GOTO3990
 4350LETS(5)=S(5)+V(J)^2
4360LETS(3)=S(3)+V(J)
  4370RETURN
   4380LETS(1)=S(1)+U(J)
  4390LETS(2)=S(2)+U(J)^2
4400LETS(4)=S(4)+U(J)*V(J)
  4410RETURN
   4570LETK=I
   4580IFK=1THEN4730
 4590IFK=2THEN4710
 4600IFK=3THEN4690
 4610IFK=4THEN4670
                4620IFK=5THEN4650
 4630PRINT"6. Y=X/(A+B*X) ";
 4640RETURN
   4650PRINT"5. Y=1/(A+B*X) ";
 4660RETURN
   4670PRINT"4. Y=A+(B/X)",
    4680RETURN
   4690PRINT"3. Y=A*(X^B)",
    4700RETURN
   4710PRINT"2. Y=A*EXP(B*X)";
 4720RETURN
   4730PRINT"1. Y=A+(B*X)",
    4740RETURN
   4750PRINT"   ";
   4760GOSUB4580
4770PRINT" IS A";
 4780IFK=1THEN4830
 4790IFK=2THEN4850
 4800IFK=3THEN4870
 4810PRINT" HYPERBOLIC";
4820GOTO4880
 4830PRINT" LINEAR";
    4840GOTO4880
 4850 PRINT "DUMMY"
4860GOTO4880
 4870PRINT" POWER";
4880PRINT" FUNCTION.  THE RESULTS"
    4890 IF K=1THEN4910
    4910IFD<>1THEN4930
               4920PRINT"      (SORTED IN ORDER OF ASCENDING VALUES OF X)"
    4930PRINT"       ARE AS FOLLOWS:"
4940PRINT
    4950PRINT"    F I T T E D   T R E N D   C U R V E"
   4960PRINT
    4970PRINT"YEAR";,;"ACTUAL DATA";,;"CALC. DATA";,;"PCT. DIFFER"
 4980RETURN
   4990LETB=((N+1)*S(4)-S(1)*S(3))/((N+1)*S(2)-(S(1)^2))
5000LETA=(S(3)-B*S(1))/(N+1)
5010LETS1=S(5)-(S(3)^2)/(N+1)
    5020LETS2=(B^2)*(S(2)-(S(1)^2)/(N+1))
 5030LETC(I)=S2/S1
 5040IF(I-1)*(I-4)*(I-5)=0THEN5110
5050IF(I-2)*(I-3)=0THEN5090
 5060LETA(6)=B
5070LETB(6)=A
5080RETURN
   5090LETA(I)=EXP(A)
5100GOTO5120
 5110LETA(I)=A
5120LETB(I)=B
5130RETURN
        5140PRINTX(J),Y(J),Y,
  5150LETD=Y(J)-Y
   5160LETD=.1*SGN(D)*INT(1000*ABS(D/Y))
 5170IFD<0THEN5220
 5180IFD>0THEN5210
 5190PRINT"      0"
5200RETURN
   5210PRINT"       ";
    5220PRINTD
   5230RETURN
   5240PRINT" THERE APPEARS TO BE GARBAGE IN YOUR INPUT-CHECK IT"
 5250STOP
5260PRINT"PREDICTIONS"
 5270FORJ=1TOM1
    5280IFI=2THEN5340
 5290IFI=3THEN5320
 5300LETY=A(4)+B(4)/Z(J)
5310GOTO5350
 5320LETY=A(3)*(Z(J)^B(3))
   5330GOTO5350
 5340LETY=A(2)*EXP(B(2)*Z(J))
5350PRINTZ(J)," --            ";Y
5360NEXTJ
    5370GOTO3990
 5380PRINT"PREDICTIONS"
 5390FORJ=1TOM1
    5400LETY=A(I)+B(I)*Z(J)
               5410IFI=1THEN5450
 5420LETY=1/Y
 5430IFI=5THEN5450
 5440LETY=Z(J)*Y
   5450PRINTZ(J)," --            ";Y
5460NEXTJ
    5470GOTO3990
 5480END
 