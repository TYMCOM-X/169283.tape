10 PRINT "TYPE 0 FOR HEADING";
20 INPUT Q
30 LET N5=8
40 LET N4=8
50 RANDOMIZE
60 IF Q<>0 THEN 340
70 PRINT
71 PRINT "CONGRATULATIONS! YOUVE BEEN MADE PREMIER OF SETATS DETINU,"
72 PRINT "A SMALL COMMUNIST ISLAND 30 BY 70 MILES LONG.  YOUR JOB IS"
73 PRINT "TO DECIDE THE COUNTRY'S FATE."
74 PRINT
80 PRINT "THE MONEY SYSTEM IS RALLODS.  EACH COUNTRYMEN MUST HAVE AT"
81 PRINT "LEAST 100 RALLODS/YEAR TO SURVIVE."
90 PRINT
91 PRINT "YOUR COUNTRY'S INCOME COMES FROM FARM PRODUCE, TOURISTS,"
92 PRINT "AND TAXES.  HALF YOUR LAND IS FARM LAND WHICH MAY BE SOLD"
93PRINT"TO FOREIGN INDUSTRY WHO IMPORT AND SUPPORT THEIR OWN WORKERS."
94 PRINT
100 PRINT "CROPS COST BETWEEN 10 AND 15 RALLODS PER SQ. MILE TO PLANT,"
101 PRINT "AND THEY BRING IN BETWEEN 45 AND 65 RALLODS PER SQ. MILE"
110 PRINT
111 PRINT "INDUSTRY WILL BUY LAND FROM 95 TO 105 RALLODS PER SQ. MILE,"
112 PRINT "AND WILL PAY TAXES OF 44 RALLODS PER SQ. MILE OF LAND."
113 PRINT "INDUSTRY  BRINGS TOURIST TRADE.  IT MUST OWN AT LEAST 2 SQ."
114 PRINT "MILES BEFORE YOU GET ANY TOURIST TRADE, AND INCOME FROM"
115 PRINT "YOUR TOURIST TRADE WILL INCREASE UNTIL INDUSTRY OWNS "
116 PRINT "26 SQ. MILES OF LAND."
117 PRINT "INDUSTRY ALSO BRINGS POLLUTION, WHICH IS MEASURED IN UNITS."
118 PRINT "POLLUTION MAY BE COMBATTED BY SPENDING .44 RALLODS PER UNIT"
119 PRINT "OF POLLUTION.  POLLUTION WILL DECREASE TOURIST TRADE AND"
120 PRINT "CROP YIELD."
121 PRINT
130 PRINT "YOU CAN INCREASE YOUR CROP YIELD THROUGH EDUCATION.  THE"
131 PRINT "VALUE OF CROPS IS DEPENDENT ON THE AMOUNT OF MONEY USED FOR"
132 PRINT "EDUCATION IN THE PAST 3 YEARS.  10 RALLODS PER PERSON IS"
133 PRINT "THE MAXIMUM USEFUL AMOUNT TO SPEND ON EDUCATION."
134 PRINT
140 PRINT "YOUR GOAL: TO COMPLETE YOUR";N4;" YEAR TERM."
340 PRINT "GOOD LUCK!"
350 LET M=INT(70000+(RND(TIM)*1000)-(RND(TIM)*1000))
360 LET P=INT(500+(RND(TIM)*10)-(RND(TIM)*10))
370 LET L=2000
380 LET W=0
390 LET X2=0
400 LET E0=0
410 LET X1=0
420 LET E1=0
430 LET S9=0
440 LET S0=0
450 LET M5=0
460 LET S1=0
470 D1=INT(P/3)
480 LET L1=INT((RND(TIM)*5)+10)
490 LET L0=INT(RND(TIM)*10+95)
500 GOSUB 2220
510 PRINT "YOU HAVE";M;" RALLODS"
520 PRINT P;" COUNTRYMEN,";
530 IF W=0 THEN 550
540 PRINT W;" FOREIGN WORKERS,";
550 PRINT " AND";L;" SQ. MILES OF LAND."
560 PRINT "THIS YEAR INDUSTRY WILL BUY LAND FOR";L0;" RALLODS/SQ. MILE"
570 PRINT "LAND CURRENTLY COSTS";L1;" RALLODS/SQ. MILE TO PLANT"
580 IF S1=0 THEN 600
590 PRINT "POLLUTION LEVEL IS";S1;" UNITS."
600 GOSUB 2220
610 PRINT "HOW MANY SQ. MILES TO SELL TO INDUSTRY";
620 INPUT L2
630 IF L2<0 THEN 610
640 IF L2<L-1000 THEN 710
650 PRINT "  THINK AGAIN, YOU'VE ONLY";L-1000;" SQ. MILES OF FARM LAND"
660 IF X1<>0 THEN 610
670 PRINT "(FOREIGN INDUSTRY IS NOT INTERESTED IN FOREST LAND BECAUSE"
680 PRINT "OF THE GREATER DEVELOPMENT COST)"
690 LET X1=1
700 GO TO 610
710 LET M=INT(M+L2*L0)
720 LET L=INT(L-L2)
730 LET X9=INT (44*(2000-L))
740 LET M2=0
750 LET M3=0
760 LET M4=0
770 PRINT "HOW MANY RALLODS TO DISTRIBUTE TO";
780 PRINT " YOUR COUNTRYMEN";
790 INPUT M1
800 IF M1<0 THEN 770
810 IF M1<=M THEN 840
820 GOSUB 2250
830 GO TO 770
840 LET M=INT(M-M1)
850 IF M=0 THEN 1170
860 PRINT "HOW MANY SQ. MILES TO PLANT";
870 INPUT L3
880 IF L3<0 THEN 860
890 IF L3<=P*2 THEN 920
900 PRINT "EACH COUNTRYMAN CAN ONLY PLANT 2 SQ. MILES"
910 GOTO 971
920 IF L3<=L-1000 THEN 950
930 PRINT "ONLY GOT";L-1000;" SQ. MILES OF FARM LAND."
940 GOTO 971
950 LET M4=INT(L3*L1)
960 IF M4<=M THEN 990
970 GOSUB 2250
971 PRINT "YOU CAN AFFORD";INT(M/L1);
972 PRINT " AND HAVE LABOR FOR";P*2;
973 PRINT " AND HAVE";L-1000;" SQ.MILES"
980 GO TO 860
990 LET M=INT (M-M4)
1000 IF M=0 THEN 1170
1010 PRINT "HOW MUCH TO SPEND FOR EDUCATION";
1020 INPUT M2
1030 IF M2<0 THEN 1010
1040 IF M2<=M THEN 1070
1050 GOSUB 2250
1060 GO TO 1010
1070 LET M=INT (M-M2)
1080 IF M=0 THEN 1170
1090 IF L=2000 THEN 1170
1100 PRINT "HOW MUCH TO SPEND TO COMBAT POLLUTION";
1110 INPUT M3
1120 IF M3<0 THEN 1100
1130 IF M3<=M THEN 1160
1140 GOSUB 2250
1150 GO TO 1100
1160 LET M=INT (M-M3)
1170 GOSUB 2220
1180 LET D0=0
1190 LET E2=M2/P
1200 LET D2=INT(P-M1/100)
1210 IF D2<=0 THEN 1240
1220 LET D0=D2
1230 PRINT D2;" COUNTRYMEN DIED OF STARVATION"
1240 LET D3=INT((S1/100000)*(.1+.4*RND(TIM))*P)
1250 IF D3<=0 THEN 1280
1260 PRINT D3;" COUNTRYMEN DIED FROM EXCESS POLLUTION."
1270 LET D0=D0+D3
1280 IF D0<=0 THEN 1400
1290 F=D0*9
1300 PRINT "YOU WERE FORCED TO SPEND";F;" RALLODS FOR FUNERAL EXPENSES"
1310 LET M=INT(M-F)
1320 IF M>=0 THEN 1400
1330 PRINT "INSUFFICIENT RESOURCES FORCED SALE OF LAND"
1340 LET T0=-INT((M-L0+1)/L0)
1350 LET L=L-T0
1360 IF L>=1000 THEN 1390
1370 PRINT "NOT ENOUGH LAND TO SELL"
1380 GO TO 2270
1390 LET M=M+T0*L0
1400 IF D0>200 THEN 2300
1410 LET P=P-D0
1420 LET D1=D1-D0
1430 IF D1<0 THEN 2410
1440 IF D2<=2 THEN 1460
1450 IF M>500 THEN 2520
1460 IF L2=0 THEN 1520
1470 LET T0=INT (L2+L2*RND(2)*2)
1480 IF W=0 THEN 1500
1490 LET T0=INT(T0+.1*W)
1500 PRINT T0;" WORKERS CAME TO THE ISLAND AND"
1510 LET W=W+T0
1520 LET T0=INT(((500-P)/10-D3/3-D2/5)*.75*(1+RND(TIM)))
1530 PRINT ABS(T0);" COUNTRYMEN ";
1540 IF T0<0 THEN 1570
1550 PRINT "CAME TO";
1560 GO TO 1580
1570 PRINT "LEFT";
1580 PRINT " THE ISLAND."
1590 LET P=P+T0
1600 IF P<W THEN 2480
1610 LET T0=0
1620 IF L=2000 THEN 1680
1630 LET T0=INT((S1/100000)*L3)
1640 IF T0<=L3 THEN 1660
1650 LET T0=L3
1660 IF T0=0 THEN 1680
1670 PRINT "OF";L3;" SQ. MILES PLANTED, ";
1680 PRINT "YOU HARVESTED";L3-T0;" SQ. MILES OF CROPS."
1690 IF T0=0 THEN 1760
1700 IF X2>=2 THEN 1760
1710 PRINT "   (DUE TO ";
1720 IF X2=0 THEN 1740
1730 PRINT "INCREASED ";
1740 PRINT "POLLUTION LEVEL)"
1750 LET X2=X2+1
1760 LET T1=INT((39+RND(TIM)*20)*(1+.25*(E0+E1)/20))
1770 PRINT "  MAKING";INT(T1*(L3-T0));" RALLODS."
1780 LET M=M+INT(T1*(L3-T0))
1790 LET E0=E1
1800 IF E2<=10 THEN 1820
1810 LET E2=10
1820 LET E1=E2
1830 LET T0=2000-L
1840 IF T0<2 THEN 2100
1850 IF T0<=26 THEN 1870
1860 LET T0=26
1870 LET T0=INT(T0*500*(.52+.5*RND(TIM)))
1880 LET T1=S1/100000
1890 IF T1<=1 THEN 1910
1900 LET T1=1
1910 LET T1=T1*T0
1920 PRINT "YOU MADE";INT(T0-T1);" RALLODS FROM TOURIST TRADE"
1930 LET M=M+INT(T0-T1)
1940 IF INT(T0-T1)>=M5 THEN 2080
1950 IF S1<=S9 THEN 2080
1960 PRINT "  DECREASE BECAUSE ";
1970 LET T2=RND(TIM)*10
1980 ON (T2/2)+1 GO TO 1990,2010,2030,2050,2070,2070
1990 PRINT "FISH POPULATION HAS DWINDLED DUE TO WATER POLLUTION"
2000 GO TO 2080
2010 PRINT "AIR POLLUTION IS KILLING GAME BIRD POPULATION"
2020 GO TO 2080
2030 PRINT "MINERAL BATHS ARE BEING RUINED BY WATER POLLUTION"
2040 GO TO 2080
2050 PRINT "UNPLEASANT SMOG IS DISCOURIGING SUN BATHERS"
2060 GO TO 2080
2070 PRINT "HOTELS ARE LOOKING SHABBY DUE TO SMOG GRIT"
2080 LET M5=INT(T0-T1)
2090 LET S9=S1
2100 LET S2=(2000-L)^2-M3/.44
2110 PRINT "YOU RECIEVED";X9;" RALLODS FROM TAXES ON INDUSTRY"
2120 IF S2>=0 THEN 2170
2130 LET S1=INT(S1+S2/2)
2140 IF S1>=S0 THEN 2160
2150 LET S1=S0
2160 GO TO 2190
2170 LET S1=INT (S1+S2)
2180 LET S0=INT (S0+S2/10)
2190 LET N5=N5-1
2200 IF N5>0 THEN 480
2210 GO TO 2570
2220 PRINT
2230 PRINT
2240 RETURN
2250 PRINT "ONLY GOT";M;" RALLODS LEFT!"
2260 RETURN
2270 PRINT "YOU HAVE BEEN THROWN OUT OF OFFICE AND YOU ARE NOW"
2280 PRINT "RESIDING IN PRISON."
2290 STOP
2300 PRINT D0;" COUNTRYMEN DIED IN ONE YEAR!!!!!"
2310 PRINT "DUE TO THIS EXTREME MISMANAGEMENT YOU HAVE NOT ONLY"
2320 PRINT "BEEN IMPEACHED AND THROWN OUT OF OFFICE BUT YOU"
2330 LET T0=INT(RND(TIM)*2.01)+1
2340 ON T0 GO TO 2350,2370,2390
2350 PRINT "ALSO HAD YOUR LEFT EYE GOUGED OUT."
2360 STOP
2370 PRINT "HAVE ALSO GAINED A VERY BAD REPUTATION."
2380 STOP
2390 PRINT "HAVE ALSO BEEN DECLARED NATIONAL FINK."
2400 STOP
2410 PRINT "OVER ONE THIRD OF THE POPULATION HAS DIED SINCE YOU"
2420 PRINT "WERE ELECTED TO OFFICE. THE PEOPLE (REMAINING)"
2430 PRINT "HATE YOUR GUTS."
2440 GO TO 2450
2450 IF RND(TIM)<=.5 THEN 2270
2460 PRINT "YOU HAVE BEEN ASSASINATED."
2470 STOP
2480 PRINT "THE NUMBER OF FOREIGN WORKERS HAS EXCEEDED THE NUMBER"
2490 PRINT "OF COUNTRYMEN. AS A MAJORITY THEY HAVE REVOLTED AND"
2500 PRINT "TAKEN OVER THE COUNTRY."
2510 GO TO 2450
2520 PRINT "MONEY WAS LEFT OVER IN THE TREASURY WHICH YOU DID NOT"
2530 PRINT "SPEND. AS A RESULT SOME OF YOUR COUNTRYMEN DIED OF"
2540 PRINT "STARVATION. THE PUBLIC IS ENRAGED AND YOU HAVE BEEN"
2550 PRINT "FORCED TO EITHER RESIGN OR COMMIT SUICIDE."
2560 STOP
2570 PRINT
2571 PRINT "CONGRATULATIONS!"
2580 PRINT "YOU HAVE SUCCESSFULLY COMPLETED YOUR";N4;" YEAR TERM"
2590 PRINT "OF OFFICE. YOU HAVE DONE SUCH A GOOD JOB THAT YOU MIGHT"
2600 PRINT "WANT TO RUN FOR ELECTION AGAIN THIS TIME."
2610 PRINT "TYPE 0 IF YOU DO"
2620 INPUT Q
2630 LET N5=N4
2640 IF Q=0 THEN 470
2650 STOP
2660 END
 