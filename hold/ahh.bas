90 PAGE(62)
100 DIM A(71,5), X(27,5), R(12,5), C(6,5), P(6,5), N(62), L(0,7)
110 PRINT "PLEASE ENTER THE NAME OF YOUR DATA FILE";
120 INPUT C$
125 FILES  ,AHEAD1,AHEAD2,AHEAD3,AHEAD4,AHEAD5
126 FILE #1, C$
130 READ #1, N$, M(0), Y, A(0,0)
140 CHANGE N$ TO N
150 LET N = N(0)
160 FOR I = 3 TO 20
170 READ #1, A(I,0)
180 NEXT I
190 READ #1, A(22,0)
200 FOR I = 24 TO 41
210 READ #1, A(I,0)
220 NEXT I
230 FOR I = 43 TO 47
240 READ #1, A(I,0)
250 NEXT I
260 READ #1, A(49,0), A(51,0), A(55,0), A(61,0), A(71,0),A(63,0),A(63,1),A(64,0),A(64,1),A(65,0),A(65,1),F
280 LET K = 14
290 FOR I = 0 TO 47
300 READ #1, A, X(1,1)
310 IF A = 0 THEN 370
320 READ #1, X(1,2), X(1,3), X(1,4), X(1,5)
330 IF A = 1 THEN 380
340 IF A = 2 THEN 420
350 PRINT "YOU HAVE ENTERED YOUR DATA INCORRECTLY -- VARIABLE #"; I
360 STOP
370 LET X(1,2) = X(1,3) = X(1,4) = X(1,5) = X(1,1)
380 FOR J = 1 TO 5
390 LET A(I,J) = A(I,J-1) * ( 1+X(1,J) )
400 NEXT J
410 GO TO 450
420 FOR J = 1 TO 5
430 LET A(I,J) = X(1,J)
440 NEXT J
450 IF I > 5 THEN 454
452 GO TO 460
454 IF I<10 THEN 500
460 IF I > 12 THEN 462
461 GO TO 470
462 IF I<>17 THEN 464
463 GO TO 470
464 IF I<>18 THEN 466
465 GO TO 470
466 IF I<>46 THEN 500
470 IF I = 0 THEN 474
472 GO TO 480
474 I=4
480 IF I = 18 THEN 484
482 GO TO 490
484 I=34
490 GO TO 600
500 IF A = 2 THEN 550
510 FOR J = 1 TO 5
520 LET X(K,J) = X(1,J)
530 NEXT J
540 GO TO 580
550 FOR J = 1 TO 5
560 IF A(I,J-1)<>0 THEN 564
562 GO TO 570
564 LET X(K,J) = ( A(I,J)-A(I,J-1) ) / A(I,J-1)
570 NEXT J
580 LET K = K + 1
590 IF I = 36 THEN 594
592 GO TO 600
594 I=42
600 NEXT I
610 FOR I = 0 TO 13
620 READ #1, X(I,1), X(I,2), X(I,3), X(I,4), X(I,5)
630 NEXT I
640 FOR I = 66 TO 68
650 READ #1, A(I,1), A(I,2), A(I,3), A(I,4), A(I,5)
660 NEXT I
670 READ #1, A
680 IF A <> 0 THEN 684
682 GO TO 690
684 IF A<>1 THEN 800
690 READ #1, A(69+A,1), A(69+A,2), A(69+A,3), A(69+A,4), A(69+A,5)
700 FOR K = 0 TO 10
710 READ I
720 READ #1, A(I,1), A(I,2), A(I,3), A(I,4), A(I,5)
730 NEXT K
740 DATA 71, 20, 21, 22, 23, 24, 51, 55, 19, 58, 60
750 READ #1, U(1), U(2), U(3), U(4), U(5)
760 READ #1, R(0,1), R(0,2), R(0,3), R(0,4), R(0,5)
770 READ #1, C(1,1), C(1,2), C(1,3), C(1,4), C(1,5)
780 READ #1, C(4,1), C(4,2), C(4,3), C(4,4), C(4,5), G
790 IF G = 999 THEN 820
800 PRINT "YOU HAVE ENTERED YOUR DATA INCORRECTLY.  CHECK LINES 500-840."
810 STOP
820 FOR I = 63 TO 65
830 LET A(I,2) = A(I,3) = A(I,4) = A(I,5) = A(I,1)
840 NEXT I
850 FOR J = 1 TO 5
860 IF Y = 2 THEN 950
870 FOR I = 29 TO 34
880 LET A(I,J) = ( A(I-24,J)+A(I-24,J-1) )*.5*X(I-27,J)
890 NEXT I
900 FOR I = 37 TO 40
910 LET A(I,J) = ( A(I-23,J)+A(I-23,J-1) )*.5*X(I-29,J)
920 NEXT I
930 LET A(41,J) = ( A(20,J)+A(20,J-1) )*.5*X(12,J)
940 GO TO 1020
950 FOR I = 29 TO 34
960 LET A(I,J) = A(I-24,J)*X(I-27,J)
970 NEXT I
980 FOR I = 37 TO 40
990 LET A(I,J) = A(I-23,J)*X(I-29,J)
1000 NEXT I
1010 LET A(41,J) = A(20,J)*X(12,J)
1020 LET A(42,J) = A(21,J)*X(13,J)
1030 NEXT J
1040 FOR J = 0 TO 5
1050 LET S(2,J) = A(30,J)+A(31,J)+A(32,J)+A(33,J)+A(34,J)
1060 LET S(3,J) = A(35,J)+A(36,J)
1070 LET S(4,J) = A(29,J)+S(2,J)+S(3,J)
1080 LET S(5,J) = A(37,J)+A(38,J)+A(39,J)
1090 LET S(6,J) = A(40,J)+A(41,J)+A(42,J)
1100 LET S(7,J) = S(5,J)+S(6,J)
1110 LET S(8,J) = A(43,J)+A(44,J)+A(45,J)+A(46,J)+A(47,J)
1120 LET S(9,J) = S(7,J)+S(8,J)
1130 LET T(1,J) = A(0,J)
1140 LET A(1,J) = A(13,J)*F
1150 LET A(2,J) = ( ( A(13,J)-T(1,J) )*A(63,J) + ( A(14,J)+A(16,J) )*A(64,J)+A(15,J)*A(65,J))/(1-A(63,J))
1170 LET A(0,J) = T(1,J)-A(1,J)-A(2,J)
1180 LET T(3,J) = A(6,J)+A(7,J)+A(8,J)+A(9,J)+A(10,J)
1190 LET T(4,J) = T(1,J)+A(5,J)+T(3,J)+A(11,J)+A(12,J)
1200 LET T(5,J) = A(13,J)+A(14,J)+A(15,J)+A(16,J)
1210 LET T(6,J) = T(5,J)+A(17,J)+A(18,J)
1220 IF J = 0 THEN 1520
1230 LET A(19,J) = A(19,J-1)-A(19,J)+A(46,J)+A(58,J)
1240 LET A(23,J) = A(23,J-1)+A(23,J)
1250 LET A(24,J) = A(24,J-1)+A(24,J)
1260 LET A(26,J) = A(26,J-1)+A(60,J)
1270 LET T(7,J) = A(20,J)+A(21,J)+A(22,J-1)+A(23,J)+A(24,J)+A(25,J-1)+A(26,J)
1280 LET T(8,J) = T(6,J)+A(19,J)+T(7,J)
1290 IF A(51,J) <0 THEN 1294
1292 GO TO 1300
1294 LET A(53,J)=A(51,J)*A(66,J)
1300 IF A(51,J) > 0 THEN 1304
1302 GO TO 1310
1304 A(53,J)=A(51,J)*A(67,J)
1310 LET A(59,J) = A(58,J)*A(66,J)
1320 LET A(71,J) = ( A(71,J-1)+A(71,J) ) * ( 1+A(22,J) )
1330 IF A = 0 THEN 1334
1332 GO TO 1340
1334 LET A(61,J)=A(69,J)*A(71,J)
1340 IF A=1 THEN 1342
1341 GO TO 1360
1342 IF Y=1 THEN 1344
1343 GO TO 1360
1344 LET A(61,J) = A(70,J) * ( (1-A(66,J)) * ( S(4,J)+A(3,J-1)*.5*X(0,J)-S(9,J))+A(4,J-1)*.5*X(1,J))
1360 IF A=1 THEN 1362
1361 GO TO 1370
1362 IF Y=2 THEN 1364
1363 GO TO 1370
1364 LET A(61,J) = A(70,J)*(S(4,J)-S(9,J))*(1-A(66,J))
1370 LET A(4,J) = T(8,J) - T(4,J) + ( S(4,J)-S(9,J) ) * ( 1-A(66,J) ) + A(51,J)-A(53,J)+A(55,J)-A(58,J)+A(59,J)-A(60,J)-A(61,J)
1390 IF Y=1 THEN 1392
1391 GO TO 1420
1392 LET A(4,J) = ( A(4,J) + (1-A(66,J))*A(3,J-1)*.5*X(0,J)+A(4,J-1)*.5*X(1,J))/(1-(1-A(70,J))*((1-A(66,J))*.5*X(0,J)*A(68,J)+.5*X(1,J)*(1-A(68,J))))
1420 IF Y=2 THEN 1422
1421 GO TO 1440
1422 LET A(4,J) = A(4,J) / (1 - (1-A(70,J))*( (1-A(66,J))*X(0,J)*A(68,J)+X(1,J)*(1-A(68,J))))
1440 LET A(3,J) = A(4,J)*A(68,J)
1450 LET A(4,J) = A(4,J)-A(3,J)
1460 IF Y = 2 THEN 1500
1470 LET A(27,J) = ( A(3,J)+A(3,J-1) )*.5*X(0,J)
1480 LET A(28,J) = ( A(4,J)+A(4,J-1) )*.5*X(1,J)
1490 GO TO 1520
1500 LET A(27,J)=A(3,J)*X(0,J)
1510 LET A(28,J) = A(4,J)*X(1,J)
1520 LET S(4,J) = S(4,J)+A(27,J)+A(28,J)
1530 LET A(48,J) = S(4,J)-S(9,J)
1540 IF J > 0 THEN 1544
1542 GO TO 1550
1544 LET A(49,J)=(A(48,J)-A(28,J))*A(66,J)
1550 LET A(50,J) = A(48,J)-A(49,J)
1560 LET A(54,J) = A(50,J)+A(51,J)-A(53,J)
1570 LET A(57,J) = A(54,J)+A(55,J)
1580 IF J = 0 THEN 1680
1590 IF A = 1 THEN 1594
1592 GO TO 1600
1594 LET A(61,J)=A(70,J)*A(50,J)
1600 LET A(62,J) = A(57,J)-A(58,J)+A(59,J)-A(60,J)-A(61,J)
1610 LET A(25,J) = A(25,J-1)+A(62,J)-A(22,J)*( A(22,J-1)+A(23,J) )-U(J)
1620 LET A(24,J) = A(24,J)+U(J)
1630 LET A(23,J) = A(23,J)+A(22,J)*A(23,J)
1640 LET A(22,J) = A(22,J-1)+A(22,J)*A(22,J-1)
1650 LET T(7,J) = T(7,J)+A(62,J)
1660 LET T(8,J) = T(8,J)+A(62,J)
1670 GO TO 1700
1680 LET T(7,0) = A(20,0)+A(22,0)+A(24,0)+A(25,0)+A(26,0)
1690 LET T(8,0) = T(6,0)+A(19,0)+T(7,0)
1700 LET S(1,J) = A(27,J)+A(28,J)+A(29,J)
1710 LET T(2,J) = A(3,J)+A(4,J)+A(5,J)
1720 LET T(4,J) = T(4,J)+A(3,J)+A(4,J)
1730 IF A(51,J) >= 0 THEN 1770
1740 LET A(52,J) = ABS(A(51,J))
1750 LET A(51,J) = 0
1760 LET A(53,J) = ABS(A(53,J))
1770 IF A(55,J) >= 0 THEN 1800
1780 LET A(56,J) = ABS(A(55,J))
1790 LET A(55,J) = 0
1800 LET A(59,J) = ABS(A(59,J))
1810 LET P(0,J) = A(50,J)/A(71,J)
1820 LET P(2,J) = A(61,J)/A(71,J)
1830 LET P(3,J) = A(61,J)/A(50,J)
1840 LET P(4,J) = A(54,J)/A(71,J)
1850 LET P(5,J) = A(57,J)/A(71,J)
1860 LET P(6,J) = ( T(7,J)-A(20,J)-A(21,J) ) / A(71,J)
1870 IF J = 0 THEN 1900
1880 LET P(1,J) = ( P(0,J)-P(0,J-1) ) / P(0,J-1)
1890 LET M(J) = M(J-1) + 1
1900 LET T(0,J) = T(7,J)+A(19,J)
1910 NEXT J
1920 LET G = ( ( P(0,5)/P(0,0) )^.2 - 1 ) * 100
1930 PRINT "HOW MANY STATEMENTS DO YOU WISH TO BE PRINTED";
1940 INPUT L
1950 IF L = 0 THEN 1990
1960 PRINT "PLEASE ENTER THE NUMBERS OF THE STATEMENTS IN THE ORDER"
1970 PRINT "IN WHICH YOU WISH THEM";
1980 MAT INPUT L
1981 FOR K=1 TO L
1982 L(K-1)=L(K)
1983 NEXT K
1990 X=0
2000 GOSUB 4200
2010 X=2
2020 IF Q$ = "YES" THEN 2110
2030 LET T$ = "FIVE YEAR COMPOUND GROWTH RATE OF EPS BEFORE SEC"
2040 GO SUB 4210
2050 FOR I = 0 TO 6
2060 READ #2,T$
2070 IF I=1 THEN 2072
2071 GO TO 2080
2072 PRINT USING 4450, T$, P(1,1), P(1,2), P(1,3), P(1,4), P(1,5)
2080 IF I<>1 THEN 2082
2081 GO TO 2090
2082 PRINT USING 4460, T$,P(I,0),P(I,1),P(I,2),P(I,3),P(I,4),P(I,5)
2090 NEXT I
2100 LET U$ = "  "
2110 FOR K = 0 TO L-1
2120 ON L(0,K) GO TO 2130, 2560, 2840, 3180, 3630, 3640, 3190
2130 LET T$ = "KEY RATIO AND GROWTH RATES"
2140 LET T = 22
2150 GO SUB 4200
2160 LET Z = 0
2170 PRINT 
2171 PRINT "RATIOS"
2172 PRINT "======"
2180 FOR J = 0 TO 5
2190 LET R(1,J) = T(3,J)/T(5,J)
2200 IF J = 0 THEN 2230
2210 IF R(0,J) < R(1,J) THEN 2212
2211 GO TO 2220
2212 LET R(2,J)=T(3,J)/R(0,J)-T(5,J)
2220 IF R(0,J) > R(1,J) THEN 2222
2221 GO TO 2230
2222 LET R(3,J)=R(0,J)*T(5,J)-T(3,J)
2230 LET R(4,J) = A(3,J)/T(5,J)
2240 LET R(5,J) = ( T(1,J)+A(3,J) ) / T(5,J)
2250 LET R(6,J) = T(0,J)/T(5,J)
2260 LET R(7,J) = A(11,J) / ( A(22,J)+A(23,J)+A(24,J) )
2270 LET R(8,J) = A(48,J)/T(4,J)
2280 LET R(9,J) = A(50,J)/T(4,J)
2290 LET R(10,J) = A(50,J) / ( T(0,J)-A(20,J)-A(21,J) )
2300 LET R(11,J) = A(49,J)/A(48,J)
2310 LET R(12,J) = S(9,J)/S(4,J)
2320 NEXT J
2330 FOR I = 0 TO 12
2340 READ #2,T$
2350 IF I = 7 THEN 2354
2352 GO TO 2360
2354 PRINT
2360 IF I < 4 THEN 2362
2361 GO TO 2370
2362 IF I<>1 THEN 2390
2370 PRINT USING 4470, T$, R(I,0), R(I,1), R(I,2), R(I,3), R(I,4), R(I,5)
2380 GO TO 2430
2390 IF R(I,1) + R(I,2) + R(I,3) + R(I,4) + R(I,5) = 0 THEN 2420
2400 PRINTUSING 4480, T$, R(I,1), R(I,2), R(I,3), R(I,4), R(I,5)
2410 GO TO 2430
2420 LET Z = Z + 1
2430 NEXT I
2440 PRINT
2441 PRINT "GROWTH RATES"
2442 PRINT "============"
2450 FOR J = 1 TO 5
2460 LET R(0,J) = ( T(3,J)-T(3,J-1) ) / T(3,J-1)
2470 LET R(1,J) = ( T(5,J)-A(13,J)-T(5,J-1)+A(13,J-1) )/( T(5,J-1)-A(13,J-1) )
2480 LET R(2,J) = ( T(5,J)-T(5,J-1) ) / T(5,J-1)
2490 LET R(3,J) = ( T(2,J)-T(2,J-1) ) / T(2,J-1)
2500 NEXT J
2510 FOR I = 0 TO 3
2520 READ #2,T$
2530 PRINT USING 4480, T$, R(I,1), R(I,2), R(I,3), R(I,4), R(I,5)
2540 NEXT I
2550 GO TO 3970
2560 LET T$ = "CAPITAL ADEQUACY TEST"
2570 LET T = 25
2580 GO SUB 4200
2590 LET Z = 7
2600 FOR J = 0 TO 5
2610 LET C(0,J) = T(0,J) / ( T(4,J)-T(1,J)-A(3,J) )
2620 LET C(3,J) = ( A(20,J)+A(21,J) ) / T(0,J)
2630 IF J = 0 THEN 2700
2640 LET C(2,J) = C(1,J)*( T(4,J)-T(1,J)-A(3,J) ) - T(0,J)
2650 IF C(2,J) <= 0 THEN 2700
2660 LET C(5,J) = ( C(4,J)*T(0,J)-A(20,J)-A(21,J) ) / ( 1-C(4,J) )
2670 IF C(5,J) > C(2,J) THEN 2672
2671 GO TO 2680
2672 LET C(5,J)=C(2,J)
2680 IF C(5,J) < C(2,J) THEN 2682
2681 GO TO 2690
2682 LET C(6,J)=C(2,J)-C(5,J)
2690 LET Z = 0
2700 NEXT J
2710 FOR I = 0 TO 6
2720 READ #3, T$
2730 IF I > 0 THEN 2732
2731 GO TO 2740
2732 IF I<>3 THEN 2770
2740 IF I=3 THEN 2742
2741 GO TO 2750
2742 PRINT
2743 PRINT "MIX OF NEW CAPITAL"
2744 PRINT "=================="
2750 PRINT USING 4470, T$, C(I,0), C(I,1), C(I,2), C(I,3), C(I,4), C(I,5)
2760 GO TO 2820
2770 IF C(I,1) + C(I,2) + C(I,3) + C(I,4) + C(I,5) = 0 THEN 2800
2780 PRINTUSING 4480, T$, C(I,1), C(I,2), C(I,3), C(I,4), C(I,5)
2790 GO TO 2810
2800 LET Z = Z + 1
2810 IF I = 2 THEN 2812
2811 GO TO 2820
2812 IF Z=7 THEN 2814
2813 GO TO 2820
2814 I=6
2820 NEXT I
2830 GO TO 3970
2840 LET T$ = "ASSUMPTIONS"
2850 T=28
2860 LET Z = 0
2870 FOR I = 0 TO 27
2880 IF X(I,1) + X(I,2) + X(I,3) + X(I,4) + X(I,5) <> 0 THEN 2910
2890 LET X(I,0) = -1
2900 LET Z = Z + 1
2910 NEXT I
2920 FOR I = 63 TO 71
2930 IF A(I,1)+ A(I,2) + A(I,3) + A(I,4) + A(I,5) <> 0 THEN 2960
2940 LET A(I,0) = -1
2950 LET Z = Z + 1
2960 NEXT I
2970 Z=Z-6
2980 X=-2.4
2990 GO SUB 4200
3000 PRINT
3001 PRINT "INTEREST RATES"
3002 PRINT "=============="
3010 FOR I = 0 TO 27
3020 READ #4, T$
3030 IFI<>3 THEN 3032
3031 GO TO 3042
3032 IF I<> 8 THEN 3034
3033 GO TO 3042
3034 IF I<>11 THEN 3036
3035 GO TO 3042
3036 IF I<>18 THEN 3038
3037 GO TO 3042
3038 IF I<>22 THEN 3040
3039 GO TO 3042
3040 IF I<>24 THEN 3050
3042 PRINT
3050 IF I=14 THEN 3052
3051 GO TO 3060
3052 PRINT
3053 PRINT "GROWTH RATES"
3054 PRINT "============"
3060 IF X(I,0) = -1 THEN 3080
3070 PRINT USING 4490, T$, X(I,1), X(I,2), X(I,3), X(I,4), X(I,5)
3080 NEXT I
3090 PRINT
3091 PRINT "RESERVE REQUIREMENTS"
3092 PRINT "===================="
3100 FOR I = 63 TO 71
3110 READ #4,T$
3120 IF I=66 THEN 3122
3121 GO TO 3130
3122 PRINT
3123 PRINT "MISC. ASSUMPTIONS"
3124 PRINT "================="
3130 IF A(I,0) = -1 THEN 3150
3140 PRINT USING 4480, T$, A(I,1), A(I,2), A(I,3), A(I,4), A(I,5)
3150 NEXT I
3160 X=2
3170 GO TO 3970
3180 LET T$ = "PRO FORMA INCOME STATEMENT"
3190 IF L(0,K) = 7 THEN 3192
3191 GO TO 3200
3192 T$="INCOME STATEMENT ANALYSIS"
3200 LET T = 23
3210 IF U = 0 THEN 3212
3211 GO TO 3220
3212 GOSUB 4050
3220 GO SUB 4140
3230 LET U$ = "  "
3240 LET T = 0
3250 IF U = 28 THEN 3290
3260 FOR J = 0 TO 5
3270 LET D(J) = S(4,J)/100
3280 NEXT J
3290 PRINT
3291 PRINT "INCOME"
3292 PRINT "======"
3300 FOR I = 27 TO 62
3310 READ #5, T$
3320 IF N(I) = -1 THEN 3390
3330 IF I < 58 THEN 3360
3340 PRINT USING 4480, T$, A(I,1), A(I,2), A(I,3), A(I,4), A(I,5)
3350 GO TO 3590
3360 IF U=28 THEN 3362
3361 GO TO 3370
3362 PRINT USING 4470, T$, A(I,0),A(I,1),A(I,2),A(I,3),A(I,4),A(I,5)
3370 IF U=30 THEN 3372
3371 GO TO 3390
3372 PRINT USING 4460, T$, A(I,0)/D(0), A(I,1)/D(1), A(I,2)/D(2),A(I,3)/D(3),A(I,4)/D(4),A(I,5)/D(5)
3390 IF I > 47 THEN 3570
3400 IFI<>29 THEN 3402
3401 GO TO 3412
3402 IF I<>34 THEN 3404
3403 GO TO 3412
3404 IF I<>36 THEN 3406
3405 GO TO 3412
3406 IF I<>39 THEN 3408
3407 GO TO 3412
3408 IF I<>42 THEN 3410
3409 GO TO 3412
3410 IF I<47 THEN 3590
3412 READ #5, T$
3420 LET T = T + 1
3430 IF T = 6 THEN 3432
3431 GO TO 3440
3432 IF N(40)+N(41)+N(42)=-3 THEN 3470
3440 IF U=28 THEN 3442
3441 GO TO 3450
3442 PRINT USING 4470, T$, S(T,0),S(T,1),S(T,2),S(T,3),S(T,4),S(T,5)
3450 IF U=30 THEN 3452
3451 GO TO 3470
3452 PRINT USING 4460, T$, S(T,0)/D(0), S(T,1)/D(1), S(T,2)/D(2),S(T,3)/D(3),S(T,4)/D(4),S(T,5)/D(5)
3470 IF T <> 5 THEN 3472
3471 GO TO 3480
3472 PRINT
3480 ON T GO TO 3590, 3590, 3412, 3490, 3590, 3412, 3590, 3412, 3590
3490 IF L(0,K)=7 THEN 3560
3510 FOR M7=1 TO 8
3520 PRINT
3530 NEXT M7
3540 X=-1.4
3545 T$=" "
3550 GOSUB 4250
3560 PRINT
3561 PRINT "EXPENSE"
3562 PRINT "======="
3570 IF I = 50 THEN 3572
3571 GO TO 3580
3572 PRINT
3580 IF I = 50 THEN 3582
3581 GO TO 3590
3582 IF U=30 THEN 3584
3583 GO TO 3590
3584 LET I= 62
3590 NEXT I
3600 RESTORE #5
3610 X=2
3620 GO TO 3970
3630 LET T$ = "PRO FORMA BALANCE SHEET"
3640 IF L(0,K) = 6 THEN 3642
3641 GO TO 3650
3642 LET T$="BALANCE SHEET ANALYSIS"
3650 LET T = 19 + L(0,K)
3660 IF U = 0 THEN 3662
3661 GO TO 3670
3662 GOSUB 4050
3670 X=-2.4
3680 GO SUB 4140
3690 LET U$ = "  "
3700 LET T = 0
3710 IF U = 28 THEN 3750
3720 FOR J = 0 TO 5
3730 LET D(J) = T(4,J)/100
3740 NEXT J
3750 PRINT
3751 PRINT "ASSETS"
3752 PRINT "======"
3760 FOR I = 0 TO 26
3770 READ #6,T$
3780 IF N(I) = -1 THEN 3820
3790 IF U=28 THEN 3792
3791 GO TO 3800
3792 PRINT USING 4470, T$, A(I,0),A(I,1),A(I,2),A(I,3),A(I,4),A(I,5)
3800 IF U=30 THEN 3802
3801 GO TO 3820
3802 PRINT USING 4460, T$, A(I,0)/D(0), A(I,1)/D(1), A(I,2)/D(2),A(I,3)/D(3),A(I,4)/D(4),A(I,5)/D(5)
3820 IF I = 19 THEN 3822
3821 GO TO 3830
3822 PRINT
3830 IF I<>2 THEN 3832
3831 GO TO 3844
3832 IF I<>5 THEN 3834
3833 GO TO 3844
3834 IF I<>10 THEN 3836
3835 GO TO 3844
3836 IF I<>12 THEN 3838
3837 GO TO 3844
3838 IF I<>16 THEN 3840
3839 GO TO 3844
3840 IF I<>18 THEN 3842
3841 GO TO 3844
3842 IF I<26 THEN 3930
3844 READ #6, T$
3860 LET T = T + 1
3870 IF T = 4 THEN 3872
3871 GO TO 3880
3872 PRINT
3880 IF U=28 THEN 3882
3881 GO TO 3890
3882 PRINT USING 4470, T$,T(T,0),T(T,1),T(T,2),T(T,3),T(T,4),T(T,5)
3890 IF U=30 THEN 3892
3891 GO TO 3910
3892 PRINT USING 4460, T$, T(T,0)/D(0), T(T,1)/D(1), T(T,2)/D(2),T(T,3)/D(3),T(T,4)/D(4),T(T,5)/D(5)
3910 PRINT
3920 IF T=4 THEN 3922
3921 GO TO 3930
3922 PRINT
3923 PRINT "LIABILITIES"
3924 PRINT "==========="
3930 IF T = 7 THEN 3844
3940 NEXT I
3950 RESTORE #6
3960 X=2
3970 NEXT K
3980 LET X = 0
3990 GO SUB 4200
4000 LET Y = 0
4010 PRINT "DO YOU WANT ANY OTHER STATEMENTS";
4020 INPUT Q$
4030 IF Q$ = "YES" THEN 1930
4040 STOP
4050 FOR I = 0 TO 62
4060 IF A(I,0) + A(I,1) + A(I,2) + A(I,3) + A(I,4) + A(I,5) <> 0 THEN 4110
4070 LET N(I) = -1
4080 IF I < 25 THEN 4082
4081 GO TO 4090
4082 LET V=V+1
4090 IF I > 24 THEN 4092
4091 GO TO 4100
4092 LET W=W+1
4100 IF I > 45 THEN 4102
4101 GO TO 4110
4102 LET W1=W1+1
4110 NEXT I
4120 IF N(40) + N(41) + N(42) = -3 THEN 4122
4121 GO TO 4130
4122 LET W=W+1
4130 RETURN
4140 IF L(0,K) > 5 THEN 4180
4150 LET U$ = "(IN MILLIONS)"
4160 LET U = 28
4170 GO TO 4200
4180 LET U$ = "(PER CENT)"
4190 LET U = 30
4200 PRINT
4210 FOR I=0 TO X
4220 PRINT
4230 NEXT I
4240 IF X = 0 THEN 4242
4241 GO TO 4250
4242 RETURN
4250 PRINT <PA>
4252 PRINT TAB ((72-N)/2);
4254 PRINT N$,"MAY 11,1973"
4260 IF X > -1.4 THEN 4320
4270 IF X>-2.4 THEN 4272
4271 GO TO 4280
4272 PRINT
4273 PRINT
4274 PRINT TAB (T);
4276 PRINT T$
4278 PRINT TAB(U);U$
4280 IF X = -1.4 THEN 4340
4290 IF X=-2.4 THEN 4292
4291 GO TO 4300
4292 PRINT TAB (T);
4293 PRINT T$
4295 PRINT TAB(U);U$
4300 PRINT
4302 PRINT
4304 PRINT USING 4420, M(0), M(1), M(2), M(3), M(4), M(5)
4306 PRINT
4310 RETURN
4320 IF T=0 THEN 4322
4321 GO TO 4330
4322 PRINT
4323 PRINT
4324 PRINT
4326 PRINT TAB (7);
4328 PRINT T$,G
4330 IF T>0 THEN 4332
4331 GO TO 4340
4332 PRINT
4333 PRINT
4334 PRINT
4336 PRINT TAB (T);
4338 PRINT T$
4339 PRINT TAB(U);U$
4340 PRINT
4342 PRINT
4344 PRINT USING 4410, M(0), M(1), M(2), M(3), M(4), M(5)
4346 PRINT
4350 RETURN
4360 :'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
4370 :'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE   ###.#
4380 :'EEEEEEEEEEEEEEEEEEEEEEEEEEEE  ###############
4390 :'LLLLLLLLLLLLLLLLLLLLLLLLLLLL  ###############
4400 :'LLLLLLLLLLLLLLLLLLLLLLLLLLLL  ###############
4410 :               YEAR   #####    #####    #####    #####    #####    #####
4420 :               YEAR   #####    #####    #####    #####    #####    #####
4430 :'EEEEEEEEEEEEEE EEEEEEEEEEEEEE
4440 :'EEEEEEEEEEEEEEEEEEEE EEEEEEEEEEEEEEEEEEEE
4450 :'LLLLLLLLLLLLLLLLLLL         ####.##  ####.##  ####.##  ####.##  ####.##
4460 :'LLLLLLLLLLLLLLLLLL ####.##  ####.##  ####.##  ####.##  ####.##  ####.##
4470 :'LLLLLLLLLLLLLLLLLL ###.###  ###.###  ###.###  ###.###  ###.###  ###.###
4480 :'LLLLLLLLLLLLLLLLLLL          ##.###   ##.###   ##.###   ##.###   ##.###
4490 :'LLLLLLLLLLLLLLLLLLLL        ##.####  ##.####  ##.####  ##.####  ##.####
4500 END
 