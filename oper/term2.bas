100 V$="|"
200 L$="-----------------------------------------------------------"
1870 Q$=CHR$(9) 'HORIZ. TAB
1880 R$=CHR$(13)  'CAR RET.
1890 W$=CHR$(27)+CHR$(50)  'CLEAR TABS
1900 X$=CHR$(27)+CHR$(49)  'SET TABS
1910 Z$=CHR$(9) +CHR$(124)  'HORIZ. TAB + V4
1920 PRINT
1930 PRINT W$;R$;
1940 PRINT L$;LEFT$(L$,51);V$
1950 PRINT W$;R$;
1960 PRINT X$;TAB(6);X$;TAB(111+1);X$;R$;
1970 PRINT Q$;Z$
1980 PRINT Q$;Z$
1990 PRINT Q$;Z$
2000 PRINT Q$;Z$
2010 PRINT Q$;" ";L$;LEFT$(L$,40);Z$
2020 PRINT W$;R$;
2030 PRINT X$;TAB(6);X$;TAB(45+1);X$;TAB(106+2);X$;TAB(111+3);X$;R$;
2040 PRINT Z$;Q$;Z$;Z$
2050 PRINT Z$;Q$;"LINE & TERMINAL FORECAST";Z$;Z$
2060 PRINT Z$;"                               ( BASED ON 19";Y$;" COMMERCIAL FORECAST )";Z$;Z$
2070 PRINT Z$;Q$;Z$;Z$
2080 PRINT Z$;" OFFICE: ";O$;Q$;Z$;Z$
2090 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2100 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2110 PRINT W$;R$;
2120 PRINT X$;TAB(6);X$;TAB(16+1);X$;TAB(25+2);X$;TAB(34+3);X$;TAB(43+4);X$;
2130 PRINT TAB(52+5);X$;TAB(61+6);X$;TAB(70+7);X$;TAB(79+8);X$;TAB(88+9);X$;
2140 PRINT TAB(97+10);X$;TAB(106+11);X$;TAB(111+12);X$;R$;
2150 PRINT Z$;Z$;Q$;Q$;Q$;Q$;"    YEAR ENDING";Q$;Q$;Q$;Q$;Z$;Z$
2160 PRINT Z$;Z$;L$;LEFT$(L$,30);V$;Z$
2170 PRINT Z$;" EQUIP";Z$;
2180 PRINT USING 2660,Y(1),Z$,Y(2),Z$,Y(3),Z$,Y(4),Z$,Y(5),Z$,Y(6),Z$,Y(7),Z$,Y(8),Z$,Y(9),Z$,Y(10),Z$,Z$
2190 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2200 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2210 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2220 PRINT Z$;" LINES";Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2230 PRINT Z$;"    FR:";Z$;
2240 PRINT USING 2670,M(1),Z$,M(2),Z$,M(3),Z$,M(4),Z$,M(5),Z$,M(6),Z$,M(7),Z$,M(8),Z$,M(9),Z$,M(10),Z$,Z$
2250 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2260 PRINT Z$;" LINES";Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2270 PRINT Z$;"    CN:";Z$;
2280 PRINT USING 2670,E(1),Z$,E(2),Z$,E(3),Z$,E(4),Z$,E(5),Z$,E(6),Z$,E(7),Z$,E(8),Z$,E(9),Z$,E(10),Z$,Z$
2290 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2300 PRINT Z$;" TOTAL:";Z$;
2310 PRINT USING 2670,S(1),Z$,S(2),Z$,S(3),Z$,S(4),Z$,S(5),Z$,S(6),Z$,S(7),Z$,S(8),Z$,S(9),Z$,S(10),Z$,Z$
2320 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2330 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2340 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2350 PRINT Z$;" TERMS";Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2360 PRINT Z$;"   REG:";Z$;
2370 PRINT USING 2670,O(1),Z$,O(2),Z$,O(3),Z$,O(4),Z$,O(5),Z$,O(6),Z$,O(7),Z$,O(8),Z$,O(9),Z$,O(10),Z$,Z$
2380 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2390 PRINT Z$;" TERMS";Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2400 PRINT Z$;"    CN:";Z$;
2410 PRINT USING 2670,E(1),Z$,E(2),Z$,E(3),Z$,E(4),Z$,E(5),Z$,E(6),Z$,E(7),Z$,E(8),Z$,E(9),Z$,E(10),Z$,Z$
2420 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2430 PRINT Z$;" TERMS";Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2440 PRINT Z$;"   ROT:";Z$;
2450 PRINT USING 2670,N(1),Z$,N(2),Z$,N(3),Z$,N(4),Z$,N(5),Z$,N(6),Z$,N(7),Z$,N(8),Z$,N(9),Z$,N(10),Z$,Z$
2460 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2470 PRINT Z$;" TOTAL:";Z$;
2480 PRINT USING 2670,T(1),Z$,T(2),Z$,T(3),Z$,T(4),Z$,T(5),Z$,T(6),Z$,T(7),Z$,T(8),Z$,T(9),Z$,T(10),Z$,Z$
2490 PRINT Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$;Z$
2500 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2510 PRINT Z$;L$;LEFT$(L$,40);V$;Z$
2520 PRINT W$;R$;
2530 PRINT X$;TAB(6);X$;TAB(106+1);X$;TAB(111+2);X$;R$;
2540 PRINT Z$;" NOTES:";Z$;Z$
2550 PRINT Z$;Z$;Z$
2560 PRINT Z$;Z$;Z$
2570 PRINT Z$;Z$;Z$
2580 PRINT Z$;Z$;Z$
2590 PRINT Z$;Z$;Z$
2600 PRINT Z$;Z$;Z$
2610 PRINT Z$;Z$;Z$
2620 PRINT Q$;" ";L$;LEFT$(L$,40);Z$
2630 PRINT L$;LEFT$(L$,51);V$
2640 PRINT W$;R$;
2650'FORMATING
2660:  ####'R  ####'R  ####'R  ####'R  ####'R  ####'R  ####'R  ####'R  ####'R  ####'R'R
2670: ######'R ######'R ######'R ######'R ######'R ######'R ######'R ######'R ######'R ######'R'R
9999 END
  