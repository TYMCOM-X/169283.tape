100 DATA 73
110 DATA 5
120 DATA "CENTRAL REGION"
130 DATA "CHICAGO"
140 DATA "DETROIT"
150 DATA "ST LOUIS"
160 DATA "MINNEAPOLIS"
170 DATA 0,31,31,28,59,31,90,30,120,31,151,30
180 DATA 181,31,212,31,243,30,273,31,304,30,334,31
190 DATA "TYMCOM-IX","TYMCOM-X ","TYMCOM-W ","TOTAL    "
200 DIM F(4,11),T(4,11),D8(12),D9(12)
210 READ Y1,D0
220 FOR D=1 TO D0
230     READ D$(D)
240 NEXT D
250 FOR M9=1 TO 12
260     READ D9(M9),D8(M9)
270 NEXT M9
280 FOR C=1 TO 4
290     READ C$(C)
300 NEXT C
310 PRINT "WEEKLY MTD REPORT FROM DISTRICTS"
320 PRINT
330 FILE :1,"DIST"+STR$(Y1)+".DAT%"
340 S=LOF(:1)
350 IF S=0 THEN 760
360 S=INT((S-1)/50)*50+1
370 SET :1,S
380 READ :1,M1,D1,T0
390 PRINT "WEEK ENDING ";STR$(M1);"/";STR$(D1);"/";STR$(Y1);" OK";
400 INPUT W$
410 IF LEN(W$)=0 THEN 830
420 IF LEFT$(W$,1)="Y" THEN 830
430 GOSUB 670
440 M1=W
450 GOSUB 670
460 D1=W
470 Y1=VAL(W$)
480 IF M1<1 THEN 700
490 IF M1>12 THEN 700
500 IF D1<1 THEN 700
510 IF D8(2)=28 THEN 560
520 D8(2)=28
530 FOR M9=3 TO 12
540     D9(M9)=D9(M9)-1
550 NEXT M9
560 IF Y1-INT(Y1/4)*4>0 THEN 610
570 D8(2)=29
580 FOR M9=3 TO 12
590     D9(M9)=D9(M9)+1
600 NEXT M9
610 IF D1>D8(M1) THEN 700
620 S1=INT(Y1*365.25+.75)
630 S=S1+D9(M1)+D1
640 IF S-INT(S/7)*7=6 THEN 790
650 PRINT "MUST BE A FRIDAY - ENTER WEEK-ENDING";
660 GOTO 710
670 FOR W1=1 TO LEN(W$)
680     IF MID$(W$,W1,1)="/" THEN 730
690 NEXT W1
700 PRINT "ENTER DESIRED WEEK AS MM/DD/YY";
710 INPUT W$
720 GOTO 430
730 W=VAL(LEFT$(W$,W1-1))
740 W$=RIGHT$(W$,LEN(W$)-W1)
750 RETURN
760 PRINT "DECLARE DIST";STR$(Y1);".DAT AS ALL RD UPD"
770 PRINT "DISTRICTS MAY THEN REPORT IN NEW YEAR"
780 STOP
790 FILE :1,"DIST"+STR$(Y1)+".DAT%"
800 S1=(((S+1)/7)-(1+INT((S1+1)/7)))*D0+1
810 S1=(S1-1)*50+1
820 GOTO 840
830 S1=INT((S+22)/(50*D0))*D0*50+1
840 W$=STR$(M1)+"/"+STR$(D1)+"/"+STR$(Y1)
850 IF S1>LOF(:1) THEN 1090
860 S=S1
870 FOR D=2 TO D0
880	S=S+50
890	IF S>LOF(:1) THEN 930
900     SET :1,S
910     READ :1,M,M,T0
920	IF M>0 THEN 940
930     P$=P$+D$(D)+"  "
940 NEXT D
950 PRINT
960 IF LEN(P$)>0 THEN 990
970 PRINT "ALL DISTRICTS REPORTED FOR ";W$
980 GOTO 1010
990 PRINT P$
1000 PRINT "NOT REPORTED FOR ";W$
1010 PRINT
1020 SET :1,S1
1030 READ :1,M,M,T0
1040 IF M=0 THEN 1110
1050 PRINT "ALTER REGION FIGURES";
1060 INPUT Z$
1070 IF LEFT$(Z$,1)="Y" THEN 1120
1080 GOTO 1510
1090 PRINT "NO DISTRICTS REPORTED FOR ";W$
1100 PRINT
1110 PRINT "ENTER REGION FIGURES"
1120 PRINT
1130 PRINT "MONTH-TO-DATE:"
1140 PRINT "HOURS ";
1150 INPUT F(1,1)
1160 PRINT "DOLLARS";
1170 INPUT F(1,2)
1180 PRINT "MONTH-END (FORECAST):"
1190 PRINT "HOURS ";
1200 INPUT F(1,3)
1210 PRINT "STORAGE";
1220 INPUT F(1,4)
1230 PRINT "ACTUAL FOR WEEK:"
1240 PRINT "HOURS ";
1250 INPUT F(1,5)
1260 PRINT "DOLLARS";
1270 INPUT F(1,6)
1280 PRINT "HOW MANY BILLABLE DAYS IN THIS WEEK";
1290 INPUT B
1300 IF F(1,1)=0 THEN 1320
1310 F(1,7)=F(1,2)/F(1,1)
1320 F(1,8)=F(1,3)*F(1,7)
1330 IF F(1,8)<10 THEN 1380
1340 IF F(1,8)<100 THEN 1370
1350 F(1,8)=100*INT((F(1,8)+50)/100)
1360 GOTO 1380
1370 F(1,8)=10*INT((F(1,8)+5)/10)
1380 F(1,9)=F(1,4)+F(1,8)
1390 F(1,10)=F(1,5)/B
1400 IF F(1,5)=0 THEN 1420
1410 F(1,11)=F(1,6)/F(1,5)
1420 SET :1,S1
1430 WRITE :1,M1,D1,0
1440 FOR F=1 TO 11
1450    F(4,F)=F(1,F)
1460    FOR C=1 TO 4
1470    WRITE :1,F(C,F)
1480	NEXT C
1490 NEXT F
1500 WRITE :1,B,0,0
1510 FILE #2,"REGWK.DAT"
1512 SCRATCH #2
1519
1520 PRINT #2 <PA>
1530 FOR D=1 TO D0
1540    FOR Z=1 TO 3
1550            PRINT #2
1560    NEXT Z
1570    SET :1,S
1580	IF LOC(:1)>LOF(:1) THEN 1610
1590    READ :1,M,M,T0
1600    IF M>0 THEN 1630
1610    PRINT #2 "NO FIGURES FOR ";D$(D);" FOR ";W$
1620    GOTO 2090
1630    T=T+T0
1640    MAT F=ZER
1650    FOR F=1 TO 11
1660            FOR C=1 TO 4
1670                    READ :1,F(C,F)
1680            NEXT C
1690    NEXT F
1700    MAT T=T+F
1710    READ :1,B,P1,P2
1720    P3=P3+P1
1730    P4=P4+P2
1740    PRINT #2 D$(D);
1750    IF D=1 THEN 1770
1760    PRINT #2 " DISTRICT";
1770    PRINT #2 " AS OF ";W$
1780    PRINT #2
1790    PRINT #2 "MONTH-TO-DATE                      MONTH-END (FORECAST)"
1800    PRINT #2 "---------------------------------  ------------------------------------"
1810    PRINT #2 "           HOURS   DOLLARS   $/HR  HOURS   DOLLARS   STORAGE$    TOTAL$"
1820    FOR C=1 TO 4
1830            IF D>1 THEN 1850
1840            IF C>1 THEN 1900
1850            PRINT #2 USING 1860,C$(C),F(C,1),F(C,2),F(C,7),F(C,3),F(C,8),F(C,4),F(C,9)
1860            :'LLLLLLLL ##,### #####,### ###.## ##,### #####,### ##,###,### #####,###
1870            IF C<>3 THEN 1890
1880            PRINT #2 "           -----   -------  -----  -----  --------   --------   -------"
1890    NEXT C
1900    PRINT #2
1910    PRINT #2 "CURRENT WEEK"
1920    PRINT #2 "----------------------------------------"
1930    PRINT #2 "           HOURS HRS/DAY  DOLLARS   $/HR"
1940    FOR C=1 TO 4
1950            IF D>1 THEN 1970
1960            IF C>1 THEN 2020
1970            PRINT #2 USING 1980,C$(C),F(C,5),F(C,10),F(C,6),F(C,11)
1980            :'LLLLLLLL ##,### #####.#  ###,### ###.##
1990            IF C<>3 THEN 2010
2000            PRINT #2 "          ------ -------  ------- ------"
2010    NEXT C
2020    IF D=1 THEN 2090
2030    PRINT #2
2040    PRINT #2 "PRODUCTS"
2050    PRINT #2 "--------------------"
2060    PRINT #2 "LEASED   SOLD  TOTAL    TASC"
2070    PRINT #2 USING 2080,P1,P2,P1+P2,T0
2080    :##,### ##,### ##,###  ##,###
2090    S=S+50
2100 NEXT D
2110 PRINT #2 <PA> "CENTRAL REGION TOTALS AS OF ";W$
2120 PRINT #2
2130 PRINT #2 "MONTH-TO-DATE                      MONTH-END (FORECAST)"
2140 PRINT #2 "---------------------------------  ------------------------------------"
2150 PRINT #2 "           HOURS   DOLLARS   $/HR  HOURS   DOLLARS   STORAGE$    TOTAL$"
2160 FOR C=1 TO 4
2170    T(C,7)=T(C,2)/T(C,1)
2180    PRINT #2 USING 1860,C$(C),T(C,1),T(C,2),T(C,7),T(C,3),T(C,8),T(C,4),T(C,9)
2190    IF C<>3 THEN 2210
2200    PRINT #2 "           -----   -------  -----  -----  --------   --------   -------"
2210 NEXT C
2220 FOR Z=1 TO 3
2230    PRINT #2
2240 NEXT Z
2250 PRINT #2 "CURRENT WEEK"
2260 PRINT #2 "---------------------------------------------------"
2270 PRINT #2 "                    HOURS  HRS/DAY  DOLLARS    $/HR"
2275 FOR C=1 TO 4
2280 S=S1
2286 PRINT #2 C$(C)
2290 FOR D=1 TO D0
2300    SET :1,S
2310    IF LOC(:1)>LOF(:1) THEN 2530
2320    READ :1,M,M,T0
2330    IF M=0 THEN 2530
2340    MAT F=ZER
2360	FOR F=1 TO 11
2365	FOR M=1 TO 4
2370		READ :1,F(M,F)
2375	NEXT M
2380	NEXT F
2460	IF F(C,5)=0 THEN 2530
2470            PRINT #2 USING 2480,D$(D),F(C,5),F(C,10),F(C,6),F(C,11)
2480            :'RRRRRRRRRRRRRRRR  ##,###  #####.#  ###,###  ###.##
2530    S=S+50
2540 NEXT D
2550 PRINT #2      "                   ------  -------  -------  ------"
2560 PRINT #2 USING 2480,"TOTAL",T(C,5),T(C,10),T(C,6),T(C,11)
2564 PRINT #2
2565 NEXT C
2570 FOR Z=1 TO 3
2580    PRINT #2
2590 NEXT Z
2600 PRINT #2 "PRODUCTS"
2610 PRINT #2 "------------------------"
2620 PRINT #2 " LEASED    SOLD    TOTAL            TASC"
2630 PRINT #2 USING 2640,P3,P4,P3+P4,T
2640 :###,### ###,### ####,###          ##,###
2650 PRINT #2 <PA>
2660 END
    