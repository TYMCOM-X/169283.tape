1 DIM M(100,4),D(4,3),C5(3)
40 K1$="MC"
50 K$="AS"
55 K2$="ACY"
60 PRINT ":";
70 INPUT E$
80 L$=LEFT$(E$,1)
100 L6=INSTR(L$,"E")
110 IF L6<>0 THEN 150
120 REM USER IN BEGINNERS MODE
130 E=1
140 GO TO 160
150 E=2
155 E9=0
160 ON E GO TO 170,190
170 PRINT "ISSUING BODY";
180 GO TO 200
190 PRINT "ISS";
200 INPUT I$
210 ON E GO TO 220,240
220 PRINT "ANNUAL OR SEMI-ANNUAL";
230 GO TO 250
240 PRINT "A OR S";
250 INPUT T$
260 L$=LEFT$(T$,1)
270 F1=INSTR(K$,L$)
280 IF F1<>0 THEN 340
290 ON E GO TO 320,300
300 GOSUB 2440
310 GO TO 330
320 PRINT T$ ;" INCORRECT RESPONSE "
330 ON E GO TO 220,240
340 ON E GO TO 350,370
350 PRINT "SPREAD";
360 GO TO 380
370 PRINT "SP";
380 INPUT S5$
390 LET H$=S5$
400 GOSUB 4830
410 IF F3=0 THEN 430
415 ON E GO TO 350,420
420 GOSUB 2440
425 ON E GO TO 350,370
430 G=1
440 ON E GO TO 450,470
450 PRINT "DATED DATE";
460 GO TO 480
470 PRINT "DD";
480 INPUT D1$,D2$,D3$
490 GOSUB 2100
500 IF D0<>1 GO TO 580
520 IF E=1 GO TO 570
530 GOSUB 2440
570 ON G GO TO 430,590,660,730
580 ON G GO TO 590,660,730,800
590 G=2
600 ON E GO TO 610,630
610 PRINT "DELIVERY DATE";
620 GO TO 640
630 PRINT "DL";
640 INPUT D1$,D2$,D3$
650 GO TO 490
660 G=3
670 ON E GO TO 680,700
680 PRINT "FIRST INTEREST DATE";
690 GO TO 710
700 PRINT "FI";
710 INPUT D1$,D2$,D3$
720 GO TO 490
730 G=4
740 ON E GO TO 750,770
750 PRINT "FIRST PRINCIPAL DATE";
760 GO TO 780
770 PRINT "FP";
780 INPUT D1$,D2$,D3$
790 GO TO 490
800 ON E GO TO 810,830
810 PRINT " ENTER BY MATURITY OR COLUMN";
820 GO TO 840
830 PRINT " M OR C";
840 INPUT T5$
850 L$=LEFT$(T5$,1)
860 L7=INSTR(K1$,L$)
870 IF L7<>0 THEN 890
880 PRINT " ERROR IN RESPONSE TO LAST QUESTION RETYPE"
885 GO TO 800
890 GO TO 4000
2100 REM SUBROUTINE FCHECKS FOR ALPHA AND RANGES OF NUMERICS
2110 D0=0
2115 LET H$=D1$
2120 GOSUB 4830
2130 IF F3=0 THEN 2155
2140 ON G GO TO 440,600,670,740
2155 LET H$=D2$
2160 GOSUB 4830
2165 IF F3=0 THEN 2175
2170 ON G GO TO 440,600,670,740
2175 LET H$=D3$
2180 GOSUB 4830
2190 IF F3=0 THEN 2220
2200 ON G GO TO 440,600,670,740
2220 LET D(G,1)=VAL(D1$)
2230 LET D(G,2)=VAL(D2$)
2240 LET D(G,3)=VAL(D3$)
2320 IF D(G,1)<=12 GO TO 2360
2330 PRINT " MONTH OUT OF RANGE PLEASE RETYPE"
2340 D0=1
2350 GO TO 2430
2360 IF D(G,2)<=31 THEN 2400
2370 PRINT "DAY OUR OF RANGE PLEASE RETYPE"
2380D0=1
2390 GO TO 2430
2400 IF D(G,3)>1960 GO TO 2430
2410 PRINT "YEAR OUT OF RANGE PLEASE RETYPE"
2420 D0=1
2430 RETURN
2440 REM THIS ROUTINE COUNTS ERRORS MADE IN EXPERT MODE
2450 REM MORE THAN 3 ERRORS AND  USER SWITCHED TO BEGINNERS MODE
2460 E9=E9+1
2470 IF E9>=3 GO TO 2490
2480 GO TO 2510
2490 PRINT "TOO MANY ERRORS IN EXPERT MODE  SWITCHING TO LONG FORM"
2500 E=1
2510 RETURN
 