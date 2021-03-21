 1 PRINT
2 PRINT" *** WELCOME TO THE HARVARD COUNTRY CLUB:  THIS COURSE IS"
3 PRINT" *** AN EIGHTEEN-HOLE CHAMPIONSHIP LAYOUT."
4 PRINT
5 PRINT" *** THE COMMENTATOR WILL EXPLAIN THE COURSE AS YOU"
6 PRINT" *** PLAY.  ENJOY YOUR GAME; SEE YOU AT THE 19TH HOLE..."
7 PRINT
10  LET G1=18
20  LET G2=0
30  LET G3=0
40  LET A=0
50  LET N=.8
60  LET S2=0
70  LET F=1
80 PRINT"WHAT IS YOUR HANDICAP? ";
90 INPUT H
100 IF H>30 THEN 470
110 IF H<0 THEN 470
120 PRINT"DIFFICULTIES AT GOLF INCLUDE:"
130 PRINT"0=HOOK, 1=SLICE, 2=POOR DISTANCE, 4=TRAP SHOTS, 5=PUTTING"
140 PRINT"   WHICH ONE (ONLY) IS YOUR WORST? ";
150 INPUT T
160 IF T>5 THEN 120
170  LET S1=0
200 FOR Z=1 TO ((H+1)/(T+1))*10
210  LET R=RND(R)
220 NEXT Z
230  LET L(0)=0
240  LET J=0
245  LET Q=0
250  LET S2 = S2+1
260  LET K=0
270 IF F=1 THEN 310
290 PRINT"YOUR SCORE ON HOLE"F-1;" WAS"S1
291 GO TO 1750
292 IF S1>P+2 THEN 297
293 IF S1=P THEN 299
294 IF S1=P-1 THEN 301
295 IF S1=P-2 THEN 303
296 GO TO  310
297 PRINT"KEEP YOUR HEAD DOWN."
298 GO TO 310
299 PRINT"A PAR. NICE GOING."
300 GO TO 310
301 PRINT "A BIRDIE"
302 GO TO 310
303 IF P=3 THEN 306
304 PRINT"A GREAT BIG EAGLE:"
305 GO TO 310
306 PRINT "A HOLE IN ONE"
310 IF F=19 THEN 1710
315  LET S1=0
316 PRINT
320 IF S1=0 THEN 1590
330 IF L(0)<1 THEN 1150
340  LET X=0
350 IF L(0)>5 THEN 1190
360 PRINT"SHOT WENT";D1;"YARDS.  IT'S";D2;"YARDS FROM THE CUP."
362 PRINT"BALL IS";INT(O);"YARDS OFF LINE...IN ";
380 GOSUB 400
390 GOTO 620
400 IF L(X)=1 THEN 480
410 IF L(X)=2 THEN 500
420 IF L(X)=3 THEN 520
430 IF L(X)=4 THEN 540
440 IF L(X)=5 THEN 560
450 IF L(X)=6 THEN 580
460 PRINT"OUT OF BOUNDS"
465 GOTO 1690
470 PRINT"PGA RULES HANDICAP = 0 TO 30"
472 GOTO 150
480 PRINT"FAIRWAY"
490 GOTO 1690
500 PRINT"ROUGH"
510 GOTO 1690
520 PRINT"TREES"
530 GOTO 1690
540 PRINT"ADJACENT FAIRWAY"
550 GOTO 1690
560 PRINT"TRAP"
570 GOTO 1690
580 PRINT"WATER"
590 GOTO 1690
620 IF A=1 THEN 629
621 PRINT"SELECTION OF CLUBS"
622 PRINT"YARDAGE DESIRED                   SUGGESTED CLUBS"
623 PRINT"200 TO 280 YARDS                      1 TO  4"
624 PRINT"100 TO 200 YARDS                     19 TO 13"
625 PRINT"  0 TO 100 YARDS                     29 TO 23"
626  LET A=1
629 PRINT"WHAT CLUB DO YOU CHOOSE? ";
630 INPUT C
632 PRINT
635 IF C<1 THEN 690
637 IF C>29 THEN 690
640 IF C>4 THEN 710
650 IF L(0)<5 THEN 740
660 IF C=14 THEN 740
665 IF C=23 THEN 740
670 GOTO 690
680  LET S1 = S1-1
690 PRINT"THAT CLUB IS NOT IN THE BAG."
693 PRINT
700 GOTO 620
710 IF C<12 THEN 690
720  LET C=C-6
730 GOTO 650
740  LET S1 = S1+1
741  LET W=1
742 IF C>13 THEN 960
746 IF INT(F/3)=F/3 THEN 952
752 IF C<4 THEN 756
754 GOTO 760
756 IF L(0)=2 THEN 862
760 IF S1>7 THEN 867
770  LET D1=INT(((30-H)*2.5+187-((30-H)*.25+15)*C/2)+25*RND(R))
780  LET D1=INT(D1*W)
800 IF T=2 THEN 1170
830  LET O=(RND(R)/.8)*(2*H+16)*ABS(TAN(D1*.0035))
840  LET D2=INT(SQR(O^2+ABS(D-D1)^2))
850 IF D-D1<0THEN 870
860 GOTO 890
862 PRINT"YOU DUBBED IT."
864  LET D1=35
866 GOTO 830
867 IF D<200 THEN 1300
868 GOTO 770
870 IF D2<20 THEN 890
880 PRINT"TOO MUCH CLUB.  YOU'RE PAST THE HOLE."
890  LET B=D
900  LET D=D2
910 IF D2>27 THEN 1020
920 IF D2>20 THEN 1100
930 IF D2>.5 THEN 1120
940  LET L(0)=9
950 GOTO 1470
952 IF S2+Q+(10*(F-1)/18)<(F-1)*(72+((H+1)/.85))/18 THEN 956
954 GOTO 752
956  LET Q = Q+1
957 IF S1/2<>INT(S1/2) THEN 1011
958 GOTO 862
960 PRINT"YOU MAY NOW GUAGE YOUR DISTANCE BY PERCENT .01 TO .99"
961 PRINT"PERCENT FULL SWING ";
970 INPUT W
972 PRINT
980 IF W>1 THEN 680
985 I F L(0) = 5 THEN 1280
990 IF C=14 THEN 760
1000  LET C = C-10
1010 GOTO 760
1011 IF D<95 THEN 862
1012 PRINT"BALL HIT TREE - BOUNCED INTO ROUGH"D-75;" YARDS FROM HOLE."
1014  LET D=D-75
1018 GOTO 620
1020 IF O<30 THEN 1150
1022 IF J>0 THEN 1150
1030 IF T>0 THEN 1070
1035  LET S9=(S2+1)/15
1036 IF INT(S9)=S9 THEN 1075
1040 PRINT"YOU HOOKED- ";
1050  LET L(0)=L(2)
1055 IF O>45 THEN 1092
1060 GOTO 320
1070  LET S9=(S2+1)/15
1071 IF INT(S9)=S9 THEN 1040
1075 PRINT"YOU SLICED- ";
1080  LET L(0)=L(1)
1090 GOTO 1055
1092 PRINT"BADLY."
1094 GOTO 320
1100  LET L(0)=5
1110 GOTO 320
1120  LET L(0)=8
1130  LET D2=INT(D2*3)
1140 GOTO 1380
1150  LET L(0)=1
1160 GOTO 320
1170  LET D1=INT(.85*D1)
1180 GOTO 830
1190 IF L(0)>6 THEN 1260
1200 PRINT"YOUR SHOT WENT INTO WATER."
1210  LET S1 = S1+1
1220 PRINT"PENALTY STROKE ASSESSED. HIT FROM PREVIOUS LOCATION."
1230  LET J=J+1
1240  LET L(0)=1
1242  LET D=B
1250 GOTO 620
1260 PRINT"YOUR SHOT WENT OUT OF BOUNDS."
1270 GOTO1210
1280 IF T=3 THEN 1320
1300  LET D2=1+(3*INT((80/(40-H))*RND(R)))
1310 GOTO 1380
1320 IF RND(R)>N THEN 1360
1330  LET N=N*.2
1340 PRINT"SHOT DUBBED, STILL IN TRAP."
1350 GOTO 620
1360  LET N=.8
1370 GOTO 1300
1380 PRINT"ON THE GREEN"D2;" FEET FROM THE PIN."
1381 PRINT"CHOOSE YOUR PUTT DISTANCE BY POTENCY NUMBER 1 TO 13."
1382 PRINT"PUTT POTENCY NUMBER - ";
1400 INPUT I
1410  LET S1 = S1+1
1420 IF S1+1-P>(H*.072)+2 THEN 1470
1425 IF K>2 THEN 1470
1428  LET K=K+1
1430 IF T=4 THEN 1530
1440  LET D2=D2-I*(4+2*RND(R))+1.5
1450 IF D2<-2 THEN 1560
1460 IF D2>2 THEN 1500
1470 PRINT"YOU HOLED IT"
1472 PRINT
1480  LET F=F+1
1490 GOTO 230
1500 PRINT"PUTT SHORT."
1505  LET D2=INT(D2)
1510 GOTO 1380
1530  LET D2=D2-I*(4+1*RND(R))+1
1550 GOTO 1450
1560 PRINT"PASSED BY CUP."
1570  LET D2=-D2
1580 GOTO 1505
1590 READ D,P,L(1),L(2)
1595 PRINT
1600 PRINT"YOU ARE AT TEE OF HOLE"F;" DISTANCE"D;" YARDS, PAR"P;" "
1605  LET G3=G3+P
1620 PRINT"ON YOUR RIGHT IS ";
1630  LET X=1
1640 GOSUB 400
1650 PRINT"ON YOUR LEFT IS ";
1660  LET X=2
1670 GOSUB 400
1680 GOTO 620
1690 RETURN
1700 DATA 361,4,4,2,389,4,3,3,206,3,4,2,500,5,7,2
1702 DATA 408,4,2,4,359,4,6,4,424,4,4,2,388,4,4,4
1704 DATA 196,3,7,2,400,4,7,2,560,5,7,2,132,3,2,2
1706 DATA 357,4,4,4,294,4,2,4,475,5,2,3,375,4,4,2
1708 DATA 180,3,6,2,550,5,6,6
1710 PRINT
1750  LET G2=G2+S1
1760 PRINT"TOTAL PAR FOR "F-1;" HOLES IS "G3;" YOUR TOTAL IS" G2
1761 IF G1=F-1 THEN 1770
1765 GOTO 292
1770 END
                                                                                                                                                                                                                                                                                       