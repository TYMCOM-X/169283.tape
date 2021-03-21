

99 MARGIN #0, 119
100:                 TRUNK QUANTITY          EFFECTIVE
110:TRUNK GROUP    IN SVC  BEFORE  AFTER       DATE      TTO/TO    ORDER      STAT   BACK   YR   SENT      ACTION     
120:-----------    ------  ------  -----    ---------    ------    -----      ----   ----   --   -----   ----------
130:'LLL ###         ###    ###     ###     'LLLLLLLL    'EEEEEEEE 'LLLLLLLL   'L    'LLL   'L   'LLLLLLLL 'LLLLLLLLLL
140:               --------------------      OR DUE(*)             JOB               TIE         DATE
150 LET S$ = "-----------------------------------------------------------"
160 FILES STATUS
170 PR INT "WHICH TOLL CENTER IS TO BE PRINTED";
180 INPUT A$
190 IF A$ = "ALL" THEN 260
200 PRINT "IS THIS A RESTART ('YES' OR 'NO')";
210 INPUT Z6$
220 IF Z6$ = "YES" THEN 280
230 IF Z6$ = "NO" THEN 280
240 PRINT "ILLEGAL ANSWER - TRY AGAIN";
250 GO TO 210
260 LET Z6$ = "YES"
280 READ #1,B$,H3,C$,J3,K3,L3,D$,E$,O3,P3,F$,R3,C1$,B3
290 IF A$ = "ALL" THEN 350
300 IF B$ = A$ THEN 350
310 IF MORE #1 THEN 280
330 PRINT "NO SUCH TOLL CENTER CAN BE FOUND IN THE FILE"
340  STOP
350 FOR I = 1 TO 14
360 BACKSPACE #1
370 NEXT I
380 LET W = -49
390 GO SUB 730
400 READ #1,G$,H,I$,J,K,L,M$,N$,O,P,Q$,C8$,A1$,B8
410 IF G$ = B$ THEN  440
420LET B$ = G$
430 GO SUB 720
440 IF VPS(0) - W > 45 THEN 460
450 GO TO 470
460 GOSUB 1500
470 IF H = V THEN 521
480 LET V = H
490'***** ROUTINE FOR NEW TRUNK GROUP
500 PRINT
510 PRINT I$
511 LET P1=P1+1
513 LET Q2=Q2+J
520'***** ROUTINE FOR DETAIL LINES
521 IF K <> 0 THEN 535
522 IF L <> 0 THEN 600
525 LET L$ = "NO DATE"
526 LET W$ = " "
530 GO TO 651
535 LET X$ = STR$(K)
540 LET E = IDA-6
545 LET R$ = STR$(E)
550 LET P$ = EXT$(R$,5,6)
551 LET W$ = " "
560 LET O$ = STR$(30)
562 IF P$ > O$ THEN 568
563 GO TO 572
568 LET E = E-70
572 IF K > E THEN 579
573 LET W$ = "OVERDUE"
574 GO TO 580
579 LET W $ = " "
580 LET O$ = "*"
590 GO TO 620
600 LET X$ = STR$(L)
601 LET W$ = " "
610 LET O$ = " "
620 LET Y$ = EXT$ (X$,5,6)
630 LET Z$ = EXT$ (X$,1,2)
640 LET V$ = EXT$ (X$,3,4)
650 LET L$ = V$ + "/" + Y$ + "/" + Z$ + O$
651 IF B8 > 500000 THEN 654
652 LET C7$ = STR$(B8)
653 GO TO 660
654 LET X1$ = STR$(B8)
655 LET Y1$ = EXT$(X1$,5,6)
656 LET Z1$ = EXT$(X1$,1,2)
657 LET V1$ = EXT$(X1$,3,4)
658 LET C7$ = V1$ + "/" + Y1$ + "/" +Z1$
660 PRINT USING 130,G$,H,J,O,P,L$,M$,N$,Q$,C8$,A1$,C7$,W$
670 IF MORE #1 THEN 400
700 GO TO 1700
710 '*****SUBROUTINE FOR TOP OF EACH PAGE
720 IF Z6$ = "NO" THEN 1710
730 LET T=1
750 I F B$ <> "CN01" THEN 780
760 LET U$ = "DOS PALOS"
770 GO TO 1490
780 IF B$ <> "CN02" THEN 810
790 LET U$ = "EXETER"
800 GO TO 1490
810 IF B$ <> "CN03" THEN 840
820 LET U$ = "GARBERVILLE"
830 GO TO 1490
840 IF B$ <> "CN04" THEN870
850 LET U$ = "GILROY"
860 GO TO 1490
870 IF B$ <> "CN06" THEN 900
880 LET U$ = "SANGER"
890 GO TO 1490
900 IF B$ <> "CN07" THEN 930
910 LET U$ = "TAFT"
920 GO TO 1490
930 IF B$ <> "CN08" THEN 960
940 LET U$ = "WEAVERVILLE"
950 GO TO 1490
960 IF B$ <> "CN57" THEN 1000
970 LET U$ = "WOODLAND"
980 GO TO 1490
1000 IF B$ <> "CS01" THEN 1030
1010 LET U$ = "BARSTOW"
1020 GO TO 1490
1030  IF B$ <> "CS02" THEN1060
1040 LET U$ = "BISHOP"
1050 GO TO 1490
1060 IF B$ <> "CS03" THEN 1090
1070 LET U$ = "BLYTHE"
1080 GO TO 1490
1090 IF B$ <> "CS04" THEN 1120
1100 LET U$ = "RIDGECREST"
1110 GO TO 1490
1120 IF B$ <> "CS05" THEN 1160
1130 LET U$ = "VICTORVILLE"
1140 GO TO 1490
1160 IF B$ <> "NV01" THEN 1190
1170 LET U$ = "GARDNERVILLE"
1180 GO TO 1490
1190 IF B$ <> "NV02" THEN 1220
1200 LET U$ = "HENDERSON"
1210 GO TO 1490
1220 IF B$ <> "NV03" THEN 1250
1230 LET U$ = "YERINGTON"
1240 GO TO 1490
1250 LET Z3 = 1
1260 IF B$ <> "CN05" THEN 1290
1270 LET U$ = "MANTECA"
1280 GO TO 1490
1290 IF B$ <> "CN53"TH EN 1320
1300 LET U$ = "HANFORD"
1310 GO TO 1490
1320 IF B$ <> "CN51" THEN1350
1330 LET U$ = "BAKERSFIELD"
1340 GO TO 1490
1350 IF B$ <> "CN54" THEN 1380
1360 LET U$ = "LODI"
1370 GO TO 1490
1380 IF B$ <> "CN56" THEN1410
1390 LET U$ = "STOCKTON"
1400 GO TO 1490
1410 IF B$ <> "CN55" THEN 1440
1420 LET U$ = "MERCED"
1430 GO TO 1490
1440 IF B$ <> "CN52" THEN 1470
1450 LET U$ = "FRESNO"
1460 GO TO 1490
1470 PRINT "THIS PROGRAM DOES NOT CONTAIN A NAME FOR YOUR TOLL CENTER"
1480 LET U$ = "UNKNOWN"
1490 PRINT
1500 IF VPS (0)-W <49 THEN 1490
1510 PRINT S$;S$
1520 LET W = VPS(0)
1530 PRINT
1540 PRINT
1550 PRINT TAB  (48); "CONTINENTAL TELEPHONE"
1560 PRINT TAB (45); "TRAFFIC TRUNK ADMINISTRATION"
1570 PRINT TAB (48); "TRUNK STATUS REPORT"
1580 PRINT TAB (54); DAT$
1590 PRINT
1600 PRINT U$; " - PAGE"; T
1610 PRINT
1620 LET T = T+1
1630 PRINT USING 100
1640 PRINT USING 140
1650 PRINT USING 110
1660 PRINT USING 120
1670 PRINT
1680 GO TO 1740
1690'***** ROUTINE FOR LAST PAGE
1700 PRINT
1710 IF VPS (0) -W < 49 THEN 1700
1720 PRINT S$; S$
1721 PRINT P1;Q2
1730 STOP
1740 RETURN
1750 PRINT P1;Q2
1751 END
  