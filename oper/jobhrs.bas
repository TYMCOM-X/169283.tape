10 FILES WTDSJ,ASHEMP,ASHRTE,MJCST2,MJCST3
16 X$="JOB"
17 PRINT USING 18,X$
18 :'RRRRR  LAST CC    HRS     QTY
19 PRINT 
20S4=0
30 Y=50
32 E$="TOT OPER CODE"
34 F$="TOT JOB"
35 G$="GRAND TOTAL"
40 DIM D(50),D$(50),M(50),N(50),K(50),P(50),L(50)
50 FOR I=1 TO Y
60 IF END #3 THEN 100
70 READ #3,D(I),D$(I),M(I),N(I),K(I),P(I),L(I)
80 D2=D2+1
90 NEXT I
100 S=1
110 X=50
120 SCRATCH #5
130 DIM A(50),B$(50)
140 FOR J=1 TO X
150 IF END #2 THEN 190
160 READ #2,A(J),B$(J)
170 D3=D3+1
180 NEXT J
190 FOR J=1 TO D3
200 GO TO 220
210 NEXT J
220 PRINT
230 IF END #1 THEN 1048
240 INPUT #1,C1,C2,E1,C3,E3,C4,B1,C5,C6
241 IF C1=0 THEN 230
242 IF C1<>0 THEN 250
244 IF C3>9 THEN 250
246 C3=22
250 IF C2>199 THEN 258
255 C3=2
258 IF S=1 THEN 290
260 IF C3<>A1 THEN 510
270 IF C2<>A2 THEN 500
280 IF C1<>A4 THEN 510
290 S=2
320 IF C3>10 THEN 400
330 T1=T1+C4
340 T2=T2+B1
350 T3=T3+C5
360 T7=T7+C4
370 T8=T8+B1
380 T9=T9+C5
385 W6=E3
386 W1=C6
390 GO TO 430
400 T1=T1+C4
420 W6=E3
430 A1=C3
440 A2=C2
450 A4=C1
460 IF C1=0 THEN 490
480 IF Z5=5 THEN 510
490 GO TO 230
500 IF C3<>A1 THEN 830
510 IF A1<10 THEN 536
520 IF C3>9 THEN 290
535 :'LLLLLLLLLLLLLLLLLLLLLL    #######.# ####.# ##########
536 IF Z7=5 THEN 1042
537 IF END #4 THEN 2000
540 IF S4=1 THEN 555
550 INPUT #4,M1,M2,M3,M4,M5,M6,M7,M8
552 IF M2=0 THEN 550
555 IF Z5=6 THEN 710
560 IF  A4>M1 THEN 710
570 IF M1>A4 THEN 1042
580 IF A2>M2  THEN 710
590 IF M2>A2 THEN 1042
600 IF A1>M3 THEN 710
610 IF M3>A1 THEN 1042
620 W3=T1+M5
630 W4=T2+M6
640 W2=W1+M8
650 W5=T3+M7
655 IF M1>A4 THEN 680
660 S4=0
670 GO TO 690
680 S4=1
690 PRINT #5,USING 740,A4,A2,A1,W6,W3,W4,W5,W2
692 W2=0
693 W3=0
694 W4=0
695 W5=0
700 GO  TO 750
710 PRINT #5,USING 740,M1,M2,M3,M4,M5,M6,M7,M8
720 S4=0
725 IF Z6=5 THEN 1041
730 GO TO 536
740 : ####### ##### ### #### ######.# #####.# ######### ##
750 T4=T4+T1
760 T5=T5+T2
770 T6=T6+T3
780 T1=0
790 T2=0
800 T3=0
805 N8=A2
810 A2=C2
812 IF END #1 THEN 963
815 IF C1<>A4 THEN 963
820 GO TO 270
830 IF C1=0 THEN 280
840 PRINT "TOT CC",T4,T5,T6
850 FOR I=1 TO D2
860 IF D(I)=C2 THEN 880
870 NEXT I
880 T4=0
890 T5=0
900 T6=0
910 IF C1<>A4 THEN 963
920 PRINT C2,D$(I)
930 T6=0
940 IF S=1 THEN 290
950 GO TO 280
963 IF A4=0 THEN 970
965 PRINT USING 966,A4,N8,T7,T9
966 :######## ##### ####.# ########
970 S1=S1+T7
980 S2=S2+T8
990 S3=S3+T9
1000 T7=0
1010 T8=0
1020 T9=0
1035 IF Z5=5 THEN 3000
1040 GO TO 290
1041 IF Z5=5 THEN 9000
1042 W3=T1
1043 W4=T2
1044 W5=T3
1045 W2=A3
1047 GO TO 680
1048 Z5=5
1049 IF Z6<>5 THEN 1054
1052 Z7=5
1054 GO TO 510
2000 Z6=5
2005 IF Z5=6 THEN 9000
2010 GO TO 1041
3000 PRINT
3010 PRINT USING 3020,S1,S3
3020:TOTAL       #######.# ########
3040 Z5=6
3045 IF Z6=5 THEN 9000
3050 GO TO 536
9000 END
 