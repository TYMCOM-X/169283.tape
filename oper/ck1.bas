5 DIM G(100)
10 OPEN 1,'AX',INPUT
20 GET 1:A,B,C,D,E
25 LET E=E+1
29 PRINT "NUMBER OF ELEMENTS";A
40 FOR I=1 TO A
50 GET 1:F
60  IF F>1 THEN 900
70  NEXT I
80 FOR I=1 TO A
90  FOR W=1 TO E
100 GET 1:G(W)
110 NEXT W
115 IF G(1)=0 THEN 970
120 IF G(W)>0 THEN 950
130 NEXT I
900 PRINT "ELEMENT TIME IN ERROR";F
910 GO TO 999
950 PRINT "PREDESSOR CHECK";G(1);G(2);G(3);G(4);G(5);G(6);G(7);G(8)
960 GO TO 90
970 PRINT "LAST VALID PREC DESCRIPTION IS NEAR ";I
980 GO TO 999
999 END
 