5 FILES TEST, OUT$
10 DIM L$(10)
20 FOR I=1 TO 6
30 READ #1 , L$(I)
40 PRINT L$(I)
50 NEXT I
60 FOR X=1 TO 500
65 SET :2,1
70 FOR I =1 TO 6
80 READ :2, M$(I)
150 IF M$(I)=L$(I) GO TO 300
160 PRINT "ER" ;X;I
170 PRINT "M$",M$(I)
180 PRINT "L$",L$(I)
300 NEXT I
400 NEXT X
500 END
    