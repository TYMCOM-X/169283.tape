4 DIM F(5),G(5,5),H(5,5)
5 DEF FNA(X,Y)=SIN(X)+TAN(Y)-10.
10 L1=12.6E-3
12 INPUT  A,B,C$
15 PRINT USING C$,A,B
16 GO SUB 205
20 PRINT A,B
21 GO TO 410
25 READ L,M,N
30 DATA 10.,7.6,10.E7,15.4,17.9,36.E-5
40 PRINT L,M,N
41 PRINT L;M;N
50 FOR Q = COS(A) TO TAN(A) STEP SIN(A)/4.
55 PRINT "Q= ";Q
60 NEXT Q
110 ON TAN(B) GO TO 115,200,300
115 PRINT "HAVE GONE HERE WITH 1 AT 110"
120 IF SIN(A) >= TAN(A)-5 THEN 150
130 STOP
150 GO TO 12
200 PRINT "HAVE GONE HERE WITH  2 IN 110"
201 GO TO 410
205 FOR L = 1 TO 5
210 PRINT L,A,B,C,FNA(A,B)
215 NEXT L
220 RETURN
300 PRINT " HAVE GONE HERE WITH A 3 IN 110"
315 FOR L =  1 TO 20
320 PRINT INT(100*RND)
325 NEXT L
330 GO TO 12
410 Z=FNA(A,B)+10.
420 PRINT FNA(10.,Z)+Z
430 GO TO 25
5000 END
 