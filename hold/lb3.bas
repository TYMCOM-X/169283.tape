10 RANDOMIZE
20 X = Y3 = A(3,1) = 15.65
30 X$ = Y$ = Z$ = "ABCDEFGH"
40 PRINT X,Y3,A(3,1)
50 PRINT X$,Y$,Z$
60 READ A,B,C$,D$,E$
70 DATA 1.65,376E-5,"##.###  'CCCCCC 'CCCC","ALPHA",BETA
80 PRINT "X=" X "SEE NO COMMA"
90 PRINT USING C$,Y3,D$,E$
200 FOR A = 1 TO 25
201 C=-0.995+6/A
202 ON C GO TO 210,220,230,240,250
205 PRINT "WHY HERE"
206 STOP
210 PRINT "1"
211 GO TO 300
220 PRINT "2"
221 GO TO 300
230 PRINT "3"
231 GO TO 300
240 PRINT "4"
241 GO TO 300
250 PRINT "5"
251 GO TO 300
300 Z=INT(100*RND)-50
310 PRINT Z,SGN(Z)+1
320 NEXT A
900 STOP
999 END
    