0001�	
  0002�	100 PRINT"THIS PROGRAM SOLVES A QUADRATIC EQUATION"
 0003�	110 PRINT
   0004�	120 PRINT
   0005�	130 PRINT A,B,C
  0006�	140 INPUT A,B,C
  0007�	150 LET X1=B^2-4*C
    0008�	160 IF X1>0 THEN 190
  0009�	170 IF X1<0 THEN 195
  0010�	180 IF X1=0 THEN 197
  0011�	195 PRINT "IMAGINARY ROOTS"
0012�	190 PRINT "REAL ROOTS"
0013�	197 PRINT "EQUAL ROOTS"
    0014�	192 GO TO 200
    0015�	196 GO TO 200
    0016�	198 GO TO 200
    0017�	200 LET D1=-B+X1
 0018�	210 LET D12=D1/2*A
    0019�	220 LET D2=-B-X1
 0020�	230 LET D21=D2/2*A
    0021�	240 PRINT D12,D21
0022�	250 END
