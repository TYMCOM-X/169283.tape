90 DIM T(100)
100 PRINT "BALANCE";
110 INPUT A
120 FOR I=1 TO 100
130 INPUT T(I)
140 IF T(I)=0 GO TO 170
150 NEXT I
170 FOR J=1 TO I
180 A=A-T(J)
190 PRINT T(J);TAB(10);A
200 NEXT J
999 END
   