100 DIM I(132)
110 PRINT "FILENAME";
120 INPUT R$
130 R$=R$+"$"
140 FILE :1: R$
150 PRINT "CHANGE";
160 INPUT R$
162 PRINT "LOF =";LOF(1)
170 READ :1: I$
172 SET :1, LOC(1)-1
174 READ :1: I2$
176 IF I$=I2$ GO TO 200
177   SET :1, LOC(1)-1
178   PRINT "RETRY"
179   GO TO 170
200 T=LEN(I$)
201 I$=LEFT$(I$,35)
210 PRINT I$
220 IF R$="NO" GO TO 170
230 CHANGE I$ TO I
240 FOR J=1 TO I(0)
250   IF I(J)<100 GO TO 270
260     I(J)=I(J)-100
270 PRINT I(J);
280 NEXT J
290 PRINT
300 GO TO 170
310 END
  