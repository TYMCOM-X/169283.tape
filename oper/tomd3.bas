100	DATA 10.0,32.5,20.35,25.9,13.0,12.7,9.2,21.0,27.0
110	PRINT "NUMBER","DEVIATION"
120	PRINT "------","---------"
130	:###.##        ###.##
140	S=0
200	FOR I=1 TO 9
210	READ N(I)
220	NEXT I
300	FOR J=1 TO I
310	S=S+N(J)
320	NEXT J
330	M=S/I
340	FOR J=1 TO I
350	D(J)=0.005+ABS(N(J)-M)
360	NEXT J
400	FOR J=1 TO I
410	PRINT USING 130, N(J),D(J)
420	NEXT J
430	PRINT
450	PRINT "THE MEAN IS EQUAL TO:",M
500	END
   