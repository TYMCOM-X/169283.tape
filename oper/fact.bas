10 P1=555
20 GOSUB 520
30 PRINT
40 PRINT
50 PRINT "FACTOR GENERATOR"
60 PRINT
70 PRINT " NUMBER  FACTOR(S)"
80 PRINT
90 P1=P1+6
100DIM S(10)
110DIM A(1000),B(1000)
120A1=2
130I1=3
140A(1)=2
150A(2)=3
160FORI = 1 TO 1000 STEP 1
170B(I)=0
180NEXT I
190I1=I1+1
195 IF I1=201 THEN 650
200FOR I = 4 TO I1-1 STEP 1
210IF INT(I1/I)=(I1/I) THEN 260
220NEXT I
230A1=A1+1
240A(A1)=I1
250GOTO 190
260I2=I1
270FOR B = 1 TO A1
280FOR C = 1 TO A1
290IF I1/A(C)<>INT(I1/A(C)) THEN 330
300I1=I1/A(C)
310B(C)=B(C)+1
320GOTO 340
330NEXT C
340NEXT B
350 PRINT I2;TAB(10);
360F2=0
370FOR I = 1 TO A1
380IF B(I)=0 THEN 470
390IF F2=0 THEN 420
400 PRINT " *";A(I);
410GOTO 430
420 PRINT A(I);
430F2=1
440IF B(I)=1 THEN 460
450 PRINT " ^";B(I);
460REM
470NEXT I
480I1=I2
490PRINT
500 GOSUB 520
510GOTO 160
520 REM PAGES
530 IF P1>54 THEN 560
540 P1=P1+1
550 RETURN
560 PRINT
570 PRINT
580 PRINT
590 PRINT "."
600 PRINT
610 PRINT
620 P1=0
630 RETURN
650 END
 