15 : JOB #########
20 FILE #1,"MJCST2",#3,"CCRATE"
100 S=1
110 Y=50
115 PAGE 55
120 A$="TOTAL"
130 B$="COST"
135 C$="WIP TOTALS"
145 DIM D(50),D$(50),M(50),N(50),K(50),P(50),L(50)
150 FOR I=1 TO Y
155 IF END #3 THEN 200
160 READ #3,D(I),D$(I),M(I),N(I),K(I),P(I),L(I)
165 D2=D2+1
170 NEXT I
200 PRINT "ENTER JOB NO."
205 X=150
210 DIM B(150)
220 FOR J=1 TO X
225 INPUT B(J)
230 IF B(J) = 9 THEN 250
240 NEXT J
250 PRINT
260 FOR J=1 TO X
270 IF S=1 THEN 335
300 PRINT
310 IF END #1 THEN 605
320 INPUT #1,M1,M2,M3,M4,M5,M6,M7,M8
325 IF S=2 THEN 340
335 IF M1<>B(J) THEN 470
337 IF S=1 THEN 350
340 IF M1<>A2 THEN 500
345 GO TO 358
350 PRINT USING 15,M1
351 PRINT
353 PRINT USING 354,B$
354 :COST CENTER      MDATE      QTY   OPER  HRS     RATE  'RRRRRR
355 S=2
356 PRINT
357 IF M2>999 THEN 380
358 FOR I=1 TO D2
360 IF D(I)=M2 THEN 375
370 NEXT I
375 IF L(I)>9000 THEN 700
380 IF M2>799 THEN 800
381 A1=M5*L(I)
382 A5=L(I)
385 A2=M1
400 PRINT USING 405,M2,D$(I),M4,M7,M3,M5,A5,A1
405 :#### 'LLLLLLLLLLL #### ######### ### ######.# ###.## ######.##
420 T1=T1+M5
425 T2=T2+A1
430 T5=T5+M7
450 GO TO 310
470 Z1=Z1+M5
472 Z2=Z2+M7
480 GO TO 310
500 PRINT
510 PRINT USING 515,A$,T5,T1,T2
515 :'LLLLLLLLLLLL         ##########    #######.#      ########.##
517 PRINT
518 PRINT
520 T3=T3+T1
525 T4=T4+T2
527 T6=T6+T5
530 T1=0
535 T2=0
537 T5=0
540 S=1
600 NEXT J
605 PRINT
615 PRINT
623 T7=T6+Z2
624 T8=T3+Z1
630 PRINT
635 PRINT USING 515,C$,T7,T8
650 GO TO 900
700 FOR L= 1 TO D3
705 IF L(I)<>W(L) THEN 750
710 IF M3<>X(L) THEN 750
720 IF M8=Y(L) THEN 760
750 NEXT L
755 PRINT "NO MATCH"
760 A1=M5*Z(L)
765 A5=Z(L)
780 GO TO 385
800 A1=M7
802 M7=0
810 GO TO 382
900 END
    