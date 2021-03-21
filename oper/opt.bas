100 PRINT "ENTER PART NUMBER";
110 INPUT A$
120 PRINT "ENTER PART DESCRIPTION";
130 INPUT E$
140 PRINT "ENTER PRODUCT LINE";
150 INPUT L
160 PRINT "ENTER NUMBER OF QUOTES";
170 INPUT N
180 PRINT"ENTER QUANTITY & PRICE PER QUOTE IN PAIRS(LOWEST VOLUME 1ST)."
190 FOR I=1 TO N
200 INPUT X(I),Y(I)
210 NEXT I
220 PRINT"ENTER YEARLY PRODUCTION QUANTITY NEEDED";
230 INPUT M1
240 M=M1
250 PRINT"PRIOR YEAR SERVICE VOLUME";
260 INPUT R
270 M=(M+R)/12
280 T=M1/12
290 FOR I = 1 TO N
300 Z(I)=X(I)/M
310 A(I)=X(I)*Y(I)
320 NEXT I
330 V(1)=0
340 FOR J=2 TO N
350 V(J)=-((Y(J)-Y(1))*X(J))
360 NEXT J
370 FOR I=1 TO N
380 B(I)=(A(I)*.015)*(Z(I)/2)
390 C(I)=V(I)-B(I)
400 D(I)=12/Z(I)*C(I)
410 NEXT I
420 FOR I= 1 TO 5
430 PRINT
440 NEXT I
450:'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
460 L$="OPTIMUM BUY ANALYSIS FOR SLIDING SCALE PARTS"
470 PRINT USING 450,L$
480 PRINT
490 PRINT "DATE MAY 4, 1973"
500 PRINT
510 PRINT
520 PRINT"PART NUMBER                         ";A$
530 PRINT
540 PRINT"PART DESCRIPTION                    ";E$
550 PRINT
560 PRINT"PRODUCT LINE                       ";L
570 PRINT
580 PRINT"PRIOR YEAR SERVICE VOLUME          ";R
590 PRINT
600 PRINT"YEARLY PRODUCTION VOLUME           ";M1
610 PRINT
620 PRINT"MONTHLY VOLUME REQUIRED            ";T
630 PRINT
640 PRINT TAB (28);"SLIDING SCALE"
650 PRINT TAB (28);"------- -----"
660 PRINT
670 PRINT TAB(28);"QUANT";"  COST EA."
680 PRINT
690 FOR K = 1 TO N
700 PRINT TAB(28);X(K);TAB(36);Y(K)
710 NEXT K
720 FOR I = 1 TO 3
730 PRINT
740 NEXT I
750 L$="FINANCIAL JUSTIFICATION"
760 PRINT USING 450,L$
770 PRINT
780 PRINT
790: PURCHASE  TOTAL   MONTHS  PURCHASE  INTREST    NET      SAV.
800:  LEVELS   COSTS   OF INV  SAVINGS     LOSS     SAV      ANN.
810: #####    #####.##  ##.##  #####.##  ####.##  #####.##  #####.##
820 PRINT USING 790,
830 PRINT USING 800,
840 PRINT USING 860,
850 FOR I=1 TO N
860:  ======   =====   == ===  =======     ====     ===       ===
870 PRINT USING 810,X(I),A(I),Z(I),V(I),B(I),C(I),D(I)
880 NEXT I
890 FOR J=1 TO N-1
900 F=J+1
910 FOR K=F TO N
920 IF D(J)<=D(K)THEN 990
930 T1=D(J)
940 T2=X(J)
950 D(J)=D(K)
960 X(J)=X(K)
970 D(K)=T1
980 X(K)=T2
990 NEXT K
1000 NEXT J
1010 PRINT
1020 PRINT
1030:                 OPTIMIM BUY QUANT. IS ######
1040 PRINT USING 1030,X(N)
1050 END
  