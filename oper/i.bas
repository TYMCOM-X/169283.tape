100 DIM M(137,12)
125 DIM D(137,3)
150 DIM Y(30,12)
151 DIM B(137,14),T(137),Z(30)
152 DIM Q(30,3)
153 DIM C(137)
200 DIM S(12,13)
201 DIM U(137),K(137)
225 Y$="YES"
260 I$="NO"
265 PRINT "TOTAL ASSET $";
266 INPUT B7
270 PRINT " % SUBSIDIARY,NEW OR USED,TAX LIFE,TAX RATE"
280 INPUT A6,Z$,Y1,T5
281 B6=A6*B7
282 IF MID$(Z$,1,4)<>"READ" THEN 290
283 PRINT "READ FROM:";
284 INPUT D$
290 PRINT"# YEARS IN LEASE? # PERIODS PER YEAR?"
300 INPUT Q1,Q6
310 PRINT"INTEREST RATE/YEAR ON AMORTIZED DEBT? # PERIODS ON INTEREST ONLY(IF ANY)?"
320 INPUT I1,N8
321 GO TO 350
330 PRINT"AMOUNT OF BALLOON(IF ANY)? INTEREST RATE PER YEAR?"
340 INPUT B5,I5
350 PRINT "% ITC,% FRONT END FEES"
358 INPUT X5,F
360 F7=F*B7
361 F=F*B6
363 PRINT "CAPITALIZE FEE"
364 INPUT K$
365 IF MID$(K$,3,3)="IDC" THEN 368
366 IF MID$(K$,4,3)="IDC" THEN 370
367 GO TO 382
368 K$="NO"
369 GO TO 371
370 K$="YES"
371 J$="IDC"
372 PRINT "TOTAL IDC ON ASSET"
373 INPUT N4
374 N3=N4*A6
382 IF MID$(Z$,1,4)="USED" THEN 388
384 M9=200
386 GO TO 410
388 M9=150
410 PRINT "% RESIDUAL FOR YIELD"
420 INPUT R7
421 R7=R7*B6
440 Q=Q1*Q6
450 N2=Q-N8
505 P$="YES"
506 PRINT "ADVANCE RENTALS";
507 INPUT A$
508 IF A$="NO" THEN 514
509 PRINT "LEASE FACTOR TO TRUST";
510 INPUT L1
511 PRINT "AMORT DEBT OVER PDS-1"
512 INPUT N$
513 GO TO 520
515 INPUT L1
520 PRINT "% EQUITY OF SUBSID."
530 INPUT E1
535 PRINT " $ CONVERTS"
540 INPUT W1
965 T=1
1000 MAT M=ZER
1025 MAT D=ZER
1075 U4,K1,V4=0
1080 V6=0
1100 K4=0
1125 MAT S=ZER
1150 B1=B6
1225 E=E1
1250 A=A1
1275 L9=L1
2650 IF K$="YES" THEN 2665
2651 E=E*(B6+N3)
2652 X5=X5*B6
2653 A=B6+N3-E-B5-W1
2655 K7=A
2660 E=E+F
2661 GO TO 2675
2665 E=E*(B6+N3+F)
2666 A=B6+N3-E-B5+F-W1
2667 K7=A
2668 X5=X5*(B6+F)
2675 IF A$="NO" THEN 2675
2677 L8=L9/100
2678 GO TO 2701
2695 L7=L9/Q6
2700 L8=L7/(1-(1/((1+L7)^Q)))
2701 IF K$="NO" THEN 2710
2702 L=L8*(B6+N3+F)
2705 GO TO 2726
2710 L=L8*(B6+N3)
2726 IF MID$(Z$,1,4)<>"READ" THEN 2749
2727 OPEN 3,D$,INPUT
2729 FOR I=1 TO Y1*Q6
2730 GET 3:M(I,2)
2731 M(I,2)=M(I,2)*B6
2732 NEXT I
2733 CLOSE 3
2734 GO TO 3681
2749 FOR N=1 TO Y1
2750 IF V4>0 THEN 2800
2775 A8=B1
2800 GOTO 2825
2825 IF M9=150 THEN 2900
2850 D1=B1*(2/Y1)
2875 GOTO 2925
2900 D1=B1*(1.5/Y1)
2925 GOTO 2950
2950 IF N>1 THEN 3050
2975 FOR K=N-1 TO Y1
3000 K1=K1+(Y1-K)
3025 NEXT K
3050 IF N=1 THEN 3125
3075 IF V4?1 THEN 3125
3100 K1=K1-((Y1-N)+2)
3125 D2=A8*(((Y1-N)+1)/K1)
3150 D3=B1*(1/((Y1+1)-N))
3175 IF M9=200 THEN 3225
3200 D2=0
3225 IF D1?D2 THEN 3350
3250 D=D2
3275 D(N,3)=2
3300 V4=V4+1
3325 GOTO 3400
3350 D=D1
3375 D(N,3)=1
3400 IF D>D3 THEN 3500
3425 D=D3
3450 D(N,3)=3
3475 GOTO 3500
3500 D(N,2)=D
3502 B1=B1-D
3550 NEXT N
3560 IF Q6=2 THEN 3685
3575 FOR X=1 TO Y1
3600 FOR Y=(4*X)-3 TO 4*X
3625 M(Y,2)=D(X,2)/4
3650 NEXT Y
3675 NEXT X
3680 IF MID$(Z$,4,1)="R" THEN 3724
3681 IF MID$(Z$,5,1)="R" THEN 3724
3682 GO TO 3800
3685 FOR X=1 TO Y1
3690 FOR Y=(2*X)-1 TO 2*X
3700 M(Y,2)=D(X,2)/2
3710 NEXT Y
3720 NEXT X
3721 IF MID$(Z$,4,1)="R" THEN 3724
3722 IF MID$(Z$,5,1)="R" THEN 3724
3723 GO TO 3800
3724 P5=INT(Y1+.91)*Q6
3725 FOR X=1 TO P5
3726 Y=P5-X+1
3727 D(Y,2)=M(X,2)
3728 NEXT X
3729 FOR X=1 TO P5
3730 M(X,2)=D(X,2)
3731 NEXT X
3800 FILE #2,"ATCF"
3801 FILE #1,"TERM2"
3805 SCRATCH #1,2
3825 WRITE #2,Q+1,(E-X5)*(-1)
3850 IF N8=0 THEN 3975
3855 IF K4=1 THEN 3975
3860 FOR N=1 TO N8
3865 M(N,4)=K7*(I1/Q6)
3866 M(N,9)=M(N,4)
3867 U(N)=K7
3868 PUT 1:K7
3870 NEXT N
3900 K4=1
3975 FOR N=1 TO Q
4000 IF N>1 THEN 4100
4025 I2=I1/Q6
4050 C1=I2/(1-(1/((1+I2)^N2)))
4051 IF A$="NO" THEN 4075
4052 IF N$="NO" THEN 4075
4055 C1=I2/(1-(1/((1+I2)^(N2-1))))
4075 C=A*C1
4100 I=I2*A
4101 IF(N+N8)<Q THEN 4125
4102 IF A$="NO" THEN 4125
4103 IF N$="NO" THEN 4125
4105 C=0
4106 I=0
4125 P=C-I
4150 M(N+N8,4)=I
4175 A=A-P
4180 IF (N+N8)>Q THEN 4225
4182 U(N)=A
4183 PUT 1:A
4225 M(N,1)=L
4230 IF N<Q THEN 4250
4232 IF A$="NO" THEN 4235
4233 M(N,1)=0
4235 M(N,1)=M(N,1)+R7
4250 M(N,5)=0
4275 M(N,6)=F/Q
4300 M(N,7)=M(N,1)-M(N,2)-M(N,4)-M(N,5)-M(N,6)
4325 M(N,8)=M(N,7)*T5*(-1)
4350 M(N+N8,9)=C
4375 M(N,10)=M(N,5)
4400 M(N,11)=M(N,1)-M(N,9)-M(N,10)
4425 M(N,12)=M(N,8)+M(N,11)
4445 PUT 2:M(N,12)
4525 NEXT N
4526 IF A$="NO" THEN 4544
4527 PRINT "LEASE FACTOR BID";
4528 INPUT P6
4529 R4=P6/100
4530 IF K$="NO" THEN 4536
4532 C6=B7+F7+N4
4534 GO TO 4538
4536 C6=B7+N4
4538 S1=C6*R4
4540 S2=C6*L8
4542 GO TO 5000
4544 PRINT "LEASE RATE BID";
4546 INPUT P6
4548 R4=P6/Q6
4550 IF K$="NO" THEN 4556
4552 C6=B7+F7+N4
4554 GO TO 4560
4556 C6=B7+N4
4560 S1=C6*R4/(1-(1/((1+R4)^Q)))
4562 S2=C6*L7/(1-(1/((1+L7)^Q)))
5000 OPEN 6,INTCONV",INPUT
5100 PRINT "WARRANTS?";
5110 INPUT O$
5120 IF O$="NO" THEN 5200
5130 PRINT "SPREAD/YR";
5135 INPUT A1
5136 A1=A1/Q6
5140 OPEN 7,"TERMX1",INPUT
5150 FOR I=1 TO Q
5160 GET 7:L6
5170 U(I)=U(I)+L6
5180 NEXT I
5185 CLOSE 7
5190 IF O$="YES" THEN 7200
5200 A1=0
7200 FOR I=1 TO Q
7205 T(I)=0
7207 GET 6: C(I)
7210 NEXT I
7211 CLOSE 6
7215 FOR I=1 TO Q
7220 B(I,1)=M(I,1)
7225 B(I,2)=M(I,2)
7230 B(I,3)=M(I,4)
7235 B(I,4)=M(I,5)
7236 B(I,4)=0
7240 B(I,5)=M(I,6)
7245 B(I,6)=B(I,1)-(B(I,2)+B(I,3)+B(I,4)+B(I,5))
7250 B(I,7)=S1-S2
7251 IF I<Q THEN 7255
7252 IF A$="NO" THEN 7255
7253 B(I,7)=0
7255 B(I,8)=C(I)
7260 B(I,9)=A1*U(I)
7265 B(I,10)=0
7270 B(I,11)=B(I,7)-B(I,8)-B(I,9)-B(I,10)
7275 B(I,12)=B(I,6)+B(I,11)
7277 IF I>1 THEN 7285
7280 B(I,12)=B(I,12)-X5
7285 NEXT I
7290 FOR I=1 TO Q1
7295 Z(I)=0
7300 FOR J=(I-1)*Q6+1 TO I*Q6
7305 Z(I)=Z(I)+B(J,12)
7310  NEXT J
7315 NEXT I
7320 FOR I=1 TO Q1
7325 Q(I,1)=Z(I)*T5
7330 IF Q(I,1)>0 THEN 7365
7335  T(I)=Q(I,1)
7340 IF I>1 THEN 7355
7345 Q(I,2)=0
7350 GO TO 7435
7355 Q(I,2)=Q(I-1,2)+Q(I-1,1)
7360 GO TO 7435
7365 IF I>1 THEN 7380
7370 Q(I,2)=0
7375 GO TO 7385
7380 Q(I,2)=Q(I-1,2)+Q(I-1,1)
7385 IF I<6 THEN 7400
7390 Y6=I-5
7395 GO TO 7405
7400 Y6=1
7405 Q(I,3)=Q(I,1)
7410 FOR J=Y6 TO I
7415 IF T(J)=0 THEN 7450
7420 Q(I,3)=Q(I,3)+T(J)
7425 IF Q(I,3)>0 THEN 7445
7430 T(J)=Q(I,3)
7435 Q(I,3)=0
7440 GO TO 7460
7445 T(J)=0
7450 NEXT J
7455 Q(I,2)=0
7460 NEXT I
7465 FOR I=1 TO Q
7470 B(I,13)=0
7471 B(I,14)=0
7475 NEXT I
7480 FOR I=1 TO Q1
7485 B(I*Q6,13)=Q(I,2)
7490 B(I*Q6,14)=Q(I,3)
7491 NEXT I
7492 PRINT "TAX POSITION";
7493 INPUT O$
7494 IF O$="NO" THEN 7651
7495 Q7=Q6
7496 PRINT "YEARLY";
7497 INPUT O$
7498 IF O$="YES" THEN 7500
7499 Q7=1
7500 PRINT"                                       ****INTERET TAX POSITION****"
7501 PRINT"                         SUBSIDIARY                                             PARENT                          CONSOLIDATED"
7502 PRINT"     ------------------------------------------------------   -----------------------------------------  ";
7503 PRINT"----------------------------------------"
7504 PRINT
7505 PRINT"     NET                                                       NET     ";
7506 PRINT"INTEREST                             NET     TAX LOSS ACTUAL"
7509 PRINT"     RENTAL     DEPRE-      INTEREST  ON             TAXABLE    RENTAL     ON     ";
7510 PRINT"BOND    OTHER  TAXABLE    TAXABLE   CARRY-   TAXES"
7512 PRINT" PD  INCOME     CIATION  DEBT  & BALLOON  FEES      INCOME     INCOME  CON.DEBT  DISC.   EXP";
7515 PRINT"      INCOME      INCOME   FORWARD    PAID"
7518 PRINT" --  --------  --------  -------  ------  ------  ---------   --------  ------  ------ ";
7520 PRINT"------ ------- ------- ------- ------- -------"
7525 I=0
7526 B1=0
7530 FOR J=1 TO 14
7535 T(J)=0
7540 NEXT J
7545 FOR J=1 TO Q7
7550 I=I+1
7555 FOR J1=1 TO 12
7560 T(J1)=T(J1)+B(I,J1)
7565 NEXT J1
7567 NEXT J
7570 T(13)=B(I,13)
7580 T(14)=T(14)+B(I,14)
 7590 B1=B1+1
7595: ##  ########  ########  #######  ######  ######  #########   ########  ######  ######  ###### ########  ######### ##
7600 PRINT USING 7595,B1,T(1),T(2),T(3),T(4),T(5),T(6),T(7),T(8),T(9),T(10),T(11),T(12)
7601 PRINT USING 7596,T(13),T(14)
7596:######## ########
7605 IF I<Q THEN 7530
7610 FOR J=1 TO 14
7615 T(J)=0
7625 FOR I=1 TO Q
7630 T(J)=T(J)+B(I,J)
7635 NEXT I
7640 NEXT J
7645 PRINT USING 7650,T(1),T(2),T(3),T(4),T(5),T(6),T(7),T(8),T(9),T(10),T(11),T(12)
7646 PRINT USING 7641,T(14)
7651:         ##########
7650:     ########  ########  #######  ######  ######  #########   ########  ######  ######  ###### ########  ######### #
7651 FOR I=1 TO Q
7652 K(I)=0
7653 NEXT I
7654 PRINT "HOW MANY PAYMENTS FOR CD'S,WHEN"
7655 INPUT A1
7656 IF A1=0 THEN 7661
7657 FOR I=1 TO A1
7658 INPUT I1
7659 K(I1)=W1/A1
7660 NEXT I
7661 A8=B6
7664 FOR I=1 TO Q
7665 B(I,2)=M(I,9)
7670 B(I,3)=B(I,4)
7671 B(I,3)=0
7680 B(I,4)=B(I,1)-B(I,2)-B(I,3)
7685 B(I,5)=B(I,7)
7690 B(I,6)=B(I,8)
7695 B(I,7)=0
7700 B(I,8)=B(I,5)-B(I,6)-B(I,7)
7710 B(I,9)=B(I,4)+B(I,8)
7715 B(I,10)=B(I,14)
7720 B(I,11)=B(I,9)-B(I,10)-K(I)
7721 PUT 1:B(I,11)
7722 A8=A8-M(I,2)
7723 IF I<Y1*Q6 THEN 7725
7724 A8=0
7725 IF I>1 THEN 7740
7726 IF A$="NO" THEN 7730
7727 B(I,11)=B(I,11)+(S1-S2)+L
7730 B(I,12)=B(I,11)
7735 GO TO 7745
7740 B(I,12)=B(I-1,12)+B(I,11)
7745 B(I,13)=E*(-1)+B(I,12)
7746 D=B(I,13)
7747 IF D<0 THEN 7749
7748 D=0
7749 PUT 1:-D,A8
7750 NEXT I
7751 FOR I=1 TO 4
7752 PRINT
7753 NEXT I
7754 CLOSE 1
7755 PRINT "CASH FLOW";
7760 INPUT O$
7761 IF O$="NO" THEN 7920
7762 Q7=Q6
7763 PRINT "YEARLY";
7764 INPUT O$
7765 IF O$="YES" THEN 7770
7766 Q7=1
7767 FOR I=1 TO 4
7768 PRINT
7769 NEXT I
7770 PRINT "                                       ****INTERET CASH FLOW****"
7771 PRINT
7772 PRINT
7773 PRINT "               SUBSIDIARY                            PARENT                           CONSOLIDATED"
7774 PRINT "     --------------------------------   -------------------------------------  --------";
7775 PRINT"-------------------------------"
7776 PRINT"      NET      DEBT SERVICE    PRE      NET      INTEREST   OTHER    PRE         PRE                 ";
7777 PRINT "AFTER        CUM"
7778 PRINT"      RENTAL   ON AMORTIZED    TAX      RENTAL      ON      CASH     TAX";
7779 PRINT"        TAX        TAXES     TAX     AFTER TAX     NET"
7780 PRINT" PD   INCOME  DEBT & BALLOON   CASH     INCOME   CONV.DEBT  EXP      CASH";
7781 PRINT"        CASH      PAID      CASH       CASH   INVESTMENT"
7783 PRINT" --  -------- ------- ------  -------  --------  --------  ------  --------   ---------  -------  --------- ";
7784 PRINT"--------- --------- "
7785 PRINT
7800 I=0
7801 B1=0
7810 FOR L=1 TO 13
7815 T(L)=0
7820 NEXT L
7825 FOR J=1 TO Q7
7830 I=I+1
7835 FOR L=1 TO 11
7840 T(L)=T(L)+B(I,L)
7845 NEXT L
7850 NEXT J
7855 T(12)=B(I,12)
7860 T(13)=B(I,13)
7865 B1=B1+1
7870 PRINT USING 7875,B1,T(1),T(2),T(3),T(4),T(5),T(6),T(7),T(8),T(9),T(10),T(11)
7871 PRINT USING 7876,T(12),T(13)
7876 :######### #########
7875: ##  ######## ####### ######  #######  ########  ########  ######  ########   #########  #######  #########  #
7877 IF I<Q THEN 7810
7880 FOR J=1 TO 13
7890 T(J)=0
7895 FOR I=1 TO Q
7900 T(J)=T(J)+B(I,J)
7905 NEXT I
7910 NEXT J
7911:     ######## ####### ######  #######  ########  ########  ######  ########   #########  #######  #########  #
7915 PRINT USING 7911,T(1),T(2),T(3),T(4),T(5),T(6),T(7),T(8),T(9),T(10),T(11)
7916 PRINT USING 7912,T(12),T(13)
7912:######### #########
7916 FOR I=1 TO 4
7917 PRINT
7918 NEXT I
7920 PRINT "EARNINGS STMT";
7930 INPUT O$
7935 IF O$="NO" THEN 9999
7940 Q7=Q6
7945 PRINT "YEARLY";
7947 INPUT O$
7950 IF O$="YES" THEN 7979
7955 Q7=1
7979 C1=0
7980 FOR I=1 TO Q
7982 C1=C1+B(I,1)
7985 NEXT I
7990 PRINT "% RESIDUAL FOR BOOK(I.E..15)";
7995 INPUT R6
7997 C2=(S1-S2)*Q+(C1-(1-R6)*B6)
8000 FOR I=1 TO Q1
8005 D8=Q1-I+1
8010 FOR J=(I-1)*Q6+1 TO Q6*I
8020 B(J,1)=D8*C2/(Q1*(Q1+1)/2)/Q6
8030 NEXT J
8040 NEXT I
8050 FOR I=1 TO Q1
8060 D8=Q1-I+1
8070 FOR J=(I-1)*Q6+1 TO Q6*I
8080 B(J,9)=D8*X5/(Q1*(Q1+1)/2)/Q6
8090 NEXT J
8100 NEXT I
8105 FOR I=1 TO Q
8110 B(I,2)=M(I,4)
8115 B(I,3)=M(I,5)
8116 B(I,3)=0
8120 B(I,4)=C(I)
8121 B(I,4)=0
8125 B(I,5)=0
8130 B(I,6)=M(I,6)
8135 B(I,7)=B(I,1)-(B(I,2)+B(I,3)+B(I,4)+B(I,5)+B(I,6))
8140 B(I,8)=(1-T5)*B(I,7)
8150 B(I,10)=B(I,8)+B(I,9)
8160 NEXT I
8161 FOR I=1 TO 4
8162 PRINT
8163 NEXT I
8170 PRINT"                           ****INTERET EARNINGS STATEMENT****"
8171 PRINT
8175 PRINT"                INTEREST EXPENSE   INTEREST                   NET INCOME   INCOME                      NET"
8180 PRINT"      EARNED     ON AMORTIZED      EXP ON     BOND              BEFORE     AFTER                    AFTER TAX"
8185 PRINT" PD   INCOME    DEBT & BALLOON     CONV.DEBT   DISC      FEES     TAX         TAX        I.T.C.     INCOME"
8190 PRINT" --  ---------  --------  -------  --------  -------  ------   ---------   ---------   ---------   ---------"
8195 I=0
8196 B1=0
8200 FOR L=1 TO 10
8205 T(L)=0
8210 NEXT L
8215 FOR J=1 TO Q7
8220 I=I+1
8225 FOR L=1 TO 10
8230 T(L)=T(L)+B(I,L)
8235 NEXT L
8240 NEXT J
8245 B1=B1+1
8250 PRINT USING 8255,B1,T(1),T(2),T(3),T(4),T(5),T(6),T(7),T(8),T(9),T(10)
8255: ##  #########  ########  #######  ########  #######  #######  #########   #########   ########   #########
8260 IF I<Q THEN 8200
8261 FOR J=1 TO 10
8265 T(J)=0
8270 FOR I=1 TO Q
8275 T(J)=T(J)+B(I,J)
8280 NEXT I
8285 NEXT J
8290 PRINT
8295 PRINT USING 8300,T(1),T(2),T(3),T(4),T(5),T(6),T(7),T(8),T(9),T(10)
8300:     #########  ########  #######  ########  #######  #######  #########   #########  ########   #########
9999 END
225 Y$="YES"
1075 U4=0
1076 K1=0
1077 V4=0
2730 READ #3,M(I,2)
3075 IF V4>=1 THEN 3125
3225 IF D1>=D2 THEN 3350
2727 FILE #3,D$
7211 RESTORE #6
7207 READ #6,C(I)
5185 RESTORE #7
5160 READ #7,L6
4445 WRITE #2,M(N,12)
4183 WRITE #1,A
3868 WRITE #1,K7
514  
2733 RESTORE #3
5000 FILE #6,"INTC"
5140 FILE #7,"TERM"
7494 IF O$="NO" THEN 7651
7721 WRITE #1,B(I,11)
7749 WRITE #1,-D,A8
7754 RESTORE #1
2728 SCRATCH #3
5001 SCRATCH #6
5141 SCRATCH #7
7641:        ########
4280 IFN<Q THEN 4300
4285 IF A$="NO" THEN 4300
4290 M(N,7)=L-M(N,2)-M(N,4)-M(N,5)-M(N,6)
4295 GOTO 4325
7241 IF I<Q THEN 7245
7242 IFA$="NO" THEN 7245
7243 B(I,6)=L-(B(I,2)+B(I,3)+B(I,4)+B(I,5))
7244 GOTO 7250
7266 IF I<Q THEN 7270
7267 IF A$="NO"THEN 7270
7268 B(I,11)=(S1-S2)-B(I,8)-B(I,9)-B(I,10)
7269 GOTO 7275
7980 C1=M(1,1)*Q
7982
7985
3800  
3801  
5140  
2730  
3825
3825  
4445  
4183  
3868  
7721  
7749  
3805
2728
5001
5141
   