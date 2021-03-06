90 E$=" "
100 PRINT"ASSET COST,DEP LIFE,RESIDUAL";
110 INPUT A,B,Z1
120 PRINT "FIT RATE,DOWN PAYMENT,EARNINGS RATE";
130 INPUT C,D,G2
140 PRINT"LOAN AMT,TERM,RATE,PERS/YR";
150 INPUT E,F,G,H
160 PRINT"LEASE LENGTH, QTRLY RENTAL";
170 INPUT I,J2
180 PRINT "ITC,OPPORTUNITY RATE";
190 INPUT V7,V1
195 PRINT "FLOWS(Y OR N)?";
197 INPUT F$
199 IF F$="N" GOTO 360
210 PRINT
220 PRINT"                                        LEASE VS PURCHASE ANALYSIS"
230 PRINT"                                    AFTER TAX COST OF $1000000 ASSET"
240 PRINT
250 PRINT
260 PRINT USING 270,E$
270 :                    LEASE                                   PURCHASE '
280 PRINT
285 PRINT USING 287,E$
287:    1      2         3          4                5           6         7           8        9          10            11 '
290 PRINT
300 PRINT USING 310,E$
310 : YEAR   ANNUAL     A/T       P.V.@           PAYMENTS    ACC DEP    INT @        A/T      P.V.@     EARNINGS     COMBINED '
320 PRINT USING 330 , V1*100,G,V1*100
330 :        RENTAL    COST     ###.##%           EQUITY &   (DDB-SYD) ###.##%       COST    ###.##%     SACRIFICED   P.V. @
340 PRINT USING 350 , G2*100,V1*100
350 :                                             DEBT SVC                                               @###.##%    ###.##%
360 REM LEASE SECTION
370 J1=J2*(1-C)
380 REM LOAN SECTION
390 K=F*H
400 E2=(E*G/H)/(1-(1+(G/H)/100)^(-K))+.5
410 E1 = INT (E2+.5)/100
420 REM DEP SECTION
430 DIM A(25)
440 DIM B(50)
450 DIM C(30)
460 DIM D(50)
470 DIM H(60)
480 DIM G(100)
490 DIM E(50)
500 DIM P(50)
510 DIM L(50)
520 DIM M(50)
530 DIM N(80)
540 DIM R(100)
545 DIM T(100)
550 DIM S(50)
551 REM DDB/SYD DEP
552 B1=0
554 B1=2*(1/B)
556 D(1)=A*B1
558 B2=A-D(1)
560 D(2)=B2*B1
562 B3=B2-D(2)
564 FOR P=1 TO B-2
570 H3=H3+P
580 NEXT P
590 B(B+1)=B3
600 FOR Y=B-1 TO 1 STEP -1
610 D3=D3+1
620 I3=Y-1
630 K1=(I3/H3)*B3
640 B(Y-1)=B(Y)-K1
650 D(D3+2)=K1
660 B(Y)=B(Y-1)
670 IF Y=2 GOTO 710
680 N(D3)=B(Y)
690 D(I+1)=N(D3)
700 NEXT Y
710 REM LOAN INT
720 B1=E
730 FOR J=1 TO I*H
740 G(J)=INT((B1*G)/(H*100)+.5)
750 P1=E1-G(J)
760 B1=B1-P1
770 X=X+1
780 G1=G(J)+G1
790 IF X<>H GO TO 830
800 V=V+1
810 H(V)=G1
820 X=G1=0
830 NEXT J
840 REM A/T COST
850 FOR Y=1 TO I
860 L(Y)=E1*H-(C*(D(Y)+H(Y)))
870 L(I+1)=-Z1-(C*(-Z1+N(I)))
880 M(Y)=J1*4-L(Y)
890 M(Y+1)=L(Y+1)
900 T1=T1+(J2*4)
910 T2= T2+J1*4
920 T3=T3+E1*H
930 T4=T4+D(Y)
940 T5=T5+H(Y)
950 T6=T6+L(Y)
960 T7=T7+M(Y)
970 NEXT Y
975 L(1)=L(1)-V7
977 T6=T6-V7
980 K=0
990 REM WC EARNINGS
1000 B2=D
1010 FOR J = 1 TO I*H
1020 R(J)=B2*(1+G2/H)-B2
1030 B2=B2+R(J)
1035 R1=R1+R(J)
1040 X = X+1
1060 IF X <>4 GO TO 1110
1070 K=K+1
1080 S(K)=R1
1090 X=R1=0
1100 R2=S(K)+R2
1110 NEXT J
1120 PRINT
1130 REM PV ROUTINE
1140 O=P=0
1150 Z=L(I+1)
1160 DIM O(25)
1170 FOR Y=1 TO I
1180 A(I)=J1*4
1190 O(Y)=O(Y)+A(I)/((1+V1)^Y)
1200 O=O+O(Y)
1210 NEXT Y
1220 FOR Y=1 TO I+1
1230 C(Y)=L(Y)+S(Y)
1240 C(I+1)=Z
1245 T(Y)=T(Y)+L(Y)/((1+V1)^Y)
1250 P(Y)=P(Y)+C(Y)/((1+V1)^Y)
1255 T=T+T(Y)
1260 P=P+P(Y)
1270 NEXT Y
1275 IF F$="N" GOTO 1410
1280 FOR Y = 0 TO I+1
1290 IF Y = 0 GOTO 1360
1300 IF Y=I+1 GOTO 1330
1310 PRINT USING 1340 , Y,J2*4,J1*4,O(Y),E1*H,D(Y),H(Y),L(Y),T(Y),S(Y),P(Y)
1320 GOTO 1340
1330 PRINT USING 1340, Y,X7,X7,X7,-Z1,N(Y-1),X7,L(Y),T(Y),X7,P(Y)
1340: ###   #######   ######    #######            #######    #######   ######     #######   #######      #######    #######
1350 GO TO 1370
1360 PRINT USING 1340, Y,X7,X7,X7,D,X7,X7,D,D,X7,D
1370 NEXT Y
1380 PRINT USING 1390 , T1,T2,O,T3-Z1+D,T4+N(I),T5,T6+D+L(Y),T+D,R2,P+D
1390:TOTALS########  #######   ########           ########   ########  #######    ########  ########     ########   ########
1400 PRINT
1410 PRINT
1420 PRINT USING 1430, T+D
1430 :  THE COST OF PURCHASING IN PRESENT VALUE TERMS ########
1440 PRINT USING 1460, O
1450 PRINT USING 1470, (T+D)-O
1460 :  THE COST OF LEASING IN PRESENT VALUE TERMS    ########
1470 :  THE DIRECT AMOUNT SAVED BY LEASING IS:        ########
1475 T9=(P+D)-(T+D)
1480 PRINT USING 1490, T9
1490 :  THE P.V. OF CONSERVED EQUITY (COL 11-9)       ########
1500 PRINT USING 1510, T9+((T+D)-O)
1510 :  THE TOTAL AMOUNT SAVED BY LEASING IS:         ########
2000 END
   