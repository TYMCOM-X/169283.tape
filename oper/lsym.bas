100 N$=" "
120 PRINT "DATAFILE";
140 INPUT K$
160 FILE #1,K$
180 REM COMPANY NAME
200 READ #1,L$
220 REM FINAL YR,QTR
240 READ #1,Y2,Q2
250 Q1=4*(Y2-1)+Q2
260 DIM A(160,3)
280 DIM B(100,20)
340 DIM K(2,25)
360 DIM H(2,55)
380 DIM T(200)
400 DIM M(200,10)
420 DIM N (200,7)
440 MAT M=ZER
460 MAT P= ZER
480 MAT T = ZER
500 MAT N= ZER
520 GOTO 7020
530 D$="NO"
540 REM "ITC",YR,QTR
560 READ #1, I5,Y3,Q3
570 C3=4*(Y3-1)+Q3
575 C4=Y3
580 REM "TAX RATE";
600 READ #1, Z1
620 REM NO. LOANS
640 READ #1,N0,I$
660 FOR N9=1 TO N0
680 REM DEBT TYPE: LEVEL,MORATORIUM,DATA INPUT
700 READ #1, A$
720 N=4
740 IF LEFT$(A$,1)="L" THEN 1340
760 IF LEFT$(A$,1)="M" THEN 840
780 IF LEFT$(A$,1)="D" THEN 1160
790 PRINT "DEBT TYPE SPECIFIED IS ";A$
800 PRINT"ERROR IN DEBT TYPE IN DATA FILE"
820 STOP
840 REM MORATORIUM CALCULATION
860 REM INPUT=PRINCIPLE,INT RATE,STARTING YR,QTR;TOTAL YRS,YRS INT ONLY
880 READ#1,B1,R,Y2,Q2,Y4,Y3
900 Q3=Q4=Q2
920 Y3=Y3+Y2
940 Y4=Y4+Y2
960 B2=4*(Y2-1)+Q2
980 B3=4*(Y3-1)+Q3
1000 I6=B1*R/400
1020 B4=4*(Y4-1)+Q4-1
1040 E=B4-B3+1
1060 FOR Y=B2 TO B3-1
1080 M(Y,3)=I6+M(Y,3)
1100 NEXT Y
1120 I7=B3
1140 GOTO 1460
1160 REM  "DATA INPUT FOR INTEREST: INTEREST,STARTING YR,QTR;ENDING YR,QTRFOR EACH AMT (ALL ZEROES TO END)"
1180 READ #1,I6,Y2,Q2,Y3,Q3
1190 IF I6=0 THEN 1660
1200 B2=4*(Y2-1)+Q2
1220 B3=4*(Y3-1)+Q3
1240 FOR Y=B2 TO B3
1260 M(Y,3)=I6+M(Y,3)
1280 NEXT Y
1320 GOTO 1180
1340 REM "AMT,RATE,STARTING YR,QTR;NO YRS
1360 READ #1, B1,R,Y2,Q2,S
1380 B3=4*(Y2-1)+Q2
1400 B4=4*S+B3-1
1420 E=B4-B3+1
1440 I7=B3
1460 P=(B1*R/N)/(1-(1+(R/N)/100)^(-E))+.5
1480 P=INT(P)/100
1500 B6=P
1520 R=R*100
1540 FOR Y=I7 TO E+I7-1
1560 I1=INT((B1*R)/(N*100)+.5)/100
1580 P1=P-I1
1600 B1=B1-P1
1620 M(Y,3)=I1+M(Y,3)
1640 NEXT Y
1660 NEXT N9
1662 REM D/S
1663 READ #1,I$
1664 READ #1,D1,Y2,Q2,Y3,Q3
1666 D2=4*(Y2-1)+Q2
1668 D3=4*(Y3-1)+Q3
1670 FOR Y=D2 TO D3
1672 M(Y,4)=D1
1674 NEXT Y
1676 IF D3>=Q1 GOTO 1880
1678 GOTO 1664
1880 B1=0
1900 REM "RENT STARTING YR,QTR;ENDING YR,QTR
1910 READ #1,I$
1920 READ #1, B1,Y2,Q2,Y3,Q3
1925 B2=4*(Y2-1)+Q2
1930 B3=4*(Y3-1)+Q3
1940 FOR Y=B2 TO B3
1960 M(Y,1)=B1
2000 NEXT Y
2020 IF B3>=Q1 GOTO 2260
2060 GOTO 1920
2260 REM "EXP";
2270 READ #1,I$
2280 READ #1, E,Y2,Q2,Y3,Q3
2285 E2=4*(Y2-1)+Q2
2290 E3=4*(Y3-1)+Q3
2300 FOR Y=E2 TO E3
2320 M(Y,5)=E
2340 NEXT Y
2350 IF E3>=Q1 GOTO 2380
2360 GOTO 2280
2380 REM "ANNUAL SF RATE (%)
2400 READ #1, R3
2410 R3=R3/100
2420 REM "FEE,FRONT(F) OR AMORTZD(A)";
2440 READ #1, H6,F$
2460 IF F$="F"  GOTO 2520
2480 H7=H6/Q1
2500 H6=0
2520 REM"EQUITY
2540 READ #1, N(1,6)
2560 REM"NET RESID";
2580 READ #1, K1
2600 PRINT "PRINT FLOWS";
2620 INPUT W$
2625 IF W$="NO" GOTO 2660
2630 PRINT "ANNUAL OR QUARTERLY FLOWS";
2640 INPUT M$
2645 M$=LEFT$(M$,1)
2660 PRINT "CF FILENAME";
2680 INPUT F$
2700 FILE #3,F$
2720 SCRATCH #3
3022 FOR I=1TO4
3024 PRINT
3026 NEXT I
3028 PRINT SPACE$(57-LEN(L$)/2);L$
3030 PRINT SPACE$(50);"LEASE ANALYSIS"
3032 FOR I=1TO3
3034 PRINT
3036 NEXT I
3040 PRINT USING 3060,N$
3060 :  PRD    RENTAL  DEPRECIA-   INTEREST  INTEREST &   OTHER    INTERNAL   TAX LOSS   TAX SAVING AFTER TAX  CUMULATIVE '
3080 PRINT USING 3100,N$
3100 :         INCOME  TION PLUS   ON AMORT  PRINCIPAL   EXPENSES  CASH FLOW  OR GAIN    OR PAYMENT CASH FLOW  AFTER TAX '
3120 PRINT USING 3140, Z1*100
3140 :                 FT END FEES  DEBT     ON DEBT               BEFORE TAX            @###.#%               CASH FLOW
3160 PRINT
3180 PRINT
3190 IF W$="NO" THEN 3220
3200 V1=V2=V3=V4=V5=V6=V7=V8=V9=0
3220 L1=L2=L3=L4=L5=L6=L7=L8=L9=0
3280 FOR Y=1 TO Q1
3300 M(Q1+1,1)=K1
3320 L9=L9+1
3340 M(Y,2)=M(Y,2)+H7
3360 M(1,2)=M(1,2)+H6
3380 M(Q1+1,2)=X0
3400 M(Y,6)=M(Y,1)-M(Y,4)-M(Y,5)
3420 T7=M(Y,6)
3440 M(Q1+1,6)=K1
3460 M(Y,7)=M(Y,1)-M(Y,2)-M(Y,3)-M(Y,5)-(T7-M(Y,6))
3480 M(Q1+1,7)=K1-X0
3500 M(Y,8)=-M(Y,7)*Z1
3520 M(Y,9)=M(Y,8)+M(Y,6)
3540 M(C3,9)=M(C3,9)+I5
3640 M(Q1+1,9)=K1-(K1-X0)*Z1
3650 L1=L1+M(Y,1)
3660 L2=L2+M(Y,2)
3680 V1=V1+M(Y,1)
3700 V2=V2+M(Y,2)
3720 V3=V3+M(Y,3)
3740 V4=V4+M(Y,4)
3760 L3=L3+M(Y,3)
3765 L4=L4+M(Y,4)
3770 L5=L5+M(Y,5)
3780 V5=V5+M(Y,5)
3800 V6=V6+M(Y,6)
3820 V7=V7+M(Y,7)
3840 V8=V8+M(Y,8)
3860 L6=L6+M(Y,6)
3880 V9=V9+M(Y,9)
3900 L7=L7+M(Y,7)
3920 L8=L8+M(Y,8)
3940 L0=L0+M(Y,9)
3960 T5=T5+M(Y,9)
3980 M(Y,10)=M(Y,2)+M(Y,3)+M(Y,5)
4000 IF W$="NO" THEN 4240
4020 IF M$="Q" THEN 4220
4040 A5=4
4045 IF Y=Q1+1 THEN 4180
4050 IF Y=Q1 THEN 4140
4060 IF L9<>4 THEN 4240
4140 PRINT USING 4160,(Y+3)/4,L1,L2,L3,L4,L5,L6,L7,L8,L0,T5
4160:#### ######### ######### ######### ########## ########## ########## ########## ########## ########## ########### ###########
4170:'LLL ######### ######### ######### ########## ########## ########## ########## ########## ########## ########### ###########
4180 L1=L2=L3=L4=L5=L6=L7=L8=L9=L0=0
4200 GOTO 4240
4220 PRINT USING 4160,Y,M(Y,1),M(Y,2),M(Y,3),M(Y,4),M(Y,5),M(Y,6),M(Y,7),M(Y,8),M(Y,9),T5
4240 T(Y)=M(Y,9)
4260 WRITE #3, T(Y)
4280 NEXT Y
4300 K2=(K1-X0)*Z1
4320 K3=K1-((K1-X0)*Z1)
4325 T(Q1+1)=K3
4330 WRITE #3, T(Q1+1)
4340 PRINT
4350 PRINT
4360 PRINT USING 4170, "SALE",K1,X0,0,0,0,K1,K1-X0,-K2,K3,T5+K3
4380 PRINT
4400 PRINT USING 4420,V1+K1,V2+X0,V3,V4,V5,V6+K1,V7+K1-X0,V8-K2,T5+K3,T5+K3
4420: TOT ######### ######### ######### ########## ########## ########## ########## ########## ########## ########### ###########
4440 PRINT
4460 B1=Q1+1
4480 FOR Y=2 TO B1+1
4500 N(Y,1)=T(Y-1)
4520 NEXT Y
4530 C=N(1,6)
4540 REM SINKING FUND ROUTINE
4560 MAT A = ZER
4580 FOR Y= 1 TO Q1
4600 A(Y,1)=T(Y)
4620 NEXT Y
4622 A(B1,1)=K3
4625 IF T(Q1+1)>0 THEN 4640
4630 A(Q1,1)=A(Q1,1)+T(Q1+1)
4640 DIM E(110)
4660 IF D$="YES" THEN 4700
4680 GOTO 4800
4700 PRINT "SF RATE,INVESTMENT,RESIDUAL";
4720 INPUT R,C,Z2
4725 P=4
4730 R=R/100
4740 Z3=Z2-((Z2-X0)*Z1)
4760 A(B1,1)=Z3
4770 T(Q1+1)=Z3
4780 GOTO 4860
4800 R=R3
4820 P=4
4860 N1=Y1=N2=N3=T=N5=0
4880 R5=R
4900 REM CALC OF SINKING FUND
4920 N2=0
4940 FOR Y=Q1 TO 1 STEP-1
4960 IF A(Y,1)+N2<0 THEN  5250
4980 A(Y,3)=A(Y,1)+N2   'RETURNS TO LESSOR
5000 N(Y+1,4)=-N2
5020 A(Y,2)=-N2
5200 N2=0
5220 GOTO 5280
5250 A(Y,2)=-N2
5255 N(Y+1,4)=A(Y,1)
5260 N2=(A(Y,1)+N2)/(1+R/P)   'PREV PERIOD S.F. BAL
5280 NEXT Y
5300 IF T(Q1+1)<0 THEN 5440
5320 A(Q1,3)=T(Q1+1)+A(Q1,3)
5440 C1=P
5480 N(1,6)=-C
5540 REM THIS SUBROUTINE DOES THE LEASE RATE OF RETURN CALCULATION
5560 R=Z=0
5580 J=0
5600 Z=0
5620 FOR J=1 TO 6
5640 Z=.1^J
5660 R =R+Z
5680 Y=0
5700 FOR I=1 TO B1
5720 Y=Y+A(I,3)/((1+R/C1)^I)
5740 NEXT I
5760 IF Y-C>0 THEN 5660
5780 R=R-Z
5800 NEXT J
5820 PRINT "AT AFTER TAX RESIDUAL OF $";A(B1,1)
5840 PRINT "THE IRR IS ";R*100;"% USING A ";100*R5;"% SF RATE"
5860 PRINT "THE EQUIVALENT PRE-TAX RETURN IS ";R*100/(1-Z1);"%"
5880 PRINT "AGAIN ";
5900 INPUT D$
5920 IF D$= "YES" GOTO 4540
5940 :    ####     ########.##  ########.##  ########.##  ########.##  #######.##  ########.##  ########.##
5960 :    'LLL     ########.##  ########.##  ########.##  ########.##  #######.##  ########.##  ########.##
5980 N(B1+1,1)=A(B1,1)
6000 FOR Y=2 TO B1
6020 N(Y,2)=-N(Y-1,6)*(R/P)
6040 N(Y,3)=N(Y,1)-N(Y,2)-N(Y,4)
6060 N(Y,6)=N(Y-1,6)+N(Y,3)
6080 N(Y,5)=A(Y-2,2)*(R5/P)
6100 N(Y,7)=A(Y-1,2)
6340 NEXT Y
6360 PRINT "PRINT RECONCILIATION";
6380 INPUT R$
6400 IF R$="NO" THEN 6980
6420 PRINT"QTRLY OR ANNUAL RECONCILIATION";
6440 INPUT U$
6450 U$=LEFT$(U$,1)
6455  PRINT
6460 R6=R*100
6465 PRINT USING 6510," "
6470 PRINT
6475 PRINT USING 6515," "
6478 PRINT
6480 PRINT USING 6520," "
6500 R7=R5*100
6510 :                             -------CASH FLOW FROM LEASE------  'L
6515 :                             --RETURN TO INVESTOR--  -CONTRIBUTION TO S.F.-  'L
6520 :    PERIOD       CASH        EARNINGS     PRINCIPAL  CASH INTO  EARN. ON SF   INVESTMENT   SINK. FUND  'L
6540 PRINT USING 6560,R6,R7
6560 :              AVAILABLE      AT###.###%   REPAYMENT  SINK. FUND  AT###.###%   REMAINING     BALANCE
6580 PRINT
6600 PRINT
6620 FOR Y=1 TO B1
6640 IF Y=1 GOTO 6800
6660 U=U+1
6680 U1=U1+N(Y,1)
6700 U2=U2+N(Y,2)
6720 U3=U3+N(Y,3)
6740 U4=U4+N(Y,4)
6760 U5=U5+N(Y,5)
6765 U6=N(Y,6)
6770 U7=N(Y,7)
6780 IF U$="A" GOTO 6860
6800 PRINT USING 5940,F1,N(Y,1),N(Y,2),N(Y,3),N(Y,4),N(Y,5),-N(Y,6),N(Y,7)
6820 F1=F1+1
6840 GOTO 6960
6860 IF Y=B1 GOTO 6900
6880 IF U<>4 GOTO 6960
6900 F2=F2+1
6920 PRINT USING 5940,F2,U1,U2,U3,U4,U5,-U6,U7
6940 U=U1=U2=U3=U4=U5=0
6960 NEXT Y
6970 PRINT
6975 PRINT USING 5960,"SALE",N(B1+1,1),N(B1+1,2),N(B1+1,3),N(B1+1,4),N(B1+1,5),-N(B1+1,6),N(B1+1,7)
6980 GOTO 9030
7000 REM DEP ROUTINE+-DDB;2 YRS    SYD REMAINDER
7010 REM NO ITEMS
7020 READ #1,N0,I$
7025 FOR I9=1 TO N0
7030 REM "DEPR AMT, YRS,STARTING YR,QTR
7040 READ #1, X1,X2,Y3,Q3
7050 B3=4*(Y3-1)+Q3
7060 REM "SALVAGE VALUE,$,DEP METHOD(1=ADR,2=GL,3=SL,4=150%,5=SYD,6=DATA)";
7080 READ #1, X3,X9
7090 X0=X0+X3
7100 ON X9, GOTO 7120,7560,7960,8120,8440,8760
7120 E=0
7140 P1=2*(1/X2)
7160 H(1,1)=X1
7180 FOR Y=1 TO 2
7200 H(2,Y)=H(1,Y)*P1
7220 H(1,Y+1)=H(1,Y)-H(2,Y)
7240 NEXT Y
7260 Q=X2-2
7280 FOR Y=1 TO Q
7300 H=H+Y
7320 NEXT Y
7340 FOR I=Q+1 TO 1 STEP -1
7360 E=E+1
7380 I1=I-1
7400 K=(I1/H)*H(1,3)
7420 K(1,1)=H(1,3)
7440 IF K(1,E)-K<= X3 THEN 7520
7460 K(1,E+1)=K(1,E)-K
7480 H(2,E+2)=K
7500 NEXT I
7520 H(2,E+2)=K(1,E)-X3
7540 GOTO 8860
7560 REM GUIDELINE
7580 E=H=0
7600 P1=2*(1/X2)
7620 H(1,1)=X1
7640 FOR Y = 1 TO 3
7660 H(2,Y)=H(1,Y)*P1
7680 H(1,Y+1)=H(1,Y)-H(2,Y)
7700 NEXT Y
7720 Q=X2-3
7740 FOR Y = 1 TO Q
7760 H = H+Y
7780 NEXT Y
7800 H(1,4)=H(1,4)-X3
7820 FOR I = Q+1 TO 1 STEP -1
7840 E=E+1
7860 I1=I-1
7880 K=(I1/H)*H(1,4)
7900 H(2,E+3)=K
7920 NEXT I
7940 GOTO 8860
7960 REM S/L DEP
7980 E=0
8000 Q=X2
8020 FOR Y= 1 TO Q
8040 H(2,Y)=(X1-X3)/X2
8060 NEXT Y
8080 H(2,Q+1)=X3
8100 GOTO 8860
8120 REM 150% DB,SWITCH YEAR S/B X4
8140 P1=1.5*(1/X2)
8160 H(1,1)=X1
8180 REM "SWITCH YEAR";
8200 READ #1,X4
8220 FOR Y=1 TO X4
8240 H(2,Y)=H(1,Y)*P1
8260 H(1,Y+1)=H(1,Y)-H(2,Y)
8280 NEXT Y
8300 X5=K=0
8320 X5=X2-X4
8340 K=H(1,Y+1)-X3
8360 FOR Y=X4+1 TO X2
8380 H(2,Y)=K/X5
8400 NEXT Y
8420 GOTO 8860
8440 REM SYD
8450 H=0
8460 H(1,1)=X1
8480 FOR Y=1 TO X2
8500 H=H+Y
8520 NEXT Y
8540 FOR I=X2+1 TO 1 STEP -1
8560 E=E+1
8580 I1=I-1
8600 K=(I1/H)*H(1,1)
8620 K(1,1)=H(1,1)
8640 IF K(1,E)-K<=X3 THEN 8720
8660 K(1,E+1)=K(1,E)-K
8680 H(2,E)=K
8700 NEXT I
8720 H(2,E)=K(1,E)-X3
8740 GO TO 8860
8760 REM DATA
8800 FOR Y = 1 TO X2
8820 READ #1,H2
8830 H(2,Y)=H2
8840 NEXT Y
8860 C=B3-1
8880 FOR X=1 TO X2
8900 H(2,X)=H(2,X)/4
8920 FOR Y=1 TO 4
8940 C=C+1
8950 M(C,2)=H(2,X)+M(C,2)
8960 S4=M(C,2)+S4
8970 NEXT Y
8980 NEXT X
8985 MAT H=ZER
8990 NEXT I9
9000 GOTO 530
9020 REM EARNINGS
9030 PRINT
9031 PRINT
9040 PRINT "UNEARNED INC";
9060 INPUT E(1)
9070 IF E(1)=0 THEN 10360
9080 PRINT "ANNUAL CF EARNGS RATE";
9100 INPUT E9
9103 E9=E9/4
9105 IF C3<5 GOTO 9120
9110 E$="1"
9115 GOTO 9240
9120 PRINT "ITC;1=PASS THRU, 2=AMORTIZED";
9140 INPUT E$
9180 IF E$="1" GOTO 9240
9200 E(6) = I5
9240 FOR P= 1 TO 6 STEP 5
9260 H=0
9280 S=Q1/4
9300 FOR I=1 TO S
9320 H=H+I
9340 NEXT I
9360 C=0
9400 FOR Y=S+1 TO 1 STEP -1
9420 C=C+1
9440 I1=Y-1
9460 B(C,P)=(I1/H)*E(P)
9480 NEXT Y
9500 IF E$="1" GOTO 9540
9520 NEXT P
9540 A1=A2=A3=A4=A5=0
9560 FOR Y=1 TO Q1
9580 A1=A1+1
9600 A2=A2+M(Y,3)
9620 A3=A3+M(Y,5)
9640 A4=A4+M(Y,9)
9660 B(Y,10)=A4
9680 IF A1<>4 GOTO 9780
9700 B(Y/4,2)=A2
9720 B(Y/4,3)=A3
9740 B(Y/4,9)=A4
9760 A1=A2=A3=0
9780 NEXT Y
9800 C1=C2=0
9820 T6=0
9840 FOR Y=1 TO Q1+1
9860 C1=C1+1
9880 T6=T6+T(Y)
9900 B(Y,10)=T6
9920 B(Y,11)=B(Y,10)+B(Y,12)
9940 B(Y+1,12)=B(Y,11)*E9
9960 C2=C2+B(Y,12)
9980 IF C1<>4 THEN 10040
10000 B(Y/4,7)=C2
10020 C1=C2=0
10040 NEXT Y
10060 PRINT
10080 IF E$<>"1" GOTO 10120
10100 B(C4,6)=I5
10120 PRINT USING 10140,N$
10140 :   YR     INCOME    INTEREST    INTEREST   LOSS/GAIN    TAXES       ITC       C.F. EARN   NET EARNINGS '
10160 PRINT
10180 FOR Y=1 TO S
10200 B(Y,4)=B(Y,1)+B(Y,14)-B(Y,2)-B(Y,3)
10220 B(Y,5)=B(Y,4)*Z1
10240 B(Y,8)=B(Y,4)*(1-Z1)+B(Y,6)+B(Y,7)
10260 PRINT USING 10280,Y,B(Y,1),B(Y,2),B(Y,3),B(Y,4),B(Y,5),B(Y,6),B(Y,7),B(Y,8)
10280 :  ###  #######.##  #######.##  #######.##  #######.##  #######.## #######.##  #######.## #######.##
10300 NEXT Y
10320 GOTO 9040
10340 GOTO 10360
10360 END
   