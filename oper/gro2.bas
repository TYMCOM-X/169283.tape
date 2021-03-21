100 PRINT USING 110
110 :ENTER NUMBER OF TEETH IN GEAR
120 INPUT N1
130 PRINT USING 140
140 :ENTER NORMAL DIAMETRAL PITCH
150 INPUT P
160 PRINT USING 170
170 :ENTER NORMAL PRESSURE ANGLE (IN DEGREES)
180 INPUT A1
190 PRINT USING 200
200 :ENTER NUMBER OF TEETH IN MASTER GEAR
210 INPUT N2
220 PRINT USING 230
230 :ENTER MINIMUM TEST RADIUS OF FINISHED GEAR
240 INPUT R1
250 PRINT USING 260
260 :ENTER MAXIMUM TEST RADIUS OF FINISHED GEAR
270 INPUT R2
280 PRINT USING 290
290 :ENTER DIAMETER OF WIRES
300 INPUT W
310 PRINT USING 320
320 :ENTER DESIRED OUTSIDE DIAMETER OF FINISHED GEAR
330 INPUT D1
340 PRINT USING 350
350 :ENTER DESIRED ROOT DIAMETER OF FINISHED GEAR
360 INPUT D2
370 PRINT USING 380
380 :ENTER ESTIMATED DIE FACTOR
390 INPUT F
400 LET U=0
410 LET K=0
420 LET C1=(N2/P)/2+R1
430 LET C3=(N2+N1)/(2*P)
440 LET A2 = A1/(180/3.1415927)
450 LET B1=C3*COS(A2)/C1
460 LET A3=ACS(B1)
470 LET E1=2*N2*C1/(N2+N1)
480 LET E2=N1/P
490 LET E3=2*C1-E1
500 LET A4=TAN(A2)-A2
510 LET A5=TAN(A3)-A3
520 LET G=(3.141593/(2*P))/(N2/P)
530 LET T1=E1*(G+A4-A5)
540 LET T2=(E3*3.1415926)/N1-T1
550 LET T3=E2*((T2/E3)+A5-A4)
560 IF K=1 GOTO 600
570 PRINT USING 580 ,T3
580 :MINIMUM ARC TOOTH THICKNESS OF GEAR =###.######
590 IF K=0 GOTO 620
600 PRINT USING 610 ,T3
610 :MAXIMUM ARC TOOTH THICKNESS OF GEAR =###.######
620 LET K=K+1
630 IF K=2 GOTO 660
640 LET R1=R2
650 GOTO 420
660 PRINT USING 670
670 :ENTER DESIRED MINIMUM ARC TOOTH THICKNESS
680 INPUT T4
690 PRINT USING 700
700 :ENTER DESIRED MAXIMUM ARC TOOTH THICKNESS
710 INPUT T5
720 LET T4=T4*F
730 LET P1=(N1/P)*F
740 LET B=P1*COS(A2)
750 LET T6=T4/P1
760 LET B1=W/B
770 LET H=3.1415926/N1
780 LET A6=T6+B1+A4-H
790 LET J1=3.1415926/180
800 FOR X=10*J1 TO 40*J1 STEP J1/8
810 LET Y=TAN(X)-X
820 IF Y>A6 THEN 840
830 NEXT X
840 LET X= X-J1/8
850 FOR X1=X TO X+J1/8 STEP J1/3600
860 LET Y1=TAN(X1)-X1
870 IF Y1>A6 THEN 890
880 NEXT X1
890 LET F1=TAN(X1-J1/3600)-(X1-J1/3600)
900 LET Q=Y1-A6
910 LET S=A6-F1
920 IF Q>S THEN 940
930 GOTO 950
940 LET X1=X1-J1/3600
950 LET C4=B/COS(X1)
960 FOR L=0 TO 2000
970 IF L=N1/2 THEN 1000
980 IF L>N1/2 THEN 1020
990 NEXT L
1000 LET M1=C4+W
1010 GOTO 1030
1020 LET M1=C4*COS(3.1415926/2/N1)+W
1030 LET D3=D1*F
1040 LET D4=D2*F
1050 IF U=1 THEN 1130
1060 PRINT USING 1070 ,B,P1
1070 :BASE DIA.  =###.######  PITCH DIA.  =###.######
1080 PRINT USING 1090 ,D3,D4
1090 :OUTSIDE DIA. =###.###### ROOT DIA.  =###.######
1100 PRINT USING 1110 ,W,M1
1110 :MINIMUM DIMENSION OVER TWO #.#### DIA. WIRES=###.######
1120 IF U=0 THEN 1150
1130 PRINT USING 1140 ,W,M1
1140 :MAXIMUM DIMENSION OVER TWO #.#### DIA. WIRES=###.######
1150 LET U=U+1
1160 IF U=2 THEN 1190
1170 LET T4=T5
1180 GOTO 720
1190 PRINT USING 1200
1200 :ENTER TOOTH FILLET RADIUS
1210 INPUT R3
1220 PRINT USING 1230
1230 :ENTER LAP CLEARANCE (ONE SIDE)
1240 INPUT S5
1250 LET Z=0
1260 LET V1=T4
1270 LET V2=T4+.0005
1280 LET D5=D3-2*S5
1290 LET D6=D4-2*S5
1300 LET R4=R3+S5
1310 LET V3=V1-2*(S5/B*P1)
1320 LET V4=V3/P1
1330 LET A8=V4+B1+A4-H
1340 FOR X3=10*J1 TO 40*J1 STEP J1/8
1350 LET Y3=TAN(X3)-X3
1360 IF Y3>A8 THEN 1380
1370 NEXT X3
1380 LET X3=X3-J1/8
1390 FOR X4=X3 TO X3+J1/8 STEP J1/3600
1400 LET Y4=TAN(X4)-X4
1410 IF Y4>A8 THEN 1430
1420 NEXT X4
1430 LET F2=TAN(X4-J1/3600)-(X4-J1/3600)
1440 LET Q1=Y4-A8
1450 LET S1=A8-F2
1460 IF Q1>=S1 THEN 1480
1470 GOTO 1490
1480 LET X4=X4-J1/3600
1490 LET C5=B/COS(X4)
1500 FOR L=0 TO 2000
1510 IF L=N1/2 THEN 1540
1520 IF L>N1/2 THEN 1560
1530 NEXT L
1540 LET M2=C5+W
1550 GOTO 1570
1560 LET M2=C5*COS(3.1415926/2/N1)+W
1570 IF Z=1 GOTO 1650
1580 PRINT USING 1590 ,D5
1590 :OUTSIDE DIAMETER        =###.######
1600 PRINT USING 1610 ,D6,R4
1610 :ROOT DIA.  =###.######  ROOT FILLET RADIUS =###.######
1620 PRINT USING 1630 ,W,M2
1630 :MINIMUM DIMENSION OVER TWO #.#### DIA. WIRES  =###.######
1640 IF Z=0 THEN 1670
1650 PRINT USING 1660 ,W,M2 
1660 :MAXIMUM DIMENSION OVER TWO #.### DIA. WIRES   =###.######
1670 LET Z=Z+1
1680 IF Z=2 THEN 1710
1690 LET V1=V2
1700 GOTO 1280
1710 PRINT USING 1720
1720 :IF NEW CLEARANCE DESIRED, ENTER IT; IF NOT ENTER ZERO
1730 INPUT S5
1740 IF S5=0 THEN 1760
1750 GOTO 1250
1760 END
  