100 '   C H G S T R
110 '   -----------
120 '
130 '   CHSTR6 - VERSION 1.0
150 '
160 '   COMPANY PROPRIETARY, LUPFER AND LONG COMPUTER SERVICES.
170 '
180 DIM H(72),F(16,15),N$(60),U$(30)
190 DIM L$(400),M$(400)
200 DIM I(132),O(132),T(40)
210 DIM F$(10),G$(10),T$(4),R$(15)
220 '
270 FILE #1, "COMCOM.TMP"
280 MAT READ #1, F$
300 '
310 FILE :1: F$(3)
320   GOSUB 8000
330 IF F$(2)="MERGE" GO TO 360
340 FILE :4: F$(5)
350   GOSUB 8500
360 '
372 PRINT "=== CHANGES TO ";N$(0);" - ";F$(1);" ==="
374 '
380 REM -------------- GET FIELD AND CHANGE --------------
390 GOSUB 4300			'  WHAT FIELD ?
400 IF R$="DONE" GO TO 1690
410 IF R$<>"STOP" GO TO 440
420   PRINT N$(0);" NOT UPDATED"
430   GO TO 2280
440 M9=0
445 IF F$(2)="MERGE" GO TO 480
450 Q$="2) CHANGE HEADINGS"
460 GOSUB 2900
470 IF R$="NO" GO TO 600
480 '     - HEADINGS -
490 PRINT "HEADINGS MAY BE UP TO ";F(8,J1);" CHARACTERS LONG"
500 FOR J=2 TO 3
510   PRINT TAB(2);"HEADING ";J-1;
520   INPUT R$
530   IF LEN(R$)<=F(8,J1) GO TO 560
540     PRINT "TOO LONG"
550     GO TO 510
560   IF LEN(R$)=F(8,J1) GO TO 580
570     R$=R$+SPACE$(F(8,J1)-LEN(R$))
580   N$(J1+J*O9)=R$
590 NEXT J
595 IF F$(2)="MERGE" GO TO 380
600 Q$="3) CHANGE EDITING CRITERIA"
610 GOSUB 2900
620 IF R$="NO" GO TO 380
630 '
640 REM ----------- ORIGINALLY LIST ----------------
650 IF F(3,J1)<>1 GO TO 1360
660 Q$="  PRINT CURRENT EDIT LIST"
670 GOSUB 2900
680 T1=0
690 IF R$="NO" GO TO 710
700   T1=1
710 '   COPY CURRENT LIST TO M$( )
720 M9=0
730 FOR J=F(11,J1) TO F(11,J1)+F(12,J1)-1
740   M9=M9+1
750   M$(M9)=L$(J)
760   IF T1=0 GO TO 780
765     CHANGE M$(M9) TO T
770     GOSUB 2800		'  PRINT T
780 NEXT J
790 J2=J1
800 L8=F(11,J1)
802 L9=L9-F(12,J1)
804 IF F(11,J1)-1=L9 GO TO 980
810 '   COMPRESS REMAINING L$( )
830 IF F(11,J2)+F(12,J2)-1=L9 GO TO 980
840 FOR J3=1 TO O9
850   IF F(11,J3)=F(11,J2)+F(12,J2) GO TO 880
860 NEXT J3
862   PRINT 862
870   GO TO 9000
880 I=L8
890 FOR J=F(11,J3) TO F(11,J3)+F(12,J3)-1
900   L$(I)=L$(J)
910   I=I+1
920 NEXT J
930 F(11,J3)=L8
940 L8=I
950 J2=J3
960 GO TO 810
970 '
980 '   CHANGE LIST
990 IF L9=L8-1 GO TO 1000
992   PRINT 992
994   GO TO 9000
1000 IF J1=H(10) GO TO 1050
1010 Q$="CHANGE METHOD"
1020 GOSUB 2900
1030 IF R$="YES" GO TO 1330
1040 '
1050 Q$="DELETE FROM LIST"
1060 GOSUB 2900
1070 IF R$="NO" GO TO 1220
1080 PRINT "TYPE ITEMS TO BE DELETED, TYPE 'DONE' WHEN THROUGH"
1090 PRINT
1100 INPUT R$(J1)
1110 IF R$(J1)="DONE" GO TO 1220
1120 GOSUB 3000
1130   IF E1>0 GO TO 1100
1140 FOR J=1 TO M9
1150   IF R$(J1)=M$(J) GO TO 1190
1160 NEXT J
1170 PRINT "NOT IN LIST - RETYPE"
1180   GO TO 1100
1190 M$(J)=""
1200   GO TO 1100
1210 '
1220 '   ADDITIONS
1230 Q$="ADD TO LIST"
1240 GOSUB 2900
1250 IF R$="NO" GO TO 1300
1260 PRINT
1270 PRINT "TYPE ADDITIONS TO LIST, TYPE 'DONE' WHEN THROUGH"
1280 GOSUB 4600
1290 '
1300 GOSUB 4800			'  PUT LIST IN L$( )
1310 GO TO 380
1320 '
1330 '   CHANGE METHOD
1340 H(4)=H(4)-1
1350 GO TO 1470
1360 REM --------- ORIGINALLY RANGE ----------------
1370 IF F(3,J1)<>2 GO TO 1470
1380 IF J1=H(13) GO TO 1440
1390 Q$="CHANGE METHOD (CURRENTLY 'RANGE')"
1400 GOSUB 2900
1410 IF R$="NO" GO TO 1440
1420   H(5)=H(5)-1
1430   GO TO 1570
1440 '   NEW RANGE
1450 GOSUB 4500
1460 GO TO 380
1470 REM -------- NEW METHOD --------------
1480 F(3,J1)=0
1490 Q$="EDIT BY RANGE"
1500 GOSUB 2900
1510 IF R$="NO" GO TO 1570
1520 F(3,J1)=2
1530 H(5)=H(5)+1
1540 GOSUB 4500
1550 GO TO 380
1560 '
1570 F(3,J1)=0
1580 Q$="EDIT BY LIST"
1590 GOSUB 2900
1600   IF R$="NO" GO TO 380
1610 F(3,J1)=1
1620 H(4)=H(4)+1
1630 M9=0
1640 PRINT
1650 PRINT "SUPPLY LEGAL VALUES, TYPE 'DONE' WHEN THROUGH"
1660 GOSUB 4600			'  BUILD LIST
1670 GOSUB 4800			'  PUT IN L$( )
1680 GO TO 380
1690 REM ======= WRAP-UP ===============
1700 '
1710 REM ------- WRITE NEW .STR --------
1730 SET :1, 1
1740 CHANGE H1$ TO O
1750 O(4)=H(4)
1760 O(5)=H(5)
1770 GOSUB 6000
1780 CHANGE O TO O$
1790 WRITE :1: O$		'  WRITE HEADER
1810 '
1820 O(0)=16
1830 FOR J1=1 TO O9
1840   IF F(6,J1)=0 GO TO 1960
1850   T1=INT(132/F(6,J1))
1860   F(10,J1)=INT(F(12,J1)/T1+.99)
1870   FOR X=1 TO 16
1880     O(X)=F(X,J1)
1890   NEXT X
1892   O(15)=INT(O(11)/100)		'  SPLIT LIST THINGS
1894   O(11)=INT(O(11)-100*O(15))	'  INTO TWO PARTS
1896   O(16)=INT(O(12)/100)
1898   O(12)=INT(O(12)-100*O(16))
1900   GOSUB 6000
1910   CHANGE O TO O$
1920   FOR I=0 TO 3
1930     O$=O$+N$(J1+I*O9)
1940   NEXT I
1950   WRITE :1: O$		'  FIELD RECORDS
1960 NEXT J1
1970 IF F$(2)="MERGE" GO TO 2280
1980 O(0)=72
1990 FOR J1=1 TO O9
2000   IF F(3,J1)<>1 THEN 2120
2010   Z=F(11,J1)
2020   FOR Q=1 TO F(10,J1)
2030      O$=L$(Z)
2040     Z=Z+1
2050     FOR J=2 TO INT(132/F(6,J1))
2060       IF Z>F(12,J1)+F(11,J1)-1 THEN 2090
2070       O$ = O$ + L$(Z)
2080       Z=Z+1
2090     NEXT J
2100   WRITE :1: O$		'  LISTS
2110   NEXT Q
2120 NEXT J1
2130 '
2140 FOR J1=1 TO O9
2150   IF F(3,J1)<>2 GO TO 2270
2160   CHANGE U$(J1) TO T
2170   V1=2
2180   GOSUB 2600
2190   CHANGE U$(O9+J1) TO T
2200   V1=2+F(6,J1)
2210   GOSUB 2600
2220   O(0)=1+2*F(6,J1)
2230   O(1)=J1
2240   GOSUB 6000
2250   CHANGE O TO O$
2260   WRITE :1: O$		'  RANGES
2270 NEXT J1
2280 REM ------- STOP -------------
2290 CHAIN F$(10)+F$(9)
2600 REM -------- INSERT ----------
2610 K1=1
2620 FOR K2=V1 TO V1+T(0)-1
2630   O(K2)=T(K1)
2640   K1=K1+1
2650 NEXT K2
2660 RETURN
2800 REM ----- LEFT STRIP & PRINT ---------
2805 IF F(2,J1)<>H(10) GO TO 2880
2810 FOR K1=1 TO T(0)-F(8,J1)+1
2815   IF T(K1)<>ASC(0) GO TO 2825
2820 NEXT K1
2825 K3=0
2830 FOR K2=K1 TO T(0)
2835   K3=K3+1
2840   T(K3)=T(K2)
2845 NEXT K2
2850 T(0)=K3
2880 CHANGE T TO S$
2885 PRINT TAB(3);S$
2890 RETURN
2900 REM ------ YES/NO -------
2910 PRINT
2920 PRINT Q$;
2930 INPUT R$
2940   IF R$="YES" GO TO 2980
2950   IF R$="NO" GO TO 2980
2960 PRINT "PLEASE ANSWER 'YES' OR 'NO'"
2970   GO TO 2900
2980 RETURN
3000 REM ------ EDIT ----------
3010 E1=0
3020 E1$="-"
3030 CHANGE R$(J1) TO T
3040 L6=F(6,J1)
3050 IF F(5,J1)<>3 GO TO 3070
3060   L6=8
3070 REM    - START -
3080 REM
3090 REM     - $ FIELD -
3100 IF F(5,J1)<>4 GO TO 3300
3110 IF T(T(0))=ASC(-) GO TO 3160
3120 IF T(T(0))=ASC(+) GO TO 3150
3130 IF T(T(0))=32     GO TO 3150
3140   T(0)=T(0)+1
3150   T(T(0))=32
3160 IF T(0)<4 GO TO 3270
3170 IF T(T(0)-3)<>ASC(.) GO TO 3270
3180 FOR K1=T(0)-3 TO T(0)
3190   T(K1)=T(K1+1)
3200 NEXT K1
3210 T(0)=T(0)-1
3260   GO TO 3300
3270 E1$="NO DECIMAL"
3280   GO TO 4120
3290 REM
3300 REM     - LENGTH -
3310 IF F(5,J1)=3 GO TO 3380
3320 IF T(0)>L6 GO TO 3350
3330 IF T(0)<F(9,J1) GO TO 3350
3340   GO TO 3380
3350 E1$="INCORRECT LENGTH"
3360   GO TO 4120
3370 REM
3380 REM     - FILL -
3390 IF T(0)>=L6 GO TO 3560
3400   D=L6-T(0)
3410   IF F(5,J1)=2 GO TO 3500
3420 REM    "0"
3430 FOR K1=L6 TO 1 STEP -1
3440   IF K1<=D GO TO 3470
3450   T(K1)=T(K1-D)
3460     GO TO 3480
3470   T(K1)=ASC(0)
3480 NEXT K1
3490 GO TO 3540
3500 REM    " "
3510 FOR K1=T(0)+1 TO L6
3520   T(K1)=32
3530 NEXT K1
3540 T(0)=L6
3550 REM
3560 REM     - DATE -
3570 IF F(5,J1)<>3 GO TO 3790
3580 IF T(6)<>ASC(-) GO TO 3760
3590 IF T(3)= ASC(-) GO TO 3650
3600 IF T(4)<>ASC(-) GO TO 3760
3610   FOR K1=1 TO 3
3620     T(K1)=T(K1+1)
3630   NEXT K1
3640   T(4)=ASC(0)
3650 IF T(1)> ASC(1) GO TO 4120
3660 IF T(4)> ASC(3) GO TO 4120
3670 IF T(7)<>ASC(7) GO TO 4120
3680 T(6)=T(5)
3690 T(5)=T(4)
3700 T(4)=T(2)
3710 T(3)=T(1)
3720 T(1)=T(7)
3730 T(2)=T(8)
3740 T(0)=6
3750   GO TO 3790
3760 E1$="USE FORM 'MM-DD-YY'"
3770   GO TO 4120
3780 REM
3790 REM     - NUMERIC -
3800 IF F(5,J1)=2 GO TO 3910
3810 T1=T(0)
3820 IF F(5,J1)<>4 GO TO 3840
3830   T1=T1-1
3840 FOR K1=1 TO T1
3850   IF T(K1)>ASC(9) GO TO 3890
3860   IF T(K1)<ASC(0) GO TO 3890
3870 NEXT K1
3880   GO TO 3910
3890 E1$="NON-NUMERIC CHARACTER"
3900   GO TO 4120
3910 GO TO 4140
4110 '
4120 PRINT N$(J1);" IN ERROR - ";E1$;" - RE-TYPE LINE"
4130 E1=1
4140 CHANGE T TO R$(J1)
4150 RETURN
4300 REM ------ WHICH FIELD -------
4305 PRINT
4310 PRINT "1) FIELD TO CHANGE (OR 'DONE')";
4315 INPUT R$
4316 IF R$="HELP" GO TO 4370
4317 IF R$="STOP" GO TO 4490
4318 IF R$="DONE" GO TO 4490
4320 FOR J1=1 TO O9
4325   IF F(2,J1)=0 GO TO 4340
4330   IF N$(J1)=R$ GO TO 4490
4340 NEXT J1
4350 PRINT "  '";R$;"' NOT PRESENT -TYPE 'HELP' IF NEEDED"
4360   GO TO 4300
4370 '  HELP
4375 PRINT
4380 PRINT "FIELDS ARE:"
4385 PRINT
4390 FOR J1=1 TO O9
4400   IF F(2,J1)=0 GO TO 4420
4410     PRINT TAB(5);N$(J1)
4420 NEXT J1
4450   GO TO 4300
4490 RETURN
4500 REM --------- NEW RANGE -------------
4505 PRINT TAB(3);"LOW VALUE";
4510 INPUT R$(J1)
4515 GOSUB 3000
4520   IF E1>0 GO TO 4505
4525 U$(J1)=R$(J1)
4528 '
4530 PRINT TAB(3);"HIGH VALUE";
4535 INPUT R$(J1)
4540 GOSUB 3000
4545   IF E1>0 GO TO 4530
4550 U$(O9+J1)=R$(J1)
4555 RETURN
4600 REM -------- BUILD LIST -------------
4605 IF J1<>H(10) THEN 4615
4610   PRINT "TYPE 'ALL' TO TAKE VALUES FROM CHART OF ACCOUNTS"
4615 INPUT R$(J1)
4620 IF R$(J1)="DONE" GO TO 4755
4625 IF J1<>H(10) THEN 4640
4630   IF R$(J1)="ALL" THEN 4725
4635 '
4640 GOSUB 3000
4645   IF E1>0 GO TO 4615
4650 IF J1<>H(10) THEN 4705
4655 '
4660 IF R$(J1)>G$(1) GO TO 4675		'  CHECK VS .COA
4665   SET :4,1
4670   GOSUB 8500
4675 GOSUB 8700
4680 IF R$(J1)=G$(1) GO TO 4705
4685 IF S1=0 GO TO 4675
4690   PRINT "NOT ON CHART OF ACCOUNTS - RETYPE"
4695   GO TO 4615
4700 '
4705 M9=M9+1
4710 M$(M9)=R$(J1)
4715 GO TO 4615
4720 '
4725 '   TAKE ACCOUNTS FROM .COA
4726 M9=0
4728 GOSUB 8700
4735 IF S1=1 GO TO 4755
4740   M9=M9+1
4742   M$(M9)=G$(1)
4745   GO TO 4728
4755 RETURN
4800 REM ------ PUT LIST IN L$( ) --------
4802 IF L9+M9<401 GO TO 4810
4803   M9=400-L9
4804   PRINT "LIST SIZE EXCEEDS AVAILABLE STORAGE, FIRST ";
4805   PRINT M9;" ELEMENTS STORED"
4810 F(11,J1)=L9+1
4820 F(12,J1)=0
4830 FOR I=1 TO M9
4840   IF M$(I)="" GO TO 4880
4845   L9=L9+1
4850   L$(L9)=M$(I)
4870   F(12,J1)=F(12,J1)+1
4880 NEXT I
4895 RETURN
6000 REM --- KLUDGE CHAR SET FOR PDP-10 ---
6010 FOR K1=1 TO O(0)
6020   IF O(K1)>13 GO TO 6040
6030     O(K1)=100+O(K1)
6040 NEXT K1
6050 RETURN
6100 REM ---- UNKLUDGE PDP-10 CHARS -----
6110 FOR K1=1 TO I(0)
6120   IF I(K1)<100 GO TO 6140
6130     I(K1)=I(K1)-100
6140 NEXT K1
6150 RETURN
8000 REM ---READ STRUCTURE FILE---
8005 O9=15
8010 READ :1: I$
8012 H1$=I$
8015 CHANGE I$ TO I
8020 GOSUB 6100
8025 FOR Y=0 TO I(0)
8030   H(Y)=I(Y)
8035 NEXT Y
8040 N$=MID$(I$,20,13)
8045 N$(0)=MID$(I$,34,H(33))
8050 FOR T2=1 TO H(3)
8055   READ :1: I$
8060   CHANGE I$ TO I
8065   GOSUB 6100
8070   T3=I(2)
8075   FOR T1=1 TO 16
8080     F(T1,T3)=I(T1)
8085   NEXT T1
8088   F(11,T3)=100*F(15,T3)+F(11,T3)
8090   F(12,T3)=100*F(16,T3)+F(12,T3)
8095   N$(T3)=MID$(I$,17,I(7))
8100   T4=17+I(7)
8105   N$(T3+1*O9)=MID$(I$,T4,I(8))
8110   T4=17+I(7)+I(8)
8115   N$(T3+2*O9)=MID$(I$,T4,I(8))
8125   N$(T3+3*O9)=MID$(I$,T4+I(8),I(8))
8130 NEXT T2
8132 IF F$(2)="MERGE" GO TO 8270
8135 L=1
8140 T5=1
8145 FOR T1=1 TO H(8)
8150   IF F(3,T1)<>1 THEN 8205
8155   FOR T2=1 TO F(10,T1)
8160     READ :1: I$
8165     T4=1
8170     FOR T3=1 TO INT(132/F(6,T1))
8175       L$(L)=MID$(I$,T4,F(6,T1))
8180       T4=T4+F(6,T1)
8185     L=L+1
8190     IF L-T5=F(12,T1) THEN 8205
8195     NEXT T3
8200   NEXT T2
8205   T5=L
8210 NEXT T1
8212 L9=L-1
8215 IF H(5)=0 GO TO 8260
8220 FOR T1=1 TO H(5)
8225   READ :1: I$
8230   CHANGE I$ TO I
8235   GOSUB 6100
8240   T2=I(1)
8245   U$(T2)=MID$(I$,2,F(6,T2))
8250   U$(T2+O9)=MID$(I$,2+F(6,T2),F(6,T2))
8255 NEXT T1
8260 F(13,0)=1
8265 F(6,0)=2
8270 RETURN
8500 REM ----- READ .COA HEADER ---------
8510 S1=0
8520 READ :4: G1$
8530 CHANGE G1$ TO I
8535 GOSUB 6100
8540 G1=I(10)
8550 G2=I(11)
8560 G3=I(12)
8570 G4=I(13)
8580 G5=I(14)
8590 G6=I(15)
8600 G7=I(16)
8601 G8=I(16)
8610 SET :4, LOC(4)+1
8690 RETURN
8700 REM ----- READ .COA RECORD -----------
8705 SET :4, LOC(4)+G7
8710 IF LOC(4)<=LOF(4) GO TO 8800
8720   S1=1
8730   GO TO 8890
8800 READ :4: I$
8830 G$(1)=LEFT$(I$,8)
8870 SET :4, LOC(4)+1
8890 RETURN
9000 REM ---------- SYSTEM ERROR --------
9010 PRINT "* SYSTEM ERROR * CHGSTR *"
9020 STOP
9999 END
 