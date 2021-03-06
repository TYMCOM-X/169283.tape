100 '     D E F V A R
110 '     -----------
120 '
130 '     COPYRIGHT 1973, LUPFER & LONG COMPUTER SERVICES
140 '
150 DIM G$(5),F$(10),V$(10)
160 DIM I(132),O(132),T(50)
170 '
180 DEF FNP(X) = X*(G(7)+2) + (V6+2)
185 '
190 FILE #3, "COMCOM.TMP"
200 FOR I=1 TO 10
210   READ #3, F$(I)
220 NEXT I
225 '
230 FILE :1: "NEWCOA.TMP$132"
240 FILE :4: F$(5)		'  .COA
250   GOSUB 8500
400 REM ====== GET RUN TYPE ==========
410 PRINT "NAME OF CATEGORY (PLANNED, PROJECTED, ETC)";
412 INPUT V$
416 IF LEN(V$)<15 GO TO 422
418   PRINT "TOO LONG, LIMIT IS 14 CHARACTERS"
420   GO TO 410
422 V$=SPACE$(15-LEN(V$))+V$
424 FOR V6=1 TO G(7)
440   IF V$=V$(V6) GO TO 490
450 NEXT V6
460 S3=1
480   GO TO 600
490 Q$="ALREADY DEFINED, REDEFINE"
500 GOSUB 2100
510 IF R$="NO" GO TO 400
520 S3=0
600 '   WHICH ACCTS
640 S4=1		'  FIXED FOR NOW
650 Q$="SHOULD BALANCES VARY FROM MONTH TO MONTH"
660 GOSUB 2100
670 IF R$="NO" GO TO 800
680 S2=1
690 S$=G1$
700 S$=MID$(S$,29,8)
710 PRINT "BALANCES WILL BE ENTERED BEGINNING WITH PERIOD 1, ENDING ";
720 PRINT S$
730 PRINT
800 REM ========== SET-UP .COA =============
810 IF S3=0 GO TO 1000
815 REM ------ NEW COA HEADER ----
820 CHANGE G1$ TO O
830 O(16)=O(16)+1
850 CHANGE O TO O$
860 WRITE :1: O$
880 GOSUB 7600
894 CHANGE V$ TO T
896 V1=10
898 GOSUB 2600
899 CHANGE O TO O$
900 WRITE :1: O$
920 REM ------ BUILD NEW COA ------
922 IF END :4 GO TO 986
925 GOSUB 7000
940 WRITE :1: I$
950 GOSUB 7600
962 CHANGE O TO O$
970 WRITE :1: O$
980   GO TO 920
986 G(7)=G(7)+1
992 V6=G(7)
993 SET :1, 1
994 SET :4, 1
995 IF END :1 GO TO 1000
996 GOSUB 7100
997 WRITE :4: I$
998   GO TO 995
1000 REM ========== BEGIN DEFINITION =========
1002 SET :4, 3+G(7)
1010 IF S4=0 GO TO 1020
1012 PRINT
1014 PRINT "TYPE 'DONE' WHEN THROUGH"
1016   GO TO 1300
1020 REM ------- ALL ACCTS --------
1030 FOR R3=1 TO 10000
1040   SET :4, FNP(R3)
1050   IF END :4 GO TO 1500
1060   GOSUB 7000
1070   S$=I$
1080   S$=MID$(S$,9-G(1),G(1))
1090   PRINT
1100   PRINT "- "S$;" -"
1110   GOSUB 5000
1140 NEXT R3
1300 REM ------ SOME ACCTS --------
1310 PRINT
1320 PRINT "FOR ACCOUNT";
1330 INPUT R$
1334 IF R$="DONE" GO TO 1500
1340 '  - LOOK-UP -
1350 FOR R3=1 TO 1000
1360   SET :4, FNP(R3)
1370   IF END :4 GO TO 1430
1380   GOSUB 7000
1390   S$=I$
1400   S$=MID$(S$,9-G(1),G(1))
1410   IF S$=R$ GO TO 1460
1420 NEXT R3
1425 '
1430 PRINT "CAN'T FIND ";R$
1440   GO TO 1300
1450 '
1460 GOSUB 5000
1480   GO TO 1300
1500 REM =========== WRAP-UP ================
1610 CHAIN F$(10)+F$(9)
2100 REM ------ YES/NO -------
2110 PRINT
2120 PRINT Q$;
2130 INPUT R$
2132   IF R$<>"Y" GO TO 2136
2134     R$="YES"
2136   IF R$<>"N" GO TO 2140
2138     R$="NO"
2140   IF R$="YES" GO TO 2180
2150   IF R$="NO" GO TO 2180
2160 PRINT "PLEASE ANSWER 'YES' OR 'NO'"
2170   GO TO 2100
2180 RETURN
2600 REM -------- INSERT ----------
2610 K1=1
2620 FOR K2=V1 TO V1+T(0)-1
2630   O(K2)=T(K1)
2640   K1=K1+1
2650 NEXT K2
2660 RETURN
2800 REM ------- WIPE RECORD -------
2810 FOR K1=10 TO 132
2820   O(K1)=ASC(0)
2830 NEXT K1
2840 O(0)=132
2890 RETURN
4200 REM ------ PACKED $ EDIT --------
4210 E1=0
4220 E1$="-"
4230 T3=10
4240 IF T(T(0))=ASC(-) GO TO 4280
4250 IF T(T(0))=ASC(+) GO TO 4270
4260   T(0)=T(0)+1
4270   T3=0
4280 IF T(0)<4 GO TO 4490
4290 IF T(T(0)-3)<>ASC(.) GO TO 4490
4300 T(T(0)-3)=T(T(0)-2)
4310 T(T(0)-2)=T(T(0)-1)
4320 T(0)=T(0)-2
4330 IF T(0)<=G(8) GO TO 4360
4340   E1$="TOO LONG"
4350   GO TO 4500
4360 K1=T(0)
4370 FOR K2=G(8) TO 1 STEP -1
4380   IF K1=0 GO TO 4440
4390   IF T(K1)>ASC(9) GO TO 4500
4400   IF T(K1)<ASC(0) GO TO 4500
4410   T(K2)=T(K1)
4420   K1=K1-1
4430     GO TO 4450
4440   T(K2)=ASC(0)
4450 NEXT K2
4460 T(0)=G(8)
4470 T(G(8))=T(G(8))+T3
4480   GO TO 4510
4490 E1$="NO DECIMAL"
4500 E1=1
4510 RETURN
5000 REM ------- GET BALAANCES, BUILD DETAIL ---------
5005 CHANGE I$ TO O
5010 GOSUB 2800
5100 N8=0			'  CURRENT FIELD COUNT
5110 N9=11*S2+1			'  MAX FIELDS
5200 PRINT TAB(2);"BALANCE";
5210 IF S2=0 GO TO 5230
5220   PRINT "(";N8+1;")";
5230 MAT INPUT R$
5240 IF R$(1)="DONE" GO TO 5480
5250 IF N8+NUM<=N9 GO TO 5300
5260   PRINT "ERROR - TOO MANY FIELDS - RETYPE LINE"
5270   GO TO 5200
5300 J3=0
5310 FOR J1=1 TO NUM
5312   IF R$(J1)<>"0" GO TO 5320
5314     R$(J1)="0.00-"
5320   CHANGE R$(J1) TO T
5330   GOSUB 4200
5340     IF E1=0 GO TO 5350
5342       PRINT "ERROR - ";E1$;" - RETYPE ";R$(J1)
5344       GO TO 5390
5350   J3=J1
5360   V1=10+G(8)*(N8+J3-1)
5370   GOSUB 2600
5380 NEXT J1
5390 N8=N8+J3
5400 IF N8<N9 GO TO 5200
5410 IF S2=1 GO TO 5490
5420 FOR I=2 TO G(3)
5430   V1=10+G(8)*(I-1)
5440   GOSUB 2600
5450 NEXT I
5480 CHANGE O TO O$
5490 SET :4, LOC(4)-1
5500 WRITE :4: O$
5590 RETURN
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
7000 REM -------- READ SINGLE RECORD (OLD) ------
7010 READ :4: I$
7020 SET :4, LOC(4)-1
7030 READ :4: S$
7040 IF I$=S$ GO TO 7090
7050   SET :4, LOC(4)-1
7060   GO TO 7010
7090 RETURN
7100 REM -------- READ SINGLE RECORD (NEW) ------
7110 READ :1: I$
7120 SET :1, LOC(1)-1
7130 READ :1: S$
7140 IF I$=S$ GO TO 7190
7150   SET :1, LOC(1)-1
7160   GO TO 7110
7190 RETURN
7600 REM ------- COPY DETAILS 2-N ----------
7602 '   G(7) IS OLD G(7)
7610 FOR T=1 TO G(7)+1
7620   GOSUB 7000
7630   WRITE :1: I$
7640 NEXT T
7645 CHANGE I$ TO O
7650 GOSUB 2800
7690 O(9)=O(9)+1
7695 RETURN
8500 REM ----- READ .COA HEADER ---------
8510 S1=0
8520 GOSUB 7000
8521 G1$=I$
8530 CHANGE G1$ TO I
8535 GOSUB 6100
8540 FOR T=1 TO 8
8550   G(T)=I(9+T)
8560 NEXT T
8562 G(8)=10			'*****
8565 GOSUB 7000
8570 FOR T=1 TO G(7)
8580   GOSUB 7000
8590   V$(T)=MID$(I$,10,15)
8600 NEXT T
8602 S$=G1$
8610 G2$=MID$(S$,29+8*(G(4)-G(6)),8)
8620 SET :4, 2
8690 RETURN
9300 S$=O$
9310 PRINT LEFT$(S$,20)
9320 RETURN
9999 END
   