120 PRINT "DATA FILE NAME?"
130 INPUT A$
140 PRINT"NO. OF CHANNELS OF DATA TO BE PROCESSED?"
150 INPUT X
160 FILE #1,A$,#2,"CORREC"
170 SCRATCH #2
190 I=0
200 INPUT #1,B$
210 PRINT #2,B$
220 INPUT #1,B$
230 Y$="0123456789"
240 IF LEFT$(B$,4)="0000" THEN 250
241 GO TO 280
250 GOSUB 480
260 GOSUB 600
270 GO TO 790
280 INPUT #1,C$
290 IF LEFT$ (C$,4)="0000" THEN 650
300 IF LEFT$ (C$,4)="0050"THEN 307
302 GO TO 670
307 GOSUB 480
310 PRINT I,LEFT$(B$,4)
320 PRINT "THIS ANGLE IS BAD"
330 E$=LEFT$(C$,4)
340 H$=(STR$(VAL(LEFT$(C$,4))-50)+(RIGHT$(B$,LEN(B$)-4)))
360 G$=H$
370 IF VAL(E$)>=950 THEN 380
375 G$="0"+H$
380 IF VAL (E$)>=50 THEN 390
385 G$="00"+H$
390 IF VAL(E$)<> 50 THEN 400
395 G$="000"+H$
400 H$=G$
410 T$=LEFT$(H$,4)
420 PRINT I,T$
430 PRINT "CORRECTED ANGLE"
440 B$=H$
450 IF P=X*4+4 THEN 467
460 GOSUB 630
462 GOSUB 700
463 GO TO 460
467 GOSUB 630
470 GO TO 800
480 I=I+1
490 P=LEN(B$)
500 FOR J=1 TO P
510 IF INSTR (Y$,MID$(B$,J,1))<> 0 THEN 520
515 GOSUB 540
520 NEXT J
530 RETURN
540 PRINTI,B$
550 PRINT"THIS LINE HAS NON-NUMERICAL CHARACTERS IN THE DATA"
560 K$=LEFT$(B$,J-1)+"0"+RIGHT$ (B$,LEN (B$)-J)
570 B$=K$
590 RETURN
600 IF P=X*4+4 THEN 630
605 GOSUB 690
610 PRINT #2,B$
620 GO TO 800
630 PRINT #2,B$
640 RETURN
650 PRINT"PLEASE CHECK FIRST TWO LINES FOR BAD SHAFT ENCODER NO."
660 STOP
670 PRINT"SHAFT ENCODER HAS TOO MANY ERRORS IN EARLY SAMPLES"
680 STOP
690 INPUT #1,C$
700 P=LEN(C$)
710 PRINT I,B$
720 PRINT"BAD LINE LENGTH"
730 F$=(STR$(VAL(LEFT$(C$,4))-50)+(RIGHT$(C$,P-4)))
740 F$=RIGHT$(F$,P)
750 IF VAL(LEFT$(C$,4))-50 <> 0 THEN 760
755 T$="0000"
760 F$=T$+MID$(F$,3)
770 B$=RIGHT$ (F$,P)
780 RETURN
790 INPUT #1,C$
800 I=I+1
810 T=LEN(C$)
820 FOR J=1 TO T
830 IF INSTR (Y$,MID$(C$,J,1))<>0 THEN 840
835 GOSUB 860
840 NEXT J
850 GO TO 920
860 PRINT I,C$
870 PRINT "THIS LINE HAS NON-NUMERICAL CHARACTERS IN THE DATA"
880 S$=LEFT$(C$,J-1)+"0"+RIGHT$(C$,LEN(C$)-J)
890 C$=S$
910 RETURN
920 D=VAL(LEFT$(C$,4))
930 E=VAL(LEFT$(B$,4))+50
940 IF E<>3600 THEN 950
945 E=0000
950 IF D=E THEN 1140
955 GO TO 980
960 E=VAL(LEFT$(B$,4))+50
970 IF VAL(LEFT$(C$,4))=VAL(LEFT$(B$,4))+50 THEN 1140
980 PRINT I,LEFT$(C$,4)
990 PRINT "THIS ANGLE IS BAD"
1000 E$=LEFT$(B$,4)
1010 H$=(STR$(VAL(LEFT$(B$,4))+50)+(RIGHT$(C$,LEN(C$)-4)))
1030 G$=H$
1040 IF VAL(E$)>=950 THEN 1050
1045 G$="0"+H$
1050 IF VAL (E$)>=50 THEN 1060
1055 G$="00"+H$
1060 H$=G$
1070 T$=LEFT$(H$,4)
1080 IF T$<>"3600" THEN 1090
1085 T$="0000"
1090 H$=T$+MID$(H$,5)
1100 PRINT I,T$
1110 PRINT "CORRECTED ANGLE"
1120 C$=H$
1130 GO TO 1180
1140 T$=LEFT$(C$,4)
1150 IF T$<>"3600" THEN 1160
1155 T$="0000"
1160 L$=T$+MID$(C$,5)
1170 C$=L$
1180 IF T=X*4+4 THEN 1190
1185 GOSUB 1230
1190 B$=C$
1200 GOSUB 630
1210 GO TO 790
1220 STOP
1230 T=LEN(B$)
1240 PRINT I,C$
1250 PRINT"BAD LINE LENGTH"
1251 IF LEFT$(B$,1)<>"0" THEN 1260
1252 A1=1
1253 IF MID$(B$,2,1)<>"0" THEN 1260
1254 A2=1
1260 F$=(STR$(VAL(LEFT$(B$,4))+50)+(RIGHT$(B$,T-4)))
1261 IF A1<>1 THEN 1270
1262 F$="0"+F$
1263 IF A2<>1 THEN 1270
1264 F$="0"+F$
1270 F$=RIGHT$(F$,T)
1280 T$=LEFT$(F$,4)
1290 IF T$<>"3600" THEN 1300
1295 T$="0000"
1300 F$=T$+MID$(F$,5)
1310 C$=RIGHT$(F$,T)
1320 RETURN
1340 DIMX$(10)
1350 RESTORE #2
1370 INPUT #2,X$
1375 FOR I=1 TO 10
1380 L$(I)=""
1385 NEXT I
1390 FOR J=1 TO 3
1400 FOR I=1 TO 30
1410 P$=MID$(X$,I,1)
1420 IF P$=";" THEN 1450
1430 L$(J)=L$(J)+P$
1440 NEXT I
1450 H$=MID$(X$,I+1)
1460 NEXT J
1470 R=0
1480 INPUT #2,X$
1490 R=R+1
1500 FOR I=1 TO (X+1)
1510 B$(I)=MID$(X$,I*4-3,4)
1520 IF I=1 THEN 1530
1525 GO TO 1550
1530 Z=VAL(B$(I))*.1
1540 NEXT I
1550 F$="13579"
1560 IF INSTR (F$,(RIGHT$(B$(I),1)))=0 THEN 1580
1565 GO TO 1610
1580 V(I)=VAL(B$(I))*.1
1590 GO TO 1540
1600 GO TO 1660
1610 B$(I)="-"+B$(I)
1630 IF B$(I)<>"-0001" THEN 1640
1635 B$(I)="0000"
1640 V(I)=VAL(B$(I))*.1
1650 GO TO 1540
1660 IF R=1 THEN 1670
1665 GO TO 1750
1670 FILE #3,"OUT"+L$(J)
1675 SCRATCH #3
1680 PRINT #3,"RUN# "+L$(J)
1690 PRINT #3,H$
1700 PRINT #3
1710 PRINT #3,"   RUN SH.EN   1     2     3     4     5     6     7     8     9     10"
1750 PRINT #3,USING " 2 ####",VAL(L$(J))
1760 PRINT #3,USING "  #### ",Z
1770 FOR I=2 TO X+1
1780 PRINT #3, USING "####.#",V(I)
1790 NEXT I
1800 PRINT #3
1810 GO TO 1480
1830 PRINT "FINISHED"
1840 END
  