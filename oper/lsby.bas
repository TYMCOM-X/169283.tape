0000? MARGIN 132
  0001? Q$=" "
 0010? DIM M(50,50)
0012? DIM T(50,50)
0100? PRINT "NAME OF CLIENT?"
    0102? INPUT A$
    0105? PRINT "EQUIPMENT COST($)?"
 0105? INPUT C
0105? PRINT "AMOUNT TO BE FINANCED($)?INTEREST ON LOAN(8%=8)?"
 0105? INPUT A,I
   0105? IF A=0 THEN 1062
 0105? PRINT "PAYMENTS IN ARREARS?(YES OR NO)"
   0105? INPUT B$
    0105? PRINT "LENGTH OF LOAN IN YEARS?PMT FREQUENCY(MONTHLY=12)?"
    0106? INPUT Q1,Q2
 0106? PRINT "TAX LIFE IN YEARS?"
 0106? INPUT Q3
    0106? PRINT "DEPRECIATION METHOD?"
    0106? PRINT " (DDB=1;SYD=2;STR=3)"
    0107? INPUT D
0107? PRINT "YEAR OF SWITCH?DEPR. METHOD?"
 0107? INPUT W,D1
  0107? PRINT "AGGREGATE TAX BRACKET (53%=53)?"
   0107? INPUT T1
    0107? PRINT "ITC AVAILABLE (YES OR NO)?"
   0107? INPUT K$
    0108? PRINT "OTHER CASH OUTFLOWS?"
    0108? INPUT S
0108? PRINT "SALVAGE?($)"
   0108? INPUT S9
    0108? PRINT "LEASE RATE (PER $100)?"
  0108? INPUT L
0108? PRINT "LENGTH OF LEASE IN YEARS?PMT. FREQ.(MONTHLY=12)"
  0109? INPUT Q4,Q5
 0109? PRINT "LEASE PMT IN ADVANCE?(YES OR NO)"
  0109? INPUT P$
    0109? PRINT "AFTER-TAX OPPORTUNITY COST OF MONEY (5%=5)?"
 0109? INPUT V
0109? PRINT "AMT. OF PURCHASE OPTION($)?IN ADVANCE?"
 0109? INPUT P,P9
  0110? PRINT "NO OF RENTALS IN ADVANCE?"
    0110? PRINT "(2 LAST PMT IN ADV.=14 PMT IN YR 1)"
    0110? INPUT R
0110? PRINT "ITC AVAILABLE UNDER LEASE(YES OR NO)?"
  0110? INPUT D$
    0125? M=12
   0130? IF Q1>Q3 THEN 1410
    0135? IF Q3>Q4 THEN 1475
    0140? N=Q4+1
 0140? GOTO 1500
   0141? N=Q1+1
 0145? GOTO 1500
   0147? N=Q3+1
 0150? K=C-A+S
0162? A1=A
   0163? PRINT "PAUSE"
    0163? INPUT O$
    0163? C7=C
   0165? FOR Y=1 TO Q4+1
  0165? IF Y=1 THEN 3025
 0186? IF Y=2 THEN 1890
 0186? IF Y<Q4+1 THEN 1870
   0186? M1=((L*C)/100)*Q5-R*((L*C)/100)
 0186? M(Y,1)=M1+P
 0186? M(Y,13)=M1*(T1/100)
   0186? M(Y,2)=(M1*((100-T1)/100))+P
    0186? GOTO 2049
   0187? M2=((L*C)/100)*Q5
0187? M(Y,1)=M2
   0187? M(Y,13)=M2*(T1/100)
   0187? M(Y,2)=M2*((100-T1)/100)
   0188? GOTO 2049
   0189? M3=((L*C)/100)*Q5+R*((L*C)/100)
 0189? M(Y,1)=M3+P9
0189? M(Y,13)=M3*(T1/100)
   0200? M(Y,2)=(M3*((100-T1)/100))+P9
   0204? IF Y>2 THEN 2080
 0205? IF D$="NO" THEN 2060
  0205? IF Q4>2 THEN 2053
0205? GOTO 2060
   0205? IF Q4>4 THEN 2056
0205? M(Y,2)=M(Y,2)-(C7*.0233333)
0205? GOTO 2060
   0205? IF Q4>6 THEN 2059
0205? M(Y,2)=M(Y,2)-(C7*.0466666)
0205? GOTO 2060
   0205? M(Y,2)=M(Y,2)-(C7*.07)
0206? T(Y,1)=M(Y,1)
    0206? T(Y,13)=M(Y,13)
  0207? T(Y,2)=M(Y,2)
    0207? GOTO 3030
   0208? T(Y,1)=T(Y-1,1)+M(Y,1)
0208? T(Y,13)=T(Y-1,13)+M(Y,13)
  0209? T(Y,2)=T(Y-1,2)+M(Y,2)
0302? GOTO 3030
   0302? M(Y,1)=0
    0302? M(Y,2)=0
    0302? M(Y,13)=0
   0303? NEXT Y
 0303? T2=T(Y,1)
   0303? S7=T(Y,13)
  0303? T3=T(Y,2)
   0303? GOTO 3050
   0303? FOR Y=Q4+2 TO Q3+1
    0303? K9=3333.33
  0303? M(Y,1)=K9
   0303? M(Y,2)=K9*(100-T1)/100
0304? M(Y,13)=K9*(T1/100)
   0304? T2=T2+M(Y,1)
0304? T3=T3+M(Y,2)
0304? S7=S7+M(Y,13)
    0304? NEXT Y
 0305? IF A=0 THEN 10222
0400? FOR Y=1 TO Q1+1
  0401? IF Y=1 THEN 10195
0402? I3=0
   0405? IF Y>2 THEN 6010
 0405? I4=I/100
    0500? I1=I4/Q2
    0505? C1=I1/(1-(1/((1+I1)^(Q1*Q2))))
  0600? C2=A1*C1
    0601? FOR X=1 TO Q2
    0605? I2=I1*A
0700? I3=I3+I2
    0705? P1=C2-I2
    0800? A=A-P1
 0805? NEXT X
 0905? M(Y,5)=C2*Q2
1000? M(Y,6)=I3
   1001? IF Y>2 THEN 10050
1002? T(Y,5)=M(Y,5)
    1003? T(Y,6)=M(Y,6)
    1004? GOTO 10200
  1005? T(Y,5)=T(Y-1,5)+M(Y,5)
1010? T(Y,6)=T(Y-1,6)+M(Y,6)
1019? GOTO 10200
  1019? M(Y,5)=M(Y,6)=0
  1020? NEXT Y
 1021? T5=T(Y,5)
   1022? T6=T(Y,6)
   1022? IF D>1 THEN 10226
1022? IF W=0 THEN 10250
1022? C=C-S9
 1025? FOR Y=1 TO Q3+1
  1025? IF Y=1 THEN 11790
1026? IF Y>2 THEN 10600
1030? FOR J=(Y-2) TO (W-1)
  1035? J1=J1+((W-1)-J)
  1040? NEXT J
 1045? FOR J=(Y-2) TO (Q3-W)+1
    1050? J2=J2+(((Q3-W)+1)-J)
  1055? NEXT J
 1056? FOR J=Y-2 TO Q3
  1057? J3=J3+(Q3-J)
1058? NEXT J
 1060? IF W=0 THEN 10715
1065? IF Y>W THEN 11200
1070? IF D=2 THEN 11050
1071? GOTO 10750
  1071? IF D=2 THEN 10725
1072? GOTO 10750
  1072? D3=((Q3-(Y-2))/J3)*C
  1073? GOTO 11525
  1075? D2=C/Q3
1080? IF D=3 THEN 10950
1085? D3=D2*2.00
  1090? GOTO 11500
  1095? D3=D2
  1100? GOTO 11525
  1105? D3=(((Q3+2)-Y)/J1)*C
  1110? GOTO 11525
  1120? IF D1=2 THEN 11350
    1125? D3=C/((Q3-W)+1)
  1130? GOTO 11525
  1135? D3=((Q3-(Y-2))/J2)*C
  1140? GOTO 11525
  1150? C=C-D3
 1152? M(Y,7)=D3
   1178? GOTO 11800
  1179? M(Y,7)=0
    1180? NEXT Y
 1185? FOR Y=1 TO N
1185? IF Y=1 THEN 11865
1186? GOTO 11900
  1186? M(Y,10)=K
   1187? GOTO 12410
  1190? GOTO 12050
  1195? IF Y<11 THEN 12050
    1200? M(Y,7)=M(Y,7)+10737.56
1205? M(Y,8)=M(Y,7)+M(Y,6)
  1215? M(Y,9)=M(Y,8)*(T1/100)
1225? M(Y,10)=M(Y,5)-M(Y,9)
 1230? IF Y>2 THEN 12370
1231? IF K$="NO" THEN 12320
 1231? IF Q3>2 THEN 12313
    1231? GOTO 12320
  1231? IF Q3>4 THEN 12316
    1231? M(Y,10)=M(Y,10)-(C7*.0233333)
   1231? GOTO 12320
  1231? IF Q3>6 THEN 12319
    1231? M(Y,10)=M(Y,10)-(C7*.0466666)
   1231? GOTO 12320
  1231? M(Y,10)=M(Y,10)-(C7*.07)
   1232? T(Y,7)=M(Y,7)
    1233? T(Y,8)=M(Y,8)
    1234? T(Y,9)=M(Y,9)
    1235? T(Y,10)=T(Y-1,10)+M(Y,10)
  1236? GOTO 12450
  1237? T(Y,7)=T(Y-1,7)+M(Y,7)
1238? T(Y,8)=T(Y-1,8)+M(Y,8)
1239? T(Y,9)=T(Y-1,9)+M(Y,9)
1239? T(Y,10)=T(Y-1,10)+M(Y,10)
  1240? GOTO 12450
  1241? M(Y,7)=M(Y,8)=0
  1242? M(Y,9)=0
    1243? T(Y,10)=M(Y,10)
  1245? NEXT Y
 1246? T7=T(Y,7)
   1246? T8=T(Y,8)
   1247? T9=T(Y,9)
   1247? S2=T(Y,10)
  1263? N9=1
   1265? A2=0
   1265? S4=0
   1266? IF (Q3-Q4)>1 THEN 12670
    1266? GOTO 12725
  1267? IF P>0 THEN 12681
1267? IF P9>0 THEN 12683
    1267? P1=0
   1268? GOTO 12720
  1268? P1=P
   1268? GOTO 12684
  1268? P1=P9
  1268? IF P1<500 THEN 12720
  1268? GOTO 12687
  1268? P1=C7*.10
   1268? FOR J=0 TO Q3-Q4
 1269? J4=J4+((Q3-Q4)-J)
1269? NEXT J
 1269? FOR Y=Q4+2 TO Q3+1
    1269? IF D=2 THEN 12707
1269? D4=P1/(Q3-Q4)
    1270? IF D=3 THEN 12704
1270? D4=D4*1.5
   1270? P1=P1-D4
    1270? M(Y,2)=(D4*(-1))*((100-T1)/100)
 1270? M(Y,13)=D4*((100-T1)/100)
  1270? GOTO 12710
  1270? D9=(((Q3-(Y-2))/J4)*P1)
    1270? M(Y,13)=D9*((100-T1)/100)
  1270? M(Y,2)=(D9*(-1))*((100-T1)/100)
 1271? S4=S4+M(Y,2)
1271? S8=S8+M(Y,13)
    1271? NEXT Y
 1272? T3=T3+S4
    1272? S7=S7+S8
    1272? FOR Y=1 TO N
1273? IF Y=1 THEN 13085
1273? M2=M(Y,2)/Q5
1274? M4=0
   1300? IF Y>2 THEN 13055
1301? IF P$="YES" THEN 13030
1302? K=1
    1302? GOTO 13055
  1303? K=0
    1305? FOR B=1 TO Q5
    1306? M4=M4+(M2/(1+(V/100)/Q5)^K)
1306? K=K+1
  1307? NEXT B
 1307? M(Y,4)=M4
   1308? GOTO 13090
  1308? M(Y,4)=M(Y,2)
    1309? A2=A2+M(Y,4)
1309? NEXT Y
 1309? FOR Y=1 TO N
1309? IF Y=1 THEN 14085
1309? IF A>0 THEN 13098
1309? Q2=12
  1309? M2=M(Y,10)/Q2
    1309? M4=0
   1400? IF Y>2 THEN 14055
1400? IF B$="NO" THEN 14020
 1401? K=1
    1401? GOTO 14055
  1402? K=0
    1402? GOTO 14055
  1403? Q2=12
  1403? K=1
    1405? FOR B=1 TO Q2
    1406? M4=M4+(M2/(1+(V/100)/Q2)^K)
1406? K=K+1
  1407? NEXT B
 1407? M(Y,12)=M4
  1408? GOTO 14090
  1408? M(Y,12)=M(Y,10)
  1409? A3=A3+M(Y,12)
    1409? NEXT Y
 1600? PRINT
  1605? PRINT
  1610? PRINT "                                  LEASE VS. BUY"
  1615? PRINT "                                 ###############"
 1620? PRINT
  1625? PRINT
  1630? PRINT USING 16350,A$
  1635? :                          PREPARED FOR 'LLLLLLLLLLLLLLLLLL'
  1640? PRINT
  1645? PRINT "                                       BY"
   1650? PRINT
  1655? PRINT "                              EQUILEASE CORPORATION"
   1660? PRINT
  1665? PRINT
  1670? PRINT
  1671? PRINT USING 16720 ,C7
 1672? :     EQUIPMENT COST:$########.##
    1675? PRINT
  1680? PRINT
  1685? PRINT "                           ANALYSIS OF LEASING PROPOSAL"
    1690? PRINT "                          ------------------------------"
   1695? PRINT
  1705? PRINT
  1708? PRINT USING 17085,Q$
  1708? :                                          NET     PRESENT VALUE  ACCUMULATED  ''
 1709? PRINT USING 17100,Q$
  1710? :              ANNUAL         TAX       CASH COST     OF A.T.    PRESENT VALUE  ''
1714? PRINT USING 17150,Q$
  1715? :     YEAR     RENTAL       SAVINGS     AFTER TAX    CASH COST   OF CASH COST  ''
 1730? PRINT USING 17350 ,T1,V
    1735? :                           @###.##%                 @###.###%
1740? PRINT
  1741? N9=N
   1741? F=0
    1741? FOR Y=1 TO N
1742? M(Y,3)=M(Y-1,3)+M(Y,4)
1742? NEXT Y
 1742? IF Q3>Q4 THEN 17426
   1742? GOTO 17450
  1742? IF P1>500 THEN 17429
  1742? GOTO 17450
  1742? N=Q4+1
 1745? FOR Y=1 TO N
1750? PRINT USING 17550 ,F,M(Y,1),M(Y,13),M(Y,2),M(Y,4),M(Y,3)
 1755? :     ###   ########.##  ########.##  ########.##  ########.##  #########.##
 1757? F=F+1
  1760? NEXT Y
 1761? IF Q3>Q4 THEN 17620
   1761? GOTO 17650
  1762? IF P1>500 THEN 17652
  1765? PRINT "            --------------------------------------------------"
  1765? GOTO 17658
  1765? FOR Y=Q4+2 TO Q3+1
    1765? PRINT USING 17654 ,F,M(Y,1),M(Y,13),M(Y,2),M(Y,4),M(Y,3)
 1765? :     ###   ########.##  ########.##* ########.##* ########.##  #########.##
 1765? F=F+1
  1765? NEXT Y
 1765? GOTO 17650
  1765? T4=A2
  1766? S3=A3
  1770? PRINT USING 17750,T2,S7,T3,T4
   1775? :  TOTALS: #########.## #########.## #########.## #########.##
1780? PRINT "                                                   ###########"
  1780? PRINT
  1780? PRINT
  1780? PRINT
  1781? PRINT "                         ANALYSIS OF PURCHASE ALTERNATIVE"
  1781? PRINT "                        ----------------------------------"
 1781? PRINT
  1781? PRINT
  1781? R$="***"
    1781? PRINT USING 17820,R$,Q$
    1782? :                             'LLL'        NET     PRESENT VALUE  ACCUMULATED  ''
 1782? PRINT USING 17824,Q$
  1782? :              PAYMENT        TAX       CASH COST     OF A.T.    PRESENT VALUE  ''
1782? PRINT USING 17828,Q$
  1782? :     YEAR     ON LOAN      SAVINGS     AFTER TAX    CASH COST   OF CASH COST  ''
 1783? PRINT USING 17832,T1,V
1783? :                           @###.##%                 @###.###%
1783? PRINT
  1783? F=0
    1783? FOR Y=1 TO N9
    1784? M(Y,11)=M(Y-1,11)+M(Y,12)
  1784? PRINT USING 17844 ,F,M(Y,5),M(Y,9),M(Y,10),M(Y,12),M(Y,11)
    1784? :     ###   ########.##  ########.##  ########.##  ########.##  #########.##
 1784? F=F+1
  1784? NEXT Y
 1785? PRINT "            --------------------------------------------------"
  1785? PRINT USING 17854 ,T5,T9,S2,S3
  1785? :  TOTALS: #########.## #########.## #########.## #########.##
1785? PRINT "                                                   ###########"
  1785? X$="*"
 1785? PRINT
  1785? PRINT
  1786? PRINT
  1786? IF T4<S3 THEN 17872
   1786? N8=T4-S3
    1786? PRINT "  **************************************************************************"
   1786? PRINT "  *                                                                        *"
   1786? PRINT USING 17868,X$,N8,T4,S3,X$
1786?       :  'L IT IS $$$$$$.$$ MORE ADVANTAGEOUS TO BUY   ($$$$$$$.$$ - $$$$$$$.$$)  'L'
  1786? PRINT "  *        ---------                                                       *"
   1787? PRINT "  **************************************************************************"
   1787? GOTO 17879
  1787? N8=S3-T4
    1787? PRINT "  ****************************************************************************"
 1787? PRINT "  *                                                                          *"
 1787? PRINT USING 17876,X$,N8,S3,T4,X$
1787?       :  'L IT IS $$$$$$$.$$ MORE ADVANTAGEOUS TO LEASE ($$$$$$$$.$$-$$$$$$$$.$$)   'L'
1787? PRINT "  *        ----------                      -----                             *"
 1787? PRINT "  ****************************************************************************"
 1787? PRINT
  1788? PRINT
  1788? PRINT
  1788? IF Q3>Q4 THEN 17886
   1788? GOTO 17892
  1788? IF P1>500 THEN 17890
  1788? GOTO 17892
  1789? PRINT "   * = REMAINING DEPRECIATION DUE TO OWNERSHIP"
   1789? PRINT 
 1789? PRINT
  1789? PRINT "            ***  COMPUTATION OF TOTAL TAX DEDUCTIONS :"
1790? PRINT
  1790? PRINT USING 17904 ,Q$
 1790? :                      BANK                   TOTAL TAX  ''
   1790? PRINT USING 17908 ,Q$
 1790? :            YEAR    INTEREST  DEPRECIATION  DEDUCTIONS  ''
   1790? PRINT USING 17910 ,I
  1791? :                    @###.###%
  1791? PRINT
  1791? F=0
    1791? FOR Y=1 TO N9
    1791? PRINT USING 17919 ,F,M(Y,6),M(Y,7),M(Y,8)
 1791? :            ###    ######.##  #######.##   #######.##
   1792? F=F+1
  1792? NEXT Y
 1792? PRINT "                   -----------------------------------"
1792? PRINT USING 17928 ,T6,T7,T8
1792? :         TOTALS: ########.## ########.## #########.##
   1793? PRINT
  1793? PRINT
  1793? PRINT
  1793? PRINT "       END OF ANALYSIS ;      THANK YOU!!"
   1793? FOR B=1 TO 6
1794? PRINT
  1794? NEXT B
 1900? GOTO 26000
  1901? PRINT "TRY FOR C/S/C?"
1902? INPUT B$
    1903? IF B$="NO" THEN 26000
 1904? PRINT "ANNUAL SIMPEL INTEREST?ANNUAL PMT?"
1905? INPUT I5,C9
 1905?     
   1906? C9=C9/Q5
    1907? FOR Y=1 TO Q4+1
  1908? IF Y=1 THEN 20120
1909? I9=0
   2000? IF Y>2 THEN 20050
2001? I6=I5/100
   2002? I7=I6/Q5
    2005? FOR X=1 TO Q5
    2006? I8=I7*C7
    2007? I9=I9+I8
    2008? P5=C9-I8
    2009? C7=C7-P5
    2010? NEXT X
 2011? M(Y,13)=C9*Q5
    2011? M(Y,14)=I9
  2011? IF Y>2 THEN 20117
2011? T(Y,13)=M(Y,13)
  2011? T(Y,14)=M(Y,14)
  2011? GOTO 20122
  2011? T(Y,13)=T(Y-1,13)+M(Y,13)
  2011? T(Y,14)=T(Y-1,14)+M(Y,14)
  2011? GOTO 20122
  2012? M(Y,13)=0
   2012? M(Y,14)=0
   2012? NEXT Y
 2012? S4=T(Y,13)
  2012? S5=T(Y,14)
  2012? FOR Y=1 TO N
2012? IF Y=1 THEN 20240
2012? M(Y,15)=M(Y,7)+M(Y,14)
2013? M(Y,16)=M(Y,15)*(T1/100)
   2013? M(Y,17)=M(Y,13)-M(Y,16)
    2014? IF Y>2 THEN 20205
2016? T(Y,15)=M(Y,15)
  2016? T(Y,16)=M(Y,16)
  2017? T(Y,17)=M(Y,17)
  2018? GOTO 20250
  2020? T(Y,15)=T(Y-1,15)+M(Y,15)
  2021? T(Y,16)=T(Y-1,16)+M(Y,16)
  2021? T(Y,17)=T(Y-1,17)+M(Y,17)
  2022? GOTO 20250
  2024? M(Y,15)=M(Y,16)=M(Y,17)=0
  2025? NEXT Y
 2025? S6=T(Y,15)
  2025? S7=T(Y,16)
  2026? S8=T(Y,17)
  2026? K=Q5
   2026? A9=0
   2026? FOR Y=1 TO N
2026? IF Y=1 THEN 20278
2027? M(Y,18)=M(Y,17)/(1+(V/100)/Q5)^K
2027? K=K+Q5
 2027? A9=A9+M(Y,18)
    2027? GOTO 20280
  2027? M(Y,18)=0
   2028? NEXT Y
 2028? PRINT "                                             CONDITIONAL"
   2028? PRINT "                                            SALES CONTRACT"
 2028? PRINT "                                           ---------------"
 2029? PRINT
  2030? PRINT
  2030? PRINT
  2031? PRINT USING 20311,Q$
  2031? :                                                                               PRESENT   ''
2032? PRINT USING 20321,Q$
  2032? :                                                                      NET       VALUE   ''
 2034? PRINT USING 20341,Q$
  2034? :                                    DEPRE-                   TAX   CASH COST  OF A.T.   ''
 2034? PRINT USING 20346,Q$
  2034?:       YEAR   PAYMENT   INTEREST   CIATION      TOTAL      SAVINGS AFTER TAX   CASHFLOW   ''
2035? PRINT USING 20360 ,V
  2036? :                                                                               @##.##%
2037? PRINT
  2038? F1=0
   2039? FOR Y=1 TO N
2040? PRINT USING 20410 ,F1,M(Y,13),M(Y,14),M(Y,7),M(Y,15),M(Y,16),M(Y,17),M(Y,18)
 2041? :       ### #######.## #######.## #######.## #######.## #######.## #######.## #######.##
    2042? F1=F1+1
2043? NEXT Y
 2044? PRINT USING 20441,Q$
  2044?:           ----------------------------------------------------------------------------   ''
2045? PRINT USING 20460 ,S4,S5,T7,S6,S7,S8,A9
   2046? : TOTALS:   #######.## #######.## #######.## #######.## #######.## #######.## #######.##
    2047? PRINT "                                                                              #########"
  2048? PRINT
  2049? PRINT
  2050? PRINT
  2051? PRINT
  2052? PRINT
  2053? PRINT
  2054? PRINT
  2055? PRINT
  2056? PRINT
  2600? END
    