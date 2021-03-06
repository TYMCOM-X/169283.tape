0001? FILES TMX1,ATF1,,ATF2,,INCV
0001? SCRATCH #1,2,4,6
 0010? DIM M(137,15),U1(600),U2(200),U4(200),U6(200)
  0010? DIM V(137,2),C(137,2) 
0011? DIM E(30,9)   
   0011? DIM I(137,2)  
   0012? DIM D(137,3)  
   0015? DIM T(30,5),Y(30,14)  
0016? DIM U(3) 
   0017? DIM N(137,5)  
   0020? DIM S(29,13)
0022? Y$="YES" 
   0025? PRINT"ITERATION METHOD?"      
  0026? READ I$
0026? IF LEFT$(I$,3)="IDC" THEN 265 
  0026? B7=B8=0  
   0026? GO TO 270
   0026? PRINT "TOTAL TAX SAVINGS";    
  0026? READ B7
0026? PRINT "TOTAL IDC TO CONSORT"; 
  0026? READ B8
0026? J$="IDC" 
   0027? PRINT "AMOUNT OF CONSORTIUM'S ASSET"       
    0027? READ B6
0027? PRINT "NEW OR USED,TAX LIFE,TAX RATE"      
    0028? READ Z$,Y1,T5
    0028? IF Z$<>"READ" THEN 285
0028? PRINT "READ FROM"     
0028? READ  D$
    0028? T6=1.00-T5    
   0029? PRINT"# YEARS IN LEASE? # PERIODS PER YEAR?"       
 0030? READ Q1,Q6
  0031? PRINT"INTEREST RATE/YEAR ON AMORTIZED DEBT? # PERIODS ON INTEREST ONLY(IF ANY)?"     
  0032? READ I1,N8
  0033? PRINT"AMOUNT OF BALLOON(IF ANY)? INTEREST RATE PER YEAR?"       
   0034? READ B5,I5
  0034? PRINT "%ITC,%FRONT END FEES"  
  0034? READ X5,F
   0034? K$="NO"  
   0034? IF F=0 THEN 364       
0036? F=F*B6   
   0036? PRINT "CAPITALIZE FEES"       
  0036? READ K$
0036? IF K$="YES" THEN 367  
0036? X5=X5*B6 
   0036? GO TO 370
   0036? X5=X5*(B6+F)  
   0037? S5=.04/Q6
   0038? IF Z$="USED" THEN 388 
0038? M9=200   
   0038? GOTO 390 
   0038? M9=150   
   0039? PRINT"DO YOU WANT A YEARLY PRINTOUT?"      
    0040? READ C$
0042? PRINT "% RESID FOR YIELD"     
  0042? READ R7
0042? R7=R7*B6 
   0044? Q=Q1*Q6  
   0045? N2=Q-N8  
   0045? PRINT "ADVANCE RENTALS";      
  0045? READ A$
0050? IF I$="YES" THEN 625  
0050? P$="YES" 
   0051? PRINT"LEASE RENTAL RATE PER YEAR?" 
  0051? READ L1
0052? PRINT "%EQUITY"       
0053? READ E1
0053? PRINT "$ CONVERTS"    
0053? READ V1
0055? GO TO 820
   0062? PRINT "% EQUITY(4)"   
0065? READ E1,E2,E3,E4
 0067? PRINT"LEASE RENTAL RATES PER YEAR (3)?"    
    0070? READ L1,L2,L3
    0080? PRINT"AMOUNT OF CONVERTIBLE DEBT (2)?"     
    0080?  READ V1,V2
 0082? PRINT"COUPON RATE PER YEAR?"  
  0082? READ C8
0083? C8=C8/Q6 
   0083? C7=C8*T6 
   0083? PRINT"CONVERSION @ PAR VALUE=1"    
  0083? PRINT"     ''      50% PREM.=2"    
  0083? PRINT"     ''     100% PREM.=3"    
  0083? PRINT"     ''     150% PREM.=4"    
  0084? PRINT"     ''     200% PREM.=5"    
  0084? PRINT"WHICH CONVERSION SCHEDULE?"  
  0084? READ F9
0084? IF F9=1 THEN 851      
0084? PRINT"DEBT CONVERTED IN WHAT PERIOD?"      
    0084? READ T9
0084? GOTO 879 
   0085? PRINT"DEBT RETIRED OVER HOW MANY PERIODS? WHICH PERIODS?"       
   0085? READ T1
0085? FOR X=1 TO T1 
   0086? READ T9
0086? V(T9,1)=V1/T1 
   0086? V(T9,2)=V2/T1 
   0087? NEXT X   
   0087? IF F9=1 THEN 900      
0088? ON F9-1 GOTO 881,883,885,887  
  0088? K6=1.5   
   0088? GOTO 888 
   0088? K6=2     
   0088? GOTO 888 
   0088? K6=2.5   
   0088? GOTO 888 
   0088? K6=3     
   0088? V(T9,1)=V1*K6 
   0088? V(T9,2)=V2*K6 
   0089? IF I$="YES" THEN 900  
0089? K=1
    0089? V9=V1    
   0089? GO TO 910
   0090? FOR K=1 TO 2  
   0090? IF K=2 THEN 908       
0090? V9=V1    
   0090? GOTO 910 
   0090? V9=V2    
   0091? FOR X=1 TO Q  
   0091? C(X,K)=C7*V9  
   0091? I(X,K)=C8*V9  
   0092? C(X,K)=C(X,K)+V(X,K)  
0092? V9=V9-V(X,K)  
   0093? NEXT X   
   0093? IF I$="YES" THEN 940  
0093? GO TO 965
   0094? NEXT K   
   0096? IF I$="YES" THEN 975  
0096? T=1      
   0097? GOTO 1000
   0097? FOR T=1 TO 24 
   0100? IF T>12 THEN 1015     
0100? V=V1     
   0100? K=1      
   0101? GOTO 1020
   0101? V=V2     
   0101? K=2      
   0102? GOTO 1025
   0102? MAT D=ZER
   0105? MAT N=ZER
   0107? MAT M=ZER
   0107? U4=K1=V4=0    
   0108? V6=0     
   0110? K4=0     
   0112? MAT S=ZER
   0115? B1=B6    
   0117? IF T<13 THEN 1200     
0118? ON T-12 GOTO 1225,1350,1475,1600,1725,1850,1975,2100,2225,2350,2475,2575
0120? IF T>1 THEN 1325      
0122? E=E1     
   0125? A=A1     
   0127? L9=L1    
   0130? GOTO 2650
   0132? IF T>2 THEN 1450      
0135? E=E2     
   0137? A=A2     
   0140? L9=L1    
   0142? GOTO 2650
   0145? IF T>3 THEN 1575      
0147? E=E3     
   0150? A=A3     
   0152? L9=L1    
   0155? GOTO 2650
   0157? IF T>4 THEN 1700      
0160? E=E4     
   0162? A=A4     
   0165? L9=L1    
   0167? GOTO 2650
   0170? IF T>5 THEN 1825      
0172? E=E1     
   0175? A=A1     
   0177? L9=L2    
   0180? GOTO 2650
   0182? IF T>6 THEN 1950      
0185? E=E2     
   0187? A=A2     
   0190? L9=L2    
   0192? GOTO 2650
   0195? IF T>7 THEN 2075      
0197? E=E3     
   0200? A=A3     
   0202? L9=L2    
   0205? GOTO 2650
   0207? IF T>8 THEN 2200      
0210? E=E4     
   0212? A=A4     
   0215? L9=L2    
   0217? GOTO 2650
   0220? IF T>9 THEN 2325      
0222? E=E1     
   0225? A=A1     
   0227? L9=L3    
   0230? GOTO 2650
   0232? IF T>10 THEN 2450     
0235? E=E2     
   0237? A=A2     
   0240? L9=L3    
   0242? GOTO 2650
   0245? IF T>11 THEN 2575     
0247? E=E3     
   0250? A=A3     
   0252? L9=L3    
   0255? GOTO 2650
   0257? E=E4     
   0260? A=A4     
   0262? L9=L3    
   0265? IF K$="YES" THEN 2662 
0265? E=E*(B6+B8)   
   0265? A=B6+B8-E-B5  
   0265? K7=A     
   0266? E=E+F    
   0266? GO TO 2675    
   0266? E=E*(B6+B8+F) 
   0266? A=B6+B8-E-B5+F
   0266? K7=A     
   0267? L7=L9/Q6 
   0270? L8=L7/(1-(1/((1+L7)^Q)))      
  0270? IF K$="NO" THEN 2710  
0270? L=L8*(B6+B8+F)
   0270? GO TO 2725    
   0271? L=L8*(B6+B8)  
   0272? IF Z$<>"READ" THEN 2749       
  0272? FILE #3,D$    
   0272? FOR I=1 TO Y1*Q6      
0273? READ #3,M(I,2)
   0273? M(I,2)=M(I,2)*B6      
0273? NEXT I   
   0273? RESTORE 3
   0273? GO TO 3800    
   0274? FOR N=1 TO Y1 
   0275? IF V4>0 THEN 2800     
0277? A8=B1    
   0280? GOTO 2825
   0282? IF M9=150 THEN 2900   
0285? D1=B1*(2/Y1)  
   0287? GOTO 2925
   0290? D1=B1*(1.5/Y1)
   0292? GOTO 2950
   0295? IF N>1 THEN 3050      
0297? FOR G=N-1 TO Y1       
0300? K1=K1+(Y1-G)  
   0302? NEXT G   
   0305? IF N=1 THEN 3125      
0307? IF V4>=1 THEN 3125
    0310? K1=K1-((Y1-N)+2)      
0312? D2=A8*(((Y1-N)+1)/K1) 
0315? D3=B1*(1/((Y1+1)-N))  
0317? IF M9=200 THEN 3225   
0320? D2=0     
   0322? IF D1>=D2 THEN 3350
   0325? D=D2     
   0327? D(N,3)=2 
   0330? V4=V4+1  
   0332? GOTO 3400
   0335? D=D1     
   0337? D(N,3)=1 
   0340? IF D>D3 THEN 3500     
0342? D=D3     
   0345? D(N,3)=3 
   0347? GOTO 3500
   0350? D(N,2)=D 
   0350? B1=B1-D  
   0355? NEXT N   
   0356? IF Q6=2 THEN 3685     
0357? FOR X=1 TO Y1 
   0360? FOR Y=(4*X)-3 TO 4*X  
0362? M(Y,2)=D(X,2)/4       
0365? NEXT Y   
   0367? NEXT X   
   0368? GO TO 3800    
   0368? FOR X=1 TO Y1 
   0369? FOR Y=(2*X)-1 TO 2*X  
0370? M(Y,2)=D(X,2)/2       
0371? NEXT Y   
   0372? NEXT X   
   0380?  U2=0
  0380?  
 0380? U5=0
   0380?  
 0381? U6=0
   0381? IF A$<>"YES" THEN 3825
0381? E7=(1-T5)*L   
   0382? U2=U2+1
0382? U2(U2)=Q+1
  0382? U2=U2+1
0382? U2(U2)=(E-E7-B7-X5)*(-1)
   0383? U5=U5+1
0383? U4(U5)=Q+1
  0383? U5=U5+1
0383? U4(U5)=(E-E7-B7-X5+V)*(-1)
 0383? U1=0
   0383?  
 0385? IF N8=0 THEN 3975     
0385? IF K4=1 THEN 3975     
0386? FOR N=1 TO N8 
   0386? M(N,4)=K7*(I1/Q6)     
0386? M(N,9)=M(N,4) 
   0386? U1=U1+1
0386? U1(U1)=K7
   0387? NEXT N   
   0390? K4=1     
   0397? FOR N=1 TO Q  
   0400? IF N>1 THEN 4100      
0402? I2=I1/Q6 
   0405? C1=I2/(1-(1/((1+I2)^N2)))     
  0405? IF A$<>"YES" THEN 4075
0405? C1=I2/(1-(1/((1+I2)^(N2-1)))) 
  0407? C=A*C1   
   0410? I=I2*A   
   0410? IF A$<>"YES" THEN 4125
0410? IF (N+N8)<Q THEN 4125 
0410? C=I=0    
   0412? P=C-I 
 0415? M(N+N8,4)=I   
   0417? A=A-P 
 0418? IF(N+N8)>Q THEN 4225
  0418? U1=U1+1
0418? U1(U1)=A
    0418? GO TO 4225    
   0422? O1=O1 
 0422? M(N,1)=L      
   0423? IF N<Q THEN 4250       
    0423? IF A$<>"YES" THEN 4235 
    0423? M(N,1)=0      
   0423? M(N,1)=M(N,1)+R7       
    0425? M(N,5)=B5*(I5/Q6)      
    0425? IF A$<>"YES" THEN 4275 
    0425? IF N<Q THEN 4275       
    0425? M(N,5)=0      
   0427? M(N,6)=F/Q    
   0430? M(N,7)=M(N,1)-M(N,2)-M(N,4)-M(N,5)-M(N,6)       
    0432? M(N,8)=M(N,7)*T5*(-1)  
    0435? M(N+N8,9)=C   
   0437? M(N,10)=M(N,5)
   0440? M(N,11)=M(N,1)-M(N,9)-M(N,10)  
 0442? M(N,12)=M(N,8)+M(N,11) 
    0443? IF N>1 THEN 4442       
    0443? M(N,13)=M(N,12)
  0444? GOTO 4444     
   0444? M(N,13)=M(N-1,13)+M(N,12)      
 0444? M(N,14)=(E-B7-E7-X5)*(-1)+M(N,13)      
   0444? U2=U2+1
0444? U2(U2)=M(N,12)
   0444? U5=U5+1
0444? U4(U5)=C(N,K)+M(N,12)
 0444? U6=U6+1
0445? U6(U6)=I(N,K)
    0446? S(T,1)=S(T,5)+M(N,1)
  0447? S(T,2)=S(T,2)+M(N,2)   
    0450? S(T,4)=S(T,4)+M(N,4)   
    0450? S(T,5)=S(T,5)+M(N,5)   
    0450? S(T,6)=S(T,6)+M(N,6)   
    0450? S(T,7)=S(T,7)+M(N,7)   
    0450? S(T,8)=S(T,8)+M(N,8)   
    0450? S(T,9)=S(T,9)+M(N,9)   
    0450? S(T,10)=S(T,10)+M(N,10)
    0450? S(T,12)=S(T,12)+M(N,12)
    0451? S(T,11)=S(T,11)+M(N,11)
    0452? NEXT N
 0452?  
 0452?  
 0453?  
 0453? A8=B6
  0453? FOR X=1 TO Q  
   0453? A8=A8-M(X,2)  
   0453? IF X<Y1*Q6 THEN 4538   
    0453? A8=0  
 0453? A9=M(X,14)    
   0453? IF A9<0 THEN 4545      
    0454? A9=0  
 0454? U1(U1+1)=M(X,12)
 0454? U1(U1+2)=-A9
0454? U1(U1+3)=A8
 0454? U1=U1+3
0454? NEXT X
 0540? MAT N=ZER     
   0542? B9=Q+1
 0545? N(1,4)=(E-B7-E7-X5+V)*(-1)     
 0545? IF T>12 THEN 5470      
    0546? K8=1  
 0546? GOTO 5475     
   0547? K8=2  
 0547? FOR Y=2 TO B9 
   0550? N(Y,1)=M(Y-1,12)+C(Y-1,K8)     
 0552? NEXT Y
 0555? B=0   
 0557? B1=0  
 0560? R=0   
 0562? FOR R=.010000 TO .015000 STEP .005000  
   0565? Z7=0  
 0566? FOR Y=2 TO B9 
   0566? N(Y,4)=0
    0566? N(Y,5)=0
    0566? N(Y,3)=0
    0566? N(Y,2)=0
    0567? NEXT Y
 0567? FOR Y=2 TO B9 
   0570? IF N(Y-1,4)<0 THEN 5800
    0572? N(Y,5)=N(Y-1,4)*S5     
    0575? N(Y,4)=N(Y-1,4)+N(Y,5)+N(Y,1)  
 0577? GOTO 6000     
   0580? N(Y,2)=ABS(N(Y-1,4))*R 
    0582? IF N(Y,2)>N(Y,1) THEN 5875     
 0585? GOTO 5925     
   0587? N(Y,3)=N(Y,1)-N(Y,2)   
    0590? GOTO 5950     
   0592? N(Y,3)=N(Y,1)-N(Y,2)   
    0595? N(Y,4)=N(Y-1,4)+N(Y,3) 
    0597? GOTO 6000     
   0600? B=B+1 
 0602? :### ###.#######  ############ 
 0605? NEXT Y
 0607? Z7=N(B9,4)    
   0610? IF U4=1 THEN 6500      
    0612? IF R=.015000 THEN 6225 
    0615? B1=Z7 
 0617? R1=R  
 0620? GOTO 6275     
   0622? B2=Z7 
 0625? R2=R  
 0627? GOTO 6300     
   0630? NEXT R
 0632? M=(B1-B2)/(R1-R2)      
    0635? Z=B2-(M*R2)   
   0637? :##.#####  ##.##### ##########  ##########      
    0640? R=-Z/M
 0642? U4=1  
 0645? W5=1  
 0647? GOTO 5650     
   0650? R3=R  
 0652? B3=Z7 
 0655? IF B3>50 THEN 6625     
    0657? IF B3<-50 THEN 6625    
    0660? GOTO 6800     
   0662? M1=(B2-B3)/(R2-R3)     
    0665? Z1=B3-(M1*R3) 
   0667? R=-Z1/M1      
   0670? W5=W5+1       
   0672? R2=R3 
 0675? B2=B3 
 0677? GOTO 5650     
   0680? IF P$="YES" THEN 6850  
    0682? GOTO 7825     
   0685? PRINT USING 6851,S(T,11)       
 0685? : TOTAL BTCF = ########
    0685? FOR I=1 TO Q  
   0685? IF M(I,11)<0 THEN 6858 
    0685? NEXT I
 0685? GO TO 6865    
   0685? PRINT "NEGATIVE BTCF"  
    0686? PRINT "YIELD=",R*Q6*100
    0686? PRINT "SEE LEASE POSITION"     
 0686? INPUT L$      
   0687? FOR A=1 TO Q1 
   0687? FOR X=1 TO 12 
   0688? IF Q6=2 THEN 6910      
    0690? Y(A,X)=M(4*A-3,X)+M(4*A-2,X)+M(4*A-1,X)+M(4*A,X)
    0690? GOTO 6925     
   0691? Y(A,X)=M(2*A-1,X)+M(2*A,X)     
 0692? NEXT X
 0695? Y(A,3)=D(A,3) 
   0705? IF A>1 THEN 7125       
    0707? Y(A,13)=Y(A,12)
  0710? GOTO 7150     
   0712? Y(A,13)=Y(A-1,13)+Y(A,12)      
 0715? Y(A,14)=(E-E7-B7-X5)*(-1)+Y(A,13)      
   0717? NEXT A
 0718? IF L$="NO" THEN 8050   
    0720? GOTO 7630     
   0722? FOR X=1 TO Q  
   0725? G2=M(X,12)    
   0727? G3=M(X,13)    
   0730? G4=M(X,14)    
   0735? PRINT USING 7375 ,X,M(X,1),M(X,2),M(X,4),M(X,5),M(X,6),M(X,7),M(X,8),M(X,9),M(X,10),M(X,11),G2,G3,G4      
 0737?:## ######## ####### ####### ##### ##### ######## ####### ####### ####### ####### ####### ######## ########
 0737?:##    #########
  0740? NEXT X
 0740? S1=S(T,5)     
   0740? S2=S(T,6)     
   0740? S3=S(T,7)     
   0740? S4=S(T,8)     
   0740? S9=S(T,9)     
   0740? S6=S(T,10)    
   0740? S8=S(T,12)    
   0741? S7=S(T,11)    
   0742? PRINT 
 0745? PRINT USING 7475 ,S(T,1),S(T,2),S(T,4),S1,S2,S3,S4,S9,S6,S7,S8  
   0747?:   ######## ####### ####### ##### ##### ######## ####### ####### ####### ####### ########
   0750? PRINT 
 0752? PRINT 
 0755? PRINT "YIELD=",R*Q6*100
    0760? :  ##.#######  
  0762? GOTO 8050     
   0763? PRINT 
 0763? PRINT"      LEASE CAPITALIZATION"       
  0763? PRINT"      ********************"       
  0763? PRINT USING 7640 ,E    
    0763? PRINT USING 7642 ,K7+B5
    0763? PRINT USING 7644,B6+F+B8       
 0764? :EQUITY $########      
    0764? :DEBT   $########      
    0764? :TOTAL $#########      
    0765? PRINT 
 0765? PRINT"     LEASE  DEPRE- INTEREST INTEREST FEE  TAX     TAX    DEBT INTEREST  B.T.   A.T.     CUM.      NET"      
   0765? PRINT"    RENTALS CIATION   ON       ONGAIN/   SAVING SERV.    ON     CASH   CASH    CASH    INVEST-"     
 0765? PRINT USING 7657 ,T5*100       
 0765? :   AMORT DBT  BALLN.      LOSS     @##.#%       BALLN.    FLOW   FLOW   
    0765? PRINT 
 0765? PRINT USING 7376,0,(E-B7-E7-X5)*(-1)   
   0766? IF C$="NO" THEN 7225   
    0766? FOR X=1 TO Q1 
   0767? H2=Y(X,12)    
   0770? H3=Y(X,13)    
   0772? H4=Y(X,14)    
   0775? PRINT USING 7375 ,X,Y(X,1),Y(X,2),Y(X,4),Y(X,5),Y(X,6),Y(X,7),Y(X,8),Y(X,9),Y(X,10),Y(X,11),H2,H3,H4      
 0777? NEXT X
 0780? GOTO 7402     
   0782? T(T,1)=L9     
   0785? T(T,2)=E      
   0787? T(T,3)=V      
   0790? T(T,4)=R*Q6*100
  0790? IF I$="YES" THEN 7915  
    0791? GOTO 8200     
   0791? NEXT T
 0791? PRINT 
 0792? PRINT"      RENTAL   LEASE   CONVERT.  AFTER TAX"       
 0792? PRINT"       RATE   EQUITY     DEBT    YRLY YIELD"      
 0792? PRINT 
 0792? FOR T=1 TO 24 
   0795? PRINT USING 7975 ,T,T(T,1),T(T,2),T(T,3),T(T,4) 
    0797? : ##  .###### ######## ####### ###.### 
   0800? NEXT T
 0802? PRINT 
 0805? IF I$="NO" THEN 8200   
    0806? PRINT"WOULD YOU LIKE ANY PARTICULAR PRINTOUT?"  
    0807? INPUT P$      
   0810? IF P$="NO" THEN 8200   
    0812? PRINT"WHICH ONE?"      
    0815? INPUT T       
   0816? W7=T  
 0817? GOTO 1000     
   0820? PRINT "SEE CASH FLOW?";
    0820? INPUT Y$      
   0820? IF Y$="NO" THEN 8700   
    0850? PRINT 
 0850? PRINT 
 0851? PRINT"       LEASE      CONV.DEBT    TOTAL"     
    0852? PRINT"       A.T.C.F.   A.T.C.F.    A.T.C.F."   
    0852? PRINT 
 0855? X=0   
 0856? PRINT USING 8620 ,X,E*(-1),V*(-1),(E+V)*(-1)    
    0860? FOR X=1 TO Q  
   0861? PRINT USING 8620 ,X,M(X,12),C(X,K8),M(X,12)+C(X,K8)     
 0862? : ### ######### ######### #########    
   0863? U(1)=U(1)+M(X,12)      
    0863? U(2)=U(2)+C(X,K8)      
    0864? U(3)=U(3)+M(X,12)+C(X,K8)      
 0865? NEXT X
 0866? PRINT 
 0866? PRINT USING 8670 ,U(1),U(2),U(3)       
   0867? :TOTAL######### ######### #########    
   0870? PRINT"DO YOU WANT A YIELD PROOF ON TOTAL ATCF?" 
    0871? INPUT Y$      
   0871? IF Y$="NO" THEN 9000   
    0872? PRINT "    ***** THE YIELD ON THE UNRECOVERED INVESTMENT *****"  
  0872? PRINT 
 0872? PRINT USING 8726,O1    
    0872? : PRD     CASH      INTEREST    PRINCIPAL   BALANCE     SINKING 
   0872? PRINT USING 8730 ,R*100,U5*100 
 0873? :       AVAILABLE   AT ###.###%  PAYMENT   REMAINING   RFUND #.###%      
    0873? PRINT USING 8734 ,R*100*Q6     
 0873? : YEARLY ###.###%      
    0873? PRINT 
 0873? PRINT 
 0874? FOR Y=1 TO B9 
   0874? PRINT USING 8748 ,F1,N(Y,1),N(Y,2),N(Y,3),N(Y,4),N(Y,5) 
 0874? F1=F1+1       
   0874? NEXT Y
 0874? : ### ########.## ########.## ########.## ########.## ########.##
  0900? PRINT"CONSORTIUM EARNINGS STATEMENT?"  
   0901? INPUT E$      
   0901? IF E$="NO" THEN 9700   
    0902? PRINT" % RESIDUAL (15%=.15)?"  
 0902? INPUT R6      
   0903? FOR X=0 TO Q1 
   0903? Y5=Y5+(Q1-X)  
   0903? NEXT X
 0903? R6=.15
 0903? N9=S(W7,1)-(B6+B8-(R6*(B6+B8)))
 0904? FOR X=0 TO (Q1-1)      
    0904? E(X+1,1)=((Q1-X)/Y5)*N9
    0904? E(X+1,2)=Y(X+1,4)      
    0904? E(X+1,3)=Y(X+1,5)      
    0904? E(X+1,4)=Y(X+1,6)      
    0905? E(X+1,5)=E(X+1,1)-E(X+1,2)-E(X+1,3)-E(X+1,4)    
    0905? E(X+1,6)=E(X+1,5)*(1-T5)       
 0905? E(X+1,7)=((Q1-X)/Y5)*X5
    0905? E(X+1,8)=Y(X+1,13)*.04 
    0905? E(X+1,9)=E(X+1,6)+E(X+1,7)+E(X+1,8)    
   0906? NEXT X
 0906? PRINT 
 0906? PRINT 
 0906? PRINT"YR   SYD OF THE  INTEREST  INTEREST      FEES  LOSS / GAIN   AFTER TAX     ITC     EARNINGS  NET AFTER"     
   0906? PRINT"       EARNED     EXPENSE   EXPENSE    BEFORE TAXES   INCOME       ON THE    TAX INCOME"    
    0907? PRINT"       INCOME    ON DEBT   ON BALLOON@ 47%CASHFLOW   REPORTED"     
    0907? PRINT 
 0907? FOR X=1 TO Q1 
   0907? PRINT USING 9078 ,X,E(X,1),E(X,2),E(X,3),E(X,4),E(X,5),E(X,6),E(X,7),E(X,8),E(X,9)       
   0907? :##     ##########  ########  #########     #####   #########  #########  ######## ######### ##########   
 0908? NEXT X
 0910? DATA "YES",8000000,"NEW",22,.53,25,4,.0825,0,960000,.0825,.04,0
    0911? DATA "YES",0,"NO",.25,.27,.28,.29,.045,.05,.055,300000,400000
 0912? DATA .034,1,4,12,16,20,24
  0970? PRINT "WRITING FILES "
0971? FOR I=1TOU1
 0971? PRINT #1,U1(I)
   0972? NEXT I
 0973? FOR I=1TOU2
 0973? PRINT #2,U2(I)
   0974? NEXT I
 0975? FOR I=1TOU4
 0975? PRINT #4,U4(I)
   0976? NEXT I
 0977? FOR I=1TOU6
 0977? PRINT #6,U6(I)
   0978? NEXT I
 0979? PRINT U1,U2,U5,U6
0980? END
    