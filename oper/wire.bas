0001� PRINT "ENTER FROM-TO FILE NAME";
0002� INPUT F1$
   0003� PRINT "ENTER MASTER DEVICE FILE NAME";
    0004� INPUT F2$
   0005� PRINT "ENTER COLOR PRIORITY FILE NAME";
   0006� INPUT F3$
   0007� FILE #2,F2$
 0008� IF END #2 THEN 220
    0009� X0=X0+1
0010� DIM D$(200)
 0011� DIM L0(200)
 0012� DIM L1(200)
 0013� DIM L6(5)
   0014� INPUT #2:D$(X0),L0$,L1$
    0015� D$(X0)=LEFT$(D$(X0)+" ",2)
 0016� L0(X0)=VAL(L0$)
  0017� L1(X0)=VAL(L1$)
  0018� IF D$(X0)<>"@ " THEN 80
    0019� L5=L5+1
0020� L6(L5)=X0
   0021� GO TO 80
    0022� FILE #3,F3$
 0023� IF END #3 THEN 300
    0024� X2=X2+1
0025� DIM C$(50)
  0026� DIM L2(50)
  0027� INPUT #3:C$(X2)
  0028� C$(X2)=LEFT$(C$(X2)+" ",2)
 0029� GO TO 230
   0030� FOR X=X2+1 TO X2+10
   0031� READ C$(X)
  0032� NEXT X
 0033� C4=X2
  0034� X2=X2+10
    0035� DATA X0,X1,X2,X3,X4,X5,X6,X7,X8,X9
   0036� FILE #1,F1$
 0037� IF END #1 THEN 1050
   0038� X4=X4+1
0039� IF END #1 THEN 780
    0040� INPUT #1:S$,W8$,W9$
   0041� CHANGE W8$ TO W8
 0042� CHANGE W9$ TO W9
 0043� L=FNA(W8(2))
0044� D1$=LEFT$(LEFT$(W8$,L)+"  ",2)
  0045� DIM P1(250)
 0046� P1(X4)=VAL(MID$(W8$,L+1))
  0047� L=FNA(W9(2))
0048� D2$=LEFT$(LEFT$(W9$,L)+"  ",2)
  0049� H$=""
  0050� IF W9(W9(0))<>42 THEN 530
  0051� H$="*"
 0052� W9$=LEFT$(W9$,W9(0)-1)
0053� K=FNN(W9(L+2))
   0054� DIM P2(250)
 0055� P2(X4)=VAL(MID$(W9$,L+1,K))
0056� A$=MID$(W9$,L+K+1)
    0057� IF LEN(A$)<3 THEN 610
 0058� PRINT USING 590,S
0059� :SOMETHING IS WRONG IN LINE 'RRRR.
   0060� E=E+1
  0061� A$=LEFT$(A$+"  ",2)
   0062� DEF FNA(X)
  0063� IF X<65 THEN 690
 0064� IF X>122 THEN 690
0065� IF X>96 THEN 670
 0066� IF X>90 THEN 690
 0067� FNA=2
  0068� GO TO 700
   0069� FNA=1
  0070� FNEND
  0071� DEF FNN(X)
  0072� IF X<48 THEN 760
 0073� IF X>57 THEN 760
 0074� FNN=2
  0075� GO TO 770
   0076� FNN=1
  0077� FNEND
  0078� D3$=D1$
0079� GOSUB 1080
  0080� DIM D1(250)
 0081� D1(X4)=X
    0082� D3$=D2$
0083� GOSUB 1080
  0084� DIM D2(250)
 0085� D2(X4)=X
    0086� IF A$="  " THEN 980
   0087� FOR X=1 TO X2
    0088� IF A$=C$(X) THEN 940
  0089� NEXT X
 0090� E=E+1
  0091� PRINT USING 920,S,A$
  0092� :LINE NUMBER 'RRRR CONTAINS A COLOR 'R NOT IN COLOR LIST
 0093� DIM A(250)
  0094� A(X4)=X
0095� X3=X3+1
0096� DIM A1(25)
  0097� A1(X3)=X4
   0098� IF H$="" THEN 370
0099� DIM H(250)
  0100� H(X4)=1
0101� X7=X7+1
0102� DIM H1(250)
 0103� H1(X7)=X4
   0104� GO TO 370
   0105� IF E=0 THEN 1150
 0106� PRINT "RUN TERMINATED BECAUSE OF ERRORS IN INPUT"
   0107� STOP
   0108� FOR X=1 TO X0
    0109� IF D3$=D$(X) THEN 1140
0110� NEXT X
 0111� E=E+1
  0112� PRINT USING 1130,S,D3$
0113� :LINE NUMBER 'RRRR CONTAINS DEVICE 'R NOT IN DEVICE LIST
 0114� RETURN
 0115� FOR X=1 TO X4
    0116� D3=D1(X)
    0117� P3=P1(X)
    0118� GOSUB 1250
  0119� D3=D2(X)
    0120� P3=P2(X)
    0121� GOSUB 1250
  0122� NEXT X
 0123� GO TO 1320
  0124� DIM P(200,50)
    0125� IF P(D3,P3)=0 THEN 1300
    0126� X6=X6+1
0127� DIM L(200,2)
0128� L(X6,1)=P(D3,P3)
 0129� L(X6,2)=X
   0130� P(D3,P3)=X
  0131� RETURN
 0132� FOR X=1 TO X6
    0133� FOR W=1 TO 2
0134� DIM F(250)
  0135� IF F(L(X,W))=0 THEN 1370
   0136� IF L(X,3-W)>=F(L(X,W)) THEN 1380
0137� F(L(X,W))=L(X,3-W)
    0138� NEXT W
 0139� NEXT X
 0140� E=0
    0141� FOR X=1 TO X4
    0142� IF F(X)=0 THEN 1460
   0143� IF F(F(X))>=F(X) THEN 1460
 0144� F(X)=F(F(X))
0145� E=E+1
  0146� NEXT X
 0147� IF E>0 THEN 1400
 0148� X=0
    0149� GO TO 1540
  0150� FOR Y=X+1 TO X4
  0151� IF F(Y)=1000 THEN 1720
0152� IF F(Y)<>0 THEN 1570
  0153� GOSUB 1670
  0154� X8=X8+1
0155� E(X8)=1000
  0156� GO TO 1500
  0157� Z=X
    0158� V=Y
    0159� GOSUB 1670
  0160� FOR W=Z+1 TO X4
  0161� IF F(W)=V THEN 1650
   0162� Z=W
    0163� NEXT W
 0164� GO TO 1540
  0165� Y=W
    0166� GO TO 1590
  0167� X8=X8+1
0168� DIM E(450)
  0169� E(X8)=Y
0170� F(Y)=1000
   0171� RETURN
 0172� X=Y
    0173� NEXT Y
 0174� FOR X=1 TO 40
    0175� FOR Y=1 TO 40
    0176� P(X,Y)=0
    0177� NEXT Y
 0178� NEXT X
 0179� FOR X=1 TO X2
    0180� DIM C(50)
   0181� C(X)=X
 0182� NEXT X
 0183� FOR X=1 TO X7
    0184� F=H1(X)
0185� FOR Y=1 TO X8
    0186� IF E(Y)=F THEN 1880
   0187� NEXT Y
 0188� IF E(Y-1)=1000 THEN 1910
   0189� Y=Y-1
  0190� GO TO 1880
  0191� H(E(Y))=1
   0192� Y=Y+1
  0193� IF E(Y)<>1000 THEN 1910
    0194� NEXT X
 0195� FOR X=1 TO X3
    0196� F=A1(X)
0197� C=A(F)
 0198� C(C)=0
 0199� FOR Y=1 TO X8
    0200� IF E(Y)=F THEN 2020
   0201� NEXT Y
 0202� IF E(Y-1)=1000 THEN 2180
   0203� Y=Y-1
  0204� GO TO 2020
  0205� IF H(E(Y))=1 THEN 2090
0206� P(D1(E(Y)),P1(E(Y)))=C
0207� P(D2(E(Y)),P2(E(Y)))=C
0208� GO TO 2110
  0209� P(D1(E(Y)),P1(E(Y)))=C+1000
0210� P(D2(E(Y)),P2(E(Y)))=C+1000
0211� IF F(E(Y))=1000 THEN 2140
  0212� PRINT "CONFLICTING COLOR ASSIGNMENT DETECTED"
  0213� GO TO 1060
  0214� F(E(Y))=C
   0215� Y=Y+1
  0216� IF E(Y)<>1000 THEN 2050
    0217� RETURN
 0218� GOSUB 2050
  0219� NEXT X
 0220� Y=2
    0221� GOSUB 2300
  0222� FOR X=2 TO X8
    0223� IF E(X)=1000 THEN 2430
0224� IF F(E(X))<>1000 THEN 2560
 0225� D=D1(E(X))
  0226� GOSUB 2350
  0227� D=D2(E(X))
  0228� GOSUB 2350
  0229� GO TO 2560
  0230� FOR Z=1 TO X2
    0231� DIM C1(200)
 0232� C1(Z)=C(Z)
  0233� NEXT Z
 0234� RETURN
 0235� FOR Z=1 TO 40
    0236� IF P(D,Z)=O THEN 2410
 0237� IF P(D,Z)<1000 THEN 2400
   0238� C1(P(D,Z)-1000)=0
0239� GO TO 2410
  0240� C1(P(D,Z))=0
0241� NEXT Z
 0242� RETURN
 0243� IF F(E(Y))<>1000 THEN 2550
 0244� FOR C=1 TO X2
    0245� IF C1(C)<>0 THEN 2510
 0246� NEXT C
 0247� PRINT USING 2480,E(Y)
 0248� :NOT ENOUGH COLORS FOR ###TH LINE
    0249� PRINT "RUN TERMINATED"
0250� STOP
   0251� IF C<=C4 THEN 2530
    0252� C5=1
   0253� GOSUB 2050
  0254� GOSUB 2300
  0255� Y=X+1
  0256� NEXT X
 0257� DIM E1(900,2)
    0258� FOR X=1 TO X8
    0259� X9=X9+1
0260� IF E(X)<>1000 THEN 2630
    0261� E1(X9,1)=1000
    0262� GO TO 2680
  0263� E1(X9,1)=D1(E(X))
0264� E1(X9,2)=P1(E(X))
0265� X9=X9+1
0266� E1(X9,1)=D2(E(X))
0267� E1(X9,2)=P2(E(X))
0268� NEXT X
 0269� Y=2
    0270� FOR X=Y TO X9
    0271� IF E1(X+1,1)=1000 THEN 2740
0272� NEXT X
 0273� GO TO 2870
  0274� FOR Z=Y TO X-1
   0275� IF E1(Z,1)<=E1(Z+1,1) THEN 2830
 0276� T=E1(Z,1)
   0277� E1(Z,1)=E1(Z+1,1)
0278� E1(Z+1,1)=T
 0279� T=E1(Z,2)
   0280� E1(Z,2)=E1(Z+1,2)
0281� E1(Z+1,2)=T
 0282� GO TO 2740
  0283� NEXT Z
 0284� Y=X+2
  0285� GO TO 2700
  0286� DIM E2(700,2)
    0287� Y1=1
   0288� E2(1,1)=1000
0289� FOR X=2 TO X9
    0290� IF E1(X,1)<>O THEN 2930
    0291� IF E1(X,2)<>O9 THEN 2940
   0292� GO TO 2980
  0293� O=E1(X,1)
   0294� O9=E1(X,2)
  0295� Y1=Y1+1
0296� E2(Y1,1)=E1(X,1)
 0297� E2(Y1,2)=E1(X,2)
 0298� NEXT X
 0299� DIM W1(200,2)
    0300� Y1=Y1-1
0301� FOR X=1 TO Y1
    0302� IF E2(X,1)<>1000 THEN 3120
 0303� IF P(E2(X+1,1),E2(X+1,2))<1000 THEN 3090
  0304� Y3=Y3+1
0305� DIM W2(50,2)
0306� W2(Y3,1)=X+1
0307� W2(Y3,2)=P(E2(X+1,1),E2(X+1,2))-1000
 0308� GO TO 3120
  0309� Y2=Y2+1
0310� W1(Y2,1)=X+1
0311� W1(Y2,2)=P(E2(X+1,1),E2(X+1,2))
 0312� NEXT X
 0313� FOR X=1 TO Y2-1
  0314� FOR Y=X+1 TO Y2
  0315� IF W1(X,2)<W1(Y,2) THEN 3240
    0316� IF W1(X,2)>W1(Y,2) THEN 3180
    0317� IF W1(X,1)<=W1(Y,1) THEN 3240
   0318� T=W1(X,1)
   0319� W1(X,1)=W1(Y,1)
  0320� W1(Y,1)=T
   0321� T=W1(X,2)
   0322� W1(X,2)=W1(Y,2)
  0323� W1(Y,2)=T
   0324� NEXT Y
 0325� NEXT X
 0326� FOR X=1 TO Y3-1
  0327� FOR Y=X+1 TO Y3
  0328� IF W2(X,2)<W2(Y,2) THEN 3370
    0329� IF W2(X,2)>W2(Y,2) THEN 3310
    0330� IF W2(X,1)<=W2(Y,1) THEN 3370
   0331� T=W2(X,1)
   0332� W2(X,1)=W2(Y,1)
  0333� W2(Y,1)=T
   0334� T=W2(X,2)
   0335� W2(X,2)=W2(Y,2)
  0336� W2(Y,2)=T
   0337� NEXT Y
 0338� NEXT X
 0339� S$=""
  0340� L=0
    0341� O=0
    0342� Z=0
    0343� FOR X=1 TO Y2
    0344� IF W1(X,2)=O THEN 3500
0345� O=W1(X,2)
   0346� GOSUB 4410
  0347� GOSUB 4410
  0348� B$=C$(W1(X,2))+"//"
   0349� GOSUB 4320
  0350� L7=0
   0351� FOR Y=W1(X,1) TO 1000
 0352� IF E2(Y,1)=1000 THEN 3800
  0353� IF L7<>0 THEN 3620
    0354� L7=1
   0355� GOSUB 3580
  0356� L2(O)=L2(O)+5+L4
 0357� GO TO 3730
  0358� L9=E2(Y,1)
  0359� L3=L0(L9)
   0360� L4=L1(L9)
   0361� RETURN
 0362� FOR L8=1 TO L5
   0363� IF L6(L8)>L9 THEN 3660
0364� NEXT L8
0365� GO TO 3710
  0366� IF L6(L8)>E2(Y,1) THEN 3710
0367� IF L3<=L0(L6(L8)) THEN 3710
0368� L2(O)=L2(O)+2*(L1(E2(Y,1))-L0(L6(L8)))+L3+L0(E2(Y,1))
    0369� GOSUB 3580
  0370� GO TO 3730
  0371� L2(O)=L2(O)+L0(E2(Y,1))-L3+2*L1(E2(Y,1))
  0372� GOSUB 3580
  0373� T1=INT(E2(Y,2)/10)
    0374� T2=E2(Y,2)-10*T1
 0375� IF T1<>0 THEN 3770
    0376� T1=-16
 0377� B$="("+D$(E2(Y,1))+CHR$(48+T1)+CHR$(48+T2)+")-"
0378� GOSUB 4320
  0379� NEXT Y
 0380� S$=LEFT$(S$,L-1)
 0381� L=L-1
  0382� B$="//"
0383� GOSUB 4320
  0384� L2(O)=L2(O)-L4
   0385� NEXT X
 0386� GOSUB 4410
  0387� PRINT "============================================="
    0388� S$=""
  0389� L=0
    0390� O=0
    0391� FOR X=1 TO Y3
    0392� IF W2(X,2)=O THEN 3980
0393� O=W2(X,2)
   0394� GOSUB 4410
  0395� GOSUB 4410
  0396� B$=C$(W2(X,2))+"//"
   0397� GOSUB 4320
  0398� L7=0
   0399� FOR Y=W2(X,1) TO 1000
 0400� IF E2(Y,1)=1000 THEN 4240
  0401� IF L7<>0 THEN 4060
    0402� L7=1
   0403� GOSUB 3580
  0404� L2(O)=L2(O)+5+L4
 0405� GO TO 4170
  0406� FOR L8=1 TO L5
   0407� IF L6(L8)>L9 THEN 4100
0408� NEXT L8
0409� GO TO 4150
  0410� IF L6(L8)>E2(Y,1) THEN 4150
0411� IF L3<=L0(L6(L8)) THEN 4150
0412� L2(O)=L2(O)+2*(L1(E2(Y,1))-L0(L6(L8)))+L3+L0(E2(Y,1))
    0413� GOSUB 3580
  0414� GO TO 4170
  0415� L2(O)=L2(O)+L0(E2(Y,1))-L3+2*L1(E2(Y,1))
  0416� GOSUB 3580
  0417� T1=INT(E2(Y,2)/10)
    0418� T2=E2(Y,2)-10*T1
 0419� IF T1<>0 THEN 4210
    0420� T1=-16
 0421� B$="("+D$(E2(Y,1))+CHR$(48+T1)+CHR$(48+T2)+")-"
0422� GOSUB 4320
  0423� NEXT Y
 0424� S$=LEFT$(S$,L-1)
 0425� L=L-1
  0426� B$="//"
0427� GOSUB 4320
  0428� L2(O)=L2(O)-L4
   0429� NEXT X
 0430� GOSUB 4410
  0431� GO TO 4450
  0432� L1=LEN(B$)
  0433� IF L+L1<=70 THEN 4380
 0434� PRINT S$
    0435� L=L1
   0436� S$=B$
  0437� RETURN
 0438� S$=S$+B$
    0439� L=L+L1
 0440� RETURN
 0441� PRINT S$
    0442� S$=""
  0443� L=0
    0444� RETURN
 0445� PRINT
  0446� PRINT USING 4470,Y2+Y3
0447� :TOTAL NUMBER OF WIRES IS ###
   0448� IF C5=0 THEN 4530
0449� PRINT "******   NOTE   ******"
  0450� PRINT "COLORS X0, X1, X2, ... ARE DUMMY COLORS USED BY THE"
   0451� PRINT "PROGRAM WHEN NOT ENOUGH COLORS WERE PROVIDED.  PLEASE"
 0452� PRINT "ESTABLISH ACTUAL COLORS TO REPLACE THEM."
    0453� DIM F1(500,4)
    0454� FOR X=1 TO Y1
    0455� IF E2(X,1)=1000 THEN 4670
  0456� Y4=Y4+1
0457� F1(Y4,1)=E2(X-1,1)
    0458� F1(Y4,2)=E2(X-1,2)
    0459� F1(Y4,3)=E2(X,1)
 0460� F1(Y4,4)=E2(X,2)
 0461� Y4=Y4+1
0462� F1(Y4,1)=E2(X,1)
 0463� F1(Y4,2)=E2(X,2)
 0464� F1(Y4,3)=E2(X-1,1)
    0465� F1(Y4,4)=E2(X-1,2)
    0466� GO TO 4680
  0467� X=X+1
  0468� NEXT X
 0469� FOR X=1 TO Y4-1
  0470� FOR Y=X+1 TO Y4
  0471� IF F1(X,1)<F1(Y,1) THEN 4830
    0472� IF F1(X,1)>F1(Y,1) THEN 4780
    0473� IF F1(X,2)<F1(Y,2) THEN 4830
    0474� IF F1(X,2)>F1(Y,2) THEN 4780
    0475� IF F1(X,3)<F1(Y,3) THEN 4830
    0476� IF F1(X,3)>F1(Y,3) THEN 4780
    0477� IF F1(X,4)<=F1(Y,4) THEN 4830
   0478� FOR Z=1 TO 4
0479� T=F1(X,Z)
   0480� F1(X,Z)=F1(Y,Z)
  0481� F1(Y,Z)=T
   0482� NEXT Z
 0483� NEXT Y
 0484� NEXT X
 0485� O=0
    0486� FOR X=1 TO Y4
    0487� IF F1(X,1)=O THEN 4900
0488� O=F1(X,1)
   0489� PRINT
  0490� T1=INT(F1(X,2)/10)
    0491� T2=F1(X,2)-10*T1
 0492� IF T1<>0 THEN 4940
    0493� T1=-16
 0494� T3=INT(F1(X,4)/10)
    0495� T4=F1(X,4)-10*T3
 0496� IF T3<>0 THEN 4980
    0497� T3=-16
 0498� Y=P(F1(X,1),F1(X,2))
  0499� IF Y>1000 THEN 5030
   0500� X$=""
  0501� Y$=C$(Y)
    0502� GO TO 5050
  0503� X$="   *"
   0504� Y$=C$(Y-1000)
    0505� PRINT D$(F1(X,1))+CHR$(48+T1)+CHR$(48+T2)+"--"+D$(F1(X,3))+CHR$(48+T3)+CHR$(48+T4)+"-"+Y$+X$
0506� NEXT X
 0507� PRINT
  0508� PRINT "NOTE:"
    0509� PRINT "* INDICATES WIRE LOCATED IN SECOND HARNESS"
  0510� PRINT
  0511� PRINT "COLOR    LENGTH (INCHES)"
0512� FOR X=1 TO X2
    0513� IF L2(X)=0 THEN 5160
  0514� PRINT C$(X),L2(X)
0515� NEXT X
 0516� END
    