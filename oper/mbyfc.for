      REAL MFGCST(6),LOTSZ,LEADTM
      DIMENSION TOTFIX(6),TOOL(6),EQUIP(6),TOTINV(6)
   11 FORMAT(V)
      FILENAME MB2
      DIMENSION DEP(6),XFLOOR(6),AMORT(6),XNET(6),XNETAT(6),LIN(99),TOTM
     1AT(6
      )
      DIMENSION AC(6),CC(99),0LBR(99),F(99),V(99),C(25)
      DIMENSION TOTLBR(6),CASHIN(6),TOTVOH(6)
      DIMENSION TOTCST(6),XX(6),TOL(6),EQ(6)
      COMMON A(2)
      DIMENSION DIRMAT(25),CCLBR(25),CRTM(25),VOH(25),
      &OPTM(25),ANNUSE(6),TM(6),TL(6),TV(6),TF(6),FIXOH(25),
      &PURCST(6),PURCOS(6),ANNSAV(6),XROI(6),XPAY(6),
      &AVCASH(3),AINVEN(6)
      ALPHA ANS
      INPUT 701,DESC,DESC1,DESC2,DESC3,DESC4
      PRINT 725
      INPUT,NO
C 
    1 FORMAT (2I5,(F10.2))
    2 FORMAT (I5,18F10.2,I2)
    3 FORMAT(1H ,"USAGE",3X,5(2X,F9.0),//1H ,"MFG COST",
      &5(2X,F9.0),//1H ,"PUR COST",5(2X,F9.0),//1H ,"AN SAV",
      & 2X,5(2X,F9.0))
    4 FORMAT(//1H ,"RETURN ON INVESTMENT",F20.2,"%",///1H ,
      &"AVERAGE ANNUAL CASH INFLOW",6X,F10.2,//)
C  *IO*  5 FORMAT(1H ,"CASH INFLOW - BUY     NOT ACCEPTABLE")
C  *IO*  6 FORMAT (1H ,"ROI - MAKE     NOT ACCEPTABLE")
    7 FORMAT(//1H ,5X,"RECOMMENDATION BASED ON AVERAGE ANNUAL
      &CASH INFLOW",//1H ,22X,"***** MAKE *****",//1H ,"------
      &-------------------------------------------------------",
      &//1H ,)
    8 FORMAT(//1H ,5X,"RECOMMENDATION BASED ON AVERAGE ANNUAL
      &CASH INFLOW",//1H ,22X,"***** BUY *****",//1H ,"------
      &---------------------------------------------------------
      &",//1H ,)
      PRINT 700,DESC,DESC1,DESC2,DESC3,DESC4
      IF(2-NO)43,44,43
   43 READ( 7,      11) (LIN(J),CC(J),XLBR(J),F(J),V(J),J=1,90)
C43 READ("HAMP71",11) (LIN(J),CC(J),XLBR(J),F(J),V(J),J=1,90)
C READ("MB",11)L1,K,(LIN(I),C(I),DIRMAT(I),CRTM(I),OPTM(I),
      READ( 2  ,11)L1,K,(LIN(I),C(I),DIRMAT(I),CRTM(I),OPTM(I),
      &I=1,K),L2,BGTCRD,PURCOS(1)
      DO 901 IJ=1,K
      DO 901 J=1,90
      IF(CC(J)-C(IJ))901,902,901
  902 CCLBR(IJ)=XLBR(J)
      VOH(IJ)=V(J)
      FIXOH(IJ)=F(J)
  901 CONTINUE
      GO TO 55 
C 44 READ("MB1",11)L1,K,(LIN(I),DIRMAT(I),CCLBR(I),CRTM(I),
   44 READ(  3,  11)L1,K,(LIN(I),DIRMAT(I),CCLBR(I),CRTM(I),
      &VOH(I),OPTM(I),FIXOH(I),I=1,K),L2,BGTCRD,PURCOS(1)
   55 CALL COST(DIRMAT,CCLBR,CRTM,VOH,OPTM,BGTCRD,K,TOTMAT,
      &TOTLBR,TOTVOH,TOTFIX,FIXOH,TOTCST,PURCOS)
      IF(3-NO)59,998,59
   59 DO 404 M=1,2
      IF(M-1)15,15,16
      GO TO 850
   16 CONTINUE
      MB2="I2"
  850 CONTINUE
      DO 91 I=1,5
      PURCST(I)=ANNUSE(I)*PURCOS(I)
      MFGCST(I)=ANNUSE(I)*TOTCST(I)
      IF(M-1)85,85,90
   85 ANNSAV(I)=PURCST(I)-MFGCST(I)
      GO TO 91
   90 ANNSAV(I)=MFGCST(I)-PURCST(I)
   91 CONTINUE
   92 AINVEN(1)=ANNUSE(1)*(TOTMAT(1)+(TOTLBR(1)+TOTVOH(1)
      &+TOTFIX(1))/2.)*LEADTM/48.4
      XROI(M)=0.
      DO 72 I=1,5
      IF(I-1)73,73,74
   73 FL=FLOOR*20.
      GO TO 75
   74 FL=0.
   75 IF(M-1)70,70,71
   71 AINVEN(1)=-AINVEN(1)
   70 TOTINV(I)=TOOL(I)+FL+EQUIP(I)+AINVEN(I)
      TOL(I)=TOOL(I)
   72 EQ(I)=EQUIP(I)
      FL=FLOOR*20.
      CALL CASH(ANNSAV,TOTINV,CASHIN,LIFE,TOOL,TAXES,
      &XX,EQUIP,FLOOR,DEP,XFLOOR,AMORT,XNET,XNETAT,PAY)
      CALL ROI (TOTINV,CASHIN,R)
      AC(M)=CASHIN(2)+CASHIN(3)+CASHIN(4)+CASHIN(5)+CASHIN(6)
      XROI(M)=R
      AVCASH(M)=AC(M)/5.
      IF(M-1)162,162,163
  160 CONTINUE
      INPUT 701,ANS
      IF(ANS.EQ."YES") GO TO 440
      GO TO 404
  161 IF(M-1)402,402,410
  410 IF(AVCASH(2)-4999.99)401,401,404
  401 PRINT 5
      GO TO 404
  402 IF(XROI(1)-29.99)403,403,404
  403 PRINT 6
  404 CONTINUE
      PRINT 710
      IF(AVCASH(1)-AVCASH(2)) 407,407,408
  408 PRINT 7
      GO TO 998
  407 PRINT 8
  700 FORMAT(////1H0,5X,"-------------------------------
      &---------------------------------",//1H ,25X,"BLACK
      & AND DECKER",//1H0,26X,"MAKE OR BUY",///1H ,25X,5A4,///
      &1H ,15X,"----------------------------------")
  702 FORMAT(//1H ,5X,"-----------------------------------
      &---------------------------------",//1H ,20X,"*****
      & MAKE *****",//1H ,15X,"SAVINGS AND INVESTMENT",/)
  703 FORMAT(///1H ,"TOOL COST",5(2X,F10.0),//1H ,"EQUIP 
      &CST",5(2X,F10.0),//1H ,"FLOOR SPACE",F10.0,//1H ,"AN I
      &--------------------------------------------------------
      &")
  704 FORMAT(///1H ,15X,"ROI AND CASH INFLOW",/)
  705 FORMAT(//1H ,5X,"-----------------------------------
      &----------------------------",//1H ,15X,"***** BUY *****
      &"//1H ,15X,"SAVINGS AND INVESTEMENT",/)
  710 FORMAT(//1H ,20X,"*************************")
  701 FORMAT(5A4)
  720 FORMAT(//1H ,"AVERAGE ANNUAL?CASH INFLOW",5X,F10.0,//)
  725 FORMAT(/1H ,"ENTER OPTION NO.")
  750 FORMAT(1H ,"MAKE - ROI=",F10.2,"%",10X,"AV CASH 
      &INFLOW=",F10.2,/1H ,"DO YOU WISH TO HAVE THE FIGURES PRINTED,
      & YES OR NO")
  751 FORMAT(1H ,"BUY - ROI=",F10.2,"%",10X,"AV CASH INFLOW=",
      &F10.2,/1H ,"DO YOU WISH  TO HAVE THE FIGURES PRINTED, YES 
      &OR NO")
  440 IF(M-1)1551,1551,1661
      GO TO 1550
 1661 PRINT 705
 1550 PRINT 3,(ANNUSE(II),II=1,5),(MFGCST(II),II=1,5),
      &(PURCST(II),II=1,5),(ANNSAV(II),II=1,5)
      PRINT 703,(TOL(I),I=1,5),(EQ(I),I=1,5),FL,
      &(AINVEN(II),II=1,1),(TOTINV(II),II=1,5)
      PRINT 704
      PRINT 760,(ANNSAV(I),I=1,5),(DEP(I),I=2,6),(XFLOOR(I),I=2,6),
      &(AMORT(I),I=2,6),(XNET(I),I=2,6),(XNETAT(I),I=2,6),
      &(XX(I),I=2,6),(CASHIN(I),I=2,6),(TOTINV(I),I=1,5)
      &,PAY
  760 FORMAT(1H0,"AN SAV",9X,5F10.0,//1H ,"DEPR",11X,
      &5F10.0,//1H ,"FLOOR SPACE",4X,5F10.0,//1H ,"TOOLING",8X,5F10.0,
      &//1H ,"NET SAVINGS",4X,5F10.0,//1H ,"NET AFTER TAXES",5F10.0,
      &//1H ,"DEPR & AMORT",3X,5F10.0,//1H ,"CASH INFLOW",
      &4X,5F10.0,//1H ,"TOTAL INV",6X,5F10.0,//1H0,"PAYBACK PERIOD",
      &5X,F5.2,"YRS")
      PRINT 4, R,AVCASH(M)
      GO TO 161
  998 A(1)=5.3
      CHAIN "MAKEBUY"
  991 STOP
      END
      SUBROUTINE?COST (DIRMAT,CCLBR,CRTM,VOH,OPTM,BGTCRD,
      &K,TM,TL,TV,TF,FIXOH,TUC,PURCOS)
      DIMENSION DIRMAT(25),CCLBR(25),CRTM(25),VOH(25),
      &FIXOH(25),OPTM(25),TM(6),TL(6),TV(6),TF(6),TUC(6)
      &,PURCOS(6)
      TOTMAT=0.
      TOTLBR=0.
      TOTVOH=0.
      TOTFIX=0.
      DO 100 I=1,K
      TOTMAT=TOTMAT+DIRMAT(I)
      TOTLBR=TOTLBR+CCLBR(I)*CRTM(I)
      TOTVOH=TOTVOH+OPTM(I)*VOH(I)
  100 TOTFIX=TOTFIX+OPTM(I)*FIXOH(I)
      TM(1)=TOTMAT
      TL(1)=TOTLBR
      TV(1)=TOTVOH+BGTCRD
      TF(1)=TOTFIX
      TUC(1)=TM(1)+TL(1)+TV(1)+TF(1)
      DO 110 L=2,5
      TM(L)=TM(L-1)*1.04
      TL(L)=TL(L-1)*1.04
      TV(L)=TV(L-1)*1.04
      TF(L)=TF(L-1)*1.04
      PURCOS(L)=PURCOS(L-1)*1.04
  110 TUC(L)=TM(L)+TL(L)+TV(L)+TF(L)
      PRINT 2
      PRINT 1,(TM(L),L=1,5),(TL(L),L=1,5),(TV(L),L=1,5),
      &(TF(L),L=1,5),(TUC(L),L=1,5),(PURCOS(L),L=1,5)
    1 FORMAT(1H ,"TOTAL MATL",5X,5F10.5,//1H ,
      &"TOT DR LBR",5X,5F10.5,//1H ,"TOT VAR OH",5X,
      &5F10.5,//1H ,"FIXED OH",7X,5F?0.5,//1H ,"TOT UNIT COST
      &",2X,5F10.5,//1H0,"PUR PRICE",6X,5F10.5,//1H ,)
    2 FORMAT(/1H ,20X,"MFG TOTAL UNIT COST",//)
      RETURN
      END
      SUBROUTINE CASH (ANNSAV,TOTINV,CASHIN,LIFE,TOOL,TAXES,
      &XX,EQUIP,FLOOR,DEP,XFLOOR,AMORT,XNET,XNETAT,PAY)
      DIMENSION E(6),T(6),ASAV(6)
      DIMENSION ANNSAV(6),AMORT(6),CASHIN(6),DEP(6),
      &XNETAT(6),XX(6),XNET(6)
      DIMENSION XFLOOR(6)
      DIMENSION TOTINV(6),PAYBAK(6)
      DIMENSION TOOL(6),EQUIP(6)
      DO 300 I=2,6
      T(I)=TOOL(I-1)
  300 E(I)=EQUIP(I-1)
      E(1)=0.
      T(1)=0.
      DO 200 I=2,6
      EQUIP(I)=E(I-1)+E(I)
      E(I)=EQUIP(I)
      DEP(I)=.93*EQUIP(I)/12.
      XFLOOR(I)=FLOOR*.4
      TOOL(I)=T(I-1)+T(I)
      T(I)=TOOL(I)
      XLIFE=LIFE
      IF(I-LIFE-1)202,202,201
  202 AMORT(I)=TOOL(I)/XLIFE
      GO TO 203
  201 AMORT(I)=0.
  203 XXI)=DEP(I)+AMORT(I)+XFLOOR(I)
      JJ=5
      ASAV(I)=ANNSAV(I-1)
      XNET(I)=ASAV(I)-XX(I)
      XNETAT(I)=XNET(IQ*(1.-TAXES)
      IF(TOTINV(I-1))10,20,20
   10 CASHIN(I)=XNETAT(I)+XX(I)-TOTINV(I-1)
      GO TO 200
   20 CASHIN(I)=XNETAT(I)+XX(I)
  200 CONTINUE
      PAYBAK(1)=TOTINV(1)
      DO 60 J=2,6
      PAYBAK(J)=PAYBAK(J-1)-CASHIN(J)
      IF (PAYBAK(J))40,40,60
   40 JJ=J-1
      GO TO 50
   60 CONTINUE
   50 O=JJ-1
      PAY=O+PAYBAK(JJ)/CASHIN(JJ+1)
      IF(TOTINV(1))11,11,12
   11 PAY=0.
   12 CONTINUE
      RETURN
      END
 1551 PRINT 702
      SUBROUTINE ROI(TOTINV,CASHIN,XI)
      DIMENSION TOT(6)
      DIMENSION TOTINV(6),CASHIN(6)
      DIMENSION C(6)
      DO 301 L=1,5
  301 TOT(L)=TOTINV(L)
      XI=.3
      DO 15 L=1,5
      IF(TOTINV(L))4,4,15
    4 TOTINV(L)=0.
   15 CONTINUE
  100 SAV=0.
      SAVE=0.
      IF(ABS(XI-1.)-.001)11,11,12
   11 XI=XI-.01
   12 DO 25 L=1,5
      C(L)=CASHIN(L+1)
      SAV=SAV+C(L)*(1./(1.+XI)**L)
   25 SAVE=SAVE+TOTINV(L)*(1./(1.+XI)**(L-1))
      R=SAVE/SAV
      IF(R)200,200 ,250
  250 X=R-?.
   10 IF(ABS(X)-.001)60,50,50
   50 IF(X)51,51,52
   51 IF(.01+X)110,110,120
  110 IF(.05+X)80,80,90
   80 IF(.15+X)65,65,75
   65 XI=XI+.1
      GO TO 100
   75 XI=XI+.05
      GO TO 100
   90 XI=XI+.01
      GO TO 100
  120 XI=XI+.001
      GO TO 100
   52 IF(.01-X)111,111,121
  111 IF(.05-X)81,81,91
   81 IF(.15-X)66,66,76
   66 XI=XI-.09
      GO TO 100
   76 XI=XI-.045
      GO TO 100
   91 XI=XI-.009
      GO TO 100
  121 XI=XI-.0009
      GO TO 100
   60 XI=XI*100.
      GO TO 303
  200 CONTINUE
      XI=0.
  303 DO 302 L=1,5
  302 TOTINV(L)=TOT(L)
  800 RETURN
      END
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      