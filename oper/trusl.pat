C... TRUSS/TRUSL
C...	TRUSS  ANALYSIS  RTN
C...	WRITTEN 6/70 -ANDREW  FRANKUS
C	MODIF'D 5/71 - PATRICK  JUDD (EXPAND MODEL SIZE- TRUSL RTN)
C
C***************************
      DOUBLE PRECISION ADATE,JOBNO  
C
C..ADJUST DIMENS: 130_90,75_50, 75_52, 305_210
C                1_1, ALL OTHERS: NEW_OLD*1.5
      DIMENSION ITYP(45),B(6,6),AFJ(6,2),A(6,305),AFX(305),MILL(130),
     1   FA(75),FL(75),KK(75),II(75),SN(75),CN(75),ISUP(45),
     2  X(45),Y(45),Z(305), R(305),DEPTH(75),ISU(45),ITY(45)
      DIMENSION D(130,130),AFK(305,1),C(305,6),RR(305,1),Q(305,1),
     1  ZZ(305,1),CC(305)
      EQUIVALENCE (R(1),RR(1,1)),(Z(1),ZZ(1,1)),(AFX(1),AFK(1,1))     
      EQUIVALENCE (CC(1),C(1,1))        
      BAM=3HTTT     
      CALL IFILE(1,BAM)       
    2 FORMAT(2F)    
    3 FORMAT(3I)    
    7 FORMAT(2X,I2,F10.2,2F10.2)         
   33 FORMAT(2I,1F) 
   50 FORMAT(2I)    
   44 FORMAT(3X,I2,4X,I2,1H-,I2,2X,F8.1,3X,F7.2)      
  121 FORMAT(///)        
  122 FORMAT(///)   
  522 FORMAT(15X,5HJOINT,I4,5X,13H--PIN-SUPPORT)  
  524 FORMAT(15X,5HJOINT,I4,5X,15H--HORIZ. ROLLER)
  525 FORMAT(15X,5HJOINT,I4,5X,14H--VERT. ROLLER) 
 5020 FORMAT(//)   
 5051 FORMAT(2X,'JOINT  X(FT)   Y(FT)') 
 5052 FORMAT(////26X,11H   TRUSS   ,/,29X,5(1H=),//24X, 
     1 16HJOINT LOCATIONS  ////)        
 5053 FORMAT('1  TRUSL PROGRAM'///
     A    /2X,A9,18X,'   TRUSS   '15X,'JOB NO. ',A10/32X,      
     1   5('='),///)        
 5054 FORMAT(///12X,'MEMBER DATA'/1H0,'MEMBER  JOINTS'3X,   
     1 'A(IN**2)   L(FT)')    
 5056 FORMAT(1X,I2,3X,I3,F10.1,F9.1,F11.1,F11.1)  
 5059 FORMAT(3X,I2,2X,2F9.3,F9.3)       
 5061 FORMAT(///11X,12HLOADING DATA //1X,5HJOINT,        
     121H    H(K)      V(K)   )         
 5060 FORMAT(3X,I2,3X,F6.2,2X,F6.2)     
 6066 FORMAT(//11X,13HDISPLACEMENTS///1X,  'JOINT    DX(IN)   DY(IN)')
 6067 FORMAT(11X,13HMEMBER FORCES///) 
 6068 FORMAT(1X,'MEMBER  JOINTS   AXIAL(K) STRESS(KSI)')
 6821 FORMAT(I,3F)  
 6996 FORMAT(A10)    
 7777 FORMAT(I2)    
 7831 FORMAT(10X,50HMAX. NUMBER OF JOINTS 45--MAX.NUMBER OF MEMBERS 52 )        
      CALL DATE(ADATE)        
C****MODULUS OF ELASTICITY IN K.S.I.    
      FE=30000.     
C*******************************        
      NQQ=05
      READ (1,6996) JOBNO     
      WRITE(NQQ,5053)ADATE,JOBNO        
C... SET SIZE RESTRICTIONS
	MAXJNT= 65
	MAXMEM= 75
	K2=4*MAXMEM+2
	K1=2*MAXJNT
C..ROUGHLY K2 IS 4*MAXMEM+2, K1 IS 2*MAXJNT
  850 WRITE(NQQ,5051)         
 1000 READ(1,3) NJ,NE,NS      
      IF((NE.LE.MAXMEM).AND.(NJ.LE.MAXJNT)) GO TO 7832    
      WRITE(NQQ,7831)         
      STOP
 7832 CONTINUE      
      NQE=4*NE      
      NQ=NJ*2       
      DO 1 J=1,NJ   
      N=2*J-1       
      READ(1,2)X(J),Y(J)      
      ITYP(J)=0
      ISUP(J)=0
    1 WRITE(NQQ,5060)J,X(J),Y(J)        
      WRITE(NQQ,5054)         
      DO 111 M=1,NE 
      READ(1,33)II(M),KK(M),FA(M)       
      I=II(M)       
      K=KK(M)       
      FL(M)=((Y(K)-Y(I))**2+(X(K)-X(I))**2)**.5   
      SN(M)=(Y(K)-Y(I))/FL(M) 
      CN(M)=(X(K)-X(I))/FL(M) 
  111 WRITE(NQQ,44)M,I,K,FA(M),FL(M)      
      DO 957 I=1,NS 
  957 READ(1,50)ISU(I),ITY(I) 
      KI=0
      DO 959 I=1,NJ 
      DO 958 J=1,NS 
      IF(ISU(J).NE.I) GO TO 958         
      KI=KI+1       
      ISUP(KI)=ISU(J)         
      ITYP(KI)=ITY(J)         
  958 CONTINUE      
  959 CONTINUE      
      WRITE(NQQ,5052)
      CALL PLOP(X,Y,NJ,MILL,NQQ)  
      WRITE(NQQ,122)
      DO 16 I=1,NQ  
      DO 17 J=1,NQ  
   17 D(I,J)=0.     
      MILL(I)=0     
   16 AFK(I,1)=0    
      IXX=1         
      ISS=1         
      DO 11 I=1,NJ  
      ICOORD=I*2-1  
      JCOORD=ICOORD+1         
      ISX=ITYP (ISS)
      IF(I.EQ.ISUP(ISS)) GO TO 12       
      DO 13 M1=ICOORD,JCOORD  
      MILL(IXX)=M1  
   13 IXX=IXX+1     
      GO TO 11      
   12 GO TO (559,559,559,557,556),ISX   
  557 WRITE(NQQ,524)I         
      GO TO 560     
  556 WRITE(NQQ,525)I         
  560 MILL(IXX)=JCOORD-5+ISX  
      IXX=IXX+1     
      GO TO 14      
  559 WRITE(NQQ,522)I         
   14 ISS=ISS+1     
   11 CONTINUE      
      WRITE(NQQ,5020)         
      DO 6 N=1,NE   
      CALL SETUX(N,SN,CN,II,KK,FE,FI,FL,FA,A,B,NE,NQ,K2)        
      CALL LW(A,B,C,NQ,4,4,2,4,K2,4,4,K2,4)       
      CALL LW(C,A,D,NQ,4,NQ,3,K2,4,4,K2,K1,K1)    
    6 CONTINUE      
      IX=IXX-1      
      NX=0
      DO 88 J=1,IX  
      IXN=MILL(J)   
      DO 88 I=1,IX  
      JXN=MILL(I)   
   88 D(I,J)=D(JXN,IXN)       
      NQ=IX         
      CALL LZ(D,Z,NQ,K1)      
 6789 WRITE(NQQ,5053) ADATE,JOBNO
      WRITE(NQQ,5061)         
      NZ=2*NJ       
      DO 6788 N=1,NZ
 6788 Q(N,1)=0.     
 6877 READ(1,6821)J,AQ1,AQ2   
      IF(J.LE.0) GO TO 6889   
      IF(J.EQ.99) GO TO 1000  
      N=2*J-1       
      Q(N,1)=AQ1    
      Q(N+1,1)=AQ2  
 1025 WRITE(NQQ,7)J,Q(N,1),Q(N+1,1)     
      GO TO 6877
 6889 CONTINUE      
      DO 1888 J=1,IX
      IXN=MILL(J)   
 1888 Q(J,1)=Q(IXN,1)         
      CALL LW(D,Q,RR,NQ,NQ,1,1,K1,K1,K2,1,K2,1)   
      J=1 
      NQ=2*NJ       
      DO 1010 I=1,NQ
  798 IF(MILL(J).NE.I) GO TO 1010       
  797 AFK(I,1)=RR(J,1)        
      J=J+1         
 1010 CONTINUE      
      WRITE(NQQ,6066)         
      DO 1020 I=1,NJ
      M=I*2-1       
      DIS1=AFK(M,1)*12.       
      DIS2=AFK(M+1,1)*12.     
 1020 WRITE(NQQ,5059)I,DIS1,DIS2        
      WRITE(NQQ,121)
      WRITE(NQQ,6067)         
      WRITE(NQQ,6068)         
      DO 1019 N=1,NE
      CALL SETUX(N,SN,CN,II,KK,FE,FI,FL,FA,A,B,NE,NQ,K2)    
      CALL LW(A,AFK,ZZ,4,NQ,1,1,4,K2,K2,1,K2,1)   
      CALL LW(B,ZZ,AFJ,4,4,1,1,4,4,K2,1,4,1)      
      KL=II(N)      
      I=1
      K=2*I-1       
      GO TO (8019,8020),I     
 8019 AXN=CN(N)*AFJ(K,1)+SN(N)*AFJ(K+1,1)         
       AXN=-AXN
      STRESS=AXN/FA(N)        
 8020 WRITE(NQQ,44)N,II(N),KK(N),AXN,STRESS    
      KL=KK(N)
 1019 CONTINUE      
      GO TO 6789    
  185 STOP
      END 
      SUBROUTINE PLOP(X,Y,NJ,MXY,NQQ)   
      DIMENSION X(1),Y(1),MXY(29),FMT(31),PART(2) 
      DATA FMT(1),FMT(31),PART(1)/5H(11X,,5H 1X) ,5HA2,  /  
      DATA PART(2),IBLANK/5HI2,  ,5H     /        
      BIG=X(1)      
      DO 15 I=1,NJ  
      IF(BIG.LT.X(I)) BIG=X(I)
   15 IF(BIG.LT.Y(I)) BIG=Y(I)
      WRITE(NQQ,23) 
    7 FORMAT(1H )   
   23 FORMAT(/////)    
	IXFLAG=0
      XN=25./BIG    
      DO 10 MNJ=1,29
      DO 5 I=1,29   
      MXY(I)=IBLANK 
    5 FMT(I+1)=PART(1)        
      N=30-MNJ      
      IFLAG=0       
      DO 1 J=1,NJ   
      MY=Y(J)*XN+2.5
      IF(MY.NE.N) GO TO 1     
      IFLAG=1       
      IXFLAG=1
      MX=X(J)*XN+2.5
      MXY(MX)=J     
      FMT(MX+1)=PART(2)       
    1 CONTINUE      
      IF(IFLAG)3,3,6
    3  IF(IXFLAG) 10,10,103
  103 WRITE(NQQ,7)  
      GO TO 10      
    6 WRITE(NQQ,FMT)(MXY(J),J=1,29)     
   10 CONTINUE      
      RETURN        
      END 
      SUBROUTINE LZ(D,R,NQ,L1)
      DIMENSION D(L1,L1),R(1),C(120)    
      N1=NQ-1       
      D(1,1)=1./D(1,1)        
C
      DO 10 N=1,N1  
      K=N+1         
      DO 12 I=1,N   
      R(I)=0        
      DO 12 J=1,N   
   12 R(I)=R(I)+D(I,J)*D(J,K) 
      F=0 
      DO 13 I=1,N   
   13 F=F+D(K,I)*R(I)         
      F=-F+D(K,K)   
      D(K,K)=1./F   
      DO 14 I=1,N   
   14 D(I,K)=-R(I)*D(K,K)     
      DO 15 J=1,N   
      C(J)=0        
      DO 15 I=1,N   
   15 C(J)=C(J)+D(K,I)*D(I,J) 
      DO 16 J=1,N   
   16 D(K,J)=-C(J)*D(K,K)     
      DO 17 I=1,N   
      DO 17 J=1,N   
   17 D(I,J)=D(I,J)-R(I)*D(K,J)         
   10 CONTINUE      
C
      RETURN        
      END 
C      MATRIX MULTIPLICATION --       **********************************        
C      IF JJ =2   FIRST MATRIX IS TRANSPOSED      
C       IF JJ=3     PRODUCT NOT INITIALIZED       
      SUBROUTINE LW(A,B,C,M,N,K,JJ,L1,L2,L3,L4,L5,L6)       
      DIMENSION A(L1,L2),B(L3,L4),C(L5,L6)        
      DO 10 I=1,M   
      DO 10 J=1,K   
      GO TO (4,4,3),JJ        
    4 C(I,J)=0      
    3 DO 12 L=1,N   
      GO TO (1,2,1),JJ        
    1 C(I,J)=C(I,J)+A(I,L)*B(L,J)       
      GO TO 12      
    2 C(I,J)=C(I,J)+A(L,I)*B(L,J)       
   12 CONTINUE      
   10 CONTINUE      
      RETURN        
      END 
      SUBROUTINE SETUX(N,SN,CN,II,KK,FE,FI,FL,FA,A,B,NE,NQ,L1)        
      DIMENSION SN(1),CN(1),II(1),KK(1),FL(1),FA(1)         
      DIMENSION A(4,L1),B(4,4)
      DO 6 J=1,4    
      DO 7 I=1,NQ   
    7 A(J,I)=0.     
      DO 6 I=1,4    
    6 B(I,J)=0.     
      L=II(N)*2-1   
      DO 5 K=1,2    
      J=K*2-1       
      A(J,L)=1      
      A(J+1,L+1)=1  
    5 L=KK(N)*2-1   
      K=1 
      F=FA(N)*FE/FL(N)        
      U=SN(N)       
      W=CN(N)       
      B(K,K)=W*W       *F     
      B(K,K+1)=U*W     *F     
      B(K,K+2)=-W*W    *F     
      B(K,K+3)=-W*U    *F     
      K=K+1         
      B(K,K)=U*U   *F         
      B(K,K+1)=-U*W*F         
      B(K,K+2)=-U*U*F         
      K=K+1         
      B(K,K)=W*W*F  
      B(K,K+1)=U*W*F
      K=K+1         
      B(K,K)=U*U*F  
      DO 8 I=1,4    
      DO 8 J=1,4    
      IF(B(I,J))8,9,8         
    9 B(I,J)=B(J,I) 
    8 CONTINUE      
      RETURN        
      END 
 