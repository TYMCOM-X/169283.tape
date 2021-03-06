0001?	      COMMON K1,K2,K3,K4,K5,K6,X1,X2,X3,X4,X5,X6,A1,A2,A3,A4,A5,
   10.0?	     +A6,A7,A8,A9,A10,T,U,V,W
   010.?	      DOUBLE PRECISION A2,A3,A4,X2,X3,X4
  10.1?	        REAL X5,X6,A5,A6,A7,A8,A9,A10,T(20),U(20),V(20),W(20)
 10.1?	      INTEGER A1,X1,K1,K2,K3,K4,K5,K6
010.?	      CALL IFILE(21,'SALESD.ATF')
    10.2?	      CALL OFILE(22,'SALESR.EPF')
    010.?	      K1=1
  10.3?	      K3=55
 010.?	      K4=66
 10.4?	      K2=K4
 010.?	      DO 10 I=1,3
10.5?	      T(I)=0
010.?	      U(I)=0
10.6?	      V(I)=0
010.?	 10   W(I)=0
10.7?	      READ(21,2130,END=2900)A1,A2,A3,A4,A5,A6,A7
    010.?	C ANY TRANSFORMATIONS REQUIRED
  10.8?	 2130 FORMAT(I2,A10,A10,A10,3(F7.2))
 010.?	      CALL PAGE
  10.9?	      CALL LINE
  0001?	 2300 X1=A1
 11.0?	      X2=A2
 011.?	      X3=A3
 11.1?	      X4=A4
 011.?	      READ(21,2130,END=2900)A1,A2,A3,A4,A5,A6,A7
    11.2?	C ANY TRANSFORMATIONS REQUIRED
  011.?	C TESTING
   11.3?	      IF (X1.NE.A1) GO TO 2370
  011.?	      IF (X2.NE.A2) GO TO 2360
  11.4?	      IF (X3.NE.A3) GO TO 2360
  011.?	      IF (X4.NE.A4) GO TO 2350
  11.5?	 2340 CALL LINE
  011.?	      GO TO 2300
 11.6?	 2350 CALL SUB1
  011.?	      CALL LINE
  11.7?	      GO TO 2300
 011.?	 2360 CALL SUB1
  11.8?	      CALL SUB2
  011.?	      CALL LINE
  11.9?	      GO TO 2300
 0001?	 2370 CALL SUB1
  12.0?	      CALL SUB2
  012.?	      CALL SUB3
  12.1?	      CALL LINE
  012.?	      GO TO 2300
 12.2?	 2900 CALL SUB1
  012.?	      CALL SUB2
  12.3?	      CALL SUB3
  012.?	      WRITE(22,2170)(W(I),I=1,3)
12.4?	 2170 FORMAT(1X///25H      ****GRAND TOTAL****,16X,2(F8.2,3X),F8.2
 12.4?	     +/)
    012.?	      END FILE 21
12.5?	      END FILE 22
012.?	      END
   12.6?	      SUBROUTINE LINE
 012.?	      COMMON K1,K2,K3,K4,K5,K6,X1,X2,X3,X4,X5,X6,A1,A2,A3,A4,A5,
   12.7?	     +A6,A7,A8,A9,A10,T,U,V,W
   012.?	      INTEGER A1,X1,K1,K2,K3,K4,K5,K6
12.8?	        REAL X5,X6,A5,A6,A7,A8,A9,A10,T(20),U(20),V(20),W(20)
 12.8?	      DOUBLE PRECISION A2,A3,A4,X2,X3,X4
  012.?	      WRITE(22,2140)A1,A2,A3,A4,A5,A6,A7
  12.9?	 2140 FORMAT(1X,I2,4X,A10,A10,2X,A10,3(3X,F8.2))
    0001?	      K2=K2+1
    13.0?	      T(1)=T(1)+A5
    013.?	      T(2)=T(2)+A6
    13.1?	      T(3)=T(3)+A7
    013.?	      IF (K2.GT.K3) CALL PAGE
   13.2?	      RETURN
013.?	      END
   13.3?	      SUBROUTINE SUB1
 013.?	      COMMON K1,K2,K3,K4,K5,K6,X1,X2,X3,X4,X5,X6,A1,A2,A3,A4,A5,
   13.4?	     +A6,A7,A8,A9,A10,T,U,V,W
   13.4?	        INTEGER A1,X1,K1,K2,K3,K4,K5,K6
   13.4?	        REAL X5,X6,A5,A6,A7,A8,A9,A10,T(20),U(20),V(20),W(20)
 13.4?	        DOUBLE PRECISION A2,A3,A4,X2,X3,X4
013.?	      WRITE(22,2150)(T(I),I=1,3)
13.5?	 2150 FORMAT(1X,20H      *MODEL*       ,21X,2(F8.2,3X),F8.2/)
 013.?	      K2=K2+2
    13.6?	      DO 11 I=1,3
013.?	      U(I)=U(I)+T(I)
  13.7?	 11   T(I)=0
013.?	      IF (K2.GT.K3) CALL PAGE
   13.8?	      RETURN
013.?	      END
   13.9?	      SUBROUTINE SUB2
 0001?	      COMMON K1,K2,K3,K4,K5,K6,X1,X2,X3,X4,X5,X6,A1,A2,A3,A4,A5,
   14.0?	     +A6,A7,A8,A9,A10,T,U,V,W
   14.1?	        INTEGER A1,X1,K1,K2,K3,K4,K5,K6
   14.1?	        REAL X5,X6,A5,A6,A7,A8,A9,A10,T(20),U(20),V(20),W(20)
 14.1?	        DOUBLE PRECISION A2,A3,A4,X2,X3,X4
14.1?	       WRITE(22,2160)(U(I),I=1,3)
    14.1?	 2160 FORMAT(1X,20H      **ITEM**      ,21X,2(F8.2,3X),F8.2/)
 014.?	      K2=K2+2
    14.2?	      DO 12 I=1,3
014.?	      V(I)=V(I)+U(I)
  14.3?	 12   U(I)=0
014.?	      IF (K2.GT.K3)CALL PAGE
    14.4?	      RETURN
014.?	      END
   14.5?	      SUBROUTINE SUB3
 014.?	      COMMON K1,K2,K3,K4,K5,K6,X1,X2,X3,X4,X5,X6,A1,A2,A3,A4,A5,
   14.6?	     +A6,A7,A8,A9,A10,T,U,V,W
   14.6?	        INTEGER A1,X1,K1,K2,K3,K4,K5,K6
   14.6?	        REAL X5,X6,A5,A6,A7,A8,A9,A10,T(20),U(20),V(20),W(20)
 14.6?	        DOUBLE PRECISION A2,A3,A4,X2,X3,X4
014.?	      WRITE(22,2170)(V(I),I=1,3)
14.7?	 2170 FORMAT(1X,20H      ***GROUP***   ,21X,2(F8.2,3X),F8.2/)
 014.?	      K2=K2+2
    14.8?	      DO 13 I=1,3
014.?	      W(I)=W(I)+V(I)
  14.9?	 13   V(I)=0
0001?	      IF (K2.GT.K3) CALL PAGE
   15.0?	      RETURN
015.?	      END
   15.1?	      SUBROUTINE PAGE
 015.?	      COMMON K1,K2,K3,K4
   15.2?	        INTEGER K1,K2,K3,K4
15.2?	        IF (K2+1.GT.K4)GO TO 9903
    015.?	      DO 9900 I=K2+1,K4
    15.3?	      WRITE(22,9902)
  015.?	 9902 FORMAT(1X)
 15.4?	 9900 CONTINUE
   015.?	9903  WRITE(22,9901)K1
15.5?	 9901 FORMAT(1X/30X,5HPAGE I2///71HGROUP        ITEM
015.?	     +              MODEL        AREA1      AREA2      AREA3//)
    15.6?	      K2=7
  015.?	      K1=K1+1
    15.7?	      RETURN
015.?	      END
   