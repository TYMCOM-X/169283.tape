C
C
      DIMENSION Q(150),AMTLCT(150),SS(150),SR(150),AS(150),AR(150),
     &LINE(150),BMTLCT(150),USL(150),UAL(150)
C
  15  TYPE 1
C
C       NOTE: INQUIRY NO ARE INPUT IN BLOCKS OF 100
C
  1   FORMAT(3(/),' TYPE INQUIRY NO.',/,2X,'FROM:',2X,$)
      ACCEPT 8,N
      TYPE 25
  25  FORMAT(/,4X,'TO:',2X,$)
      ACCEPT 8,M
  8   FORMAT(I3)
      TYPE 30,N,M
  30  FORMAT('N = ',I3,',','M = ',I3)
      KOUNT=0
      M2=(M-N)+1
      TYPE 7
  7   FORMAT(/,'ENTER DATA:')
      DO 9 I=1,M2
      ACCEPT 3,Q(I),AMTLCT(I),SS(I),SR(I),AS(I),AR(I)
      KOUNT=KOUNT+1
      IF(KOUNT .EQ. M2) GO TO 10
  9   CONTINUE
  10  TYPE 5, N,M
  5   FORMAT(3(/),43X,'INQUIRY NO S FROM',I3,2X,'TO',I3)
      TYPE 11
  11  FORMAT(3(/),5X,'MATERIAL COST',5X,'UNIT SHOP LABOR',5X,
     &'UNIT ASSY LABOR',4X,'INQUIRY NO.')
      KLINE=0
  3   FORMAT(6F10.3)
      M3 = N-1
      DO 4 I=1,M2
      BMTLCT(I)=AMTLCT(I)/Q(I)
       USL(I)=(SS(I)+SR(I))/Q(I)
      KLINE=KLINE+1
       UAL(I)=(AS(I)+AR(I))/Q(I)
      LINE(I) = KLINE+M3
      IF(KLINE .EQ. M2) GO TO 12
  4    CONTINUE
  12  TYPE 6,(BMTLCT(K),USL(K),UAL(K),LINE(K),K=1,M2)
  6   FORMAT(7X,F8.3,12X,F7.3,12X,F7.3,10X,I4,//)
      LETN=2HNO
      TYPE 13
  13  FORMAT(3(/),'NEXT?',2X,$)
      ACCEPT 14,INFO
  14  FORMAT(A3)
      IF(LETN.EQ.INFO) STOP
      GO TO 15
      END
   