C     PROGRAM TO DEVELOPE COST
C
C
      DIMENSION Q(150),AMTLCT(150),SS(150),SR(150),AS(150),AR(150),
     &LINE(150),BMTLCT(150),USL(150),UAL(150)
C
      LETN=2HNO
C
      TYPE 33
  33  FORMAT(//,'ARE INSTRUCTIONS REQUIRED?  ',$)
      ACCEPT 14, INFO
      IF(LETN .EQ. INFO) GO TO 36
      TYPE 35
C
  35  FORMAT(//,'DATA INPUT IS SPARE PART COST FROM PROG R28I40
     - QUOTATION DATA SUMMARY.',/,'ALL NUMERICAL INPUTS MUST
     - CONTAIN A DECIMAL. REPLY YES OR NO TO',/,'CONVERSATIONAL
     - QUERIES.','INPUT IS FROM INQUIRY TOTALS & BY INQUIRY NO.',
     -/,'THE MAXIMUM IS 150 AT A TIME.',/,
     -'INPUT MUST BE AS FOLLOWS:',//,
     -7X,'QTY    TOTAL MATL     UNIT     TOTAL       UNIT        TOTAL
     -',/,15X,' PRICE       M/S 01    M/S 05     ASSY 01    ASSY 05',//
     -,'EACH DATA VALUE MUST CONTAIN A DECIMAL AND BE SEPARATED
     - BY A COMMA',/,'EXCEPT THE LAST. A ZERO CHARACTER CAN BE 
     - REPRESENTED BY A COMMA.',//,17X,'''SEE PROGRAM NOTECOOK
     - FOR SAMPLE''',//)
C
  36  CONTINUE
  15  TYPE 1
  1   FORMAT(3(/),'TYPE INQUIRY NO.',/,2X,'FROM:',2X,$)
      ACCEPT 8,N
      TYPE 25
  25  FORMAT(/,4X,'TO:',2X,$)
      ACCEPT 8,M
  8   FORMAT(I3)
      TYPE 30,N,M
  30  FORMAT('N = ',I3,',','M = ',I3)
      KOUNT=0
      M2=(M-N)+1
      TYPE 21
  21  FORMAT('DATE:   ',$)
      ACCEPT 22,DATE1,DATE2
  22  FORMAT(2A6)
      TYPE 7
  7   FORMAT(/,'ENTER DATA:')
      DO 9 I=1,M2
      ACCEPT 3,Q(I),AMTLCT(I),SS(I),SR(I),AS(I),AR(I)
      KOUNT=KOUNT+1
      IF(KOUNT .EQ. M2) GO TO 10
  9   CONTINUE
  10  MM=M
      NN=N
      LASTPG=1
      NPAGE=1
      KLINE=0
      M3=N-1
      M4=0
      TYPE 71
  71  FORMAT('........')
      I=1
C     CHECK THE RANGE OF M2
      IF(M2-26) 40,40,20
  40  TYPE 5, DATE1,DATE2,NPAGE,LASTPG,NN,MM
  5   FORMAT(2(/),5X,2A6,38X,'PAGE ',I2,' OF ',I2,/43X,'INQUIRY NO"S
     & FROM ',I3,' TO ',I3,/)
      TYPE 11
  11  FORMAT(/,5X,'MATERIAL COST',5X,'UNIT SHOP LACOR',5X,
     &'UNIT ASSY LACOR',4X,'INQUIRY NO.')
  3   FORMAT(6F10.3)
      GO TO 45
  20  M5=M2
      ILINE=0
      DO 100 J=1,10
      M4=M5-27
      M5=M4
      ILINE=ILINE+1
      IF(M4-27) 101,102,100
 100  CONTINUE
 101  LASTPG=ILINE+1
      GO TO 103
 102  LASTPG=ILINE
 103  MM=N+26
      GO TO 40
  45  BMTLCT(I)=AMTLCT(I)/Q(I)
       USL(I)=(SS(I)+SR(I))/Q(I)
      KLINE=KLINE+1
       UAL(I)=(AS(I)+AR(I))/Q(I)
      LINE(I) = KLINE+M3
      TYPE 6, BMTLCT(I),USL(I),UAL(I),LINE(I)
  6   FORMAT(7X,F8.3,12X,F7.3,12X,F7.3,10X,I4,/)
      IF(KLINE .EQ. M2) GO TO 95
      IF(LINE(I) .EQ. MM) GO TO 50
      CONTINUE
      I=I+1
      GO TO 45
  50  NN=MM+1
  52  NPAGE=NPAGE+1
      IF(NPAGE .EQ. LASTPG) GO TO 51
      MM=NN+26
      GO TO 69
  51  MM=NN+(M4-1)
  69  TYPE 70
  70  FORMAT(55X,'CONTINUED',//,'........')
      I=I+1
      GO TO 40
  95  KDIFF = (27*NPAGE)-KLINE
      IF(KDIFF .EQ. 0) KDIFF = 1
      DO 75 K = 1,KDIFF
  76  TYPE 75
  75  FORMAT(/)
  85  TYPE 86
  86  FORMAT('........')
      LETN=2HNO
      TYPE 13
  13  FORMAT(///,'NEXT?  ',$)
      ACCEPT 14,INFO
  14  FORMAT(A3)
      IF(LETN .EQ. INFO) STOP
      GO TO 15
      END
   