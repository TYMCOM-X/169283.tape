C: MM6240  ZETA PLOTTER DRIVER ROUTINE---MAR 29, 1973
      K=1
      DIMENSION MESS(9)
      ASSIGN 100 TO KEOF
      CALL EOF(KEOF)
      OPEN(5,RANDIN,BINARY,$DATAFILE$)
      OPEN(3,INPUT,/INFO/)
      READ(3,1)MESS(J),J=2,7
      CLOSE(3)
1     FORMAT(4A3/2A3)
      CALL PLOTF(0)
      CALL SLINE
      CALL PLOT(0.,-10.,-3)
      CALL PLOT(7.,9.50,-3)
      CALL PLOT(-6.35,-1.5,3)
      CALL PLOT(-6.35,-1.65,2)
      CALL PLOT(-6.55,-1.65,2)
      CALL PLOT(-6.55,-1.50,2)
      CALL PLOT(-6.35,-1.50,2)
      CALL PLOT(-6.7,-8.45,3)
      CALL PLOT(-6.7,-8.75,2)
      CALL PLOT(-6.8,-8.75,2)
      CALL PLOT(-6.8,-8.45,2)
      CALL PLOT(-6.7,-8.45,2)
      MESS(1)=3HA
      CALL SYMBOL(3.,.25,.3,MESS,0.,21)
      IFLAG=1
      XMIN=0.999
      XMAX=131.001
      YMIN=-1.E-5
      YMAX=31.0001
      XF=1./.14
      YF=-1./.3
      XA=.03*XF
      CALL OFFSET(1.,XF,0.,YF)
5     POSITION(5,((K-1)*2)+1)
      READ(5)IY,IX
       Y=IY/100.
       X=IX/100.
      K=K+1
10    FORMAT(2E12.6)
      IF(YMAX-Y)55,55,20
20    IF(X-XMIN)5,5,30
30    IF(Y-YMIN)5,5,40
40    IF(XMAX-X)5,5,50
50    CALL PLOT(X-XA,Y,13)
      CALL PLOT(X+XA,Y,12)
      GO TO 5
55    IFLAG=-1
60    CALL PLOT(17.2,.2,3)
      CALL PLOT(17.2,.32,2)
      CALL PLOT(18.55,.32,2)
      CALL PLOT(18.55,.20,2)
      CALL PLOT(17.2,.2,2)
      CALL PLOT(35.0,0.,-3)
      IF(IFLAG)120,65,120
65    YMIN=31.9999
      YMAX=63.0001
      CALL OFFSET(1.,XF,32.,YF)
      CALL PLOT(-6.7,-1.2,3)
      CALL PLOT(-6.7,-1.45,2)
      CALL PLOT(-6.8,-1.45,2)
      CALL PLOT(-6.8,-1.20,2)
      CALL PLOT(-6.7,-1.20,2)
      CALL PLOT(-5.35,-6.2,3)
      CALL PLOT(-5.35,-6.45,2)
      CALL PLOT(-5.50,-6.45,2)
      CALL PLOT(-5.50,-6.20,2)
      CALL PLOT(-5.35,-6.20,2)
      MESS(1)=3HB
      CALL SYMBOL(3.,.25,.3,MESS,0.,21)
      GO TO 75
70    POSITION(5,((K-1)*2)+1)
      READ(5)IY,IX
       Y=IY/100.
       X=IX/100.
      K=K+1
75    IF(X-XMIN)70,70,80
80    IF(XMAX-X)70,70,85
85    IF(YMAX-Y)70,70,90
90    IF(Y-YMIN)70,70,95
95    CALL PLOT(X-XA,Y,13)
      CALL PLOT(X+XA,Y,12)
      GO TO 70
100   CLOSE(5)
110   CALL PLOT(19.35,-4.05,3)
      CALL PLOT(19.35,-4.55,2)
      CALL PLOT(19.50,-4.55,2)
      CALL PLOT(19.50,-4.05,2)
      CALL PLOT(19.30,-4.05,2)
      CALL PLOT(20.,-9.75,999)
      CALL EXIT
120   CALL POFF
      CALL EXIT
      END
                                                                                                                          