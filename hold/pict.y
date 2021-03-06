        DIMENSION X(4,120), IT(720), JT(720),  DONE(720), A(4,4)
        DIMENSION NE(120,12), NC(120), V(4), PJ(3), PK(3)
        DIMENSION NN(120)
        SCL = 200.
        SL = 0.
        T = (SQRT(5.)+1.)/2.
        X(1,2) = T
        X(2,2) = T
        X(3,2) = T
        X(4,2) = 1./T**2
        X(1,1) = T**2
        X(2,1) = 1./T
        X(3,1) = 1./T
        X(4,1) = 1./T
        X(1,3) = SQRT(5.)
        X(2,3) = 1.
        X(3,3) = 1.
        X(4,3) = -1.
        DO 10 I = 4, 12
        X(1,I) = X(2,I-3)
        X(2,I) = X(3,I-3)
        X(3,I) = X(4,I-3)
10      X(4,I) = X(1,I-3)
        DO 20 I = 13,24
        X(1,I) = X(1,I-12)
        X(2,I) = X(2,I-12)
        X(3,I) = -X(3,I-12)
20      X(4,I) = -X(4,I-12)
        DO 30 I = 25, 48
        X(1,I) = X(1,I-24)
        X(2,I) = -X(2,I-24)
        X(3,I) = -X(3,I-24)
30      X(4,I) = X(4,I-24)
        DO 40 I = 49, 96
        X(1,I) = -X(1,I-48)
        X(2,I) = -X(2,I-48)
        X(3,I) = X(3,I-48)
40      X(4,I) = X(4,I-48)
        DO 50 I = 1, 2
        DO 50 J = 1, 2
        X(1,94+I+2*J) = 2.*(-1.)**I
        X(2,94+I+2*J) = 2.*(-1.)**J
        X(3,94+I+2*J) = 0.
50      X(4,94+I+2*J) = 0.
        DO 60 J = 101, 104
        X(1,J) = X(1,J-4)
        X(2,J) = 0.
        X(3,J) = X(2,J-4)
        X(4,J) = 0.
        X(1,J+4) = 0.
        X(2,J+4) = 0.
        X(3,J+4) = X(1,J-4)
60      X(4,J+4) = X(2,J-4)
        DO 70 J = 109, 120
        X(1,J) = X(2,J-12)
        X(2,J) = X(3,J-12)
        X(3,J) = X(4,J-12)
70      X(4,J) = X(1,J-12)
        L = 1
        DO 100 I = 1, 119
        DO 100 J = I+1, 120
        S = 0.
        DO 90 K = 1, 4
90      S = S+(X(K,I)-X(K,J))**2
        IF (S .GT. 3.1) GO TO 100
        IT(L) = I
        JT(L) = J
        L = L+1
        NC(I) = NC(I)+1
        NE(I,NC(I)) = J
        NC(J) = NC(J)+1
        NE(J,NC(J)) = I
100     CONTINUE
        IF(L .NE. 721) STOP
        DO 101  I = 1, 120
101     IF(NC(I) .NE. 12) STOP
        DO 105 I = 1, 4
105     A(I,I) = 1.
        DO 150 I = 1, 4
        S = 0.
        DO 110 J = 1, 4
110     S = S + A(I,J)**2
        S = 1./SQRT(S)
        DO 120 J = 1, 4
120     A(I,J) = A(I,J)*S
        IF (I .EQ. 4) GO TO 150
        DO 140 J = I+1, 4
        S = 0.
        DO 130 K = 1, 4
130     S = S + A(I,K)*A(J,K)
        DO 140 K = 1, 4
140     A(J,K) = A(J,K) - S*A(I,K)
150     CONTINUE
        DO 170 I = 1, 120
        DO 160 J = 1, 4
160     V(J) = X(J,I)
        DO 170 J = 1, 4
        X(J,I) = 0.
        DO 170 K = 1, 4
170     X(J,I) = X(J,I) + A(J,K)*V(K)
        CALL OPENPL
        DO 901 I = 1, 120
        DO 901 JX = 1, 11
        IF(NE(I,JX) .LT. I) GO TO 901
        J = NE(I, JX)
        DO 900 K = JX+1, 12
        DO 900 L = 1, 12
        IF(NE(I,K) .NE. NE(J,L)) GO TO 900
        NN(I) = NN(I)+1
        NN(J) = NN(J)+1
        NN(NE(J,L)) = NN(NE(J,L))+1
        IG=8+(X(1,I) .GT. SL)+2*(X(1,J) .GT. SL)+4*(X(1,NE(J,L)).GT.SL)
        GO TO (230, 180, 190, 200, 200, 190, 180, 230), IG
180     IP = I
        JP = J
        KP = NE(J,L)
        GO TO 210
190     IP = J
        JP = I
        KP = NE(J,L)
        GO TO 210
200     IP = NE(J,L)
        JP = I
        KP = J
210     AJ = (SL - X(1,IP))/(X(1,JP) - X(1,IP))
        AK = (SL - X(1,IP))/(X(1,KP) - X(1,IP))
        DO 225 M = 2, 4
        PJ(M-1) = X(M,IP) + AJ*(X(M,JP) - X(M,IP))
225     PK(M-1) = X(M,IP) + AK*(X(M,KP) - X(M,IP))
        CALL LINE(SCL*PJ(1), SCL*PJ(2), SCL*PK(1), SCL*PK(2))
        ITR = ITR+1
230     CONTINUE
900     CONTINUE
901     CONTINUE
        DO 999 I = 1, 120
999     IF(NN(I) .NE. 30) TYPE 78, I, NN(I)
        TYPE 78, ITR
78      FORMAT(1X,13I4)
        END
   