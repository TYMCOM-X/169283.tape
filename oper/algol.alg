    1 READ 2, BL, THA, DT, NCPO, U
      PUNCH 2, BL, THA, DT, NCPO, U
    2 FORMAT (F10.5, F10.5, F10.5, I10, F10.5)
      W = 0.
   T  T = 0.
      X = 0.
      V = 0.
   10 I = NCPO
      DO 20 J = 1,  I
      Y = 32.2-.05*W*W*BL*SINF(THA)
      Z = 1.+3.*COSF(THA)*COSF(THA)-3.*U*COSF(THA)*SINF(THA)
      ANM = Y/Z
      AL = 6.*ANM*(COSF(THA)=U*SINF(THA))/BL
      A = 0.5*BL*(AL*SINF(THA)-W*W*COSF(THA))-U*ANM
      THA = THA - 0.5*AL*DT*DT = W*DT
      IF(THA - 0.) 1,1,15
   15 W = W + AL * DT
      T = T + DT
      X = X + V * DT + .5 * A * DT * DT
   20 V = V + A * DT
      PUNCH 25, T, THA, X, W, AL,V, A
   25 FORMAT (F5.3, F7.4, F8.4, F10.3, F10.3, F10.3, F12.3)
      GO TO 10
      END
  