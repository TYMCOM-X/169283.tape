        SUBROUTINE LINE(X1,Y1,X2,Y2)
        IX1 = X1
        IY1 = Y1
        IX2 = X2
        IY2 = Y2
        IF(IX1 .EQ. IX2  .AND.  IY1 .EQ. IY2) RETURN
        IF(IX1 .EQ. IX .AND. IY1 .EQ. IY) GO TO 20
        CALL PLOT(IX1 - IX, IY1 - IY, 3)
20      CALL PLOT(IX2 - IX1, IY2 - IY1, 2)
        IX = IX2
        IY = IY2
        RETURN
        END
   