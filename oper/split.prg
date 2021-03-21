        SUBROUTINE SPLIT(X,IFAC,IDATE,MONDAY)
        IF(IT.EQ.2)GOTO 991
        IT=2
        CALL IFILE(21,'WSPLT')
        OPEN(21,'WEK1',RANDIO,SYMBOLIC)
        OPEN (22,'WEK2',RANDIO,SYMBOLIC)
        OPEN (23,'WEK3',RANDIO,SYMBOLIC)
        OPEN (24,'WEK4',RANDIO,SYMBOLIC)
        OPEN (11,'WSMBL',RANDIO,SYMBOLIC)
1       FORMAT(6X,I4)
        READ(11#1,1)N
991     READ(21,7,END=990)X,IFAC,IDDD
7       FORMAT(1X,A5,I5,I6)
         IF(IDDD.EQ.IDATE) CALL WKSPLT(X,IFAC,N)
         IF(IDDD.NE.IDATE)GOTO 991
        IF(MONDAY.EQ.1)GOTO 991
        GOTO 993
990     X='ZZZZZ'
993     RETURN
        END
        SUBROUTINE WKSPLT(X,IFAC,N)
        DIMENSION MM(208)
        TYPE 3,I1,X
90      CONTINUE
        S=X
        IF(S.LE.'ZZZZZ')I1=4
        IF(S.LE.'RZZZZ')I1=3
        IF(S.LE.'KZZZZ')I1=2
        IF(S.LE.'EZZZZ')I1=1
5       FORMAT(A5)
        IX=0
1       FORMAT(6X,I4)
2       FORMAT(I1,A5,I4)
3       FORMAT(I2,A6/)
        N1=1
        N2=N
98      NN=N1+(N2)/2
97      NX=(NN-1)*12+13
        READ(11#NX,2)IV,S1,I2
6       FORMAT(1X,I2,A6,I5)
        IX=IX+1
        IF(IX.GT.20)GOTO 1001
        IF(S-S1)99,100,101
99      N2=NN
        NN=(NN-N1)/2+N1
        GOTO 97
101     N1=NN
        NN=N1+(N2-N1)/2
        GOTO 97
100     NX=(I2-1)*94*14+4*94+1
        I1=I1+20
        NY=NX
        READ(I1#NX,7)IX,Z,MM
        TYPE 17,IX,Z,MM(1),MM(2),MM(3),MM(4)
17      FORMAT(I2,A6,4I6/)
        FAC=FLOAT(IFAC+10000)/10000
        DO 77 J=1,208,4
        MM(J)=FLOAT(MM(J))/FAC+.5
        MM(J+1)=FLOAT(MM(J+1))/FAC+.5
        MM(J+3)=FLOAT(MM(J+3))*FAC+.5
        MM(J+2)=FLOAT(MM(J+2))/FAC+.5
77      CONTINUE
        TYPE 17,IX,Z,MM(1),MM(2),MM(3),MM(4)
        WRITE(I1#NY,7)IX,Z,MM
7        FORMAT(I1,A5,86X/12(4(3I6,I5)/),4(3I6,I5))
        GOTO 999
1001    TYPE 9,S
9       FORMAT(' SYMBOL ',A5,' IS INCORRECT OR UNAVAILABLE')
999     CONTINUE
        RETURN
        END
