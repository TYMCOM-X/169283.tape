100 DIM R$(4)
110 DIM T(132),O(132)
175 FOR K1=1 TO 132
180   O(K1)=ASC(0)
190 NEXT K1
700 FOR J9=1 TO 10000
710 IF J9<>1000*INT(J9/1000) GO TO 800
720   PRINT J9;
800 MAT READ R$
810 Q(2)=25
840 R$(2)=R$(2)+SPACE$(Q(2)-LEN(R$(2)))
845 CHANGE R$(2) TO T
850   V1=24
855   GOSUB 2600
890 CHANGE O TO O$
895 RESTORE
900 NEXT J9
1000 STOP
2600 REM -------- INSERT ----------
2610 K1=1
2620 FOR K2=V1 TO V1+T(0)-1
2630   O(K2)=T(K1)
2640   K1=K1+1
2650 NEXT K2
2660 RETURN
9000 DATA "1111","AAAAABBBBBCCCCCDD","A","100.00"
9999 END
