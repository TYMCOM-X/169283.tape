2 FILE #1,"D1": #3,"D2"
10 DIM A(100,2),B(100,2)
20 FOR I=1 TO 100
30 IF A(I,1)=B(I,1) THEN A(I,1)=A(I,1)+B(I,2)
40 NEXT I
50 MAT PRINT #3:A
60 PRINT"DONE"
70 END
    