100REM..SORTBD
110REM..SORT NUMERICS,BUBBLE METHOD,
120REM.. DECREASING ORDER
130 FOR I9=1 TO N9
140 K9=0
150 FOR I=2 TO N9
160 IF F(I-1)>=F(I) THEN 280
170REM SWAP PRIMARY SORT PARAMETER
180 S9=F(I-1)
190 F(I-1)=F(I)
200 F(I)=S9
210REM SWAP SECONDARY FIELD
220 S9=E(I-1)
230 E(I-1)=E(I)
240 E(I)=S9
250 REM SWAP TERCIARY AND LESSER FIELDS
260 REM  NO TERC.
270 K9=1
280 NEXT I
290 IF K9=0 THEN 310
300 NEXT I9
310 RETURN
                                                                                                                                                                                                   