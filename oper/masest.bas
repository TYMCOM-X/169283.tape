100 FILES DIES1,DIES2,DIES3,WASTE
110 F=1
115 X1=1
120 DIM Q(13),A(13)
125 FOR S= 1TO 13
127 READ Q(S),A(S)
130 NEXT S
132 DATA 1,2,2,4,3,6,5,10,10,15,15,20,20,25
134 DATA 25,30,50,55,75,83,100,110,200,210,500,515
140 READ S1,S2,S3,S4,S5,S6
142 READ P1,P2,P3,P4,P5
145 DATA .5,.7,1,.7,.9,1.2
147 DATA .3,.2,.3,.3,.3
150 READ R1,R2,R3,R4,R5,R6,R7,R8
152 DATA 12,12,15,13,17,11,12,12
160 DIM M(14,14)
161 Z9=1
162 FOR I= 1 TO Z9
165 FOR J= 1 TO Z9
167 READ #4,M(I,J)
170 NEXT J
172 NEXT I
180 FOR E=1 TO 4
182 READ W(E),X(E),Y(E),Z(E),T(E),V(E)
185 NEXT E
190 DATA 3.75,1800,1700,1300,11000,13000
192 DATA 7.75,1700,1600,1200,11000,13000
194 DATA 11,1600,1500,1100,9000,10000
196 DATA 20,1600,1500,1100,7000,7000
200 READ A$,A1,A2
210 DATA "7G",.18,1
230 DIM D(13,11)
232 FOR K= 1 TO 13
234 FOR L=1 TO 11
237 READ #1,D(K,L)
240 NEXT L
245 NEXT K
250 IF F<>1 THEN 260
252 G1=S1
254 G2=S2
256 G3=S3
258 GO TO 270
260 G1=S4
262 G2=S5
264 G3=S6
266 IF A2<> 1 THEN 272
268 H1=G1+P3
270 GO TO 290
272 IF A2<>2 THEN 278
274 H1=G1+P4+P3
276 GO TO 290
278 IF A2<>3 THEN 286
280 H1=G2+P3
282 GO TO 290
286 H1=G3+P5+P3
290 X3=X3+X1
292 FOR I= 1 TO 14
294 FOR J= 1 TO 14
295 IF M(I,1)=X3 THEN 303
297 NEXT J
299 NEXT I
303 FOR K= 1 TO 13
305 X5=1
310 FOR L=1 TO 10
315 IF X5=1 THEN 800
320 IF D(K,L)=0 THEN 600
325 B1=(A(S)*X3)
327 B2=B1+(B1*M(I,1))
330 B3=B2*.1
335 B4=(B2+B3)*A1
340 B5=B4*.1
345 IF B5>10 THEN 355
350 B5=10
354 PRINT
355 PRINT "MATL",X3,B4
360 PRINT "INK",B5
370 C1=A(S)/D(K,L)
372 IF X5<>1 THEN 376
374 C3=X(E)
375 GO TO 385
376 IF X5<>2 THEN 385
378 C3=Y(E)
380 GO TO 385
382 C3=Z(E)
383 X5=1
385 C2=C1/C3
390 C4=H1+(.3*X5)+(.2*D(K,L))
395 T1=C4*R3
400 T2=C2*R4
405 PRINT "SET UP",C4,T1
410 PRINT "RUN",C2,T2
600 NEXT L
650 NEXT K
670 GO TO 290
680 F=F+1
685 IF F>3 THEN 9000
690 GO TO 230
800 X6=0
802 FOR S=1 TO 26
805 IF D(K,1)=Q(S) THEN 325
810 NEXT S
9000 END
 