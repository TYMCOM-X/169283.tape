100 RANDOM
115 INPUT A1
116 A=A1
120 N=0
130 L=1
131 B=0
132 S=0
140 K=0
142 B=B+1
150 N=0
155 IF A<0 THEN 999
160 A=A-5*L
170 PRINT A,L,S
180 K=K+1
190 D1=INT(6*RND+1)
210 D2=INT(6*RND+1)
230 D=D1+D2
240 
250 PRINT D;
260 IF D=N THEN 265
261 IF D=7 THEN 265
262 GO TO 270
265 PRINT
270 IF K=1 THEN 400
280 IF D=7 THEN 290
285 GO TO 310
290 L=1
300 GO TO 140
310 IF D=2 THEN 180
311 IF D=3 THEN 180
312 IF D=11 THEN 180
313 IF D=12 THEN 180
320 IF D=N THEN 330
325 GO TO 380
330 A=A+10*L+O*L+30*L
340 IF A>A1*(L+1) THEN 342
341 GO TO 345
342 L=L+1
345 N=0
370 GO TO 140
380 A=A+C(D)*L
390 GO TO 180
400 IF D=2 THEN 140
401 IF D=3 THEN 140
402 IF D=12 THEN 140
410 IF D=7 THEN420
411 IF D=11 THEN 420
412 GO TO 440
420 A=A+10*L
430 GO TO 140
440 N=D
450 IF D=4 THEN 460
451 IF D=10 THEN 460
452 GO TO 500
460 A=A-5*L-31.5*L
470 O=15
480 C(4)=16
481 C(10)=16
482 C(5)=9
483 C(9)= 9
484 C(6)=6
485 C(8)=6
490 GO TO 180
500 IF D=5 THEN 510
501 IF D=9 THEN 510
502 GO TO 550
510 A=A-6*L-31.5*L
520 O=15
530 C(4)=12
531 C(10)=12
532 C(5)=12
533 C(9)=12
534 C(6)=6
535 C(8)=6
540 GO TO 180
550 A=A-5*L-31.5*L
560 O=11
570 C(4)=8
571 C(10)=8
572 C(5)=9
573 C(9)=9
574 C(6)=12
575 C(8)=12
580 GO TO 180
999 END
   