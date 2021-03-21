10 READ A,B,Q
11 FOR P=1 TO Q
12 READ D(P)
13 NEXT P
15 READ U,V,W
16 PRINT USING 20
20:                     VIBRACHOC
30 PRINT
40 PRINT
50 PRINT USING 60
60:             SYSTEME ELASTIQUE A UN DEGRE DE LIBERTE
70:                      CHOC DEMI-SINUS
71 PRINT USING 70
72 FOR I1=1 TO6
73 PRINT
74 NEXT I1
84 PRINT A
85 PRINT
87 PRINT B
88 PRINT
89 PRINT
90 PRINT"                 ------------------------------------"
100 PRINT
115 PRINT "   EXPRESSION DES RESULTATS :    D/A    "
116 PRINT "   D : DEFLEXION DES AMORTISSEURS EN MM "
117 PRINT "   A : ACCELERATION DU MATERIEL EN G"
118 PRINT
119 PRINT " PREMIERE COLONNE: FREQUENCE A LA SUSPENSION EN HZ"
120 PRINT
122 PRINT"                ------------------------------------"
123 PRINT
124 PRINT
125 PRINT " DIFFERENTES VALEURS DU ....."
126 FOR P=1 TOQ
127 PRINT D(P)
128 NEXT P
130 PRINT
131 PRINT
132 PRINT
133 E=9.81
140 F=3.14116
150 G=A*E
160 I=B*1E-3
170 J=(A+1)*I/F-I/2
175 FOR C=U TO V STEP W
176 PRINT C,
178 FOR P=1 TO Q
180 K=2*F*C
185 A(1)=0
186 A(2)=0
190 L=E/K^2*(-1+EXP(-D(P)*K*J)*(D(P)*SIN(K*J)+COS(K*J)))
200 M=-E/K*EXP(-D(P)*K*J)*SIN(K*J)
210 N=E*K*EXP(-D(P)*K*J)*(SIN(K*J)+2*D(P)*COS(K*J))
220 R=E*K*EXP(-D(P)*K*J)*(D(P)*SIN(K*J)-COS(K*J))
230 H=1E-3/C
240 R(1)=R-H*N
250 R(2)=R-2*H*N
260 FOR T=H TO 2/C STEP H
270 L(1)=L+H*M+H^2/6*(4*R-R(1))
280 M(1)=M+H/12*(5*R(2)-16*R(1)+23*R)
290 IF T>1 THEN 330
300 S=(G+E)*SIN(F*T/I)-E-2*D(P)*K*M(1)-K^2*L(1)
310 X=S-(G+E)*SIN(F*T/I)+E
320 GO TO 350
330 S=-E2*D(P)*K*M(1)-K^2*L(1)
340 X=S+E
350 IF ABS(X)<A(1) THEN 380
360 A(1)=ABS(X)
370 T(1)=T
380 IF ABS(L(1))<A(2) THEN 410
390 A(2)=ABS(L(1))
400 T(2)=T
410 L=L(1)
420 M=M(1)
430 R(2)=R(1)
440 R(1)=R
450 R=S
460 NEXT T
470 PRINT A(2)*1E3,A(1)/E,
490 NEXT P
650 PRINT
660 NEXT C
670 DATA 30,11,4
680 DATA 0.13,0.16,0.20,0.25
690 DATA 12,23,1
999 END
 