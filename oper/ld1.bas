0000� REM LD1 *****(NOTE: TAPE GARBLED AT LINES 240 AND 520. CHECK BEFORE US
  0010� PRINT "AMT,RATE,PAYMENT,# OF YRS,PDS PER YR,PRINT CODE"
  0010� INPUT A,R,P,Y,N,L
0011� I=I2=P2=0
   0012� IF N<>0 THEN 140
 0014� E=Y*N
  0015� IF P=0 THEN 210
  0016� IF Y=0 THEN 290
  0017� IF R=0 THEN 390
  0018� IF A=0 THEN 690
  0019� PRINT 'INPUT ERROR, PLEASE REENTER'
  0020� GO TO 100
   0021� IF Y=0 THEN 180
  0022� IF R=0 THEN 190
  0023� IF A=0 THEN 190
  0023� REM ????? NEXT INSTRUCTION GARBLED ON TAPE, PLEASE CK.
   0024� P=(A*R/N)/(1-(1+(R/N)/100)^(-E))+.5
  0025� P=INT(P+.5)/100
  0026� PRINT USING 270 ,P
    0027� : PAYMENT = #########.##
   0027� GOTO 100
    0028� GO TO 730
   0029� IF R=0 THEN 190
  0030� IF A=0 THEN 190
  0031� Y=-(LOG(1-A*R/(N*P*100)))/(N*LOG(1+R/(N*100)))
 0032� PRINT USING 330 ,Y
    0033� : NUMBER OF YEARS = ###.##
 0034� IF (Y-INT(Y))<>0 THEN 370
  0035� E=Y*N
  0036� GO TO 730
   0037� E=INT(Y+1)*INT(N)
0038� GO TO 730
   0039� IF A=0 THEN 190
  0040� R1=P/A
 0041� R2=.01
 0042� R3=R1*(1-(1+R2)^(-E))
 0043� IF R2<R3 THEN 620
0044� IF R2=R3 THEN 580
0045� R4=R2
  0046� R2=R4-.001
  0047� R3=R1*(1-(1+R2)^(-E))
 0048� IF R2>R3 THEN 450
0049� IF R2=R3 THEN 580
0050� R5=R2
  0051� R2=(R5+R4)/2
0051� REM ???? CHECK NEXT STATEMENT
   0052� R3=R1*(1-(1+R2)^(-E))
 0053� IF ABS(R2-R3)<.0000001 THEN 580
 0054� IF R2=R3 THEN 580
0055� IF R2>R3 THEN 670
0056� R5=R2
  0057� GO TO 510
   0058� R=INT(R3*N*100000+.5)/1000
 0059� PRINT USING 600 ,R
    0060� : RATE = ##.###
  0061� GO TO 730
   0062� R5=R2
  0063� R2=R5+.01
   0064� R3=R1*(1-(1+R2)^(-E))
 0065� IF R2<R3 THEN 620
0066� IF R2=R3 THEN 580
0067� R4=R2
  0068� GO TO 510
   0069� A=(P*100*N*(1-(1+R/(N*100))^(-E)))/R
 0070� A=INT(A)
    0071� PRINT USING 720 ,A
    0072� : AMOUNT = #########.##
    0073� R=R*100
0074� IF L=0 THEN 1070
 0075� PRINT
  0076� PRINT '   N     INTEREST    PRINCIPAL      BALANCE          BBF'
   0077� PRINT
  0078� B1=A
   0079� B2=0
   0080� I=I+1
  0081� I1=INT((B1*R)/(N*100)+.5)/100
   0082� IF I=E THEN 930
  0083� P1=INT((B2+P-I1)/L)*L
 0084� B2=P-I1-P1+B2
    0085� B1=B1-P1
    0086� PRINT USING 870 ,I,I1,P1,B1,B2
  0087� :#### #########.## #########.## #########.## #########.##
0088� I2=I2+I1
    0089� P2=P2+P1
    0090� IF L=0 THEN 990
  0091� IF I<E THEN 800
  0092� GO TO 1000
  0093� P1=B1
  0094� B1=P1+I1-B2
 0095� PRINT USING 960 ,I,I1,P1,B1
0096� :#### #########.## #########.## #########.## *
 0097� PRINT
  0098� GO TO 880
   0099� IF I<E THEN 1110
 0100� D=I2+P2
0101� PRINT USING 1020 ,I2
  0102� :  TOTAL INTEREST  = ##########.##
   0103� PRINT USING 1040 ,D
   0104� :  TOTAL DEBT SERV = ##########.##
   0105� PRINT
  0106� GO TO 100
   0107� PRINT
  0108� PRINT '     N         INTEREST        PRINCIPAL          BALANCE'
  0109� PRINT
  0110� B1=A
   0111� I=I+1
  0112� I1=INT((B1*R)/(N*100)+.5)/100
   0113� IF I=E THEN 1190
 0114� P1=P-I1
0115� B1=B1-P1
    0116� PRINT USING 1170 ,I,I1,P1,B1
    0117� :  ####     #########.##     #########.##    ##########.##
    0118� GO TO 880
   0119� P1=B1
  0120� B1=I1+P1
    0121� PRINT USING 1220 ,I,I1,P1,B1
    0122� :  ####     #########.##     #########.##    ##########.## *
  0123� PRINT
  0124� GO TO 880
   0125� END
    