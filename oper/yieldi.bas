0007? DIM P(60)
   0009? PRINT"FILE NAME?"
0009? INPUT A$
    0010? DIM N(120,5)
0011? PRINT" # OF PERIODS PER YEAR"
   0011? INPUT Q6
    0012? S5=.035/Q6
  0013? F1=0
   0020? FILE#1,A$
   0021? READ#1,A
    0022? Q=A-1
  0023? READ#1,B
    0024? E=B
    0025? B9=Q+1
 0025? PRINT USING 256 ,E,B9
 0025? : #############  ################
    0026? N(1,4)=E
    0547? FOR Y=2 TO B9
    0549? READ#1,C
    0550? N(Y,1)=C
    0552? NEXT Y
 0555? B=0
    0557? B1=0
   0560? R=0
    0562? FOR R=.010000 TO .03000 STEP .02000
  0565? Z7=0
   0566? FOR Y=2 TO B9
    0566? N(Y,2)=N(Y,3)=N(Y,4)=N(Y,5)=0
   0567? NEXT Y
 0567? FOR Y=2 TO B9
    0570? IF N(Y-1,4)<0 THEN 5800
    0572? N(Y,5)=N(Y-1,4)*S5
    0575? N(Y,4)=N(Y-1,4)+N(Y,5)+N(Y,1)
   0577? GOTO 6000
   0580? N(Y,2)=ABS(N(Y-1,4))*R
0582? IF N(Y,2)>N(Y,1) THEN 5875
 0585? GOTO 5925
   0587? N(Y,3)=N(Y,1)-N(Y,2)
  0590? GOTO 5950
   0592? N(Y,3)=N(Y,1)-N(Y,2)
  0595? N(Y,4)=N(Y-1,4)+N(Y,3)-P(Y)
0597? GOTO 6000
   0600? B=B+1
  0602? :### ###.#######  ############
  0605? NEXT Y
 0607? Z7=N(B9,4)
  0610? IF U4=1 THEN 6500
0612? IF R=.03000 THEN 6225
 0615? B1=Z7
  0617? R1=R
   0620? GOTO 6275
   0622? B2=Z7
  0625? R2=R
   0627? GOTO 6300
   0630? NEXT R
 0632? M=(B1-B2)/(R1-R2)
0635? Z=B2-(M*R2)
 0637? :##.#####  ##.##### ##########  ##########
0640? R=-Z/M
 0642? U4=1
   0645? W5=1
   0647? GOTO 5650
   0650? R3=R
   0652? B3=Z7
  0653? : ###  ###.#######  #############
    0655? IF B3>50 THEN 6625
    0657? IF B3<-50 THEN 6625
   0660? GOTO 6800
   0662? M1=(B2-B3)/(R2-R3)
    0665? Z1=B3-(M1*R3)
    0667? R=-Z1/M1
    0670? W5=W5+1
0672? R2=R3
  0675? B2=B3
  0677? GOTO 5650
   0680? PRINT
  0805? PRINT'           ***** THE YIELD ON THE UNRECOVERED INVESTMENT *****'
   0807? PRINT
  0810? PRINT " PRD      CASH       INTEREST    PRINCIPAL   BALANCE       SINKING    SINKING"
  0812? : PRD     CASH      INTEREST    PRINCIPAL   BALANCE        SINKING       SINKING
  0815? PRINT USING 8175,R*100
0817? :       AVAILABLE   AT ###.###%  PAYMENT   REMAINING       FUND      FUND EARNINGS
0820? PRINT USING 8225 ,R*100*Q6,S5*100
    0822? :                  YEARLY ####.###%                                #.###%
    0825? PRINT
  0827? PRINT
  0830? FOR Y=1 TO B9
    0830? IF N(Y,4)<0 THEN 8335
 0832? PRINT USING 8400,F1,N(Y,1),N(Y,2),N(Y,3),0,N(Y,4),N(Y,5)
 0832? GO TO 8350
  0833? PRINT USING 8400,F1,N(Y,1),N(Y,2),N(Y,3),N(Y,4),0,N(Y,5)
 0835? F1=F1+1
0837? NEXT Y
 0840? : ### ########.## ########.## ########.## ##########.## ##########.##  #########.##
    0850? END
    