500 REM: LOAN PAYMENT & INTEREST SECTION
510 REM:     T1=LOAN TERM IN PERIODS
520 REM:       L(T)=LOAN RATE APPLICABLE FOR LEASE TERM T
530 REM:       L1=TOTAL LOAN=.80*COST THEN DECREASES BY PYMT
540 REM:       A=LOAN PAYMENT AMT
550 REM:       I2=INTEREST AMT
560 REM:       P=LOAN PRINCIPAL PYMT
563 REM:       F2=12/F
568 F2=12/F
570 T1=T*12/F
580 L1=.80*C
590 K=0
600 A=L1*(L(T)*((1.0+L(T))^T1)/((1.0+L(T))^T1)-1.0)
610 FOR J=1 TO T1
620 K=K+F2
630 I2=L1*L(T)
640 P=A-I2
650 M(K,3)=M(K,3)+I2
660 M(K,4)=M(K,4)+P
670 L1=L1-P
680 NEXT J
690 REM:       END OF LOAN SEGMENT
1000 REM:      DEPRECIATION SECTION
1010 REM:      A2=ACCUM DEPRECIATION
1020 REM:      D3=DDB OR 150 DB DEPR AMT
1030 REM:      D4=SL DEPR AMT
1040 REM:      R3=DEPR RATE
1050 REM:      T4=REMAINING TERM IN YEARS
1060 A2=0
1070 D4=0
1080 D3=0
1090 T4=T
1100 IF D=3 GO TO 1260
1110 IF D=2 GO TO 1140
1120 R3=2.0/T
1130 GO TO 1150
1140 R3=1.5/T
1150 D3=((C-A2)*R3)/12
1160 D4=((C-A2-SV)/(T4*12)
1161 REM:!!!!!!!! WHAT IS SV IN ABOVE STMT??
1170 IF D3>D4 GO TO 1190
1180 GO TO 1260
1190 J=(T-T4)*12 + 1
1200 FOR K=J TO J+11
1210 M(K,9)=M(K,9)+D3
1215 NEXT K
1220 A2=A2+12*D3
1230 T4=T4-1
1240 IF T4=0 GO TO 1310
1250 GO TO 1150
1260 D4=(C-A2-D1)/(T4*12)
1270 J=(T-T4)*12+1
1280 FOR K=J TO T*12
1290 M(K,9)=M(K,9)+D4
1300 NEXT K
1310 REM:      END DEPREC ROUTINE
2500 REM:      LEASE RECEIPTS CALCULATION; BASED ON EITHER C2C OR DRR
2510 REM:      F1=FIRST TIME SW-INIT AT ZERO
2520 REM:      F2=12/F
2530 REM:      T2=T1 ADJUSTED FOR ADV/ARR
2540 REM:      A1=LEASE PAYMENT AMOUNT
2550 REM:      N=NEW LEASE RATE-EITHER C2C OR DRR FIRST TIME
2560 REM:      R1=RATE FOR CALC LR'S
2570 REM:      B1=BEGINNING PERIOD
2580 REM:      E1=ENDING PERIOD
2590 IF F1=1 GO TO 2650
2595 F1=1
2600 IF D2=0 GO TO 2640
2610 N=D2/100
2630 GO TO 2650
2640 N=C2/100
2650 IF D2=0 GO TO 2680
2660 R1=N/F
2670 GO TO 2690
2680 R1=(1.0+N)^(1.0/F)-1.0
2690 T2=T1
2700 IF A$="N" GO TO 2720
2710 T2=T2-1
2720 A1=R1*((1.0+R1)^T2)/(((1.0+R1)^T2)-1)
2730 IF A$="N" GO TO 2780
2740 A1=A00*(A1/(1.0+A1))
2750 B1=1
2760 E1=T1-F2+1
2770 GO TO 2710
2780 A1=A1*100
2790 B1=1+F2+1
2800 E1=1+T1+1
2810 FOR K=B1 TO E1
2820 M(K,1)=A1
2830 K=K+F2
2840 NEXT K
2850 REM:      END OF LEASE RECPTS SECTION
3100 REM:      PRETAX INCOME SECTION
3110 REM:      T3=12*T=RESIDUAL PERIOD
3120 REM:      D3=SUM OF DEPR=SUM(M(*,9))
3125 REM:      T9=T*12+15
3130 K=0
3140 IF A$="N" GO TO 3190
3150 FOR K=2 TO T9
3160 M(K,10)=M(K-1,1)
3170 NEXT K
3180 GO TO 3220
3190 FOR K=1 TO T9
3200 M(K,10)=M(K,1)
3210 NEXT K
3220 FOR K=1 TO T9
3230 M(K,10)=M(K,10)-M(K,2)-M(K,3)
3240 NEXT K
3250 T3=T*12
3260 M(T3,1)=M(T3,1)+R
3270 M(T3,10)=M(T3,10)+R
3280 D3=0
3290 M(2,10)=M(2,10)+M(1,10)
3300 M(1,10)=0
3310 FOR K=1 TO T9
3320 M(K,10)=M(K,10)-M(K,9)
3330 D3=D3+M(K,9)
3340 NEXT K
3350 M(T3,10)=M(T3,10)-(100.0-D3)
3360 REM:      END OF PRETAX INCOME
3600 REM:      PRETAX CASH FLOW SECTION
3610 FOR K=1 TO T9
3620 M(K,5)=M(K,1)-M(K,2)-M(K,3)-M(K,4)
3630 NEXT K
3640 REM:      END OF PRETAX CASH SECTION
4100 REM:      TAX SECTION-ALL TAXES ARE DELAYED 1 YEAR, AND PD QTRLY
4110 REM:      F4=MONTHS BETWEEN TAX PAYMENTS-READ FROM FILE
4130 REM:      I=INPUT ITC OPTION
4140 REM:      R2=TAX RATE-READ FROM FILE
4150 REM:      S=PERIODIC SUM OF TAXABLE INCOME
4160 REM:      X3=STD ITC % READ FROM FILE
4170 REM:      M1=STD ITC MONTH- READ FROM FILE
4180 S=0
4190 X1=12
4200 X2=X1+T*12
4210 R2=R2/100
4220 FOR J=1 TO T9 STEP F4
4230 L=J+F4-1
4240 FOR K=J TO L
4250 S=S+M(K,10)*R2
4260 NEXT K
4270 M(L+12,6)=M(J+12,6)+S
4280 S=0
4290 NEXT J
4300 IF I=0 GO TO 4350
4310 IF I=1 GO TO 4340
4320 M(M1,6)=M(M1,6)-I1
4330 GO TO 4350
4340 M(M1,5)=M(M1,6)-X3
4350 REM:      END OF TAXES SECTION
4600 REM:      AFTER TAX CASH SECTION
4610 REM:      M(K,7) IS MISC CASH- NOT CURRENTLY USED
4620 FOR K=1 TO T9
4630 M(K,8)=M(K,5)-M(K,6)+M(K,7)
4640 NEXT K
4650 M(1,8)=M(1,8)-.20*C
4660 REM:      END OF AFTER TAX CASH
                                                                                                                                                                                                                                                                                                                                                                                                                       