0010� FILES PIA7
  0010� PRINT"ENTER 1 FOR SUMMARY ONLY"
 0010� INPUT D
0010� PRINT
  0011� T1=0
   0011� IF D=1 THEN 140
  0012� PRINT
  0013� PRINT
  0014� READ#1 M1,M9,M2,M3,M4,M5,M6,M7,M8
    0015� IF M1=9999 THEN 530
   0015� IF D=1 THEN 189
  0016�PRINT USING 170
   0017� :CST CTR   CO CODE  M.SHIFTS MEN  INVEST  DEPR  SPACE  HSPR  OTHER:
0018� PRINT
  0018� IF D=1 THEN 300
  0019� IF M5=0 THEN 240
 0020� IF M5>1 THEN 240
 0020� IF D=1 THEN 300
  0021� PRINT USING 220,M1,M9,M2,M3,M4,M5,M6,M7,M8
0022� :%%%%%%   %%%%%%  %%%%%%%%  %%% %%%%%%% %.%%% %%%%%% %%% %%%%%.%%%:
0023� GO TO 260
   0024� PRINT USING 250,M1,M9,M2,M3,M4,M5,M6,M7,M8
0025� :%%%%%%   %%%%%%  %%%%%%%%  %%% %%%% %%%%%%%% %%%%%% %%% %%%%%.%%%:
0026� PRINT
  0027� PRINT USING 290
  0028� PRINT
  0029� :   NAME                   CLOCK NO MO DAY YR CODE  RATE  O/T HRS:
 0030� READ#1 K$,N2,N3,N4,N5,N6,N7,N8
  0030� IF N6<>2 THEN 310
0030� N7=N7*1.10
  0031� A1=A1+1
0032� IF N6<5 THEN 410
 0033� IF N6=5 THEN 440
 0034� IF N6=6 THEN 470
 0035� IF N6=7 THEN 500
 0035� IF D=1 THEN 380
  0036� PRINT USING 370,K$,N2,N3,N4,N5,N6,N7,N8
   0037� :"""""""""""""""""""""""""  %%%%%    %% %% %% %% %%%%%.%%% %%%%%:
  0038� T1=T1+1
0039� IF T1=M3 THEN 110
0040� GO TO 300
   0041� A2=A2+1
0042� A3=A3+N7
    0043� GO TO 359
   0044� A4=A4+1
0045� A5=A5+N7
    0046� GO TO 359
   0047� A6=A6+1
0048� A7=A7+N7
    0049� GO TO 359
   0050� A8=A8+1
0051� A9=A9+N7
    0052� GO TO 359
   0053� B2=A3/A2
    0054� IF A4=0 THEN 560
 0055� B3=A5/A4
    0056� IF A6=0 THEN 580
 0057� B4=A7/A6
    0058� IF A8=0 THEN 600
 0059� B5=A9/A8
    0060� C1=A2+A4+A6+A8
   0061� PRINT USING 620
  0062� : DESCRIPTION                            EMPLOYEES  AVE RATE:
 0063� PRINT USING 640,A2,B2
 0064� : HOURLY PROD                             %%%%%     %%%.%%%:
  0065� PRINT USING 660,A4,B3
 0066� : HOURLY OFFICE                           %%%%%     %%%.%%%:
  0067� PRINT USING 680,A6,B4
 0068� : PART-TIME                               %%%%%     %%%.%%%:
  0069� PRINT USING 700,A8,B5
 0070� : SALARY                                  %%%%%  %%%%%%.%%%:
  0071� PRINT USING 720,C1
    0072� :TOTAL EMPLOYEES                        %%%%%%%:
    0073� END
    