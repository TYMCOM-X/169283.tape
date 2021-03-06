      DIMENSION DUMMY1(298),DUMMY2(249),IDUM1(23),DUMMY3(374)                   
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      COMMON /RM/ LMAXM1,LMAXM2                                                 
      EQUIVALENCE (A(1),DUMMY1(1)),(AFRAC,DUMMY2(1)),(IWASH,IDUM1(1)),          
     1(OXTOC,DUMMY3(1))                                                         
C     MAINLINE PROGRAM                                                  QAL70010
      INP=5                                                                     
      IOU=6                                                                     
      LCNT1 = 0                                                                 
      LCNT2 = 0                                                                 
      DO 9998 I=1,298                                                           
 9998 DUMMY1(I)=0.0                                                             
       DO  1000 I=1,10                                                          
1000  LZ(I)=0                                                                   
      DO 9997 I=1,249                                                           
 9997 DUMMY2(I)=0.0                                                             
      DO 9995 I=1,374                                                           
 9995 DUMMY3(I)=0.0                                                             
      DO 9996 IFINK =1,23                                                       
 9996 IDUM1(IFINK) = 0                                                          
   13 CALL READX                                                                
      NHIPR=NHIPR                                                       QAL70050
      NLOPR=NLOPR                                                       QAL70060
      N=N                                                               QAL70070
      M=M                                                               QAL70080
    3 CALL PPTR                                                                 
    4 CALL SPTLIQ                                                               
      CALL FLASH                                                        QAL70110
      CALL SETTLE                                                       QAL70120
    5 CALL BAUX2                                                                
      CALL BLO                                                          QAL70140
      CALL DIG2                                                         QAL70150
      CALL HEATER                                                       QAL70160
      CALL WASH1                                                        QAL70170
      CALL WASH2                                                        QAL70180
      IF(DABS(SMKUP-SMKP)-TEST1)322,500,500                                     
  500 LCNT1 = LCNT1 + 1                                                         
      IF (LCNT1 - LMAXM1) 319,319,501                                           
  501 WRITE (IOU,502) SMKUP,SMKP                                                
  502 FORMAT (' LCNT1 IN MAIN EXCEEDED,  SMKUP = ',G13.6,'  SMKP = ',           
     1G13.6)                                                                    
      GO TO 322                                                                 
  319 SMKUP=SMKP                                                        QAL70200
      ALOSP=ALOSS-(AEXLS+AUTSA+AUTWA+APISO+OSLKFA+ARVR)*(1.-OAR/OAR1)   QAL70210
      ALOSP=ALOSP-.56464*FAIDLM*(1.-OAR/OAR1)                           QAL70220
      OAR=APROD/(APROD+ALOSP)                                           QAL70230
      SUMLC=CPISO+CHTWL+CSWR+CUNAC+CRVR+OXMDC                           QAL70240
      GO TO 3                                                           QAL70250
  322 IF (DABS(ALOST-ALOSS)-TEST1)323,503,503                                   
  503 LCNT2 = LCNT2 + 1                                                         
      IF (LCNT2 - LMAXM2) 319,319,504                                           
  504 WRITE (IOU,505) ALOST,ALOSS                                               
  505 FORMAT (' LCNT2 IN MAIN EXCEEDED,  ALOST = ',G13.6,'  ALOSS = ',          
     1G13.6)                                                                    
  323 ASH=SMKUP+TOTFAS+SSFLR+TOWSTS                                     QAL70270
      FLRSDA=SSFLR+TOWSTS                                               QAL70280
      STARCH=TOTWST+FLRS                                                QAL70290
      EMIL=WLIME+DGLIM+REACLM+FAIDLM                                    QAL70300
      UNITBX=BXREQ/APROD                                                QAL70310
      UNITSD=ASH/APROD                                                  QAL70320
      UNITLM=EMIL/APROD                                                 QAL70330
      UNITST=STARCH/APROD                                               QAL70340
      ST650=DGST/.012                                                   QAL70350
      ST5LB=S5LB/.012                                                   QAL70360
      VNTHI=SPR(NHIPR)/.012                                             QAL70370
      COND=COND+STFL(N+1)                                               QAL70380
      VNTLO=SPR(NLOPR)/.012                                             QAL70390
      VNTBLO=STBLO/.012                                                 QAL70400
      XINJ=STFL(1)+XTTK-BXSPL                                           QAL70410
      RAT=(XTTK-BXSPL)/XTTK                                             QAL70420
      CINJ=CTTK*RAT                                                     QAL70430
      SINJ=STTK*RAT                                                     QAL70440
      AINJ=ATTK*RAT                                                     QAL70450
      WFINJ=CINJ/XINJ                                                   QAL70460
      CCINJ=CC3(WFINJ,RTTK)                                             QAL70470
      DINJ=.001*CCINJ/WFINJ                                             QAL70480
      FINJ=XINJ*.166443/DINJ                                            QAL70490
      CPINJ=CPF(CCINJ,RTTK)                                             QAL70500
      T=TFL(1)-BPE(1)                                                   QAL70510
      CONHI=1186.7+.04*T+.75*BPE(1)                                     QAL70520
      CONLO=PF(T)                                                       QAL70530
      TOINJ=32.+(STFL(1)*CONHI+A101*(THTR(M)-32.))/(XINJ*CPINJ)         QAL70540
      BPEINJ=BPF(CCINJ,TOINJ)                                           QAL70550
      TEM=TOINJ-BPEINJ                                                  QAL70560
      PRINJ=PF(TEM)                                                     QAL70570
      DBC=.001*CCBC/WFBC                                                QAL70580
      FBC=.166443*XBC/DBC                                               QAL70590
      STTOF=CTTOF/CTSLP                                                 QAL70600
      OXWW=DILOXM*REACMD                                                QAL70610
      TOLMSL=WLIME+DGLIM+REACLM                                         QAL70620
      XRESLM=XRLIM+REACLM-REACSL                                        QAL70630
      CRESLM=CWLIM*XRESLM/WLMLQ                                         QAL70640
      SRESLM=SWLIM*XRESLM/WLMLQ                                         QAL70650
      SLURLQ=BXMST+XLMSL+BXSPL                                          QAL70660
      SLURSL=BXREQ+DLMUD                                                QAL70670
      BXLMLQ=SLURLQ+SLURSL                                              QAL70680
      TBXSLR=32.+(A103*(TSLLQ-32.)+A104*(TBX-32.)+A105*(TLIME-32.))/(   QAL70690
     1A103+A104+A105)                                                   QAL70700
      STCHLQ=XSFLR+TOWSTL                                               QAL70710
      STCHC=CSFLR+TOWSTC                                                QAL70720
      STCHS=SSFLR+TOWSTS                                                QAL70730
      T=XSRT+DILSR                                                      QAL70740
      XRT1=AFRAC*T                                                      QAL70750
      XRT2=BFRAC*T                                                      QAL70760
      XRT3=(1.-AFRAC-BFRAC)*T                                           QAL70770
      SXRT1=SSRT*CFRAC                                                  QAL70780
      SXRT2=SSRT*DFRAC                                                  QAL70790
      SXRT3=SSRT*(1.-CFRAC-DFRAC)                                       QAL70800
      CXRT1=SXRT1*CTSSFD                                                QAL70810
      CXRT2=SXRT2*CTSSFD                                                QAL70820
      CXRT3=SXRT3*CTSSFD                                                QAL70830
      AXRT1=RSFD*CXRT1                                                  QAL70840
      AXRT2=RSFD*CXRT2                                                  QAL70850
      AXRT3=RSFD*CXRT3                                                  QAL70860
      WTSLUR=SLURLQ+SLURSL                                              QAL70870
      DO 6 I=1,N                                                        QAL70880
    6 U(I)=U(I)/A(I)                                                    QAL70890
      OXTOFL=OXTOFL+CAOHO*FAIDLM*AVCAO                                  QAL70900
      OXTOFC=OXTOFC+FAUFC+CRESLM                                        QAL70910
      OXTOFS=OXTOFS+FAUFS+SRESLM                                        QAL70920
      A102=OXTOFL-OXOFL                                                 QAL70930
      A101=.56464*FAIDLM                                                QAL70940
      CALL WRITEX                                                               
      GO TO 13                                                          QAL70960
      END                                                                       
      SUBROUTINE READX                                                          
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
  OMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      COMMON /RM/ LMAXM1,LMAXM2                                                 
      COMMON /RD/ LMAXD1,LMAXD2,LMAXD3                                          
      COMMON /RH/ LMAXH1,LMAXH2,LMAXH3,LMAXH4,LMAXH5                            
      INP = 5                                                                   
      IOU=6                                                                     
      DO 1 I=1,10                                                       QAL70970
    1 SPR(I)=0.                                                                 
C     INPUTS FOR COMBINED PROGRAMS                                      QAL70990
      READ (INP,103)N,NHIPR,NLOPR,NWASH,MWASH,IWASH,JWASH,KWASH         QAL71000
      IF (N-999999) 50,51,51                                                    
   50 M=N+1                                                                     
      WRITE(IOU, 403)N,NHIPR,NLOPR,NWASH,MWASH,IWASH,JWASH,KWASH         QAL7   
  403 FORMAT ('1 INPUT DATA'//8I15)                                             
      READ (INP,102)TPYAL,PDPHR,SHIPR,SLOPR,SMKUP,SUMLC,CRVR,OAR,TFILL, QAL71020
     1TSLLQ,TBX,TLIME,TMWO,TWW,TOFLO,Z101,TESTA,TESTB,TESTD,TESBP,TESCC,QAL71030
     1TESCN,TESTM,Z102,TKP,TEVFD,CPLIM,CPHYD,CPBX,CPMST,CPMUD,Z103,TESTKQAL71040
     1,TEST1,H2OSND,SWRSDA,FACDG,TCLN,FACND,STOIMP,TAA,THA,PISO,SILREA, QAL71050
     1SILQTZ,WFORC,XMOIS,P2O5,RFRCT,SAND,TTTK,TDIG,TFL(1),THTR(M),TFL(M)QAL71060
     1,EFFSLK,SLKFOC,SLKFCC,SLKFHC,SOLPH,RRVR,OXTOC,OXFAC,OXDEN,AFRAC,  QAL71070
     1BFRAC,CFRAC,DFRAC,FAIDF,CPLLQ,FATMP,FASC,PURE,OPFAC,RLTOP,CCLP,   QAL71080
     1PPTVL,PPTNO,CTSLP,ORENA,FDHYD,SAFAC,DGEXT,TMKUP,FASOL,PNAOH,SOLKP,QAL71090
     1CTSMK,DORWW,DORSOL,ASCLE,AKILN,AHAND,DILPC,DILSW,DILOXM,OXMSOL,   QAL71100
     1PRDIL,SUNAC,WFOXML,FAUTS,FAUTW,STLIQ,STBLO                        QAL71110
      WRITE(IOU,402)TPYAL,PDPHR,SHIPR,SLOPR,SMKUP,SUMLC,CRVR,OAR,TFILL, QAL71020
     1TSLLQ,TBX,TLIME,TMWO,TWW,TOFLO,Z101,TESTA,TESTB,TESTD,TESBP,TESCC,QAL71030
     1TESCN,TESTM,Z102,TKP,TEVFD,CPLIM,CPHYD,CPBX,CPMST,CPMUD,Z103,TESTKQAL71040
     1,TEST1,H2OSND,SWRSDA,FACDG,TCLN,FACND,STOIMP,TAA,THA,PISO,SILREA, QAL71050
     1SILQTZ,WFORC,XMOIS,P2O5,RFRCT,SAND,TTTK,TDIG,TFL(1),THTR(M),TFL(M)QAL71060
     1,EFFSLK,SLKFOC,SLKFCC,SLKFHC,SOLPH,RRVR,OXTOC,OXFAC,OXDEN,AFRAC,  QAL71070
     1BFRAC,CFRAC,DFRAC,FAIDF,CPLLQ,FATMP,FASC,PURE,OPFAC,RLTOP,CCLP,   QAL71080
     1PPTVL,PPTNO,CTSLP,ORENA,FDHYD,SAFAC,DGEXT,TMKUP,FASOL,PNAOH,SOLKP,QAL71090
     1CTSMK,DORWW,DORSOL,ASCLE,AKILN,AHAND,DILPC,DILSW,DILOXM,OXMSOL,   QAL71100
     1PRDIL,SUNAC,WFOXML,FAUTS,FAUTW,STLIQ,STBLO                                
  402 FORMAT (1H0,8F15.5)                                                       
      READ (INP,102)SETSF,STWFC,SOLS,CONST,CAUSC,TEMPC,HOLDC,FSRT,PISF, QAL71120
     1TRSOL,GPLAL,SHWFC,H5LB,CTSFA,CTOSR,HVAP,STAID,SLSOL,AVCAO,CAOHO,  QAL71130
     1FDSIL,FNONA,FCARB,CCSRT,CLDIL,CLNCC,UTIL,ANDYS,(U(I),I=1,N),(A(I),QAL71140
     1I=1,N),(WSTFAC(I),I=1,NWASH),(SOL(I),I=1,NWASH),(FLR(I),I=1,NWASH)QAL71150
      WRITE(IOU,402)SETSF,STWFC,SOLS,CONST,CAUSC,TEMPC,HOLDC,FSRT,PISF, QAL71120
     1TRSOL,GPLAL,SHWFC,H5LB,CTSFA,CTOSR,HVAP,STAID,SLSOL,AVCAO,CAOHO,  QAL71130
     1FDSIL,FNONA,FCARB,CCSRT,CLDIL,CLNCC,UTIL,ANDYS,(U(I),I=1,N),(A(I),QAL71140
     1I=1,N),(WSTFAC(I),I=1,NWASH),(SOL(I),I=1,NWASH),(FLR(I),I=1,NWASH)QAL71150
  102 FORMAT (8F10.0)                                                   QAL71160
  103 FORMAT (8I10)                                                     QAL71170
      READ (INP,500) LMAXM1,LMAXM2,LMAXD1,LMAXD2,LMAXD3,LMAXH1,LMAXH2,          
     1LMAXH3,LMAXH4,LMAXH5                                                      
  500 FORMAT (10I8)                                                             
      DO 44 I=1,N                                                       QAL71180
   44 U(I)=U(I)*A(I)                                                    QAL71190
      XN=N                                                              QAL71200
      RETURN                                                            QAL71210
   51 STOP                                                                      
      END                                                                       
      SUBROUTINE PPTR                                                           
C  CALCULATION OF MATERIAL BALANCE ON PRECIPITATION SECTION                     
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION U(10)                                                           
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
C     PRELIMINARY CALCULATIONS                                          QAL71220
      APROD=TPYAL*PURE/(365.*OPFAC)                                     QAL71230
      ASOL=.03*APROD                                                    QAL71240
    3 AREQD=APROD/OAR                                                   QAL71250
      BXREQ=AREQD/TAA                                                   QAL71260
      OXIN=OXFAC*WFORC*BXREQ                                            QAL71270
      OXKLN=PISO*APROD/PURE                                             QAL71280
      OXLIQ=OXTOC*SUMLC                                                 QAL71290
      OXSLK=OXIN-OXKLN-OXLIQ                                            QAL71300
      RFIN=.350                                                         QAL71310
      OSLKFO=OXSLK/EFFSLK                                               QAL71320
      OSLKFL=1000.*OXDEN*OSLKFO/SLKFOC                                  QAL71330
      OSLKFC=SLKFCC*OSLKFO/SLKFOC                                       QAL71340
      OSLKFH=SLKFHC*OSLKFO/SLKFOC                                       QAL71350
  522 OSLKFA=RFIN*OSLKFC                                                QAL71360
      OSLKFS=OSLKFC/CTSLP                                               QAL71370
      ALMNA=APROD/PURE                                                  QAL71380
      ALUMNA=ALMNA                                                      QAL71390
      AKSTK=AKILN*APROD                                                 QAL71400
      ALOAD=AHAND*APROD                                                 QAL71410
      ASCL=ASCLE*APROD                                                  QAL71420
      PDIMP=ALUMNA-APROD                                                QAL71430
      DSTIMP=(AKSTK+ALOAD)*(1.-PURE)/PURE                               QAL71440
      AKFD=APROD+AKSTK+ALOAD                                            QAL71450
      BOUND=.5300902*AKFD                                               QAL71460
      DORIMP=PDIMP+DSTIMP                                               QAL71470
      XFSOL=DORIMP+BOUND+AKFD                                           QAL71480
      CKMOI=XFSOL*(100.-DORSOL)/DORSOL                                  QAL71490
      TOTMO=BOUND+CKMOI                                                 QAL71500
      REACOC=OXSLK+OSLKFC                                               QAL71510
      WWDOR=DORWW*ALUMNA                                                QAL71520
      ORESDM=1.710061*ORENA*ALUMNA                                      QAL71530
      ORESDA=ORESDM-OXKLN                                               QAL71540
      OREC=ORESDA*CTSLP                                                 QAL71550
      ORECRB=ORESDA-OREC                                                QAL71560
      DLTOP=.8710+.22*RLTOP+.0013725*CCLP                               QAL71570
      DILT=DILPC+DILSW+WWDOR-CKMOI                                      QAL71580
  525 PPT=XFSOL+OSLKFH+1.5300902E0*(ASCL+ASOL)                          QAL71590
      APPT=AKFD+OSLKFA+ASCL+ASOL+OSLKFH/1.5300902E0                     QAL71600
      CPPT=OREC+OSLKFC                                                  QAL71610
      X1=.001*CCLP/DLTOP                                                QAL71620
      X2=DILT-PPT                                                       QAL71630
      X3=CAUSC*CCLP+TEMPC*TFILL+CONST                                   QAL71640
      X4=HOLDC*PPTVL*PPTNO*DLTOP*.10013438E0                            QAL71650
      X5=X1*(RLTOP-X3)                                                  QAL71660
      X6=APPT-X3*CPPT-X4*X1                                             QAL71670
      X7=X4*CPPT                                                        QAL71680
      XLTOP=((X6*X6-4.*X5*X7)**.5+X6)/(2.*X5)                           QAL71690
      CLTOP=X1*XLTOP                                                    QAL71700
      ALTOP=RLTOP*CLTOP                                                 QAL71710
      FLTOP=.166443*XLTOP/DLTOP                                         QAL71720
      SLTOP=CLTOP/CTSLP                                                 QAL71730
      XTTOF=XLTOP+X2                                                    QAL71740
      CTTOF=CLTOP-CPPT                                                  QAL71750
      ATTOF=ALTOP-APPT                                                  QAL71760
      WFTTOF=CTTOF/XTTOF                                                QAL71770
      RTTOF=ATTOF/CTTOF                                                 QAL71780
      ASLKF=RTTOF*OSLKFC                                                QAL71790
      IF (DABS(ASLKF-OSLKFA)-TEST1)520,521,521                                  
  521 RFIN=RTTOF                                                        QAL71810
      GO TO 522                                                         QAL71820
  520 CCTTO=CC3(WFTTOF,RTTOF)                                           QAL71830
      ASTTO=GPLAL*CTTOF/CCTTO                                           QAL71840
      IF (DABS(ASOL-ASTTO)-TEST1)523,524,524                                    
  524 ASOL=ASTTO                                                        QAL71860
      HYSOL=1.5300902E0*ASOL                                            QAL71870
      GO TO 525                                                         QAL71880
  523 DTTOF=.001*CCTTO/WFTTOF                                           QAL71890
      STTOF=SLTOP+ORESDA-OSLKFS                                         QAL71900
      FTTOF=.166443*(XTTOF/DTTOF+.63148584E0*ASOL)                      QAL71910
      DEN=1./(1./DLTOP+.0002*(TFILL-77.))                               QAL71920
      HOURS=.1001344*DEN*PPTVL*PPTNO/XLTOP                              QAL71930
      TTOFT=Z101+Z102*TFILL+Z103*HOURS                                  QAL71940
      CPTTO=CPF(CCTTO,RTTOF)                                            QAL71950
      RETURN                                                            QAL71960
      END                                                                       
      SUBROUTINE SPTLIQ                                                         
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION U(10)                                                           
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      OXCAO=.52909154E0*OXSLK                                           QAL71970
      REACAO=STOIMP*OXCAO                                               QAL71980
      REACLM=REACAO/AVCAO                                               QAL71990
      REACSL=(1.+.32125535E0*AVCAO)*REACLM                              QAL72000
      XRLIM=CAOHO*REACAO                                                QAL72010
      CAOX=1.2085892E0*OXSLK                                            QAL72020
      REACMD=REACSL+.50952*OXSLK+OSLKFH+.82353*OSLKFA                   QAL72030
      OXLMLQ=XRLIM+REACLM-REACSL                                        QAL72040
      REACOC=OXSLK+OSLKFC                                               QAL72050
      REACOL=OSLKFL+OSLKFH+OXLMLQ+REACSL-REACMD                         QAL72060
      REACOS=OSLKFS+OXSLK                                               QAL72070
      OXMDLQ=REACMD*(100.-OXMSOL)/OXMSOL                                QAL72080
      OXMDC=WFOXML*OXMDLQ                                               QAL72090
      OXMDS=OXMDC*REACOS/REACOC                                         QAL72100
      OXTOFL=REACOL-OXMDLQ+DILOXM*REACMD                                QAL72110
      OXTOFC=REACOC-OXMDC                                               QAL72120
      WFOXTO=OXTOFC/(OXTOFL-XRLIM)                                      QAL72130
      OXTOCC=1021.2*WFOXTO/(1.-1.139*WFOXTO)                            QAL72140
      OXTOFS=REACOS-OXMDS                                               QAL72150
      FAIDLM=FAIDF*APROD                                                QAL72160
      FAID=FAIDLM*(1.+.32126*AVCAO)                                     QAL72170
      TOTFAC=0.                                                         QAL72180
      TOTFAS=TOTFAC/CTSFA                                               QAL72190
      XIN=CAOHO*FAIDLM*AVCAO                                            QAL72200
      FAUFL=XIN+FAIDLM-FAID                                             QAL72210
      FAUFC=XIN*OXTOFC/(OXTOFL-XRLIM)                                   QAL72220
      FAUFS=XIN*OXTOFS/(OXTOFL-XRLIM)                                   QAL72230
      OXTOFL=OXTOFL-XIN                                                 QAL72240
      OXTOFC=OXTOFC-FAUFC                                               QAL72250
      OXTOFS=OXTOFS-FAUFS                                               QAL72260
      WF=FAUFC/FAUFL                                                    QAL72270
      FALQCC=1021.2*WF/(1.-1.139*WF)                                    QAL72280
      CPFALQ=CPLLQ                                                      QAL72290
      CTSFA=FAUFC/FAUFS                                                 QAL72300
      FATMP=TLIME                                                       QAL72310
      FALQDN=FALQCC/(1000.*WF)                                          QAL72320
      FAUFFL=.166443*(FAUFL/FALQDN+FAID/2.2)                            QAL72330
      DENMK=1.113-.0003889*TMKUP+.00875*PNAOH                           QAL72340
      XMKUP=75.475*SMKUP/PNAOH                                          QAL72350
      CMKUP=SMKUP*CTSMK                                                 QAL72360
      RMKUP=ASCL/CMKUP                                                  QAL72370
      DMKUP=.97598+.095*RMKUP+.0012*CLNCC                               QAL72380
      CLNL=1000.*DMKUP*CMKUP/CLNCC                                      QAL72390
      CPCLN=CPF(CLNCC,RMKUP)                                            QAL72400
      FSMKP=.166443*CLNL/DMKUP                                          QAL72410
      CLNDIL=CLNL-XMKUP-1.5300902E0*ASCL                                QAL72420
      XSPL=XTTOF+CLNL                                                   QAL72430
      ASPL=ATTOF+ASCL                                                   QAL72440
      CSPL=CTTOF+CMKUP                                                  QAL72450
      SSPL=CTTOF/CTSLP+SMKUP                                            QAL72460
      CTSSPL=CSPL/SSPL                                                  QAL72470
      RSPL=ASPL/CSPL                                                    QAL72480
      WFSPL=CSPL/XSPL                                                   QAL72490
      CCSPL=CC3(WFSPL,RSPL)                                             QAL72500
      CPSPL=CPF(CCSPL,RSPL)                                             QAL72510
      TSPL=XTTOF*CPTTO*(TTOFT-32.)                                      QAL72520
      TSPL=(TSPL+CLNL*CPCLN*(TCLN-32.))/(XSPL*CPSPL)+32.                QAL72530
      DSPL=.001*CCSPL*XSPL/CSPL                                         QAL72540
      FLOSPL=.166443*(XSPL/DSPL+.63148584E0*ASOL)                       QAL72550
C     EVAPORATION                                                       QAL72560
      EVAP=PDPHR*.012                                                   QAL72570
      SHTWL=SHWFC*EVAP                                                  QAL72580
      CHTWL=SHTWL*CTSSPL                                                QAL72590
      AHTWL=RSPL*CHTWL                                                  QAL72600
      XHTWL=CHTWL/WFSPL                                                 QAL72610
C     TEST TANK CONDITIONS                                              QAL72620
      XTTK=XSPL-EVAP+1.5300902E0*ASOL-XHTWL                             QAL72630
 18   ATTK=ASPL+ASOL-AHTWL                                              QAL72640
      CTTK=CSPL-CHTWL                                                   QAL72650
      STTK=SSPL-SHTWL                                                   QAL72660
      RTTK=ATTK/CTTK                                                    QAL72670
      WFTTK=CTTK/XTTK                                                   QAL72680
      CCTTK=CC3(WFTTK,RTTK)                                             QAL72690
      DTTK=CCTTK/(1000.*WFTTK)                                          QAL72700
      FTTK=XTTK*.166443/DTTK                                            QAL72710
      CPTTK=CPF(CCTTK,RTTK)                                             QAL72720
C     FLASH CALCULATIONS                                                QAL72730
      QSPL=(XSPL*CPSPL+1.52941*ASOL*CPHYD)*(TEVFD-TSPL)                 QAL72740
      CPLP=CPF(CCLP,RLTOP)                                              QAL72750
      BPRLP=BPF(CCLP,TFILL)                                             QAL72760
      HVLP=1067.90+.39*TFILL+.085*BPRLP                                 QAL72770
      THI=TKP-.96*(TEVFD-TSPL)                                          QAL72780
      RETURN                                                            QAL72790
      END                                                                       
      SUBROUTINE FLASH                                                          
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
C     ENTER LOOP B (ASSUME LIQ. TEMP OUT OF HID FLASH TANKS)            QAL72800
 19   CPHI=CPLP+.000071*(THI-TFILL)                                     QAL72810
C     ENTER LOOP A (ASSUME CP OF LIO. OUT OF FLASH TANKS)               QAL72820
 20   T=CPHI*(THI-32.)                                                  QAL72830
      STBC=XLTOP*(T-CPLP*(TFILL-32.))/(HVLP-T)                          QAL72840
      XBC=XLTOP+STBC                                                    QAL72850
      WFBC=CLTOP/XBC                                                    QAL72860
      CCBC=CCF(WFBC,RLTOP)                                              QAL72870
      CPHI1=CPF(CCBC,RLTOP)                                             QAL72880
      IF (DABS(CPHI1-CPHI)-TESTA)24,23,23                                       
 23   CPHI=CPHI1                                                        QAL72900
      GO TO 20                                                          QAL72910
   24 BPRHI=BPF(CCBC,THI)                                               QAL72920
      HVHI=1067.9+.39*THI+.085*BPRHI                                    QAL72930
      HFHI=1.003*(THI-BPRHI)-32.61                                      QAL72940
C     ENTER LOOP C (ASSUME CP ENTERING HID FLASH TANKS)                 QAL72950
      CPKP=CPLP+.000071*(TKP-TFILL)                                     QAL72960
 25   V=CPKP*(TKP-32.)                                                  QAL72970
      STHI=XBC*(V-T)/(HVHI-V)                                           QAL72980
      XKP=STHI+STBC+XLTOP                                               QAL72990
      WFKP=CLTOP/XKP                                                    QAL73000
      CCKP=CCF(WFKP,RLTOP)                                              QAL73010
      CPKP1=CPF(CCKP,RLTOP)                                             QAL73020
      IF (DABS(CPKP1-CPKP)-TESTA)29,28,28                                       
 28   CPKP=CPKP1                                                        QAL73040
      GO TO 25                                                          QAL73050
 29   QHI=STHI*(HVHI-HFHI)                                              QAL73060
      THI1=TKP-QSPL/QHI*(TKP-THI)                                       QAL73070
      IF (DABS(THI-THI1)-TESTB)33,32,32                                         
 32   THI=THI1                                                          QAL73090
      GO TO 19                                                          QAL73100
   33 TCBC=TFILL-BPRLP                                                  QAL73110
      PABC=PRES(TCBC)                                                   QAL73120
      TCHI=THI-BPRHI                                                    QAL73130
      PAHI=PRES(TCHI)                                                   QAL73140
      RETURN                                                            QAL73150
      END                                                                       
      SUBROUTINE SETTLE                                                         
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      DKP=CCKP/WFKP*.001                                                QAL73160
      FKP=XKP*.166443/DKP                                               QAL73170
      FASC=1.4946*FAIDLM                                                QAL73180
      XKPMD=FASC*(100.-SOLKP)/SOLKP                                     QAL73190
      CKPMD=WFKP*XKPMD                                                  QAL73200
      AKPMD=RLTOP*CKPMD                                                 QAL73210
      SKPMD=CKPMD/CTSLP                                                 QAL73220
      SSWR=SWRSDA*APROD                                                 QAL73230
      CSWR=CTSLP*SSWR                                                   QAL73240
      ASWR=RLTOP*CSWR                                                   QAL73250
      XSWR=CSWR/WFKP                                                    QAL73260
      XKPFD=XKP+XSWR-PRDIL+XKPMD+(.4946-.32126*AVCAO)*FAIDLM            QAL73270
      CKPFD=CLTOP+CKPMD+CSWR                                            QAL73280
      AKPFD=ALTOP+AKPMD+ASWR+.56464*FAIDLM                              QAL73290
      SKPFD=SLTOP+SKPMD+SSWR                                            QAL73300
      XPFTK=XKPFD-FAUFL                                                 QAL73310
      CPFTK=CKPFD-FAUFC                                                 QAL73320
      SPFTK=SKPFD-FAUFS                                                 QAL73330
      CTSPFD=CPFTK/SPFTK                                                QAL73340
      APFTK=AKPFD                                                       QAL73350
      RPFTK=APFTK/CPFTK                                                 QAL73360
      CUNAC=SUNAC*CTSPFD                                                QAL73370
      AUNAC=RPFTK*CUNAC                                                 QAL73380
      XUNAC=CUNAC*XPFTK/CPFTK                                           QAL73390
      XLSO=XPFTK+XUNAC                                                  QAL73400
      CLSO=CPFTK+CUNAC                                                  QAL73410
      ALSO=APFTK+AUNAC                                                  QAL73420
      SLSO=SPFTK+SUNAC                                                  QAL73430
      WFLSO=CLSO/XLSO                                                   QAL73440
      RLSO=ALSO/CLSO                                                    QAL73450
      CCLSO=CCF(WFLSO,RLSO)                                             QAL73460
      DLSO=CCLSO/WFLSO*.001                                             QAL73470
      FLSO=XLSO*.166443/DLSO                                            QAL73480
      RETURN                                                            QAL73490
      END                                                                       
      SUBROUTINE BAUX2                                                          
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      BXMST=BXREQ*(XMOIS/(100.-XMOIS))                                          
      DGLIM=(STAID+1.5802*P2O5/AVCAO)*BXREQ                             QAL73510
      XLIME=CAOHO*DGLIM*AVCAO                                           QAL73520
      DLMUD=DGLIM*(1.+AVCAO*.32125535E0)                                QAL73530
      SIL=SILREA+SILQTZ*RFRCT                                           QAL73540
      XLMSL=XLIME-DLMUD+DGLIM                                           QAL73550
      BXSPL=(BXREQ+DLMUD)*(100.-SLSOL)/SLSOL-BXMST-XLMSL                QAL73560
      CDSIL=SIL*FDSIL*BXREQ                                             QAL73570
      CNONA=FNONA*BXREQ                                                 QAL73580
      CCARB=FCARB*BXREQ                                                 QAL73590
      CDLOS=CDSIL+CNONA+CCARB                                           QAL73600
      AEXLS=(1.-DGEXT)*TAA*BXREQ                                        QAL73610
      DGMUD=(1.-AVCAO)*DGLIM/BXREQ+1.5802*P2O5+1.32125535E0*STAID*AVCAO QAL73620
      DGMUD=(1.5300902E0*THA+1.1766967E0*(TAA-THA))*DGEXT-DGMUD         QAL73630
      DGMUD=(1.-DGMUD)*BXREQ+.58477447E0*CDSIL-FDHYD*BXREQ              QAL73640
      PISOM=SAND*BXREQ                                                  QAL73650
      AUTSA=FAUTS*AEXLS                                                 QAL73660
      AUTSH=1.52941*AUTSA                                               QAL73670
      SSFMD=DGMUD-PISOM+AUTSH                                           QAL73680
      SOLPHS=SOLPH*SSFMD                                                QAL73690
      SOLPHC=SOLPHS*CTSSFD                                              QAL73700
      FLRS=SETSF*SSFMD                                                  QAL73710
      XSFLR=STLIQ*FLRS                                                  QAL73720
      CSFLR=STWFC*XSFLR                                                 QAL73730
      SSFLR=CSFLR/CTSMK                                                 QAL73740
      SUFM=SSFMD+FLRS+SOLPHS                                            QAL73750
      SUFL=SUFM*(100.-SOLS)/SOLS                                        QAL73760
      SUFC=WFLSO*SUFL                                                   QAL73770
      SUFA=RLSO*SUFC                                                    QAL73780
      SUFS=SUFC/CTSPFD                                                  QAL73790
      FSUF=.166443*SUFL/DLSO+.04702*SUFM                                QAL73800
      XSFD=XLSO+SUFL+AUTSH-XSFLR+SOLPHS                                 QAL73810
      CSFD=CLSO+SUFC+SOLPHC-CSFLR                                       QAL73820
      SSFD=SLSO+SUFS+SOLPHS-SSFLR                                       QAL73830
      CTSSFD=CSFD/SSFD                                                  QAL73840
      ASFD=ALSO+SUFA+AUTSA                                              QAL73850
      WFSFD=CSFD/XSFD                                                   QAL73860
      RSFD=ASFD/CSFD                                                    QAL73870
      XMSFD=DGMUD-PISOM                                                 QAL73880
      CCSFD=CCF(WFSFD,RSFD)                                             QAL73890
      DSFD=CCSFD/WFSFD*.001                                             QAL73900
      FSFD=.166443*XSFD/DSFD+.04702*XMSFD                               QAL73910
      SPISO=PISF*PISOM                                                          
      CPISO=SPISO*CTSSFD                                                QAL73930
      APISO=RSFD*CPISO                                                  QAL73940
      XPISO=PISOM*(100.-ANDYS)/ANDYS                                    QAL73950
      XTRUF=PISOM*(100.-TRSOL)/TRSOL                                    QAL73960
      CTRUF=WFSFD*XTRUF                                                 QAL73970
      ATRUF=RSFD*CTRUF                                                  QAL73980
      STRUF=CTRUF/CTSSFD                                                        
      SNDC=CTRUF-CPISO                                                  QAL74000
      SNDA=ATRUF-APISO                                                  QAL74010
      PIDIL=H2OSND*PISOM                                                QAL74020
      SNDL=PIDIL+XTRUF-XPISO                                            QAL74030
      DLPIS=STRUF-SPISO                                                 QAL74040
      CSRT=FSRT*CCSRT/166.443                                           QAL74050
      ASRT=RSFD*CSRT                                                    QAL74060
      XSRT=CSRT/WFSFD                                                   QAL74070
      SSRT=CSRT/CTSSFD                                                  QAL74080
      DILSR=(1.+.001*CCSRT/CTSSFD)*FSRT/.166443-XSRT                    QAL74090
      RETURN                                                            QAL74100
      END                                                                       
      SUBROUTINE BLO                                                            
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      IOU = 6                                                                   
C     WASHER STARCH                                                     QAL74110
      EQTAA=TAA                                                         QAL74120
      EQTHA=THA                                                         QAL74130
      TOWSTC=0.                                                         QAL74140
      TOWSTS=0.                                                         QAL74150
      TOTWST=0.                                                         QAL74160
      TOWSTL=0.                                                         QAL74170
      DO 530 I=1,NWASH                                                  QAL74180
      WSTCH(I)=WSTFAC(I)*SUFM                                           QAL74190
      TOTWST=WSTCH(I)+TOTWST                                            QAL74200
      XWSHST(I)=STLIQ*WSTCH(I)                                          QAL74210
      CWSHST(I)=XWSHST(I)*STWFC                                         QAL74220
      SWSHST(I)=CWSHST(I)/CTSMK                                         QAL74230
      TOWSTL=TOWSTL+XWSHST(I)                                           QAL74240
      TOWSTC=TOWSTC+CWSHST(I)                                           QAL74250
  530 TOWSTS=TOWSTS+SWSHST(I)                                           QAL74260
C     WASHER LIME                                                       QAL74270
      C150=SPISO-CPISO                                                  QAL74280
      C151=SHTWL-CHTWL                                                  QAL74290
      C152=SSWR-CSWR                                                    QAL74300
      C153=SUNAC-CUNAC                                                  QAL74310
      C154=CRVR*(1./CTOSR)-CRVR                                         QAL74320
      C155=OXMDS-OXMDC                                                  QAL74330
      C156=SOLPHS-SOLPHC                                                QAL74340
      CARBO=ORECRB+C150+C151+C152+C153+C154+C155+C156                   QAL74350
      C157=TOTFAS-TOTFAC                                                QAL74360
      C158=SSFLR-CSFLR                                                  QAL74370
      C159=TOWSTS-TOWSTC                                                QAL74380
      C160=SMKUP-CMKUP                                                  QAL74390
      CARBI=CCARB+C157+C158+C159+C160                                   QAL74400
      CAUST=CARBI-CARBO                                                 QAL74410
      WLIME=.5290915*CAUST/(AVCAO*UTIL)-STAID*BXREQ                     QAL74420
      XWLIM=CAOHO*WLIME*AVCAO                                           QAL74430
      TLMH2O=XWLIM+XRLIM+XLIME                                          QAL74440
      H100=OXTOFL-TLMH2O                                                QAL74450
      IF (DABS(H100)-TEST1)532,532,531                                          
  531 IF (H100)532,532,533                                              QAL74470
  532 DILLIM=-H100                                                      QAL74480
      H101=DGLIM/(DGLIM+WLIME)                                          QAL74490
      H102=WLIME/(DGLIM+WLIME)                                          QAL74500
      CDLIM=H101*OXTOFC                                                 QAL74510
      SDLIM=H101*OXTOFS                                                 QAL74520
      CWLIM=H102*OXTOFC                                                 QAL74530
      SWLIM=H102*OXTOFS                                                 QAL74540
      OXOFL=0.                                                          QAL74550
      OXOFC=0.                                                          QAL74560
      OX0FS=0.                                                          QAL74570
      GO TO 534                                                         QAL74580
  533 OXOFL=H100                                                        QAL74590
      H103=H100+XWLIM+XLIME                                             QAL74600
      CDLIM=XLIME*OXTOFC/H103                                           QAL74610
      SDLIM=XLIME*OXTOFS/H103                                           QAL74620
      CWLIM=XWLIM*OXTOFC/H103                                           QAL74630
      SWLIM=XWLIM*OXTOFS/H103                                           QAL74640
      OXOFC=H100*OXTOFC/H103                                            QAL74650
      OXOFS=H100*OXTOFS/H103                                            QAL74660
      GO TO 534                                                         QAL74670
  534 WLIMD=WLIME*(1.+.32125535E0*AVCAO)                                QAL74680
      WLMLQ=WLIME+XWLIM-WLIMD                                           QAL74690
      XBLO=XSFD+XSRT-CLDIL+XTRUF                                        QAL74700
      CBLO=CSFD+CSRT+CTRUF                                              QAL74710
      ABLO=ASFD+ASRT+ATRUF                                              QAL74720
      SBLO=SSFD+CSRT/CTSSFD+STRUF                                       QAL74730
      WFBLO=CBLO/XBLO                                                   QAL74740
      RBLO=ABLO/CBLO                                                    QAL74750
      CCBLO=CCF(WFBLO,RBLO)                                             QAL74760
      DBLO=CCBLO/WFBLO*.001                                             QAL74770
      FBLO=.166443*XBLO/DBLO+.04702*DGMUD                               QAL74780
      ZBLO=(CCBLO-180.)/40.                                             QAL74790
      A100=2.86+.1125*ZBLO-.6375*ZBLO*ZBLO                              QAL74800
      A101=.0233+.00945*ZBLO+.00305*ZBLO*ZBLO                           QAL74810
      CPBLO=CPF(CCBLO,RBLO)                                             QAL74830
      BPRBL=(A100+212.*A101)/(1.-A101)                                  QAL74820
      HBLO=1150.4+.5*BPRBL                                              QAL74840
      TBLO=212.+BPRBL                                                   QAL74850
      AUTWA=FAUTW*AEXLS                                                 QAL74860
      AUTWH=1.52941*AUTWA                                               QAL74870
      SWFD=SUFS+SWLIM                                                   QAL74880
      CDIG=CTTK-CDLOS+CDLIM                                             QAL74890
      ADIG=ATTK+AREQD-AEXLS                                             QAL74900
      RDIG=ADIG/CDIG                                                    QAL74910
      SDIG=STTK-CDSIL-CNONA+SDLIM                                       QAL74920
      CTSDIG=CDIG/SDIG                                                  QAL74930
C  C  DIGESTER                                                          QAL74940
      XHTR=XTTK-BXSPL                                                   QAL74950
      XLIQ=XHTR                                                         QAL74960
      THTR(1)=TTTK                                                      QAL74970
      AMWOA=ABLO-ADIG                                                   QAL74980
      CMWOA=CBLO-CDIG                                                   QAL74990
      RMWOA=AMWOA/CMWOA                                                 QAL75000
      SMWOA=SBLO-SDIG                                                   QAL75010
      A101=(XTTK-BXSPL)*CPTTK                                           QAL75030
      A102=XTTK+BXREQ+BXMST+DGLIM+XLIME-DGMUD                           QAL75040
      A103=BXSPL*CPTTK                                                  QAL75050
      CTSWO=CMWOA/SMWOA                                                 QAL75020
      A104=BXREQ*CPBX+BXMST*CPMST                                       QAL75060
      A105=DGLIM*CPLIM+XLIME*CPLLQ                                      QAL75070
      A106=(310.*EQTHA+126.*(EQTAA-EQTHA))*DGEXT*BXREQ                  QAL75080
      A107=A103+A104+A105                                               QAL75090
      A108=A103*TSLLQ+A104*TBX+A105*TLIME-A106                          QAL75100
      SUMUA=0.                                                          QAL75110
      DO 540 I=1,N                                                      QAL75120
  540 SUMUA=SUMUA+U(I)                                                  QAL75130
      F10=SUMUA/XTTK                                                    QAL75140
      FACTK=.0002*(975.-F10)                                            QAL75150
 4    SPR(NHIPR)=SHIPR                                                  QAL75160
      SPR(NLOPR)=SLOPR                                                  QAL75170
      RETURN                                                            QAL75180
      END                                                                       
      SUBROUTINE HEATER                                                         
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      COMMON /RH/ LMAXH1,LMAXH2,LMAXH3,LMAXH4,LMAXH5                            
      IOU=6                                                                     
      LCNT4 = 0                                                                 
      LCNT5 = 0                                                                 
    1 KQ=1                                                              QAL75830
    2 HFFL(1)=0.                                                        QAL75840
      XN=N                                                              QAL75850
      XW2=0.                                                            QAL75860
      COND=0.                                                           QAL75870
C     SET UP SWITCHES FOR TAKEOFF STREAMS                               QAL75880
C     SET STAGE INDEX K                                                 QAL75890
      K=1                                                               QAL75900
      IS1=1                                                             QAL75910
      IS2=1                                                             QAL75920
C     SET FLASH INDEX I                                                 QAL75930
 91   I=K                                                               QAL75940
      L=I+1                                                             QAL75950
C     SET HEATER INDEX J                                                QAL75960
      J=N-I+1                                                           QAL75970
C     EST.FLASH EFFLUENT TEMP                                           QAL75980
      TFL(I+1)=TFL(I)-(TFL(1)-TFL(M))/XN                                QAL75990
      IPREV=1                                                           QAL76000
      IFIRS=1                                                           QAL76010
      LCNT1 = 0                                                                 
      LCNT2 = 0                                                                 
      LCNT3 = 0                                                                 
C     EST CAUSTIC CONC                                                  QAL76020
 83   CCFL(L)=CCFL(I)+.266*(TFL(I)-TFL(L))                              QAL76030
 76   CPFL(L)=CPF(CCFL(L),RDIG)                                         QAL76040
      BPE(L)=BPF(CCFL(L),TFL(L))                                        QAL76050
      TCND(L)=TFL(L)-BPE(L)                                             QAL76060
      T=TCND(L)                                                         QAL76070
      PR(I)=PF(T)                                                       QAL76080
      HGFL(I)=1031.7+.71409091E0*TCND(L)-.00073140496E0*TCND(L)**2      QAL76090
      HGFL(I)=HGFL(I)+(.47725947E0+.0010659834E0*PR(I)-7.8116343E-7*PR(IQAL76100
     1)**2)*BPE(L)                                                      QAL76110
      HFFL(L)=(.8943181+.23429752E-3*T)*T-19.96                         QAL76120
      T=(TFL(I)-32.)*CPFL(I)                                            QAL76130
      T1=(TFL(L)-32.)*CPFL(L)                                           QAL76140
      SS=(XFL(I)*(T-T1)+DGMUD*CPMUD*(TFL(I)-TFL(L)))/(HGFL(I)-T1)       QAL76150
      XFL(L)=XFL(I)-SS                                                  QAL76160
      WFFL(L)=CDIG/XFL(L)                                               QAL76170
      CC=CCF(WFFL(L),RDIG)                                              QAL76180
      LCNT1= LCNT1 + 1                                                          
      IF (DABS(CC-CCFL(L))-TESCC) 80,79,79                                      
   79 IF (LCNT1 - LMAXH1) 300,300,500                                           
  300 CCFL(L) = CC                                                              
      GO TO 76                                                                  
  500 WRITE (IOU,501) CC,CCFL(L)                                                
  501 FORMAT (' LCNT1 IN HEATER EXCEEDED,  CC = ',G13.6,'  CCFL(L) = ',         
     1G13.6)                                                                    
      LCNT1 = 0                                                                 
 80   IF (U(J))800,801,800                                              QAL76220
 801  IF (DABS(SS-SPR(L))-FACND)802,803,803                                     
 802  THTR(J)=THTR(J+1)                                                 QAL76240
      HFFL(L)=HFFL(L-1)                                                 QAL76250
      STFL(L)=0.                                                        QAL76260
      GO TO 89                                                          QAL76270
 803  TFL(L)=TFL(L)+(SS-SPR(L))*(TFL(I)-TFL(L))/SS                      QAL76280
      LCNT2 = LCNT2 + 1                                                         
      IF (LCNT2 - LMAXH2) 83,83,502                                             
  502 WRITE (IOU,503) SS,SPR(L)                                                 
  503 FORMAT (' LCNT2 IN HEATER EXCEEDED, SS = ',G13.6,'  SPR(L) = ',           
     1G13.6)                                                                    
      LCNT2 = 0                                                                 
      GO TO 802                                                                 
 800  STFL(L)=SS-SPR(L)                                                 QAL76300
      T=STFL(L)                                                         QAL76310
      TH=T*(HGFL(I)-HFFL(L))+COND*(HFFL(I)-HFFL(L))                     QAL76320
      TH=THTR(J+1)-TH/(XLIQ*CPTTK)                                      QAL76330
      THTR(J)=TH                                                        QAL76340
      IF (IS1)245,71,245                                                QAL76350
 245  DT=THTR(J)-TSLLQ                                                  QAL76360
C     DETERMINE WHETHER TO TAKE OUT SPENT LIQUOR STREAM                 QAL76370
      IF (DT)244,235,235                                                QAL76380
 235  ISN=1                                                             QAL76390
      IF (IFIRS)236,237,236                                             QAL76400
 236  IFIRS=0                                                           QAL76410
      GO TO 238                                                         QAL76420
 237  IF (IPREV)239,239,238                                             QAL76430
 238  XW4=0.                                                            QAL76440
      GO TO 240                                                         QAL76450
 239  XWN=BXSPL*(TSLLQ-THTR(J))/(THTR(J+1)-THTR(J))                     QAL76460
      XW4=.5*(XWN+XW4)                                                  QAL76470
      GO TO 240                                                         QAL76480
 244  ISN=-1                                                            QAL76490
      IF (IFIRS)242,241,242                                             QAL76500
 241  IF (IPREV)243,243,239                                             QAL76510
 242  IFIRS=0                                                           QAL76520
 243  XW4=BXSPL*(TSLLQ-THTR(J))/(THTR(J+1)-THTR(J))                     QAL76530
 240  IPREV=ISN                                                         QAL76540
      XLIQ=XW4+XHTR                                                     QAL76550
      GO TO 85                                                          QAL76560
 71   IF (IS2)74,85,74                                                  QAL76570
C     GET COOLER TAKEOFF STREAM                                         QAL76580
 74   XW2=BXSPL-XW4                                                     QAL76590
      IS2=0                                                             QAL76600
      XLIQ=XW4+XHTR+XW2                                                 QAL76610
 85   XK=DEXP(U(J)/(XLIQ*CPTTK)*.012)                                           
      TC=(THTR(J)-THTR(J+1)*XK)/(1.-XK)                                 QAL76630
      IF (DABS(TC-TCND(L))-TESCN)252,88,88                                      
 88   TFL(L)=TC+FACND*(TCND(L)-TC)+BPE(L)                               QAL76650
      LCNT3 = LCNT3 + 1                                                         
      IF (LCNT3 - LMAXH3) 83,83,504                                             
  504 WRITE (IOU,505) TC,TCND(L)                                                
  505 FORMAT (' LCNT3 IN HEATER EXCEEDED,  TC = ',G13.6,'  TCND(L) = ',         
     1G13.6)                                                                    
      LCNT3 = 0                                                                 
 252  IF (XW4)251,250,251                                               QAL76670
 250  IS1=1                                                             QAL76680
      GO TO 89                                                          QAL76690
 251  IS1=0                                                             QAL76700
   89 IF (K-N)90,202,202                                                QAL76710
 90   K=K+1                                                             QAL76720
      COND=COND+STFL(L)                                                 QAL76730
      GO TO 91                                                          QAL76740
C     SUM TOTAL FLASH                                                   QAL76750
 202  IF (DABS(THTR(1)-TTTK)-TESTK)92,203,203                                   
 203  THTR(M)=THTR(M)-FACTK*(THTR(1)-TTTK)                              QAL76770
      LCNT4 = LCNT4 + 1                                                         
      IF (LCNT4 - LMAXH4) 506,506,507                                           
  507 WRITE (IOU,508) THTR(1),TTTK                                              
  508 FORMAT (' LCNT4 IN HEATER EXCEEDED,  THTR(1) = ',G13.6,'  TTTK =',        
     1G13.6)                                                                    
      LCNT4 = 0                                                                 
      GO TO 92                                                                  
  506 CALL DIG2                                                         QAL76780
      GO TO 1                                                           QAL76790
 92   XWOF=XBLO+STBLO-XFL(M)                                            QAL76800
      WFWOF=CMWOA/XWOF                                                  QAL76810
      CCWOF=CC2(WFWOF,RMWOA)                                            QAL76820
      CPWOF=.9870-.0009*CCWOF                                           QAL76830
      QINBL=(CPFL(M)*XFL(M)+CPMUD*DGMUD)*(TFL(M)-32.)                   QAL76840
      QINBL=QINBL+CPWOF*XWOF*(TMWO-32.)                                 QAL76850
      QOUT=(CPBLO*XBLO+CPMUD*DGMUD)*(TBLO-32.)+STBLO*HBLO               QAL76860
      DELQ=QINBL-QOUT                                                   QAL76870
      IF (DABS(DELQ)-TESTM)226,510,510                                          
  510 LCNT5 = LCNT5 + 1                                                         
      IF (LCNT5 - LMAXH5) 220,220,511                                           
  511 WRITE (IOU,512) DELQ                                                      
  512 FORMAT (' LCNT5 IN HEATER EXCEEDED, DELQ = ',G13.6)                       
 226  RETURN                                                            QAL76890
 220  GO TO (221,225,222),KQ                                            QAL76900
 221  IF (DELQ)223,223,224                                              QAL76910
C     DELQ IS NEG.                                                      QAL76920
 223  KQ=2                                                              QAL76930
 225  IF (SPR(NLOPR)+SPR(NHIPR))227,227,229                             QAL76940
  227 SUMUA=0.                                                          QAL76950
      DO 4 II=1,N                                                       QAL76960
      IF (U(II))4,4,5                                                   QAL76970
    5 U(II)=U(II)-100000.                                               QAL76980
    4 SUMUA=SUMUA+U(II)                                                 QAL76990
      IF (SUMUA)6,6,1                                                   QAL77000
    6 CALL EXIT                                                         QAL77010
 229  T=SPR(NLOPR)*HGFL(NLOPR)                                          QAL77020
      IF (T+DELQ)230,230,233                                            QAL77030
 230  SPR(NLOPR)=0.                                                     QAL77040
      SPR(NHIPR)=SPR(NHIPR)+(DELQ+T)/HGFL(NHIPR)                        QAL77050
      IF (SPR(NHIPR))232,1,1                                            QAL77060
 232  SPR(NHIPR)=0.                                                     QAL77070
      KQ=3                                                              QAL77080
      GO TO 2                                                           QAL77090
 233  SPR(NLOPR)=SPR(NLOPR)+DELQ/HGFL(NLOPR)                            QAL77100
      GO TO 1                                                           QAL77110
C     DELQ IS POS.                                                      QAL77120
 224  KQ=3                                                              QAL77130
  222 STBLO=DELQ/HBLO+STBLO                                             QAL77140
      GO TO 92                                                          QAL77150
      END                                                                       
      SUBROUTINE WASH1                                                          
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      YLP1 = 0.                                                                 
C  C  WASHER SECTION                                                    QAL77160
      DO 2 I=1,108                                                      QAL77170
 2    E(I)=0.                                                           QAL77180
      DO 101 I=1,NWASH                                                  QAL77190
  101 E(10*I+8)=FLR(I)                                                  QAL77200
C     MISC STREAMS                                                      QAL77210
C     LET J=10*I, FOLLOWING ARE STORAGE EQUIVALENTS                     QAL77220
C     DMPMD(I)EQE(J+7)    F(I)  EQ  E(J-7)                              QAL77230
C     SRTLQ(I) EQ E(J+10)         O(I) EQ E(J+2) (CALC AFTER XDUMP(I)   QAL77240
C     SSRT(I)  EQ E(J+9)                      IS USED)                  QAL77250
C     SDUMP(I) EQ E(J+6)                                                QAL77260
C     XDUMP(I) EQ E(J+2)                                                QAL77270
      M1=10*MWASH+7                                                     QAL77280
      E(M1)=FASC                                                        QAL77290
C     SAVE DMPMD(N)                                                     QAL77300
      DMPMN=FAID                                                        QAL77310
      T=XSRT+DILSR                                                      QAL77320
      I1=10*IWASH+10                                                    QAL77330
      J1=10*JWASH+10                                                    QAL77340
      K1=10*KWASH+10                                                    QAL77350
      E(I1)=AFRAC*T+OXOFL                                               QAL77360
      E(J1)=BFRAC*T                                                     QAL77370
      E(K1)=(1.-AFRAC-BFRAC)*T                                          QAL77380
      FDSDA=SWFD                                                        QAL77390
      T=SSRT                                                            QAL77400
      E(I1-1)=CFRAC*T+OXOFS                                             QAL77410
      E(J1-1)=DFRAC*T                                                   QAL77420
      E(K1-1)=(1.-DFRAC-CFRAC)*T                                        QAL77430
      E(M1-1)=SKPMD+DLPIS                                               QAL77440
      FMUD=SUFM+WLIMD                                                   QAL77450
      XN=NWASH                                                          QAL77460
      E(M1-5)=XKPMD                                                     QAL77470
      E(3)=WLMLQ+SUFL                                                   QAL77480
      E(2)=1.                                                           QAL77490
      E(8)=1.                                                           QAL77500
      T1=XWOF                                                           QAL77510
      Z(1)=XWOF                                                         QAL77520
      T2=E(12)                                                          QAL77530
      T=AUTWH/XN                                                        QAL77540
      IM2=NWASH-2                                                       QAL77550
      STM=0.                                                            QAL77560
      SFLRT=0.                                                          QAL77570
      VSDAT=0                                                           QAL77580
C SET UP MATRIX                                                         QAL77590
      DO 304 I=1,NWASH                                                  QAL77600
      J=10*I                                                            QAL77610
      E(J+2)=T2                                                         QAL77620
      EJ2=T2                                                            QAL77630
      EJ6=E(J+6)                                                        QAL77640
      EJ7=E(J+7)                                                        QAL77650
      EJ9=E(J+9)                                                        QAL77660
      EJ10=E(J+10)                                                      QAL77670
      VMUD=EJ7+T                                                        QAL77680
      FMUD=FMUD+VMUD+SUFM*WSTFAC(I)                                     QAL77690
C     CALC F(I+1)                                                       QAL77700
      E(J+3)=FMUD*(100.-SOL(I))/SOL(I)                                  QAL77710
      EJ3=E(J+3)                                                        QAL77720
      E(J+11)=-EJ3                                                      QAL77730
C     SAVE F(I+1) TO CALC USODA(I)                                      QAL77740
      E(I+100)=EJ3                                                      QAL77750
      XFLR=SUFM*WSTFAC(I)*STLIQ                                         QAL77760
      SFLR=XFLR*STWFC/CTSMK                                             QAL77770
C     GET VSODA(I)                                                      QAL77780
      W=EJ9+SFLR+EJ6                                                    QAL77790
      VSDAT=VSDAT+W                                                     QAL77800
C     GET TERM TO CORRECT VSODA(I) IN LAST TWO STAGES                   QAL77810
      IF (I-NWASH+1)603,601,602                                         QAL77820
  601 T3=0.                                                             QAL77830
      W=W+T3                                                            QAL77840
      GO TO 603                                                         QAL77850
  602 W=W-T3                                                            QAL77860
C     STORE VSODA(I)                                                    QAL77870
 603  E(J+5)=W                                                          QAL77880
C     CLEAR SSRT(I)                                                     QAL77890
      E(J+9)=0.                                                         QAL77900
      IF (I-IM2)303,301,302                                             QAL77910
 301  W=T1+EJ3-E(J-7)                                                   QAL77920
      STM=W*.99*(TWW-TOFLO)/(H5LB-.99*(TOFLO-32.))                      QAL77930
      S5LB=STM                                                          QAL77940
      GO TO 303                                                         QAL77950
 302  STM=0.                                                            QAL77960
 303  V=EJ10+XFLR+EJ2-T+STM                                             QAL77970
C     SAVE XDUMP(I+1)                                                   QAL77980
      T2=E(J+12)                                                        QAL77990
C     CALC O(I+1)                                                       QAL78000
      E(J+2)=T1                                                         QAL78010
      E(J+12)=T1+EJ3-E(J-7)-V                                           QAL78020
C     SAVE O(I+1)                                                       QAL78030
      EJ12=E(J+12)                                                      QAL78040
      T1=EJ12                                                           QAL78050
      Z(I+1)=T1                                                         QAL78060
      EJ8=E(J+8)                                                        QAL78070
      E(J+6)=(1.-EJ8)/EJ8                                               QAL78080
      E(J+8)=-1./EJ8                                                    QAL78090
C  PUT ALT. CONST TERM =0                                               QAL78100
      E(J+10)=0.                                                        QAL78110
C  PUT SOME TERMS=1                                                     QAL78120
      E(J+7)=1.                                                         QAL78130
 304  E(J+4)=-EJ12                                                      QAL78140
C     FINISH SETUP                                                      QAL78150
      E(15)=E(15)+FDSDA                                                 QAL78160
      E(20)=(1.+E(18))*FDSDA/E(3)                                       QAL78170
      N1=10*NWASH+5                                                     QAL78180
      E(N1)=E(N1)+YLP1*E(N1+7)                                          QAL78190
      RETURN                                                            QAL78200
      END                                                                       
      SUBROUTINE WASH2                                                          
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      YLP1 = 0.                                                                 
C     MATRIX REDUCTION                                                  QAL78210
 307  IFIN=10*NWASH+2                                                   QAL78220
      DO 310 I=2,IFIN,10                                                QAL78230
C     DIV FIRST ROW BY DIAG. (ONLY 3 ELEMENTS)-ODD                      QAL78240
      DO 308 J=1,3                                                      QAL78250
      L=I+J                                                             QAL78260
 308  E(L)=E(L)/E(I)                                                    QAL78270
C     ELIMINATE X(I) FROM REM. EQS (3 ELEMENTS IN NEXT ROW)             QAL78280
      DO 309 J=6,8                                                      QAL78290
      L=I+J                                                             QAL78300
 309  E(L)=E(L)-E(I+5)*E(L-5)                                           QAL78310
C     DIV. 2ND ROW BY DIAG.            - EVEN                           QAL78320
      E(I+7)=E(I+7)/E(I+6)                                              QAL78330
C     ELIMINATE X(I) FROM REM. EQS (2 ELEMENT IN NEXT TWO ROWS)         QAL78340
      E(I+8)=E(I+8)/(E(I+6))                                            QAL78350
      E(I+10)=E(I+10)-E(I+9)*E(I+7)                                     QAL78360
      E(I+13)=E(I+13)-E(I+9)*E(I+8)                                     QAL78370
      E(I+15)=E(I+15)-E(I+14)*E(I+7)                                    QAL78380
 310  E(I+18)=E(I+18)-E(I+14)*E(I+8)                                    QAL78390
C     SOLUTION                                                          QAL78400
 312  I=NWASH*10+5                                                      QAL78410
      E(10*NWASH+15)=0.                                                 QAL78420
 313  E(I)=E(I)-E(I-1)*E(I+10)-E(I-2)*E(I+5)                            QAL78430
      E(I-5)=E(I-5)-E(I-6)*E(I)                                         QAL78440
      IF (I-15)315,315,314                                              QAL78450
 314  I=I-10                                                            QAL78460
      GO TO 313                                                         QAL78470
  315 E(10*NWASH+15)=YLP1                                               QAL78480
      IFIN=NWASH+1                                                      QAL78490
      DO 316 I=1,IFIN                                                   QAL78500
C     USE E(10*I+7) FOR OSODA(I)                                        QAL78510
      J=10*I                                                            QAL78520
 316  E(J+7)=Z(I)*E(J+5)                                                QAL78530
      DO 317 I=1,NWASH                                                  QAL78540
C     USE E(J+9) FOR USODA(I)                                           QAL78550
      J=10*I                                                            QAL78560
 317  E(J+9)=E(I+100)*E(J+10)                                           QAL78570
C     FINAL CALCS                                                       QAL78580
      SDAIN=FDSDA+VSDAT+E(10*IFIN+7)                                    QAL78590
      SRVR=E(10*NWASH+9)                                                QAL78600
      CRVR=SRVR*CTOSR                                                   QAL78610
      ARVR=CRVR*RRVR                                                    QAL78620
      TOTRS=SRVR+SOLPHS                                                 QAL78630
      WOFS=SDAIN-SRVR                                                   QAL78640
      ALOST=TAA*BXREQ*(1.-OAR)                                          QAL78650
      ALOSS=AKSTK+ALOAD+AHTWL+AEXLS+AUTSA+AUTWA+APISO+ARVR+ASWR+OSLKFA  QAL78660
      ALOSS=ALOSS+AUNAC+OSLKFH/1.53009                                  QAL78670
      ALOSS=ALOSS+.56464*FAIDLM                                         QAL78680
      OAR1=APROD/(APROD+ALOSS)                                          QAL78690
      SMKP=CDSIL+CNONA-OXSLK+TOTRS+SUNAC+SPISO+SHTWL+ORESDA+SSWR+OXMDS  QAL78700
      SMKP=SMKP-TOTFAS-SSFLR-TOWSTS                                     QAL78710
      SUMLC=CRVR+CUNAC+CPISO+CHTWL+CSWR+OXMDC                           QAL78720
      RETURN                                                            QAL78730
      END                                                               QAL78740
                       FUNCTION CPF(CC,R)                                       
      DEL=CC/50.-3.                                                     QAL70030
      D1=.89112-.043*R                                                  QAL70040
      D2=-.017704-.0099*R                                               QAL70050
      D3=.0024548+.00888*R                                              QAL70060
      D4=-.00117518E0+.000417*R                                         QAL70070
      D5=.0003925-.001375*R                                             QAL70080
      CPF=D1+D2*DEL+D3*DEL**2+D4*DEL**3+D5*DEL**4                       QAL70090
      RETURN                                                            QAL70100
      END                                                               QAL70110
                       FUNCTION PF(T)                                           
    1 PF=(((T*1.9579719E-8 -5.980466E-6)*T+8.5151515E-4)*T-6.72727E-3)*T        
     1- 4.73                                                                    
    5 RETURN                                                                    
      END                                                               QAL70060
                       FUNCTION PRES(TCBC)                                      
      PRES=((.000008*TCBC-.00276)*TCBC+.3826)*TCBC-18.59                QAL70030
      RETURN                                                            QAL70040
      END                                                               QAL70050
                       FUNCTION CCF(W,R)                                        
      CCF=(220.*R+871.0)*W/(1.-1.3725*W)                                QAL70030
      RETURN                                                            QAL70040
      END                                                               QAL70050
                       FUNCTION BPF(CC,T)                                       
      EPSI=CC/40.-4.5                                                   QAL70030
      D10=2.86+.1125*EPSI-.6375*EPSI**2                                 QAL70040
      D11=.0233+.00945*EPSI+.00305*EPSI**2                              QAL70050
      BPF=D10+D11*T                                                     QAL70060
      RETURN                                                            QAL70070
      END                                                               QAL70080
                       FUNCTION CC3(W,R)                                        
      CC3=(975.98+95.*R)*W/(1.-1.2*W)                                   QAL70030
      RETURN                                                            QAL70040
      END                                                                       
                       FUNCTION CC2(W,R)                                        
      CC2   =   (82.5*R+973.5)*W/(1.-1.25*W)                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DIG2                                                           
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      IOU = 6                                                                   
      ISKYP = 0                                                                 
      LCNT3 = 0                                                                 
      ISW = 0                                                                   
      PDIG=1000.                                                        QAL75190
   10 STBLO=0                                                           QAL75200
      DO 1 I=1,10                                                       QAL75210
 1    STFL(I)=0.                                                        QAL75220
      LCNT2 = 0                                                                 
C     BEGIN DIGESTER LOOP                                               QAL75230
 34   CCDIG=(RDIG+SAFAC+.42883-.0021537*TDIG)/(.0024724-4.0662E-6*TDIG) QAL75240
      DDIG=.8710+.220*RDIG+.0013725*CCDIG                               QAL75250
      XDIG=1000.*CDIG*DDIG/CCDIG                                        QAL75260
      FDIG=.04702*DGMUD+.166443*XDIG/DDIG                               QAL75270
      CPDIG=CPF(CCDIG,RDIG)                                             QAL75280
      BPE(1)=BPF(CCDIG,TDIG)                                            QAL75290
      T=TFL(1)-BPE(1)                                                   QAL75300
      TMP=TDIG-BPE(1)                                                   QAL75310
  103 PRDIG=PF(TMP)                                                             
      PR(1)=PF(T)                                                       QAL75330
      HGFL(1)=1186.7+.04*T+.75*BPE(1)                                   QAL75340
C     ASSUME CPFL(1)                                                    QAL75350
      CPFL(1)=CPDIG-7.13E-5*(TDIG-TFL(1))                               QAL75360
      LCNT1 = 0                                                                 
 40   T1=CPFL(1)*(TFL(1)-32.)                                           QAL75370
      T2=XDIG*(CPDIG*(TDIG-32.)-T1)                                     QAL75380
      STFL(1)=(T2+DGMUD*CPMUD*(TDIG-TFL(1)))/(HGFL(1)-T1)               QAL75390
      XFL(1)=XDIG-STFL(1)                                               QAL75400
      WFFL(1)=CDIG/XFL(1)                                               QAL75410
      CCFL(1)=CCF(WFFL(1),RDIG)                                         QAL75420
      CPFL1=CPF(CCFL(1),RDIG)                                           QAL75430
 42   IF (DABS(CPFL1-CPFL(1))-TESTA)39,43,43                                    
 43   CPFL(1)=CPFL1                                                     QAL75450
      LCNT1 = LCNT1 + 1                                                         
      IF (LCNT1 - LMAXD1) 40,40,500                                             
  500 WRITE (IOU,501) CPFL1,CPFL(1)                                             
  501 FORMAT (' LCNT1 IN DIG2 EXCEEDED,  CPLF1 = ',G13.6,'  CPFL(1) = ',        
     1G13.6)                                                                    
C     END OF CPFL1 LOOP                                                 QAL75470
   39 XINJ=STFL(1)+XTTK-BXSPL                                           QAL75480
      CINJ=CTTK*(XTTK-BXSPL)/XTTK                                       QAL75490
      WFINJ=CINJ/XINJ                                                   QAL75500
      CCINJ=CC3(WFINJ,RTTK)                                             QAL75510
      CPINJ=CPF(CCINJ,RTTK)                                             QAL75520
      TOINJ=32.+(STFL(1)*HGFL(1)+A101*(THTR(M)-32.))/(XINJ*CPINJ)       QAL75530
      BPEINJ=BPF(CCINJ,TOINJ)                                           QAL75540
      TEM=TOINJ-BPEINJ                                                  QAL75550
  107 PRINJ=PF(TEM)                                                             
      IF(TDIG-TFL(1))804,802,804                                                
  804 IF(DABS(PR(1)-PRINJ-50.)-.5)802,802,803                                   
  803 TFL(1)=TFL(1)-(PR(1)-PRINJ-50.)/8.0                                       
      IF (TFL(1).GT.TDIG) TFL(1)=TDIG                                           
      IF (TFL(1).EQ.TDIG) GO TO 802                                             
      LCNT2 = LCNT2 + 1                                                         
      IF (LCNT2 - LMAXD2) 34,34,502                                             
  502 WRITE (IOU,503) PR(1),PRINJ,TFL(1),TDIG,LCNT3                             
  503 FORMAT (' LCNT2 IN DIG2 EXCEEDED,  PR(1) = ',G13.6,'  PRINJ = ',          
     1G13.6,'  TFL(1) = ',G13.6,'  TDIG = ',G13.6,'  LCNT3 = ',I4)              
      LCNT2 = 0                                                                 
  802 DGST=XDIG-A102-STFL(1)                                            QAL75620
      TDIG1=XDIG*CPDIG+DGMUD*CPMUD                                      QAL75630
      A109=A108+A101*(THTR(M)-32.)-(A107-TDIG1)*32.                     QAL75640
      TDIG1=(DGST*HVAP+STFL(1)*HGFL(1)+A109)/TDIG1                      QAL75650
      IF (DABS(TDIG1-TDIG)-TESTD)36,504,504                                     
  504 LCNT3 = LCNT3 + 1                                                         
      IF (LCNT3 - LMAXD3) 35,35,505                                             
  505 WRITE (IOU,506) TDIG1,TDIG,PR(1),PRINJ,TFL(1)                             
  506 FORMAT ('LCNT3 IN DIG2 EXCEEDED,  TDIG1 = ',G11.5,'  TDIG = ',            
     1G11.5,'  PR(1) = ',G11.5,'  PRINJ = ',G11.5,'  TFL(1) = ',G11.5)          
      GO TO 36                                                                  
35    CONTINUE                                                                  
      IF(ISKYP.EQ.1)GO TO 700                                                   
      OTDIG = TDIG                                                              
      OTDIG1 = TDIG1                                                            
      ISKYP = 1                                                                 
      TDIG = TDIG + TESTD                                                       
      GO TO 34                                                                  
700   ODEL = OTDIG-OTDIG1                                                       
      DEL=TDIG-TDIG1                                                            
      IF((DEL-ODEL).EQ.0.0)GO TO 36                                             
      TDIG = TDIG-((TDIG-OTDIG)/(DEL-ODEL))*DEL                                 
      LCNT2 = 0                                                                 
      GO TO 34                                                                  
   36 RETURN                                                            QAL75820
      END                                                                       
      SUBROUTINE WRITEX                                                         
C     COMMON AND DIMENSION STATEMENTS ASSOCIATED WITH EACH SECTION              
      DIMENSION U(10)                                                           
      DIMENSION E(108),A(10),FLR(10),SPR(10),Z(20),LZ(10),SOL(10)               
      DIMENSION BPE(10),PR(10),HGFL(10),HFFL(10),XFL(10),WFFL(10)               
      DIMENSION THTR(10),TFL(10),CCFL(10),CPFL(10),TCND(10),STFL(10)            
      DIMENSION WSTCH(10),CWSHST(10),SWSHST(10),XWSHST(10),WSTFAC(10)           
      COMMON A,BPE,CCFL,CPFL,E,FLR,HGFL,HFFL,PR,SOL,SPR,STFL,THTR               
      COMMON TFL,TCND,U,WFFL,XFL,Z,LZ                                           
      COMMON AFRAC,ATTO,ASCLE,AKILN,AHAND,AVCAO,ANDYS,ALOSS,ALMNA,APPT          
      COMMON ASLKF,ASTTO,ASH,ARVR,ASWR,ALOST                                    
      COMMON APROD,ATTOF,ALTOP,ASOL ,AKFD ,AKSTK,ALOAD,ASCL ,ASPL               
      COMMON AHTWL,ATTK ,ALSO ,AKPMD,AKPFD,APFTK,AUNAC,ASPIL,AREQD              
      COMMON AEXLS,AUTSA,AUTSH,ASFD ,APISO,ATRUF,ASRT ,ABLO ,ALIME              
      COMMON ADLIM,AWLIM,AUTWA,AUTWH,ADIG ,AMWOA,A101 ,A102 ,A103               
      COMMON A104 ,A105 ,A106 ,A107 ,A108 ,A109 ,ALOSP                          
      COMMON BFRAC,BPRLP,BPRHI,BXREQ,BOUND,BXMST,BXSPL,BPRBL                    
      COMMON CMKUP,CRVR,CPLIM,CPHYD,CPBX,CPMST,CPMUD,CFRAC,CTSMK,CLNL           
      COMMON CLNDIL,CTSSPL,CSWR,CTSLP,CPPT,CPTTO,CPFALQ,CLNCC,CPCLN             
      COMMON CPLLQ,CCLP,CONST,CAUSC,CTSFA,CTOSR,CTSPFD,CTSSFD,CTSDIG            
      COMMON CKMOI,CARBO,CARBI,CHTWL,CTSWO                                      
      COMMON CAOHO,CCSRT,CLDIL,CSFLR,CTTOF,CLTOP,CCTTO,CSPL ,CTTK               
      COMMON CCTTK,CPTTK,CCSPL,CPSPL,CPLP ,CPHI ,CCBC ,CPKP                     
      COMMON CCLSO,CLSO ,CACO3,CCACO,CKPMD,CKPFD,CPFTK,CUNAC,CSPIL              
      COMMON CCKP ,CPHI1,CPKP1,CDSIL,CNONA,CCARB,CDLOS,CSFD                     
      COMMON CCSFD,CPISO,CTRUF,CSRT ,CBLO ,CCBLO,CPBLO,CLIME,CARLS              
      COMMON CAUST,CDLIM,CWLIM,CDIG ,CMWOA,CCDIG,CPDIG,CPFL1,CPINJ              
      COMMON COND ,CONHI,CONLO,CC   ,CCWOF,CPWOF,CMKP                           
      COMMON DFRAC,DGEXT,DORWW,DLTOP,DTTOF,DTTK,DOIMP,DORSOL,DENMK,DSPL         
      COMMON DILOXM,DSTIMP,DILT,DILPC,DILSW,DILLIM                              
      COMMON DLSO ,DILKP,DKP  ,DGLIM,DLMUD,DGMUD,DSFD ,DLPIS,DILSR              
      COMMON DBLO ,DDIG ,DGST ,DT   ,DELQ ,DMPMN                                
      COMMON EVAP,EFFSLK,EMIL,FLOSPL,FLRSDA,HYSOL,H2OSND                        
      COMMON FAID,FAIDLM,FAUFL,FALQDN,FAIDF,FASC,FATMP,FAUFC,FAUFS,FAOFC        
      COMMON FAOFS,FAOFL,FALQCC,FAUFFL,FAOFFL,FSMKP                             
      COMMON FACDG,FACTK,FACND,FDHYD,FCACO,FASOL,FAUTS,FAUTW,FSRT               
      COMMON FDSIL,FNONA,FCARB,FLTOP,FTTOF,FTTK ,FLSO ,FKP  ,FLRS               
      COMMON FSUF ,FSFD ,FBLO ,FDIG ,FDSDA,FMUD                                 
      COMMON GPLAL,GPLTO,HIDIL,HOLDC,H5LB ,HVAP ,HOURS,HVLP ,HFLP               
      COMMON HVHI ,HFHI ,HBLO ,IWASH,IS1  ,IS2  ,I    ,IPREV,IFIRS              
      COMMON ISN  ,IRET ,IM2  ,IFIN ,JWASH,J    ,KWASH,KQ   ,K                  
      COMMON LOOP ,L    ,M    ,N    ,NHIPR,NLOPR,NWASH,MWASH                    
      COMMON IJ                                                                 
      COMMON OXTOC,OXFAC,OPFAC,OAR,OXTOFC,OXTOFS,OXTOCC,OXTOFL,OXMDS            
      COMMON ORESDM,ORESDA,OREC,ORECRB,OXMSOL,OXMDC,OXOFL,OXOFC,OXOFS           
      COMMON OXSLK,OAR1,OXCAO,OXIN,OXKLN,OXLIQ,OSLKFO,OSLKFC,OSLKFH             
      COMMON OSLKFA,OSLKFS,ORENA,OXDEN,OXLMLQ,PPT,PNAOH,PIDIL                   
      COMMON PDPHR,PISO ,P2O5 ,PITAA,PISIL,PURE ,PPTVL,PPTNO,PRDIL              
      COMMON PISF ,PDIMP,PABC ,PAHI ,PISOM                                      
      COMMON QSPL,QHI,QINBL,QOUT,RRVR,RLTOP,RTTOF,RSPL                          
      COMMON RTTK ,RLSO ,RPFTK,RSFD ,RBLO ,RDIG ,RMWOA                          
      COMMON RFIN,RMKUP,REACAO,REACLM,REACSL,REACMD,RFRCT                       
      COMMON SHIPR,SLOPR,SIL,SOLPH,SAFAC,SWFFA,SDIG,SMKP,SMWOA,STARCH           
      COMMON ST650,ST5LB,SNDC,SNDA,SLTOP,SDLIM,SKPMD,SSWR,SWRSDA,STOIMP         
      COMMON SPFTK,SLSO,SILREA,SILQTZ,SAND,SOLPHS,SOLPHC,SSFLR,SSFD,SNDL        
      COMMON SBLO,SUMUA,SUMLC,SLKFOC,SLKFCC,SLKFHC,SMKUP,SSPL,SKPFD,SSRT        
      COMMON SSPIL,STLIQ,SETSF,STWFC,SOLS ,SHWFC,SLSOL,SHTWL                    
      COMMON STBC ,SCACO,SOLKP,SUNAC,STHI ,STAID,SSFMD,SUFM ,SUFL               
      COMMON SUFC ,SUFA ,SPISO,STRUF,SLIME,SWLIM,SWFD ,SS   ,STBLO              
      COMMON STM  ,SFLRT,SFLR ,SUFS ,SDAIN,SRVR                                 
      COMMON TPYAL,TFILL,TSLLQ,TBX  ,TLIME,TMWO ,TWW  ,TOFLO,TESTA              
      COMMON TESTB,TESTD,TESBP,TESCC,TESCN,TESTM,TKP  ,TEVFD,TESTK              
      COMMON TTTK ,TESOA,TESMK,TL   ,TAA  ,THA                                  
      COMMON TRSOL,TDIG ,TEMPC,TSPL ,THI  ,TCBC ,TCHI ,T1   ,T2                 
      COMMON T3   ,T    ,THI1 ,TBLO ,TDIG1,TH   ,TC   ,TOTRS                    
      COMMON TOWSTC,TOWSTS,TOTWST,TLMH2O,TOWSTL,TOTMO,TEST1,TTOFT,TOTFAC        
      COMMON TOTFAS,TMKUP,TCLN     ,UNITBX,UNITSD,UNITLM,UNITST,VNTBLO          
      COMMON UTIL,V,VSDAT,VMUD,WFORC,WSHSF,WFTTOF,WFOXML,WFOXTO,WWDOR           
      COMMON WLMLQ,WFTTK,WFSPL,WFBC,WFKP,WFLSO,WFSFD,WFBLO,VNTHI,VNTLO          
      COMMON WLIME,WLIMD,WFWOF,W    ,WOFS                                       
      COMMON XMOIS,XLTOP,XFSOL,XSPL,XTTK,XBC,XTTOF,XRLIM,XHTWL,XSWR             
      COMMON XKP  ,XLSO ,XCACO,XKPMD,XKPFD,XPFTK,XUNAC,XSPIL,XK2                
      COMMON XK3  ,XK4  ,XK5  ,XK6  ,XK7  ,XK8  ,XK9  ,XK10 ,XSFLR              
      COMMON XSFD ,XMSFD,XTRUF,XSRT ,XBLO ,XWLIM,XHTR ,XLIQ ,XLIME              
      COMMON XDIG ,XW2  ,XN   ,XW4  ,XWN  ,XK   ,XWOF ,XFLR                     
      COMMON  CWSHST,SWSHST,WSTCH,WSTFAC,XWSHST                                 
      COMMON Z101,Z102,Z103                                                     
      COMMON AINJ,AXRT1,AXRT2,AXRT3,BXLMLQ,CINJ,CCINJ,CRESLM,CXRT1,CXRT2        
     1,CXRT3,DINJ,DORIMP,DBC,DMKUP,FINJ,FBC,OXMDLQ,OXWW,OSLKFL,PRINJ            
      COMMON PRDIG,REACOS,REACOC,REACOL,SINJ,S5LB,STTOF,SRESLM,SLURLQ,          
     1SLURSL,STTK,STCHLQ,STCHC,STCHS,SXRT1,SXRT2,SXRT3,TOINJ,TOLMSL,            
     2TBXSLR,WTSLUR,XINJ,XLMSL,XRESLM,XPISO,XMKUP,XRT1,XRT2,XRT3                
      INP=5                                                                     
      IOU=6                                                                     
      NHIPR=NHIPR                                                       QAL70050
      NLOPR=NLOPR                                                       QAL70060
      N=N                                                               QAL70070
      M=M                                                               QAL70080
      II=NHIPR-1                                                        QAL70090
      KK=NLOPR-1                                                        QAL70100
      WRITE (IOU,5000)BXREQ,UNITBX,ASH,UNITSD,EMIL,UNITLM,STARCH,UNITST,QAL70110
     1ST650,ST5LB,PR(II),VNTHI,PR(KK),VNTLO,VNTBLO,OAR                  QAL70120
 5000 FORMAT (1H1,18X2HA.,23X25HRAW MATERIAL REQUIREMENTS/52X12H(SHORT T        
     1ONS)//42X14HTONS/OPER. DAY13X19HTONS/TONS OF AL203 /32X7HBAUXITE7X        
     1F8.2,19XF7.4/32X4HSODA10XF8.2,19XF7.4/32X4HLIME10XF8.2,19XF7.4/32X        
     16HSTARCH8XF8.2,19XF7.4//19X2HB.18X38HDIGEST AND WASH H2O HTNG STM         
     1(LBS/HR) //42X9HDIGESTION,10X2H= F7.0/42X22HWASH WATER HEATING =          
     1F7.0//19X2HC.18X34HFLASH STM FROM DIGESTION (LBS/HR) /42X19HHIGH P        
     1RESSURE VENT(F4.0,8HPSIA) = F7.0/42X18HLOW PRESSURE VENT(F4.0,9HPS        
     1IA)  = F7.0/42X18HBLOW OFF(14.7PSIA)11X2H= F7.0//19X2HD.18X28HOVER        
     1ALL RECOVERY OF AL203 = F6.5//19X2H1.18X19HTEST TANK EFFLUENT )           
      WRITE (IOU,5001)XTTK,DTTK,FTTK,ATTK,CPTTK,CCTTK,CTTK,RTTK,TTTK,           
     1STTK,CTSSPL                                                               
 5001 FORMAT (44X2HL=F8.1,15X5HDENS=F6.4,15X5HFLOW=F7.1/44X2HA=F8.2,15X         
     15HSPHT=F6.4,15X5HCONC=F7.1/44X2HC=F8.2,15X5HA/C =F6.4,15X5HTEMP=F7        
     1.1/44X2HS=F8.2,15X5HC/S =F6.4)                                            
      WRITE (IOU,3000)                                                          
 3000 FORMAT (1H0)                                                              
      WRITE (IOU,4999)                                                          
 4999 FORMAT (19X2H2.18X50HINDIRECT SPENT LIQUOR HEATERS,TEMPERATURE PRO        
     1FILE /42X8HHTR. N0.7X4HAREA15X1HU12X8HTEMP OUT)                           
      DO 100 I=1,N                                                              
  100 WRITE (IOU,5002)I,A(I),U(I),THTR(I+1)                                     
 5002 FORMAT (45XI2,9XF6.0,14XF4.0,11XF5.1)                                     
      WRITE (IOU,5003)THTR(1),XW4,XW2                                           
 5003 FORMAT (1H044X29HINLET TEMP TO FIRST HEATER = F6.2,//44X31HBAUXITE        
     1 SLURRY LIQ.(TONS/DAY)  /47X15HAT HIGH TEMP = F7.2/47X15HAT LOW TE        
     1MP  = F7.2)                                                               
      WRITE (IOU,3001)                                                          
 3001 FORMAT (1H018X2H3.18X33HLAST SPENT LIQUOR HEATER EFFLUENT)                
      WRITE (IOU,5004)XINJ,DINJ,FINJ,AINJ,CPINJ,CCINJ,CINJ,RTTK,TOINJ,          
     1SINJ,CTSSPL,PRINJ                                                         
 5004 FORMAT (44X2HL=F8.1,15X5HDENS=F6.4,15X5HFLOW=F7.1/44X2HA=F8.2,15X         
     15HSPHT=F6.4,15X5HCONC=F7.1/44X2HC=F8.2,15X5HA/C =F6.4,15X5HTEMP=F7        
     1.1/44X2HS=F8.2,15X5HC/S =F6.4,15X,8HPRES.=  ,F5.1//1H1,18X,               
     1                                 2H4.18X18HDIGESTER EFFLUENT )            
      WRITE (IOU,5005)XDIG,DDIG,FDIG,ADIG,CPDIG,CCDIG,CDIG,RDIG,TDIG,           
     1SDIG,CTSDIG,    PRDIG,DGMUD,CDSIL,CCARB,CNONA                             
 5005 FORMAT (44X2HL=F8.1,15X5HDENS=F6.4,15X5HFLOW=F7.1/44X2HA=F8.2,15X         
     15HSPHT=F6.4,15X5HCONC=F7.1/44X2HC=F8.2,15X5HA/C =F6.4,15X5HTEMP=F7        
     1.1/44X2HS=F8.2,15X5HC/S =F6.4,15X,8HPRES.=  ,F5.1/44X,2HM=,F8.2/          
     1                                         39X22HDIGESTION SODA LOSS        
     1ES /50X13HDESILICATION=F6.2/50X13HCARBONATION =F6.2/50X13HNON ALKA        
     1LINE=F6.2//18X3H5A.18X35HDIGESTION FLASH TANKS(1 THROUGH 5) /54X7H        
     1FL TK 17X7HFL TK 27X7HFL TK 37X7HFL TK 47X7HFL TK 5)                      
      KK=1                                                                      
      II=1                                                                      
      IJ=5                                                                      
  101 WRITE (IOU,4998)(XFL(I),I=II,IJ)                                          
 4998 FORMAT (35X,7HLIQ OUT5X5(6XF8.1))                                         
      WRITE (IOU,4997)(STFL(I),I=II,IJ)                                         
 4997 FORMAT (35X10HSTM TO HTR2X5(7XF7.2))                                      
      WRITE (IOU,4996)(SPR(I),I=II,IJ)                                          
 4996 FORMAT (35X10HVENT STEAM2X5(7XF7.2))                                      
      GO TO (102,104),KK                                                        
  102 WRITE (IOU,4995)CONHI,(HGFL(I),I=1,4)                                     
      GO TO 111                                                                 
 4995 FORMAT (35X12HSTM ENTHALPY1X5(6XF6.1,2X))                                 
  104 WRITE (IOU,4995)(HGFL(I),I=5,9)                                           
  111 WRITE (IOU,4994)(HFFL(I),I=II,IJ)                                         
 4994 FORMAT (35X13HCOND ENTHALPY1X5(5XF6.1,3X))                                
      WRITE (IOU,4993)(TFL(I),I=II,IJ)                                          
 4993 FORMAT (35X,8HTEMP OUT3X5(8XF6.1))                                        
      WRITE (IOU,4992)(CCFL(I),I=II,IJ)                                         
 4992 FORMAT (35X8HCONC OUT3X5(8XF6.1))                                         
      GO TO (103,105),KK                                                        
  103 WRITE (IOU,4991)CONLO,(PR(I),I=1,4)                                       
      GO TO 109                                                                 
 4991 FORMAT (35X8HPRESSURE3X5(8XF6.1))                                         
  105 WRITE (IOU,4991)(PR(I),I=5,9)                                             
  109 WRITE (IOU,4990)(BPE(I),I=II,IJ)                                          
 4990 FORMAT (35X8HEFFL BPE3X5(8XF6.1))                                         
      WRITE (IOU,4899)(CPFL(I),I=II,IJ)                                         
 4899 FORMAT (1H034X9HSP HT OUT5X5(8XF6.4)///)                                  
      KK=KK+1                                                                   
      GO TO (106,106,110),KK                                                    
  106 WRITE (IOU,4898)                                                          
 4898 FORMAT (18X3H5B.18X36HDIGESTION FLASH TANKS(6 THROUGH 10) /54X7HFL        
     1 TK 67X7HFL TK 77X7HFL TK 87X7HFL TK 97X8HFL TK 10)                       
      II=6                                                                      
      IJ=10                                                                     
      GO TO 101                                                                 
  110 WRITE (IOU,4897)VNTBLO,HBLO,COND                                          
 4897 FORMAT (35X18HBLOW OFF STEAM =  F7.0,10HLBS/HR AT F6.1,7H BTU/LB/         
     135X27HTOTAL WT OF CONDENSATE   = F8.2)                                    
      WRITE (IOU,5007)                                                          
 5007 FORMAT (1H017X22HBLOW OFF TANK EFFLUENT)                                  
      WRITE (IOU,5001)XBLO,DBLO,FBLO,ABLO,CPBLO,CCBLO,CBLO,RBLO,TBLO,           
     1SBLO,CTSSFD                                                               
      WRITE (IOU,5008)DGMUD                                                     
 5008 FORMAT (44X2HM=F8.2/1H1,18X,                                              
     1                        2H7.18X12HSETTLER FEED)                           
      WRITE (IOU,5009)XSFD,DSFD,FSFD,ASFD,CCSFD,CSFD,RSFD,SSFD,CTSSFD           
 5009 FORMAT (44X2HL=F8.1,15X5HDENS=F6.4,15X5HFLOW=F7.1/44X2HA=F8.2,41X         
     15HCONC=F7.1/44X2HC=F8.2,15X5HA/C =F6.4/44X2HS=F8.2,15X5HC/S =F6.4)        
      WRITE (IOU,5010)XMSFD                                                     
 5010 FORMAT (44X2HM=F8.2//19X2H8.18X17HSETTLER OVERFLOW )                      
      WRITE (IOU,5009)XLSO,DLSO,FLSO,ALSO,CCLSO,CLSO,RLSO,SLSO,CTSPFD           
      WRITE (IOU,5011)XPFTK,XKPFD,APFTK,AKPFD,CPFTK,CKPFD,SPFTK,SKPFD,          
     1RPFTK,RLTOP,CTSPFD,CTSLP                                                  
 5011 FORMAT (1H018X2H9.18X22HKELLY PRESS FEED TANK /46X5HINPUT20X6HOUTP        
     1UT/24X,2(20X2HL=F8.1)/24X,2(20X2HA=F8.2)/24X2(20X2HC=F8.2)/24X2(20        
     1X2HS=F8.2)/21X2(21X4HA/C=F5.4)/21X2(21X4HC/S=F5.4)//18X3H10.18X21H        
     1KELLY PRESS EFFLUENT )                                                    
      WRITE (IOU,5001)XKP,DKP,FKP,ALTOP,CPKP,CCKP,CLTOP,RLTOP,TKP,SLTOP,        
     1CTSLP                                                                     
      WRITE (IOU,5012)                                                          
 5012 FORMAT (1H017X3H11.18X13HHID EFFLUENT )                                   
      WRITE (IOU,4896)XBC,ALTOP,CPHI,CCBC,CLTOP,RLTOP,THI,SLTOP,CTSLP,          
     1BPRHI                                                                     
 4896 FORMAT (44X2HL=F8.1/44X2HA=F8.2,15X5HSPHT=F5.4,15X5HCONC=F5.1/44X         
     12HC=F8.2,15X5HA/C =F5.4,15X5HTEMP=F5.1/44X2HS=F8.2,15X5HC/S =F5.4,        
     115X5HBPE =F5.1)                                                           
      WRITE (IOU,5013)                                                          
 5013 FORMAT(1H0,17X,3H12.,18X,21HLIQUORTOPRECIPITATION)                        
      WRITE (IOU,5001)XLTOP,DLTOP,FLTOP,ALTOP,CPLP,CCLP,CLTOP,RLTOP,            
     1TFILL,SLTOP,CTSLP                                                         
      WRITE (IOU,5014)BPRLP                                                     
 5014 FORMAT (1H&95X7HBPE =  F5.2//18X3H13.18X47HLIQUOR FROM PRECIPITATI        
     1ON TO SPENT LIQUOR TANK )                                                 
      WRITE (IOU,5001)XTTOF,DTTOF,FTTOF,ATTOF,CPTTO,CCTTO,CTTOF,RTTOF,          
     1TTOFT,STTOF,CTSLP                                                         
      WRITE (IOU,5015)GPLAL,ASOL,HYSOL,HOURS,ASCL                               
 5015 FORMAT (1H054X24HTRAY THICK OFLOW SOLIDS /60X6HGPL  =F7.3/60X6HAL2        
     1O3=F6.2/60X6HHYD  =F6.2//55X13HHOLDING TIME=F6.2//55X13HSCALE AL2O        
     13= F6.2/1H1,17X,                                                          
     1            3H14.18X27HSPENT LIQUOR TANK EFFLUENT )                       
      WRITE (IOU,5001)XSPL,DSPL,FLOSPL,ASPL,CPSPL,CCSPL,CSPL,RSPL,TSPL,         
     1SSPL,CTSSPL                                                               
      WRITE (IOU,5016)PDPHR,WWDOR,AKFD,CKMOI,BOUND,DORIMP,XFSOL,AKSTK,          
     1ALOAD,DSTIMP,APROD,ORESDM,ALMNA,OREC,ORECRB,OXIN,OXKLN,OXLIQ              
 5016 FORMAT (1H017X3H15.18X12HEVAPORATION=F9.0,7H LBS/HR//18X3H16.18X          
     116HHYDRATE FILTERS /44X11HWASH WATER=F7.2/44X11HAL2O3     =F7.2/44        
     1X11HFREE MOIST=F7.2/44X11HBOUND H2O =F7.2/44X11HIMPURITIES=F7.2/44        
     1X11HTOTAL SOLS=F7.2//18X3H17.18X25HKILN AND LOADOUT LOSSES  /44X          
     111HKILN AL2O3=F6.2/44X11HLOAD AL2O3=F6.2/44X11HIMP(COMB) =F6.2//18        
     1X3H18.18X28HPRODUCT TO REDUCTION PLANTS /44X8HAL2O3  =F7.2,20X19HT        
     1OT. SODIUM ON ORE=F5.2/44X8HALUMINA=F7.2,20X19HTOT. ORE CAUS SODA=        
     1F5.2/79X19HTOT. ORE CARBONATE=F5.2//18X3H19.18X16HOXALATE BALANCE         
     1/44X19HOXAL PRODUCED     =F6.2/44X19HDESTROYED IN KILN =F6.2/44X          
     119HLIQUOR LOSSES     =F6.2)                                               
      WRITE (IOU,5017)OXSLK,OXIN,OSLKFL,SLKFCC,OSLKFA,SLKFOC,OSLKFC,            
     1SLKFHC,OSLKFS,OXDEN,OSLKFO,OSLKFH,DILSW,REACOL,REACOC,REACOS,             
     1REACMD,OXTOFL,OXTOCC,OXWW                                                 
 5017 FORMAT (1H 43X19HSEED WASH REACTOR =F6.2//44X                             
     119HTOTAL REMOVED     =F6.2//18X3H20.18X47HFILTRATE FROM SEED WASHI        
     1NG TO OXALATE REACTOR  /44X2HL=F7.2,20X10HCAUS CONC=F5.2/44X2HA=F7        
     1.2,20X10HOXAL CONC=F5.2/44X2HC=F7.2,20X10HHYD CONC =F5.2/44X2HS=F7        
     1.2,20X10HLIQ DENS =F6.4/43X3HOX=F7.2/42X4HHYD=F7.2,20X28HNET DILUT        
     1ION OF PPT STREAM= F7.2//18X3H21.18X24HOXALATE REACTOR EFFLUENT/44        
     1X2HL=F7.2/44X7HA=   0./44X2HC=F7.2/44X2HS=F7.2/44X2HM=F7.2//18X3H2        
     12.18X61HOXALATE MUD THICKENER OFLOW AND FILTRATE FROM OX MUD FILTE        
     1R  /44X2HL=F7.2,15X5HCONC=F6.2/44X7HA=   0.17X22HWASH WATER TO FIL        
     1TER =F7.2)                                                                
      WRITE (IOU,5018)OXTOFC,A102,OXTOFS,OXOFL,OXMDLQ,OXMDC,OXMDS,FAIDLM        
     1,FAID,FAOFL,FAUFL,FATMP,FAOFC,FAUFC,FALQCC,FAOFS,FAUFS,FALQDN,            
     1FAOFFL,FAUFFL,CPFALQ,CTSFA,WLIME,DGLIM                                    
 5018 FORMAT (44X2HC=F7.2,15X22HLIQUOR TO LIME SLAKER=F7.2/44X2HS=F7.2,         
     115X22HLIQUOR TO WASHERS    =F7.2/1H1,17X,                                 
     1                                     3H23.18X26HOXALATE MUD FROM P        
     1ROCESS  /44X2HL=F7.2/44X7HA=   0./44X2HC=F7.2/44X2HS=F7.2//18X            
     13H24.18X17HFILTER AID PLANT /44X15HAS IS LIME IN= F7.2,15X20HFILTE        
     1R AID PROCESS= F6.2//44X8HOFLOW L=F7.2,12X8HUFLOW L=F7.2,12X6HTEMP        
     1= F5.1/44X8HOFLOW C=F7.2,12X8HUFLOW C=F7.2,12X6HCAUS= F5.1/44X8HOF        
     1LOW S=F7.2,12X8HUFLOW S=F7.2,12X6HDENS= F8.4/43X9HGPM OFLO=F7.2,11        
     1X9HGPM UFLO=F7.2,12X6HSPHT= F8.4/98X6HC/S = F8.4///18X3H25.18X20HL        
     1IME SLAKER INPUT   /44X20HWASHER LIME        =F7.2/44X20HLIME TO B        
     1AUX.SLUR  =F7.2)                                                          
      WRITE (IOU,5019)REACLM,TOLMSL,DILLIM,OXTOFL,TLMH2O,WLIMD,DLMUD,           
     1REACSL,WLMLQ,XLMSL,XRESLM,CWLIM,CDLIM,CRESLM,SWLIM,SDLIM,SRESLM,          
     1TLIME,CPLIM,CPLLQ,XMKUP,CMKUP,TMKUP,CTSMK,SMKUP,PNAOH                     
 5019 FORMAT (44X20HOXAL. REACTOR LIME =F7.2/43X21HTOTAL LIME TO SLAKER=        
     1F7.2//44X20HH2O TO SLAKER      =F7.2/44X20HOXAL.OFLOW/FILTRATE=F7.        
     12/41X23HTOTAL LIQUOR TO SLAKER=F7.2///18X3H26.18X19HLIME SLAKER OU        
     1TPUT /55X7HWASHERS10X7HBX.SLUR10X8HOX.REAC./44X6HSOLIDS5XF7.2,2(10        
     1XF7.2)/44X6HLIQUOR5XF7.2,2(10XF7.2)/44X6HCAUST 5XF7.2,2(10XF7.2)/         
     144X6HSODA  5XF7.2,2(10XF7.2)//44X10HTEMP    = F5.1/44X                    
     110HSPHT SOL= F8.4/44X10HSPHT LIQ= F8.4///18X3H27.18X35HMAKE UP SOD        
     1A AND EVAP/PPT CLEANING /40X23HMAKE UP TO CAUSTIC CLNG/44X2HL=F7.2        
     1/44X2HC=F7.2,15X9HTEMP    =F5.1,15X4HC/S=F5.4/44X2HS=F7.2,15X9HPCT        
     1 NAOH=F5.1/40X23HSPENT MAKEUP FROM CLNG )                                 
      WRITE (IOU,5001)CLNL,DMKUP,FSMKP,ASCL,CPCLN,CLNCC,CMKUP,RMKUP,TCLN        
     1,SMKUP,CTSMK                                                              
      WRITE (IOU,5020)BXREQ,DLMUD,SLURSL,BXMST,XLMSL,BXSPL,SLURLQ,WTSLUR        
     1,CPBX,TBX,CPMST,TBX,CPLIM,TLIME,CPLLQ,TLIME,CPTTK,TSLLQ,TBXSLR,           
     1ST650,HVAP                                                                
 5020 FORMAT (1H117X3H28.18X20HBAUXITE/LIME SLURRY /40X14HDRY BAUXITE =         
     1F7.1/40X14HSLAKED LIME = F7.2/40X12HTOTAL SOLIDS11X1H=F7.1/40X14HB        
     1X MOISTURE =  F7.2/40X14HLIME SLUR LQ= F7.2/40X14HTEST TANK LQ= F7        
     1.1/40X24HTOTAL SLURRY LIQUOR    =F7.1/40X29HTOTAL WEIGHT(S0LIDS AN        
     1D LIQ)=F8.2//50X23HSPECIFIC HEATS AND TEMP/59X7HSP HEAT10X4HTEMP/         
     144X16HDRY BAUXITE     F6.4,11XF5.1/44X16HBAUX MOISTURE   F6.4,11X         
     1F5.1/44X16HLIME SOLIDS     F6.4,11XF5.1/44X16HLIME SLUR LIQ   F6.4        
     1,11XF5.1/44X16HTEST TANK LIQ   F6.4,11XF5.1//44X20HSLURRY TEMPERAT        
     1URE =F6.2///18X3H29.18X18HSTEAM TO DIGESTION/40X8HLBS/HR= F9.0/40X        
     18HBTU/LB= F9.2///18X3H30.18X17HSTARCH UNIT INPUT)                         
      WRITE (IOU,5021)STCHLQ,STCHC,STCHS,STARCH,XSFLR,TOWSTL,CSFLR,             
     1TOWSTC,SSFLR,TOWSTS,FLRS,TOTWST,XTRUF,ATRUF,CTRUF,TRSOL,STRUF,            
     1PISOM,XPISO,SNDL,APISO,SNDA,CPISO,SNDC,SPISO,DLPIS,PISOM,ANDYS,           
     1PIDIL,H2OSND                                                              
 5021 FORMAT (1H 43X3HL= F7.2/44X3HC= F7.2/44X3HS= F7.2/39X8HSTARCH= F7.        
     12//18X3H31.18X19HSTARCH UNIT OUTPUT /45X8HSETTLERS15X7HWASHERS/44X        
     12(2HL=F7.2,12X)/44X2(2HC=F7.2,12X)/44X2(2HS=F7.2,12X)/39X2(7HSTARC        
     1H=F7.2,7X)///18X3H32.18X20HSAND TRAP UNDERFLOW /44X2HL=F7.2/44X           
     12HA=F7.2/44X2HC=F7.2,15X15HUFLOW PCT SOL= F6.2/44X2HS=F7.2/44X2HM=        
     1F7.2/1H1,17X,                                                             
     1          3H33.18X36HEFFLUENTS FROM SAND WASHING SYSTEM  /45X8HTO         
     1WASTE15X10HTO WASHERS/44X2(2HL=F7.2,15X)/44X2(2HA=F7.2,15X)/44X2(         
     12HC=F7.2,15X)/44X2(2HS=F7.2,15X)/44X2HM=F7.2/38X8HPCT SOL=F7.2//44        
     1X11HWASH WATER=F7.2,1X3HAT F5.2,14HLBS/LB OF SAND///18X3H34.18X22H        
     1KELLY PRESS MUD DUMPS )                                                   
      WRITE (IOU,5022)XKPMD,AKPMD,CKPMD,SKPMD,FASC,A101,XHTWL,XSWR,XUNAC        
     1,AHTWL,ASWR,AUNAC,CHTWL,CSWR,CUNAC,SHTWL,SSWR,SUNAC,DILPC,PRDIL,          
     1CLDIL,XSRT,ASRT,CSRT,SSRT,DILSR,IWASH,JWASH,KWASH,XRT1,XRT2,XRT3,         
     1AXRT1,AXRT2,AXRT3,CXRT1,CXRT2,CXRT3,SXRT1,SXRT2,SXRT3                     
 5022 FORMAT (1H 43X2HL=F6.2/44X2HA=F6.2/44X2HC=F6.2/44X2HS=F6.2/44X2HM=        
     1F6.2/44X8HA(COMB)=F5.2///18X3H35.18X                                      
     128HMISCELLANEOUS LIQUOR LOSSES /44X8HHOTWELLS17X6HSEWERS17X9HUNACC        
     1OUNT/44X3(2HL=F7.2,15X)/44X3(2HA=F7.2,15X)/44X3(2HC=F7.2,15X)/44X3        
     1(2HS=F7.2,15X)///18X3H36.18X24HMISCELLANEOUS DILUTIONS /44X17HPPTN        
     1 AND CALCIN =F7.2/44X17HKELLY PRESSES   =F7.2/44X17HDIGEST AND CLA        
     1R =F7.2///18X3H37.18X22HSUMP RELAY TANK INPUT /44X2HL=F7.2/44X2HA=        
     1F7.2/44X2HC=F7.2/44X2HS=F7.2/44X9HDILUTION=F7.2///18X3H38.18X23HSU        
     1MP RELAY TANK OUTPUT /45X3(5HSTAGEI2,15X)/44X3(2HL=F7.2,15X)/44X3(        
     12HA=F7.2,15X)/44X3(2HC=F7.2,15X)/44X3(2HS=F7.2,15X)////)                  
      WRITE (IOU,5023)AUTSA,AUTSH,AUTWA,AUTWH,SOLPHS,SOLPHC,SUFL,DLSO,          
     1FSUF,SUFA,RLSO,CCLSO,SUFC,CTSPFD,SUFS,SUFM,AUTSH,AUTSA,XWOF,AMWOA,        
     1CPWOF,CMWOA,RMWOA,CCWOF,SMWOA,CTSWO,TMWO                                  
 5023 FORMAT (1H1,17X,                                                          
     1           3H39.18X14HAUTO-PRECIP.  /54X5HAL2O310X7HHYDRATE/44X9HS        
     1ETTLERS=2(F6.2,10X)/44X9HWASHERS =2(F6.2,10X)///18X3H40.18X36HSOLI        
     1D PHASE SODA (TAKEN AT SETTLERS)/44X8HSODA   =F9.2/44X8HCAUSTIC=F9        
     1.2///18X3H41.18X13HSETTLER UFLOW/44X2HL=F7.1,15X5HDENS=F6.4,15X5HF        
     1LOW=F7.2/44X2HA=F7.2,15X5HA/C =F6.4,15X5HCONC=F7.2/44X2HC=F7.2,15X        
     15HC/S =F6.4/44X2HS=F7.2/44X2HM=F7.2,12X29HSETTLER AUTOPPTN (HYDRAT        
     1E)=  F6.2/84X8H(AL2O3)=F8.2///18X3H42.18X27HWASHER OFLOW (FIRST ST        
     1AGE) /44X2HL=F8.2/44X2HA=F8.2,15X5HSPHT=F5.4/44X2HC=F8.2,15X              
     15HA/C =F5.4,15X5HCONC=F6.2/44X2HS=F8.2,15X5HC/S =F5.4,15X5HTEMP=F6        
     1.2///)                                                                    
      K=NWASH+1                                                                 
      WRITE (IOU,5024)E(NWASH+100),CAUST,ARVR,Z(K),CRVR,S5LB,SRVR,AUTWH,        
     1FMUD,AUTWA                                                                
 5024 FORMAT (18X3H43.18X26HWASHER UFLOW (LAST STAGE) /44X2HL=F8.2,15X          
     126HWASHER CAUSTICIZATION   = F8.2/44X2HA=F8.2,15X26HWASH WATER TO         
     1LAST STAGE= F8.2/44X2HC=F8.2,15X26HWASH WATER HEATING STEAM= F8.2/        
     144X2HS=F8.2,15X26HWASHER AUTOPPTN (HYDRATE)=F8.2/44X2HM=F8.2,33X8H        
     1(AL2O3)=F8.2///18X3H44.18X15HWASHER PROFILE /47X5HOFLOW21X5HUFLOW/        
     130X5HSTAGE5X6HLIQUOR9X4HSODA7X6HLIQUOR9X4HSODA)                           
      DO 5026 I=1,8                                                             
 5026 WRITE (IOU,5025)I,Z(I),E(10*I+7),E(I+100),E(10*I+9)                       
 5025 FORMAT (1H 31XI1,2X4(F13.2))                                              
      DATA NCASE/0/                                                             
      NCASE = NCASE + 1                                                         
      WRITE (IOU,10000) NCASE                                                   
10000 FORMAT ('1  TELEX'/'0  REFERENCE NO.  QUALUM AA79018'/'0  TO'//           
     1'0  CASE',I2)                                                             
      WRITE (IOU,10006) UNITBX,UNITSD,UNITLM,UNITST                             
10006 FORMAT ('0  A.    ',4F13.4)                                               
      WRITE (IOU,10009) OAR,CDSIL,CCARB,CNONA                                   
10009 FORMAT ('0  D.',11X,F6.5/'0  4.    ',3F13.2)                              
      WRITE (IOU,10013) WLIME,DGLIM,REACLM,FLRS,TOTWST                          
10013 FORMAT ('0 25.    ',3F13.2/'0 31.    ',2F13.2)                            
      WRITE (IOU,10019) XPISO,SPISO,PISOM,ANDYS                                 
10019 FORMAT ('0 33.    ',4F13.2)                                               
      WRITE (IOU,10026) XUNAC,SUNAC,E(NWASH+100),SRVR,FMUD                      
10026 FORMAT ('0 35.    ',2F13.2/'0 43.    ',3F13.2)                            
      RETURN                                                                    
      END                                                                       
   @P?