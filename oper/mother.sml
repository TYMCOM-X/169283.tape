C       MOTHER SPICE
C
C       INTERACTIVE VERSION
C
C
        COMMON/CPLIST/LIST(1/457) /CPLIDX/LSTIDX(0/17)
        COMMON/CPGLOB/ KURSOR, LINSIZ, NATCH, INLINE(270), ISTTY,
     1          HUSH, UNCOLA, IPROMP, IDFILE(5)
        LOGICAL ISTTY,HUSH,UNCOLA
        LOGICAL OPENFILE,XMODP,CHECK
        INTEGER CCSMAX
        LOGICAL CCSLIN,CCSCMD,CCSINI,CCSESC
C
C
C
      COMMON NODPLC(800),YNL(2001),TSTORE(2001),TRACUR(1700),VN(401),
     1   VNIM1(401),IORDER(401),IUR(402),IUC(800),MATLOC(1800)
      COMMON/INDATA/NUMEL,NUNODS,NUMNOD,NOSTOP,JELCNT(20),LOCATE(21),
     1   ICURNT(21),JUNODE(401),NAME(200),LOCAL(200),MNAME(200)
      COMMON/PARAM/VALUE(200),SOURCE(150),SYMVAL(25,25)
      COMMON/MODELS/NUMMOD,MODNAM(25),KIND(25)
      COMMON/MISCEL/NOGO,IGOOF,NOPRNT,IACCT,JOBNAM(16),IDI,IDO
      COMMON/CARDS/ID,NUMFLD,ICARD1,KEOF
      COMMON/STATUS/MODE,OMEGA,TIME,DELTA,DELOLD,ICALC
      COMMON/KNSTNT/TWOPI,XLOG2,XLOG10,RAD,BOLTZ,CHARGE,VT
      COMMON/POINTS/IUS,ILS,MIRROR,NSTOP,NUMVS,LASTUT,LASTLT
      COMMON/OUTDAT/ROUT(101,10),FREQ(101),IONUM,IONAM(10),IOPND(10),
     1   IONND(10),IOFLG(10),NUMOR(3),IOVAR(10,2),IACVAR(5)
      COMMON/OCNTRL/IPRCOM(10,2),IPLCOM(10,2),PLTLIM(10,2,2),
     1   IACPRT(5,4),IACPLT(5,4),ACPLIM(5,4,2)
      COMMON/ITER/GMIN,PERTOL,VNTOL,IPASS1,IFINAL,ITERNO,IFIND
      COMMON/DC/ICVFLG,ITCELM,TCSTAR,TCSTOP,TCINCR,KSSOP,KINEL,KOVAR
      COMMON/TRAN/JTRFLG,TSTEP,TSTOP,TSTART,NOTINT,STEPS(5),ENDPTS(5),
     1   KFROUT,FORFRE,KFPTS
      COMMON/AC/JACFLG,FSTART,FSTOP,IDFREQ,FINCR,INOISE,NOSPRT,
     1   NOSOUT,NOSIN
      COMMON/TEMPER/TEMPS(6),NUMTEM,ITEMNO
      COMMON/SENVAR/NSENS,KSNOUT(10)
      COMMON/DESIGN/JDSFLG,NSPEC,NDES,ERROR,NOELEM(30),NELTPE(30),
     1   NOUNUM(10),DESVAL(10),WEIGHT(10)
        COMMON/NAMPAR/NAMPAR(25,5)
        COMMON/TBLOK/FNDATA(25,5)
        COMMON/SAVE/SOUSAV(150),IACFLG,NSPRNT,NSIN,NSOUT
        COMMON/ERROR/IERCHK,IDCCHK,IACCHK,ITRCHK,ISECHK,IERINP,IANLYZE
        COMMON/OUTPUT/NOUTN(401),NOUTD(200),IPWR,IOPNOD,IOPDEV
        COMMON/STAT/PERSPA
        COMMON/PRISET/ITEM,NAMELE,ISTART,ISTOP,IPALL,ITHPAR
        COMMON/TRU/DTRU,TRU
        COMMON/PLOTP/YMIN(10),YMAX(10),PSCALE(10),NPLOT(10),
     +  ISPT(10),ILG(10),NUMPLT
        LOGICAL IPALL,NMFIND,MODFIND
C
C
      DATA IVERSE /4H 1G /
      DATA NAMPAR /0,'BF','BR','RB','RC','RE',
     1   'CCS','TF','TR','CJE','CJC','IS','PE',
     2   'PC','VA','EG',9*0,
     3   0,'BFM','BRM','RB','RC','RE','CCS',
     4   'TF','TR','CJE','CJC','IS','VA','VB',
     5   'C2','IK','NE','C4','IKR','NC','PE',
     6   'ME','PC','MC','EG',
     7   'RS','TT','CJO','IS','N','PHI','EG',18*0,
     8   0,'VTO','BET','LAM','RD','RS','CGS',
     9   'CGD','PB','IS',15*0,
     A   0,'VTO','PHI','BET','GAM','LAM','RD',
     B   'RS','CGS','CGD','CGB','CBD','CBS','PB',
     C   'IS',10*0/
        DATA LPRN/'('/
C
C  CONSTANTS AND INITIALIZATION
C
        IF(IDEBUG.EQ.1)GO TO 95
        IDEBUG=1
C
        CALL CPINIT
        CALL CCSINI
        CALL CPU(DTRU)
        CALL VALPRI(3,202,'APPUS')
        CALL ERRSET(0)
C
        IPROMP=0
1       IERCHK=0
        IDCCHK=0
        IACCHK=0
        ITRCHK=0
        ISECHK=0
        IERINP=0
        IANLYZE=0
      IACCT=1
      KEOF=0
      VNIM1(1)=0.0
      JUNODE(1)=0
      IUS=402
      ILS=1202
      MIRROR=800
      BOLTZ=1.38053E-23
      CHARGE=1.602095E-19
      GMIN=1.0E-12
      PERTOL=0.01
      VNTOL=100.0E-6
      TEMPS(1)=27.0
      TWOPI=8.0*ATAN2(1.0,1.0)
      RAD=360.0/TWOPI
      XLOG2=ALOG(2.0)
      XLOG10=ALOG(10.0)
        NOGO=0
      VT=BOLTZ*(TEMPS(1)+273.0)/CHARGE
        ICARD1=-1
      ICVFLG=0
      INOISE=0
      IONUM=0
      JACFLG=0
      JDSFLG=0
      JTRFLG=0
      KEXMOD=0
      KFROUT=0
      KINEL=0
      KSSOP=0
      LOCSOR=0
      NDES=0
      NOPRNT=0
      NOSOUT=0
      NOSPRT=0
      NOSTOP=0
      NOTINT=0
      NSENS=0
      NSPEC=0
      NUMEL=0
      NUMMOD=0
      NUMNOD=0
      NUMTEM=1
      NUNODS=0
      DO 5 I=1,3
5     NUMOR(I)=0
        DO 2 I=1,25
        DO 2 J=1,5
2       FNDATA(I,J)=0.0
      DO 10 I=1,20
   10 JELCNT(I)=0
        DO 15 I=1,200
15      NOUTD(I)=0
        DO 20 I=1,401
20      NOUTN(I)=0
        IPWR=0
        IOPNOD=0
        IOPDEV=0
C
        IDO=5
C
C
        GO TO 100
95      UNCOLA=.TRUE.
        IER=0
        GO TO 105
100     UNCOLA=.FALSE.
        NOGO=0
105     CALL LF(5,1)
106     CALL CPPROM
110     CALL FETCHL(0)
        ICOM=LPARSE(1)
        GO TO (200,106,
     +  1000,2000,3000,4000,5000,6000,6000,6000,6000,10000,
     +  11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,
     +  21000,22000,23000,24000,5830,6000,27000,28000,200),ICOM+2
C
C
C       ERROR
C
200     IER=IER+1
        IF(IER.GT.1)GO TO 210
        TYPE 201
201     FORMAT(1H+,'?',/)
        GO TO 106
210     CALL OUTCOM
        TYPE 211
211     FORMAT(1H+,' IS NOT A VALID COMMAND',/)
        GO TO 95
300     TYPE 301
301     FORMAT(1H+,'AMBIGUOUS ABBREVIATION, TYPE MORE CHARACTERS'/)
        GO TO 95
C
C       R U N ,  G O
C
1000    IF(IERINP.EQ.0)GO TO 1010
        IF(.NOT.CHECK(NOARGS))GO TO 95
        CALL ANLYZE
        CALL LF(IDO,1)
        IANLYZE=1
        GO TO 100
1010    TYPE 1011
1011    FORMAT(1H+,'NO PROGRAM'/)
        GO TO 95
C
C       D U M P
C
2000    IF(.NOT.OPENFILE(22,IDFILE,2,2))GO TO 95
        WRITE(22)NODPLC,YNL,TSTORE,TRACUR,VN,
     +  VNIM1,IORDER,IUR,IUC,MATLOC,
     +  NUMEL,NUNODS,NUMNOD,NOSTOP,JELCNT,LOCATE,
     +  ICURNT,JUNODE,NAME,LOCAL,MNAME,
     +  VALUE,SOURCE,SYMVAL,
     +  NUMMOD,MODNAM,KIND,
     +  NOGO,IGOOF,NOPRNT,IACCNT,JOBNAM,IDI,IDO,
     +  ID,NUMFLD,ICARD1,KEOF,
     +  MODE,OMEGA,TIME,DELTA,DELOLD,ICALC,
     +  TWOPI,XLOG2,XLOG10,RAD,BOLTZ,CHARGE,VT,
     +  IUS,ILS,MIRROR,NSTOP,NUMVS,LASTUT,LASTLT,
     +  ROUT,FREQ,IONUM,IONAM,IOPND,
     +  IONND,IOFLG,NUMOR,IOVAR,IACVAR,
     +  IPRCOM,IPLCOM,PLTLIM,
     +  IACPRT,IACPLT,ACPLIM,
     +  GMIN,PERTOL,VNTOL,IPASS1,IFINAL,ITERNO,IFIND,
     +  ICVFLG,ITCELM,TCSTAR,TCSTOP,TCINCR,KSSOP,KINEL,KOVAR,
     +  JTRFLG,TSTEP,TSTOP,TSTART,NOTINT,STEPS,ENDPTS,
     +  KFROUT,FORFRE,KFPTS
        WRITE(22)JACFLG,FSTART,FSTOP,IDFREQ,FINCR,INOISE,NOSPRT,
     +  NOSOUT,NOSIN,
     +   TEMPS,NUMTEM,ITEMNO,
     +   NSENS,KSNOUT,
     +  FNDATA,
     +  SOUSAV,IACFLG,NSPRNT,NSIN,NSOUT,
     +  IERCHK,IDCCHK,IACCHK,ITRCHK,ISECHK,IERINP,IANLYZE,
     +  NOUTN,NOUTD,IPWR,IOPNOD,IOPDEV,
     +  PERSPA
        CLOSE(22)
        GO TO 100
C
C       R E C O V E R
C
3000    IF(.NOT.OPENFILE(22,IDFILE,1,2))GO TO 95
        READ(22)NODPLC,YNL,TSTORE,TRACUR,VN,
     +  VNIM1,IORDER,IUR,IUC,MATLOC,
     +  NUMEL,NUNODS,NUMNOD,NOSTOP,JELCNT,LOCATE,
     +  ICURNT,JUNODE,NAME,LOCAL,MNAME,
     +  VALUE,SOURCE,SYMVAL,
     +  NUMMOD,MODNAM,KIND,
     +  NOGO,IGOOF,NOPRNT,IACCNT,JOBNAM,IDI,IDO,
     +  ID,NUMFLD,ICARD1,KEOF,
     +  MODE,OMEGA,TIME,DELTA,DELOLD,ICALC,
     +  TWOPI,XLOG2,XLOG10,RAD,BOLTZ,CHARGE,VT,
     +  IUS,ILS,MIRROR,NSTOP,NUMVS,LASTUT,LASTLT,
     +  ROUT,FREQ,IONUM,IONAM,IOPND,
     +  IONND,IOFLG,NUMOR,IOVAR,IACVAR,
     +  IPRCOM,IPLCOM,PLTLIM,
     +  IACPRT,IACPLT,ACPLIM,
     +  GMIN,PERTOL,VNTOL,IPASS1,IFINAL,ITERNO,IFIND,
     +  ICVFLG,ITCELM,TCSTAR,TCSTOP,TCINCR,KSSOP,KINEL,KOVAR,
     +  JTRFLG,TSTEP,TSTOP,TSTART,NOTINT,STEPS,ENDPTS,
     +  KFROUT,FORFRE,KFPTS
        READ(22)JACFLG,FSTART,FSTOP,IDFREQ,FINCR,INOISE,NOSPRT,
     +  NOSOUT,NOSIN,
     +   TEMPS,NUMTEM,ITEMNO,
     +   NSENS,KSNOUT,
     +  FNDATA,
     +  SOUSAV,IACFLG,NSPRNT,NSIN,NSOUT,
     +  IERCHK,IDCCHK,IACCHK,ITRCHK,ISECHK,IERINP,IANLYZE,
     +  NOUTN,NOUTD,IPWR,IOPNOD,IOPDEV,
     +  PERSPA
        CLOSE(22)
        GO TO 100
C
C       S E T
C
4000    KURSAV=KURSOR
        ITEM=LPARSE(5)
        IF(ITEM.EQ.25)GO TO 4002
        IF(ITEM.GT.20)GO TO 4700
4002    IF(IERCHK.EQ.1)GO TO 4010
        TYPE 5001
        GO TO 95
4010    IF(ITEM.EQ.13)GO TO 4600
        IF(ITEM.EQ.0)GO TO 200
        KURSOR=KURSAV
        ITEM=LPARSE(6)
        KURSOR=KURSAV
        I=NUMSTR(Z)
        IF(I.EQ.1)GO TO 5900
        CALL IAEQU(NAMELE,Z)
        IF(ITEM.EQ.2.OR.ITEM.EQ.3.OR.ITEM.EQ.4)GO TO 4300
        IF(ITEM.EQ.2.OR.ITEM.EQ.3.OR.ITEM.EQ.4)GO TO 4300
        IF(ITEM.EQ.5)GO TO 4200
        IF(ITEM.EQ.10)GO TO 4400
C
C       LOOK FOR V AFTER CURRENT VARIABLE
C
4200    IV=LPARSE(6)
        IF(IV.NE.10)GO TO 4400
C
C       CONTINUE IF VARIABLE IS A VCCS
C
4300    IF(ITEM.EQ.5)GO TO 4305
        ISTART=LOCATE(ITEM-1)
        ISTOP=LOCATE(ITEM)-1
        GO TO 4306
4305    ISTART=LOCATE(5)
        ISTOP=LOCATE(6)-1
4306    IF(.NOT.NMFIND(NOARGS))GO TO 5900
        I=NUMSTR(R1)
        IF(I.EQ.2)GO TO 4900
        IF(ITEM.EQ.2.OR.ITEM.EQ.4)VALUE(ISTOP)=1.0/R1
        IF(ITEM.EQ.3)VALUE(ISTOP)=R1
        IF(ITEM.EQ.5)GO TO 4310
        GO TO 100
C
C       VCCS...LOOK FOR DELAY VALUE
C
4310    R2=0.0
        IF(NOWCH(1,KANDO).EQ.0)GO TO 4320
        I=NUMSTR(R2)
        IF(I.EQ.2)GO TO 4900
4320    VALUE(ISTOP)=R1
        SOURCE(MNAME(ISTOP))=R2
        GO TO 100
C
C       INDEPENDENT SOURCES
C
4400    DO 4410 I=4,6,2
        ISTART=LOCATE(I)
        ISTOP=LOCATE(I+1)-1
4410    IF(NMFIND(NOARGS))GO TO 4420
        GO TO 5900
4420    MNAM=MNAME(ISTOP)
        ITEM=LPARSE(13)
        GO TO (4450,4430,4430,4440),ITEM+2
C
C       SET DC VALUE
C
4430    I=NUMSTR(Z)
        IF(I.EQ.2)GO TO 4900
        SOUSAV(MNAM)=Z
        GO TO 4500
C
C       SET AC VALUE
C
4440    R2=0.0
        I=NUMSTR(R1)
        IF(I.EQ.2)GO TO 4900
        IF(NOWCH(1,KANDO).EQ.0)GO TO 4445
        I=NUMSTR(R2)
        IF(I.EQ.2)GO TO 4900
4445    SOUSAV(MNAM+1)=R1
        SOUSAV(MNAM+4)=R2
        GO TO 4500
4450    ITYPE=SOUSAV(MNAM+3)
C
C       SET SOURCE PARAMETER VALUES
C
        ITEM=LPARSE(13+ITYPE)
        IF(ITEM.LT.0)GO TO 4910
        IF(ITEM.EQ.0)GO TO 200
        I=NUMSTR(R1)
        IF(I.EQ.2)GO TO 4900
        SOUSAV(MNAM+4+ITEM)=R1
C
4500    DO 4510 I=1,150
4510    SOURCE(I)=SOUSAV(I)
        CALL SOUCHK
        GO TO 100
C
C       SET MODEL PARAMETER
C
4600    I=NUMSTR(Z)
        IF(I.EQ.1)GO TO 5920
        CALL IAEQU(NAMELE,Z)
        IF(.NOT.MODFIND(NOARGS))GO TO 5900
        ITYPE=KIND(ISTART)
        IF(ITYPE.EQ.20)GO TO 4930
        GO TO (4610,4620,4630,4640,4650),ITYPE
4610    ITHPAR=LPARSE(7)
        GO TO 4660
4620    ITHPAR=LPARSE(8)
        GO TO 4660
4630    ITHPAR=LPARSE(9)
        GO TO 4660
4640    ITHPAR=LPARSE(10)
        GO TO 4660
4650    ITHPAR=LPARSE(11)
4660    IF(ITHPAR.LT.0)GO TO 5920
        IF(ITHPAR.EQ.0)GO TO 200
        I=NUMSTR(Z)
        IF(I.EQ.2)GO TO 4910
        CALL MODSET(Z)
        GO TO 100
4700    I=NUMBER(Z)
        IF(I.EQ.2)GO TO 4900
        GO TO (4710,4720,4730,200),ITEM-20
4710    PERTOL=Z
        GO TO 100
4720    VNTOL=Z
        GO TO 100
4730    NITER8=Z
        GO TO 100
4900    TYPE 4901
4901    FORMAT(1H+,'ILLEGAL NUMBER',/)
        GO TO 95
4910    TYPE 4911
4911    FORMAT(1H+,'ILLEGAL SOURCE PARAMETER',/)
        GO TO 100
4930    TYPE 4931
4931    FORMAT(1H+,'EXTERNAL MODEL ELEMENTS MUST BE REFERENCED BY'/
     +  ' THEIR COMPOUND NAMES'/)
        GO TO 95
C
C       P R I N T
C
5000   KURSAV=KURSOR
5002    ITEM=LPARSE(5)
        IF(ITEM.EQ.25)GO TO 5003
        IF(ITEM.GT.20)GO TO 5300
        IF(ITEM.EQ.12)GO TO 5002
5003    IF(IERCHK.EQ.1)GO TO 5005
        TYPE 5001
5001    FORMAT(1H+,'CHECK OR GO COMMAND MUST BE GIVEN FIRST'/)
        GO TO 95
5005    IF(ITEM.EQ.0)ITEM=12
        IF(ITEM.EQ.25)GO TO 5006
        IF(ITEM.GT.11)GO TO 5300
        IPALL=.TRUE.
        IF(ITEM.GT.0)GO TO 5150
5006    KURSOR=KURSAV
        ITEM=LPARSE(6)
        IPALL=.FALSE.
        KURSOR=KURSAV
        I=NUMSTR(Z)
        IF(I.EQ.1)GO TO 5900
        CALL IAEQU(NAMELE,Z)
5009    GO TO (5900,5900,5010,5020,5030,5040,5050,5060,5070,
     +  5080,5090,5100,5900),ITEM+2
C
C       PRINT EXTERNAL MODEL
C
5010    ITIMES=0
        ISTART=LOCATE(20)
        ISTOP=LOCATE(21)-1
        IF(NMFIND(NOARGS))GO TO 5210
C
C       CHECK TO SEE IF ELEMENT IS EXTERNAL ELEMENT
C
        IF(ITIMES.EQ.1)GO TO 5900
        ITIMES=1
        INLINE(1)=ICHAR(NAMELE,3)
        INLINE(2)=32
        KURSOR=1
        ITEM=LPARSE(6)
        GO TO 5009
C
C       PRINT RESISTOR
C
5020    ISTART=1
        ISTOP=LOCATE(2)-1
        GO TO 5200
C
C       PRINT CAPACITOR
C
5030    ISTART=LOCATE(2)
        ISTOP=LOCATE(3)-1
        GO TO 5200
C
C       PRINT INDUCTOR
C
5040    ISTART=LOCATE(3)
        ISTOP=LOCATE(4)-1
        GO TO 5200
C
C       PRINT VOLTAGE CONTROLLED CURRENT SOURCE
C
5050    IV=LPARSE(6)
        IF(IV.NE.10)GO TO 5100
5051    ISTART=LOCATE(5)
        ISTOP=LOCATE(6)-1
        GO TO 5200
C
C       PRINT BJT
C
5060    ISTART=LOCATE(7)
        ISTOP=LOCATE(8)-1
        GO TO 5200
C
C       PRINT DIODE
C
5070    ISTART=LOCATE(8)
        ISTOP=LOCATE(9)-1
        GO TO 5200
C
C       PRINT JFET
C
5080    ISTART=LOCATE(9)
        ISTOP=LOCATE(10)-1
        GO TO 5200
C
C       PRINT MOSFET
C
5090    ISTART=LOCATE(10)
        ISTOP=LOCATE(11)-1
        GO TO 5200
C
C       PRINT INDEPENDENT SOURCE
C
5100    ITEM=10
        DO 5105 I=4,6,2
        ISTART=LOCATE(I)
        ISTOP=LOCATE(I+1)-1
5105    IF(NMFIND(NOARGS))GO TO 5210
        GO TO 5900
C
C       PRINT NODE
C
5110    IPALL=.FALSE.
        I=NUMSTR(Z)
        IF(I.NE.1)GO TO 5910
        NMNODE=Z
        DO 5120 I=1,NUNODS
        IF(JUNODE(I).EQ.NMNODE)GO TO 5130
5120    CONTINUE
        GO TO 5910
5130    ITHPAR=I
        GO TO 5210
C
C
C
5150    IF(ITEM.NE.11)GO TO 5210
        IF(KURSOR.GT.LINSIZ)GO TO 5210
        GO TO 5110
C
C       LOOK UP ELEMENT NAME
C
5200    IF(.NOT.NMFIND(NOARGS))GO TO 5900
5210    CALL ELEPRI
        GO TO 100
5300    GO TO (5350,5400,5500,5510,5520,5530,5540,5600,5700,5800,5810,
     +  5820,5830,5900),ITEM-11
C
C       PRINT ALL
C
5350    IPALL=.TRUE.
        CALL ELEPRI
        CALL MODPRI
        CALL OUTPRI
        GO TO 100
C
C       PRINT MODELS
C
5400    ITEM=LPARSE(5)
        KURSAV=KURSOR
        IF(ITEM.NE.0)GO TO 5410
C
C       PRINT ALL MODELS
C
        IPALL=.TRUE.
        CALL MODPRI
        GO TO 100
5410    I=NUMSTR(Z)
        IF(I.EQ.1)GO TO 5900
        CALL IAEQU(NAMELE,Z)
        IF(.NOT.MODFIND(NOARGS))GO TO 5920
        IPALL=.FALSE.
C
C       LOOK FOR SPECIFIC MODEL PARAMETER
C
        ITYPE=KIND(ISTART)
        GO TO (5420,5430,5440,5450,5460),ITYPE
5420    ITHPAR=LPARSE(7)
        GO TO 5470
5430    ITHPAR=LPARSE(8)
        GO TO 5470
5440    ITHPAR=LPARSE(9)
        GO TO 5470
5450    ITHPAR=LPARSE(10)
        GO TO 5470
5460    ITHPAR=LPARSE(11)
5470    IF(ITHPAR.NE.0)GO TO 5480
C
C       PRINT SPECIFIC MODEL
C
        CALL MODPRI
        GO TO 100
C
C       PRINT SPECIFIC MODEL PARAMETER
C
5480    IF(ITHPAR.LT.0)GO TO 5930
        CALL MODPP
        GO TO 100
C
C       PRINT OUTPUT PARAMETER
C
5500    ISTART=1
        GO TO 5550
C
C       PRINT DC INFO
C
5510    ISTART=2
        GO TO 5550
C
C       PRINT AC INFO
C
5520    ISTART=3
        GO TO 5550
C
C       PRINT TR AND FOURIER INFO
C
5530    ISTART=4
        GO TO 5550
C
C       PRINT SENSITIVITY
C
5540    ISTART=5
5550    IPALL=.FALSE.
        CALL OUTPRI
        GO TO 100
C
C       PRINT TEMPERATURES
C
5600    WRITE(IDO,5601)(TEMPS(I),I=1,NUMTEM)
5601    FORMAT(1H+,6F10.3/)
        GO TO 100
C
C       PRINT MATRIX STATISTICS
C
5700    IF(IANLYZE.EQ.0)GO TO 95
        NNOD=NUMNOD-1
        WRITE(IDO,5701)NNOD,PERSPA
5701    FORMAT(1X,'NODES PERCENT SPARCITY',/1X,I4,4X,F6.3)
        GO TO 100
C
C       PRINT RELERR(PERTOL)
C
5800    WRITE(IDO,5801)PERTOL
5801    FORMAT(1H+,1PE10.3/)
        GO TO 100
C
C       PRINT VNTOL
C
5810    WRITE(IDO,5801)VNTOL
        GO TO 100
C
C       PRINT ITERATE
C
5820    WRITE(IDO,5821)ITERATE
5821    FORMAT(1H+,I4/)
        GO TO 100
C
C       PRINT TRU
C
5830    CALL CPU(TRU)
        DTRU=TRU-DTRU
        WRITE(IDO,5831)DTRU,TRU
5831    FORMAT(1H+,F8.2,' TRU'/1X,F8.2,' TRU'/)
        DTRU=TRU
        GO TO 100
5900    TYPE 5901
5901    FORMAT(1H+,'UNDEFINED ELEMENT'/)
        GO TO 95
5910    TYPE 5911
5911    FORMAT(1H+,'UNDEFINED NODE'/)
        GO TO 95
5920    TYPE 5921
5921    FORMAT(1H+,'UNDEFINED MODEL NAME'/)
        GO TO  95
5930    TYPE 5931
5931    FORMAT(1H+,'IMPROPER MODEL PARMETER'/)
        GO TO 95
C
C       D C
C
6000    IF(IERCHK.EQ.1)GO TO 6010
        TYPE 6001
6001    FORMAT(1H+,'THIS STATEMENT CANNOT BE GIVEN UNTIL CIRCUIT',/1X,
     +  ' HAS BEEN ENTERED AND DEBUGGED'/)
        GO TO 95
6010    KURSOR=1
        IF(ICOM.EQ.6)IDCCHK=0
        IF(ICOM.EQ.7)IACCHK=0
        IF(ICOM.EQ.8)ITRCHK=0
        IF(ICOM.EQ.26)ISECHK=0
        CALL INSPIC
        CALL PARSER
        IF(NOGO.EQ.1)GO TO 95
        IF(.NOT.CHECK(NOARGS))GO TO 95
        IF(ICOM.NE.8)GO TO 100
        DO 6020 I=1,150
6020    SOURCE(I)=SOUSAV(I)
        CALL SOUCHK
        GO TO 100
C
C       A C
C
7000    GO TO 100
C
C       T R
C
8000    GO TO 100
C
C       O U T P U T
C
9000    GO TO 100
C
C       P R O B E
C
10000   IF(IANLYZE.EQ.0)GO TO 10500
        KURSAV=KURSOR
        ITEM=LPARSE(17)
        IF(ITEM.EQ.1)GO TO 10400
        KURSOR=KURSAV
10100   IF(NOWCH(1,KANDO).EQ.0)GO TO 100
        KURSAV=KURSOR
        I=NUMSTR(Z)
        IF(I.EQ.1)GO TO 10200
        CALL LF(5,1)
        CALL OUTLIN(KURSAV,KURSOR)
        TYPE 10201
10201   FORMAT(1H+,' IS AN ',$)
        GO TO 10220
10200   J=Z
        DO 10210 I=2,NUNODS
10210   IF(JUNODE(I).EQ.J)GO TO 10300
        TYPE 28221,J
10220   TYPE 5911
        GO TO 10100
10300   TYPE 10310,J,VNIM1(I)
10310   FORMAT(1X,'NODE ',I4,': ',F10.4)
        GO TO 10100
10400   TYPE 10401
10401   FORMAT(1X,4(' NODE   VOLTAGE   '))
        TYPE 10402,(LPRN,JUNODE(I),VNIM1(I),I=2,NUNODS)
10402   FORMAT(4(1X,A1H),F10.4,2X))
        CALL LF(5,1)
        GO TO 100
10500   TYPE 10501
10501   FORMAT(1X,'GO COMMAND MUST BE GIVEN FIRST',/)
        GO TO 95
C
C       C H E C K
C
11000   IF(.NOT.CHECK(NOARGS))GO TO 95
        GO TO 100
C
C       H E L P
C
12000   GO TO 100
C
C       C A P A B I L I T I E S
C
13000   GO TO 100
C
C       I N S T R U C T I O N S
C
14000   GO TO 100
C
C       P R E M I U M
C
15000   GO TO 100
C
C       V E R S I O N
C
16000   TYPE 16001
16001   FORMAT(1H+,'000000 000011'/' VERSION 1L',/)
        GO TO 100
C
C       C R E D I T S
C
17000   TYPE 17001
17001   FORMAT(1H+,'WRITTEN BY LARRY NAGEL,D. O. PEDERSON, ET AL',/)
        GO TO 100
C
C       H U S H
C
18000   GO TO 100
C
C       N O  H U S H
C
19000   GO TO 100
C
C       T E R M I N A L , T E L E T Y P E , T T Y
C
20000   GO TO 100
C
C       D O
C
21000   GO TO 100
C
C       COPY,INITIALIZE,LIST,MOVE,RENUMBER,QUIT
C
22000   KURSOR=1
        I=CCSCMD(NOARGS)
        GO TO 100
C
C       EDIT,MODIFY,ENTER,DELETE,OPEN,CLEAR
C
23000   KURSOR=1
        I=CCSCMD(NOARGS)
        GO TO 1
C
C       T O U T
C
24000   IF(.NOT.OPENFILE(IDI,IDFILE,1,1))GO TO 95
        IERCHK=0
        IERINP=0
        NOGO=0
24010   CALL FETCHL(IDI)
        CALL INSPIC
        IF(ID.EQ.0)GO TO 24010
        IF(ID.EQ.21)GO TO 24020
        CALL PARSER
        GO TO 24010
24020   IF(NOGO.EQ.0)IERINP=1
        CLOSE(IDI)
        GO TO 100
C
C       C L E A R   O U T P U T
C
27000   ITEM=LPARSE(5)
        IF(ITEM.EQ.0)GO TO 23000
        IF(ITEM.NE.14)GO TO 95
        IONUM=0
        NSENS=0
        JACFLG=0
        JTRFLG=0
        ICVFLG=0
        IDCCHK=0
        IACCHK=0
        ITRCHK=0
        ISECHK=0
        GO TO 100
C
C       O P
C
28000   DO 28010 I=1,200
28010   NOUTD(I)=0
        DO 28020 I=1,NUNODS
28020   NOUTN(I)=0
        IOPNOD=0
        IOPDEV=0
        IPWR=0
28100   ITEM=LPARSE(17)
        GO TO (28102,100,28300,28305,28305,28400,28102),ITEM+2
28102   I=NUMSTR(Z)
        IF(I.EQ.1)GO TO 28200
        CALL IAEQU(NAMELE,Z)
        ISTART=LOCATE(7)
        ISTOP=LOCATE(11)-1
        IF(NMFIND(NOARGS))GO TO 28110
        TYPE 28101,NAMELE
28101   FORMAT(1H+,A5,' IS AN ',$)
        TYPE 5901
        GO TO 28100
28110   NOUTD(ISTART)=1
        IOPDEV=1
        GO TO 28100
28200   NMNODE=Z
        DO 28210 I=1,NUNODS
28210   IF(NMNODE.EQ.JUNODE(I))GO TO 28220
        TYPE 28221,NMNODE
28221   FORMAT(1H+,I4,' IS AN ',$)
        TYPE 5911
        GO TO 28100
28220   NOUTN(I)=1
        IOPNOD=1
        GO TO 28100
28300   ITEM=LPARSE(17)
28305   IALL=0
        GO TO (200,28330,200,28310,28320,28330),ITEM+2
28310   DO 28315 I=1,NUNODS
28315   NOUTN(I)=1
        IOPNOD=1
        IF(IALL.EQ.0)GO TO 28100
28320   ISTART=LOCATE(7)
        ISTOP=LOCATE(11)-1
        DO 28325 I=ISTART,ISTOP
28325   NOUTD(I)=1
        IOPDEV=1
        GO TO 100
28330   IPWR=1
        IALL=1
        GO TO 28310
C
28400   IPWR=1
        GO TO 28100
        END
  DlaM?