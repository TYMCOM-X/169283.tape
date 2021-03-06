;JOB%1(15) TO MAKE QMANGR.SHR FROM QMANGR.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=QMANGR/RESTART:1
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	JOBDAT.REL
;	CREF.SHR
;[SELF]	QMANGR.MAC
;
;OUTPUT FILE:
;	QMANGR.SHR
;OUTPUT LISTINGS:
;	QMANGR  MAP
;	QMANGR  CREF LISTING
;	QMANGR.LOG
;
;
;COPY FILES FROM [10,7] AND USE PRIVATE "SYS:"
.RUN DSK:PIP[10,7]
*/X_DSK:PIP.SHR[10,7],COMPIL.SHR[10,7],MACRO.SHR[10,7],LOADER.SHR[10,7]
*/X_DSK:JOBDAT.REL[10,7],CREF.SHR[10,7]
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.SAV+*.SHR+*.HLP
.GOTO A
A:.RUN DSK: DIRECT[10,7]
*TTY:/CHECKSUM=*.MAC+*.REL
;****HERE INCLUDE OTHER SUBROUTINES AND THERE CHECKSUM****
;*****EXAMPLES ARE SCANER,QUEUER ETC                 *****
.ASSIGN DSK: SYS:
;
;COMPILE, LOAD, AND SAVE; PRODUCING MAP AND CREF FILE
.LOAD /MAP:LPT:QMANGR /CREF /COMPILE QMANGR.MAC %A
.SSAVE DSK:QMANGR
.VERSION
.IF (ERROR) .E 137
;
.RU DSK: DIRECT[10,7]
*TTY:/CHECKSUM=QMANGR.SHR+QMANGR.LOW+QMANGR.SAV
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.PLEASE QMANGR SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DELETE QMANGR.REL,MACRO.SHR,LOADER.SHR,CREF.SHR
.IF (ERROR) ;DON'T CARE IF FAILED
.DELETE COMPIL.SHR,PIP.SHR,JOBDAT.REL

;
;
;
;
;
;
;Copyright 1971, Digital Equipment Corp., Maynard, Mass.
;
;
;Assembly switches may  be  specified  by  appending  to  the
;beginning  of  the  QMANGR  source a file containing the new
;value of the assembly switches.  Assembly switches:
;
;    DEBUG=0 ;do not include code needed only for debugging
;    DEBUG=1 ;include code for debugging
;  default=1
;
;   PURESW=0 ;create non-reentrant version
;   PURESW=1 ;create reentrant version
;  default=1
;
;   SEQUNC=0 ;do not include code for sequence numbers
;   SEQUNC<0 ;include  code  for  sequence  numbers,  do  not
;          override specified sequence numbers
;   SEQUNC>0  ;override  specified  sequence   numbers   with
;          internal sequence numbers
;  default=<0
;
;   LN.MAS=1000 ;this parameter is the size of a master queue
;          window
;
;   REQPRT=xxx ;this parameter is  the  file  protection  for
;          queue request files
;   REQPRT=0 ;use installation standard
;  default=177
;
;   QUEUES: the queues to be listed for ALL may be  specified
;          by including
;
;          DEFINE QUEUES<X<queue 1,queue 2, etc...>>
;
;          Legal queues are INP, LPT, CDP, PTP, PLT.
;  default=INP, LPT, CDP, PTP.
;
;
;
;
;To aid debuging, it is convenient to keep the symbol table  in  the
;high segment.
;
;.LOAD /CREF %S %1B QMANGR.MAC
;
;.SSAVE DSK:QMANGR
;
;.E 116
;
;Write down the contents of 116.  Then, to debug the  high  segment,
;place  a  breakpoint  at location "GOLOW" in QUEUER.  At the
;breakpoint, remove  all  hi-seg  breakpoints,  and  place  a
;breakpoint  at  c(R)+CLHISG.   At  this breakpoint, move the
;breakpoint down one to catch the return.  Examine JOBSYM and
;change  it to the contents of 116 from the above examination
;and open symbol table  QMANGR.   At  the  later  breakpoint,
;restore   the   JOBSYM   contents   and  remove  any  hi-seg
;breakpoints.  Note that breakpoints should be placed in  the
;hi-seg  only if it is not sharable (.SAVE instead of .SSAVE,
;and .ASSIGN DSK SYS).
;
   