;JOB%1(17) TO MAKE PIP.SHR FROM PIP.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=PIP/RESTART:1
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	JOBDAT.REL
;	CREF.SHR
;[SELF]	PIP.MAC
;	PIP.HLP
;
;OUTPUT FILE:
;	PIP.SHR
;OUTPUT LISTINGS:
;	PIP  MAP
;	PIP  CREF LISTING
;	PIP.LOG
;
;
;COPY FILES FROM [10,7] AND USE PRIVATE "SYS:"
.RUN DSK:PIP[10,7]
*/X_DSK:PIP.SHR[10,7],COMPIL.SHR[10,7],MACRO.SHR[10,7],LOADER.SHR[10,7]
*/X_DSK:JOBDAT.REL[10,7],CREF.SHR[10,7]
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF (ERROR)	;TOO BAD
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=PIP???.*+*.SHR+*.REL
.ASSIGN DSK: SYS:
;
;COMPILE, LOAD, AND SAVE; PRODUCING MAP AND CREF FILE
.LOAD /MAP:LPT:PIP /CREF /COMPILE PIP.MAC %A
.SSAVE DSK:PIP
.VERSION
.IF (ERROR) .E 137
;
;TRY IT JUST TO MAKE SURE IT WORKS
.RUN DSK:PIP
*/Q
;
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=PIP.SHR
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.PLEASE PIP SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DELETE PIP.REL,MACRO.SHR,LOADER.SHR,CREF.SHR
.IF (ERROR) ;DON'T CARE IF FAILED
.DELETE COMPIL.SHR,JOBDAT.REL
   