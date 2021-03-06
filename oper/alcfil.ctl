;JOB%1(15) TO MAKE ALCFIL.SAV FROM ALCFIL.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=ALCFIL/RESTART:1
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	JOBDAT.REL
;	CREF.SHR
;[SELF]	ALCFIL.MAC
;
;OUTPUT FILE:
;	ALCFIL.SAV
;OUTPUT LISTINGS:
;	ALCFIL  MAP
;	ALCFIL  CREF LISTING
;	ALCFIL.LOG
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
;****HERE INCLUDE OTHER SUBROUTINES AND THEIR CHECKSUM****
;*****EXAMPLES ARE SCANER,QUEUER ETC                 *****
.ASSIGN DSK: SYS:
;
;COMPILE, LOAD, AND SAVE; PRODUCING MAP AND CREF FILE
.LOAD /MAP:LPT:ALCFIL /CREF /COMPILE ALCFIL.MAC %A
.SSAVE DSK:ALCFIL
.VERSION
.IF (ERROR) .E 137
;
;TRY IT JUST TO MAKE SURE IT WORKS
.RUN DSK:ALCFIL
*/H
.RU DSK: DIRECT[10,7]
*TTY:/CHECKSUM=ALCFIL.SHR+ALCFIL.LOW+ALCFIL.SAV
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.PLEASE ALCFIL SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DELETE ALCFIL.REL,MACRO.SHR,LOADER.SHR,CREF.SHR
.IF (ERROR) ;DON'T CARE IF FAILED
.DELETE COMPIL.SHR,PIP.SHR,JOBDAT.REL
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      