;JOB%1(15) TO MAKE COPY.SAV FROM COPY.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=COPY/RESTART:1
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	JOBDAT.REL
;	CREF.SHR
;[SELF]	COPY.MAC
;	DTBOOT.MAC
;
;OUTPUT FILE:
;	COPY.SAV
;OUTPUT LISTINGS:
;	COPY  MAP
;	COPY  CREF LISTING
;	COPY.LOG
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
*TTY:/CHECKSUM=*.SAV
.GOTO A
A:.RUN DSK: DIRECT[10,7]
*TTY:/CHECKSUM=*.MAC+*.REL
;****HERE INCLUDE OTHER SUBROUTINES AND THERE CHECKSUM****
;*****EXAMPLES ARE SCANER,QUEUER ETC                 *****
.ASSIGN DSK: SYS:
;
;COMPILE, LOAD, AND SAVE; PRODUCING MAP AND CREF FILE
.LOAD /MAP:LPT:COPY /CREF /COMPILE COPY.MAC %A
.SAVE DSK:COPY
.VERSION
.IF (ERROR) .E 137
;
;ASSEMBLE DTBOOT WITH CONDITIONAL SWITCH REL SET TO "1"
;  PRODUCING BSLDR.REL
.RUN DSK:MACRO
*BSLDR.REL,_TTY:,DSK:DTBOOT
REL==1
^Z
^Z
.ASSIGN DSK PTR    ;ASSIGN LOGICAL NAME "PTR" FOR COPY'S "/L" SWITCH
;TRY IT JUST TO MAKE SURE IT WORKS
.RUN DSK:COPY
*/H
*/L
.SAVE DSK:COPY	;SAVE COPY WITH A BOOTSTRAP LOADER IN IT

.RU DSK: DIRECT[10,7]
*TTY:/CHECKSUM=COPY.SAV
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.PLEASE COPY SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DELETE COPY.REL,MACRO.SHR,LOADER.SHR,CREF.SHR,BSLDR.REL
.IF (ERROR) ;DON'T CARE IF FAILED
.DELETE COMPIL.SHR,PIP.SHR,JOBDAT.REL
;COPY.OPR
;
;TO LOAD TYPE	"LOAD COPY"
;TO SAVE TYPE	"SAVE COPY"
;
;COPY MAY BE LOADED WITH DTBOOT AS FOLLOWS:
;
;		.R MACRO
;		*DTBOOT,_TTY:,DSK:DTBOOT
;		*REL==1
;		^Z
;		END OF PASS 1		;TYPED BY MACRO
;		^Z
;
;		.ASSIGN DSK PTR
;		.EXECUTE COPY
;		*/L
;		*^C
;		.SAVE COPY
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    