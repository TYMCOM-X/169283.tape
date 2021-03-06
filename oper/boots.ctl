;BOOTS.CTL%001 - 05 OCT 71 - TO CREATE BOOTS.REL FOR USE
; WITH WTBOOT, AND A 96K PAPER-TAPE BOOTS FOR HARDWARE-READ-IN
; FROM THE PAPER TAPE READER OF THE PDP-10
;
;SUBMIT THIS CONTROL FILE WITH THE COMMAND
;  .QUEUE INP:=BOOTS.CTL/RESTART:1
;
;REQUIRED FILES:
; IN YOUR OWN AREA,
;	DTBOOT.MAC
; IN THE STANDARD DISTRIBUTION AREA, [10,7], THE
; LATEST RELEASED VERSIONS OF
;	MACRO.SHR
;	CREF.SHR
;	TECO.SHR
;	DIRECT.SHR
;
; THE OUTPUTS OF THIS CONTROL FILE ARE
;	BOOTS.REL	;USABLE WITH WTBOOT
;	BOOTS CREF LISTING, CORRESPONDING TO THE ABOVE REL FILE
;	PAPER TAPE OF 96K ABSOLUTE BOOTS, IN PDP10 HRI(RIM10B) FORMAT
;
;
;THE FOLLOWING ASSEMBLY INSTRUCTIONS ARE EXTRACTED FROM BOOTS.MAC
;
;;ASSEMBLY INSTRUCTIONS:
;;1. TO MAKE LOCATION INDEPENDENT VERSION FOR LOADING WITH WTBOOT PROGRAM
;;	ASSEMBLE WITH NO SPECIAL SYMBOL DEFINITIONS.
;;2. TO MAKE ABSOLUTE PAPER TAPE VERSION, DEFINE CORE TO BE:
;;	40000	FOR 16K
;;	100000	FOR 32K
;;	140000	FOR 48K
;;	200000	FOR 64K
;;	240000	FOR 80K
;;	300000	FOR 96K, ETC
;;
;;	.MAKE FTBOOT.MAC
;;	ICORE=40000
;;	$$
;;	EX$$
;;	.R MACRO
;;	PTP:_FTBOOT,BOOTS
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=BOOTS.MAC,MACRO.SHR[10,7],TECO.SHR[10,7],CREF.SHR[10,7]
.GOTO A
A:.
;MAKE BOOTS.REL FOR WTBOOT AND THE CREF LISTING
.RUN DSK:MACRO[10,7]
*BOOTS,/C_BOOTS
.RUN DSK:CREF[10,7]
*LPT:BOOTS_
;NOW MAKE THE PAPER TAPE OF 96 K BOOTS
.RUN DSK:TECO[10,7]
*EWFTBOOT.MACI
CORE=300000		;MODIFY THIS LINE FOR OTHER SIZES
*EX
.RUN DSK:MACRO[10,7]
*PTP:BOOTS_FTBOOT,BOOTS
;THIS OBJECT PROGRAM BEING AN EXEC MODE BOOTSTRAP, THERE IS
;NO WAY TO TEST IT UNDER BATCH, SO WE LET THAT PASS.
;
;TELL THE OPR WE WERE SUCCESSFUL
.PLEASE BOOTS SUCCESSFUL
;DELETE THE FTBOOT FILE, WHICH IS EXTRANEOUS NOW
.DELETE FTBOOT.MAC
    