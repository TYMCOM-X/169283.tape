; Job %1(6) to make SYSERR.SHR from SYSERR.MAC
; Submit with command .SUBMIT SYSERR/RESTART:1/TIME:10:0/UNIQ:0

; Required files: [10,7]  (Latest released versions)
;	MACRO.SHR
;	LOADER.SHR
;	COMPIL.SHR
;	JOBDAT.REL
;	SCAN.REL
;	HELPER.REL
;	SCNMAC.MAC
;[SELF] SYSERR.MAC

; Output files
;	SYSERR.SHR
;	SYSERR.MAP
;	SYSERR.LST
;	SYSERR.LOG

.PLEASE SYSERR SUBMISSION CONTROL FILE INITIALIZING

; Copy files from [10,7] and use private "SYS:"
.RUN DSK:PIP[10,7]
*DSK:(XB)_[10,7]MACRO.SHR,LOADER.SHR,COMPIL.SHR,JOBDAT.REL,SCAN.REL,HELPER.REL,SCNMAC.MAC

; Make a record of what is being used
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.SHR
A::.ASSIGN DSK: SYS:

; Compile, Load, Save, and Testing Section
.LOAD /MAP:SYSERR /CREF /COMPILE SCNMAC.MAC+SYSERR.MAC(P,,),SCAN,HELPER %A
.SSAV DSK:SYSERR

; Try it just to make sure it works
.RUN DSK:SYSERR
*/HELP
.DEASSIGN


; Produce source listing
.RUN CREF[10,7]
*DSK:SYSERR.LST_SYSERR.CRF
;
; Now get checksummed directories of all files
.RU DIRECT[10,7]
*TTY:=/CHECKSUM SYSERR.MAC,SYSERR.SHR,SYSERR.CTL,SYR???.RN?,SYSERR.HLP,JOBDAT.REL,SCAN.REL,HELPER.REL,SCNMAC.MAC

; Submission tape creation routine
.OPERATOR %
.RUN TECO[10,7]
*!AGAIN!
*% PLEASE SYSERR.CTL - MAKE A SUBMISSION DECTAPE<YES/NO>?
*HK0UM
*!LOOP!UMQM-13"EOLOOP'QM-27"E10UM'QM-10"NQMIOLOOP'
*BJH-3"E:SYES"SOYES''
*H-2"E:SYE"SOYES':SNO"SONO''
*H-1"E:SY"SOYES':SN"SONO''OAGAIN
*!YES!
*YES, MAKE THE TAPE
*OEND!NO!
*? NO, FORGET MAKING THE TAPE
=!END!
.NOOPERATOR
.IF (ERROR) .GOTO OVER

MAKE::.MOUNT DTA:DEC/VID:'SYSERR SUBMISSION TAPE'/WE
.RU PIP[10,7]
*DEC:^^SYSERR^^_/Z
.IF (ERROR)	; OK, we can do without the ID
*DEC:(XB)_DSK:SYSERR.MAC,SYSERR.SHR,SYSERR.CTL,SYR???.RN?,SYSERR.HLP
.IF (NOERROR) .GOTO CONT
.PLEASE SYSERR - TROUBLE MAKING SUBMISSION DTA<TRY AGAIN?>
.DEASSIGN
.BACKTO MAKE

CONT::.DIR DEC:
.PLEASE SYSERR - OPERATOR GET A DIRECTORY OF SYSERR SUBMISSION TAPE & DISMOUNT
.REWIND DEC:
.IF (ERROR)
.DEASSIGN

OVER::.RUN PIP[10,7]
*SYSERR.RUN_SYR???.RND
.RUN RUNOFF[10,7]
*SYSERR.RUN
*SYSERR.DOC
.DELETE SYSERR.RUN
.IF (ERROR)

; BLAK Pack update routine
.OPERATOR %
.RUN TECO[10,7]
*!AGAIN!
*% PLEASE SYSERR.CTL - UPDATE THE BLAK PACKS<YES/NO>?
*HK0UM
*!LOOP!UMQM-13"EOLOOP'QM-27"E10UM'QM-10"NQMIOLOOP'
*BJH-3"E:SYES"SOYES''
*H-2"E:SYE"SOYES':SNO"SONO''
*H-1"E:SY"SOYES':SN"SONO''OAGAIN
*!YES!
*YES, UPDATE THE BLAK PACKS
*OEND!NO!
*? NO, FORGET UPDATING THE BLAK PACKS
=!END!
.NOOPERATOR
.IF (ERROR) .GOTO NOUPD

UPD::.MOUNT BLKF:/VID:'SYSERR SUBMISSION CONTROL FILE'
.IF (ERROR) .GOTO NOUPD
.RU PIP[10,7]
*BLKF:[7,1342](XB)<100>_SYSERR.MAC,SYSERR.SHR,SYSERR.CTL,SYR???.RN?,SYSERR.HLP
.IF (NOERROR) .GOTO BCON
.PLEASE SYSERR - TROUBLE UPDATING THE BLAK PACKS<TRY AGAIN?>
.BACKTO UPD

BCON::.PLEASE SYSERR - BLAK PACKS UPDATED SUCCESSFULLY

NOUPD::.PLEASE SYSERR SUCCESSFUL

; Remove all temporary files

%ERR::
%CERR::
%FIN::.DEASSIGN
.DELETE MACRO.SHR,LOADER.SHR,COMPIL.SHR,JOBDAT.REL,SCAN.REL,HELPER.REL,SCNMAC.MAC
.IF (ERROR) 	; Don't care if failed
.PLEASE SYSERR DONE
.PRINT /AFTER:+3 SYSERR.LST,SYSERR.MAP/DISP:REN,SYSERR.HLP/COP:3,SYSERR.DOC/COP:3/DISP:REN,SYSERR.LOG
.IF (ERROR) .GOTO END
.K/F
END:
  