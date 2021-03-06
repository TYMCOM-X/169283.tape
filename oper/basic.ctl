; Job %1(11) to make BASIC.SHR,BASIC.LOW from BASIC.MAC
; Submit with command .SUBMIT BASIC/RESTART:1/TIME:10:0/UNIQ:0

; Required files: [10,7]  (Latest released versions)
;	MACRO.SHR
;	LOADER.SHR
;	COMPIL.SHR
;	JOBDAT.REL
;	QUEUER.REL
;	HELPER.REL
;[SELF] BASICH.MAC
;	BASICL.MAC

; Output files
;	BASIC.SHR
;	BASIC.LOW
;	BASIC.MAP
;	BASIC.LST
;	BASIC.LOG

.PLEASE BASIC SUBMISSION CONTROL FILE INITIALIZING

; Copy files from [10,7] and use private "SYS:"
.RUN DSK:PIP[10,7]
*DSK:(XB)_[10,7]MACRO.SHR,LOADER.SHR,COMPIL.SHR,JOBDAT.REL,QUEUER.REL,HELPER.REL

; Make a record of what is being used
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.SHR
A::.ASSIGN DSK: SYS:

; Compile, Load, Save, and Testing Section
.LOAD /MAP:BASIC /CREF /COMPILE BASICL.MAC,BASICH.MAC,/REL QUEUER.REL,HELPER.RELL %A
.SSAV DSK:BASIC
.VERSION
.IF (ERROR) .E 137
;

; Try it just to make sure it works
.RUN DSK:BASIC
*LIS
;
.DEASSIGN


; Produce source listing
.RUN CREF[10,7]
*DSK:BASICL.LST_BASICL.CRF
*DSK:BASICH.LST_BASICH.CRF
;
; Now get checksummed directories of all files
.RU DIRECT[10,7]
*TTY:=/CHECKSUM BASICH.MAC,BASICL.MAC,BASIC.SHR,BASIC.LOW,BASIC.CTL,BAS???.RN?,BASIC.HLP,JOBDAT.REL,QUEUER.REL,HELPER.REL

; Submission tape creation routine
.OPERATOR %
.RUN TECO[10,7]
*!AGAIN!
*% PLEASE BASIC.CTL - MAKE A SUBMISSION DECTAPE<YES/NO>?
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

MAKE::.MOUNT DTA:DEC/VID:'BASIC SUBMISSION TAPE #1'/WE
.RU PIP[10,7]
*DEC:^^BASIC^^_/Z
.IF (ERROR)	; OK, we can do without the ID
*DEC:(XB)_DSK:BASICH.MAC,BASICL.MAC,BASIC.CTL,BAS???.RN?,BASIC.HLP
.DIRECT DEC:
.REWIND DEC:
.DEASSIGN
.PLEASE OPERATOR - GET A DIRECTORY OF BASIC SUBMISSION TAPE AND CONT
.UNLOAD DEC:
.IF (ERROR)	; OK
.MOUNT DTA:DEC/VID:'BASIC SUBMISSION TAPE #2'/WE
.RU PIP[10,7]
*DEC:^^BASIC^^_/Z
.IF (ERROR)	; OK, we can do without the ID
*DEC:(XB)_DSK:BASIC.SHR,BASIC.LOW
.IF (NOERROR) .GOTO CONT
.PLEASE BASIC - TROUBLE MAKING SUBMISSION DTA<TRY AGAIN?>
.DEASSIGN
.BACKTO MAKE

CONT::.DIR DEC:
.DEASSIGN
.PLEASE BASIC - OPERATOR GET A DIRECTORY OF BASIC SUBMISSION TAPE & DISMOUNT
.REWIND DEC:
.IF (ERROR)
.DEASSIGN

OVER::.RUN PIP[10,7]
*BASIC.RUN_BAS???.RND
.RUN RUNOFF[10,7]
*BASIC.RUN
*BASIC.DOC
.DELETE BASIC.RUN
.IF (ERROR)

; BLAK Pack update routine
.OPERATOR %
.RUN TECO[10,7]
*!AGAIN!
*% PLEASE BASIC.CTL - UPDATE THE BLAK PACKS<YES/NO>?
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

UPD::.MOUNT BLKB:/VID:'BASIC SUBMISSION CONTROL FILE'
.IF (ERROR) .GOTO NOUPD
.RU PIP[10,7]
*BLKB:[7,6200](XB)<100>_BASICH.MAC,BASICL.MAC,BASIC.SHR,BASIC.LOW,BASIC.CTL,BAS???.RN?,BASIC.HLP
.IF (NOERROR) .GOTO BCON
.PLEASE BASIC - TRANSFER ERROR ON BLACK PACKS  PROCEEDING ...
.GOTO NOUPD
BCON::.PLEASE BASIC - BLAK PACKS UPDATED SUCCESSFULLY

NOUPD::.PLEASE BASIC SUCCESSFUL

; Remove all temporary files

%ERR::
%CERR::
%FIN::.DEASSIGN
.DELETE MACRO.SHR,LOADER.SHR,COMPIL.SHR,JOBDAT.REL,QUEUER.REL,HELPER.REL
.IF (ERROR) 	; Don't care if failed
.PLEASE BASIC DONE
.PRINT /AFTER:+3 BASIC.LOG,BASICH.LST,BASICL.LST,BASIC.MAP/DISP:REN,BASIC.HLP/COP:3,BASIC.DOC/COP:3/DISP:REN
.IF (ERROR) .GOTO END
.K/F
END:
  