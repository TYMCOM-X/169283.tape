; Job %1(11) to make DAEMON.SAV from DAEMON.MAC
; Submit with command .SUBMIT DAEMON/RESTART:1/TIME:10:0/UNIQ:0

; Required files: [10,7]  (Latest released versions)
;	MACRO.SHR
;	LOADER.SHR
;	COMPIL.SHR
;	JOBDAT.REL
;	SCANER.REL
;[SELF] DAEMON.MAC

; Output files
;	DAEMON.SAV
;	DAEMON.MAP
;	DAEMON.LST
;	DAEMON.LOG

.PLEASE DAEMON SUBMISSION CONTROL FILE INITIALIZING

; Copy files from [10,7] and use private "SYS:"
.RUN DSK:PIP[10,7]
*DSK:(XB)_[10,7]MACRO.SHR,LOADER.SHR,COMPIL.SHR,JOBDAT.REL,SCANER.REL

; Make a record of what is being used
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.SHR
A::.ASSIGN DSK: SYS:

; Compile, Load, Save, and Testing Section
.LOAD /MAP:DAEMON /CREF /COMPILE %0H DAEMON.MAC,SCANER.REL %A
.SAV DSK:DAEMON
.VERSION
.IF (ERROR) .E 137
;
.DEASSIGN


; Produce source listing
.RUN CREF[10,7]
*DSK:DAEMON.LST_DAEMON.CRF
;
; Now get checksummed directories of all files
.RU DIRECT[10,7]
*TTY:=/CHECKSUM DAEMON.MAC,DAEMON.SAV,DAEMON.CTL,DMN???.RN?,JOBDAT.REL,SCANER.REL

; Submission tape creation routine
.OPERATOR %
.RUN TECO[10,7]
*!AGAIN!
*% PLEASE DAEMON.CTL - MAKE A SUBMISSION DECTAPE<YES/NO>?
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

MAKE::.MOUNT DTA:DEC/VID:'DAEMON SUBMISSION TAPE'/WE
.RU PIP[10,7]
*DEC:^^DAEMON^^_/Z
.IF (ERROR)	; OK, we can do without the ID
*DEC:(XB)_DSK:DAEMON.MAC,DAEMON.SAV,DAEMON.CTL,DMN???.RN?
.IF (NOERROR) .GOTO CONT
.PLEASE DAEMON - TROUBLE MAKING SUBMISSION DTA<TRY AGAIN?>
.DEASSIGN
.BACKTO MAKE

CONT::.DIR DEC:
.PLEASE DAEMON - OPERATOR GET A DIRECTORY OF DAEMON SUBMISSION TAPE & DISMOUNT
.REWIND DEC:
.IF (ERROR)
.DEASSIGN

OVER::.RUN PIP[10,7]
*DAEMON.RUN_DMN???.RND
.RUN RUNOFF[10,7]
*DAEMON.RUN
*DAEMON.DOC
.DELETE DAEMON.RUN
.IF (ERROR)

; BLAK Pack update routine
.OPERATOR %
.RUN TECO[10,7]
*!AGAIN!
*% PLEASE DAEMON.CTL - UPDATE THE BLAK PACKS<YES/NO>?
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

UPD::.MOUNT BLKF:/VID:'DAEMON SUBMISSION CONTROL FILE'
.IF (ERROR) .GOTO NOUPD
.RU PIP[10,7]
*BLKF:[7,1342](XB)<100>_DAEMON.MAC,DAEMON.SAV,DAEMON.CTL,DMN???.RN?
.IF (NOERROR) .GOTO BCON
.PLEASE DAEMON - TRANSFER ERROR ON BLACK PACKS  PROCEEDING ...
.GOTO NOUPD
BCON::.PLEASE DAEMON - BLAK PACKS UPDATED SUCCESSFULLY

NOUPD::.PLEASE DAEMON SUCCESSFUL

; Remove all temporary files

%ERR::
%CERR::
%FIN::.DEASSIGN
.DELETE MACRO.SHR,LOADER.SHR,COMPIL.SHR,JOBDAT.REL,SCANER.REL
.IF (ERROR) 	; Don't care if failed
.PLEASE DAEMON DONE
.PRINT /AFTER:+3 DAEMON.LST,DAEMON.MAP/DISP:REN,DAEMON.DOC/COP:3/DISP:REN,DAEMON.LOG
.IF (ERROR) .GOTO END
.K/F
END:
 