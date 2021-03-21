;CONTROL FILE TO BUILD ALGOL VERSION 2B(146)
;
;SUBMIT WITH COMMAND	.Q I: =ALGOL/TIME:5000/PAGE:10000
;
;REQUIRED FILES (LATEST RELEASED VERSIONS):
;
;[10,7]	PIP.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	CREF.SHR
;	JOBDAT.REL
;
;[SELF]	ALGPRM.MAC
;	ALGSYS.MAC
;	ALGOTS.MAC
;	ALGLIB.MAC
;	ALGCON.MAC
;	ALGMAC.MAC
;	ALGDEC.MAC
;	ALGSTM.MAC
;	ALGEXP.MAC
;	ALGFOR.MAC
;	ALGUTL.MAC
;	ALGSER.MAC
;	ALGCOD.MAC
;	ALGFUN.MAC
;
;OUTPUT FILES:	ALGOL.SHR,ALG146.SHR,ALGLIB.REL
;
;OUTPUT LISTINGS:	ALGPRM.LST,ALGSYS.LST,ALGOTS.LST,ALGLIB.LST
;			ALGCON.LST,ALGMAC.LST,ALGDEC.LST,ALGSTM.LST
;			ALGEXP.LST,ALGFOR.LST,ALGUTL.LST,ALGSER.LST
;			ALGCOD.LST,ALGFUN.LST
;COPY FILES FROM [10,7]:
;
.RUN DSK:PIP[10,7]
*/X_DSK:[10,7]PIP.SHR,COMPIL.SHR,MACRO.SHR,LOADER.SHR,CREF.SHR,JOBDAT.REL
;
;RECORD VERSION NUMBERS:
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.SHR
.GOTO A
A:.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.REL,*.MAC,ALGOL.CTL,ALG???.RND
;
.ASS DSK: SYS:
;
;ASSEMBLE AND CREF OBJECT TIME SYSTEM AND LIBRARY.
;NOTE THAT ALGPRM AND ALGSYS ARE UNIVERSAL FILES, AND MUST
;ALWAYS BE ASSEMBLED IN THIS ORDER BEFORE ALGOTS AND ALGLIB.
;
;
.R MACRO
*ALGPRM,ALGPRM/C_ALGPRM
*ALGSYS,ALGSYS/C_ALGSYS
*ALGOTS,ALGOTS/C_ALGOTS
*ALGLIB,ALGLIB/C_ALGLIB
;
.LOAD %1H ALGOTS.REL
.SSAVE DSK:ALG146
.VERSION
.IF (ERROR) .E 137
.RU DSK:DIRECT[10,7]
*TTY:/CHECKSUM=ALG146.SHR,ALGLIB.REL
;
;ASSEMBLE AND CREF COMPILER.
;NOTE THAT ALGPRM AND ALGMAC ARE UNIVERSAL FILES,
;AND MUST BE ALWAYS ASSEMBLED IN THIS ORDER.
;
.R MACRO
*ALGPRM,ALGPRM/C/P_ALGPRM
*ALGCON,ALGCON/C/P_ALGCON
*ALGMAC,ALGMAC/C/P_ALGMAC
*ALGDEC,ALGDEC/C/P_ALGDEC
*ALGSTM,ALGSTM/C/P_ALGSTM
*ALGEXP,ALGEXP/C/P_ALGEXP
*ALGFOR,ALGFOR/C/P_ALGFOR
*ALGUTL,ALGUTL/C/P_ALGUTL
*ALGSER,ALGSER/C/P_ALGSER
*ALGCOD,ALGCOD/C/P_ALGCOD
*ALGFUN,ALGFUN/C/P_ALGFUN
;
.LOAD ALGCON,ALGMAC,ALGDEC,ALGSTM,ALGEXP,ALGFOR,ALGUTL,ALGSER,ALGCOD,ALGFUN
.D 0 0 133
.SSAVE DSK:ALGOL
.VERSION
.IF (ERROR) .E 137
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=ALGOL.SHR
;
;PRODUCE LISTINGS:
;
.R CREF
*ALGPRM_ALGPRM
*ALGSYS_ALGSYS
*ALGOTS_ALGOTS
*ALGLIB_ALGLIB
*ALGCON_ALGCON
*ALGMAC_ALGMAC
*ALGDEC_ALGDEC
*ALGSTM_ALGSTM
*ALGEXP_ALGEXP
*ALGFOR_ALGFOR
*ALGUTL_ALGUTL
*ALGSER_ALGSER
*ALGCOD_ALGCOD
*ALGFUN_ALGFUN
.DEA
;
.PLEASE ALGOL SUCCESSFUL
;
;REMOVE UNWANTED FILES
%FIN: .DELETE PIP.SHR,COMPIL.SHR,MACRO.SHR,LOADER.SHR,CREF.SHR,JOBDAT.REL,ALGPRM.REL,ALGSYS.REL
.if (ERROR)
  