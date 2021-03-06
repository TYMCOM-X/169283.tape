; %1(7) VERSION NUMBER OF FRTMAC.CTL		11-SEP-73
;
; FUNCTION:	THIS CONTROL FILE BUILDS THE STANDARD FORTRAN COMPILER
;		FROM ITS MACRO SOURCES.  IT UTILIZES DSKB:[10,7] FOR
;		FIELD IMAGE SOFTWARE AS REQUIRED BY -10 QA.
;
;			IT IS RECOMMENDED THAT USERS INTERESTED IN
;			BUILDING THE FORTRAN-10 COMPILER FROM THE
;			MACRO SOURCES START WITH P0STR. THIS
;			DOES NOT REBUILD THE SYNTAX TABLES. THE PROCESS
;			THAT REBUILDS THE SYNTAX TABLES REQUIRES THE USE
;			OF BLIS10.
; INPUT:	THE FOLLOWING FILES ARE REQUIRED BY THIS JOB IN THE
;		DISK AREAS INDICATED:
;
;	DSKB:[10,7]
;		C.MAC
;		SCNMAC.MAC
;		SCAN.REL
;		WILD.REL
;		HELPER.REL
;		DIRECT.SHR
;		JOBDAT.REL
;		LOADER.SHR
;		MACRO.SHR
;
;	DSK:[AREA UNDER WHICH FRTMAC.CTL IS BEING RUN]
;
;		INDIRECT COMMAND FILES USED BY FORTRAN.CTL:
;		IO.CMD		LOADS I/O UTILITIES FOR SYNTAX TABLE BUILDERS
;		P0.CMD		LOADS FORTRA
;		P1.CMD		LOADS FORTB
;		P2S.CMD		LOADS FORTC
;		P2.CMD		LOADS FORTD
;		P3R.CMD		LOADS FORTE
;		P3G.CMD		LOADS FORTF
;		P3.CMD		LOADS FORTG
;
;		SOURCES FOR BUILDING SYNTAX TABLES:
;		BLIO.MAC
;		BUILD.MAC
;		DEFLT.MAC
;		ERROR0.MAC
;		ERRTB3.MAC
;		F72BNF.SYN
;		FORMAT.SYN
;		LEFT.MAC
;		LHEAD.MAC
;		NUMIO1.MAC
;		OUTZ.MAC
;		QTAB1.MAC
;		SCAN0.MAC
;		SCNR.MAC
;		SYNTAB.MAC
;		SYNTAX.MAC
;		SYNTHD.MAC
;		TBL.MAC
;		TRACE1.MAC
;
;		REMAINING COMPILER SOURCES:
;
; OUTPUT:	THE FOLLOWING FILES ARE GENERATED BY THIS CONTROL JOB
;		AND WILL BE AVAILABLE ON THIS DISK AREA AT JOB TERMINATION.
;
;	DSK:[AREA UNDER WHICH FRTMAC.CTL IS BEING RUN]
;
;		FROM SYNTAX TABLE BUILDING SECTION:
;		*	INDICATES REQUIRED FORTRAN COMPILER SOURCE COMPONENT
;		BLIO.REL
;		BUILD.HGH
;		BUILD.LOW
;		BUILD.LST
;		BUILD.REL
;		DEFLT.REL
;		ERROR0.REL
;		ERRTB3.REL
;	       *F72BNF.BLI
;	       *FORMAT.BLI
;		LEFT72.LST
;		LEFT72.REL
;		LEFTFM.LST
;		LEFTFM.REL
;	       *LOOK72.BLI
;	       *LOOKFM.BLI
;	       *META72.BLI
;		METAFM.BLI
;		NUMIO1.REL
;		OUTZ.REL
;		QTAB1.REL
;		SCAN0.REL
;		SCNR.REL
;		TBL.REL
;		TRACE1.REL
;
;
;
;

P0BEG::
.CHKPNT P0BEG
.SET WATCH ALL

;
;
; FIRST WE BUILD THE SYNTAX TABLE BUILDERS AND SYNTAX TABLES
;
; FIRST COMPILE AND LOAD THE TABLE BUILDING PROGRAM.

.RUN DSK:MACRO[10,7]
*BUILD_BUILD
;
;
;
; NOW COMPILE ALL I/O UTILITIES.
;
;
.RUN DSK:MACRO[10,7]
*QTAB1_QTAB1
;
;
;
.RUN DSK:MACRO[10,7]
*ERRTB3_ERRTB3
;
;
;

.RUN DSK:MACRO[10,7]
*SCAN0_SCAN0
;
;
;

.RUN DSK:MACRO[10,7]
*SCNR_SCNR
;
;
;

.RUN DSK:MACRO[10,7]
*TRACE1_TRACE1
;
;
;
.RUN DSK:MACRO[10,7]
*ERROR0_ERROR0
;
;
;

.RUN DSK:MACRO[10,7]
*BLIO_BLIO
;
;
;

.RUN DSK:MACRO[10,7]
*NUMIO1_NUMIO1
;
;
;

.RUN DSK:MACRO[10,7]
*DEFLT_DEFLT
;
;
;

.RUN DSK:MACRO[10,7]
*TBL_TBL
;
;
;

.RUN DSK:MACRO[10,7]
*OUTZ_OUTZ
;
;
;
; NOW LOAD THE BUILDER WITH I/O UTILITIES AND EXECUTE
.TYPE IO.CMD
.IF (ERROR)	; OK
.RUN DSK:LOADER[10,7]
*QTAB1,BLIO,SCAN0,TRACE1,ERRTB3,ERROR0,SCNR,NUMIO1,DEFLT,TBL,OUTZ,BUILD
.SAVE BUILD
;
;
;
;BLIS10 MUST BE USED HERE.
;
; BUILD STATEMENT SYNTAX TABLES
;
.NOERROR
.RUN DSK:BUILD
*META72.BLI,F72BNF.BLI_F72BNF.SYN
*0
*N
;
;
;
; NOW BUILD FORMAT SYNTAX TABLES
.RUN DSK:BUILD
*METAFM.BLI,FORMAT.BLI_FORMAT.SYN
*0
*N
;
;
.ERROR

; NOW DO THE COMPILATIONS FOR THE LOOK AHEAD TABLE BUILDERS.

.RUN DSK:BLIS10[10,7]
*LEFT72/C/S,LEFT72.MAC/M_LHEAD,F72BNF,LEFT
*LEFTFM/C/S,LEFTFM.MAC/M_SYNTAX,FORMAT,LEFT
;
;
;
; NOW LOAD, SAVE, AND EXECUTE THE GENERAL SYNTAX LOOK AHEAD BUILDER
PKLOD::
.CHKPNT PKLOD
.SET WATCH ALL
.NOERROR
;IGNORE ERRORS THROUGHOUT THIS SECTION FOR NOW......
.RUN DSK:LOADER[10,7]
*QTAB1,BLIO,SCAN0,TRACE1,ERRTB3,ERROR0,SCNR,NUMIO1,DEFLT,TBL,OUTZ,LEFT72/P
.SAVE LEFT72
.RUN DSK:LEFT72
*3_LOOK72.BLI
*0
;
;
; NOW LOAD, SAVE, AND EXECUTE THE FORMAT LOOK AHEAD TABLE BUILDER
.RUN DSK:LOADER[10,7]
*QTAB1,BLIO,SCAN0,TRACE1,ERRTB3,ERROR0,SCNR,NUMIO1,DEFLT,TBL,OUTZ,LEFTFM
.SAVE LEFTFM
.RUN LEFTFM
*3_LOOKFM.BLI
*0
.ERROR
;
;
;
;000000000000000000000000000000000000000000000000000000000000
;*****************************************
;****************************************************
;
;
; NOW LET'S COMPILE AND LOAD PHASE 0.
;
;
;
; ********** FORTRA.CTL **********

P0STR::
.CHKPNT P0STR
.SET WATCH ALL
.RUN DSK:MACRO[10,7]
*MAIN_MAIN
;
;
;

GLOBK:: .RUN DSK:MACRO[10,7]
*GLOBAL_GLOBAL
;
;
;

ERR3K:: .RUN DSK:MACRO[10,7]
*ERR3_ERR3
;
;
;

COMMK:: .RUN DSK:MACRO[10,7]
*JOBD_JOBD
*SCANDB_SCANDB
*COMMAN_C[10,7],SCNMAC[10,7],COMMAN
;
;
;

; ALL PHASE 0 SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 0.
P0CK::
.CHKPNT P0CK
.SET WATCH ALL
.TYPE P0.CMD
.IF (ERROR)  ;OKAY
.RUN DSK:LOADER[10,7]
*P0.CMD@
.SSAVE FORTRA
;
;
;
P0END::
; THIS IS THE END OF PHASE 0 BUILDING SECTION.
;000000000000000000000000000000000000000000000000000000000000
; ********** FORTB.CTL **********

;111111111111111111111111111111111111111111111111111111111111
; NOW LET'S COMPILE AND LOAD PHASE 1.
;
;
;
SRCAK::
.CHKPNT SRCAK
.SET WATCH ALL
.RUN DSK:MACRO[10,7]
*SRCA_SRCA
;
;
;

INOUK:: .RUN DSK:MACRO[10,7]
*INOUT_INOUT
;
;
;

ERROK:: .RUN DSK:MACRO[10,7]
*ERROUT_ERROUT
;
;
;

SRCBK:: .RUN DSK:MACRO[10,7]
*SRCB_SRCB
;
;
;

FLTGK:: .RUN DSK:MACRO[10,7]
*FLTGEN_FLTGEN
;
;
;

VLTPC:: .RUN DSK:MACRO[10,7]
*VLTPPR_VLTPPR
;
;
;

ARRXC:: .RUN DSK:MACRO[10,7]
*ARRXPN_ARRXPN
;
;
;

MAKEC:: .RUN DSK:MACRO[10,7]
*MAKEPR_MAKEPR
;
;
;

DOXPC:: .RUN DSK:MACRO[10,7]
*DOXPN_DOXPN
;
;
;
CNSCK:: .RUN DSK:MACRO[10,7]
*CNSTCM_CNSTCM
;
;
;
DPSIC:: .RUN DSK:MACRO[10,7]
*DPSIM_DPSIM
;
;
;

FAZ1C:: .RUN DSK:MACRO[10,7]
*FAZ1_FAZ1
;
;
;

GNRCC:: .RUN DSK:MACRO[10,7]
*GNRCFN_GNRCFN
;
;
;

EXPRC:: .RUN DSK:MACRO[10,7]
*EXPRES_EXPRES
;
;
;

FORMC:: .RUN DSK:MACRO[10,7]
*FORMAT_FORMAT
;
;
;

ACT0C:: .RUN DSK:MACRO[10,7]
*ACT0_ACT0
;
;
;

ACT1C:: .RUN DSK:MACRO[10,7]
*ACT1_ACT1
;
;
;
STA0C::.RUN DSK:MACRO[10,7]
*STA0_STA0
;
;
;

STA1C:: .RUN DSK:MACRO[10,7]
*STA1_STA1
;
;
;

STA2C:: .RUN DSK:MACRO[10,7]
*STA2_STA2
;
;
;

STA3C:: .RUN DSK:MACRO[10,7]
*STA3_STA3
;
;
;

CLASC:: .RUN DSK:MACRO[10,7]
*CLASS_CLASS
;
;
;
; ALL PHASE 1 SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 1.
P1CK::
.CHKPNT P1CK
.SET WATCH ALL
.TYPE P1.CMD
.IF (ERROR)
.RUN DSK:LOADER[10,7]
*P1.CMD@
.SSAVE FORTB
;
; THIS IS THE END OF PHASE 1 BUILDING SECTION.
;111111111111111111111111111111111111111111111111111111111111
; ********** FORTC.CTL **********
;2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S
; NOW LET'S COMPILE AND LOAD PHASE 2S.
;
;
;

MEMCK::
.CHKPNT MEMCK
.SET WATCH ALL

.RUN DSK:MACRO[10,7]
*MEMCMP_MEMCMP
;
;
;

SKSTC:: .RUN DSK:MACRO[10,7]
*SKSTMN_SKSTMN
;
;
;

P2S1C:: .RUN DSK:MACRO[10,7]
*P2S1_P2S1
;
;
;

P2S2C:: .RUN DSK:MACRO[10,7]
*P2S2_P2S2
;
;
;

CANNC:: .RUN DSK:MACRO[10,7]
*CANNON_CANNON
;
;
;

COMSC:: .RUN DSK:MACRO[10,7]
*COMSUB_COMSUB
;
;
;

GOPTC:: .RUN DSK:MACRO[10,7]
*GOPTIM_GOPTIM
;
;
;

UTILC:: .RUN DSK:MACRO[10,7]
*UTIL_UTIL
;
;
;

PH2SC:: .RUN DSK:MACRO[10,7]
*PH2S_PH2S
;
;
;
; ALL PHASE 2S SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 2S.
P2SCK::
.CHKPNT P2SCK
.SET WATCH ALL
.TYPE P2S.CMD
.IF (ERROR)	; OK
.RUN DSK:LOADER[10,7]
*P2S.CMD@
.SSAVE FORTC
;
; THIS IS THE END OF PHASE 2S BUILDING SECTION.
;2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S
; ********** FORTD.CTL **********


;222222222222222222222222222222222222222222222222222222222222
; NOW LET'S COMPILE AND LOAD PHASE 2.
;
;
;

GRACK::
.CHKPNT GRACK
.SET WATCH ALL

.RUN DSK:MACRO[10,7]
*GRAPH_GRAPH
;
;
;

EFPCK:: .RUN DSK:MACRO[10,7]
*DEFPT_DEFPT
;
;
;

PNROC:: .RUN DSK:MACRO[10,7]
*PNROPT_PNROPT
;
;
;

PHA2C:: .RUN DSK:MACRO[10,7]
*PHA2_PHA2
;
;
;
; ALL PHASE 2 SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 2.
P2CK::
.CHKPNT P2CK
.SET WATCH ALL
.TYPE P2.CMD
.IF (ERROR)	; OK
.RUN DSK:LOADER[10,7]
*P2.CMD@
.SSAV FORTD
;
;
; THIS IS THE END OF PHASE 2 BUILDING SECTION.
;222222222222222222222222222222222222222222222222222222222222
; ********** FORTE.CTL **********


;3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G
; NOW LET'S COMPILE AND LOAD PHASE 3G.
;
;
;
STREK::
.CHKPNT STREK
.SET WATCH ALL
.RUN DSK:MACRO[10,7]
*STREGA_STREGA
;
;
;

CMPLC::.RUN DSK:MACRO[10,7]
*CMPLEX_CMPLEX
;
;
;

DOALC:: .RUN DSK:MACRO[10,7]
*DOALC_DOALC
;
;
;

REGAC:: .RUN DSK:MACRO[10,7]
*REGAL2_REGAL2
;
;
;

.RUN DSK:MACRO[10,7]
*DATAST_DATAST
;
;
;

OUTMC:: .RUN DSK:MACRO[10,7]
*OUTMOD_OUTMOD
;
;
;


.RUN DSK:MACRO[10,7]
*PH3G_PH3G
;
;
;
; ALL PHASE 3G SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 3G.
P3GCK::
.CHKPNT P3GCK
.SET WATCH ALL
.TYPE P3G.CMD
.IF (ERROR)	; OK
.RUN DSK:LOADER[10,7]
*P3G.CMD@
.SSAVE FORTE
;
; THIS IS THE END OF THE PHASE 3G BUILDING SECTION.
;3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G3G
; ********** FORTF.CTL **********


;3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R
; NOW LET'S COMPILE AND LOAD PHASE 3R.
;
;
;

P3RCK::
.CHKPNT P3RCK
.SET WATCH ALL
.RUN DSK:MACRO[10,7]
*P3R_P3R
;
;
;
; ALL PHASE 3R SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 3R.
.TYPE P3R.CMD
.IF (ERROR)	; OK
.RUN DSK:LOADER[10,7]
*P3R.CMD@
.SSAVE FORTF
;
; THIS IS THE END OF THE PHASE 3R BUILDING SECTION.
;3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R3R
; ********** FORTG.CTL **********


;333333333333333333333333333333333333333333333333333333333333
; NOW LET'S COMPILE AND LOAD PHASE 3.
;
;
;
OPTCK::
.CHKPNT OPTCK
.SET WATCH ALL

.RUN DSK:MACRO[10,7]
*OPTAB_OPTAB
;
;
;

CGDOC:: .RUN DSK:MACRO[10,7]
*CGDO_CGDO
;
;
;

CGSTC:: .RUN DSK:MACRO[10,7]
*CGSTMN_CGSTMN
;
;
;

GEXCK:: .RUN DSK:MACRO[10,7]
*CGEXPR_CGEXPR
;
;
;
OPGNC:: .RUN DSK:MACRO[10,7]
*OPGNTA_OPGNTA
;
;
;

PEEPC:: .RUN DSK:MACRO[10,7]
*PEEPOP_PEEPOP
;
;
;

.RUN DSK:MACRO[10,7]
*LISTOU_LISTOU
;
;
;

PHA3C:: .RUN DSK:MACRO[10,7]
*PHA3_PHA3
;
;
;
; ALL PHASE 3 SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 3.

P3CK::
.CHKPNT P3CK
.SET WATCH ALL
.TYPE P3.CMD
.IF (ERROR)	; OK
.RUN DSK:LOADER[10,7]
*P3.CMD@
.SSAVE FORTG
P3END::
;
; THIS IS THE END OF THE PHASE 3 BUILDING SECTION.
;333333333333333333333333333333333333333333333333333333333333
;
;
; NOW THAT COMPILER BUILDING IS COMPLETE, LET'S GET A
; CHECKSUMMED DIRECTORY OF ALL FILES.

MKTAP::
.PLEASE FRTMAC - GETTING CHECKSUMMED DIRECTORY OF ENTIRE DSK AREA   
.RUN DSK:DIRECT[10,7]
*FRTMAC=DSK:/CHECKSUM
;
.PRINT FRTMAC.DIR
.IF (ERROR)	; OKAY
.KJOB/F
;
;
; END OF FRTMAC.CTL
   