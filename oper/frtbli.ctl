;%1(7) VERSION NUMBER OF FRTBLI.CTL      28 AUG 1973
; NAME:		FORTRAN.CTL
; DATE:	27 JUL 73
;
; FUNCTION:	THIS CONTROL FILE BUILDS THE STANDARD FORTRAN COMPILER
;		FROM ITS BASIC SOURCES.  IT UTILIZES DSKB:[10,7] FOR
;		FIELD IMAGE SOFTWARE AS REQUIRED BY -10 QA.
;
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
;		COMPIL.SHR
;		BLIS10.SHR
;
;	DSK:[AREA UNDER WHICH FRTBLI.CTL IS BEING RUN]
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
;		BLIO.BLI
;		BUILD.BLI
;		DEFLT.BLI
;		ERROR0.BLI
;		ERRTB3.MAC
;		F72BNF.SYN
;		FORMAT.SYN
;		LEFT.BLI
;		LHEAD.BLI
;		NUMIO1.BLI
;		OUTZ.BLI
;		QTAB1.MAC
;		SCAN0.BLI
;		SCNR.BLI
;		SYNTAB.BLI
;		SYNTAX.BLI
;		SYNTHD.BLI
;		TBL.BLI
;		TRACE1.BLI
;
;		REMAINING COMPILER SOURCES:
;
;
;
; OUTPUT:	THE FOLLOWING FILES ARE GENERATED BY THIS CONTROL JOB
;		AND WILL BE AVAILABLE ON THIS DISK AREA AT JOB TERMINATION.
;
;	DSK:[AREA UNDER WHICH FRTBLI.CTL IS BEING RUN]
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
.SET WATCH ALL
.IF (ERROR)  ;OKAY

;
;
; FIRST WE BUILD THE SYNTAX TABLE BUILDERS AND SYNTAX TABLES
;
; FIRST COMPILE AND LOAD THE TABLE BUILDING PROGRAM.

.RUN DSK:BLIS10[10,7]
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

.RUN DSK:BLIS10[10,7]
*SCAN0_SCAN0
;
;
;

.RUN DSK:BLIS10[10,7]
*SCNR_SCNR
;
;
;

.RUN DSK:BLIS10[10,7]
*TRACE1_TRACE1
;
;
;
.RUN DSK:BLIS10[10,7]
*ERROR0_ERROR0
;
;
;

.RUN DSK:BLIS10[10,7]
*BLIO_BLIO
;
;
;

.RUN DSK:BLIS10[10,7]
*NUMIO1_NUMIO1
;
;
;

.RUN DSK:BLIS10[10,7]
*DEFLT_DEFLT
;
;
;

.RUN DSK:BLIS10[10,7]
*TBL_TBL
;
;
;

.RUN DSK:BLIS10[10,7]
*OUTZ_OUTZ
;
;
;
; NOW LOAD THE BUILDER WITH I/O UTILITIES AND EXECUTE
.TYPE IO.CMD
.RUN DSK:LOADER[10,7]
*QTAB1,BLIO,SCAN0,TRACE1,ERRTB3,ERROR0,SCNR,NUMIO1,DEFLT,TBL,OUTZ,BUILD
.SAVE BUILD
;
;
;
; BUILD STATEMENT SYNTAX TABLES
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
*LEFT72_LHEAD,F72BNF,LEFT
*LEFTFM,LEFTFM.MAC/M_SYNTAX,FORMAT,LEFT
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
*3=LOOK72.BLI
*0
;
;
; NOW LOAD, SAVE, AND EXECUTE THE FORMAT LOOK AHEAD TABLE BUILDER
.RUN DSK:LOADER[10,7]
*QTAB1,BLIO,SCAN0,TRACE1,ERRTB3,ERROR0,SCNR,NUMIO1,DEFLT,TBL,OUTZ,LEFTFM
.SAVE LEFTFM
.RUN LEFTFM
*3=LOOKFM.BLI
*0
.ERROR
;
;
;000000000000000000000000000000000000000000000000000000000000
;*****************************************
;NOTE: TO BUILD A PHASE 1 THAT IS 1K SMALLER THE FILE F72BNF.BLI
;MAY BE EDITED TO MAKE THE VECTOR METANAME A COMMENT.
;THE FILE FORMAT.BLI MAY ALSO BE EDITED TO MAKE THE VECTOR
;METANAME A COMMENT.
;THE VECTOR METANAME IS ONLY NEEDED IN BUILDING LEFT72
;AND LEFTFM. ALSO NOTE THAT ANY UNDEFINED GLOBALS
;IN THE LOADS INVOLVED IN THIS BUILDING PROCESS UP TO THIS POINT
;CAN SAFELY BE IGNORED. SUBSEQUENT RELEASES OF THE COMPILER WILL
;NOT GENERATE THESE UNDEFINED GLOBALS. IT IS FURTHER RECOMMENDED
;THAT IF AN INSTALLATION IS NOT MODIFYING THE SYNTAX TABLES THAT
;THE BUILDING PROCESS BE STARTED HERE AT P0STR.
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
;
MAINCK:: .RUN DSK:BLIS10[10,7]
*MAIN/S/G_MAIN.BLI
;
;
;

GLOBCK:: .RUN DSK:BLIS10[10,7]
*GLOBAL/S/G_GLOBHD,FIRST,TABLES,GLOBAL
;
;
;

ERR3CK:: .RUN DSK:MACRO[10,7]
*ERR3_ERR3
;
;
;

;SCNCK::.DIRECT /CHECKSUM SCANHD,C[10,7],SCNMAC[10,7],SCAN[10,7]
; .COMPILE /COMPILE SCANHD+<C[10,7],SCNMAC[10,7],SCAN[10,7]>

; THE FOLLOWING REPLACES THE ABOVE COMPILE CLASS COMMAND
;.RUN DSK:MACRO[10,7]
;*C_SCANHD,C[10,7]
;*SCNMAC_SCANHD,SCNMAC[10,7]
;*SCAN_SCANHD,SCAN[10,7]
;
;WILDCK:: .RUN DSK:MACRO[10,7]
;*WILD_C,SCNMAC,WILD
;
;
;
COMMCK:: .RUN DSK:MACRO[10,7]
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
SRACK::
.CHKPNT SRACK
.SET WATCH ALL
 .RUN DSK:BLIS10[10,7]
*SRCA/S/G_SRCAHD,FIRST,TABLES,SRCA,END.BLI
;
;
;

INOUCK:: .RUN DSK:BLIS10[10,7]
*INOUT/S/G_INOUT.BLI
;
;
;

ERROCK:: .RUN DSK:BLIS10[10,7]
*ERROUT/S/G_ERROUT.BLI
;
;
;

SRCBCK:: .RUN DSK:BLIS10[10,7]
*SRCB/S/G_SRCBHD,FIRST,TABLES,SRCB,END.BLI
;
;
;

FLTGCK:: .RUN DSK:MACRO[10,7]
*FLTGEN_FLTGEN
;
;
;

VLTPCK:: .RUN DSK:BLIS10[10,7]
*VLTPPR/S/G_VLTPHD,FIRST,TABLES,VLTPPR,END.BLI
;
;
;

ARRXCK:: .RUN DSK:BLIS10[10,7]
*ARRXPN/S/G_ARRXHD,FIRST,TABLES,ARRXPN,END.BLI
;
;
;

MAKECK:: .RUN DSK:BLIS10[10,7]
*MAKEPR/S/G_MAKEHD,FIRST,TABLES,MAKEPR,END.BLI
;
;
;

DOXPCK:: .RUN DSK:BLIS10[10,7]
*DOXPN/S/G_DOXPHD,FIRST,TABLES,DOXPN,END.BLI
;
;
;
CNSCK:: .RUN DSK:MACRO[10,7]
*CNSTCM_CNSTCM
;
;
;
DPSICK:: .RUN DSK:MACRO[10,7]
*DPSIM_DPSIM
;
;
;

FAZ1CK:: .RUN DSK:BLIS10[10,7]
*FAZ1/S_FAZ1HD,F72BNF,LOOK72,FIRST,FAZ1,END.BLI
;
;
;

GNRCCK:: .RUN DSK:BLIS10[10,7]
*GNRCFN/S_GNRCHD,FIRST,TABLES,GNRCFN
;
;
;

EXPRCK:: .RUN DSK:BLIS10[10,7]
*EXPRES/S/G_EXPRHD,FIRST,TABLES,EXPRES,END.BLI
;
;
;

FORMCK:: .RUN DSK:BLIS10[10,7]
*FORMAT/S_FORMHD,FORMAT,LOOKFM,FMBODY,END.BLI
;
;
;

ACT0CK:: .RUN DSK:BLIS10[10,7]
*ACT0/S_ACT0HD,FIRST,TABLES,META72,ACT0,END.BLI
;
;
;

ACT1CK:: .RUN DSK:BLIS10[10,7]
*ACT1/S_ACT1HD,FIRST,TABLES,META72,ACT1,END.BLI
;
;
;
STA0CK::.RUN DSK:BLIS10[10,7]
*STA0/S/G_STA0HD,FIRST,TABLES,META72,STA0,END.BLI
;
;
;

STA1CK:: .RUN DSK:BLIS10[10,7]
*STA1/S_STA1HD,FIRST,TABLES,META72,STA1,END.BLI
;
;
;

STA2CK:: .RUN DSK:BLIS10[10,7]
*STA2/S/G_STA2HD,FIRST,TABLES,META72,STA2,END.BLI
;
;
;

STA3CK:: .RUN DSK:BLIS10[10,7]
*STA3/S_STA3HD,FIRST,TABLES,META72,STA3,END.BLI
;
;
;

CLASCK:: .RUN DSK:BLIS10[10,7]
*CLASS/S_CLASHD,FIRST,CLASS.BLI
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
;
P1END::
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

.RUN DSK:BLIS10[10,7]
*MEMCMP/S_MEMCHD,FIRST,TABLES,MEMCMP
;
;
;

SKSTCK:: .RUN DSK:BLIS10[10,7]
*SKSTMN/S/G_SKSTHD,FIRST,TABLES,SKSTMN,END.BLI
;
;
;

P2S1C:: .RUN DSK:BLIS10[10,7]
*P2S1/S/G_P2S1HD,FIRST,TABLES,P2S1,END.BLI
;
;
;

P2S2CK:: .RUN DSK:BLIS10[10,7]
*P2S2/S/G_P2S2HD,FIRST,TABLES,P2S2,END.BLI
;
;
;

CANNCK:: .RUN DSK:BLIS10[10,7]
*CANNON/S/G_CANNHD,FIRST,TABLES,OPTMAC,CANNON,END.BLI
;
;
;

COMSCK:: .RUN DSK:BLIS10[10,7]
*COMSUB/S/G_COMSHD,FIRST,TABLES,OPTMAC,COMSUB,END.BLI
;
;
;

GOPTCK:: .RUN DSK:BLIS10[10,7]
*GOPTIM/S/G_GOPTHD,FIRST,TABLES,OPTMAC,GOPTIM,END.BLI
;
;
;

UTILCK:: .RUN DSK:BLIS10[10,7]
*UTIL/S/G_UTILHD,FIRST,TABLES,UTIL
;
;
;

PH2SCK:: .RUN DSK:BLIS10[10,7]
*PH2S/S/G_PH2SHD,FIRST,TABLES,PH2S,END.BLI
;
;
;
; ALL PHASE 2S SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 2S.
P2SCK::
.CHKPNT P2SCK
.SET WATCH ALL
.TYPE P2S.CMD
.RUN DSK:LOADER[10,7]
*P2S.CMD@
.SSAVE FORTC
;
;
P2SND::
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
.RUN DSK:BLIS10[10,7]
*GRAPH/S_GRAPHD,OPTMAC,FIRST,TABLES,GRAPH,END.BLI
;
;
;

EFPCK:: .RUN DSK:BLIS10[10,7]
*DEFPT/S_DEFPHD,FIRST,TABLES,OPTMAC,DEFPT,END.BLI
;
;
;

PNROCK:: .RUN DSK:BLIS10[10,7]
*PNROPT/S_PNROHD,FIRST,TABLES,OPTMAC,PNROPT,END.BLI
;
;
;

PHA2C:: .RUN DSK:BLIS10[10,7]
*PHA2/S_PHA2HD,FIRST,TABLES,OPTMAC,PHA2,END.BLI
;
;
;
; ALL PHASE 2 SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 2.
P2CK::
.CHKPNT P2CK
.SET WATCH ALL
.TYPE P2.CMD
.RUN DSK:LOADER[10,7]
*P2.CMD@
.SSAV FORTD
;
P2END::
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
.RUN DSK:BLIS10[10,7]
*STREGA/S_STREHD,FIRST,TABLES,STREGA,END.BLI
;
;
;

CMPLCK::.RUN DSK:BLIS10[10,7]
*CMPLEX/S_CMPLHD,FIRST,TABLES,CMPLEX,END.BLI
;
;
;

DOALCK:: .RUN DSK:BLIS10[10,7]
*DOALC/S_DOALHD,FIRST,TABLES,DOALC,END.BLI
;
;
;

REGAC:: .RUN DSK:BLIS10[10,7]
*REGAL2/S_REGAHD,FIRST,TABLES,REGAL2,END.BLI
;
;
;

.RUN DSK:BLIS10[10,7]
*DATAST/S_DATAHD,FIRST,TABLES,DATAST
;
;
;
OUTMCK:: .RUN DSK:BLIS10[10,7]
*OUTMOD/S_OUTMHD,FIRST,TABLES,OUTMOD,END.BLI
;
;
;

PH3GK:: .RUN DSK:BLIS10[10,7]
*PH3G/S_PH3GHD,FIRST,TABLES,OPTMAC,PH3G,END.BLI
;
;
;
; ALL PHASE 3G SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 3G.
P3GCK::
.CHKPNT P3GCK
.SET WATCH ALL
.TYPE P3G.CMD
.RUN DSK:LOADER[10,7]
*P3G.CMD@
.SSAVE FORTE
;
;
P3GND::
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
.RUN DSK:BLIS10[10,7]
*P3R/S_P3RHD,FIRST,TABLES,P3R,END.BLI
;
;
;
; ALL PHASE 3R SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 3R.
.TYPE P3R.CMD
.RUN DSK:LOADER[10,7]
*P3R.CMD@
.SSAVE FORTF
;
;
P3RND::
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
.RUN DSK:BLIS10[10,7]
*OPTAB/S_OPTAHD,OPTAB
;
;
;

CGDOCK:: .RUN DSK:BLIS10[10,7]
*CGDO/S_CGDOHD,FIRST,TABLES,CGDO,END.BLI
;
;
;

CGSTCK:: .RUN DSK:BLIS10[10,7]
*CGSTMN/S_CGSTHD,FIRST,TABLES,CGSTMN,END.BLI
;
;
;

GEXCK:: .RUN DSK:BLIS10[10,7]
*CGEXPR/S_CGEXHD,FIRST,TABLES,CGEXPR
;
;
;
OPGNCK:: .RUN DSK:MACRO[10,7]
*OPGNTA_OPGNTA
;
;
;

PEEPCK:: .RUN DSK:BLIS10[10,7]
*PEEPOP/S_PEEPHD.BLI,FIRST,TABLES,PEEPOP.BLI
;
;
;

LISTCK:: .RUN DSK:BLIS10[10,7]
*LISTOU/S_LISTHD,FIRST,TABLES,LISTOU,END.BLI
;
;
;

PHA3CK:: .RUN DSK:BLIS10[10,7]
*PHA3/S_PHA3HD,FIRST,TABLES,PHA3,END.BLI
;
;
;
; ALL PHASE 3 SOURCES ARE NOW COMPILED.  LET'S LOAD PHASE 3.
P3CK::
.CHKPNT P3CK
.SET WATCH ALL
.TYPE P3.CMD
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
;
.RUN DSKB:DIRECT[10,7]
*FRTBLI.DIR=/CHECKSUM
.PRINT FRTBLI.DIR
;
;
.KJOB/F
; END OF FRTBLI.CTL
    