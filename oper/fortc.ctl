;FILENAME:	FORTC.CTL
;DATE:		14 FEB 73
;QUEUER:	.QUEUE INP:/UNIQUE:0/TIME:30:00=FORTC
;
;
;
.NOERROR
.R SETSRC
*C FORT,DSKB
*T
.MOUNT FORT
;2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S
;NOW LET US COMPILE AND LOAD PHASE 2S.
;
;
;
SKSTCK:: .CHKPNT SKSTCK
.R BLIS10
*/M/S/GSKSTMN_SKSTHD,FIRST,TABLES,SKSTMN,END
;
;
;
P2S1CK:: .CHKPNT P2S1CK
.R BLIS10
*/M/S/GP2S1_P2S1HD,FIRST,TABLES,P2S1,END
;
;
;
P2S2CK:: .CHKPNT P2S2CK
.R BLIS10
*/M/S/GP2S2_P2S2HD,FIRST,TABLES,P2S2,END
;
;
;
MEMCCK:: .CHKPNT MEMCCK
.R BLIS10
*/S/MMEMCMP_MEMCHD,FIRST,TABLES,MEMCMP
;
;
;
.IF (ERROR); IGNORE IT
CANNCK:: .CHKPNT CANNCK
.R BLIS10
*/M/S/GCANNON_CANNHD,FIRST,TABLES,OPTMAC,CANNON,END
;
;
;
COMSCK:: .CHKPNT COMSCK
.R BLIS10
*/M/S/GCOMSUB_COMSHD,FIRST,TABLES,OPTMAC,COMSUB,END
;
;
;
GOPTCK:: .CHKPNT GOPTCK
.R BLIS10
*/M/S/GGOPTIM_GOPTHD,FIRST,TABLES,OPTMAC,GOPTIM,END
;
;
;
UTILCK:: .CHKPNT UTILCK
.R BLIS10
*/M/S/GUTIL_UTILHD,FIRST,TABLES,UTIL
;
;
;
PH2SCK:: .CHKPNT PH2SCK
.R BLIS10
*/M/S/GPH2S_PH2SHD,FIRST,TABLES,PH2S,END
;
;
;
;ALL PHASE 2S SOURCES ARE NOW COMPILED.  LET US LOAD PHASE 2S.
P2SCK:: .CHKPNT P2SCK
.R LOADER
*P2S.CMD@
.IF (ERROR); IGNORE IT.
.SSAVE FORTC
;
;
;
;THIS IS THE END OF PHASE 2S BUILDING SECTION.
;2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S2S
.SUBMIT FORTD/UNI:0/PRIOR:60/TIME:30:00
 