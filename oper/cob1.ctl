;IF DEBUG VERSION DESIRED, GO TO NEXT PAGE
.TYPE DEBUG.MAC
.IF (NOERROR) .GOTO DEBUG
;
;ASSEMBLE SOURCES
;
;NOTE:	TO PRODUCE A RESTRICTED VERSION OF COBOL, CHANGE P.MAC
;	AS FOLLOWS:
;		TO REMOVE ISAM FEATURES:
;			INSERT "ISAM==0" AT THE BEGINNING
;		TO REMOVE REPORT WRITER FEATURES:
;			INSERT "RPW==0" AT THE BEGINNING
;		TO REMOVE SEARCH VERB FEATURES:
;			INSERT "SERCH==0" AT THE BEGINNING
;
;
.R MACRO
*CLEANT,CLEANT/C=P,CLEANT
*CLRNAM,CLRNAM/C=P,CLRNAM
*CMNGEN,CMNGEN/C=P,CMNGEN
*COBCOM,COBCOM/C=P,COBCOM
*COBOLA,COBOLA/C=P,COBOLA
*COBOLB,COBOLB/C=P,COBOLB
*COBOLC,COBOLC/C=P,COBOLC
*COBOLD,COBOLD/C=P,COBOLD
*COBOLE,COBOLE/C=P,COBOLE
*COBOLF,COBOLF/C=P,COBOLF
*COBOLG,COBOLG/C=P,COBOLG
*COBOLK,COBOLK/C=P,COBOLK
*DIAGS,DIAGS/C=P,DIAGS
*EXPGEN,EXPGEN/C=P,EXPGEN
*GETASY,GETASY/C=P,GETASY
*GETCPY,GETCPY/C=P,GETCPY
*GETERA,GETERA/C=P,GETERA
*GETGEN,GETGEN/C=P,GETGEN
*GETITM,GETITM/C=P,GETITM
*GETTAG,GETTAG/C=P,GETTAG
*IFGEN,IFGEN/C=P,IFGEN
*IMPURE,IMPURE/C=P,IMPURE
*IOGEN,IOGEN/C=P,IOGEN
*IPCGEN,IPCGEN/C=P,IPCGEN
*MATGEN,MATGEN/C=P,MATGEN
*MOVGEN,MOVGEN/C=P,MOVGEN
*MSCGEN,MSCGEN/C=P,MSCGEN
*PSCAN,PSCAN/C=P,PSCAN
*PURAB,PURAB/C=TTY:,DSK:P,PURE
*%MLOAD=="A"
=^Z
=^Z
*PUREC,PUREC/C=TTY:,DSK:P,PURE
*%MLOAD=="C"
=^Z
=^Z
*PURED,PURED/C=TTY:,DSK:P,PURE
*%MLOAD=="D"
=^Z
=^Z
*PUREE,PUREE/C=TTY:,DSK:P,PURE
*%MLOAD=="E"
=^Z
=^Z
*PURFG,PURFG/C=TTY:,DSK:P,PURE
*%MLOAD=="F"
=^Z
=^Z
*PUTAS1,PUTAS1/C=P,PUTAS1
*PUTAS2,PUTAS2/C=P,PUTAS2
*PUTAS3,PUTAS3/C=P,PUTAS3
*PUTBIN,PUTBIN/C=P,PUTBIN
*PUTCPY,PUTCPY/C=P,PUTCPY
*PUTERA,PUTERA/C=P,PUTERA
*PUTGEN,PUTGEN/C=P,PUTGEN
*PUTLST,PUTLST/C=P,PUTLST
*RPWGEN,RPWGEN/C=P,RPWGEN
*SQUIRL,SQUIRL/C=P,SQUIRL
*SRTCRF,SRTCRF/C=P,SRTCRF
*SRTGEN,SRTGEN/C=P,SRTGEN
*SRTTAB,SRTTAB/C=P,SRTTAB
*STINFL,STINFL/C=P,STINFL
*SYMBOL,SYMBOL/C=P,SYMBOL
*TRACER,TRACER/C=P,TRACER
*TRYNAM,TRYNAM/C=P,TRYNAM
*XFRGEN,XFRGEN/C=P,XFRGEN
*XPAND,XPAND/C=P,XPAND
*XPNPPL,XPNPPL/C=P,XPNPPL
.IF (ERROR) .GOTO TRUBLE
.GOTO SKPDEB
DEBUG::;ASSEMBLE SOURCES FOR DEBUG VERSION
 