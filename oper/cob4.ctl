;CREATE COBOLL.REL
.R PIP
*COBOLL.REL/B=IMPURE.REL,DDT.REL,HELPER.REL,SQUIRL.REL,TRACER.REL,PURAB.REL,PUREC.REL
*COBOLL.REL/B=COBOLL.REL,PURED.REL,PUREE.REL,PURFG.REL,CLEANT.REL,XPNPPL.REL,XPAND.REL,SYMBOL.REL
*COBOLL.REL/B=COBOLL.REL,GETITM.REL,PSCAN.REL,DIAGS.REL,SRTCRF.REL,SRTTAB.REL,STINFL.REL,CLRNAM.REL
*COBOLL.REL/B=COBOLL.REL,TRYNAM.REL,PUTGEN.REL,PUTCPY.REL,PUTBIN.REL,PUTERA.REL,GETGEN.REL,GETCPY.REL
*COBOLL.REL/B=COBOLL.REL,GETERA.REL,GETASY.REL,COBCOM.REL,PUTLST.REL,XFRGEN.REL,EXPGEN.REL,IPCGEN.REL
*COBOLL.REL/B=COBOLL.REL,IOGEN.REL,MATGEN.REL,IFGEN.REL,MOVGEN.REL,MSCGEN.REL,RPWGEN.REL,SRTGEN.REL
*COBOLL.REL/B=COBOLL.REL,CMNGEN.REL,GETTAG.REL,PUTAS1.REL,PUTAS2.REL,PUTAS3.REL
.IF (ERROR) .GOTO TRUBLE
;
;
;THROW AWAY LOCAL SYMBOLS FROM COBOLL
.R FUDGE2
=COBOLL.REL/X=COBOLL.REL
.IF (ERROR) .GOTO TRUBLE
 