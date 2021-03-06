; THIS BATCH CONTROL FILE BUILDS A KA10-COMPATIBLE COMPILER AND LIBRARY
; 
; EDIT THE PARAMETER FILE
.TECO SDDNDF.MAC
*NP$KI10=}1D}I0}EX}}
.DEL *.BAK
;
; RE-ASSEMBLE LIBRARY PROGRAM
.R MACRO
*SDDNDF.REL,_SDDNDF.MAC
*SDDSYS.REL,_SDDSYS.MAC
;
; BUILD LIBRARY FILE
.R PIP
*FASLIB.REL_SDDLIB.REL,SDDPRM.REL,SDDIOR.REL,SDDPAT.REL,SDDUTL.REL,SDDBKT.REL,SDDSYS.REL,SDDDUM.REL
;
; BUILD COMPILER FILE WITH DUMMY CROSS-REFERENCE MODULE
.LOAD @SNOBOL
.SAVE FASBOL.MIN
.RUN FASBOL.MIN 32
.SAVE FASBOL.MIN
;
; BUILD COMPILER FILE WITH FULL CROSS-REFERENCE MODULE
.RUN FASBOL.MIN
*CROSPH.FAS,_CROSPH.SNO
.R MACRO
*CROSPH.REL,/Q_CROSPH.FAS
.LOAD @SNOBOL
.SAVE FASBOL
.RUN FASBOL 33
.SAVE FASBOL
   