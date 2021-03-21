$ID
-ID ORG-PRINT.
-BY FRED. BUTTERFIELD.
$ED
INPUT-OUTPUT SECTION.
FILE-CONTROL. SELECT ORGFIL ASSIGN TO DSK
        RM= ASCII.
SELECT ODATA1 ASSIGN TO DSK
        RM= ASCII.
$DD
-FS
FD ORGFIL VID= 'ORGFILDAT'.
        01 FUNC-RECORD.
                02 D    PIC A.
                02 S    PIC A.
                02 C    PIC A.
                02 G    PIC A.
                02 FUNC PIC 999.
                02 DESC PIC A(30).
FD ODATA1 VID= 'ODATA1DAT'.
        01 OUT-RECORD.
                02 DO   PIC A.
                02 SO   PIC A.
                02 CO   PIC A.
                02 G-O  PIC A.
                02 FUNCO PIC 999.
$PD
PAR-1. OPEN INPUT ORGFIL OUTPUT ODATA1.
PAR-2. READ ORGFIL AT END GO TO END-ROUTINE.
        MOVE D TO DO. MOVE S TO SO.
        MOVE C TO CO. MOVE G TO  G-O.
        MOVE FUNC TO FUNCO.
        WRITE OUT-RECORD.
        DISPLAY DO,SO,CO,G-O,FUNCO,DESC. GO TO PAR-2.
END-ROUTINE. DISPLAY 'FINISHED'.
        CLOSE ORGFIL,ODATA1.
        STOP RUN.
