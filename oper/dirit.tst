DIRIT.FAL
TEST PROGRAM FOR DIRIT - STATUS REQUIRED RF - MUST BE ABLE TO ACCESS LPT
DIRIT TEST PROGRAM FOR PERFORM -&W AND &F SWITCHES ARE INTERNAL TO DIRIT
 DELETE THE ABOVE AND CREATE A NEW PERFORM FILE
TYPE DIRIT.TST
COPY RKRKRK TO RKRKRK.TMP
COPY RKRKRK TO RKRK30.ABC
COPY RKRKRK TO RKRK30.MAC
COPY RKRKRK TO RKRK20.MAC
COPY RKRKRK TO RKRK20.FAL
COPY RKRKRK TO RKRK20
FIL/ACCESS RKRKRK%A
FIL/LICENSE (SYS)OPER.SAV%B
FIL/CREATION RKRKRK%C
FIL/TIME RKRKRK%D
FIL/EXTENSION DIRIT.*%E
FIL/UFDBITS RKRKRK%G
FIL/STORAGE RKRKRK%H
FIL/LPT RKRKRK%I
FIL/SEC RKRKRK%J
FIL/TEMPS RKRKRK.*%K
REN RKRK20,RKRK20.SAV%L
FIL/MODE RKRKRK%M
FIL/TOTAL%N
FIL/PROTECTION RKRKRK%P
DEL/WAIT RKRK20.*%Q
FIL/REVERSE DIRIT.*%R
FIL/SIZE RKRKRK%S
FIL/ALPHABETICAL *.CMD%T
FIL/UNSORT RKRKRK%U
DEL/NOPRINT RKRK30.*%V
DEL RKRK20.*%X
FIL/WORDS RKRKRK%Y
FIL/STATUS RKRKRK%Z
FIL/TODAY%>
FIL/BEFORE 12-JUN-73%<
COPY RKRKRK TO TEST1
COPY TEST1 TO TEST1.LOW
COPY TEST1 TO TEST1.SAV
COPY TEST1 TO TEST1.HGH
REN/WAIT TEST1.* AS TEST2.*
DEL/NOP TEST1.*,TEST2.*
COPY RKRKRK TO TEST1.1
COPY TEST1.1 TO TEST1.2
COPY TEST1.1 TO TEST1.3
COPY TEST1.1 TO TEST1.4
COPY TEST1.1 TO TEST1.5
COPY TEST1.1 TO TEST1.6
COPY TEST1.1 TO TEST1.7
COPY TEST1.1 TO TEST1.8
COPY TEST1.1 TO TEST1.9
COPY TEST1.1 TO TEST1.10
DEC ALL RD NO TEST1.1
DEC NO NO NO TEST1.2
DEC RD NO NO TEST1.3
DEC UPD NO NO TEST1.4
DEC AP NO NO TEST1.5
DEC NO RD NO TEST1.6
DEC NO UPD NO TEST1.7
DEC NO ALL NO TEST1.8
DEC ALL ALL ALL (SALTGAVERR)TEST1.9
DEC ALL RD NO (SALTGAVERR)TEST1.10
FIL/PROTECTION TEST1.*
DEC ALL RD NO TEST1.*/NOP
DEL TEST1.*/NOP
  