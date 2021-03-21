;JOB %1(05) TO MAKE CDRSTK.LOW AND .SHR FROM CDRSTK.MAC
;SUBMIT WITH COMMAND   .QUEUE INP:=CDRSTK/RESTART:1

.SET WATCH VERSION

.GOTO DOIT

							PAGE 1

	CDRSTK IS THAT SYSTEMS PROGRAM USED FOR INPUT TO THE
MULTI-PROGRAMMING-BATCH SYSTEM.  ITS INPUT MAY BE IN THE FORM OF
CARDS IN ASCII, D029, AND 026 CODES; OR FILES FROM A DIRECTORY
DEVICE (DTA, DSK) IN ASCII MODE.


THE DEFAULTS ARE THE FOLLOWING:


1.  CDR INPUT IS DEFAULTED TO ASCII MODE UNLESS OTHERWISE SPECIFIED
    BY USE OF $MODE CARDS OR MODE SWITCHES.

2.  NON-CDR INPUT IS DEFAULTED TO ASCII MODE AND ANY OTHER MODE
    WILL RESULT IN AN ERROR MESSAGE TO THE USER.

3.  MAXIMUM CARD WIDTH IS DEFAULTED TO DECIMAL 80 COLUMNS.  THIS IS
    DEFINED BY THE CONDITIONAL "MAXWID" AND MAY BE CHANGED BY THE
    INDIVIDUAL INSTALLATION.

4.  MAXIMUM CORE IN THE SYSTEM IS DEFAULTED TO DECIMAL 64K.  THIS IS
    DEFINED BY THE CONDITIONAL "MAXCOR" AND MAY BE CHANGED BY THE
    INSTALLATION.

5.  THE MAXIMUM CARDS ALLOWED TO BE PUNCHED FOR THIS JOB IS DEFAULTED
    TO DECIMAL 10000 WORDS.  THIS IS DEFINED BY THE CONDITIONAL
    "MAXCAR" AND MAY BE CHANGED BY THE INDIVIDUAL INSTALLATION.

6.  THE CORE LIMIT FOR EACH INDIVIDUAL JOB IS DEFAULTED TO DECIMAL
    25K WORDS.  THIS VALUE MAY BE CHANGED BY USE OF THE CORE SWITCH
    OR THE DEFAULT MAY BE CHANGED BY THE INSTALLATION BY MODIFYING
    THE CONDITIONAL "DEFCOR".

7.  PAGES OF OUTPUT FROM A JOB IS DEFAULTED TO DECIMAL 200.  THIS
    VALUE MAY BE CHANGED BY THE USER VIA THE PAGE SWITCH OR THE DEFAULT
    MAY BE MODIFIED BY THE INSTALLATION AT THE CONDITIONAL "DEFPAG".

8.  EACH JOB'S TIME LIMIT IS DEFAULTED TO 60 DECIMAL SECONDS.  THIS 
    VALUE MAY BE CHANGED BY THE USER VIA THE TIME SWITCH OR BY THE
    INSTALLATION AT THE CONDITIONAL "DEFTIM".

9.  THE DEFAULT CARD WIDTH IF DECIMAL 80 COLUMNS.  THIS MAY BE CHANGED
    BY THE USER VIA THE WIDTH SWITCH.  THE INSTALLATION MAY CHANGE 
    THIS VALUE AT THE CONDITIONAL "DEFWID".

							PAGE 2

10. PASSWORDS ARE REQUIRED FOR ENTRANCE TO MULTI-PROGRAMMING-
    BATCH THROUGH CDRSTK BY DEFAULT.  INDIVIDUAL USERS MAY BE
    EXEMPTED FROM THIS REQUIREMENT VIA MODIFICATION OF THEIR
    INDIVIDUAL ACCT.SYS ENTRIES.  THOSE INSTALLATIONS REQUIRING
    PASSWORDS FOR NONE OF ITS USERS SHOULD DEFINE THE
    CONDITIONAL "CODEWD" AS SOME VALUE OTHER THAN 0.

11. OPERATOR APPROVAL IS NOT NORMALLY REQUIRED TO APPEND AN
    END OF JOB CARD TO A JOB WHICH DOES NOT PRESENTLY HAVE ONE.
    THE INSTALLATION WHICH REQUIRES OPERATOR INTERVENTION AT 
    THIS POINT SHOULD DEFINE THE CONDITIONAL "OPEROK" AS SOME
    VALUE OTHER THAN 0.  WHEN "OPEROK" IS SET TO NON-ZERO, A
    MESSAGE STATING THAT THE PREVIOUS JOB DID NOT HAVE AN END-
    OF-JOB CARD AND THE OPERATOR MUST TYPE "CONT" TO CONTINUE
    ON WITH THE PRESENT JOB.

12. NORMALLY, FORTRAN DATA IS SPOOLED TO THE DISK FROM CARDS
    AND CDRSTK TAKES ADVANTAGE OF THIS FEATURE BY PUTTING DATA
    INTO QUEUE FILES WITH THE EXTENSION .CDR.  THE INSTALLATION
    MAY WISH TO BYPASS THIS FEATURE AND HAVE THE DATA PUT INTO
    DATA FILES WHICH WILL BE RENAMED TO "FOR02.DAT"
    PRIOR TO EXECUTION BY CDRSTK COMMAND.  THIS CAN BE ACCOMPLISHED
    BY DEFINING THE CONDITIONAL "SPOOLR" AS SOME VALUE OTHER
    THAN 0.  IF THIS CONDITIONAL IS SET TO NON-ZERO, IT MUST
    BE REMEMBERED THAT A USER RUNNING TWO FORTRAN JOBS UNDER
    MULTI-PROGRAMMING-BATCH AT THE SAME TIME COULD VERY WELL LOSE
    ON ONE OF THE JOBS BECAUSE AS FILES ARE BEING RENAMED IN
    ONE JOB, THEY MAY BE REQUIRED IN THE OTHER JOB.

13. BLANK SUPPRESSION IS NORMALLY IN EFFECT FOR EACH CARD,
    BUT THE USER MAY OVERRIDE THIS BY USING THE "SUPPRESS"
    SWITCH.

14.  IF "SUBROU" IS SET EQUAL TO ZERO, THE SUBROUTINE "TRYUFD"
     IS USED TO LOOKUP AND ENTER UFDS.  IF SET <>0, THE CODE
     INTERNAL TO CDRSTK IS USED TO PERFORM THESE FUNCTIONS.

15.  IF "JST026" IS SET TO 0 (ITS DEFAULT), THE DEFAULT MODE
     FOR CHARACTER INTERPRETATION IS ASCII.  IF "JST026"<>0,
     CHARACTER INTERPRETATION FROM THE CDR DEFAULTS TO 026 MODE.

16.  IF "JST029" IS SET TO 0 (ITS DEFAULT), THE DEFAULT MODE
     FOR CHARACTER INTERPRETATION IS ASCII.  IF "JST029"<>0,
     CHARACTER INTERPRETATION FROM THE CDR DEFAULTS TO D029 MODE.


							PAGE 3


DOIT:
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	JOBDAT.REL
;	CREF.SHR
;	QUEUER.REL
;	HELPER.REL
;	TRYUFD.REL
;	CDRSTK.HLP
;[SELF]	CDRSTK.MAC
;
;OUTPUT FILES
;	CDRSTK.LOW
;	CDRSTK.SHR
;OUTPUT LISTINGS
;	CDRSTK.MAP
;	CREF LISTING OF CDRSTK
;	CDRSTK.LOG
;
;
;COPY FILES FROM [10,7] AND USE PRIVATE "SYS:"
.RUN DSK:PIP[10,7]
*/X_DSK:PIP.SHR[10,7],COMPIL.SHR[10,7],MACRO.SHR[10,7],LOADER.SHR[10,7]
*/X_DSK:JOBDAT.REL[10,7],CREF.SHR[10,7],QUEUER.REL[10,7],HELPER.REL[10,7]
*/X_DSK:TRYUFD.REL[10,7]
;
;MAKE A RECORD OF WHAT IS BEING USED
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.MAC+*.REL+*.DOC+*.RND+*.HLP+*.CTL
.ASSIGN DSK:SYS:
;
;COMPILE, LOAD, AND SAVE--PRODUCING MAP AND CREF FILE
.COMPILE /CREF /COMPILE CDRSTK
.LOAD /MAP:LPT:CDRSTK CDRSTK,HELPER,TRYUFD,QUEUER %A
.SSAVE DSK:CDRSTK
.VERSION
.IF (ERROR) .E 137
.R CDRSTK
HELP
EXIT
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=CDRSTK.SHR+CDRSTK.LOW+CDRSTK.SAV

.PLEASE CDRSTK SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DEL CDRSTK.REL,MACRO.SHR,LOADER.SHR,CREF.SHR,HELPER.REL
.IF (ERROR)
.DELETE COMPIL.SHR,PIP.SHR,JOBDAT.REL,TRYUFD.REL,QUEUER.REL
