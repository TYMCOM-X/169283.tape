;JOB%1(22) TO MAKE BATCON.SHR,BATCON.LOW,BATOPR.SHR FROM BATCON.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=BATCON/REST:1/TIME:0:10:0
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	LOADER.SHR
;	JOBDAT.REL
;	QUEUER.REL
;	HELPER.REL
;	CREF.SHR
;[SELF]	BATCON.MAC
;	BATCON.HLP
;
;OUTPUT FILE:
;	BATCON.SHR
;	BATCON.LOW
;	BATOPR.SHR
;OUTPUT LISTINGS:
;	BATCON.MAP
;	BATCON.CRF LISTING
;	BATOPR.CRF LISTING
;	BATOPR.MAP
;	BATCON.LOG
;
;
;COPY FILES FROM [10,7] AND USE PRIVATE "SYS:"
.RUN DSK:PIP[10,7]
*/X_DSK:PIP.SHR[10,7],COMPIL.SHR[10,7],MACRO.SHR[10,7],LOADER.SHR[10,7]
*/X_DSK:JOBDAT.REL[10,7],QUEUER.REL[10,7],HELPER.REL[10,7],CREF.SHR[10,7]
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF(NOERROR) .GOTO A
.RU DSK: DIRECT[10,7]
*TTY:/CHECKSUM=*.SAV+*.SHR+*.HLP
.GOTO A
A::.RUN DSK: DIRECT[10,7]
*TTY:/CHECKSUM=*.MAC+*.REL+*.DOC+*.RND+*.CTL+*.HLP
.ASSIGN DSK: SYS:
;
;COMPILE, LOAD, AND SAVE; PRODUCING MAP AND CREF FILE
.LOAD /MAP:LPT:BATCON /CREF /COMPILE DSK:QUEUER,BATCON %A
.SSAVE DSK:BATCON
.VERSION
.IF(ERROR).E 137
;
;HERE TRY TO ASSEMBLE THE BATCH OPERATOR SEGMENT OF THE CONTROLLER
;TO DO THIS ONE MUST DEFINE THE SWITCH FTOPR.
;IF FTOPR=1 DEFAULT THIS WILL GENERATE BATCON
;IF FTOPR=0 THIS WILL GENERATE BATCON AS TWO SEGMENT PROGRAM.
;IF FTOPR=-1 THIS GENERATES BATOPR MAKING BATCON MULTISEGMENT PROGRAM
;SO TO DEFINE FTOPR CREATE A FILE
.RUN DSK: PIP
*DSK:FTOPR.MAC=TTY:
*FTOPR==-1
=^Z
=^Z
;
; NOW THAT THE FTOPR.MAC IS MADE ASSEMBLE BATOPR
;
;
.LOAD /MAP:LPT:BATOPR /CREF /COMPILE DSK:FTOPR+BATCON.MAC,HELPER %A
.VERSION
.IF (ERROR) .E 137
.D 0 0 133
.SSAVE DSK: BATOPR
;TRY IT JUST TO MAKE SURE IT WORKS
.RUN DSK:BATCON
*HELP
;
.RU DSK: DIRECT[10,7]
*TTY:/CHECKSUM=BATCON.SHR+BATCON.LOW+BATCON.SAV+BATOPR.SHR
;
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.CREF
.PLEASE BATCON SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN::.DELETE BATCON.REL,FTOPR.MAC,CREF.SHR,MACRO.SHR,LOADER.SHR
.IF (ERROR) ;DON'T CARE IF FAILED
.DELETE COMPIL.SHR,PIP.SHR,JOBDAT.REL,QUEUER.REL,HELPER.REL,*.TMP
;
;
;*******    OPERATIONAL  INFORMATION      *********
;
;
;I. DESCRIPTION OF FEATURE TEST SWITCHES.
;
;	1. MORTIM
;	   MORTIM REPRESENTS THE EXTRA TIME ALLOWED (PERCENTAGE)
;	   TO THE JOB, AFTER THE JOB'S REQUESTED TIME LIMIT EXPIRES.
;	   MORTIM VARIES BETWEEN 0 AND 100. DEFAULT IS 10,REPRESENTING
;	   10% OF THE ORIGINAL TIME GIVEN AS EXTRA TIME TO THE JOB.
;	   THIS EXTRA TIME IS USED FOR ERROR PROCESSING ONLY. IF THE
;	   USER HAS NO ERROR PROCESSING LABELS THE JOB WILL BE LOGGED 
;	   OFF.
;
;	2. DEBUG
;	   DEBUG SWITCH HAS BEEN SET AT 1. IT SHOULD BE LEFT AT 1.
;
;	3. JOBMAX
;	   JOBMAX REPRESENTS NUMBER OF MPB BATCH STREAMS. JOBMAX VARIES
;	   BETWEEN 1 AND 14.
;
;	4. MXSLEP
;	   MXSLEP REPRESENTS SLEEP TIME. DEFAULT MXSLEP IS 30
;	   SECONDS. THIS PARAMETER CAN BE CHANGED AT RUN TIME WITH 
;	   ZZZ OPERATOR COMMAND
;
;	5. DEFREQ
;	   DEFREQ REPRESENTS THE DEFAULT REQUEUE TIME. IN CERTAIN CASES
;	   BATCON REQUEUES JOBS TO BE RUN AT A LATER TIME,DETERMINED BY 
;	   DEFREQ. AS AN EXAMPLE A JOB IS REQUEUED IF THE DISK IS FULL 
;	   AND RUN AFTER DEFREQ MINUTES. DEFAULT VALUE USED FOR DEFREQ
;	   IS 10 MINUTES.
;
;	6. REQMX
;	   REQMX REPRESENTS THE MAXIMUM REQUEUE TIME ALLOWED FOR THE
;	   ARGUMENTS OF MPB REQUEUE COMMAND. THE VALUE FOR REQMX IS IN
;	   MINUTES AND REPRESENTS MAXIMUM OF 15 DAYS.
;
;	7. FTPLS.
;	   IF FTPLS IS 1, THE MONITOR PLEASE COMMAND IS TRAPPED FOR
;	   THE MPB JOBS, AND WILL COME OUT ON THE BATCON CONTROLLING
;	   TERMINAL. HOWEVER IF FTPLS IS 0,THE PLEASE COMMAND WILL
;	   WORKS THE SAME WAY AS FOR TIMESHARING JOBS.
;
;	8. PURE
;	   IF PURE IS 1 BATCON ASSEMBLES AS A RE-ENTRANT PROGRAM.
;
;	9.FTDFL
;	   FTDFL WHEN 1 LISTS ALL THE FILES DELETED BY KJOB AT LOGOUT
;	   TIME. THIS MAY HAPPEN IF USER IS OVER HIS LOGOUT QUOTA.
;;
;	10. SCHINT
;	    SHCINT REPRESENTS THE INTERVAL AT WHICH THE JOBS IN THE 
;	    INPUT QUEUE WILL BE SCHEDULED. THIS PARAMETER IS SET AT
;	    10 SECONDS.
;
;	11. MODFY
;	    MODFY IS SET TO 20 AND I #OF TIMES BATCON WILL TRY TO WRITE
;	    INTO A LOG FILE,WHEN BEING MODIFIED.
;
;	12. SKPLIN
;	    SKPLIN IS SET TO 25 AND IS #OF LINES BATCON SKIPS IN SKIP
;	    MODE BEFORE CHECKING WITH THE OPERATOR.
;
;	13. LEVELC
;	    LEVELC IS SET TO ZERO FOR LEVELD
;
;	14. LOOPCT
;	    LOOPCT IS 50 AND REPRESENSTS THE #OF TIMES BATCON 
;	    WILL LISTEN TO A JOB IN MULTISTREAM ENVIORNMENT BEFORE GOING
;	    TO NEXT SUBJOB.
;
;	15. REMSW
;	    REMSW IS SET TO 1 WHEN THE SYSTEM IS CONFIGURED FOR REMOTE
;	    STATIONS. THOSE INSTALLATION THAT DONOT HAVE A REMOTE
;	    STATION MAY PUT REMSW TO 0.
;
;	16. OPRINT
;	    IF SYSTEM JOB TABLE IS FULL BATCON WAITS A FIXED TIME
;	    DEFINED BY OPRINT BEFORE TRYING TO SCHEDULE  NEXT JOB
;	    THE DEFAULT VALUE IS SET TO 30 SECONDS.
;II. SCHEDULING ALGORITHM
;
;    THE FOLLOWING EQUATION DESCRIBES THE SCHEDULING ALGORITHM USED
;    IN BATCON
;
;	JOB PRIORITY=1+AEXT*PREXT+(ATM*DEFTIM/TEXT)
;			+(ACR*DEFCDR/CEXT)+(APG*DEFPAG/PEXT)
;			+(ACD*DEFCRD/CDEXT)+(APP*DEFPTP/PPEXT)
;			+(ADP*DEFDSP/DPEXT)+(AGE*AGEFAC)
;
;   THE DEFAULT PARAMETERS MUST MATCH THE SAME PARAMETERS IN THE QUEUE
;   CUSP. THE WEIGHT CONSTANTS HOWEVER ARE AN INSTALLATION
;   CHOICE.
;
;	1. DEFTIM
;	   DEFAULT VALUE FOR DEFTIM IS 60 AND REPRESENTS 60
;	   SECONDS OF CPU TIME LIMIT.
;
;	2. DEFCOR
;	   DEFAULT VALUE FOR DEFCOR IS 25K AND REPRESENTS 25K OF CORE
;	   LIMIT.
;
;	3. DEFPAG
;	   DEFAULT VALUE FOR DEFPAG IS 200 AND REPRESENTS 200 PAGES OF
;	   PRINTOUT LIMIT.
;
;	4. DEFCRD
;	   DEFAULT VALUE FOR DEFCRD IS 500 AND REPRESENTS 500 CARDS
;	   CARDPUNCH LIMIT.
;
;	5. DEFPTP
;	   DEFAULT VALUE FOR DEFPTP IS 10 AND REPRESENTS 10 FEET OF
;	   PAPERTAPE LIMIT.
;
;	6. DEFDSP
;	   DEFAULT VALUE FOR DEFDSP IS 10 AND REPRESENTS 10 MINUTES OF
;	   DISPLAY TIME LIMIT.
;
;	7. WEIGHT CONSTANTS
;	   THE WEIGHT CONSTANTS ATM,ACR,APG,APP ARE DEFINED TO BE EQUAL
;	   TO 8. THE WEIGHT CONSTANTS ACD AND ADP ARE DEFINED
;	   AS ZERO,AS THE CARD PUNCH, AND DISPLAY SPOOLERS
;	   ARE NOT PRESENTLY AVAILABLE. THE CONSTANTS AEXT,AGE ARE BOTH
;	   EQUAL TO 4.
    