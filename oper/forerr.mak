	TITLE FORERR %2.(147) ERROR PROCESSING MODULE FOR THE FOROTS SYSTEM
	SUBTTL	D. TODD /DRT/     04-APR-1973


;***COPYRIGHT 1972,1973 DIGITAL EQUIPMENT CORP., MAYNARD, MASS.***
	ENTRY	FORER%,FORER.	;ENTRY POINT TO FORERR - MUST BE DEFINED BEFORE
.JBOPS=135


	SEARCH	FORPRM	;GLOBAL SYMBOLS DEFINED IN FORPRM

VERNO==2		;MAJOR VERSION NUMBER
VEDIT==110		;MAJOR EDIT NUMBER
VMINOR==0		;MINOR EDIT NUMBER
VWHO==0			;WHO EDITED LAST

VERERR==BYTE (3)VWHO(9)VERNO(6)VMINOR(18)VEDIT

;	DEFINE THE LOADING
	SEGMEN
HGH.AC==T5			;NUMBER OF AC'S TO SAVE


;CONTROL FFLAGS IN THE LEFT HALF OF THE MESSAGE TABLE ENTRIES
;	FOLLOWING FLAGS ARE CONTAINED IN T5 DURING ERROR PROCESSING

ER.HDR==400000	;MESSAGE HEADER TO BE TYPED OUT
ER.DDB==200000	;DEVICE INFO TO BE TYPED OUT
ER.EDB==100000	;EXTENDED DEVICE INFO TO BE TYPED (IMPLIES ER.DDB)
ER.MSG==040000	;ASSOCIATED SPECIAL ROUTINE (ROUTINE ADDRESS)
ER.USR==020000	;USER'S ADDRESS IS NOT AVAILABLE FOR MESSAGE HEADER
	SUBTTL FORERR ENTRY POINTS DEFINED BY ERRDIR IN (FORRM)
ERDIR%:		;DEFINE THE BEGINNING OF THE DISPATCH TABLE
	SALL

	ERRDIR
FORER.:
FORER%:	JRST	1,.+1		;ALLOW THE FOROTS CALL TO WORD TOO
	PUSHJ	P,.+1		;SAVE THE CALLING PC
	ADD	P,[XWD HGH.AC+1,HGH.AC+1]	;MAKE ROOM TO SAVE THE AC'S
	MOVEM	HGH.AC,(P)	;SAVE THE LAST AC
	MOVEI	HGH.AC,-HGH.AC(P)	;GET THE BEGINNING OF THE SAVE AREA
	BLT	HGH.AC,-1(P)	;SAVE THE AC'S
	N.==HGH.AC+1		;DEFINE THE STACK DEPTH
	PUSH	P,P4		;SAVE P4 FOR A BASE REGISTER
	N.=N.+1			;ACCOUNT FOR IT
				;THE AC'S PLUS RETURN ADDRESS
	HRRZ	P4,.JBOPS	;GET THE LOW SEGMENT POINTER
	MOVE	T3,-N.(P)	;GET THE XCT ADDR +1
	HLRZ	T4,(T3)		;GET THE TYPE AND SEVERITY CODE
	ANDI	T4,757		;SAVE THE INDEX AND AC FIELD
	ROT	T4,-5		;POSITION THE AC FIELD
	PUSH	P,T4		;SAVE THE TYPE CODE ON THE  STACK
	N.=N.+1			;COUNT THE SEVERITY CODE AND TYPE CODE
	ANDCMI	T4,-1		;CLEAR THE RIGHT HALR
	ROT	T4,5		;GET THE SEVERITY CODE BACK
	HRLM	T4,(P)		;PUT THE SEVERITY IN THE LEFT HALF
	HLRZ	T4,-1(T3)	;GET THE CLASS CODE
	LSH	T4,-5		;GET THE AC FIELD
	ANDI	T4,17		;SAVE FOUR BITS
	MOVEI	T1,FORRTN	;GET THE RETURN ADDRESS
	HLL	T1,ERDIR%(T4)	;GET THE CLASS NAME
	PUSH	P,T1		;SAVE ON THE STACK
	N.=N.+1		;COUNT THE PUSH
	HRRZ	T1,ERDIR%(T4)	;GET THE DISPATCH ADDRESS
NN.==N.			;DEFINE THE STACK DEPTH FOR THE REST
	JRST	(T1)		;GO TO THE ERROR CLASS ROUTINE
FORRTN:				;RETURN FROM THE CLASS ROUTINE
	N.=N.-1			;ACCOUNT FOR THE POPJ BACK HEHRE
	HRRZ	T3,@-N.(P)	;GET THE RETURN ADDRESS
	JUMPN	T3,FORRT3	;IS A RETURN SPECIFIED
FORRT0:	PUSHJ	P,TRAC%%	;GIVE A USERS TRACE
	MOVEI	T3,XIT##	;NO, USE SYSTEM RETURN
	OUTSTR	[ASCIZ /
? Job aborted
/]
FORRT3:	MOVEM	T3,-N.(P)	;SET THE RETURN ADDRESS
FORRT1:	POP	P,(P)		;GET THE TYPE CODE AND SEVERITY OFF THE STACK
	N.=N.-1
	POP	P,P4		;RESTORE THE BASE REG
	N.=N.-1		;ACCOUNT
	MOVSI	HGH.AC,-HGH.AC(P)	;SET A BLT POINT TO RESTORE THE AC'S
	BLT	HGH.AC,HGH.AC	;RESTORE THE AC'S
	SUB	P,[XWD HGH.AC+1,HGH.AC+1] ;MAKE THE STACK RIGHT
	N.=N.-HGH.AC
	POPJ	P,		;RETURN
	N.=N.-1

SYSRET:	POP	P,T0		;RETURN TO MONITOR VIA EXIT
	JRST	FORRT0		;LOAD THE EXIT RETURN

USRRET:	POP	P,T0		;REMOVE THE CALLING ADDRESS
	JRST	FORRT1		;EXIT
	SUBTTL	TY%XXX GENERAL PURPOSE OUTPUT ROUTINES TO THE TTY
;				;ROUTINE TO TYPE A STRING ON THE
				;CURRENT OUTPUT DEVICE
;	CALL
;	TYPSTR (ADDR OF STRING)		;CALLED BY THE TYPE STRING MACRO
;	(RETURN)
TY%STR:	MOVE	T2,(P)		;GET THE ARGUMENT
	MOVEI	T2,@(T2)	;GET THE LOCATION OF THE MESSAGE
	HRLI	T2,100		;SET UPPER CASE SHIFT MODE
TY%FI1:	MOVE	T1,(T2)		;GET A FIVBIT WORD
	TRNN	T1,1		;CHECK FOR LAST WORD OF THE STRING
	TLO	T2,400000	;YES, LAST WORD SET FLAG
TY%FI3:	SETZ	T0,		;CLEAR THE OUTPUT WORD
	LSHC	T0,5		;GET FIVE BITS
	CAIN	T0,37		;IS THIS A CASE SHIFT
	JRST	[TLC  T2,40	;YES, COMPLEMENT CASE SHIFT
		JRST	TY%FI3]	;GET THE NEXT CHARACTER
	JUMPE	T0,.+2		;JUMP IS A FIVBIT BLANK SEEN
	TSOA	T0,T2		;SET UP THE CASE SHIFT
	MOVEI	T0," "		;GET A BLANK
	JUMPN	T1,.+2		;CHECK FOR END OF WORD
	AOJGE	T2,TY%FI1	;CONTINUE UNLESS END OF STRING
	OUTCHR	T0		;OUTPUT THE ASCII CHARACTER
	JUMPN	T1,TY%FI3	;ANY CHARACTERS LEFT
	JUMPGE	T2,TY%FI3	;AND NOT LAST WORD 
	POPJ	P,		;UNLESS END OF STRING

TY%SIX:				;OUTPUT THE SIXBIT WORD IN T1
	SETZ	T0,		;CLEAR THE RECEIVER OF THE SIXBIT CHARACTER
	LSHC	T0,6		;GET A SIXBIT CHARACTER
	ADDI	T0," "		;CONVERT TO ASCII
	OUTCHR	T0		;OUTPUT THE CHARACTER
	JUMPN	T1,TY%SIX	;CONTINUE, IF ANY CHARACTERS LEFT
	POPJ	P,		;RETURN

TY%XWD:	HRLM	T0,(P)		;SAVE THE RIGHT HALF
	HLRZS	T0		;GET THE LEFT HALF
	PUSHJ	P,TY%OCT	;TYPE IT
	OUTSTR	[ASCIZ/,/]	;TYPE A COMMA
	HLRZ	T0,(P)		;GET THE RIGHT HALF
;	PJRST	TY%OCT		;TYPE IT
TY%OCT:	SKIPA	T2,[10]		;SET OCTAL RADIX
TY%DEC:	MOVEI	T2,^D10		;SET DECIMAL RADIX
TY%RDX:	JUMPGE	T0,TYRDX1	;JUMP IF +
	OUTSTR	[ASCIZ /-/]	;SUMP A MINUS SIGN
	MOVNS	T0		;NEGATE THE NUMBER
TYRDX1:IDIVI	T0,(T2)		;GET A DIGIT
	HRLM	T1,(P)		;SAVE ON THE STACK
	SKIPE	T0		;ANY DIGITS LEFT
	PUSHJ	P,TYRDX1	;YES, CONTINUE
	HLRZ	T0,(P)		;GET A DIGIT BACK
	ADDI	T0,"0"		;CONVERT TO ASCII
	CAILE	T0,"9"		;IF DIGIT IS GREATER THAN 9
	ADDI	T0,"A"-"0"	;CONVERT TO LETTERS
	OUTCHR	T0		;OUTPUT
	POPJ	P,		;RETURN FOR NEXT DIGIT

TY%TIM:			;PRINT THE TIME IN TO "HH:MM:SS.HH"
	ADDI	T0,5		;ROUND OFF THE HUNDREDTHS OF SECONDS
	IDIVI	T0,^D1000	;COMPUTE SECONDS
	PUSH	P,T1		;SAVE THOUSANDS OF A SECOND
	IDIVI	T0,^D60		;COMPUTE MINUTES
	PUSH	P,T1		;SAVE MINUES
	JUMPE	T0,TY%TI2	;SKIP IF NO MINUTES
	IDIVI	T0,^D60		;COMPUTE HOURS
	PUSH	P,T1		;SAVE THE MINUTES
	JUMPE	T0,TY%TI1	;SKIP IF NO HOURS
	PUSHJ	P,TY%DEC	;TYPE THE HOURS
	OUTSTR	[ASCIZ /:/]	;TYPE A SEPERATOR
TY%TI1:	POP	P,T0		;GET THE MINUTES BACK
	PUSHJ	P,TY%DEC	;TYPE THE MINUTES
	OUTSTR	[ASCIZ /:/]	;TYPE A SEPERATOR
TY%TI2:	POP	P,T0		;GET THE SECONDS BACK
	PUSHJ	P,TY%DEC	;TYPE THE SECONDS
	OUTSTR	[ASCIZ /./]	;TYPE A SEPERATOR
	POP	P,T0		;GET THE THOUSANDS BACK
	IDIVI	T0,^D10		;CHANGE TO HUNDREDTHS
	IDIVI	T0,^D10
	IORI	T0,"0"		;CONVERT TO ASCII
	OUTCHR	T0		;TYPE IT
	MOVEI	T0,(T1)
	PJRST	TY%DEC		;TYPE THE HUNDREDTHS
	SUBTTL ERROR MESSAGE PROCESSOR
TY%HDR:
	SKPINC			;KILL ^O TYPE OUT
	JFCL
	OUTSTR	[ASCIZ/
%/]	;TYPE A WARNING FLAG
	HLRZ	T0,-2(P)	;GET THE SEVERITY CODE
	PUSHJ	P,TY%OCT	;OUTPUT IN OCTAL
	TLNE	T5,ER.USR	;IS THERE A USER'S CALL
	JRST	TY%HD1		;NO, SKIP THE USER'S ADDRESS
	OUTSTR	[ASCIZ / User's call @ /]
	HRRZ	T0,USR.PC(P4)	;GET THE USER'S PC
	MOVSI	T1,(POP P,(P))	;CHECK FOR THE FORJAK CALL
	CAMN	T1,@T0
	HRRZ	T0,@.JB41
	SUBI	T0,1		;POINT TO THE INSTRUCTION
	PUSHJ	P,TY%OCT	;TYPE IT
TY%HD1:	OUTSTR	[ASCIZ / FOROTS Call @ /];TYPE IT
	HRRZ	T0,-NN.-1(P)	;GET THE ERROR MACRO ADDRESS
	SUBI	T0,1		;POINT TO THE XCT
	PUSHJ	P,TY%OCT	;TYPE IT
	OUTSTR	[ASCIZ / class /]
	HLLZ	T1,-1(P)		;GET THE CLASS NAME
	PUSHJ	P,TY%SIX	;type OUT THE SIXBIT
	OUTSTR	[ASCIZ / type /]
	HRRZ	T0,-2(P)	;GET THE TYPE CODE
	PUSHJ	P,TY%OCT	;TYPE IT
	OUTSTR	[ASCIZ/
/]				;END UP THE ERROR MESSAGE
	POPJ	P,		;RETURN TO SOMEONE

;THE FOLLOWING ENTRIES ARE NOT DEFINED
ER%UUO:ER%QUE:ER%UNF:ER%US0:ER%US1:ER%US2:
ERCALL:	PUSHJ	P,TY%HDR	;TYPE THE HEADER
	TYPSTR	[FIVBIT (UNDEFINED ENTRY IN FORERR)]
	POPJ	P,
	SUBTTL SYS ERROR PROCESSOR
ER%SYS:
	HRRZ	T5,-1(P)	;GET THE TYPE CODE
	CAILE	T5,SYS.MX		;CHECK FOR IN RANGE
	PJRST	ERCALL		;UNDEFINED ENTRY
	MOVE	T5,SYSTAB(T5)	;GET THE ERROR ENTRY
	TLNE	T5,ER.HDR	;HEADER TO BE TYPED
	PUSHJ	P,TY%HDR	;YES, TYPE IT
	TLNN	T5,ER.MSG	;MESSAGE TO BE TYPE
	PJRST	@T5		;NO, ROUTINE DISPATCH
	TYPSTR	(@T5)		;YES, TYPE THE MESSAGE
	POPJ	P,		;EXIT
SYSTAB:				;SYSTEM ERROR TABLE
XWD ER.HDR!ER.MSG,[FIVBIT (FOROTS system error)]			;(0)
XWD	,SYS01								;(1)
XWD ER.HDR!ER.MSG,[FIVBIT (ARGUMENT BLOCK not in the correct format)]	;(2)
XWD ER.HDR!ER.MSG,[FIVBIT (MONITOR not built to support FOROTS)]	;(3)
XWD ,SYSRET								;(4)
XWD ER.HDR!ER.MSG,[FIVBIT (USER PROGRAM HAS REQUESTED MORE CORE THAN IS AVAILABLE)]	;(5)
SYS.MX==.-SYSTAB-1		;SYSTBL SIZE

SYS01:			;PRINT THE TIMES OUT
	SKPINC		;KILL ^O TYPE OUT
	JFCL
	OUTSTR	[ASCIZ /
END OF EXECUTION
CPU TIME: /]
	SETZ	T0,		;ASK FOR OUT RUNTIME
	RUNTIME	T0,		;GET THE TOTAL RUNTIME
	SUB	T0,RUN.TM(P4)	;MINUS THE STARTING TIME
	PUSHJ	P,TY%TIM	;TYPE THE TIME OUT
	OUTSTR	[ASCIZ /	ELAPSED TIME: /]
	MSTIME	T0,		;GET THE TIME OF DAY
	SUB	T0,DAY.TM(P4)	;GET THE STARTING TIME
	JUMPGE	T0,.+2		;CHECK FOR AFTER MIDNIGHT
	ADD	T0,[^D1000*^D3600*^D24]	;ADD MILLISECONDS IN A DAY
	JUMPL	T0,.-1		;MAY BE MANYS DAY OF RUNNING
	PUSHJ	P,TY%TIM	;TYPE THE TIME OUT
	CALLI	12		;EXIT TO MONITOR
	SUBTTL OPN ERROR PROCESSOR
ER%OPN:	HALT
	SUBTTL APR ARITHMETIC FAULT ERROR PROCESSOR
FXU=1B11	;FLOATING EXPONENT UNDERFLOW FLAG
FOV=1B3		;FLOATING OVERFLOW BIT
NDV=1B12	;NO DIVIDE BIT
ER%APR:		;ENTRY TO APR FAULT
	AOS	OVCNT.(P4)	;COUNT THE APR FAULT
	SOSGE	ERRMX.(P4)	;COUNT THE ERRORS
	POPJ	P,		;TOO MANY DON'T PRINT
	HRRZ	T4,-NN.(P)	;GET THE ERROR MACRO PC
	HRRZ	T5,-1(P)	;GET THE TYPE CODE
	SOJGE	T5,ERAPR1	;SPECIAL ENTRY FOR A MESSAGE TYPE
	MOVE	T4,.JBTPC	;GET THE APR TRAP LOC
	HLRZ	T5,T4		;GET THE TRAP BITS
	ANDI	T5,(FXU!FOV!NDV)	;SAVE THE FLAG BITS
	LSH	T5,-5		;MAKE A MESSAGE POINTER
	TRZE	T5,(1B8)	;INDEX
	IORI	T5,1B33		;BETWEEN 0-7
ERAPR1:	MOVE	T5,APRTAB(T5)		;GET THE FLAGS
	PUSHJ	P,TY%HDR	;TYPE OUT THE HEADER
	TYPSTR	(@T5)		;TYPE THE ERROR MESSAGE
	OUTSTR	[ASCIZ /	PC= /]
	MOVEI	T0,-1(T4)	;GET THE ERROR LOCATION
	PUSHJ	P,TY%OCT	;60;TYPE OUT THE PC
	OUTSTR	[ASCIZ /
/]
	POPJ	P,		;RETURN
APRTAB:
	XWD	ER.USR,[FIVBIT (Integer overflow)]	;(1)
	XWD	ER.USR,[FIVBIT (Integer divide check)];(2)
	XWD	ER.USR,[FIVBIT (Illegal APR trap)]	;(3)
	ARG	ER.USR,@APRTAB+2			;(4)
	XWD	ER.USR,[FIVBIT (Floating overflow)]	;(5)
	XWD	ER.USR,[FIVBIT (Floating divide check)];(6)
	XWD	ER.USR,[FIVBIT (Floating underflow)]	;(7)
	SUBTTL LIB LIBRARY ERROR FAULT PROCESSOR
ER%LIB:				;ENTRY POINT
	SOSGE	ERRMX.(P4)	;COUNT THE LIB ERROR
	JRST	USRRET		;IGNORE THE ERROR MESSAGE
	PUSHJ	P,TY%HDR	;TYPE THE HEADER
	PJRST	ER%MSG		;AND THE MESSAGE
	SUBTTL DAT DATA ERROR FAULT PROCESSOR
ER%DAT:	HALT			;ENTRY POINT
	SUBTTL DEV DEVICE ERROR FAULT PROCESSOR
ER%DEV:	HALT
	SUBTTL MSG TYPE A MESSAGE OUT
ER%MSG:
	MOVE	T5,@-NN.(P)	;GET THE MESSAGE ADDRESS
	OUTSTR	(T5)		;OUTPUT THE MESSAGE
	OUTSTR	[ASCIZ /
/]
	JRST	USRRET		;RETURN TO THE ERROR MACRO
SUBTTL	FORTRAN TRACE ROUTINES
		ENTRY	TRACE%
	SIXBIT	/TRACE./	;NAME FOR TRACE
TRACE%:
;	PUSHJ	P,SAVE.##	;SAVE THE AC'S FOR USER CALL
TRAC%%:				;FOROTS ENTRY WHEN AC'S ARE SAVED
	MOVEI	T5,(P)		;GET THE CURRENT STACK POSITION

	OUTSTR	[ASCIZ/
Name	(Loc)	 <<----	Caller	(Loc)	<#Args>	[Arg Types]
/]
	MOVEI	T5,(P)		;GET THE CURRENT STACK POINTER
TRACE1:	HRRZ	T4,USR.PC(P4)	;GET THE USER'S CALLING ADDRESS
	PUSHJ	P,GETCAZ	;ADDRESS CHECK IT
	POPJ	P,		;RETURNS T3= PUSHJ ADDRESS+1
TRACE2:	MOVE	T1,-1(T3)	;YES, GET THE SIXBIT SUBROUTINE NAME
	PUSHJ	P,TY%SIX	;TYPE THE SUBROUTINE NAME OUT
	OUTSTR	[ASCIZ/	(/]
	MOVEI	T0,(T3)		;ENTRY POINT TO THE SUBROUTINE
	PUSHJ	P,TY%OCT
	OUTSTR	[ASCIZ/) <<---	 /]	;AND A CLOSEING PAREN
	PUSH	P,T4		;SAVE THE ADDRESS OF THE PUSHJ +1
	SUBI	T5,1		;SET THE STACK BACK ONE
	PUSHJ	P,GETCAL	;GET THE NEXT CALL
	JRST	TRACE3		;END OF TRACE TO THE MAIN PROGAM
	EXCH	T4,(P)		;GET THE OLD PUSHJ+1 ADDRESS
	PUSHJ	P,TYPTRC	;TYPE THE TRACE
	POP	P,T4		;RESTORE THE NEW POINTER
	JRST	TRACE2		;CONTINUE
TRACE3:
	POP	P,T4		;RESTORE THE OLD POINTER
	HRRZ	T3,.JBSA	;GET THE STRATING ADDRESS
	SKIPA	T1,[SIXBIT /MAIN./]	;MAIN PROGRAM CALL
TYPTRC:	MOVE	T1,-1(T3)	;GET THE CALLING SUBROUTINE NAME
	PUSHJ	P,TY%SIX	;TYPE THE NAME
	OUTSTR	[ASCIZ/+/]	;
	MOVEI	T0,(T4)		;GET THE CALFER ADDRESS
	SUBI	T0,1(T3)	;MINUS THE ENTRY POINT
	PUSHJ	P,TY%OCT	;TYPE THE OFFSET FROM THE ENTRY
	OUTSTR	[ASCIZ/(/]	;LEFT PAREN
	MOVEI	T0,-1(T4)	;GET THE PUSHJ ADDRESS
	PUSHJ	P,TY%OCT	;TYPE IT
	OUTSTR	[ASCIZ /)	<#/];#ARGUEMENTS
	MOVE	T4,-2(T4)	;GET THE MOVEI ADDRESS
	HLL	T4,-1(T4)	;BUILD AN AOBJN POINTER
	HLRE	T0,T4		;GET THE ARGUMENT COUNT
	MOVMS	T0		;MAKE POSITIVE
	PUSHJ	P,TY%DEC	;TYPE IT OUT
	OUTSTR	[ASCIZ />	[/]	;TYPE A CLOSING BRACKET
	JUMPGE	T4,TYPTR2	;JUP IF NO ARGUMENTS
TYPTR1:	LDB	T1,[POINT 4,(T4),12]	;GET THE ARGUMENT TYPE
	IDIVI	T1,5		;FIVE ENTRIES / WORD
	MOVE	T0,TYPCOD	;GET THE ASCII TYPE CODE
	IMULI	T2,7		;GET THE SHIFT COUNT
	ROT	T0,7(T2)	;POSITION THE CHARACTER
	OUTCHR	T0		;OUTPUT THE ASCII CHARACTER
	AOBJN	T4,TYPTR1	;CONTINUE THRU THE ARGLIST
TYPTR2:	OUTSTR	[ASCII/]
/]				;CLOSING BRACKET
	POPJ	P,		;RETURN
;		 0123456701234567
TYPCOD:	ASCII	/ULIUFUOSDUUUCUUK/
;ROUTINE TO NEXT THE NEXT SUBROUTINE CALL FROM THE STACK
;	ENTRY		T5=THE STACK ADDRESS TO START THE SEARCH
;			PUSHJ	P,GETCAL
;	RETURN		(NON-SKIP)	;END OF STACK
;			(SKIP)		;T4= THE ADDRESS+1 OF THE PUSHJ
;					;T3=THE PUSHJ INSTRUCTION
GETCAL:
	CAIGE	T5,STK.SV(P4)	;IS THE THE BEGINNING OF THE STACK
	POPJ	P,		;YES, END OF SEARCH
	HRRZ	T4,(T5)		;GET THE STACK ENTRY
GETCAZ:	PUSHJ	P,ADRCHK	;IS THIS A VALID ADDRESS
	SOJA	T5,GETCAL	;NO, GET THE NEXT ENTRY
	MOVE	T3,-1(T4)	;GET THE CALLING INSTRUCTION
	HLRZ	T1,T3		;GET THE OP CODE
	CAIE	T1,(PUSHJ P,)	;IS IT A PUSHJ P,
	SOJA	T5,GETCAL	;NO, GET THE NEXT ENTRY
	HLRZ	T1,-2(T4)	;GET THE INSTRUCTION BEFORE THE PUSHJ
	CAIE	T1,(MOVEI L,)	;MUST LOAD THE ARGUMENT LIST
	SOJA	T5,GETCAL	;NO, GET THE NEXT ENTRY
	HLRZ	T1,(T3)		;GET THE TARGET INSTRUCTION
	CAIN	T1,(JRST 1,)	;CHECK FOR CONCEALED ENTRY
	HRR	T3,(T3)		;YES, GO TO THE TRUE ENTRY
	JRST	CPOPJ1		;SKIP RETURN


;ROUTINE TO ADDRESS CHECK THE CONTENTS OF AC T4
;	ENTRY		MOVE	T4,ADDRESS TO BE CHACKED
;			PUSHJ	P,ADRCHK
;	RETURN		(NON-SKIP)	;INVALID ADDRESS
;			(SKIP)		;VALID ADDRESS

ADRCHK:				;ENTRY POINT
	CAIG	T4,140		;CHECK THE LIMITS OF THE LOW SEGMENT
	POPJ	P,		;BELOW THE LOW SEGMENT EXIT
	CAMG	T4,.JBREL	;CHECK FOR A VALID LOW SEG ADDRESS
	JRST	CPOPJ1		;YES, GIVE A SKIP RETURN
	SKIPN	T1,.JBHRL	;IS THERE A HIGH SEGMENT
	POPJ	P,		;NO, ERROR
	CAIGE	T4,400010	;ABOVE THE BEGINNING OF THE HIGH SEGMENT
	POPJ	P,		;NO, EXIT
	CAIGE	T4,(T1)		;BELOW THE END OF THE HIGH SEGMENT
CPOPJ1:	AOS	(P)		;YES, SKIP RETURN
CPOPJ:	POPJ	P,		;ERROR RETURN
	END
	SUBTTL MSG TYPE A MESSAGE OUT
ER%MSG:
	OUTSTR	@T5	;TYPE THE MESSAGE
	POPJ	P,
	LIT
	END
   