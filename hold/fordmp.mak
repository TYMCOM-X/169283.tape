	TITLE	FORDMP  %2.(105)
	SUBTTL	D.TODD/DMN 15-JAN-1973
;***COPYRIGHT 1972,1973 DIGITAL EQUIPMENT CORP., MAYNARD, MASS.***
;FROM	1 MAY 1966 ED YOURDON, 2/12/68 NSR

	;THE PROGRAMS DUMP AND PDUMP MAY BE CALLED BY A FORTRAN PROGRAM
	;IN THE FOLLOWING MANNER:
	;	CALL DUMP(A(1),B(1),F(1),. . .,A(N),B(N),F(N))
	;	CALL PDUMP(A(1),B(1),F(1),.. .,A(N),B(N),F(N))
	;BOTH PROGRAMS CAUSE CORE TO BE DUMPED BETWEEN THE LIMITS A(I)
	;AND B(I), AS SPECIFIED BY THE MODE PARAMETER F(I). EITHER
	;A(I) OR B(I) MAY BE UPPER OR LOWER CORE LIMITS. DUMP CALLS
	;[SIXBIT /EXIT/] WHEN DONE, WHILE PDUMP RESTORES THE STATE
	;OF THE MACHINE AND RETURNS TO THE USERS PROGRAM. BOTH
	;PROGRAMS INDICATE THE CONTNETS OF THE ACCUMULATORS AND THE
	;FOLLOWING FLAGS BEFORE BEGINNING THE ACTUAL CORE DUMP:
	;	AR OV FLAG
	;	AR CRY0 FLAG
	;	AR CRY1 FLAG
	;	PC CHANGE FLAG
	;	BIS FLAG
	;THE MODE OF THE DUMP IS CONTROLLED BY THE PARAMETER F(I), WHICH
	;MAY BE ONE OF THE FOLLOWING NUMBERS:
	;	0	OCTAL		(O12 FORMAT)
	;	1	FLOATING POINT	(G12.5 FORMAT)
	;	2	INTEGER		(I12 FORMAT)
	;	3	ASCII		(A12 FORMAT)
	;	4	DOUBLE PRECISION (G25.16)
	;THE FOLLOWING CONVENTIONS HAVE BEEN ADOPTED FOR UNUSUAL
	;ARGUMENT LISTS:
	;	1. IF NO ARGUMENTS ARE GIVEN, THE ENTIRE USER AREA
	;	   IS DUMPED IN OCTAL.
	;	2. IF THE LAST MODE ASSIGNMENT, F(N), IS MISSING,
	;	   THAT SECTION OF CORE IS DUMPED IN OCTAL.
	;	3. IF THE LAST TWO ARGUMENTS, B(N) AND F(N), ARE MISSING
	;	   AN OCTAL DUMP IS MADE FROM A(N) TO THE END OF USER AREA
	;	4. AN ILLEGAL MODE ASSIGNMENT CAUSES THE DUMP TO BE
	;	   MADE IN OCTAL.
	;IF A GROUP OF REGISTERS HAVE THE SAME CONTENTS, DUMP AND
	;PDUMP WILL FINISH PRINTING THE CURRENT LINE, THEN INDICATE THE NUMBER OF
	;OF REPEATED LINES WITH A COMMENT
	;LOCATION XXXXXX THROUGH XXXXXX CONTAIN XXXXXXXXXXXX

	ENTRY	PDUMP,DUMP
	SEARCH	FORPRM

	;ACCUMULATOR ASSIGNMENTS AND PARAMETER ASSIGNMENTS

		P=	17	;PUSHDOWN POINTER
		Q=	16	;JSA ACCUMULATOR
		B=	3	;SCRATCH
		C=	4	;...
		S=	5	;ADDRESS OF LOCATION CURRENTLY DUMPED
		F=	6	;ADDRESS OF HIGH LOCATION TO BE DUMPED
		I=	7	;ARGUMENT INDICATOR
		LL=	10	;LOOP COUNTER
		FRMT=	11	;HOLDS FORMAT FOR REPEATED LINES
		PP=	15	;BLT AC, ALSO HOLDS A FORMAT ADDRESS

		N=	12	;SIZE OF AC BLOCK TO BE SAVED ON PD LIST
		DEVICE==-3	;DEVICE ASSIGNMENT FOR PRINT
		NLIST= 5	;NO. OF DIFFERENT FORMAT DUMPS AVAILABLE
	HELLO	(DUMP)	;BEGINNING OF DUMP ROUTINE
	JSR	DUMPA		;CALL COMMON DUMPING ROUTINE
	FUNCT	(EXIT.)		;CALL /EXIT/ WHEN DONE

	HELLO	(PDUMP)		;BEGINNING OF PDUMP ROUTINE
	JSR	DUMPA		;CALL COMMON DUMPING ROUTINE
	GOODBY	(0)		;RETURN TO USER PROGRAM

DUMPA:	0			;BEGINNING OF COMMON DUMPING
	PUSH	P, PP		;SAVE BLT AC
	HRRZI	PP, 1(P)	;SET UP BLT POINTER IN AC PP
	ADD	P, NUMBER	;MAKE ROOM ON PUSHDOWN LIST
	BLT	PP, (P)		;BLT ACS ONTO PUSHDOWN LIST
IFN F40LIB,<
	PUSH	P,L		;SAVE THE LINK
	N=N+1			;UPDATE THE STACK SIZE
	TLZN	L,-1		;F40 CALL
	JRST	DUMPB		;NO, F10 CALL
	SETZ	C,		;CLEAR THE ARG COUNTER
	HLL	C,(L)		;GET THE OP COD
	TLC	C,(<JUMP>)	;JUMP OP CODE
	TLNE	C,777000	;CHECK THE OP CODE
	JRST	DUMPC		;END OF ARG LIST
	ADDI	L,1		;STEP TO NEXT ARG
	SOJA	C,.-5		;CONTINUE SEARCH
DUMPB:
>
	HLL	L,-1(L)		;NO, F10 GET THE ARG COUNT
IFN F40LIB,<
	JRST	DUMPD		;CONTINUE
DUMPC:	MOVE	L,(P)		;RESTORE L
	HRLI	L,(C)		;INSERT THE ARG COUNT
DUMPD:
>
	PUSH	P,L		;SAVE THE LINK OVER THE I/O CALLS
	MOVEI	L,[XWD 3000,DEVICE
		XWD	0,0
		XWD	400010,MESS1]
	PUSHJ	P,OUT.##
	MOVE	C, BYTEP	;GET BYTE POINTER FOR FLAGS
	MOVEI	F, 5		;LOOP FOR FIVE FLAGS
FLAGS:	ILDB	B, C		;GET FLAG BIT STORED BY JSR
	MOVE	S, OFFON(B)	;GET EITHER "OFF" OR "ON"
	MOVEI	L,[XWD 001000,S
		XWD	0,0]	;OUTPUT IT
	PUSHJ	P,IOLST.##
	SOJG	F, FLAGS	;LOOP BACK FOR MORE FLAGS
	PUSHJ	P,FIN.##		;FINISH THIS FORMAT STATEMENT
	MOVEI	L,[XWD 3000,DEVICE
		XWD	0,0
		XWD	400006,MESS2]
	PUSHJ	P,OUT.##
	CLEARB	S, I		;AC0-AC7, SET INDICATOR TO ZERO
	MOVEI	L,[XWD 001000,S
		XWD	0,0]	;OUTPUT IT
	PUSHJ	P,IOLST.##
	CAIGE	S, 7		;WHICH CONTAINS 0,1,2,3,4,5,6,7
	AOJA	S, .-2		;LOOP BACK UNTIL DONE
	MOVEI	F, 1-N(P)	;GET CONTENTS OF AC0-AC7 OFF PD
	MOVEI	L,[XWD 001000,(F)
		XWD	0,0]	;OUTPUT IT
	PUSHJ	P,IOLST.##
	CAIGE	F, 1-N+7(P)	;LOOP FOR 8 ACCUMULATORS
	AOJA	F, .-2
	MOVEI	S, 10		;PRINT AC10 - AC17
	MOVEI	L,[XWD 001000,S
		XWD	0,0]	;OUTPUT IT
	PUSHJ	P,IOLST.##
	CAIGE	S, 17		;LOOP FOR 8 ACS
	AOJA	S, .-2
	MOVEI	S,-N(P)		;GET THE BLT ACC ADDR
	MOVEI	L,[XWD 002000,5
		XWD	0,1
		XWD	0,10
		XWD	001000,(S)
		XWD	001000,N+1(S)
		XWD	001000,17
		XWD	004000,0];FIN CALL IMPLIED
	PUSHJ	P,IOLST.##	;OUTPUT THE LIST
	POP	P,L		;RESTORE THE LINK
;ARGUMENT PROCESSOR
SGET:
	PUSH	P,L	;SAVE THE LINK
	MOVEI	L,[XWD 003000,DEVICE	;OUTPUT SOME LINE FEEDS AT
		XWD	0,0	;BEGINNING OF EACH SECTION
		XWD	400001,MESS3]	;END OF THIS FORMAT STATEMENT
	PUSHJ	P,OUT.##
	PUSHJ	P,FIN.##
	POP	P,L		;RESTORE THE LINK
	AOBJP	L,ENDCHK	;NO, QUIT OR DUMP ALL OF CORE
	MOVEI	S,@-1(Q)	;YES, PICK UP THE ADDRESS
	AOJ	I,		;INDICATE THAT ARGUMENTS HAVE BEEN SEEN
	AOBJP	L,ENDCK2	;NO, END OF ARGUMENT LIST
	MOVEI	F, @-1(Q)	;YES, PICK UP THE ADDRESS
	AOBJP	Q, ENDCK3	;NO, END OF ARGUMENT LIST
	MOVE	C, @-1(Q)	;YES, GET THE FORMAT NUMBER
	CAIL	C,NLIST		;IS THIS A LEGAL ARGUMENT
	JRST	ENDCK3		;YES, DUMP IN OCTAL MODE
SCHEK:	CAML	S, F		;ARE ARGUMENTS IN ORDER?
	EXCH	S, F		;NO, SWITCH THEM
	MOVE	PP,LIST(C)	;GET THE FORMAT ADDRESS
	HRRM	PP,ARG1+2
	HLRM	PP,ARG2+2
	HLRZS	PP
	MOVE	B,TABLE(C)	;V6 SET UP FORTRAN DATA UUO
	DPB	B,[POINT 4,IOLSTC,12]	;V6 DEPOSIT POINTER
	DPB	B,[POINT 4,IOLSTS,12]	;V6 ....
;MAIN DUMP PROCESSOR

DPROC:	PUSH	P,L		;SAVE THE LINK
	MOVE	B, S		;GET CURRENT ADDRESS IN B
	MOVE	LL, S		;POINTER IN REPETITION CHECK
	MOVE	C, (S)		;MEMORY WORD FOR REPETITION CHECK
LOOK:	CAMN	C, (LL)		;DO WORDS MATCH?
	CAIGE	F, (LL)		;FINISHED THIS SECTION OF CORE?
	JRST	DIFF		;GO COMPUTE REPEATED LINES
	CAIL	LL, 7(S)		;FINISHED CHECKING A LINE?
	ADDI	S, 10		;YES, INCREMENT S TO NEXT LINE
	CAML	S,F		;STILL IN RANGE
	AOJA	LL, LOOK		;INCREMENT POINTER, CHECK MORE
DIFF:	CAMN	B, S		;WERE ANY LINES REPEATED?
	JRST	OLOOP1		;NO, DUMP THIS LINE INDIVIDUALLY
	MOVEI	L,ARG1		;YES GET FORMAT FOR MESSAGE
	PUSHJ	P,OUT.##
	MOVEI	L,IOLSTC	;OUTPUT REPEATED WORD
	PUSHJ	P,IOLST.##
	MOVEI	L,[XWD 003000,DEVICE
		XWD	0,0
		XWD	400012,MESS4]	;MESSAGE IS IN TWO PARTS
	MOVEI	C,-1(S)		;LAST LOC OFF BY ONE
	PUSHJ	P,OUT.##
	MOVEI	L,[XWD 001000,B	;PRINT PART ABOUT ADDRESSES
		XWD 001000,C	;FIRST LOCATION THAT REPEATED
		XWD 004000,0]	;LAST LOCATION, S WAS ONE OFF
	PUSHJ	P,IOLST.##	;END OF REPETITION MESSAGE
OLOOP1:	CAMLE	S, F		;ALL DONE DUMPING?
	JRST	NEXT+1		;YES, CHECK ARGUMENTS
	MOVEI	L,ARG2		;NO, OUTPUT FOR 8 WORDS/LINE
	PUSHJ	P,OUT.##	
	MOVEI	B, 10		;LOOP COUNTER
	MOVEI	L,[XWD 001000,S
		XWD	0,0]
	PUSHJ	P,IOLST.##
OLOOP2:	MOVEI	L,IOLSTS	;ADDRESS FOR THIS LINE
	PUSHJ	P,IOLST.##	;MEMORY WORD
	CAML	S, F		;ALL DONE DUMPING
	JRST	NEXT		;YES, CHECK ARGUMENTS
	CAIE	PP,DFRMT	;DOUBLE PRECISION?
	AOJA	S,.+3		;NO, MOVE POINTER TO NEXT WORD
	ADDI	S,2		;YES, ADVANCE POINTER ONE WORD
	SOJ	B,		;OUTPUTS ONLY 4 WORDS
	SOJG	B,OLOOP2	;DONE WITH THIS LINE?
	PUSHJ	P,FIN.##	;YES, FINISH OFF FORMAT STATEMENT
	JRST	DPROC+1		;SCAN NEXT LINE


ARG1:	XWD	003000,DEVICE
	XWD	0,0
	XWD	400004,0

IOLSTC:	XWD	001000,C
	XWD	004000,0

ARG2:	XWD	003000,DEVICE
	XWD	0,0
	XWD	400004,0

IOLSTS:	XWD	001000,(S)
	XWD	0,0

;ROUTINES THAT ARE CALLED AT TERMINATION OF ARGUMENT STRINGS,
;AND END OF CORE SECTION DUMPS

ENDCHK:	JUMPN	I, SDOUT	;ANY ARGUMENTS?
	HRRZI	S, 20		;DUMP FROM 20
ENDCK2:	HRRZ	F, .JBFF	;TO END OF USER AREA
	SUBI	F,1		;DO NOT DUMP FIRST FREE
ENDCK3:	CLEARB	C, I		;END OF DUMP, OCTAL MODE
	JRST	SCHEK		;FIX EXIT, CHECK CORE LIMITS

SDOUT:
IFN F40LIB,<
	POP	P,L		;RESTORE THE LINK
	N=N-1
>
	MOVEM	Q, Q+1-N(P)	;SAVE EXIT ACCUMULATOR
	HRLZI	PP, 1-N(P)	;FIX BLT POINT AC
	BLT	PP, N-1		;GET ACS BACK FROM PD LIST
	SUB	P, NUMBER	;FIX UP PUSHDOWN POINTER
	POP	P, PP		;RESTORE BLT AC
	JRST	@DUMPA		;EXIT FROM COMMON DUMPING ROUTINE

NEXT:	PUSHJ	P,FIN.##			;FINISH FORMAT
	POP	P,L		;RESTORE THE LINK
	JUMPE	I, SDOUT	;MORE ARGUMENTS TO COME?
	JRST	SGET		;GO GET SOME MORE ARGUMENTS

;FORMAT STATEMENTS FOR OUTPUT

MESS1:	ASCII	"(1H148X9HCORE DUMP/1H 7HOV FLAG17X9HCRY0"
	ASCII	" FLAG15X9HCRY1 FLAG15X14HPC CHANGE FLAG9"
	ASCII	"X8HBIS FLAG/1H 5(A9,15X))"
MESS2:	ASCII	"(2(1H-8(9X3HAC O2)/7X8O14/))"
MESS3:	ASCII	"(1H-)"
MESS4:	ASCII	"(11H+LOCATIONS O6,9H THROUGH O6,9H CONTAIN /1H )"
;MORE FORMAT STATEMENTS AND SOME CONSTANTS, TOO
OFRMT:	ASCII	"(1H0,O6,8O14)"
EFRMT:	ASCII	"(1H0,O6,8G14.5)"
IFRMT:	ASCII	"(1H0,O6,8I14)"
AFRMT:	ASCII	"(1H0,O6,8A14)"
DFRMT:	ASCII	"(1H0,O6,4G25.16)"
OFRMT2:	ASCII   "(1H0,40X,O14)"
EFRMT2:	ASCII	"(1H0,40X,G14.5)"
IFRMT2:	ASCII	"(1H0,40X,I14)"
AFRMT2:	ASCII	"(1H0,40X,A14)"
DFRMT2:	ASCII	"(1H0,40X,G25.16)"
OFFON:	ASCII	"OFF  "
	ASCII	"ON   "
LIST:	XWD	OFRMT, OFRMT2
	XWD	EFRMT, EFRMT2
	XWD	IFRMT, IFRMT2
	XWD	AFRMT, AFRMT2
	XWD	DFRMT,DFRMT2

TABLE:	EXP	TP%OCT,TP%REA,TP%INT,TP%LIT,TP%DOR
BYTEP:	POINT 1, DUMPA
NUMBER:	XWD	N, N

	END
   