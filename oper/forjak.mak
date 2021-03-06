	TITLE FORJAK	%2.(100) INTERFACE MODULE BETWEEN OLD F40 AND FOROTS
	SUBTTL	D. TODD /DRT/     08-DEC-1972


;***COPYRIGHT 1972,1973 DIGITAL EQUIPMENT CORP., MAYNARD, MASS.***


	INTERNAL VERJAK

VERNO==2		;MAJOR VERSION NUMBER
VEDIT==100		;EDIT NUMBER
VMINOR==0		;MINOR EDIT NUMBER
VWHO==0		;WHO EDITED LAST

VERJAK==BYTE (3)VWHO(9)VERNO(6)VMINOR(18)VEDIT



	EXTERNAL	RESET.
	EXTERNAL IN.,OUT.,FIN.,RTB.,WTB.,MTOP.
	EXTERNAL NLI.,NLO.,DEC.,ENC.,OPEN.,IOLST.


T0=0
T1=1
T2=2
T3=3
L=16
P=17
.JB41=41


	;DUMMY ENTRY POINT FOR THE OLD F40
	ENTRY	FORSE.
FORSE.:
	INTERN ERR.,END.,RECNO.,RANAC.,VADDR.
	INTERN MBSR.,NMLST.,MSPR.,TPFCN.,ALLIO.,BINWR.
	INTERN	DIRT.,DOUBT.,OCTO.,OCTI.,TFMT.
DIRT.:DOUBT.:OCTO.:OCTI.:TFMT.:
	INTERN ALPHO.,ALPHI.,INTI.,INTO.,FLOUT.,DUMMY.,FLIRT.,LINT.,LOUT.


DUMMY.:MBSR.:NMLST.:MSPR.:TPFCN.:ALLIO.:BINWR.:
ALPHO.:ALPHI.:INTI.:INTO.:FLOUT.:FLIRT.:LINT.:LOUT.:


	EXTERN .JBUUO
	LOC	.JB41
	JSR	UUO.		;GO TO THE UUO PROCESSOR

	RELOC	0
	SUBTTL FORJAK - UUO DISPATCH ROUTINE
UUO.:	Z		;ENTRY FROM UUO
	MOVEM	L,SAVEL	;SAVE AC L
	MOVEM	T0,SAVET0	;SAVE T0 ALSO
	LDB	L,[POINT 9,.JBUUO,8]	;GET UUO OP CODE
	CAIL	L,15		;CHECK FOR FORTRAN RANGE
	CAILE	L,35		;ABOVE FORTRAN RANGE
	HALT
	CAIN	L,15		;IS THIS A RESET UUO
	JRST	RESET		;YES, DO A RESET
	PUSH	P,DISPUU-16(L)	;SAVE THE DISPATCH ADDRESS
	HRRZ	L,DISPUU-16(L)	;GET THE ROUTINE ADDRESS
	JRST	(L)		;GO TO THE ROUTINE

DISPUU:	XWD	IN.,INP
	XWD	OUT.,OUTP
	XWD	IOLST.,DATA
	XWD	FIN.,FIN
	XWD	RTB.,RTB
	XWD	WTB.,WTB
	XWD	MTOP.,MTOP
	XWD	IOLST.,SLIST
	XWD	OPEN.,INF
	XWD	OPEN.,OUTF
	XWD	IN.,REREAD
	XWD	NLI.,NLI
	XWD	NLO.,NLO
	XWD	DEC.,DEC
	XWD	ENC.,ENC
	XWD	ERROR,FCALLI


	SUBTTL FORJAK - FOROTS CALLING SEQUENCE SIMULATION ROUTINES
RESET:	HLRZ	L,120		;GET THE END OF THE LOW SEG
	SUBI	L,140		;MINUS THE BEGINNING
	MOVNS	L		;NEGATE
	HRLZI	L,(L)		;BUILD AN AOBJN POINTER
	HRRI	L,140		;TO SWEEP THE LOW SEQ FOR
RESET0:	MOVE	T0,(L)		;FORLIB ENTRY POINTS
	CAMN	T0,[CAIA
			PUSH P,CEXIT.##]+1;THE MAY BE DISTROYED BY
	JRST	RESET1	;JSA ENTRY POINTS
RESET2:	AOBJN	L,RESET0	;CONTINUE
	JSP	L,RESET.	;MAKE A RESET CALL TO FOROTS
	Z			;FLAG WORD OF ZERO
	HRRZS	UUO.		;CLEAR THE PC FLAG WORD
	JRST	UUORT.		;RETURN TO FORTRAN
RESET1:	MOVSI	T0,(CAIA)
	MOVEM	T0,-1(L)
	JRST	RESET2

NLI:
NLO:
	LDB	L,[POINT 4,.JBUUO,12]	;GET THE AC CONTAUNG THE POINTER
	MOVE	L,(L)		;GET THE POINTER
	SKIPGE	(L)		;HAS THE ARG BLOCK BE CONVERTED
	JRST	INP		;YES
	PUSH	P,T1		;SAVE SOME AC'S
	PUSH	P,T2
	PUSH	P,T3
	MOVEI	T3,-1(L)	;SET UP A STACK FOR THE ARG BLOCK
	MOVE	T0,(L)		;GET THE NAMELIST NAME
	PUSHJ	P,C50TO6	;CONVERT SIXBIT
	PUSH	T3,T2		;PUT THE NAME LIST NAME ON THE STACK
	ADDI	L,1		;POINT TO THE FIRST VARIABLE
NLST1:	SKIPN	(L)		;END OF NAME LIST
	JRST	NLST2		;YES, FINISH UP
	MOVE	T0,(L)		;GET THE VARIABLE NAME
	PUSHJ	P,C50TO6	;CONVERT TO SIXBIT
	LDB	T1,[POINT 3,(L),3] ;GET THE VARIABLE TYPE
	MOVE	T0,ARGTAB(T1);GET THE FOROTS TYPE CODE
	ADDI	L,1		;SET FORSE POINTER
	MOVE	T1,(L)		;GET THE VARIABLE ADDRESS
	DPB	T0,[POINT 13,T1,12];STORE THE VARIABLE TYPE CODE
	MOVE	T0,-1(L)	;GET THE RADIX 50 NAME BACK
	PUSH	T3,T2		;STORE THE SIXBIT NAME
	PUSH	T3,T1		;STORE THE ADDRESS AND TYPE
	JUMPGE	T0,NLST1-1	;JUMP IF A SCALAR VARIABLE
	ADDI	L,1		;POINT TO THE DIMENSIONALITY
	MOVE	T2,(L)		;GET IT
	DPB	T2,[POINT 9,(T3),8];STORE IT
	AOS	T1,L		;POINT TO THE FACTORS
	ADDI	T1,(T2)		;POINT TO THE OFFSET
	MOVE	T0,(T1)		;GET THE OFFSET
	HRL	T0,1(T1)	;GET THE SIZE
	PUSH	T3,T0		;STORE THE SIZE AND OFFSET
	MOVE	T0,(L)		;GET A FACTOR
	PUSH	T3,T0		;STORE THE FACTOR
	SOSLE	T2		;COUNT THIS FACTOR
	AOJA	L,.-3		;STEP THE FORSE POINTER CONTINUE
	ADDI	L,2		;SKIP THE SIZE AND OFFSET FIELD
	AOJA	L,NLST1		;GET THE NEXT VARIABLE
NLST2:	SETZM	1(T3)		;SET THE END FLAG
	POP	P,T3		;RESTORE THE AC'S
	POP	P,T2
	POP	P,T1
	JRST	INP		;CONTINUE NORMAL
C50TO6:			;CONVERT RADIX 50 TO SIXBIT
	TLZ	T0,740000	;CLEAR THE CONTROL BITS
	SETZ	T2,		;CLEAR THE OUTPUT WORD
	IDIVI	T0,50		;CONVERT THE RADIX 50
	CAILE	T1,12		;TO A SIXBIT
	ADDI	T1,7		;CHARACTER
	ADDI	T1,17		;IN AC
	LSHC	T1,-6		;T2
	JUMPN	T0,.-5		;CONTINUE THRU THE CHARACTERS
	POPJ	P,		;RETURN
DEC:
ENC:	TLOA	L,4000		;SET ARG BLOCK COUNT TO 4

REREAD:
OUTP:
INP:	MOVSI	L,3000		;SET THE ARG BLOCK COUNT TO 3
	HLLM	L,ARGBLK	;SAVE THE ARG BLOCK COUNT
	LDB	L,[POINT 4,.JBUUO,12]	;LOAD THE INDEX POINT
	MOVE	L,(L)		;GET THE FORMAT STATEMENT POINTER WORD
	HLRZ	T0,(L)		;GET THE FORMAT STATEMENT WORD
	TLO	L,400000	;ASSUME ENCODED LIST TO BE DELETED
	CAIE	T0,(JRST)	;IS IT A JRST INSTRUCTION
	JRST	.+5		;NOT AN F40 FORMAT NAME LIST OR ARRAY
	HRRZ	T0,(L)		;NO, GET THE ENDING ADDRESS
	ADDI	L,1		;SET THE POINTER TO THE FMT
	SUBI	T0,(L)		;END-BEGIN = SIZE OF FORMAT
	HRL	L,T0		;SAVE THE SIZE (CLEAR BIT 0)
	MOVEM	L,ARGBLK+2	;SAVE FORMAT WORD IN ARGBLK
	JRST	RANDOM		;CHECK FOR RANDOM ACCESS

WTB:
RTB:	SETZM	ARGBLK+2	;CLEAR THE FORMAT FIELD
	MOVSI	L,2000		;SET ARG COUNT FOR 2
	HLLM	L,ARGBLK	;SET IN ARGBLK

RANDOM:	SKIPN	RANAC.		;RANDOM ACCESS MODE
	JRST	.+6		;NO,CONTINUE
	MOVEI	L,RECNO.	;YES GET A POINTER TO THE RECORD NUMBER
	MOVEM	L,ARGBLK+3	;SAVE IN ARGBLK
	SETZM	RANAC.		;CLEAR THE RANDOM ACCESS FLAG
	MOVSI	L,4000		;SET THE ARG COUNT TO 4
	HLLM	L,ARGBLK	;SAVE IN ARGBLK

UNIT:	HRR	L,.JBUUO	;GET THE UNIT NUMBER
	HRRM	L,ARGBLK	;SAVE IN THE ARGBLK

ENDERR:	MOVE	L,[XWD ERRRTN,ENDRTN];SET UP THE RETURN ADDRESS
	SKIPN	END.		;END= SPECIFIED
	ANDCMI	L,-1		;NO, CLEAR THE END RETURN
	SKIPN	ERR.		;ERR= SPECIFIED
	ANDI	L,-1		;NO, CLEAR THE ERROR RETURN
	MOVEM	L,ARGBLK+1	;STROE THE RETURN ADDRESS
EXEC:

EXEC0:	MOVEI	L,ARGBLK	;LOAD THE ARGBLK POINTER WORD
	HLRZS	(P)		;CLEAR, LEFT (P) AND SET ADDRESS
	MOVE	T0,SAVET0	;RESTORE AC0
	PUSHJ	P,@(P)		;GO TO FOROTS
EXEC1:	POP	P,(P)		;MAKE THE STACK RIGHT
ERROR:
FCALLI:
UUORT.:				;RETURN TO FORTRAN CALLER
	MOVE	L,SAVEL		;RESTORE AC L
	JRSTF	@UUO.		;RETURN TO USER PROGRAM

SLIST:	MOVE	L,@UUO.		;LOAD THE SECOND ARGUMENT FOR SLIST
	HRLI	L,2000		;SET THE SLIST FLAG
	MOVEM	L,ARGBLK	;SAVE INC FOR SLIST OR TERMINATOR
	AOS	UUO.		;UPDATE THE RETURN ADDRESS
	MOVE	L,.JBUUO	;GET THE UUO
	MOVEM	L,ARGBLK+2	;STORE THE ARRAY ADDRESS
	LDB	L,[POINT 3,.JBUUO,12];GET THE TYPE CODE
	MOVE	L,ARGTAB(L)	;GET THE FOROTS TYPE CODE
	DPB	L,[POINT 13,ARGBLK+2,12];STORE IN THE ARG BLOCK
	MOVEI	L,1		;GET THE DEFAULT INCREMENT
	MOVEM	L,ARGBLK+1	;STORE IN THE ARGBLOCK
	SETZM	ARGBLK+3	;SET THE END OF THE ARGBLOCK
	JRST	EXEC		;GO TO FOROTS
DATA:	SETZM	ARGBLK+1	;SET THE END FLAG FOR (IOLST.)
	MOVE	L,.JBUUO	;GET THE DATA UUO
	HRLI	L,1000		;SET THE DATA ARG CALL
	MOVEM	L,ARGBLK	;SAVE IN ARGBLK
	LDB	L,[POINT 3,.JBUUO,12] ;GET THE OLD ARG TYPE
	MOVE	L,ARGTAB(L)		;GET THE NEW ARG TYPE
	DPB	L,[POINT 4,ARGBLK,12] ;SAVE THE NEW ARG TYPE
	JRST	EXEC		;DISPATCH TO FOROTS

INF:	SKIPA	L,[XWD 2000,[ASCIZ/SEQIN/]]	;SET "SEQIN" MODE
OUTF:	MOVE	L,[XWD 02000,[ASCIZ/SEQOUT/]]	;SET "SEGOUT"
	MOVEM	L,ARGBLK+1	;SAVE IN ARGBLK
	MOVEM	0,FILE		;SAVE THE FILE NAME
	HRR	L,.JBUUO	;GET THE UNIT NUMBER
	HRLI	L,003000	;SET THE ARG COUNT TO 4
	MOVEM	L,ARGBLK	;SAVE IN THE ARG BLOCK
	MOVE	L,[XWD 006000,FILE]	;GET THE FILE ADDRESS
	MOVEM	L,ARGBLK+2	;SAVE IN ARGBLK
	JRST	EXEC		;GO TO FOROTS

FILE:	BLOCK	1		;TEMP STORE FOR THE FILE NAME

FIN:	HLRZS	(P)		;GET THE ENTRY ADDRESS
	MOVE	T0,SAVET0	;RESTORE T0
	PUSHJ	P,@(P)	;GO TO FOROTS
	JRST	ENDXIT		;CLEAR THE END= ERR= RETURNS

MTOP:	MOVE	L,.JBUUO	;LOAD THE FORTRAN UNIT NUMBER
	TLC	L,25000		;CLEAR THE UUO/SET ARG SIZE TO 1
	MOVEM	L,ARGBLK	;SAVE IN ARGBLK
	JRST	EXEC		;GO TO FOROTS
ERRRTN:	SKIPA	L,ERR.		;GET THE REAL RETURN
ENDRTN:	MOVE	L,END.		;END RETURN
	HRRM	L,UUO.		;STORE THE RETURN
ENDXIT:	SETZM	ERR.
	SETZM	END.
	JRST	EXEC1		;EXIT TO THE USER

ARGTAB:	Z	;0
	Z	;1
	4	;2
	1	;3
	6	;4
	17	;5
	10	;6
	14	;7
ARGBLK:	BLOCK	1
	BLOCK	1
	BLOCK	1
VADDR.:	BLOCK	1

END.:	BLOCK	1
ERR.:	BLOCK	1
RANAC.:	BLOCK	1
RECNO.:	BLOCK	1
SAVEL:	BLOCK	1	;TEMP FOR AC L
SAVET0:	BLOCK	1	;TEMP FOR AC0
	SUBTTL FORJAK - DUMMY ENTRY POINTS FOR FOROTS

	ENTRY	EXER1.	;SOURCE LEVEL ERROR ENTRY
EXER1.:
	SOS	T1,(P)	;GET THE ADDRESS
	MOVSI	T0,(JFCL)
	MOVEM	T0,(T1)
	OUTSTR	[ASCIZ /
%5 Source level error at user's loc	/]
	HRLOS	T1		;SWAP HALFS
EXER0:	TRZN	T1,700000	;END OF DIGITS
	JRST	EXER2
	SETZ	T0
	LSHC	T0,3		;GET AN OCTAL DIGIT
	IORI	T0,60		;ASCII DIGIT
	OUTCHR	T0		;TYPE IT
	JRST	EXER0
EXER2:	OUTSTR	[ASCIZ /
/]
	JRST	PAUSE.##		;DO A PAUSE
	END
  