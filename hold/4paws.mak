TITLE 4PAWS  %2.(102)
SUBTTL	D. TODD /DRT/ 15-JAN-1973
;***COPYRIGHT 1972,1973 DIGITAL EQUIPMENT CORP., MAYNARD, MASS.***
;FROM	20 MARCH 1966		ED YOURDON


;THE PAUSE SUBROUTINE MAY BE CALLED FOR ANY OF THE THREE
;FOLLOWING FORTRAN STATEMENTS:
;	PAUSE
;	PAUSE N
;	PAUSE "MESSAGE"
;WHERE N IS AN OCTAL NUMBER, AND "MESSAGE" IS AN ASCII
;MESSAGE.
;THE CALLING SEQUENCES FOR THE THREE TYPES OF PAUSE STATEMENTS
;ARE AS FOLLOWS:
;	PAUSE	GENERATES
;	MOVEI	1, 0
;	PUSHJ	P, PAUSE.

;	PAUSE N		GENERATES
;	HRROI	1, CONST
;	PUSHJ	P, PAUSE.	;CONST HAS THE OCTAL NUMBER IN IT

;	PAUSE "MESSAGE"	GENERATES DIFFERENTLY.

;AFTER TYPING PAUSE, AN OCTAL NUMBER AND/OR A MESSAGE, THE
;ROUTINE CALLS THE DEBUGGER.

	SEARCH	FORPRM

	P=	17
	A=	0
	B=	1
        TP%STR= 15


IFN F10LIB,<
	HELLO	(PAUS.)
	SETOM	PSEFLG		;SET FLAG FOR PAUSE ENTRY
	JRST	PSEARG

	HELLO	(STOP.)
	SETZM	PSEFLG		;SET STOP FLAG
PSEARG:
        MOVEM   17,SAVE+17
        MOVEI   17,SAVE
        BLT     17,SAVE+16
	SETZ	T1,		;ASSUME NO ARG
	SKIPL	-1(L)	;IS THERE AN ARG
	JRST	PAUSEZ
	LDB	T1,[POINT 4,(L),12]	;GET THE ARG TYPE
        CAIE    T1,TP%STR        ; STRING
        JRST    PAUSEN          ; NO, A CONSTANT.
        MOVE    1,[XWD 6,MESS5]         ;CREATE A POINTER FOR SFPRINT.
        SKIPN   PSEFLG          ; SKIP IF PAUSE,ELSE TO PRINT
        HRRI    1,MESS6         ; 'STOP' INSTEAD OF 'PAUSE'.
        JSP     17,SPNT.##      ; SFPRINT.
        MOVE    1,@(L)           ;POINTER FOR MESSAGE.
        JSP     17,SPNT.##
        MOVE    1,[2,,[BYTE(7)15,12]]
        JSP     17,SPNT.        ; CR,LF.
        JRST    PAWS3A          ; COMMON CODING.

PAUSEN:
	HRRO	T1,(L)		;GET THE ADDRESS OF THE CONSTANT
>
IFN F40LIB,<
	JRST	PAUSEZ
	HELLO	(PAUSE.)
        MOVEM   17,SAVE+17
        MOVEI   17,SAVE
        BLT     17,SAVE+16
	SETOM	PSEFLG		;SET PAUSE FLAG
	JRST	PAUSEZ
>
PAUSEZ:
	TTCALL	13,0		;TURN OFF ^O
	  JFCL
	HLRE	A, B		;GET WORD COUNT OF MESSAGE
	JUMPLE	A, PAUSE1	;CONSTANT, OR NO MESSAGE?
	MOVNS	A		;MESSAGE - FORM AOBJN WORD
	HRLM	A, B		;IN ACCUMULATOR B
        MOVEI   L,MSBLK3                ;OUTPUT THE MSG ON TTY.
	PUSHJ	P,OUT.##
	MOVEI	L,[XWD 001000,(B)
		XWD 0,0]
PRLOOP:	PUSHJ	P,IOLST.##
	AOBJN	B, PRLOOP	;LOOP BACK FOR MORE WORDS
	JRST	PAUSE3	;ALL DONE, TYPE G AND X STUFF

PAUSE1: MOVEI   L,MSBLK4
	PUSHJ	P,OUT.##
	MOVEI	L,[XWD 001000,MESS5
		XWD 0,0]
	SKIPN	PSEFLG		;SKIP IF PAUSE CALL
	MOVEI	L,[XWD 001000,MESS6
		XWD 0,0]
	PUSHJ	P,IOLST.##
	HRRZ	A, (B)		;GET THE NUMBER IF THERE IS ONE
	JUMPGE	B,PAUSE3	;IS THERE REALLY A NUMBER?
	MOVEI	L,[XWD 001000,A
		XWD 0,0]
	PUSHJ	P,IOLST.##
PAUSE3:	PUSHJ	P,FIN.##	;FINISH PREVIOUS IO
PAWS3A:	SKIPN	PSEFLG		;PAUSE/STOP
	JRST	EXIT.##	;STOP EXIT
        JSP     17,PAWS.##
        MOVSI   17,SAVE
        BLT     17,17
	POPJ	P,		;EXIT


MESS3:	ASCII	"(1H 14A5/)"
MESS4:	ASCII	"(1H A5,1X,I6)"
MESS5:	ASCII	"PAUSE     "
MESS6:	ASCII	"STOP  "
PSEFLG:	BLOCK	1
        XWD     -5,0
MSBLK3: XWD     0,1
        Z
        Z
        MESS3
        EXP     MESS4-MESS3
        
        XWD     -5,0
MSBLK4: XWD     0,1
        Z
        Z
        MESS4
        EXP     MESS5-MESS4
        
SAVE:   BLOCK   20
	END

  