COMMENT    VALID 00019 PAGES 
RECORD PAGE   DESCRIPTION
 00001 00001
 00007 00002	BEGIN "PROFILE" 
 00009 00003	    COMMENT Swinehart's scanner package (an old version)
 00014 00004	    DEFINE OVERDEL="14", NOTATOM="13", STRSTOP="12", STRTEST="11"
 00017 00005	    PROCEDURE DOLAND(REFERENCE INTEGER I INTEGER MASK)
 00019 00006	    PROCEDURE ATOMINIT(
 00021 00007	    INTEGER PROCEDURE ATOM(REFERENCE STRING TOSSED,TOKEN)
 00024 00008	VARIOUS PRELOADED ARRAYS
 00027 00009	MAIN PROGRAM, EXECUTION STARTS HERE
 00031 00010	PROCEDURES SCAN1, SCAN2, TERP1, AND TERPRI
 00035 00011	PROCEDURES INDENT,UNDENT,SPRINT, PRINT1, & COUNTSTR
 00039 00012	PROCEDURES WIDTH,PRINTS, PRINTC, AND FINISH
 00045 00013	RECURSIVE PROCEDURE SCANSTMT(INTEGER DOINDENT)
 00048 00014	ROUTINES FOR SIMPLE EX AND NON-EX STMTS AND PROC. DECLS
 00050 00015	ROUTINES FOR BLOCK AND CASE STATEMENTS
 00053 00016	DO,DONE,RETURN,FOR,FOREACH,WHILE,GOTO
 00055 00017	IF STATEMENT AND STARTCODE, ALSO NULL STATEMENT
 00057 00018	END OF THE VARIOUS STATEMENT ROUTINES
 00058 00019	THE REST OF THE MAIN PROGRAM
 00061 ENDMK
;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  BEGIN "PROFILE" 

COMMENT A PROGRAM TO PRODUCE PROGRAM PROFILES USING THE
 LIST AND COUNTER FILES CREATED BY USING THE /K OPTION
 OF SAIL;

DEFINE SWTSIZ="12", GOODSWT="""BCFIKLNST""",NEXTCOUNTER="KOUNTR[IKNT_IKNT+1]",
SRCMODE="'17", LSTMODE="0", RELMODE="0", SRCEXT="""KNT""",LSTEXT="NULL",
RELEXT="""PFL""",PROCESSOR="""PROFILE""",EMRK="132",OFILE="BIN",TAB="'11",
FINDFF="4",FINDSEMI="5",FINDLF="6",FINDTAB="7",CMNTCODE="133";

REQUIRE 200 STRINGPDL; REQUIRE 300 SYSTEMPDL;

STRING ARRAY NAMES[1:10];
INTEGER ARRAY STRT[1:11],INP[1:4];
INTEGER I,J,N,IFIL,NFIL,FL,CURCOUNT,Q,NTABS,NT,NLEN,OLEN,TYP1,TYP2,
  NTYPE,OTYPE,NLINES,IKNT,NKNT,SEMICOL,FFLAG,PLVL,MXKTR,STPFLG,
  CONIND,BLKIND,LASCOL,CTRCOL,MAXCOL,TI1,TI2,TI3,
  SPAC1,SPAC2,SPACO,SPACT,SPACN,FIRSTSCAN,TIA,IGNCMT;
STRING HEAD1,HEADNG,TABS,NLINE,OLINE,STR1,STR2,STR1A,STR2A,BLANX,BFILL,
  CONFIL,TS1,TS2,IGN;

                                                                                                                                                                                                                                                                                                                                                                                                     COMMENT Swinehart's scanner package (an old version);

    REQUIRE "SCNCMD.SAI[1,DCS]"	SOURCEFILE;

DSCR SCNSER.SAI -- a package to provide SCANNER operations
DES This insert provides a token-scanning service more 
   extensive than the SAIL SCAN function can handle. It
   can handle delimiters, identifiers, and string constants,
   as well as filler characters, and characters which are
   to be ignored completely.  There is provision for extension
   to handle numbers.
CAL Call ATOMINIT("DEL", "TS", "TC", "IGN", "IFN", SS, NUMF)
  to parameterize the scanner.  All characters in DEL will be
                        considered delimiters.  The characters in TS are valid characters
  for the start of an identifier.  Those in TC are valid identifier
  characters after the first.  The characters in IGN are fillers --
  they are returned separately, and never appear in a token, but
  will break an identifier scan.  Those in IGN will be ignored on
  input.  SS is the string constant quote character -- otherwise,
  string constants are scanned like SAIL scans them.  NUMF is true
  if numbers are to be handled (not implemented).
 Call I_ATOM(@"TOSS",@"TOKEN") to scan from the input file
                                                       (ignoring line numbers).  See Results below for exact returns.
RES The result of ATOM is a code -- the character code in ASCII
  for a delimiter -- otherwise an integer >127 -- TOKENCODE for
  identifiers (numbers currently returned in string form as IDs),
  STRCONCODE for string constants, ILLEGALCODE for illegal characters,
  EOFCODE when EOF is seen.  TOSSED contains all those fill
   (IGN) characters passed over before reaching the token.  TOKEN
   contains the character(s) of the token itself.  TOKLEN (a local
   variable, see below) contains the length of the token (for string
                                         constants, the length of the string from the last LF to the end).
PAR The following will be local to the REQUIRing block:
  SETBIT, SELSTR procedures, SCANTABLE(SCT) array,
   TEMP, TEMP1, STEMP, STEMP1 variables, LETTER, LETDIG ... defs.
 The following are also local, and possibly useful:
  DOLAND, DOLOR, UPPERCASE routines (uses obvious on inspection),
  RESCAN variable, if set, causes same token to be returned again
  TOKLEN (see above), OVERDEL, NOTATOM, ... break tables (14-10),
  TOKENCODE, STRCONCODE ... (see above) return codes.
 The following should be set to affect the ATOM routine:
                             RESCAN -- set to rescan -- TOKEN and TOSSED will NOT be 
   set during a rescan -- only the return code is saved!!!!!
SID SCNCMD.SAI is required
  Other side effects should be limited to changes to the variables
  described above.
;
                                                                                                                                                                                                                                                                                                                                                                                                                   DEFINE OVERDEL="14", NOTATOM="13", STRSTOP="12", STRTEST="11";
    DEFINE CHKLEN="10";

    DEFINE LETTER="1", LETDIG="2", DIGIT="4", PARTOFNUMBER="8",
     STRINGSTART="16", DELIM="32", IGNORE="64",
     IGNORE="128", ILLEGAL="256",LOWERCASE="512";
    DEFINE TOKENCODE="128", STRCONCODE="129", ILLEGALCODE="130",
     EOFCODE="131";



 COMMENT These values go into the scan table, which controls all,
    iff default is indicated by the user
    ;
#

PRELOADWITH
 	ILLEGAL,		Comment 0;
 [8]	DELIM,			Comment        ;
	IGNORE,		Comment TAB;
	DELIM,			Comment LF;
	IGNORE,			Comment VT;
	DELIM,			Comment FF;
	IGNORE,			Comment CR;
 [10]	DELIM,			Comment           ;
	LETDIG LOR LETTER,	Comment UNDERLINE;
 [7]	DELIM,			Comment        ;
	IGNORE,		Comment SPACE;
	DELIM,			Comment ! ;
	STRINGSTART,		Comment " ;
 [13]	DELIM,			Comment # # $ % & ' ( ) * + - . /;
 [10]	DIGIT LOR
       LETDIG LOR PARTOFNUMBER,	Comment 0-9;
 [7]	DELIM,			Comment : SEMIC < = > ? @ ;
 [26]	LETDIG LOR LETTER,	Comment A-Z;
 [6]	DELIM,			Comment [ \ ] ^ _ ` ;
 [26]	LETDIG LOR LETTER
	       LOR LOWERCASE,	Comment a-z;
 [2]	DELIM,			Comment { | ;
	ILLEGAL,		Comment ALTMODE;
	DELIM,			Comment ~ ;
	ILLEGAL;		Comment DELETE;

#
    SAFE INTEGER ARRAY SCANTABLE[0:127];
    DEFINE SCT="SCANTABLE";

    INTEGER TEMP,TEMP1,RESCAN,TOKLEN;
    STRING STEMP,STEMP1;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PROCEDURE DOLAND(REFERENCE INTEGER I; INTEGER MASK);
    I_I	LAND MASK;

    PROCEDURE DOLOR(REFERENCE INTEGER I; INTEGER MASK);
    I_I	LOR MASK;

    PROCEDURE CLEAR(INTEGER MASK);
    BEGIN "CLEAR"
       TEMP1_-1	XOR MASK;
       FOR TEMP_0 STEP 1 UNTIL 127 DO
	DOLAND(SCT[TEMP],TEMP1);
    END	"CLEAR";

    PROCEDURE SETBIT(STRING S;INTEGER MASK);
    IF S'177 THEN BEGIN "SETBIT"
       CLEAR(MASK);
       WHILE TEMP_LOP(S) DO DOLOR(SCT[TEMP],MASK)
    END	"SETBIT";

    STRING PROCEDURE SELSTR(INTEGER MASK);
    BEGIN "SELSTR"
       STEMP_NULL;
       FOR TEMP_0 STEP 1 UNTIL 127 DO
	IF SCT[TEMP] LAND MASK THEN STEMP_STEMP&TEMP;
       RETURN(STEMP)
    END	"SELSTR";

    STRING PROCEDURE UPPERCASE(STRING S);
    BEGIN "UPPERCASE"
       STEMP_NULL;
       WHILE LENGTH(S) DO STEMP_STEMP&
	(IF LOWERCASE LAND (TEMP1_SCT[TEMP_LOP(S)]) THEN
	 (TEMP1	LSH -18) ELSE TEMP);
       RETURN(STEMP)
    END	"UPPERCASE";
                                                                                                                                                                                                                                                                                                                             PROCEDURE ATOMINIT(
     STRING  DELIMITERSTRING,
     TOKENSTART,
     TOKENCONTINUE,
     IGNORESTRING,
     IGNORESTRING;
    INTEGER STRINGSTART,
     NUMBERFLAG		);

    BEGIN "ATOMINIT"
       FOR TEMP_"a" STEP 1 UNTIL "z" DO
	DOLOR(SCT[TEMP],(TEMP-"a"+"A") LSH 18);

       SETBIT(DELIMITERSTRING,DELIM);
       SETBIT(TOKENSTART,LETTER);
       SETBIT(TOKENCONTINUE,LETDIG);
       IF STRINGSTART'177 THEN BEGIN
	  CLEAR(STRINGSTART);
	  DOLOR(SCT[STRINGSTART],STRINGSTART)
       END ELSE	STRINGSTART_"""";
       SETBIT(IGNORESTRING,IGNORE);
       SETBIT(IGNORESTRING,IGNORE);

       STEMP1_SELSTR(IGNORE); "ALWAYS IGNORED COMPLETELY"

       SETBREAK(OVERDEL,SELSTR(IGNORE)&STEMP1,STEMP1,"XNR");
       SETBREAK(NOTATOM,SELSTR(LETDIG)&STEMP1,STEMP1,"XNR");
       SETBREAK(STRSTOP,STRINGSTART,NULL,"INA");
       SETBREAK(STRTEST,NULL,NULL,"XNR");
       SETBREAK(CHKLEN,'12,'15,"I");
       RESCAN_FALSE;
    END	"ATOMINIT";
                                                                                                                                                                                                                                                                                               INTEGER PROCEDURE ATOM(REFERENCE STRING TOSSED,TOKEN);
    BEGIN "ATOM"
       INTEGER RET;
       IF RESCAN THEN BEGIN
	  RESCAN_FALSE;
	  RETURN(RET)
       END;
       SOURCECOUNT_200;	TOKLEN_1;
       TOSSED_INPUT(SRC,OVERDEL);		"BLANKS AND SUCH"
       IF SRCEOF THEN RETURN(RET_EOFCODE);
       TEMP_SCT[SRCBRK];			"SCANNER TABLE BITS"
       IF TEMP LAND LETTER THEN	BEGIN "TOKEN"
	  TOKEN_INPUT(SRC,NOTATOM);	"GET IDENTIFIER"
	  TOKLEN_LENGTH(TOKEN);
	  RETURN(RET_TOKENCODE)
       END "TOKEN";

       IF TEMP LAND STRINGSTART	THEN BEGIN "STRCON"
	  TOKEN_NULL;
	  DO BEGIN "GET STRING"
	     SOURCECOUNT_1;
	     TOKEN_TOKEN&INPUT(SRC,0);	"PICK UP STRINGSTART"
	     SOURCECOUNT_200;
	     DO	TOKEN_TOKEN&
	      INPUT(SRC,STRSTOP) UNTIL SRCBRK; "GO UNTIL STRINGSTART"
	     INPUT(SRC,STRTEST);		"CHECK FOR 2 STRINGSTARTS";
	  END "GET STRING" UNTIL (SCT[SRCBRK] LAND STRINGSTART);
	  STEMP1_TOKEN;	"COMPUTE TOKLEN"
	  DO STEMP_SCAN(STEMP1,CHKLEN,TEMP) UNTIL TEMP'12;
	  TOKLEN_LENGTH(STEMP);	IF TOKLENLENGTH(TOKEN)	THEN TOKLEN_-TOKLEN;
	  RETURN(RET_STRCONCODE)
       END "STRCON";

       "MUST NOW BE EITHER DELIMITER OR ILLEGAL"

       SOURCECOUNT_1; TEMP1_SRCBRK;
       TOKEN_INPUT(SRC,0);			"GET THE CHARACTER"
       IF TEMP LAND ILLEGAL THEN RETURN(RET_ILLEGALCODE)
	ELSE RETURN(RET_TEMP1)

    END	"ATOM";
DSCR END OF SCNSER
;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       COMMENT VARIOUS PRELOADED ARRAYS;

PRELOADWITH
"ARRAY","BEGIN","BOOLEAN","CASE","DEFINE","DO","DONE","ELSE","END","EXTERNAL",
"FOR","FOREACH","FORTRAN","FORWARD","GLOBAL","GO","GOTO","IF","INTEGER",
"INTERNAL","ITEM","ITEMVAR","LABEL","MESSAGE","NEEDNEXT","OWN","PRELOADWITH",
"PROCEDURE","QUICKCODE","REAL","RECURSIVE","REQUIRE","RETURN","SAFE","SET",
"STARTCODE","STRING","UNTIL","WHILE";
STRING ARRAY STAB[1:39];
PRELOADWITH
[65]0,'100,'203,'400,'507,'1012,'1316,'1721,0,'2226,[2]0,'2700,'3000,
'3100,'3200,'3334,'3500,'3641,'4245,0,'4600,0,'4700;
SAFE INTEGER ARRAY XFERTAB[0:127];

COMMENT RESERVED WORDS ARE CLASSIFIED AS FOLLOWS:
	1-	"FORWARD" AND "FORTRAN"
	2-	"EXTERNAL"
	3-	NECESSARILY DETERMINES A NON-EXECUTABLE STATEMENT
	4-	A TYPE, THIS MIGHT BE A PROCEDURE DECL.
	5-	"PROCEDURE"
	6-	"NEEDNEXT"
	7-	"BEGIN"
	8-	"CASE"
	9-	"DO"
	10-	"DONE","RETURN"
	11-	"FOR","FOREACH"
	12-	"WHILE"
	13-	"GO","GOTO"
	14-	"IF"
	15-	"QUICKCODE","STARTCODE"
	16-	"END", "ELSE", OR "UNTIL" - NULL STATEMENT
;
PRELOADWITH
3,7,4,8,3,9,10,16,16,2,11,11,1,1,4,13,13,14,[4]4,3,4,6,3,3,5,15,4,4,3,10,3,4,
15,4,16,12;
INTEGER ARRAY SVAL[1:39];

COMMENT IN THE FOLLOWING TRANSITION TABLE, THE POSITIVE ENTRIES INDICATE
	STATE TRANSITIONS AND THE NEGATIVE ONES INDICATE EXITS AS FOLLOWS:
	(-2)	EXECUTABLE STATEMENT
	(-3)	NON-EXECUTABLE STATEMENT
	(-4)	FORWARD OR EXTERNAL PROCEDURE DECL.
	(-5)	ACTUAL PROCEDURE DECL.
	(-6)	ERROR
;
PRELOADWITH
-2,-4,2,-3,3,-5,1,
-3,-4,-6,-3,2,-4,-6,
-3,-4,-6,-3,3,-5,-6;
INTEGER ARRAY XITION[1:3,0:6];

INTEGER PROCEDURE LOOKR;
	BEGIN INTEGER I,I1,I2;
	I1_(I2_XFERTAB[STR1]) LSH (-6);
	IF I1=0 THEN RETURN(0);
	I2_I2 LAND '77;
	IF I2=0 THEN RETURN(IF EQU(STR1,STAB[I1]) THEN SVAL[I1] ELSE 0);
	FOR I_I1 STEP 1 UNTIL I2 DO
		IF EQU(STR1,STAB[I]) THEN RETURN(SVAL[I]);
	RETURN(0);
	END "LOOKR";


                                  COMMENT MAIN PROGRAM, EXECUTION STARTS HERE;

SCANTABLE['177]_DELIM;
ATOMINIT(",+-|()[]/^_&<>=*%;:{~"&'14&'177,
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$0123456789@.'",
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$0123456789@.'",
	'11&'12&'40,
	'177,
	"""",
	0);
COMMENT THE FOLLOWING IS NECESSARY SINCE SAIL DOES NOT ALWAYS CLEAR
THE LOW ORDER BIT IN THE LIST FILE;

FOR I_1 STEP 1 UNTIL 18 DO BREAKSET(I,NULL,"P");
SETBREAK(FINDFF,'14&'12,'15,"IPS");
SETBREAK(FINDSEMI,";",NULL,"IPA"); 
SETBREAK(FINDLF,'12,'15,"IPS");
SETBREAK(FINDTAB,'11,NULL,"IPS");

ONETIME_0;

HEAD1_"     PROGRAM PROFILE 				";
NT_0;
SETFORMAT(8,0);
BLANX_"                                                                         "&
  "                                                     ";
TABS_"									"&
  "								";
WHILE TRUE DO
	BEGIN "SUPERLOOP"
	NXTFIL_0;
	WANTBIN_TRUE;
	WANTLST_FALSE;
	SOURCECOUNT_100;

COMMENT  FIRST OPEN COUNTER FILE AND LISTING FILE;

	COMMANDSCAN;

COMMENT THE FOLLOWING SWITCHES ARE IMPLEMENTED:
	
	/nB	INDENT n SPACES FOR BLOCKS (default 4)
	/nC	INDENT n SPACES FOR CONTINUATIONS (default 2)
	/F	FILL OUT EVERY 4th LINE WITH .   .   .
	/I	IGNORE COMMENTS OTHER THAN STRING CONSTANTS
	/nK	MAKE COUNTER ARRAY OF SIZE n (default 200)
	/nL	MAXIMUM LINE LENGTH OF n (default 120)
	/N	DON'T FILL OUT EVERY 4th LINE WITH .   .   .
	/S	STOP (EXIT PROGRAM) AFTER THIS PROFILE
	/T	TELETYPE MODE = /1C/2B/80L/F
;

	MXKTR_200;
	CONIND_2;BLKIND_4;
	IGNCMT_STPFLG_0;
	FFLAG_1;
	MAXCOL_ 120;
	I_1;
	WHILE LENGTH(SWTSTR)>0 DO
		BEGIN TEMP_LOP(SWTSTR);
		CASE TEMP-1 OF
			BEGIN 
			BLKIND_SWTVAL[I];
			CONIND_SWTVAL[I];
LAG_1;
			IGNCMT_1;
			MXKTR_SWTVAL[I];
			MAXCOL_SWTVAL[I];
			FFLAG_0;
			STPFLG_1;
			BEGIN
			  CONIND_1; BLKIND_2; MAXCOL_80; FFLAG_1;
			END;
			USERERR(0,0,"ILLEGAL SWITCH")
			END;
		I_I+1;
 		END;

	CONFIL_BLANX[1 FOR CONIND];
	CTRCOL_ (MAXCOL LAND '777777777770) -8;
	LASCOL_ CTRCOL-9;

BEGIN "KNTLOOP"

INTEGER ARRAY KOUNTR[1:MXKTR];

                                                                                                                                                                                                                                                                                                                                                                                                                                                                         COMMENT PROCEDURES SCAN1, SCAN2, TERP1, AND TERPRI;


PROCEDURE SCAN2;
	BEGIN 
	SPAC2_0;
	TYP2_ATOM(IGN,STR2A);
	WHILE TYP2='177 DO
		BEGIN TYP2_ATOM(IGN,STR2A);
		IF TYP2=2 THEN TYP2_ATOM(IGN,STR2A)
		ELSE IF TYP2=3 THEN TYP2_EMRK
		ELSE USERERR(0,0,"ILLEGAL CHAR  <'"&CVOS(TYP2)&"> AFTER '177
FILE= "&NAMES[IFIL]&"
OLINE= "&OLINE&"
NLINE= "&NLINE&"
NEXT 2 TOKENS= "&STR1A&STR2A);
		END;
	IF TYP2=EOFCODE THEN
		BEGIN TYP2_TOKENCODE; STR2_"END";
		STR2A_"END (supplied by scanner)";SPAC2_1;
		RETURN;
		END;
	IF TYP2=TOKENCODE THEN
		BEGIN SPAC2_1;
		STR2_UPPERCASE(STR2A);
		IF EQU(STR2,"COMMENT") THEN
        			BEGIN 
			DO 
				STR2A_STR2A&INPUT(SRC,FINDSEMI)
			UNTIL SRCBRK;
			TYP2_CMNTCODE;
			END;
		END
	ELSE BEGIN IF TYP2=STRCONCODE THEN SPAC2_1; STR2_STR2A; END;
	IF TYP2='14 THEN
		BEGIN
		SOURCECOUNT_200; TS1_INPUT(SRC,FINDLF);
		TS2_NULL;
		WHILE (TS1[ FOR 1]"-")(LENGTH(TS1)>0) DO
			BEGIN TS2_TS1[ FOR 1]& TS2;
			TS1_TS1[1 TO -1];
			END;
		TI1_ INTSCAN(TS2,TI2);
 		TS1_INPUT(SRC,FINDLF); "READ THE SECOND HEADING LINE"
		IF (TI1=1)FIRSTSCAN THEN
			BEGIN STR2A_ '15&'14;
			TYP2_CMNTCODE;
			END
		ELSE SCAN2;
		END;
	END "SCAN2";

PROCEDURE SCAN1;
	BEGIN
	SPAC1_SPAC2;
	STR1_STR2;
           	STR1A_STR2A;
	TYP1_TYP2;
	SCAN2;
	END "SCAN1";

PROCEDURE TERP1;
	BEGIN INTEGER L2;
	STRING FILL;
	IF NLEN=0 THEN SPACO_0 ELSE IF SPACN THEN
		BEGIN TIA_LOP(NLINE); NLEN_NLEN-1; SPACN_0 END;
	IF LENGTH(OLINE)=0 THEN RETURN;
	L2_(OLEN+8) LAND '777777777770;
	OUT(OFILE,BFILL&OLINE&TAB);
	IF (OTYPE0) THEN
		BEGIN 
		FILL_ IF (FFLAG0)((NLINES LAND 3)=0) THEN "   .   ." ELSE TAB;
		WHILE L2<CTRCOL DO
      			BEGIN OUT(OFILE,FILL);
			L2_L2+8;
			END;
		OUT(OFILE,CVS(CURCOUNT)&CRLF);
		OTYPE_0;
		END
	ELSE OUT(OFILE,CRLF);
	OLINE_NULL;
	NLINES_NLINES+1;
	END "TERP1";

PROCEDURE TERPRI;
	BEGIN
	TERP1;
  	TI3_NT % 8;
	BFILL_TABS[1 FOR TI3]&BLANX[1 FOR NT-8*TI3];
	OLEN_NT;
	END "TERPRI";


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    COMMENT PROCEDURES INDENT,UNDENT,SPRINT, PRINT1, & COUNTSTR;

PROCEDURE INDENT(INTEGER NCOLS);
	BEGIN
	NT_ NT+NCOLS;
	IF NTLASCOL-1 THEN 
	    BEGIN SETFORMAT(0,0);
	    USERERR(0,0,"NESTING TOO DEEP FOR PRINTING. CHANGE INDENTION COUNTS
FOR BLOCKS AND CONTINUATIONS FROM /"&CVS(BLKIND)&"B/"&CVS(CONIND)&"C");
	    END;
	TERPRI;
	END;

PROCEDURE UNDENT(INTEGER NCOLS);
	BEGIN
	NT_ NT-NCOLS;
	TERPRI;
	END;

PROCEDURE SPRINT;
	BEGIN
	IF NLEN=0 THEN RETURN;
	OLINE_OLINE&NLINE;
	OLEN_OLEN+NLEN;
	OTYPE_ OTYPE LOR NTYPE;
	NLINE_NULL;
	SPACN_0;
	NLEN_0;
	END;

PROCEDURE PRINT1;
	BEGIN INTEGER L1;
	IF SPAC1  SPACO THEN 
		BEGIN STR1A_" "&STR1A; SPACT_1; 
		IF NLEN=0 THEN SPACN_1;
		END ELSE SPACT_0;
	L1_LENGTH(STR1A);
	IF OLEN+NLEN+L1<LASCOL THEN
		BEGIN NLINE_NLINE&STR1A;
		NLEN_NLEN+L1;
		END
	ELSE	BEGIN TERPRI; 
		IF SPACN  (NLEN=0) THEN
			BEGIN SPACN_SPACT_0; TIA_LOP(STR1A); L1_L1-1 END;
		IF OLEN+NLEN+L1<LASCOL THEN
			BEGIN NLEN_NLEN+L1;
			NLINE_NLINE&STR1A;
			END
		ELSE	BEGIN OLINE_NLINE;
			OTYPE_NTYPE;
			OLEN_OLEN+NLEN;
			TERPRI; IF SPACT THEN BEGIN TIA_LOP(STR1A); L1_L1-1 END;
			WHILE L1+NT+CONIND > LASCOL DO
				BEGIN OLINE_CONFIL& STR1A[1 FOR LASCOL-NT-CONIND];
				OTYPE_NTYPE;
				OLEN_ LASCOL;
				TERPRI;
				STR1A_STR1A[LASCOL-NT-CONIND+ 1 TO ];
				L1_LENGTH(STR1A);
				END;
			NLINE_CONFIL&STR1A;
			NLEN_L1+CONIND;
			END;
		END;
	SPACO_SPAC1;
	END "PRINT1";
DEFINE PASSCOMMENT= "WHILE (TYP1=STRCONCODE)(TYP1=CMNTCODE) DO
	BEGIN NTYPE_0; IF TYP1=STRCONCODE THEN PRINTS ELSE
	 PRINTC; SPRINT;SCAN1; END",
	CHECKSEMI= "IF TYP1="";"" THEN 
		BEGIN PRINT1;SPACO_1;SCAN1;SEMICOL_1;
		END ELSE SEMICOL_0;  SPRINT";
DEFINE PASSTOKEN=
		"BEGIN IF TYP1=STRCONCODE THEN PRINTS ELSE 
		  IF TYP1=CMNTCODE THEN PRINTC ELSE
		  BEGIN  IF TYP1=EMRK THEN COUNTSTR; PRINT1 END;
		SCAN1;
		END";

PROCEDURE COUNTSTR;
	BEGIN
	INTEGER I,J;
	GETFORMAT(I,J);
	SETFORMAT(0,0);
	STR1A_"<<"&CVS(NEXTCOUNTER)&">>"; SPAC1_0;
	SETFORMAT(I,J);
	END;

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             COMMENT PROCEDURES WIDTH,PRINTS, PRINTC, AND FINISH;

INTEGER PROCEDURE WIDTH(STRING ST1; INTEGER STCOL);
	BEGIN COMMENT RETURN THE FINAL COLUMN IF STRING
	  ST1 IS PRINTED STARTING IN COLUMN STCOL;
	INTEGER L1,BRK; STRING ST2;
	L1_STCOL;
	WHILE LENGTH(ST1)>0 DO
		BEGIN 
		ST2_SCAN(ST1,FINDTAB,BRK);
		L1_L1+LENGTH(ST2);
		IF BRK=TAB THEN L1_8+(L1 LAND '777777777770);
		END;
	RETURN(L1);
	END;

PROCEDURE PRINTS;
	BEGIN
	INTEGER L1,BRK,T1,L2;
	STRING ST1;
	IF SPAC1  SPACO THEN 
		BEGIN STR1A_" "&STR1A; SPACT_1; IF NLEN=0 THEN SPACN_1 END
		ELSE SPACT_0; 
	ST1_SCAN(STR1A,FINDLF,BRK);
	L1_WIDTH(ST1,NLEN+OLEN);
	IF L1LASCOL THEN
		BEGIN NLINE_NLINE&ST1;
		NLEN_L1-OLEN;
		END
	ELSE	BEGIN
		TERPRI; IF SPACN  (NLEN=0) THEN
			BEGIN TIA_LOP(ST1); SPACT_SPACN_0 END;
		L1_WIDTH(ST1,NLEN+OLEN);
		IF L1LASCOL THEN
			BEGIN
			NLINE_NLINE&ST1; NLEN_L1-OLEN;
			END
		ELSE
			BEGIN OLINE_NLINE; OTYPE_NTYPE;
			NLINE_NULL;
			OLEN_OLEN+NLEN; TERPRI;
			IF SPACT THEN  BEGIN TIA_LOP(ST1); SPACT_0 END;
			L1_WIDTH(ST1,OLEN+CONIND);
			IF L1LASCOL THEN
				BEGIN NLINE_CONFIL&ST1; NLEN_ L1-OLEN;
				END
			ELSE	BEGIN L2_WIDTH(ST1,0);
				IF L2LASCOL THEN
				  BEGIN T1_LASCOL-L2;
				  WHILE (L2_WIDTH(ST1,T1))>LASCOL DO
				    T1_T1-1;
				  OLEN_L2;
				  BFILL_BLANX[1 FOR T1];
				  OLINE_ST1; TERPRI;
				  END
				ELSE
				  BEGIN COMMENT JESUS THAT'S A LONG STRING;
				  WHILE(T1_ LENGTH(ST1))>0 DO
				    BEGIN
				    WHILE (L1_WIDTH(ST1[1 FOR T1],OLEN+CONIND))>LASCOL-2 DO
				      T1_T1-1;
				    NLINE_CONFIL&ST1[1 FOR T1]; NLEN_ L1-OLEN;
				    ST1_ST1[T1+1 TO ];
				    IF LENGTH(ST1)>0 THEN
				      BEGIN NLINE_NLINE&"""&"; NLEN_NLEN+2; SPRINT;
				      TERPRI; ST1_CONFIL&""""&ST1;
				      END;
				    END;
				  END;
				END;
			END;
		END;

COMMENT BY HOOK OR CROOK WE GOT THE FIRST LINE OF THE STRING OUT.
  NOW DO THE REST;

	WHILE LENGTH(STR1A)>0 DO
		BEGIN SPRINT; TERP1; ST1_SCAN(STR1A,FINDLF,BRK);
		BFILL_NULL; OLEN_0;
		L1_WIDTH(ST1,0);
		IF L1LASCOL THEN
		      BEGIN NLINE_ST1;
		      NLEN_L1; L1_0;
		      END
		ELSE
		      WHILE(T1_ LENGTH(ST1))>0 DO
			BEGIN
			WHILE (L1_WIDTH(ST1[1 FOR T1],0))>LASCOL-2 DO
			  T1_T1-1;
			NLINE_ST1[1 FOR T1]; NLEN_ L1;
			ST1_ST1[T1+1 TO ];
			IF LENGTH(ST1)>0 THEN
			  BEGIN NLINE_NLINE&"""&"; NLEN_NLEN+2; SPRINT;
			  TERPRI; ST1_CONFIL&""""&ST1;
			  END;
			END;
		END;
	SPACO_SPAC1;
	END "PRINTS";

PROCEDURE PRINTC;
	BEGIN 
	STRING ST1;
	INTEGER BRK;
	IF (LENGTH(STR1A)=2)EQU(STR1A,'15&'14) THEN 
		BEGIN TERPRI; OUT(OFILE,STR1A); RETURN;
		END;
	IF IGNCMT THEN RETURN;
	TERPRI;
	ST1_SCAN(STR1A,FINDLF,BRK);
	NTYPE_0;
	OLINE_ST1;
  	WHILE LENGTH(STR1A)>0 DO
		BEGIN ST1_SCAN(STR1A,FINDFF,BRK);
		IF BRK='14 THEN
		  BEGIN
		  SOURCECOUNT_200; TS1_SCAN(STR1A,FINDLF,BRK);
		  TS2_NULL;
		  WHILE (TS1[ FOR 1]"-")(LENGTH(TS1)>0) DO
			  BEGIN TS2_TS1[ FOR 1]& TS2;
			  TS1_TS1[1 TO -1];
			  END;
		  TI1_ INTSCAN(TS2,TI2);
		  TS1_SCAN(STR1A,FINDLF,BRK);
		  IF (TI1=1) THEN
			  BEGIN TERP1; OUT(OFILE,'15&'14);
			  OLEN_0;
			  END
		  END
		ELSE
		  BEGIN
		  TERP1; OLEN_0; BFILL_NULL;
		  OLINE_ST1;
		  END;
		END;
	TERPRI;
	END "PRINTC";

PROCEDURE FINISH;
	BEGIN
	NTYPE_1;
	WHILE (TYP1";")((TYP1TOKENCODE) (EQU(STR1,"UNTIL")
	  EQU(STR1,"END")(EQU(STR1,"ELSE")(TYP2=EMRK)))) DO
		PASSTOKEN;
	CHECKSEMI;
	END;

                                                                                                                                                                                                                                                                                                                                                                      RECURSIVE PROCEDURE SCANSTMT(INTEGER DOINDENT);

COMMENT THIS IS THE MAIN PROCEDURE.  IT WILL SCAN A SINGLE STATEMENT
	AND WRITE IT OUT TO THE LIST FILE.  IF THE STATEMENT ENDS
	WITH A SEMICOLON, THE VARIABLE SEMICOL WILL BE SET TO 1 ELSE
	0;

	BEGIN "SCANSTMT"
	INTEGER S1,S2;
	WHILE (TYP1=STRCONCODE)(TYP1=CMNTCODE)
	  ((TYP1=TOKENCODE)(TYP2=":")) DO
		BEGIN COMMENT FIRST HANDLE LABELS AND COMMENTS;
		IF (TYP1=STRCONCODE) THEN
		      BEGIN NTYPE_0;
		      PRINTS; SPRINT;
		      END
		ELSE  IF TYP1=CMNTCODE THEN 
		      BEGIN PRINTC; SPRINT END
		   ELSE
		      BEGIN TERPRI;
                               		      NTYPE_1; CURCOUNT_NEXTCOUNTER;
   		      PRINT1; SCAN1;
   		      PRINT1; SPRINT; TERPRI;
   		      END;
 	        SCAN1;
		END;

COMMENT FIRST DETERMINE WHETHER THIS IS A NULL STATEMENT (SOME NULL
	STATEMENTS ARE CAUGHT BELOW BY THE "PARSER";

	IF TYP1=";" THEN
		BEGIN PRINT1;SPACO_1;NTYPE_1;SCAN1;SPRINT;SEMICOL_1; RETURN;
		END;

COMMENT  DETERMINE THE STATEMENT TYPE BY A FINITE STATE "PARSER";

	J_LOOKR;
	IF J6 THEN
		BEGIN Q_1;
		WHILE Q>0 DO
			BEGIN PRINT1; SCAN1;
			IF J>6 THEN USERERR(0,0,"ERROR IN STMT TYPING
FILE= "&NAMES[IFIL]&"
OLINE= "&OLINE&"
NLINE= "&NLINE&"
                    NEXT 2 TOKENS= "&STR1A&STR2A);
			Q_XITION[Q,J];
			J_LOOKR;
			END;
		J_ -Q;
		END;
	CASE J-2 OF
		BEGIN "BIGSW"

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    COMMENT ROUTINES FOR SIMPLE EX AND NON-EX STMTS AND PROC. DECLS;
 	
	
COMMENT  -- A GARDEN VARIETY EXECUTABLE STATEMENT;
	
	FINISH;

COMMENT  -- A GARDEN VARIETY NON-EXECUTABLE STATEMENT;

	BEGIN  "NONEX"
	NTYPE_0;
	WHILE TYP1";" DO
		PASSTOKEN;
	PRINT1;SPACO_1;SCAN1;SEMICOL_1;SPRINT;
	END;

COMMENT -- A FORWARD OR EXTERNAL PROCEDURE DECLARATION;

	BEGIN "FPROC"
	TERPRI;
	NTYPE_0;
	PLVL_0;
	WHILE (PLVL>0)(TYP1";") DO
		BEGIN PRINT1;
		IF TYP1="(" THEN PLVL_PLVL+1
		ELSE IF TYP2=")" THEN PLVL_PLVL-1;
		SCAN1;
		END;
	PRINT1; SPACO_1;
	SPRINT; SEMICOL_1;
	SCAN1;
	END;

                             COMMENT  -- AN ACTUAL REAL-LIFE PROCEDURE DECLARATION;

	BEGIN  "PROC"
	TERPRI;
	NTYPE_0;
	PLVL_0;
	WHILE (PLVL>0)(TYP1";") DO
		BEGIN PRINT1;
		IF TYP1="(" THEN PLVL_PLVL+1
		ELSE IF TYP2=")" THEN PLVL_PLVL-1;
		SCAN1;
		END;
	PRINT1; SPACO_1;
	SCAN1;
	SPRINT;
	INDENT(CONIND);
	S1_CURCOUNT;
	CURCOUNT_NEXTCOUNTER;
	SCANSTMT(1);
	UNDENT(CONIND);
	CURCOUNT_S1;
	END;

COMMENT  -- ERROR;

	USERERR(0,0,"ERROR IN STMT TYPING
OLINE= "&OLINE&"
NLINE= "&NLINE&"
NEXT 2 TOKENS= "&STR1A&STR2A);

                                                                                                                      COMMENT ROUTINES FOR BLOCK AND CASE STATEMENTS;

COMMENT  -- BEGIN, A BLOCK OR COMPOUND STATEMENT;

	BEGIN "BLOCK"
	TERPRI;
	NTYPE_1;
	PRINT1; SPRINT; SCAN1;
	PASSCOMMENT;
	IF DOINDENT THEN INDENT(BLKIND);
	WHILE (TYP1TOKENCODE)EQU(STR1,"END") DO
		SCANSTMT(1);
	IF DOINDENT THEN UNDENT(BLKIND) ELSE TERPRI;
	PRINT1; SPRINT; SCAN1;
	PASSCOMMENT;
	CHECKSEMI;
	TERPRI;
	END;

COMMENT  -- CASE STATEMENT;

	BEGIN  "CASE"
	NTYPE_1;
	S1_0;
	TERPRI;
	WHILE (TYP1TOKENCODE)EQU(STR1,"OF")(TYP2=EMRK) DO
		PASSTOKEN;
	PRINT1; SPRINT; INDENT(CONIND);
	SCAN1; NTYPE_0;
                                            	PASSCOMMENT; IF EQU(STR1,"BEGIN") THEN USERERR(0,0,"NO BEGIN AFTER CASE
FILE= "&NAMES[IFIL]&"
OLINE= "&OLINE&"
NLINE= "&NLINE&"
NEXT 2 TOKENS= "&STR1A&STR2A);
	PRINT1;SPRINT; SCAN1; INDENT(CONIND);
	DO
		BEGIN  "CASE1"
		TERPRI; SEMICOL_0;
		CURCOUNT_NEXTCOUNTER;
		PASSCOMMENT;
		IF TYP1="[" THEN
			BEGIN DO 
				BEGIN PRINT1; SCAN1;
				END
			UNTIL TYP1="]";
			PRINT1; SCAN1
			END;
		SCANSTMT(1);
		S1_S1+CURCOUNT;
		END
	UNTIL SEMICOL=0;
	PASSCOMMENT; IF EQU(STR1,"END") THEN USERERR(0,0,"NO END AFTER CASE
FILE= "&NAMES[IFIL]&"
OLINE= "&OLINE&"
NLINE= "&NLINE&"
NEXT 2 TOKENS= "&STR1A&STR2A);
        	UNDENT(CONIND); PRINT1; NTYPE_0; 
	SCAN1;
	CHECKSEMI;
	UNDENT(CONIND); CURCOUNT_S1;
	END;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                COMMENT DO,DONE,RETURN,FOR,FOREACH,WHILE,GOTO;

COMMENT  -- DO STATEMENT;

	BEGIN "DOSTMT"
	TERPRI; PRINT1; NTYPE_1; SPRINT;
	INDENT(CONIND);
	CURCOUNT_NEXTCOUNTER;
	SCAN1;
	SCANSTMT(1);
	PASSCOMMENT;
	UNDENT(CONIND);
	IF EQU(STR1,"UNTIL") THEN USERERR(0,0,"NO UNTIL AFTER DO
FILE= "&NAMES[IFIL]&"
OLINE= "&OLINE&"
NLINE= "&NLINE&"
NEXT 2 TOKENS= "&STR1A&STR2A);
	PRINT1; SCAN1;
	FINISH;
	TERPRI;
	CURCOUNT_NEXTCOUNTER;
	END;

COMMENT  -- DONE, RETURN STATEMENTS;

	BEGIN "DONE"
	FINISH;
	TERPRI;
	CURCOUNT_0;
	END;

COMMENT  -- FOR, AND FOREACH STATEMENTS;

 	BEGIN "FORST"
	TERPRI;
	NTYPE_1;
       	WHILE EQU(STR1,"DO") DO
		PASSTOKEN;
	PRINT1; SPRINT;
	INDENT(CONIND); CURCOUNT_NEXTCOUNTER;
	SCAN1;
	SCANSTMT(1);
	UNDENT(CONIND); CURCOUNT_NEXTCOUNTER;
	END;

COMMENT  -- WHILE STATEMENT;

	BEGIN  "WHILE"
	TERPRI;
	CURCOUNT_NEXTCOUNTER;
	NTYPE_1;
	WHILE EQU(STR1,"DO") DO
		PASSTOKEN;
	PRINT1; SPRINT;
	INDENT(CONIND); CURCOUNT_NEXTCOUNTER;
	SCAN1;
	SCANSTMT(1);
	UNDENT(CONIND); CURCOUNT_NEXTCOUNTER;
	END;

COMMENT  -- GO TO STATEMENT;

	BEGIN "GOTO"
	FINISH;
	TERPRI;
	CURCOUNT_0;
	END;

                                                                                                              COMMENT IF STATEMENT AND STARTCODE, ALSO NULL STATEMENT;

COMMENT  -- IF STATEMENT;

	BEGIN "IFSTMT"
	TERPRI;
	S1_CURCOUNT;
	NTYPE_1;
	WHILE (TYP1TOKENCODE)EQU(STR1,"THEN")(TYP2=EMRK) DO
		PASSTOKEN;
	PRINT1; SPRINT; SCAN1;
	INDENT(CONIND); CURCOUNT_NEXTCOUNTER;
	S2_S1-CURCOUNT;
	SCANSTMT(1); S1_CURCOUNT;
	PASSCOMMENT;
	IF (SEMICOL=0) EQU(STR1,"ELSE") THEN
		BEGIN UNDENT(CONIND); CURCOUNT_S2;
		PRINT1; SPRINT; SCAN1;
		INDENT(CONIND); SCANSTMT(1);
		S2_CURCOUNT;
		END;
	UNDENT(CONIND);
	CURCOUNT_S1+S2;
	END;

COMMENT  -- STARTCODE AND QUICKCODE;

	BEGIN "CODE"
	TERPRI;
                        	PRINT1; NTYPE_1; SPRINT; SCAN1; PASSCOMMENT;
	IF DOINDENT THEN INDENT(BLKIND) ELSE TERPRI;
	NTYPE_0;
	WHILE EQU(STR1,"END") DO
		BEGIN PRINT1;
		IF TYP1=";" THEN
			BEGIN  SCAN1; PASSCOMMENT;
			SPRINT; TERPRI;
			END
		ELSE SCAN1;
		END;
	IF DOINDENT THEN UNDENT(BLKIND) ELSE TERPRI;
	PRINT1; SPRINT; SCAN1; PASSCOMMENT;
	CHECKSEMI;
	TERPRI;
	END;

COMMENT  -- NULL STATEMENT NOT ENDING IN SEMICOLON;

	BEGIN  "NULL"
	STRING TSA;
	TSA_STR1A; STR1A_"  "; SPAC1_0;
	PRINT1;STR1A_TSA; SPAC1_1;
	NTYPE_1; SPRINT;
	SEMICOL_0;
	END;

                                                                                  COMMENT END OF THE VARIOUS STATEMENT ROUTINES;

		END;
	END "SCANSTMT";

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                COMMENT THE REST OF THE MAIN PROGRAM;

LABEL MLO; COMMENT PUT THERE TO LOCATE LOOP IN DDT;

	NFIL_NKNT_IKNT_0;

COMMENT NOW READ IN THE COUNTER FILE;

	WHILE TRUE DO
		BEGIN 
		ARRYIN(SRC,INP[1],4);
		IF SRCEOF THEN DONE;
		N_INP[4];
		N_-(N%262144);
		NAMES[NFIL_NFIL+1]_CVXSTR(INP[1]);
		STRT[NFIL]_NKNT_NKNT+1;
		IF NKNT+N>MXKTR THEN USERERR(0,0,
		  "TOO MANY COUNTERS, USE THE /#K SWITCH (YOU HAVE AT LEAST "&CVS(NKNT+N)&" COUNTERS)");
		ARRYIN(SRC,KOUNTR[NKNT],N);
		NKNT_NKNT+N-1;
		END;
	STRT[NFIL+1]_NKNT+1;
	RELEASE(SRC);


COMMENT NOW READ IN THE LIST FILES AND PRODUCE THE 
 PROFILES.  THE LIST FILE NAMES ARE FOUND IN THE COUNTER
 BLOCK HEADERS WRITTEN OUT TO THE DISK AFTER EXECUTION;

	OPEN(SRC,"DSK",0,2,0,SOURCECOUNT,SRCBRK,SRCEOF);
MLO:	FOR IFIL_1 STEP 1 UNTIL NFIL DO
		BEGIN "SRCLOOP"
		IKNT_STRT[IFIL]-1;
		FL_-1; LOOKUP(SRC,NAMES[IFIL]&".LST",FL);
		IF FL THEN USERERR(0,0,"CAN'T FIND FILE-"&NAMES[IFIL]&".LST");
		HEADNG_HEAD1&NAMES[IFIL];
		OUT(OFILE,'14&'15&HEADNG&'15&'12&'12);
		NLINES_0;
		FIRSTSCAN_1; SCAN1;
		FIRSTSCAN_0; SCAN1;
		PASSCOMMENT;
		IF EQU(STR1,"ENTRY") THEN 
			BEGIN CURCOUNT_0; NTYPE_0;
			WHILE TYP1";" DO 
				BEGIN PRINT1; SCAN1 END;
			PRINT1;SPACO_1; SCAN1;
			SPRINT;
			END
		ELSE CURCOUNT_1;
		TERPRI;
		SCANSTMT(0);
		TERPRI;
		CLOSE(SRC);
		IF IKNT(I_STRT[IFIL+1]-1) THEN
			USERERR(0,1,CVS(ABS(IKNT-I))&(IF IKNT<I THEN
 			  " TOO FEW" ELSE " TOO MANY")&" COUNTERS FOUND FOR-"&
			  NAMES[IFIL]);
		END "SRCLOOP";
	RELEASE(SRC);
	RELEASE(OFILE);
	IF STPFLG THEN DONE;
END "KNTLOOP";
	END "SUPERLOOP";
OUTSTR("
THAT'S ALL FOLKS
");
END "PROFILE";
                                                                                                                                                                                                                                                     u,[?