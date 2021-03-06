0002?	
  0003?	%DATE OF LAST SYSTEM UPDATE;
    0004?	
  0005?	DATE!* := "20-MAY-72";
0006?	
  0007?	%R E D U C E PREPROCESSOR FOR P D P 1 0;
  0008?	
  0009?	LISP PROCEDURE NEWFORM U;
  0010?	   DEFLIST(U,'NEWFORM);
    0011?	
  0012?	LISP PROCEDURE NEWNAM U;
   0013?	   DEFLIST(U,'NEWNAM);
0014?	
  0015?	LISP PROCEDURE LOSE U;
0016?	   MAP(U,FUNCTION (LAMBDA (J);
  0017?		 PUT(CAR J,'LOSE,T)));
    0018?	
  0019?	LISP PROCEDURE DEFLIST(L,V);
    0020?	   IF NULL L THEN NIL
 0020?	    ELSE PROG2(PUT(CAAR L,V,CADAR L),CAAR L) . DEFLIST(CDR L,V);
   0022?	
  0023?	NEWNAM ('(
  0025?		(DIGIT NUMBERP)
           0026?		(ERROR ERR)
0027?		(EXPLODE EXPLODEC)
   0028?		(FLAGP GET)
0029?		(GETEL EVAL)
    0030?		(GTS EVAL)
 0031?		(NCONS NCONSX)
  0032?		(PTS SET)
  0033?		(REMOVE REMOVEX)
0033?		(STRINGP ATOM)
  0034?		(*APPLY APPLY)
  0034?		(*EVAL EVAL)
    0035?		(**BLANK (QUOTE /  ))
0035?		(**DASH (QUOTE /-))
  0035?		(**DOLLAR (QUOTE /$))
0035?		(**EOF (QUOTE $EOF$))
0035?		(**ESC (QUOTE /#))
   0035?		(**FMARK (QUOTE /&))
 0035?		(**QMARK (QUOTE /'))
 0035?		(**XMARK (QUOTE /!))
 0035?		(**PLUSS (QUOTE /+))
 0036?	));
    0036?	
  0037?	NEWFORM ('(
 0038?		(ERRORSET (LAMBDA (U V) (LIST (QUOTE ERRSET)
            0039?			(LIST (QUOTE EVAL) U) V)))
    0040?		(EQS (LAMBDA (U V) (LIST (QUOTE EQ) U (LIST (QUOTE QUOTE)
    0041?			(CAR (EXPLODEC (CADR V)))))))
 0041?		(FUNCTION (LAMBDA (U) (LIST 
   0041?		   (COND ((NULL *DEFN) (QUOTE *FUNCTION)) (T (QUOTE FUNCTION)))
   0041?		   U)))
    0041?		(GCD (LAMBDA (U) (LIST (QUOTE GCD*) U)))
 0042?		(MAP (LAMBDA (U V) (LIST (QUOTE MAP) V U)))
   0043?		(MAPLIST (LAMBDA (U V) (LIST (QUOTE MAPLIST) V U)))
0044?		(MAPCAR (LAMBDA (U V) (LIST (QUOTE MAPCAR) V U)))
  0046?		(PUT (LAMBDA (U V W) (LIST (QUOTE PUTPROP) U W V)))
0047?		(ONEP (LAMBDA (U) (LIST (QUOTE EQUAL) U 1)))
                 0048?	));
    0049?	
  0050?	%The following definition does not allow for compilation of functions
   0051?	%during REDUCE execution;
  0052?	
  0053?	LISP PROCEDURE DEF!*(NAME,VARLIS,BODY,TYPE);
   0054?	   PUT(NAME,TYPE,LIST('LAMBDA,VARLIS,BODY));
   0055?	
  0070?	%LISTING OF SPECIAL VARIABLES;
  0071?	
  0072?	%THE FOLLOWING ARE EXTENDED VARIABLES IN REDUCE;
    0073?	
  0074?	%SPECIAL '(*S* *S1*);
 0075?	
  0075?	%THE FOLLOWING VARIABLE IS USED AS A FUNCTIONAL ARGUMENT;
0075?	
  0075?	%SPECIAL '(*PI*);
0075?	
  0075?	
  0076?	%STANDARD LISP FUNCTIONS NOT DEFINED IN LISP 1.6;
   0077?	
                      0078?	LISP PROCEDURE COMPRESS U;
 0079?	   IF NUMBERP CAR U THEN MAKNAM U ELSE READLIST U;
  0080?	
  0084?	LISP PROCEDURE FLAG(L,!*S!*);
   0085?	   MAP(L,FUNCTION (LAMBDA (J); PUT(CAR J,!*S!*,T)));
0087?	
  0088?	LISP PROCEDURE REMFLAG(L,!*S!*);
0089?	   MAP(L,FUNCTION (LAMBDA (J); REMPROP(CAR J,!*S!*)));
   0091?	
  0092?	LISP PROCEDURE GETD U;
0093?	   (LAMBDA X; IF X AND NOT GET(U,'**ARRAY) THEN CADR X ELSE NIL) 
  0095?	      GETL(U,'(EXPR FEXPR SUBR FSUBR LEXPR LSUBR MACRO));
0096?	
  0097?	LISP PROCEDURE MAPCON(X,!*PI!*);
0098?	   IF NULL X THEN NIL ELSE NCONC(!*PI!* X,MAPCON(CDR X,!*PI!*));
   0099?	
       0100?	LISP PROCEDURE FIXP N;
0101?	   NUMBERP N AND (N EQ 0 + N) OR NOT (CADR N EQ 'FLONUM);
0102?	
  0120?	LISP PROCEDURE SPACES N;
   0121?	   IF ZEROP N THEN NIL ELSE PROG2(PRINC " ",SPACES SUB1 N);
   0123?	
  0124?	LISP PROCEDURE SUBLIS(U,V);
0124?	   BEGIN SCALAR X;
    0125?		IF NULL U THEN RETURN V;
  0125?		X := U;
    0126?	  A:	IF NULL X THEN RETURN IF ATOM V
 0126?			   OR (X := SUBLIS(U,CAR V) . SUBLIS(U,CDR V)) = V
0127?			 THEN V
   0127?			ELSE X
    0128?		 ELSE IF V = CAAR X THEN RETURN CDAR X;
  0128?		X := CDR X;
0129?		GO TO A
    0129?	  END;
 0130?	
  0131?	LISP PROCEDURE PAIR(X,Y);
       0132?	   IF NULL X AND NULL Y THEN NIL
0133?	    ELSE IF NULL X OR NULL Y
    0134?	       THEN REDERR LIST("MISMATCH OF ARGUMENTS",X,Y)
0135?	    ELSE (CAR X . CAR Y) . PAIR(CDR X,CDR Y);
  0136?	
  0137?	LISP PROCEDURE /N;
    0138?	   QUOTIENT(1,N);
0139?	
  0140?	LISP PROCEDURE !*ARRAY U;
  0141?	   MAP(U,FUNCTION (LAMBDA J; EVAL('ARRAY . (CAAR J . (T . CDAR J)))));
  0144?	
  0145?	LISP PROCEDURE SETEL(U,V);
 0146?	   EVAL LIST('STORE,U,LIST('QUOTE,V));
    0147?	
  0148?	LISP PROCEDURE OPEN(U,V);
  0149?	   BEGIN
    0152?		 EVAL (V . (MKAT U . U));
 0152?		 IF V EQ 'INPUT  THEN IPL!* := U . IPL!*
           0152?		  ELSE OPL!* := U . OPL!*;
0153?		 RETURN U
  0154?	   END;
0155?	
  0156?	LISP PROCEDURE RDS U;
 0157?	   IF NULL U THEN INC(NIL,NIL)
  0158?	    ELSE IF U MEMBER IPL!* THEN INC(MKAT U,NIL)
0159?	    ELSE REDERR ("RDS GIVEN CLOSED FILE" . U);
 0160?	
  0161?	LISP PROCEDURE WRS U;
 0162?	   IF NULL U THEN OUTC(NIL,NIL)
 0163?	    ELSE IF U MEMBER OPL!* THEN OUTC(MKAT U,NIL)
    0164?	    ELSE REDERR ("WRS GIVEN CLOSED FILE" . U);
 0165?	
  0166?	LISP PROCEDURE CLOSE U;
    0167?	   IF NULL U THEN NIL
 0168?	    ELSE IF U MEMBER IPL!* THEN INC(NIL,T)
0169?	    ELSE IF U MEMBER OPL!*
                          0170?	       THEN OUTC(IF NOT (U=OFL!*) THEN OUTC(MKAT U,NIL) ELSE NIL,T)
0172?	    ELSE REDERR ("CLOSE GIVEN CLOSED FILE" . U);
    0173?	
  0174?	LISP PROCEDURE DELETE(U,V);
0175?	   IF NULL V THEN NIL
 0176?	    ELSE IF U=CAR V THEN CDR V
  0177?	    ELSE CAR V . DELETE(U,CDR V);
    0178?	
  0179?	LISP PROCEDURE MKAT U;
0180?	   COMPRESS DELETE(':,
0181?			   MAPCON(FLATTEN REVERSE FLATTEN U,
    0182?				  FUNCTION (LAMBDA (J); EXPLODE CAR J)));
   0184?	
  0185?	LISP PROCEDURE FLATTEN U;
  0186?	   IF NULL U THEN NIL
 0187?	    ELSE IF ATOM U THEN LIST U
                                                         0188?	    ELSE IF ATOM CAR U THEN CAR U . FLATTEN CDR U
   0189?	    ELSE NCONC(FLATTEN CAR U,FLATTEN CDR U);
   0190?	
  0190?	LOSE '(DELETE FLATTEN);
    0190?	
  0191?	%REDUCE FUNCTIONS WITH SYSTEM DEPENDENT PROPERTIES;
 0192?	
  0193?	LISP PROCEDURE TOKEN;
 0194?	   BEGIN SCALAR X;
    0195?		 SCANSET();
0195?		X := SCAN();
    0195?		SCANRESET();
    0196?		RETURN IF X=0 THEN INTERN SCNVAL
    0198?			ELSE IF X=1 THEN LIST('STRING,SCNVAL)
   0199?			ELSE IF X=2 THEN SCNVAL 
 0199?			ELSE IF SCNVAL = 39 THEN LIST('QUOTE,READ())
 0199?			ELSE INTERN ASCII SCNVAL
 0202?	   END;
0203?	
                           0204?	LISP PROCEDURE DELCP U;
    0205?	   U MEMBER '(; $ /);
0205?	
  0205?	LISP PROCEDURE LITER X;
    0205?	   NOT NUMBERP X AND
  0205?	   (X := LSH (MAKNUM(CAAR GET(X,'PNAME),'FIXNUM),-11))>64
0205?	    AND 91>X;
    0206?	
  0206?	LISP PROCEDURE MKVAR(U,V);
 0206?	   U;
  0206?	
  0206?	%LISP PROCEDURE READCH!*;
  0207?	%   (LAMBDA X; IF X MEMBER '(10 11 12 13) THEN READCH!*()
0207?	%		ELSE IF X>47 AND X<58 THEN X-48 
  0207?	%		ELSE INTERN ASCII X)
    0207?	%	TYI();
    0207?	
  0208?	%NEWNAM '((READCH READCH*));
    0208?	
  0208?	%LISP PROCEDURE MKSTRING U;
                                             0208?	%   MAKNAM MAPCON(U,FUNCTION (LAMBDA J; LIST ('//,CAR J)));
   0208?	
  0209?	%LISP PROCEDURE SEPRP U;
   0209?	%   U MEMBER '(/  /	 );
    0209?	
  0209?	
  0210?	LOSE '(TOKEN);
   0210?	
  0210?	LISP PROCEDURE OUTDEF(NAME,VARLIS,BODY,TYPE);
  0210?	   BEGIN
    0210?		TERPRI();
  0211?		PRINC "(DEFPROP ";
   0211?		PRINC NAME;
0213?		PRINC " ";
 0214?		TERPRI();
  0215?		SPRINT(LIST('LAMBDA,VARLIS,BODY),2,0);
   0216?		PRINC " ";
 0217?		TERPRI();
  0218?		SPRINT(TYPE,1,1);
    0219?		PRINC ")";
 0219?		TERPRI()
   0220?	   END;
0221?	
  0221?	LISP PROCEDURE DFPRINT U;
                                0221?	   BEGIN SPRINT(U,2,0); TERPRI(); TERPRI() END;
0221?	
  0221?	PUT('DEFN,'SIMPFG,'((T ED T)));
 0222?	
  0224?	%REDUCE FUNCTIONS HANDLING IO;
  0225?	
  0226?	LISP PROCEDURE REFG;
  0226?	   BEGIN SCALAR X;
    0226?		 X := ECHOL!*;
  0226?		 ECHOL!* := !*ECHO;
  0226?		 !*ECHO := X
    0227?	   END;
0227?	
  0232?	LISP PROCEDURE INOUT(U,V);
 0233?	   BEGIN SCALAR DEV,FL,!*S!*;
   0234?		DEV := 'DSK: ;
  0235?		ECHOL!* := !*ECHO;
   0237?	    A:  IF NULL U THEN GO TO D ELSE IF NOT DEVP CAR U THEN GO TO B;
0238?		DEV := CAR U;
   0239?		U := CDR U;
0240?		GO TO A;
                                           0241?	    B:  IF V EQ 'OUTPUT  THEN GO TO C;
    0242?		FL := LIST(DEV,MKFIL CAR U);
   0243?		IF FL MEMBER IPL!* THEN GO TO B1;
   0244?		OPEN(FL,V);
0245?	    B1: RDS (IFL!* := FL);
 0245?		IF NOT ATOM CADR FL AND CDADR FL MEMBER '(LSP LAP)
 0245?		 THEN GO TO L;
  0246?		!*ECHO := IECHO!*;
   0248?		BEGIN1();
  0249?	    B2:	U := CDR U;
   0250?		GO TO A;
   0251?	    C:  IF (CAR U EQ 'L) OR (DEV EQ 'LPT:) THEN FL := '(LPT:)
 0253?		 ELSE IF CAR U EQ 'T  THEN GO TO E
  0254?		 ELSE FL := LIST(DEV,MKFIL CAR U);
  0255?		IF FL MEMBER OPL!* THEN GO TO C1;
   0256?		OPEN(FL,V);
0257?	    C1: WRS (OFL!* := FL);
      0257?		LINELENGTH 68;
  0258?	    D:  IF V EQ 'INPUT  THEN REFG();
 0259?		RETURN NIL;
0260?	    E:  OFL!* := NIL;
 0261?		WRS NIL;
   0261?	    L:	IF CDADR FL EQ 'LAP THEN IBASE := 8;
    0261?	    L1:	!*S!* := ERRSET(READ(),T);
   0261?		IF ATOM !*S!* OR CDR !*S!* THEN GO TO L2;
0261?		!*S!* := ERRSET(EVAL CAR !*S!*,T);
  0261?		IF ATOM !*S!* OR CDR !*S!* THEN GO TO L2;
0261?		PRINT CAR !*S!*;
0261?		TERPRI();
  0261?		GO TO L1;
  0262?	    L2:	IBASE := 10;
  0262?		CLOSE FL;
  0262?		IF !*S!* EQ !*!*EOF THEN GO TO B2
   0262?		 ELSE REDERR "ERROR TERMINATION"
    0262?	   END;
0263?	
                           0264?	LISP PROCEDURE IN U;
  0265?	   INOUT(U,'INPUT);
   0266?	
  0267?	LISP PROCEDURE OUT U;
 0268?	   INOUT(U,'OUTPUT);
  0269?	
  0274?	LISP PROCEDURE SHUT U;
0275?	   BEGIN SCALAR X;
    0276?	    A:  IF NULL U THEN RETURN NIL
    0277?		ELSE IF DEVP CAR U THEN GO TO D
0278?		ELSE IF CAR U EQ 'L  THEN X := '(LPT:)
   0279?		ELSE X := LIST('DSK:,MKFIL CAR U);
  0280?	    A1: IF X MEMBER OPL!* THEN GO TO B
    0281?		ELSE IF NOT X MEMBER IPL!*
0282?		   THEN REDERR LIST(X,"NOT OPEN");
  0283?		CLOSE X;
   0284?		IPL!* := DELETE(X,IPL!*);
 0285?		IF NOT (X=IFL!*) THEN GO TO C;
                                    0286?		RDS (IFL!* := (IF IPL!* THEN CAR IPL!* ELSE NIL));
 0287?		GO TO C;
   0288?	    B:  CLOSE X;
 0289?		OPL!* := DELETE(X,OPL!*);
 0290?		IF NOT (X=OFL!*) THEN GO TO C;
 0291?		OFL!* := NIL;
   0292?		WRS NIL;
   0293?	    C:  U := CDR U;
   0294?		GO TO A;
   0295?	    D:  IF NULL CDR U OR CDDR U THEN GO TO ERR;
0296?		X := LIST(CAR U,CADR U);
  0297?		U := CDR U;
0298?		GO TO A1;
  0299?	    ERR:REDERR LIST("CLOSE FORMAT",U)
0300?	   END;
0301?	
  0302?	LISP PROCEDURE DEVP U;
0303?	   IF ATOM U THEN CAR REVERSE EXPLODE U EQ ':
  0304?	    ELSE NOT (CAR U EQ 'CONS);
  0305?	
                                0306?	LISP PROCEDURE MKFIL U;
    0307?	   IF NUMBERP CAR U THEN U
 0308?	    ELSE IF CAR U EQ 'CONS THEN CADR U . CADDR U
    0309?	    ELSE REDERR "FILE FORMAT" ;
 0309?	
  0309?	OPL!* := NIL;
    0309?	
  0309?	
  0316?	%REDUCE FUNCTIONS HANDLING INTERACTIVE FEATURES;
    0317?	
  0318?	LISP PROCEDURE PAUSE;
 0318?	   BEGIN 
   0319?		IF NULL IFL!* THEN RETURN NIL
  0319?		 ELSE IF NULL ERFG!* OR NULL CLOC!* THEN GO TO C
   0320?		 ELSE IF YESP 'EDIT?  THEN GO TO A
  0320?		 ELSE IF NULL FLG!* THEN GO TO C;
   0321?		FLG!* := SOS!* := NIL;
    0322?		!*APPLY('SHUT,LIST IFL!*);
0322?		RETURN NIL;
               0323?	    A:	CONTL!* := NIL;
0323?		IF NULL OFL!* THEN GO TO B;
    0324?		LPRIM LIST(OFL!*,"SHUT");
 0324?		!*APPLY('SHUT,LIST OFL!*);
0325?	    B:	RETURN EDIT1(CLOC!*,NIL);
0325?	    C:   IF YESP 'CONT?  THEN RETURN NIL;
 0326?		REFG();
    0326?		CONTL!* := IFL!* . CONTL!*;
    0327?		IFL!* := NIL;
   0327?		IPL!* := CDR IPL!*;
  0327?		RDS NIL
    0328?	   END;
0328?	
  0334?	LISP PROCEDURE YESP U;
0335?	   BEGIN SCALAR X,Y;
  0336?		IF IFL!* THEN RDS NIL;
    0337?		IF OFL!* THEN WRS NIL;
    0338?		IF ATOM U THEN PRINC U ELSE LPRI U;
 0339?		TERPRI();
  0340?	    A:  X := READ();
  0341?		TERPRI();
       0342?		IF ((X EQ 'Y) AND (Y := T)) OR (X EQ 'N) THEN GO TO B;
  0343?		PRINC "TYPE Y OR N";
 0344?		GO TO A;
   0345?	    B:  IF OFL!* THEN WRS OFL!*;
0346?		IF IFL!* THEN RDS IFL!*;
  0347?		CURSYM!* := '*SEMICOL* ;
  0348?		RETURN Y
   0349?	   END;
0350?	
  0361?	LISP PROCEDURE CONT;
  0361?	   BEGIN
    0361?		IF NULL CONTL!* THEN REDERR "NO FILE OPEN";
   0361?		REFG();
    0361?		IFL!* := CAR CONTL!*;
0362?		CONTL!* := CDR CONTL!*;
   0362?		IPL!* := IFL!* . IPL!*;
   0362?		RDS IFL!*
  0362?	   END;
0363?	
  0363?	FLAG ('(CONT),'IGNORE);
    0363?	
  0364?	LISP PROCEDURE PRINTTY U;
                      0365?	   BEGIN
    0366?		IF NOT !*FORT AND !*NAT THEN PRINT U;
    0367?		IF NULL OFL!* THEN RETURN NIL;
 0368?		OUTC(NIL,NIL);
  0369?		PRINT U;
   0370?		OUTC(MKAT OFL!*,T)
   0371?	   END;
0372?	
  0373?	LISP PROCEDURE REDMSG1(U,V);
    0374?	   YESP LIST('DECLARE,U,V,"? (Y/N)");
0375?	
  0376?	DEFLIST ('((PAUSE ENDSTAT) (CONT ENDSTAT)),'STAT);
  0377?	
  0378?	ECHOL!* := NIL;
  0379?	
  0382?	%FUNCTIONS FOR TIMING EXECUTION;
0383?	
  0384?	LISP PROCEDURE STIME U;
    0385?	   BEGIN SCALAR X;
    0386?		 X := GTS U;
    0387?		 PTS(U,!*EVAL '(TIME NIL));
    0387?		TERPRI();
                                0388?		 PRINC !*DIF(GTS U,X);
    0389?		 PRINC " ";
0390?		 PRINC 'MS ;
    0391?		 TERPRI()
  0392?	   END;
0393?	
  0394?	LISP PROCEDURE TIMSTAT;
    0395?	   PROG2(SCAN!*(),'(STIME (QUOTE TIME2*)));
    0396?	
  0397?	DEFLIST ('((TIME TIMSTAT)),'STAT);
   0398?	
  0399?	FLAG ('(STIME),'DIRECT);
   0400?	
  0401?	%INTRODUCTION OF SPECIAL CHARACTER STRINGS;
    0402?	
  0403?	SWITCH!* := '(
   0404?		($ NIL *SEMICOL* NIL)
0405?		(/] NIL *SEMICOL* NIL)
    0406?		(; NIL *SEMICOL* NIL)
0407?		(/+ NIL PLUS NIL / +/ )
   0408?		(/- NIL DIFFERENCE NIL / /-/ )
 0410?		(* * TIMES EXPT)
                              0411?		(// NIL QUOTIENT NIL)
0412?		(= NIL EQUAL NIL)
    0413?		(/, NIL *COMMA* NIL)
 0414?		(/( NIL *LPAR* NIL)
  0415?		(/) NIL *RPAR* NIL)
  0418?		(: = *COLON* SETQ NIL / /:/=/ )
0420?		(/. NIL CONS NIL)
    0423?		(< = LESSP LEQ)
 0424?		(> = GREATERP GEQ)
   0425?	);
0426?	
  0426?	%CHARACTERS PECULIAR TO THE PDP-10 IMPLEMENTATION;
  0426?	
  0426?	NEWNAM '(
   0426?		( AND)
    0427?		( OR)
0427?		( NOT)
    0427?		( NEQ)
    0427?		(^ EXPT)
   0427?		( MEMBER)
 0428?		(_ SETQ)
   0428?		( EQ)
0428?		( LAMBDA)
 0428?	);
0428?	
                                                                   0429?	(LAMBDA (); SWITCH!* := SUBST(INTERN ASCII 125,'/],SWITCH!*)) ();
  0430?	
  0431?	%FUNCTION DEFINITIONS TO BE 'LOST';
  0432?	
  0433?	LOSE '(ABS ASSOC);
    0435?	
  0436?	%DEFINITION OF BEGIN;
 0437?	
  0438?	LISP PROCEDURE BEGIN;
 0439?	   BEGIN
    0440?		TIME1!* := TIME2!* := !*EVAL '(TIME NIL);
0442?		!*INT := T;
0443?		!*ECHO := NIL;
  0447?		CONTL!* :=  IFL!* := IPL!* := OFL!* := OPL!* := NIL;
    0448?		IF DATE!* EQ NIL THEN GO TO A;
 0449?		PRINC "REDUCE 2 (";
  0450?		PRINC DATE!*;
   0451?		PRINC ") ...";
  0452?		TERPRI();
  0453?		DATE!* := NIL;
  0454?	  A:	!*MODE := IMODE!*;
              0455?		BEGIN1();
  0455?		MAPCAR(CONTL!*,FUNCTION CLOSE);
0456?		PRINC "ENTERING LISP...";
 0457?		TERPRI()
   0458?	   END;
0459?	
  0460?	%DEFINITION OF INITL;
 0461?	
  0462?	DEFLIST ('(
 0463?	(INITL (LAMBDA NIL
    0464?	 (PROG (X)
  0465?		(COND ((NOT *NEW) (GO A)))
0466?		(GETSYM SUBR SCANINIT LETTER IGNORE SCAN SCANSET SCANRESET)
  0467?		(SCANINIT 37 13 34 34 33)
 0468?		(IGNORE 9)
 0469?		(IGNORE 10)
0470?		(IGNORE 12)
0471?		(IGNORE 13)
0472?		(IGNORE 32)
0473?		(SETQ SCNVAL NIL)
    0474?		(SETQ X (MAKNUM (GET (QUOTE ACHLOC) (QUOTE SYM))
   0475?			(QUOTE FIXNUM)))
                                  0476?		(DEPOSIT X (PLUS (EXAMINE X) -3))
   0477?	  A	(SETQ KLIST NIL)
  0480?		(SETQ BASE 10)
  0481?		(SETQ IBASE 10)
 0482?		(SETQ LLENGTH* 67)
   0483?		(SETQ *NOPOINT T)
    0484?		(DDTIN NIL)
0485?		(NOUUO NIL)
0485?		(BAKGAG NIL)
    0485?		(SETQ IECHO* T)
 0485?		(REMPROP (QUOTE DF) (QUOTE FEXPR))
  0486?		(OUTC NIL T)
    0488?		(REMPROP @INITL @EXPR)
    0489?		(COND ((GET (QUOTE APNINIT) (QUOTE EXPR)) (APNINIT)))
   0490?		(EXCISE)
   0491?		(COND (*NEW (INITFN @BEGIN)))
  0492?		(RETURN (QUOTE ***)))))
   0493?	
  0496?	), 'EXPR);
  0497?	
                                                              0498?	DEFLIST ('((IN RLIS) (OUT RLIS) (SHUT RLIS)),'STAT);
0500?	
  0501?	DEFLIST ('((RETRY ENDSTAT)),'STAT);
  0502?	
  0503?	(LAMBDA (); !*NEW AND PUTSYM SCNVAL GET('SCNVAL,'VALUE)) ();
  0504?	
  0505?	DEFLIST ('(
 0506?	(CORSTAT (LAMBDA NIL
  0507?	   (PROG NIL
0508?		(CORE (XREAD NIL))
   0509?		 (ALLOC))))
0510?	),'EXPR);
   0511?	
  0512?	DEFLIST ('((CORE CORSTAT)),'STAT);
   0513?	
  0514?	%Definition of ORDERP in LAP;
   0515?	
  0516?	LAP(ORDERP,SUBR);
0517?	" "(104960,1,2);
 0518?	" "(112640,1,C 0);
    0519?	MOVEI(1,'T);
0520?	POPJ P;
0521?	NIL;
   0522?	
  0523?	
                                0524?	%THE FOLLOWING RENAMING IS NECESSARY IN ALGEBRAIC MODE;
  0525?	
  0526?	NEWNAM '((LCOEFF CDAR)
0527?		(LDEG CDAAR)
    0528?		(MVAR CAAAR)
    0529?	   );
  0530?	
  0531?	
  0600?	END OF P D P R E D U C E PREPROCESSOR;
                                                                                                                                                                                                                                                                                                                                                                                                                    