0010?	LET PROGRAM (X) = {{REP 0 M * {[IF NEXT('EOF) THEN FAILURE()]  <expression>  ?#  ?;
  0020?				[BEGIN  NEW X;
0030?				 IF X _ PREVIOUS(3) THEN PRINT EVAL X
  0040?					ALSO IF ?!PCOD THEN
    0050?						IF ATOM X & X[1] EQ 'DEFPROP & X[4]  '(EXPR FEXPR LEXPR MACRO) THEN
   0060?							?!MISCELLANEOUS{NIL~ _ X[2] CONS ?!MISCELLANEOUS
   0070?						ELSE ?!MISCELLANEOUS{NIL~ _ X CONS ?!MISCELLANEOUS;
 0080?				 TOKEN();				% SKIP OVER THE ; %
  0090?				 FLUSH();
0100?				 END] ~~
 0110?			?#  EOF ~
    0120?		MEAN NIL;
  0130?	
  0140?	
                                                                        0150?	LET expression (X) = {{ALT LET  [IDENTIFIER]  ?(  {REP 1 M * {{ALT {OPT SPECIAL~ [IDENTIFIER] | ?*~~ ?,~  ?)
    0160?					{OPT [IDENTIFIER]~  ?=  ?{  <*pattern>  ?~  MEAN  <expression>
 0170?				|  ?#  ?  [SEXPRESSION] ~~
  0180?		MEAN
  0190?		IF X[1] EQ 1 THEN			% A PRODUCTION DEFINED USING "LET". %
    0200?			BEGIN  NEW ?!PROD, ?!PROD?#, ?!CODE, ?!PC, ?!LAST, LOC, CONLIST, GEN, REMOB;
    0210?			SPECIAL    ?!PROD, ?!PROD?#, ?!CODE, ?!PC, ?!LAST, LOC, CONLIST, GEN, REMOB, ?!PCOD;
 0220?			NEW ARGS, NARGS, NITEMS, PUSHLIST;
 0230?			?!PROD   _ X[3];				% THE NAME OF THE PRODUCTION. %
                             0240?			?!PROD?# _ SYNAM(?!PROD, ?!PROD.?!PROD?#);	% THE NAME OF ITS SYNTAX ROUTINE. %
  0250?			NARGS    _ 0;					% # OF ARGUMENTS TO THE SEMANTICS ROUTINE. %
   0260?			FOR NEW I IN X[5] FOR NITEMS_1 TO 1000 DO
    0270?				IF I[1,1] EQ 1 THEN
0280?					BEGIN
   0290?					ARGS _ I[1,3] CONS ARGS;
    0300?					NARGS _ NARGS+1;
  0310?					PUSHLIST _ T CONS PUSHLIST;
 0320?					IF I[1,2] & (I[1,3]?!SPECIALS) THEN ?!SPECIALS{NIL~ _ I[1,3] CONS ?!SPECIALS;
    0330?					END
0340?				ELSE PUSHLIST _ NIL CONS PUSHLIST;
0350?			PUTPROP(?!PROD, NARGS, 'ARGUMENTS);
                                                            0360?			IF ?!PROD?#.SUBR THEN WARNING("PRODUCTION REDEFINED", ?!PROD)
    0370?				ALSO REMPROP(?!PROD, 'PREDS)		% MAKE SURE EARLIER DEF LEFT NO PREDICATES. %
    0380?			ELSE IF GETL(?!PROD, '(EXPR FEXPR LEXPR SUBR FSUBR LSUBR MACRO)) THEN
 0390?				WARNING("FUNCTION REDEFINED", ?!PROD);
 0400?			LAPST(?!PROD?#);				% EMIT THE SYNTAX CODE. %
0410?			EPAT(X[10], NARGS, NITEMS, REVERSE PUSHLIST, NIL);
0420?			LAPFN(?!PROD?#);
    0430?			PUTPROP(?!PROD, <'LAMBDA, REVERSE ARGS, X[13]>, 'EXPR);		% EMIT THE SEMANTICS CODE. %
0440?			FIXUP(?!PROD, ?!PROD?#);
                                                                       0450?			IF X _ X[7] THEN ADALT(?!PROD, ?!PROD?#, X[1], SYNAM(X[1], X[1].?!PROD?#));
0460?			IF ?!PCOD THEN PUTPROP(?!PROD, ?!CODE, 'CODE);
    0470?			PRINTTY(?!PROD);
    0480?			END
  0490?		ELSE IF X[1] EQ 2 THEN X[4]				% AN S-EXPRESSION. %
0500?		ELSE X[2];						% OTHERWISE, AN ADDED EXPRESSION. %
0510?	
  0520?	
  0530?	LET pattern (L) = {{REP 0 M * {
 0540?			{ALT [IDENTIFIER]
   0550?			  |  ?<  {OPT ?*~  [IDENTIFIER]  ?>
0560?			  |  ?{  {ALT REP  [NUMBER]  {ALT [NUMBER] | M~  {OPT ?*~  ?{  <*pattern>  ?~ {OPT <*pattern>~
 0570?				   |  OPT  <*pattern>
                                                          0580?				   |  ALT  {REP 0 M * {<*pattern>~ ?|~~  ?~
 0590?			  |  ?[  <expression>  ?]
0600?			  |  [NUMBER] ~~~~
  0610?		MEAN L;
        ?  0010?	(DEFPROP BPORG           T SPECIAL);
0020?	(DEFPROP BPEND1          T SPECIAL);
0030?	(DEFPROP IBASE           T SPECIAL);
0040?	(DEFPROP ?!MISCELLANEOUS T SPECIAL);
0050?	(DEFPROP ?!SPECIALS      T SPECIAL);
0060?	(DEFPROP ?!SCANVAL       T SPECIAL);
0070?	(DEFPROP ?&SPECLIST      T SPECIAL);
0080?	
  0090?	
  0100?	(DEFPROP PREFIX
 0110?	 (LAMBDA NIL (COND ((PROPERTY (QUOTE ?&PREFIX)) (CAR (TOKEN))) (T (ISPROP (PROPERTY (QUOTE ?&PABB))))))
                   0120?	EXPR);
 0130?	
  0140?	
  0150?	(DEFPROP INFIX
  0160?	 (LAMBDA NIL
0170?	  (COND ((OR (AND (ISIDENTIFIER) (NOT (PROPERTY (QUOTE LITERAL)))) (PROPERTY (QUOTE ?&INFIX))) (CAR (TOKEN)))
   0180?		(T (FAILURE))))
 0190?	EXPR);
 0200?	
  0210?	
  0220?	
  0230?	
  0240?	(DEFPROP HIER
   0250?	 (LAMBDA (L RBP)
 0260?	  (COND ((OR (NULL (CDR L)) (GEQUAL RBP (BP (CAADR L) (QUOTE ?&LEFT)))) L)
   0270?		(T (HIER (CONS (TFN (TRANS (CAADR L))
    0280?				    (CAR L)
   0290?				    (CAR (SETQ L (HIER (CONS (CADADR L) (CDDR L)) (BP (CAADR L) (QUOTE ?&RIGHT))))))
0300?			       (CDR L))
0310?			 RBP))))
            0320?	EXPR);
 0330?	
  0340?	
  0350?	(DEFPROP TFN
    0360?	 (LAMBDA (FN X Y)
0370?	  (COND ((EQ FN (QUOTE ?&ARROW)) (?&ARROW (LIST FN X Y)))
0380?		(T (LIST FN X Y))))
  0390?	EXPR);
 0400?	
  0410?	
  0420?	(DEFPROP CONTEXTVAR
  0430?	 (LAMBDA (X)
0440?	  (COND ((ATOM X) X)
  0450?		((EQ (CAR X) (QUOTE SETQ)) (CONS (LIST (QUOTE QUOTE) (CADR X)) (QUOTE (QUOTE VALUE))))
0460?		((EQ (CAR X) (QUOTE PUT)) (CONS (CADR X) (CADDDR X)))
   0470?		(T (ERROR (CAT (QUOTE "CANNOT SET CONTEXT OF ") X)))))
  0480?	EXPR);
 0490?	
  0500?	
                                                                                       0510?	(DEFPROP ID (LAMBDA NIL (COND ((AND (ISIDENTIFIER) (NOT (PROPERTY (QUOTE LITERAL)))) (CAR (TOKEN))))) EXPR);
   0520?	
  0530?	(DEFPROP BP (LAMBDA (X IND) (COND ((GET X IND)) (T (GET (QUOTE DEFAULT) IND)))) EXPR);
0540?	
  0550?	(DEFPROP TRANS (LAMBDA (X) (COND ((GET X (QUOTE ?&INFIX))) (T X))) EXPR);
   0560?	
  0570?	(DEFPROP TIND (LAMBDA (L) (COND ((EQ (CAR L) 1) (LIST (QUOTE QUOTE) (CADR L))) (T (CADR L)))) EXPR);
 0580?	
  0590?	(DEFPROP ISPROP (LAMBDA (P) (COND (P (TOKEN) P) (T (FAILURE)))) EXPR);
 0600?	
  0610?	(DEFPROP LITERAL (LAMBDA NIL (OR (PROPERTY (QUOTE LITERAL)) (FAILURE))) EXPR);
             0620?	
  0630?	
  0640?	(MAPCAR (FUNCTION (LAMBDA (X) (PROG2 (MAPCAR (FUNCTION (LAMBDA (Y)
0650?		(COND ((ATOM Y) (PUTPROP Y T (CAR X))) (T (PUTPROP (CAR Y) (CDR Y) (CAR X)))))) (CDR X)) NIL)))
 0660?	
  0670?		(QUOTE ((?&FUNCTION	(FUNCTION.EXPR) (MACRO.MACRO))
 0680?	
  0690?			(LITERAL	FUNCTION MACRO)
 0700?	
  0710?			(?&PREFIX	STR STRP STRLEN AT PRINTSTR PLUS
   0720?					CAR CDR CAAR CADR CDAR CDDR CAAAR CAADR CADAR CDAAR CADDR CDADR CDDAR CDDDR
   0730?					CAAAAR CAAADR CAADAR CADAAR CDAAAR CAADDR CADADR CDAADR CADDAR CDADAR CDDAAR
  0740?					CADDDR CDADDR CDDADR CDDDAR CDDDDR
                                  0750?					ABS ADD1 ARG ASCII ATOM BAKGAG CSYM DDTIN ED ERR EVAL ?*EVAL EXAMINE EXPLODE
  0760?					EXPLODEC FIX FLATSIZE FUNCTION ?*FUNCTION GCGAG ?*GETSYM GO INITFN INTERN
0770?					LAST LENGTH LINELENGTH MAKNAM MINUS MINUSP NCONS NOT NOUUO NULL NUMBERP NUMVAL
0780?					PRINC PRINT PRIN1 QUOTE READLIST RETURN REVERSE ?*RSET SUB1 TERPRI TYO ZEROP)
 0790?	
  0800?			(?&PABB		(?.NOT) (?+.PLUS) (?-.MINUS))
 0810?	
  0820?			(?&INFIX	(?*.TIMES) (?/.QUOTIENT) (?+.PLUS) (?-.DIFFERENCE) (?^.PRELIST) (?.SUFLIST)
0830?					(?@.APPEND) (?=.EQUAL) (?.NEQUAL) (?.LEQUAL) (?.GEQUAL)
                                                  0840?					(?.MEMBER) (?&.AND) (?.AND) (?|.OR) (?.OR) (?_.?&ARROW))
    0850?	
  0860?			(?&LEFT		(?&ARROW. 800) (TIMES. 700) (QUOTIENT. 700) (PLUS. 600) (DIFFERENCE. 600)
   0870?					(DEFAULT. 500) (APPEND. 450) (NCONC. 450) (CONS. 450) (XCONS. 450) (CAT. 450)
 0880?					(EQ. 300) (NEQ. 300) (EQUAL. 300) (NEQUAL. 300) (LEQUAL. 300) (GEQUAL. 300)
   0890?					(LESSP. 300) (GREATERP. 300) (MEMBER. 300) (MEMQ. 300) (AND. 200) (OR. 100)
   0900?					(?_. 800) (?*. 700) (?/. 700) (?+. 600) (?-. 600) (?@. 450) (?=. 300) (?. 300)
    0910?					(?. 300) (?. 300) (?. 300) (?&. 200) (?. 200) (?|. 100) (?. 100))
             0920?	
  0930?			(?&RIGHT	(TIMES. 750) (QUOTIENT. 750) (PLUS. 650) (DIFFERENCE. 650) (DEFAULT. 550)
   0940?					(APPEND. 400) (NCONC. 400) (CONS. 400) (XCONS. 400) (CAT. 400)
 0950?					(EQ. 350) (NEQ. 350) (EQUAL. 350) (NEQUAL. 350) (LEQUAL. 350) (GEQUAL. 350)
   0960?					(LESSP. 350) (GREATERP. 350) (MEMBER. 350) (MEMQ. 350) (AND. 250) (OR. 150)
   0970?					(?*. 750) (?/. 750) (?+. 650) (?-. 650) (?@. 400) (?=. 350) (?. 350)
    0980?					(?. 350) (?. 350) (?. 350) (?&. 250) (?. 250) (?|. 150) (?. 150)
    0990?					(?&ARROW. 0) (?_. 0))
  1000?	
  1010?			(?&ASSOC	PLUS TIMES AND OR)
   1020?	
                 1030?			(?&ACCESS	(PUBLIC . PUBLICDECLARATION) (PRIVATE . PRIVATEDECLARATION)))));
    1040?	
  1050?	
  1060?	EXPR PUBLICDECLARATION (VARLIST);  PROG2(?&SPECLIST{NIL~ _ VARLIST @ ?&SPECLIST, VARLIST);
 1070?	
  1080?	
  1090?	EXPR PRIVATEDECLARATION (VARLIST);  VARLIST;
  1100?	
  1110?	
  1120?	EXPR ?&ARROW (ALR);
   1130?		BEGIN  NEW L,X;
 1140?		L _ ALR[2];
1150?		X _ ALR[3];
1160?		RETURN	IF ATOM L THEN <'SETQ, L, X>
 1170?			ELSE IF L[1] EQ '?&GETFIELD THEN <'?&PUTFIELD,  L[2], L[3], X>
 1180?			ELSE IF L[1] EQ '?&GETBYTE THEN <'?&PUTBYTE, L[2], L[3], X>
                                                 1190?			ELSE IF L[1] EQ 'GET THEN <'PUTPROP, L[2], X, L[3]>
    1200?			ELSE IF L[1] EQ '?&GETGENERAL THEN <'?&PUTGENERAL,  L[2], L[3,1], X>
1210?			ELSE IF L[1] EQ '?&GINDEX THEN <'?&GREPLACE, L[2], L[3], X>
 1220?			ELSE IF L[1] EQ 'SIZE THEN ERROR("CAN'T CHANGE SIZE")
  1230?			ELSE IF L[1] EQ '?&CURLY THEN
 1240?				IF ATOM L[2] THEN <'SETCONTEXT, <'QUOTE, L[2]>, '(QUOTE VALUE), L[3], X>
 1250?				ELSE <'SETCONTEXT, <'QUOTE, L[2,2]>, <'QUOTE, L[2,3]>, L[3], X>
1260?			ELSE <'store, L, X>;	% ASSUME ARRAY %
   1270?		END;
      ?                                                                                       0010?	LET expr (EX,L) expression = {<simpex>  {REP 0 M * {[INFIX]  <simpex>~~~
    0020?		MEAN
  0030?		IF L THEN CAR HIER(EX CONS L, 0) ELSE EX;
0040?	
  0050?	
  0060?	LET simpex (F, ARGLISTS) =
 0070?			{<basic>  {REP 0 M * {
   0080?				{ALT ?(  <arglist>  {ALT ?) | [ERROR("ILLEGAL ARGUMENT")]~
 0090?				  |  ?[  <arglist>  {ALT ?] | [ERROR("ILLEGAL INDEX")]~
    0100?				  |  ?{  <expr>  {ALT ?~ | [ERROR("ILLEGAL CONTEXT")]~
0110?				  |  ?.  {ALT [IDENTIFIER] | <basic>~
  0120?			~~~~
 0130?		MEAN
  0140?		BEGIN
 0150?		FOR NEW L IN ARGLISTS DO
  0160?			F _ CASE (L _ CAR L)[1] OF
    0170?				BEGIN
         0180?				IF ATOM F OR F.?&VAR THEN <'?&GETGENERAL, F, 'LIST CONS L[3]>
 0190?				ELSE IF F.?&FIELD THEN <'?&GETFIELD, <'QUOTE ,F>, L[3,1]>
 0200?				ELSE IF F.?&RCLASS THEN <F,'LIST CONS L[3]>
 0210?				ELSE IF F.?&BYTE THEN <'?&GETBYTE, <'QUOTE ,F>, L[3,1]>
   0220?				ELSE F CONS L[3];
  0230?	
  0240?				<'?&GINDEX, F, 'LIST CONS L[3]>;
  0250?	
  0260?				<'?&CURLY, F, L[3]>;
    0270?	
  0280?				<'GET, F, CASE L[3,1] OF BEGIN <'QUOTE, L[3,2]>; L[3,2]; END>;
  0290?				END;
0300?		RETURN F;
  0310?		END;
  0320?	
  0330?	
  0340?	LET basic (EX) = {{ALT [LITERAL]  <addedexpr>
                               0350?			    |  [PREFIX]  <simpex>
    0360?			    |  [IDENTIFIER]
0370?			    |  [NUMBER]
    0380?			    |  [STRING] ~~
 0390?		MEAN
  0400?		CASE EX[1] OF BEGIN  EX[3];  CDR EX;  EX[2];  EX[2];  <'QUOTE, EX[2]>;  END;
0410?	
  0420?	
  0430?	LET arglist (L) = {{REP 0 M * {<expr>~ ?,~~ MEAN (COND (L (MAPCAR (QUOTE CAR) L)));
   0440?	
  0450?	
  0460?	LET idlist (L) = {{REP 0 M * {[IDENTIFIER]~ ?,~~ MEAN arglist(L);
  0470?	
  0480?	
  0490?	LET addedexpr (X) = {{ALT [ISIDENTIFIER() | FAILURE()]  <idexp>
    0500?			       |  ?#  ?'  [SEXPRESSION]
                                                                0510?			       |  ?<  <arglist>  {ALT ?> | [ERROR("ILLEGAL EXPRESSION IN LIST BRACKETS")]~
   0520?			       |  ?(  <expr>  {ALT ?) | [ERROR("ILLEGAL PARENTHESIZED EXPRESSION")]~~~
  0530?		MEAN
  0540?		(COND ((EQ (CAR X) 1) (CADDR X))
   0550?		       ((EQ (CAR X) 2) (LIST (QUOTE QUOTE) (CADDDR X)))
 0560?		       ((EQ (CAR X) 3) (CONS (QUOTE LIST) (CADDR X)))
   0570?		       ((EQ (CAR X) 4) (CADDR X))
   0580?		       (T (CADR X)));
0590?	
  0600?	
  0610?	LET idexp (X) = {{ALT <IF>~~ MEAN X[2];
   0620?	
  0630?	
  0640?	LET OCTAL (*,*,*,N) addedexpr =
                                                             0650?			{?#  ?`  [(PROG NIL (SETQ IBASE 8) (SCANNER) (SETQ IBASE 10))]
  0660?				{ALT [NUMBER] | [ERROR("` NOT FOLLOWED BY A NUMBER")]~~
    0670?		MEAN N[2];
 0680?	
  0690?	
  0700?	LET IF (*,BE,*,E1,E2) = {IF  <expr>  {ALT THEN | [ERROR("ILLEGAL EXPRESSION AFTER 'IF'")]~
  0710?			{REP 1 M * {<expr>~ ALSO~  {OPT ELSE {REP 1 M * {<expr>~ ALSO~~~
 0720?		MEAN
  0730?		(CONS (QUOTE COND)
  0740?		       (CONS (CONS BE (MAPCAR (QUOTE CAR) E1))
0750?			     (COND ((NULL E2) NIL)
    0760?				   ((AND (NULL (CDR (SETQ E2 (CADR E2))))
   0770?					 (NOT (ATOM (CAAR E2)))
0780?					 (EQ (CAAAR E2) (QUOTE COND)))
        0790?				    (CDAAR E2))
    0800?				   (T (LIST (CONS T (MAPCAR (QUOTE CAR) E2)))))));
    0810?	
  0820?	
  0830?	LET LAMBDA (*,*,VARS,*,*,EX,ARGS) idexp = {LAMBDA  ?(  <declarations>  ?)  ?;  <expr>  {OPT ?; ?( <arglist> ?)~~
0840?		MEAN
  0850?		IF ARGS THEN <'LAMBDA, VARS, EX> CONS ARGS[3] ELSE <'LAMBDA, VARS, EX>;
0860?	
  0870?	
  0880?	LET FOR (L,D,EX,BE) idexp = {
   0890?			{REP 1 M * {
   0900?				FOR
 0910?				{OPT <access>~
0920?				{ALT [IDENTIFIER] | [ERROR("NON-IDENTIFIER USED AS CONTROL VARIABLE IN FOR-LOOP")]~
 0930?				{ALT IN  <expr>
    0940?				  |  ON  <expr>
                                  0950?				  |  ?_  <expr>  {ALT TO | [ERROR("ILLEGAL EXPRESSION AFTER _ IN FOR-LOOP")]~  <expr>
    0960?						{OPT BY <expr>~~~~
    0970?			{ALT DO | COLLECT | ?; [IDENTIFIER] | [ERROR("EXPECTED DO, COLLECT OR ; IN FOR-LOOP")]~
   0980?			<expr>
    0990?			{OPT UNTIL <expr>~~
 1000?		MEAN
  1010?		<'?&FOR, <'QUOTE, MAPCAR(FUNCTION(LAMBDA (I); <
    1020?				IF I[2] THEN LAMBDA(FN); FN(<I[3,2]>); (I[2,1]) ALSO 'NEW ELSE 'OLD,
 1030?				I[3,2],
  1040?				(I _ I[4])[2],
1050?				IF I[1] EQ 3 THEN <'?&RANGE, I[3], I[5], IF I[6] THEN I[6,2] ELSE 1> ELSE I[3]>), L)>,
                                                          1060?			 <'QUOTE, IF D[1] EQ 1 THEN 'PROG2 ELSE IF D[1] EQ 2 THEN 'APPEND ELSE D[3]>,
   1070?			 <'QUOTE, EX>,
 1080?			 <'QUOTE, IF BE THEN BE[2] ELSE NIL>>;
  1090?	
  1100?	
  1110?	LET WHILE (*,BE,D,EX) idexp = {WHILE  <expr>
   1120?			{ALT DO | COLLECT | [ERROR("EXPECTED DO OR COLLCT IN WHILE-LOOP")]~  <expr>~
    1130?		MEAN
  1140?		<'?&WHILE, <'QUOTE, IF D[1] EQ 1 THEN 'PROG2 ELSE 'APPEND>, <'QUOTE, BE>, <'QUOTE, EX>>;
   1150?	
  1160?	
  1170?	LET UNTIL (D,EX,*,BE) idexp = {{ALT DO | COLLECT~  <expr>
1180?			{ALT UNTIL | [ERROR("ILLEGAL EXPRESSION AFTER " CAT PREVIOUS(2)[2])]~  <expr>~
  1190?		MEAN
       1200?		<'?&DO, <'QUOTE, IF D[1] EQ 1 THEN 'PROG2 ELSE 'APPEND>, <'QUOTE, EX>, <'QUOTE, BE>>;
 1210?	
  1220?	
  1230?	LET BEGIN (*,VARS,EXLIST,*) idexp = {BEGIN  <blockdeclarations>  <EXLIST>  END~
  1240?		MEAN
  1250?		'PROG CONS VARS CONS FOR NEW I IN EXLIST COLLECT IF I THEN <I> ELSE NIL;
    1260?	
  1270?	
  1280?	LET EXLIST (L) = {{REP 0 M * {[IF NEXT('END) THEN FAILURE()]  <expr>~  <SEMI>  [FLUSH]~~
    1290?		MEAN
  1300?		MAPCAR('CADR, L);
    1310?	
  1320?	
  1330?	LET SEMI (X) = {{ALT ?; | [IF NEXT('END) THEN FAILURE()]~~ MEAN X;
1340?	
  1350?	
                                                         1360?	LET globaldeclaration (*, L) idexp = {GLOBAL <typelist>~ MEAN
1370?		IF ?!PCOD THEN
  1380?			(?!MISCELLANEOUS{NIL~ _ ('SPECIAL CONS L) CONS ?!MISCELLANEOUS)
  1390?		ALSO NIL ;
 1400?	
  1410?	
  1420?	LET blockdeclarations (L) = {{REP 0 M * {<blockdeclaration>  ?;~~~ MEAN FOR NEW I IN L COLLECT CAR I;
    1430?	
  1440?	
  1450?	LET blockdeclaration (A,L) = {<decl> <typelist>~ MEAN A(L);
  1460?	
  1470?	
  1480?	LET declarations (L) = {{REP 0 M * {<declaration>~ ?;~~ MEAN FOR NEW I IN L COLLECT CAR I;
  1490?	
  1500?	
  1510?	LET declaration (A,L) = {{OPT <decl>~  <typelist>~
  1520?		MEAN
            1530?		blockdeclaration(IF A THEN CAR A ELSE 'PRIVATE.?&ACCESS, L);
1540?	
  1550?	
  1560?	LET decl (X) = {{ALT <datatype> | <access>~~ MEAN X[2];
 1570?	
  1580?	
  1590?	LET datatype (X) = {[IF PROPERTY('?&TYPE) THEN IDENTIFIER().?&TYPE ELSE FAILURE()]~ MEAN X;
1600?	
  1610?	
  1620?	LET access (X) = {[IF PROPERTY('?&ACCESS) THEN IDENTIFIER().?&ACCESS ELSE FAILURE()]~ MEAN X;
    1630?	
  1640?	
  1650?	LET typelist (TL,L) = {{REP 1 M * {[IDENTIFIER]~~  {REP 0 M * {?,  [IDENTIFIER]~~~
1660?		MEAN
  1670?		LAST(TL)[1,1] CONS MAPCAR('CADR, L);
1680?	
  1690?	
                                                    1700?	LET FNDEF (IND,FN,*,VARS,PVARS,*,EX) idexp =
   1710?			{[IF PROPERTY('?&FUNCTION) THEN ?&SPECLIST{NIL~ _ NIL ALSO IDENTIFIER().?&FUNCTION
   1720?				ELSE FAILURE()]
    1730?			[IDENTIFIER]  ?(  <declarations>  {OPT ?: <declarations>~
   1740?			{ALT ?) ?= | [ERROR("ILLEGAL LAMBDA VARIABLE LIST FOR FUNCTION " CAT PREVIOUS(3))]~
  1750?			<expr>~
   1760?		MEAN
  1770?		BEGIN
 1780?		IF GETL(FN, '(EXPR FEXPR LEXPR SUBR FSUBR LSUBR MACRO)) THEN WARNING("FUNCTION REDEFINED", FN);
 1790?		PUTPROP(FN, <'LAMBDA, VARS, IF PVARS THEN <'PROG, PVARS[2], <'RETURN,EX>> ELSE EX>, IND);
  1800?		IF ?!PCOD THEN
                 1810?			IF ?&SPECLIST THEN 
 1820?				?!MISCELLANEOUS{NIL~_('UNSPECIAL CONS ?&SPECLIST) CONS FN CONS ('SPECIAL CONS ?&SPECLIST)
1830?				CONS ?!MISCELLANEOUS
    1840?				ALSO ?&SPECLIST{NIL~ _ NIL
   1850?			ELSE ?!MISCELLANEOUS{NIL~ _ FN CONS ?!MISCELLANEOUS;
   1860?		PRINTTY(FN);
    1870?		END;
  1880?	
  1890?	
  1900?	LET CASE (*,N,*,*,EX,*) idexp = {CASE  <expr>  {ALT OF | [ERROR("ILLEGAL EXPRESSION AFTER 'CASE'")]~
  1910?			{ALT BEGIN | [ERROR("'BEGIN' NEEDED AFTER 'OF' IN CASE expression")]~  <EXLIST>  END~
1920?		MEAN
  1930?		BEGIN  NEW LABELS, EXPRS, L;
                                                     1940?		FOR NEW I IN EX DO PROG2(LABELS _ (L _ GENSYM()) CONS LABELS, EXPRS _ <'RETURN, I> CONS L CONS EXPRS);
    1950?		RETURN('PROG CONS NIL CONS <'GO, <'?&GINDEX, <'QUOTE, REVERSE LABELS>, <'LIST, N>>> CONS REVERSE EXPRS);
  1960?		END;
  1970?	
  1980?	
  1990?	LET INLINE (*,*,*,L,*,*,*) idexp = {?#  INLINE  [IBASE_8]
2000?			{REP 1 M * {[SEXPRESSION]  [IF PREVIOUS(1) THEN FAILURE()]~  ?#  ?;~  [IBASE_10]  ?;  NIL~
    2010?		MEAN
  		BEGIN  NEW FN, GEN, CONLIST, LOC, REMOB;   SPECIAL GEN, CONLIST, LOC, REMOB;      
    2030?		L _ MAPCAR('CAR, L);
                                                                  2040?		IF GETL(FN _ L[1,2], '(EXPR FEXPR LEXPR SUBR FSUBR LSUBR MACRO)) THEN WARNING("FUNCTION REDEFINED", FN);
  2050?		GEN _ GENSYM();   CONLIST _ <NIL>;   LOC _ BPORG;
  2060?		FOR NEW I IN CDR L DO IF ATOM I THEN DEFSYM(I, LOC) ELSE DEPOSIT(LOC, GWD(I)) ALSO INLOC();
2070?		DEFSYM(GEN, LOC);
    2080?		FOR NEW I IN CDR CONLIST DO PROG2(DEPOSIT(LOC, GWD(I)), INLOC());
 2090?		PUTPROP(FN, NUMVAL BPORG, L[1,3]);
  2100?		BPORG _ LOC;
    2110?		IF ?!PCOD THEN ?!MISCELLANEOUS{NIL~ _ (L @ <NIL>) CONS ?!MISCELLANEOUS;
2120?		PRINTTY(FN);
    2130?		END;
  2140?	
  2150?	
                                                    2160?	EXPR INLOC ();  IF (LOC _ LOC+1)  BPEND1 THEN FATALERROR("NOT ENOUGH BINARY PROGRAM SPACE FOR INLINE CODE");
  2170?	
  2180?	
  2190?	LET SELECT (*,VALUEFN,*,VAR,ARG,SUCCESSORFN,TERMIN) idexp = {SELECT  {OPT <expr>~
    2200?			FROM  {OPT [IDENTIFIER] ?:~  <expr>  {OPT NEXT <expr>~
 2210?			{OPT  UNLESS  <expr>  {OPT IN WHICH CASE <expr>~~~
2220?		MEAN
  2230?		BEGIN
 2240?		IF VALUEFN | SUCCESSORFN | TERMIN THEN
 2250?			IF VAR THEN VAR _ <VAR[1]> ELSE ERROR("VARIABLE NEEDED IN SELECT expression")
   2260?			ELSE IF VAR THEN VAR _ <VAR[1]>
    2270?			ELSE VAR _ <INTERN GENSYM()>;
                          2280?		RETURN <'?&SLCT, ARG,
2290?			IF VALUEFN THEN <'FUNCTION, <'LAMBDA, VAR, VALUEFN[1]>> ELSE '(QUOTE CAR),
    2300?			IF SUCCESSORFN THEN <'FUNCTION, <'LAMBDA, VAR, SUCCESSORFN[2]>> ELSE '(QUOTE CDR),
 2310?			IF TERMIN THEN <'FUNCTION, <'LAMBDA, VAR, TERMIN[2]>> ELSE '(QUOTE NULL),
  2320?			IF TERMIN & TERMIN[3] THEN <'FUNCTION, <'LAMBDA, VAR, TERMIN[3,4]>> ELSE '(QUOTE FAILURE)>;
    2330?		END;
  2340?	
  2350?	
  2360?	LET RECOMPILE (*,SPECIAL ?&FNS,*,FILE,EXT) idexp = {RECOMPILE <idlist> IN [IDENTIFIER] {OPT ?. [IDENTIFIER]~~
   2370?		MEAN
  2380?		BEGIN  NEW LEN, N, ?&FOO;   SPECIAL ?&FOO;
              2390?		LEN _ LENGTH ?&FNS;				% NUMBER OF FUNCTIONS TO COMPILE. %
   2400?		N _ 0;
2410?		EVAL <'INPUT, 'DSK?:, IF EXT THEN FILE CONS EXT[2] ELSE FILE>;
    2420?		TERPRI INC(T,NIL);
   2430?	 START;	TOKEN();					% GET THE FIRST TOKEN. %
  2440?		IF ?!SCANVAL  '(LET FUNCTION MACRO) THEN GO SEL
   2450?		ELSE IF NEXT('EOF) THEN RETURN TERPRI INC(NIL,T)
 2460?		ELSE WHILE NEXT('?;) DO TOKEN();
   2470?		FLUSH();					% WE GOT TO A ; %
 2480?		GO START;
  2490?	 SEL;	SELECT	BEGIN
    2500?			TOKEN();
  2510?			?&FOO{NIL~ _ ?!SCANVAL  ?&FNS;		% FUNCTION TO BE COMPILED? %
    2520?			FAILURE();
2530?			END
            2540?		FROM L:'(NIL) UNLESS L IN WHICH CASE NIL;
    2550?		IF ?&FOO THEN expression(expression?#())
 2560?			ALSO IF LEN EQ N_N+1 THEN RETURN TERPRI TERPRI INC(NIL,T);
  2570?		GO START;
  2580?		END;
  2590?	
  2600?	
  2610?	LET LAYOUT (*,ID,VARS,*,*,L,*) idexp =
    2620?			{ LAYOUT  [IDENTIFIER]  {OPT  ?(  <idlist>  ?)~  ?=  ?(
2630?				{REP 1 M * {[IDENTIFIER]  ?(  <expr>  ?:  <expr>  ?)~  ?,~  ?)~
 2640?		MEAN
  2650?		BEGIN  NEW NBITS;
    2660?		NBITS _ 0;
 2670?		FOR NEW I IN L DO
    2680?			BEGIN  NEW B;
  2690?			B _	IF ATOM I[5] THEN <'?&BYTE, I[3], I[5], NIL, NBITS/36>	% FOR NOW %
                    2700?				ELSE <'?&BYTE, I[3], I[5,2], T, NBITS/36>;		% FOR NOW %
    2710?			PUTPROP(I[1], B, '?&BYTE);
    2720?			IF ?!PCOD THEN ?!MISCELLANEOUS{NIL~ _ <'DEFPROP,I[1],B,'?&BYTE> CONS ?!MISCELLANEOUS ;
    2730?			NBITS _ NBITS + B[3];
    2740?			END;
 2750?		PRINTTY(ID);
    2760?		END;
  2770?	
  2780?	
  2790?	LET RECORDCLASS (*,*,ID,TYP,*,FLDS,*,RECS,INCR) idexp =
 2800?			{ RECORD  CLASS  [IDENTIFIER]  {OPT ?[  [IDENTIFIER]  ?]~  ?(  <declarations>  ?)
    2810?				{OPT <expr>~  {OPT ?, <expr>~ ~
   2820?		MEAN
  2830?		BEGIN  NEW L, FRM;
   2840?		L _ LENGTH FLDS;
2850?		PUTPROP(ID, T, '?&RCLASS);
          2870?		FRM _ MAKERECORDFORM(ID, FLDS,
    2880?			IF RECS THEN EVAL(RECS[1]) ELSE IF L11  L=0 THEN 10 ELSE 100/L,
2890?	 		IF INCR THEN EVAL(INCR[2]) ELSE IF L11  L=0 THEN 15 ELSE 150/L);
   2895?		FOR NEW FLD IN ?&GETFIELD('FIELDTABLE, FRM) DO
   2897?			PUTPROP(CAR FLD, CADR FLD CONS (CAR FLD).?&FIELD, '?&FIELD);
2900?		TYP _ IF TYP THEN TYP[2] ELSE ID;
   2910?		MAKETYPE(<TYP, 'ERROR, 'UNIMPLEMENTED,	'ERROR, 'ERROR, 'IDENTITY, 'IDENTITY, FRM, NIL>) ;
 2920?		PUTPROP(ID, <'LAMBDA, '(FIELDS), <'RECORD, <'QUOTE, TYP.?&TYPE>, 'FIELDS>>, 'EXPR) ;
  2930?		PRINTTY(ID);
    2940?		END ;
 2950?	
  2960?	
            2970?	LET VECTORCLASS (*,*,ID,BYTESIZE) idexp =
    2980?			{ VECTOR  CLASS  [IDENTIFIER]  {OPT ?(  <expr>  ?,  [IDENTIFIER]  ?,  [IDENTIFIER]  ?)~ ~
 2990?		MEAN
  3000?		BEGIN  SPECIAL BYTEVECTORFORM, WORDVECTORFORM;
 3010?		IF BYTESIZE THEN MAKETYPE(<ID, 'INCONVERTVECTOR, 'OUTCONVERTVECTOR, 'ERROR, 'ERROR,
  3020?			BYTESIZE[4], BYTESIZE[6], BYTEVECTORFORM, NIL>)
    3030?		ELSE MAKETYPE(<ID, 'INCONVERTVECTOR, 'OUTCONVERTVECTOR, 'ERROR, 'ERROR,
3040?			'IDENTITY, 'IDENTITY, WORDVECTORFORM, NIL>) ;
   3050?		PUTPROP(ID,
3060?			<'LAMBDA, '(?&L),
                                                     3070?				<'VECTOR, <'QUOTE, ID.?&TYPE>, IF BYTESIZE THEN EVAL BYTESIZE[2] ELSE -1, '?&L>>,
 3080?			'EXPR) ;
  3090?		PRINTTY(ID);
    3100?		END;
      ?  0010?	EOF
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @A;