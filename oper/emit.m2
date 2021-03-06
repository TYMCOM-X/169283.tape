0010?	BEGIN
  0020?	
  0030?	SPECIAL SCNVAL, !SCANVAL, !SCANTYPE, !IDTYPE, !STRTYPE, !NUMTYPE, !DELIMTYPE, !SEXPTYPE,
    0040?		!PRODUCTIONS, !LITERALS, !SPECIALS, !FIXUPS, !MISCELLANEOUS, !INITIALIZED, !BACKUP, !SAW?#, !PCOD,
   0050?		!PROD, !PROD?#, !CODE, !PC, !LAST, LOC, CONLIST, GEN, REMOB, KLIST,
    0060?		BASE, IBASE, BPORG, BPEND, BPORG1, BPEND1, OBLIST, !CONTEXT!, !CONTEXTLIST!;
    0070?	
  0080?	DEFINE EMIT PREFIX, Q PREFIX, PUSH PREFIX, POPINTO PREFIX, POPJ PREFIX, CONSTANT PREFIX,
    0090?		JUMP PREFIX, LABEL PREFIX, MEMQ ;
  0100?	
  0110?	EXPR EPAT (PAT, NARGS, NITEMS, PUSHLIST, NOVALUE);
            0120?		% THIS EMITS CODE TO PARSE A PATTERN, ASSEMBLE IT, AND LEAVE THE ASSEMBLED CODE IN ACCUMULATOR 1.
    0130?		ALL EMITTERS CALLED MUST EITHER LEAVE THEIR VALUE IN ACC 1 (LEN1) OR ON TOP OF THE STACK (LEN>1).
   0140?		THE STACK WILL BE UNCHANGED BEFORE AND AFTER THE EXECUTION OF THIS CODE. %
  0150?		IF NULL PAT THEN MOVEI(1, '(QUOTE NIL))
  0160?		ELSE	BEGIN  NEW LEN, PUSHIT;
   0170?			LEN _ LENGTH PAT;
   0180?			FOR NEW I IN PAT DO			% EMIT CODE FOR EACH ITEM IN THE PATTERN. %
0190?				BEGIN
    0200?				I _ CAR I;			% STRIP OFF THE EXTRA LEVEL OF PARENTHESES. %
                                                   0210?				IF LEN EQ 1 | NOVALUE THEN PUSHIT _ NIL
0220?				ELSE IF PUSHLIST THEN PUSHIT _ CAR PUSHLIST ALSO PUSHLIST _ CDR PUSHLIST
  0230?				ELSE PUSHIT _ T;
   0240?				IF I[1] EQ 1 THEN PUTPROP(I _ I[2], T, 'LITERAL)		% IDENTIFIER %
0250?					ALSO	IF I EQ '?# THEN !SAW?# _ T
 0260?							ALSO PUSHIT & PUSH CONSTANT '(QUOTE ?#)
  0270?						ELSE EIDEN(I, PUSHIT)
 0280?				ELSE IF I[1] EQ 2 THEN						% <PRODUCTION> %
0290?					ECALL(SYNAM(I[4], GET(I[4],'!PROD?#)), I[3], I[4], PUSHIT)
    0300?				ELSE IF I[1] EQ 3 THEN
  0310?					IF (I _ I[3])[1] EQ 1 THEN				% REP %
                                                   0320?						EREP(I[3] CONS I[4,2], I[5], I[7], I[9], PUSHIT)
    0330?					ELSE IF I[1] EQ 2 THEN EOPT(I[3], PUSHIT)		% OPT %
   0340?					ELSE EALT(MAPCAR('CAR, I[3]), PUSHIT)			% ALT %
 0350?				ELSE IF I[1] EQ 4 THEN EPRED(I[3], PUSHIT)			% [EXPRESSION] %
   0360?				ELSE EIDEN(I[2], PUSHIT);					% NUMBER %
    0370?				END;
0380?			IF NOVALUE THEN RETURN NIL
    0390?			ELSE IF NULL NITEMS THEN		% DO A STANDARD ASSEMBLE. %
  0400?				RETURN IF LEN EQ 1 THEN CALL(1, 'NCONS) ELSE MOVEI(1, LEN) ALSO CALL(1, 'ASSEM)
0410?			ELSE IF LEN EQ NITEMS THEN		% RIGHT NUMBER OF ARGUMENTS. %
                                          0420?				IF LEN EQ 1 THEN RETURN NIL ELSE NIL
   0430?			ELSE IF LEN GREATERP NITEMS THEN	% ASSEMBLE SOME ARGUMENTS AND POP THE OTHERS. %
0440?				BEGIN
    0450?				MOVEI(1, LEN - NITEMS + 1);
  0460?				CALL(1, 'ASSEM);
   0470?				IF NARGS NEQ 1 THEN MOVE(NARGS, 1);
    0480?				NARGS _ NARGS-1;
   0490?				END
 0500?			ELSE FATALERROR("PATTERN NOT LONG ENOUGH: " CAT !PROD);
    0510?			FOR NEW N_NARGS TO 1 BY -1 DO POPINTO N; % POP THE PATTERN INTO THE ARGUMENT AC'S. %
 0520?			END;
 0530?	
  0540?	
  0550?	EXPR EREP (MINMAX, ST, PAT, SEP, P);			% EMIT A REP. %
                                                     0560?		BEGIN  NEW GOTMAX, FAIL, LOOP, START;
    0570?		LOOP _ GENSYM();   FAIL _ GENSYM();   IF SEP THEN START _ GENSYM();
    0580?		IF ST THEN					% START OF A REP *. %
0590?			BEGIN
0600?			MOVEI(1, FAIL);
0610?			CALL(1, 'SREP?*);			% THIS TAKES CARE OF SETTING UP A DECISION POINT. %
    0620?			IF SEP THEN
    0630?				BEGIN
    0640?				JUMP START;
   0650?				LABEL LOOP;
   0660?				EPAT(SEP[1], NIL, NIL, NIL, T);	% EMIT CODE FOR THE REP SEPARATOR(S). %
   0670?				LABEL START;
  0680?				END
 0690?			ELSE LABEL LOOP;			% LOOP AFTER THE DECISION POINT. %
  0700?			END
                                          0710?		ELSE	BEGIN					% START OF A NORMAL REP. %
0720?			CALL(0, 'SREP);
0730?			IF SEP THEN
    0740?				BEGIN
    0750?				MOVEI(1, FAIL);
    0760?				CALL(1, 'DPNT);
    0770?				JUMP START;
   0780?				LABEL LOOP;
   0790?				MOVEI(1, FAIL);
    0800?				CALL(1, 'DPNT);
    0810?				EPAT(SEP[1], NIL, NIL, NIL, T);	% EMIT CODE FOR THE REP SEPARATOR(S). %
   0820?				LABEL START;
  0830?				END
 0840?			ELSE	BEGIN
0850?				LABEL LOOP;
   0860?				MOVEI(1, FAIL);
    0870?				CALL(1, 'DPNT);
    0880?				END;
0890?			END;
 0900?		EPAT(PAT, NIL, NIL, NIL, NIL);			% EMIT THE REP PATTERN. %
                       0910?		IF ST THEN CALL(1,'RREP?*) ELSE CALL(1,'RREP);	% ADD THE CURRENT VALUE TO THE REP VALUE. %
 0920?		IF CDR MINMAX NEQ 'M THEN
 0930?			BEGIN
0940?			GOTMAX _ GENSYM();
  0950?			IF ST THEN EMIT '(HRRZ?@ 1 0 P) ELSE MOVE(1, 0, 'P);
   0960?			CALL(1, 'LENGTH);
   0970?			SKIPIFN(1, Q CDR MINMAX);		% HAVE WE GOTTEN THE MAXIMUM? %
 0980?			JUMP GOTMAX;				% YES. %
 0990?			END;
 1000?		JUMP LOOP;					% GO BACK FOR ANOTHER CYCLE. %
 1010?		LABEL FAIL;					% WHAT TO DO ON FAILURE IN A REP. %
1020?		IF ST THEN CALL(1, 'RDDPNT?*) ELSE CALL(1, 'RDDPNT);
    1030?		IF CDR MINMAX NEQ 'M THEN LABEL GOTMAX;
                 1040?		IF CAR MINMAX NEQ 0 THEN
  1050?			BEGIN
1060?			IF ST THEN EMIT '(HRRZ?@ 1 0 P) ELSE MOVE(1, 0, 'P);	% COMPUTE # OF REP CYCLES. %
    1070?			CALL(1, 'LENGTH);
   1080?			SKIPIFGE(1, Q CAR MINMAX);		% DID WE GET THE MINIMUM? %
    1090?			JCALL(0, 'FAILURE);			% NO. %
 1100?			END;
 1110?		IF ST THEN EMIT '(HRRZ?@ 1 0 P)			% COMPUTE THE VALUE OF THE REP. %
    1120?		ELSE MOVE(1, 0, 'P) ALSO CALL(1, 'REVERSE);
   1130?		IF P THEN MOVEM(1, 0, 'P) ELSE POPINTO 2;
1140?		END;
  1150?	
  1160?	
  1170?	EXPR EOPT (PAT, P);					% EMIT AN OPT. %
  1180?		BEGIN  NEW EXIT, FAIL;
                                       1190?		MOVEI(1, FAIL _ GENSYM());
1200?		CALL(1, 'DPNT);
 1210?		EPAT(PAT, NIL, NIL, NIL, NIL);
 1220?		CALL(1, 'RDPNT);
1230?		IF P THEN PUSH 1;
    1240?		JUMP EXIT _ GENSYM();
1250?		LABEL FAIL;					% WHAT TO DO ON FAILURE IN AN OPT. %
    1260?		CALL(1, 'RDDPNT);				% DELETE -- THE OPT IS NO LONGER A DECISION POINT. %
   1270?		IF P THEN PUSH CONSTANT '(QUOTE NIL) ELSE MOVEI(1, '(QUOTE NIL));	% THE OPT VALUE BECOMES NIL. %
1280?		LABEL EXIT;
1290?		END;
  1300?	
  1310?	
  1320?	EXPR EALT (L, P);					% EMIT AN ALT. %
    1330?		BEGIN  NEW SUCCESS, FAIL, N;
                                                          1340?		SUCCESS _ GENSYM();				% THE SUCCESS EXIT. %
  1350?		N _ 0;						% THE NUMBER OF THE ALTERNATIVE WE'RE ON. %
 1360?		CALL(1, 'DPNT);
 1370?		IF NULL L THEN LABEL GENSYM()			% EMIT A DUMMY LABEL IF THIS IS A DUMMY ALT. %
   1380?		ELSE FOR NEW PAT IN L DO
  1390?			BEGIN
1400?			MOVEI(1, FAIL _ GENSYM());
    1410?			EMIT '(HRRM 1 0 SSTACK);		% DON'T CLOBBER THE CONTEXT # IN THE LEFT HALF. %
1420?			EPAT(PAT, NIL, NIL, NIL, NIL);
1430?			MOVEI(2, Q N _ N+1);			% CONS ON THE NUMBER OF THE ALT. %
   1440?			JUMP SUCCESS;
  1450?			LABEL FAIL;				% WHERE TO GO IF THIS ALTERNATIVE FAILS. %
   1460?			END;
           1470?		PUTPROP(!PROD, <N+1, LOC, LOC+1, !PC+1, SUCCESS, !LAST>, 'NEXTALT);
    1480?		JCALL(0, 'ENDOFALT);				% THIS INST IS REPLACED IF MORE ALT'S ARE ADDED LATER. %
    1490?		LABEL SUCCESS;
  1500?		CALL(2, 'XCONS);
1510?		CALL(1, 'RDPNT);
1520?		IF P THEN PUSH 1;
    1530?		END;
  1540?	
  1550?	
  1560?	EXPR ECALL (PROD?#, SEM, PROD, P);			% EMIT A CALL ON A PRODUCTION. %
   1570?		BEGIN
 1580?		PUSH '(SPECIAL !PMRK);				% PUSH THE PREVIOUS MARKER ON THE STACK. %
   1590?		IF GET(PROD,'ARGUMENTS) THEN CALL(0, PROD?#) ALSO (IF SEM THEN CALL(GET(PROD,'ARGUMENTS), PROD))
                                             1600?		ELSE	BEGIN					% NO ARGS > FORWARD REFERENCE TO THE PRODUCTION. %
1610?			IF (PROD  !FIXUPS) THEN SETCONTEXT('!FIXUPS, 'VALUE, NIL, PROD CONS !FIXUPS);
1620?			JCALL(0, 'FAILURE);
 1630?			PUTPROP(PROD, <!PC, !PROD, !PROD?#> CONS GET(PROD,'FIXUP), 'FIXUP);
   1640?			IF SEM THEN CALL(0, PROD)
1650?				ALSO PUTPROP(PROD, <!PC, !PROD, !PROD?#> CONS GET(PROD,'ARGSFIXUP), 'ARGSFIXUP);
    1660?			END;
 1670?		IF P THEN MOVEM(1,0,'P) ELSE POPINTO 2;
  1680?		END;
  1690?	
  1700?	
  1710?	EXPR EPRED (PRED, P);					% EMIT A PREDICATE. %
                                                                                1720?		IF PRED  '(IDENTIFIER STRING NUMBER DELIMITER SEXPRESSION) THEN CALL(0, PRED) ALSO (IF P THEN PUSH 1)
    1730?		ELSE	BEGIN
 1740?			PUSH '(SPECIAL !PMRK);			% PUSH THE PREVIOUS MARKER ON THE STACK. %
   1750?			IF ATOM PRED THEN CALL(0, PRED)		% AN ATOM; TREAT IT AS A FUNCTION NAME. %
 1760?			ELSE	BEGIN  NEW G;
  1770?				G _ GENAM(!PROD, GENSYM());		% GENERATE A PREDICATE NAME. %
1780?				PUTPROP(G, <'LAMBDA, NIL, PRED>, 'EXPR);
    1790?				CALL(0, G);
   1800?				IF !PCOD THEN PUTPROP(!PROD, G CONS GET(!PROD,'PREDS), 'PREDS);
 1810?				END;
                                                                      1820?			IF P THEN MOVEM(1,0,'P) ELSE POPINTO 2;	% REPLACE THE PREVIOUS MARKER WITH VALUE OF THE PRED. %
1830?			END;
 1840?	
  1850?	
  1860?	EXPR EIDEN (I, P);					% EMIT AN IDENTIFIER. %
 1870?		BEGIN
 1880?		MOVEI(1, Q I);
  1890?		IF NUMBERP I THEN CALL(1, 'NCHK) ELSE SKIPIFME(1, '(SPECIAL !SCANVAL)) ALSO JCALL(0, 'FAILURE);
1900?		IF P THEN PUSH 1;
    1910?		IF !SAW?# THEN !SAW?# _ NIL			% INHIBIT SCANNING AFTER A # IN THE SYNTAX. %
 1920?		ELSE CALL(0, 'SCANNER) ALSO (IF P THEN MOVEI(1, Q I));
 1930?		END;
  1940?	
  1950?	
  1960?	EXPR EMIT (INST);					% THE CODE EMITTER. %
                                  1970?		BEGIN  SPECIAL LOC, !PC, !LAST, !PCOD;
   1980?		IF ATOM INST THEN
    1990?			IF INST THEN DEFSYM(INST, LOC) ELSE NIL
 2000?		ELSE DEPOSIT(LOC, GWD(INST))
   2010?			ALSO UPLOC()				% LOC POINTS TO THE NEXT LOCATION TO DEPOSIT INTO. %
  2020?			ALSO !PC _ !PC + 1;			% !PC POINTS TO THE LAST INSTRUCTION EMITTED. %
 2030?		IF !PCOD THEN !LAST _ CDR RPLACD(!LAST,<INST>);	% !LAST CONTAINS THE LAST INSTRUCTION EMITTED. %
2040?		END;
  2050?	
  2060?	
  2070?	EXPR LAPST (NAME);					% LAP START - START OF A PRODUCTION. %
 2080?		BEGIN  SPECIAL !PC, !CODE, !LAST, !PCOD, GEN, CONLIST, LOC, BPORG;
                         2090?		!PC     _ 0;
    2100?		!CODE   _ !LAST _ IF !PCOD THEN <<'LAP, NAME, 'SUBR>> ELSE NIL;
   2110?		GEN     _ GENSYM();				% BEGIN LAP. %
    2120?		CONLIST _ <NIL>;
2130?		LOC     _ BPORG;
2140?		END;
  2150?	
  2160?	
  2170?	EXPR LAPFN (NAME);					% LAP FINISH - END OF A PRODUCTION. %
  2180?		BEGIN  SPECIAL GEN, LOC, CONLIST, KLIST, BPORG;
    2190?		IF NAME THEN POPJ 'P ALSO EMIT NIL;		% DON'T EMIT THIS IF NAME = NIL. %
2200?		DEFSYM(GEN, LOC);				% END LAP. %
   2210?		FOR NEW I IN CDR CONLIST DO
    2220?			BEGIN
2230?			KLIST _ (I CONS LOC) CONS KLIST;	% ADD GENERATED CONSTANTS TO THE KLIST. %
           2240?			DEPOSIT(LOC, GWD(I));
    2250?			UPLOC();
  2260?			END;
 2270?		IF NAME THEN PUTPROP(NAME, NUMVAL(BPORG), 'SUBR);
  2280?		BPORG _ LOC;
    2290?		END;
  2300?	
  2310?	
  2320?	EXPR FIXUP (PROD, PROD?#);				% FIXUP FORWARD REFERENCES TO THIS PRODUCTION. %
    2330?		IF GET(PROD,'FIXUP) THEN
  2340?			FOR NEW INST IN <<'CALL, 0, <'E, PROD?#>>, <'CALL, GET(PROD,'ARGUMENTS), <'E, PROD>>>
2350?			FOR NEW IND IN '(FIXUP ARGSFIXUP) DO
    2360?				BEGIN  NEW CINST;
  2370?				CINST _ GWD(INST);			% COMPILE THE INSTRUCTION INTO BINARY. %
   2380?				FOR NEW A IN GET(PROD, IND) DO		% A = (PC  PROD  PROD#). %
      2390?					BEGIN
   2400?					DEPOSIT(ADDR(A[3])-1 + A[1], CINST);
  2410?					IF !PCOD THEN RPLACA(FIND(A[1], CDR GET(A[2],'CODE)), INST);
   2420?					END;
    2430?				REMPROP(PROD, IND);
2440?				END;
2450?	
  2460?	
  2470?	EXPR ADALT (PROD, PROD?#, ALT, ALT?#);		% ADD 'PROD' AS AN ALTERNATIVE TO 'ALT'. %
2480?		BEGIN  NEW !PC, !CODE, !LAST, LOC, CONLIST, GEN, REMOB, G;
   2490?		   SPECIAL !PC, !CODE, !LAST, LOC, CONLIST, GEN, REMOB;
 2500?		NEW FAIL, A, N, ABSOLUTELOC, RETURNLOC, PC, SUCCESSLABEL, ALTCODE;
  2510?		%  A  =  (N  ABSOLUTELOC  RETURNLOC  PC  SUCCESSLABEL  ALTCODE) %
                            2520?		IF A _ GET(ALT,'NEXTALT) THEN RETURN AERR(ALT);	% OK TO ADD AN ALTERNATIVE? %
   2530?		N _ A[1];					% NUMBER OF NEXT ALTERNATIVE. %
 2540?		IF NULL ABSOLUTELOC _ A[2] THEN		% ABSOLUTE LOCATION OF ENDOFALT. %
 2550?			ABSOLUTELOC _ ADDR(ALT?#)-1 + A[4]
2560?			ALSO RETURNLOC _ ABSOLUTELOC + 1
 2570?		ELSE RETURNLOC _ A[3];				% SUCCESS RETURN. %
2580?		PC _ A[4];					% PC OF ENDOFALT. %
2590?		IF !PCOD THEN SUCCESSLABEL _ RETURNLOC
2600?		ELSE IF NULL SUCCESSLABEL _ A[5] THEN
   2610?			(IF G _ GET(ALT,'CODE) THEN RETURN AERR(ALT))
                                                                     2620?			ALSO ALTCODE _ CDR FIND(PC, G)		% ALTCODE = (LABEL  JUMP('ENDOFALT)  LABEL  ...) %
 2630?			ALSO DEFSYM(SUCCESSLABEL _ ALTCODE[3], RETURNLOC)
    2640?		ELSE IF ALTCODE _ A[6] THEN RETURN AERR(ALT);	% THE CODE HAS TO BE IN ONE OF THESE TWO PLACES. %
    2650?	
  2660?		LAPST(NIL);					% GENERATE THE NEW ALT CODE. %
2670?		DEPOSIT(ABSOLUTELOC, GWD(<'JRST, 0, LOC>));	% FIXUP THE ALT FAILURE TO POINT TO US. %
2680?		MOVEI(1, FAIL _ GENSYM());
2690?		EMIT '(HRRM 1 0 SSTACK);
  2700?		ECALL(PROD?#, T, PROD, NIL);			% CALL THE NEW ALTERNATIVE. %
 2710?		CALL(1, 'NCONS);
2720?		MOVEI(2, Q N);
                      2730?		JUMP SUCCESSLABEL;				% JUMP BACK INTO THE OLD CODE. %
 2740?		LABEL FAIL;
2750?		PUTPROP(ALT, <N+1, LOC, RETURNLOC, PC+!PC, SUCCESSLABEL, !LAST>, 'NEXTALT);
    2760?		JCALL(0, 'ENDOFALT);
    2770?		IF !PCOD THEN RPLACD(!LAST, CDDR ALTCODE)	% PATCH IN THE NEW CODE. %
   2780?			ALSO RPLACD(ALTCODE, CDR !CODE);
   2790?		LAPFN(NIL);
2800?		END;
  2810?	
  2820?	
  2830?	EXPR FIND (N, L);				% RETURNS A LIST WHOSE CAR WAS THE N'TH LIST IN 'L'. %
  2840?		BEGIN  NEW I;
   2850?		FOR I ON L DO IF ATOM CAR I THEN N _ N-1 UNTIL N EQ 0;
                                                                            2860?		IF I THEN RETURN I ELSE FATALERROR("FIND FAILED");	% CAN'T HAPPEN. %
  2870?		END;
  2880?	
  2890?	
  2900?	EXPR BARE (PROD);	% CREATES A BARE SYSTEM CONSISTING OF ONLY THOSE PRODUCTIONS REACHABLE FROM PROD. %
 2910?		IF GET(PROD,'CODE) & (PROD  !PRODUCTIONS) THEN
   2920?			BEGIN  NEW X;				% THE 'X' IS A KLUDGE -- COMPILER BUG. %
   2930?			!PRODUCTIONS _ PROD CONS !PRODUCTIONS;
  2940?			IF GET(PROD,'FIXUP) THEN !FIXUPS _ PROD CONS !FIXUPS;
  2950?			FOR NEW I IN GET(PROD,'CODE) DO
    2960?				IF ATOM I THEN NIL
 2970?				ELSE IF I[1] EQ 'CALL THEN
                                                          2980?					CAR(I _ REVERSE EXPLODE I[3,2]) EQ '?# & BARE(READLIST REVERSE CDR I)
    2990?				ELSE IF I[1] EQ 'MOVEI & ATOM(X _ I[3]) & X[1] EQ 'QUOTE & NUMBERP(X _ X[2])
 3000?					& GET(X,'LITERAL) & (X  !LITERALS) THEN !LITERALS _ X CONS !LITERALS;
  3010?			END;
 3020?	
  3030?	
  3040?	EXPR UPLOC ();  IF (LOC _ LOC+1) EQ BPEND1 THEN FATALERROR("NO BINARY PROGRAM SPACE LEFT");
3050?	
  3060?	EXPR AERR (ALT);  WARNING("CANNOT ADD ALTERNATIVE TO PRODUCTION", ALT);
 3070?	
  3080?	EXPR SYNAM (PROD, PROD?#);
 3090?		IF PROD?# THEN PROD?#
                                                                                3100?		ELSE PUTPROP(PROD, PROG2(SCANRESET(), AT(PROD CAT "#"), SCANSET()), '!PROD?#);
   3110?	
  3120?	EXPR GENAM (PROD, G);  PROG2(PUTPROP(G, GET(G,'PNAME) @ GET(PROD,'PNAME), 'PNAME), INTERN G);
    3130?	
  3140?	END.
                                                                                                                                                                                                                                                                                                                                                                                                                             