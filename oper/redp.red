4600?	%*********************************************************************
  4601?	%*********************************************************************
  4602?	%                     HIGH ENERGY PHYSICS PACKAGE
   4607?	%*********************************************************************
  4608?	%********************************************************************;
  4609?	
  4610?	
  4610?	%*********************************************************************
  4610?	%             GLOBAL VARIABLES REFERENCED IN THIS PACKAGE
4610?	%********************************************************************;
  4610?	
       4610?	TYPL!* := UNION('(VECTORP),TYPL!*);
  4611?	GAMIDEN!*	:= NIL;
4611?	INDICES!* := NIL;	%list of indices in High Energy Physics
4611?				%tensor expressions;
    4611?	% SUBFG!*	
  4611?	% IVARS!*		
 4612?	% !*NCMP		;
 4612?	
  4612?	
  4612?	%*********************************************************************
  4613?	%                         SOME DECLARATIONS
    4615?	%********************************************************************;
  4700?	
  4701?	DEFLIST ('((CONS SIMPDOT)),'SIMPFN);
 4702?	
  4703?	LISP PROCEDURE VECTOR U;
   4703?	   VECTOR1 U;
    4703?	
  4703?	LISP PROCEDURE VECTOR1 U;
       4704?	   MAP(U,FUNCTION (LAMBDA J; PUT(CAR J,'VECTOR,'VECTOR)));
    4704?	
  4704?	PUT('VECTOR,'STAT,'OPSTAT);
4704?	
  4704?	PUT('VECTOR,'FN,'VECFN);
   4704?	LISP PROCEDURE VECTORP U;
  4705?	   NSP(U,'VECTOR);
    4705?	
  4705?	PUT('VECTORP,'LETFN,'NSLET);
    4705?	
  4705?	PUT('VECTORP,'NAME,'VECTOR);
    4706?	
  4706?	PUT('VECTORP,'EVFN,'VEVAL);
4706?	
  4706?	LISP PROCEDURE INDEX U;
    4706?	   BEGIN VECTOR1 U; INDICES!* := UNION(INDICES!*,U) END;
 4706?	
  4706?	LISP PROCEDURE REMIND U;
   4707?	   BEGIN INDICES!* := SETDIFF(INDICES!*,U) END;
4707?	
  4707?	LISP PROCEDURE MASS U;
                    4707?	   BEGIN
    4707?	   A:	IF NULL U THEN RETURN NIL;
4707?		PUT(CADAR U,'MASS,CADDAR U);
   4707?		PUT(CADAR U,'VECTOR,'VECTOR);
  4708?		U := CDR U;
4708?		GO TO A
    4708?	   END;
4708?	
  4708?	LISP PROCEDURE MSHELL U;
   4709?	   BEGIN SCALAR X,Z;
  4709?	    A:   IF NULL U THEN RETURN LET0(Z,NIL);
    4709?		 X := GETMAS CAR U;
  4709?		 Z := LIST('EQUAL,LIST('CONS,CAR U,CAR U),LIST('TIMES,X,X)) . Z;
  4710?		 U := CDR U;
    4710?		 GO TO A
   4710?	   END;
4710?	
  4711?	DEFLIST ('((MSHELL RLIS) (MASS RLIS) (INDEX RLIS) (REMIND RLIS)),
  4711?	  'STAT);
   4711?	
                                     4712?	%*********************************************************************
  4712?	%          FUNCTIONS FOR SIMPLIFYING HIGH ENERGY EXPRESSIONS
  4712?	%********************************************************************;
  4713?	
  4713?	LISP PROCEDURE VEVAL U;
    4714?	   BEGIN SCALAR Z;
    4714?		U := NSSIMP(U,'VECTOR,FUNCTION VECTORP);
 4715?	    A:	IF NULL U THEN RETURN REPLUS Z
4715?		 ELSE IF NULL CDAR U THEN REDERR "MISSING VECTOR"
  4716?		 ELSE IF CDDAR U THEN REDERR LIST("REDUNDANT VECTOR",CDAR U);
4716?		Z := ACONC(Z,RETIMES(PREPSQ2 CAAR U . CDAR U));
    4717?		U := CDR U;
4717?		GO TO A
              4718?	   END;
4718?	
  4720?	
  4720?	LISP PROCEDURE ISIMPQ U;
   4721?	   ISIMP CAR U . CDR U;
    4721?	
  4722?	LISP PROCEDURE ISIMP U;
    4722?	   ISIMP1(U,INDICES!*,NIL,NIL,NIL);
  4723?	
  4723?	LISP PROCEDURE ISIMP1(U,I,V,W,X);
    4724?	   IF NULL U THEN NIL
 4724?	    ELSE IF NUMBB U
   4725?	       THEN IF V OR X THEN REDERR APPEND("UNMATCHED INDEX ERROR",I)
4725?		     ELSE IF W THEN MULTF(EMULT W,ISIMP1(U,I,V,NIL,X))
  4726?		     ELSE U
4726?	    ELSE ADDF(ISIMP2(CAR U,I,V,W,X),ISIMP1(CDR U,I,V,W,X));
   4727?	
  4732?	LISP PROCEDURE ISIMP2(U,I,V,W,X);
    4733?	   BEGIN SCALAR Z;
                   4734?		IF ATOM (Z := CAAR U) THEN GO TO A
  4735?		 ELSE IF CAR Z EQ 'CONS AND XN(CDR Z,I)
  4736?		    THEN RETURN DOTSUM(U,I,V,W,X)
   4737?		 ELSE IF CAR Z EQ 'G THEN RETURN SPUR0(U,I,V,W,X)
  4738?		 ELSE IF CAR Z EQ 'EPS THEN RETURN ESUM(U,I,V,W,X);
4739?	    A:  RETURN MULTF2(CAR U,ISIMP1(CDR U,I,V,W,X))
  4740?	   END;
4741?	
  4742?	LISP PROCEDURE DOTSUM(U,I,V,W,X);
    4743?	   BEGIN SCALAR I1,N,U1,U2,V1,Y,Z;
   4744?		N := CDAR U;
    4745?		IF NOT (CAR (U1 := CDAAR U) MEMBER I) THEN U1 := REVERSE U1;
 4746?		U2 := CADR U1;
  4747?		U1 := CAR U1;
   4748?		V1 := CDR U;
                                       4749?		IF N=2 THEN GO TO H ELSE IF NOT ONEP N THEN REDERR U;
   4750?	    A:  IF NOT (U1 MEMBER I)
    4751?		    THEN RETURN MULTF(MKDOT(U1,U2),ISIMP1(V1,I1,V,W,X));
4752?	    A1: I1 := DELETE(U1,I);
4753?		IF U1 EQ U2 THEN RETURN MULTN(4,ISIMP1(V1,I1,V,W,X))
    4754?		 ELSE IF NOT (Z := ASSOC(U1,V)) THEN GO TO C
  4755?		 ELSE IF U2 MEMBER I THEN GO TO D;
  4756?		U1 := CDR Z;
    4757?		GO TO E;
   4758?	    C:  IF Z := MEMLIS(U1,X)
    4759?		    THEN RETURN SPUR0((('G . SUBST(U2,U1,Z)) . 1) . V1,
 4760?				      I1,
4761?				      V,
 4762?				      W,
 4763?				      DELETE(Z,X))
                               4764?		 ELSE IF Z := MEMLIS(U1,W)
4765?		    THEN RETURN ESUM((('EPS . SUBST(U2,U1,Z)) . 1) . V1,
4766?				     I1,
 4767?				     V,
  4768?				     DELETE(Z,W),
  4769?				     X)
  4770?		 ELSE IF U2 MEMBER I AND NULL Y THEN GO TO G;
 4771?		RETURN ISIMP1(V1,I,(U1 . U2) . V,W,X);
   4772?	    D:  U1 := U2;
4773?		U2 := CDR Z;
    4774?	    E:  I := I1;
 4775?		V := DELETE(Z,V);
    4776?		GO TO A;
   4777?	    G:  Y := T;
  4778?		Z := U1;
   4779?		U1 := U2;
  4780?		U2 := Z;
   4781?		GO TO A1;
  4782?	    H:  IF U1 EQ U2 THEN REDERR U;
   4783?		I := I1 := DELETE(U1,I);
  4784?		U1 := U2;
                 4785?		GO TO A
    4786?	   END;
4787?	
  4788?	LISP PROCEDURE VMULT U;
    4789?	   BEGIN SCALAR Z;
    4791?		Z := LIST LIST '(1 . 1);
  4792?	    A:	IF NULL U THEN RETURN Z;
 4793?		Z := VMULT1(NSSIMP(CAR U,'VECTOR,FUNCTION VECTORP),Z);
  4794?		IF NULL Z THEN RETURN NIL;
4795?		U := CDR U;
4796?		GO TO A
    4797?	   END;
4798?	
  4799?	LISP PROCEDURE VMULT1(!*S!*,V);
 4800?	   BEGIN SCALAR Z;
    4801?		IF NULL V THEN RETURN NIL;
4802?	    A:	IF NULL !*S!* THEN RETURN Z
   4803?		 ELSE IF CDDAR !*S!* THEN REDERR("REDUNDANT VECTOR" . CDAR !*S!*);
4804?		Z := NCONC(Z,MAPCAR(V,FUNCTION (LAMBDA J;
               4805?		      MULTSQ(CAR J,CAAR !*S!*) . APPEND(CDR J,CDAR !*S!*))));
4806?		!*S!* := CDR !*S!*;
  4807?		GO TO A
    4807?	   END;
4807?	
  4809?	LISP PROCEDURE SIMPDOT U;
  4810?	   MKVARG(U,FUNCTION DOTORD);
   4811?	
  4812?	LISP PROCEDURE DOTORD U;
   4813?	   PROG2(IF XN(U,INDICES!*) AND NOT MEMBER('ISIMPQ,MUL!*)
4814?		   THEN MUL!* := ACONC(MUL!*,'ISIMPQ) ELSE NIL,
    4814?		 MKSQ('CONS . ORD2(CAR U,CARX CDR U),1));
4815?	
  4816?	LISP PROCEDURE MKVARG(U,!*PI!*);
4817?	   BEGIN SCALAR Z;
    4818?		U := VMULT U;
   4819?		Z := NIL . 1;
   4820?	    A:  IF NULL U THEN RETURN Z;
                              4821?		Z := ADDSQ(MULTSQ(!*PI!* CDAR U,CAAR U),Z);
   4822?		U := CDR U;
4823?		GO TO A
    4824?	   END;
4825?	
  4826?	LISP PROCEDURE MKDOT(U,V);
 4827?	   MKSF('CONS . ORD2(U,V),1);
   4828?	
  4855?	LISP PROCEDURE GETMAS U;
   4856?	   (LAMBDA X; IF X THEN X ELSE REDERR LIST(U,"HAS NO MASS"))
  4858?	      GET!*(U,'MASS);
 4859?	
  4873?	
  4874?	%*********************************************************************
  4874?	%           FUNCTIONS FOR SIMPLIFYING DIRAC GAMMA MATRICES
    4874?	%********************************************************************;
  4875?	
  4876?	PUT('G,'SIMPFN,'SIMPGAMMA);
     4877?	
  4877?	FLAGOP NONCOM,NOSPUR;
 4877?	
  4878?	FLAG ('(G),'NONCOM);
  4879?	
  4880?	LISP PROCEDURE SPUR U;
4880?	   PROG2(RMSUBS(),
    4880?		 MAP(U,
    4880?		     FUNCTION (LAMBDA J;
  4880?			   PROG2(REMFLAG(LIST CAR J,'NOSPUR),
   4881?				 REMFLAG(LIST CAR J,'REDUCE)))));
 4881?	
  4881?	PUT('SPUR,'STAT,'RLIS);
    4881?	
  4882?	LISP PROCEDURE GMULT(U,V);
 4883?	   IF NOT (CDR U=1) OR NOT (CDR V=1) THEN ERRACH LIST('GMULT,U,V)
  4885?	    ELSE IF NOT (CADAR U EQ CADAR V) THEN 'FAILED
   4886?	    ELSE GCHECK(REVERSE CDDAR U,CDDAR V,CADAR U);
   4887?	
  4888?	DEFLIST ('((G GMULT)),'MRULE);
       4889?	
  4904?	LISP PROCEDURE SIMPGAMMA !*S!*;
 4905?	   IF NULL !*S!* OR NULL CDR !*S!*
   4906?	       THEN REDERR "MISSING ARGUMENTS FOR G OPERATOR"
    4907?	    ELSE BEGIN
   4908?		GAMIDEN!* := UNION(LIST CAR !*S!*,GAMIDEN!*);
 4908?		IF NOT GET(CAR !*S!*,'NOSPUR) AND NOT MEMBER('ISIMPQ,MUL!*)
  4908?		  THEN MUL!* := ACONC(MUL!*,'ISIMPQ);
    4909?		!*NCMP := T;
    4910?		RETURN MKVARG(CDR !*S!*,FUNCTION (LAMBDA J;
   4912?					 GCHECK(REVERSE J,NIL,CAR !*S!*) . 1))
4914?	    END;
    4915?	
  4916?	LISP PROCEDURE GCHECK(U,V,L);
   4917?	   IF CAR V EQ 'A THEN GCHKA(U,CDR V,T,L) ELSE GCHKV(U,V,T,L);
          4918?	
  4919?	LISP PROCEDURE GCHKA(U,V,X,W);
  4920?	   IF NULL U THEN MULTN(NB X,MKG('A . V,W))
    4921?	    ELSE IF CAR U EQ 'A  THEN GCHKV(CDR U,V,X,W)
    4922?	    ELSE GCHKA(CDR U,CAR U . V,NOT X,W);
  4923?	
  4924?	LISP PROCEDURE GCHKV(U,V,X,L);
  4925?	   IF NULL U THEN IF NULL V THEN NB X ELSE MULTN(NB X,MKG(V,L))
    4926?	    ELSE IF CAR U EQ 'A  THEN GCHKA(CDR U,V,X,L)
    4927?	    ELSE GCHKV(CDR U,CAR U . V,X,L);
 4927?	
  4927?	
  4927?	%*********************************************************************
  4927?	%       FUNCTIONS FOR COMPUTING TRACES OF DIRAC GAMMA MATRICES
                         4927?	%********************************************************************;
  4928?	
  4929?	LISP PROCEDURE MKG(U,L);
   4930?	   LIST ((('G . (L . U)) . 1) . 1);
  4931?	
  4932?	LISP PROCEDURE MKA L;
 4933?	   MKG(LIST 'A,L);
    4934?	
  4934?	LISP PROCEDURE MKG!*(U,L);
 4934?	   MKG1(U,L);
    4934?	
  4935?	LISP PROCEDURE MKG1(U,L);
  4936?	   IF NOT FLAGP(L,'NOSPUR) OR NULL CDR U OR CDDR U OR ORDOP(CAR U,
 4937?								     CADR U)
   4938?		OR CAR U EQ 'A  THEN MKG(U,L)
  4939?	    ELSE ADDF(MULTN(2,MKDOT(CAR U,CADR U)),
    4940?		      MULTN(-1,MKG(REVERSE U,L)));
  4941?	
                                4942?	LISP PROCEDURE NB U;
  4943?	   IF U THEN 1 ELSE -1;
    4944?	
  4945?	LISP PROCEDURE SPUR0(U,I,V1,V2,V3);
  4946?	   BEGIN SCALAR L,V,W,I1,Z,KAHP;
4947?		L := CADAAR U;
  4948?		V := CDDAAR U;
  4949?		IF NOT ONEP CDAR U THEN V := APPN(V,CDAR U);
  4950?		U := CDR U;
4951?		IF GET(L,'NOSPUR) THEN GO TO A
 4952?		ELSE IF (CAR V EQ 'A AND (LENGTH V<5 OR EVENP V))
  4953?		        OR (NOT (CAR V EQ 'A) AND NOT EVENP V)
4954?		 THEN RETURN NIL
4954?		ELSE IF NULL I THEN GO TO END0;
4955?	    A:  IF NULL V THEN GO TO END1 ELSE IF CAR V MEMBER I THEN
 4956?		    GO TO B;
    4957?	    A1: W := CAR V . W;
         4958?		V := CDR V;
4959?		GO TO A;
   4960?	    B:  IF CAR V MEMBER CDR V THEN GO TO KAH1
  4961?		 ELSE IF CAR V MEMBER I1 THEN GO TO A1
   4962?		 ELSE IF Z := BASSOC(CAR V,V1) THEN GO TO E
   4963?		 ELSE IF Z := MEMLIS(CAR V,V2)
 4964?		    THEN RETURN (LAMBDA X;
4965?			IF FLAGP(L,'REDUCE) AND NULL V1 AND NULL V3
  4965?			   AND NULL CDR V2
  4966?			 THEN MULTF(MKG!*(X,L),MULTF(MKEPS1 Z,ISIMP U))
   4967?			ELSE ISIMP1(SPUR0(CAAR MKG(X,L) . U,
    4968?					  NIL,
  4969?					  V1,
   4970?					  DELETE(Z,V2),
   4971?					  V3),
  4972?				    I,
   4973?				    NIL,
 4974?				    LIST Z,
                  4975?				    NIL))
4976?		    (APPEND(REVERSE W,V))
 4977?		 ELSE IF Z := MEMLIS(CAR V,V3) THEN GO TO C
   4978?		 ELSE RETURN ISIMP1(U,I,V1,V2,(L . APPEND(REVERSE W,V)) . V3);
    4983?	    C:  V3 := DELETE(Z,V3);
4984?		KAHP := NIL;
    4985?		IF FLAGP(L,'NOSPUR) AND FLAGP(CAR Z,'NOSPUR) THEN REDERR 'HELP
    4987?		 ELSE IF FLAGP(CAR Z,'NOSPUR) THEN KAHP := CAR Z;
  4988?		Z := CDR Z;
4989?		I1 := NIL;
 4990?	    C1: IF CAR V EQ CAR Z THEN GO TO D;
   4991?		I1 := CAR Z . I1;
    4992?		Z := CDR Z;
4993?		GO TO C1;
  4994?	    D:  Z := CDR Z;
   4995?		I := DELETE(CAR V,I);
4996?		V := CDR V;
                    4997?		IF NOT FLAGP(L,'NOSPUR) THEN GO TO D0;
   4998?		W := W . (V . (I1 . Z));
  4999?		I1 := CAR W;
    5000?		Z := CADR W;
    5001?		V := CADDR W;
   5002?		W := CDDDR W;
   5003?	    D0: W := REVERSE W;
    5004?		IF (NULL V OR NOT (CAR W EQ 'A)) AND (V := APPEND(V,W))
 5005?		 THEN GO TO D1
  5005?		ELSE IF NOT EVENP V THEN U := MULTN(-1,U);
    5007?		V := 'A . APPEND(V,CDR W);
5008?	    D1: IF KAHP THEN L := KAHP;
 5009?		IVARS!* := NIL;
 5010?		Z := MULTF(MKG(REVERSE I1,L),
  5011?			   MULTF(BRACE(V,L,I),MULTF(MKG1(Z,L),U)));
  5012?		Z := ISIMP1(Z,APPEND(IVARS!*,I),V1,V2,V3);
                             5013?		IF NULL Z THEN RETURN Z
   5014?		 ELSE IF NULL (Z := QUOTF(Z,2))
5015?		    THEN ERRACH LIST('SPUR0,U,I,V1,V2,V3);
    5016?		RETURN Z;
  5017?	    E:  V1 := DELETE(Z,V1);
5018?		I := DELETE(CAR W,I);
5019?		V := OTHER(CAR V,Z) . CDR V;
   5020?		GO TO A;
   5021?	    KAH1:IF CAR V EQ CADR V THEN GO TO K2;
5022?		KAHP := T;
 5023?		I1 := CAR V . I1;
    5024?		GO TO A1;
  5025?	    K2: I := DELETE(CAR V,I);
   5026?		V := CDDR V;
    5027?		U := MULTN(4,U);
5028?		GO TO A;
   5029?	    END0:W := REVERSE V;
   5030?	    END1:IF KAHP THEN GO TO END2
                                                            5031?		  ELSE IF NULL (Z := SPURR(W,L,NIL,1)) THEN RETURN NIL
  5032?		  ELSE RETURN IF GET('EPS,'KLIST) AND NOT FLAGP(L,'NOSPUR)
   5032?			    THEN ISIMP1(MULTF(Z,U),I,V1,V2,V3)
  5032?			   ELSE MULTF(Z,ISIMP1(U,I,V1,V2,V3));
  5033?	    END2:IVARS!* := NIL;
   5034?		Z := MULTF(KAHANE(REVERSE W,I1,L),U);
    5035?		RETURN ISIMP1(Z,APPEND(IVARS!*,SETDIFF(I,I1)),V1,V2,V3)
 5036?	   END;
5037?	
  5038?	LISP PROCEDURE APPN(U,N);
  5039?	   IF ONEP N THEN U ELSE APPEND(U,APPN(U,SUB1 N));
  5040?	
  5041?	LISP PROCEDURE OTHER(U,V);
 5042?	   IF U EQ CAR V THEN CDR V ELSE CAR V;
   5043?	
                                5044?	LISP PROCEDURE KAHANE(U,I,L);
   5044?	   %The Kahane algorithm for Dirac matrix string reduction
    5044?	   %Ref: Kahane, J., Journ. Math. Phys. 9 (1968) 1732-1738;
   5044?	   BEGIN SCALAR K,M,P,R,V,W,X,Y,Z;
   5045?		K := 0;
    5045?	    MARK:
   5045?		IF EQCAR(U,'A) THEN GO TO A1;
  5046?	    A:  P := NOT P;		%vector parity;
 5046?		IF NULL U THEN GO TO D ELSE IF CAR U MEMBER I THEN GO TO C;
  5047?	    A1: W := ACONC(W,CAR U);
    5047?	    B:  U := CDR U;
   5048?		GO TO A;
   5048?	    C:  Y := CAR U . P;
    5049?		Z := (X . (Y . W)) . Z;
   5049?		X := Y;
    5050?		W := NIL;
                      5050?		K := K+1;
  5051?		GO TO B;
   5051?	    D:  Z := (NIL . (X . W)) . Z;
    5052?		%BEWARE ... END OF STRING HAS OPPOSITE CONVENTION;
 5052?	    PASS2:
  5052?		M := 1;
    5053?	    L1: IF NULL Z THEN GO TO L9;
5053?		U := CAAR Z;
    5054?		X := CADAR Z;
   5054?		W := CDDAR Z;
   5055?		Z := CDR Z;
5055?		M := M+1;
  5056?		IF NULL U THEN GO TO L2
   5056?		 ELSE IF (CAR U EQ CAR X) AND EXC(X,CDR U) THEN GO TO L7;
    5057?		W := REVERSE W;
 5057?		R := T;
    5058?	    L2: P := NOT EXC(X,R);
 5058?		X := CAR X;
5059?		Y := NIL;
  5059?	    L3: IF NULL Z THEN ERRACH "UNMATCHED INDEX IN KAHANE"
          5060?		  ELSE IF (X EQ CAR (I := CADAR Z)) AND NOT EXC(I,P)
    5060?		   THEN GO TO L5 
    5061?		  ELSE IF (X EQ CAR (I := CAAR Z)) AND EXC(I,P) THEN GO TO L4;
    5061?		Y := CAR Z . Y;
 5062?		Z := CDR Z;
5062?		GO TO L3;
  5063?	    L4: X := CADAR Z;
 5063?		W := APPR(CDDAR Z,W);
5064?		R := T;
    5064?		GO TO L6;
  5065?	    L5: X := CAAR Z;
  5065?		W := APPEND(CDDAR Z,W);
   5066?		R := NIL;
  5066?	    L6: Z := APPR(Y,CDR Z);
5067?		IF NULL X THEN GO TO L8
   5067?		 ELSE IF NOT EQCAR(U,CAR X) THEN GO TO L2;
    5068?	    L7: IF W AND CDR U THEN W := ACONC(CDR W,CAR W);
                                   5068?		V := MULTF(BRACE(W,L,NIL),V);	%V := ('BRACE . L . W) . V;
    5069?		GO TO L1;
  5069?	    L8: V := MKG(W,L);			%V := LIST('G . L . W);
    5070?		Z := REVERSE Z;
 5070?		K := K/2;
  5071?		GO TO L1;
  5071?	    L9: U := 2**K;
    5072?		IF NOT (REMAINDER(K-M,2) = 0) THEN U :=  - U;
 5072?		RETURN MULTN(U,V)		%RETURN 'TIMES . U . V;
    5073?	   END;
5073?	
  5074?	LISP PROCEDURE APPR(U,V);
  5074?	   IF NULL U THEN V ELSE APPR(CDR U,CAR U . V);
5075?	
  5075?	LISP PROCEDURE EXC(U,V);
   5076?	   IF NULL CDR U THEN V ELSE NOT V;
  5076?	
  5109?	LISP PROCEDURE BRACE(U,L,I);
                                  5110?	   IF NULL U THEN 2
   5111?	    ELSE IF XN(I,U) OR FLAGP(L,'NOSPUR)
   5111?	     THEN ADDF(MKG1(U,L),MKG1(REVERSE U,L))
    5112?	    ELSE IF CAR U EQ 'A
    5113?	       THEN IF EVENP U THEN MULTN(-2,SPRAU(CDR U,L))
5114?		     ELSE MULTF(MKA L,SPR2(CDR U,L,2,NIL))
    5115?	    ELSE IF EVENP U THEN SPR2(U,L,2,NIL)
  5115?	    ELSE SPR1(U,L,2,NIL);
  5116?	
  5117?	LISP PROCEDURE SPR1(U,L,N,B);
   5118?	   IF NULL U THEN NIL
 5119?	    ELSE IF NULL CDR U THEN MULTN(N,MKG1(U,L))
 5120?	    ELSE BEGIN SCALAR M,X,Z;
    5121?		       X := U;
  5122?		       M := 0;
  5123?		  A:   IF NULL X THEN RETURN Z;
     5124?		       Z := ADDF(MULTF(MKG1(LIST CAR X,L),
    5125?				       IF NULL B THEN SPURR(REMOVE(U,M),L,NIL,N)
 5127?					ELSE SPR1(REMOVE(U,M),L,N,NIL)),
 5128?				 Z);
5129?		       X := CDR X;
   5130?		       N :=  - N;
    5131?		       M := ADD1 M;
  5132?		       GO TO A
  5133?	    END;
    5134?	
  5135?	LISP PROCEDURE SPR2(U,L,N,B);
   5136?	   IF NULL CDDR U AND NULL B THEN MULTN(N,MKDOT(CAR U,CADR U))
5137?	    ELSE (LAMBDA X; IF B THEN ADDF(SPR1(U,L,N,B),X) ELSE X)
   5139?	       ADDF(SPURR(U,L,NIL,N),
   5140?		     MULTF(MKA L,SPURR(APPEND(U,LIST 'A),L,NIL,N)));
    5141?	
                           5142?	LISP PROCEDURE SPRAU(U,L);
 5143?	   PROG2(IVARS!* := LIST GENSYM(),
   5144?		 MULTF(MKG1(IVARS!*,L),
   5145?		       ISIMP1(SPURR(APPEND(IVARS!*,APPEND(U,LIST 'A)),L,NIL,1),
   5146?			      IVARS!*,
 5147?			      NIL,
5148?			      NIL,
5149?			      NIL)));
  5150?	
  5151?	LISP PROCEDURE EVENP U;
    5152?	   NULL U OR NOT EVENP CDR U;
   5153?	
  5154?	LISP PROCEDURE BASSOC(U,V);
5155?	   IF NULL V THEN NIL
 5156?	    IF U EQ CAAR V OR U EQ CDAR V THEN CAR V
   5157?	    ELSE BASSOC(U,CDR V);
  5158?	
  5159?	LISP PROCEDURE MEMLIS(U,V);
5160?	   IF NULL V THEN NIL
                               5161?	    ELSE IF U MEMBER CAR V THEN CAR V
5161?	    ELSE MEMLIS(U,CDR V);
  5162?	
  5163?	LISP PROCEDURE SPURR(U,L,V,N);
  5164?	   BEGIN SCALAR M,W,X,Y,Z;
 5165?	    A:  IF NULL U THEN GO TO B ELSE IF CAR U MEMBER CDR U THEN
5166?		    GO TO G;
    5167?		V := CAR U . V;
 5168?		U := CDR U;
5169?		GO TO A;
   5170?	    B:  IF NULL V THEN RETURN N
 5171?		 ELSE IF FLAGP(L,'NOSPUR) THEN RETURN MULTN(N,MKG!*(V,L))
    5172?		 ELSE RETURN SPRGEN(V,N);
 5173?	    G:  X := CAR U;
   5174?		Y := CDR U;
5175?		W := Y;
    5176?		M := 0;
    5177?	    H:  IF X EQ CAR W
                                                   5178?		    THEN RETURN ADDF(MULTF(MKDOT(X,X),
   5179?					   SPURR(DELETE(X,Y),L,V,N)),
    5180?				     Z);
 5181?		Z := ADDF(MULTF(MKDOT(X,CAR W),SPURR(REMOVE(Y,M),L,V,2*N)),
  5183?			  Z);
5184?		W := CDR W;
5185?		N :=  - N;
 5186?		M := ADD1 M;
    5187?		GO TO H
    5188?	   END;
5189?	
  5190?	LISP PROCEDURE SPRGEN(V,N);
5191?	   BEGIN SCALAR X,Z;
  5192?		IF NOT (CAR V EQ 'A) THEN RETURN SPRGEN1(V,N)
 5193?		 ELSE IF NULL (X := COMB(V := CDR V,4)) THEN RETURN NIL
 5194?		ELSE IF NULL CDR X THEN GO TO E;
    5195?	    C:  IF NULL X THEN RETURN MULTF2(MKSP('I,1),Z);
                                         5196?		Z := ADDF(MULTN(ASIGN(CAR X,V,N),
   5197?				MULTF(MKEPS1 CAR X,
5198?				      SPRGEN1(SETDIFF(V,CAR X),1))),
   5199?			  Z);
5200?	    D:  X := CDR X;
   5201?		GO TO C;
   5202?	    E:  Z := MULTN(N,MKEPS1 CAR X);
  5203?		GO TO D
    5204?	   END;
5205?	
  5205?	LISP PROCEDURE ASIGN(U,V,N);
    5205?	   IF NULL U THEN N ELSE ASIGN(CDR U,V,ASIGN1(CAR U,V,-1)*N);
 5205?	
  5205?	LISP PROCEDURE ASIGN1(U,V,N);
   5205?	   IF U EQ CAR V THEN N ELSE ASIGN1(U,CDR V,-N);
    5205?	
  5206?	LISP PROCEDURE SPRGEN1(U,N);
    5207?	   IF NULL U THEN NIL
                                                             5208?	    ELSE IF NULL CDDR U THEN MULTN(N,MKDOT(CAR U,CADR U))
5209?	    ELSE BEGIN SCALAR W,X,Y,Z;
  5210?		       X := CAR U;
   5211?		       U := CDR U;
   5212?		       Y := U;
  5213?		  A:   IF NULL U THEN RETURN Z
 5214?			ELSE IF NULL (W := MKDOT(X,CAR U)) THEN GO TO B;
  5215?		       Z := ADDF(MULTF(W,SPRGEN1(DELETE(CAR U,Y),N)),Z);
5216?		  B:   N :=  - N;
    5217?		       U := CDR U;
   5218?		       GO TO A
  5219?	    END;
    5219?	
  5219?	
  5219?	%*********************************************************************
  5219?	%                    FUNCTIONS FOR EPSILON ALGEBRA
                      5220?	%********************************************************************;
  5220?	
  5220?	
  5220?	PUT('EPS,'SIMPFN,'SIMPEPS);
5221?	
  5221?	LISP PROCEDURE COMB(U,N);
  5222?	   %value is list of all combinations of N elements from the list U;
    5223?	   BEGIN SCALAR V; INTEGER M;
   5224?		IF (M:=LENGTH U-N) < 0 THEN RETURN NIL;
  5225?	    A:  IF M=0 THEN RETURN U . V;
    5226?		V := NCONC(V,MAPCONS(COMB(CDR U,N-1),CAR U));
 5227?		U := CDR U;
5228?		M := M-1;
  5229?		GO TO A
    5230?	   END;
5235?	
  5236?	LISP PROCEDURE SIMPEPS U;
  5237?	   MKVARG(U,
5238?		  FUNCTION (LAMBDA J;
                    5239?			(IF REPEATS J THEN NIL ELSE MKEPS1 J) . 1));
 5240?	
  5241?	LISP PROCEDURE MKEPS1 U;
   5241?	   PROG2(IF XN(U,INDICES!*) AND NOT MEMBER('ISIMPQ,MUL!*)
5241?		   THEN MUL!* := ACONC(MUL!*,'ISIMPQ) ELSE NIL,
    5242?	     (LAMBDA X; MULTN(NB PERMP(X,U),MKSF('EPS . X,1))) ORDN U);
    5245?	
  5251?	LISP PROCEDURE ESUM(U,I,V,W,XX);
5252?	   BEGIN SCALAR X,Y,Z;
5253?		X := CAR U;
5254?		U := CDR U;
5255?		IF NOT ONEP CDR X
    5256?		 THEN U := MULTF(NMULTF(MKEPS1 CDAR X,SUB1 CDR X),U);
   5259?		X := CDAR X;
    5260?	    A:  IF REPEATS X THEN RETURN NIL;
                                                       5261?	    B:  IF NULL X THEN RETURN ISIMP1(U,I,V,REVERSE Y . W,XX)
  5262?		 ELSE IF NOT (CAR X MEMBER I) THEN GO TO D
    5263?		 ELSE IF NOT (Z := BASSOC(CAR X,V)) THEN GO TO C;
  5264?		V := DELETE(Z,V);
    5265?		I := DELETE(CAR X,I);
5266?		X := APPEND(REVERSE Y,OTHER(CAR X,Z) . CDR X);
5267?		Y := NIL;
  5268?		GO TO A;
   5269?	    C:  IF Z := MEMLIS(CAR X,W) THEN GO TO C1
  5270?		 ELSE IF Z := MEMLIS(CAR X,XX)
 5271?		    THEN RETURN SPUR0((('G . Z) . 1) . U,
5272?				      I,
 5273?				      V,
 5274?				      APPEND(REVERSE Y,X) . W,
    5275?				      DELETE(Z,XX));
                                       5276?		RETURN ISIMP1(U,I,V,APPEND(REVERSE Y,X) . W,XX);
   5277?	    C1: X := APPEND(REVERSE Y,X);
    5278?		Y := XN(I,XN(X,Z));
  5279?		RETURN ISIMP1(MULTF(EMULT1(Z,X,Y),U),
    5280?			      SETDIFF(I,Y),
 5281?			      V,
  5282?			      DELETE(Z,W),
  5283?			      XX);
5284?	    D:  Y := CAR X . Y;
    5285?		X := CDR X;
5286?		GO TO B
    5287?	   END;
5288?	
  5289?	LISP PROCEDURE EMULT U;
    5290?	   IF NULL CDR U THEN MKEPS1(CAR U,1)
5291?	    ELSE IF NULL CDDR U THEN EMULT1(CAR U,CADR U,NIL)
    5292?	    ELSE MULTF(EMULT1(CAR U,CADR U,NIL),EMULT CDDR U);
   5293?	
                                     5294?	LISP PROCEDURE EMULT1(U,V,I);
   5295?	   (LAMBDA (X,!*S!*);
 5296?		 (LAMBDA (M,N);
 5297?		       IF M=4 THEN 24*N
   5298?			ELSE IF M=3 THEN MULTN(6*N,MKDOT(CAR X,CAR !*S!*))
5300?			ELSE MULTN(N*(IF M = 0 THEN 1 ELSE M),
  5301?				   CAR DETQ MAPLIST(X,
  5302?					FUNCTION (LAMBDA !*S1!*;
    5304?					   MAPLIST(!*S!*,
 5305?					      FUNCTION (LAMBDA J;
   5307?						 MKDOT(CAR !*S1!*,CAR J)
   5308?						 . 1))))))
  5312?		    (LENGTH I,
  5313?		     (LAMBDA J;
 5314?			   NB IF PERMP(U,APPEND(I,X)) THEN NOT J ELSE J)
  5315?			PERMP(V,APPEND(I,!*S!*))))
                                            5316?	      (SETDIFF(U,I),SETDIFF(V,I));
   5317?	
  5317?	
  5387?	END OF HIGH ENERGY PHYSICS PACKAGE;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    n 5?