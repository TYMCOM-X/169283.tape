0010?	(DEFPROP %DEFIN
  0020?		 (LAMBDA (X V F P)
   0030?			 (PROG (R)
0040?			       (SETQ R
 0050?				     (COND ((GETL X
0060?						  (QUOTE (EXPR FEXPR
  0070?							       SUBR
0080?							       FSUBR
    0090?							       LSUBR
    0100?							       MACRO)))
 0110?					    (LIST X (QUOTE REDEFINED)))
  0120?					   (T X)))
   0130?			       (PUTPROP X (LIST (QUOTE LAMBDA) V F) P)
    0140?			       (RETURN R)))
 0150?		 EXPR)
0160?	
  0170?	(DEFPROP DE
 0180?		 (LAMBDA (L) (%DEFIN (CAR L) (CADR L) (CADDR L) (QUOTE EXPR)))
    0190?		 FEXPR)
    0200?	
  0210?	(DEFPROP DF
                               0220?		 (LAMBDA (L) (%DEFIN (CAR L) (CADR L) (CADDR L) (QUOTE FEXPR)))
   0230?		 FEXPR)
    0240?	
  0250?	(DEFPROP DM
 0260?		 (LAMBDA (L) (%DEFIN (CAR L) (CADR L) (CADDR L) (QUOTE MACRO)))
   0270?		 FEXPR)
    0280?	
  0290?	(DEFPROP PLUS (LAMBDA (L) (*EXPAND L (QUOTE *PLUS))) MACRO)
   0300?	
  0310?	(DEFPROP DIFFERENCE (LAMBDA (L) (*EXPAND L (QUOTE *DIF))) MACRO)
   0320?	
  0330?	(DEFPROP TIMES (LAMBDA (L) (*EXPAND L (QUOTE *TIMES))) MACRO)
 0340?	
  0350?	(DEFPROP QUOTIENT (LAMBDA (L) (*EXPAND L (QUOTE *QUO))) MACRO)
0360?	
  0370?	(DEFPROP LESSP
   0380?		 (LAMBDA (L)
    0390?			 (LIST (QUOTE *LESS)
     0400?			       (*EXPAND1 (CDR (REVERSE (CDR L)))
0410?					 (QUOTE (LAMBDA (X Y)
  0420?							(COND ((AND X (*LESS X Y))
0430?							       Y)))))
   0440?			       (CAR (LAST L))))
  0450?		 MACRO)
    0460?	
  0470?	(DEFPROP GREATERP
0480?		 (LAMBDA (L)
    0490?			 (LIST (QUOTE *GREAT)
    0500?			       (*EXPAND1 (CDR (REVERSE (CDR L)))
0510?					 (QUOTE (LAMBDA (X Y)
  0520?							(COND ((AND X
   0530?								    (*GREAT X Y))
   0540?							       Y)))))
   0550?			       (CAR (LAST L))))
  0560?		 MACRO)
    0570?	
  0580?	(DEFPROP %DEVP
   0590?		 (LAMBDA (X)
                                            0600?			 (OR (EQ (CAR (LAST (EXPLODE X))) (QUOTE :))
 0610?			     (AND (NOT (ATOM X)) (NOT (ATOM (CDR X))))))
  0620?		 EXPR)
0630?	
  0640?	(DE %READCHAN
    0650?	    (%CHAN %TALK)
0660?	    (PROG (%OLDCHAN %SEXPR)
0670?		  (SETQ %OLDCHAN (INC %CHAN NIL))
   0680?	     LOOP (SETQ %SEXPR (ERRSET (READ)))
   0690?		  (COND ((ATOM %SEXPR) (GO END)))
   0700?		  (SETQ %SEXPR (EVAL (CAR %SEXPR)))
 0710?		  (COND (%TALK (PRINT %SEXPR)))
0720?		  (GO LOOP)
0730?	     END  (INC %OLDCHAN T)
 0740?		  (RETURN NIL)))
0750?	
  0760?	(DE %READAFILE
   0770?	    (%DEV %FNAM %TALK)
                                             0780?	    (%READCHAN (EVAL (LIST (QUOTE INPUT) (GENSYM) %DEV %FNAM)) %TALK))
  0790?	
  0800?	(DE READIN
  0810?	    (%DEV %FLIST %TALK)
    0820?	    (PROG NIL
    0830?	     LOOP (COND ((NULL %FLIST) (RETURN (QUOTE FINISHED-LOADING)))
  0840?			((%DEVP (CAR %FLIST)) (SETQ %DEV (CAR %FLIST))
    0850?					      (SETQ %FLIST (CDR %FLIST))
 0860?					      (GO LOOP)))
 0870?		  (%READAFILE %DEV (CAR %FLIST) %TALK)
   0880?		  (SETQ %FLIST (CDR %FLIST))
   0890?		  (GO LOOP)))
   0900?	
  0910?	(DF DSKIN (%L) (READIN (QUOTE DSK:) %L T))
0920?	
  0930?	(DF SYSIN (%L) (READIN (QUOTE SYS:) %L NIL))
   0940?	
            0950?	(DEFPROP PUTSYM
  0960?	 (LAMBDA (L)
0970?		 (MAPCAR (FUNCTION (LAMBDA (X)
 0980?					   (COND ((ATOM X) (*PUTSYM X X))
0990?						 (T (*PUTSYM (CAR X)
  1000?							     (EVAL (CADR X)))))))
 1010?			 L))
 1020?	 FEXPR)
1030?	
  1040?	(DEFPROP GETSYM
  1050?	 (LAMBDA (L)
1060?	  (MAPCAR
   1070?	   (FUNCTION (LAMBDA (X)
   1080?			     (PROG (V)
 1090?				   (SETQ V (*GETSYM X))
 1100?				   (COND (V (PUTPROP X (NUMVAL V) (CAR L)))
 1110?					 (T (PRINT (CONS X
1120?							 (QUOTE (NOT IN
 1130?								     SYMBOL
    1140?								     TABLE))))))
    1150?				   (RETURN V))))
   1160?	   (CDR L)))
     1170?	 FEXPR)
1180?	
  1190?	(DF BREAK
   1200?	    (%LL%)
  1210?	    (PROG (%EX% %ICH% %OCH%)
    1220?		  (SETQ %ICH% (INC NIL NIL))
   1230?		  (SETQ %OCH% (OUTC NIL NIL))
  1240?		  (PRINT (CONS (QUOTE *BREAK*) (CAR %LL%)))
   1250?	     LOOP (TERPRI)
    1260?		  (SETQ %EX% (ERRSET (READ)))
  1270?		  (COND ((ATOM %EX%) (GO LOOP)))
    1280?		  (COND ((EQ (CAR %EX%) *BPROCEED*) (GO END)))
1290?		  (ERRSET (PRINC (EVAL (CAR %EX%))))
1300?		  (GO LOOP)
1310?	     END  (INC %ICH% NIL)
  1320?		  (OUTC %OCH% NIL)
   1330?		  (RETURN (EVAL (CADR %LL%)))))
1340?	
  1350?	(SETQ *BPROCEED* (QUOTE P))
1360?	
       1370?	(DM FILEIN
  1380?	    (L)
1390?	    (PROG NIL
    1400?		  (SYSIN (SOSLNK.LAP))
    1410?		  (REMPROP (QUOTE FILEIN) (QUOTE MACRO))
 1420?		  (RETURN (LIST (QUOTE QUOTE) (EVAL L)))))
    1430?	
  1440?	(PROG (EX)
  1450?	      (SETQ EX
   1460?	       (QUOTE
    1470?		(LAMBDA (L)
1480?			(PROG NIL
 1490?			      (SYSIN LAP)
   1500?			      (MAPC (FUNCTION (LAMBDA (X)
  1510?						      (REMPROP X (QUOTE MACRO))))
    1520?				    (QUOTE (DEFSYM LAP DSYM)))
    1530?			      (RETURN (LIST (QUOTE QUOTE) (EVAL L)))))))
  1540?	      (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X EX (QUOTE MACRO))))
                  1550?		    (QUOTE (LAP DEFSYM DSYM))))
1560?	
  1570?	(PROG (EX)
  1580?	      (SETQ EX
   1590?	       (QUOTE
    1600?		(LAMBDA (L)
1610?			(PROG NIL
 1620?			      (SYSIN TRACE)
 1630?			      (MAPC (FUNCTION (LAMBDA (X)
  1640?						      (REMPROP X (QUOTE MACRO))))
    1650?				    (QUOTE (TRACE TRACET)))
  1660?			      (RETURN (LIST (QUOTE QUOTE) (EVAL L)))))))
  1670?	      (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X EX (QUOTE MACRO))))
   1680?		    (QUOTE (TRACE TRACET))))
   1690?	
  1700?	(DF DECLARE (L) NIL)
  1710?	
  1720?	(COND ((NULL (ERRSET (INPUT INITCHAN DSK: (INIT . LSP)) NIL)))
                    1730?	      (T (%READCHAN (QUOTE INITCHAN) NIL)))
    1740?	
  1750?	(PROG NIL (INC NIL T) (OUTC NIL T) (EXCISE) (CSYM G0000) (ERR))
    1760?	
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            