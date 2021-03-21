

(DEFPROP BIGINIT 
 (LAMBDA NIL
  (PROG NIL
	(PUTSYM (VBASE (GET (QUOTE BASE) (QUOTE VALUE)))
 		NEGNUM
 		POSNUM
 		FIXNUM
 		FLONUM
		(MINUSP (GET (QUOTE MINUSP) (QUOTE SUBR)))
		(VNOPOI (GET (QUOTE *NOPOINT) (QUOTE VALUE)))))) 
EXPR)

(DEFPROP APNINIT 
 (LAMBDA NIL
  (PROG NIL
	(GETSYM SUBR BIGINI)
	(BIGINI)
	(REMPROP (QUOTE APNINIT) (QUOTE EXPR))
	(REMPROP (QUOTE BIGINIT) (QUOTE EXPR))
	(REMPROP (QUOTE BIGINI) (QUOTE SUBR)))) 
EXPR)

(BIGINIT)                                                                                                                                                                    