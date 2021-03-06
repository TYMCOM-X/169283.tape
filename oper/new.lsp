
(DEFPROP SCNVAL T SPECIAL) 

(DEFPROP !SCANVAL T SPECIAL) 

(DEFPROP !SCANTYPE T SPECIAL) 

(DEFPROP !IDTYPE T SPECIAL) 

(DEFPROP !STRTYPE T SPECIAL) 

(DEFPROP !NUMTYPE T SPECIAL) 

(DEFPROP !DELIMTYPE T SPECIAL) 

(DEFPROP !SEXPTYPE T SPECIAL) 

(DEFPROP !PRODUCTIONS T SPECIAL) 

(DEFPROP !LITERALS T SPECIAL) 

(DEFPROP !SPECIALS T SPECIAL) 

(DEFPROP !FIXUPS T SPECIAL) 

(DEFPROP !MISCELLANEOUS T SPECIAL) 

(DEFPROP !INITIALIZED T SPECIAL) 

(DEFPROP !BACKUP T SPECIAL) 

(DEFPROP !SAW# T SPECIAL) 

(DEFPROP !PCOD T SPECIAL) 

(DEFPROP !PROD T SPECIAL) 

(DEFPROP !PROD# T SPECIAL) 

(DEFPROP !CODE T SPECIAL) 

(DEFPROP !PC T SPECIAL) 

(DEFPROP !LAST T SPECIAL) 

(DEFPROP LOC T SPECIAL) 

(DEFPROP CONLIST T SPECIAL) 

(DEFPROP GEN T SPECIAL) 

(DEFPROP REMOB T SPECIAL) 

(DEFPROP KLIST T SPECIAL) 

(DEFPROP BASE T SPECIAL) 

(DEFPROP IBASE T SPECIAL) 

(DEFPROP BPORG T SPECIAL) 

(DEFPROP BPEND T SPECIAL) 

(DEFPROP BPORG1 T SPECIAL) 

(DEFPROP BPEND1 T SPECIAL) 

(DEFPROP OBLIST T SPECIAL) 

(DEFPROP !CONTEXT! T SPECIAL) 

(DEFPROP !CONTEXTLIST! T SPECIAL) 

(DEFPROP PARSE
 (LAMBDA (L)
  (PROG (X RT MT !PCOD !SAW# !BACKUP E RVRS !MISCELLANEOUS !PRODUCTIONS !LITERALS !FIXUPS)
        (SETQ !PCOD T)
        (COND
         (L (COND ((ISDEV (CAR L))
                   (EVAL (LIST (QUOTE INC) (LIST (QUOTE INPUT) (CAR L) (CADR L)) NIL))
                   (SETQ L (CDDR L)))
                  ((CAR L)
                   (EVAL (LIST (QUOTE INC) (LIST (QUOTE INPUT) (QUOTE DSK:) (CAR L)) NIL))
                   (SETQ L (CDR L)))
                  (T (SETQ L (CDR L))))))
        (SETQ RT (DTIME))
        (SETQ MT (TIME))
        (PRINTSTR (QUOTE "* * * * * * * * * * LISP70 !!! * * * * * * * * * * *"))
        (SCANSET)
        (SETQ X (TPROG))
        (SCANRESET)
        (TERPRI (PRINT (TERPRI (QUOTE *****))))
        (PTIME (QUOTE "TRANSLATION TIME:  ") (*DIF (DTIME) RT) (*DIF (TIME) MT))
        (PRINTSTR (CAT (CAR X) (QUOTE " STATE STACK CELLS USED")))
        (PRINTSTR (CAT (CDR X) (QUOTE " TOKEN STACK CELLS USED")))
        (INC NIL T)
        (COND ((NOT L) (RETURN NIL)))
        (TERPRI (PRINTSTR (TERPRI (CAT (QUOTE "PRINTING TRANSLATION ONTO ") (CAT (CAR L) (QUOTE ".LSP"))))))
        (SETQ RT (DTIME))
        (SETQ MT (TIME))
        (EVAL (LIST (QUOTE OUTPUT) (QUOTE DSK:) (CONS (CAR L) (QUOTE LSP))))
        (OUTC T NIL)
        (SETQ BASE 10)
        (PROG (&V ?&LST1 I)
              (SETQ ?&LST1 (SETQ RVRS (REVERSE !MISCELLANEOUS)))
         LOOP (COND ((NULL ?&LST1) (RETURN &V)) (T NIL))
              (SETQ I (CAR ?&LST1))
              (SETQ &V (TERPRI (TERPRI (SPRINT I 1 T))))
              (SETQ ?&LST1 (CDR ?&LST1))
              (GO LOOP))
        (PROG (&V ?&LST1 I)
              (SETQ ?&LST1 RVRS)
         LOOP (COND ((NULL ?&LST1) (RETURN &V)) (T NIL))
              (SETQ I (CAR ?&LST1))
              (SETQ &V (COND ((GET (CADR I) (QUOTE &APPLY)) (SPRINT (GET (CADR I) (QUOTE &APPLY)) 1 T))))
              (SETQ ?&LST1 (CDR ?&LST1))
              (GO LOOP))
        (PRINT (LIST (QUOTE CSYM) (GENSYM)))
        (OUTC NIL T)
        (SETQ BASE 12)
        (PTIME (QUOTE "PRINTING TIME:  ") (*DIF (DTIME) RT) (*DIF (TIME) MT))))
FEXPR)

(REMPROP (QUOTE <) (QUOTE LITERAL))

(REMPROP (QUOTE >) (QUOTE LITERAL))

(DEFPROP < LESSP &INFIX)

(DEFPROP < 454 &LEFT)

(DEFPROP < 536 &RIGHT)

(DEFPROP > GREATERP &INFIX)

(DEFPROP > 454 &LEFT)

(DEFPROP > 536 &RIGHT)

(CSYM G0322)                                                                                                                                                                                                                                                                      