
(DEFPROP !EXCLUDE T SPECIAL) 

(DEFPROP !REPLACE T SPECIAL) 

(DEFPROP !XEXPRS T SPECIAL) 

(DEFPROP !RULEVARS T SPECIAL) 

(DEFPROP !VARLIST T SPECIAL) 

(DEFPROP QUESTION
         (LAMBDA &ARGS
          (PROG (&VAL &&1 &1 &&2 &2 &&3 &3 &&4 &4)
                (TEMDEC &ARGS)
                (TEMATOM HOW)
                (TEMLOOP &VAL)
                (TEMALT ((STRMCALL ADV &VAL)
                          (TEMCOLON1 &1)
                          (TEMLOOP &VAL)
                          (STRMCALL DO &VAL)
                          (TEMCOLON1 &2)
                          (TEMLOOP &VAL)
                          (STRMCALL SUBJECT &VAL)
                          (TEMCOLON1 &3)
                          (TEMLOOP &VAL)
                          (STRMCALL ACTION &VAL)
                          (TEMCOLON1 &4)
                          (PCALL (QUOTE OPT) T NIL)
                          (TEMREC 1)
                          (STORE &VAL (LOOKUP (COLON &1) (SS (COLON &3) (COLON &4) X)))
                          (RETURN &VAL))
                         ((STRMCALL ADJ &VAL)
                          (TEMCOLON1 &1)
                          (TEMLOOP &VAL)
                          (STRMCALL LINKVERB &VAL)
                          (TEMCOLON1 &2)
                          (TEMLOOP &VAL)
                          (STRMCALL SUBJECT &VAL)
                          (TEMCOLON1 &3)
                          (TEMREC 1)
                          (STORE &VAL (GET (COLON &3) (QUOTE A)))
                          (RETURN &VAL)))))
         XEXPR)

(DEFPROP LAST
         (LAMBDA &ARGS
          (PROG (&VAL &&2 &2)
                (TEMDEC &ARGS)
                (TEMIN LIST)
                (TEMLOOP (TEMIGNORE))
                (TEMCOLON1 &2)
                (TEMOUT LIST)
                (TEMREC 1)
                (STORE &VAL (COLON &2))
                (RETURN &VAL)))
         XEXPR)

(CSYM G0433)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      