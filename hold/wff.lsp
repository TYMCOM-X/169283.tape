(DE ISBOUND (Y) (MEMBER Y VAR))

(DE ASG NIL
 (PROG2 (PROG2
  (SETQ PRED (LIST @P @Q))
   (SETQ VAR (LIST @X @Y @Z))
    (PRINT @(WFF: )))
    (PRINT (WFF (READ))) (ASG]

(DE WFF (L)
 (COND
  ((ATOM (CAR L)) (COND
    ((NULL L) T)
    ((OR (EQ (CAR L) @NO) (EQ (CAR L) @NEG)) (WFF (CDDR L)))
    ((AND (OR (EQ (CAR L) @EXIST) (EQ (CAR L) @E)) (ISBOUND (CADR L)))
     (WFF (CDDR L)))
    ((AND (EQ (CAR L) @ALL) (ISBOUND (CADR L))) (WFF (CDDR L)))
    ((AND (MEMBER (CAR L) PRED) (ISBOUND (CADR L))) (WFF (CDDR L)))
    (T NIL)))
   ((EQUAL (LIST (CAR L)) L) (WFF (CAR L)))
   (T (AND (WFF (CAR L)) (EQ (CADR L) @N) (WFF (CDDR L)))]
 