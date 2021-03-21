(DECLARE CONS RECORDCLASS (LIST) ((CAR PRIVATE NIL) (CDR PRIVATE NIL)) (144 144))

(DECLARE IDENTIFIER RECORDCLASS (IDENTIFIER) ((PNAME PRIVATE (STRING)) (PROPERTIES PRIVATE (LIST))) (24 24))

(DECLARE STACK
         STRUCTURE
         (TYPE)
         NIL
         ((DECLARE TOP STRUCTOP NIL ((S PRIVATE (STACK))) (CAR S))
          (DECLARE POP STRUCTOP NIL ((S PRIVATE (STACK))) (PROG1 (CAR S) (SETQ S (CDR S))))
          (DECLARE PUSH STRUCTOP (1 /]) ((X PRIVATE NIL) (S PRIVATE (STACK))) (SETQ S (CONS X S)))
          (DECLARE INDEX STRUCTOP NIL ((S PRIVATE (STACK)) (SUBS PRIVATE (LIST))) (INDEX S SUBS))
          (DECLARE REPLACE
                   STRUCTOP
                   NIL
                   ((S PRIVATE (STACK)) (SUBS PRIVATE (LIST)) (Y PRIVATE NIL))
                   (REPLACE S SUBS Y))))


(CSYM G0352)                                                                                                                                                                                                                                                                                                                                                                                                                                                   