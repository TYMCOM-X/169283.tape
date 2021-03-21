;       TEST.CMD
WIRRAP, COM, BD, RESIDE, WIR, TABLES, @TEST1, ;
                %H(BIGJOHN)FIOCS.REL/NOCOMPILE, ;
        [BOARD ! QUIT, KLUTZ ! PLGFCN, ;
                [PLUG ! LIST, ;
                        [ DOCMEN! PLUGIT, LINKIT, ;
                                [ENTER ! CHANGE  ;
                                ] ;
                        ] ;
                ] ! START, START1/INITIAL ;
        ]
