   2048: /* IS THE ORIGIN OF THIS PROGRAM */
DECLARE TTO LITERALLY '2', CR LITERALLY '15Q', LF LITERALLY '0AH',
        TRUE LITERALLY '1', FALSE LITERALLY '0';

SQUARE$ROOT: PROCEDURE(X) BYTE;
    DECLARE (X,Y,Z) ADDRESS;
    Y = X; Z = SHR(X+1,1);
        DO WHILE Y <> Z;
        Y = Z; Z = SHR(X/Y + Y + 1, 1);
        END;
    RETURN Y;
    END SQUAREROOT;

PRINT$CHAR: PROCEDURE (CHAR);
    DECLARE BIT$CELL LITERALLY '91',
        (CHAR,I) BYTE;
    OUTPUT (TTO) = 0;
    CALL TIME (BIT$CELL);
        DO I = 0 TO 7;
        OUTPUT(TTO) = CHAR; /* DATA PULSES */
        CHAR = ROR(CHAR,1);
        CALL TIME(BIT$CELL);
        END;
    OUTPUT (TTO) = 1;
    CALL TIME (BIT$CELL+BITCELL);
    /* AUTOMATIC RETURN IS GENERATED */
    END PRINT$CHAR;

PRINT$STRING: PROCEDURE(NAME,LENGTH);
    DECLARE NAME ADDRESS,
        (LENGTH,I,CHAR BASED NAME) BYTE;
        DO I = 0 TO LENGTH - 1;
        CALL PRINT$CHAR(CHAR(I));
        END;
    END PRINT$STRING;

PRINT$NUMBER: PROCEDURE(NUMBER,BASE,CHARS,ZERO$SUPPRESS);
    DECLARE NUMBER ADDRESS, (BASE,CHARS,ZERO$SUPPRESS,I,J) BYTE;
    DECLARE TEMP (16) BYTE;
    IF CHARS > LAST(TEMP) THEN CHARS = LAST(TEMP);
        DO I = 1 TO CHARS;
        J = NUMBER MOD BASE + '0';
        IF J > '9' THEN J = J + 7;
        IF ZERO$SUPPRESS AND I <> 1 AND NUMBER = 0 THEN
            J = ' ';
        TEMP(LENGTH(TEMP)-I) = J;
        NUMBER = NUMBER / BASE;
        END;
    CALL PRINT$STRING(.TEMP + LENGTH(TEMP) - CHARS, CHARS);
    END PRINT$NUMBER;

DECLARE I ADDRESS,
    CRLF LITERALLY 'CR,LF',
    HEADING DATA (CRLF,LF,LF,
    '                        TABLE OF SQUARE ROOTS', CRLF,LF,
    ' VALUE  ROOT VALUE  ROOT VALUE  ROOT VALUE  ROOT VALUE  ROOT',
    CRLF,LF);

    /* SILENCE TTY AND PRINT COMPUTED VALUES */
    OUTPUT(TTO) = 1;
    DO I = 1 TO 1000;
    IF I MOD 5 = 1 THEN
        DO; IF I MOD 250 = 1 THEN
            CALL PRINT$STRING(.HEADING,LENGTH(HEADING));
        ELSE
            CALL PRINTSTRING(.(CR,LF),2);
        END;
    CALL PRINT$NUMBER(I,10,6,TRUE /* TRUE SUPPRESSES LEADING ZEROES */);
    CALL PRINT$NUMBER(SQUARE$ROOT(I), 10,6, TRUE);
    END;

DECLARE MONITOR$USES (10) BYTE;
EOF
    