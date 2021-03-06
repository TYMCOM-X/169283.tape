$ID
-ID PHONEY.
-REM CONVERTS A PHONE NUMBER TO A LIST OF ALL
      ALPHABETIC COMBINATIONS REPRESENTING IT.
$ED
-IO
-FC
        -SEL LISTING,-AT DSK.
        -SEL SORTING, -AT DSK,DSK,DSK.
$DD
-FS
FD    LISTING,VID= "PHONESLST".
01    PHONES,-UD7.
        02 PHONE, -OCC 7 -T, P= X.
        02 FP= X(29).
01    MULTI-PHONES,-UD7.
      02 M-PH, -OCC 3 -T, P= X(12).
SD    SORTING.
01    SORT-REC.
      02 S-R,P= X(7).
      02 FP= X(29).
-WS
01      ALPHA,P= X(30) -V "000111ABCDEFGHIJKLMNOPRSTUVWXY".
01    CHARS, -RD ALPHA.
        02 CHAR -OCC 10 -T.
                03 C,-OCC 3 -T P= X.
01    NUMBR.
        02 N, -OCC 8 -T P= X.
01    INDXS.
        02 INDX,-OCC 7 -T P= 9.
01    PHONE-STORE.
      02 PH-STORE,P= X(7).
        02 FP= X(29).
77      I P= 99 -UC.
77      J P= 99 -UC.
77      K P= 99 -UC.
77      L P= 99 -UC.
$PD

START.
        -DIS "TYPE PHONE-NUMBER".
        -MS NUMBR.
        -ACC NUMBR.
        -PER GET-NUM -VAR K FROM 4 BY 1 UNTIL K -GT 7.
        MOVE 1111111 TO INDXS.
        MOVE 1 TO I.
        -OP OUTPUT LISTING.
        -MS PHONES.
LOOP.
        -PER ASSEMBLE -VAR J FROM 1 BY 1 UNTIL J -GT 7.
        -W PHONES.
        -MS PHONES.
        -PER BUMP-INDX THRU BX.
        -G LOOP.
END-IT.
        -CL LISTING.
        SORT SORTING -OAK SORT-REC,
                -U LISTING,
                -OUTP THREE-PHONES.
        -SR
*       SUBROUTINES
GET-NUM.
        MOVE K TO J.
        SET J -U1.
        MOVE N (J) TO N (K).
ASSEMBLE.
        MOVE INDX (J) TO K.
        MOVE N (J) TO L.
        ADD 1 TO L.
        MOVE C (L, K) TO PHONE (J).
BUMP-INDX.
        MOVE 7 TO J.
B2.     ADD 1 TO INDX (J).
        IF INDX (J) -GT 3 AND J -EQ 1,GO TO END-IT.
        IF INDX (J) -GT 3,        MOVE 1 TO INDX (J),
        SET J -D1
        -G B2.
BX.     EXIT.
*       OUTPUT PROCEDURE FOR SORT
THREE-PHONES SECTION.
START.
        -OP OUTPUT LISTING.
L-1.
        MOVE 1 TO I.
        -MS MULTI-PHONES.
L-2.
        -RET SORTING INTO PHONE-STORE, -AEG END-3-PHONES.
        MOVE PH-STORE TO M-PH (I).
        SET I -U1.
        IF I -GT 3,     WRITE MULTI-PHONES,
        -G L-1.
        ELSE -G L-2.
END-3-PHONES.
        IF I -GT 1,-W MULTI-PHONES.
        -CL LISTING.
 