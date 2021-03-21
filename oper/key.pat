C...KEYST
C...   DRIVER FOR ALPHA SORT RTN
C..BODY 'A' WILL SORT INTO BODY 'B'
C    IN THE MAIN PROGRAM
C..BODY TO BE SORTED HAS 'NROW' LINES,'JB' ITEMS/LINE
C..WE WILL SORT ON THE 'JK'TH ITEM
C..
C.."KEYST" SUBRTN WILL CONSTRUCT A (KEY)SORTING DIRECTOR SHOWING THE
C   SEQUENCE OF THE SUBSCRIPTS OF THE ITEMS DESIRED.
C..    KEY (A NEEDED WORK ARRAY) IS DIM. (LINES)
C..    A & B ARE DIM. ( LINES,ITEMS)
C..
	DIMENSION A(50,15),B(50,15),KEY(50)
	JB=15
C...SET KEY WORD TO SORT ON
	JK  =1
	NROW=20
	CALL IFILE(1,'ALF')
C
	DO 20 I=1,NROW
20	READ(1,120)(A(I,J),J=1,JB)
120	FORMAT(15A1)
	CALL  KEYST(NROW,JB,JK,A,KEY)
C...USING KEY AS GUIDE, LOADUP B-ARRAY WITH SORTED A
110	DO 150 I=1,NROW
	IK=KEY(I)
	DO 150 J=1,JB
	B(I,J)= A(IK,J)
150	CONTINUE
C
	WRITE(5,152)((B(I,J),J=1,JB),I=1,NROW)
150	CONTINUE
C
152	FORMAT(/1X,15A1)
	STOP
	END
	SUBROUTINE KEYST (NROW,JB,KS,A,KEY)
C...SORTING ROUTINE -
C...	 LONG RECORDS - 6 OR MORE WORDS EACH
C...	 ASCENDING ORDER
	DIMENSION A(50,15), B(50,15), KEY(50), ITEM(50)
	INTEGER A,B
C
C...ISOLATE THE KS(KEYSORT) COLUMN
C
	IX=0
	DO 10 I=1,N
	IX=IX+ I
	IF(IX-N) 10,10,40
10	M=2*IX-1
C
40	M=M/2
	IF ( M ) 44, 20,44
44	K= N-M
C
	DO 51 J=1,K
	DO 30I=J,1,-M
	IF(A(I+M)- A(I)) 47, 51, 51
47	W   =A(I)
	A(I)= A(I+M)
	A(I+M) =W
30	CONTINUE
51	CONTINUE
	GO TO 40
	RETURN
	END
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      