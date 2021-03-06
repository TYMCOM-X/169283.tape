IDENTIFICATION DIVISION.
PROGRAM-ID. UNIDIR.
AUTHOR. WL HARDY.
REMARKS. DECTAPE DIRECTORY MERGE AND SORT PROGRAM.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
OBJECT-COMPUTER. PDP-10; MEMORY SIZE IS 5 MODULES.
SPECIAL-NAMES.
	CHANNEL (1) IS TO-NEW-PAGE.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT DIR-IN ASSIGN TO DSK.
	SELECT DIR-OUT ASSIGN TO DSK.
	SELECT SORT-FILE ASSIGN TO DSK, DSK, DSK.

DATA DIVISION.
FILE SECTION.

FD DIR-IN
	DATA RECORDS ARE RECORD-IN, FIXER
	VALUE OF IDENTIFICATION IS INPUT-DIR.

01 RECORD-IN DISPLAY-7.
	02 FILIN	PIC	X(6).
	02 DOTIN	PIC	X.
	02 EXTIN	PIC	XXX.
	02 SPIN		PIC	X.
	02 BLOCKS-IN	PIC	9(3).
	02 FILL-IN	PIC	X(9).
	02 TEST		PIC	XX.
	02 FAKER	PIC	XXX.
	02 FAKER-1	PIC	XXX.

01 FIXER DISPLAY-7.
	02 FIRST-CHAR	PIC	X.
	02 RANDOM	PIC	XXXX.
	02 I-D		PIC	XXX.
	02 SP-1		PIC	X.
	02 TAPEID	PIC	X(6).

FD DIR-OUT
	DATA RECORDS ARE RECORD-OUT, DUMP-LAYOUT, HEADER-BLOCK,
	DUMMY-OUT, DATE-AND-TIME, BLANK-LINE
	VALUE OF IDENTIFICATION IS OUTPUT-DIR.

01 RECORD-OUT DISPLAY-7.
	02 FILOUT	PIC	X(6).
	02 DOTOUT	PIC	X.
	02 EXTOUT	PIC	XXX.
	02 SPOUT	PIC	X.
	02 BLOCK-OUT	PIC	XXX.
	02 FILL-OUT	PIC	X(9).
	02 TEST-OUT	PIC	XX.
	02 TAB-ULATE	PIC	XXX.
	02 TAP-NUM	PIC	XXX.

01 DUMP-LAYOUT DISPLAY-7.
	02 TAPE-NAM	PIC	X(7).
	02 NUMBER-1	PIC	XXX.
	02 SPACE-1	PIC	XX.
	02 FILE-1	PIC	XX.
	02 FILE-2	PIC	X(7).
	02 BLOCK-1	PIC	XXX.
	02 BLOCK-2	PIC	X(12).
	02 IDHEAD	PIC	X(11).
	02 IDOUT	PIC	X(6).

01 HEADER-BLOCK DISPLAY-7.
	02 HEAD-OUT	PIC	X(8).
	02 HEADING-1	PIC	X(21).

01 DUMMY-OUT DISPLAY-7.
	02 FILIN	PIC	X(6).
	02 DOTIN	PIC	X.
	02 EXTIN	PIC	XXX.
	02 SPIN		PIC	X.
	02 BLOCKS-IN	PIC	XXX.
	02 FILL-IN	PIC	X(9).
	02 TEST		PIC	XX.
	02 FAKER	PIC	XXX.
	02 FAKER-1	PIC	XXX.

01 DATE-AND-TIME, DISPLAY-7.
	02 K0	PIC	X(9).
	02 TIME-OUT.
		03 K1	PIC	XX.
		03 K2	PIC	X.
		03 K3	PIC	XXX.
		03 K4	PIC	X.
		03 K5	PIC	XX.
		03 K6	PIC	XX.
		03 K7	PIC	XX.
		03 K8	PIC	X.
		03 K9	PIC	XX.
		03 K10	PIC	X.
		03 K11	PIC	XX.

01 BLANK-LINE	PIC	X, DISPLAY-7.

SD SORT-FILE
	DATA RECORD IS SORT-RECORD.

01 SORT-RECORD DISPLAY-7.
	02 SORT-ON	PIC	X(10).
	02 FILLER	PIC	X(18).
	02 TAP-NUMB	PIC	XXX.

WORKING-STORAGE SECTION.

01 HOLD-MONTH, DISPLAY-7.
	02 FILLER PIC X(18)  VALUE IS "JANFEBMARAPRMAYJUN".
	02 FILLER PIC X(18) VALUE IS "JULAUGSEPOCTNOVDEC".

01 MONTH1 REDEFINES HOLD-MONTH, DISPLAY-7.
	02 MONTH; OCCURS 12 TIMES; PIC XXX.

01 KLUNK, DISPLAY.
	02 YY	PIC	XX.
	02 MO	PIC	XX.
	02 DD	PIC	XX.
	02 HH	PIC	XX.
	02 MM	PIC	XX.
	02 SS	PIC	XX.

01 INPUT-DIR DISPLAY-7.
	02 INPUT-NAME		PIC	X(6).
	02 INPUT-EXTENSION	PIC	XXX VALUE "DIR".

01 OUTPUT-DIR DISPLAY-7.
	02 OUTPUT-NAME		PIC	X(6) VALUE "UNIDIR".
	02 OUTPUT-EXTENSION	PIC	XXX VALUE "DIR".

01 MISC-DATA DISPLAY-7.
	02 DMP-FIL-OUT	PIC	X(10) VALUE IS SPACES.
	02 DMP-HD-1	PIC	X(21) VALUE IS "DIRECTORY INFORMATION".
	02 DUMP-TAPE	PIC	X(7) VALUE "TAPE  #".
	02 DUMP-SPACE-1	PIC	XX VALUE "  ".
	02 DUMP-FILE-2	PIC	X(7) VALUE " FILES ".
	02 DUMP-BLOCK-2	PIC	X(12) VALUE " BLOCKS FREE".
	02 THIS-NUMBER	PIC	9.
	02 BLK-FIX	PIC	XXX.
	02 SPAC-NUMB	PIC	XXX VALUE IS "  #".
	02 TAPCNT	PIC	99.
	02 TAPNUM	PIC	99.

01 WORKER.
	02 WORK-1	PIC	X.
	02 WORK-2	PIC	X(5).

01 MISC-ITEMS-SIX.
	02 LINE-COUNT	PIC	99.
	02 TMP-BLK	PIC	999.
	02 TOT-BLK	PIC	9(3) OCCURS 200 TIMES.
	02 NUL-FIL	PIC	99   OCCURS 200 TIMES.

01 XTEST DISPLAY-7.
	02 XNAME	PIC	X(6).
	02 XDOT		PIC	X VALUE ".".
	02 XEXT		PIC	XXX.
77	CHECK-NAM	PIC	X(6).
77	MON-IND		PIC	99.
77	SAVNUM		PIC	999	DISPLAY-7.
77	NEWNUM		PIC	999	DISPLAY-7.
77	LAST-CHAR	PIC	X	DISPLAY-7.
77	I		PIC	S999	COMP.
77	CRFLAG		PIC	9	VALUE 0.
77	DEL-FILE	PIC	XXX	VALUE "BAD"	DISPLAY-7.
77	ATTEMPT		PIC	XXX	VALUE "NO "	DISPLAY-7.
77	IDENT		PIC	X(6) OCCURS 200 TIMES DISPLAY-7.
PROCEDURE DIVISION.

INIT. MOVE ZEROS TO MISC-ITEMS-SIX. SET I TO 1.
	ENTER MACRO TYPOUT. MOVE TODAY TO KLUNK.
	EXAMINE MO REPLACING LEADING SPACES BY ZEROS.
	MOVE MO TO MON-IND. MOVE MONTH(MON-IND) TO K3. MOVE DD
	TO K1. MOVE YY TO K5. MOVE HH TO K7. MOVE MM TO K9. MOVE
	SS TO K11.  MOVE "-" TO K2, K4. MOVE ":" TO K8, K10.
	MOVE SPACES TO K0, K6. DISPLAY TIME-OUT.
	DISPLAY "NUMBER OF DIRECTORIES TO BE MERGED? " WITH NO ADVANCING.
	ACCEPT TAPNUM.
	DISPLAY "DELETE EXISTING DIR FILES? " WITH NO ADVANCING.
WANTON.	ACCEPT DEL-FILE.
	IF DEL-FILE = "Y" OR "YE" OR "YES" MOVE "YES" TO DEL-FILE.
	IF DEL-FILE = "N" OR "NO" MOVE "NO" TO DEL-FILE.
	IF DEL-FILE = "YES" OR "NO" GO TO OK.
	IF ATTEMPT = "YES" DISPLAY "HEY DING DONG " WITH NO ADVANCING.
	DISPLAY "YES OR NO? " WITH NO ADVANCING.
	MOVE "YES" TO ATTEMPT. GO TO WANTON.
OK.	OPEN OUTPUT DIR-OUT.
	MOVE SPACES TO BLANK-LINE.

START. MOVE I TO WORKER.

LOOP. IF WORK-1 NOT EQUAL ZERO GO TO WORK-DONE.  MOVE WORK-2 TO
	WORKER.  GO TO LOOP.

WORK-DONE. MOVE WORKER TO INPUT-NAME.  MOVE INPUT-NAME TO
	CHECK-NAM. ENTER MACRO CHECK USING CHECK-NAM.
	IF CHECK-NAM EQUALS SPACES GO TO SETUP.
	MOVE SPACES TO IDENT(I).

OPENUP. OPEN INPUT DIR-IN.

CONT.	READ DIR-IN RECORD AT END GO TO END-DIR.
	IF I-D = "ID:" MOVE TAPEID TO IDENT(I).
	IF TEST IN RECORD-IN IS ALPHABETIC GO TO CONT.  MOVE BLOCKS-IN IN
	RECORD-IN TO BLK-FIX.  EXAMINE BLK-FIX REPLACING ALL
	SPACES BY ZEROES.  EXAMINE BLK-FIX REPLACING ALL " " BY
	ZEROES.  MOVE BLK-FIX TO TMP-BLK.  ADD TMP-BLK TO
	TOT-BLK(I). ADD 1 TO NUL-FIL(I).


	MOVE FILIN IN RECORD-IN TO FILOUT. MOVE DOTIN
	IN RECORD-IN TO DOTOUT. MOVE EXTIN IN RECORD-IN TO EXTOUT.
	MOVE SPIN IN RECORD-IN TO SPOUT.

	MOVE BLOCKS-IN IN RECORD-IN TO
	BLOCK-OUT.  MOVE FILL-IN IN RECORD-IN TO FILL-OUT.  MOVE
	TEST IN RECORD-IN TO TEST-OUT.	MOVE SPAC-NUMB TO
	TAB-ULATE.  MOVE I TO TAP-NUM.	EXAMINE TAP-NUM REPLACING
	LEADING ZEROS BY SPACES.  WRITE RECORD-OUT.  GO TO CONT.

END-DIR. CLOSE DIR-IN. IF DEL-FILE = "YES" ENTER MACRO DELFIL USING CHECK-NAM.

SETUP. SET I UP BY 1.	IF I < TAPNUM OR = TAPNUM GO TO START.

SORT-ROUT. CLOSE DIR-OUT.  MOVE "UNIDIR" TO INPUT-NAME.  MOVE
	"DIR" TO INPUT-EXTENSION.  MOVE "SRT" TO
	OUTPUT-EXTENSION.  SORT SORT-FILE ON ASCENDING KEY
	SORT-ON, TAP-NUMB USING DIR-IN GIVING DIR-OUT.  MOVE "SRT"
	TO INPUT-EXTENSION.  OPEN INPUT DIR-IN.  MOVE "SRT" TO
	OUTPUT-EXTENSION.  OPEN OUTPUT DIR-OUT. MOVE SPACE TO
	BLANK-LINE. WRITE BLANK-LINE AFTER ADVANCING 3 LINES.
	MOVE 4 TO LINE-COUNT. MOVE 1 TO THIS-NUMBER.
	READ DIR-IN RECORD AT END GO TO DATA-BANK.
	MOVE FILIN IN RECORD-IN TO XNAME.
	MOVE FIRST-CHAR TO LAST-CHAR. MOVE EXTIN IN RECORD-IN
	TO XEXT. MOVE FAKER-1 IN RECORD-IN TO SAVNUM.
	GO TO CONTST-1.

READ-OUT. READ DIR-IN RECORD AT END GO TO DATA-BANK.
 IF FILIN IN RECORD-IN IS NOT EQUAL TO XNAME OR EXTIN IN
	RECORD-IN IS NOT EQUAL TO
	XEXT GO TO CONTST-1.
	MOVE FAKER-1 IN RECORD-IN TO NEWNUM.
	IF CRFLAG = 0 PERFORM CRLF.
	DISPLAY "DUPLICATION - TAPES ",SAVNUM," & ",NEWNUM," ",XTEST.

CONTST-1. ADD 1 TO LINE-COUNT.
	IF FIRST-CHAR > LAST-CHAR PERFORM SKIP. IF LINE-COUNT > 55
	GO TO OP-1.  MOVE
	CORR RECORD-IN TO DUMMY-OUT.  WRITE DUMMY-OUT AFTER
	ADVANCING THIS-NUMBER LINES.
	MOVE FILIN IN RECORD-IN TO XNAME. MOVE EXTIN IN RECORD-IN
	TO XEXT. MOVE FAKER-1 IN RECORD-IN TO SAVNUM.
	GO TO READ-OUT.

OP-1. SET THIS-NUMBER TO 3.  MOVE SPACE TO BLANK-LINE.  WRITE
	BLANK-LINE AFTER ADVANCING TO-NEW-PAGE.	MOVE CORR
	RECORD-IN TO DUMMY-OUT.  WRITE DUMMY-OUT AFTER ADVANCING
	THIS-NUMBER LINES.  SET LINE-COUNT TO 4.  SET THIS-NUMBER
	TO 1.  GO TO READ-OUT.

DATA-BANK. MOVE SPACE TO BLANK-LINE. WRITE BLANK-LINE AFTER
	ADVANCING TO-NEW-PAGE. MOVE SPACES TO HEAD-OUT. MOVE DMP-HD-1 TO
	HEADING-1.  WRITE HEADER-BLOCK AFTER ADVANCING
	3 LINES. MOVE SPACE TO BLANK-LINE.
	WRITE BLANK-LINE AFTER ADVANCING 1 LINE.
	MOVE SPACES TO DATE-AND-TIME.
	MOVE MONTH(MON-IND) TO K3. MOVE DD
	TO K1. MOVE YY TO K5. MOVE HH TO K7. MOVE MM TO K9. MOVE
	SS TO K11.  MOVE "-" TO K2, K4. MOVE ":" TO K8, K10.
	WRITE DATE-AND-TIME BEFORE ADVANCING 1 LINE.
	MOVE 3 TO THIS-NUMBER. MOVE 1 TO I.

LOOPER. IF I > TAPNUM GO TO GO-AWAY.

DUMP-ARRAY. MOVE DUMP-TAPE TO TAPE-NAM.  MOVE DUMP-SPACE-1 TO
	SPACE-1.  MOVE DUMP-FILE-2 TO FILE-2.  MOVE DUMP-BLOCK-2
	TO BLOCK-2.  MOVE I TO NUMBER-1.  EXAMINE NUMBER-1
	REPLACING LEADING ZEROES BY SPACES.  IF NUL-FIL(I) = ZERO
	GO TO SPECIAL-DUMP.  COMPUTE NUL-FIL(I) = 22 -
	NUL-FIL(I).  MOVE NUL-FIL(I) TO FILE-1.  IF NUL-FIL(I)
	NOT EQUAL TO ZERO GO TO STAN-DARD.  EXAMINE FILE-1
	REPLACING FIRST ZERO BY SPACE.	GO TO COMP-BLOCK.

STAN-DARD. EXAMINE FILE-1 REPLACING LEADING ZEROS BY SPACES.

COMP-BLOCK. COMPUTE TOT-BLK(I) = 574 - TOT-BLK(I).  MOVE
	TOT-BLK(I) TO BLOCK-1.	IF TOT-BLK(I) = 0 GO TO FIX-ZERO.
	EXAMINE BLOCK-1 REPLACING LEADING ZEROS BY SPACES.  GO TO
	GO-AROUND.

FIX-ZERO. EXAMINE BLOCK-1 REPLACING FIRST ZERO BY SPACE.
	EXAMINE BLOCK-1 REPLACING FIRST ZERO BY SPACE. GO TO
	GO-AROUND.

SPECIAL-DUMP. MOVE "DI" TO FILE-1. MOVE "RECTORY" TO FILE-2.
	MOVE " FO" TO BLOCK-1. MOVE "UND EMPTY .." TO BLOCK-2.

GO-AROUND. IF IDENT(I) = SPACES MOVE SPACES TO IDHEAD,IDOUT ELSE
	MOVE " /TAPE ID: " TO IDHEAD, MOVE IDENT(I) TO IDOUT.

	WRITE DUMP-LAYOUT AFTER ADVANCING THIS-NUMBER LINES.
	SET THIS-NUMBER TO 1. ADD 1 TO I.
	GO TO LOOPER.

GO-AWAY. MOVE SPACE TO BLANK-LINE. WRITE BLANK-LINE AFTER
	ADVANCING 1 LINE. CLOSE DIR-IN. ENTER MACRO HACK.
	DISPLAY "PLEASE PRINT UNIDIR.SRT".
	STOP RUN.

SKIP.	MOVE SPACE TO BLANK-LINE. WRITE BLANK-LINE
	AFTER ADVANCING 1 LINE. MOVE FIRST-CHAR TO LAST-CHAR.
	ADD 1 TO LINE-COUNT.

CRLF.	DISPLAY SPACE. MOVE 1 TO CRFLAG.
   