0001?	
  0002?	IDENTIFICATION DIVISION.
   0003?	PROGRAM-ID. TYMIO-CBL-VERSION.
  0004?	ENVIRONMENT DIVISION.
 0005?	CONFIGURATION SECTION.
0006?	INPUT-OUTPUT SECTION.
 0007?	FILE-CONTROL.
    0008?	SELECT FOOBAR ASSIGN TO DSK.
    0009?	DATA DIVISION.
   0010?	FILE SECTION.
    0011?	FD FOOBAR   
0012?	VALUE OF IDENTIFICATION IS EXTERNAL-FILE-NAME
  0013?	DATA RECORD IS ZOT.
   0014?	01 ZOT USAGE IS DISPLAY-7.
 0015?	  02 ZOT1 PICTURE 99999999 OCCURS 30 TIMES.
    0016?	WORKING-STORAGE SECTION.
   0017?	77 L1 USAGE INDEX.
    0018?	77 L2 USAGE INDEX.
    0019?	77 EXTERNAL-FILE-NAME PICTURE X(9).
                 0020?	PROCEDURE DIVISION.
   0021?	BEGIN-AT-THE-BEGINNING.
    0022?	DISPLAY "START WRITE".
0023?	        MOVE "GOSTOHELL" TO EXTERNAL-FILE-NAME.
0024?	        OPEN OUTPUT FOOBAR.
0025?	        SET L2 TO 1.
  0026?	LOOP0.
 0027?	        SET L1 TO 1.
  0028?	LOOP1.
 0029?	        COMPUTE ZOT1(L1) = L1 + 30 * L2.
  0030?	        SET L1 UP BY 1.
    0031?	        IF L1 EQUALS 31 NEXT SENTENCE; ELSE GO TO LOOP1.
 0032?	        WRITE ZOT.
    0033?	        SET L2 UP BY 1.
    0034?	        IF L2 EQUALS 1001 NEXT SENTENCE; ELSE GO TO LOOP0.
    0035?	READ-IT-BACK-AND-CHECK-IT.
 0036?	CLOSE FOOBAR.
                        0037?	DISPLAY "FINISH WRITE".
    0038?	OPEN INPUT FOOBAR.
    0039?	DISPLAY "START READ".
 0040?	PERFORM FORNICATION VARYING L2 FROM 1 BY 1 UNTIL L2 EQUALS 1001.
   0041?	FORNICATION.
0042?	        READ FOOBAR;AT END GO TO DONE-AT-LAST.
 0043?	        PERFORM LEWD-ACTS VARYING L1 FROM 1 BY 1 UNTIL L1 EQUALS 31.
    0044?	LEWD-ACTS.
  0045?	        IF ZOT1(L1) EQUALS (L1 + 30 * L2) NEXT SENTENCE; ELSE DISPLAY "TILT".
0047?	DONE-AT-LAST.
    0048?	DISPLAY "SHIP IT!".
   0049?	STOP RUN.
   0050?	
  