File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

1)1	00010	TITLE PIP V.033(77)
1)	00020	SUBTTL	VJC/PMH/AK-DAG/DMN	04-APR-72
****
2)1	00020	SUBTTL	VJC/PMH/AK-DAG/DMN	04-APR-72
**************
1)1	00070	VPIP==33		;VERSION NUMBER
1)	00080	VUPDATE==0		;DEC UPDATE LEVEL
1)	00090	VEDIT==77		;EDIT NUMBER
1)	00100	VCUSTOM==0		;NON-DEC UPDATE LEVEL
1)	00110	
1)	00120		LOC 124
****
2)1	00061	; DEFINE THE VERSION NUMBER --
2)	00070		TYMVER==7	;TYMSHARE VERSION NUMBER
2)	00080		TYMREL==0	;TYMSHARE RELEASE NUMBER
2)	00090		DECVER==33	;DEC VERSION NUMBER
2)	00100		DECREL==77	;DEC RE LEASE NUMBER
2)	00105	DEFINE TITL(TV,TR,DV,DR)<
2)	00107	TITLE PIP -- VERSION TV'.'TR'-'DV'.'DR
2)	00108	VLISTER==BYTE(9)DV,TV,DR,TR>
2)	00110	
2)	00112		TITL (\TYMVER,\TYMREL,\DECVER,\DECREL)
2)	00120		LOC 124
**************
1)1	00160		<VCUSTOM>B2+<VPIP>B11+<VUPDATE>B17+VEDIT
1)	00170		RELOC
****
2)1	00160	JOBVER:	VLISTER
2)	00170		RELOC
**************
1)1	00310	
1)	00320	
****
2)1	00310	;TYMSW==1 TYMSHARE VERSION
2)	00320	
**************
1)1	00430	
****
2)1	00425	IFNDEF TYMSW,	<TYMSW==1>
2)	00430	
**************
1)20	00420		LOOKUP IN,ZRF
****
2)20	00411	IFN TYMSW,<
2)	00412		PUSHJ	P,LKSIZ		;RE-FORMAT FOR EXTENDED LOOKUP
2)	00418	>
2)	00420		LOOKUP IN,ZRF
**************
1)20	00500		PUSHJ P,OKBLKS
****
2)20	00492	IFN TYMSW,<
2)	00494		PUSHJ	P,WLDOUT	;MAKE SURE /X WITH WILD CHAR!
2)	00496	>
2)	00500		PUSHJ P,OKBLKS
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

**************
1)20	00560		ENTER	OUT,DTON	;CREATE OUTPUT FILE
1)	00570		  JRST	ERR4		;DIR. FULL OR 0 FILE NAME
1)	00580		JRST	PSCANA
1)	00590	
1)	00600	MAINA5:
****
2)20	00551	IFN TYMSW,<
2)	00552		SKIPE	DTON		;OUTPUT FILE GIVEN?
2)	00554		PUSHJ	P,ENTSIZ	;YES, ACCEPT EXTENDED ENTER
2)	00567	>
2)	00569		ENTER	OUT,DTON	;CREATE OUTPUT FILE
2)	00570		  JRST	ERR4		;DIR. FULL OR 0 FILE NAME
2)	00572	IFN TYMSW,<
2)	00574		MOVE	DTON+11		;# OF BLKS ALLOCATED TO OUTFILE
2)	00576		CAMGE	DTON+10		;DID WE GET ENOUGH BLOCKS?
2)	00577		JRST	[CLOSE	OUT,40	;NO, CLOSE THE OUTPUT FILE
2)	00578			JRST	ERRSIZ	;HE LOSES
2)	00579			]
2)	00580	>
2)	00588		JRST	PSCANA
2)	00590	
2)	00600	IFN TYMSW,<
2)	00610	;THIS ROUTINE REFORMATS INFORMATION IN THE LOOKUP BLOCK SO
2)	00620	;INTO THE PROPER FORMAT FOR AN EXTENDED LOOKUP.  (ONE ARGUMENT
2)	00630	;RETURNED BY THE EXTENDED LOOKUP IS THE FILE SIZE IN WORDS --- 
2)	00640	;THIS QUANTITY MOD '200 (PLUS ONE) CAN THEN BE USED AS THE
2)	00650	;ESTIMATED FILE SIZE WHEN COPYING TO AN OUTPUT FILE IS DONE.
2)	00660	LKSIZ:	MOVEI	11		;***'11 WORDS IN EXTENDED LOOKUP
2)	00670		MOVEM	ZRF		;*** BEGINNING OF LOOKUP BLOCK
2)	00672		TLNN	FL,MFLG		;*** ARE WE MASKING??
2)	00674		JRST	.+3		;*** NO
2)	00676		MOVE	FNPPN		;*** RETRIEVE OLD PPN
2)	00678		SKIPA			;*** (DON'T CLOBBER IT)
2)	00680		MOVE	PP		;*** PROJ-PROG NUMBER
2)	00690		MOVEM	ZRF+1
2)	00700		MOVE	[FILNAM,,ZRF+2]	;*** FILNAM, EXT
2)	00710		BLT	ZRF+4		;*** COMPLETES EXTENDED BLOCK
2)	00720		POPJ	P,
2)	00730	
2)	00740	
2)	00750	;THE FOLLOWING ROUTINE RE-FORMATS THE INFORMATION IN THE ENTER
2)	00760	;BLOCK (DTON) FOR AN "EXTENDED" ENTER IN WHICH THE EXPECTED
2)	00770	;FILE SIZE IS GIVEN -- THIS FACILITATES COPYING OF LARGE FILES
2)	00780	ENTSIZ:	MOVEI	11		;WORD COUNT FOR EXTNDED LOOKUP
2)	00790		EXCH	DTON		;MOVE COUNT INTO DTON
2)	00800		EXCH	DTON+2		;MOVE NAME TO DTON+2
2)	00810		EXCH	DTON+4		;MOVE PROTECTION INTO DTON+4
2)	00820		MOVE	DTON+1		;FETCH EXTENSION
2)	00830		EXCH	DTON+3		;MOVE EXTENSION TO DTON+3
2)	00840		MOVEM	DTON+1		;MOVE PPN TO DTON+1
2)	00850		MOVE	ZRF+11		;INPUT FILE'S SIZE IN BLOCKS
2)	00860		MOVEM	DTON+10		;IS ESTIMATED BLKSIZ FOR OUTFILE
2)	00930		POPJ	P,		;RETURN AFTER RE-FORMATTING
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

2)	00940	
2)	00950	
2)	00960	;THE FOLLOWING ROUTINE ZEROES OUT THE ARGUMENT BLOCKS FOR THE
2)	00970	;EXTENDED LOOKUP AND ENTER UUOS.
2)	00980	
2)	00990	BLKZRO:	SETZM	ZRF		;ZERO FIRST LOC IN LOOKUP BLK
2)	01000		MOVE	[ZRF,,ZRF+1]
2)	01010		BLT	ZRF+11		;ZERO THE REST
2)	01020		MOVE	[ZRF,,DTON]
2)	01030		BLT	DTON+11		;ZEROES THE ENTER ARG. BLOCK
2)	01040		POPJ	P,		;RETURN
2)	01042	
2)	01044	
2)	01046	;THE FOLLOWING ROUTINE TESTS FOR THE USE OF WILD CARD
2)	01048	;CONSTRUCTS IN THE OUTPUT FILE NAME WITHOUT THE USE OF THE "/X" 
2)	01050	;SWITCH, AND PRINTS AN ERROR MESSAGE.
2)	01052	
2)	01054	WLDOUT:	SKIPE	OQMASK		;WILD CHR IN OUT FILE?
2)	01056		TRNE	FLAG,XFLG	;YES, HAVE WE SEEN /X?
2)	01058		POPJ	P,		;GOOD, FOUND IT - SO GO BACK
2)	01060		JRST	ERRXSW		;HE NEEDED /X, SO HE LOSES
2)	01068	>
2)	01070	MAINA5:
**************
1)21	00400		SKIP	1	;NO
1)	00410		JRST	PSCAN4	;YES
****
2)21	00392	IFE TYMSW,<	SKIP	1	;NO >
2)	00394	IFN TYMSW,<
2)	00396		JRST	[TRNN	AUXFLG,TTYIN
2)	00398			JRST	PSCAN2
2)	00400			CAIE	CHR,"Z"-100
2)	00402			CAIN	CHR,"D"-100
2)	00404			JRST	PSCAN1
2)	00406			JRST	.+2	]
2)	00408	>
2)	00410		JRST	PSCAN4	;YES
**************
1)22	00170		CAIN	CHR,CZ		;IS IT ^Z
1)	00180		TRNN	AUXFLG,TTYIN	;FROM TTY?
1)	00190		JRST	PSCAN6		;NO
1)	00200		SETZ	CHR,		;YES,CLEAR CHAR.
****
2)22	00162	IFE TYMSW,<	CAIN	CHR,CZ		;IS IT ^Z  
2)	00164		TRNN	AUXFLG,TTYIN	;FROM TTY?
2)	00166		JRST	PSCAN6		;NO  > 
2)	00170	IFN TYMSW,<
2)	00172		TRNN	AUXFLG,TTYIN	;INPUT FROM TTY?
2)	00174		JRST	PSCAN6		;NO
2)	00176		CAIE	CHR,"Z"-100	;CTRL-Z ENDS INPUT FROM TTY
2)	00178		CAIN	CHR,"D"-100	;SO DOES CTRL-D AT TYMSHARE
2)	00180		SKIP	1
2)	00182		JRST	PSCAN6		;(SAVE THE CHARACTER)
2)	00184	>
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

2)	00200		SETZ	CHR,		;YES,CLEAR CHAR.
**************
1)53	     
****
2)53	00720	IFN TYMSW,<
2)	00730	ERRXSW:	ERRPNT	<z?need /x for wild char in output file name!z>
2)	00735	ERRSIZ:	ERRPNT	</?insufficient disk space available for copy!/>
2)	00740	>
2)	     
**************
1)58	00040		LOOKUP	IN,ZRF		;LOOKUP INPUT FILE NAME
****
2)58	00032	IFN TYMSW,<
2)	00034		PUSHJ	P,LKSIZ		;RE-FORMAT FOR EXTENDED LOOKUP
2)	00036	>
2)	00040		LOOKUP	IN,ZRF		;LOOKUP INPUT FILE NAME
**************
1)58	00090	COPY6B:	MOVE	0,ZRF		;INPUT FILE NAME
****
2)58	00085	IFE TYMSW,<
2)	00090	COPY6B:	MOVE	0,ZRF		;INPUT FILE NAME
**************
1)58	00130	
****
2)58	00122	>
2)	00124	IFN TYMSW,<
2)	00126	COPY6B:	MOVE	0,ZRF+2		;ZRF+2 HAS NAME IN EXTENDED LOOK
2)	00128		MOVEM	0,DTON		;OUTFILE GETS INFILE NAME
2)	00130		HLLZ	0,ZRF+3		;LIKEWISE EXTENSION
2)	00132		HLLZM	0,DTON+1	;(NOW RE-FORMAT DTON BLOCK)
2)	00134	>
2)	00138	
**************
1)58	00390		ENTER	OUT,DTON	;GOT DATA, CREATE NEW FILE
1)	00400		  JRST	ERR4		;DIRECTORY FULL
1)	00410		MOVE	0,ZRO		;GET ASCII/00000/AND
****
2)58	00382	IFN TYMSW,<
2)	00384		SKIPE	DTON		;OUTPUT FILE GIVEN?
2)	00386		PUSHJ	P,ENTSIZ	;YES, ACCEPT EXTENDED ENTER
2)	00388	>
2)	00390		ENTER	OUT,DTON	;GOT DATA, CREATE NEW FILE
2)	00400		  JRST	ERR4		;DIRECTORY FULL
2)	00402	IFN TYMSW,<
2)	00404		PUSHJ	P,BLKZRO	;ZERO EXTENDED LOOKUP/ENTER BLKS
2)	00406	>
2)	00410		MOVE	0,ZRO		;GET ASCII/00000/AND
**************
1)61	00210		AND	T1,ZRF		;GET SUBSTITUTE ONES
1)	00220		ORM	T1,DTON		;PUT THEM IN
****
2)61	00205	IFE TYMSW,<
2)	00210		AND	T1,ZRF		;GET SUBSTITUTE ONES
2)	00212	>
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

2)	00214	IFN TYMSW,<
2)	00216		AND	T1,ZRF+2	;FOR TYMSHARE'S EXTENDED LOOKUP
2)	00218	>
2)	00220		ORM	T1,DTON		;PUT THEM IN
**************
1)61	00260		AND	T1,ZRF+1
1)	00270		ORM	T1,DTON+1
****
2)61	00252	IFE TYMSW,<
2)	00254		AND	T1,ZRF+1
2)	00256	>
2)	00258	IFN TYMSW,<
2)	00260		AND	T1,ZRF+3	;FOR TYMSHARE'S EXTENDED LOOKUP
2)	00262	>
2)	00270		ORM	T1,DTON+1
**************
1)69	00030	IFN FTDSK,<SKIPE GENERI		;SEARCHING F/S ?
1)	00040		POPJ	P,		;YES, WAIT TIL END OF F/S SEARCH LIST>
****
2)69	00025	IFN FTDSK,<
2)	00027	IFE TYMSW,<
2)	00030		SKIPE GENERI		;SEARCHING F/S ?
2)	00031	>
2)	00033	IFN TYMSW,<
2)	00035		SKIPN GENERI		;*** ONLY 1 DEVICE ON F/S
2)	00037	>
2)	00040		POPJ	P,		;YES, WAIT TIL END OF F/S SEARCH LIST>
**************
1)71	00220		POPJ    P,>
****
2)71	00212		REPEAT 3,<SOS UFDIN+2	;*** MODIFY BUFFER COUNT
2)	00214		IBP UFDIN+1>	;*** AND MODIFY BUFFER PTR
2)	00220		POPJ    P,>
**************
1)96	00150	DTON:	BLOCK   4	;OUTPUT DIR. ENTRY
1)	00160	DEVA:	BLOCK	1	;SAVE INPUT DEV. NAME
1)	00170	NO.:	BLOCK	1	;GENERATE FILE NAMES
1)	00180	ZRF:	BLOCK   4	;LOOKUP FILE NAMES
1)	00190	MTAREQ:	BLOCK	1	;STORE MTA REQUESTS
****
2)96	00145	IFE TYMSW,<
2)	00150	DTON:	BLOCK   4	;OUTPUT DIR. ENTRY
2)	00152	>
2)	00154	IFN TYMSW, <
2)	00156	DTON:	BLOCK 12	;*** EXTENDED ENTER BLOCK
2)	00158	>
2)	00160	DEVA:	BLOCK	1	;SAVE INPUT DEV. NAME
2)	00170	NO.:	BLOCK	1	;GENERATE FILE NAMES
2)	00175	IFE TYMSW,<
2)	00180	ZRF:	BLOCK   4	;LOOKUP FILE NAMES
2)	00182	>
2)	00184	IFN TYMSW,<
2)	00186	ZRF:	BLOCK	12	;*** EXTENDED LOOKUP BLOCK
2)	00188	>
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

2)	00190	MTAREQ:	BLOCK	1	;STORE MTA REQUESTS
**************
1)105	00030	
1)	00040	DSKZRO:	SKIPE	T1,ODEV		;GET REAL DSK
1)	00050		MOVEM	T1,ADSK		;SO AS TO INIT CORRECT F/S
****
2)105	00021	DSKZRO:
2)	00022	IFN TYMSW, <
2)	00024		OUTSTR	[ASCIZ /Deleting all files! okay?  /]
2)	00026		INCHRW	0,
2)	00028		CAIE	0,"Y"		;*** OKAY, IF YOU SAY SO
2)	00032		jrst	ttcrlf		;*** prints a crlf
2)	00034	>
2)	00040		SKIPE	T1,ODEV		;GET REAL DSK
2)	00050		MOVEM	T1,ADSK		;SO AS TO INIT CORRECT F/S
**************
1)105	00110		  POPJ	P,
1)	00120		ILDB    0,UFDIN+1
****
2)105	00105	IFE TYMSW,<
2)	00110		  POPJ	P,
2)	00112	>
2)	00114	IFN TYMSW,<
2)	00116		JRST	TTCRLF		;*** A "SKIP" RETURN
2)	00118	>
2)	00120		ILDB    0,UFDIN+1
**************
1)105	00340		JRST    DSKZ1		;REPEAT
1)	     
****
2)105	00332	IFN TYMSW, <
2)	00334	REPEAT 3,  <SOS	UFDIN+2		;*** DECREMENT COUNT
2)	00336		IBP	UFDIN+1		;*** INC UFD POINTER
2)	00338	>
2)	00340		JRST    DSKZ1		;REPEAT
2)	00350	
2)	00360	TTCRLF:	OUTSTR	[ASCIZ /
2)	00370	/]
2)	00380		POPJ	P,
2)	     
**************
1)106	00090		MOVEI	0,ADSK		;ADDRESS OF DEVICE
****
2)106	00082	IFE TYMSW,<
2)	00090		MOVEI	0,ADSK		;ADDRESS OF DEVICE
**************
1)106	00140	INIFS1:	SETOM	STRARG		;CURRENT JOB NUMBER
****
2)106	00135	>
2)	00140	INIFS1:	SETOM	STRARG		;CURRENT JOB NUMBER
**************
1)106	00280	NXTFS:	MOVEI	0,STRARG	;GET ADDRESS
****
2)106	00275	IFE TYMSW,<
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

2)	00280	NXTFS:	MOVEI	0,STRARG	;GET ADDRESS
**************
1)106	00390	
****
2)106	00382	>
2)	00384	IFN TYMSW,<
2)	00385	NXTFS:	SKIPE	GENERI		;BEEN THRU HERE BEFORE?
2)	00386		JRST	NOFNCE		;NO, SO MARK AND CONTINUE
2)	00387		JRST	ENDFS		;YES. END OF F/S
2)	00388	>
2)	00390	
**************
1)106	00490	ENDFS:	MOVSI	0,'DSK'		;GENERIC "DSK"
****
2)106	00482	IFN TYMSW,<
2)	00484		AOS	(P)		;SKIP THE FIRST TIME THRU
2)	00486	>
2)	00490	ENDFS:	MOVSI	0,'DSK'		;GENERIC "DSK"
**************
1)106	00540		POPJ	P,		;RETURN
1)	00550	
****
2)106	00540		POPJ	P,
2)	00550	
**************
1)107	00490		POPJ	P,		;RETURN
1)	     
****
2)107	00490		POPJ	P,
2)	     
**************
1)108	00040	DSKDIR:	MOVE    T1,PP		;GET [P,P] INTO T1
1)	00050		JUMPN   T1,.+2		;IS IT ZERO?
****
2)108	00040	DSKDIR:	MOVE    T1,PPP		;GET [P,P] INTO T1
2)	00050		JUMPN   T1,.+2		;IS IT ZERO?
**************
1)108	01030		JRST	LSTU1		;NO,GET NEXT FILE
1)	01040		TRNN	CALFLG,MATFN	;MATCH FILENAME?
****
2)108	01025	IFE TYMSW,  <
2)	01030		JRST	LSTU1		;NO,GET NEXT FILE
2)	01032		>
2)	01034	IFN TYMSW, <
2)	01036		JRST LSTU1A		;*** UFD BUFFER MOD
2)	01038	>
2)	01040		TRNN	CALFLG,MATFN	;MATCH FILENAME?
**************
1)108	01070		JRST	LSTU1		;NO
1)	01080	LSTU2A:	CAIE	DOUT,'UFD'	;IS FILE MFD
****
2)108	01062	IFE TYMSW, <
2)	01064		JRST	LSTU1		;NO
2)	01066	>
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

2)	01068	IFN TYMSW, <
2)	01070		JRST	LSTU1A		;*** UFD BUFFER MOD
2)	01075	>
2)	01080	LSTU2A:	CAIE	DOUT,'UFD'	;IS FILE MFD
**************
1)108	01270		SKIP	2
****
2)108	01265	IFE TYMSW,<
2)	01270		SKIP	2
**************
1)108	01300		SKIPN	FILEX
****
2)108	01292	>
2)	01294	IFN TYMSW,<
2)	01295		SKIP	1		;JUMP OVER SHORT LIST JRST
2)	01296		JRST	LSTU8		;INCR BFR STUFF AND CONTINUE
2)	01298	>
2)	01300		SKIPN	FILEX
**************
1)109	00030		TRNE	CALFLG,LISTTY	;OUTPUT DEVICE A TTY?
1)	00040		JRST	LSTU7		;YES, SKIP LONG DIRECTORY
****
2)109	00022	IFN TYMSW,<
2)	00024		TRNE	AUXFLG,TTYOUT	;IS THE DEVICE A REAL TTY?
2)	00026	>
2)	00028	IFE TYMSW,<
2)	00030		TRNE	CALFLG,LISTTY	;OUTPUT DEVICE A TTY?
2)	00035	>
2)	00040		JRST	LSTU7		;YES, SKIP LONG DIRECTORY
**************
1)109	00370		JRST	LSTU1
1)	00380	
****
2)109	00365	IFE TYMSW, <
2)	00368		JRST	LSTU1
2)	00370	>
2)	00371	IFN TYMSW, <
2)	00372	LSTU1A:	REPEAT 3,<SOS UFDIN+2	;*** MODIFY BUFFER COUNT
2)	00376		IBP UFDIN+1>		;*** MODIFY BUFFER POINTER
2)	00377		JRST	LSTU1		;*** GET ANOTHER FILE
2)	00378	>
2)	00380	
**************
1)110	00050		JUMPN	0,LSTU1		;MATCH FAILED
1)	00060	MLSTU1:	TRNN	CALFLG,MATEX	;MATCH EXT
****
2)110	00042	IFN TYMSW,<
2)	00044		JUMPN	0,LSTU1A	;*** FILE NAME MATCH FAILED
2)	00046	>
2)	00048	IFE TYMSW,<
2)	00050		JUMPN	0,LSTU1		;MATCH FAILED
2)	00052	>
2)	00060	MLSTU1:	TRNN	CALFLG,MATEX	;MATCH EXT
**************
File 1)	DSK:DECPIP.MAC	created: 0923 31-DEC-73
File 2)	DSK:NEWPIP.MAC	created: 1258 17-JAN-74

1)110	00100		JUMPN	DOUT,LSTU1	;FAILED
1)	00110		JRST	LSTU2A		;MATCH FOUND
****
2)110	00092	IFN TYMSW,<
2)	00094		JUMPN	DOUT,LSTU1A	;*** FILE EXT MATCH FAILED
2)	00096	>
2)	00098	IFE TYMSW,<
2)	00100		JUMPN	DOUT,LSTU1	;FAILED
2)	00102	>
2)	00110		JRST	LSTU2A		;MATCH FOUND
**************
1)116	00030	HEDR4:	TRNE	CALFLG,LISTTY
1)	00040		JRST	[POP	P,(P)	;BACKUP ONE LEVEL
****
2)116	00022	IFN TYMSW,<
2)	00024	HEDR4:	TRNE	AUXFLG,TTYOUT	;IS THE DEVICE A REAL TTY?
2)	00026	>
2)	00028	IFE TYMSW,<
2)	00030	HEDR4:	TRNE	CALFLG,LISTTY
2)	00035	>
2)	00040		JRST	[POP	P,(P)	;BACKUP ONE LEVEL
**************
1)117	00120		MOVEI	T1,LSTU0	;RETURN ADDRESS
1)	00130		HRRM	T1,(P)		;OF POPJ
1)	00140		JRST	DIRSK2		;GET NEXT FILE STRUCTURE
1)	00150	NOUFD:	ASCIZ	/	no UFD created for /
1)	00160	
****
2)117	00120		SOS	(P)		;CORRECT THE RETURN ADDRESS
2)	00130		SOS	(P)		; TO MAINAD OR LSTU0
2)	00140		JRST	DIRSK2		;GET NEXT FILE STRUCTURE
2)	00150	NOUFD:	ASCIZ	/%no UFD created for /
2)	00160	
**************
1)117	00290	DERR5:	MOVEI   T3,ZRF		;LOCATION OF FILENAME (INPUT)
1)	00300	DERTYP:	HRRZ	T7,1(T3)	;ERROR TYPE
****
2)117	00285	IFE TYMSW,<
2)	00290	DERR5:	MOVEI   T3,ZRF		;LOCATION OF FILENAME (INPUT)
2)	00292	>
2)	00294	IFN TYMSW,<
2)	00296	DERR5:	MOVEI	T3,ZRF+2	;LOCATION OF FILENAME (INPUT)
2)	00298	>
2)	00300	DERTYP:	HRRZ	T7,1(T3)	;ERROR TYPE
**************
  