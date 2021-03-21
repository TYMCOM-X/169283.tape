PAGE 01  C11X10.TYM  10-APR-73  11:44:54.716

      	000017	MACHNUM         E   17  MACHINE NUMBER
      	000005	NLINES          E    5D NUMBER OF 201 LINES
      	000013	HOST0NUM        E  11D HOST ZERO
      	000001	HOST1NUM        E   1D HOST ONE
      	000032	NUMGRP          E 32
      	000002	NUMHOST         E 2  NUMBER OF HOSTS
      	001027	NEIGH0          E 1027  NEIGHBOR FOR LINE 0
      	000013	NEIGH1          E   13  NEIGHBOR FOR LINE 1
      	000027	NEIGH2          E   27  NEIGHBOR FOR LINE 2
      	000006	NEIGH3          E    6  NEIGHBOR FOR LINE 3
      	000014	NEIGH4          E   14  NEIGHBOR FOR LINE 4
      	000000	NEIGH5          E    0  NEIGHBOR FOR LINE 5
      	000000	NEIGH6          E    0  NEIGHBOR FOR LINE 6
      	000000	NEIGH7          E    0  NEIGHBOR FOR LINE 7
      	000003	NGRPS0          E    3D NUMBER OF GROUPS ON LINE 0
      	000003	NGRPS1          E    3D NUMBER OF GROUPS ON LINE 1
      	000003	NGRPS2          E    3D NUMBER OF GROUPS ON LINE 2
      	000003	NGRPS3          E    3D NUMBER OF GROUPS ON LINE 3
      	000003	NGRPS4          E    3D NUMBER OF GROUPS ON LINE 4
      	000000	NGRPS5          E    0D NUMBER OF GROUPS ON LINE 5
      	000000	NGRPS6          E    0D NUMBER OF GROUPS ON LINE 6
      	000000	NGRPS7          E    0D NUMBER OF GROUPS ON LINE 7
      	      	*
      	177777	CARD201         E -1  201 CARD INSTALLED     
      	000000	EBCD2741        E  0  EBCD 2741S             
      	000000	MARKXV          E  0  MARK XV                
      	000000	ONLY16          E  0  TERMINATES 16 LINES    
      	000000	PRINTER         E  0  PRINTER                
      	177777	MACH12K         E -1  12K MACHINE            
      	000000	COMS            E  0  COMNET                 
      	000000	MGHS            E  0  MASGEN                 
      	000000	OSUS            E  0  OHIO ST.               
      	000000	DRIS            E  0  DRI                    
      	000000	PDP10           E  0  PDP10                  
      	000000	QUASISBOD       E  0  NEW START BAUD CODE    
      	177777	DUALBASE        E -1  DUAL BASE              
      	000000	NLMS            E  0  NLM                    
      	000000	BAUD12          E  0  1200 BAUD LINE         
      	000000	BAUD48          E  0  4800 BAUD  DEFAULT=2400
      	000000	BAUD96          E  0  9600 BAUD              
PAGE 01  BSYM.VCD  10-APR-73  11:44:55.900

      	000010	01000	MAXLINES	E	-(DUALBASE)*10-(#DUALBASE)*5
      	002260	01010	TICKSPS		E	1200D	CLOCK TICKS/SEC
      	000030	01020	ABZERO		E	30	A & B REGISTERS = ZERO
      	000050	01030	AXZERO		E	50	A & X REGISTERS = ZERO
      	000300	01040	SS12SET		E	300	SENSE SWITCHES 1 & 2 SET
      	      	01050	*
00000	001100	01060		JSS1	*
00001	000000	
00002	002000	01070		JMPM	CRASH
00003	052156	
00004	000000	01072		0
00005	177777	01075	ZAPCIR	-1		PLEASE PLACE A ZAPPER IN THE THIS BUFFER (IF NON-NEG)
00006	000000	01077		0; +MACHNO
00007	000100	
00010	      	01080		R	10	POWER FAILURE TRAP LOCATION AND ERROR COUNT FOR LINE 4
00010	000000	01090		0; 0
00011	000000	
00012	100077	01100		EXC	77
00013	040016	01110		INR	POWFAL
00014	002000	01120		JMPM	CRASH
00015	052156	
00016	000000	01130	POWFAL	0		POWER FAILURE FLAG
00017	      	01140		R	20
00020	000021	01150	ZDET	21; 23; 25; 27; 31; 33; 35; 37
00021	000023	
00022	000025	
00023	000027	
00024	000031	
00025	000033	
00026	000035	
00027	000037	
00030	      	01160		I	*+6
00036	      	01170		R	36
00036	002000	01180		JMPM	RNG
00037	001545	
00040	002000	01190		JMPM	OUT0
00041	001345	
00042	002000	01200		JMPM	IN0
00043	001425	
00044	002000	01210		JMPM	OUT1
00045	001353	
00046	002000	01220		JMPM	IN1
00047	001437	
00050	002000	01230		JMPM	OUT2
00051	001361	
00052	002000	01240		JMPM	IN2
00053	001451	
00054	002000	01250		JMPM	OUT3
00055	001367	
00056	002000	01260		JMPM	IN3
00057	001463	
00060	002000	01280		JMPM	OUT4
00061	001375	
00062	002000	01290		JMPM	IN4
PAGE 01  BSYM.VCD  10-APR-73  11:44:57.016

00063	001475	
00064	002000	01300		JMPM	OUT5
00065	001403	
00066	002000	01310		JMPM	IN5
00067	001507	
00070	002000	01320		JMPM	OUT6
00071	001411	
00072	002000	01330		JMPM	IN6
00073	001521	
00074	002000	01340		JMPM	OUT7
00075	001417	
00076	002000	01350		JMPM	IN7
00077	001533	
00100	      	01440		R	100
00100	000017	01450	MACHNO	+MACHNUM	MACHINE NUMBER
00101	000501	01460		501		VERSION NUMBER
00102	000000	01470	DLOCK	0		DEBUGGER LOCK
00103	000000	01480	DSWICH	0		DOWNSTREAM SWITCH
00104	000000	01490	CRSHCT	0		CRASH COUNT
00105	000205	01500	N940L	(NUMHOST_6)+NLINES	NUMBER OF HOSTS AND 201 LINES
00106	000013	01510	HOST0	+HOST0NUM	HOST ZERO NUMBER
00107	177777	01520	UPDOWN	-1
00110	000001	01540	HOST1	+HOST1NUM	HOST ONE NUMBER
00111	177777	01550	UPDN1	-1
      	000010	01570	SWTCHO	RPT1	MAXLINES
00112	000000	01580		0
00113	000000	
00114	000000	
00115	000000	
00116	000000	
00117	000000	
00120	000000	
00121	000000	
00122	001027	01590	MACH	+NEIGH0; +NEIGH1; +NEIGH2; +NEIGH3; +NEIGH4
00123	000013	
00124	000027	
00125	000006	
00126	000014	
00127	000000	01610		+NEIGH5; +NEIGH6; +NEIGH7
00130	000000	
00131	000000	
00132	014436	01630	IOTA	+IOTAB0; +IOTAB1; +IOTAB2; +IOTAB3; +IOTAB4
00133	014516	
00134	014576	
00135	014656	
00136	014736	
00137	015016	01650		+IOTAB5; +IOTAB6; +IOTAB7
00140	015016	
00141	015016	
00142	015016	01670		+IOTABI
00143	177772	01680	IOTABS	-2*NGRPS0; -2*NGRPS1; -2*NGRPS2; -2*NGRPS3; -2*NGRPS4
00144	177772	
00145	177772	
00146	177772	
PAGE 01  BSYM.VCD  10-APR-73  11:44:58.050

00147	177772	
00150	000000	01700		-2*NGRPS5; -2*NGRPS6; -2*NGRPS7
00151	000000	
00152	000000	
00153	177776	01720		-2
00154	000003	01730	GROUPN	+NGRPS0; +NGRPS1; +NGRPS2; +NGRPS3; +NGRPS4
00155	000003	
00156	000003	
00157	000003	
00160	000003	
00161	000000	01750		+NGRPS5; +NGRPS6; +NGRPS7
00162	000000	
00163	000000	
00164	000000	01770	GROUPI	0
00165	000003	01780		0+NGRPS0
00166	000006	01790		0+NGRPS0+NGRPS1
00167	000011	01800		0+NGRPS0+NGRPS1+NGRPS2
00170	000014	01810		0+NGRPS0+NGRPS1+NGRPS2+NGRPS3
00171	000017	01830		0+NGRPS0+NGRPS1+NGRPS2+NGRPS3+NGRPS4
00172	000017	01840		0+NGRPS0+NGRPS1+NGRPS2+NGRPS3+NGRPS4+NGRPS5
00173	000017	01850		0+NGRPS0+NGRPS1+NGRPS2+NGRPS3+NGRPS4+NGRPS5+NGRPS6
00174	000031	01870		+NUMGRP-1
00175	010000	01880	BORO	10000
      	000010	01890	BORI	RPT1	MAXLINES
00176	010000	01900		10000
00177	010000	
00200	010000	
00201	010000	
00202	010000	
00203	010000	
00204	010000	
00205	010000	
      	000011	01910	STOPIN	RPT1	MAXLINES+1
00206	177777	01920		-1
00207	177777	
00210	177777	
00211	177777	
00212	177777	
00213	177777	
00214	177777	
00215	177777	
00216	177777	
00217	      	01930	CRSAVE	R	*+10	ARRAY TO SAVE CRASH INFORMATION
      	000010	02010	LINER	RPT1	MAXLINES+(#DUALBASE)
00227	000000	02020		0
00230	000000	
00231	000000	
00232	000000	
00233	000000	
00234	000000	
00235	000000	
00236	000000	
00237	000001	02030	MASK	1; 2; 4; 10; 20; 40; 100; 200; 400; 1000; 2000; 4000
00240	000002	
PAGE 01  BSYM.VCD  10-APR-73  11:44:58.516

00241	000004	
00242	000010	
00243	000020	
00244	000040	
00245	000100	
00246	000200	
00247	000400	
00250	001000	
00251	002000	
00252	004000	
00253	010000	02040		10000; 20000; 40000; 100000
00254	020000	
00255	040000	
00256	100000	
00257	177776	02050	CMSK	177776; 177775; 177773; 177767; 177757; 177737; 177677; 177577; 177377
00260	177775	
00261	177773	
00262	177767	
00263	177757	
00264	177737	
00265	177677	
00266	177577	
00267	177377	
00270	176777	02060		176777; 175777; 173777; 167777; 157777; 137777; 77777
00271	175777	
00272	173777	
00273	167777	
00274	157777	
00275	137777	
00276	077777	
00277	000021	02070	RSF	21; 42; 104; 210; 231; 63; 146; 314; 335; 273; 167; 356
00300	000042	
00301	000104	
00302	000210	
00303	000231	
00304	000063	
00305	000146	
00306	000314	
00307	000335	
00310	000273	
00311	000167	
00312	000356	
      	000313	02080	FLAG	E	*
      	      	02090	* GENBUF MUST BE FIRST   UNASSIGNED BUFFERS USE BIT 0 OF GENBUF
      	      	02100	* IIN1 MUST BE SECOND FOR RT8B STOPIN SETTING
00313	      	02110	GENBUF	R	*+1
00314	      	02120	IIN	R	*+MAXLINES-(#DUALBASE)
      	      	02130	* WARNING! RT6 REQUIRES CFOB TO BE THE LAST GROUP OF FLAGS
00324	      	02140	CFOB	R	*+NUMGRP	CANDIDATES FOR OUTPUT BUFFER
00356	      	02150	PCFOB	R	*+NUMGRP	PROCESS CAND. FOR OBUF.
00410	      	02160	RSBF	R	*+NUMGRP	RESTRICTED BUFFER FLAGS
00442	      	02170	LRESET	R	*+MAXLINES	LINE RESET
00452	      	02180	TIME1	R	*+MAXLINES	TIME SINCE LAST RECORD MADE
00462	      	02190	OSF	R	*+MAXLINES	OUTPUT SECTOR FLAGS
PAGE 01  BSYM.VCD  10-APR-73  11:44:58.900

00472	      	02200	ISF	R	*+MAXLINES	INPUT SECTOR FLAGS
00502	      	02210	CAOS	R	*+MAXLINES	CURRENT ASSEMBLING OUTPUT SECTOR
00512	      	02220	COS	R	*+MAXLINES	CURRENT OUTPUT SECTOR
00522	      	02230	CDIS	R	*+MAXLINES	CURRENT DISASSEMBLING INPUT SECTOR
00532	      	02240	CIS	R	*+MAXLINES	CURRENT INPUT SECTOR
00542	      	02250	LRR	R	*+MAXLINES	LAST RECORD RECIEVED
00552	      	02260	ASDW	R	*+MAXLINES	ASSEMBLED WORD
00562	      	02270	NDASW	R	*+MAXLINES	NEXT DISASSEMBLING WORD
00572	      	02280	TSLR	R	*+MAXLINES	TIME SINCE LAST RESET
00602	      	02290	QOVERT	R	*+MAXLINES	OUTPUT VERT CKSM.
00612	      	02300	QODIAG	R	*+MAXLINES	OUTPUT DIAG. CKSM.
00622	      	02310	QIFRST	R	*+MAXLINES	FIRST WORD OF RECORD
00632	      	02320	QIVERT	R	*+MAXLINES	INPUT VERT. CKSM.
00642	      	02330	QIDIAG	R	*+MAXLINES	INPUT DIAG. CKSM.
00652	      	02340	IRC	R	*+MAXLINES	INPUT RECORD COUNT
00662	      	02350	ORC	R	*+MAXLINES	OUTPUT RECORD COUNT
00672	      	02360	SWTCHI	R	*+MAXLINES	SWITCH FOR INPUT CLEANUP
00702	      	02370	LINSAT	R	*+MAXLINES	LINE SATURATION COUNTER
00712	      	02380	INDATA	R	*+MAXLINES	INCOMMING DATA FLG FOR DIAGNOSTICS
00722	      	02390	ORA	R	*+MAXLINES	OUTPUT RECORD ADDRESS
00732	      	02400	IRA	R	*+MAXLINES	INPUT RECORD ADDRESS
00742	      	02410	STOPO	R	*+MAXLINES-(#DUALBASE)
00752	000001	02420	ACTPRT	1		ACTIVE PORTS
      	000007	02430		RPT1	MAXLINES+(DUALBASE)
00753	000000	02440		0
00754	000000	
00755	000000	
00756	000000	
00757	000000	
00760	000000	
00761	000000	
00762	000000	02450	W1	0		WORKING STORAGE
00763	000000	02460	W2	0
00764	000000	02470	W3	0
00765	000000	02480	W4	0
00766	000000	02490	W5	0
00767	000000	02500	W6	0
00770	000000	02510	W7	0
00771	000000	02520	W8	0
00772	000000	02530	W9	0
00773	000000	02540	W10	0
00774	000000	02550	W11	0
00775	000000	02560	W12	0
00776	000000	02570	W13	0
      	      	02580	*
      	      	02590	*
      	      	02600	* OSEC = ADDRESS OF OUTPUT SECTORS
      	      	02610	* ISEC = ADDRESS OF INPUT SECTORS
      	      	02620	* CHECK BR BEFORE CHANGING OSEC
      	      	02630	*
      	015732	02640	OSECADD	E	-(DUALBASE)*15732-(#DUALBASE)*12400
      	020000	02650	ISECADD	E	-(DUALBASE)*20000-(#DUALBASE)*(OSECADD+200*MAXLINES)
      	044000	02660	BUFADD	E	2*(ISECADD+200*MAXLINES)
      	060000	02670	BUFEND	E	2*(-(DUALBASE)*30000-(#DUALBASE)*17733)
PAGE 01  BSYM.VCD  10-APR-73  11:44:59.833

      	015732	02680	ENDCODE	E	OSECADD
      	      	02690	*
00777	015732	02700	OSEC	0+OSECADD; 200+OSECADD; 400+OSECADD; 600+OSECADD
01000	016132	
01001	016332	
01002	016532	
01003	016732	02720		1000+OSECADD; 1200+OSECADD; 1400+OSECADD; 1600+OSECADD
01004	017132	
01005	017332	
01006	017532	
01007	020000	02740	ISEC	0+ISECADD; 200+ISECADD; 400+ISECADD; 600+ISECADD
01010	020200	
01011	020400	
01012	020600	
01013	021000	02760		1000+ISECADD; 1200+ISECADD; 1400+ISECADD; 1600+ISECADD
01014	021200	
01015	021400	
01016	021600	
      	      	02780	* JUMP ADDRESS FOR ZERO DETECT, NORMAL JUMP
      	      	02790	* ADDRESS, AND ADDRESS OF JUMP INSTRUCTION.
      	      	02800	*
01017	001433	02810	INZD	+IN0+6; +IN1+6; +IN2+6; +IN3+6
01020	001445	
01021	001457	
01022	001471	
01023	001503	02830		+IN4+6; +IN5+6; +IN6+6; +IN7+6
01024	001515	
01025	001527	
01026	001541	
01027	101425	02850	INNORM	+IN0+.; +IN1+.; +IN2+.; +IN3+.
01030	101437	
01031	101451	
01032	101463	
01033	101475	02870		+IN4+.; +IN5+.; +IN6+.; +IN7+.
01034	101507	
01035	101521	
01036	101533	
01037	001432	02890	INAD	+IN0+5; +IN1+5; +IN2+5; +IN3+5
01040	001444	
01041	001456	
01042	001470	
01043	001502	02910		+IN4+5; +IN5+5; +IN6+5; +IN7+5
01044	001514	
01045	001526	
01046	001540	
01047	001345	02930	OUTT	+OUT0; +OUT1; +OUT2; +OUT3
01050	001353	
01051	001361	
01052	001367	
01053	001375	02950		+OUT4; +OUT5; +OUT6; +OUT7
01054	001403	
01055	001411	
01056	001417	
01057	001425	02970	INTT	+IN0; +IN1; +IN2; +IN3
PAGE 01  BSYM.VCD  10-APR-73  11:45:00.500

01060	001437	
01061	001451	
01062	001463	
01063	001475	02990		+IN4; +IN5; +IN6; +IN7
01064	001507	
01065	001521	
01066	001533	
      	      	03010	*
      	      	03020	*
01067	      	03030		L	*+200
01267	      	03040	CLNLOK	R	*+1
01270	      	03050	UPDWN	R	*+1	UP-DOWN FLG.
01271	      	03060	INA	R	*+1	SAVE LOCATIONS FOR INTERRUPTS
01272	      	03070	CNA	R	*+1	SAVE LOCATIONS FOR CLEANUP
01273	      	03080	CNB	R	*+1
01274	      	03090	CNX	R	*+1
01275	      	03100	FULL	R	*+1
01276	000000	03110	NRW	0
01277	000000	03120	NRW1	0
01300	000000	03130	NRM	0
01301	000000	03140	NRA	0
01302	000000	03150	NRSW	0
01303	000000	03160	SUCUNT	0
01304	000000	03170	SCOUNT	0
01305	000000	03180	SPORT	0
01306	000000	03190	TCH	0
01307	000000	03200	TIMSEC	0		TIME SINCE LAST ONE HALF SECOND
01310	000000	03210	TIMMIN	0		TIME SINCE LAST MINUTE
01311	000000	03220	TIMEH	0		HIGH ORDER REAL TIME
01312	000000	03230	TIMEL	0		LOW ORDER REAL TIME
01313	      	03240	UNHAP	R	*+1	UNHAPPY FLG.
01314	      	03250	SACTIV	R	*+1	SUPERVISOR ACTIVE
01315	      	03260	USWICH	R	*+1	UPSTREAM SWITCH
01316	000000	03270	ICOUNT	0
01317	000000	03280	IRING	0		INPUT RING ADDRESS
01320	000000	03290	ORING	0		OUTPUT RING ADDRESS
01321	000000	03310	ICNT1	0
01322	000000	03320	IRING1	0
01323	000000	03330	ORING1	0
01324	      	03340	UNHAP1	R	*+1
01325	      	03350	MICRJ2	R	*+1
01326	      	03360	MICER2	R	*+1
01327	      	03370	FULL1	R	*+1
01330	      	03380	UPDWN1	R	*+1
01331	      	03410	SPSTAT	R	*+1
01332	      	03420	SIRING	R	*+1	SUPERVISOR INPUT RING
01333	      	03430	SORING	R	*+1	SUPERVISOR OUTPUT RING
01334	      	03440	PAGE0	R	*+1	SUPERVISOR PAGE 0
01335	      	03450	SFULL	R	*+1
01336	      	03460	STIME	R	*+1	SUPERVISOR TIME
01337	000000	03480	MICRJC	0		MIC REJECT COUNT
01340	000000	03490	MICERR	0		MIC ERROR COUNT
01341	001400	03510	RD940	1400		HIGH ORDER BITS TO READ MIC
01342	106262	03520	K1	106262
PAGE 01  BSYM.VCD  10-APR-73  11:45:01.616

01343	157646	03530	K2	157646
01344	101234	03540	K3	101234
      	      	03560	*
      	      	03570	*
      	      	03580	*  201 CARD INTERRRUPT ROUTINES.
      	      	03590	*
01345	000000	03600	OUT0	0
01346	103076	03610		OME	76,NDASW
01347	000562	
01350	100377	03620		EXC	377
01351	001000	03630		JMP	OUT0+.
01352	101345	
01353	000000	03640	OUT1	0
01354	103076	03650		OME	76,NDASW+1
01355	000563	
01356	100377	03660		EXC	377
01357	001000	03670		JMP	OUT1+.
01360	101353	
01361	000000	03680	OUT2	0
01362	103076	03690		OME	76,NDASW+2
01363	000564	
01364	100377	03700		EXC	377
01365	001000	03710		JMP	OUT2+.
01366	101361	
01367	000000	03720	OUT3	0
01370	103076	03730		OME	76,NDASW+3
01371	000565	
01372	100377	03740		EXC	377
01373	001000	03750		JMP	OUT3+.
01374	101367	
01375	000000	03770	OUT4	0
01376	103076	03780		OME	76,NDASW+4
01377	000566	
01400	100377	03790		EXC	377
01401	001000	03800		JMP	OUT4+.
01402	101375	
01403	000000	03810	OUT5	0
01404	103076	03820		OME	76,NDASW+5
01405	000567	
01406	100377	03830		EXC	377
01407	001000	03840		JMP	OUT5+.
01410	101403	
01411	000000	03850	OUT6	0
01412	103076	03860		OME	76,NDASW+6
01413	000570	
01414	100377	03870		EXC	377
01415	001000	03880		JMP	OUT6+.
01416	101411	
01417	000000	03890	OUT7	0
01420	103076	03900		OME	76,NDASW+7
01421	000571	
01422	100377	03910		EXC	377
01423	001000	03920		JMP	OUT7+.
01424	101417	
PAGE 01  BSYM.VCD  10-APR-73  11:45:03.616

01425	000000	03940	IN0	0
01426	102076	03950		IME	76,ASDW
01427	000552	
01430	100377	03960		EXC	377
01431	001000	03970		JMP	IN0+.
01432	101425	
01433	103077	03980		OME	77,ZDET
01434	000020	
01435	001000	03990		JMP	IN0+.
01436	101425	
01437	000000	04000	IN1	0
01440	102076	04010		IME	76,ASDW+1
01441	000553	
01442	100377	04020		EXC	377
01443	001000	04030		JMP	IN1+.
01444	101437	
01445	103077	04040		OME	77,ZDET+1
01446	000021	
01447	001000	04050		JMP	IN1+.
01450	101437	
01451	000000	04060	IN2	0
01452	102076	04070		IME	76,ASDW+2
01453	000554	
01454	100377	04080		EXC	377
01455	001000	04090		JMP	IN2+.
01456	101451	
01457	103077	04100		OME	77,ZDET+2
01460	000022	
01461	001000	04110		JMP	IN2+.
01462	101451	
01463	000000	04120	IN3	0
01464	102076	04130		IME	76,ASDW+3
01465	000555	
01466	100377	04140		EXC	377
01467	001000	04150		JMP	IN3+.
01470	101463	
01471	103077	04160		OME	77,ZDET+3
01472	000023	
01473	001000	04170		JMP	IN3+.
01474	101463	
01475	000000	04190	IN4	0
01476	102076	04200		IME	76,ASDW+4
01477	000556	
01500	100377	04210		EXC	377
01501	001000	04220		JMP	IN4+.
01502	101475	
01503	103077	04230		OME	77,ZDET+4
01504	000024	
01505	001000	04240		JMP	IN4+.
01506	101475	
01507	000000	04250	IN5	0
01510	102076	04260		IME	76,ASDW+5
01511	000557	
01512	100377	04270		EXC	377
PAGE 01  BSYM.VCD  10-APR-73  11:45:03.966

01513	001000	04280		JMP	IN5+.
01514	101507	
01515	103077	04290		OME	77,ZDET+5
01516	000025	
01517	001000	04300		JMP	IN5+.
01520	101507	
01521	000000	04310	IN6	0
01522	102076	04320		IME	76,ASDW+6
01523	000560	
01524	100377	04330		EXC	377
01525	001000	04340		JMP	IN6+.
01526	101521	
01527	103077	04350		OME	77,ZDET+6
01530	000026	
01531	001000	04360		JMP	IN6+.
01532	101521	
01533	000000	04370	IN7	0
01534	102076	04380		IME	76,ASDW+7
01535	000561	
01536	100377	04390		EXC	377
01537	001000	04400		JMP	IN7+.
01540	101533	
01541	103077	04410		OME	77,ZDET+7
01542	000027	
01543	001000	04420		JMP	IN7+.
01544	101533	
      	      	04440	*
      	      	04450	*
      	      	04460	*  RING DRIVER:  RUNS THE CLOCK AND THE QUASI-INTERRUPT CODE.
      	  04470	*
01545	000000	04480	RNG	0
01546	051271	04490		STA	INA
01547	041312	04500		INR	TIMEL
01550	011312	04510		LDA	TIMEL
01551	003010	04520		XAZ	BUMPTH	BUMP TIME HIGH
01552	002035	
01553	011267	04530		LDA	CLNLOK
01554	001010	04540		JAZ	RNG1
01555	001562	
01556	011271	04550		LDA	INA
01557	100377	04560		EXC	377
01560	001000	04570		JMP	RNG+.
01561	101545	
01562	005201	04580	RNG1	COM	,A
01563	051267	04590		STA	CLNLOK
01564	011545	04600		LDA	RNG
01565	050001	04610		STA	CLENRT+1
01566	100377	04620		EXC	377
01567	011271	04630		LDA	INA
      	      	04640	* CLEANUP FOR 201 INTERRUPTS
01570	051272	04650		STA	CNA
01571	061273	04660		STB	CNB
01572	071274	04670		STX	CNX
01573	005004	04680	CLNOUT	TZX
PAGE 01  BSYM.VCD  10-APR-73  11:45:04.500

01574	011345	04690		LDA	OUT0
01575	001002	04700		JAP	QIO
01576	001641	
01577	011353	04710		LDA	OUT1
01600	151361	04720		ANA	OUT2
01601	151367	04730		ANA	OUT3
01602	151375	04750		ANA	OUT4
01603	151403	04760		ANA	OUT5
01604	151411	04770		ANA	OUT6
01605	151417	04780		ANA	OUT7
01606	001004	04800		JAN	CLENIN
01607	002036	
01610	005104	04810		INC	,X
01611	011353	04820		LDA	OUT1
01612	001002	04830		JAP	QIO
01613	001641	
01614	005144	04840		IXR
01615	011361	04850		LDA	OUT2
01616	001002	04860		JAP	QIO
01617	001641	
01620	005144	04870		IXR
01621	011367	04890		LDA	OUT3
01622	001002	04900		JAP	QIO
01623	001641	
01624	005144	04910		IXR
01625	011375	04920		LDA	OUT4
01626	001002	04930		JAP	QIO
01627	001641	
01630	005144	04940		IXR
01631	011403	04950		LDA	OUT5
01632	001002	04960		JAP	QIO
01633	001641	
01634	005144	04970		IXR
01635	011411	04980		LDA	OUT6
01636	001002	04990		JAP	QIO
01637	001641	
01640	005144	05000		IXR
      	      	05020	* OUTPUT INTERRUPT CLEANUP ROUTINE  LINE NUMBER IN X
01641	005041	05030	QIO	TXA
01642	121067	05040		ADD	=OUTT
01643	005012	05050		TAB
01644	005201	05060		COM	,A
01645	026000	05070		LDB	0,6
01646	056000	05080		STA	0,6
01647	015112	05090		LDA	SWTCHO,5
01650	001004	05100		JAN	QOH
01651	001765	
01652	004241	05110		LRLA	1
01653	001004	05120		JAN	QOE
01654	001717	
01655	004241	05130		LRLA	1
01656	001004	05140		JAN	QOC
01657	001703	
      	      	05150	* OUTPUT ONES, AFTER WHICH CHECK BIT 3 OF SWITCH
PAGE 01  BSYM.VCD  10-APR-73  11:45:05.816

      	      	05160	* TO SEE WHETHER TO OUTPUT RESET PATTERN OR RETRANSMIT
01660	025662	05170		LDB	ORC,5
01661	001020	05180		JBZ	QOB
01662	001671	
01663	005322	05190		DBR
01664	065662	05200		STB	ORC,5
01665	005201	05210	QOA	COM	,A
01666	055562	05220		STA	NDASW,5
01667	001000	05230		JMP	CLNOUT
01670	001573	
01671	004241	05240	QOB	LRLA	1
01672	001004	05250		JAN	QOK
01673	002022	
01674	004343	05260		LSRA	3
01675	111070	05270		ORA	=20000
01676	055112	05280		STA	SWTCHO,5
01677	011071	05290		LDA	=5253
01700	055562	05300		STA	NDASW,5
01701	001000	05310		JMP	CLNOUT
01702	001573	
      	      	05320	* TRANSMIT SECOND HALF OF RESET PATTERN, THEN CHECK BIT 4 OF SWITCH
      	      	05330	* TO SEE WHETHER TO OUTPUT ONES OR LOOK FOR NEW RECORD
01703	010100	05340	QOC	LDA	MACHNO
01704	055562	05350		STA	NDASW,5
01705	015112	05360		LDA	SWTCHO,5
01706	004244	05370		LRLA	4
01707	001004	05380		JAN	QOJ+2
01710	002016	
01711	005001	05390		TZA
01712	055112	05400		STA	SWTCHO,5
01713	011072	05410		LDA	=41
01714	055662	05420		STA	ORC,5
01715	001000	05430		JMP	CLNOUT
01716	001573	
      	      	05440	* LOOK FOR NEW RECORD TO OUTPUT
01717	015512	05450	QOE	LDA	COS,5
01720	151073	05460		ANA	=3
01721	005012	05470		TAB
01722	015462	05480		LDA	OSF,5
01723	156277	05490		ANA	RSF,6
01724	146237	05500		SUB	MASK,6
01725	001010	05510		JAZ	QOF
01726	001737	
01727	001004	05520		JAN	QOA
01730	001665	
01731	011074	05530		LDA	=40
01732	055662	05540		STA	ORC,5
01733	011075	05550		LDA	=10000
01734	055112	05560		STA	SWTCHO,5
01735	001000	05570		JMP	QOA
01736	001665	
      	      	05580	* NEW RECORD FOUND   SEND IT OUT
01737	045512	05590	QOF	INR	COS,5
01740	015462	05600		LDA	OSF,5
PAGE 01  BSYM.VCD  10-APR-73  11:45:06.483

01741	116243	05610		ORA	MASK+4,6
01742	055462	05620		STA	OSF,5
01743	005021	05630		TBA
01744	004245	05640	QOG	LRLA	5
01745	125777	05650		ADD	OSEC,5
01746	005012	05660		TAB
01747	005111	05670		IAR
01750	055722	05680		STA	ORA,5
01751	016000	05690		LDA	0,6
01752	115542	05700		ORA	LRR,5
01753	055562	05710		STA	NDASW,5
01754	055602	05720		STA	QOVERT,5
01755	055612	05730		STA	QODIAG,5
01756	004346	05740		LSRA	6
01757	151076	05750		ANA	=37
01760	055662	05760		STA	ORC,5
01761	011077	05770		LDA	=.
01762	055112	05780		STA	SWTCHO,5
01763	001000	05790		JMP	CLNOUT
01764	001573	
      	      	05800	* OUTPUT NEXT WORD OF RECORD
01765	015662	05810	QOH	LDA	ORC,5
01766	001010	05820		JAZ	QOJ
01767	002014	
01770	005311	05830		DAR
01771	055662	05840		STA	ORC,5
01772	001010	05850		JAZ	QOI
01773	002010	
01774	025722	05860		LDB	ORA,5
01775	045722	05870		INR	ORA,5
01776	016000	05880		LDA	0,6
01777	055562	05890		STA	NDASW,5
02000	135602	05900		ERA	QOVERT,5
02001	055602	05910		STA	QOVERT,5
02002	015612	05920		LDA	QODIAG,5
02003	004241	05930		LRLA	1
02004	135562	05940		ERA	NDASW,5
02005	055612	05950		STA	QODIAG,5
02006	001000	05960		JMP	CLNOUT
02007	001573	
      	      	05970	* SECOND TO LAST WORD OF RECORD
02010	015602	05980	QOI	LDA	QOVERT,5
02011	055562	05990		STA	NDASW,5
02012	001000	06000		JMP	QOI-6
02013	002002	
      	      	06010	* LAST WORD OF RECORD
02014	015612	06020	QOJ	LDA	QODIAG,5
02015	055562	06030		STA	NDASW,5
02016	011100	06040		LDA	=40000
02017	055112	06050		STA	SWTCHO,5
02020	001000	06060		JMP	CLNOUT
02021	001573	
      	      	06070	* CHECK FOR RETRANSMIT
02022	011100	06080	QOK	LDA	=40000
PAGE 01  BSYM.VCD  10-APR-73  11:45:07.216

02023	055112	06090		STA	SWTCHO,5
02024	015462	06100		LDA	OSF,5
02025	131101	06110		ERA	=377
02026	005311	06120		DAR
02027	001002	06130		JAP	QOE
02030	001717	
02031	015512	06140		LDA	COS,5
02032	151073	06150		ANA	=3
02033	001000	06160		JMP	QOG
02034	001744	
      	      	06170	*
      	      	06180	*
      	      	06190	*  BUMP TIME - HIGH WORD
02035	041311	06200	BUMPTH	INR	TIMEH
      	      	06210	*
      	      	06220	*
      	      	06230	* 201 INPUT INTERRUPT CLEANUP
02036	005004	06240	CLENIN	TZX
02037	011425	06250		LDA	IN0
02040	001002	06260		JAP	QII
02041	002104	
02042	011437	06270		LDA	IN1
02043	151451	06280		ANA	IN2
02044	151463	06290		ANA	IN3
02045	151475	06310		ANA	IN4
02046	151507	06320		ANA	IN5
02047	151521	06330		ANA	IN6
02050	151533	06340		ANA	IN7
02051	001004	06360		JAN	CLENDN
02052	000000	
02053	005144	06370		IXR
02054	011437	06380		LDA	IN1
02055	001002	06390		JAP	QII
02056	002104	
02057	005144	06400		IXR
02060	011451	06410		LDA	IN2
02061	001002	06420		JAP	QII
02062	002104	
02063	005144	06430		IXR
02064	011463	06450		LDA	IN3
02065	001002	06460		JAP	QII
02066	002104	
02067	005144	06470		IXR
02070	011475	06480		LDA	IN4
02071	001002	06490		JAP	QII
02072	002104	
02073	005144	06500		IXR
02074	011507	06510		LDA	IN5
02075	001002	06520		JAP	QII
02076	002104	
02077	005144	06530		IXR
02100	011521	06540		LDA	IN6
02101	001002	06550		JAP	QII
02102	002104	
PAGE 01  BSYM.VCD  10-APR-73  11:45:07.966

02103	005144	06560		IXR
02104	005041	06580	QII	TXA
02105	121102	06590		ADD	=INTT
02106	005012	06600		TAB
02107	005201	06610		COM	,A
02110	026000	06620		LDB	0,6
02111	056000	06630		STA	0,6
02112	015672	06640		LDA	SWTCHI,5
02113	001004	06650		JAN	QID
02114	002254	
02115	004241	06660		LRLA	1
02116	001004	06670		JAN	QIC
02117	002174	
02120	004241	06680		LRLA	1
02121	001004	06690		JAN	QIB
02122	002144	
      	      	06700	* SWITCH A IS SET BY INIT
      	      	06710	* LOOK FOR FIRST HALF OF RESET PATTERN
02123	015552	06720	QIA	LDA	ASDW,5
02124	131071	06730		ERA	=5253
02125	001010	06740		JAZ	*+4
02126	002131	
02127	001000	06750		JMP	QIRZ
02130	031000	
02131	011312	06760		LDA	TIMEL
02132	145572	06770		SUB	TSLR,5
02133	001004	06780		JAN	*+5
02134	002140	
02135	141103	06790		SUB	=2*TICKSPS
02136	001004	06800		JAN	QIRZ
02137	031000	
      	      	06810	* SET B SWITCH
02140	011070	06820		LDA	=20000
02141	055672	06830		STA	SWTCHI,5
02142	001000	06840		JMP	QIRZ
02143	031000	
      	      	06850	* LOOK FOR SECOND HALF OF RESET PATTERN
      	      	06860	* SET C SWITCH
02144	011100	06870	QIB	LDA	=40000
02145	055672	06880		STA	SWTCHI,5
02146	015552	06890		LDA	ASDW,5
02147	135122	06900		ERA	MACH,5
02150	001010	06910		JAZ	*+4
02151	002154	
02152	001000	06920		JMP	QIRZ
02153	031000	
02154	021104	06930		LDB	=7
02155	065542	06940		STB	LRR,5
02156	055472	06950		STA	ISF,5
02157	055522	06960		STA	CDIS,5
02160	045442	06970		INR	LRESET,5
02161	055462	06980		STA	OSF,5
02162	055512	06990		STA	COS,5
02163	055502	07000		STA	CAOS,5
PAGE 01  BSYM.VCD  10-APR-73  11:45:10.916

02164	011312	07010		LDA	TIMEL
02165	055572	07020		STA	TSLR,5
02166	011105	07030		LDA	=4000
02167	055112	07040		STA	SWTCHO,5
02170	011072	07050		LDA	=41
02171	055662	07060		STA	ORC,5
02172	001000	07070		JMP	QIRN
02173	100000	
      	      	07080	* LOOK FOR FIRST WORD OF RECORD
02174	015552	07090	QIC	LDA	ASDW,5
02175	050000	07100		STA	QW
02176	151106	07110		ANA	=74000
02177	135176	07120		ERA	BORI,5
02200	005311	07130		DAR
02201	001002	07140		JAP	QIA
02202	002123	
02203	010000	07150		LDA	QW
02204	055622	07160		STA	QIFRST,5
02205	055632	07170		STA	QIVERT,5
02206	055642	07180		STA	QIDIAG,5
02207	004343	07190		LSRA	3
02210	005012	07200		TAB
02211	004343	07210		LSRA	3
02212	151076	07220		ANA	=37
02213	055652	07230		STA	IRC,5
02214	005321	07240		DEC	B,A
02215	145542	07250		SUB	LRR,5
02216	151107	07260		ANA	=4
02217	001010	07270		JAZ	*+6
02220	002225	
      	      	07280	* SET D SWITCH
02221	011077	07290	QICC	LDA	=100000
02222	055672	07300		STA	SWTCHI,5
02223	001000	07310		JMP	QIRN
02224	100000	
02225	005021	07320		TBA
02226	151073	07330		ANA	=3
02227	005012	07340		TAB
02230	015472	07350		LDA	ISF,5
02231	156237	07360		ANA	MASK,6
02232	005311	07370		DAR
02233	001002	07380		JAP	QICC
02234	002221	
      	      	07390	* SET E SWITCH
02235	011110	07400		LDA	=140000
02236	055672	07410		STA	SWTCHI,5
02237	065532	07420		STB	CIS,5
02240	005041	07430		TXA
02241	121111	07440		ADD	=ISEC
02242	005012	07450		TAB
02243	015532	07460		LDA	CIS,5
02244	004245	07470		LRLA	5
02245	126000	07480		ADD	0,6
02246	055732	07490		STA	IRA,5
PAGE 01  BSYM.VCD  10-APR-73  11:45:12.233

02247	005012	07500		TAB
02250	010000	07510		LDA	QW
02251	056000	07520		STA	0,6
02252	001000	07530		JMP	QIRN
02253	100000	
02254	004241	07540	QID	LRLA	1
02255	001004	07550		JAN	QIE
02256	002310	
      	      	07560	* INPUT RECORD WITHOUT STORING IT IN SECTOR
02257	015652	07570		LDA	IRC,5
02260	001010	07580		JAZ	QIDD
02261	002277	
02262	005313	07590		DEC	A,AB
02263	055652	07600		STA	IRC,5
02264	015552	07610		LDA	ASDW,5
02265	135632	07620		ERA	QIVERT,5
02266	055632	07630		STA	QIVERT,5
02267	015642	07640		LDA	QIDIAG,5
02270	004241	07650		LRLA	1
02271	135552	07660		ERA	ASDW,5
02272	055642	07670		STA	QIDIAG,5
02273	001020	07680		JBZ	QIRZ
02274	031000	
02275	001000	07690		JMP	QIRN
02276	100000	
      	      	07700	* LAST WORD OF RECORD   SET C SWITCH
02277	011100	07710	QIDD	LDA	=40000
02300	055672	07720		STA	SWTCHI,5
02301	015552	07730		LDA	ASDW,5
02302	135642	07740		ERA	QIDIAG,5
02303	115632	07750		ORA	QIVERT,5
02304	001010	07760		JAZ	QIF
02305	100000	
02306	001000	07770		JMP	QIRN
02307	100000	
      	      	07780	* INPUT RECORD AND STORE IN SECTOR
02310	015652	07790	QIE	LDA	IRC,5
02311	001010	07800		JAZ	QIEE
02312	000000	
02313	005311	07810		DAR
02314	055652	07820		STA	IRC,5
02315	045732	07830		INR	IRA,5
02316	025732	07840		LDB	IRA,5
02317	015552	07850		LDA	ASDW,5
02320	056000	07860		STA	0,6
02321	135632	07870		ERA	QIVERT,5
02322	055632	07880		STA	QIVERT,5
02323	015642	07890		LDA	QIDIAG,5
02324	004241	07900		LRLA	1
02325	136000	07910		ERA	0,6
02326	055642	07920		STA	QIDIAG,5
02327	015652	07930		LDA	IRC,5
02330	001010	07940		JAZ	QIRZ
02331	031000	
PAGE 01  BSYM.VCD  10-APR-73  11:45:14.266

02332	001000	07950		JMP	QIRN
02333	100000	
      	      	07960	* LAST WORD OF RECORD   SET C SWITCH
02334	011100	07970	QIEE	LDA	=40000
02335	055672	07980		STA	SWTCHI,5
02336	015552	07990		LDA	ASDW,5
02337	135642	08000		ERA	QIDIAG,5
02340	115632	08010		ORA	QIVERT,5
02341	001010	08020		JAZ	*+5
02342	002346	
02343	045227	08030		INR	LINER,5
02344	001000	08040		JMP	QIRN
02345	100000	
02346	025532	08050		LDB	CIS,5
02347	015472	08060		LDA	ISF,5
02350	116237	08070		ORA	MASK,6
02351	055472	08080		STA	ISF,5
      	      	08090	* PHYSICAL RECORD HAS GOOD CHECKSUMS
      	      	08100	* CHECK ACKNOWLEDGEMENT AND SEE WHICH OUTPUT SECTORS CAN BE CLEARED
02352	015502	08110	QIF	LDA	CAOS,5
02353	005313	08120		DEC	A,AB
02354	145622	08130		SUB	QIFRST,5
02355	151104	08140		ANA	=7
    *@ U�