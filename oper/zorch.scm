File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

1)1	 7 DEC 72
1)	** SYNTAX AND SEMANTICS FOR INITIAL SLAVE COMMANDS
****
2)1	 6 SEPT 72
2)	** SYNTAX AND SEMANTICS FOR INITIAL SLAVE COMMANDS
**************
1)1	<CHAN NAME> ::= <INTEGER>  WITH RANGE 0:127
1)	<TEXT> ::= A SEQUENCE OF CHARACTERS NOT <EOC> OR XSOH OR XSTX OR XETX,
****
2)1	<CHAN NAME> ::= <ALPHA>
2)	<TEXT> ::= A SEQUENCE OF CHARACTERS NOT <EOC> OR XSOH OR XSTX OR XETX,
**************
1)1	           /  <ACKN COMMAND>
1)	           /  <ACKP COMMAND>
1)	           /  <BCKT COMMAND>
1)	           /  <CHOKE COMMAND> 
1)	           /  <BSDATA COMMAND>
1)	           /  <CHAN COMMAND>
1)	           /  <CHOKE COMMAND>
1)	           /  <CLOSE COMMAND>
1)	           /  <CLOSEA COMMAND>
1)	           /  <CREATE COMMAND>
1)	           /  <ECHO COMMAND>
1)	           /  <ECHOR COMMAND>
1)	           /  <ERASE COMMAND>
1)	           /  <ERR COMMAND>
1)	           /  <FCBDATA COMMAND>
1)	           /  <NULL COMMAND>
1)	           /  <OPEN COMMAND>
1)	           /  <PAUSE COMMAND>
1)	           /  <QUIT COMMAND>
1)	           /  <RBLKS COMMAND>
1)	           /  <RENAME COMMAND>
1)	           /  <RTEXT COMMAND>
1)	           /  <SBLKS COMMAND>
1)	           /  <SENDBS COMMAND>
1)	           /  <SENDCP COMMAND>
1)	           /  <SENDFCB COMMAND^^>
1)	           /  <SENDFS COMMAND^^>
1)	           /  <SENDV COMMAND>
1)	           /  <SETBS COMMAND>
1)	           /  <SETCP COMMAND>
1)	           /  <SETFS COMMAND>
1)	           /  <SETV COMMAND>
1)	           /  <STEST COMMAND>
1)	           /  <STEXT COMMAND>
1)	           /  <VAR COMMAND>
****
2)1	           /  <BCKT COMMAND>
2)	           /  <CHAN COMMAND>
2)	           /  <ECHO COMMAND>
2)	           /  <ECHOR COMMAND>
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

2)	           /  <ERR COMMAND>
2)	           /  <NULL COMMAND>
2)	           /  <PAUSE COMMAND>
2)	           /  <QUIT COMMAND>
2)	           /  <RTEXT COMMAND>
2)	           /  <STEXT COMMAND>
2)	           /  <SENDV COMMAND>
2)	           /  <SETV COMMAND>
2)	           /  <VAR COMMAND>
**************
1)1	<ACKN COMMAND> ::= ACKN <INTEGERD>,<INTEGER>,<INTEGER>
1)	<ACKP COMMAND> ::= ACKP <INTEGERD>,<INTEGER>,<INTEGER>
1)	<BCKT COMMAND> ::= BCKT <INTEGER>,<CHAN NAME>
1)	<BSDATA COMMAND> ::= BSDATA <identifier>,<integer>
1)	<CHAN COMMAND> ::= CHAN <CHAN NAME>,<CHAN NAME>
1)	<CHOKE COMMAND> ::= CHOKE <CHAN NAME>,<INTEGER>
1)	<close COMMAND> ::= CLOSE <identifier>
1)	<CLOSEA COMMAND> ::= CLOSEA
1)	<CREATE COMMAND> ::= CREATE <identifier>,<integer>
1)	<ECHO COMMAND> ::= ECHO <INTEGER>,<INTEGER>
1)	<ECHOR COMMAND> ::= ECHOR <INTEGER>,<INTEGER>
1)	<ERASE COMMAND> ::= ERASE <identifier>,<integer>,<integer>
1)	<ERR COMMAND> ::= ERR  <TEXT>
1)	<FCBDATA COMMAND> ::= FCBDATA <identifier>,<varval>
1)	<NULL COMMAND> ::= NULL <TEXT>
1)	<open COMMAND> ::= OPEN <identifier>,<integer>
1)	<PAUSE COMMAND> ::= PAUSE <INTEGER>
1)	<QUIT COMMAND> ::= QUIT <TEXT>
1)	<RBLKS COMMAND> ::=  RBLKS  <idnetifier>,<integer>,<data>
1)	<open COMMAND> ::= OPEN <identifier>,<integer>
1)	<RTEXT COMMAND> ::= RTEXT <IDENTIFIER>
1)	<SBLKS COMMAND> ::=  SBLKS  <identifier>,<integer>
1)	<SENDBS COMMAND> ::=  SENDBS <identifier>
1)	<sendcp COMMAND> ::= SENDCP <idnetifier>
1)	<SENDFCB COMMAND> ::=  SENDFCB<identifier>
1)	<SENDFS COMMAND> ::=  SENDFS  <identifier>
1)	<STEXT COMMAND> ::= STEXT <CHAN NAME>,<IDENTIFIER>
1)	<SENDV COMMAND> ::= SENDV <IDENTIFIER>,<INTEGER>
1)	<SETV COMMAND> ::= SETV <IDENTIFIER>,<INTEGER>,<VAR VALUE>
1)	<SETBS COMMAND> ::=  SETBS  <identifier>,<integer>
1)	<setcp COMMAND> ::= SETCP  <identifier>,<integer>
1)	<SETFS COMMAND> ::=  SETFS  <identifier>, <integer>
1)	<VAR COMMAND> ::= VAR <IDENTIFIER>,<INTEGER>,<VAR VALUE>
****
2)1	<BCKT COMMAND> ::= BCKT <INTEGER>,<CHAN NAME>
2)	     <BSDATA command> ::= BSDATA <identifier>,<integer>
2)	<CHAN COMMAND> ::= CHAN <CHAN NAME>,<CHAN NAME>
2)	<close command> ::= CLOSE <identifier>
2)	<CLOSEA command> ::= CLOSEA
2)	<CREATE command> ::= CREATE <identifier>,<integer>
2)	<ECHO COMMAND> ::= ECHO <INTEGER>,<INTEGER>
2)	<ECHOR COMMAND> ::= ECHOR <INTEGER>,<INTEGER>
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

2)	<ERASE command> ::= ERASE <identifier>,<integer>,<integer>
2)	<ERR COMMAND> ::= ??????
2)	<FCBDATA command> ::= FCBDATA <identifier>,<varval>
2)	<NULL COMMAND> ::= NULL <TEXT>
2)	<open command> ::= OPEN <identifier>,<integer>
2)	<PAUSE COMMAND> ::= PAUSE <INTEGER>
2)	<QUIT COMMAND> ::= QUIT <TEXT>
2)	<RBLKS command> ::=  RBLKS  <idnetifier>,<integer>,<data>
2)	<open command> ::= OPEN <identifier>,<integer>
2)	<RTEXT COMMAND> ::= RTEXT <IDENTIFIER>
2)	<STEXT COMMAND> ::= STEXT <CHAN NAME>,<IDENTIFIER>
2)	<SBLKS command> ::=  SBLKS  <identifier>,<integer>
2)	<SENDBS command> ::=  SENDBS <identifier>
2)	<sendcp command> ::= SENDCP <idnetifier>
2)	<SENDFCB command> ::=  SENDFCB<identifier>
2)	<SENDFS command> ::=  SENDFS  <identifier>
2)	<SENDV COMMAND> ::= SENDV <IDENTIFIER>,<INTEGER>
2)	<SETV COMMAND> ::= SETV <IDENTIFIER>,<INTEGER>,<VAR VALUE>
2)	<SETBS command> ::=  SETBS  <identifier>,<integer>
2)	<setcp command> ::= SETCP  <identifier>,<integer>
2)	<SETFS command> ::=  SETFS  <identifier>, <integer>
2)	<VAR COMMAND> ::= VAR <IDENTIFIER>,<INTEGER>,<VAR VALUE>
**************
1)1	* ACKN - NEGATIVE ACKNOWLEDGEMENT
1)	     <ACKN COMMAND> ::= ACKN <INTEGER>,<INTEGER>,<INTEGER>
1)	          THE ACKN COMMAND IS USED TO ACKNOWLEDGE THE
1)	     NON-RECEIPT OF A BLOCK CHECKSUM OR BLOCK DURING FILE MOVEMENT.
1)	          THE INTEGER OPERANDS ARE:
1)	                1.  CODE# FOR THIS SBLKS SERIES
1)	                2.  BLOCK/CHECKSUM PREDICATE:  1=BLOCK,2=CHECKSUM
1)	                3.  BLOCK SEQUENCE # WITHIN THIS SBLKS SERIES
1)	* ACKP - POSITIVE ACKNOWLEDGEMENT
1)	     <ACKP COMMAND> ::= ACKP <INTEGER>,<INTEGER>,<INTEGER>
1)	     THE ACKP COMMAND IS USED TO POSITIVELY ACKNOWLEDGE THE RECEIPT
1)	     OF A BLOCK CHECKSUM OR BLOCK DURING FILE BLOCK MOVEMENT.
1)	     THE INTEGER OPERANDS ARE:
1)	                1.  CODE# FOR THIS SBLKS SERIES
1)	                2.  BLOCK/CHECKSUM PREDICATE:  1=BLOCK,2=CHECKSUM
1)	                3.  BLOCK SEQUENCE # WITHIN THIS SBLKS SERIES
1)	*BUILD CIRCUIT
****
2)1	*BUILD CIRCUIT
**************
1)1	* BSDATA Command
1)	     <BSDATA COMMAND> ::= BSDATA <identifier>,<integer>
1)	          The block size in bytes of the file with name <identifier>
1)	     is the value of <integer>.
1)	* CHOKE COMMAND
1)	     <CHOKE COMMAND> ::= CHOKE <CHAN NAME>,<INTEGER>
1)	          CHOKE LIMITS THE OUTPUT ON CHANNEL <CHAN NAME> TO THE 
1)	     RATE OF <INTEGER> CHARACTERS PER SECOND.
1)	     DEFAULTS: 
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

1)	          IF <CHAN NAME> IS NIL, THEN THE LAST CHANNEL 
1)	               REFERENCED IN A CHANNEL COMMAND IS USED
1)	          IF <INTEGER> IS NIL, THEN USE THE DEFAULT RATE IN
1)	               THE SLAVE VARIABLE 'CURCHOKE', WHICH IS INITIALLIZED
1)	               TO 20 CHARACTERS PER SECOND.
1)	*CHANNEL ASSIGNMENT
****
2)1	*CHANNEL ASSIGNMENT
**************
1)1	* CLOSE FILE
1)	     <close COMMAND> ::= CLOSE <identifier>
1)	          The existing opened file with filename <identifier> is
1)	     closed.
1)	          The 'CLOSE' command will send an error message to the
1)	     master (1) if the file is already closed or (2) if the file
1)	     does not exist.
1)	          If <identifier> is defaulted, then the filename in the
1)	     slave variable 'curfile' is used.
1)	* CLOSE ALL FILES
1)	     <CLOSEA COMMAND> ::= CLOSEA
1)	          The CLOSEA command closes all files that are open.
1)	     There are no operands.
1)	     <identifier> and the file type <integer> (*** need to specify
1)	     file types, e.g. symbolic, data, etc.)
1)	          If a file already exists then an error message is sent
1)	     to the master.
1)	* CREATE FILE
1)	     <CREATE COMMAND> ::= CREATE <identifier>,<integer>
1)	          The 'CREATE' command creates a new file with the name
1)	*ECHO
****
2)1	*ECHO
**************
1)1	* Erase Block (set cursor and erase)
1)	     <ERASE COMMAND> ::= ERASE <identifier>,<integer>,<integer>
1)	          The cursor position of the file with name <identifier>
1)	     is set to the value of the first <integer>, after which the
1)	     second <integer> number of blocks are erased.  After the
1)	     erase operation, the cursor position remains at the value
1)	     of the first <integer>.
1)	*ERROR
1)	<ERR COMMAND>  ::=  ERR  <TEXT>
1)	* File Control Block
1)	     <FCBDATA COMMAND> ::= FCBDATA <identifier>,<varval>
1)	          The file control block of the file with name <identifier>
1)	     follows in the <varval> list of integers.  The <varval> has
1)	     the following format:
1)	          <integer1>,<integer2>, . . . ,<integern>,
1)	     Where integer
1)	          1.  True/false if the file exists
1)	          2.  File number
1)	          3.  Cursor position
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

1)	          4.  Open/close status
1)	          5.  Open mode
1)	          6.  Block size
1)	          7.  File type
1)	          8.  File size
1)	*NULL
****
2)1	*ERROR
2)	????????
2)	*NULL
**************
1)1	* OPEN FILE
1)	     <open COMMAND> ::= OPEN <identifier>,<integer>
1)	          The already existing file with filename <identifier> will
1)	     be opened in READ,WRITE, or UPDATE mode if <integer> is
1)	     1, 2, or 3 respectively.  A new file is created by the CREATE
1)	     command; the OPEN command will send an error message to the
1)	     master if the given file does not exist.
1)	*PAUSE
****
2)1	*PAUSE
**************
1)1	 *Receive Blocks
1)	     <RBLKS COMMAND> ::=  RBLKS  <idnetifier>,<integer>,<data>
1)	          The RBLKS command is generated in response to SBLKS or
1)	     STEST commands.  The blocks from file with name <identifier>
1)	     (or a pseudonym if generated by STEST) are sent <integer.>
1)	     blocks per RBLKS command.  The <integer>'s are:
1)	          1.  Number of blocks in this RBLKS
1)	          Each block has a header with
1)	          1.  Seq # of this block within SBLKS request or
1)	              STEST request.
1)	          2.  Cursor position
1)	* RENAME File
1)	     <RENAME COMMAND> ::= RENAME <identifier>,<identifier>
1)	          The file name given in the first <identifier> is renamed
1)	     to the name given in the second <identifier>.  The file
1)	     retains its file control block with new name.
1)	*RECEIVED FREE TEXT
****
2)1	*RECEIVED FREE TEXT
**************
1)1	* Send Blocks  (Read)
1)	     <SBLKS COMMAND> ::=  SBLKS  <identifier>,<integer>
1)	          The SBLKS command requests that <integer> number of
1)	     blocks be sent from file with name <identifier>.  The
1)	     blocks are sent via the RBLKS command.  The cursor position
1)	     and block size are taken from the files f.c.b.
1)	* Send Cursor Position
1)	     <sendcp COMMAND> ::= SENDCP <idnetifier>
1)	          The cursor position in the file control block (f.c.b.) of
1)	     the file with name <identifier> is returned to the sender via
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

1)	     a SETCP command.
1)	          If the file does not exist, a 0 is sent as the value.
1)	* Send File Control Block
1)	     <SENDFCB COMMAND> ::=  SENDFCB<identifier>
1)	          The file control block (f.c.b.) of the file with name
1)	     <identifier> is sent to the requestor via the FCB command.
1)	          If the file does not exist, an error message is sent
1)	     in the FCB command.
1)	          If the file exists, but has no f.c.b. then the file is
1)	     opened and closed in order to create one.
1)	* send file size
1)	     <SENDFS COMMAND> ::=  SENDFS  <identifier>
1)	          The file size in bytes of the file with name <identifier> is
1)	     sent to the requestor via a FSDATA command.  If the file does not
1)	     exist, then a zero is sent.
1)	* SEND TEST PATTERN BLOCK
1)	     <STEST COMMAND> ::= STEST
1)	          THE STEST COMMAND CAUSES A TEST PATTERN BLOCK OF 8-BIT CHARS
1)	     TO BE SHIPPED TO THE SENDER.  THE PATTERN IS DETERMINED BY THE
1)	     ZERO THEN THE PATTERN IS THE ITERATIVE SEQUENCE OF 8-BIT CHARACTERS
1)	     STARTING WITH 8 BITS OF ZERO; OTHERWISE, THE PATTERN IS THE 
1)	     ITERATIVE SEQUENCE OF THREE CHARACTERS PACKED IN PATTERN'S 24 BITS.
1)	     THE BLOCKSIZE IS THE STANDARD BLOCK SIZE DETERMINED BY THE VARIABLE
1)	     'STDBLKSZ'.
1)	*SEND TEXT
****
2)1	*SEND TEXT
**************
1)1	* Send Block Size
1)	     <SENDBS COMMAND> ::=  SENDBS <identifier>
1)	          The block size from the file control block (f.c.b) of
1)	     the file with name <identifier> is sent to the requestor
1)	     via a 'BSDATA' command.
1)	*SEND VARIABLE
****
2)1	*SEND VARIABLE
**************
1)1	* Set Block Size
1)	     <SETBS COMMAND> ::=  SETBS  <identifier>,<integer>
1)	          The block size in the f.c.b of the file with name <identifier>
1)	     is set to the value of <integer>.
1)	          If the file does not exist, then an error message is sent
1)	     to the master.
1)	* Set Cursor Position
1)	     <setcp COMMAND> ::= SETCP  <identifier>,<integer>
1)	          The byte cursor position in the file control block (f.c.b.)
1)	     of the file with name <identifier> is set to the value of
1)	     <integer>.  If the new cursor position if larger than the file
1)	     size, then the file size is used and an error message sent to
1)	     the master.
1)	* Set file size
1)	     <SETFS COMMAND> ::=  SETFS  <identifier>, <integer>
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

1)	          The file size in bytes of the file with name <identifier>
1)	     is the value of <integer>.
1)	*SET VARIABLE
****
2)1	*SET VARIABLE
**************
2)1	The File Control Block
2)1	     File names used in slave commands are entered in the symbol table
2)1	with a symbol type of file (S.T.FILE).  The symbol value type is ptr
2)1	(S.V.T.PTR) and the value is the file control block pointer.
2)1	     The file control block (or F.C.B.) contains the information
2)1	necessary to define the status of a file for use by the slave
2)1	command routines.
2)1	     The f.c.b. is usually created at open time.  All fields are
2)1	filled with some value, either given or defaulted.
2)1	     The fields of the f.c.b. are discussed below:
2)1	     1.  fn.  The file number is returned by the 940 open BRS or
2)1	              or the PDP-10 IOCS open routine, and is used by the
2)1	              slave in subsequent file operations.
2)1	     2.  cp.  The cursor position is a byte count, initially 
2)1	              zero at open time.  The cp is changed implicitly by
2)1	              EACH 'READ','WRITE' OR 'ERASE' COMMAND, OR SET 
2)1	              EXPLICITLY BY THE 'SETCP' COMMAND.
2)1	     3.  Open/close indicator.  True if file open, false if not.
2)1	     4.  Open mode.  If the file is open then the mode is one of
2)1	                     the three:
2)1	                        .read
2)1	                        .write
2)1	                        .update  (read and write)
2)1	     5.  Block size.  The block size is a byte count, initially
2)1	                      set to the value of the 'std.blk.size' slave
2)1	                      variable at open time.  The 'setbs' (set block
2)1	                      size) command is used to change this value.
2)1	                      The 'sendbs' command is used to request this
2)1	                      value.
2)1	     6.  File Type.  This is the problem area - what properties are
2)1	                     the same or isomorphic between 940 - PDP10?
2)1	                     See Codie's proposals for file standardization.
2)1	     7.  File name.  The bufferlet string ptr to the filename char-
2)1	                     acter string.
2)1	Variables associated with files
2)1	     1.  std.block.size - used as the block size (in bytes) in 
2)1	              file.control.block when f.c.b is set up.
2)1	* send file size
2)1	     <SENDFS command> ::=  SENDFS  <identifier>
2)1	          The file size in bytes of the file with name <identifier> is
2)1	     sent to the requestor via a FSDATA command.  If the file does not
2)1	     exist, then a zero is sent.
2)1	* Set file size
2)1	     <SETFS command> ::=  SETFS  <identifier>, <integer>
2)1	          The file size in bytes of the file with name <identifier>
2)1	     is the value of <integer>.
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

2)1	* Send Blocks  (Read)
2)1	     <SBLKS command> ::=  SBLKS  <identifier>,<integer>
2)1	          The SBLKS command requests that <integer> number of
2)1	     blocks be sent from file with name <identifier>.  The
2)1	     blocks are sent via the RBLKS command.  The cursor position
2)1	     and block size are taken from the files f.c.b.
2)1	 *Receive Blocks
2)1	     <RBLKS command> ::=  RBLKS  <idnetifier>,<integer>,<data>
2)1	          The RBLKS command is generated in response to SBLKS or
2)1	     STEST commands.  The blocks from file with name <identifier>
2)1	     (or a pseudonym if generated by STEST) are sent <integer.>
2)1	     blocks per RBLKS command.  The <integer>'s are:
2)1	          1.  Number of blocks in this RBLKS
2)1	          Each block has a header with
2)1	          1.  Seq # of this block within SBLKS request or
2)1	              STEST request.
2)1	          2.  Cursor position
2)1	* Set Block Size
2)1	     <SETBS command> ::=  SETBS  <identifier>,<integer>
2)1	          The block size in the f.c.b of the file with name <identifier>
2)1	     is set to the value of <integer>.
2)1	          If the file does not exist, then an error message is sent
2)1	     to the master.
2)1	* Send Block Size
2)1	     <SENDBmand> ::=  SENDBS <identifier>
2)1	          The block size from the file control block (f.c.b) of
2)1	     the file with name <identifier> is sent to the requestor
2)1	     via a 'BSDATA' command.
2)1	* BSDATA Command
2)1	     <BSDATA command> ::= BSDATA <identifier>,<integer>
2)1	          The block size in bytes of the file with name <identifier>
2)1	     is the value of <integer>.
2)1	* Send File Control Block
2)1	     <SENDFCB command> ::=  SENDFCB<identifier>
2)1	          The file control block (f.c.b.) of the file with name
2)1	     <identifier> is sent to the requestor via the FCB command.
2)1	          If the file does not exist, an error message is sent
2)1	     in the FCB command.
2)1	          If the file exists, but has no f.c.b. then the file is
2)1	     opened and closed in order to create one.
2)1	* File Control Block
2)1	     <FCBDATA command> ::= FCBDATA <identifier>,<varval>
2)1	          The file control block of the file with name <identifier>
2)1	     follows in the <varval> list of integers.  The <varval> has
2)1	     the following format:
2)1	          <integer1>,<integer2>, . . . ,<integer4>,
2)1	     Where integer
2)1	          1.  True/false if the file exists
2)1	          2.  File number
2)1	          3.  Cursor position
2)1	          4.  Open/close status
2)1	          5.  Open mode
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

2)1	          6.  Block size
2)1	          7.  File type
2)1	          8.  File size
2)1	* Set Cursor Position
2)1	     <setcp command> ::= SETCP  <identifier>,<integer>
2)1	          The byte cursor position in the file control block (f.c.b.)
2)1	     of the file with name <identifier> is set to the value of
2)1	     <integer>.  If the new cursor position if larger than the file
2)1	     size, then the file size is used and an error message sent to
2)1	     the master.
2)1	* Send Cursor Position
2)1	     <sendcp command> ::= SENDCP <idnetifier>
2)1	          The cursor position in the file control block (f.c.b.) of
2)1	     the file with name <identifier> is returned to the sender via
2)1	     a SETCP command.
2)1	          If the file does not exist, a 0 is sent as the value.
2)1	* Erase Block (set cursor and erase)
2)1	     <ERASE command> ::= ERASE <identifier>,<integer>,<integer>
2)1	          The cursor position of the file with name <identifier>
2)1	     is set to the value of the first <integer>, after which the
2)1	     second <integer> number of blocks are erased.  After the
2)1	     erase operation, the cursor position remains at the value
2)1	     of the first <integer>.
2)1	* RENAME File
2)1	     <RENAME command> ::= RENAME <identifier>,<identifier>
2)1	          The file name given in the first <identifier> is renamed
2)1	     to the name given in the second <identifier>.  The file
2)1	     retains its file control block with new name.
2)1	* CREATE FILE
2)1	     <CREATE command> ::= CREATE <identifier>,<integer>
2)1	          The 'CREATE' command creates a new file with the name
2)1	     <identifier> and the file type <integer> (*** need to specify
2)1	     file types, e.g. symbolic, data, etc.)
2)1	          If a file already exists then an error message is sent
2)1	     to the master.
2)1	* CLOSE ALL FILES
2)1	     <CLOSEA command> ::= CLOSEA
2)1	          The CLOSEA command closes all files that are open.
2)1	     There are no operands.
2)1	* CLOSE FILE
2)1	     <close command> ::= CLOSE <identifier>
2)1	          The existing opened file with filename <identifier> is
2)1	     closed.
2)1	          The 'CLOSE' command will send an error message to the
2)1	     master (1) if the file is already closed or (2) if the file
2)1	     does not exist.
2)1	          If <identifier> is defaulted, then the filename in the
2)1	     slave variable 'curfile' is used.
2)1	* OPEN FILE
2)1	     <open command> ::= OPEN <identifier>,<integer>
2)1	          The already existing file with filename <identifier> will
2)1	     be opened in READ,WRITE, or UPDATE mode if <integer> is
File 1)	DSK:SLCMD1	created: 1503 07-DEC-72
File 2)	DSK:SLCMD1.BKP	created: 1249 04-DEC-72

2)1	     1, 2, or 3 respectively.  A new file is created by the CREATE
2)1	     command; the OPEN command will send an error message to the
2)1	     master if the given file does not exist.

   @?