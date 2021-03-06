SCHEMA CAIS
% Version B9.2, File CAIS.SCH, Feb. 24, 1976, VEV
Changes format of TERM.CODE %
% Version B9.1, File CAIS.SCH, Feb. 19, 1976, VEV
Adds relation SPEC.CHAR for Convert routine %
% Version B9.0, File CAIS.SCH, Feb. 17, 1976, VEV
Merges all detail and royalty definitions and relations
Adds new relations and associated fields:
USE.COUNTS, SESS.LOG, HOST.LOG which are used by USER.LOG
and USER.GRAPH procedures.
Also adds relation CONNECT & TRU.370 for 370 session records %
% Version A8.5, Jan. 21, 1975, VEV
Add USR.NAM into NET & NET.SUSPENSE relations %
% Version A8.4, Dec. 22, 1975, VEV
Changes TSTART & TSTOP to decimal of hour vformat, 2N.4N %
% Version A8.3, Dec. 18, 1975, VEV
Removes field REC.TYP from relations TRAN & TRAN.BACKUP
Also changes PROD.CODE to RESOURCE in relation MIN.QTY %
% Version A8.2, Dec. 17, 1975, VEV
Changes definition of RESOURCE to 5N %
% Version A8.1, Dec. 12, 1975, VEV
Changes PREM relation (see) so that all charge information goes
in TRAN %
% Version 8.0, Dec. 11, 1975, VEV
Adds fields to detail relations, changes all names to standard
abbreviations %
% For centralized accounting - contains all detail and common databases %
BEGIN

% Use FNAM "P" option to find usage of FIELDS %
FIELDS


ACC.CALLS       AS '6Z',        % RTYCHG - # calls to RTYCHG %
ACC.TRU         AS '12Z.4N',    % RTYCHG - accumulated TRU'S %
ACC.WRITES      AS '6Z',        % RTYCHG - # RTYCHG disc writes %
ACTG.SYS.NAM    AS '15C',       % Computer system name %
ACTG.SYS.TYP    AS '2Z',        % Accounting system type %
AMT             AS '7Z.2N',     % Currency amounts %
AREA            AS '2Z',        % Tymshare area - currently DIST./100 %
ATTN            AS '51C',       % Attention line part of address %
BASE.TRU.AMT    AS '7Z.2N',     % RTY - %
BILL.BIT        AS '1C'  VERIFY BILL.BIT = 'Y' OR 'N', % Billable flag %
BILL.DATE       AS DATE 'YYMMDD',  % Billing date %
BILL.START      AS DATE 'YYMMDD',  % Start of billing period %
BLK.TIME        AS '10Z', % N.A. input block time (secs. since 1/1/1974) %
CHAR.IN         AS '1C',        % SPECIAL CHAR. TRANS., INPUT %
CHARS.OUT       AS '2C',        % SPECIAL CHAR. TRAN., OUTPUT %
CHRG.BACK       AS '1C',        %RTYACT.CHG-CHARGE BACK SALESMAN%
CHRG.BACK.AMT   AS '7Z.2N',     % RTY - revenue adjustment amount %
CHRG.CODE       AS '2Z',        %RTYCHG CHARGE CODE%
CHRG.IDX        AS '2Z',        %RTYCHG - NODE SET #%
CHRG.PRU        AS '12Z.4N',    % RTYCHG - marked up TRU's %
CIN             AS '9Z',        % Input character count %
CITY            AS '13C',       % City %
CLASS           AS '2C',        % Device class for 370 %
CNC.AMT         AS '7Z.2N',     % RTY - for CNC hours %
CNC.HRS         AS '7Z.4N',     % RTY - connect hours %
CNC.MIN         AS '12Z',
COUNTRY         AS '5C',        % Country %
COUT            AS '9Z',        % Output char. count %
CRDB            AS '1C',        % Credit or debit flag %
CRDB.CODE       AS '2N',        % Code for type of CRDB %
CURRENCY        AS '2Z',        % Currency code %
CUS.AREA        AS '2Z'  VERIFY CUS.AREA LE 25, % Customer area %
CUS.DET.CODE    AS '3Z',
CUS.DIST        AS '3Z'  VERIFY CUS.DIST LE 255, % Customer district %
CUS.NAM         SAME AS ATTN,   % Customer name %
CUS.NUM         AS '5Z'  VERIFY CUS.NUM GE 0 AND LE 32535, % Customer number %
CUS.REG         AS '1Z'  VERIFY CUS.REG LE 2,  % Customer region %
CUS.SLSM        AS '2Z',        % Customer salesman %
CUTOFF.DATE     AS DATE 'YYMMDD', % Cutoff date %
DAY.SET         AS '2Z',
DET.CODE        AS '3Z',        % Detail code %
DEV.CODE        AS '8C',        % Device code %
DFLT.PRIC       AS '2Z',        % Default pricing %
DIST            AS '3Z',        % District no. %
DLVRY.ORDER     AS '72C',       % Delivery order no. %
DST             AS '1C'  VERIFY DST = 'Y' OR 'N', % Daylight S. T. flag %
DSTART          AS DATE 'YYMMDD', % Start date %
DSTOP           SAME AS DSTART,    % Stop date %
END.BLK.TIME    SAME AS BLK.TIME, % Ending block time for N.A. Block control %
EXTEND.DATE     SAME AS DSTART,
FLAG            AS '1N',        % File name flag %
GAN             AS '6Z'  VERIFY GAN GE 0 AND LE 777777, %Group account no.%
HOLIDAY.CODE   AS '2N',        % Holiday code, controls holiday sets %
HOST            AS '3Z',        % Host computer %
HOST.PORT       AS '3Z',        % Host computer port (for NAC) %
HOUR            AS '2Z.3N',     %TIME FUNCTION / 3600%
INHOUSE.COUNT   AS '5Z',        % Accumulated count of inhouse users %
INHOUSE.SESS    AS '8Z',        % Count of sessions by day %
IN.CODE         AS '2Z',        % InterData record code %
INIT.CODE       AS '2Z',        % Initiation code %
INV.CODE        AS '3Z',        % Invoice code %
INV.LINE.ORDER  AS '5Z',
IO.CNT          AS '10Z',       % for TRU.370 %
JOB             AS '3Z',        % Computer job no. %
JOB.CLASS       AS '1C',        % Batch/on-line code %
JOB.PRTY        AS '2Z',        % Job priority %
LANGUAGE        AS '2Z',        % Language code %
LOC             AS '5C',        % Locater for Session no. %
MAP.CODE        AS '5Z',
MAX.DSTART      SAME AS DSTART, % Latest DSTART logged in counters %
MIN.CODE        AS '2Z',        % Minimum code %
MIN.DSTART      SAME AS DSTART, % Earliest DSTART logged in counters %
MODEL           AS '2C',        % Device model for 370 %
NEXT            AS '4C',        % only key in INV.LOOP.CTL %
NEXT.AREA       SAME AS CUS.AREA,   % records next area to be done %
NEXT.DIST       SAME AS CUS.DIST,   % records next dist to be done %
NODE            AS '5Z',        % Node no. %
NODE.DEST       AS '5Z',        % Destination Node no. %
NUM.BLKS        AS '6Z',        % RTYCHG - # reads to get CHRG.IDX %
OUTHOUSE.COUNT  AS '5Z',        % Accumulated count of outhouse users %
OUTHOUSE.SESS   AS '8Z',        % Count of sessions by day %
PAYMENT.CODE    AS '1N',        % RTYCHG-PAY ON TRU OR PRU CODE%
PROCESS.CODE    AS '1Z',        % RTY - processed CNC, TRU charge %
                                % (For multiple vendor payments) %
PROG.NAM        AS '8C',        % Royalty program name (for 370 only ) %
PO              AS '30C',       % Purchase order no. %
PO.DSTART       SAME AS DSTART,    % Purchase order start date %
PO.DSTOP        SAME AS DSTART,    % Purchase order stop date %
PO.MAX          AS '7Z',        % Purchase order maximum %
PORT            AS '3Z',        % Host port %
PRIC.CODE       AS '3Z',        % Pricing code %
PREM.CONNECT    AS '2Z.3N',     % Premiium connect time %
PRIME           AS '3Z',        % Prime code %
PRIME.CODE      AS '2ZZ',       
PRIME.P         AS '1C'  VERIFY PRIME.P = "Y" OR "N" OR " ",
PROD.CODE       AS '5N',        % Product code %
PROD.DESCR      AS '27C',       % Product description %
PROG.CODE       AS '5Z',        % RTYCHG-PROGRAM CODE #%
PROGRAM.NAM     AS '11C',       % RTYCHG-PROGRAM NAME%
PROJ.CODE       AS '12C',       % Project code %
PROJ.CODE.CNT   AS '1Z',        % Input pj code group count %
PROJ.CODE.GRP   AS '3C',        % Input project code group %
PRU             AS '8ZV2N',     % Premium usage time %
PRU.RATE        AS '7Z.4N',     % RTYCHG-(MULTIPLIER, ADDER, ETC.)%
PTALE.NAM       AS '10C',       % PTALE file names, incl. Lockheed host %
QTY             AS '10ZV3N',    % Amount of item %
RADDR           AS '4C',        % Real address (used for 370) %
RATE            AS '5Z.4N',     % Rate for this quantity %
REC.TYP         AS '5Z',        % Record type %
REGION          AS '1Z',        % Region %
RES.MIN         AS '2N',        % Resource minimum %
RESOURCE        AS '5N',        % Resource (replaces old product code) %
SEGMENT.TIME    AS '8Z.2N',     % for TRU.370 %
SESS            AS '8Z',        % Session number %
SIC.GRP         AS '1N',        % SIC Sub-group code %
SIC.GRP.DESCR   AS '50C',       % SIC.GRP Description %
SIC.MJR         AS '2N',        % Standard Ind. Code (SIC) major code %
SIC.MJR.DESCR   AS '50C',       % SIC.MJR description %
SIC.PROD        AS '1N',        % SIC Product group code %
SIC.PROD.DESCR  SAME AS SIC.MJR.DESCR,  % SIC.PROD Description %
SRC.FNAM        AS '10C',       % Source file name %
START.BLK.TIME  SAME AS BLK.TIME, % Starting block time for NA Blk control %
STATE           AS '2C',        % State %
STATUS          AS '1C',        % Accounting status codes %
STEP.LEVEL      AS '4ZN',       % Controls pricing levels %
STREET1         SAME AS ATTN,   % Street address first line %
STREET2         SAME AS ATTN,   % Street address second line %
STRING          AS '25C',
STRING1         SAME AS STRING,
STD.CODE        AS '1C',        % Y/N for standard or non-std. invoice %
SUPER           AS '1N',        % Supervisor number %
TAX.AMT         AS '6Z.2N',     % Computed amount of taxes %
TAX.CODE        AS '5Z',        % Code that determines tax rate %
TERM.CODE       AS '2N',        % Termination type code %
TID             AS '3Z',        % Terminal type %
TIME.OF.COUNT   AS '2Z',        % Minutes of day/15 for USER.GRAPH %
TOTAL.CPU       AS '7Z.2N',     % for TRU.370 %
TRAN.CODE       AS '2Z',        % Transaction code (370) %
TRAN.DATE       SAME AS DSTART,    % Transaction date %
TRAN.TIME       AS '4N',        % Transaction time %
TRAN.TYP        AS '1N',        % Transaction type (for 370) %
TRK.CODE        AS '2Z',        % Tracking code %
TRU.LIM         AS '7Z.4N',     % RTYCHG - TRU threshold %
TSTART          AS '2N.4N'  VERIFY TSTART LT 24, % Start time %
TSTART.HR       AS 'ZN'   VERIFY TSTART.HR LT 24,  % Start hour %
TSTART.MIN      AS 'NN'   VERIFY TSTART.MIN LT 60,  % Start min. %
TSTOP           SAME AS TSTART  VERIFY TSTOP LT 24, % Stop time %
TSTOP.HR        AS 'ZN'  VERIFY TSTOP.HR LT 24,  % Stop hour %
TSTOP.MIN       AS 'NN'  VERIFY TSTOP.MIN LT 60, % Stop min. %
TYM             AS '1C'  VERIFY TYM = 'Y' OR 'N', %Tymshare flag%
TYM.RATE        AS '2B.4N',     % RTYACT.CHG-TYMSHARE RATE/TRU OR PRU%
TYM.RTY.AMT     AS '7Z.2N',     % RTY - TYMSHARE PAYMENT %
TYP             AS '2C',        % Device type for 370 %
TZ              AS '2Z',        % Time zone %
TZ.NAM          AS '10C',       % Time zone name %
UNIQ            AS '5N',        % Forces uniqueness %
UNIQ1           AS '5C',        % UNIQUENESS FOR 370 STORAGE %
UNIT.DESCR      AS '10C',       % Units description %
UNIT.TYP        AS '2Z',        % Unit type %
UPD.TYP         AS '1C',        % Update type %
USR.DET.CODE    SAME AS DET.CODE,
USR.DIST        AS '3Z',        % User district %
USR.NAM         AS '12C',       % User name %
USR.SLSM        AS '2Z',        % User salesman %
USR.TZ          AS '2Z',        % User time zone %
UUN             AS '6Z'  VERIFY UUN GE 1,       % Universal user no. %
V.CPU           AS '5Z.2N',     % for TRU.370 %
V.PRINTER.IO    AS '8Z',        % for TRU.370 %
V.PUNCH.IO      AS '8Z',        % for TRU.370 %
V.READER.IO     AS '8Z',        % for TRU.370 %
VAR.PRIC        AS '2Z',        % Variable pricing flag %
VEN.NAM         AS '24C',       %RTYACT.CHG - VENDOR NAME%
VEN.RATE        AS '2B.4N',     %RTYACT.CHG - VENDOR RATE%
VEN.RTY.AMT     AS '7Z.2N',     % RTY - vendor payment %
WR.DATE         SAME AS CUTOFF.DATE,  % Write date - date data entered %
XL              SAME AS ATTN,    % Extra line for address %
ZIP             AS '5C'         % Zip code %

% Relation definitions %


% Session Record Common %
RELATION SESSION IS
  KEY
    UUN, ACTG.SYS.TYP, HOST, PROJ.CODE, DSTART, TSTART
  DATA
    TZ, JOB, JOB.CLASS, JOB.PRTY, SESS

% Session Transaction details %
RELATION TRAN IS
  KEY
    SESS, RESOURCE, DSTART, TSTART, CRDB, RADDR
  DATA
    DSTOP, TSTOP, CRDB.CODE, NODE, PORT, TID, QTY,
    RATE, AMT, TAX.CODE, TAX.AMT,
    PROD.CODE, DAY.SET, PRIME.P, INV.LINE.ORDER, EXTEND.DATE

% Session connect details %
RELATION CONNECT IS
  KEY
    SESS, DSTART, TSTART, CRDB
  DATA
    DSTOP, TSTOP, CRDB.CODE, NODE, PORT, TID, QTY, TERM.CODE, CIN, COUT,
    RATE, AMT, TAX.CODE, TAX.AMT,
    PROD.CODE, DAY.SET, PRIME.P, INV.LINE.ORDER, EXTEND.DATE,
    RESOURCE

% Session Premium
QTY holds TRU's already included in the total
TRU'S for a session (a TRAN record).  (PRU's are now recorded as a
QTY in a TRAN  record with the premium RESOURCE) %
RELATION PREM IS
  KEY
    SESS, RESOURCE, DSTART, TSTART
  DATA
    QTY, PRU, PREM.CONNECT

% Total TRU components for 370 session records %
RELATION TRU.370 IS
  KEY  UUN, HOST, JOB.CLASS
  DATA IO.CNT, SEGMENT.TIME, TOTAL.CPU, V.CPU, V.PRINTER.IO,
    V.PUNCH.IO, V.READER.IO, CIN, COUT, QTY

% Session no. log,
For LOC = "LSESS", last session number for input.
For LOC = "XSESS", last session number used for data into counters.
For  LOC = "CSESS", Cutoff Session number for last
output to billing and CUTOFF.DATE has first date of the accounting period %
RELATION SESS.LOG IS
  KEY LOC
  DATA SESS, CUTOFF.DATE

% Minimum and maximum dates of stored detail data %
RELATION HOST.LOG IS
  KEY
    HOST
  DATA
    MIN.DSTART, MAX.DSTART

% Program name to product code table %
RELATION PROG.NAMS IS
  KEY PROG.NAM, JOB.CLASS
  DATA RESOURCE

% 370 device information to product code table %
RELATION DEV.NAM IS
  KEY CLASS, TYP, MODEL
  DATA RESOURCE


% Permanent Storage Master
contains all processed transaction records %

RELATION STORAGE IS
  KEY  UUN, ACTG.SYS.TYP, HOST, PROJ.CODE, PROD.CODE, UNIQ1, DSTART
  DATA DSTOP, QTY, UNIT.TYP

% Record of User Validations %
RELATION USR.VAL IS
  KEY UUN, ACTG.SYS.TYP, HOST, PROJ.CODE, DSTART, PROD.CODE


% Permanent storage Input File Names %
RELATION SRC.FNAM.STG IS
  KEY SRC.FNAM
  DATA WR.DATE, FLAG

% Session record input file names %
RELATION SRC.FNAM.SESS IS
  KEY SRC.FNAM
  DATA WR.DATE, FLAG

% Accounting System to Host computer number table %
RELATION SYS.HOST IS
  KEY HOST, DSTART
  DATA DSTOP, ACTG.SYS.TYP

% Host computer name table %
% HOSTNM will be replaced by relation ACTG.SYS.TYP.DESCRS %
RELATION HOST.NAM IS
  KEY ACTG.SYS.TYP
  DATA ACTG.SYS.NAM

% Input data cutoff date
for LOC =
PSIDT   Previous input cutoff date for Permanent storage files
SRIDT   Previous input cutoff date for Session Record files
CTLDT   New final cutoff date %
RELATION IN.CNTL.DATE IS
  KEY LOC
  DATA CUTOFF.DATE

% For a given PROD.CODE, supplies a minimum quantity %
% MINQTY will be replaced by relation MIN.QTYS %
RELATION MIN.QTY IS
  KEY RESOURCE
  DATA QTY


% Description of product code, units %
RELATION PROD.DESCRS IS
  KEY PROD.CODE
  DATA PROD.DESCR, UNIT.DESCR, RATE

% Validation database %
RELATION VAL IS
  KEY UUN, ACTG.SYS.TYP, HOST, DSTART
  DATA DSTOP

% Address database %
RELATION CUS.ADDR IS
  KEY CUS.NUM, ACTG.SYS.TYP
  DATA CUS.NAM, ATTN, STREET1, STREET2, CITY, STATE, ZIP, XL,
    COUNTRY, DLVRY.ORDER

% User database %
RELATION USER IS
  KEY UUN, DSTART
  DATA GAN, USR.NAM, USR.DIST, CUS.NUM, CURRENCY, LANGUAGE, TYM,
    BILL.BIT, DSTOP

% User accounting system type database %
RELATION USER.ACTG.SYS IS
  KEY UUN, ACTG.SYS.TYP, DSTART
  DATA USR.SLSM, TRK.CODE, PRIC.CODE, USR.DET.CODE, PRIME.CODE, USR.TZ, DSTOP,
    HOLIDAY.CODE, RES.MIN, DST, MAP.CODE

% Customer database %
RELATION CUSTOMER IS
  KEY CUS.NUM, ACTG.SYS.TYP, DSTART
  DATA CUS.DIST, CUS.AREA, CUS.REG, DSTOP, CUS.DET.CODE, INV.CODE, CUS.SLSM,
    MIN.CODE, PO, PO.MAX, PO.DSTART, PO.DSTOP, VAR.PRIC, DFLT.PRIC

% Relates customer area to regions %
RELATION REG.AREA IS
  KEY CUS.REG, CUS.AREA

% Relates UUN to customer number and district %
% Built from CUSTOMER & USER relations at load time %
RELATION DIST.UUN IS
  KEY CUS.DIST, CUS.NUM, UUN

% Relates district to area %
RELATION AREA.DIST IS
  KEY CUS.AREA, CUS.DIST

% List of regions built from REG.AREA %
RELATION AREAS IS
  KEY CUS.AREA

% SIC Major industry code %
RELATION SIC.MJRS IS
  KEY SIC.MJR
  DATA SIC.MJR.DESCR

% SIC Sub-group code %
RELATION SIC.GRPS IS
  KEY SIC.MJR, SIC.GRP
  DATA SIC.GRP.DESCR

% SIC Product code %
RELATION SIC.PRODS IS
  KEY SIC.MJR, SIC.GRP, SIC.PROD
  DATA SIC.PROD.DESCR

% Customer number to Standard Industry Code (SIC) and status %
RELATION CUS.SIC.STATUS IS
  KEY CUS.NUM
  DATA SIC.MJR, SIC.GRP, SIC.PROD, STATUS, CUS.NAM

% Network Accounting Detail database %
RELATION NET IS
  KEY   UUN, ACTG.SYS.TYP, HOST, PROJ.CODE, DSTART, TSTART
  DATA  NODE, TID, PORT, HOST.PORT, DSTOP, TSTOP,
        CIN, COUT, TERM.CODE, NODE.DEST, USR.NAM

% Temporary storage for Network Accounting Detail
Contains all incomplete session records %
RELATION NET.SUSPENSE IS
  KEY   SESS
  DATA  UUN, HOST, DSTART, TSTART,
        NODE, TID, PORT, HOST.PORT, DSTOP, TSTOP,
        CIN, COUT, TERM.CODE, NODE.DEST, USR.NAM, PROJ.CODE.CNT

% For accumulating project code temporary partial fields %
RELATION PROJ.CODE.SUSP IS
  KEY   SESS, BLK.TIME, PROJ.CODE.CNT
  DATA  PROJ.CODE.GRP

% Supervisor's Current Node for locating current session %
RELATION CUR.NODE IS
  KEY   SUPER
  DATA  NODE.DEST

% Active Session numbers for each node %
RELATION NODE.SESS IS
  KEY   SUPER, NODE.DEST
  DATA  SESS

% Network block start and stop times
for checking for contiguous data blocks %
RELATION BLOCK.CONTROL IS
  KEY   START.BLK.TIME
  DATA  END.BLK.TIME

% FOR ROYALTY PAYMENTS AND REPORTS%

% For holding summarized data from each PDP 10 %
RELATION VENDOR.DATA IS
  KEY
    PROG.CODE, CHRG.IDX, CHRG.CODE, UUN, TRU.LIM, PRU.RATE
  DATA
    GAN, ACC.TRU, CHRG.PRU, CNC.MIN, ACC.CALLS, ACC.WRITES, NUM.BLKS


% list of authorized abbreviations %
RELATION STD.ABBR IS
  KEY STRING, STRING1


% Count of system users by 15 min. increments through time period %
RELATION USE.COUNTS IS
  KEY
    HOST, DSTART, TIME.OF.COUNT
  DATA
    INHOUSE.COUNT, OUTHOUSE.COUNT

% Count of no. of sessions per day (for statistics) %
RELATION SESS.COUNT IS
  KEY
     HOST, DSTART
  DATA
     INHOUSE.SESS, OUTHOUSE.SESS

%        ROYALTY & DETAIL ADDED RELATIONS %

% THE FOLLOWING TWO RELATIONS SHOULD REPLACE REG.AREA & AREA.DIST%
RELATION AREA.REG IS
KEY AREA
DATA REGION

RELATION DIST IS
KEY DIST
DATA AREA

%DATE RANGE OF MONTHLY BILLING PERIOD%
RELATION BILL.PERIOD IS
 KEY NEXT
 DATA BILL.START, BILL.DATE

% FOR ROYALTY PAYMENTS AND REPORTS%

%PTALE FILE NAMES%
RELATION PTALE.FILES IS
 KEY PTALE.NAM

% FOR CROSS REFERENCE BET CUS.DIST, USR.DIST, CUS.SLSM, USR.SLSM AS
  THE DATA IN CUSTOMER IS CURRENTLY NOT ACCURATE%
RELATION CUS.DIST.SLSM IS
 KEY CUS.NUM
 DATA
   CUS.DIST, CUS.NAM, CUS.SLSM, INV.CODE, AREA, REGION

% FOR CONVERTING FILE (BILLING10)RTYACT.CHG TO MAGNUM%
RELATION VEN.CHRG IS
  KEY
    VEN.NAM, PROG.CODE, CHRG.IDX, CHRG.CODE, DSTART
  DATA
    PROGRAM.NAM, VEN.RATE, TYM.RATE, PAYMENT.CODE, CHRG.BACK, DSTOP

%FOR ADDING AMOUNTS TO VENDOR DATA AND ALLOWING MULTIPLE
VENDOR PAYMENTS%
RELATION VEN.EXT IS
  KEY
    VEN.NAM, PROG.CODE, UUN, HOST, CHRG.BACK, PROCESS.CODE
  DATA
    PROGRAM.NAM, CNC.HRS, ACC.TRU, CHRG.PRU, CNC.AMT, BASE.TRU.AMT,
    TYM.RTY.AMT, VEN.RTY.AMT

%FOR RTY SUMMARY BY HOSTS%
RELATION RTY.DATA.HOST IS
 KEY
   PROG.CODE, CHRG.IDX, CHRG.CODE, TRU.LIM, PRU.RATE, UUN, HOST
 DATA
   GAN, ACC.TRU, CHRG.PRU, CNC.MIN, ACC.CALLS, ACC.WRITES, NUM.BLKS

% FOR REPORT REVENUE ADJUSTMENT FOR BOB HALL.%
RELATION CUS.EXT IS
 KEY
   CUS.DIST, CUS.NUM, PROG.CODE, USR.NAM, UUN, HOST
 DATA
   CUS.NAM, CUS.SLSM, PROGRAM.NAM, CNC.HRS, CNC.AMT, ACC.TRU,
   VEN.NAM, AREA, REGION, PRIC.CODE, USR.SLSM, CHRG.BACK, CHRG.PRU,
   BASE.TRU.AMT, TYM.RTY.AMT, VEN.RTY.AMT, CHRG.BACK.AMT, STD.CODE

% Relations that follow are used for the Invoice and Tracking procedure %


% Relation INV.LOOP.CTL is used to record the next area to be processed
in the big invoice loop.  %

RELATION INV.LOOP.CTL IS
  KEY
        NEXT            % always has the value "NEXT" %
  DATA
        NEXT.AREA, NEXT.DIST


% Relation CUS.SET controls the order and number of customers that will 
  be invoiced or tracking files built for each time through the big
  invoice loop in the  INV.TRK procedure.       %

RELATION CUS.SET IS     
  KEY
        CUS.DIST,               % from DIST.UUN %
        CUS.NUM,                % from DIST.UUN %
        CURRENCY,               % from USER  %
        ACTG.SYS.TYP,           % from CUSTOMER  %
        UUN                     % from DIST.UUN %
  DATA
        USR.NAM,                % from USER  %
        GAN,                    % from USER %
        CUS.DET.CODE,           % from CUSTOMER  %
        INV.CODE,               % from CUSTOMER  %
        LANGUAGE,               % from USER %
        PRIC.CODE,              % from CUSTOMER, for stg qty discounts %
        CUS.NAM                 % from CUS.ADDR   %

% Relation DETAIL is used to write the detail report %

RELATION DETAIL IS              % Source: %
  KEY
        CUS.NUM,                % CUS.SET %
        CURRENCY,               % CUS.SET %
        ACTG.SYS.TYP,           % CUS.SET %
        UUN,                    % CUS.SET %
        HOST,                   % SESSION %
        PROJ.CODE,              % SESSION %
        DSTART,                 % TRAN    %
        TSTART,                 % TRAN    %
        PROD.CODE,              % TRAN    %
        SESS,                   % TRAN    %
        CRDB,                   % TRAN    %
        RADDR                   % TRAN    %
  DATA
        TZ,                     % SESSION %
        JOB,                    % SESSION %
        JOB.CLASS,              % SESSION %
        JOB.PRTY,               % SESSION %
        DSTOP,                  % TRAN    %
        TSTOP,                  % TRAN    %
        TSTART.HR,
        TSTART.MIN,
        TSTOP.HR,
        TSTOP.MIN,
        CRDB.CODE,              % TRAN    %
        NODE,                   % TRAN    %
        PORT,                   % TRAN    %
        TID,                    % TRAN    %
        QTY,                    % TRAN    %
        RATE,                   % TRAN    %
        AMT,                    % TRAN    %
        TAX.CODE,               % TRAN    %
        TAX.AMT,                % TRAN    %
        INV.LINE.ORDER,         % TRAN    %
        GAN,                    % CUS.SET %
        USR.NAM                 % CUS.SET %

% Relation INV.SUMM is used to write the invoice report. %
% Each instance will appear as a separate line item on an invoice. %
% An invoice is produced for each c@CUS.NUM | CURRENCY combination. %

RELATION INV.SUMM IS            % Source: %
  KEY
        CUS.NUM,                % CUS.SET %
        CURRENCY,               % CUS.SET %
        ACTG.SYS.TYP,           % CUS.SET %
        INV.LINE.ORDER,         % TRAN    %
        PROD.CODE,              % TRAN    %
        STEP.LEVEL              % Initially set to 1; if additional %
                                % records are added to STEP.LEVEL pricing, %
                                % then STEP.LEVELs > 1 will come from RATES. %
  DATA
        QTY,                    % TRAN    %
        AMT,                    % TRAN    %
        TAX.AMT                 % TRAN    %

% UUN summary relation is used for tracking and detail %

RELATION UUN.SUMM IS            % Source: %
  KEY
        UUN,                    % CUS.SET %
        ACTG.SYS.TYP,           % CUS.SET %
        PROJ.CODE,              % SESSION %
        PROD.CODE               % SESSION %
  DATA
        QTY,                    % TRAN    %
        AMT,                    % TRAN    %
        TAX.AMT,                % TRAN    %
        CUS.NUM,                % CUS.SET %
        CURRENCY                % CUS.SET %

% Special character translation table %
RELATION SPEC.CHAR IS
  KEY CHAR.IN
  DATA CHARS.OUT

END % Of SCHEMA CAIS %
    9 [z