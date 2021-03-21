SCHEMA TYCOMAN % EXT: TYCOMN.SCH  MAG: GCS8Z2.M##2 %
% Tymshare Commission Analysis %
% Original version:  1975.04.02  / Dick Ouellette (GIGO) %
% Current version:  1976.03.29    /Steve albanese %
%
Changes:
    1976.03.29 - Added LEASCO Transaction field for tracking and reports.
%

BEGIN


% Procedure control fields %

FIELDS
  RESPONSE      AS '5C'         VERIFY RESPONSE = 'Y' OR 'N' OR 'y' OR 'n'
                                        OR 'YES' OR 'NO' OR 'yes' OR 'no'
                                        OR 'Yes' OR 'No',
  REC.IN        AS 'ZZ,ZZN',
  REC.ACCEPTED  SAME AS REC.IN,
  REC.REJECTED  SAME AS REC.IN


% Source fields %

FIELDS
  REQUEST       AS '3C'         VERIFY REQUEST = 'INS' OR 'DEL'
                                        OR 'CHG',
  SREV.TRANS.TYPE AS 'NN'      VERIFY SREV.TRANS.TYPE  EQ 1 OR 2 OR 3 OR 4
                                                OR 7 OR 11 OR 14 OR 15 OR 60
                                                OR 61 OR 65 OR 17,
  AMOUNT        AS '7ZN.NN-'


% Key fields %

FIELDS                                       % Five-character transaction code %
  YR.PROCESSED  AS 'ZN',
  MO.PROCESSED  AS 'ZN'  VERIFY MO.PROCESSED GE 1  AND  LE 12,
  AREA.2N       AS 'NN',                                               % AREAN %
  DISTRICT.3N   AS '3N',                                               % DISTN %
  SALESNUM.5N   AS '5N',                                               % SALSN %
  EMPLOYE.NUM   AS '5N',                                               % EMPLN %
  NEW.EMPLOYE.NUM SAME AS EMPLOYE.NUM,
  COM.PLAN      AS '5C'         VERIFY COM.PLAN = 'ATM' OR 'DM'        % CPLAN %
                                        OR 'AC' OR 'SR' OR 'BM',
  TRANSACTION   AS '5C'

% General data fields %

FIELDS                                       % Five-character transaction code %
  HOUSE.ACCOUNT AS '5N',
  REGION.2N     AS 'NN',                                               % REGIN %
  GROUP.NAME    AS '40C',                                              % GNAME %
  E.LAST.NAME   AS '20C',                                              % ELNAM %
  E.FIRST.NAME  SAME AS E.LAST.NAME,                                   % EFNAM %
  E.MIDDLE.NAME SAME AS E.LAST.NAME                                    % EMNAM %


% Commission plan data fields %

FIELDS                                       % Five-character transaction code %
  PLAN.STATUS   AS 'C'          VERIFY PLAN.STATUS = 'A' OR 'I',       % PSTAT %
  BEGIN.MO      AS 'NN',                                               % BEGMO %
  BASE.REV.Q    AS '7ZN.NN-',                                          % BREVQ %
  NEW.REV.Q.BAS SAME AS BASE.REV.Q,                                    % NRVQB %
  NEW.REV.Q.INC SAME AS BASE.REV.Q,                                    % NRVQI %
  DRAW.PER.BEG  AS 'ZN' VERIFY DRAW.PER.BEG GE 0  AND  LE 12,          % DRBEG %
  DRAW.PER.END  AS 'ZN' VERIFY DRAW.PER.END GE 0  AND  LE 12,          % DREND %
  DRAW.TYPE     AS 'C'          VERIFY DRAW.TYPE = 'R' OR 'N' OR ' ',  % DRTYP %
  DRAW.AMOUNT   SAME AS BASE.REV.Q,                                    % DRAMT %
  INCENT.BASE.Q SAME AS BASE.REV.Q,                                    % INCBQ %
  PC.NEW.REV    AS 'ZZN.N-',                                   % [percent]NREV %
  PC.N.R.O.Q    SAME AS PC.NEW.REV,                            % [percent]NROQ %
  PC.N.ACC.REV  SAME AS PC.NEW.REV,                            % [percent]NACR %
  PC.TASC.DEVEL SAME AS PC.NEW.REV,                            % [percent]TDEV %
  PC.PROD.PROF  SAME AS PC.NEW.REV                             % [percent]PPRF %


% Product transaction fields %
FIELDS
  PRD.SIGN      AS 'C'     VERIFY PRD.SIGN  EQ 'C' OR 'D' OR 'I',
  PRD.CLASS     AS '4C'         VERIFY PRD.CLASS =
                                        'CASH' OR 'CLS' OR 'CMAI' 
                                        OR 'EQPT' OR 'FRGT' OR 'MAIN' OR 'MAN'
                                        OR 'SUPP' OR 'TAX',
                % Above PRD.CLASS codes must agree with those found  %
                % in INV.SIM found elsewhere on system 38.           %
  PRD.TRANS.TYPE AS '4C'        VERIFY PRD.TRANS.TYPE =
                                        'CAN' OR 'CON' OR 'DEMO' OR 'DINS'
                                        OR 'INST' OR 'INT' OR 'INV'
                                        OR 'LEAS' OR 'LOAN' OR 'LOST'
                                        OR 'ORD' OR 'SALE' OR 'SFR' OR 'TAX',
                % Above PRD.TRANS.TYPE codes must agree with those found  %
                % in INV.SIM found elsewhere on system 38.                %
  PRD.REVENUE  AS '9-N.NN',
  PRD.COST     SAME AS PRD.REVENUE,
  PRD.COM.RATE AS 'ZN.NZZ',
  PRD.COM.AMT  AS '7-N.NN',
  INV.NUM      AS '5ZN',
  CUS.NUM      AS '4ZN',
  CUS.DIST.NUM SAME AS DISTRICT.3N,
  NEW.SALESNUM.5N  SAME AS SALESNUM.5N,
  MODEL.NUM    AS '4C',
  SERIAL.NUM   AS '10C',
  TRANS.NUM   AS '11ZN',
  SALES.REVENUE SAME AS PRD.REVENUE,
  LEASE.REVENUE SAME AS PRD.REVENUE,
  SALES.COST    SAME AS PRD.REVENUE,
  LEASE.COST    SAME AS PRD.REVENUE,
  NON.REV.COST  SAME AS PRD.REVENUE,
  NON.COMM.REV  SAME AS PRD.REVENUE,
  PROD.PROFIT   AS '(7ZN.NN)'

% Year-to-date summary data fields %

FIELDS                                       % Five-character transaction code %
  M.BASE.REV.Q  AS '(7ZN.NN)',
  Y.BASE.REV.Q  SAME AS M.BASE.REV.Q,
  M.NEW.REV.Q   SAME AS M.BASE.REV.Q,
  Y.NEW.REV.Q   SAME AS Y.BASE.REV.Q,
  M.REV.BEF.ADJ SAME AS M.BASE.REV.Q,                                  % RVBFA %
  Y.REV.BEF.ADJ SAME AS Y.BASE.REV.Q,
  M.REV.ADJ     SAME AS M.BASE.REV.Q,                                  % RVADJ %
  Y.REV.ADJ     SAME AS Y.BASE.REV.Q,
  M.INCENT.BASE  SAME AS M.BASE.REV.Q,
  Y.INCENT.BASE  SAME AS Y.BASE.REV.Q,
  TOT.INCENT.ERND SAME AS M.BASE.REV.Q,
  M.INCENT.ERND.Y SAME AS Y.BASE.REV.Q,
  M.NEW.ACC.REV SAME AS M.BASE.REV.Q,                                  % NACCR %
  Y.NEW.ACC.REV SAME AS Y.BASE.REV.Q,
  M.PRD.COM.REV SAME AS M.BASE.REV.Q,
  Y.PRD.COM.REV SAME AS Y.BASE.REV.Q,
  M.NEW.REV.COM SAME AS M.BASE.REV.Q,
  Y.NEW.REV.COM SAME AS Y.BASE.REV.Q,
  M.NROQ.COM    SAME AS M.BASE.REV.Q,
  Y.NROQ.COM    SAME AS Y.BASE.REV.Q,
  TOT.NROQ.BONUS  SAME AS Y.BASE.REV.Q,
  M.NROQ.BONUS.Y  SAME AS Y.BASE.REV.Q,
  M.NAR.COM     SAME AS M.BASE.REV.Q,
  Y.NAR.COM     SAME AS Y.BASE.REV.Q,
  M.TAS.DEV.COM SAME AS M.BASE.REV.Q,
  Y.TAS.DEV.COM SAME AS Y.BASE.REV.Q,
  M.PRODUCT.COM SAME AS M.BASE.REV.Q,
  Y.PRODUCT.COM SAME AS Y.BASE.REV.Q,
  M.PROD.PROF.COM SAME AS M.BASE.REV.Q,
  Y.PROD.PROF.COM SAME AS Y.BASE.REV.Q,
  COM.EARNED    SAME AS M.BASE.REV.Q,
  PREV.COM.EARNED  SAME AS M.BASE.REV.Q,
  M.RESERVE.PMT SAME AS M.BASE.REV.Q,                                  % RSPMT %
  Y.RESERVE.PMT SAME AS Y.BASE.REV.Q,
  RESERVE       SAME AS M.BASE.REV.Q,
  COM.THIS.MO   SAME AS M.BASE.REV.Q,
  M.PROD.PROF   SAME AS M.BASE.REV.Q,
  Y.PROD.PROF   SAME AS Y.BASE.REV.Q,
  M.COM.ADJ     SAME AS M.BASE.REV.Q,                                  % CMADJ %
  Y.COM.ADJ     SAME AS Y.BASE.REV.Q,
  M.CHARGEBACKS SAME AS M.BASE.REV.Q,                                  % CHGBK %
  Y.CHARGEBACKS SAME AS Y.BASE.REV.Q,
  M.RECV.DRAW.USED SAME AS M.BASE.REV.Q,
  Y.RECV.DRAW.USED SAME AS Y.BASE.REV.Q,
  M.NONR.DRAW.USED SAME AS M.BASE.REV.Q,
  Y.NONR.DRAW.USED SAME AS Y.BASE.REV.Q,
  M.DRAW.RECVRY SAME AS M.BASE.REV.Q,                                  % DRRCV %
  Y.DRAW.RECVRY SAME AS Y.BASE.REV.Q,
  M.COM.PAYABLE SAME AS M.BASE.REV.Q,
  Y.COM.PAYABLE SAME AS Y.BASE.REV.Q,

  M.TYMCOM.9    SAME AS M.BASE.REV.Q,                                  % TYMC9 %
  Y.TYMCOM.9    SAME AS Y.BASE.REV.Q,
  M.TYMCOM.10   SAME AS M.BASE.REV.Q,                                  % TYMCX %
  Y.TYMCOM.10   SAME AS Y.BASE.REV.Q,
  M.TYMCOM.370  SAME AS M.BASE.REV.Q,                                  % TY370 %
  Y.TYMCOM.370  SAME AS Y.BASE.REV.Q,
  M.W.SYSTEMS   SAME AS M.BASE.REV.Q,                                  % WSYST %
  Y.W.SYSTEMS   SAME AS Y.BASE.REV.Q,
  M.LEASCO      SAME AS M.BASE.REV.Q,                                  % LESCO %
  Y.LEASCO      SAME AS M.BASE.REV.Q,
  M.TASC.DEVEL  SAME AS M.BASE.REV.Q,                                  % TADEV %
  Y.TASC.DEVEL  SAME AS Y.BASE.REV.Q,
  M.TASC.INSTL  SAME AS M.BASE.REV.Q,                                  % TAINS %
  Y.TASC.INSTL  SAME AS Y.BASE.REV.Q,
  M.TASC.PRDTN  SAME AS M.BASE.REV.Q,                                  % TAPRD %
  Y.TASC.PRDTN  SAME AS Y.BASE.REV.Q,
  M.INF.SYS.DIV SAME AS M.BASE.REV.Q,                                  % INFSD %
  Y.INF.SYS.DIV SAME AS Y.BASE.REV.Q,
  M.PRD.BEF.TAX SAME AS M.BASE.REV.Q,
  Y.PRD.BEF.TAX SAME AS Y.BASE.REV.Q

% Identification for area %

RELATION IAREA IS % 'Area Master' %
  KEY
    AREA.2N
  DATA
    GROUP.NAME, REGION.2N,
    SALESNUM.5N         % Number for area technical manager %


% Identification for district %

RELATION IDIST IS % 'District Master' %
  KEY
    DISTRICT.3N
  DATA
    GROUP.NAME, AREA.2N,
    SALESNUM.5N         % Number for district manager %


% Identification for commissioned personnel %

RELATION ISP IS % 'Salesman Master' %
  KEY
    SALESNUM.5N
  DATA
    AREA.2N, DISTRICT.3N, COM.PLAN, EMPLOYE.NUM


% Identification for employe %

RELATION IEMPL IS % 'Employe Master' %
  KEY
    EMPLOYE.NUM
  DATA
    E.LAST.NAME, E.FIRST.NAME, E.MIDDLE.NAME

% Commission plan for area technical manager %

RELATION CATM IS
  KEY
    SALESNUM.5N
  DATA
    PLAN.STATUS, % active or inactive %
    BEGIN.MO,
    BASE.REV.Q, NEW.REV.Q.BAS, NEW.REV.Q.INC,
    DRAW.PER.BEG, DRAW.PER.END,
    DRAW.TYPE, % recoverable or non-recoverable %
    DRAW.AMOUNT,
    INCENT.BASE.Q,
    PC.N.R.O.Q


% Commission plan for branch manager %

RELATION CBM IS
  KEY
    SALESNUM.5N
  DATA
    PLAN.STATUS, % active or inactive %
    BEGIN.MO,
    BASE.REV.Q, NEW.REV.Q.BAS, NEW.REV.Q.INC,
    DRAW.PER.BEG, DRAW.PER.END,
    DRAW.TYPE, % recoverable or non-recoverable %
    DRAW.AMOUNT,
    INCENT.BASE.Q,
    PC.N.R.O.Q,
    PC.N.ACC.REV, PC.TASC.DEVEL, PC.PROD.PROF



% Commission plan for district manager %

RELATION CDM IS
  KEY
    SALESNUM.5N
  DATA
    PLAN.STATUS, % active or inactive %
    BEGIN.MO,
    BASE.REV.Q, NEW.REV.Q.BAS, NEW.REV.Q.INC,
    DRAW.PER.BEG, DRAW.PER.END,
    DRAW.TYPE, % recoverable or non-recoverable %
    DRAW.AMOUNT,
    INCENT.BASE.Q,
    PC.N.R.O.Q,
    PC.N.ACC.REV, PC.TASC.DEVEL, PC.PROD.PROF


% Commission plan for application consultant %

RELATION CAC IS
  KEY
    SALESNUM.5N
  DATA
    PLAN.STATUS, % active or inactive %
    BEGIN.MO,
    BASE.REV.Q, NEW.REV.Q.BAS, NEW.REV.Q.INC,
    DRAW.PER.BEG, DRAW.PER.END,
    DRAW.TYPE, % recoverable or non-recoverable %
    DRAW.AMOUNT,
    INCENT.BASE.Q,
    PC.N.R.O.Q



% Commission plan for sales representative %

RELATION CSR IS
  KEY
    SALESNUM.5N
  DATA
    PLAN.STATUS, % active or inactive (house account) %
    BEGIN.MO,
    BASE.REV.Q, NEW.REV.Q.BAS, NEW.REV.Q.INC,
    DRAW.PER.BEG, DRAW.PER.END,
    DRAW.TYPE, % recoverable or non-recoverable %
    DRAW.AMOUNT,
    PC.NEW.REV,
    PC.N.R.O.Q,
    PC.N.ACC.REV, PC.TASC.DEVEL

% Monthly transactions for technical personnel %

RELATION MACATM IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N
  DATA
    M.COM.ADJ, M.RESERVE.PMT, M.DRAW.RECVRY, M.CHARGEBACKS


% Monthly transactions for district managers and sales representatives %

RELATION MBMDMSR IS
  KEY
    YR.PROCESSED, MO.PROCESSED, AREA.2N, DISTRICT.3N, COM.PLAN,
    SALESNUM.5N, TRANSACTION
  DATA
    AMOUNT


% Monthly product revenue transactions %

RELATION MPRDREV IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N
  DATA
    M.PRD.COM.REV, M.PRODUCT.COM, M.PRD.BEF.TAX


% Product profit transactions %

RELATION MPRDPRF IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N
  DATA
    M.PROD.PROF

% Salesman summary detail lines for Product Profit Report (PRODUCTS) %

RELATION DIST.PROFIT IS
  KEY
    AREA.2N, DISTRICT.3N, SALESNUM.5N
  DATA
    SALES.REVENUE, LEASE.REVENUE, SALES.COST, LEASE.COST,
    NON.REV.COST, NON.COMM.REV, PROD.PROFIT

% Product Detail from INVREG.2 %

RELATION PRD.INVREG IS
  KEY
        AREA.2N, DISTRICT.3N, SALESNUM.5N, MODEL.NUM, SERIAL.NUM, TRANS.NUM, PRD.SIGN,
    PRD.CLASS, CUS.NUM, INV.NUM
  DATA
    CUS.DIST.NUM, PRD.TRANS.TYPE, PRD.COST,
    PRD.REVENUE, PRD.COM.RATE, PRD.COM.AMT

% Salesman #'s encountered in INVREG.2 and NOT Defined in the Commission Plan %

RELATION UNDEF.SALESNUM IS
  KEY
    SALESNUM.5N

% Transaction Cross-Reference for Product Profit equipment %

RELATION PRD.TRANS.ADJ IS
  KEY
    YR.PROCESSED, MO.PROCESSED, DISTRICT.3N, SALESNUM.5N, MODEL.NUM,
    SERIAL.NUM, TRANS.NUM, PRD.SIGN

  DATA
    NEW.SALESNUM.5N, CUS.DIST.NUM, INV.NUM, PRD.TRANS.TYPE,
    PRD.COST, PRD.REVENUE, PRD.COM.RATE, PRD.COM.AMT

% Undefined Cross- reference suspense numbers %

RELATION TRANS.ADJ.SALESNUMS IS
  KEY
    YR.PROCESSED, MO.PROCESSED, DISTRICT.3N, SALESNUM.5N



% Year-to-date summary for area %

RELATION YAREA IS
  KEY
    YR.PROCESSED, MO.PROCESSED, AREA.2N
  DATA
    M.BASE.REV.Q, Y.BASE.REV.Q,
    M.NEW.REV.Q, Y.NEW.REV.Q,
    M.REV.BEF.ADJ, Y.REV.BEF.ADJ,
    M.REV.ADJ, Y.REV.ADJ,
    M.TYMCOM.9, Y.TYMCOM.9,
    M.TYMCOM.10, Y.TYMCOM.10,
    M.TYMCOM.370, Y.TYMCOM.370,
    M.W.SYSTEMS, Y.W.SYSTEMS,
    M.LEASCO, Y.LEASCO,
    M.TASC.DEVEL, Y.TASC.DEVEL,
    M.TASC.INSTL, Y.TASC.INSTL,
    M.TASC.PRDTN, Y.TASC.PRDTN,
    M.INF.SYS.DIV, Y.INF.SYS.DIV,
    M.PRD.BEF.TAX, Y.PRD.BEF.TAX


% Year-to-date summary for area technical manager %

RELATION YATM IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N, EMPLOYE.NUM
  DATA
    M.BASE.REV.Q, Y.BASE.REV.Q,
    M.NEW.REV.Q, Y.NEW.REV.Q,
    M.REV.BEF.ADJ, Y.REV.BEF.ADJ,
    M.REV.ADJ, Y.REV.ADJ,
    M.INCENT.BASE, Y.INCENT.BASE,
    TOT.INCENT.ERND, M.INCENT.ERND.Y,
    TOT.NROQ.BONUS, M.NROQ.BONUS.Y,
    COM.EARNED, PREV.COM.EARNED,
    M.RESERVE.PMT, Y.RESERVE.PMT,
    RESERVE,
    COM.THIS.MO,
    M.COM.ADJ, Y.COM.ADJ,
    DRAW.AMOUNT,
    M.RECV.DRAW.USED, Y.RECV.DRAW.USED,
    M.NONR.DRAW.USED, Y.NONR.DRAW.USED,
    M.DRAW.RECVRY, Y.DRAW.RECVRY,
    M.COM.PAYABLE, Y.COM.PAYABLE

% Year-to-date summary for district manager %

RELATION YDM IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N, EMPLOYE.NUM
  DATA
    M.BASE.REV.Q, Y.BASE.REV.Q,
    M.NEW.REV.Q, Y.NEW.REV.Q,
    M.REV.BEF.ADJ, Y.REV.BEF.ADJ,
    M.REV.ADJ, Y.REV.ADJ,
    M.NEW.ACC.REV, Y.NEW.ACC.REV,
    M.INCENT.BASE, Y.INCENT.BASE,
    TOT.INCENT.ERND, M.INCENT.ERND.Y,
    TOT.NROQ.BONUS, M.NROQ.BONUS.Y,
    COM.EARNED, PREV.COM.EARNED,
    M.NAR.COM    , Y.NAR.COM,
    M.RESERVE.PMT, Y.RESERVE.PMT,
    RESERVE,
    COM.THIS.MO,
    M.PROD.PROF, Y.PROD.PROF,
    M.TAS.DEV.COM, Y.TAS.DEV.COM,
    M.PRODUCT.COM, Y.PRODUCT.COM,
    M.PROD.PROF.COM, Y.PROD.PROF.COM,
    M.CHARGEBACKS, Y.CHARGEBACKS,
    M.COM.ADJ, Y.COM.ADJ,
    DRAW.AMOUNT,
    M.RECV.DRAW.USED, Y.RECV.DRAW.USED,
    M.NONR.DRAW.USED, Y.NONR.DRAW.USED,
    M.DRAW.RECVRY, Y.DRAW.RECVRY,
    M.COM.PAYABLE, Y.COM.PAYABLE,
    M.TYMCOM.9, Y.TYMCOM.9,
    M.TYMCOM.10, Y.TYMCOM.10,
    M.TYMCOM.370, Y.TYMCOM.370,
    M.W.SYSTEMS, Y.W.SYSTEMS,
    M.LEASCO, Y.LEASCO,
    M.TASC.DEVEL, Y.TASC.DEVEL,
    M.TASC.INSTL, Y.TASC.INSTL,
    M.TASC.PRDTN, Y.TASC.PRDTN,
    M.INF.SYS.DIV, Y.INF.SYS.DIV,
    M.PRD.BEF.TAX, Y.PRD.BEF.TAX


% Year-to-date summary for branch manager %

RELATION YBM IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N, EMPLOYE.NUM
  DATA
    M.BASE.REV.Q, Y.BASE.REV.Q,
    M.NEW.REV.Q, Y.NEW.REV.Q,
    M.REV.BEF.ADJ, Y.REV.BEF.ADJ,
    M.REV.ADJ, Y.REV.ADJ,
    M.NEW.ACC.REV, Y.NEW.ACC.REV,
    M.INCENT.BASE, Y.INCENT.BASE,
    TOT.INCENT.ERND, M.INCENT.ERND.Y,
    TOT.NROQ.BONUS, M.NROQ.BONUS.Y,
    COM.EARNED, PREV.COM.EARNED,
    M.NAR.COM    , Y.NAR.COM,
    M.RESERVE.PMT, Y.RESERVE.PMT,
    RESERVE,
    COM.THIS.MO,
    M.PROD.PROF, Y.PROD.PROF,
    M.TAS.DEV.COM, Y.TAS.DEV.COM,
    M.PRODUCT.COM, Y.PRODUCT.COM,
    M.PROD.PROF.COM, Y.PROD.PROF.COM,
    M.CHARGEBACKS, Y.CHARGEBACKS,
    M.COM.ADJ, Y.COM.ADJ,
    DRAW.AMOUNT,
    M.RECV.DRAW.USED, Y.RECV.DRAW.USED,
    M.NONR.DRAW.USED, Y.NONR.DRAW.USED,
    M.DRAW.RECVRY, Y.DRAW.RECVRY,
    M.COM.PAYABLE, Y.COM.PAYABLE,
    M.TYMCOM.9, Y.TYMCOM.9,
    M.TYMCOM.10, Y.TYMCOM.10,
    M.TYMCOM.370, Y.TYMCOM.370,
    M.W.SYSTEMS, Y.W.SYSTEMS,
    M.LEASCO, Y.LEASCO,
    M.TASC.DEVEL, Y.TASC.DEVEL,
    M.TASC.INSTL, Y.TASC.INSTL,
    M.TASC.PRDTN, Y.TASC.PRDTN,
    M.INF.SYS.DIV, Y.INF.SYS.DIV,
    M.PRD.BEF.TAX, Y.PRD.BEF.TAX

% Year-to-date summary for applications consultant %

RELATION YAC IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N, EMPLOYE.NUM
  DATA
    M.BASE.REV.Q, Y.BASE.REV.Q,
    M.NEW.REV.Q, Y.NEW.REV.Q,
    M.REV.BEF.ADJ, Y.REV.BEF.ADJ,
    M.REV.ADJ, Y.REV.ADJ,
    M.INCENT.BASE, Y.INCENT.BASE,
    TOT.INCENT.ERND, M.INCENT.ERND.Y,
    TOT.NROQ.BONUS, M.NROQ.BONUS.Y,
    COM.EARNED, PREV.COM.EARNED,
    M.RESERVE.PMT, Y.RESERVE.PMT,
    RESERVE,
    COM.THIS.MO,
    M.PRODUCT.COM, Y.PRODUCT.COM,
    M.CHARGEBACKS, Y.CHARGEBACKS,
    M.COM.ADJ, Y.COM.ADJ,
    DRAW.AMOUNT,
    M.RECV.DRAW.USED, Y.RECV.DRAW.USED,
    M.NONR.DRAW.USED, Y.NONR.DRAW.USED,
    M.DRAW.RECVRY, Y.DRAW.RECVRY,
    M.COM.PAYABLE, Y.COM.PAYABLE

% Year-to-date summary for sales representative %

RELATION YSR IS
  KEY
    YR.PROCESSED, MO.PROCESSED, SALESNUM.5N, EMPLOYE.NUM
  DATA
    M.BASE.REV.Q, Y.BASE.REV.Q,
    M.NEW.REV.Q, Y.NEW.REV.Q,
    M.REV.BEF.ADJ, Y.REV.BEF.ADJ,
    M.REV.ADJ, Y.REV.ADJ,
    M.NEW.ACC.REV, Y.NEW.ACC.REV,
    M.PRD.COM.REV, Y.PRD.COM.REV,
    M.NEW.REV.COM, Y.NEW.REV.COM,
    M.NROQ.COM, Y.NROQ.COM,
    M.TAS.DEV.COM, Y.TAS.DEV.COM,
    COM.EARNED, PREV.COM.EARNED,
    M.NAR.COM, Y.NAR.COM,
    M.PRODUCT.COM, Y.PRODUCT.COM,
    M.CHARGEBACKS, Y.CHARGEBACKS,
 COM.ADJ, Y.COM.ADJ,
    DRAW.AMOUNT,
    M.RECV.DRAW.USED, Y.RECV.DRAW.USED,
    M.NONR.DRAW.USED, Y.NONR.DRAW.USED,
    M.DRAW.RECVRY, Y.DRAW.RECVRY,
    M.COM.PAYABLE, Y.COM.PAYABLE,
    M.TYMCOM.9, Y.TYMCOM.9,
    M.TYMCOM.10, Y.TYMCOM.10,
    M.TYMCOM.370, Y.TYMCOM.370,
    M.W.SYSTEMS, Y.W.SYSTEMS,
    M.LEASCO, Y.LEASCO,
    M.TASC.DEVEL, Y.TASC.DEVEL,
    M.TASC.INSTL, Y.TASC.INSTL,
    M.TASC.PRDTN, Y.TASC.PRDTN,
    M.INF.SYS.DIV, Y.INF.SYS.DIV,
    M.PRD.BEF.TAX, Y.PRD.BEF.TAX


END % Of schema TYCOMAN %
 64dñ