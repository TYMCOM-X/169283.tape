SCHEMA ORDER.ENTRY.EXAMPLE

  BEGIN       %SCHEMA DEFINITIONS%

     %FIELD DECLARATIONS%

FIELDS   NEXT.RUN.DATE      AS  '6N',
         PROD.NO            AS  '10C',
         PROD.LINE.NO       AS  '2N',
         PROD.LIST.PRICE    AS  '4NVNN',
         PROD.STD.COST      AS  '4NVNN',
         PROD.DESC          AS  '40C',
         CUST.NO            AS  '10C',
         CUST.NAME          AS  '40C',
         CUST.ADDR          AS  '30C',
         CUST.CITY          AS  '30C',
         CUST.STATE         AS  '15C',
         CUST.ZIP           AS  '5N',
         SALES.ORD.NO       AS  '6N',
         LI.NO              AS  '3N',
         PROD.QTY           AS  '4N',
         ORD.DATE           AS  '6N',
         SLSMAN.NO          AS  '5N',
         BILLTO.ADDR        AS  '30C',
         SHIPTO.ADDR        AS  '30C',
         BILLTO.NAME        AS  '40C',
         SO.STATUS.CODE     AS  '3N',
         BILLTO.CITY        AS  '30C',
         BILLTO.STATE       AS  '15C',
         BILLTO.ZIP         AS  '5N',
         DEL.DATE           AS  '6N',
         SHIPTO.DATE        AS  '6N',
         SHIPTO.NAME        AS  '40C',
         SHIPTO.CITY        AS  '30C',
         SHIPTO.STATE       AS  '15C',
         SHIPTO.ZIP         AS  '5N',
         SLSMAN.NAME        AS  '40C',
         SO.OFC.CODE        AS  '30C',
         SO.TOTAL           AS  '6NVNN',
         ORG.DIST.CODE      AS  '6N',
         LEAD.TIME          AS  '20C',
         BACK.ORD.CODE      AS  '2N',
         BILL.DATE          AS  '6N',
         S.CUSTOMERS        AS  '40C',
         PARENT.REG         AS  '30C',
         RE.ORD.PT          AS  '6N',
         QTY.ON.HAND        AS  '6N',
         LOC.NAME           AS  '40C' 




%RELATION DECLARATION%

 RELATION PRODUCT IS 
 
 KEY      PROD.NO,
          PROD.LINE.NO

 DATA     PROD.LIST.PRICE,
          PROD.STD.COST,
          PROD.DESC,
          NEXT.RUN.DATE


 RELATION CUSTOMER IS 

 KEY      CUST.NO

 DATA     CUST.NAME,
          CUST.ADDR,
          CUST.CITY,
          CUST.STATE,
          CUST.ZIP


 RELATION SALES.ORDER IS 
 
 KEY      SALES.ORD.NO

 DATA     ORD.DATE,
          SLSMAN.NO,
          BILLTO.ADDR,
          BILLTO.NAME,
          BILLTO.CITY,
          BILLTO.STATE,
          BILLTO.ZIP,
          SHIPTO.ADDR,
          SHIPTO.NAME,
          SHIPTO.CITY,
          SHIPTO.STATE,
          SHIPTO.ZIP,
          CUST.NO,
          SO.TOTAL,
          DEL.DATE,
          BILL.DATE,
          SO.STATUS.CODE


 RELATION SO.LINE.ITEM IS
 KEY      SALES.ORD.NO,
          LI.NO

 DATA     PROD.NO,
          PROD.QTY


 RELATION INVENTORY IS

 KEY      PROD.NO

 DATA     LEAD.TIME,
          RE.ORD.PT,
          BACK.ORD.CODE,
          QTY.ON.HAND


 RELATION SALES.REP IS

 KEY      SLSMAN.NO

 DATA     SLSMAN.NAME,
          S.OFC.CODE,
          ORG.DIST.CODE,
          SO.TOTAL


 RELATION LOCATION IS

 KEY      S.OFC.CODE

 DATA     LOC.NAME


 RELATION ORGANIZATION IS 

 KEY      ORG.DIST.CODE

 DATA     PARENT.REG,
          SLSMAN.NAME


 RELATION S.CUSTOMERS IS

 KEY      SLSMAN.NO

 DATA     CUST.NO


END  %SCHEMA DECLARATIONS%
    