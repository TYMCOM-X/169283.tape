IDENTIFICATION DIVISION.
PROGRAM-ID. DIPROD.
AUTHOR. MUDGEY.
DATE-WRITTEN. 31-JAN-73.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    CHANNEL (1) IS TOP-OF-FORM.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT IN-FILE              ASSIGN TO DSK;
    RECORDING MODE IS ASCII.

    SELECT TEMPFIL              ASSIGN TO DSK,DSK,DSK;
    RECORDING MODE IS ASCII.


    SELECT K-IN                 ASSIGN TO DSK;
    RECORDING MODE IS ASCII.


    SELECT L-PRINT              ASSIGN TO DSK;
    RECORDING MODE IS ASCII.

DATA DIVISION.
FILE SECTION.


FD  K-IN
      LABEL RECORDS ARE STANDARD
      VALUE OF ID IS "INVREG2  ".

01  IN-REC.
        02 FILLER       PIC X(93).


FD  IN-FILE
    LABEL RECORDS ARE STANDARD
    VALUE OF IDENTIFICATION IS "KAYREGDAT".

 01 INPUT-RECORD.
        02 INVNO        PIC 9(6).
        02 FILLER       PIC X.
        02 CUST         PIC 9(4).
        02 FILLER       PIC X.
        02 DIST         PIC 99.
        02 FILLER       PIC X.
        02 SALNUM       PIC 9(4).
        02 FILLER       PIC X.
        02 NAME         PIC X(20).
        02 FILLER       PIC X.
        02 STAT         PIC X(4).
        02 FILLER       PIC X.
        02 PROD         PIC X(4).
        02 QTY-IN.
                03 A-FLD1       PIC S9(5).
                03 FILLER       PIC X.
                03 B-FLD1       PIC 9(3).
        02 AMT-IN.
                03 A-FLD2       PIC S9(10).
                03 FILLER       PIC X.
                03 B-FLD2       PIC 99.
        02 FILLER       PIC X.
        02 STATE        PIC 9(6).
        02 COST-IN.
                03 A-FLD3       PIC S9(10).
                03 FILLER       PIC X.
                03 B-FLD3       PIC 99.
        02 FILLER       PIC X(27).



SD  TEMPFIL.

01  SSRT-REC; DISPLAY-7.
        02 FILLER       PIC X(12).
        02 NOX          PIC 99.
        02 FILLER       PIC X(27).
        02 NO1          PIC X(4).
        02 FILLER       PIC X.
        02 NO2          PIC X(4).
        02 FILLER       PIC X(42).


FD  L-PRINT
    LABEL RECORDS ARE STANDARD
    VALUE OF IDENTIFICATION IS "REPORTDPD".

01  PRINT-OUT; DISPLAY-7.
        02 FILLER       PIC X(90).

01  OUT-LINE; DISPLAY-7.
        02 FILLER                       PIC X(90).

01  PRINT-LINE; DISPLAY-7.
        02 FILLER               PICTURE X(90).

01  NUM-LINE; DISPLAY-7.
        02 NUM-FIRST    PIC X(12).
        02 FILLER               PIC X(58).
        02 NUM-HEAD             PIC X(5).
        02 HEADER-PAGE          PIC Z9.


WORKING-STORAGE SECTION.
77  FLAG                        PIC 99.
77  LAS-DIST                    PIC 99.
77  SAVE-REG                    PIC 99.

77  C-FLD1      PIC S9(5); COMP.
77  C-FLD2      PIC S9(5); COMP.
77  C-FLD3      PIC S9(5); COMP.


77  TEMP-A      PIC S9(5); COMP.
77  ETEMP       PIC S9(5); COMP.

77  COST                PIC S9(10)V99; COMP.
77  QTY                 PIC S9(5)V999; COMP.
77  AMT                 PIC S9(10)V99; COMP.

77  LAS-PROD            PIC X(4); DISPLAY-7.

77  TEMP1                       PICTURE S9(10)V99; COMPUTATIONAL.
77  PAGE-COUNT                  PICTURE S9(2); COMPUTATIONAL.
77  LINE-COUNT                  PICTURE S99; COMPUTATIONAL.

77  SAVE-01                     PICTURE X(4).
77  SAVE-02                     PICTURE X(4).
77 DASHES                       PICTURE X(10);
        VALUE "----------".





01  HOLD-LINE;DISPLAY-7.
        02 HELP         PIC X(90).

01  PRINTOUT-LINE.

        02 TOT-LINE     PIC X(19).
        02 ITEM-OUT     PIC X(4).
        02 FILLER       PIC X(5).
        02 Q-OUT        PIC -(5).99.
        02 FILLER       PIC XX.
        02 A-OUT        PIC -(10).99.
        02 FILLER       PIC XXX.
        02 C-OUT        PIC -(10).99.
        02 FILLER       PIC XXX.
        02 P-OUT        PIC -(10).99.

01  OUTPUT-LINE; DISPLAY-7.
        02 STAT-OUT.
                03 FILLER       PIC X(5).
                03 ST1          PIC X(4).
        02 FILLER       PIC X(19).
        02 QTY-OUT      PIC -(5).99.
        02 FILLER       PIC X(2).
        02 AMT-OUT      PIC -(10).99.
        02 FILLER       PIC X(3).
        02 COST-OUT     PIC -(10).99.
        02 FILLER       PIC X(3).
        02 PROFIT       PIC -(10).99.

01  DASH-OUT.
        02 FILLER       PIC X(27).
        02 DASH-1       PIC X(9).
        02 FILLER       PIC X(7).
        02 DASH-2       PIC X(8).
        02 FILLER       PIC X(8).
        02 DASH-3       PIC X(8).
        02 FILLER       PIC X(8).
        02 DASH-4       PIC X(8).



01  LEVEL-REG-ACS; COMPUTATIONAL.
        02 ACCUMULATOR-1        PIC S9(6)V999.
        02 ACCUMULATOR-2        PIC S9(10)V99.
        02 ACCUMULATOR-3        PIC S9(10)V99.
        02 ACCUMULATOR-4        PIC S9(10)V99.

01  LEVEL-0-ACS; COMPUTATIONAL.
    02 ACCUMULATOR-1            PIC S9(6)V999.
    02 ACCUMULATOR-2            PIC S9(10)V99.
    02 ACCUMULATOR-3            PIC S9(10)V99.
    02 ACCUMULATOR-4            PIC S9(10)V99.

01  LEVEL-1-ACS; COMPUTATIONAL.
    02 ACCUMULATOR-1             PICTURE S9(6)V9(3).
    02 ACCUMULATOR-2             PICTURE S9(10)V9(2).
    02 ACCUMULATOR-3             PICTURE S9(10)V99.
    02 ACCUMULATOR-4             PICTURE S9(10)V99.

01  LEVEL-F-ACS; COMPUTATIONAL.
    02 ACCUMULATOR-1             PICTURE S9(6)V9(3).
    02 ACCUMULATOR-2             PICTURE S9(10)V9(2).
    02 ACCUMULATOR-3             PICTURE S9(10)V99.
    02 ACCUMULATOR-4             PICTURE S9(10)V99.

01  HEADER.
    02 FILL PIC X(29); VALUE "   TRANSACTION               ".
    02 FILL PIC X(38); VALUE "  QTY           AMT            COST".
    02 FILL PIC X(19); VALUE "       PROFIT".

01  HEAD-OUT-1.
        02 FILLER               PIC X(29).
        02 FILLER               PIC X(26);
             VALUE  "PRODUCT REPORT BY DISTRICT".


PROCEDURE DIVISION.

ONLY SECTION.
START.
        DISPLAY " ". DISPLAY " ".
        DISPLAY "SORTING INVREG..." WITH NO ADVANCING.

   SORT TEMPFIL ON ASCENDING KEY NOX,NO2,NO1 USING K-IN GIVING IN-FILE.

RPT.
        DISPLAY " ". DISPLAY " ".
        DISPLAY "GENERATING REPORT.DPD ..." WITH NO ADVANCING.

    OPEN INPUT IN-FILE.
    OPEN OUTPUT L-PRINT.
    MOVE SPACES TO OUTPUT-LINE.
    MOVE SPACES TO INPUT-RECORD.
    MOVE ZERO TO PAGE-COUNT.
    PERFORM PL-HDR THRU PL-EXIT.
    MOVE DASHES TO DASH-1,DASH-2,DASH-3,DASH-4.
        MOVE "TOTAL FOR PRODUCT: " TO TOT-LINE.

READ-IN.
        MOVE DIST TO LAS-DIST.
        MOVE PROD TO LAS-PROD.
    READ IN-FILE; AT END GO TO LEVEL-0-BREAK.

        NOTE ***THIS IS THE SUPER-SECTION.

        NOTE **ANOTHER SUPER -MESS.






  COMPUTE C-FLD1 = B-FLD1 * 10.
  COMPUTE C-FLD2 = B-FLD2 * 10.
  COMPUTE C-FLD3 = B-FLD3 * 10.
        COMPUTE ETEMP = B-FLD1.
        COMPUTE ETEMP = ETEMP * 10.
        IF A-FLD1 IS NOT LESS THAN ZERO GO TO POSIT.

                COMPUTE ETEMP = ETEMP * 2.
                COMPUTE ETEMP = C-FLD1 - ETEMP.
POSIT.

                COMPUTE QTY = A-FLD1 * 1000 +  ETEMP.
                COMPUTE QTY = QTY / 1000.

                COMPUTE ETEMP = B-FLD2.
        COMPUTE ETEMP = ETEMP * 10.

        IF A-FLD2 = 0 AND STAT = "CRED" GO TO POS2.
        IF A-FLD2 IS NOT LESS THAN ZERO GO TO POSIT2.
POS2.

                COMPUTE ETEMP = ETEMP * 2.
                COMPUTE ETEMP = C-FLD2 - ETEMP.
POSIT2.
                COMPUTE AMT = A-FLD2 * 1000  + ETEMP.
                COMPUTE AMT = AMT / 1000.

                COMPUTE ETEMP = B-FLD3.

        COMPUTE ETEMP = ETEMP * 10.
        IF A-FLD3 = 0 AND STAT = "CRED" GO TO POS3.
        IF A-FLD3 IS NOT LESS THAN ZERO GO TO POSIT3.
POS3.

                COMPUTE ETEMP = ETEMP * 2.
                COMPUTE ETEMP = C-FLD3 - ETEMP.
POSIT3.
                COMPUTE COST = A-FLD3 * 1000  + ETEMP.
                COMPUTE COST = COST / 1000.

INIT-SW. GO TO RESET-INIT.
        NOTE AFTER THE FIRST READ, INIT-SW WILL GO TO COMPARE.
RESET-INIT. ALTER INIT-SW TO PROCEED TO COMPARE.
    GO TO RESET-LEVEL-F.

COMPARE.

    IF SAVE-REG IS NOT EQUAL TO DIST
        ALTER LEVEL-REG-SW TO PROCEED TO RESET-LEVEL-REG;
        GO TO LEVEL-0-BREAK.

    IF SAVE-01 IS NOT EQUAL TO PROD
        ALTER LEVEL-1-SW TO PROCEED TO RESET-LEVEL-1;
        GO TO LEVEL-0-BREAK.

    IF SAVE-02 IS NOT EQUAL TO STAT
        ALTER LEVEL-0-SW TO PROCEED TO RESET-LEVEL-0;
        GO TO LEVEL-0-BREAK.


PRINT-DETAIL.
    ADD QTY TO ACCUMULATOR-1 OF LEVEL-0-ACS.
    ADD AMT TO ACCUMULATOR-2 OF LEVEL-0-ACS.
    ADD COST TO ACCUMULATOR-3 OF LEVEL-0-ACS.
    SUBTRACT COST FROM AMT GIVING TEMP1.
    ADD TEMP1 TO ACCUMULATOR-4 OF LEVEL-0-ACS.


    GO TO READ-IN.



LEVEL-0-BREAK.


    MOVE ACCUMULATOR-1 OF LEVEL-0-ACS TO QTY-OUT.
    MOVE ACCUMULATOR-2 OF LEVEL-0-ACS TO AMT-OUT.
    MOVE ACCUMULATOR-3 OF LEVEL-0-ACS TO COST-OUT.
    MOVE ACCUMULATOR-4 OF LEVEL-0-ACS TO PROFIT.





    MOVE OUTPUT-LINE TO PRINT-LINE.
    PERFORM PRINT-1 THRU P1-END.
    MOVE SPACES TO OUTPUT-LINE.

    ADD CORRESPONDING LEVEL-0-ACS TO LEVEL-1-ACS.

LEVEL-0-SW. GO TO. NOTE GO TO RESET-LEVEL-0 OR LEVEL-1-BREAK.

LEVEL-1-BREAK.

    MOVE ACCUMULATOR-1 OF LEVEL-1-ACS TO Q-OUT.
    MOVE ACCUMULATOR-2 OF LEVEL-1-ACS TO A-OUT.
    MOVE ACCUMULATOR-3 OF LEVEL-1-ACS TO C-OUT.
    MOVE ACCUMULATOR-4 OF LEVEL-1-ACS TO P-OUT.
        MOVE DASH-OUT TO PRINT-LINE.
        PERFORM PRINT-1 THRU P1-END.
        MOVE "TOTAL FOR PRODUCT:" TO TOT-LINE.
    MOVE LAS-PROD TO ITEM-OUT.
        MOVE PRINTOUT-LINE TO PRINT-LINE.
    PERFORM PRINT-1 THRU P1-END 2 TIMES.
    MOVE SPACES TO PRINT-LINE.
    PERFORM PRINT-1 THRU P1-END 5 TIMES.
        MOVE SPACES TO OUTPUT-LINE.

    ADD CORRESPONDING LEVEL-1-ACS TO LEVEL-REG-ACS.

LEVEL-1-SW. GO TO. NOTE GO TO RESET-LEVEL-1 OR LEVEL-F-BREAK.

LEVEL-REG-BREAK.
        MOVE ACCUMULATOR-1 OF LEVEL-REG-ACS TO Q-OUT.
        MOVE ACCUMULATOR-2 OF LEVEL-REG-ACS TO A-OUT.
        MOVE ACCUMULATOR-3 OF LEVEL-REG-ACS TO C-OUT.
        MOVE ACCUMULATOR-4 OF LEVEL-REG-ACS TO P-OUT.


        PERFORM PRINT-1 THRU P1-END 5 TIMES.
        MOVE "TOTAL FOR DISTRICT " TO TOT-LINE.
        MOVE LAS-DIST TO ITEM-OUT.
        MOVE PRINTOUT-LINE TO PRINT-LINE.
        PERFORM PRINT-1 THRU P1-END.
        MOVE SPACES TO PRINT-LINE.
        MOVE -1 TO LINE-COUNT.
        WRITE PRINT-LINE BEFORE TOP-OF-FORM.
        MOVE 1 TO FLAG.
        PERFORM PRINT-1 THRU P1-END.

  ADD CORR LEVEL-REG-ACS TO LEVEL-F-ACS.

LEVEL-REG-SW.  GO TO.  NOTE  GOTO TO RESET-REG OR LEVEL-F-BRK.

LEVEL-F-BREAK.
    MOVE SPACES TO PRINT-LINE.
    PERFORM PRINT-1 THRU P1-END.

        MOVE ACCUMULATOR-1 OF LEVEL-F-ACS TO Q-OUT.
        MOVE ACCUMULATOR-2 OF LEVEL-F-ACS TO A-OUT.
        MOVE ACCUMULATOR-3 OF LEVEL-F-ACS TO C-OUT.
        MOVE ACCUMULATOR-4 OF LEVEL-F-ACS TO P-OUT.
        MOVE "GRAND TOTALS" TO TOT-LINE.
        MOVE " " TO ITEM-OUT.
        MOVE PRINTOUT-LINE TO PRINT-LINE.
        WRITE PRINT-LINE BEFORE TOP-OF-FORM.
        GO TO FINISH.



RESET-LEVEL-F.
        MOVE LOW-VALUES TO LEVEL-F-ACS.


RESET-LEVEL-REG.
        MOVE LOW-VALUES TO LEVEL-REG-ACS.
        MOVE DIST TO SAVE-REG.
        ALTER LEVEL-REG-SW TO PROCEED TO LEVEL-F-BREAK.

RESET-LEVEL-1.
        MOVE LOW-VALUES TO LEVEL-1-ACS.
    MOVE PROD TO SAVE-01.
        MOVE PROD TO ST1.
    ALTER LEVEL-1-SW TO PROCEED TO LEVEL-REG-BREAK.

RESET-LEVEL-0.
        MOVE LOW-VALUES TO LEVEL-0-ACS.
    MOVE STAT TO SAVE-02.
    MOVE STAT TO ST1.
    ALTER LEVEL-0-SW TO PROCEED TO LEVEL-1-BREAK.
    GO TO PRINT-DETAIL.



PRINT-1. SUBTRACT 1 FROM LINE-COUNT.
        IF LINE-COUNT IS NEGATIVE PERFORM PAGER THRU P-EXIT.
RET1.
        WRITE PRINT-LINE.
        MOVE SPACES TO PRINT-LINE.
P1-END.  EXIT.


PL-HDR. MOVE SPACES TO PRINT-LINE.

PRINT-CH-1. WRITE PRINT-LINE BEFORE TOP-OF-FORM.
    ADD 1 TO PAGE-COUNT; MOVE PAGE-COUNT TO HEADER-PAGE.
    MOVE "+PIT REPORT+" TO NUM-FIRST.
    MOVE "PAGE" TO NUM-HEAD.
    WRITE NUM-LINE BEFORE ADVANCING 4 LINES.
        IF PAGE-COUNT > 1 GO TO SKIP-HEAD.
        MOVE HEAD-OUT-1 TO PRINT-LINE.
        WRITE PRINT-LINE BEFORE ADVANCING 5 LINES.
        MOVE SPACES TO PRINT-LINE.
SKIP-HEAD.
    MOVE HEADER TO PRINT-LINE.
    WRITE PRINT-LINE BEFORE ADVANCING 4 LINES.
        MOVE SPACES TO PRINT-LINE.
    MOVE 44 TO LINE-COUNT.

PL-EXIT.  EXIT.


PAGER.
        MOVE PRINT-LINE TO HELP.

        MOVE SPACES TO PRINT-OUT.
        IF FLAG IS NOT EQUAL TO 1 WRITE
                PRINT-LINE BEFORE ADVANCING 3 LINES.
        MOVE 0 TO FLAG.
        ADD 1 TO PAGE-COUNT; MOVE PAGE-COUNT TO HEADER-PAGE.
        MOVE "+PIT REPORT+" TO NUM-FIRST.
        MOVE "PAGE" TO NUM-HEAD.
        MOVE NUM-LINE TO PRINT-OUT.
        WRITE PRINT-OUT BEFORE 3 LINES.
        MOVE SPACES TO PRINT-OUT.
        MOVE HEADER TO PRINT-OUT.
        WRITE PRINT-OUT BEFORE ADVANCING 4 LINES.
        MOVE SPACES TO OUT-LINE.
        MOVE SPACES TO PRINT-OUT.
        MOVE 49 TO LINE-COUNT.
        MOVE HELP TO PRINT-LINE.
P-EXIT.  EXIT.

FINISH.
    CLOSE IN-FILE.
    CLOSE L-PRINT.
    DISPLAY " ".
    DISPLAY "FINISHED.".
        ENTER MACRO LINKER.
    STOP RUN.
   