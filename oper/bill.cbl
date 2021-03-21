       IDENTIFICATION DIVISION.
       PROGRAM-ID.  'SAMPLE'.
       REMARKS.  THIS IS AN ISAM TEST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT EMPFILE ASSIGN TO DSK, DSK
                RECORDING MODE IS ASCII
               ACCESS MODE IS INDEXED
               SYMBOLIC KEY IS WH-EMP
               RECORD KEY IS I-EMP.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPFILE VALUE OF ID IS 'EMPMSTIDX'
          USER-NUMBER IS 34,67075
               BLOCK CONTAINS 3 RECORDS.
       01  I-EMP-REC USAGE IS DISPLAY-7.
               03  I-EMP   PIC 9(6) .
               03  I-NAME   PIC X(5).
               03  I-ADDRESS   PIC X(5).
               03  I-TELE   PIC 9(7).
       WORKING-STORAGE SECTION.
       77  WH-EMP PIC 9(6) VALUE ZERO  USAGE IS DISPLAY-7.
       77  WH-REC PIC X(72) VALUE SPACE.
       01  OUT-REC PIC X(72).
       01  IN-REC USAGE IS DISPLAY-7.
               03  IN-1-7.
                   04  IN-1-6.
                    05  IN-1-5.
                     06  IN-1-4.
                      07  IN-1-3.
                       08  IN-1-2.
                        09  IN-1 PIC X.
                        09  FILLER  PIC X.
                       08  FILLER PIC X.
                      07  FILLER PIC X.
                     06  FILLER PIC X.
                    05  FILLER PIC X.
                   04  FILLER PIC X.
               03  FILLER PIC X(65).
       01  WH1.
               03  FILLER PIC X(10) VALUE SPACE.
               03  FILLER PIC X(14) VALUE 'SAMPLE PROGRAM'.
               03  FILLER PIC X(48) VALUE SPACE.
       01  WH2.
               03  FILLER PIC X(10) VALUE SPACE.
               03  FILLER PIC X(13) VALUE 'EMPLOYEE LIST'.
               03  FILLER PIC X(49) VALUE SPACE.
       01  WH3.
               03  FILLER PIC X(28) VALUE
                'EMP.NO.  NAME  ADDR.    TELE'.
               03  FILLER PIC X(44) VALUE SPACE.
       01  WH4.
               03  FILLER PIC X(31) VALUE
                'XXXXXX   XXXXX XXXXX    XXXXXXX'.
               03  FILLER PIC X(41) VALUE SPACE.
       01  WH5.
               03  WH5-EMP  PIC 9(6) VALUE ZERO.
               03  FILLER PIC XXX VALUE SPACE.
               03  WH5-NAME PIC X(5) VALUE SPACE.
               03  FILLER PIC X VALUE SPACE.
               03  WH5-ADDRESS PIC X(5) VALUE SPACE.
               03  FILLER PIC X(4) VALUE SPACE.
               03  WH5-TELE PIC 9(7) VALUE ZERO.
               03  FILLER PIC X(41) VALUE SPACE.
       PROCEDURE DIVISION.
       DECLARATIVES.
       ERR-EMPFILE SECTION.
       USE AFTER STANDARD ERROR PROCEDURE ON EMPFILE OPEN.
       1-START.
               DISPLAY "FILE BUSY".
       END DECLARATIVES.
       1-OPEN.
               DISPLAY "OPENING FILE NOW".
               OPEN I-O EMPFILE.
       2-BEGIN-MSG.
               MOVE 'SAMPLE PROGRAM HAS BEGUN' TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               
       3-ENTER-COMMAND.
               MOVE 'ENTER COMMAND+' TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               PERFORM 11-READ-IN THRU 11-X.
               IF IN-1 = '+' MOVE '*COMMANDS ARE AS FOLLOWS'
                 TO OUT-REC
               PERFORM 10-WRITE-OUT THRU 10-X
               MOVE '   END - END OF COMMAND' TO OUT-REC
               PERFORM 10-WRITE-OUT THRU 10-X
               MOVE '   ADD - ADDS AN EMPLOYEE ' TO OUT-REC
               PERFORM 10-WRITE-OUT THRU 10-X
               MOVE '   DEL - DELETES AN EMPLOYEE'
                 TO OUT-REC
               PERFORM 10-WRITE-OUT THRU 10-X
               MOVE '   STOP - ENDS SESSION' TO OUT-REC
               PERFORM 10-WRITE-OUT THRU 10-X
               GO TO 3-ENTER-COMMAND.
               IF IN-1-3 = 'ADD' GO TO 4-ADD.
               IF IN-1-3 = 'DEL' GO TO 5-DEL.
               IF IN-1-3 = 'RPT' GO TO 6-RPT.
               IF IN-1-3 = 'END' GO TO 3-ENTER-COMMAND.
               MOVE '*INVALID COMMAND-TRY AGAIN*'
                 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               GO TO 3-ENTER-COMMAND.
       4-ADD.
               MOVE WH3 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               MOVE WH4 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               PERFORM 11-READ-IN THRU 11-X.
               IF IN-1-3 = 'END' GO TO 3-ENTER-COMMAND.
               MOVE IN-1-6 TO WH-EMP.
               MOVE IN-REC TO WH5.
               READ EMPFILE INVALID KEY GO TO 4-1.
       4-0.
               MOVE '*EMPLOYEE ALREADY EXISTS-REPLACE?'
                 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               PERFORM 11-READ-IN THRU 11-X.
               IF IN-1-3 = 'END' GO TO 3-ENTER-COMMAND.
               IF IN-1 = 'Y' GO TO 4-3.
               IF IN-1 = 'N' GO TO 4-9.
               GO TO 4-0.
       4-1.
               MOVE WH5-EMP TO I-EMP  WH-EMP.
                               MOVE WH5-NAME TO I-NAME.
               MOVE WH5-ADDRESS TO I-ADDRESS.
               MOVE WH5-TELE TO I-TELE.
       4-2.
               WRITE I-EMP-REC, INVALID KEY GO TO 4-1.
               GO TO 4-9.
       4-3.
               PERFORM 4-1.
               REWRITE I-EMP-REC, INVALID KEY GO TO 4-3.
               GO TO 4-9.
       4-9.
               MOVE 'ADD ANOTHER EMPLOYEE? Y OR N'
                 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               PERFORM 11-READ-IN THRU 11-X.
               IF IN-1-3 = 'END' GO TO 3-ENTER-COMMAND.
               IF IN-1 = 'Y' GO TO 4-ADD.
               IF IN-1 = 'N' GO TO 3-ENTER-COMMAND.
               GO TO 4-9.
       5-DEL.
               MOVE 'ENTER EMP.NO.-' TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               PERFORM 11-READ-IN THRU 11-X.
               IF IN-1-3 = 'END' GO TO 3-ENTER-COMMAND.
               IF IN-1-6 NOT NUMERIC GO TO 5-DEL.
               MOVE IN-1-6 TO WH-EMP.
               READ EMPFILE INVALID KEY GO TO 5-5.
               DELETE I-EMP-REC INVALID KEY GO TO 5-5.
               GO TO 5-9.
       5-5.
               MOVE '*EMP NOT ON FILE - NOT DELETED'
                 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               GO TO 5-9.
       5-9.
               MOVE 'ANOTHER DELETE? Y OR N' TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               PERFORM 11-READ-IN THRU 11-X.
               IF IN-1-3 = 'END' GO TO 3-ENTER-COMMAND.
               IF IN-1 = 'N' GO TO 3-ENTER-COMMAND.
               IF IN-1 = 'Y' GO TO 5-DEL.
               GO TO 5-9.
       6-RPT.
               MOVE WH1 TO OUT-REC.
               DISPLAY ' '.  DISPLAY ' '.  DISPLAY ' '.
               DISPLAY OUT-REC.
               MOVE WH2 TO OUT-REC.
               DISPLAY OUT-REC.
               DISPLAY ' '.  DISPLAY ' '.
               MOVE WH3 TO OUT-REC.
               DISPLAY OUT-REC.
               DISPLAY ' '.  DISPLAY ' '.
               CLOSE EMPFILE.
               OPEN I-O EMPFILE. MOVE LOW-VALUES TO WH-EMP.
       6-1.
               READ EMPFILE INVALID KEY GO TO 6-9.
               MOVE SPACE TO WH5.
               MOVE I-EMP TO WH5-EMP.
               MOVE I-ADDRESS TO WH5-ADDRESS.
               MOVE I-NAME TO WH5-NAME.
                MOVE I-TELE TO WH5-TELE.
               MOVE WH5 TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               GO TO 6-1.
       6-9.
               CLOSE EMPFILE.
               OPEN I-O EMPFILE.
               GO TO 3-ENTER-COMMAND.
       10-WRITE-OUT.
               DISPLAY OUT-REC.
               MOVE SPACE TO OUT-REC.
       10-X.    EXIT.
       11-READ-IN.
               ACCEPT IN-REC.
               IF IN-1-4 = 'STOP' GO TO 99-END.
       11-X.    EXIT.
       99-END.
               CLOSE EMPFILE.
               MOVE 'SESSION OVER' TO OUT-REC.
               PERFORM 10-WRITE-OUT THRU 10-X.
               STOP RUN.
  