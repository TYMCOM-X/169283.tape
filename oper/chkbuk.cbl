IDENTIFICATION DIVISION.
PROGRAM-ID. 'CHECKBOOK'.
        AUTHOR. B ELKIN.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
                SELECT CHECKS ASSIGN TO DSK
                RECORDING MODE IS ASCII.
                SELECT DEPOSITS ASSIGN TO DSK
                RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD CHECKS VALUE OF IDENTIFICATION IS 'CHECKSDAT'.
                01 CHECK-REC.
                        02 CHECK-NUMB PIC 9(3).
                        02 CHECK-MONTH  PIC 9(2).
                        02 CHECK-DAY    PIC 9(2).
                        02 CHECK-YEAR   PIC 9(2).
                        02 PAYEE        PIC X(35).
                        02 CHECK-AMOUNT PIC 9(4).9(2).
FD DEPOSITS VALUE OF IDENTIFICATION IS 'DEPOSIDAT'.
        01 DEPOSIT-REC.
                02 DEPOSIT-MONTH        PIC 9(2).
                02 DEPOSIT-DAY  PIC 9(2).
                02 DEPOSIT-YEAR         PIC 9(2).
                02 DEPOSIT-AMOUNT       PIC 9(4).9(2).
WORKING-STORAGE SECTION.
        77  NUMB       PIC 9(3).
        77  MONTH        PIC 9(2).
        77  DAY  PIC 9(2).
        77  PAY-TO       PIC X(35).
        77 AMOUNT        PIC 9(4)V9(2).
        77  TEMP PIC X(3).
        77  TEMP1        PIC 9(2).
        77 TEMP2 PIC 9(2).
PROCEDURE DIVISION.
ANY-CHECKS.
        DISPLAY 'DO YOU HAVE ANY CHECKS TO ENTER : ' WITH NO ADVANCING.
        ACCEPT TEMP.
        IF TEMP EQUAL 'YES' GO TO ENTER-CHECKS.
ANY-DEPOSITS.
        DISPLAY 'DO YOU HAVE ANY DEPOSITS TO ENTER : ' WITH NO ADVANCING.
        ACCEPT TEMP.
        IF TEMP EQUAL 'NO ' GO TO END-RUN.
ENTER-DEPOSITS.
        DISPLAY 'HOW MANY DEPOSITS : ' WITH NO ADVANCING.
        ACCEPT TEMP1.
        OPEN OUTPUT DEPOSITS.
        MOVE 1 TO TEMP2.
        DISPLAY 'YOU WILL BE ASKED FOR INFO, 1 DEPOSIT AT A TIME.'.
INPUT-DEPOSITS.
        DISPLAY 'MONTH : ' WITH NO ADVANCING.
        ACCEPT MONTH.
        DISPLAY ' DAY : ' WITH NO ADVANCING.
        ACCEPT DAY.
        DISPLAY ' AMOUNT : ' WITH NO ADVANCING.
        ACCEPT AMOUNT.
        MOVE MONTH TO DEPOSIT-MONTH.
        MOVE DAY TO DEPOSIT-DAY.
        MOVE AMOUNT TO DEPOSIT-AMOUNT.
        MOVE 73 TO DEPOSIT-YEAR.
        ADD 1 TO TEMP2.
        WRITE DEPOSIT-REC.
        IF TEMP2 GREATER THAN TEMP1 GO TO END-RUN.
        GO TO INPUT-DEPOSITS.
ENTER-CHECKS.
        DISPLAY ' TEST POINT'.
END-RUN.
        STOP RUN.
