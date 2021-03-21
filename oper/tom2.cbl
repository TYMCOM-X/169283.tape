IDENTIFICATION DIVISION.
PROGRAM-ID. TOM2.
AUTHOR. TOM DICKER.
INSTALLATION. NWREGION.
DATE-WRITTEN. MAY 1973.
REMARKS. COBOL HOMEWORK ASSIGNMENT NUMBER TWO.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. PDP-10.
OBJECT-COMPUTER. PDP-10.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT OLDMAST ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT NEWMAST ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT SRTFIL01 ASSIGN TO DSK,DSK,DSK
        RECORDING MODE IS ASCII.
        SELECT ACTFILE ASSIGN TO DSK
        RECORDING MODE IS ASCII.
        SELECT SRTMAST ASSIGN TO DSK,DSK,DSK
        RECORDING MODE IS ASCII.
        SELECT REPFIL ASSIGN TO DSK
        RECORDING MODE IS ASCII.
DATA DIVISION.
FILE SECTION.
FD OLDMAST
        VALUE OF IDENTIFICATION IS "OLDMAST  ".
01      OLD-MAST-REC.
        05  OLD-SALESMAN        PIC X(8).
        05  OLD-NUMB    PIC 9(4).
        05  FILLER      PIC X.
        05  OLD-SALES-AMT       PIC 9(6).
FD NEWMAST
        VALUE OF IDENTIFICATION IS "NEWMAST  ".
01      NEW-MAST-REC.
                05  NEW-SALESMAN        PIC X(8).
                05  NEW-NUMB    PIC 9(4).
                05  FILLER      PIC X.
                05  NEW-SALES-AMT       PIC 9(6).
SD SRTFIL01.
01      SORT-REC-ONE.
                05  SRT-01-NUMB PIC 9(4).      
                05  FILLER      PIC X(2).
                05  SRT-01-S-AMT        PIC 9(5).
FD ACTFILE   
        VALUE OF IDENTIFICATION IS "ACTFILE  ".
01      ACT-REC.
                05  ACT-NUMB    PIC 9(4).
                05  FILLER      PIC X(2).
                05  ACT-S-AMT   PIC 9(5).
SD SRTMAST.
01      SRT-MAST-REC.
                05  S-M-SALESMAN        PIC X(8).
                05  S-M-NUMB.                 
                        10  S-M-DIST-NO  PIC X(2).
                        10  S-M-MAN-NO   PIC X(2).
                05  FILLER      PIC X.
                05  S-M-SALES-AMT       PIC 9(6).
FD REPFIL   
        VALUE OF IDENTIFICATION IS "REPFIL   ".
01      REPT-PRINT-LINE PIC X(28).    
                        
WORKING-STORAGE SECTION.
01      REPT-REC-HEAD   PIC X(27)
                        VALUE 'DIST SALESMAN SALES TO DATE'.
01      REPT-REC-LINE.
                05  REP-DIST-NO PIC X(2).
                05  FILLER      PIC X(3)  VALUE SPACES.   
                05  REP-SALESMAN        PIC X(8).
                05  FILLER      PIC X(8)  VALUE SPACES.
                05  REPT-SALES-AMT      PIC 9(6).
01      REPT-REC-SUB-TOT.
                05  FILLER      PIC X(2)  VALUE '**'.
                05  FILLER      PIC X(18)  VALUE SPACES.
                05  REPT-DIST-S-AMT-TOT PIC 9(7).
01      REPT-REC-GRAND-TOT.
                05  FILLER      PIC X(10)  VALUE '**** TOTAL'.
                05  FILLER      PIC X(10)  VALUE SPACES.
                05  REPT-S-AMT-GRAND-TOT        PIC 9(7).
                                       
77      WS-DIST-S-AMT-TOT       PIC 9(7).  
77      WS-S-AMT-GRAND-TOT      PIC 9(7).    
77      WS-OLDMASTER-FLAG       PIC X.    
        88  OLDMASTER-EOF       VALUE 'E'.
                        
PROCEDURE DIVISION.
USER-FIRST-PARAGRAPH.
I-INITIALIZATION.
        OPEN INPUT  OLDMAST.
        OPEN OUTPUT NEWMAST.  
        MOVE ZEROS TO WS-DIST-S-AMT-TOT.
        MOVE ZEROS TO WS-S-AMT-GRAND-TOT.
        MOVE SPACE TO WS-OLDMASTER-FLAG.
                        
II-SORT-ACT-FILE.
        SORT SRTFIL01 ON ASCENDING KEY SRT-01-NUMB
                USING ACTFILE
                OUTPUT PROCEDURE IS 
                III-READ-UPDATE-NEWMASTER THRU IV-CLOSE-INPUT-FILES.
        GO TO V-SORT-NEWMASTER.
                
III-READ-UPDATE-NEWMASTER.
        RETURN SRTFIL01 
                AT END PERFORM READ-WRITE-OLDMASTER
                     UNTIL OLDMASTER-EOF
                     GO TO IV-CLOSE-INPUT-FILES.
III-B-READ-OLDMASTER.
        PERFORM READ-OLDMASTER.                            
        IF OLDMASTER-EOF, GO TO IV-CLOSE-INPUT-FILES.
        IF OLD-NUMB EQUALS SRT-01-NUMB    
                GO TO III-C2-DIST-MATCH.
        WRITE NEW-MAST-REC FROM OLD-MAST-REC.
        GO TO III-B-READ-OLDMASTER.
III-C2-DIST-MATCH.
        ADD SRT-01-S-AMT TO OLD-SALES-AMT.
        WRITE NEW-MAST-REC FROM OLD-MAST-REC.
        GO TO III-READ-UPDATE-NEWMASTER.
                
IV-CLOSE-INPUT-FILES.
        CLOSE OLDMAST.
        CLOSE NEWMAST.
                
V-SORT-NEWMASTER.
        SORT SRTMAST                            
                ON ASCENDING KEY S-M-DIST-NO
                ON DESCENDING KEY S-M-SALES-AMT
                USING NEWMAST
                OUTPUT PROCEDURE IS VI-WRITE-SALES-REPORT THRU VI-I-EOF.
        STOP RUN.
                
VI-WRITE-SALES-REPORT.
        MOVE ZEROS TO REP-DIST-NO.
        OPEN OUTPUT REPFIL.
        WRITE REPT-PRINT-LINE FROM REPT-REC-HEAD.
        MOVE SPACES TO REPT-PRINT-LINE.
        WRITE REPT-PRINT-LINE.
VI-C-RETURN-SORTMASTER.
        RETURN SRTMAST AT END GO TO VI-I-EOF.
        ADD S-M-SALES-AMT TO WS-S-AMT-GRAND-TOT.
        IF S-M-DIST-NO EQUALS REP-DIST-NO
                GO TO VI-G-SAME-DISTRICT.
        IF WS-DIST-S-AMT-TOT IS GREATER THAN ZERO
                PERFORM WRITE-DIST-TOTAL.
        MOVE S-M-SALES-AMT TO WS-DIST-S-AMT-TOT.
        GO TO VI-H-WRITE-SORTMASTER.
VI-G-SAME-DISTRICT.
        ADD S-M-SALES-AMT TO WS-DIST-S-AMT-TOT.
VI-H-WRITE-SORTMASTER.
        MOVE S-M-DIST-NO TO REP-DIST-NO.
        MOVE S-M-SALESMAN TO REP-SALESMAN.
        MOVE S-M-SALES-AMT TO REPT-SALES-AMT.
        WRITE REPT-PRINT-LINE FROM REPT-REC-LINE.
        GO TO VI-C-RETURN-SORTMASTER.
VI-I-EOF.
        PERFORM WRITE-DIST-TOTAL.
        MOVE WS-S-AMT-GRAND-TOT TO REPT-S-AMT-GRAND-TOT.
        WRITE REPT-PRINT-LINE FROM REPT-REC-GRAND-TOT.
        CLOSE REPFIL.


SUBROUTINE SECTION.
                
WRITE-DIST-TOTAL.
        MOVE WS-DIST-S-AMT-TOT TO REPT-DIST-S-AMT-TOT.
        WRITE REPT-PRINT-LINE FROM REPT-REC-SUB-TOT.
        MOVE SPACES TO REPT-PRINT-LINE.
        WRITE REPT-PRINT-LINE.
                
                
READ-WRITE-OLDMASTER.
        PERFORM READ-OLDMASTER.       
        IF NOT OLDMASTER-EOF, WRITE NEW-MAST-REC FROM OLD-MAST-REC.
                
                
READ-OLDMASTER.
        READ OLDMAST, AT END, MOVE 'E' TO WS-OLDMASTER-FLAG.
                
                
  