C                                                                               
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C   RAX  TABLE  LOOKUP - SEARCH WITH XT ON TABX TO FIND YA FROM TABY            
C                                                                               
C     ------------------------------------------------------------------        
C ---                                                                           
C WARNING - NOT RECOMMENDED FOR HIGH-FREQUENCY USE OR LARGE TABLES              
C     ------------------------------------------------------------------        
C     XT # KNOWN INDEPENDENT VARIABLE                                   LINI9390
C     YA # UNKNOWN DEPENDENT VARIABLE                                   LINI9400
C     N  # NO. OF VARIABLES IN THE TABLES                                       
C     I1 # NEAREST TABLED X ON LEFT , INDEX VALUE OF.                       0060
C     I2 # NEAREST TABLED X ON RIGHT, INDEX VALUE OF.                       0070
C     TABY # DEPENDENT VARIABLES                                        LINI9430
C     TABX # INDEPENDENT VARIABLES                                      LINI9420
C                                                                           0120
C     WHERE -                                                               0130
C     TABX IS STRICTLY MONOTONE INCREASING AND SEARCH IS UPWARD,            0140
C        STARTING  AT  X%1<. OFFTABLE VALUES ARE EXTRAPOLATED .             0150
C                                                                           0160
C-MUST  DIMENSION TABX%2< , TABY%2< OR GREATER                          LINI9440
C     ------------------------------------------------------------------        
C                                                                               
      NL # N-1                                                                  
      DO 2  I2 #2,NL                                                            
      IF % XT - TABX < 5,5,2                                                    
    2 CONTINUE                                                          LINI9500
C ---   X TOO HIGH - EXTRAPOLATE                                            9505
      I2 # N                                                                    
    5 I1 # I2 - 1                                                               
C ---                                                                           
      YA # % %TABY%I2< - TABY%I1< < < / %TABX%I2< - TABX%I1< < <                
     1 * % XT - TABX%I1< <  & TABY%I1<                                          
C     ------------------------------------------------------------------        
       END                                                                      
    