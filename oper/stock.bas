5 DIM C(100)
10 PRINT
20 PRINT "THIS PROGRAM LISTS STOCKS OWNED BY A COMPANY"
30 PRINT
40 PRINT "DO YOU WANT TO LIST STOCKS FOR ABLE CO. OR BAKER CO."
45 PRINT "(TYPE ABLE OR BAKER)";
50 INPUT A$
55 PRINT
56 PRINT
60 IF A$="BAKER" THEN 90
70 FILE #1,"ABLE.DAT"
80 GO TO 100
90 FILE #1,"BAKER.DAT"
100 FOR I=1 TO 100
105 IF END #1 THEN 145
110 INPUT #1,B$,C
120 PRINT USING 200,B$,C
130 D=D+C
140 NEXT I
145 PRINT
150 PRINT USING 210,D
200 :'LLLLLLLLLLLL #####
210 :TOTAL NO. OF SHARES IS ##########
220 END
    