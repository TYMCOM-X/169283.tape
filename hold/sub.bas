10 PRINT "     START      SUB"
20 INPUT S
30 IF S=0 THEN 90
40 PRINT "               ";S
50 INPUT A
60 S=S-A
65 PRINT "          ";A;S
70 IF A=0 THEN 10
80 GO TO 50
90 END
   