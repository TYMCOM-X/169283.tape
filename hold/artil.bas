10 REMARK "ARTILLARY" 
20 REMARK VER. 2.00  15/3/'71  
30 Z$(1)=""
40PRINT 
50PRINT 
60PRINT 
70PRINT 
80PRINT 
90 PRINT "YOU ARE AN ARTILLERY OFFICER WHO GIVES FIRING ORDERS TO A GUN"  
100 PRINT "CREW.  THE GUN HAS A MAXIMUM RANGE OF 46500 YARDS. A HIT WITHIN"
110 PRINT "100 YARDS OF THE TARGET WILL DESTROY IT."  
120 PRINT
130PRINT"YOU GIVE THE DEGREES OF ELEVATION THAT YOU THINK WILL PLACE THE"  
140 PRINT "PROJECTILE ON TARGET WHEN REQUESTED."
150REM T= DISTANCE TO TARGET  
160REM I = DISTANCE OF THE SHOT  
170REM E = DISTANCE OVER(NEGATIVE), OR UNDER(POSITIVE) THE TARGET 
180T =43000-25999*RND(X)
190S=0
200GO TO 510
210PRINT 
220PRINT"<<<<<BOOM!!!>>>>>"
230 PRINT
240PRINT"YOUR HAVE JUST DESTROYED YOUR GUN!" 
250PRINT "TOUGH LUCK, CHARLIE."  
260 STOP 
270GO TO 550
280PRINT "MAX. ELEVATION OF THE GUN IS 89 DEGREES."
290GO TO 550
300PRINT "OVER THE TARGET BY"ABS(E)" YARDS." 
310GO TO 550
320PRINT "SHORT OF THE TARGET BY"ABS(E)" YARDS."
330GO TO 550
340PRINT 
350 PRINT "    <<<>>>"  
360 PRINT "<<<<BOOM!!>>>>" 
370 PRINT
380 IF S<>1 THEN 410
390 Z$(2)="" 
400 GOTO 420
410 Z$(2)="S"
420 PRINT "TARGET DESTROYED AFTER"S" ROUND"Z$(2)" EXPENDED." 
430PRINT 
440PRINT "THE FORWARD AIR CONTROLLER HAS LOCATED ANOTHER TARGET." 
450PRINT "WILL YOU BE ABLE TO FIRE AGAIN (1=YES, 0=NO)";  
460INPUT G  
470IF G=1 THEN 180
480PRINT 
490PRINT "GOOD WORK! FREE BEER AT THE PX.  CREW DISMISSED." 
500STOP  
510PRINT 
520PRINT "THE "Z$(1)"TARGET IS"INT(T)" YARDS AWAY." 
530 Z$(1)="NEW "
540PRINT 
550PRINT "ELEVATION"; 
560INPUT B  
570IF B>89 THEN 280
580IF B<1 THEN 210
590S=S+1 
600B2=2*B/57.3 
610I=46500*SIN(B2)
620X=T-I 
630E=INT(X) 
640IF ABS(E)<100 THEN 340
650IF E>100 THEN 320
660IF E<-100 THEN 300
670END
 