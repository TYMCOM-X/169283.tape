490 INPUT H$
500 CHANGE H$ TO A
510 FOR I=1 TO A(0)
520 IF A(I)>57 THEN 560
530 IF A(I)<48 THEN 560
540 NEXT I
545 F3=0
550 GO TO 580
560 PRINT "ALPHABETIC CHARACTER  "+ CHR$(A(I)) +"  IN  " +H$
570 F3=1
580 END
  