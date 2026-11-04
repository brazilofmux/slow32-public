' the text screen's rows as QBasic keeps them on a VGA: WIDTH 80, 43 and
' 40, 50 (the BIOS's rows and columns), the last row outside the viewport
' (a line filled there leaves the cursor at the viewport's bottom, and
' nothing scrolls), LOCATE past the rows or the columns, WIDTH , rows,
' and the row counts a VGA has not (26, 30, 60) -- values printed last
DIM r(40), e$(1)
ON ERROR GOTO caught
WIDTH 80, 43
DEF SEG = 0
r(0) = PEEK(&H484): r(1) = PEEK(&H44A): r(2) = CSRLIN
LOCATE 43, 1: PRINT "end";
r(3) = CSRLIN: r(4) = SCREEN(43, 3)
LOCATE 42, 1: PRINT "m"
PRINT "n"
r(5) = CSRLIN
k = 1: LOCATE 44, 1
VIEW PRINT 6 TO 30
r(6) = CSRLIN
WIDTH 40, 50
r(7) = PEEK(&H484): r(8) = PEEK(&H44A)
LOCATE 50, 40: PRINT "k";
r(9) = CSRLIN: r(10) = POS(0)
k = 2: LOCATE 1, 41
WIDTH 80, 25
LOCATE 25, 80: PRINT "s";
r(11) = CSRLIN: r(12) = POS(0): r(13) = SCREEN(25, 80): r(14) = SCREEN(24, 1)
LOCATE 25, 1: PRINT "t"
r(15) = CSRLIN: r(16) = SCREEN(25, 1)
VIEW PRINT 4 TO 12
LOCATE 12, 80: PRINT "u";
r(17) = CSRLIN: r(18) = POS(0)
LOCATE 25, 79: PRINT "vw";
r(19) = CSRLIN: r(20) = POS(0)
VIEW PRINT
k = 3: WIDTH 80, 26
k = 4: WIDTH 80, 30
k = 5: WIDTH 80, 60
k = 6: WIDTH , 43
r(21) = PEEK(&H484)
DEF SEG
WIDTH 80, 25
LOCATE 1, 1
FOR i = 0 TO 21: PRINT r(i);: NEXT
PRINT
PRINT e$(0)
END
caught:
e$(0) = e$(0) + STR$(k) + ":" + MID$(STR$(ERR), 2)
RESUME NEXT
