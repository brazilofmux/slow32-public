' the text screen as QBasic 1.1 keeps it, read back by SCREEN(row, col
' [, attribute]): PRINT's characters and COLOR's attributes, LOCATE,
' rows 1 to 24 scrolling and the 25th outside them, the cursor after the
' last column, VIEW PRINT's viewport and CLS 2, the errors -- the values
' printed last
DIM r(40)
ON ERROR GOTO caught
CLS
PRINT "AB"
COLOR 14, 1: PRINT "C";: COLOR 7, 0
r(0) = SCREEN(1, 1): r(1) = SCREEN(1, 2): r(2) = SCREEN(2, 1): r(3) = SCREEN(2, 1, 1)
r(4) = SCREEN(3, 1): r(5) = SCREEN(3, 1, 1)
LOCATE 5, 10: PRINT "Z";
r(6) = SCREEN(5, 10): r(7) = CSRLIN: r(8) = POS(0)
FOR i = 1 TO 30: PRINT i: NEXT
r(9) = CSRLIN: r(10) = SCREEN(1, 2): r(11) = SCREEN(24, 2): r(12) = SCREEN(25, 1)
LOCATE 25, 1: PRINT "w";
r(13) = CSRLIN: r(14) = SCREEN(25, 1)
LOCATE 25, 1: PRINT "v"
r(15) = CSRLIN: r(16) = SCREEN(25, 1)
LOCATE 10, 80: PRINT "y";
r(17) = CSRLIN: r(18) = POS(0)
CLS
LOCATE 3, 1: PRINT "keep";
VIEW PRINT 5 TO 10
r(19) = CSRLIN
FOR i = 1 TO 12: PRINT i: NEXT
r(20) = CSRLIN: r(21) = SCREEN(3, 1): r(22) = SCREEN(5, 2): r(23) = SCREEN(9, 3)
n = 24
LOCATE 2, 1
r(25) = CSRLIN
CLS 2
r(26) = SCREEN(3, 1): r(27) = CSRLIN
VIEW PRINT
n = 28
r(n) = SCREEN(0, 1)
r(n) = SCREEN(26, 1)
r(n) = SCREEN(1, 81)
CLS
FOR i = 0 TO n - 1: PRINT r(i);: NEXT
END
caught:
r(n) = 1000 + ERR: n = n + 1
RESUME NEXT
