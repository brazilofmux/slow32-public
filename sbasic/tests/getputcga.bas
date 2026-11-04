' graphics GET and PUT in SCREEN 1 (CGA, two bits a pixel), and their
' errors: PUT past the screen's edge, an array too small -- QBasic
' 1.1's answers, read back in SCREEN 0 (a string array QBasic refuses
' as it loads: Type mismatch)
DEFINT A-Z
DIM a(40), tiny(2), errs(5)
ON ERROR GOTO caught
SCREEN 1: CLS     ' (the screen QBasic is run in starts with dots on it)
PSET (0, 10), 1: PSET (1, 10), 2: PSET (2, 10), 3: PSET (9, 10), 1
PSET (0, 11), 3: PSET (2, 11), 1
GET (0, 10)-(9, 11), a
PUT (100, 50), a, PSET
x = POINT(100, 50): y = POINT(102, 51): z = POINT(109, 50)
PUT (311, 100), a
PUT (310, 198), a
GET (0, 10)-(9, 11), tiny
PUT (100, 60), a, PRESET
p = POINT(101, 60): q = POINT(103, 60)
PUT (100, 50), a
r = POINT(100, 50)
SCREEN 0: WIDTH 80
FOR i = 0 TO 5: PRINT HEX$(a(i)); " ";: NEXT: PRINT
PRINT x; y; z; p; q; r
FOR i = 0 TO ne - 1: PRINT errs(i);: NEXT: PRINT
END
caught:
errs(ne) = ERR: ne = ne + 1
RESUME NEXT
