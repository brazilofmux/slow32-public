' graphics GET and PUT in SCREEN 13 (a byte a pixel) and 12 (four
' planes), compiled: the array's words, the actions, an element of a
' two-dimensional array as the start, PUT in a SUB on a SHARED array,
' and the errors (PUT past the edge, an array too small)
DECLARE SUB stamp (x%, y%)
DEFINT A-Z
DIM SHARED sprite(100)
DIM grid(3, 20), small(1), errs(5), seen(12)
ON ERROR GOTO caught
SCREEN 13
FOR i = 0 TO 4: PSET (i, 0), i * 50: PSET (i, 1), 255 - i: NEXT
GET (0, 0)-(4, 1), sprite
PSET (100, 100), 7: PSET (101, 100), 200
PUT (100, 100), sprite, AND
seen(0) = POINT(100, 100): seen(1) = POINT(101, 100): seen(2) = POINT(102, 101)
stamp 150, 20
seen(3) = POINT(151, 20): seen(4) = POINT(154, 21)
GET (3, 1)-(0, 0), grid(2, 4)
PUT (316, 0), sprite
PUT (10, 199), sprite
GET (0, 0)-(4, 1), small
PUT (60, 60), grid(2, 4), PRESET
seen(5) = POINT(60, 60): seen(6) = POINT(63, 61)
SCREEN 12
PSET (0, 0), 5: PSET (7, 0), 12: PSET (8, 0), 15: PSET (1, 1), 9
GET (0, 0)-(8, 1), sprite(50)
PUT (300, 300), sprite(50), PSET
seen(7) = POINT(300, 300): seen(8) = POINT(307, 300): seen(9) = POINT(308, 300): seen(10) = POINT(301, 301)
PUT (300, 300), sprite(50), XOR
seen(11) = POINT(308, 300)
SCREEN 0: WIDTH 80
FOR i = 0 TO 8: PRINT HEX$(sprite(i)); " ";: NEXT: PRINT
FOR i = 50 TO 60: PRINT HEX$(sprite(i)); " ";: NEXT: PRINT
FOR i = 4 TO 8: PRINT grid(2, i);: NEXT: PRINT
FOR i = 0 TO 11: PRINT seen(i);: NEXT: PRINT
FOR i = 0 TO ne - 1: PRINT errs(i);: NEXT: PRINT
END
caught:
errs(ne) = ERR: ne = ne + 1
RESUME NEXT

SUB stamp (x%, y%)
  PUT (x%, y%), sprite, PSET
END SUB
