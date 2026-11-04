' graphics GET and PUT in SCREEN 7 (EGA, four planes): the array's
' words as QuickBASIC lays them out, each PUT action, an element as the
' start, SINGLE and LONG arrays, STEP, the last point -- QBasic 1.1's
' answers, read back in SCREEN 0
DEFINT A-Z
DIM pic(60), wide&(10), flo(10) AS SINGLE
SCREEN 7
PSET (0, 0), 1: PSET (1, 0), 2: PSET (2, 0), 3: PSET (9, 0), 5
PSET (0, 1), 15: PSET (2, 1), 9
GET (0, 0)-(9, 1), pic
PSET (40, 40), 6: PSET (41, 40), 3: PSET (42, 40), 0
PSET (60, 0), 5: PSET (61, 0), 10: PSET (62, 0), 0
GET (60, 0)-(62, 0), pic(20)
DIM hit(5, 2)
FOR act = 0 TO 5
  PSET (40, 40 + act), 6: PSET (41, 40 + act), 3: PSET (42, 40 + act), 0
  SELECT CASE act
    CASE 0: PUT (40, 40), pic(20)
    CASE 1: PUT (40, 41), pic(20), PSET
    CASE 2: PUT (40, 42), pic(20), PRESET
    CASE 3: PUT (40, 43), pic(20), AND
    CASE 4: PUT (40, 44), pic(20), OR
    CASE 5: PUT (40, 45), pic(20), XOR
  END SELECT
  FOR k = 0 TO 2: hit(act, k) = POINT(40 + k, 40 + act): NEXT
NEXT
PSET (100, 100), 9: PSET (103, 100), 4
GET (100, 100)-(107, 100), wide&
GET (100, 100)-(107, 100), flo
PSET (150, 150), 2
GET STEP(0, 0)-STEP(3, 0), pic(30)
PSET STEP(1, 1), 7
lastget = POINT(154, 151)
PUT (200, 100), pic(30), PSET
PSET STEP(1, 1), 7
lastput = POINT(201, 101)
GET (9, 9)-(6, 6), pic(45)
SCREEN 0: WIDTH 80     ' (QBasic keeps SCREEN 7's 40 columns)
FOR i = 0 TO 13: PRINT HEX$(pic(i)); " ";: NEXT: PRINT
PRINT pic(20); pic(21); HEX$(pic(22)); pic(30); pic(45); pic(46)
FOR act = 0 TO 5: PRINT hit(act, 0); hit(act, 1); hit(act, 2): NEXT
PRINT HEX$(wide&(0)); " "; HEX$(wide&(1)); flo(0); flo(1)
PRINT lastget; lastput
