' CIRCLE as QBasic 1.1 draws it: the mode's aspect (4/3 of a pixel's
' height over its width), an aspect given (under 1 the y radius shrinks,
' over 1 the x), arcs counterclockwise from start to end, round the far
' side when start is past end; read back in SCREEN 0 (each figure's
' extremes, and its pixels' count)
DECLARE SUB measure (n%)
DIM SHARED r(80)
ON ERROR GOTO caught
SCREEN 7
CIRCLE (160, 100), 20, 4: measure 0
CIRCLE (160, 100), 20, 4, , , 1: measure 5
CIRCLE (160, 100), 20, 4, , , 2: measure 10
CIRCLE (160, 100), 20, 4, , , .5: measure 15
CIRCLE (160, 100), 20, 4, 0, 1.5708, 1: measure 20
CIRCLE (160, 100), 20, 4, 5.5, .5, 1: measure 25
CIRCLE (160, 100), 20, 4, 3, , .75: measure 30
CIRCLE (160, 100), 3, 4: measure 35
CIRCLE (160, 100), 0, 4: measure 40
SCREEN 8
CIRCLE (160, 100), 20, 4: measure 45
CIRCLE (160, 100), 20, 4, 7
SCREEN 0: WIDTH 80
FOR i = 0 TO 54
  PRINT r(i);
  IF i MOD 5 = 4 THEN PRINT
NEXT
END
caught:
r(50) = ERR
RESUME NEXT

' the lit pixels' extremes up and down column 160, along row 100 (the
' first and last of each), and their count; then the screen cleared
SUB measure (n%)
  r(n%) = -1: r(n% + 1) = -1: r(n% + 2) = -1: r(n% + 3) = -1
  FOR y = 60 TO 140
    IF POINT(160, y) = 4 THEN IF r(n%) < 0 THEN r(n%) = y ELSE r(n% + 1) = y
  NEXT
  FOR x = 120 TO 200
    IF POINT(x, 100) = 4 THEN IF r(n% + 2) < 0 THEN r(n% + 2) = x ELSE r(n% + 3) = x
  NEXT
  c = 0
  FOR y = 60 TO 140: FOR x = 120 TO 200
    IF POINT(x, y) = 4 THEN c = c + 1
  NEXT: NEXT
  r(n% + 4) = c
  CLS
END SUB
