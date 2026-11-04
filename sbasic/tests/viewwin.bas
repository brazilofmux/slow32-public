' VIEW and WINDOW in SCREEN 7: the world mapped onto the viewport (y up,
' or down under WINDOW SCREEN), PMAP and POINT both ways, STEP in the
' world, clipping, VIEW's fill and border, CLS of the viewport, the last
' point, and the errors -- QBasic 1.1's answers, read back in SCREEN 0
DIM r(80)
ON ERROR GOTO caught
SCREEN 7
WINDOW (0, 0)-(2, 2)
r(0) = PMAP(1, 0): r(1) = PMAP(1, 1): r(2) = PMAP(0, 1): r(3) = PMAP(2, 0)
r(4) = PMAP(160, 2): r(5) = PMAP(100, 3): r(6) = PMAP(0.5, 1): r(7) = PMAP(3, 0): r(8) = PMAP(-3, 1)
WINDOW SCREEN (0, 0)-(2, 2)
r(9) = PMAP(1, 1): r(10) = PMAP(0.5, 1): r(11) = PMAP(100, 3)
WINDOW (2, 2)-(0, 0)
r(12) = PMAP(0, 0): r(13) = PMAP(0, 1)
PSET (1, 1), 2
r(14) = POINT(0): r(15) = POINT(1): r(16) = POINT(2): r(17) = POINT(3)
PSET STEP(0.5, 0.5), 3
r(18) = POINT(0): r(19) = POINT(1): r(20) = POINT(2): r(21) = POINT(3)
r(22) = POINT(1.5, 1.5)
WINDOW
r(23) = POINT(239, 50)
VIEW (10, 20)-(109, 69)
PSET (0, 0), 3: PSET (99, 49), 5: PSET (100, 0), 6
r(24) = PMAP(0, 0): r(25) = POINT(0): r(26) = POINT(1)
WINDOW (0, 0)-(1, 1)
r(27) = PMAP(1, 0): r(28) = PMAP(0, 1): r(29) = PMAP(0.5, 0): r(30) = PMAP(50, 2)
PSET (1, 1), 4
VIEW
WINDOW
r(31) = POINT(10, 20): r(32) = POINT(109, 69): r(33) = POINT(110, 20): r(34) = POINT(109, 20)
VIEW SCREEN (150, 20)-(249, 69)
PSET (145, 25), 3: PSET (155, 25), 5
r(35) = PMAP(155, 0): r(36) = POINT(155, 25): r(37) = POINT(145, 25)
VIEW
r(38) = POINT(145, 25)
PSET (2, 2), 7
VIEW (10, 90)-(20, 100), 4, 2
r(39) = POINT(5, 5): r(40) = POINT(0, 0)
CLS
r(41) = POINT(5, 5)
VIEW
r(42) = POINT(15, 95): r(43) = POINT(9, 89): r(44) = POINT(21, 95): r(45) = POINT(2, 2)
VIEW (40, 90)-(50, 100)
LINE (0, 0)-(30, 0), 5
CIRCLE (5, 5), 3, 6
VIEW
r(46) = POINT(40, 90): r(47) = POINT(50, 90): r(48) = POINT(51, 90): r(49) = POINT(48, 95)
n = 50
VIEW (10, 10)-(400, 20)
VIEW (5, 5)-(5, 5)
WINDOW (0, 0)-(0, 5)
WINDOW (1, 1)-(2, 1)
SCREEN 0: WIDTH 80
FOR i = 0 TO n - 1
  PRINT r(i);
  IF i MOD 10 = 9 THEN PRINT
NEXT
PRINT
END
caught:
r(n) = 1000 + ERR: n = n + 1
RESUME NEXT
