REM GW-BASIC graphics statements over the tube fb (self-verifying
REM via POINT; runs headless -- no viewer needed, tube grants OPEN)
SCREEN 1
PSET (10, 10), 4
PRINT POINT(10, 10); POINT(11, 10)
PRESET (10, 10)
PRINT POINT(10, 10)
LINE (0, 5)-(9, 5), 2
PRINT POINT(0, 5); POINT(5, 5); POINT(9, 5); POINT(10, 5)
LINE (20, 20)-(30, 26), 3, B
PRINT POINT(20, 23); POINT(30, 23); POINT(25, 20); POINT(25, 26); POINT(25, 23)
LINE (40, 40)-(50, 46), 5, BF
PRINT POINT(45, 43)
LINE -(60, 46), 6
PRINT POINT(55, 46)
CIRCLE (100, 100), 20, 14
PRINT POINT(120, 100); POINT(80, 100); POINT(100, 120); POINT(100, 80)
PAINT (100, 100), 9, 14
PRINT POINT(100, 100); POINT(110, 100); POINT(100, 78)
PSET STEP(0, -30), 12
PRINT POINT(100, 70)
REM diagonal line and STEP on the second point
LINE (0, 0)-STEP(4, 4), 7
PRINT POINT(0, 0); POINT(2, 2); POINT(4, 4)
REM palette animation is host-side; entry writes just must not error
PALETTE 4, 16744448
PALETTE
REM clipping is silent; out-of-range POINT is -1
PSET (-5, -5), 1
PRINT POINT(-1, -1); POINT(320, 0)
CLS
PRINT POINT(100, 100)
REM 256-color mode
SCREEN 13
PSET (0, 0), 200
PRINT POINT(0, 0)
SCREEN 0
PRINT "done"
