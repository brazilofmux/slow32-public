' a jump into a FOR's body, a limit that is a constant: the FOR never ran,
' so its NEXT has the limit 0, and the step 1 if none is written, else 0 --
' the compiler may not fold either into a NEXT reached this way.  And the
' same after a FOR whose start overflowed, resumed past
DEFINT A-Z
CONST L = 5
i = 2
GOTO inside
FOR i = 1 TO L
  PRINT "body";
inside:
  PRINT i;
NEXT i
PRINT "out"; i
j = -3: c = 0
GOTO inner2
FOR j = 1 TO 9 STEP 2
inner2:
  PRINT j;
  c = c + 1: IF c > 2 THEN j = 4
NEXT j
PRINT "out"; j
ON ERROR GOTO skip
c = 0
FOR k = 40000 TO 40004 STEP 3
  PRINT k;
  c = c + 1: IF c > 2 THEN k = 7
NEXT k
PRINT "out"; k
m = 2
FOR m = 40000 TO 40004
  PRINT m;
NEXT m
PRINT "out"; m
END
skip: RESUME NEXT
