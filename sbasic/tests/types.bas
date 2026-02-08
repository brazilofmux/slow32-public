REM === TYPE records test ===

TYPE Point
  X AS DOUBLE
  Y AS DOUBLE
END TYPE

TYPE Person
  PNAME AS STRING
  AGE AS INTEGER
  SCORE AS DOUBLE
END TYPE

DIM p AS Point
p.X = 3.5
p.Y = 7.2
PRINT p.X; " "; p.Y

DIM who AS Person
who.PNAME = "Alice"
who.AGE = 30
who.SCORE = 95.5
PRINT who.PNAME; " age "; who.AGE; " score "; who.SCORE

REM Modify a field
who.AGE = who.AGE + 1
PRINT "After birthday: "; who.AGE

REM Second instance of same type
DIM p2 AS Point
p2.X = -1.0
p2.Y = 0.0
PRINT p2.X; " "; p2.Y

REM TYPE field used in expression
PRINT "Sum: "; p.X + p.Y

PRINT "Done"
