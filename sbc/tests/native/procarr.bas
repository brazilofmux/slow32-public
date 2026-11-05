' A procedure's array held across calls: moved by the callee through a
' parameter, its own in each recursion, the same name in two procedures,
' a STATIC kept between activations, a parameter that means another array
' on the next call
DECLARE SUB holder (x%())
DECLARE SUB mover (y%())
DECLARE SUB rec (d%)
DECLARE SUB first ()
DECLARE SUB second ()
DECLARE SUB kept (v%)
DECLARE SUB either (z%())
DECLARE FUNCTION tally% (w%())
DEFINT A-Z
REDIM big(3)
big(1) = 11
holder big()
PRINT big(1); UBOUND(big)
rec 3
PRINT
first
second
first
kept 1: kept 2: kept 3
DIM p(4), q(6)
p(2) = 21: q(2) = 22
either p(): either q(): either p()
PRINT tally(p()); tally(q())
END
SUB holder (x())
x(1) = x(1) + 1
PRINT x(1); UBOUND(x)
mover x()
PRINT x(1); UBOUND(x)
x(7) = 70
mover x()
PRINT x(7); UBOUND(x)
END SUB
SUB mover (y())
REDIM y(UBOUND(y) + 4)
y(1) = UBOUND(y)
END SUB
SUB rec (d)
DIM own(d + 1)
own(d) = d * 2
IF d > 0 THEN rec d - 1
own(d + 1) = own(d) + 1
PRINT own(d + 1); UBOUND(own);
END SUB
SUB first
DIM same(2)
same(1) = same(1) + 3
second
PRINT same(1); UBOUND(same)
END SUB
SUB second
DIM same(5)
same(5) = same(5) + 9
PRINT same(5); UBOUND(same)
END SUB
SUB kept (v)
STATIC tot()
IF v = 1 THEN DIM tot(3)
tot(1) = tot(1) + v
second
PRINT tot(1)
END SUB
SUB either (z())
z(2) = z(2) + 1
second
PRINT z(2); UBOUND(z)
END SUB
FUNCTION tally (w())
s = 0
FOR i = LBOUND(w) TO UBOUND(w)
s = s + w(i)
NEXT
tally = s
END FUNCTION
