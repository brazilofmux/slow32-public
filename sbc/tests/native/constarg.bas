' a CONST passed to a SUB is a value of its own each time: nothing the
' SUB does reaches the CONST, or the other argument it was passed as
DECLARE SUB s (a%, b%)
DECLARE SUB t (x%, y%)
DECLARE SUB u (q!, r$)
DECLARE FUNCTION f% (p%)
CONST k = -2, m = 7, half = 2.5, word$ = "hi"
v% = 1
s k, k
s k, m
s 3, 3
s v%, v%
PRINT v%
PRINT k; m
t k, k
u half, word$
u m, word$
s half, half
PRINT half; word$; f%(m); m
SYSTEM
SUB s (a%, b%)
    PRINT a%; b%;
    a% = 10
    PRINT a%; b%;
    b% = 20
    PRINT a%; b%
END SUB
SUB t (x%, y%)
    IF x% = k THEN x% = 5
    IF y% = k THEN y% = 6
    PRINT x%; y%; k
END SUB
SUB u (q!, r$)
    q! = q! + 1: r$ = r$ + "!"
    PRINT q!; r$
END SUB
FUNCTION f% (p%)
    p% = p% + 1
    f% = p%
END FUNCTION
