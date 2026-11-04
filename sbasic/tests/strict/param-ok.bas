REM what QB takes: the same type by reference, (x) by value, an expression, a CONST, a suffix or AS matching
DECLARE SUB s (x#)
DECLARE SUB t (n AS INTEGER)
DECLARE FUNCTION f% (a%)
CONST K = 3
DIM d#(2), i AS INTEGER
v = 1: w# = 2: i = 4
s w#: s d#(1): s (v): s v + 1: s K: s 7
t i: t (v): PRINT f%(i); f%((v))
CALL s(w#)
SUB s (x#)
x# = x# + 1
END SUB
SUB t (n AS INTEGER)
n = n + 1
END SUB
FUNCTION f% (a%)
f% = a% * 2
END FUNCTION
