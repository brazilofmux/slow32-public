REM a variable passed by reference to a parameter of another type: QB says Parameter type mismatch
DECLARE SUB s (x#)
v = 1
s v
PRINT v
SUB s (x#)
x# = 2
END SUB
