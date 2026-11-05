' x! under DEFINT is a SINGLE: as a variable, a parameter, an array of a
' procedure, in the main program and out of it
DEFINT A-Z
DECLARE SUB fade ()
DECLARE SUB param (p!, q)
DECLARE FUNCTION half! (v!)
x! = 1.5: y = 1.5
DIM m!(3)
m!(1) = 1 / 63
FOR j = 1 TO 63: x! = x! + m!(1): NEXT j
PRINT x!; y
fade
a! = 2.25: b = 3
param a!, b
PRINT a!; b; half!(1.5); half!(a!)
SYSTEM
SUB fade
    DIM r!(0 TO 3), s!(0 TO 3, 1)
    g = 1
    r!(1) = g
    s!(1, 0) = g / 63
    FOR j = 1 TO 63
        r!(1) = r!(1) - s!(1, 0)
        t! = t! + s!(1, 0)
    NEXT j
    PRINT r!(1); t!; LEN(t!); LEN(g)
END SUB
SUB param (p!, q)
    p! = p! / 2: q = q + 1
END SUB
FUNCTION half! (v!)
    half! = v! / 2
END FUNCTION
