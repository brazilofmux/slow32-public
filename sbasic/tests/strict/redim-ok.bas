REM REDIMs QB loads: of a DIM with bounds not constant, of a REDIM's own, a procedure's own array of the same name
DECLARE SUB s ()
n = 5
DIM a(n)
REDIM a(6)
REDIM b(5)
REDIM b(7)
DIM c(3)
s
PRINT UBOUND(a); UBOUND(b)
SUB s
REDIM c(9)
PRINT UBOUND(c)
END SUB
