REM the forms QB takes: a variable, an element, a field, an unused name
TYPE r
 s AS STRING * 2
END TYPE
DIM v AS r, t$(2), a%(2)
PRINT VARPTR(a%(1)); VARSEG(a%); VARPTR(v); VARPTR(v.s); SADD(t$(1)); SADD(q$)
BLOAD "x"
BLOAD "x", 1
BSAVE "x", 1, 2
SYSTEM
