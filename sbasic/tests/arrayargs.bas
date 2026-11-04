' whole arrays as arguments, name(), as QBasic 1.1 runs them: the
' parameter is the caller's array (its elements, bounds, REDIM and ERASE
' through it), passed on again, from a SUB's own, a DIM SHARED one, a
' string array, two dimensions, the same array twice, recursion, and a
' STATIC SUB's
DECLARE SUB fill (c%(), n%)
DECLARE SUB deeper (d%())
DECLARE SUB grow (g())
DECLARE FUNCTION total$ (w$())
DECLARE SUB pair (a(), b())
DECLARE SUB mat (m%())
DECLARE SUB own ()
DECLARE SUB wipe (z())
DECLARE SUB down (r%(), k%)
DECLARE SUB counted (q%())
DIM grid%(1 TO 5), w$(2)
DIM SHARED keep(3)
CALL fill(grid%(), 3)
PRINT grid%(3); LBOUND(grid%); UBOUND(grid%)
deeper grid%()
PRINT grid%(1); grid%(5)
REDIM dyn(2)
dyn(1) = 7
grow dyn()
PRINT UBOUND(dyn); dyn(1); dyn(6)
w$(0) = "ab": w$(2) = "cd"
PRINT total$(w$())
DIM sq%(2, 3)
mat sq%(): PRINT sq%(2, 3); UBOUND(sq%, 2)
keep(1) = 5: pair keep(), keep(): PRINT keep(1); keep(2)
own
REDIM gone(4): gone(2) = 9: wipe gone(): PRINT UBOUND(gone)
DIM lvl%(4): down lvl%(), 4: PRINT lvl%(1); lvl%(4)
counted lvl%(): counted lvl%()
END

SUB fill (c%(), n%)
  c%(n%) = n% * 4
  PRINT LBOUND(c%); UBOUND(c%)
END SUB

SUB deeper (d%())
  d%(1) = 11
  CALL fill(d%(), 5)
END SUB

SUB grow (g())
  REDIM g(8)
  g(6) = 66
END SUB

FUNCTION total$ (w$())
  FOR i = LBOUND(w$) TO UBOUND(w$): t$ = t$ + w$(i) + ",": NEXT
  total$ = t$
END FUNCTION

SUB pair (a(), b())
  a(2) = a(1) + 1: b(1) = b(1) * 10
END SUB

SUB mat (m%())
  m%(2, 3) = UBOUND(m%, 1) * 100
END SUB

SUB own
  DIM mine%(3)
  CALL fill(mine%(), 2)
  PRINT "own"; mine%(2)
END SUB

SUB wipe (z())
  PRINT "wipe"; z(2);
  ERASE z
  REDIM z(1)
  PRINT UBOUND(z)
END SUB

SUB down (r%(), k%)
  IF k% = 0 THEN EXIT SUB
  r%(k%) = k% * k%
  down r%(), k% - 1
END SUB

SUB counted (q%()) STATIC
  n = n + 1
  PRINT "counted"; n; q%(2)
END SUB
