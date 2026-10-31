* Functions: SETMODE() changes the display mode, and MAXROW() and MAXCOL()
* follow it; a size left out (or not a number) stays as it is
Mode("50 by 80",  {|| SETMODE(50, 80)})
Mode("43 by 132", {|| SETMODE(43, 132)})
Mode("rows only", {|| SETMODE(28)})
Mode("cols only", {|| SETMODE(NIL, 80)})
Mode("not nums",  {|| SETMODE("x", "y")})
Mode("none",      {|| SETMODE()})
Mode("zero",      {|| SETMODE(0, 0)})
Mode("no rows",   {|| SETMODE(0, 80)})
Mode("25 by 80",  {|| SETMODE(25, 80)})
RETURN

STATIC PROCEDURE Mode(cWhat, bSet)
   LOCAL lOk := EVAL(bSet)
   ? cWhat, lOk, MAXROW(), MAXCOL()
RETURN
