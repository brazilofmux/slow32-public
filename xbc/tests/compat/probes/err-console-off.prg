// The default error handler's report is on standard error, as CA's
// ERRORSYS.PRG writes it: SET CONSOLE OFF does not hide it.
PROCEDURE Main
   LOCAL x := "a"
   ? "before"
   SET CONSOLE OFF
   ? "hidden"
   Sub(x)
RETURN

PROCEDURE Sub(x)
   ? x + 1
RETURN
