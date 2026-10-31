// An argument error raised inside a built-in function, no handler of the
// program's: the function heads the calls it was raised in, at line 0.
PROCEDURE Main
   LOCAL n := 12
   ? "start"
   ? SUBSTR(n, 1)
   ? "not reached"
   RETURN
