// An error no handler of the program's answers: the default handler shows
// it (Alert, Quit), prints it with the calls that led there, and ends the
// program with ERRORLEVEL() 1.
PROCEDURE Main
   ? "before"
   Inner(3)
   ? "not reached"
   RETURN

STATIC PROCEDURE Inner(n)
   ? "inner", n
   ? "abc" + n
   ? "after"
   RETURN
