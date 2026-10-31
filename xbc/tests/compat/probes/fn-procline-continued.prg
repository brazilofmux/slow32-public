// A statement continued over lines with ";" is numbered by its last line,
// and each line it took counts toward the lines after it.
// A comment continued the same way counts too.
PROCEDURE Main
   LOCAL s
   ? PROCLINE()
   s := "a" + ;
        "b" + ;
        "c"
   ? s, PROCLINE()
*  a comment ;
*     continued
   ? "x", ;
     PROCLINE()
   Sub()
RETURN

PROCEDURE Sub
   ? PROCLINE(), ;
     PROCLINE(1)
RETURN
