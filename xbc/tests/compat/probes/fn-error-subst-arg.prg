// A handler's value stands in for a failed operation that can take one:
// an argument error on an operator or a function, as for a zero divisor.
#include "error.ch"
PROCEDURE Main
   LOCAL bOld := ErrorBlock({|e| iif(e:canSubstitute, "S" + LTRIM(STR(e:subCode)), Break(e))})
   LOCAL x
   x := "abc" + 1
   ? x
   ? 5 * "x", LEN(SUBSTR(12, 1))
   ? "abc" < 3
   ErrorBlock(bOld)
   RETURN
