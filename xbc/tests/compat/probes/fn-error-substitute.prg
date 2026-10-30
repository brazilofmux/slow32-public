// An error handler that answers a value for a substitutable error (a zero
// divisor): the operation takes the value and the statement goes on;
// a handler that BREAKs still reaches the RECOVER.
#include "error.ch"
PROCEDURE Main
   LOCAL bOld := ErrorBlock({|e| iif(e:genCode == EG_ZERODIV .AND. e:canSubstitute, -1, Break(e))})
   LOCAL n
   ? "substituted", 10 / 0, (10 / 0) + 5
   n := 3 / 0
   ? "assigned", n
   BEGIN SEQUENCE
      ? "other", UPPER(42)
      ? "not here"
   RECOVER USING n
      ? "recovered", n:genCode, n:description
   END SEQUENCE
   ErrorBlock(bOld)
   RETURN
