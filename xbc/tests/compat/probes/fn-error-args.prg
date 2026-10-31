// The error object's args: the operands of the failing operation, or the
// failing function's arguments; operation and subCode beside them.
#include "error.ch"
PROCEDURE Main
   LOCAL e
   ErrorBlock({|x| Break(x)})
   BEGIN SEQUENCE
      ? "abc" + 1
   RECOVER USING e
      Show(e)
   END SEQUENCE
   BEGIN SEQUENCE
      ? 5 * "x"
   RECOVER USING e
      Show(e)
   END SEQUENCE
   BEGIN SEQUENCE
      ? SUBSTR(12, 1)
   RECOVER USING e
      Show(e)
   END SEQUENCE
   BEGIN SEQUENCE
      ? 7 / 0
   RECOVER USING e
      Show(e)
   END SEQUENCE
   RETURN

STATIC PROCEDURE Show(e)
   LOCAL i
   ?? e:genCode, e:subCode, "[" + e:operation + "]", VALTYPE(e:args)
   IF VALTYPE(e:args) == "A"
      ?? LEN(e:args)
      FOR i := 1 TO LEN(e:args)
         ?? "", VALTYPE(e:args[i])
      NEXT
   ENDIF
   ? e:canSubstitute, e:canRetry, e:canDefault
   RETURN
