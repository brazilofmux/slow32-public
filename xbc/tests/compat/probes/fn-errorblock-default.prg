// ERRORBLOCK() before a program posts one answers the default handler's
// block (ErrorSys posts it at startup), and that block answers a zero
// divisor with 0; a handler of the program's own can hand an error on
// to it.
PROCEDURE Main
   LOCAL b := ErrorBlock()
   ? VALTYPE(b)
   ? 10 / 0, 1 + 10 / 0
   ErrorBlock({|e| iif(e:genCode == 1, "mine", Eval(b, e))})
   ? "a" + 1, 4 + 8 / 0
   ErrorBlock(b)
   RETURN
