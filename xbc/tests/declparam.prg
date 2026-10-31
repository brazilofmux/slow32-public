* Parameters declared in the header, FUNCTION f(a, b): LOCALs of the call,
* NIL past the arguments given; and a BREAK that leaves a called function
* for its caller's sequence, from a statement and from an error's handler
LOCAL bOld
? Twice(3, 4), Twice(3)
DO Show WITH "hi", 2
? Outer()
bOld := ERRORBLOCK({|e| BREAK(e)})
? Guard({|| Inner()})
? Guard({|| Opens()})
ERRORBLOCK(bOld)
? "end"

STATIC FUNCTION Twice(a, b)
   b := IF(b == NIL, 0, b)
   RETURN a * 2 + b

PROCEDURE Show(c, n)
   ? "show", c, n
   RETURN

FUNCTION Outer()
   LOCAL r := "none"
   BEGIN SEQUENCE
      Thrower(7)
      r := "not reached"
   RECOVER USING r
   END SEQUENCE
   RETURN r

FUNCTION Thrower(n)
   BREAK n * 6
   RETURN NIL

FUNCTION Guard(b)
   LOCAL e
   BEGIN SEQUENCE
      EVAL(b)
      RETURN "no error"
   RECOVER USING e
   END SEQUENCE
   RETURN e:description

FUNCTION Inner()
   RETURN UndefinedHere + 1

FUNCTION Opens()
   USE nodir/nosuch
   RETURN NIL
