* Functions: the Error object -- its exported variables, the handler
* returning the previous block, an error in a called function recovering
* in the caller, a nested sequence, := into an instance variable
LOCAL bOld, bPrev, e, n
bOld := ERRORBLOCK({|o| BREAK(o)})
BEGIN SEQUENCE
   n := Trouble(2)
   ? "not here"
RECOVER USING e
   ? "class", e:className()
   ? "severity", e:severity, "subsystem", e:subSystem
   e:cargo := "noted"
   ? "cargo", e:cargo
   ? VALTYPE(e:canRetry), VALTYPE(e:description), VALTYPE(e:genCode)
END
BEGIN SEQUENCE
   BEGIN SEQUENCE
      BREAK 7
   RECOVER USING n
      ? "inner", n
      BREAK n * 2
   END
   ? "not here either"
RECOVER USING n
   ? "outer", n
END
bPrev := ERRORBLOCK(bOld)
? "handler was a block:", VALTYPE(bPrev) == "B"
? "done"

FUNCTION Trouble(x)
   LOCAL y
   y := x + "oops"
   RETURN y
