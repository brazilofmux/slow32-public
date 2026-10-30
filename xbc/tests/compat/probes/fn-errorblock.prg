* Functions: ERRORBLOCK posts a handler; BREAK out of it to RECOVER
LOCAL bOld, x
bOld := ERRORBLOCK({|e| BREAK(e)})
BEGIN SEQUENCE
   x := 1 + "a"
RECOVER USING x
   ? "caught", VALTYPE(x)
END
ERRORBLOCK(bOld)
