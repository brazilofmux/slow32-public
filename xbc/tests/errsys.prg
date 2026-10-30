* The error system, interpreted and compiled alike
LOCAL bOld, e, n
bOld := ERRORBLOCK({|o| BREAK(o)})
BEGIN SEQUENCE
   n := 1 + "a"
   ? "not here"
RECOVER USING e
   ? "caught", VALTYPE(e), e:className()
   e:cargo := 5
   ? "cargo", e:cargo
END
BEGIN SEQUENCE
   BREAK "a value"
RECOVER USING n
   ? "broke with", n
END
BEGIN SEQUENCE
   EVAL({|k| BREAK(k * 2)}, 21)
RECOVER USING n
   ? "block broke with", n
END
ERRORBLOCK(bOld)
? "done"
