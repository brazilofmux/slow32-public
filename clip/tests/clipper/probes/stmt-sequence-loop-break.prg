* Statements: BREAK out of a loop and a function inside a sequence; a
* sequence with no RECOVER; a BREAK inside EVAL
LOCAL i, v
BEGIN SEQUENCE
   FOR i := 1 TO 10
      IF i == 4
         BREAK i
      ENDIF
   NEXT
RECOVER USING v
   ? "left the loop at", v
END
BEGIN SEQUENCE
   Deep(1)
   ? "not here"
END
? "after the plain sequence"
BEGIN SEQUENCE
   EVAL({|k| BREAK(k + 1)}, 41)
RECOVER USING v
   ? "from the block", v
END

FUNCTION Deep(n)
   IF n > 3
      BREAK
   ENDIF
   Deep(n + 1)
   ? "unwound through", n
   RETURN NIL
