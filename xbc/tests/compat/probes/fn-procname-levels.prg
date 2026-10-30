* Functions: PROCNAME() and PROCLINE() at levels 0 to 3, in a function,
* in a code block, and in an error block's handler (ERRORSYS's traceback)
PROCEDURE Main
   Outer()
   RETURN

STATIC PROCEDURE Outer
   LOCAL b := {|| Show("block")}
   Show("direct")
   EVAL(b)
   BEGIN SEQUENCE
      ERRORBLOCK({|e| Handler(e)})
      ? nosuchvar
   END SEQUENCE
   RETURN

STATIC FUNCTION Show(cWhere)
   LOCAL i
   ? cWhere + ":"
   FOR i := 0 TO 3
      ?? " " + PROCNAME(i) + "(" + LTRIM(STR(PROCLINE(i))) + ")"
   NEXT
   RETURN NIL

STATIC FUNCTION Handler(e)
   Show("handler")
   BREAK(e)
   RETURN NIL
