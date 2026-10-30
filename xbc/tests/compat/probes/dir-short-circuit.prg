// .AND. and .OR. evaluate their right side only when the left leaves the
// answer open (DBU writes oGet:type == "C" .AND. LEN(oGet:varGet()) > 78
// over numeric cells).
PROCEDURE Main
   LOCAL n := 5
   ? "and", .F. .AND. Boom("and-right")
   ? "or", .T. .OR. Boom("or-right")
   ? "and open", .T. .AND. Boom("and-open")
   ? "or open", .F. .OR. Boom("or-open")
   ? "nested", (n > 9 .AND. Boom("nested")) .OR. (n > 1 .OR. Boom("nested2"))
   IF n == 5 .OR. Boom("if")
      ? "if taken"
   ENDIF
   ? "mixed type", n > 9 .AND. LEN(n) > 0
   RETURN

FUNCTION Boom(c)
   ? "  evaluated", c
   RETURN .T.
