// A ; continuation joins lines: the next line's indentation must still
// separate the tokens (22 then .AND., as DBU writes it), and a number
// directly followed by a logical operator is still two tokens.
PROCEDURE Main
   LOCAL n := 22, k := 22
   IF n == 22;
      .AND. k == 22
      ? "joined", "yes"
   ENDIF
   IF n == 22 .AND.;
      k == 22
      ? "joined after operator", "yes"
   ENDIF
   ? "tight", n == 22.AND.k == 22, k == 22.OR.n == 0
   ? "sum", 1 +;
      2 +;
      3
   RETURN
