// A number a Get assigns is as wide as its edit buffer: a "999" field
// gives back a number three wide, "99999.99" eight.
PROCEDURE Main
   LOCAL n := 0, g
   ? VAL("42 "), VAL("1"), VAL("12.50"), VAL("  7")
   g := GetNew(1, 1, {|x| IIF(x == NIL, n, n := x)}, "n", "999")
   g:setFocus()
   g:overStrike("4"); g:overStrike("2")
   ? "[" + g:buffer + "]"
   g:assign()
   ? n, VALTYPE(n), LEN(STR(n))
   g:killFocus()
   n := 5
   g := GetNew(1, 1, {|x| IIF(x == NIL, n, n := x)}, "n", "99999.99")
   g:setFocus()
   ? "[" + g:buffer + "]", g:pos, g:clear
   g:overStrike("3"); g:overStrike("4")
   ? "[" + g:buffer + "]", g:pos
   g:assign()
   ? n, LEN(STR(n))
   RETURN
