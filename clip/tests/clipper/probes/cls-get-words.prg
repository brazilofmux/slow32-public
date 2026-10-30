// The Get's word moves and deletions, as GETSYS.PRG's reader binds them:
// wordLeft, wordRight, delWordLeft, delWordRight, delEnd on a buffer of
// several words, with pos and buffer shown after each.
PROCEDURE Main
   LOCAL oGet, cVar := "alpha beta  gamma   "
   oGet := GetNew(0, 0, {|x| iif(PCount() == 0, cVar, cVar := x)}, "cVar")
   oGet:setFocus()
   oGet:end()
   ? "end", oGet:pos
   oGet:wordLeft()
   ? "wordLeft", oGet:pos
   oGet:wordLeft()
   ? "wordLeft", oGet:pos
   oGet:wordRight()
   ? "wordRight", oGet:pos
   oGet:home()
   oGet:wordRight()
   ? "home wordRight", oGet:pos
   oGet:delWordRight()
   ? "delWordRight", oGet:pos, "[" + oGet:buffer + "]"
   oGet:wordRight()
   oGet:delWordLeft()
   ? "delWordLeft", oGet:pos, "[" + oGet:buffer + "]"
   oGet:right()
   oGet:right()
   oGet:delEnd()
   ? "delEnd", oGet:pos, "[" + oGet:buffer + "]", oGet:changed
   oGet:assign()
   oGet:killFocus()
   ? "var", "[" + cVar + "]"
   RETURN
