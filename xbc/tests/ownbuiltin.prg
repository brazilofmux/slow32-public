* A program's routine named as a built-in is the program's -- also when
* the first call the program makes is that routine, as a statement
Bar()
? Bar()
? Pad("x")
RETURN

PROCEDURE Bar
   ? "own Bar"
RETURN

FUNCTION Pad(c)
RETURN "[" + c + "]"
