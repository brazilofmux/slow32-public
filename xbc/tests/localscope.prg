* A LOCAL is its routine's alone, at a main program's top level too:
* a routine it calls does not see it, nor a macro or TYPE(); a field of
* its name does not hide it; M->name is a memory variable beside it
LOCAL cTop := "top local"
PRIVATE cShared := "private"
? TYPE("cTop"), TYPE("cShared"), Peek()
? Twin()
? cTop
RETURN

FUNCTION Peek()
RETURN TYPE("cTop") + " " + TYPE("cShared")

FUNCTION Twin()
   LOCAL cPair := "local"
   M->cPair := "memvar"
RETURN cPair + " " + M->cPair
