// PCOUNT() inside a block counts the block's own arguments: the set/get
// block idiom of CA's DBU, {|x| if(PCount() == 0, var, var := x)}, reads
// with no argument and writes with one.  Outside the block PCOUNT() is
// the function's again.
PROCEDURE Main
   LOCAL bSetGet := {|x| iif(PCount() == 0, mVar, mVar := x)}
   PRIVATE mVar := "start"
   ? "read", EVAL(bSetGet)
   ? "write", EVAL(bSetGet, "changed")
   ? "read again", EVAL(bSetGet), mVar
   ? "three", Three(1, 2, 3)
   ? "none", Three()
   RETURN

FUNCTION Three(a, b, c)
   LOCAL b0 := {|| PCount()}, b2 := {|p, q| PCount()}
   RETURN LTRIM(STR(PCount())) + " " + LTRIM(STR(EVAL(b0))) + " " + LTRIM(STR(EVAL(b2, 7))) + " " + LTRIM(STR(PCount()))
