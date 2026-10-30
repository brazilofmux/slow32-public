// A Get over a set/get block, as DBU's cell editor makes one: the block
// reads the memvar with no argument and writes it with one, type and
// varGet()/varPut() follow the variable, and a Get over a name alone
// does the same.
PROCEDURE Main
   LOCAL oGet
   PRIVATE mGetVar := "London", nNum := 36
   oGet := GetNew(1, 1, {|x| iif(PCount() == 0, mGetVar, mGetVar := x)}, "mGetVar")
   ? "type", oGet:type, "value", oGet:varGet(), "len", LEN(oGet:varGet())
   oGet:varPut("Paris")
   ? "after put", mGetVar, oGet:varGet()
   oGet := GetNew(2, 1, {|x| iif(PCount() == 0, nNum, nNum := x)}, "nNum")
   ? "numeric", oGet:type, oGet:varGet()
   oGet := GetNew(3, 1, NIL, "mGetVar")
   ? "by name", oGet:type, oGet:varGet()
   RETURN
