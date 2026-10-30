// A block made in one function and evaluated in another, deeper one: the
// deeper function's LOCALs survive the evaluation, including when the
// block itself calls a function (DBU's freshOrder() lost its nRec across
// oB:forceStable(), whose column blocks call functions).
PROCEDURE Main
   LOCAL bCall := {|| Helper() }, bPlain := {|| 1 }, bParam := {|x| Helper() + x }
   ? "plain", Deeper(bPlain)
   ? "call", Deeper(bCall)
   ? "param", Deeper2(bParam)
   ? "nested", Deeper(Nested())
   RETURN

FUNCTION Deeper(b)
   LOCAL nRec := 5, cName := "kept"
   LOCAL nGot := EVAL(b)
   RETURN cName + " " + LTRIM(STR(nRec)) + " " + LTRIM(STR(nGot))

FUNCTION Deeper2(b)
   LOCAL nRec := 7
   LOCAL n := EVAL(b, 10)
   nRec += n
   RETURN nRec

FUNCTION Nested()
   LOCAL nIn := 3
   RETURN {|| Helper() + nIn }

FUNCTION Helper()
   LOCAL x := 1, y := 2
   RETURN x + y
