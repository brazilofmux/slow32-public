// Blocks made in one call share its LOCALs after it returns (detached
// locals): one block's change is the other's.  Two calls, two variables.
PROCEDURE Main
   LOCAL a := MakeCounter(10), b := MakeCounter(100)
   EVAL(a[1]); EVAL(a[1]); EVAL(b[1])
   ? EVAL(a[2]), EVAL(b[2])
   EVAL(a[3], 50)
   EVAL(a[1])
   ? EVAL(a[2]), EVAL(b[2])
   ? Inner(7)
   RETURN

STATIC FUNCTION MakeCounter(nStart)
   LOCAL n := nStart, cName := "c"
   LOCAL bInc := {|| n := n + 1, cName := cName + "+"}
   LOCAL bGet := {|| STR(n, 4) + " " + cName}
   LOCAL bSet := {|x| n := x}
   RETURN {bInc, bGet, bSet}

// a block that outlives its call, evaluated while another call at the
// same depth has a LOCAL of the same name
STATIC FUNCTION Inner(n)
   LOCAL c := MakeCounter(1)
   EVAL(c[1])
   RETURN EVAL(c[2]) + " / " + STR(n, 2)
