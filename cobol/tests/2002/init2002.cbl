*> COBOL 2002's INITIALIZE phrases (2023 14.9.20): WITH FILLER, ALL TO
*> VALUE, THEN REPLACING, THEN TO DEFAULT, and each combined; a REDEFINES
*> item below the receiver is never a receiving operand, a FILLER only
*> WITH FILLER, an item without VALUE only under DEFAULT (GR 5).  The
*> category-restricted VALUE phrase is init2002cat.  docs/conformance/initialize.md
identification division.
program-id. init2002.
data division.
working-storage section.
01 g.
   05 a  pic x(3) value "abc".
   05 filler pic x(2) value "zz".
   05 n  pic 9(3) value 7.
   05 e  pic zz9 value " 42".
   05 m  pic 9(2).
   05 t  pic x occurs 2 value "q".
   05 u  pic x(2) value "uu".
   05 r  redefines u pic x(2).
procedure division.
    move all "#" to g (1:8) move 99 to m
    initialize g
    display "85:      [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m
    initialize g with filler
    display "filler:  [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m move "xy" to r
    initialize g all to value
    display "value:   [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m move "xy" to r
    initialize g all to value then to default
    display "val+def: [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m move "xy" to r
    initialize g replacing numeric data by 3 then to default
    display "rep+def: [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m move "xy" to r
    initialize g with filler all to value
    display "fil+val: [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m move "xy" to r
    initialize g with filler all to value then replacing numeric data by 5 then to default
    display "all:     [" g (1:17) "]"
    move all "#" to g (1:8) move 99 to m move "xy" to r
    initialize a n m all to value
    display "elem:    [" g (1:17) "]"
    stop run.
