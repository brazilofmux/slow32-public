*> MOVE identifies its sender once, before the first receiver (X3.23-1985
*> VI-103 MOVE general rule 2; 2023 14.9.25.4 rule 1): MOVE a (b) TO b,
*> c (b) is MOVE a (b) TO temp, then temp to each.  A subscript, a
*> reference modifier's start, an OCCURS DEPENDING ON item and a function
*> are all evaluated once, whatever the receivers ahead do to them.
*> docs/conformance/move.md
identification division.
program-id. moveonce.
data division.
working-storage section.
01 t.
   05 te pic 9 occurs 5.
01 b pic 9 value 2.
01 c.
   05 ce pic 9 occurs 5.
01 s pic x(6) value "abcdef".
01 k pic 9 value 2.
01 y pic x(2).
01 cnt pic 9 value 3.
01 g.
   05 e pic x occurs 1 to 5 depending on cnt.
01 h2 pic x(5).
01 r1 pic 9v9(8).
01 r2 pic 9v9(8).
procedure division.
    move "31415" to t
    move zeros to c
    move te(b) to b, ce(b)
    display "subscript " b " " c
    move "45" to s(2:2)
    move s(k:2) to k y
    display "refmod " k " " y
    move 5 to cnt
    move "31abc" to g
    move 2 to cnt
    move g to cnt h2
    display "odo " cnt " [" h2 "]"
    move function random(7) to r1
    move function random to r1 r2
    if r1 = r2 display "function once" else display "function twice" end-if
    stop run.
