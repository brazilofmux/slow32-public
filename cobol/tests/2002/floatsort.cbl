*> SORT of a table on a FLOAT-LONG key (docs/usage.md): numeric order,
*> negatives and fractions included.
identification division.
program-id. floatsort.
data division.
working-storage section.
01 t.
   05 e occurs 6 ascending key f indexed by x.
      10 f float-long.
      10 n pic x.
01 i pic 9.
procedure division.
    move 2.5 to f(1) move "a" to n(1)
    move -0.5 to f(2) move "b" to n(2)
    move 2.25 to f(3) move "c" to n(3)
    move -3 to f(4) move "d" to n(4)
    move 0 to f(5) move "e" to n(5)
    move 0.125 to f(6) move "f" to n(6)
    sort e ascending f
    perform varying i from 1 by 1 until i > 6 display n(i) with no advancing end-perform
    display space
    stop run.
