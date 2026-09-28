identification division.
program-id. boolval.
*> A boolean item takes a boolean literal or ZERO as its VALUE.
data division.
working-storage section.
01  b pic 1(4) value "0101".
procedure division.

    stop run.
