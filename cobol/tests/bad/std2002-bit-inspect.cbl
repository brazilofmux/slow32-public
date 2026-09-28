identification division.
program-id. bitins.
*> INSPECT works on characters; a USAGE BIT item is refused.
data division.
working-storage section.
01  b pic 1(8) usage bit.
01  k pic 99.
procedure division.
    inspect b tallying k for all b"1"
    stop run.
