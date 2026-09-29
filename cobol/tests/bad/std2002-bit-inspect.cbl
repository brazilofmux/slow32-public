identification division.
program-id. bitins.
*> INSPECT takes items of usage display or national (2023 14.9.22.3 rule 1).
data division.
working-storage section.
01  b pic 1(8) usage bit.
01  k pic 99.
procedure division.
    inspect b tallying k for all b"1"
    stop run.
