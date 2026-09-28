identification division.
program-id. natrm.
*> Reference modification counts national characters; not yet.
data division.
working-storage section.
01  n pic n(4).
procedure division.
    display n(1:2)
    stop run.
