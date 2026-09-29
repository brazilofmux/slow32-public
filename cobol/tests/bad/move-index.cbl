identification division.
program-id. mv.
data division.
working-storage section.
01 ix usage index.
01 n pic 9(4).
procedure division.
    move ix to n
    stop run.
