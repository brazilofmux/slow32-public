identification division.
program-id. bitidx.
*> INDEXED BY on a bit array comes later; refused by name.
data division.
working-storage section.
01  t.
    05 b pic 1 usage bit occurs 8 indexed by bx.
procedure division.

    stop run.
