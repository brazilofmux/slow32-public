identification division.
program-id. p18.
*> float-literal: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a usage float-long.
procedure division.
    compute a = 1.5E+3
    goback.
