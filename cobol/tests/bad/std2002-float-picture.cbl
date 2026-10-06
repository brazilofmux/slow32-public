identification division.
program-id. p19.
*> float-picture: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic +9.9(5)E+99.
procedure division.
    display a
    goback.
