identification division.
program-id. p23.
*> set-content: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a usage float-long.
procedure division.
    set content of a to farthest-from-zero
    goback.
