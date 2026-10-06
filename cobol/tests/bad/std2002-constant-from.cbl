identification division.
program-id. p29.
*> constant-from: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 k constant from a.
procedure division.
    display 'x'
    goback.
