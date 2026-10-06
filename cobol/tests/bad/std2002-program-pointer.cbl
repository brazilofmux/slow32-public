identification division.
program-id. p15.
*> program-pointer: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 p usage program-pointer.
procedure division.
    display 'x'
    goback.
