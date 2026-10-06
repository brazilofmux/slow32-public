identification division.
program-id. p26.
*> set-entry: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 p usage pointer.
procedure division.
    set p to entry 'abc'
    goback.
