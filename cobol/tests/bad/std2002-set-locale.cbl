identification division.
program-id. p24.
*> set-locale: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
procedure division.
    set locale lc-all to user-default
    goback.
