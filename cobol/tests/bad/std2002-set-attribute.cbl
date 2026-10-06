identification division.
program-id. p25.
*> set-attribute: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic 9.
procedure division.
    set a attribute blink on
    goback.
