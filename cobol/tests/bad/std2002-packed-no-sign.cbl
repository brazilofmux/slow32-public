identification division.
program-id. p16.
*> packed-no-sign: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic 9(3) usage packed-decimal no sign.
procedure division.
    display a
    goback.
