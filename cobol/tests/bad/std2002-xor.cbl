identification division.
program-id. p21.
*> xor: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic 9 value 1.
procedure division.
    if a = 1 xor a = 2 display 'x' end-if
    goback.
