identification division.
program-id. p20.
*> float-condition: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a usage float-long value 1.
procedure division.
    if a is infinity display 'i' end-if
    goback.
