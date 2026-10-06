identification division.
program-id. p14.
*> aligned: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic x aligned.
procedure division.
    display a
    goback.
