identification division.
program-id. p22.
*> inspect-backward: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic x(5) value 'abcab'.
01 n pic 99 value 0.
procedure division.
    inspect backward a tallying n for all 'a'
    goback.
