identification division.
program-id. p17.
*> occurs-dynamic: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 t. 05 e pic x occurs dynamic capacity in n.
01 n pic 99.
procedure division.
    display n
    goback.
