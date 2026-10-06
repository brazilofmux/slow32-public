identification division.
program-id. p32.
*> anycase: refused by name (docs/plans/standard-queue.md item 1); it was a
*> parse error naming something else.
data division.
working-storage section.
01 a pic x(5) value '1,00'.
procedure division.
    display function numval-c(a 'EUR' anycase)
    goback.
