identification division.
program-id. vtl.
*> A VALUE literal longer than its item is refused; under -dialect=mf it
*> is cut with a warning (BP-D7, free/mf-valtrunc).
data division.
working-storage section.
01 m pic x(3) value "abcd".
procedure division.
    stop run.
