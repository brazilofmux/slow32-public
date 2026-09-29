identification division.
program-id. natofrm.
*> Reference modification of a function at a computed position comes later.
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  k pic 9 value 2.
procedure division.
    display function national-of(a)(k:2)
    stop run.
