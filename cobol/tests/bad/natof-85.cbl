identification division.
program-id. natof85.
*> NATIONAL-OF is COBOL 2002; under -std=85 it is refused.
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  k pic 99 value 66.
procedure division.
    display function national-of(a)
    stop run.
