identification division.
program-id. natofnum.
*> NATIONAL-OF converts alphanumeric text; a number is not text (2002 15.66.3).
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  n pic n(4) value n"abcd".
01  k pic 99 value 66.
procedure division.
    display function national-of(k)
    stop run.
