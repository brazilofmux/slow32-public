identification division.
program-id. natofnat.
*> NATIONAL-OF of national data is refused: it is already national (2002 15.66.3).
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  n pic n(4) value n"abcd".
01  k pic 99 value 66.
procedure division.
    display function national-of(n)
    stop run.
