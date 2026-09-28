identification division.
program-id. charnata.
*> CHAR-NATIONAL takes an integer ordinal position (2002 15.16.3).
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  n pic n(4) value n"abcd".
01  k pic 99 value 66.
procedure division.
    display function char-national(a)
    stop run.
