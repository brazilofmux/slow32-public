identification division.
program-id. dispofa.
*> DISPLAY-OF converts national data; alphanumeric is refused (2002 15.26.3).
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  n pic n(4) value n"abcd".
01  k pic 99 value 66.
procedure division.
    display function display-of(a)
    stop run.
