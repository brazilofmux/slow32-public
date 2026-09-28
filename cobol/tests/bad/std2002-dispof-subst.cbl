identification division.
program-id. dispofs.
*> The substitution character of DISPLAY-OF is one alphanumeric character (2002 15.26.3).
data division.
working-storage section.
01  a pic x(4) value "abcd".
01  n pic n(4) value n"abcd".
01  k pic 99 value 66.
procedure division.
    display function display-of(n, "??")
    stop run.
