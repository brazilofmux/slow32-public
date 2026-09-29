identification division.
program-id. mv.
data division.
working-storage section.
01 a pic a(4) value "abcd".
01 n pic 9(4).
procedure division.
    move a to n
    stop run.
