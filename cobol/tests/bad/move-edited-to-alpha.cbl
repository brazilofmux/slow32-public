identification division.
program-id. mv.
data division.
working-storage section.
01 e pic zz9.99 value "  1.23".
01 a pic a(6).
procedure division.
    move e to a
    stop run.
