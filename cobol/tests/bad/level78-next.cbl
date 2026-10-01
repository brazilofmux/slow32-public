identification division.
program-id. l78next.
*> A level 78 VALUE of NEXT (Micro Focus's offset) is not implemented.
data division.
working-storage section.
01 a pic x(4).
78 after-a value next.
procedure division.
    stop run.
