identification division.
program-id. subx85.
data division.
working-storage section.
01 t.
   05 e pic 9 occurs 9.
01 i pic 9 value 3.
procedure division.
    move 1 to e(9 - i)
    stop run.
