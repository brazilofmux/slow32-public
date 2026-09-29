identification division.
program-id. p.
data division.
working-storage section.
01 v pic 9(3).
screen section.
01 sc.
   05 line 1 col 1 pic 9(3) using v usage comp.
procedure division.
    stop run.
