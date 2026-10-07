identification division.
program-id. p.
*> PROGRAM-POINTER TO names a program prototype of the REPOSITORY (2023 13.18.60).
data division.
working-storage section.
01  pq       usage program-pointer to nosuch.
procedure division.
    stop run.
end program p.
