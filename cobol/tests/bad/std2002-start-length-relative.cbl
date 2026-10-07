identification division.
program-id. p1.
*> WITH LENGTH on a relative file's START: the phrase is an indexed
*> file's, a leading part of its key (2023 14.9.41.3 rule 8).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization relative access dynamic relative key k.
data division.
file section.
fd f.
01 r pic x(10).
working-storage section.
01 k pic 9.
procedure division.
    open input f
    start f key = k with length 1
    close f
    goback.
