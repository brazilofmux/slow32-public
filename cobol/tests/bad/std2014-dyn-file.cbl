identification division.
program-id. p-dyn-file.
*> A dynamic-capacity table in the FILE SECTION (2023 8.5.1.9.1: any place other than the file section).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization line sequential.
data division.
file section.
fd f.
01 r. 05 t pic x occurs dynamic capacity in c.
procedure division.
    open input f close f
    goback.
