identification division.
program-id. skeycat.
*> SOURCE IS parts are alphanumeric or national (2002 12.3.4.12 rule 2).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic
        record key is fk source is a b.
data division.
file section.
fd f.
01 r.
   05 a pic x(2).
   05 b pic 9(3).
procedure division.
    continue
    stop run.
