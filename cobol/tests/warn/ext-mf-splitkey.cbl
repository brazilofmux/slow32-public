identification division.
program-id. extmfsk.
*> -warn-extensions under -dialect=mf: a split key's = form (BP-D2).
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic
        record key is fk = a b.
data division.
file section.
fd f.
01 r.
   05 a pic x(2).
   05 b pic 9(3).
procedure division.
    continue
    stop run.
