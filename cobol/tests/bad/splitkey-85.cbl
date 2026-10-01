identification division.
program-id. skey85.
*> A record key SOURCE IS ... (a split key) is COBOL 2002.
environment division.
input-output section.
file-control.
    select f assign to "x.dat" organization indexed access dynamic
        record key is fk source is a.
data division.
file section.
fd f.
01 r.
   05 a pic x(2).
   05 b pic 9(3).
procedure division.
    continue
    stop run.
