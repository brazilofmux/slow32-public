identification division.
program-id. p.
environment division.
configuration section.
special-names.
    alphabet eb is ebcdic
    alphabet lit is "A" thru "Z".
input-output section.
file-control.
    select f assign to "x.dat" organization sequential.
data division.
file section.
fd  f code-set is lit.
01  r.
    05 a pic x.
procedure division.
    stop run.
