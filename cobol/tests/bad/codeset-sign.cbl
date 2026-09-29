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
fd  f code-set is eb.
01  r.
    05 a pic s9(4).
procedure division.
    stop run.
