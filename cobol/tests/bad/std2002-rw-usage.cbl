identification division.
program-id. p.
environment division.
input-output section.
file-control.
    select rf1 assign to "o.txt".
data division.
file section.
fd rf1 report is r.
working-storage section.
01 v pic 9(3).
report section.
rd r.
01 d type detail.
   05 line 1.
      10 column 1 pic 9(3) source v usage comp.
procedure division.
    stop run.
