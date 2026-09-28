identification division.
program-id. rw.
*> A national report field (PIC N) waits for a ruling on page columns; refused by name.
environment division.
input-output section.
file-control.
    select p assign to "x.prn".
data division.
file section.
fd  p report is r.
working-storage section.
01  n pic n(4) value n"東京".
report section.
rd  r.
01  d type detail line plus 1.
    05 column 1 pic n(4) source n.
procedure division.
    open output p initiate r generate d terminate r close p
    stop run.
