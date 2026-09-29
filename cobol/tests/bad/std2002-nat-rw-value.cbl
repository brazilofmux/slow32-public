identification division.
program-id. rw.
*> A national VALUE in an alphanumeric report field would be a
*> national-to-alphanumeric MOVE; refused.
environment division.
input-output section.
file-control.
    select p assign to "x.prn".
data division.
file section.
fd  p report is r.
report section.
rd  r.
01  d type detail line plus 1.
    05 column 1 pic x(8) value n"東京".
procedure division.
    open output p initiate r generate d terminate r close p
    stop run.
