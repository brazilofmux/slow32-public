identification division.
program-id. rw.
*> USAGE NATIONAL on a report field takes PICTURE N or a numeric
*> (numeric-edited) PICTURE; PICTURE X is refused.
environment division.
input-output section.
file-control.
    select p assign to "x.prn".
data division.
file section.
fd  p report is r.
working-storage section.
01  a pic x(4) value "abcd".
report section.
rd  r.
01  d type detail line plus 1.
    05 column 1 pic x(4) usage national source a.
procedure division.
    open output p initiate r generate d terminate r close p
    stop run.
