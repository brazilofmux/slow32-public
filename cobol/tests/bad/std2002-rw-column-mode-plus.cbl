identification division.
program-id. p-std2002-rw-column-mode-plus.
*> COLUMN LEFT/RIGHT/CENTER names an absolute column, not PLUS (13.18.14.3 rule 9).
environment division.
input-output section.
file-control.
    select prt assign to "x.prn" organization line sequential.
data division.
file section.
fd prt report is rep.
working-storage section.
01 n pic 99 value 1.
01 tb.
   05 tv pic 99 occurs 3 times.
report section.
rd rep page limit 20 lines heading 1 first detail 3.
01 det type de.
   05 line plus 1.
      10 column right plus 2 pic 99 source n.
procedure division.
    open output prt. initiate rep. generate det. terminate rep. close prt.
    stop run.
