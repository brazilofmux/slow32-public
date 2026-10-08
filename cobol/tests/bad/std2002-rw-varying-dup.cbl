identification division.
program-id. p-std2002-rw-varying-dup.
*> a VARYING data-name is defined nowhere else (13.18.64.3 rule 2).
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
      10 column 1 pic 99 source tv (n) occurs 2 times step 5 varying n from 1.
procedure division.
    open output prt. initiate rep. generate det. terminate rep. close prt.
    stop run.
