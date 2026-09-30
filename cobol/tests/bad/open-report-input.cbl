identification division.
program-id. rpo.
*> A report file is opened OUTPUT or EXTEND (X3.23-1985 Report
*> Writer OPEN format; 2023 14.9.27.3 rule 1).
environment division.
input-output section.
file-control.
    select prf assign to "p.txt".
data division.
file section.
fd prf report is rp.
report section.
rd rp.
01 type detail.
   05 line plus 1 column 1 pic x(5) value "hello".
procedure division.
    open input prf
    stop run.
