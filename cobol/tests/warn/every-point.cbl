identification division.
program-id. every-point.
*> Every behavior point, once each: -warn-74 must name all ten ids,
*> and a compile without it must be silent.  docs/behavior-points.md
author. a comment-entry.
environment division.
configuration section.
object-computer. slow32 memory size 64000 characters.
input-output section.
file-control.
    select tape assign to "every-point.dat"
        organization is sequential.
data division.
file section.
fd  tape
    label records are standard
    value of file-id is "every-point.dat"
    data record is tape-rec.
01  tape-rec pic x(10).
working-storage section.
01  i pic 99.
01  j pic 99.
01  n pic 99 value 3.
01  grp.
    05 cnt pic 99.
    05 elem pic x occurs 1 to 5 depending on n.
01  src pic x(8) value "abcdefgh".
procedure division.
p0.
    perform varying i from 1 by 1 until i > 2
            after j from i by 1 until j > 3
        continue
    end-perform
    move src to grp
    open input tape reversed
    close tape
    stop "operator".
    alter p1 to proceed to p2.
p1.
    go to.
p2.
    stop run.
