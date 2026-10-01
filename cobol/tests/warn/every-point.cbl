identification division.
program-id. every-point.
*> Every behavior point, once each: -warn-74 must name all fifteen ids,
*> and a compile without it must be silent.  docs/behavior-points.md
author. a comment-entry.
environment division.
configuration section.
source-computer. slow32 with debugging mode.
object-computer. slow32 memory size 64000 characters.
input-output section.
file-control.
    select tapefile assign to "every-point.dat"
        organization is sequential.
i-o-control.
    rerun on tapefile every 100 records
    multiple file tape contains tapefile.
data division.
file section.
fd  tapefile
    label records are standard
    value of file-id is "every-point.dat"
    data record is tape-rec.
01  tape-rec pic x(10).
working-storage section.
01  i pic 99.
01  j pic 99.
01  n pic 99 value 3.
*> BP-N1: CLASS became reserved in COBOL 85; RM/COBOL payroll programs name items so
01  class pic x.
01  grp.
    05 cnt pic 99.
    05 elem pic x occurs 1 to 5 depending on cnt.
01  src pic x(8) value "abcdefgh".
01  num pic 99v99.
procedure division.
p0.
    perform nothing varying i from 1 by 1 until i > 2
            after j from i by 1 until j > 3
    move src to grp
    move all "123" to num
    open input tapefile reversed
    close tapefile
    stop "operator".
    alter p1 to proceed to p2.
p1.
    go to.
p2.
    stop run.
nothing.
    continue.
