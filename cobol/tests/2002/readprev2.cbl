*> READ PREVIOUS (2023 14.9.30.4 GR 21d3 and relative c): straight after
*> OPEN an indexed file is at end (status 10); on a relative file the
*> previous record is the first EXISTING one with a lower number, so a
*> missing record is passed over; after START, the record START found.  docs/conformance/io-statements.md
*> No oracle: GnuCOBOL gives status 46 after OPEN, and its relative READ
*> PREVIOUS skips existing records and stops at gaps.
identification division.
program-id. readprev2.
environment division.
input-output section.
file-control.
    select ix assign to "tmp/rq.idx" organization indexed access dynamic
        record key ik file status fs.
    select rl assign to "tmp/rq.rel" organization relative access dynamic
        relative key rn.
data division.
file section.
fd ix.
01 ir.
   05 ik pic x(2).
   05 ia pic x.
fd rl.
01 rr pic x(3).
working-storage section.
01 fs pic xx.
01 rn pic 9(2).
procedure division.
    open output ix
    move "10a" to ir write ir
    close ix
    open input ix
    read ix previous at end display "q1 at end " fs end-read
    close ix
    open output rl
    move 1 to rn move "r01" to rr write rr
    move 3 to rn move "r03" to rr write rr
    move 4 to rn move "r04" to rr write rr
    close rl
    open input rl
    move 4 to rn
    start rl key = rn
    read rl previous at end display "q2 end" not at end display "q2 " rr " " rn end-read
    read rl previous at end display "q3 end" not at end display "q3 " rr " " rn end-read
    read rl previous at end display "q4 end" not at end display "q4 " rr " " rn end-read
    read rl previous at end display "q5 end" not at end display "q5 " rr " " rn end-read
    close rl
    stop run.
