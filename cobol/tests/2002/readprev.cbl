*> READ PREVIOUS (COBOL 2002; 2023 14.9.30.4 general rule 21): on an
*> indexed file, after START the record START found, after a READ the
*> record before it; NEXT and PREVIOUS mixed; an alternate key with
*> duplicates, backwards.  After OPEN, and relative files: readprev2.
*> docs/conformance/io-statements.md
identification division.
program-id. readprev.
environment division.
input-output section.
file-control.
    select ix assign to "tmp/rp.idx" organization indexed access dynamic
        record key ik alternate record key ia with duplicates.
data division.
file section.
fd ix.
01 ir.
   05 ik pic x(2).
   05 ia pic x.
working-storage section.
01 i pic 9.
01 line-out pic x(40).
procedure division.
    open output ix
    move "10a" to ir write ir
    move "20b" to ir write ir
    move "30a" to ir write ir
    move "40b" to ir write ir
    move "50a" to ir write ir
    close ix
    open input ix
    move "30" to ik
    start ix key = ik
    read ix previous at end display "p2 end" not at end display "p2 " ir end-read
    read ix previous at end display "p3 end" not at end display "p3 " ir end-read
    read ix next at end display "p4 end" not at end display "p4 " ir end-read
    read ix previous at end display "p5 end" not at end display "p5 " ir end-read
    read ix previous at end display "p6 end" not at end display "p6 " ir end-read
    read ix previous at end display "p7 end" not at end display "p7 " ir end-read
    move "99" to ik
    start ix key < ik
    move spaces to line-out
    perform varying i from 1 by 1 until i > 6
        read ix previous at end exit perform end-read
        string line-out delimited space ir delimited size " " delimited size into line-out
    end-perform
    display "p8 " line-out
    move "a" to ia
    start ix key = ia
    read ix next end-read display "p9 " ir
    read ix next end-read display "p10 " ir
    read ix previous end-read display "p11 " ir
    close ix
    stop run.
