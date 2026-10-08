*> WRITE FILE and REWRITE FILE (2023 14.9.51, 14.9.35 format 2; docs/plans/
*> standard-queue.md item 40): the record written is the sending item's
*> implicit record (GR 8) -- on a RECORD VARYING file its length is the
*> item's, the literal's, or a function result's; a record of the file
*> itself is written as itself (GR 7); on a fixed-length file the record
*> area, padded by the move. A second FD with DEPENDING ON reads the
*> lengths back. GnuCOBOL has no FILE phrase: no oracle.
identification division.
program-id. writefile.
environment division.
input-output section.
file-control.
    select vf assign to "writefile-v.dat" organization sequential file status fs.
    select vf2 assign to "writefile-v.dat" organization sequential file status fs.
    select relf assign to "writefile-r.dat" organization relative access random relative key rk file status fs.
data division.
file section.
fd vf record is varying in size from 1 to 20.
01 vf-rec pic x(20).
fd vf2 record is varying in size from 1 to 20 depending on rlen.
01 vf2-rec pic x(20).
fd relf record contains 8 characters.
01 relf-rec pic x(8).
working-storage section.
01 fs pic xx.
01 rk pic 9(2).
01 rlen pic 99.
01 five pic x(5) value "abcde".
01 grp.
   05 g1 pic x(3) value "xyz".
   05 g2 pic 9(4) value 1234.
01 n pic 9(4) value 42.
procedure division.
    open output vf.
    write file vf from five.
    write file vf from "twelve chars".
    write file vf from grp.
    write file vf from function trim("  trimmed  ").
    write file vf from vf-rec.
    close vf.
    open input vf2.
    perform until fs not = "00"
        read vf2
        if fs = "00" display "len " rlen " [" vf2-rec(1:rlen) "]" end-if
    end-perform.
    close vf2.
    open output relf.
    move 1 to rk. write file relf from "rec one!".
    move 2 to rk. write file relf from five.
    close relf.
    open i-o relf.
    move 1 to rk. rewrite file relf from "REWRITE!".
    move 2 to rk. read relf. display "[" relf-rec "]".
    move 1 to rk. read relf. display "[" relf-rec "]".
    close relf.
    stop run.
