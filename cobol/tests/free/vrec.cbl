*> Sequential mode V: records of different lengths behind the IBM RDW
*> (docs/framing.md).  One file infers V from RECORD CONTAINS m TO n
*> and two 01s of different lengths -- the length written is the 01
*> named in the WRITE; the other uses RECORD IS VARYING ... DEPENDING
*> ON, the length written and read being the item.  Both are read back
*> and shown by the fields that were written.  The bytes on disk are
*> checked against tapemgr by the harness (vrec.tapemgr).
identification division.
program-id. vrec.
environment division.
input-output section.
file-control.
    select v-file assign to 'tmp/vrec.dat'
        organization is sequential.
    select d-file assign to 'tmp/vdep.dat'
        organization is sequential
        file status is d-status.
data division.
file section.
fd  v-file
    record contains 12 to 60 characters.
01  short-rec.
    05  s-tag   pic x.
    05  s-id    pic 9(3).
    05  s-text  pic x(8).
01  long-rec.
    05  l-tag   pic x.
    05  l-id    pic 9(3).
    05  l-text  pic x(56).
fd  d-file
    record is varying in size from 5 to 40 characters
    depending on d-len.
01  d-rec       pic x(40).
working-storage section.
01  d-status    pic xx.
01  d-len       pic 99 comp.
01  eof         pic x value 'N'.
01  n           pic 99 comp value 0.
01  i           pic 99 comp.
procedure division.
    open output v-file.
    move 'S' to s-tag. move 1 to s-id. move 'first' to s-text.
    write short-rec.
    move 'L' to l-tag. move 2 to l-id.
    move 'a long record, fifty-six characters of text in here....' to l-text.
    write long-rec.
    move 'S' to s-tag. move 3 to s-id. move 'third' to s-text.
    write short-rec.
    close v-file.
    open input v-file.
    perform until eof = 'Y'
        read v-file
            at end move 'Y' to eof
            not at end
                add 1 to n
                if s-tag = 'S'
                    display n ' short ' s-id ' [' s-text ']'
                else
                    display n ' long  ' l-id ' [' l-text ']'
                end-if
        end-read
    end-perform.
    close v-file.
    open output d-file.
    perform varying i from 1 by 1 until i > 4
        move all 'x' to d-rec
        move 'abcdefghijklmnopqrstuvwxyz0123456789' to d-rec
        compute d-len = 5 + (i - 1) * 11
        write d-rec
    end-perform.
    move 50 to d-len.
    write d-rec.
    display 'write past TO: ' d-status.
    close d-file.
    open input d-file.
    move 'N' to eof.
    perform until eof = 'Y'
        read d-file
            at end move 'Y' to eof
            not at end display d-len ' [' d-rec(1:d-len) ']'
        end-read
    end-perform.
    close d-file.
    stop run.
