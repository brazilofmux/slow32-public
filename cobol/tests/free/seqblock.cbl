*> A fixed-length sequential file read through the runtime's block
*> buffer (cob_read; docs/performance.md): records that do not divide
*> the block, so that one straddles every refill; a file read a
*> character at a time; a last record shorter than the record area (04;
*> what lies past its seven characters is undefined, and not shown),
*> then the end (10), then a READ past it (46); the file
*> opened again and read again; and EXTEND, whose records a later READ
*> must see.
identification division.
program-id. seqblock.
environment division.
input-output section.
file-control.
    select f7 assign to "seqblock.dat" organization is sequential
        file status is st.
    select f10 assign to "seqblock.dat" organization is sequential
        file status is st.
    select f1 assign to "seqblock.dat" organization is sequential
        file status is st.
data division.
file section.
fd  f7.
01  r7.
    05  r7-n     pic 9(5).
    05  r7-x     pic xx.
fd  f10.
01  r10          pic x(10).
fd  f1.
01  r1           pic x.
working-storage section.
01  st           pic xx.
01  i            pic 9(5) comp.
01  n            pic 9(5) comp value 3001.
01  cnt          pic 9(7) comp.
01  bad          pic 9(7) comp.
01  dsum         pic 9(12) comp.
01  eof          pic x.
procedure division.
    open output f7
    perform varying i from 1 by 1 until i > n
        move i to r7-n
        move "ab" to r7-x
        write r7
    end-perform
    close f7
    *> seven bytes at a time: every record checked
    perform read7
    perform read7
    *> ten at a time: 2100 records and one of seven bytes
    move 0 to cnt
    move "n" to eof
    open input f10
    perform until eof = "y"
        read f10
            at end move "y" to eof
            not at end
                add 1 to cnt
                if st not = "00" display "status " st " at " cnt " [" r10(1:7) "]" end-if
        end-read
    end-perform
    display "tens " cnt " end " st
    read f10 at end continue end-read
    display "past the end " st
    close f10
    *> one at a time
    move 0 to cnt dsum
    move "n" to eof
    open input f1
    perform until eof = "y"
        read f1
            at end move "y" to eof
            not at end
                add 1 to cnt
                if r1 is numeric add function numval(r1) to dsum end-if
        end-read
    end-perform
    close f1
    display "bytes " cnt " digit sum " dsum
    *> EXTEND, then the whole file again
    open extend f7
    move 99999 to r7-n
    move "zz" to r7-x
    write r7
    close f7
    add 1 to n
    perform read7
    stop run.
read7.
    move 0 to cnt bad
    move "n" to eof
    open input f7
    perform until eof = "y"
        read f7
            at end move "y" to eof
            not at end
                add 1 to cnt
                if cnt < 3002 and (r7-n not = cnt or r7-x not = "ab") add 1 to bad end-if
        end-read
    end-perform
    display "sevens " cnt " wrong " bad " last " r7-n r7-x " end " st
    close f7.
