*> WRITE and READ of fixed-length sequential records through the short
*> entries (libcob.c, "The short entries of READ and WRITE"): what the
*> first record of a file finds out is kept until CLOSE, and a one-byte
*> record after it is stored in, or taken from, a buffer with no call.
*> So: more one-byte records than the buffers hold, each byte checked
*> and the FILE STATUS item set back from something else by every
*> statement; the same bytes as nine-byte records (one straddles every
*> buffer, the last is short: 04); EXTEND; nine-byte records written and
*> read a byte at a time; a file with no FILE STATUS; and the same file
*> connector closed and opened the other way round, where what the last
*> OPEN found out must not outlive it -- a READ of a file open for output
*> is 47, a WRITE of one open for input 48, both in the middle of a run
*> of ordinary ones.
identification division.
program-id. seqbyte.
environment division.
input-output section.
file-control.
    select f1 assign to "tmp/seqbyte.dat" organization is sequential
        file status is st.
    select f9 assign to "tmp/seqbyte.dat" organization is sequential
        file status is st.
    select g1 assign to "tmp/seqbyte2.dat" organization is sequential.
data division.
file section.
fd  f1.
01  r1           pic x.
fd  f9.
01  r9           pic x(9).
fd  g1.
01  q1           pic x.
working-storage section.
01  st           pic xx.
01  i            pic 9(7) comp.
01  j            pic 9(7) comp.
01  k            pic 9(7) comp.
01  cnt          pic 9(7) comp.
01  bad          pic 9(7) comp.
01  badst        pic 9(7) comp.
01  want         pic x.
01  eof          pic x.
procedure division.
main-para.
    *> 10,007 bytes, one WRITE each
    move 0 to badst
    open output f1
    perform varying i from 1 by 1 until i > 10007
        perform byte-of-i
        move want to r1
        move "xx" to st
        write r1
        if st not = "00" add 1 to badst end-if
    end-perform
    close f1
    display "wrote 10007, statuses not 00: " badst " close " st
    perform read-bytes
    *> the same bytes nine at a time: 1111 records and one of eight
    move 0 to cnt bad badst
    move "n" to eof
    open input f9
    perform until eof = "y"
        move "xx" to st
        read f9
            at end move "y" to eof
            not at end
                add 1 to cnt
                if st = "04" display "short record " cnt
                    move 8 to k
                else
                    move 9 to k
                    if st not = "00" add 1 to badst end-if
                end-if
                perform varying j from 1 by 1 until j > k
                    compute i = (cnt - 1) * 9 + j
                    perform byte-of-i
                    if r9(j:1) not = want add 1 to bad end-if
                end-perform
        end-read
    end-perform
    display "nines " cnt " wrong " bad " statuses " badst " end " st
    close f9
    *> EXTEND: 4,100 more, across a buffer
    open extend f1
    perform varying i from 10008 by 1 until i > 14107
        perform byte-of-i
        move want to r1
        write r1
    end-perform
    close f1
    perform read-bytes
    *> nine-byte records out, bytes in: 1,000 records, 9,000 bytes
    open output f9
    perform varying cnt from 1 by 1 until cnt > 1000
        perform varying j from 1 by 1 until j > 9
            compute i = (cnt - 1) * 9 + j
            perform byte-of-i
            move want to r9(j:1)
        end-perform
        move "xx" to st
        write r9
        if st not = "00" display "write r9 " st end-if
    end-perform
    close f9
    perform read-bytes
    *> the connector the other way round, and the wrong statement in
    *> the middle of a run of right ones
    open output f1
    move "a" to r1 write r1
    move "b" to r1 write r1
    read f1 at end display "at end?" end-read
    display "READ, open for output: " st
    move "c" to r1 write r1
    display "and the next WRITE: " st
    close f1
    open input f1
    read f1 at end display "at end?" end-read
    read f1 at end display "at end?" end-read
    display "read " r1 " " st
    move "z" to r1
    write r1
    display "WRITE, open for input: " st
    read f1 at end display "at end?" end-read
    display "and the next READ: " r1 " " st
    read f1 at end display "the end " st end-read
    close f1
    open extend f1
    move "d" to r1 write r1
    close f1
    open input f9
    read f9 at end display "at end?" end-read
    display "four bytes of nine: " st " [" r9(1:4) "]"
    close f9
    *> no FILE STATUS item
    open output g1
    perform varying i from 1 by 1 until i > 5000
        perform byte-of-i
        move want to q1
        write q1
    end-perform
    close g1
    move 0 to cnt bad
    move "n" to eof
    open input g1
    perform until eof = "y"
        read g1
            at end move "y" to eof
            not at end
                add 1 to cnt
                move cnt to i
                perform byte-of-i
                if q1 not = want add 1 to bad end-if
        end-read
    end-perform
    close g1
    display "no status item: " cnt " wrong " bad
    stop run.

read-bytes.
    move 0 to cnt bad badst
    move "n" to eof
    open input f1
    perform until eof = "y"
        move "xx" to st
        read f1
            at end move "y" to eof
            not at end
                add 1 to cnt
                if st not = "00" add 1 to badst end-if
                move cnt to i
                perform byte-of-i
                if r1 not = want add 1 to bad end-if
        end-read
    end-perform
    display "bytes " cnt " wrong " bad " statuses " badst " end " st
    read f1 at end continue end-read
    display "past the end " st
    close f1.

byte-of-i.
    move function char(function mod(i * 7, 251) + 2) to want.
