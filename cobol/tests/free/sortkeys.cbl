identification division.
program-id. sortkeys.
*> SORT keys of every usage the normalized key must order exactly as
*> cob_cmp does: signed DISPLAY with negatives and overpunch, COMP-3
*> with negatives, COMP (binary) with negatives, unsigned DISPLAY with
*> a decimal scale, alphanumeric with space padding and lower/upper
*> case, ascending and descending mixed, equal keys kept in RELEASE
*> order.  Records are released from a table, so the input order is
*> fixed and the output is byte-exact.  COMP-3 key: default dialect.
environment division.
input-output section.
file-control.
    select work-file assign to 'tmp/sortkeys-work.tmp'.
data division.
file section.
sd  work-file.
01  wr.
    05  wr-alpha     pic x(6).
    05  wr-disp      pic s9(5).
    05  wr-pack      pic s9(5)v99 comp-3.
    05  wr-bin       pic s9(8) comp.
    05  wr-udisp     pic 9(3)v9.
    05  wr-seq       pic 99.
working-storage section.
01  n            pic 99.
01  eof          pic x value 'n'.
01  out-line.
    05  o-alpha  pic x(6).
    05  filler   pic x value ' '.
    05  o-disp   pic -(5)9.
    05  filler   pic x value ' '.
    05  o-pack   pic -(5)9.99.
    05  filler   pic x value ' '.
    05  o-bin    pic -(8)9.
    05  filler   pic x value ' '.
    05  o-udisp  pic zz9.9.
    05  filler   pic x value ' '.
    05  o-seq    pic 99.
01  src.
    05  s-alpha  pic x(6) occurs 12 times.
    05  s-disp   pic s9(5) occurs 12 times.
    05  s-pack   pic s9(5)v99 occurs 12 times.
    05  s-bin    pic s9(8) occurs 12 times.
    05  s-udisp  pic 9(3)v9 occurs 12 times.
procedure division.
main.
    perform load
    display '-- alpha asc, disp desc, pack asc, bin desc, udisp asc'
    sort work-file
        on ascending key wr-alpha
        on descending key wr-disp
        on ascending key wr-pack
        on descending key wr-bin
        on ascending key wr-udisp
        with duplicates in order
        input procedure is rel
        output procedure is out
    display '-- disp asc only (ties keep release order)'
    sort work-file
        on ascending key wr-disp
        with duplicates in order
        input procedure is rel
        output procedure is out
    display '-- pack desc, alpha asc'
    sort work-file
        on descending key wr-pack
        on ascending key wr-alpha
        input procedure is rel
        output procedure is out
    display '-- bin asc, udisp desc'
    sort work-file
        on ascending key wr-bin
        on descending key wr-udisp
        input procedure is rel
        output procedure is out
    stop run.
load.
    move 'beta  ' to s-alpha(1)  move -12    to s-disp(1)  move 1.50    to s-pack(1)  move -99999999 to s-bin(1)  move 0.5   to s-udisp(1)
    move 'Alpha ' to s-alpha(2)  move 12     to s-disp(2)  move -1.50   to s-pack(2)  move 99999999  to s-bin(2)  move 999.9 to s-udisp(2)
    move 'alpha ' to s-alpha(3)  move 0      to s-disp(3)  move 0       to s-pack(3)  move 0         to s-bin(3)  move 0     to s-udisp(3)
    move 'beta  ' to s-alpha(4)  move -12    to s-disp(4)  move 1.50    to s-pack(4)  move -99999999 to s-bin(4)  move 0.5   to s-udisp(4)
    move 'gamma ' to s-alpha(5)  move -99999 to s-disp(5)  move -999.99 to s-pack(5)  move -1        to s-bin(5)  move 10.0  to s-udisp(5)
    move 'gamma ' to s-alpha(6)  move 99999  to s-disp(6)  move 999.99  to s-pack(6)  move 1         to s-bin(6)  move 10.1  to s-udisp(6)
    move 'a     ' to s-alpha(7)  move -1     to s-disp(7)  move -0.01   to s-pack(7)  move -2        to s-bin(7)  move 1.0   to s-udisp(7)
    move 'a b   ' to s-alpha(8)  move 1      to s-disp(8)  move 0.01    to s-pack(8)  move 2         to s-bin(8)  move 1.1   to s-udisp(8)
    move 'ZZZZZZ' to s-alpha(9)  move -12    to s-disp(9)  move 1.49    to s-pack(9)  move -99999998 to s-bin(9)  move 0.4   to s-udisp(9)
    move '      ' to s-alpha(10) move 12     to s-disp(10) move 1.51    to s-pack(10) move 3         to s-bin(10) move 0.6   to s-udisp(10)
    move 'beta  ' to s-alpha(11) move -12    to s-disp(11) move 1.50    to s-pack(11) move -99999999 to s-bin(11) move 0.5   to s-udisp(11)
    move '0000  ' to s-alpha(12) move -100   to s-disp(12) move 100     to s-pack(12) move -100      to s-bin(12) move 100.0 to s-udisp(12).
rel.
    perform varying n from 1 by 1 until n > 12
        move s-alpha(n) to wr-alpha
        move s-disp(n)  to wr-disp
        move s-pack(n)  to wr-pack
        move s-bin(n)   to wr-bin
        move s-udisp(n) to wr-udisp
        move n to wr-seq
        release wr
    end-perform.
out.
    move 'n' to eof
    perform until eof = 'y'
        return work-file
            at end move 'y' to eof
            not at end
                move wr-alpha to o-alpha
                move wr-disp  to o-disp
                move wr-pack  to o-pack
                move wr-bin   to o-bin
                move wr-udisp to o-udisp
                move wr-seq   to o-seq
                display out-line
        end-return
    end-perform.
