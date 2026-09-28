identification division.
program-id. sortgive2.
*> Every GIVING file gets every record (cobol ISSUES-42).  The sorted
*> stream is read once, so a runtime that wrote each GIVING file in
*> turn drained it into the first and left the rest empty; CCVS-85
*> ST147A (MERGE with three GIVING files) caught it.  Here a SORT whose
*> records spill (the .env beside this source) gives two files, and a
*> MERGE of those two gives two more.  Keys are unique within a file
*> and the two sorted files are identical, so the merge order of equal
*> keys cannot change a byte.  Each file's count and checksum.
environment division.
input-output section.
file-control.
    select sw assign to 'tmp/sortgive2-sw.tmp'.
    select mw assign to 'tmp/sortgive2-mw.tmp'.
    select ga assign to 'tmp/sortgive2-a.dat' organization line sequential.
    select gb assign to 'tmp/sortgive2-b.dat' organization line sequential.
    select gc assign to 'tmp/sortgive2-c.dat' organization line sequential.
    select gd assign to 'tmp/sortgive2-d.dat' organization line sequential.
data division.
file section.
sd  sw.
01  sr.
    05  sk           pic 9(5).
    05  ss           pic 9(5).
    05  sp           pic x(10).
sd  mw.
01  mr.
    05  mk           pic 9(5).
    05  ms           pic 9(5).
    05  mp           pic x(10).
fd  ga.
01  xa               pic x(20).
fd  gb.
01  xb               pic x(20).
fd  gc.
01  xc               pic x(20).
fd  gd.
01  xd               pic x(20).
working-storage section.
01  n                pic 9(5) value 3000.
01  i                pic 9(9) comp.
01  q                pic 9(9) comp.
01  t                pic 9(12) comp.
01  rec.
    05  rk           pic 9(5).
    05  rs           pic 9(5).
    05  rp           pic x(10).
01  cnt              pic 9(9) comp.
01  ck               pic 9(9) comp.
01  firstk           pic 9(5).
01  lastk            pic 9(5).
01  eof              pic x.
01  nm               pic x(2).
01  cnt-ed           pic z(8)9.
01  ck-ed            pic z(9)9.
procedure division.
main.
    sort sw on ascending key sk
        input procedure is gen
        giving ga gb
    merge mw on ascending key mk
        using ga gb
        giving gc gd
    move 'a ' to nm
    open input ga
    perform scan-a
    close ga
    move 'b ' to nm
    open input gb
    perform scan-b
    close gb
    move 'c ' to nm
    open input gc
    perform scan-c
    close gc
    move 'd ' to nm
    open input gd
    perform scan-d
    close gd
    stop run.
gen.
    perform varying i from 1 by 1 until i > n
        compute t = i * 7919
        divide t by 10007 giving q remainder sk
        move i to ss
        move all '-' to sp
        release sr
    end-perform.
scan-a.
    perform start-scan
    perform until eof = 'y'
        read ga into rec
            at end move 'y' to eof
            not at end perform take
        end-read
    end-perform
    perform report-scan.
scan-b.
    perform start-scan
    perform until eof = 'y'
        read gb into rec
            at end move 'y' to eof
            not at end perform take
        end-read
    end-perform
    perform report-scan.
scan-c.
    perform start-scan
    perform until eof = 'y'
        read gc into rec
            at end move 'y' to eof
            not at end perform take
        end-read
    end-perform
    perform report-scan.
scan-d.
    perform start-scan
    perform until eof = 'y'
        read gd into rec
            at end move 'y' to eof
            not at end perform take
        end-read
    end-perform
    perform report-scan.
start-scan.
    move 'n' to eof
    move 0 to cnt ck firstk lastk.
take.
    add 1 to cnt
    if cnt = 1 move rk to firstk end-if
    move rk to lastk
    compute t = ck * 31 + rs
    divide t by 1000000007 giving q remainder ck.
report-scan.
    move cnt to cnt-ed
    move ck to ck-ed
    display nm ' count=' cnt-ed ' checksum=' ck-ed
        ' first=' firstk ' last=' lastk.
