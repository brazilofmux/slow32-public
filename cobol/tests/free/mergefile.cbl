identification division.
program-id. mergefile.
*> MERGE: two and three record sequential inputs already in key order,
*> GIVING a file and an OUTPUT PROCEDURE, one ascending key and a
*> descending second key, equal keys across files kept in USING order.
*> The inputs are written by the program first.  Under S32_SORT_MEMORY
*> =4K (the .env) every input spills as presorted runs, so the merge
*> merges files rather than a sorted buffer.  COMP-3 key: default dialect.
environment division.
input-output section.
file-control.
    select f1 assign to 'tmp/merge-1.txt' organization sequential.
    select f2 assign to 'tmp/merge-2.txt' organization sequential.
    select f3 assign to 'tmp/merge-3.txt' organization sequential.
    select fo assign to 'tmp/merge-out.txt' organization sequential.
    select fb assign to 'tmp/merge-out.txt' organization sequential.
    select mf assign to 'tmp/merge-work.tmp'.
data division.
file section.
fd  f1.
01  r1              pic x(20).
fd  f2.
01  r2              pic x(20).
fd  f3.
01  r3              pic x(20).
fd  fo.
01  ro              pic x(20).
fd  fb.
01  rb              pic x(20).
sd  mf.
01  mr.
    05  mr-key      pic x(4).
    05  filler      pic x.
    05  mr-amt      pic s9(5)v99 comp-3.
    05  filler      pic x.
    05  mr-src      pic x(10).
working-storage section.
01  eof             pic x value 'n'.
01  ln              pic x(20).
01  amt-ed          pic -(5)9.99.
procedure division.
main.
    open output f1 f2 f3
    perform put1 perform put2 perform put3
    close f1 f2 f3
    display '-- merge 2 files giving'
    merge mf on ascending key mr-key on descending key mr-amt
        using f1 f2 giving fo
    perform show
    display '-- merge 3 files, output procedure'
    merge mf on ascending key mr-key on descending key mr-amt
        using f1 f2 f3 output procedure is take
    stop run.
w1. move ln to r1 write r1.
w2. move ln to r2 write r2.
w3. move ln to r3 write r3.
put1.
    move 'A001' to mr-key move 100.00 to mr-amt move 'one       ' to mr-src perform rel1
    move 'A001' to mr-key move 50.00 to mr-amt move 'one       ' to mr-src perform rel1
    move 'B002' to mr-key move 10.00 to mr-amt move 'one       ' to mr-src perform rel1
    move 'D004' to mr-key move -5.00 to mr-amt move 'one       ' to mr-src perform rel1.
put2.
    move 'A001' to mr-key move 100.00 to mr-amt move 'two       ' to mr-src perform rel2
    move 'A001' to mr-key move 75.00 to mr-amt move 'two       ' to mr-src perform rel2
    move 'C003' to mr-key move 1.00 to mr-amt move 'two       ' to mr-src perform rel2
    move 'C003' to mr-key move -1.00 to mr-amt move 'two       ' to mr-src perform rel2
    move 'E005' to mr-key move 0 to mr-amt move 'two       ' to mr-src perform rel2.
put3.
    move 'A000' to mr-key move 999.99 to mr-amt move 'three     ' to mr-src perform rel3
    move 'A001' to mr-key move 100.00 to mr-amt move 'three     ' to mr-src perform rel3
    move 'B002' to mr-key move 10.00 to mr-amt move 'three     ' to mr-src perform rel3
    move 'E005' to mr-key move 0 to mr-amt move 'three     ' to mr-src perform rel3
    move 'F006' to mr-key move -99.99 to mr-amt move 'three     ' to mr-src perform rel3.
rel1. move mr to ln perform w1.
rel2. move mr to ln perform w2.
rel3. move mr to ln perform w3.
show.
    open input fb
    move 'n' to eof
    perform until eof = 'y'
        read fb at end move 'y' to eof
            not at end move rb to mr perform line-out
        end-read
    end-perform
    close fb.
take.
    move 'n' to eof
    perform until eof = 'y'
        return mf at end move 'y' to eof
            not at end perform line-out
        end-return
    end-perform.
line-out.
    move mr-amt to amt-ed
    display mr-key ' ' amt-ed ' ' mr-src.
