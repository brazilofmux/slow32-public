identification division.
program-id. locking.
*> File sharing and record locking within the run unit (2023 9.1.15, 9.1.16,
*> 12.4.5.9, 12.4.5.15, Table 19; 14.7.9 RETRY; the LOCK phrases of READ,
*> WRITE, REWRITE; UNLOCK): several connectors on one physical file.
*> No oracle: GnuCOBOL's sharing checks and locks are between processes,
*> none within one run unit, so every status here is 00 there.  No gcobol.
environment division.
input-output section.
file-control.
    select f1 assign to "lk.idx" organization indexed access dynamic record key k1 file status s1
        sharing with all other lock mode is manual with lock on multiple records.
    select f2 assign to "lk.idx" organization indexed access dynamic record key k2 file status s2
        sharing with all other lock mode is manual.
    select f3 assign to "lk.idx" organization indexed access dynamic record key k3 file status s3
        lock mode is automatic.
    select f4 assign to "lk.idx" organization indexed access dynamic record key k4 file status s4
        sharing with no other.
    select f5 assign to "lk.idx" organization indexed access dynamic record key k5 file status s5
        sharing with read only.
    select f6 assign to "lk.idx" organization indexed access dynamic record key k6 file status s6
        sharing with all other.
    select ra assign to "lk.rel" organization relative access random relative key rka file status sa
        lock mode is manual with lock on multiple records.
    select rb assign to "lk.rel" organization relative access random relative key rkb file status sb
        lock mode is manual.
    select rc assign to "lk.rel" organization relative access dynamic relative key rkc file status sc
        lock mode is automatic with lock on multiple records.
    select qa assign to "lk.seq" organization sequential file status qsa
        lock mode is manual.
    select qb assign to "lk.seq" organization sequential file status qsb
        lock mode is manual.
data division.
file section.
fd f1.
01 r1.
   05 k1 pic x(2).
   05 d1 pic x(6).
fd f2.
01 r2.
   05 k2 pic x(2).
   05 d2 pic x(6).
fd f3.
01 r3.
   05 k3 pic x(2).
   05 d3 pic x(6).
fd f4.
01 r4.
   05 k4 pic x(2).
   05 d4 pic x(6).
fd f5.
01 r5.
   05 k5 pic x(2).
   05 d5 pic x(6).
fd f6.
01 r6.
   05 k6 pic x(2).
   05 d6 pic x(6).
fd ra.
01 rra pic x(4).
fd rb.
01 rrb pic x(4).
fd rc.
01 rrc pic x(4).
fd qa.
01 rqa pic x(4).
fd qb.
01 rqb pic x(4).
working-storage section.
01 s1 pic xx.
01 s2 pic xx.
01 s3 pic xx.
01 s4 pic xx.
01 s5 pic xx.
01 s6 pic xx.
01 sa pic xx.
01 sb pic xx.
01 sc pic xx.
01 qsa pic xx.
01 qsb pic xx.
01 rka pic 9(4).
01 rkb pic 9(4).
01 rkc pic 9(4).
01 i pic 9(4).
01 n pic 9(4).
procedure division.
    display "-- indexed: sharing modes (Table 19)".
    open output f1.
    move "01" to k1. move "first " to d1. write r1.
    move "02" to k1. move "second" to d1. write r1.
    move "03" to k1. move "third " to d1. write r1.
    close f1.
    open i-o f1. open i-o f2.
    display "open f1 " s1 " f2 " s2.
    open i-o f4.
    display "open f4 (no other) while open elsewhere: " s4.
    open output f2.
    display "open output f2 (already open): " s2.
    close f2.
    open output f2.
    display "open output f2 while f1 open: " s2.
    open i-o f2.
    display "-- indexed: manual locks, f1 on multiple records".
    move "01" to k1. read f1 with lock.
    display "f1 read 01 with lock: " s1.
    move "01" to k2. read f2.
    display "f2 read 01: " s2.
    read f2 retry 2 times.
    display "f2 read 01 retry 2 times: " s2.
    read f2 retry for 0.01 seconds.
    display "f2 read 01 retry for 0.01 seconds: " s2.
    read f2 ignoring lock.
    display "f2 read 01 ignoring lock: " s2 " [" d2 "]".
    move "02" to k2. read f2.
    display "f2 read 02: " s2.
    move "03" to k1. read f1 with lock.
    move "03" to k2. read f2.
    display "f2 read 03 (f1 holds 01 and 03): " s2.
    rewrite r2.
    display "f2 rewrite 03: " s2.
    delete f2 record.
    display "f2 delete 03: " s2.
    move "03" to k2. start f2 key is equal k2.
    display "f2 start 03 (START sees no locks): " s2.
    move "01" to k1. read f1.
    display "f1 reads its own locked 01: " s1.
    move "03" to k1. read f1 with no lock.
    display "f1 read 03 with no lock (frees 03 alone): " s1.
    move "03" to k2. read f2.
    display "f2 read 03 now: " s2.
    move "01" to k2. read f2.
    display "f2 read 01 still: " s2.
    unlock f1 records.
    display "unlock f1: " s1.
    read f2.
    display "f2 read 01 after unlock: " s2.
    display "-- indexed: f2 single record, a lock on REWRITE".
    move "02" to k2. read f2 with lock.
    move "02" to k1. read f1.
    display "f1 read 02 (f2 holds it): " s1.
    move "03" to k2. read f2 ignoring lock. rewrite r2 with lock.
    display "f2 rewrite 03 with lock: " s2.
    move "02" to k1. read f1.
    display "f1 read 02 (f2's rewrite released 02): " s1.
    move "03" to k1. read f1.
    display "f1 read 03 (f2's new lock): " s1.
    move "01" to k2. read f2.
    move "03" to k1. read f1.
    display "f1 read 03 (f2's plain read released it): " s1.
    move "01" to k2. read f2 with lock.
    close f2.
    move "01" to k1. read f1.
    display "f1 read 01 (f2 closed): " s1.
    close f1.
    display "-- indexed: automatic".
    open i-o f3. open i-o f2.
    move "02" to k3. read f3.
    display "f3 (automatic) read 02: " s3.
    move "02" to k2. read f2.
    display "f2 read 02 while f3 holds it: " s2.
    move "01" to k3. read f3.
    display "f3 read 01 (single: 02 released): " s3.
    move "02" to k2. read f2.
    display "f2 read 02 now: " s2.
    move "01" to k2. rewrite r2 retry 1 times.
    display "f2 rewrite 01 (f3 holds it): " s2.
    move "01" to k2. delete f2 record.
    display "f2 delete 01 (f3 holds it): " s2.
    close f3. close f2.
    display "-- indexed: read only, and a SHARING clause without LOCK MODE".
    open input f5.
    open i-o f1.
    display "open i-o f1 while f5 read only: " s1.
    open input f1.
    display "open input f1 while f5 read only: " s1.
    close f1. close f5.
    open i-o f6. open i-o f2.
    move "01" to k6. read f6.
    move "01" to k2. read f2 with lock.
    move "01" to k6. read f6.
    display "f6 (no LOCK MODE) reads f2's locked 01: " s6.
    close f6. close f2.
    open i-o sharing with read only f2.
    open i-o f1.
    display "open i-o f1 while f2 opened SHARING WITH READ ONLY: " s1.
    close f2.
    open i-o sharing with no other f2.
    open input f1.
    display "open input f1 while f2 opened SHARING WITH NO OTHER: " s1.
    close f2.
    display "-- relative: the record is its number".
    open output ra.
    perform varying i from 1 by 1 until i > 300
        move i to rka
        move "rec " to rra
        write rra
    end-perform.
    close ra.
    open i-o ra. open i-o rb. open i-o rc.
    move 5 to rka. read ra with lock.
    move 5 to rkb. read rb.
    display "rb read 5 (ra holds it): " sb.
    move 6 to rkb. read rb.
    display "rb read 6: " sb.
    move 5 to rkb. rewrite rrb.
    display "rb rewrite 5: " sb.
    delete rb record.
    display "rb delete 5: " sb.
    move 7 to rkb. write rrb with lock.
    display "rb write 7 (slot taken): " sb.
    move 301 to rkb. move "new " to rrb. write rrb with lock.
    display "rb write 301 with lock: " sb.
    move 301 to rka. read ra.
    display "ra read 301 (rb holds it): " sa.
    move 5 to rkc. read rc.
    display "rc (automatic, multiple) read 5 held by ra: " sc.
    move 6 to rkc. read rc.
    move 8 to rkc. read rc.
    move 6 to rkb. read rb.
    display "rb read 6 (rc holds 6 and 8): " sb.
    move 8 to rkb. read rb.
    display "rb read 8: " sb.
    unlock rc.
    move 6 to rkb. read rb.
    display "rb read 6 after rc's UNLOCK: " sb.
    display "-- relative: the limits".
    move 0 to n.
    perform varying i from 10 by 1 until i > 290
        move i to rka
        read ra with lock
        if sa not = "00" add 1 to n end-if
    end-perform.
    display "ra locking 281 more: " n " refused, last " sa.
    move 1 to rka. read ra with lock.
    display "ra lock 256th: " sa.
    close ra.
    move 10 to rkb. read rb.
    display "rb read 10 after ra closed: " sb.
    close rb. close rc.
    display "-- sequential: ADVANCING ON LOCK".
    open output qa.
    move "aaaa" to rqa. write rqa.
    move "bbbb" to rqa. write rqa.
    move "cccc" to rqa. write rqa.
    move "dddd" to rqa. write rqa.
    close qa.
    open i-o qa. open input qb.
    read qa. read qa with lock.
    display "qa holds the second: " rqa.
    read qb.
    display "qb first: " qsb " " rqb.
    read qb.
    display "qb second: " qsb.
    read qb advancing on lock.
    display "qb second advancing on lock: " qsb " " rqb.
    read qb.
    display "qb fourth: " qsb " " rqb.
    read qb.
    display "qb at end: " qsb.
    close qa. close qb.
    open extend qa.
    open input qb.
    display "open input while open EXTEND (all other): " qsb.
    close qa.
    stop run.
