identification division.
program-id. lockkey.
*> An indexed record lock is the whole primary key, up to 255 bytes
*> (the tree's BT_KEYMAX), so two keys that share a 60-byte prefix are
*> distinct records.  No oracle: GnuCOBOL's locks are between processes,
*> none within one run unit.  No gcobol.
environment division.
input-output section.
file-control.
    select f1 assign to "lk80.idx" organization indexed access dynamic
        record key k1 file status s1
        lock mode is automatic.
    select f2 assign to "lk80.idx" organization indexed access dynamic
        record key k2 file status s2
        lock mode is automatic.
data division.
file section.
fd f1.
01 r1.
   05 k1 pic x(80).
   05 d1 pic x(4).
fd f2.
01 r2.
   05 k2 pic x(80).
   05 d2 pic x(4).
working-storage section.
01 s1 pic xx.
01 s2 pic xx.
procedure division.
    open output f1
    move all "A" to k1
    move "1111" to k1(61:4)
    move "aaaa" to d1
    write r1
    move all "A" to k1
    move "2222" to k1(61:4)
    move "bbbb" to d1
    write r1
    close f1
    open i-o f1
    open i-o f2
    move all "A" to k1
    move "1111" to k1(61:4)
    read f1
    display "f1 locked 1111: " s1
    move all "A" to k2
    move "2222" to k2(61:4)
    read f2
    display "f2 read 2222 (distinct past 60): " s2 " [" d2 "]"
    move all "A" to k2
    move "1111" to k2(61:4)
    read f2
    display "f2 read 1111 (f1 holds it): " s2
    close f1
    close f2
    stop run.
