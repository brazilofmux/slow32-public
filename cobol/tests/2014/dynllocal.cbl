identification division.
program-id. dynllocal.
*> DYNAMIC LENGTH and OCCURS DYNAMIC in LOCAL-STORAGE: each activation's
*> heap is given up on GOBACK, so a recursive CALL does not leak the outer
*> copy, and a second CALL sees a fresh item.  No oracle: GnuCOBOL 4 has
*> neither clause.  No gcobol either.
data division.
working-storage section.
01 n pic 9 value 1.
procedure division.
    move 1 to n
    call "dlrec" using n
    move 1 to n
    call "dlrec" using n
    stop run.
end program dynllocal.

identification division.
program-id. dlrec is recursive.
data division.
local-storage section.
01 s pic x dynamic length.
01 g.
   05 t occurs dynamic capacity in c.
      10 k pic 9.
01 m pic 9.
linkage section.
01 n pic 9.
procedure division using n.
    if n = 1 move "one" to s end-if
    if n = 2 move "two-two" to s end-if
    if n = 3 move "three-three-three" to s end-if
    set c to 3
    move n to k(2)
    display "n=" n " s=[" s "] len=" function length(s) " c=" c " k2=" k(2)
    if n < 3
        compute m = n + 1
        call "dlrec" using m
        display "back n=" n " s=[" s "] c=" c " k2=" k(2)
    end-if
    goback.
end program dlrec.
