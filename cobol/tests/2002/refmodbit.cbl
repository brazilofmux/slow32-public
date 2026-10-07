identification division.
program-id. refmodbit.
*> BY CONTENT of a bit item's part (standard-queue item 10): the bits
*> moved to a boolean record on a byte boundary, which is the copy the
*> program receives.  No oracle: GnuCOBOL 4 has no USAGE BIT.
data division.
working-storage section.
01  b        pic 1(16) usage bit value b"1100101011110000".
procedure division.
    call "showbits" using by content b(3:6)
    call "showbits" using by content b(9:6)
    stop run.
end program refmodbit.
identification division.
program-id. showbits.
data division.
linkage section.
01  p        pic 1(6) usage bit.
procedure division using p.
    display "bits: " p
    goback.
end program showbits.
