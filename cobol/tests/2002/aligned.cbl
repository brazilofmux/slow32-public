identification division.
program-id. aligned.
*> ALIGNED (2023 13.18.1; standard-queue item 11): a bit item on the
*> first bit of the next byte, what follows it at the next bit, each
*> occurrence of an aligned bit array on a byte (general rules 1-2).
*> a takes bits 0-2 of byte 0; b, aligned, byte 1's bits 0-2; c follows
*> at bits 3-4; d's three occurrences take bytes 2, 3 and 4; e follows
*> d(3) at byte 4's bit 3: five bytes.  No oracle: GnuCOBOL 4 has no
*> USAGE BIT.
data division.
working-storage section.
01  rec.
    05  a        pic 1(3) usage bit value b"101".
    05  b        pic 1(3) usage bit aligned value b"111".
    05  c        pic 1(2) usage bit value b"01".
    05  d        pic 1(3) usage bit aligned occurs 3.
    05  e        pic 1 usage bit value b"1".
01  k            pic 9 value 2.
procedure division.
    move b"110" to d(1)
    move b"011" to d(2)
    move b"100" to d(k + 1)
    display a " " b " " c " " d(1) d(2) d(3) " " e
    display function byte-length(rec)
    stop run.
end program aligned.
