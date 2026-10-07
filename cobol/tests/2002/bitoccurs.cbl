identification division.
program-id. bitoccurs.
*> Bit data's leftovers (docs/plans/standard-queue.md item 12): OCCURS on
*> a bit group -- a bit item of the group's bits, its occurrences at the
*> next bits (13.18.29.4 rule 1b, 8.5.1.6.3), the items in it reached
*> through the group's subscript; an arithmetic-expression subscript of
*> a bit array or of an item in an occurring bit group; and OCCURS
*> DEPENDING ON a bit array, the group's length the bits' bytes.
*> No oracle: GnuCOBOL 4 has no USAGE BIT.
data division.
working-storage section.
01  rec.
    05  hdr      pic 1(3) usage bit value b"111".
    05  flagset  group-usage bit occurs 3.
        10  on-f  pic 1 usage bit.
        10  kind  pic 1(2) usage bit.
        10  more  pic 1(2) usage bit value b"10".
    05  tail     pic 1(2) usage bit value b"01".
01  tb.
    05  arr     pic 1(3) usage bit occurs 4 value b"001".
01  i           pic 9 value 2.
01  k           pic 9 value 1.
01  n           pic 9 value 3.
01  g.
    05  hd      pic 1(3) usage bit value b"101".
    05  fl      pic 1(3) usage bit occurs 1 to 5 depending on n value b"011".
01  cnt         pic 9(2).
procedure division.
    move b"1" to on-f(1)
    move b"11" to kind(2)
    move b"1" to on-f(i + 1)
    move b"10110" to flagset(i)
    display "flagsets:  " hdr " " flagset(1) " " flagset(2) " " flagset(3) " " tail
    display "items:     " on-f(1) on-f(2) on-f(3) " " kind(i) " " more(k + 2)
    display "rec bytes: " function byte-length(rec)
    move b"110" to arr(i + k)
    display "arr:       " arr(1) arr(2) arr(3) arr(4)
    move b"110" to fl(n)
    display "odo:       " hd fl(1) fl(2) fl(3)
    move function length(g) to cnt display "length 3:  " cnt
    move 1 to n
    move function length(g) to cnt display "length 1:  " cnt
    move 5 to n
    move function length(g) to cnt display "length 5:  " cnt
    stop run.
end program bitoccurs.
