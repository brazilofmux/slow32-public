identification division.
program-id. bitarray.
*> OCCURS on USAGE BIT items and reference modification of bits at
*> computed positions (2023 8.5.1.6.3, 8.4.3.3.4 rule 5a; cobol
*> ISSUES-84).  A bit array's occurrences follow one another bit by bit:
*> twelve 1-bit flags after the byte A take 12 bits, so the record is
*> 61 FF F0 7A with VALUE B"1" on every occurrence, and 61 DF F0 7A when
*> flag 3 is cleared.  Elements of 3 bits straddle bytes; a subscript or
*> a reference modification computed at run time finds the byte holding
*> the first bit and the bit within it.
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  rec.
    05 a     pic x value "a".
    05 fl    pic 1 usage bit occurs 12 value b"1".
    05 z     pic x value "z".
01  rx redefines rec pic x(4).
01  tri.
    05 pr    pic 1(3) usage bit occurs 5.
01  w        pic 1(16) usage bit.
01  i        pic 99.
01  k        pic 99.
01  line-out pic x(16).
procedure division.
main.
    if rx = x"61FFF07A" display "value on every occurrence: 61 FF F0 7A" end-if
    move b"0" to fl(3)
    if rx = x"61DFF07A" display "fl(3) cleared: 61 DF F0 7A" end-if
    move spaces to line-out
    perform varying i from 1 by 1 until i > 12
        move fl(i) to line-out(i:1)
    end-perform
    display "fl(1..12): " line-out
    perform varying i from 1 by 1 until i > 5
        move function boolean-of-integer(i, 3) to pr(i)
    end-perform
    display "pr: " pr(1) " " pr(2) " " pr(3) " " pr(4) " " pr(5)
    move 4 to i
    if pr(i) = b"100" display "pr(i) with i = 4: 100" end-if
    display "length of an element: " function length(pr(2))
    move 7 to k
    move b"101" to w(k:3)
    display "w(7:3) receives 101, across a byte: " w
    display "w(k:3): " w(k:3) "  w(k + 1:): " w(k + 1:)
    stop run.
