identification division.
program-id. bitarray2.
*> Bit arrays, part two (cobol ISSUES-93). INDEXED BY on a bit array,
*> with SET, SEARCH and index-name subscripts. Reference modification
*> of an element, its positions counting bits within the element
*> (8.4.3.3.4 rule 5a), with the subscript and the start each literal
*> or computed, and a part running to the element's end. SYNCHRONIZED on
*> a bit item, which here starts it at a byte and puts what follows at
*> the next byte (the placement is the implementor's, 8.5.1.6.3). VALUE
*> on a bit group (GROUP-USAGE BIT). B-NOT of an ALL literal, still an ALL literal, each
*> position inverted. Record bytes are checked through REDEFINES.
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  tri.
    05 pr    pic 1(3) usage bit occurs 5 indexed by px.
01  rec.
    05 a     pic 1(3) usage bit value b"111".
    05 s     pic 1(2) usage bit synchronized value b"11".
    05 b     pic 1(3) usage bit value b"101".
01  rx redefines rec pic x(3).
01  grp group-usage bit value b"1100101".
    05 g1    pic 1(3) usage bit.
    05 g2    pic 1(4) usage bit.
01  gx redefines grp pic x.
01  w        pic 1(6).
01  i        pic 99.
01  k        pic 99.
procedure division.
main.
    perform varying i from 1 by 1 until i > 5
        move function boolean-of-integer(i, 3) to pr(i)
    end-perform
    set px to 3
    display "pr(px) with px = 3: " pr(px)
    set px up by 1
    display "pr(px) after SET UP BY 1: " pr(px) "  pr(px - 1): " pr(px - 1)
    set px to 1
    search pr at end display "search: not found"
        when pr(px) = b"101" display "search: 101 at " px
    end-search
    display "pr(5)(2:2): " pr(5)(2:2) "  pr(4)(1:1): " pr(4)(1:1)
    move 5 to i move 2 to k
    display "pr(i)(2:2): " pr(i)(2:2) "  pr(5)(k:2): " pr(5)(k:2) "  pr(i)(k:1): " pr(i)(k:1)
    display "pr(i)(k:): " pr(i)(k:) "  pr(3)(k + 1:): " pr(3)(k + 1:)
    move b"11" to pr(2)(2:2)
    move b"0" to pr(i)(1:1)
    display "pr(2) after (2:2) gets 11: " pr(2) "  pr(5) after (1:1) gets 0: " pr(5)
    set px to 2
    move b"1" to pr(px)(k - 1:1)
    display "pr(px)(k - 1:1) gets 1: " pr(2) "  pr(1) and pr(3) untouched: " pr(1) " " pr(3)
    if rx = x"E0C0A0" display "synchronized: E0 C0 A0" else display "synchronized: wrong" end-if
    if gx = x"CA" display "bit group VALUE 1100101: CA" else display "bit group VALUE: wrong" end-if
    display "g1: " g1 "  g2: " g2
    move b"101101" to w
    compute w = w b-and b-not all b"10"
    display "w b-and b-not all b'10': " w
    move zero to w
    compute w = b-not all b"011" b-or w
    display "b-not all b'011' b-or 000000: " w
    stop run.
