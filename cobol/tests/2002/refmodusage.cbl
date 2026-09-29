identification division.
program-id. refmodusage.
*> Reference modification by usage (2023 8.4.3.3.4; cobol ISSUES-82).  A
*> USAGE NATIONAL numeric item counts characters and its part is national
*> (rule 3, 6c); a USAGE NATIONAL boolean item counts characters and its
*> part is boolean (rule 6); a USAGE BIT item counts bits (rule 5a), its
*> part bits that may start inside a byte and cross into the next.
*> No oracle (docs/boolean.md, docs/national.md).
data division.
working-storage section.
01  a        pic 9(4) usage national value 1234.
01  n        pic n(2).
01  k        pic 9 value 2.
01  b        pic 1(4) usage national value b"1010".
01  rec.
    05 x     pic x value "x".
    05 f     pic 1(6) usage bit value b"110011".
    05 y     pic x value "y".
01  rx redefines rec pic x(3).
01  fl       pic 1(12) usage bit.
procedure division.
main.
    display "a(2:2): " a(2:2) "  a(k:2): " a(k:2) "  length: " function length(a(2:2))
    move a(2:2) to n
    display "to national: [" n "]"
    move n"98" to a(3:2)
    display "a after a(3:2) receives 98: " a
    display "b(2:2): " b(2:2)
    move b"11" to b(1:2)
    display "b after b(1:2) receives 11: " b
    if b(3:1) display "b(3:1) is on" end-if
    display "f(2:3): " f(2:3)
    move b"111" to f(4:3)
    display "f after f(4:3) receives 111: " f
    if rx(2:1) = x"DC" display "the bits in place: 110111 00 = X'DC'" end-if
    move b"1111" to fl(7:4)
    display "fl(7:4) across a byte: " fl
    if fl(7:4) = b"1111" display "fl(7:4) = 1111" end-if
    stop run.
