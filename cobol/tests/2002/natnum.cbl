identification division.
program-id. natnum.
*> Numeric and numeric-edited USAGE NATIONAL (2023 13.18.66 rule 12;
*> cobol ISSUES-72): the DISPLAY form with each character a UTF-16BE code
*> unit -- digits U+0030..U+0039, a separate sign U+002B/U+002D, an
*> unseparated one the DISPLAY overpunch widened (the implementor's
*> choice, 13.18.52.4 rule 4).  Arithmetic, comparison, editing, MOVE to
*> and from national (national to numeric is valid, 14.9.25), LENGTH in
*> characters, INSPECT, a class test, UNSTRING into a numeric national
*> receiver, USAGE NATIONAL on a group, and numeric items in a national
*> group, signed ones SIGN SEPARATE (13.18.29.3 rule 3).
*> No oracle (docs/national.md).
data division.
working-storage section.
01  a        pic 9(5) usage national value 123.
01  b        pic s9(3)v99 usage national sign trailing separate.
01  c        pic s9(4) usage national value -42.
01  cx redefines c pic x(8).
01  e        pic zz,zz9.99- usage national.
01  n        pic n(6).
01  x        pic x(5).
01  k        pic 99.
01  g usage national.
    05 g1    pic 999 value 7.
    05 g2    pic n(2) value n"ab".
01  ng group-usage national.
    05 q     pic 99.
    05 r     pic s9 sign leading separate.
01  src      pic n(8) value n"12,3456".
01  bx       pic x(10).
procedure division.
main.
    display "a: " a " c: " c
    add 1 to a
    display "a + 1: " a
    compute b = a / 7
    display "b = a / 7: " b
    compute c = c * 3
    display "c * 3: " c
    if cx = x"0030003100320076" display "stored: 0 1 2 v, each two bytes" end-if
    move b to e
    display "edited: [" e "]"
    if a > 100 display "compare: a > 100" end-if
    move a to n
    display "to national: [" n "]"
    move n"00042" to a
    display "from national: " a
    move a to x
    display "to alphanumeric: [" x "]"
    display "length: " function length(a) " " function byte-length(a)
    move 0 to k
    inspect a tallying k for all zeros
    display "inspect zeros: " k
    if a is numeric display "class: numeric" end-if
    display "group usage national: " g1 " [" g2 "]"
    move 5 to q  move -3 to r
    display "national group: [" ng "]"
    unstring src delimited by n"," into q a
    display "unstring: " q " " a
    move a to bx
    if bx(1:2) = x"3033" display "moved as digits" end-if
    stop run.
