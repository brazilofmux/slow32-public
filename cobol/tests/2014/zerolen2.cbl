*> Zero-length items, the part GnuCOBOL 4 does not take (2023 8.5.4,
*> 7.3.23): a written length of 0 under >>REF-MOD-ZERO-LENGTH ON, national
*> and boolean parts and literals (N"" and B""), a class condition of a
*> zero-length item false (8.8.4.4.4 rule 1), a bit part of length zero
*> moved and compared; and the directive OFF: a zero length is
*> EC-BOUND-REF-MOD again when checking is on (7.3.23.3 rule 1).
*> No oracle: GnuCOBOL 4 refuses a written (3:0) and its national is unfinished.
*> docs/conformance/refmod.md
identification division.
program-id. zerolen2.
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 nn pic n(4) value n"wxyz".
01 b pic 1(8) value b"10101010".
01 n pic 9(3) value 0.
01 m pic 9(3) value 4.
01 r pic x(8).
procedure division.
declaratives.
ub section.
    use after exception condition ec-bound-ref-mod.
u1.
    display "  EC-BOUND-REF-MOD".
end declaratives.
main section.
m1.
    >>ref-mod-zero-length on
    display "[" x(3:0) "][" x(5:0) "][" x(1:0) "]"
    display "[" nn(2:n) "][" nn(2:0) "] " function length(nn(2:n))
    move nn(1:n) to nn display "[" nn "]"
    if nn(3:n) = n"" display "national: two zero-length operands are equal" end-if
    if nn(3:n) = n"  " display "national: and equal to spaces" end-if
    display "[" n"" "][" n"" & n"ab" "]"
    display "[" b(3:n) "][" b(1:0) "] " function length(b(3:n))
    if b(1:n) = b"" display "boolean: two zero-length operands are equal" end-if
    if b(1:n) = b"0" display "boolean: and equal to zero" end-if
    move b(1:n) to b display b
    move b"" to b(1:4) display b
    display "[" b"" "]"
    if x(1:n) is alphabetic display "alphabetic" else display "a zero-length item is not alphabetic" end-if
    if x(1:n) is numeric display "numeric" else display "nor numeric" end-if
    if x(1:n) is not alphabetic-upper display "not alphabetic-upper" end-if
    if b(1:n) is boolean display "boolean" else display "nor boolean" end-if
    move 2 to m
    move all "*" to r
    string x(1:m - 2) "k" x(3:n) x(m:m - 2) delimited by size into r
    display "[" r "]"
    move 4 to m
    >>ref-mod-zero-length off
    display "off: [" x(2:m) "]"
    >>turn ec-bound-ref-mod checking on
    display "[" x(2:n) "] (not expected: the run unit ends in the declarative)"
    stop run.
