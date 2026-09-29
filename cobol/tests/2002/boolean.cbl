identification division.
program-id. boolean.
*> Boolean data, part one (2023 8.3.3.4, 8.5.2.5, 14.6.8.6, 8.8.4.2.8,
*> 8.8.4.3; cobol ISSUES-76): PICTURE 1 in USAGE DISPLAY (one character
*> 0 or 1 a position) and USAGE NATIONAL (one national character), the
*> literals B"..." and BX"...", VALUE, MOVE -- aligned left, zero-filled
*> or truncated on the right; from alphanumeric and national, to
*> alphanumeric and national -- comparison with the shorter side
*> extended by zeros, the simple boolean condition, the BOOLEAN class
*> test, INITIALIZE, reference modification, LENGTH, and the functions
*> BOOLEAN-OF-INTEGER and INTEGER-OF-BOOLEAN.
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  f        pic 1 value b"1".
01  g        pic 1(4) value b"1100".
01  h        pic 1(6).
01  nb       pic 1(4) usage national value bx"A".
01  x        pic x(6).
01  n        pic n(4).
01  k        pic 99 value 6.
procedure division.
main.
    display "g: " g "  nb: " nb "  h: " h
    if f display "f is on" end-if
    move b"0" to f
    if not f display "f is off" end-if
    move g to h
    display "zero-filled: " h
    move b"101101" to g
    display "truncated: " g
    if g = b"1011" display "g = B'1011'" end-if
    if h not = b"1" display "110000 not = 1(00000)" end-if
    if h = bx"C" display "110000 = 1100(00)" end-if
    move g to x
    display "to alphanumeric: [" x "]"
    move g to n
    display "to national: [" n "]"
    move "0110" to g
    display "from alphanumeric: " g
    move n"0011" to g
    display "from national: " g
    move zero to h
    display "zero: " h
    move all b"10" to h
    display "all: " h
    if h is boolean display "h is boolean" end-if
    if h = all b"10" display "h = all 10" end-if
    move "01x1" to g
    if g is not boolean display "01x1 is not boolean" end-if
    initialize g
    display "initialized: " g
    initialize g replacing boolean data by b"1"
    display "replacing: " g
    display "part: " h(2:3)
    move b"111" to h(4:3)
    display "part moved: " h
    display "lengths: " function length(h) " " function length(nb) " " function byte-length(nb)
    display "boolean-of-integer(10, 6): " function boolean-of-integer(10, 6)
    display "boolean-of-integer(10, k): " function boolean-of-integer(10, k)
    display "integer-of-boolean: " function integer-of-boolean(b"1010")
        " " function integer-of-boolean(nb)
    stop run.
