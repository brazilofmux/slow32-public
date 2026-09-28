identification division.
program-id. boolbit.
*> USAGE BIT and GROUP-USAGE BIT (2023 8.5.1.6.3, 13.18.29, 13.18.66;
*> cobol ISSUES-78).  Bit items at one level take the next bit position:
*> F1 (1 bit), F2 (3) and F3 (6) after the byte A share byte 2 and spill
*> into byte 3, whose last six bits are implicit filler; Z starts on the
*> next byte (the first bit of the first available byte).  So byte 2 is
*> 1 011 1010 = X'BA' and byte 3 is 10 000000 = X'80'.  A MOVE to one bit
*> item leaves its neighbours' bits alone.  A bit group is one boolean
*> item of its bits, its subordinates USAGE BIT by implication.  Bits
*> take part in MOVE, comparison, expressions, the boolean condition,
*> DISPLAY and the functions as the other usages do.
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  rec.
    05 a     pic x value "a".
    05 f1    pic 1 usage bit value b"1".
    05 f2    pic 1(3) usage bit value b"011".
    05 f3    pic 1(6) usage bit value b"101010".
    05 z     pic x value "z".
01  rx redefines rec pic x(4).
01  fl       pic 1(8) usage bit.
01  g group-usage bit.
    05 g1    pic 1(4).
    05 g2    pic 1(4).
01  nb       pic 1(4).
01  x        pic x(8).
procedure division.
main.
    display "f1 " f1 " f2 " f2 " f3 " f3
    if rx = x"61BA807A" display "layout: 61 BA 80 7A" end-if
    display "lengths: " function length(f3) " " function length(rec) " " function length(g)
    move b"111" to f2
    if rx = x"61FA807A" display "f2 moved, neighbours kept: 61 FA 80 7A" end-if
    compute f2 = f2 b-xor b"101"
    display "f2 xor 101: " f2
    if f3 = b"101010" display "f3 = 101010" end-if
    if f1 display "f1 is on" end-if
    move b"0" to f1
    if not f1 display "f1 is off" end-if
    display "integer-of-boolean(f3): " function integer-of-boolean(f3)
    move function boolean-of-integer(5, 3) to f2
    display "boolean-of-integer(5, 3): " f2
    move "0110" to fl
    display "from alphanumeric: " fl
    move f3 to x
    display "to alphanumeric: [" x "]"
    move b"11110000" to g
    display "bit group: " g " = " g1 " " g2
    move g2 to nb
    display "to a DISPLAY boolean: " nb
    if g1 is boolean display "is boolean" end-if
    stop run.
