identification division.
program-id. boolreview.
*> Fixes from the Stage B review (cobol ISSUES-94), BOOLEAN and bits.
*> Record bytes are checked through REDEFINES against hex literals.
*> B1: INITIALIZE sets each bit item by MOVE, not its whole bytes, so the
*> bits beside it keep their values; the item a REDEFINES redefines is
*> initialized (only the REDEFINES item is left alone, X3.23 6.16).
*> B2, B3: INITIALIZE REPLACING BOOLEAN reaches every element of a bit
*> array, a BOOLEAN phrase may follow a NUMERIC one, and an element of a
*> bit array may be INITIALIZEd. INITIALIZE of a reference-modified item
*> sets its part as an elementary alphanumeric item.
*> B4: an ALL literal beside a reference modification of computed length
*> is repeated to that length at run time, in MOVE and comparison.
*> B5: a group with a group-level USAGE BIT clause stays alphanumeric.
*> B7: a shift count is taken whole: 2^31 - 1 shifts everything out.
*> B8: a USAGE BIT item longer than 256 bits works.
*> B9: an entry's own VALUE wins over its type's, written before TYPE.
*> B10: a group TYPE starts a byte, as a level 1 item.
*> B14: a MOVE between an alphanumeric group and a bit group copies bytes.
*> B15: MOVE ALL "1" to a boolean item.
*> B16: a shift after B-NOT takes its precedence.
*> B17: a bit item after a character REDEFINES starts the next byte.
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  r1.
    05 x1   pic x value "A".
    05 b1   pic 1(3) usage bit.
    05 b2   pic 1(3) usage bit.
    05 b3   pic 1(4) usage bit.
    05 y1   pic x value "B".
01  r1x redefines r1 pic x(4).
01  g.
    05 a    pic x(3) value "abc".
    05 ar   redefines a pic 9(3).
    05 c    pic 9(2) value 12.
01  t.
    05 tx   pic x value "A".
    05 fl   pic 1(2) usage bit occurs 4.
    05 tn   pic 9 value 5.
01  tr redefines t pic x(3).
01  d8      pic 1(8).
01  k       pic 9 value 3.
01  l       pic 9 value 4.
01  ub usage bit.
    05 ub1  pic 1(4) value b"0100".
    05 ub2  pic 1(4) value b"0001".
01  big     pic 1(300) usage bit.
01  sb      pic 1(4) value b"1011".
01  sr      pic 1(4).
01  sc      pic 9(10) value 2147483647.
01  num3 typedef pic 9(3) value 5.
01  n1 value 7 type num3.
01  flags typedef group-usage bit.
    05 f1   pic 1.
    05 f2   pic 1(3).
01  r10.
    05 lead pic 1(3) usage bit value b"111".
    05 fg   type flags.
    05 z10  pic x value "Z".
01  r10x redefines r10 pic x(3).
01  ag.
    05 ag1  pic x value "A".
    05 ag2  pic x value "B".
01  bg group-usage bit.
    05 bg1  pic 1(8).
    05 bg2  pic 1(8).
01  bgx redefines bg pic x(2).
01  b4      pic 1(4).
01  na      pic 1(4) value b"0001".
01  nb      pic 1(4) value b"0011".
01  r17.
    05 ra   pic 1(4) usage bit value b"1111".
    05 rx   redefines ra pic x.
    05 rb   pic 1(4) usage bit value b"1111".
01  r17x redefines r17 pic x(2).
01  ok      pic x(3).
procedure division.
    move all b"1" to b1 b2 b3
    initialize b2
    if r1x = x"41E3C042" move "ok" to ok else move "BAD" to ok end-if
    display "B1 initialize b2 keeps its neighbours: " ok
    initialize g
    display "B1 the redefined item is initialized: [" g "]"
    move low-values to tr
    initialize t replacing numeric data by 7 boolean data by b"10"
    if tr = x"00AA37" move "ok" to ok else move "BAD" to ok end-if
    display "B2 B3 REPLACING every element, BOOLEAN after NUMERIC: " ok
    move all b"1" to fl(1) fl(2) fl(3) fl(4)
    initialize fl(2)
    if tr = x"00CF37" move "ok" to ok else move "BAD" to ok end-if
    display "B3 INITIALIZE of one element: " ok
    move "XYZ12" to g
    initialize g(2:1)
    display "B3 INITIALIZE of a reference-modified part: [" g "]"
    move zero to d8
    move all b"1" to d8(k:l)
    display "B4 MOVE ALL B'1' to d8(k:l): " d8
    if d8(k:l) = all b"1" display "B4 d8(k:l) = ALL B'1'" end-if
    if d8(1:l) not = all b"1" display "B4 d8(1:l) not = ALL B'1'" end-if
    display "B5 a group with USAGE BIT: [" ub "] length " function length(ub)
    compute sr = sb b-shift-l sc
    display "B7 1011 shifted left 2^31 - 1: " sr
    move all b"1" to big
    move b"0" to big(300:1)
    if big(299:1) = b"1" and big(300:1) = b"0" display "B8 a 300-bit item" end-if
    display "B9 the entry's own VALUE: " n1
    move all b"1" to fg
    if r10x = x"E0F05A" move "ok" to ok else move "BAD" to ok end-if
    display "B10 a group TYPE starts a byte: " ok
    move ag to bg
    if bgx = "AB" move "ok" to ok else move "BAD" to ok end-if
    display "B14 alphanumeric group to bit group, bytes: " ok
    move bx"4344" to bg
    move bg to ag
    display "B14 bit group to alphanumeric group, bytes: [" ag "]"
    move all "1" to b4
    display "B15 MOVE ALL '1': " b4
    compute b4 = na b-or b-not nb b-shift-l 1
    display "B16 0001 B-OR B-NOT 0011 B-SHIFT-L 1: " b4
    if r17x = x"F0F0" move "ok" to ok else move "BAD" to ok end-if
    display "B17 a bit item after a REDEFINES starts a byte: " ok
    stop run.
