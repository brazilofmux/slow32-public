identification division.
program-id. natreview.
*> Fixes from the Stage B review (cobol ISSUES-94).
*> N1: a binary or packed sender to a national item, or compared with
*> one, keeps every digit (the conversion buffer was sized in bytes).
*> N5: UNSTRING with no DELIMITED BY counts the receiver's character
*> positions, not its bytes, so a SIGN SEPARATE numeric USAGE NATIONAL
*> receiver takes three digits and the next receiver the next three
*> (2023 14.9.48.4 rule 11b).
*> N6: class conditions test a PIC N item's characters, not its bytes
*> (8.8.4.4.3 rules 3 and 8): a character past U+00FF is no digit, no
*> Latin letter and in no class-name's set.
*> No oracle (docs/national.md).
environment division.
configuration section.
special-names.
    class hexdig is "0" thru "9" "A" thru "F".
data division.
working-storage section.
01  b9  pic 9(9) comp value 123456789.
01  p9  pic s9(9) comp-3 value 123456789.
01  n9  pic n(9).
01  m9  pic n(9) value n"123456789".
01  s   pic n(8) value n"12345678".
01  k   pic s9(3) usage national sign trailing separate.
01  j   pic 9(3) usage national.
01  nd  pic n(3) value n"123".
01  na  pic n(3) value n"abc".
01  nu  pic n(3) value n"ABC".
01  nk  pic n(3) value n"1二3".
01  nh  pic n(4) value n"0F9A".
procedure division.
    move b9 to n9  display "binary to national: " n9
    move p9 to n9  display "packed to national: " n9
    if m9 = b9 display "national = binary: equal" else display "national = binary: NOT equal" end-if
    if m9 = p9 display "national = packed: equal" else display "national = packed: NOT equal" end-if
    unstring s into k j
    display "unstring: k=" k " j=" j
    if nd is numeric display "123 numeric" else display "123 NOT numeric" end-if
    if nk is numeric display "1二3 numeric" else display "1二3 NOT numeric" end-if
    if na is alphabetic display "abc alphabetic" else display "abc NOT alphabetic" end-if
    if na is alphabetic-lower display "abc lower" else display "abc NOT lower" end-if
    if nu is alphabetic-upper display "ABC upper" else display "ABC NOT upper" end-if
    if nh is hexdig display "0F9A hexdig" else display "0F9A NOT hexdig" end-if
    if nk is hexdig display "1二3 hexdig" else display "1二3 NOT hexdig" end-if
    stop run.
