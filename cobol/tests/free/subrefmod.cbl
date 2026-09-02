*> subrefmod -- a reference-modified element of a table, where the element
*> is chosen by a runtime subscript AND the start position is an
*> expression: e(i)(d - 1:2).  Both halves of the address are computed at
*> run time, and emit_ref_addr holds the subscript's partial sum in r11
*> across the start expression -- which goes through emit_expr_tokens and
*> emit_push, so any lowering reachable from there must leave r11 alone.
*>
*> GitHub #29 shape (2) did not: it kept the constant ten in r11 while
*> decoding a DISPLAY integer, so the accumulator became 10 and the
*> subscript resolved to the wrong element.  It was invisible in the first
*> test written for it because that table's element size was also 10, which
*> is the whole reason the element sizes here are 7 and the expected
*> answers are spelled out.  CCVS NC122A caught it; this is the small case.
identification division.
program-id. subrefmod.
data division.
working-storage section.
01  tbl.
    05  e pic x(7) occurs 3 times.
01  i   pic 9(3) value 2.
01  d   pic 9(3) value 3.
01  n   pic 9(3) comp value 2.
01  t   pic x(2).
procedure division.
main-procedure.
    move 'AAAAAAA' to e(1)
    move 'BCDEFGH' to e(2)
    move 'ZZZZZZZ' to e(3)
    move e(i)(d - 1:2) to t
    display 'A [' t ']'
    move 1 to i
    move 4 to d
    move e(i)(d - 1:2) to t
    display 'B [' t ']'
    move 3 to i
    move 2 to d
    move e(i)(d + 1:2) to t
    display 'C [' t ']'
    move 2 to i
    move e(n)(d - 1:2) to t
    display 'D [' t ']'
    stop run.
end program subrefmod.
