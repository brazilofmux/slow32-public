*> Alphanumeric relations of two items of one length compile to memcmp
*> under the native collating sequence (slow32-dbt runs it natively): every
*> relation, a subscripted key, bytes past X'7F' ordering unsigned, and a
*> pair of different lengths, which keeps the runtime compare with its
*> space padding.
identification division.
program-id. cmpalnum.
data division.
working-storage section.
01 a      pic x(4) value "ABCD".
01 b      pic x(4) value "ABCE".
01 c      pic x(4).
01 hi     pic x(4) value all x"C3".
01 lo     pic x(4) value low-value.
01 short  pic x(2) value "AB".
01 t.
   05 e   pic x(4) occurs 3.
01 k      pic 9 value 2.
procedure division.
    move "ABCD" to c
    if a = c display "eq" else display "ne" end-if
    if a < b display "lt" else display "ge" end-if
    if b > a display "gt" else display "le" end-if
    if a not = b display "ne2" end-if
    if a <= c and a >= c display "le-ge" end-if
    if hi > a display "high byte orders after" else display "WRONG signed" end-if
    if lo < a display "low-value first" end-if
    move "ZZZZ" to e(1) move "ABCD" to e(2) move "AAAA" to e(3)
    if e(k) = a display "subscript eq" end-if
    if e(1) > e(3) display "subscript gt" end-if
    if short = "AB  " display "padded eq" end-if
    if a > short display "longer gt" end-if
    stop run.
