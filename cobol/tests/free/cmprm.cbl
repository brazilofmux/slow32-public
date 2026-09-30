*> Reference-modified operands of constant length compared in line
*> (s32-cobc cmp_is_onebyte, cmp_is_rm_lit): one byte against a byte, and
*> a part against a literal of its length -- equality and ordering, a
*> group's part, a numeric item's part, a computed start.
identification division.
program-id. cmprm.
data division.
working-storage section.
01 rec.
   05 r-a  pic x(6) value "ABCDEF".
   05 r-n  pic 9(4) value 1234.
01 s    pic x(10) value "CUSTOMER-1".
01 c    pic x value "N".
01 k    pic 99 value 3.
procedure division.
    if s(1:4) = "CUST" display "eq4" end-if
    if s(1:4) not = "CUSP" display "ne4" end-if
    if s(1:4) > "CUSA" display "gt4" end-if
    if "CUSZ" > s(1:4) display "gt4r" end-if
    if s(9:1) = "-" display "eq1" end-if
    if s(10:1) < "2" display "lt1" end-if
    if s(k:1) = "S" display "k eq" end-if
    if s(k:2) = "ST" display "k eq2" end-if
    if rec(7:2) = "12" display "grp num" end-if
    if r-n(3:1) = "3" display "num part" end-if
    if rec(1:1) = c display "vs item" end-if
    if s(1:1) = c display "BAD" else display "ne item" end-if
    if s(2:3) = "UST" and s(5:1) not < "O" display "and" end-if
    stop run.
