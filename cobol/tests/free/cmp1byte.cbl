*> cmp1byte -- a one-byte alphanumeric relation is a byte load and one
*> compare (GitHub #29, ISSUES-26: the flag test was 99.7% of the batch's
*> remaining cob_cmp calls).  Every operator, item against item and
*> against a literal and the figuratives, ordering by byte value ('a' is
*> above 'Z', a space is below a digit), a subscripted element, PIC A,
*> NOT, and the shapes that must stay on the runtime path and pad with
*> spaces: a two-byte item against a one-character literal, and a
*> one-byte item against a longer literal.
identification division.
program-id. cmp1byte.
data division.
working-storage section.
01  f   pic x value 'N'.
01  g   pic x value 'Y'.
01  h   pic x value 'Y'.
01  la  pic a value 'q'.
01  z   pic x value 'Z'.
01  sp1 pic x value ' '.
01  d0  pic x value '0'.
01  two pic xx value 'Y '.
01  tbl.
    05  e occurs 3 pic x value 'Y'.
01  i   pic 9 value 2.
procedure division.
main-procedure.
    if f = 'Y' display 'A BAD' else display 'A ok' end-if
    if g = 'Y' display 'B ok' else display 'B BAD' end-if
    if f not = 'Y' display 'C ok' else display 'C BAD' end-if
    if g = h display 'D ok' else display 'D BAD' end-if
    if f = h display 'E BAD' else display 'E ok' end-if
    if f < g display 'F ok' else display 'F BAD' end-if
    if g > f display 'G ok' else display 'G BAD' end-if
    if g <= h display 'H ok' else display 'H BAD' end-if
    if g >= h display 'I ok' else display 'I BAD' end-if
    if f >= g display 'J BAD' else display 'J ok' end-if
    if la > z display 'K ok' else display 'K BAD' end-if
    if sp1 < d0 display 'L ok' else display 'L BAD' end-if
    if sp1 = space display 'M ok' else display 'M BAD' end-if
    if d0 = zero display 'N ok' else display 'N BAD' end-if
    if f = space display 'O BAD' else display 'O ok' end-if
    if f < high-value display 'P ok' else display 'P BAD' end-if
    if f > low-value display 'Q ok' else display 'Q BAD' end-if
    if 'Y' = g display 'R ok' else display 'R BAD' end-if
    if e (i) = 'Y' display 'S ok' else display 'S BAD' end-if
    move 'N' to e (2)
    if e (i) = 'Y' display 'T BAD' else display 'T ok' end-if
    if e (1) = e (3) display 'U ok' else display 'U BAD' end-if
    if two = 'Y' display 'V ok' else display 'V BAD' end-if
    if g = 'Y ' display 'W ok' else display 'W BAD' end-if
    if g = 'YY' display 'X BAD' else display 'X ok' end-if
    if f = all 'N' display 'Y ok' else display 'Y BAD' end-if
    if la = quote display 'Z BAD' else display 'Z ok' end-if
    stop run.
end program cmp1byte.
