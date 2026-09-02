*> hotarith -- the inline integer paths GitHub #27 widened.
*> Comparisons: an unsigned four-byte COMP uses the whole 32-bit range, so
*> it is ordered by the unsigned compare, while anything that can be
*> negative must keep the signed one; both are exercised at the boundaries.
*> Stores: the picture still truncates, and an unsigned receiver still
*> takes the magnitude -- the compare-and-subtract that replaced the REM,
*> and the elided sign fixup, must not change either answer.
*> Not covered here: a four-byte unsigned item holding more than 2^31,
*> where the unsigned ordering is necessary rather than merely equivalent.
*> Every such item built from a picture holds at most 999999999, below the
*> sign bit, so only COMP-5 reaches past it -- and storing such a value is
*> itself broken today (GitHub #28), so there is no way to set one up here.
*> Re-add the case with that fix.
identification division.
program-id. hotarith.
data division.
working-storage section.
01  u9  pic 9(9)  comp value 0.
01  s9  pic s9(9) comp value 0.
01  u4  pic 9(4)  comp value 0.
01  u2  pic 9(2)  comp value 0.
procedure division.
main-procedure.
    move 999999999 to u9
    if u9 > 999999998 then display 'A ok' else display 'A BAD' end-if
    if u9 > 1000000000 then display 'B BAD' else display 'B ok' end-if
    if u9 = 999999999 then display 'C ok' else display 'C BAD' end-if
    move 0 to u9
    if u9 < 1 then display 'D ok' else display 'D BAD' end-if
    if u9 >= 0 then display 'E ok' else display 'E BAD' end-if
    move -5 to s9
    if s9 < 0 then display 'F ok' else display 'F BAD' end-if
    if s9 < u9 then display 'G ok' else display 'G BAD' end-if
    if u9 > s9 then display 'H ok' else display 'H BAD' end-if
    move 999999999 to u9
    add 1 to u9
    display 'I ' u9
    if u9 = 0 then display 'N ok' else display 'N BAD' end-if
    move 9999 to u4
    add 1 to u4
    display 'J ' u4
    if u4 = 0 then display 'O ok' else display 'O BAD' end-if
    move 98 to u2
    add 3 to u2
    display 'K ' u2
    if u2 = 1 then display 'P ok' else display 'P BAD' end-if
    move 5 to u2
    add 400 to u2
    display 'Q ' u2
    if u2 = 5 then display 'R ok' else display 'R BAD' end-if
    move 3 to u9
    subtract 5 from u9
    display 'L ' u9
    move 7 to u9
    add 4 to u9
    display 'M ' u9
    stop run.
end program hotarith.
