*> cmpmixed -- GitHub #29 shape (2): an unsigned DISPLAY integer compared
*> against a binary item or a literal is decoded in line and compared in a
*> register, instead of building two descriptors and calling cob_cmp.
*> The decode is (byte - '0') per digit, so like shape (1) it agrees with
*> cob_get_num on every canonical value and is undefined-input territory
*> otherwise; only canonical values are exercised here, deliberately.
*> Boundaries on both sides of each relation, both orders, and a DISPLAY
*> against a DISPLAY of a different width (which decodes both sides).
identification division.
program-id. cmpmixed.
data division.
working-storage section.
01  d2   pic 9(2)  value 42.
01  d5   pic 9(5)  value 12345.
01  d8   pic 9(8)  value 20260501.
01  d9   pic 9(9)  value 999999999.
01  z5   pic 9(5)  value 0.
01  sbin pic s9(9) comp value 20260501.
01  nbin pic s9(9) comp value -7.
01  ubin pic 9(4)  comp value 42.
procedure division.
main-procedure.
    if d8  =  sbin      display 'A ok' else display 'A BAD' end-if
    if d8 <=  sbin      display 'B ok' else display 'B BAD' end-if
    if d8  >  nbin      display 'C ok' else display 'C BAD' end-if
    if nbin <  d8       display 'D ok' else display 'D BAD' end-if
    if d2  =  ubin      display 'E ok' else display 'E BAD' end-if
    if d5  =  12345     display 'F ok' else display 'F BAD' end-if
    if d5  >  12344     display 'G ok' else display 'G BAD' end-if
    if d5  <  12346     display 'H ok' else display 'H BAD' end-if
    if d9  =  999999999 display 'I ok' else display 'I BAD' end-if
    if d9  >  d8        display 'J ok' else display 'J BAD' end-if
    if z5  =  0         display 'K ok' else display 'K BAD' end-if
    if z5  <  d2        display 'L ok' else display 'L BAD' end-if
    if d2  <  d5        display 'M ok' else display 'M BAD' end-if
    if d8 >=  d8        display 'N ok' else display 'N BAD' end-if
    if d5 not = d8      display 'O ok' else display 'O BAD' end-if
    stop run.
end program cmpmixed.
