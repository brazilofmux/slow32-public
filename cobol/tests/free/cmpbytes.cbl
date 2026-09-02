*> A relation between two items of one unsigned DISPLAY descriptor is a
*> byte compare (GitHub #29).  For every value the 1985 text defines this
*> is invisible: the text compares the algebraic value, and for canonical
*> fields of one descriptor byte order and algebraic order coincide.
*>
*> The cases below are the ones where it shows -- a numeric item holding
*> non-digits, which the text does not define.  Three compilers give three
*> readings there (measured 2026-09-02): GnuCOBOL 4.0 compares the bytes,
*> gcobol 15.3.0 and s32-cobc-before-#29 decode and compare the values.
*> We take the byte compare because it is exact on every defined value and
*> far cheaper, not because GnuCOBOL does it -- gcobol's decode is the
*> literal reading of the text, and agreeing with GnuCOBOL here is a side
*> effect rather than a warrant.  So the A/B/C rows below record a CHOICE
*> on undefined input, not a rule.  They are pinned so that changing it is
*> visible rather than silent; D through I are canonical values, where all
*> three implementations agree and we must not drift.  No .oracle-expected
*> is needed only because the harness oracle happens to land the same way.
identification division.
program-id. cmpbytes.
data division.
working-storage section.
01  raw   pic x(4) value '  12'.
01  a redefines raw pic 9(4).
01  b     pic 9(4) value 12.
01  raw2  pic x(4) value '00 5'.
01  c redefines raw2 pic 9(4).
01  d     pic 9(4) value 5.
*> canonical values: every implementation agrees, and so must we
01  p     pic 9(6) value 123456.
01  q     pic 9(6) value 123456.
01  r     pic 9(6) value 123457.
01  s2    pic 9(3)v99 value 12.34.
01  t2    pic 9(3)v99 value 12.35.
procedure division.
main-procedure.
    if a = b display 'A equal' else display 'A differs' end-if
    if a < b display 'B less'  else display 'B not-less' end-if
    if c = d display 'C equal' else display 'C differs' end-if
    if p = q display 'D equal' else display 'D differs' end-if
    if p < r display 'E less'  else display 'E not-less' end-if
    if r > p display 'F greater' else display 'F not-greater' end-if
    if p >= q display 'G ge' else display 'G not-ge' end-if
    if s2 < t2 display 'H less' else display 'H not-less' end-if
    if s2 = t2 display 'I equal' else display 'I differs' end-if
    stop run.
end program cmpbytes.
