identification division.
program-id. scritembad.
*> A screen item with a PICTURE and no FROM, TO or USING is GnuCOBOL's
*> own storage (BP-G4): refused without -dialect=gnucobol, naming it
*> (cobol ISSUES-124).
data division.
screen section.
01  s1.
    03  screen-nos  pic 9  line 1 col 1.
procedure division.
    display s1
    stop run.
