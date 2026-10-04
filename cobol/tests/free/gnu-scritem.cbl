*> A screen item with a PICTURE and no FROM, TO or USING, used as its
*> own storage (BP-G4, -dialect=gnucobol only): MOVEd to as data, shown
*> by DISPLAY of its screen, filled by ACCEPT of it.  ACAS's sys002
*> (cobol ISSUES-124) numbers its setup screens this way.  The keys come
*> from gnu-scritem.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. gnu-scritem.
data division.
working-storage section.
screen section.
01  s1.
    03  value "SCREEN"             line 1 col 1.
    03  screen-nos   pic 9         line 1 col 8.
procedure division.
    move 3 to screen-nos
    display s1
    accept s1
    if screen-nos = 7 display "SEVEN" at 0301 end-if
    stop run.
