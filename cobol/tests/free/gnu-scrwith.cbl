*> A WITH phrase on DISPLAY and ACCEPT of a screen-name (BP-G6,
*> -dialect=gnucobol only): read and ignored, as GnuCOBOL ignores it --
*> the screen is painted in its own colours; UPDATE among the phrase's
*> words is still BP-G3's.  ACAS's sys002 (cobol ISSUES-124) writes
*> "display user-data at 0101 with foreground-color 2."  The keys come
*> from gnu-scrwith.keys; the ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. gnu-scrwith.
data division.
working-storage section.
01  flag   pic x     value 'N'.
screen section.
01  s1.
    03  value "["           line 1 col 1.
    03  using flag pic x    col 2.
    03  value "]"           col 3.
procedure division.
    display s1 at 0101 with foreground-color 2 highlight
    accept s1 with update foreground-color 3
    display flag at 0301
    stop run.
