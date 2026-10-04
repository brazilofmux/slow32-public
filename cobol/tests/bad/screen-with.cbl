*> A WITH phrase on DISPLAY of a screen-name is GnuCOBOL's (BP-G6): the
*> standard's screen formats and Micro Focus's have none.  Refused
*> without -dialect=gnucobol.
identification division.
program-id. screen-with.
data division.
working-storage section.
01  flag pic x value 'N'.
screen section.
01  s1.
    03  using flag pic x line 1 col 2.
procedure division.
    display s1 with foreground-color 2
    stop run.
