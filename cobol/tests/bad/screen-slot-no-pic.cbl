*> A USING screen item with no PICTURE is GnuCOBOL's (BP-G5): 2023
*> 13.17.3 rule 7 wants the PICTURE written, and Micro Focus's screen
*> PICTURE goes with FROM, TO or USING.  Refused without -dialect=gnucobol.
identification division.
program-id. screen-slot-no-pic.
data division.
working-storage section.
01  flag pic x value 'N'.
screen section.
01  s1.
    03  using flag line 1 col 2.
procedure division.
    display s1
    stop run.
