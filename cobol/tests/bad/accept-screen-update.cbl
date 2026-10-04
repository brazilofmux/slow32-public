identification division.
program-id. scrupdbad.
*> ACCEPT screen-name WITH UPDATE is GnuCOBOL's (BP-G3): refused
*> without -dialect=gnucobol, naming it (cobol ISSUES-124).
data division.
working-storage section.
01 v pic x(3).
screen section.
01 s1.
   03 pic x(3) to v line 1 col 1.
procedure division.
    accept s1 with update
    stop run.
