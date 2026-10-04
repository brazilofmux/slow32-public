identification division.
program-id. crtbad.
*> COB-CRT-STATUS is GnuCOBOL's implicit CRT STATUS item (BP-G2):
*> without -dialect=gnucobol it is declared by no one, and the refusal
*> names the switch rather than only "not declared" (cobol ISSUES-124).
data division.
working-storage section.
01 v pic x(3).
procedure division.
    accept v
    if cob-crt-status = 0 display "ok" end-if
    goback.
