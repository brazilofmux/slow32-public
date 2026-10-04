*> ACCEPT screen-name WITH UPDATE (BP-G3, -dialect=gnucobol only): for
*> that ACCEPT the screen's TO fields start from their items' current
*> values, as USING fields do, so Enter keeps them; a plain ACCEPT
*> starts a TO field blank.  ACAS (cobol ISSUES-124) writes it.  The
*> keys come from gnu-scrupdate.keys; the ANSI stream is the expected
*> output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. gnu-scrupdate.
data division.
working-storage section.
01  ws-in  pic x(3) value "abc".
screen section.
01  s1.
    03  value "IN:"        line 1 col 1.
    03  pic x(3) to ws-in  line 1 col 5.
procedure division.
    accept s1 with update
    display ws-in at 0301
    accept s1
    display "[" at 0401
    display ws-in at 0402
    display "]" at 0405
    stop run.
