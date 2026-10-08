identification division.
program-id. scrrulings.
*> The three screen rulings (docs/plans/standard-queue.md item 50) as the
*> text has them: BLANK SCREEN ignored during the ACCEPT (13.18.7.3 rule 5;
*> the DISPLAY after it clears), COLUMN PLUS 1 immediately after the item
*> before (13.18.14.4 rule 15), the CRT STATUS item X(4) (12.3.7.3 rule 30).
*> No oracle: screens need a tty; GnuCOBOL counts the other way (gnu-scrrulings).
environment division.
configuration section.
special-names.
    crt status is ws-crt.
data division.
working-storage section.
77  ws-crt  pic x(4).
77  nm      pic x(3) value 'abc'.
77  qty     pic 99 value 7.
screen section.
01  scr.
    05  blank screen.
    05  line 2 column 5 value 'Name:'.
    05  column plus 1 pic x(3) using nm.
    05  column plus 3 pic 99 using qty.
procedure division.
    accept scr.
    display 'crt=' ws-crt ' nm=[' nm '] qty=' qty.
    display scr.
    stop run.
