identification division.
program-id. gnu-scrrulings.
*> The same screen under -dialect=gnucobol: BLANK SCREEN clears on the
*> ACCEPT too, COLUMN PLUS n leaves n columns after the item before, the
*> CRT STATUS item PIC 9(4) -- what GnuCOBOL (and ACAS) do. No oracle: tty.
environment division.
configuration section.
special-names.
    crt status is ws-crt.
data division.
working-storage section.
77  ws-crt  pic 9(4).
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
