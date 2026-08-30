*> SPECIAL-NAMES CRT STATUS IS: the ACCEPT's ending in GnuCOBOL's
*> numbering -- Enter 0000; F3 1003 and PgDn 2002 end the ACCEPT
*> with the fields kept; Escape 2005 abandons (the item unchanged).
*> The last ACCEPT types a character after a lone Escape to prove
*> the escape reader's pushback. No oracle: screens need a tty.
identification division.
program-id. screen3.
environment division.
configuration section.
special-names.
    crt status is ws-crt.
data division.
working-storage section.
77  ws-crt  pic 9(4) value 9999.
77  fld     pic x(3) value 'abc'.
screen section.
01  sc.
    05  blank screen.
    05  line 2 column 5 value 'F:'.
    05  line 2 column 8 pic x(3) using fld.
procedure division.
    accept sc.
    display 'crt=' ws-crt ' fld=[' fld ']'.
    accept sc.
    display 'crt=' ws-crt ' fld=[' fld ']'.
    accept sc.
    display 'crt=' ws-crt ' fld=[' fld ']'.
    accept sc.
    display 'crt=' ws-crt ' fld=[' fld ']'.
    stop run.
end program screen3.
