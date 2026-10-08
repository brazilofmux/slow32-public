identification division.
program-id. p-gnu-crt-status-three.
*> a three-byte CRT STATUS item is Micro Focus's, not GnuCOBOL's
environment division.
configuration section.
special-names.
    crt status is ws-crt.
data division.
working-storage section.
77  ws-crt pic x(3).
77  nm pic x(3).
screen section.
01  scr.
    05  line 2 column 5 pic x(3) using nm.
procedure division.
    accept scr.
    stop run.
