identification division.
program-id. p-crt-status-numeric.
*> the CRT STATUS item is alphanumeric, four characters (12.3.7.3 rule 30); 9(4) is GnuCOBOL's and Micro Focus's
environment division.
configuration section.
special-names.
    crt status is ws-crt.
data division.
working-storage section.
77  ws-crt pic 9(4).
77  nm pic x(3).
screen section.
01  scr.
    05  line 2 column 5 pic x(3) using nm.
procedure division.
    accept scr.
    stop run.
