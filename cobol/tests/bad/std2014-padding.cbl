identification division.
program-id. p-padding.
*> PADDING CHARACTER was removed from COBOL 2014 (E.2 item 19).
environment division.
input-output section.
file-control.
    select f assign to "p.dat" organization sequential padding character is "x".
data division.
file section.
fd f.
01 frec pic x(10).
procedure division.
    display "x"
    stop run.
