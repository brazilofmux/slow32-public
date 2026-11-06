*> ACCEPT with a zero in its place, and AT rrrccc (Micro Focus's ACCEPT
*> rules 26 and 27).  The field stays where it started while keys come, and
*> what follows goes on from it: a repaint is not a new statement.  RM's
*> prompt and ACCEPT LINE 0 POSITION 0 is the corpus's own form.  The keys
*> come from posaccat.keys.  No oracle: positioned I/O needs a real tty.
identification division.
program-id. posaccat.
data division.
working-storage section.
01 a pic x(3).
01 z4 pic 9(4) value 0.
01 v6 pic 9(6) value 012030.
screen section.
01 s1.
   05 pic x(3) to a line 2 column 3.
procedure division.
    display "Yes?" line 1 position 1
    accept a line 0 position 0
    display "<" a ">" at 0000
    accept a at 0300
    accept a at 0007
    accept a at z4
    accept a at v6
    display "n" at 0000
    accept s1 at 0000
    display "p" at 0000
    stop run.
end program posaccat.
