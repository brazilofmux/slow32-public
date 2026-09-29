identification division.
program-id. scr.
*> A national screen field's input goes into a national item; an
*> alphanumeric TO item would be a national-to-alphanumeric MOVE.
data division.
working-storage section.
01  x pic x(8).
screen section.
01  s.
    05 line 1 col 1 pic n(4) to x.
procedure division.
    accept s
    stop run.
