identification division.
program-id. scr.
*> A national FROM item is shown through a national screen field; PIC X
*> would be a national-to-alphanumeric MOVE.
data division.
working-storage section.
01  n pic n(4) value n"東京".
screen section.
01  s.
    05 line 1 col 1 pic x(8) from n.
procedure division.
    display s
    stop run.
