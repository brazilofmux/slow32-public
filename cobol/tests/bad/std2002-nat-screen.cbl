identification division.
program-id. scr.
*> A national screen field (PIC N) waits for a ruling on screen columns; refused by name.
data division.
working-storage section.
01  n pic n(4) value n"東京".
screen section.
01  s.
    05 line 1 col 1 pic n(4) from n.
procedure division.
    display s
    stop run.
