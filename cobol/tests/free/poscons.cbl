*> A plain ACCEPT once the terminal is in use (after a positioned
*> DISPLAY): the line is read a key at a time from the terminal and
*> echoed where the console's output stands, Backspace taking back a
*> character -- not by stdio, whose read-ahead took the keys the next
*> screen ACCEPT was owed (and which never saw a line end on a real
*> terminal in raw mode).  The keys come from poscons.keys.
*> No oracle: positioned I/O needs a real tty.
identification division.
program-id. poscons.
data division.
working-storage section.
01  name-in pic x(10).
01  code-in pic x(3).
procedure division.
    display 'Name?' line 1 position 1
    accept name-in
    accept code-in line 3 position 1
    display '[' line 5 position 1 name-in '][' code-in ']'
    stop run.
