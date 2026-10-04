*> Escape sequences the editor does not act on are swallowed whole, not
*> typed into the field: a modified cursor key (ESC [ 1 ; 5 D, Ctrl-Left;
*> ESC [ 1 ; 2 D, Shift-Left) acts as the key itself; the paste brackets
*> (ESC [ 200 ~, ESC [ 201 ~),
*> a mouse report (ESC [ < 0 ; 1 ; 1 M) and a sequence with a final byte
*> nothing knows (ESC [ 5 n) do nothing.  Before, the tail of each was
*> typed as text ("5D").  The keys come from scrkeys.keys.
*> No oracle: screens need a real tty.
identification division.
program-id. scrkeys.
data division.
working-storage section.
01  w pic x(8) value spaces.
screen section.
01  s1.
    05  line 1 column 1 value '['.
    05  line 1 column 2 pic x(8) using w.
    05  line 1 column 10 value ']'.
procedure division.
    display s1
    accept s1
    display '<' at 0301 w '>'
    stop run.
