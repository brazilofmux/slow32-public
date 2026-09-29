identification division.
program-id. natscreen.
*> National screen fields (cobol ISSUES-92). A PIC N(n) field is n
*> columns, its text laid out by display width as on a report line. Keys
*> arrive as UTF-8 and are edited a character (cluster) at a time: a
*> combining mark joins the character before the cursor, and a character
*> that would not fit the field's columns or its code units is refused
*> with a beep. Here: a national VALUE; USING, End, then e and a
*> combining acute typed at the end; TO, where a wide character after
*> "abcde" would take a seventh column and is refused, then Backspace and
*> it fits; FROM; a positioned DISPLAY of a national literal and a
*> positioned ACCEPT of a national item; then plain DISPLAYs, whose
*> columns move by display width too.
*> No oracle: screens need a tty.
data division.
working-storage section.
01  nm  pic n(6) value n"東京".
01  nx  pic n(4) value n"cafe".
01  nt  pic n(6).
01  np  pic n(3).
screen section.
01  s.
    05  blank screen.
    05  line 1 column 1 value n"名前:".
    05  line 1 column 7 pic n(6) using nm.
    05  line 1 column 14 value "|".
    05  line 2 column 1 value "x:".
    05  line 2 column 7 pic n(4) from nx.
    05  line 3 column 7 pic n(6) to nt.
    05  line 3 column 14 value "|".
procedure division.
    accept s
    display n"日本" at line 5 column 1
    accept np at line 6 column 1
    display "nm=[" nm "]"
    display "nt=[" nt "]"
    display "np=[" np "]"
    stop run.
