*> Subscripted and LINKAGE screen items. cell(i) is a slot whose
*> address is computed at each ACCEPT/DISPLAY (the same trick as a
*> subscripted Report Writer SOURCE); cell(2) with a literal
*> subscript stays a static address. The contained program's screen
*> reads and writes a LINKAGE item. No oracle: screens need a tty.
identification division.
program-id. screen5.
data division.
working-storage section.
77  i    pic 9 value 1.
01  tbl.
    05  cell occurs 3 pic x(4).
screen section.
01  s.
    05  blank screen.
    05  line 2 column 1 value 'ith:'.
    05  line 2 column 6 pic x(4) using cell(i).
    05  line 3 column 1 value '2nd:'.
    05  line 3 column 6 pic x(4) from cell(2).
procedure division.
    move 'aaaa' to cell(1).
    move 'bbbb' to cell(2).
    move 'cccc' to cell(3).
    accept s.
    move 3 to i.
    accept s.
    display 'cells=[' tbl ']'.
    call 'subscr' using cell(2).
    display 'cells=[' tbl ']'.
    stop run.
identification division.
program-id. subscr.
data division.
linkage section.
01  lk pic x(4).
screen section.
01  ls.
    05  line 5 column 1 value 'lk:'.
    05  line 5 column 5 pic x(4) using lk.
procedure division using lk.
    accept ls.
    exit program.
end program subscr.
end program screen5.
