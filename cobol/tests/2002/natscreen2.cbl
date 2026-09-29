identification division.
program-id. natscreen2.
*> Fixes from the Stage B review (cobol ISSUES-94), on the terminal.
*> N2: a national field is edited over the item's own code units, split
*> into grapheme clusters (UAX #29) however long, so a family emoji of
*> five code points survives an ACCEPT where only Enter is typed: the
*> item is unchanged.
*> N7: plain DISPLAY after positioned mode moves its column a cluster at
*> a time, as the screen does: the family emoji is two columns, so "|"
*> lands in column 3; "a ZWJ b" is two clusters (UAX #29 GB11 joins only
*> emoji after a ZWJ), two columns, "|" again in column 3.
*> No oracle: screens need a tty.
data division.
working-storage section.
01  nm  pic n(10) value n"👨‍👩‍👧x".
01  sv  pic n(10).
01  fam pic n(5) value n"👨‍👩".
screen section.
01  s.
    05  line 1 column 1 pic n(10) using nm.
procedure division.
    move nm to sv
    accept s
    if nm = sv display "unchanged" at line 2 column 1 else display "CHANGED" at line 2 column 1 end-if
    display "x" at line 3 column 1
    display fam with no advancing
    display "|"
    display n"a‍b" with no advancing
    display "|"
    stop run.
