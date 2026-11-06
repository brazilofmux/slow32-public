*> Several operands in one positioned DISPLAY: each has its own phrases
*> (cobol ISSUES-126).  One with no place, ahead of one that has, goes at
*> the cursor -- home before anything is painted -- and so do those after
*> it; LINE alone is column 1; a clause applies to its own operand.  Each
*> placement checked by hand against GnuCOBOL 3.3 and 4.0 on a terminal.
*> The ANSI stream is the expected output.  No oracle: GnuCOBOL's screens
*> need a real tty.
identification division.
program-id. posmulti.
data division.
working-storage section.
01 s pic x(3) value "abc".
procedure division.
    display "[" s "]" at line 8 column 1
    display "(" s ")" at line 9 column 5 with highlight
    display "A" at line 11 column 2 "B" "C" at line 12 column 3
    display "D" with highlight "E" at line 14 column 8
    display "F" "G" line 16
    display "H" with highlight
    stop run.
end program posmulti.
