*> A zero in AT rrcc, or in LINE and COLUMN, is said of where the last
*> DISPLAY ended (Micro Focus's general rule 16; GnuCOBOL 3.3 and 4.0 agree,
*> on a terminal): column 0 the column after it, line 0 the next line, both
*> the very next cell, a screen-name's AT as well.  Six digits are rrrccc
*> (rule 15).  RM's LINE 0 POSITION n stays on the line.  Past the edge the
*> column rolls onto the next line (16.1), and past the bottom the line is
*> the bottom one (16.2, less the scroll; GnuCOBOL leaves these two out).
*> The ANSI stream is the expected output.  No oracle: GnuCOBOL's screens
*> need a real tty.
identification division.
program-id. posat0.
data division.
working-storage section.
01 z4 pic 9(4) value 0.
01 v4 pic 9(4).
01 v6 pic 9(6) value 007020.
01 zl pic 99 value 0.
01 zc pic 99 value 0.
01 i  pic 9.
screen section.
01 mark.
   05 value "M" line 2 column 3.
procedure division.
    display "aa" at 0301
    display "bb" at 0000
    display "cc" at 0500
    display "dd" at 0007
    display "ee" at 010020
    display "ff" at v6
    display "gg" at z4
    move 0800 to v4
    display "hh" at v4
    move 0009 to v4
    display "ii" at v4
    display "j" at 1001 "k" at 0000 "l" at 0015 "m" at 1200
    display "nn" at 1301
    display "oo" line zl column zc
    display "pp" line 14 column zc
    display "qq" line zl column 30
    display "rr" line 0 column 0
    display "ss" line 0
    display "tt" line 16 position 0
    display "uu" line 0 position 40
    perform varying i from 1 by 1 until i > 2
        display "v" at z4
    end-perform
    move 1701 to v4
    display "w" at v4
    display mark at 0000
    display "x" at 0000
    display mark at 018001
    display "yyy" at 2079
    display "z" at 0000
    display "A" at 2190
    display "B" at 9901
    stop run.
end program posat0.
