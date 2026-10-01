identification division.
program-id. strovf.
*> STRING's overflow is tested "before each move of a character"
*> (X3.23-1985 VI-133, STRING general rule 9; the same words in 2002
*> and 2023): a POINTER outside the receiver is the overflow only when a
*> character comes to be moved.  Found by tests/gen (string85.py);
*> GnuCOBOL and this compiler both tested the POINTER on entry
*> (docs/oracles.md).  A POINTER below 1 breaks rule 5 and is not tested.
data division.
working-storage section.
01 r pic x(6).
01 s pic x(5).
01 p pic 99.
01 ov pic x.
procedure division.
*> past the end, the only source delimited at its first character
    move "abcdef" to r  move ",xyz" to s  move 9 to p
    string s delimited by "," into r with pointer p
        on overflow move "O" to ov not on overflow move "N" to ov
    end-string
    display "1 [" r "] " p " " ov
*> past the end with a character to move: the overflow
    move "abcdef" to r  move "xyz" to s  move 7 to p
    string s delimited by "," into r with pointer p
        on overflow move "O" to ov not on overflow move "N" to ov
    end-string
    display "2 [" r "] " p " " ov
*> the receiver filled exactly, then a source with nothing to move
    move "abcdef" to r  move "xy,z" to s  move 5 to p
    string s delimited by "," s delimited by "x"
        into r with pointer p
        on overflow move "O" to ov not on overflow move "N" to ov
    end-string
    display "3 [" r "] " p " " ov
*> filled exactly, then one character more: the overflow
    move "abcdef" to r  move "xy,z" to s  move 5 to p
    string s delimited by "," "q" delimited by size
        into r with pointer p
        on overflow move "O" to ov not on overflow move "N" to ov
    end-string
    display "4 [" r "] " p " " ov
    stop run.
