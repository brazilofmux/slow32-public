*> COLUMN without LINE: AT COLUMN n is the standard's own form, and an
*> omitted LINE is line 1 (2023 14.9.11.4 rule 15); COLUMN before LINE;
*> COL as a reserved word, and as a data item in a program that declares
*> one.  A first paint at home after plain output still moves the cursor
*> there.  The ANSI stream is the expected output.  No oracle: GnuCOBOL's
*> screens need a real tty.
identification division.
program-id. poscolumn.
data division.
working-storage section.
01 r pic 9(4) value 1203.
screen section.
01 scr.
   05 value "S" line 2 column 1.
procedure division.
    display "before"
    display "a" at 0101
    display "b" at column 9
    display "c" at column 3 line 6
    display "d" at col 5 line 10
    display "e" col 7
    display "f" column number 8 line number 7
    display "g" at r
    display scr at column 4
    stop run.
end program poscolumn.
