*> Reference modification of computed length in screen I/O (docs/plans/
*> standard-queue.md item 10; each was "not implemented"): a positioned
*> ACCEPT into a part of computed length, a positioned DISPLAY of one
*> under SIZE, and SCREEN SECTION fields FROM, USING and over a part to
*> the item's end from a computed start.  The keys come from
*> posrefmod2.keys; the ANSI stream is the expected output.  No oracle:
*> GnuCOBOL's screens need a real tty.
identification division.
program-id. posrefmod2.
data division.
working-storage section.
01 s pic x(12) value "abcdefghijkl".
01 i pic 9(4) value 3.
01 n pic 9(4) value 4.
screen section.
01 scr.
   05 line 2 column 3 pic x(6) from s(i:n).
   05 line 3 column 3 pic x(6) using s(i + 1:n).
   05 line 4 column 3 pic x(4) from s(i:).
procedure division.
    accept s(i:n) at line 5 column 1
    display s(i + 1:n) at line 6 column 1
    display s(i:n) with size 8 at line 7 column 1
    display scr
    display s at line 8 column 1
    stop run.
end program posrefmod2.
