identification division.
program-id. optionspara.
*> The OPTIONS paragraph (2023 11.9; standard-queue item 16): ARITHMETIC
*> IS NATIVE, DEFAULT ROUNDED MODE IS (11.9.6: the mode of a ROUNDED
*> without MODE -- NEAREST-EVEN here, so 2.5 rounds to 2 and 3.5 to 4,
*> and a MODE phrase still says its own), ENTRY-CONVENTION IS COBOL; the
*> clauses hold for a contained program unless its own OPTIONS says
*> otherwise (11.9.4: inner inherits NEAREST-EVEN, inner2 says
*> TRUNCATION).  No oracle: GnuCOBOL 4 does not take DEFAULT ROUNDED.
options.
    arithmetic is native
    default rounded mode is nearest-even
    entry-convention is cobol.
data division.
working-storage section.
01  r        pic 9(3).
01  r2       pic 9(3)v9.
procedure division.
    compute r rounded = 2.5 display "2.5 even    " r
    compute r rounded = 3.5 display "3.5 even    " r
    compute r rounded mode is nearest-away-from-zero = 2.5 display "2.5 away    " r
    compute r = 2.5 display "2.5 trunc   " r
    compute r2 rounded = 1.25 display "1.25 even   " r2
    call "inner"
    call "inner2"
    stop run.
identification division.
program-id. inner.
data division.
working-storage section.
01  r        pic 9(3).
procedure division.
    compute r rounded = 2.5 display "inner 2.5   " r
    goback.
end program inner.
identification division.
program-id. inner2.
options.
    default rounded mode is truncation.
data division.
working-storage section.
01  r        pic 9(3).
procedure division.
    compute r rounded = 2.5 display "inner2 2.5  " r
    goback.
end program inner2.
end program optionspara.
