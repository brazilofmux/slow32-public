identification division.
program-id. fnrefmod.
*> Reference modification of an alphanumeric function's result
*> (X3.23a-1989, the reference-modifier format; cobol ISSUES-54):
*> FUNCTION name [(arguments)] (leftmost:[length]).  The clock is fixed
*> (fnrefmod.env).  Found with it: DISPLAY FUNCTION REVERSE was taken for
*> RM/COBOL's REVERSE video attribute, a positioned DISPLAY.
data division.
working-storage section.
01  w        pic x(10).
01  n        pic 9(4).
procedure division.
main.
    display "date " function current-date(1:8)
    display "time " function current-date(9:6)
    move function upper-case("abcdef")(2:3) to w
    display "upper-case(abcdef)(2:3) [" w "]"
    display "reverse(abcde)(3:) [" function reverse("abcde")(3:) "]"
    move function length(function current-date(1:4)) to n
    display "length of current-date(1:4) = " n
    if function current-date(1:4) = "2026" display "the year compares" end-if
    move function ord(function upper-case("xyz")(2:1)) to n
    display "ord(of the second letter) = " n
    stop run.
end program fnrefmod.
