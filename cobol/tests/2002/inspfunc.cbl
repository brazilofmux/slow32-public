*> INSPECT ... TALLYING of a function's value (2023 14.9.22; 8.4.3.2):
*> a function-identifier is an identifier, and TALLYING only reads its
*> subject, so it is a sending operand -- REPLACING and CONVERTING are
*> refused (8.4.3.2.3 rule 1; bad/std2002-inspfunc-*).  X-COBOL's
*> command-line parser writes INSPECT FUNCTION TRIM(args) TALLYING, which
*> met "'function' is not declared" (ISSUES 120).
identification division.
program-id. inspfunc.
data division.
working-storage section.
01 args   pic x(20) value "  -a --map x -v    ".
01 n      pic 99.
01 m      pic 99.
procedure division.
    move 0 to n
    inspect function trim(args) tallying n for all "--", "-"
    display "dashes " n
    move 0 to n m
    inspect function trim(args) tallying n for all " " m for characters
    display "spaces " n " characters " m
    move 0 to n
    inspect function reverse(args) tallying n for leading " "
    display "trailing spaces " n
    move 0 to n
    inspect function upper-case(args) tallying n for all "A" before initial "X"
    display "A before X " n
    stop run.
