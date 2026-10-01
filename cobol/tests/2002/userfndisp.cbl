identification division.
function-id. noisy.
*> displays a line of its own, then returns its argument
data division.
linkage section.
01  x        pic x(2).
01  r        pic x(2).
procedure division using x returning r.
    display "(noisy " x ")"
    move x to r
    goback.
end function noisy.

identification division.
program-id. userfndisp.
*> A statement is its user function calls, then its code (2023 14.6.4:
*> the identifiers in a statement are evaluated as the first operation
*> of its execution).  DISPLAY wrote each operand as it read it, so a
*> function among them ran after the operands before it had been shown
*> -- and under UPON SYSERR the function's own DISPLAY went to the error
*> stream.  Each call's code is now cut out as it is made and placed
*> before the statement's (cobol ISSUES-121).
environment division.
configuration section.
repository.
    function noisy.
data division.
working-storage section.
01  a        pic x(2) value "AA".
procedure division.
main.
    display "one " function noisy("n1") " two"
    display "three " a " " function noisy("n2") " " function noisy("n3")
    display "err " function noisy("n4") " end" upon syserr
    display "no-adv " function noisy("n5") with no advancing
    display " done"
    stop run.
end program userfndisp.
