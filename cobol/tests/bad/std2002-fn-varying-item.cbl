identification division.
function-id. pick.
*> A PERFORM VARYING item's subscripting is evaluated each time the item
*> is set or augmented (X3.23-1985 XVII-64, substantive change 27; 2023
*> 14.9.28.4 rule 12); a user function there would be called at every
*> step.  A
*> statement's calls are made once, so it is refused (cobol ISSUES-121).
data division.
linkage section.
01  r pic 9.
procedure division returning r.
    move 1 to r
    goback.
end function pick.
identification division.
program-id. varyitem.
environment division.
configuration section.
repository.
    function pick.
data division.
working-storage section.
01  t.
    05  i pic 9 occurs 3.
procedure division.
    perform varying i(pick()) from 1 by 1 until i(1) > 3
        display i(1)
    end-perform
    stop run.
end program varyitem.
