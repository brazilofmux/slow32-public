identification division.
function-id. twice.
*> An invocation with the wrong number of arguments (cobol ISSUES-50).
data division.
linkage section.
01  x pic 9(4).
01  r pic 9(5).
procedure division using x returning r.
    compute r = x * 2
    goback.
end function twice.
identification division.
program-id. argcount.
environment division.
configuration section.
repository.
    function twice.
procedure division.
    display twice(1 2)
    stop run.
end program argcount.
