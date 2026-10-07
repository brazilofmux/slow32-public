identification division.
function-id. sq.
*> OMITTED stands for an OPTIONAL parameter only (2023 14.8.2.1).
data division.
linkage section.
01  n pic 9(3).
01  r pic 9(6).
procedure division using n returning r.
    compute r = n * n
    goback.
end function sq.
identification division.
program-id. p.
environment division.
configuration section.
repository.
    function sq.
procedure division.
    display sq(omitted)
    stop run.
end program p.
