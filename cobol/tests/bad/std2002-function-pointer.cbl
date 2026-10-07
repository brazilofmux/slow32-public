identification division.
function-id. binop is prototype.
data division.
linkage section.
01 a pic s9(5).
01 b pic s9(5).
01 r pic s9(7).
procedure division using a b returning r.
end function binop.
identification division.
function-id. neg is prototype.
data division.
linkage section.
01 a pic s9(5).
01 r pic s9(7).
procedure division using a returning r.
end function neg.

identification division.
program-id. p-function-pointer.
*> USAGE FUNCTION-POINTER is COBOL 2014 (2023 13.18.60).
environment division.
configuration section.
repository.
    function binop.
data division.
working-storage section.
01 op usage function-pointer to binop.
procedure division.
    display "x"
    stop run.
end program p-function-pointer.
