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
program-id. p-fnptr-value.
*> No VALUE with USAGE FUNCTION-POINTER (2023 13.18.63.3 rule 9).
environment division.
configuration section.
repository.
    function binop.
data division.
working-storage section.
01 op usage function-pointer to binop value null.
procedure division.
    display "x"
    stop run.
end program p-fnptr-value.
