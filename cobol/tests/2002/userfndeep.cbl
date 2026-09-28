identification division.
function-id. sumto.
*> Recursion 1000 deep through an expression (cobol ISSUES-50): each
*> activation's COMPUTE has its operand pending on libcob's evaluation
*> stack while the inner call runs, so that stack, and the PERFORM
*> stack, grow with the recursion instead of stopping at 32 and 256.
data division.
local-storage section.
01  m        pic s9(4).
linkage section.
01  n        pic s9(4).
01  res      pic 9(9).
procedure division using n returning res.
    if n <= 0
        move 0 to res
    else
        compute m = n - 1
        compute res = n + sumto(m)
    end-if
    goback.
end function sumto.
identification division.
program-id. userfndeep.
environment division.
configuration section.
repository.
    function sumto.
data division.
working-storage section.
01  k        pic s9(4) value 1000.
procedure division.
    display "sumto(1000) = " sumto(k)
    stop run.
end program userfndeep.
