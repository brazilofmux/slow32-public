identification division.
function-id. sq is prototype.
*> A prototype's procedure division is its header: no statements (2023 11.5 format 2, 14.2).
data division.
linkage section.
01  n pic 9(3).
01  r pic 9(6).
procedure division using n returning r.
    compute r = n * n
    goback.
end function sq.
