identification division.
function-id. sq is prototype.
*> A function definition conforms to its prototype (2023 14.8.2.3.2
*> rule 2): the same parameters, passed the same way, the same result.
data division.
linkage section.
01  n pic 9(3).
01  r pic 9(6).
procedure division using n returning r.
end function sq.
identification division.
function-id. sq.
data division.
linkage section.
01  n pic 9(4).
01  r pic 9(6).
procedure division using n returning r.
    compute r = n * n
    goback.
end function sq.
