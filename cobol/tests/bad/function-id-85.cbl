identification division.
function-id. f85.
*> A user-defined function is COBOL 2002: refused under -std=85.
data division.
linkage section.
01  r pic 9.
procedure division returning r.
    goback.
end function f85.
