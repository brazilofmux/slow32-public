identification division.
function-id. p-exit-function.
*> COBOL 2023 removed EXIT FUNCTION (Annex E.2 item 1; BP-R5): GOBACK.
data division.
linkage section.
01 r pic 9.
procedure division returning r.
    move 1 to r
    exit function.
end function p-exit-function.
