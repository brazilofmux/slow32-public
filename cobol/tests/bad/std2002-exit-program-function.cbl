identification division.
function-id. exf.
*> EXIT PROGRAM is only in a program's procedure division (2023
*> 14.9.14.3 rule 7): a function ends with GOBACK.
data division.
linkage section.
01 r pic 9.
procedure division returning r.
    move 1 to r
    exit program
    goback.
end function exf.
