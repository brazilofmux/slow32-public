identification division.
function-id. twice.
*> An identifier passed BY REFERENCE must be described as the parameter
*> is (2023 14.8.2.3); a literal or an expression would be converted.
data division.
linkage section.
01  x pic 9(4).
01  r pic 9(5).
procedure division using x returning r.
    compute r = x * 2
    goback.
end function twice.
identification division.
program-id. byref.
environment division.
configuration section.
repository.
    function twice.
data division.
working-storage section.
01  a pic x(4) value "0012".
procedure division.
    display twice(a)
    stop run.
end program byref.
