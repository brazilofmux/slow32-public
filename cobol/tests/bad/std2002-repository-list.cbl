identification division.
program-id. p.
*> A user-defined-function-specifier names one function (2023 12.3.8:
*> FUNCTION name [AS literal]); only the intrinsic form lists names,
*> closed by INTRINSIC.
environment division.
configuration section.
repository.
    function twice fact.
procedure division.
    display "x"
    stop run.
end program p.
