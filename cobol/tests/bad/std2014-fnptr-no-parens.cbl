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
program-id. p-std2014-fnptr-no-parens.
*> A function-pointer is invoked with parentheses (2023 8.4.3.2.3 rule 5).
environment division.
configuration section.
repository.
    function binop
    function neg.
data division.
working-storage section.
01 op usage function-pointer to binop.
01 np usage function-pointer to neg.
01 name pic x(10).

procedure division.
    display op
    stop run.
end program p-std2014-fnptr-no-parens.
