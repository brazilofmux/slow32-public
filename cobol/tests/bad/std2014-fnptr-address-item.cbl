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
program-id. p-std2014-fnptr-address-item.
*> ADDRESS OF FUNCTION identifier: an alphanumeric or national item (2023 8.4.3.12.3 rule 1).
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
01 k pic 9(3).
procedure division.
    set op to address of function k
    stop run.
end program p-std2014-fnptr-address-item.
