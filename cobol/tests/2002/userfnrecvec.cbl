identification division.
function-id. idx.
*> returns its argument: a subscript that depends on an item's value now
data division.
linkage section.
01  x        pic 9(2).
01  r        pic 9(2).
procedure division using x returning r.
    move x to r
    goback.
end function idx.

identification division.
program-id. userfnrecvec.
*> As 2002/userfnrecv, with EC-DATA-INCOMPATIBLE checked.  A receiver that
*> is summed too (ADD a TO b) has its content checked, and the check was
*> made before the arithmetic -- which identified the receiver early, so
*> a function in its subscript was called before the statement's earlier
*> stores.  The check now waits, with the calls, for the receiver's access
*> (cobol ISSUES-121).  The last ADD's receiver holds "1a3": the check is
*> still made, there, and the condition raised.
*> No oracle (the exception machinery is GnuCOBOL's own).
environment division.
configuration section.
repository.
    function idx.
data division.
working-storage section.
01  n        pic 9(2) value 1.
01  tb.
    05  el   pic 9(3) occurs 5 value 0.
01  raw      redefines tb.
    05  rw   pic x(3) occurs 5.
procedure division.
declaratives.
dx section.
    use after exception condition ec-data-incompatible.
d1.
    display "declarative: " function exception-status.
end declaratives.
main section.
m1.
>>TURN EC-DATA-INCOMPATIBLE CHECKING ON
    move 1 to n move zeros to tb
    add 2 to n el(function idx(n))
    display "add: " el(1) " " el(2) " " el(3)
    move 5 to n move zeros to tb
    subtract 2 from n el(function idx(n))
    move 9 to el(3)
    subtract 2 from n el(function idx(n) + 2)
    display "subtract: " n " " el(1) " " el(2) " " el(3)
    move 1 to n move zeros to tb
    move 4 to el(2)
    multiply 2 by n el(function idx(n))
    display "multiply: " n " " el(1) " " el(2) " " el(3)
    move 8 to n move zeros to tb
    move 8 to el(4)
    divide 2 into n el(function idx(n))
    display "divide: " n " " el(3) " " el(4)
    move 1 to n move zeros to tb
    move "1a3" to rw(2)
    add 1 to n el(function idx(n))
    display "not reached"
    stop run.
end program userfnrecvec.
