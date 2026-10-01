identification division.
function-id. twice.
data division.
linkage section.
01  x        pic s9(4).
01  r        pic s9(5).
procedure division using x returning r.
    compute r = x * 2
    goback.
end function twice.

identification division.
function-id. pad.
data division.
linkage section.
01  s        pic x(3).
01  out      pic x(8).
procedure division using s returning out.
    move all "." to out
    move s to out(3:3)
    goback.
end function pad.

identification division.
program-id. userfnarith.
*> User-defined functions as the operands of ADD, SUBTRACT, MULTIPLY and
*> DIVIDE, in every format, DIVIDE's REMAINDER and SIZE ERROR included
*> (cobol ISSUES-50).  The statements are read whole before their code
*> (docs/plans/frontend-pass.md, step 4): each call is made first, in
*> the order written, then the arithmetic.
environment division.
configuration section.
repository.
    function twice.
data division.
working-storage section.
01  a        pic s9(4) value 21.
01  i        pic s9(4) value 2.
01  k        pic s9(4) value 0.
01  b        pic s9(5).
procedure division.
main.
    add twice(a) to 8 giving b
    display "add giving: " b
    add twice(a) twice(i) to b
    display "add to: " b
    subtract twice(i) from twice(a) giving b
    display "subtract giving: " b
    subtract twice(i) from b
    display "subtract from: " b
    multiply twice(i) by twice(a) giving b
    display "multiply giving: " b
    multiply twice(i) by b
    display "multiply by: " b
    divide twice(i) into twice(a) giving b remainder k
    display "divide into giving: " b " remainder " k
    divide twice(a) by twice(i) giving b remainder k
    display "divide by giving: " b " remainder " k
    divide twice(i) into b
    display "divide into: " b
    move 0 to k
    divide twice(k) into b
        on size error display "size error: twice(0) is zero"
        not on size error display "no size error"
    end-divide
    stop run.
end program userfnarith.
