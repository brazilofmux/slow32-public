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
program-id. userfnnest.
*> A user-defined function called inside an expression that is itself an
*> argument, a subscript, a reference modifier's start or length, or a
*> condition's operand (cobol ISSUES-50; docs/plans/frontend-pass.md).
*> Until the expression trees, twice(twice(a) + 1) passed the inner
*> call's record to the outer call -- recording the inner call overwrote
*> the outer's while it was being made -- and the result was 0, not 86;
*> a subscript like el(twice(twice(i) - 3) + 1) in a condition picked the
*> wrong element.
environment division.
configuration section.
repository.
    function twice
    function pad.
data division.
working-storage section.
01  a        pic s9(4) value 21.
01  i        pic s9(4) value 2.
01  k        pic s9(4) value 0.
01  b        pic s9(5).
01  tb.
    05  el   pic 9(3) occurs 9.
01  s        pic x(20) value "abcdefghijklmnopqrst".
procedure division.
main.
    perform varying i from 1 by 1 until i > 9
        compute el(i) = i * 11
    end-perform
    move 2 to i
    display "subscript: " el(twice(i) - 1)
    display "refmod start: " s(twice(i):3)
    display "refmod length: " s(2:twice(i))
    compute b = twice(twice(a) + 1)
    display "nested: " b
    compute b = twice(twice(a) + 1) + el(twice(i))
    display "nested plus subscript: " b
    move twice(twice(a) + 1) to b
    display "move: " b
    display "display: " twice(twice(a) + 1)
    compute b = function max(twice(twice(a) + 1), 3)
    display "intrinsic argument: " b
    if twice(a) + 1 = 43 display "condition: expression" end-if
    if el(twice(twice(i) - 3) + 1) = 33 display "condition: subscript" end-if
    if twice(twice(a) + 1) = 86 display "condition: nested" end-if
    perform until twice(twice(k) + 1) > 20
        add 1 to k
    end-perform
    display "until: " k
    evaluate true
        when twice(twice(a) + 1) = 86 display "when: nested"
        when other display "when: other"
    end-evaluate
    display "function refmod: " function upper-case(pad("abc"))(twice(i) - 1:twice(i) - 1)
    move s(twice(i):twice(i)) to s(10:4)
    display "move refmod: " s
    stop run.
end program userfnnest.
