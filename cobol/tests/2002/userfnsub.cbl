identification division.
function-id. bump.
*> Counts its calls; returns the count plus its argument.
data division.
working-storage section.
01  n        pic 9(4) value 0.
linkage section.
01  x        pic s9(4).
01  r        pic s9(5).
procedure division using x returning r.
    add 1 to n
    compute r = n + x
    goback.
end function bump.

identification division.
program-id. userfnsub.
*> A user function called inside a subscript, in every place a
*> subscripted item can stand, and in an EVALUATE subject: each call
*> written is made once per execution of the statement (cobol ISSUES-121;
*> docs/plans/frontend-pass.md).  Each line shows the calls its statement
*> made; a subscript here holds two.
*> - MULTIPLY into such an element, and COMPUTE with one on both sides,
*>   crashed the compiler: the call's argument was moved through the
*>   register tree the statement was being emitted from.
*> - ADD into such an element made the calls twice, once for each time
*>   its address was formed.
*> - An EVALUATE subject is evaluated once, at the beginning (2023
*>   14.9.13.4 rule 3): its calls were made again for each WHEN tested.
*>   GnuCOBOL makes them per WHEN too (.oracle-expected,
*>   docs/oracles.md).
environment division.
configuration section.
repository.
    function bump.
data division.
working-storage section.
01  b        pic s9(5).
01  c        pic s9(5).
01  s        pic x(8).
01  n        pic s9(5).
01  prev     pic s9(5) value 0.
01  d        pic s9(5).
01  tb.
    05  el   pic s9(4) occurs 4 value 0.
procedure division.
main.
    evaluate bump(0) + 0 when 990 continue when 991 continue when other continue end-evaluate
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "eval-expr-3when calls=" d
    evaluate bump(0) = 999 when true continue when false continue end-evaluate
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "eval-cond-false calls=" d
    evaluate el(bump(0) - bump(0) + 2) when 990 continue when 991 continue when other continue end-evaluate
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "eval-sub calls=" d
    add 1 to el(bump(0) - bump(0) + 2)
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "add-recv-sub calls=" d
    move 5 to el(bump(0) - bump(0) + 2)
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "move-recv-sub calls=" d
    compute el(bump(0) - bump(0) + 2) = 7
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "compute-recv-sub calls=" d
    display "  " el(bump(0) - bump(0) + 2)
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "display-sub calls=" d
    if el(bump(0) - bump(0) + 2) = 990 continue end-if
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "if-sub calls=" d
    move el(bump(0) - bump(0) + 2) to b c
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "move-send-sub2 calls=" d
    multiply 2 by el(bump(0) - bump(0) + 2)
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "mult-recv-sub calls=" d
    compute el(bump(0) - bump(0) + 2) = el(bump(0) - bump(0) + 2) + 1
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "compute-sub-both calls=" d
    perform until el(bump(0) - bump(0) + 2) > 0 add 1 to el(1) end-perform
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "until-sub calls=" d
    display "el(1) after add, move, compute, multiply, compute: " el(1)
    move 0 to el(1)
    perform until el(bump(0) - bump(0) + 2) > 1 add 1 to el(1) end-perform
    move bump(0) to n
    compute d = n - prev - 1
    move n to prev
    display "until-sub-3-times calls=" d
    stop run.
end program userfnsub.
