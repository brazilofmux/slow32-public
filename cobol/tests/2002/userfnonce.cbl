identification division.
function-id. bump.
*> A user-defined function with a side effect: each call counts.  An
*> expression that begins with a call -- bump(0) + 1 -- in a condition,
*> an argument or an intrinsic's argument calls it once, and so does a
*> condition, IF, UNTIL or WHEN (cobol ISSUES-50; docs/plans/
*> frontend-pass.md).  Two defects made extra calls: an operand read, an
*> operator found after it, and the whole expression read again from its
*> first token; and a condition holding a call made its calls twice when
*> it was a single relation.  GnuCOBOL makes each call once, but passes
*> 0 for the expression argument (bump(bump(0) + 0) is 4 there, 7 here;
*> docs/oracles.md).
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
program-id. userfnonce.
environment division.
configuration section.
repository.
    function bump.
data division.
working-storage section.
01  b        pic s9(5).
01  k        pic 9(4) value 0.
procedure division.
main.
    if bump(0) + 0 = 1 display "condition: first call" else display "condition: not the first" end-if
    move bump(0) to b
    display "next call: " b
    move bump(bump(0) + 0) to b
    display "argument: " b
    compute b = function max(bump(0) + 0, 0)
    display "intrinsic argument: " b
    move bump(0) to b
    display "calls so far: " b
    perform until bump(0) >= 9
        add 1 to k
    end-perform
    display "until ran its body: " k " times"
    evaluate true
        when bump(0) = 10 display "when: the tenth call"
        when other display "when: not the tenth"
    end-evaluate
    move bump(0) to b
    display "calls at the end: " b
    stop run.
end program userfnonce.
