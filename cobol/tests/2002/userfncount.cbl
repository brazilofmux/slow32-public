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
function-id. pad.
data division.
linkage section.
01  v        pic s9(5).
01  out      pic x(3).
procedure division using v returning out.
    move "ab" to out
    goback.
end function pad.

identification division.
program-id. userfncount.
*> A user function with a side effect in each statement that takes an
*> operand: every call written is made once, and the count shown after
*> each statement says so (cobol ISSUES-50, -121).  The front-end pass
*> reads statements as scans and makes their calls afterwards
*> (docs/plans/frontend-pass.md); tests/asm-snapshot.sh cannot check
*> that, the corpus hardly calling user functions, so this does --
*> GnuCOBOL counts the same.
environment division.
configuration section.
repository.
    function bump
    function pad.
data division.
working-storage section.
01  b        pic s9(5).
01  c        pic s9(5).
01  k        pic 9(4) value 0.
01  s        pic x(8).
01  n        pic s9(5).
procedure division.
main.
    display "  " bump(0)
    move bump(0) to n
    display "display " n
    move bump(0) to b
    move bump(0) to n
    display "move " n
    move bump(0) to b c
    move bump(0) to n
    display "move2 " n
    compute b = bump(0)
    move bump(0) to n
    display "compute " n
    compute b = bump(0) * 2
    move bump(0) to n
    display "compute+ " n
    add bump(0) to b
    move bump(0) to n
    display "add " n
    add bump(0) to 1 giving b
    move bump(0) to n
    display "addgiving " n
    subtract bump(0) from b
    move bump(0) to n
    display "subtract " n
    multiply bump(0) by b
    move bump(0) to n
    display "multiply " n
    divide bump(0) into b
    move bump(0) to n
    display "divide " n
    if bump(0) > 0 continue end-if
    move bump(0) to n
    display "if " n
    if not bump(0) > 0 continue end-if
    move bump(0) to n
    display "ifnot " n
    if bump(0) > 0 and b > 0 continue end-if
    move bump(0) to n
    display "ifand " n
    evaluate bump(0) when 0 continue when other continue end-evaluate
    move bump(0) to n
    display "evaluate " n
    evaluate true when bump(0) > 0 continue when other continue end-evaluate
    move bump(0) to n
    display "evalwhen " n
    evaluate b when bump(0) continue when other continue end-evaluate
    move bump(0) to n
    display "evalwhen2 " n
    string bump(0) delimited by size into s
    move bump(0) to n
    display "string " n
    perform bump(0) times continue end-perform
    move bump(0) to n
    display "performtimes " n
    initialize s replacing alphanumeric by pad(bump(0))
    move bump(0) to n
    display "initialize " n
    inspect s tallying k for all pad(bump(0))
    move bump(0) to n
    display "inspect " n
    call "subp" using by content bump(0)
    move bump(0) to n
    display "call " n
    display "  " bump(0) " " bump(0)
    move bump(0) to n
    display "display2 " n
    unstring pad(bump(0)) into s
    move bump(0) to n
    display "unstring " n
    stop run.
end program userfncount.

identification division.
program-id. subp.
data division.
linkage section.
01  p        pic s9(5).
procedure division using p.
    goback.
end program subp.
