identification division.
program-id. ecrecur.
*> EC-PROGRAM-RECURSIVE-CALL through the caller (COBOL 2002 14.9.4
*> general rule 3f; cobol ISSUES-60): with checking on, the CALL of an
*> active program that is not RECURSIVE raises it at the CALL, so the
*> caller's declarative runs before the run ends (fatal).  2002/recnot is
*> the same call with checking off: the called program's own prologue
*> stops the run.  No oracle.
data division.
working-storage section.
01  depth    pic 9 value 0.
procedure division.
declaratives.
rc section.
    use after exception condition ec-program-recursive-call.
r1.
    display "declarative: " function exception-status " at depth " depth.
end declaratives.
main section.
m1.
>>TURN EC-PROGRAM-RECURSIVE-CALL CHECKING ON
    add 1 to depth
    display "entry " depth
    call "ecrecur"
    display "not reached"
    stop run.
end program ecrecur.
