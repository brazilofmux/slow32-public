identification division.
program-id. ecpgm.
*> EC-PROGRAM-NOT-FOUND (COBOL 2002 14.9.4 general rule 3b; cobol
*> ISSUES-59): with checking on, a CALL of a program the run unit does
*> not hold, with no ON EXCEPTION phrase, raises it; fatal.  With the
*> phrase, the phrase handles it, as before.  The CALL resolves at run
*> time, so the link does not need the missing program.  No oracle.
data division.
working-storage section.
01  nm       pic x(12) value "nowhere".
procedure division.
declaratives.
pg section.
    use after exception condition ec-program.
p1.
    display "declarative: " function exception-status
            " in " function exception-statement(1:4).
end declaratives.
main section.
m1.
>>TURN EC-PROGRAM-NOT-FOUND CHECKING ON WITH LOCATION
    call "ecpgm-here"
    call nm on exception display "the ON EXCEPTION phrase" end-call
    call "not-linked-anywhere"
    display "not reached"
    stop run.
end program ecpgm.

identification division.
program-id. ecpgm-here.
procedure division.
    display "ecpgm-here called"
    goback.
end program ecpgm-here.
