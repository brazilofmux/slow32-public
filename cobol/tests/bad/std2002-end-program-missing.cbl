identification division.
program-id. p-end-program-missing.
*> A program that contains another ends with END PROGRAM (2023 10.7.3 rule 1);
*> without it the second program is read as contained and the first has no
*> end marker.
procedure division.
    display "outer"
    stop run.
identification division.
program-id. inner.
procedure division.
    goback.
end program inner.
