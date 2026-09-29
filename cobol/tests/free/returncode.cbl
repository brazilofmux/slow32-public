*> RETURN-CODE (IBM, Micro Focus; docs/dialect.md): a program sets it,
*> and after the CALL its caller's RETURN-CODE has the value; a program
*> that leaves it alone returns what it held; the main program's STOP
*> RUN exits with it (7 here; the harness does not check exit statuses).
*> An extension: the oracle compiles it in its default dialect.
identification division.
program-id. returncode.
procedure division.
    call "setrc"
    display "after setrc: " return-code
    move 0 to return-code
    call "leave"
    display "after leave: " return-code
    compute return-code = return-code + 7
    stop run.
end program returncode.

identification division.
program-id. setrc.
procedure division.
    move 42 to return-code
    goback.
end program setrc.

identification division.
program-id. leave.
procedure division.
    goback.
end program leave.
