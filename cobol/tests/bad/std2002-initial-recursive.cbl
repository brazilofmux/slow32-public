identification division.
program-id. outer is recursive.
*> A program contained in a RECURSIVE program cannot be INITIAL
*> (2023 11.10.3 rule 5), even under -std=2002.
procedure division.
    stop run.
identification division.
program-id. inner is initial.
procedure division.
    exit program.
end program inner.
end program outer.
