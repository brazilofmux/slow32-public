identification division.
program-id. p.
*> ENTRY-CONVENTION is not for a contained program (2023 11.9.7.3 rule 1).
procedure division.
    stop run.
identification division.
program-id. q.
options.
    entry-convention is cobol.
procedure division.
    goback.
end program q.
end program p.
