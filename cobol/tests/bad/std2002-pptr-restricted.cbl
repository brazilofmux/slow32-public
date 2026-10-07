identification division.
program-id. q is prototype.
*> A restricted program-pointer takes NULL or a value restricted to the
*> same prototype (2023 14.9.39.3 rule 22); a literal's address is not.
procedure division.
end program q.
identification division.
program-id. p.
environment division.
configuration section.
repository.
    program q.
data division.
working-storage section.
01  pq       usage program-pointer to q.
procedure division.
    set pq to address of program "q"
    stop run.
end program p.
