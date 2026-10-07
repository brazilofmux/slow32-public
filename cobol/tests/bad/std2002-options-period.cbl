identification division.
program-id. p.
*> With a clause, the OPTIONS paragraph ends in a period (2023 11.9.3 rule 1).
options.
    arithmetic is native
data division.
working-storage section.
01 a pic 9.
procedure division.
    stop run.
end program p.
