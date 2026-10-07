identification division.
program-id. p-fig-all-of-fig.
*> ALL of a figurative constant: ALL takes a literal (2023 8.3.3.6.3 rule 2).
data division.
working-storage section.
01 x pic x(3).
procedure division.
    move all all "a" to x
    goback.
