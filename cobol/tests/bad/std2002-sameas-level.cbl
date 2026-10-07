identification division.
program-id. p.
*> SAME AS names an elementary item, or a group at level 01 (2023 13.18.49.3 rule 7).
data division.
working-storage section.
01  a.
    05  g.
        10  h pic x.
01  x same as g.
procedure division.
    stop run.
end program p.
