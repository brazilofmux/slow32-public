identification division.
program-id. p.
*> SAME AS data-name-1: not subject to an OCCURS clause (2023 13.18.49.3 rule 1).
data division.
working-storage section.
01  t.
    05  e occurs 3.
        10  f pic x.
01  x same as f.
procedure division.
    stop run.
end program p.
