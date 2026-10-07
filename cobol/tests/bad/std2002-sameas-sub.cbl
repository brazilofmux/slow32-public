identification division.
program-id. p.
*> An entry with SAME AS is not followed by a subordinate entry (2023 13.18.49.3 rule 2).
data division.
working-storage section.
01  a pic x.
01  x same as a.
    05  y pic x.
procedure division.
    stop run.
end program p.
