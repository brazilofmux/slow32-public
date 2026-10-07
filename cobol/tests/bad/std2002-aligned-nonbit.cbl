identification division.
program-id. p.
*> ALIGNED is for a bit group item or an elementary bit data item (2023 13.18.1.3 rule 1).
data division.
working-storage section.
01  a pic x(3) aligned.
procedure division.
    stop run.
end program p.
