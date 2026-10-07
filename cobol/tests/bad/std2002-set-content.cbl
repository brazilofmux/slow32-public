identification division.
program-id. p23.
*> SET CONTENT OF under -std=2002: format 15 is 2014's, and the refusal names
*> the switch that takes it (item 21).
data division.
working-storage section.
01 a usage float-long.
procedure division.
    set content of a to farthest-from-zero
    goback.
