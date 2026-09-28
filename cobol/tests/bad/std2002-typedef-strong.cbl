identification division.
program-id. tdstrong.
*> Only a group is strongly typed (2023 13.18.58.3 rule 1).
data division.
working-storage section.
01  e-t pic x(4) typedef strong.
procedure division.

    stop run.
