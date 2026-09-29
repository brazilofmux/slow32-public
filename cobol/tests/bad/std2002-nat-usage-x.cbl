identification division.
program-id. natx.
*> USAGE NATIONAL takes N, numeric or numeric-edited pictures, not X (2023 13.18.60.3 rule 12).
data division.
working-storage section.
01  a pic x(3) usage national.
procedure division.

    stop run.
