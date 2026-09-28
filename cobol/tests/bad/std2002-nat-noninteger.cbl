identification division.
program-id. natnonint.
*> A numeric noninteger has no national form (2023 14.9.25 table).
data division.
working-storage section.
01  d pic 9v9 value 1.5.
01  n pic n(4).
procedure division.
    move d to n
    stop run.
