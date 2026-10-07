identification division.
program-id. p-fig-all-zero-arith.
*> ADD ALL ZERO: where a numeric literal goes, ZERO is the one figurative
*> constant and without ALL (2023 8.3.3.6.3 rule 1a).
data division.
working-storage section.
01 n pic 9(3).
procedure division.
    add all zero to n
    goback.
