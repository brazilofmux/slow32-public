identification division.
program-id. p-std2014-zero-move-numeric.
*> A zero-length alphanumeric literal moved is the figurative constant
*> SPACE (2023 14.9.25.4 rule 2), which does not go to a numeric item
*> (14.9.25.3 rule 5).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    move "" to n
    stop run.
