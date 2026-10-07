identification division.
program-id. p-std2014-zero-max.
*> The arguments of MAX, MIN, ORD-MAX and ORD-MIN are not zero-length
*> literals (2023 15.59.3 rule 3 and its siblings).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    display function max("a" "")
    stop run.
