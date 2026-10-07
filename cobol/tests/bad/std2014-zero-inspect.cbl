identification division.
program-id. p-std2014-zero-inspect.
*> A zero-length literal is not an INSPECT operand (2023 14.9.22.3 rule 3).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    inspect x tallying n for all ""
    stop run.
