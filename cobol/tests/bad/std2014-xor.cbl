identification division.
program-id. p-std2014-xor.
*> XOR is COBOL 2023 (8.7.6).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    if n = 1 xor n = 2 display "x" end-if
    stop run.
