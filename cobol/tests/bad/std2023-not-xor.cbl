identification division.
program-id. p-std2023-not-xor.
*> NOT XOR is not a permitted pair (2023 8.8.4.11.3, table 5).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    if n = 1 not xor n = 2 display "x" end-if
    stop run.
