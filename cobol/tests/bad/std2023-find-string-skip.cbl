identification division.
program-id. p-std2023-find-string-skip.
*> FIND-STRING: argument-3 is an integer (15.37.3 rule 3).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
01 sn pic s9(3) value -1.
01 fl usage float-long.
01 r pic x(20).

procedure division.
    move function find-string(x "b" start after -1) to n.
    stop run.
