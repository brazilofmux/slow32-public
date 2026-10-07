identification division.
program-id. p-std2014-exception-file-arg.
*> EXCEPTION-FILE with a file-name is COBOL 2023 (15.28).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
01 sn pic s9(3) value -1.
01 fl usage float-long.
01 r pic x(20).

procedure division.
    move function exception-file(x) to r.
    stop run.
