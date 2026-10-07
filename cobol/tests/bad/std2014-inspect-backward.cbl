identification division.
program-id. p-std2014-inspect-backward.
*> INSPECT BACKWARD is COBOL 2023 (14.9.22).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    inspect backward x tallying n for all "a"
    stop run.
