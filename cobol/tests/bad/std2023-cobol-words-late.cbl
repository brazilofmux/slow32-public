identification division.
program-id. p-std2023-cobol-words-late.
*> COBOL-WORDS before the first IDENTIFICATION DIVISION (7.3.10.3 rule 1).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
>>COBOL-WORDS RESERVE "FOO"
    display x.
    stop run.
