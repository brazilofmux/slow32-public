>>COBOL-WORDS RESERVE "FOO"
>>COBOL-WORDS EQUATE "DISPLAY" WITH "FOO"
identification division.
program-id. p-std2023-cobol-words-dup.
*> a word in two COBOL-WORDS directives (7.3.10.3 rule 5).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
