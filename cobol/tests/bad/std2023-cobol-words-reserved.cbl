>>COBOL-WORDS EQUATE "DISPLAY" WITH "MOVE"
identification division.
program-id. p-std2023-cobol-words-reserved.
*> the word brought in is not a reserved word (7.3.10.3 rule 4).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9(3) value 1.
procedure division.
    display x.
    stop run.
