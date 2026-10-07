>>COBOL-WORDS RESERVE "COUNTER"
identification division.
program-id. p-std2023-cobol-words-reserve.
*> a RESERVEd word cannot name an item (7.3.10.4 rule 5).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 counter pic 9(3) value 1.
procedure division.
    move 1 to counter.
    stop run.
