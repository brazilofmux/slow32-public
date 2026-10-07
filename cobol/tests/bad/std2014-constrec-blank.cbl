identification division.
program-id. p-constrec-blank.
*> No BLANK WHEN ZERO inside a structured constant (2023 13.16.3 rule 13).
data division.
working-storage section.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 f2 pic z9(3) blank when zero.
procedure division.
    display f1
    stop run.
