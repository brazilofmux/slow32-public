identification division.
program-id. brel.
*> Boolean operands relate by EQUAL and NOT EQUAL only (2023 8.8.4.2.2,
*> format 2; cobol ISSUES-94 B11).
data division.
working-storage section.
01  b1 pic 1(4) value b"1010".
01  b2 pic 1(4) value b"0101".
procedure division.
    if b1 > b2 display "greater" end-if
    stop run.
