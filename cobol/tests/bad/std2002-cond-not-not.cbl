identification division.
program-id. p-cond-not-not.
*> NOT NOT in a condition: not a permitted pair of elements (2023 8.8.4.11.3,
*> table 5).
data division.
working-storage section.
01 a pic 9 value 1.
procedure division.
    if not not a = 1 display "x" end-if
    goback.
