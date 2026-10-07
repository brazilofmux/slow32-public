identification division.
program-id. p-cond-not-or.
*> NOT OR in a condition: not a permitted pair of elements (2023 8.8.4.11.3,
*> table 5); OR NOT is.
data division.
working-storage section.
01 a pic 9 value 1.
procedure division.
    if a = 1 not or a = 2 display "x" end-if
    goback.
