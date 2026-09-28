identification division.
program-id. boollit.
*> A boolean literal holds only 0 and 1 (2023 8.3.3.4.3 rule 2).
data division.
working-storage section.
01  b pic 1(4).
procedure division.
    move b"0121" to b
    stop run.
