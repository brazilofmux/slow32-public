identification division.
program-id. p-class-alpha-packed.
*> ALPHABETIC of a packed item: the character tests want usage DISPLAY or
*> NATIONAL (2023 8.8.4.4.3 rule 3).
data division.
working-storage section.
01 c3 pic 9(3) comp-3 value 7.
procedure division.
    if c3 is alphabetic display "x" end-if
    goback.
