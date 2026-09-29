identification division.
program-id. sval.
*> A VALUE on a strongly-typed group (2023 13.18.63.3 rule 1; cobol
*> ISSUES-94 B13).
data division.
working-storage section.
01  st1 typedef strong.
    05 n1 pic 9(3).
01  s1 type st1 value spaces.
procedure division.
    display n1 of s1
    stop run.
