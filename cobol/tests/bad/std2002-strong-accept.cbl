identification division.
program-id. sacc.
*> ACCEPT into a strongly-typed group (2023 14.9.1.3 rule 1; cobol
*> ISSUES-94 B13).
data division.
working-storage section.
01  st1 typedef strong.
    05 n1 pic 9(3).
01  s1 type st1.
procedure division.
    accept s1
    stop run.
