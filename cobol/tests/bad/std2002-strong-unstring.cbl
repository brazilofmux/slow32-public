identification division.
program-id. sunst.
*> A strongly-typed group as an UNSTRING receiver: its category is its
*> type (8.5.2.1), none that 14.9.48.3 rule 4 allows (cobol ISSUES-94 B13).
data division.
working-storage section.
01  st1 typedef strong.
    05 n1 pic 9(3).
01  s1 type st1.
01  a  pic x(6) value "12,34".
procedure division.
    unstring a delimited by "," into s1
    stop run.
