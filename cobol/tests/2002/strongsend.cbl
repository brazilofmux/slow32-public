identification division.
program-id. strongsend.
*> A strongly-typed group as a MOVE sender goes anywhere a group does:
*> 14.9.25.3 rule 2 constrains only a strongly-typed receiver, which takes
*> a group of its own type (cobol ISSUES-94 B13). So a strong group moves
*> to an alphanumeric item as a group move, and an elementary item in a
*> strong group receives as any elementary item does.
*> No oracle: GnuCOBOL 4 has no strong types.
data division.
working-storage section.
01  st1 typedef strong.
    05 n1 pic 9(3).
    05 c1 pic x(2).
01  s1 type st1.
01  s2 type st1.
01  al pic x(5).
procedure division.
    move 42 to n1 of s1 move "AB" to c1 of s1
    move s1 to al
    display "strong group to alphanumeric: [" al "]"
    move 7 to n1 of s2
    move n1 of s1 to n1 of s2
    display "elementary in a strong group receives: " n1 of s2
    move s1 to s2
    display "same type: [" c1 of s2 "]"
    stop run.
