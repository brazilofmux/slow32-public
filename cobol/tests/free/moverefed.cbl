*> A MOVE to a reference-modified edited item: the receiver is
*> alphanumeric (2023 8.4.2.4.3; X3.23-1985 5.5.4), so the characters go
*> in as they are, with no editing and no insertion.  Until the SET sweep
*> (2026-09-30) this compiler edited them: "1234" into ne(2:4) gave
*> "*1,234.0", "abc" into ae(1:3) gave "ab c ".
identification division.
program-id. moverefed.
data division.
working-storage section.
01 ne pic z,zz9.99 value all "*".
01 ae pic xxbxx value all "*".
procedure division.
    move "1234" to ne(2:4) display "[" ne "]"
    move "abc" to ae(1:3) display "[" ae "]"
    stop run.
