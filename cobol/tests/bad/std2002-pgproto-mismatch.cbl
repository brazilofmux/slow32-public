identification division.
program-id. sq is prototype.
*> A program definition conforms to its prototype: the same parameters,
*> passed the same way (2023 14.8.2).
data division.
linkage section.
01  n pic 9(3).
procedure division using by value n.
end program sq.
identification division.
program-id. sq.
data division.
linkage section.
01  n pic 9(3).
procedure division using n.
    display n
    goback.
end program sq.
