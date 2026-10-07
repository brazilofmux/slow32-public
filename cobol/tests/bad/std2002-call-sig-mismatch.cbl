identification division.
program-id. area is prototype.
*> Through a program-specifier, a BY REFERENCE argument is described as
*> the parameter is (2023 14.8.2.3.2 rule 2); 9(5)V99 for 9(3)V9 is not.
data division.
linkage section.
01  w        pic 9(3)v9.
01  r        pic 9(7)v99.
procedure division using w returning r.
end program area.
identification division.
program-id. p.
environment division.
configuration section.
repository.
    program area.
data division.
working-storage section.
01  a        pic 9(5)v99 value 12.5.
01  res      pic 9(7)v99.
procedure division.
    call area using a returning res
    stop run.
end program p.
