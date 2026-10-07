identification division.
program-id. sub1 is prototype.
*> A program prototype with a WORKING-STORAGE SECTION: only LINKAGE (2023
*> 10.6.2 rule 4e).
data division.
working-storage section.
01 w pic x.
procedure division.
end program sub1.
