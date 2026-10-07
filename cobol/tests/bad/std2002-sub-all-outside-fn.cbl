identification division.
program-id. p-sub-all-outside-fn.
*> The subscript ALL outside an intrinsic function's argument (2023
*> 8.4.2.3.3 rule 6).
data division.
working-storage section.
01 t.
   05 v pic 9 occurs 3.
procedure division.
    display v (all)
    goback.
