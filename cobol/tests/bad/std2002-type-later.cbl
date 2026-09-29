identification division.
program-id. tlater.
*> A TYPE clause names a type declared before the entry (2023 13.18.57.3;
*> a type does not refer to itself or to a later one, 13.18.58.3 rule 2;
*> cobol ISSUES-94): here the type comes after.
data division.
working-storage section.
01  a type later-t.
01  later-t typedef pic x(3).
procedure division.
    display a
    stop run.
