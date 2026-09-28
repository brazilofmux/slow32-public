identification division.
program-id. tdstrong.
*> Strongly-typed groups come later; TYPEDEF STRONG is refused by name.
data division.
working-storage section.
01  t typedef strong.
    05 a pic x.
procedure division.

    stop run.
