identification division.
program-id. alpic.
*> ANY LENGTH takes PICTURE X or N, one symbol (rule 1).
data division.
linkage section.
01 l pic x(5) any length.
procedure division using l.
    goback.
