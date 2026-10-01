identification division.
program-id. alval.
*> An ANY LENGTH parameter is BY REFERENCE (rules 3-4).
data division.
linkage section.
01 l pic x any length.
procedure division using by value l.
    goback.
