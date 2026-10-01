identification division.
program-id. alnp.
*> An ANY LENGTH item is a parameter of the header (rules 3-4).
data division.
linkage section.
01 l pic x any length.
01 m pic x(4).
procedure division using m.
    goback.
