identification division.
program-id. al88.
*> No condition-name on an ANY LENGTH item (2023 13.16.3 rule 24f).
data division.
linkage section.
01 l pic x any length.
   88 l-yes value "y".
procedure division using l.
    goback.
