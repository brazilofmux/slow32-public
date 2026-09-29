*> RENAMES at the edges of 13.18.45.3 rule 11 (85 rule 8): a range
*> starting inside a group and ending after it, a range ending in a
*> group, and a RENAMES of a group alone.  Data-name-2 inside
*> data-name-3 is renames3.
*> docs/conformance/renames.md
identification division.
program-id. renames2.
data division.
working-storage section.
01 g.
   05 a  pic x(2) value "aa".
   05 b.
      10 b1 pic x value "1".
      10 b2 pic x value "2".
   05 c  pic x(2) value "cc".
66 r4 renames b2 thru c.
66 r2 renames a thru b.
66 r3 renames b.
procedure division.
    display "[" r2 "][" r3 "][" r4 "]"
    move "wxyz" to r4
    display "[" g "]"
    move spaces to r3
    display "[" g "]"
    stop run.
