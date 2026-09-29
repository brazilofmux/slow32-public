*> RENAMES b1 THRU b, b1 the first item of the group b: 13.18.45.3 rule
*> 11 (85 rule 8) asks only that b begin no earlier than b1 and end after
*> it, which it does; the range is b1's start to b's end.
*> docs/conformance/renames.md
*> No oracle: GnuCOBOL refuses a THRU item that is declared before
*> data-name-2 ("may not come before"), though its storage does not.
identification division.
program-id. renames3.
data division.
working-storage section.
01 g.
   05 a  pic x(2) value "aa".
   05 b.
      10 b1 pic x value "1".
      10 b2 pic x value "2".
   05 c  pic x(2) value "cc".
66 r1 renames b1 thru b.
procedure division.
    display "[" r1 "]"
    move "xyz" to r1
    display "[" g "]"
    stop run.
