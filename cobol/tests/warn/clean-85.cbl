identification division.
program-id. clean-85.
*> Nothing here changed meaning or became obsolete: -warn-74 must be
*> silent.  The AFTER item starts from a literal, not an outer VARYING
*> item, and the ODO group is only ever sent, never received whole.
data division.
working-storage section.
01  i pic 99.
01  j pic 99.
01  n pic 99 value 2.
01  grp.
    05 cnt pic 99.
    05 elem pic x occurs 1 to 5 depending on n.
01  dst pic x(8).
procedure division.
m1.
    perform nothing varying i from 1 by 1 until i > 2
            after j from 1 by 1 until j > 3
    move grp to dst
    move "x" to elem (1)
    stop run.
nothing.
    continue.
