identification division.
program-id. odomove.
*> MOVE of a group that ends in an OCCURS DEPENDING ON table.  Sending,
*> the group is as long as DEPENDING ON says.  Receiving, the 1985 text
*> gives it its maximum length when the DEPENDING ON item lies outside
*> the group (docs/oracles.md: GnuCOBOL uses the current length there
*> too, so the second line differs and .oracle-expected carries its).
data division.
working-storage section.
01  n        pic 99 value 5.
01  tbl.
    05  e occurs 1 to 5 times depending on n pic x(3).
01  x        pic x(15) value all '.'.
01  y        pic x(4) value all '-'.
01  rec.
    05  hdr  pic x(2) value 'HD'.
    05  cnt  pic 9 value 3.
    05  itm occurs 0 to 4 times depending on cnt pic 99.
procedure division.
main.
    move 'ABCDEFGHIJKLMNO' to tbl.
    move 2 to n.
    move tbl to x.
    display '[' x ']'.
    move 'abcdefghijklmno' to tbl.
    move 5 to n.
    move tbl to x.
    display '[' x ']'.
    move 2 to n.
    move tbl to y.
    display '[' y ']'.
    move 1 to itm(1). move 2 to itm(2). move 3 to itm(3). move 4 to itm(4).
    move rec to x.
    display '[' x ']'.
    move 2 to cnt.
    move rec to x.
    display '[' x ']'.
    stop run.
