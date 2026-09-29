identification division.
program-id. bitrb.
*> A character item over a bit item that starts inside a byte would start
*> at a bit (13.18.44.4 rule 1); not implemented, refused by name.
data division.
working-storage section.
01  rec.
    05 f1 pic 1(3) usage bit.
    05 f2 pic 1(8) usage bit.
    05 x redefines f2 pic x.
procedure division.
    stop run.
