identification division.
program-id. odorecv.
*> A receiving group over an OCCURS DEPENDING ON table whose DEPENDING
*> ON item is outside it uses only the part that item's value gives at
*> the start of the MOVE (X3.23-1985 VI-27, OCCURS general rule 3a) --
*> receiving as sending.  With the item inside the group, a receiving
*> group has its maximum length (rule 3b).  Every receiving group took
*> the maximum, overwriting the part past the current count (found by
*> the differential generator, tests/gen).  Rule 2 leaves the content of
*> occurrences past the count undefined; what is checked here is that
*> the MOVE did not write them, as rule 3a says and as programs building
*> variable-length records rely on.
data division.
working-storage section.
01 hn pic 99.
01 h.
   05 hh pic x(2).
   05 he occurs 1 to 8 times depending on hn pic x(2).
01 g.
   05 gn pic 99.
   05 ge occurs 1 to 8 times depending on gn pic x(2).
procedure division.
*> 3a: the item outside -- the move at count 3 writes 2 + 3 x 2 bytes
    move 8 to hn
    move all "=" to h
    move 3 to hn
    move "abcdefghijklmnopqr" to h
    move 8 to hn
    display "3a [" h "]"
*> 3a sending and receiving, two counts
    move 2 to hn
    move "ABCDEF" to h
    move 8 to hn
    display "3a [" h "]"
*> 3b: the item inside -- a receiving group has its maximum length
    move 8 to gn
    move all "=" to ge(1) ge(2) ge(3) ge(4) ge(5) ge(6) ge(7) ge(8)
    move 2 to gn
    move "05abcdefghijklmnop" to g
    display "3b [" g "]"
    stop run.
