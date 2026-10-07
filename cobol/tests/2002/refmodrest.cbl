identification division.
program-id. refmodrest.
*> Reference modification's leftovers (docs/plans/standard-queue.md item
*> 10), each refused until now: LENGTH OF and the intrinsic functions of
*> a part of computed length (a result of run-time length), a function's
*> reference modification with an expression length, INITIALIZE of a
*> part with the 2002 phrases, MOVE's general rule 1 for a sender of
*> computed length whose length a receiver changes (identified once).
*> BY CONTENT of a bit item's part is 2002/refmodbit (GnuCOBOL has no
*> USAGE BIT).  GnuCOBOL 4 agrees but for FUNCTION LENGTH and
*> BYTE-LENGTH of the part, which it takes as the whole item's
*> (docs/oracles.md).
data division.
working-storage section.
01  s        pic x(20) value "hello world of cobol".
01  t        pic x(12) value "abcdefghijkl".
01  n        pic 9(4) value 5.
01  i        pic 9(4) value 2.
01  len      pic 9(4).
01  r        pic x(10).
01  r1       pic x(6).
01  grp.
    05  n2   pic 9(4) value 2.
    05  r2   pic x(6).
procedure division.
    move length of s(i:n) to len display "length of:    " len
    move function length(s(i:n)) to len display "length():     " len
    move function byte-length(s(i:n)) to len display "byte-length:  " len
    move function upper-case(s(i:n)) to r display "upper-case:   " r
    move function reverse(s(i:n)) to r display "reverse:      " r
    move function trim(s(i:n + 1)) to r display "trim:         " r
    move function upper-case(s)(i:n + 1) to r display "fn refmod:    " r
    move function upper-case(s)(2:n + 1) to r display "fn refmod 2:  " r
    initialize s(i:n) with filler display "init filler:  " s
    initialize s(1:2) replacing alphanumeric by "zz" display "init replace: " s
    initialize s(1:2) to default display "init default: " s
    move t(n2 + 1:n2) to r2 grp r1
    display "move once:    [" grp "] [" r1 "]"
    move t(1:i) to i r1
    display "move once 2:  [" r1 "]"
    stop run.
end program refmodrest.
