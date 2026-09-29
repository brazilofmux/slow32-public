*> MOVE CORRESPONDING skips a pair whose MOVE would be invalid and an
*> index item (2023 14.7.6 rules 2 and 4); an ALL literal of digits goes
*> to an integer item (14.9.25.3 rule 5, an obsolete feature).
*> docs/conformance/move.md.  No oracle: GnuCOBOL refuses the invalid
*> pairs instead of skipping them (docs/oracles.md).
identification division.
program-id. movecorr.
data division.
working-storage section.
01 src.
   05 amt   pic 9(3)v99 value 123.45.
   05 code1 pic x(4)    value "0012".
   05 name  pic a(5)    value "HELLO".
   05 qty   pic 9(4)    value 42.
   05 ix    usage index.
01 dst.
   05 amt   pic x(5)    value "-----".
   05 code1 pic 9(4)    value 9999.
   05 name  pic 9(5)    value 77777.
   05 qty   pic a(4)    value "ZZZZ".
   05 ix    usage index.
01 n pic 9(6).
procedure division.
    set ix of src to 3
    set ix of dst to 1
    move corresponding src to dst
    *> amt (noninteger to alphanumeric), name (alphabetic to numeric),
    *> qty (integer to alphabetic) and ix do not correspond; code1 does
    display "[" amt of dst "] [" code1 of dst "] [" name of dst "] [" qty of dst "]"
    if ix of dst = 1 display "index untouched" else display "index moved" end-if
    move all "12" to n
    display n
    stop run.
