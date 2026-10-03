*> A packed item whose digits do not fill its nibbles has a pad nibble
*> at the top: an even count of digits with a sign, an odd count without
*> (COMP-6).  Whatever that nibble holds -- a group MOVE puts the high
*> nibble of '0', 3, there -- it is not a digit: the item reads as its
*> picture says.  gen-native seed 51 found the runtime counting it where
*> the compiler's in-line decoders did not.  COMP-3, COMP-6 and X"..."
*> are extensions in 85: the oracle compiles it in its default dialect.
identification division.
program-id. packedpad.
data division.
working-storage section.
01 pk4  pic 9(4) comp-3.
01 pk4r redefines pk4 pic x(3).
01 sk2  pic s9(2) comp-3.
01 sk2r redefines sk2 pic x(2).
01 pv   pic v9(4) comp-3.
01 grp.
    05 g1 pic x(3) value "016".
01 r6   pic 9(6).
01 r2   pic 9(2).
01 s4   pic s9(4).
procedure division.
    move x"301234" to pk4r
    add pk4 0 giving r6
    display "9(4) comp-3 x'301234': " pk4 " " r6
    if pk4 = 1234 display "equal 1234" else display "not equal 1234" end-if
    move x"F12D" to sk2r
    move sk2 to s4
    display "s9(2) comp-3 x'F12D': " sk2 " " s4
    move grp to pv
    add pv 0 giving r2
    display "v9(4) comp-3 from '016': " pv " " r2
    stop run.
