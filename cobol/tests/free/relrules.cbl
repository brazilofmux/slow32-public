*> Relation conditions (X3.23-1985 6.3.1.1; 2023 8.8.4.2; default dialect:
*> COMP-3): zero equal whatever its sign, an integer DISPLAY item beside
*> text compared as its digits, edited items as characters, the shorter
*> operand padded with spaces, abbreviated combined relations.
identification division.
program-id. relrules.
data division.
working-storage section.
01 nz   pic s9(3) value -0.
01 pz   pic s9(3) value 0.
01 pk   pic s9(3) comp-3 value -0.
01 nd   pic 9(3) value 42.
01 xs   pic x(3) value "042".
01 xl   pic x(5) value "042  ".
01 ed1  pic zz9.99.
01 ed2  pic z9.999.
01 a    pic x(4) value "ab".
01 b    pic x(2) value "ab".
01 c4   pic 9(4) comp value 7.
01 d4   pic s9(4)v9 value 7.0.
01 sm   pic s9(4) comp value -1.
01 um   pic 9(4) comp value 1.
procedure division.
    move 1.5 to ed1 ed2
    if nz = pz display "-0 = +0" else display "BAD -0" end-if
    if pk = 0 display "packed -0 = 0" else display "BAD packed" end-if
    if nz = zero and pk not < zero and pz not > nz display "zero unique" end-if
    if nd = xs display "9(3) 42 = x(3) '042'" end-if
    if nd = xl display "vs longer, space-padded" end-if
    if nd < "1" display "042 < '1' as characters" end-if
    if ed1 = ed2 display "BAD edited equal" else display "edited compare as characters" end-if
    if a = b display "x(4) 'ab' = x(2) 'ab'" end-if
    if a > "ab" display "BAD pad" else display "pad with spaces" end-if
    if c4 = d4 display "comp 7 = 7.0" end-if
    if sm < um display "signed -1 < unsigned 1" end-if
    if c4 > 6 and < 8 display "abbreviated and" end-if
    if c4 = 1 or 7 display "abbreviated or" end-if
    if not c4 = 1 or 2 display "not over or" end-if
    if c4 not = 1 and 2 display "not = and" end-if
    stop run.
