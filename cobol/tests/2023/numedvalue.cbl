*> A numeric literal as the VALUE of a numeric-edited item (2023
*> 13.18.63.3 rule 6; E.3.3 item 43): edited as a MOVE would, by the
*> runtime's own editing kernel compiled into the compiler, no digit or
*> sign truncated; ZERO the literal zero (E.2 item 28); a numeric item
*> with BLANK WHEN ZERO is numeric-edited (13.18.8.4 rule 2), its zero
*> VALUE spaces.  Each VALUE stands beside the MOVE of the same literal,
*> which must give the same bytes: a sign, a currency symbol, insertion,
*> BLANK WHEN ZERO, CR, a floating sign.  An alphanumeric VALUE of such an
*> item is its picture edited (rule 7; E.2 items 27 and 29: k, l).
*> No oracle: GnuCOBOL 4 refuses a numeric literal there.
*> docs/conformance/value.md
identification division.
program-id. numedvalue.
environment division.
configuration section.
special-names.
    currency sign is "E".
data division.
working-storage section.
01 a pic zz,zz9.99- value -1234.5.
01 a2 pic zz,zz9.99-.
01 b pic E**,**9.99 value 42.1.
01 b2 pic E**,**9.99.
01 c pic 9(3) blank when zero value 0.
01 c2 pic 9(3) blank when zero.
01 d pic +9(5) value zero.
01 d2 pic +9(5).
01 e pic -ZZ9 value -7.
01 e2 pic -ZZ9.
01 f pic 99/99/9999 value 10072026.
01 f2 pic 99/99/9999.
01 g pic 9.9(2)CR value 1.5.
01 g2 pic 9.9(2)CR.
01 h pic zzz value 0.
01 h2 pic zzz.
01 k pic EE,EE9.99- value "   E12.50-".
01 l pic zz9 blank when zero value "   ".
procedure division.
    move -1234.5 to a2 move 42.1 to b2 move 0 to c2 move zero to d2 move -7 to e2 move 10072026 to f2 move 1.5 to g2 move 0 to h2
    display "[" a "][" a2 "]" display "[" b "][" b2 "]" display "[" c "][" c2 "]" display "[" d "][" d2 "]"
    display "[" e "][" e2 "]" display "[" f "][" f2 "]" display "[" g "][" g2 "]" display "[" h "][" h2 "]"
    display "[" k "][" l "]"
    if a = a2 and b = b2 and c = c2 and d = d2 and e = e2 and f = f2 and g = g2 and h = h2 display "every VALUE as its MOVE" end-if
    stop run.
