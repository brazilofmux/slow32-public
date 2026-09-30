*> Micro Focus's rules for internal floating point (docs/usage.md):
*> an arithmetic result from a float is always rounded, ROUNDED being
*> documentary; a MOVE truncates; a float subscript or reference-
*> modification position is rounded to the nearest integer; an 88 on a
*> float.  No oracle: GnuCOBOL refuses a float subscript, which MF
*> rounds -- reviewed by hand against MF's reference.
identification division.
program-id. floatmf.
data division.
working-storage section.
01 f   comp-2.
01 d   pic s9(3)v99.
01 t.
   05 e pic x occurs 5.
01 s   pic x(8) value "abcdefgh".
01 g   comp-2.
   88 g-half value 0.5.
01 i   pic 9.
procedure division.
    move "12345" to t
    compute f = 2 / 3
    compute d = f display "compute, no ROUNDED: " d
    move f to d display "move: " d
    move 2.6 to f
    display "subscript 2.6: " e(f)
    display "refmod (f:2): " s(f:2)
    compute i = f display "compute into 9: " i
    move 0.5 to g
    if g-half display "88 on a float" end-if
    stop run.
