identification division.
program-id. divremu.
*> DIVIDE ... REMAINDER with an unsigned quotient item and operands of
*> opposite sign.  X3.23-1985 VI-81, DIVIDE rule 6: the remainder is the
*> dividend less the product of the quotient (identifier-3) and the
*> divisor -- the quotient as stored, its magnitude in an unsigned item.
*> 2002 and 2023 (14.9.12, general rules 6c and 7) use a signed
*> subsidiary quotient instead.  The user's ruling, 2026-09-30: each
*> edition as its text says.  tests/2002/divremu is this program under
*> 2002.  CCVS is silent; GnuCOBOL takes the signed quotient under 85 too
*> (docs/oracles.md).  Found by tests/gen/arith85.py.
data division.
working-storage section.
01 q   pic 9.
01 qs  pic s9.
01 r   pic s9.
01 rw  pic s99.
01 q2  pic 9v9.
01 r2  pic s9v99.
procedure division.
*> -7 / 2: Q stores 3.  85: -7 - 3 x 2 = -13, past S9: size error, R
*> unchanged.  2002: -7 - (-3) x 2 = -1
    move 5 to r
    divide -7 by 2 giving q remainder r
        on size error display "1 size error q=" q " r=" r
        not on size error display "1 q=" q " r=" r
    end-divide
*> the same into S99: 85 gives -13, 2002 -1
    divide -7 by 2 giving q remainder rw
    display "2 q=" q " rw=" rw
*> a signed quotient item: -1 in both editions
    divide -7 by 2 giving qs remainder r
    display "3 qs=" qs " r=" r
*> ROUNDED, decimals: -1 / 0.3 = -3.33.., Q stores 3.3 rounded; the
*> intermediate is truncated, 3.3 (85, unsigned) or -3.3 (2002, signed)
    divide -1 by 0.3 giving q2 rounded remainder r2
    display "4 q2=" q2 " r2=" r2
    stop run.
