*> USAGE BINARY-DOUBLE [SIGNED | UNSIGNED] (2023 13.18.60, COBOL 2002):
*> eight bytes holding at least -9223372036854775808 through
*> 9223372036854775807 (0 through 18446744073709551615 unsigned) -- its
*> 19 and 20 digits take the wide path (docs/wide.md).  VALUE, MOVE,
*> DISPLAY, arithmetic up to the capacity, ON SIZE ERROR past it, and
*> FUNCTION HIGHEST-ALGEBRAIC / LOWEST-ALGEBRAIC.
identification division.
program-id. bindouble.
data division.
working-storage section.
01 s  binary-double value -42.
01 u  binary-double unsigned value 18446744073709551615.
01 m  binary-double signed.
01 w  pic s9(25).
procedure division.
    display "s " s
    display "u " u
    move 9223372036854775807 to m display "max " m
    add 1 to m on size error display "size error at max" end-add
    move -9223372036854775808 to m display "min " m
    compute w = m * 2 display "w " w
    compute m = s * 1000000000000000 display "m " m
    compute u = u - 1 display "u-1 " u
    add 1 to u add 1 to u on size error display "size error unsigned" end-add
    move function highest-algebraic(m) to w display "high " w
    move function lowest-algebraic(m) to w display "low " w
    move function highest-algebraic(u) to w display "uhigh " w
    stop run.
