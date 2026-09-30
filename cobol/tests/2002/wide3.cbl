*> 31 digits (docs/wide.md): exponentiation into a 31-digit receiver --
*> 2 ** 90 and 2 ** 100 fit in 31 digits, 2 ** 110 does not (ON SIZE
*> ERROR), 10 ** 30, and a negative base.
*> No oracle: GnuCOBOL gives zero for 2 ** 90 into S9(31).
identification division.
program-id. wide3.
data division.
working-storage section.
01 c  pic s9(31).
procedure division.
    compute c = 2 ** 90 display "p90 " c
    compute c = 2 ** 100 on size error display "p100 size" not on size error display "p100 " c end-compute
    compute c = 2 ** 110 on size error display "p110 size" not on size error display "p110 " c end-compute
    compute c = 10 ** 30 display "p10 " c
    compute c = -3 ** 61 display "neg " c
    stop run.
