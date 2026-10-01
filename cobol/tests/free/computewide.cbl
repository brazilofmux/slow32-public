identification division.
program-id. computewide.
*> COMPUTE whose products pass 18 digits though no item does.  The
*> stack shed the operands' fraction digits to fit 64 bits, losing the
*> eighth significant digit (found by the differential generator,
*> tests/gen); such an expression is now computed wide.
data division.
working-storage section.
01 k04  pic s9(3)v9(6) usage binary.
01 k06  pic v9(6) usage binary.
01 k07  pic 9(7)v9(2) usage binary.
01 n02  pic s9(12)v9(6).
01 k01  pic s9(5)v9(3) usage binary.
01 k02  pic s9(6)v9(3).
01 k03  pic s9(1)v9(6) usage packed-decimal.
01 n18  pic s9(12) usage packed-decimal.
procedure division.
*> exactly -513019442.64757771534638
    move 0.095222 to k06  move 9297706.47 to k07  move -579.456307 to k04
    compute n02 rounded = k06 * k07 * k04
    display "three " n02
*> exactly -5415801597.093253736076
    move -29325.814 to k01  move 28091.909 to k02  move 6.574026 to k03
    compute n18 rounded = k01 * k02 * k03
    display "round " n18
*> a sum of products, and a condition
    compute n02 = k06 * k07 * k04 + k01 * k02 * k03
    display "sum   " n02
    if k06 * k07 * k04 < -513019442.6475
        display "cond  below"
    else
        display "cond  not below"
    end-if
    stop run.
