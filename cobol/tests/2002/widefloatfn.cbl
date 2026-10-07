identification division.
program-id. widefloatfn.
*> The floating intrinsic functions with 31-digit arguments and
*> results (docs/plans/standard-queue.md item 13; docs/wide.md): SQRT,
*> LOG, LOG10, the trigonometric ones, MEAN, MEDIAN, VARIANCE,
*> STANDARD-DEVIATION, ANNUITY, PRESENT-VALUE, EXP of items past 18
*> digits, into receivers past 18 digits.  SIN, COS and TAN reduce the
*> argument by 2 pi in decimal first, so SIN(10 ** 24) is sin of
*> 10 ** 24, not of the nearest double.  A float item of 10 ** 25 into
*> a 31-digit item is the double's own value.  The receivers hold at
*> most 13 significant digits: a result is computed in double and
*> given to 15, where GnuCOBOL's is exact, and the 15th is the soft
*> libm's to lose (docs/oracles.md, docs/wide.md).
data division.
working-storage section.
01  big      pic 9(25) value 1000000000000000000000000.
01  a        pic 9(22) value 2000000000000000000000.
01  b        pic 9(22) value 4000000000000000000000.
01  f        usage float-long.
01  r        pic 9(26)v9(5).
01  ri       pic 9(13)v9(2).
01  rs       pic s9(2)v9(10).
01  rw       pic 9(25)v9(6).
procedure division.
    compute ri = function sqrt(big) display "sqrt      " ri
    compute rs = function log(big) display "log       " rs
    compute rs = function log10(big) display "log10     " rs
    compute rs = function sin(big) display "sin       " rs
    compute rs = function cos(big) display "cos       " rs
    compute rs = function tan(big) display "tan       " rs
    compute rs = function atan(big) display "atan      " rs
    compute r = function mean(a b) display "mean      " r
    compute r = function median(a b big) display "median    " r
    compute r = function standard-deviation(a b) display "sd        " r
    compute rs = function annuity(0.05 120) display "annuity   " rs
    compute ri = function present-value(0.1 a b) / 1000000000000000 display "pv / 10**15 " ri
    compute ri = function exp(20) display "exp 20    " ri
    compute rs = function sqrt(2) display "sqrt 2    " rs
    move 10000000000000000905969664 to f
    compute r = f display "float     " r
    compute r = f / 4 display "float / 4 " r
    stop run.
end program widefloatfn.
