*> FUNCTION MOD and REM with an ITEM as the divisor, in registers: the
*> checked 64-bit path takes them with a test on the divisor (zero goes
*> to the stack's code), the sign rules the text gives (MOD takes the
*> divisor's sign, REM the dividend's), dividends up to 18 digits.
*> Inside an in-line loop and out of one.  Must equal -fno-hot-arith (the stack throughout) and the oracle.
identification division.
program-id. moditem.
data division.
working-storage section.
01 i      pic s9(9) comp.
01 a      pic s9(9) comp.
01 d      pic s9(9) comp.
01 big    pic s9(18) comp.
01 r1     pic s9(18) comp.
01 r2     pic s9(18) comp.
01 r3     pic s9(18) comp.
01 r4     pic s9(18) comp.
01 n      pic 9(9) comp value 100000.
01 k      pic 9(9) comp.
procedure division.
    perform varying i from -3 by 1 until i > 3
        compute a = i * 7919 + 5
        move 7 to d
        compute r1 = function mod(a, d)
        compute r2 = function rem(a, d)
        move -7 to d
        compute r3 = function mod(a, d)
        compute r4 = function rem(a, d)
        display i " " a " " r1 " " r2 " " r3 " " r4
    end-perform
*>  the kidx shape: a product then MOD by an item, plus one
    perform varying i from 99990 by 1 until i > 100003
        compute k = function mod(i * 7919, n) + 1
        display "k " k
    end-perform
*>  a zero divisor: the stack's answer
    move 0 to d
    move 123 to a
    compute r1 = function mod(a, d)
    compute r2 = function rem(a, d)
    display "zero " r1 " " r2
*>  eighteen-digit dividends, a negative one, the divisor an item
    move 999999999999999989 to big
    move 1000003 to d
    compute r1 = function mod(big, d)
    compute r2 = function rem(big, d)
    move -999999999999999989 to big
    compute r3 = function mod(big, d)
    compute r4 = function rem(big, d)
    display "big " r1 " " r2 " " r3 " " r4
    move -1000003 to d
    compute r1 = function mod(big, d)
    compute r2 = function rem(big, d)
    display "bigneg " r1 " " r2
*>  the divisor an expression's item inside a larger expression
    move 17 to d
    move 1000 to a
    compute r1 = (function mod(a * 3 + 1, d) * 2) - function rem(a, d)
    display "expr " r1
    stop run.
