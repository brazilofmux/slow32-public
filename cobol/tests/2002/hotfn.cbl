*> FUNCTION MOD, REM, INTEGER, INTEGER-PART and ABS computed in registers
*> (docs/performance.md): signs, negative divisors, nesting, 64 bits.
identification division.
program-id. hotfn.
data division.
working-storage section.
01 i   pic s9(9) comp.
01 k   pic s9(4) comp.
01 big pic 9(10).
01 d   pic s9(5)v99 value -12.75.
01 e   pic s9(5)v99 value 12.75.
01 r   pic s9(11).
01 r2  pic s9(5)v99.
01 m   pic 99.
procedure division.
    perform varying i from -7 by 3 until i > 8
        compute k = function mod(i, 3) display i " mod3 " k
        compute k = function rem(i, 3) display i " rem3 " k
        compute k = function mod(i, -4) display i " mod-4 " k
        compute k = function rem(i, -4) + function abs(i) display i " rem-4+abs " k
    end-perform
    move 9876543210 to big
    compute r = function mod(big * 1103515245 + 12345, 2147483648) display r
    compute r = function mod(big, 97) + function integer(d) display r
    compute r = function integer(d) display r
    compute r = function integer-part(d) display r
    compute r = function integer(e) display r
    compute r2 = function abs(d) * 2 display r2
    compute m = function mod(i * 7, 26) + 1 display m
    compute r = function mod(function mod(big, 1000) * 3, 7) display r
    stop run.
