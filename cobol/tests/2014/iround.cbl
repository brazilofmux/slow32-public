*> INTERMEDIATE ROUNDING (COBOL 2014; 2023 11.9.11): how the intermediates
*> of a unit's arithmetic lose the digits they cannot keep.  With NATIVE
*> arithmetic the rule is the implementor's (GR 1): TRUNCATION unless the
*> clause says otherwise, applied wherever the stacks shed digits -- a
*> quotient's last digit, the fraction digits a product sheds for room,
*> the alignment and fitting of the 38-digit stack.  Three contained
*> programs, one per mode, each computing the same statements: 2 / 3 * 3
*> is 1.99 truncated and 2.00 rounded; a product of forty decimals cut
*> to the stack's 38; a wide quotient; PROHIBITED makes an inexact intermediate a size error
*> (EC-SIZE-TRUNCATION) and leaves an exact one alone; a contained
*> program's clause is its own, the caller's mode back on return.
*> No oracle: GnuCOBOL 4 does not take the clause.
*> docs/conformance/options.md, arithmetic.md
identification division.
program-id. iround.
data division.
working-storage section.
01 s pic 9(3)v9(2).
procedure division.
    compute s = 2 / 3 * 3 display "outer (truncation) " s
    call "mtrunc"
    call "maway"
    call "meven"
    call "mprohib"
    compute s = 2 / 3 * 3 display "outer again        " s
    stop run.
identification division.
program-id. mtrunc.
options.
    intermediate rounding is truncation.
data division.
working-storage section.
01 r pic 9v9(8).
01 s pic 9(3)v9(2).
01 w pic 9(20)v9(10).
01 a pic 9v9(20) value 0.99999999999999999999.
01 b pic 9v9(20) value 0.50000000000000000005.
01 big pic 9(18)v9(13) value 1.
procedure division.
    display "truncation"
    compute s = 2 / 3 * 3 display "  2/3*3      " s
    compute s = 0.625 * 2 display "  0.625*2    " s
    compute s = 2.5 / 1 * 2 display "  2.5/1*2    " s
    compute w = 1 / 7 display "  1/7 wide   " w
    compute w = big / 3 on size error display "  big/3 size error" not on size error display "  big/3      " w end-compute
    compute w = a * b * 10000000000000000000000000000 display "  a*b*1E28   " w
    goback.
end program mtrunc.
identification division.
program-id. maway.
options.
    intermediate rounding is nearest-away-from-zero.
data division.
working-storage section.
01 r pic 9v9(8).
01 s pic 9(3)v9(2).
01 w pic 9(20)v9(10).
01 a pic 9v9(20) value 0.99999999999999999999.
01 b pic 9v9(20) value 0.50000000000000000005.
01 big pic 9(18)v9(13) value 1.
procedure division.
    display "nearest-away-from-zero"
    compute s = 2 / 3 * 3 display "  2/3*3      " s
    compute s = 0.625 * 2 display "  0.625*2    " s
    compute s = 2.5 / 1 * 2 display "  2.5/1*2    " s
    compute w = 1 / 7 display "  1/7 wide   " w
    compute w = big / 3 on size error display "  big/3 size error" not on size error display "  big/3      " w end-compute
    compute w = a * b * 10000000000000000000000000000 display "  a*b*1E28   " w
    goback.
end program maway.
identification division.
program-id. meven.
options.
    intermediate rounding is nearest-even.
data division.
working-storage section.
01 r pic 9v9(8).
01 s pic 9(3)v9(2).
01 w pic 9(20)v9(10).
01 a pic 9v9(20) value 0.99999999999999999999.
01 b pic 9v9(20) value 0.50000000000000000005.
01 big pic 9(18)v9(13) value 1.
01 h pic 9(18)v9(2) value 1.
procedure division.
    display "nearest-even"
    compute s = 2 / 3 * 3 display "  2/3*3      " s
    compute s = 0.625 * 2 display "  0.625*2    " s
    compute s = 2.5 / 1 * 2 display "  2.5/1*2    " s
    compute w = 1 / 7 display "  1/7 wide   " w
    compute w = big / 3 on size error display "  big/3 size error" not on size error display "  big/3      " w end-compute
    compute w = a * b * 10000000000000000000000000000 display "  a*b*1E28   " w
    goback.
end program meven.
identification division.
program-id. mprohib.
options.
    intermediate rounding is prohibited.
data division.
working-storage section.
01 r pic 9v9(8).
01 s pic 9(3)v9(2).
01 w pic 9(20)v9(10).
01 big pic 9(18)v9(13) value 1.
procedure division.
    display "prohibited"
    compute s = 2 / 3 * 3 on size error display "  2/3*3 size error" not on size error display "  2/3*3      " s end-compute
    compute s = 0.625 * 2 on size error display "  size error" not on size error display "  0.625*2    " s end-compute
    compute s = 5 / 8 * 2 on size error display "  size error" not on size error display "  5/8*2      " s end-compute
    compute w = big / 3 on size error display "  big/3 size error" not on size error display "  big/3      " w end-compute
    compute w = big / 4 on size error display "  size error" not on size error display "  big/4      " w end-compute
    goback.
end program mprohib.
end program iround.
